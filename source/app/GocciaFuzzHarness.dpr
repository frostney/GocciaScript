program GocciaFuzzHarness;

{ Coverage-blind crash harness for the lexer, parser, compiler, and VM, and
  for the heap snapshot reader (ADR 0148).

  Reads one source input from a file argument or stdin, then drives it
  through lex -> parse -> compile -> bytecode VM under tight instruction,
  timeout, memory, and stack bounds. The bounds exist so that a merely slow
  or merely large input is not reported as a crash: the harness treats every
  engine-defined limit and every JavaScript-level throw as a NORMAL outcome
  and exits 0. Only outcomes the engine does not model — an unexpected
  Pascal exception, an access violation, a heap corruption abort — reach the
  fuzzer as a nonzero exit.

  Usage:
    GocciaFuzzHarness <file>          # AFL++ / manual reproduction
    GocciaFuzzHarness -               # read stdin
    GocciaFuzzHarness --verbose <f>   # print the classification and detail
    GocciaFuzzHarness --heap-snapshot <file>
                                      # restore <file> as a heap snapshot
    GocciaFuzzHarness --write-heap-snapshot-seeds <dir>
                                      # write real snapshots to seed with

  A heap snapshot names the engine build that wrote it, and a reader refuses
  another build's, so the snapshot seeds are written by the harness that
  fuzzes them. Functions and generators are named by the bytecode of the
  module that compiled them, so the engine a snapshot is restored in runs
  the seeds' own program first. What restores is then used
  (Goccia.HeapSnapshot.Exercise): its functions called and its generators
  resumed, returned or thrown into.

  See docs/contributing/tooling.md for the AFL++ and reproduction workflow. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  { Backtrace symbolisation needs no unit here: dev builds pass -gl, which
    links FPC's DWARF line-info reader automatically (adding lnfodwrf by
    hand is a duplicate-identifier error). On macOS the frames still print
    as bare addresses because FPC emits DWARF into a separate .dSYM it does
    not read back — resolve those with `atos -o build/GocciaFuzzHarness`.
    The Linux CI job where the fuzzers actually run symbolises directly. }
  Classes,
  SysUtils,

  TextEncoding,
  TextSemantics,

  Goccia.CLI.Stdin,
  Goccia.Engine,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.HeapSnapshot.Exercise,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Modules.ContentProvider,
  Goccia.Arguments.Collection,
  Goccia.Scope.BindingMap,
  Goccia.StackLimit,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

const
  FUZZ_PROGRAM_NAME = 'GocciaFuzzHarness';
  FUZZ_EXECUTOR_NAME = 'bytecode';

  { Bounds are deliberately tight. A fuzzer generates far more slow inputs
    than interesting ones, and every second spent on a runaway loop is a
    second not spent exploring. These are also what makes an unbounded
    input distinguishable from a genuine hang. }
  FUZZ_TIMEOUT_MS = 1000;
  FUZZ_MAX_INSTRUCTIONS = 5000000;
  FUZZ_MAX_MEMORY_BYTES = Int64(256) * 1024 * 1024;
  FUZZ_STACK_DEPTH = 256;

  { A fuzzer will happily feed us a multi-megabyte file. Lexing that is not
    a useful discovery, so oversized inputs are rejected before the engine
    ever sees them. }
  FUZZ_MAX_INPUT_BYTES = 1024 * 1024;

  { Every opt-in dialect flag the engine has. A fuzzer exploring the default
    dialect would be rejected at the parser for `var`, `while`, traditional
    `for`, labels, `for...in`, loose equality, and `arguments`, which is most
    of the interesting grammar. Turning them all on is the widest reachable
    surface, and matches this harness's job of finding faults rather than
    exercising the shipped default configuration. }
  FUZZ_COMPATIBILITY = [cfASI, cfVar, cfFunction, cfTraditionalFor,
    cfWhileLoops, cfLooseEquality, cfNonStrictMode, cfArgumentsObject,
    cfLabel, cfForIn];

  EXIT_OK = 0;
  EXIT_USAGE = 64;

type
  { How a single run ended. Everything except feUnexpected is a bounded,
    engine-modelled outcome and must not be reported to the fuzzer.

    Stack overflow has no outcome of its own: the engine deliberately models a
    depth breach as a script-catchable RangeError (see Goccia.StackLimit), so it
    is indistinguishable from a guest throw at this boundary and lands in
    foScriptThrow. foMemoryLimit, by contrast, is a distinct RTL type. }
  TFuzzOutcome = (
    foCompleted,
    foScriptThrow,
    foParseError,
    foRuntimeError,
    foInstructionLimit,
    foTimeout,
    foMemoryLimit,
    foModuleDenied,
    foInputRejected,
    foSnapshotRefused,
    foUnexpected
  );

  { Raised instead of loading a module. Typed so the outcome ladder can tell
    "this input imported something" apart from a genuine stream fault. }
  EFuzzModuleDenied = class(Exception);

  { Denies every module load.

    Two reasons, both mandatory. First, a fuzz input must never reach the host
    filesystem — without this, `import "/etc/passwd"` is a file read driven by
    attacker-shaped input, which is precisely the property this harness exists
    to protect. Second, the engine's default provider refuses with a
    script-catchable JavaScript error, so the fuzzed input could swallow its own
    import with a try/catch and the outcome ladder would score the run as
    ordinary script behaviour; EFuzzModuleDenied is an RTL type the input cannot
    intercept, so owning the provider means owning that distinction. }
  TFuzzDeniedModuleContentProvider = class(TGocciaModuleContentProvider)
  public
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

var
  GVerbose: Boolean = False;

function TFuzzDeniedModuleContentProvider.Exists(const APath: string): Boolean;
begin
  Result := False;
end;

function TFuzzDeniedModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
begin
  raise EFuzzModuleDenied.Create('module loading is disabled: ' + APath);
end;

function TFuzzDeniedModuleContentProvider.TryGetLastModified(
  const APath: string; out ALastModified: TDateTime): Boolean;
begin
  ALastModified := 0;
  Result := False;
end;

function OutcomeName(const AOutcome: TFuzzOutcome): string;
begin
  case AOutcome of
    foCompleted:        Result := 'completed';
    foScriptThrow:      Result := 'script-throw';
    foParseError:       Result := 'parse-error';
    foRuntimeError:     Result := 'runtime-error';
    foInstructionLimit: Result := 'instruction-limit';
    foTimeout:          Result := 'timeout';
    foMemoryLimit:      Result := 'memory-limit';
    foModuleDenied:     Result := 'module-denied';
    foInputRejected:    Result := 'input-rejected';
    foSnapshotRefused:  Result := 'snapshot-refused';
  else
    Result := 'UNEXPECTED';
  end;
end;

procedure Report(const AOutcome: TFuzzOutcome; const ADetail: string);
begin
  if not GVerbose then
    Exit;
  if ADetail = '' then
    WriteLn(ErrOutput, Format('[%s] %s', [FUZZ_EXECUTOR_NAME,
      OutcomeName(AOutcome)]))
  else
    WriteLn(ErrOutput, Format('[%s] %s: %s', [FUZZ_EXECUTOR_NAME,
      OutcomeName(AOutcome), ADetail]));
end;

{ Renders the frames of the exception currently being handled. Kept separate
  from the report so the capture happens before the handler unwinds. }
function CaptureBackTrace: string;
var
  Frames: PPointer;
  I: Integer;
begin
  Result := '';
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Result := Result + BackTraceStrFunc(Frames[I]) + LineEnding;
end;

{ Runs one source on an engine of its own and classifies the outcome.

  The except ladder is the whole point of this harness, so it is exhaustive
  by construction: every engine-defined error type is named explicitly and
  the bare `on E: Exception` fallback is what a genuine finding looks like.
  Do not add a blanket handler above the specific ones. }
function RunOnce(const ASource: TStringList;
  out ADetail: string): TFuzzOutcome;
var
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
begin
  ADetail := '';
  StartExecutionTimeout(FUZZ_TIMEOUT_MS);
  StartInstructionLimit(FUZZ_MAX_INSTRUCTIONS);
  SetMaxStackDepth(FUZZ_STACK_DEPTH);
  try
    Engine := TGocciaEngine.Create(FUZZ_PROGRAM_NAME, ASource);
    try
      { Widen the accepted grammar as far as the engine allows. The default
        dialect rejects `var`, `while`, traditional `for`, labels, and more,
        which would wall the fuzzer off from most of the parser. Compatibility
        must be assigned after Create — the source pipeline runs at Execute,
        which is also why syntax errors are classified by type below rather
        than by which call raised them. }
      Engine.Compatibility := FUZZ_COMPATIBILITY;
      Engine.LabelStatementsEnabled := True;
      Engine.ForInLoopsEnabled := True;
      Engine.ModuleLoader.SetContentProvider(
        TFuzzDeniedModuleContentProvider.Create, True);

      GC := TGarbageCollector.Instance;
      if Assigned(GC) then
        GC.MaxBytes := FUZZ_MAX_MEMORY_BYTES;
      try
        Engine.Execute;
        Result := foCompleted;
      except
        on E: EGocciaBytecodeThrow do
        begin
          Result := foScriptThrow;
          ADetail := 'bytecode throw';
        end;
        on E: TGocciaThrowValue do
        begin
          Result := foScriptThrow;
          ADetail := E.Message;
        end;
        on E: TGocciaMemoryLimitError do
        begin
          { The 256 MB budget is a bounded, engine-modelled outcome, not a
            finding. TGocciaMemoryLimitError is an RTL type the input cannot
            catch (uncatchable from script by design), so the host owns this
            distinction here at its top-level boundary. Without this branch it
            would escape the ladder and be misreported as an unexpected fault. }
          Result := foMemoryLimit;
          ADetail := E.Message;
        end;
        on E: TGocciaInstructionLimitError do
        begin
          Result := foInstructionLimit;
          ADetail := E.Message;
        end;
        on E: TGocciaTimeoutError do
        begin
          Result := foTimeout;
          ADetail := E.Message;
        end;
        on E: EFuzzModuleDenied do
        begin
          Result := foModuleDenied;
          ADetail := E.Message;
        end;
        on E: TGocciaSyntaxError do
        begin
          { Covers TGocciaLexerError too — both are rejections of the input
            text, which is the single most common fuzz outcome. }
          Result := foParseError;
          ADetail := E.Message;
        end;
        on E: TGocciaError do
        begin
          Result := foRuntimeError;
          ADetail := E.Message;
        end;
      end;
    finally
      Engine.Free;
    end;
  finally
    ClearInstructionLimit;
    ClearExecutionTimeout;
  end;
end;

{ Reads stdin but stops the instant the FUZZ_MAX_INPUT_BYTES cap is breached,
  so a fuzzer piping a multi-gigabyte input through `-` is rejected without the
  harness ever holding it in memory. Reading raw bytes from the fd in chunks —
  rather than materialising every line and measuring afterwards — is what makes
  the bound apply before the allocation, not after: a single unbounded line
  would otherwise be fully read before any length check could see it.
  THandleStream never consults a (pipe-invalid) length and never closes the
  handle it did not open. }
function ReadBoundedStdin(out ASourceText: string; out ADetail: string): Boolean;
const
  STDIN_CHUNK_BYTES = 65536;
var
  Stream: THandleStream;
  Chunk, Collected: TBytes;
  Total, BytesRead: Integer;
begin
  ASourceText := '';
  ADetail := '';
  SetLength(Collected, 0);
  Total := 0;
  Stream := THandleStream.Create(StdInputHandle);
  try
    SetLength(Chunk, STDIN_CHUNK_BYTES);
    repeat
      BytesRead := Stream.Read(Chunk[0], STDIN_CHUNK_BYTES);
      if BytesRead > 0 then
      begin
        if Total + BytesRead > FUZZ_MAX_INPUT_BYTES then
        begin
          ADetail := Format('input exceeds %d bytes', [FUZZ_MAX_INPUT_BYTES]);
          Exit(False);
        end;
        SetLength(Collected, Total + BytesRead);
        Move(Chunk[0], Collected[Total], BytesRead);
        Inc(Total, BytesRead);
      end;
    until BytesRead <= 0;
  finally
    Stream.Free;
  end;
  if Total > 0 then
    SetString(ASourceText, PAnsiChar(@Collected[0]), Total);
  Result := True;
end;

function ReadInput(const AFileName: string; out ASource: TStringList;
  out ADetail: string): Boolean;
var
  SourceText: string;
  InputStream: TFileStream;
  FileByteSize: Int64;
  FileBytes: TBytes;
  DecodeErrorOffset: Integer;
begin
  ADetail := '';
  ASource := nil;
  try
    if (AFileName = '') or (AFileName = '-') then
    begin
      if not ReadBoundedStdin(SourceText, ADetail) then
        Exit(False);
    end
    else
    begin
      if not FileExists(AFileName) then
      begin
        ADetail := 'no such input file: ' + AFileName;
        Exit(False);
      end;
      { Reject an oversized file by its on-disk length BEFORE reading it.
        ReadUTF8FileText would otherwise allocate the whole file first, so a
        pathologically large input could exhaust memory before the size gate
        ran — and a harness that OOMs on input it should have rejected reports
        a false crash to the fuzzer. Opening the stream reads no content; only
        its length is consulted. }
      InputStream := TFileStream.Create(AFileName,
        fmOpenRead or fmShareDenyWrite);
      try
        FileByteSize := InputStream.Size;
        if FileByteSize > FUZZ_MAX_INPUT_BYTES then
        begin
          ADetail := Format('input exceeds %d bytes', [FUZZ_MAX_INPUT_BYTES]);
          Exit(False);
        end;
        { Read the handle that was just size-checked rather than reopening
          AFileName. Reopening would leave a TOCTOU window in which the path
          could be swapped for a larger file after the size check, defeating
          the cap; reading the validated handle binds the check and the read
          to the same open file. }
        SetLength(FileBytes, FileByteSize);
        if FileByteSize > 0 then
          InputStream.ReadBuffer(FileBytes[0], FileByteSize);
      finally
        InputStream.Free;
      end;
      if not TryDecodeUTF8(FileBytes, SourceText, DecodeErrorOffset) then
      begin
        ADetail := Format('invalid UTF-8 at byte %d', [DecodeErrorOffset]);
        Exit(False);
      end;
    end;
  except
    { Unreadable or undecodable bytes are an input-selection problem, not an
      engine finding. }
    on E: Exception do
    begin
      ADetail := E.Message;
      Exit(False);
    end;
  end;

  { Belt-and-suspenders: both interfaces above bound input at or before read,
    so this decoded-length re-check can only ever confirm the cap already held. }
  if Length(SourceText) > FUZZ_MAX_INPUT_BYTES then
  begin
    ADetail := Format('input exceeds %d bytes', [FUZZ_MAX_INPUT_BYTES]);
    Exit(False);
  end;

  ASource := CreateFileTextLines(SourceText);
  Result := True;
end;

{ The graphs the snapshot seeds hold: every codec the reader has, shared
  references, cycles, symbols, built-ins named and attributes, and built-ins
  the run changed, one of them built during the run. }
const
  HEAP_SNAPSHOT_SEEDS: array[0..6] of string = (
    '({ n: 1.5, s: "text", b: 12345678901234567890n, t: true, u: undefined, ' +
      'z: null, neg: -0 })',
    '(() => { const a = [1, , "three", [4]]; a.length = 8; a.extra = 1; ' +
      'return a; })()',
    '(() => { const shared = { s: 1 }; const o = { x: shared, y: [shared] }; ' +
      'o.self = o; return o; })()',
    '({ [Symbol("local")]: 1, [Symbol.for("shared")]: 2, ' +
      '[Symbol.iterator]: Array.prototype.values, m: Math, p: Object.prototype })',
    '(() => { const o = {}; Object.defineProperty(o, "h", { value: 1 }); ' +
      'Object.defineProperty(o, "g", { get: Object.prototype.valueOf, ' +
      'enumerable: true }); return [Object.freeze({ f: 1 }), Object.seal([1]), ' +
      'Object.preventExtensions(o), Object.create(null)]; })()',
    'Array.from({ length: 64 }, (_, i) => ({ i, next: { s: String(i) } }))',
    '(() => { const shared = { s: 1 }; JSON.fuzz = shared; ' +
      'delete JSON.rawJSON; Object.defineProperty(JSON, "acc", ' +
      '{ get: Object.prototype.valueOf, configurable: true }); ' +
      'JSON[Symbol("k")] = [shared]; Goccia.shims.push(shared); ' +
      'Object.seal(JSON); Reflect.fuzz = shared; return shared; })()');

{ The program whose functions and suspended generators the VM seeds hold:
  closures sharing a cell, a generator inside try/finally, one with a binding
  in its temporal dead zone, one with rest arguments and one not started,
  methods with their home object and a getter. capture(i, v) hands seed i
  over. Every engine that restores a snapshot runs it first, so the seeds'
  functions resolve there; it runs in the default dialect, as it was written. }
const
  HEAP_SNAPSHOT_VM_PROGRAM =
    '(() => { let n = 0; const inc = () => { n += 1; return n; }; ' +
    'const log = []; const source = { *steps(start) { try { ' +
    'const a = yield start; const b = yield a * 2; return a + b + start; } ' +
    'finally { log.push(n); } }, *late() { const peek = () => x; ' +
    'yield peek; let x = 5; yield peek(); }, *wide(...args) { yield 0; ' +
    'yield args.length + args[args.length - 1]; }, get count() { ' +
    'return n; } }; const g = source.steps(1); g.next(); g.next(10); ' +
    'inc(); const late = source.late(); const peek = late.next().value; ' +
    'const wide = source.wide(1, 2, 3, 4); wide.next(); ' +
    'capture(0, { inc, g, log }); capture(1, { late, peek }); ' +
    'capture(2, { wide, source, fresh: source.steps(7) }); ' +
    'capture(3, Array.from({ length: 16 }, (_, i) => i).reduce(' +
    '(prev, i) => { const f = () => prev; f.i = i; return f; }, null)); })();';
  HEAP_SNAPSHOT_VM_SEEDS = 4;

type
  { capture(i, v): keeps v as VM seed i, rooted until the harness exits. }
  TFuzzSeedCapture = class
  public
    Values: array[0..HEAP_SNAPSHOT_VM_SEEDS - 1] of TGocciaValue;
    function Capture(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Ignore(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

function TFuzzSeedCapture.Capture(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Index: Integer;
begin
  Index := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  if (Index >= 0) and (Index < HEAP_SNAPSHOT_VM_SEEDS) and
     not Assigned(Values[Index]) then
  begin
    Values[Index] := AArgs.GetElement(1);
    TGarbageCollector.Instance.AddTempRoot(Values[Index]);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TFuzzSeedCapture.Ignore(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ An engine that has run the VM seeds' program, with capture(i, v) bound to
  ACapture. }
function CreateVMSeedEngine(const ALines: TStringList;
  const ACapture: TGocciaNativeFunctionCallback): TGocciaEngine;
begin
  ALines.Text := HEAP_SNAPSHOT_VM_PROGRAM;
  Result := TGocciaEngine.Create(FUZZ_PROGRAM_NAME, ALines);
  Result.GlobalScope.DefineLexicalBinding('capture',
    TGocciaNativeFunctionValue.Create(ACapture, 'capture', 2), dtConst);
end;

procedure WriteSeed(const ASession: TGocciaHeapSnapshotSession;
  const AValue: TGocciaValue; const AFileName: string);
var
  Output: TFileStream;
begin
  Output := TFileStream.Create(AFileName, fmCreate);
  try
    ASession.Snapshot([AValue], Output);
  finally
    Output.Free;
  end;
end;

{ Writes one snapshot per seed graph into ADirectory, from a fresh engine
  each, and one per VM seed from one run of their program. Returns the
  number written. }
function WriteHeapSnapshotSeeds(const ADirectory: string): Integer;
var
  I: Integer;
  Lines: TStringList;
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  Capture: TFuzzSeedCapture;
begin
  Result := 0;
  ForceDirectories(ADirectory);
  for I := 0 to High(HEAP_SNAPSHOT_SEEDS) do
  begin
    Lines := TStringList.Create;
    Engine := nil;
    Session := nil;
    try
      Engine := TGocciaEngine.Create(FUZZ_PROGRAM_NAME, Lines);
      Session := TGocciaHeapSnapshotSession.Create(Engine);
      Session.CheckAccounting := False;
      Value := Engine.EvaluateGlobalEval(HEAP_SNAPSHOT_SEEDS[I],
        FUZZ_PROGRAM_NAME, Stage);
      TGarbageCollector.Instance.AddTempRoot(Value);
      try
        WriteSeed(Session, Value, IncludeTrailingPathDelimiter(ADirectory) +
          Format('heap-snapshot-%d.ghsn', [I]));
      finally
        TGarbageCollector.Instance.RemoveTempRoot(Value);
      end;
      Inc(Result);
    finally
      Session.Free;
      Engine.Free;
      Lines.Free;
    end;
  end;
  Lines := TStringList.Create;
  Capture := TFuzzSeedCapture.Create;
  Engine := nil;
  Session := nil;
  try
    Engine := CreateVMSeedEngine(Lines, Capture.Capture);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Session.CheckAccounting := False;
    Engine.Execute;
    for I := 0 to HEAP_SNAPSHOT_VM_SEEDS - 1 do
    begin
      WriteSeed(Session, Capture.Values[I],
        IncludeTrailingPathDelimiter(ADirectory) +
        Format('heap-snapshot-vm-%d.ghsn', [I]));
      Inc(Result);
    end;
  finally
    for I := 0 to HEAP_SNAPSHOT_VM_SEEDS - 1 do
      if Assigned(Capture.Values[I]) then
        TGarbageCollector.Instance.RemoveTempRoot(Capture.Values[I]);
    Session.Free;
    Engine.Free;
    Capture.Free;
    Lines.Free;
  end;
end;

{ Restores one input as a heap snapshot on an engine of its own, which has
  run the VM seeds' program, under the same bounds as a script run, and uses
  what restored. A refusal is the reader doing its job, and the run's limits
  and a script error are outcomes a run can have; only what escapes the
  typed ladder is a finding. }
function RestoreOnce(const AFileName: string;
  out ADetail: string): TFuzzOutcome;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Input: TFileStream;
  Restore: TGocciaHeapSnapshotRestore;
  Limits: TGocciaHeapSnapshotLimits;
  GC: TGarbageCollector;
  Capture: TFuzzSeedCapture;
begin
  ADetail := '';
  if not FileExists(AFileName) then
  begin
    ADetail := 'no such input file: ' + AFileName;
    Exit(foInputRejected);
  end;
  Limits := TGocciaHeapSnapshotLimits.Defaults;
  Limits.MaxInputBytes := FUZZ_MAX_INPUT_BYTES;
  Lines := TStringList.Create;
  Capture := TFuzzSeedCapture.Create;
  Engine := nil;
  Input := nil;
  Restore := nil;
  try
    Engine := CreateVMSeedEngine(Lines, Capture.Ignore);
    Engine.Execute;
    StartExecutionTimeout(FUZZ_TIMEOUT_MS);
    StartInstructionLimit(FUZZ_MAX_INSTRUCTIONS);
    SetMaxStackDepth(FUZZ_STACK_DEPTH);
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.MaxBytes := FUZZ_MAX_MEMORY_BYTES;
    Input := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Restore := TGocciaHeapSnapshotRestore.Create(Engine, Input, Limits);
      // A root can be any object a codec writes, a cell among them; what is
      // used is a value.
      if (Length(Restore.Roots) > 0) and
         (Restore.Roots[0] is TGocciaValue) then
        // The input's size picks how its generators are used, so each
        // way is fuzzed.
        ExerciseRestored(Engine, TGocciaValue(Restore.Roots[0]),
          Integer(Input.Size mod EXERCISE_MODES));
      Result := foCompleted;
    except
      on E: EGocciaHeapSnapshotRefused do
      begin
        Result := foSnapshotRefused;
        ADetail := E.Message;
      end;
      on E: TGocciaMemoryLimitError do
      begin
        Result := foMemoryLimit;
        ADetail := E.Message;
      end;
      on E: TGocciaInstructionLimitError do
      begin
        Result := foInstructionLimit;
        ADetail := E.Message;
      end;
      on E: TGocciaTimeoutError do
      begin
        Result := foTimeout;
        ADetail := E.Message;
      end;
      on E: TGocciaThrowValue do
      begin
        Result := foScriptThrow;
        ADetail := E.Message;
      end;
    end;
  finally
    Restore.Free;
    Input.Free;
    Engine.Free;
    Capture.Free;
    Lines.Free;
    ClearInstructionLimit;
    ClearExecutionTimeout;
  end;
end;

procedure PrintUsage;
begin
  WriteLn(ErrOutput, 'Usage: ', FUZZ_PROGRAM_NAME, ' [--verbose] <file>|-');
  WriteLn(ErrOutput, '       ', FUZZ_PROGRAM_NAME,
    ' [--verbose] --heap-snapshot <file>');
  WriteLn(ErrOutput, '       ', FUZZ_PROGRAM_NAME,
    ' --write-heap-snapshot-seeds <dir>');
  WriteLn(ErrOutput);
  WriteLn(ErrOutput, 'Drives one input through lex, parse, and both ' +
    'executors under tight bounds.');
  WriteLn(ErrOutput, 'Exits 0 for every engine-modelled outcome; nonzero ' +
    'only for an unexpected fault.');
end;

var
  I: Integer;
  Arg, FileName, Detail, Trace, SeedDirectory: string;
  Source: TStringList;
  Outcome: TFuzzOutcome;
  Failed, SelfTestFault, HeapSnapshot: Boolean;
begin
  FileName := '';
  Failed := False;
  SelfTestFault := False;
  HeapSnapshot := False;
  SeedDirectory := '';
  Trace := '';

  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if (Arg = '--verbose') or (Arg = '-v') then
      GVerbose := True
    else if Arg = '--self-test-fault' then
      SelfTestFault := True
    else if Arg = '--heap-snapshot' then
      HeapSnapshot := True
    else if Arg = '--write-heap-snapshot-seeds' then
    begin
      if I < ParamCount then
        SeedDirectory := ParamStr(I + 1);
    end
    else if (Arg = '--help') or (Arg = '-h') then
    begin
      PrintUsage;
      Halt(EXIT_OK);
    end
    else
      FileName := Arg;
  end;

  { Proves the finding path end to end without needing a live engine bug:
    the fault is raised where an engine fault would be, so the classification,
    the backtrace, and the nonzero exit are all exercised. Regression-tested
    in tests/fuzz-harness.test.js. }
  if SelfTestFault then
  begin
    try
      raise Exception.Create('injected self-test fault');
    except
      on E: Exception do
      begin
        WriteLn(ErrOutput, Format(
          '%s: unexpected fault in %s executor: %s: %s',
          [FUZZ_PROGRAM_NAME, FUZZ_EXECUTOR_NAME, E.ClassName,
           E.Message]));
        WriteLn(ErrOutput, BackTraceStrFunc(ExceptAddr) + LineEnding +
          CaptureBackTrace);
      end;
    end;
    Halt(1);
  end;

  if SeedDirectory <> '' then
  begin
    WriteLn(Format('%d heap snapshot seeds written to %s',
      [WriteHeapSnapshotSeeds(SeedDirectory), SeedDirectory]));
    Halt(EXIT_OK);
  end;

  if HeapSnapshot then
  begin
    try
      Outcome := RestoreOnce(FileName, Detail);
    except
      on E: Exception do
      begin
        Outcome := foUnexpected;
        Detail := E.ClassName + ': ' + E.Message;
        Trace := BackTraceStrFunc(ExceptAddr) + LineEnding +
          CaptureBackTrace;
      end;
    end;
    if Outcome = foUnexpected then
    begin
      WriteLn(ErrOutput, Format('%s: unexpected fault in the heap snapshot ' +
        'reader: %s', [FUZZ_PROGRAM_NAME, Detail]));
      WriteLn(ErrOutput, Trace);
      Halt(1);
    end;
    Report(Outcome, Detail);
    Halt(EXIT_OK);
  end;

  if (FileName = '') and IsInputTerminal then
  begin
    PrintUsage;
    Halt(EXIT_USAGE);
  end;

  if not ReadInput(FileName, Source, Detail) then
  begin
    Report(foInputRejected, Detail);
    Halt(EXIT_OK);
  end;

  try
    try
      Outcome := RunOnce(Source, Detail);
    except
      { The finding path. Anything that escapes RunOnce's typed ladder is
        by definition not an outcome the engine models. Keep the message
        on stderr and the reproduction in the fuzzer's crash directory.
        The backtrace is captured here, at the raise site, because the
        handler below unwinds past it. Dev builds compile with -gl/-gw so
        the frames carry file and line. }
      on E: Exception do
      begin
        Outcome := foUnexpected;
        Detail := E.ClassName + ': ' + E.Message;
        Trace := BackTraceStrFunc(ExceptAddr) + LineEnding +
          CaptureBackTrace;
      end;
    end;

    if Outcome = foUnexpected then
    begin
      WriteLn(ErrOutput, Format('%s: unexpected fault in %s executor: %s',
        [FUZZ_PROGRAM_NAME, FUZZ_EXECUTOR_NAME, Detail]));
      WriteLn(ErrOutput, Trace);
      Failed := True;
    end
    else
      Report(Outcome, Detail);
  finally
    Source.Free;
  end;

  if Failed then
    ExitCode := 1
  else
    ExitCode := EXIT_OK;
end.
