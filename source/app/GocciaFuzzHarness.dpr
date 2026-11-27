program GocciaFuzzHarness;

{ GocciaFuzzHarness: a coverage-blind crash harness for the engine's parser,
  compiler, bytecode tiers and JIT, and for its heap snapshot reader.

  Reads one source input from a file argument or stdin and runs it twice,
  each time on an engine of its own:

    [bytecode]  under the engine's limits (timeout, instruction budget,
                frame limit, memory ceiling). Armed limits keep the JIT
                off, so this run fuzzes the interpreter and the limits.
    [jit]       with no engine limits, so the baseline JIT and the
                optimizing tier compile what gets hot (GOCCIA_JIT_HOT lowers
                the threshold). The memory ceiling still holds; a watchdog
                thread ends the process with exit 0 at the deadline, since
                JIT code does not poll the engine's timeout.

  Every limit, every uncaught JavaScript throw and every parse error is an
  outcome the engine models, and exits 0. Only what it does not model (a
  Pascal exception escaping the engine, an access violation, a heap
  corruption abort) reaches the fuzzer as a nonzero exit or a crash.

  A module import never reaches the host's files: the run's resolver refuses
  every specifier before it probes a path, the run's file reader (Goccia.Modules
  SetFileReadHook) refuses every path as well, and a run that asked for
  either reports module-denied. Without capabilities the engine also refuses fetch()
  and leaves FFI out.

  With --grammar the input is not source text but the choices of a program
  generator (Goccia.Fuzz.Generator): every input is a program that parses,
  and it prints the same lines in every run, so the harness also compares
  the two runs. When both end as the program does (completed, or an
  uncaught throw) and neither reached the frame limit, a difference in what
  they printed or in how they ended is a finding: the JIT computed
  something the interpreter did not.

  Usage:
    GocciaFuzzHarness <file>          # AFL++ / manual reproduction
    GocciaFuzzHarness --grammar <f>   # <f>'s bytes choose a program
    GocciaFuzzHarness --grammar --print-program <f>
                                      # print the program <f> chooses
    GocciaFuzzHarness -               # read stdin
    GocciaFuzzHarness --verbose <f>   # print each run's outcome
    GocciaFuzzHarness --heap-snapshot <file>
                                      # restore <file> as a heap snapshot
    GocciaFuzzHarness --write-heap-snapshot-seeds <dir>
                                      # write real snapshots to seed with

  A heap snapshot names the engine build that wrote it and the code of the
  entry its closures were compiled from, so the seeds and the restore run
  one program (HEAP_SNAPSHOT_PROGRAM): in a seed run it hands each seed
  graph to capture(i, v), which writes it; in a restore run restored()
  reads the input, and the program then uses what restored, calling its
  functions. The engine's writer has no codec for a generator's frame, a Map's
  table or a buffer's bytes (Goccia.HeapSnapshot), so the seeds hold data
  graphs, closures, bound functions, errors and wrapper objects. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads, BaseUnix,{$ENDIF}
  Classes,
  SysUtils,

  TextEncoding,

  Goccia.CLI.Stdin,
  Goccia.Fuzz.Generator,
  Goccia.ModuleResolver,

  Goccia.Core,
  Goccia.Embed,
  Goccia.HeapSnapshot,
  Goccia.Modules,
  Goccia.VM;

const
  FUZZ_PROGRAM_NAME = 'GocciaFuzzHarness';

  { Bounds are deliberately tight. A fuzzer generates far more slow inputs
    than interesting ones, and every second spent on a runaway loop is a
    second not spent exploring. }
  FUZZ_TIMEOUT_MS = 1000;
  FUZZ_MAX_INSTRUCTIONS = 5000000;
  FUZZ_MAX_MEMORY_BYTES = Int64(256) * 1024 * 1024;
  FUZZ_MAX_FRAMES = 256;

  { A multi-megabyte input is not a useful discovery, so oversized inputs
    are rejected before the engine sees them. }
  FUZZ_MAX_INPUT_BYTES = 1024 * 1024;

  EXIT_OK = 0;
  EXIT_USAGE = 64;

  { The program both heap snapshot modes run. fuzzMode() is 0 in a seed run,
    which hands each seed graph to capture(i, v) after the closure that
    built it has returned (a binding a running frame holds has no codec),
    and 1 in a restore run, which uses what restored() gives: its functions
    called, accessors read, everything it reaches walked under a budget.
    exerciseMode() picks how a function is called, from the input's size,
    so each way is fuzzed. }
  HEAP_SNAPSHOT_PROGRAM =
    'if (fuzzMode() === 0) {' + LineEnding +
    '  capture(0, { n: 1.5, s: "text", b: 12345678901234567890n, t: true, ' +
    'u: undefined, z: null, neg: -0 });' + LineEnding +
    '  capture(1, (() => { const a = [1, , "three", [4]]; a.length = 8; ' +
    'a.extra = 1; return a; })());' + LineEnding +
    '  capture(2, (() => { const shared = { s: 1 }; ' +
    'const o = { x: shared, y: [shared] }; o.self = o; return o; })());' +
    LineEnding +
    '  capture(3, { [Symbol("local")]: 1, [Symbol.for("shared")]: 2, ' +
    '[Symbol.iterator]: Array.prototype.values, m: Math, ' +
    'p: Object.prototype });' + LineEnding +
    '  capture(4, (() => { const o = {}; ' +
    'Object.defineProperty(o, "h", { value: 1 }); ' +
    'Object.defineProperty(o, "g", { get: Object.prototype.valueOf, ' +
    'enumerable: true }); return [Object.freeze({ f: 1 }), ' +
    'Object.seal([1]), Object.preventExtensions(o), ' +
    'Object.create(null)]; })());' + LineEnding +
    '  capture(5, Array.from({ length: 64 }, (_, i) => ' +
    '({ i, next: { s: String(i) } })));' + LineEnding +
    '  capture(6, (() => { let n = 0; const inc = () => { n += 1; ' +
    'return n; }; const source = { m(x) { return super.toString === ' +
    'Object.prototype.toString ? x + n : -1; }, get count() { return n; }, ' +
    'set count(v) { n = v; } }; inc(); return { inc, source, ' +
    'bound: source.m.bind(source, 3), ' +
    'chain: Array.from({ length: 16 }, (_, i) => i).reduce((prev, i) => ' +
    '{ const f = () => prev; f.i = i; return f; }, null) }; })());' +
    LineEnding +
    '  capture(7, (() => { const e = new TypeError("seed", { cause: 1 }); ' +
    'e.extra = [e]; return [e, Object(1), Object("s"), Object(true), ' +
    'Object(Symbol("w"))]; ' +
    '})());' + LineEnding +
    '} else {' + LineEnding +
    '  const root = restored();' + LineEnding +
    '  if (root !== undefined) {' + LineEnding +
    '    const how = exerciseMode();' + LineEnding +
    '    const seen = new Set();' + LineEnding +
    '    let budget = 4096;' + LineEnding +
    '    const use = (v, depth) => {' + LineEnding +
    '      if (budget <= 0 || depth > 12 || v === null || ' +
    '(typeof v !== "object" && typeof v !== "function") || seen.has(v)) ' +
    'return;' + LineEnding +
    '      budget -= 1;' + LineEnding +
    '      seen.add(v);' + LineEnding +
    '      if (typeof v === "function") {' + LineEnding +
    '        try {' + LineEnding +
    '          const r = how === 0 ? v(1, "two") : how === 1 ? ' +
    'v.call(root, root) : new v(v);' + LineEnding +
    '          use(r, depth + 1);' + LineEnding +
    '        } catch (e) { use(e, depth + 1); }' + LineEnding +
    '      }' + LineEnding +
    '      let keys = [];' + LineEnding +
    '      try { keys = Reflect.ownKeys(v); } catch (e) {}' + LineEnding +
    '      for (const k of keys) {' + LineEnding +
    '        try {' + LineEnding +
    '          const d = Object.getOwnPropertyDescriptor(v, k);' + LineEnding +
    '          if (d === undefined) continue;' + LineEnding +
    '          if ("value" in d) use(d.value, depth + 1);' + LineEnding +
    '          else { use(d.get, depth + 1); use(d.set, depth + 1); ' +
    'use(v[k], depth + 1); }' + LineEnding +
    '        } catch (e) { use(e, depth + 1); }' + LineEnding +
    '      }' + LineEnding +
    '      try { use(Object.getPrototypeOf(v), depth + 1); } catch (e) {}' +
    LineEnding +
    '    };' + LineEnding +
    '    use(root, 0);' + LineEnding +
    '    try { JSON.stringify(root); } catch (e) {}' + LineEnding +
    '    try { String(root); } catch (e) {}' + LineEnding +
    '  }' + LineEnding +
    '}' + LineEnding;
  HEAP_SNAPSHOT_SEEDS = 8;
  HEAP_SNAPSHOT_FILE = 'heap-snapshot-seeds.js';

type
  { How a single run ended. Everything except foUnexpected is a bounded,
    engine-modelled outcome and must not be reported to the fuzzer. A frame
    limit is a RangeError the script can catch, so it lands in
    foScriptThrow. }
  TFuzzOutcome = (
    foCompleted,
    foScriptThrow,
    foParseError,
    foInstructionLimit,
    foTimeout,
    foMemoryLimit,
    foHostAbort,
    foModuleDenied,
    foInputRejected,
    foSnapshotRefused,
    foUnexpected
  );

  { What a run's console writes is dropped: the fuzzer reads only the exit
    status, and printing costs time. A grammar run keeps it, to compare. }
  TFuzzConsole = class
  public
    procedure Discard(const AMethod, ALine: UnicodeString);
  end;

  { Refuses every import before any path is probed, so an input learns
    nothing about the host's files, not even whether one exists. The engine turns
    the exception into the error the import fails with. }
  TFuzzDeniedResolver = class(TModuleResolver)
  public
    function Resolve(const AModulePath,
      AImportingFilePath: string): string; override;
  end;

  { Ends the process with exit 0 when a run passes its deadline, for the run
    whose engine arms no timeout. }
  TFuzzWatchdog = class(TThread)
  private
    FWake: PRTLEvent;
    FDisarmed: LongInt;
    FMilliseconds: Integer;
    FExecutor: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AMilliseconds: Integer; const AExecutor: string);
    destructor Destroy; override;
    procedure Disarm;
  end;

var
  GVerbose: Boolean = False;
  GConsole: TFuzzConsole;
  { a grammar run's console lines (nil: dropped) }
  GCapture: TStringList;
  GResolver: TFuzzDeniedResolver;
  { the first path a run's import asked for, refused ('' none) }
  GDeniedPath: UnicodeString;
  { heap snapshot mode: 0 seeds, 1 restore; the seed directory and how many
    seeds were written; the input a restore reads, and its refusal }
  GSnapshotMode: Integer;
  GSeedDirectory: string;
  GSeedsWritten: Integer;
  GSeedFailure: string;
  GSnapshotInput: TBytes;
  GSnapshotRefused: Boolean;
  GSnapshotRefusal: string;
  GSetupError: UnicodeString;

procedure TFuzzConsole.Discard(const AMethod, ALine: UnicodeString);
begin
  if Assigned(GCapture) then
    GCapture.Add(UTF8Encode(AMethod + ' ' + ALine));
end;

function TFuzzDeniedResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
begin
  if GDeniedPath = '' then
    GDeniedPath := UTF8Decode(AModulePath);
  raise Exception.Create('module loading is disabled: ' + AModulePath);
end;

function OutcomeName(const AOutcome: TFuzzOutcome): string;
begin
  case AOutcome of
    foCompleted:        Result := 'completed';
    foScriptThrow:      Result := 'script-throw';
    foParseError:       Result := 'parse-error';
    foInstructionLimit: Result := 'instruction-limit';
    foTimeout:          Result := 'timeout';
    foMemoryLimit:      Result := 'memory-limit';
    foHostAbort:        Result := 'host-abort';
    foModuleDenied:     Result := 'module-denied';
    foInputRejected:    Result := 'input-rejected';
    foSnapshotRefused:  Result := 'snapshot-refused';
  else
    Result := 'UNEXPECTED';
  end;
end;

procedure Report(const AExecutor: string; const AOutcome: TFuzzOutcome;
  const ADetail: string);
begin
  if not GVerbose then
    Exit;
  if ADetail = '' then
    WriteLn(ErrOutput, Format('[%s] %s', [AExecutor, OutcomeName(AOutcome)]))
  else
    WriteLn(ErrOutput, Format('[%s] %s: %s', [AExecutor,
      OutcomeName(AOutcome), ADetail]));
  Flush(ErrOutput);
end;

constructor TFuzzWatchdog.Create(AMilliseconds: Integer;
  const AExecutor: string);
begin
  FWake := RTLEventCreate;
  FMilliseconds := AMilliseconds;
  FExecutor := AExecutor;
  FDisarmed := 0;
  inherited Create(False);
end;

destructor TFuzzWatchdog.Destroy;
begin
  inherited Destroy;
  RTLEventDestroy(FWake);
end;

{$IFNDEF UNIX}
procedure ExitProcessNow(Code: LongWord); stdcall; external 'kernel32' name 'ExitProcess';
{$ENDIF}

procedure TFuzzWatchdog.Execute;
begin
  RTLEventWaitFor(FWake, FMilliseconds);
  { the run finished first when Disarm got here before the deadline }
  if InterlockedCompareExchange(FDisarmed, 2, 0) <> 0 then
    Exit;
  Report(FExecutor, foTimeout, Format('the run passed %d ms',
    [FMilliseconds]));
  { The engine is still running on the main thread: leave without unwinding
    it or running finalization. }
  {$IFDEF UNIX}
  FpExit(EXIT_OK);
  {$ELSE}
  ExitProcessNow(EXIT_OK);
  {$ENDIF}
end;

procedure TFuzzWatchdog.Disarm;
begin
  { once the watchdog fired it is exiting the process: wait for that }
  if InterlockedCompareExchange(FDisarmed, 1, 0) <> 0 then
    while True do
      Sleep(1000);
  RTLEventSetEvent(FWake);
  { WaitFor on the main thread polls in 100 ms steps until Finished: wait
    for that first, so the join is immediate }
  while not Finished do
    ThreadSwitch;
  WaitFor;
end;

{ The run's file reader: refuses every path, before any file is opened, and
  remembers the first one. }
function DenyFileRead(const APath: UnicodeString;
  var ABytes: RawByteString): Boolean;
begin
  ABytes := '';
  if GDeniedPath = '' then
    GDeniedPath := APath;
  Result := False;
end;

function DenyFileTime(const APath: UnicodeString;
  out AModified: TDateTime): Boolean;
begin
  AModified := 0;
  Result := False;
end;

{ Every opt-in dialect switch, as the old harness turned them all on: the
  default dialect rejects `var`, `while`, traditional `for`, labels and
  more, which would wall the fuzzer off from most of the grammar. }
function FuzzProfile: TLangProfile;
begin
  Result := RecommendedProfile;
  Result.Asi := True;
  Result.VarDecl := True;
  Result.FuncDecl := True;
  Result.ForLoop := True;
  Result.WhileLoop := True;
  Result.LooseEq := True;
  Result.NonStrict := True;
  Result.ArgumentsObject := True;
  Result.Labels := True;
  Result.ForIn := True;
end;

function NewFuzzEngine(ABounded: Boolean): TGocciaEngine;
begin
  Result := TGocciaEngine.Create;
  Result.Profile := FuzzProfile;
  Result.OnConsole := GConsole.Discard;
  Result.Resolver := GResolver;
  Result.MaxMemory := FUZZ_MAX_MEMORY_BYTES;
  if ABounded then
  begin
    Result.TimeoutMs := FUZZ_TIMEOUT_MS;
    Result.MaxInstructions := FUZZ_MAX_INSTRUCTIONS;
    Result.MaxFrames := FUZZ_MAX_FRAMES;
  end;
end;

{ A run's result as an outcome. A denied import wins: the input reached for
  the host's files, whatever it then did with the refusal. }
function Classify(const AResult: TGocciaRunResult;
  out ADetail: string): TFuzzOutcome;
begin
  ADetail := UTF8Encode(AResult.ErrorMessage);
  if GDeniedPath <> '' then
  begin
    ADetail := UTF8Encode(GDeniedPath);
    Exit(foModuleDenied);
  end;
  if not AResult.Compiled then
  begin
    { a parse error names its error; a host that could not be set up does
      not, and is not something an input can cause }
    if AResult.ErrorIsError then
      Exit(foParseError);
    ADetail := 'the engine could not be set up: ' + ADetail;
    Exit(foUnexpected);
  end;
  case AResult.Aborted of
    gaTimeout:          Exit(foTimeout);
    gaInstructionLimit: Exit(foInstructionLimit);
    gaMemoryLimit:      Exit(foMemoryLimit);
    gaHostAbort:        Exit(foHostAbort);
  end;
  if AResult.Ok then
    Result := foCompleted
  else
    Result := foScriptThrow;
end;

{ Runs ASource on an engine of its own. Unbounded runs have the watchdog in
  place of the engine's timeout. }
function RunOnce(const ASource: UnicodeString; const AFileName: string;
  ABounded: Boolean; const AExecutor: string;
  out ADetail: string): TFuzzOutcome;
var
  Engine: TGocciaEngine;
  Watchdog: TFuzzWatchdog;
  RunResult: TGocciaRunResult;
begin
  GDeniedPath := '';
  Watchdog := nil;
  Engine := NewFuzzEngine(ABounded);
  try
    SetFileReadHook(DenyFileRead);
    SetFileTimeHook(DenyFileTime);
    try
      if not ABounded then
        Watchdog := TFuzzWatchdog.Create(FUZZ_TIMEOUT_MS, AExecutor);
      try
        RunResult := Engine.Run(AFileName, ASource);
      finally
        if Assigned(Watchdog) then
        begin
          Watchdog.Disarm;
          Watchdog.Free;
        end;
      end;
    finally
      SetFileReadHook(nil);
      SetFileTimeHook(nil);
    end;
    Result := Classify(RunResult, ADetail);
  finally
    Engine.Free;
  end;
end;

{ Reads stdin but stops the instant the FUZZ_MAX_INPUT_BYTES cap is breached,
  so a fuzzer piping a multi-gigabyte input through `-` is rejected without
  the harness ever holding it in memory. }
function ReadBoundedStdin(out ABytes: TBytes; out ADetail: string): Boolean;
const
  STDIN_CHUNK_BYTES = 65536;
var
  Stream: THandleStream;
  Chunk: TBytes;
  Total, BytesRead: Integer;
begin
  ADetail := '';
  SetLength(ABytes, 0);
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
        SetLength(ABytes, Total + BytesRead);
        Move(Chunk[0], ABytes[Total], BytesRead);
        Inc(Total, BytesRead);
      end;
    until BytesRead <= 0;
  finally
    Stream.Free;
  end;
  Result := True;
end;

{ Reads a file whose on-disk length is checked against the cap before any of
  it is read, from the handle that was checked, so the path cannot be
  swapped for a larger file in between. }
function ReadBoundedFile(const AFileName: string; out ABytes: TBytes;
  out ADetail: string): Boolean;
var
  Input: TFileStream;
  Size: Int64;
begin
  ADetail := '';
  SetLength(ABytes, 0);
  if not FileExists(AFileName) then
  begin
    ADetail := 'no such input file: ' + AFileName;
    Exit(False);
  end;
  Input := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Size := Input.Size;
    if Size > FUZZ_MAX_INPUT_BYTES then
    begin
      ADetail := Format('input exceeds %d bytes', [FUZZ_MAX_INPUT_BYTES]);
      Exit(False);
    end;
    SetLength(ABytes, Size);
    if Size > 0 then
      Input.ReadBuffer(ABytes[0], Size);
  finally
    Input.Free;
  end;
  Result := True;
end;

{ The input as source text: valid UTF-8 within the cap. Unreadable or
  undecodable bytes are an input-selection problem, not an engine finding. }
function ReadInput(const AFileName: string; out ASource: UnicodeString;
  out ADetail: string): Boolean;
var
  Bytes: TBytes;
  Text: string;
  ErrorOffset: Integer;
begin
  ASource := '';
  try
    if (AFileName = '') or (AFileName = '-') then
      Result := ReadBoundedStdin(Bytes, ADetail)
    else
      Result := ReadBoundedFile(AFileName, Bytes, ADetail);
    if not Result then
      Exit;
    if not TryDecodeUTF8(Bytes, Text, ErrorOffset) then
    begin
      ADetail := Format('invalid UTF-8 at byte %d', [ErrorOffset]);
      Exit(False);
    end;
  except
    on E: Exception do
    begin
      ADetail := E.Message;
      Exit(False);
    end;
  end;
  ASource := Text;
  Result := True;
end;

{ ---- heap snapshots ---- }

{ fuzzMode(): 0 in a seed run, 1 in a restore run }
function Native_FuzzMode(AArgs: PValue; AArgc: Integer;
  var ARes: TValue): Boolean;
begin
  ARes := MkInt(GSnapshotMode);
  Result := True;
end;

{ exerciseMode(): how a restore run calls what restored, from the input's
  size }
function Native_ExerciseMode(AArgs: PValue; AArgc: Integer;
  var ARes: TValue): Boolean;
begin
  ARes := MkInt(Length(GSnapshotInput) mod 3);
  Result := True;
end;

{ capture(i, v): in a seed run, v written as seed i }
function Native_Capture(AArgs: PValue; AArgc: Integer;
  var ARes: TValue): Boolean;
var
  Index: Double;
  Data: TBytes;
  Refusal: UnicodeString;
  Output: TFileStream;
begin
  ARes := VUndefined;
  Result := True;
  if (GSnapshotMode <> 0) or (AArgc < 2) or not IsNumber(AArgs[1]) then
    Exit;
  Index := NumOf(AArgs[1]);
  if not ((Index >= 0) and (Index < HEAP_SNAPSHOT_SEEDS) and
    (Frac(Index) = 0)) then
    Exit;
  Refusal := '';
  if not SnapshotWrite(AArgs[2], Data, Refusal) then
  begin
    if GSeedFailure = '' then
      GSeedFailure := Format('seed %d: %s', [Trunc(Index),
        UTF8Encode(Refusal)]);
    Exit;
  end;
  Output := TFileStream.Create(IncludeTrailingPathDelimiter(GSeedDirectory) +
    Format('heap-snapshot-%d.ghsn', [Trunc(Index)]), fmCreate);
  try
    if Length(Data) > 0 then
      Output.WriteBuffer(Data[0], Length(Data));
  finally
    Output.Free;
  end;
  Inc(GSeedsWritten);
end;

{ restored(): in a restore run, the input restored; undefined when the
  reader refuses it }
function Native_Restored(AArgs: PValue; AArgc: Integer;
  var ARes: TValue): Boolean;
var
  Refusal: UnicodeString;
  Root: TValue;
begin
  ARes := VUndefined;
  Result := True;
  if GSnapshotMode <> 1 then
    Exit;
  Refusal := '';
  Root := VUndefined;
  if not SnapshotRestore(GSnapshotInput, Root, Refusal) then
  begin
    GSnapshotRefused := True;
    GSnapshotRefusal := UTF8Encode(Refusal);
    Exit;
  end;
  ARes := Root;
end;

{ An engine with the heap snapshot program's natives, run on it, bounded as
  a script run is. Both modes set it up the same way, so a seed run's
  built-ins and code are the ones the restore run names. }
function RunSnapshotProgram(AMode: Integer; out ADetail: string): TFuzzOutcome;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Err: UnicodeString;
begin
  GSnapshotMode := AMode;
  GSnapshotRefused := False;
  GSnapshotRefusal := '';
  GDeniedPath := '';
  Engine := NewFuzzEngine(True);
  try
    Engine.HeapSnapshots := True;
    Err := '';
    if not Engine.PrepareHost(Err) then
    begin
      ADetail := 'the engine could not be set up: ' + UTF8Encode(Err);
      Exit(foUnexpected);
    end;
    DefineNative(GlobalObj, 'fuzzMode', Native_FuzzMode, 0);
    DefineNative(GlobalObj, 'exerciseMode', Native_ExerciseMode, 0);
    DefineNative(GlobalObj, 'capture', Native_Capture, 2);
    DefineNative(GlobalObj, 'restored', Native_Restored, 0);
    SetFileReadHook(DenyFileRead);
    SetFileTimeHook(DenyFileTime);
    try
      RunResult := Engine.Run(ExpandFileName(HEAP_SNAPSHOT_FILE),
        HEAP_SNAPSHOT_PROGRAM);
    finally
      SetFileReadHook(nil);
      SetFileTimeHook(nil);
    end;
    Result := Classify(RunResult, ADetail);
    if GSnapshotRefused then
    begin
      Result := foSnapshotRefused;
      ADetail := GSnapshotRefusal;
    end;
  finally
    Engine.Free;
  end;
end;

{ Writes the seeds into ADirectory: the number written, or -1 when a seed
  could not be written (ADetail says why). }
function WriteHeapSnapshotSeeds(const ADirectory: string;
  out ADetail: string): Integer;
var
  Outcome: TFuzzOutcome;
begin
  ForceDirectories(ADirectory);
  GSeedDirectory := ADirectory;
  GSeedsWritten := 0;
  GSeedFailure := '';
  Outcome := RunSnapshotProgram(0, ADetail);
  if Outcome <> foCompleted then
  begin
    ADetail := OutcomeName(Outcome) + ': ' + ADetail;
    Exit(-1);
  end;
  if (GSeedFailure <> '') or (GSeedsWritten <> HEAP_SNAPSHOT_SEEDS) then
  begin
    ADetail := Format('%d of %d seeds written; %s',
      [GSeedsWritten, HEAP_SNAPSHOT_SEEDS, GSeedFailure]);
    Exit(-1);
  end;
  Result := GSeedsWritten;
end;

function RestoreOnce(const AFileName: string;
  out ADetail: string): TFuzzOutcome;
begin
  if not ReadBoundedFile(AFileName, GSnapshotInput, ADetail) then
    Exit(foInputRejected);
  Result := RunSnapshotProgram(1, ADetail);
end;

{ ---- the invocation ---- }

{ Renders the frames of the exception currently being handled. }
function CaptureBackTrace: string;
var
  Frames: PPointer;
  I: Integer;
begin
  Result := BackTraceStrFunc(ExceptAddr) + LineEnding;
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Result := Result + BackTraceStrFunc(Frames[I]) + LineEnding;
end;

procedure ReportFault(const AWhere, ADetail, ATrace: string);
begin
  WriteLn(ErrOutput, Format('%s: unexpected fault in %s: %s',
    [FUZZ_PROGRAM_NAME, AWhere, ADetail]));
  if ATrace <> '' then
    WriteLn(ErrOutput, ATrace);
end;

procedure PrintUsage;
begin
  WriteLn(ErrOutput, 'Usage: ', FUZZ_PROGRAM_NAME, ' [--verbose] <file>|-');
  WriteLn(ErrOutput, '       ', FUZZ_PROGRAM_NAME,
    ' [--verbose] --heap-snapshot <file>');
  WriteLn(ErrOutput, '       ', FUZZ_PROGRAM_NAME,
    ' --write-heap-snapshot-seeds <dir>');
  WriteLn(ErrOutput, '       ', FUZZ_PROGRAM_NAME,
    ' [--verbose] --grammar [--print-program] <file>|-');
  WriteLn(ErrOutput);
  WriteLn(ErrOutput, 'Runs one input under the engine''s limits, then with ' +
    'the JIT on, each on an engine of its own.');
  WriteLn(ErrOutput, 'Exits 0 for every engine-modelled outcome; nonzero ' +
    'only for an unexpected fault.');
end;

{ One executor's run of the input, a fault reported. True when it faulted. }
function RunExecutor(const ASource: UnicodeString; const AFileName: string;
  ABounded: Boolean; const AExecutor: string;
  out AOutcome: TFuzzOutcome): Boolean;
var
  Detail, Trace: string;
begin
  Trace := '';
  try
    AOutcome := RunOnce(ASource, AFileName, ABounded, AExecutor, Detail);
  except
    { anything that escapes the engine is by definition not an outcome it
      models; the backtrace is captured here, before the handler unwinds }
    on E: Exception do
    begin
      AOutcome := foUnexpected;
      Detail := E.ClassName + ': ' + E.Message;
      Trace := CaptureBackTrace;
    end;
  end;
  Result := AOutcome = foUnexpected;
  if Result then
    ReportFault('the ' + AExecutor + ' run', Detail, Trace)
  else
    Report(AExecutor, AOutcome, Detail);
end;

{ RunExecutor, with the run's detail. True when it faulted. }
function RunGrammarExecutor(const ASource: UnicodeString;
  const AFileName: string; ABounded: Boolean; const AExecutor: string;
  out AOutcome: TFuzzOutcome; out ADetail: string): Boolean;
var
  Trace: string;
begin
  Trace := '';
  ADetail := '';
  try
    AOutcome := RunOnce(ASource, AFileName, ABounded, AExecutor, ADetail);
  except
    on E: Exception do
    begin
      AOutcome := foUnexpected;
      ADetail := E.ClassName + ': ' + E.Message;
      Trace := CaptureBackTrace;
    end;
  end;
  Result := AOutcome = foUnexpected;
  if Result then
    ReportFault('the ' + AExecutor + ' run', ADetail, Trace)
  else
    Report(AExecutor, AOutcome, ADetail);
end;

{ ---- grammar runs ---- }

const
  FRAME_LIMIT_MESSAGE = 'Maximum call stack size exceeded';

{ A run that reached the frame limit: the interpreter run's limit is
  FUZZ_MAX_FRAMES and the JIT run's the engine's own, so where recursion
  stops differs by design. }
function ReachedFrameLimit(const ALines: TStringList;
  const ADetail: string): Boolean;
var
  I: Integer;
begin
  if Pos(FRAME_LIMIT_MESSAGE, ADetail) > 0 then
    Exit(True);
  for I := 0 to ALines.Count - 1 do
    if Pos(FRAME_LIMIT_MESSAGE, ALines[I]) > 0 then
      Exit(True);
  Result := False;
end;

{ Compares the two runs of a generated program: '' when they agree or are
  not comparable, else what differs. }
function CompareRuns(AFirst, ASecond: TFuzzOutcome;
  const AFirstDetail, ASecondDetail: string;
  const AFirstLines, ASecondLines: TStringList): string;
var
  I: Integer;
begin
  Result := '';
  if not (AFirst in [foCompleted, foScriptThrow]) or
    not (ASecond in [foCompleted, foScriptThrow]) then
    Exit;
  if ReachedFrameLimit(AFirstLines, AFirstDetail) or
    ReachedFrameLimit(ASecondLines, ASecondDetail) then
    Exit;
  for I := 0 to AFirstLines.Count - 1 do
  begin
    if I >= ASecondLines.Count then
      Exit(Format('line %d: the bytecode run printed %s, the jit run ' +
        'nothing more', [I + 1, AFirstLines[I]]));
    if AFirstLines[I] <> ASecondLines[I] then
      Exit(Format('line %d: the bytecode run printed %s, the jit run %s',
        [I + 1, AFirstLines[I], ASecondLines[I]]));
  end;
  if ASecondLines.Count > AFirstLines.Count then
    Exit(Format('line %d: the jit run printed %s, the bytecode run ' +
      'nothing more', [AFirstLines.Count + 1,
      ASecondLines[AFirstLines.Count]]));
  if AFirst <> ASecond then
    Exit(Format('the bytecode run ended %s (%s), the jit run %s (%s)',
      [OutcomeName(AFirst), AFirstDetail, OutcomeName(ASecond),
      ASecondDetail]));
  if (AFirst = foScriptThrow) and (AFirstDetail <> ASecondDetail) then
    Exit(Format('the bytecode run threw %s, the jit run %s',
      [AFirstDetail, ASecondDetail]));
end;

{ Runs the program AData chooses in both executors and compares them.
  The exit code: 0, or 1 for a fault or a difference. }
function RunGrammar(const AData: TBytes; const ARunName: string): Integer;
var
  Source: UnicodeString;
  BytecodeLines, JitLines: TStringList;
  BytecodeOutcome, JitOutcome: TFuzzOutcome;
  BytecodeDetail, JitDetail, Difference: string;
begin
  Source := GenerateFuzzProgram(AData);
  BytecodeLines := TStringList.Create;
  JitLines := TStringList.Create;
  try
    GCapture := BytecodeLines;
    try
      if RunGrammarExecutor(Source, ARunName, True, 'bytecode',
        BytecodeOutcome, BytecodeDetail) then
        Exit(1);
      GCapture := JitLines;
      if RunGrammarExecutor(Source, ARunName, False, 'jit', JitOutcome,
        JitDetail) then
        Exit(1);
    finally
      GCapture := nil;
    end;
    Difference := CompareRuns(BytecodeOutcome, JitOutcome, BytecodeDetail,
      JitDetail, BytecodeLines, JitLines);
    if Difference <> '' then
    begin
      WriteLn(ErrOutput, Format('%s: the runs differ: %s',
        [FUZZ_PROGRAM_NAME, Difference]));
      Exit(1);
    end;
    if GVerbose then
      WriteLn(ErrOutput, Format('[grammar] %d lines; %s',
        [BytecodeLines.Count, 'the runs agree or are not comparable']));
    Result := EXIT_OK;
  finally
    JitLines.Free;
    BytecodeLines.Free;
  end;
end;

procedure RunHarness;
var
  I: Integer;
  Arg, FileName, RunName, Detail, Trace, SeedDirectory: string;
  Source: UnicodeString;
  Outcome: TFuzzOutcome;
  SelfTestFault, HeapSnapshot, Grammar, PrintProgram, Readable: Boolean;
  Written: Integer;
  Bytes: TBytes;
begin
  FileName := '';
  SelfTestFault := False;
  HeapSnapshot := False;
  Grammar := False;
  PrintProgram := False;
  SeedDirectory := '';
  I := 1;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);
    if (Arg = '--verbose') or (Arg = '-v') then
      GVerbose := True
    else if Arg = '--self-test-fault' then
      SelfTestFault := True
    else if Arg = '--heap-snapshot' then
      HeapSnapshot := True
    else if Arg = '--grammar' then
      Grammar := True
    else if Arg = '--print-program' then
      PrintProgram := True
    else if Arg = '--write-heap-snapshot-seeds' then
    begin
      if I < ParamCount then
      begin
        Inc(I);
        SeedDirectory := ParamStr(I);
      end;
    end
    else if (Arg = '--help') or (Arg = '-h') then
    begin
      PrintUsage;
      ExitCode := EXIT_OK;
      Exit;
    end
    else
      FileName := Arg;
    Inc(I);
  end;

  { Proves the finding path end to end without a live engine bug: the fault
    is raised where an engine fault would be, so its report, backtrace and
    nonzero exit are all exercised. }
  if SelfTestFault then
  begin
    try
      raise Exception.Create('injected self-test fault');
    except
      on E: Exception do
        ReportFault('the bytecode run', E.ClassName + ': ' + E.Message,
          CaptureBackTrace);
    end;
    ExitCode := 1;
    Exit;
  end;

  if SeedDirectory <> '' then
  begin
    Written := WriteHeapSnapshotSeeds(SeedDirectory, Detail);
    if Written < 0 then
    begin
      WriteLn(ErrOutput, FUZZ_PROGRAM_NAME,
        ': the heap snapshot seeds could not be written: ', Detail);
      ExitCode := 1;
      Exit;
    end;
    WriteLn(Format('%d heap snapshot seeds written to %s',
      [Written, SeedDirectory]));
    ExitCode := EXIT_OK;
    Exit;
  end;

  if HeapSnapshot then
  begin
    Trace := '';
    try
      Outcome := RestoreOnce(FileName, Detail);
    except
      on E: Exception do
      begin
        Outcome := foUnexpected;
        Detail := E.ClassName + ': ' + E.Message;
        Trace := CaptureBackTrace;
      end;
    end;
    if Outcome = foUnexpected then
    begin
      ReportFault('the heap snapshot reader', Detail, Trace);
      ExitCode := 1;
      Exit;
    end;
    Report('heap-snapshot', Outcome, Detail);
    ExitCode := EXIT_OK;
    Exit;
  end;

  if (FileName = '') and IsInputTerminal then
  begin
    PrintUsage;
    ExitCode := EXIT_USAGE;
    Exit;
  end;

  if Grammar then
  begin
    if (FileName = '') or (FileName = '-') then
      Readable := ReadBoundedStdin(Bytes, Detail)
    else
      Readable := ReadBoundedFile(FileName, Bytes, Detail);
    if not Readable then
    begin
      Report('grammar', foInputRejected, Detail);
      ExitCode := EXIT_OK;
      Exit;
    end;
    if PrintProgram then
    begin
      Write(UTF8Encode(GenerateFuzzProgram(Bytes)));
      ExitCode := EXIT_OK;
      Exit;
    end;
    ExitCode := RunGrammar(Bytes, ExpandFileName('grammar.js'));
    Exit;
  end;

  if not ReadInput(FileName, Source, Detail) then
  begin
    Report('bytecode', foInputRejected, Detail);
    ExitCode := EXIT_OK;
    Exit;
  end;

  { relative imports resolve against the input's directory; stdin's against
    the working directory }
  if (FileName = '') or (FileName = '-') then
    RunName := ExpandFileName('stdin.js')
  else
    RunName := ExpandFileName(FileName);

  if RunExecutor(Source, RunName, True, 'bytecode', Outcome) then
  begin
    ExitCode := 1;
    Exit;
  end;
  { the same text the parser just rejected has nothing more to show the JIT }
  if Outcome in [foParseError, foModuleDenied] then
  begin
    ExitCode := EXIT_OK;
    Exit;
  end;
  if RunExecutor(Source, RunName, False, 'jit', Outcome) then
  begin
    ExitCode := 1;
    Exit;
  end;
  ExitCode := EXIT_OK;
end;

begin
  { build.pas snapshots the compiled preludes from a run with
    GOCCIA_SNAPSHOT_OUT set: a host's, as the other apps take them }
  if GetEnvironmentVariable('GOCCIA_SNAPSHOT_OUT') <> '' then
  begin
    GSetupError := '';
    if not TGocciaEngine.Create.PrepareHost(GSetupError) then
    begin
      WriteLn(ErrOutput, 'Error: ', UTF8Encode(GSetupError));
      Halt(1);
    end;
    Halt(0);
  end;
  GConsole := TFuzzConsole.Create;
  GResolver := TFuzzDeniedResolver.Create;
  try
    RunHarness;
  finally
    GResolver.Free;
    GConsole.Free;
  end;
end.
