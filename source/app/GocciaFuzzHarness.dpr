program GocciaFuzzHarness;

{ Coverage-blind crash harness for the lexer, parser, and both executors.

  Reads one source input from a file argument or stdin, then drives it
  through lex -> parse -> interpreter -> bytecode under tight instruction,
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

  TextSemantics,

  Goccia.CLI.Stdin,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Modules.ContentProvider,
  Goccia.StackLimit,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.VM.Exception;

const
  FUZZ_PROGRAM_NAME = 'GocciaFuzzHarness';

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
    foUnexpected
  );

  TFuzzMode = (fmInterpreted, fmBytecode);

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
  else
    Result := 'UNEXPECTED';
  end;
end;

function ModeName(const AMode: TFuzzMode): string;
begin
  if AMode = fmInterpreted then
    Result := 'interpreter'
  else
    Result := 'bytecode';
end;

procedure Report(const AMode: TFuzzMode; const AOutcome: TFuzzOutcome;
  const ADetail: string);
begin
  if not GVerbose then
    Exit;
  if ADetail = '' then
    WriteLn(ErrOutput, Format('[%s] %s', [ModeName(AMode),
      OutcomeName(AOutcome)]))
  else
    WriteLn(ErrOutput, Format('[%s] %s: %s', [ModeName(AMode),
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

function CreateExecutorForMode(const AMode: TFuzzMode): TGocciaExecutor;
begin
  if AMode = fmInterpreted then
    Result := TGocciaInterpreterExecutor.Create
  else
    Result := TGocciaBytecodeExecutor.Create;
end;

{ Runs one source through one executor and classifies the outcome.

  The except ladder is the whole point of this harness, so it is exhaustive
  by construction: every engine-defined error type is named explicitly and
  the bare `on E: Exception` fallback is what a genuine finding looks like.
  Do not add a blanket handler above the specific ones. }
function RunOnce(const ASource: TStringList; const AMode: TFuzzMode;
  out ADetail: string): TFuzzOutcome;
var
  Engine: TGocciaEngine;
  Executor: TGocciaExecutor;
  GC: TGarbageCollector;
begin
  ADetail := '';
  StartExecutionTimeout(FUZZ_TIMEOUT_MS);
  StartInstructionLimit(FUZZ_MAX_INSTRUCTIONS);
  SetMaxStackDepth(FUZZ_STACK_DEPTH);
  try
    Executor := CreateExecutorForMode(AMode);
    try
      Engine := TGocciaEngine.Create(FUZZ_PROGRAM_NAME, ASource, Executor);
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
      Executor.Free;
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
      finally
        InputStream.Free;
      end;
      if FileByteSize > FUZZ_MAX_INPUT_BYTES then
      begin
        ADetail := Format('input exceeds %d bytes', [FUZZ_MAX_INPUT_BYTES]);
        Exit(False);
      end;
      SourceText := ReadUTF8FileText(AFileName);
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

procedure PrintUsage;
begin
  WriteLn(ErrOutput, 'Usage: ', FUZZ_PROGRAM_NAME, ' [--verbose] <file>|-');
  WriteLn(ErrOutput);
  WriteLn(ErrOutput, 'Drives one input through lex, parse, and both ' +
    'executors under tight bounds.');
  WriteLn(ErrOutput, 'Exits 0 for every engine-modelled outcome; nonzero ' +
    'only for an unexpected fault.');
end;

var
  I: Integer;
  Arg, FileName, Detail, Trace: string;
  Source: TStringList;
  Mode: TFuzzMode;
  Outcome: TFuzzOutcome;
  Failed, SelfTestFault: Boolean;
begin
  FileName := '';
  Failed := False;
  SelfTestFault := False;
  Trace := '';

  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if (Arg = '--verbose') or (Arg = '-v') then
      GVerbose := True
    else if Arg = '--self-test-fault' then
      SelfTestFault := True
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
          [FUZZ_PROGRAM_NAME, ModeName(fmInterpreted), E.ClassName,
           E.Message]));
        WriteLn(ErrOutput, BackTraceStrFunc(ExceptAddr) + LineEnding +
          CaptureBackTrace);
      end;
    end;
    Halt(1);
  end;

  if (FileName = '') and IsInputTerminal then
  begin
    PrintUsage;
    Halt(EXIT_USAGE);
  end;

  if not ReadInput(FileName, Source, Detail) then
  begin
    Report(fmInterpreted, foInputRejected, Detail);
    Halt(EXIT_OK);
  end;

  try
    for Mode := Low(TFuzzMode) to High(TFuzzMode) do
    begin
      try
        Outcome := RunOnce(Source, Mode, Detail);
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
          [FUZZ_PROGRAM_NAME, ModeName(Mode), Detail]));
        WriteLn(ErrOutput, Trace);
        Failed := True;
      end
      else
        Report(Mode, Outcome, Detail);
    end;
  finally
    Source.Free;
  end;

  if Failed then
    ExitCode := 1
  else
    ExitCode := EXIT_OK;
end.
