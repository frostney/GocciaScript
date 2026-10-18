program GocciaHeapSnapshotProbe;

{ Heap snapshot prototype host.

  An embedder that pauses on a host call and resumes later, possibly in
  another process. The entry module gets two globals: print(...) writes a
  line to standard output, and hostCall(name, arg) returns a pending promise
  the host settles once the engine is idle (every microtask has run).

    GocciaHeapSnapshotProbe run FILE [OPTIONS]
      Answers every host call and runs to the end. This is also the replay
      a snapshot competes with: the time to a pause is the time a replay
      from the start needs to get back there.
    GocciaHeapSnapshotProbe census FILE [OPTIONS]
      As run, and at every pause writes a census of the live classes the
      script created to standard error.
    GocciaHeapSnapshotProbe snapshot FILE OUT [OPTIONS]
      Runs until the --pause=N-th pause (default 1), writes the snapshot to
      OUT and stops.
    GocciaHeapSnapshotProbe restore FILE IN [OPTIONS]
      Recompiles FILE without running its body, restores the snapshot IN,
      answers the pending host calls and continues to the end.

  Options:
    --answer=X        the number every host call is answered with (3)
    --pause=N         the pause a snapshot is taken at (1)
    --no-accounting   skip the check that every live object was serialized
    --compat          enable the compatibility flags the AWFY lane uses
                      (traditional for, while, for-in, function)
    --sandbox=DIR     run FILE (a sandbox path such as /main.js) in a
                      sandbox whose file system is a copy of DIR, with the
                      loader runtime profile (console, fs, goccia:* modules),
                      as a sandboxed embedder runs it
    --tools=a,b.c     with --sandbox: a global `tools` object whose
                      functions (dotted names nest) are host calls
    --ast, --vitest   with --sandbox: install goccia:ast, or vitest and timers
    --deterministic   with --sandbox: the deterministic host profile
    --compat-all      with --sandbox: every compatibility flag (var,
                      function, ASI, loose equality, ...)

  With --sandbox, the modules the entry imports are evaluated again on
  restore and what they hold is named like a built-in, not written: a
  prototype shortcut for programs whose imported modules keep no state the
  run changes.

  Measurements go to standard error as lines that start with "stats " and
  carry one JSON object. Times are milliseconds from the start of the
  process's work (before the engine is created). }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.Bytecode.Module,
  Goccia.Capabilities,
  Goccia.CLI.SandboxHost,
  Goccia.Engine,
  Goccia.HostEnvironment,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.AST,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.RuntimeExtensions.Timers,
  Goccia.RuntimeExtensions.VitestCompat,
  Goccia.RuntimeProfiles.Loader,
  Goccia.Sandbox.Context,
  Goccia.Values.ObjectValue,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Census,
  Goccia.HeapSnapshot.Codec,
  Goccia.MicrotaskQueue,
  Goccia.Scope,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

type
  TProbeMode = (pmRun, pmCensus, pmSnapshot, pmRestore);

  TProbeHost = class;

  TProbeExtension = class(TGocciaEngineExtension)
  public
    Host: TProbeHost;
    procedure WaitForIdle; override;
    function HasPendingWork: Boolean; override;
    procedure DiscardPending; override;
  end;

  ESnapshotTaken = class(Exception);

  TProbeHost = class
  private
    FMode: TProbeMode;
    FAnswer: Double;
    FSnapshotPath: string;
    FSnapshotPause: Integer;
    FAccounting: Boolean;
    FEngine: TGocciaEngine;
    FPending: TList<TGocciaPromiseValue>;
    FBaseline: TGocciaHeapBaseline;
    FBaselineBytes: Int64;
    FPauseCount: Integer;
    FModuleScope: TGocciaScope;
    FModule: TObject;
    FBuiltins: TGocciaHeapSnapshotBuiltinTable;
    FBuiltinTableMs: Double;
    FToolNames: TStringList;
    FAst, FVitest, FDeterministic, FCompatAll, FSandboxed: Boolean;
    procedure AnswerAll;
    procedure TakeSnapshot;
    procedure RestoreSnapshot(const AModuleScope: TGocciaScope;
      const AModule: TObject);
  public
    constructor Create(const AMode: TProbeMode; const AAnswer: Double;
      const ASnapshotPath: string; const ASnapshotPause: Integer;
      const AAccounting: Boolean);
    destructor Destroy; override;
    function Print(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HostCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure EntryModuleReady(const AEngine: TGocciaEngine;
      const AModuleScope: TGocciaScope; const AModule: TObject;
      var ASkipBody: Boolean);
    procedure CaptureBaseline;
    procedure ConsoleLine(const AMethod, ALine: string);
    procedure ConfigureSandboxEngine(const AEngine: TGocciaEngine;
      const AContext: TGocciaSandboxContext; const AEntryPath: string;
      const AParentEngine: TGocciaEngine;
      const AParentHostEnvironment: TGocciaHostEnvironment);
    procedure Pause;
    procedure Discard;
    property Pending: TList<TGocciaPromiseValue> read FPending;
    property Engine: TGocciaEngine read FEngine write FEngine;
    property Builtins: TGocciaHeapSnapshotBuiltinTable read FBuiltins
      write FBuiltins;
    property BuiltinTableMs: Double read FBuiltinTableMs write FBuiltinTableMs;
    property PauseCount: Integer read FPauseCount;
    property ToolNames: TStringList read FToolNames;
    property Ast: Boolean read FAst write FAst;
    property Vitest: Boolean read FVitest write FVitest;
    property Deterministic: Boolean read FDeterministic write FDeterministic;
    property CompatAll: Boolean read FCompatAll write FCompatAll;
  end;

var
  GStartNs: Int64;

function ElapsedMs: Double;
begin
  Result := (GetNanoseconds - GStartNs) / 1e6;
end;

function Ms(const AValue: Double): string;
begin
  Result := FormatFloat('0.000', AValue);
end;

procedure Stats(const AJson: string);
begin
  WriteLn(ErrOutput, 'stats ', AJson);
  Flush(ErrOutput);
end;

{ TProbeExtension }

procedure TProbeExtension.WaitForIdle;
begin
  if Host.Pending.Count > 0 then
    Host.Pause;
end;

function TProbeExtension.HasPendingWork: Boolean;
begin
  Result := Host.Pending.Count > 0;
end;

procedure TProbeExtension.DiscardPending;
begin
  Host.Discard;
end;

{ TProbeHost }

constructor TProbeHost.Create(const AMode: TProbeMode; const AAnswer: Double;
  const ASnapshotPath: string; const ASnapshotPause: Integer;
  const AAccounting: Boolean);
begin
  inherited Create;
  FMode := AMode;
  FAnswer := AAnswer;
  FSnapshotPath := ASnapshotPath;
  FSnapshotPause := ASnapshotPause;
  FAccounting := AAccounting;
  FPending := TList<TGocciaPromiseValue>.Create;
  FToolNames := TStringList.Create;
end;

destructor TProbeHost.Destroy;
begin
  Discard;
  FPending.Free;
  FToolNames.Free;
  FBaseline.Free;
  inherited;
end;

procedure TProbeHost.Discard;
var
  GC: TGarbageCollector;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    for I := 0 to FPending.Count - 1 do
      GC.RemoveTempRoot(FPending[I]);
  FPending.Clear;
end;

procedure TProbeHost.CaptureBaseline;
begin
  FBaseline := TGocciaHeapBaseline.Create;
  FBaseline.Capture;
  FBaselineBytes := TGarbageCollector.Instance.BytesAllocated;
end;

function TProbeHost.Print(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Line: string;
begin
  Line := '';
  for I := 0 to AArgs.Length - 1 do
  begin
    if I > 0 then
      Line := Line + ' ';
    Line := Line + AArgs.GetElement(I).ToStringLiteral.Value;
  end;
  WriteLn(Line);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TProbeHost.HostCall(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Promise);
  FPending.Add(Promise);
  Result := Promise;
end;

procedure TProbeHost.ConsoleLine(const AMethod, ALine: string);
begin
  WriteLn(ALine);
end;

{ As a sandboxed embedder configures its engine: the loader runtime profile,
  the sandbox's fs, optional modules, and `tools` whose functions are host
  calls. The probe's own hooks go on last, then the built-in table and the
  baseline, so everything installed here counts as engine state. }
procedure TProbeHost.ConfigureSandboxEngine(const AEngine: TGocciaEngine;
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AParentEngine: TGocciaEngine;
  const AParentHostEnvironment: TGocciaHostEnvironment);
var
  Runtime: TGocciaRuntimeCore;
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  Extension: TProbeExtension;
  Root, Node, Child: TGocciaObjectValue;
  Existing: TGocciaValue;
  I, P: Integer;
  Name, Part, Rest: string;
  StartNs: Int64;
begin
  if Assigned(AParentEngine) then
    raise Exception.Create('the probe does not support nested runs');
  Runtime := AttachRuntime(AEngine);
  ApplyLoaderRuntimeProfile(Runtime);
  Runtime.Install(TGocciaSandboxRuntimeExtension.Create(AContext));
  if FAst then
    Runtime.Install(TGocciaASTRuntimeExtension.Create);
  if FVitest then
  begin
    Runtime.Install(TGocciaTimersRuntimeExtension.Create);
    Runtime.Install(TGocciaVitestCompatRuntimeExtension.Create);
  end;
  AEngine.SuppressWarnings := True;
  ConsoleExtension := TGocciaConsoleRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
  if Assigned(ConsoleExtension) then
  begin
    ConsoleExtension.BuiltinConsole.Enabled := True;
    ConsoleExtension.BuiltinConsole.OutputCallback := ConsoleLine;
  end;
  AEngine.SourceType := stModule;
  AEngine.GlobalBackedTopLevel := False;
  if FDeterministic then
    AEngine.HostEnvironment.UseDeterministicProfile;
  if FCompatAll then
  begin
    // the full compatibility set a sandboxed embedder's compat switch enables
    AEngine.Compatibility := [cfASI, cfVar, cfFunction, cfTraditionalFor,
      cfWhileLoops, cfLooseEquality, cfNonStrictMode, cfArgumentsObject,
      cfLabel, cfForIn];
    AEngine.LabelStatementsEnabled := True;
    AEngine.ForInLoopsEnabled := True;
  end;
  FSandboxed := True;
  if FToolNames.Count > 0 then
  begin
    Root := TGocciaObjectValue.Create;
    for I := 0 to FToolNames.Count - 1 do
    begin
      Node := Root;
      Rest := FToolNames[I];
      repeat
        P := Pos('.', Rest);
        if P = 0 then
        begin
          Part := Rest;
          Rest := '';
        end
        else
        begin
          Part := Copy(Rest, 1, P - 1);
          Rest := Copy(Rest, P + 1, MaxInt);
        end;
        if Rest = '' then
          Node.AssignProperty(Part,
            TGocciaNativeFunctionValue.Create(HostCall, Part, -1))
        else
        begin
          Existing := Node.GetProperty(Part);
          if Existing is TGocciaObjectValue then
            Child := TGocciaObjectValue(Existing)
          else
          begin
            Child := TGocciaObjectValue.Create;
            Node.AssignProperty(Part, Child);
          end;
          Node := Child;
        end;
      until Rest = '';
    end;
    AEngine.GlobalScope.DefineLexicalBinding('tools', Root, dtConst);
  end;
  FEngine := AEngine;
  Extension := TProbeExtension.Create;
  Extension.Host := Self;
  AEngine.AddExtension(Extension);
  AEngine.OnEntryModuleReady := EntryModuleReady;
  if FMode in [pmSnapshot, pmRestore] then
  begin
    StartNs := GetNanoseconds;
    FBuiltins := TGocciaHeapSnapshotBuiltinTable.Create;
    BuildBuiltinTable(FBuiltins, AEngine);
    FBuiltinTableMs := (GetNanoseconds - StartNs) / 1e6;
  end;
  // the baseline and the module roots wait for the entry module: what the
  // other modules hold once evaluated is named, not written (EntryModuleReady)
end;

procedure TProbeHost.AnswerAll;
var
  Calls: array of TGocciaPromiseValue;
  GC: TGarbageCollector;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  SetLength(Calls, FPending.Count);
  for I := 0 to FPending.Count - 1 do
    Calls[I] := FPending[I];
  FPending.Clear;
  for I := 0 to High(Calls) do
  begin
    Calls[I].Resolve(TGocciaNumberLiteralValue.Create(FAnswer));
    GC.RemoveTempRoot(Calls[I]);
  end;
end;

procedure TProbeHost.Pause;
var
  Census: TGocciaHeapCensus;
begin
  Inc(FPauseCount);
  case FMode of
    pmRun:
      Stats('{"event":"pause","index":' + IntToStr(FPauseCount) +
        ',"ms":' + Ms(ElapsedMs) + '}');
    pmCensus:
      begin
        Census := TGocciaHeapCensus.Create;
        try
          Census.Take(FBaseline);
          WriteLn(ErrOutput, Census.ToJson(FEngine.SourcePath,
            'pause-' + IntToStr(FPauseCount)));
        finally
          Census.Free;
        end;
      end;
    pmSnapshot:
      if FPauseCount = FSnapshotPause then
      begin
        TakeSnapshot;
        raise ESnapshotTaken.Create('snapshot taken');
      end;
  end;
  AnswerAll;
end;

procedure TProbeHost.EntryModuleReady(const AEngine: TGocciaEngine;
  const AModuleScope: TGocciaScope; const AModule: TObject;
  var ASkipBody: Boolean);
begin
  FModuleScope := AModuleScope;
  FModule := AModule;
  if FSandboxed then
  begin
    // every other module has been evaluated: its state is named like a
    // built-in, and the census and the accounting count from here
    if Assigned(FBuiltins) then
      AddModuleRoots(FBuiltins, AEngine, AModule);
    if FMode in [pmCensus, pmSnapshot] then
      CaptureBaseline;
  end;
  if FMode = pmRestore then
  begin
    RestoreSnapshot(AModuleScope, AModule);
    ASkipBody := True;
  end;
end;

procedure TProbeHost.TakeSnapshot;
var
  PendingValues: array of TGocciaValue;
  I: Integer;
  Writer: TGocciaHeapSnapshotWriter;
  Stream: TMemoryStream;
  Census: TGocciaHeapCensus;
  PauseMs, SerializeMs, AccountingMs, WriteMs: Double;
  StartNs: Int64;
  LiveObjects: Integer;
  LiveBytes: Int64;
begin
  PauseMs := ElapsedMs;
  // The stacks keep what the last frames left in them; at an idle point no
  // frame needs it.
  if not FEngine.VM.ReleaseIdleScratch then
    raise EGocciaHeapSnapshotRefused.Create('a bytecode frame is running');
  // The census collects first, so the byte count is what survives.
  Census := TGocciaHeapCensus.Create;
  try
    Census.Take(FBaseline);
    LiveObjects := Census.Total;
  finally
    Census.Free;
  end;
  LiveBytes := TGarbageCollector.Instance.BytesAllocated - FBaselineBytes;

  SetLength(PendingValues, FPending.Count);
  for I := 0 to FPending.Count - 1 do
    PendingValues[I] := FPending[I];
  Stream := TMemoryStream.Create;
  Writer := TGocciaHeapSnapshotWriter.Create(FBuiltins,
    TGocciaBytecodeModule(FModule), FModuleScope);
  try
    StartNs := GetNanoseconds;
    // Built-ins the run materialized or created on first use join the
    // table.
    ExtendBuiltinTable(FBuiltins, FEngine, Writer.LateSlots);
    Writer.Serialize(PendingValues, Stream);
    SerializeMs := (GetNanoseconds - StartNs) / 1e6;
    AccountingMs := 0;
    if FAccounting then
    begin
      StartNs := GetNanoseconds;
      Writer.CheckAccounting(FBaseline);
      AccountingMs := (GetNanoseconds - StartNs) / 1e6;
    end;
    StartNs := GetNanoseconds;
    Stream.SaveToFile(FSnapshotPath);
    WriteMs := (GetNanoseconds - StartNs) / 1e6;
    Stats('{"event":"snapshot","pause":' + IntToStr(FPauseCount) +
      ',"pauseMs":' + Ms(PauseMs) +
      ',"liveObjects":' + IntToStr(LiveObjects) +
      ',"liveHeapBytes":' + IntToStr(LiveBytes) +
      ',"objects":' + IntToStr(Writer.ObjectCount) +
      ',"builtinRefs":' + IntToStr(Writer.BuiltinRefCount) +
      ',"lateIntrinsics":' + IntToStr(Writer.LateSlots.Count) +
      ',"bytes":' + IntToStr(Stream.Size) +
      ',"builtinTableMs":' + Ms(FBuiltinTableMs) +
      ',"serializeMs":' + Ms(SerializeMs) +
      ',"accountingMs":' + Ms(AccountingMs) +
      ',"writeFileMs":' + Ms(WriteMs) + '}');
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

procedure TProbeHost.RestoreSnapshot(const AModuleScope: TGocciaScope;
  const AModule: TObject);
var
  Reader: TGocciaHeapSnapshotReader;
  Resolver: TGocciaBuiltinPathResolver;
  Stream: TMemoryStream;
  Promises: TArray<TGocciaValue>;
  I: Integer;
  CompiledMs, ReadFileMs, RestoreMs: Double;
  StartNs: Int64;
begin
  CompiledMs := ElapsedMs;
  Stream := TMemoryStream.Create;
  Reader := TGocciaHeapSnapshotReader.Create(FBuiltins,
    TGocciaBytecodeModule(AModule), AModuleScope);
  Resolver := TGocciaBuiltinPathResolver.Create(FBuiltins, FEngine);
  try
    Reader.Context := FEngine.VM;
    Reader.BuiltinResolve := Resolver.Resolve;
    StartNs := GetNanoseconds;
    Stream.LoadFromFile(FSnapshotPath);
    ReadFileMs := (GetNanoseconds - StartNs) / 1e6;
    StartNs := GetNanoseconds;
    Promises := Reader.Restore(Stream);
    RestoreMs := (GetNanoseconds - StartNs) / 1e6;
    for I := 0 to High(Promises) do
    begin
      if not (Promises[I] is TGocciaPromiseValue) then
        raise EGocciaHeapSnapshotRefused.Create(
          'a pending root is not a promise');
      TGarbageCollector.Instance.AddTempRoot(Promises[I]);
      FPending.Add(TGocciaPromiseValue(Promises[I]));
    end;
    Stats('{"event":"restore","bytes":' + IntToStr(Stream.Size) +
      ',"objects":' + IntToStr(Reader.ObjectCount) +
      ',"builtinTableMs":' + Ms(FBuiltinTableMs) +
      ',"compiledMs":' + Ms(CompiledMs) +
      ',"readFileMs":' + Ms(ReadFileMs) +
      ',"restoreMs":' + Ms(RestoreMs) +
      ',"resumedMs":' + Ms(ElapsedMs) + '}');
  finally
    Resolver.Free;
    Reader.Free;
    Stream.Free;
  end;
end;

function ParseMode(const AText: string): TProbeMode;
begin
  if AText = 'run' then
    Result := pmRun
  else if AText = 'census' then
    Result := pmCensus
  else if AText = 'snapshot' then
    Result := pmSnapshot
  else if AText = 'restore' then
    Result := pmRestore
  else
    raise Exception.Create('Unknown mode: ' + AText);
end;

function RunProbe: Integer;
var
  Mode: TProbeMode;
  FileName, SnapshotPath, Arg: string;
  Answer: Double;
  SnapshotPause, I, FirstOption: Integer;
  Accounting, Compat, Ast, Vitest, Deterministic, CompatAllFlag: Boolean;
  SandboxDir, ToolList: string;
  Sandbox: TGocciaSandboxHost;
  SandboxResult: TGocciaSandboxRunResult;
  Source: TStringList;
  Host: TProbeHost;
  Engine: TGocciaEngine;
  Extension: TProbeExtension;
  Builtins: TGocciaHeapSnapshotBuiltinTable;
  StartNs: Int64;
begin
  GStartNs := GetNanoseconds;
  Result := 0;
  if ParamCount < 2 then
  begin
    WriteLn(ErrOutput,
      'Usage: GocciaHeapSnapshotProbe run|census FILE | snapshot FILE OUT | ' +
      'restore FILE IN  [--answer=X] [--pause=N] [--no-accounting] ' +
      '[--compat]');
    Exit(2);
  end;
  Mode := ParseMode(ParamStr(1));
  FileName := ParamStr(2);
  SnapshotPath := '';
  FirstOption := 3;
  if Mode in [pmSnapshot, pmRestore] then
  begin
    SnapshotPath := ParamStr(3);
    FirstOption := 4;
  end;
  Answer := 3;
  SnapshotPause := 1;
  Accounting := True;
  Compat := False;
  Ast := False;
  Vitest := False;
  Deterministic := False;
  CompatAllFlag := False;
  SandboxDir := '';
  ToolList := '';
  for I := FirstOption to ParamCount do
  begin
    Arg := ParamStr(I);
    if Copy(Arg, 1, 9) = '--answer=' then
      Answer := StrToFloat(Copy(Arg, 10, MaxInt))
    else if Copy(Arg, 1, 8) = '--pause=' then
      SnapshotPause := StrToInt(Copy(Arg, 9, MaxInt))
    else if Arg = '--no-accounting' then
      Accounting := False
    else if Arg = '--compat' then
      Compat := True
    else if Copy(Arg, 1, 10) = '--sandbox=' then
      SandboxDir := Copy(Arg, 11, MaxInt)
    else if Copy(Arg, 1, 8) = '--tools=' then
      ToolList := Copy(Arg, 9, MaxInt)
    else if Arg = '--ast' then
      Ast := True
    else if Arg = '--vitest' then
      Vitest := True
    else if Arg = '--deterministic' then
      Deterministic := True
    else if Arg = '--compat-all' then
      CompatAllFlag := True
    else
      raise Exception.Create('Unknown option: ' + Arg);
  end;

  if SandboxDir <> '' then
  begin
    Host := TProbeHost.Create(Mode, Answer, SnapshotPath, SnapshotPause,
      Accounting);
    Sandbox := TGocciaSandboxHost.Create(256 * 1024 * 1024, 100000);
    try
      Host.ToolNames.StrictDelimiter := True;
      Host.ToolNames.Delimiter := ',';
      if ToolList <> '' then
        Host.ToolNames.DelimitedText := ToolList;
      Host.Ast := Ast;
      Host.Vitest := Vitest;
      Host.Deterministic := Deterministic;
      Host.CompatAll := CompatAllFlag;
      Sandbox.KeepRootResult := False;
      Sandbox.TimeoutMilliseconds := 600000;
      Sandbox.OnConfigureEngine := Host.ConfigureSandboxEngine;
      Sandbox.CopyIn(SandboxDir, '/', False);
      Sandbox.CaptureBaseline;
      SandboxResult := Sandbox.Run(FileName);
      if (Mode = pmSnapshot) and (SandboxResult.ErrorMessage = 'snapshot taken') then
        Result := 0
      else if not SandboxResult.Ok then
      begin
        WriteLn(ErrOutput, 'Error: ', SandboxResult.ErrorMessage);
        Result := 1;
      end
      else if Mode = pmSnapshot then
      begin
        WriteLn(ErrOutput, 'Error: the run ended after ',
          Host.PauseCount, ' pause(s), before pause ', SnapshotPause);
        Result := 1;
      end;
      if (Result = 0) and (Mode in [pmRun, pmRestore]) then
        Stats('{"event":"end","pauses":' + IntToStr(Host.PauseCount) +
          ',"ms":' + Ms(ElapsedMs) + '}');
    finally
      Host.Discard;
      Sandbox.Free;
      Host.Builtins.Free;
      Host.Free;
    end;
    Exit;
  end;

  Source := TStringList.Create;
  Builtins := nil;
  try
    Source.LoadFromFile(FileName);
    Host := TProbeHost.Create(Mode, Answer, SnapshotPath, SnapshotPause,
      Accounting);
    try
      Engine := TGocciaEngine.Create(FileName, Source);
      try
        Engine.SourceType := stModule;
        if Compat then
          Engine.Compatibility := [cfTraditionalFor, cfWhileLoops, cfForIn,
            cfFunction];
        Host.Engine := Engine;
        Engine.RegisterGlobal('print',
          TGocciaNativeFunctionValue.CreateWithoutPrototype(Host.Print,
            'print', -1));
        Engine.RegisterGlobal('hostCall',
          TGocciaNativeFunctionValue.CreateWithoutPrototype(Host.HostCall,
            'hostCall', 2));
        Engine.RefreshGlobalThis;
        Extension := TProbeExtension.Create;
        Extension.Host := Host;
        Engine.AddExtension(Extension);
        Engine.OnEntryModuleReady := Host.EntryModuleReady;

        if Mode in [pmSnapshot, pmRestore] then
        begin
          StartNs := GetNanoseconds;
          Builtins := TGocciaHeapSnapshotBuiltinTable.Create;
          BuildBuiltinTable(Builtins, Engine);
          Host.Builtins := Builtins;
          Host.BuiltinTableMs := (GetNanoseconds - StartNs) / 1e6;
        end;
        if Mode in [pmCensus, pmSnapshot] then
          Host.CaptureBaseline;

        try
          Engine.Execute;
          if Mode = pmSnapshot then
          begin
            WriteLn(ErrOutput, 'Error: the run ended after ',
              Host.PauseCount, ' pause(s), before pause ', SnapshotPause);
            Result := 1;
          end;
        except
          on E: ESnapshotTaken do
            Result := 0;
          on E: EGocciaBytecodeThrow do
          begin
            WriteLn(ErrOutput, 'Uncaught: ',
              E.ThrownValue.ToStringLiteral.Value);
            Result := 1;
          end;
          on E: TGocciaThrowValue do
          begin
            WriteLn(ErrOutput, 'Uncaught: ', E.Value.ToStringLiteral.Value);
            Result := 1;
          end;
        end;
        if (Result = 0) and (Mode in [pmRun, pmRestore]) then
          Stats('{"event":"end","pauses":' + IntToStr(Host.PauseCount) +
            ',"ms":' + Ms(ElapsedMs) + '}');
      finally
        Host.Discard;
        Engine.Free;
      end;
    finally
      Host.Free;
    end;
  finally
    Builtins.Free;
    Source.Free;
  end;
end;

begin
  try
    ExitCode := RunProbe;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'Error: ', E.ClassName, ': ', E.Message);
      if GetEnvironmentVariable('GOCCIA_HEAP_SNAPSHOT_TRACE') <> '' then
        DumpExceptionBackTrace(ErrOutput);
      ExitCode := 1;
    end;
  end;
end.
