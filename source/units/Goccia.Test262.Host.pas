unit Goccia.Test262.Host;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.HostEnvironment,
  Goccia.SourcePipeline,
  Goccia.Values.Primitives;

type
  TGocciaTest262EngineOptions = record
    Compatibility: TGocciaCompatibilityFlags;
    ExperimentalJSModuleSourceEnabled: Boolean;
    UnsafeShadowRealm: Boolean;
    AgentCanSuspend: Boolean;
    SourceType: TGocciaSourceType;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
    StackSize: Integer;
    { The case engine's capability set. $262.createRealm() realms and agents
      are created with the same set, so they never reach more than the case
      that created them (ADR 0122). The default value grants nothing. }
    Capabilities: TGocciaCapabilities;
  end;

  TGocciaTest262Host = class
  private
    FImplementation: TObject;
  public
    constructor Create(const AOptions: TGocciaTest262EngineOptions);
    destructor Destroy; override;
    procedure ConfigureHostEnvironment(const AParent: TGocciaHostEnvironment);
    procedure Install(const AEngine: TGocciaEngine);
    property Backend: TObject read FImplementation;
  end;

procedure ConfigureTest262Engine(const AEngine: TGocciaEngine;
  const AOptions: TGocciaTest262EngineOptions;
  const AParentEnvironment: TGocciaHostEnvironment = nil);
function Test262SourceCanSuspendAgent(const APath: string): Boolean;
{ Parses ASourceLines as a Script under AEngine's language profile and
  evaluates it in AEngine's realm, as its own global Script: its directive
  prologue alone decides its strictness, and its global declarations share
  the realm's global environment with every other Script evaluated there.
  ASourcePath names the Script in diagnostics. Raises what parsing, global
  declaration instantiation or evaluation raises. $262.evalScript and the
  runner's harness includes both evaluate through it. }
function EvaluateTest262Script(const AEngine: TGocciaEngine;
  const ASourceLines: TStringList; const ASourcePath: string): TGocciaValue;

implementation

uses
  Generics.Collections,
  StrUtils,
  SysUtils,

  CriticalSections,
  TextSemantics,
  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Atomics,
  Goccia.Builtins.GlobalEval,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.Error,
  Goccia.ExecutionContext,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.StackLimit,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.VM.Exception;

type
  TTest262Realm = class;
  TTest262AgentHost = class;
  TTest262AgentThread = class;
  TTest262HostImplementation = class;

  TGocciaTest262IsHTMLDDAValue = class(TGocciaObjectValue)
  public
    function HasHTMLDDAInternalSlot: Boolean; override;
    function TypeOf: string; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
  end;

  TTest262EvalHost = class
  private
    FEngine: TGocciaEngine;
  public
    constructor Create(const AEngine: TGocciaEngine);
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TTest262Realm = class
  private
    FEngine: TGocciaEngine;
    FEvalHost: TTest262EvalHost;
    FTest262Host: TTest262HostImplementation;
    FSource: TStringList;
  public
    constructor Create(const AOptions: TGocciaTest262EngineOptions;
      const AParentEnvironment: TGocciaHostEnvironment);
    destructor Destroy; override;
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GlobalObject: TGocciaObjectValue;
  end;

  TTest262HostImplementation = class
  private
    FAgentHost: TTest262AgentHost;
    FEvalHost: TTest262EvalHost;
    FHostEnvironment: TGocciaHostEnvironment;
    FOptions: TGocciaTest262EngineOptions;
    FRealms: TObjectList<TTest262Realm>;
  public
    constructor Create(const AOptions: TGocciaTest262EngineOptions);
    destructor Destroy; override;
    procedure ConfigureHostEnvironment(const AParent: TGocciaHostEnvironment);
    function CreateAgentObject: TGocciaObjectValue;
    function CreateRealm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure Install(const AEngine: TGocciaEngine);
    property HostEnvironment: TGocciaHostEnvironment read FHostEnvironment;
  end;

  TTest262AgentHost = class
  private
    FBroadcastGeneration: Integer;
    FBroadcastValue: TGocciaValue;
    FLock: TGocciaCriticalSection;
    FOptions: TGocciaTest262EngineOptions;
    FReports: TStringList;
    FShuttingDown: Boolean;
    FTest262Host: TTest262HostImplementation;
    FWorkers: TObjectList<TTest262AgentThread>;
    procedure PumpCurrentThreadWork;
  public
    constructor Create(const AOptions: TGocciaTest262EngineOptions;
      const ATest262Host: TTest262HostImplementation);
    destructor Destroy; override;
    function Broadcast(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GetReport(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Leaving(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function MonotonicNow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ReceiveBroadcast(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Report(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SleepAgent(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Start(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TTest262AgentThread = class(TThread)
  private
    FHost: TTest262HostImplementation;
    FOptions: TGocciaTest262EngineOptions;
    FSourceText: string;
    procedure WaitForAsyncAgentWork;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHost: TTest262HostImplementation;
      const AOptions: TGocciaTest262EngineOptions;
      const ASourceText: string);
  end;

procedure ConfigureTest262Engine(const AEngine: TGocciaEngine;
  const AOptions: TGocciaTest262EngineOptions;
  const AParentEnvironment: TGocciaHostEnvironment);
begin
  if Assigned(AParentEnvironment) then
    AEngine.HostEnvironment.ConfigureAsChildOf(AParentEnvironment);
  AEngine.Compatibility := AOptions.Compatibility;
  { Conformance tests leave promises rejected on purpose (a synchronous test
    that only inspects what `Promise.reject` returns, an `import('')` syntax
    test), and the harness reports asynchronous failures through $DONE. }
  AEngine.UnhandledRejections := urIgnore;
  AEngine.LabelStatementsEnabled := True;
  AEngine.ForInLoopsEnabled := True;
  AEngine.ExperimentalJSModuleSourceEnabled :=
    AOptions.ExperimentalJSModuleSourceEnabled;
  AEngine.SourceType := AOptions.SourceType;
  { test262 runs eval through the engine's supported option, the one an
    embedder uses, rather than a conformance-only copy. }
  AEngine.FunctionConstructor.Enabled := True;
  EnableEval(AEngine);
  if AOptions.UnsafeShadowRealm then
    EnableShadowRealm(AEngine);
  SetAtomicsAgentCanSuspend(AOptions.AgentCanSuspend);
  AEngine.ModuleLoader.SetContentProvider(
    TGocciaFileSystemModuleContentProvider.Create, True);
  AEngine.GlobalBackedTopLevel := AOptions.SourceType = stScript;
end;

procedure InstallTest262HostGlobals(const AEngine: TGocciaEngine;
  const AHost: TTest262HostImplementation;
  const AEvalHost: TTest262EvalHost);
var
  Test262Object: TGocciaObjectValue;
begin
  AEngine.GocciaGlobal.AssignProperty('test262Host',
    TGocciaBooleanLiteralValue.TrueValue);
  Test262Object := TGocciaObjectValue.Create;
  Test262Object.AssignProperty('createRealm',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      AHost.CreateRealm, 'createRealm', 0));
  Test262Object.AssignProperty('evalScript',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      AEvalHost.EvalScript, 'evalScript', 1));
  Test262Object.AssignProperty('abstractModuleSourcePrototype',
    TGocciaModuleSourceValue.SharedPrototype);
  Test262Object.AssignProperty('isHTMLDDA',
    TGocciaTest262IsHTMLDDAValue.Create);
  Test262Object.AssignProperty('agent', AHost.CreateAgentObject);
  AEngine.GocciaGlobal.AssignProperty('test262', Test262Object);
  AEngine.RefreshGlobalThis;
end;

function TGocciaTest262IsHTMLDDAValue.HasHTMLDDAInternalSlot: Boolean;
begin
  Result := True;
end;

function TGocciaTest262IsHTMLDDAValue.TypeOf: string;
begin
  Result := 'undefined';
end;

function TGocciaTest262IsHTMLDDAValue.ToBooleanLiteral:
  TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

constructor TTest262EvalHost.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

function EvaluateTest262Script(const AEngine: TGocciaEngine;
  const ASourceLines: TStringList; const ASourcePath: string): TGocciaValue;
var
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  ScriptOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  RealmScope: TGocciaExecutionContextScope;
begin
  ScriptOptions := TGocciaSourcePipeline.DefaultOptions;
  ScriptOptions.Preprocessors := AEngine.Preprocessors;
  ScriptOptions.Compatibility := AEngine.Compatibility;
  ScriptOptions.LabelStatementsEnabled := AEngine.LabelStatementsEnabled;
  ScriptOptions.ForInLoopsEnabled := AEngine.ForInLoopsEnabled;
  ScriptOptions.ExperimentalJSModuleSourceEnabled :=
    AEngine.ExperimentalJSModuleSourceEnabled;
  ScriptOptions.WarningUnsupportedFeatures :=
    AEngine.WarningUnsupportedFeatures;
  ScriptOptions.SourceType := stScript;
  ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(ScriptOptions);
  try
    RealmScope := AEngine.ActivateRealmExecutionContext;
    try
      PipelineResult := TGocciaSourcePipeline.Parse(ASourceLines,
        ASourcePath, ScriptOptions);
      try
        Result := AEngine.ExecuteScriptProgram(PipelineResult.ProgramNode,
          ASourcePath);
      finally
        PipelineResult.Free;
      end;
    finally
      RealmScope.Free;
    end;
  finally
    ActiveOptionsScope.Free;
  end;
end;

function TTest262EvalHost.EvalScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ScriptSource: TStringList;
  SourceValue: TGocciaValue;
begin
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  SourceValue := AArgs.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
    Exit(SourceValue);

  ScriptSource := CreateECMAScriptSourceLines(
    TGocciaStringLiteralValue(SourceValue).Value);
  try
    Result := EvaluateTest262Script(FEngine, ScriptSource,
      '<test262-eval-script>');
  finally
    ScriptSource.Free;
  end;
end;

constructor TTest262Realm.Create(
  const AOptions: TGocciaTest262EngineOptions;
  const AParentEnvironment: TGocciaHostEnvironment);
var
  ChildOptions: TGocciaTest262EngineOptions;
begin
  inherited Create;
  FSource := TStringList.Create;
  FEngine := TGocciaEngine.Create('<test262-realm>', FSource,
    AOptions.Capabilities);
  ChildOptions := AOptions;
  ChildOptions.SourceType := stScript;
  ConfigureTest262Engine(FEngine, ChildOptions, AParentEnvironment);
  FEvalHost := TTest262EvalHost.Create(FEngine);
  FTest262Host := TTest262HostImplementation.Create(ChildOptions);
  FTest262Host.ConfigureHostEnvironment(FEngine.HostEnvironment);
  InstallTest262HostGlobals(FEngine, FTest262Host, FEvalHost);
  FEngine.SuspendRealmExecutionContext;
end;

destructor TTest262Realm.Destroy;
var
  RealmScope: TGocciaExecutionContextScope;
begin
  if Assigned(FEngine) then
  begin
    RealmScope := FEngine.ActivateRealmExecutionContext;
    try
      FEngine.Free;
    finally
      RealmScope.Free;
    end;
  end;
  FTest262Host.Free;
  FEvalHost.Free;
  FSource.Free;
  inherited;
end;

function TTest262Realm.GlobalObject: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue(FEngine.Realm.GlobalObject);
end;

function TTest262Realm.EvalScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FEvalHost.EvalScript(AArgs, AThisValue);
end;

constructor TTest262HostImplementation.Create(
  const AOptions: TGocciaTest262EngineOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FAgentHost := TTest262AgentHost.Create(AOptions, Self);
  FRealms := TObjectList<TTest262Realm>.Create(True);
end;

destructor TTest262HostImplementation.Destroy;
begin
  FAgentHost.Free;
  FRealms.Free;
  FEvalHost.Free;
  FHostEnvironment.Free;
  inherited;
end;

procedure TTest262HostImplementation.ConfigureHostEnvironment(
  const AParent: TGocciaHostEnvironment);
begin
  FreeAndNil(FHostEnvironment);
  FHostEnvironment := TGocciaHostEnvironment.Create;
  FHostEnvironment.ConfigureAsChildOf(AParent);
end;

function TTest262HostImplementation.CreateAgentObject: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty('broadcast',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.Broadcast, 'broadcast', 1));
  Result.AssignProperty('getReport',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.GetReport, 'getReport', 0));
  Result.AssignProperty('leaving',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.Leaving, 'leaving', 0));
  Result.AssignProperty('monotonicNow',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.MonotonicNow, 'monotonicNow', 0));
  Result.AssignProperty('receiveBroadcast',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.ReceiveBroadcast, 'receiveBroadcast', 1));
  Result.AssignProperty('report',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.Report, 'report', 1));
  Result.AssignProperty('sleep',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.SleepAgent, 'sleep', 1));
  Result.AssignProperty('start',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FAgentHost.Start, 'start', 1));
end;

function TTest262HostImplementation.CreateRealm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Realm: TTest262Realm;
  RealmRecord: TGocciaObjectValue;
begin
  Realm := TTest262Realm.Create(FOptions, FHostEnvironment);
  FRealms.Add(Realm);
  RealmRecord := TGocciaObjectValue.Create;
  RealmRecord.AssignProperty('global', Realm.GlobalObject);
  RealmRecord.AssignProperty('evalScript',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Realm.EvalScript, 'evalScript', 1));
  RealmRecord.AssignProperty('createRealm',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      CreateRealm, 'createRealm', 0));
  Result := RealmRecord;
end;

procedure TTest262HostImplementation.Install(const AEngine: TGocciaEngine);
begin
  FreeAndNil(FEvalHost);
  FEvalHost := TTest262EvalHost.Create(AEngine);
  InstallTest262HostGlobals(AEngine, Self, FEvalHost);
end;

constructor TTest262AgentHost.Create(
  const AOptions: TGocciaTest262EngineOptions;
  const ATest262Host: TTest262HostImplementation);
begin
  inherited Create;
  FOptions := AOptions;
  FTest262Host := ATest262Host;
  FReports := TStringList.Create;
  FWorkers := TObjectList<TTest262AgentThread>.Create(True);
  CriticalSectionInit(FLock);
end;

destructor TTest262AgentHost.Destroy;
var
  Worker: TTest262AgentThread;
begin
  CriticalSectionEnter(FLock);
  try
    FShuttingDown := True;
  finally
    CriticalSectionLeave(FLock);
  end;
  for Worker in FWorkers do
    Worker.WaitFor;
  CriticalSectionEnter(FLock);
  try
    if Assigned(FBroadcastValue) and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveQueuedRoot(FBroadcastValue);
    FBroadcastValue := nil;
  finally
    CriticalSectionLeave(FLock);
  end;
  FWorkers.Free;
  CriticalSectionDone(FLock);
  FReports.Free;
  inherited;
end;

procedure TTest262AgentHost.PumpCurrentThreadWork;
var
  Queue: TGocciaMicrotaskQueue;
begin
  PumpAtomicsWaitAsyncCompletions;
  Queue := TGocciaMicrotaskQueue.Instance;
  if Assigned(Queue) and Queue.HasPending then
    Queue.DrainOneJob;
end;

function TTest262AgentHost.Broadcast(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  CriticalSectionEnter(FLock);
  try
    if Assigned(FBroadcastValue) and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveQueuedRoot(FBroadcastValue);
    FBroadcastValue := Value;
    if Assigned(FBroadcastValue) and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddQueuedRoot(FBroadcastValue);
    Inc(FBroadcastGeneration);
  finally
    CriticalSectionLeave(FLock);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTest262AgentHost.GetReport(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ReportText: string;
begin
  PumpCurrentThreadWork;
  CriticalSectionEnter(FLock);
  try
    if FReports.Count = 0 then
      Exit(TGocciaNullLiteralValue.NullValue);
    ReportText := FReports[0];
    FReports.Delete(0);
  finally
    CriticalSectionLeave(FLock);
  end;
  Result := TGocciaStringLiteralValue.Create(ReportText);
end;

function TTest262AgentHost.Leaving(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  PumpCurrentThreadWork;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTest262AgentHost.MonotonicNow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(GetMilliseconds);
end;

function TTest262AgentHost.ReceiveBroadcast(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  BroadcastValue: TGocciaValue;
  Callback: TGocciaValue;
  CallbackArgs: TGocciaArgumentsCollection;
begin
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('$262.agent.receiveBroadcast callback must be callable');
  Callback := AArgs.GetElement(0);
  BroadcastValue := nil;
  repeat
    CriticalSectionEnter(FLock);
    try
      if FShuttingDown then
        Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
      if FBroadcastGeneration > 0 then
      begin
        BroadcastValue := FBroadcastValue;
        Break;
      end;
    finally
      CriticalSectionLeave(FLock);
    end;
    PumpCurrentThreadWork;
    CheckInstructionLimit;
    CheckExecutionTimeoutNow;
    Sleep(1);
  until False;
  CallbackArgs := TGocciaArgumentsCollection.Create([BroadcastValue]);
  try
    // Any callable, a callable Proxy included.
    Result := DispatchCall(Callback, CallbackArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallbackArgs.Free;
  end;
end;

function TTest262AgentHost.Report(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ReportText: string;
begin
  if AArgs.Length > 0 then
    ReportText := AArgs.GetElement(0).ToStringLiteral.Value
  else
    ReportText :=
      TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;
  CriticalSectionEnter(FLock);
  try
    FReports.Add(ReportText);
  finally
    CriticalSectionLeave(FLock);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTest262AgentHost.SleepAgent(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Deadline: Int64;
  Milliseconds: Int64;
  NowMilliseconds: Int64;
  Remaining: Int64;
begin
  if AArgs.Length = 0 then
    Milliseconds := 0
  else
    Milliseconds := ToInt64Value(AArgs.GetElement(0));
  if Milliseconds < 0 then
    Milliseconds := 0;
  Deadline := GetMilliseconds + Milliseconds;
  repeat
    PumpCurrentThreadWork;
    CheckInstructionLimit;
    CheckExecutionTimeoutNow;
    NowMilliseconds := GetMilliseconds;
    if NowMilliseconds >= Deadline then
      Break;
    Remaining := Deadline - NowMilliseconds;
    if Remaining > 5 then
      Sleep(5)
    else
      Sleep(LongWord(Remaining));
  until False;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTest262AgentHost.Start(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  SourceText: string;
  Worker: TTest262AgentThread;
begin
  if (AArgs.Length = 0) or
     not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('$262.agent.start source must be a string');
  SourceText := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  CriticalSectionEnter(FLock);
  try
    if FShuttingDown then
      Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
    Worker := TTest262AgentThread.Create(FTest262Host, FOptions, SourceText);
    FWorkers.Add(Worker);
    Worker.Start;
  finally
    CriticalSectionLeave(FLock);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

constructor TTest262AgentThread.Create(
  const AHost: TTest262HostImplementation;
  const AOptions: TGocciaTest262EngineOptions; const ASourceText: string);
const
  AGENT_STACK_SIZE = 8 * 1024 * 1024;
begin
  inherited Create(True, AGENT_STACK_SIZE);
  FreeOnTerminate := False;
  FHost := AHost;
  FOptions := AOptions;
  FSourceText := ASourceText;
end;

procedure TTest262AgentThread.WaitForAsyncAgentWork;
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  repeat
    PumpAtomicsWaitAsyncCompletions;
    if Assigned(Queue) and Queue.HasPending then
    begin
      Queue.DrainQueue;
      Continue;
    end;
    if not HasPendingAtomicsWaitAsyncCompletions then
      Break;
    CheckInstructionLimit;
    CheckExecutionTimeoutNow;
    Sleep(1);
  until False;
end;

procedure TTest262AgentThread.Execute;
var
  AgentOptions: TGocciaTest262EngineOptions;
  Engine: TGocciaEngine;
  EvalHost: TTest262EvalHost;
  Source: TStringList;
begin
  InitThreadRuntime(False, FOptions.MaxMemoryBytes);
  try
    StartExecutionTimeout(FOptions.TimeoutMs);
    StartInstructionLimit(FOptions.MaxInstructions);
    SetMaxStackDepth(FOptions.StackSize);
    Source := CreateECMAScriptSourceLines(
      ReadUTF8FileText(ExpandFileName('scripts' + PathDelim +
        'test262_harness' + PathDelim + '$262.js')) + sLineBreak +
      FSourceText);
    try
      Engine := TGocciaEngine.Create('<test262-agent>', Source,
        FOptions.Capabilities);
      try
        AgentOptions := FOptions;
        AgentOptions.SourceType := stScript;
        ConfigureTest262Engine(Engine, AgentOptions, FHost.HostEnvironment);
        EvalHost := TTest262EvalHost.Create(Engine);
        try
          InstallTest262HostGlobals(Engine, FHost, EvalHost);
          Engine.SuspendRealmExecutionContext;
          Engine.Execute;
          WaitForAsyncAgentWork;
        finally
          EvalHost.Free;
        end;
      finally
        Engine.Free;
      end;
    finally
      Source.Free;
    end;
  finally
    ShutdownThreadRuntime;
  end;
end;

function NormalizeTest262ListItem(const AValue: string): string;
begin
  Result := Trim(AValue);
  if (Result <> '') and (Result[Length(Result)] = ',') then
    Delete(Result, Length(Result), 1);
  Result := Trim(Result);
  if (Length(Result) >= 2) and
     (((Result[1] = '''') and
       (Result[Length(Result)] = '''')) or
      ((Result[1] = '"') and (Result[Length(Result)] = '"'))) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

function Test262SourceCanSuspendAgent(const APath: string): Boolean;
var
  Block: string;
  ClosePos: NativeInt;
  ColonPos: NativeInt;
  InFlags: Boolean;
  Key: string;
  Line: string;
  Lines: TStringList;
  OpenPos: NativeInt;
  Source: string;
  Trimmed: string;
  Value: string;
begin
  Result := True;
  if (APath = '') or not FileExists(APath) then
    Exit;
  Source := ReadUTF8FileText(APath);
  OpenPos := Pos('/*---', Source);
  if OpenPos <= 0 then
    Exit;
  ClosePos := PosEx('---*/', Source, OpenPos + Length('/*---'));
  if ClosePos <= OpenPos then
    Exit;
  Block := Copy(Source, OpenPos + Length('/*---'),
    ClosePos - OpenPos - Length('/*---'));
  Lines := TStringList.Create;
  try
    Lines.Text := Block;
    InFlags := False;
    for Line in Lines do
    begin
      Trimmed := Trim(Line);
      if Trimmed = '' then
        Continue;
      if (Line[1] <> ' ') and (Line[1] <> #9) then
      begin
        ColonPos := Pos(':', Line);
        if ColonPos <= 0 then
        begin
          InFlags := False;
          Continue;
        end;
        Key := Trim(Copy(Line, 1, ColonPos - 1));
        Value := Trim(Copy(Line, ColonPos + 1, MaxInt));
        InFlags := Key = 'flags';
        if InFlags then
        begin
          if Pos('CanBlockIsFalse', Value) > 0 then
            Exit(False);
          if Pos('CanBlockIsTrue', Value) > 0 then
            Exit(True);
        end;
        Continue;
      end;
      if InFlags and StartsStr('-', Trimmed) then
      begin
        Value := NormalizeTest262ListItem(
          Trim(Copy(Trimmed, 2, MaxInt)));
        if Value = 'CanBlockIsFalse' then
          Exit(False);
        if Value = 'CanBlockIsTrue' then
          Exit(True);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

constructor TGocciaTest262Host.Create(
  const AOptions: TGocciaTest262EngineOptions);
begin
  inherited Create;
  FImplementation := TTest262HostImplementation.Create(AOptions);
end;

destructor TGocciaTest262Host.Destroy;
begin
  FImplementation.Free;
  inherited;
end;

procedure TGocciaTest262Host.ConfigureHostEnvironment(
  const AParent: TGocciaHostEnvironment);
begin
  TTest262HostImplementation(FImplementation).ConfigureHostEnvironment(
    AParent);
end;

procedure TGocciaTest262Host.Install(const AEngine: TGocciaEngine);
begin
  TTest262HostImplementation(FImplementation).Install(AEngine);
end;

end.
