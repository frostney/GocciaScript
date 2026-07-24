unit Goccia.Test262.Host;

{$I Goccia.inc}

interface

uses
  Goccia.Engine,
  Goccia.Executor,
  Goccia.HostEnvironment,
  Goccia.SourcePipeline;

type
  TGocciaTest262ExecutionMode = (t262emInterpreted, t262emBytecode);

  TGocciaTest262EngineOptions = record
    Compatibility: TGocciaCompatibilityFlags;
    ExperimentalJSModuleSourceEnabled: Boolean;
    UnsafeShadowRealm: Boolean;
    AgentCanSuspend: Boolean;
    Mode: TGocciaTest262ExecutionMode;
    SourceType: TGocciaSourceType;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
    StackSize: Integer;
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

function CreateTest262Executor(
  const AMode: TGocciaTest262ExecutionMode): TGocciaExecutor;
procedure ConfigureTest262Engine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor;
  const AOptions: TGocciaTest262EngineOptions;
  const AParentEnvironment: TGocciaHostEnvironment = nil);
function Test262SourceCanSuspendAgent(const APath: string): Boolean;

implementation

uses
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils,

  CriticalSections,
  TextSemantics,
  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Builtins.Atomics,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Scope,
  Goccia.Scope.Redeclaration,
  Goccia.StackLimit,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
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
    function Eval(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TTest262Realm = class
  private
    FEngine: TGocciaEngine;
    FExecutor: TGocciaExecutor;
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

  TTest262ModuleContentProvider =
    class(TGocciaFileSystemModuleContentProvider)
  private
    function BuildHarnessPrelude(const APath, ASource: string): string;
    function FindSuiteRoot(const APath: string): string;
    function HarnessFilePath(const ASuiteRoot, AName: string): string;
    function ShouldPrependHarness(const APath: string): Boolean;
  public
    function LoadContent(const APath: string): TGocciaModuleContent; override;
  end;

function CreateTest262Executor(
  const AMode: TGocciaTest262ExecutionMode): TGocciaExecutor;
begin
  case AMode of
    t262emInterpreted: Result := TGocciaInterpreterExecutor.Create;
    t262emBytecode: Result := TGocciaBytecodeExecutor.Create;
  end;
end;

procedure ConfigureTest262Engine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor;
  const AOptions: TGocciaTest262EngineOptions;
  const AParentEnvironment: TGocciaHostEnvironment);
begin
  if Assigned(AParentEnvironment) then
    AEngine.HostEnvironment.ConfigureAsChildOf(AParentEnvironment);
  AEngine.Compatibility := AOptions.Compatibility;
  AEngine.LabelStatementsEnabled := True;
  AEngine.ForInLoopsEnabled := True;
  AEngine.ExperimentalJSModuleSourceEnabled :=
    AOptions.ExperimentalJSModuleSourceEnabled;
  AEngine.SourceType := AOptions.SourceType;
  AEngine.FunctionConstructor.Enabled := True;
  if AOptions.UnsafeShadowRealm then
    EnableShadowRealm(AEngine);
  SetAtomicsAgentCanSuspend(AOptions.AgentCanSuspend);
  AEngine.ModuleLoader.SetContentProvider(
    TTest262ModuleContentProvider.Create, True);
  if AExecutor is TGocciaBytecodeExecutor then
    TGocciaBytecodeExecutor(AExecutor).GlobalBackedTopLevel :=
      AOptions.SourceType = stScript;
end;

procedure InstallTest262EvalGlobal(const AEngine: TGocciaEngine;
  const AEvalHost: TTest262EvalHost);
var
  EvalFunction: TGocciaNativeFunctionValue;
  GlobalObject: TGocciaObjectValue;
begin
  EvalFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    AEvalHost.Eval, 'eval', 1);
  EvalFunction.DirectEvalHost := True;
  GlobalObject := TGocciaObjectValue(AEngine.Realm.GlobalObject);
  GlobalObject.DefineProperty('eval',
    TGocciaPropertyDescriptorData.Create(EvalFunction,
      [pfWritable, pfConfigurable]));
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
  InstallTest262EvalGlobal(AEngine, AEvalHost);
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

function HasEvalTopLevelUsingDeclaration(
  const AProgram: TGocciaProgram): Boolean;
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    if AProgram.Body[I] is TGocciaUsingDeclaration then
      Exit(True);
  Result := False;
end;

constructor TTest262EvalHost.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TTest262EvalHost.Eval(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  EvalOptions: TGocciaSourcePipelineOptions;
  EvalSource: TStringList;
  PipelineResult: TGocciaSourcePipelineResult;
  RealmScope: TGocciaExecutionContextScope;
  EvalScope: TGocciaScope;
  VarScope: TGocciaScope;
  EvalContext: TGocciaEvaluationContext;
  SourceValue: TGocciaValue;
  SourceText: string;
  StrictEval: Boolean;
begin
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  SourceValue := AArgs.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
    Exit(SourceValue);

  SourceText := TGocciaStringLiteralValue(SourceValue).Value;
  EvalSource := CreateECMAScriptSourceLines(SourceText);
  try
    EvalOptions := TGocciaSourcePipeline.DefaultOptions;
    EvalOptions.Preprocessors := FEngine.Preprocessors;
    EvalOptions.Compatibility := FEngine.Compatibility;
    EvalOptions.LabelStatementsEnabled := FEngine.LabelStatementsEnabled;
    EvalOptions.ForInLoopsEnabled := FEngine.ForInLoopsEnabled;
    EvalOptions.ExperimentalJSModuleSourceEnabled :=
      FEngine.ExperimentalJSModuleSourceEnabled;
    EvalOptions.WarningUnsupportedFeatures :=
      FEngine.WarningUnsupportedFeatures;
    EvalOptions.SourceType := stScript;
    ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(EvalOptions);
    try
      RealmScope := FEngine.ActivateRealmExecutionContext;
      try
        PipelineResult := TGocciaSourcePipeline.Parse(EvalSource,
          '<test262-realm-eval>', EvalOptions);
        try
          if HasEvalTopLevelUsingDeclaration(PipelineResult.ProgramNode) then
            ThrowSyntaxError(
              'Using declarations are not allowed at the top level of eval');
          StrictEval := HasUseStrictDirective(PipelineResult.ProgramNode);
          if StrictEval then
            EvalScope := FEngine.Interpreter.GlobalScope.CreateChild(
              skFunction, 'StrictTest262Eval')
          else
            EvalScope := FEngine.Interpreter.GlobalScope.CreateChild(
              skBlock, 'Test262Eval');
          EvalScope.ThisValue := FEngine.Interpreter.GlobalScope.ThisValue;
          if Assigned(TGarbageCollector.Instance) then
            TGarbageCollector.Instance.AddTempRoot(EvalScope);
          try
            EvalContext := FEngine.Interpreter.CreateEvaluationContext;
            EvalContext.Scope := EvalScope;
            EvalContext.CurrentFilePath := '<test262-realm-eval>';
            EvalContext.NonStrictMode := not StrictEval;
            if StrictEval then
              VarScope := EvalScope
            else
              VarScope := FEngine.Interpreter.GlobalScope;
            Result := EvaluateEvalProgram(PipelineResult.ProgramNode,
              EvalContext, VarScope, EvalScope, StrictEval, False, nil, False,
              False, False);
          finally
            if Assigned(TGarbageCollector.Instance) then
              TGarbageCollector.Instance.RemoveTempRoot(EvalScope);
          end;
        finally
          PipelineResult.Free;
        end;
      finally
        RealmScope.Free;
      end;
    finally
      ActiveOptionsScope.Free;
    end;
  finally
    EvalSource.Free;
  end;
end;

function TTest262EvalHost.EvalScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  ScriptOptions: TGocciaSourcePipelineOptions;
  ScriptSource: TStringList;
  PipelineResult: TGocciaSourcePipelineResult;
  RealmScope: TGocciaExecutionContextScope;
  SourceValue: TGocciaValue;
  SourceText: string;
begin
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  SourceValue := AArgs.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
    Exit(SourceValue);

  SourceText := TGocciaStringLiteralValue(SourceValue).Value;
  ScriptSource := CreateECMAScriptSourceLines(SourceText);
  try
    ScriptOptions := TGocciaSourcePipeline.DefaultOptions;
    ScriptOptions.Preprocessors := FEngine.Preprocessors;
    ScriptOptions.Compatibility := FEngine.Compatibility;
    ScriptOptions.LabelStatementsEnabled := FEngine.LabelStatementsEnabled;
    ScriptOptions.ForInLoopsEnabled := FEngine.ForInLoopsEnabled;
    ScriptOptions.ExperimentalJSModuleSourceEnabled :=
      FEngine.ExperimentalJSModuleSourceEnabled;
    ScriptOptions.WarningUnsupportedFeatures :=
      FEngine.WarningUnsupportedFeatures;
    ScriptOptions.SourceType := stScript;
    ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(ScriptOptions);
    try
      RealmScope := FEngine.ActivateRealmExecutionContext;
      try
        PipelineResult := TGocciaSourcePipeline.Parse(ScriptSource,
          '<test262-eval-script>', ScriptOptions);
        try
          CheckTopLevelRedeclarations(PipelineResult.ProgramNode,
            FEngine.Interpreter.GlobalScope, '<test262-eval-script>');
          Result := FEngine.ExecuteProgram(PipelineResult.ProgramNode);
        finally
          PipelineResult.Free;
        end;
      finally
        RealmScope.Free;
      end;
    finally
      ActiveOptionsScope.Free;
    end;
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
  FExecutor := CreateTest262Executor(AOptions.Mode);
  FEngine := TGocciaEngine.Create('<test262-realm>', FSource, FExecutor);
  ChildOptions := AOptions;
  ChildOptions.SourceType := stScript;
  ConfigureTest262Engine(FEngine, FExecutor, ChildOptions,
    AParentEnvironment);
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
  FExecutor.Free;
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
    CheckExecutionTimeout;
    Sleep(1);
  until False;
  CallbackArgs := TGocciaArgumentsCollection.Create([BroadcastValue]);
  try
    Result := TGocciaFunctionBase(Callback).Call(CallbackArgs,
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
    CheckExecutionTimeout;
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
    CheckExecutionTimeout;
    Sleep(1);
  until False;
end;

procedure TTest262AgentThread.Execute;
var
  AgentOptions: TGocciaTest262EngineOptions;
  Engine: TGocciaEngine;
  EvalHost: TTest262EvalHost;
  Executor: TGocciaExecutor;
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
      Executor := CreateTest262Executor(FOptions.Mode);
      try
        Engine := TGocciaEngine.Create('<test262-agent>', Source, Executor);
        try
          AgentOptions := FOptions;
          AgentOptions.SourceType := stScript;
          ConfigureTest262Engine(Engine, Executor, AgentOptions,
            FHost.HostEnvironment);
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
        Executor.Free;
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

procedure AddTest262ListItem(const AItems: TStrings; const AValue: string);
var
  Item: string;
begin
  Item := NormalizeTest262ListItem(AValue);
  if (Item <> '') and (AItems.IndexOf(Item) < 0) then
    AItems.Add(Item);
end;

procedure AddTest262InlineList(const AItems: TStrings; const AValue: string);
var
  DelimiterPos: Integer;
  Item: string;
  ListText: string;
begin
  ListText := Trim(AValue);
  if ListText = '' then
    Exit;
  if ListText[1] = '[' then
  begin
    Delete(ListText, 1, 1);
    DelimiterPos := Pos(']', ListText);
    if DelimiterPos > 0 then
      ListText := Copy(ListText, 1, DelimiterPos - 1);
  end;
  repeat
    DelimiterPos := Pos(',', ListText);
    if DelimiterPos > 0 then
    begin
      Item := Copy(ListText, 1, DelimiterPos - 1);
      Delete(ListText, 1, DelimiterPos);
    end
    else
    begin
      Item := ListText;
      ListText := '';
    end;
    AddTest262ListItem(AItems, Item);
  until DelimiterPos = 0;
end;

procedure ReadTest262Frontmatter(const ASource: string;
  const AIncludes: TStrings; out ARaw: Boolean);
var
  Block: string;
  ClosePos: NativeInt;
  ColonPos: NativeInt;
  Key: string;
  Line: string;
  Lines: TStringList;
  OpenPos: NativeInt;
  Trimmed: string;
  Value: string;
  InFlags: Boolean;
  InIncludes: Boolean;
begin
  ARaw := False;
  OpenPos := Pos('/*---', ASource);
  if OpenPos <= 0 then
    Exit;
  ClosePos := PosEx('---*/', ASource, OpenPos + Length('/*---'));
  if ClosePos <= OpenPos then
    Exit;
  Block := Copy(ASource, OpenPos + Length('/*---'),
    ClosePos - OpenPos - Length('/*---'));
  Lines := TStringList.Create;
  try
    Lines.Text := Block;
    InFlags := False;
    InIncludes := False;
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
          InIncludes := False;
          Continue;
        end;
        Key := Trim(Copy(Line, 1, ColonPos - 1));
        Value := Trim(Copy(Line, ColonPos + 1, MaxInt));
        InFlags := Key = 'flags';
        InIncludes := Key = 'includes';
        if InFlags then
          ARaw := Pos('raw', Value) > 0
        else if InIncludes then
          AddTest262InlineList(AIncludes, Value);
        Continue;
      end;
      if (InIncludes or InFlags) and StartsStr('-', Trimmed) then
      begin
        Value := Trim(Copy(Trimmed, 2, MaxInt));
        if InFlags then
          ARaw := ARaw or (NormalizeTest262ListItem(Value) = 'raw')
        else
          AddTest262ListItem(AIncludes, Value);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TTest262ModuleContentProvider.FindSuiteRoot(
  const APath: string): string;
var
  Candidate: string;
  DirectoryPath: string;
  ParentDirectory: string;
begin
  Result := '';
  DirectoryPath := ExcludeTrailingPathDelimiter(
    ExtractFilePath(ExpandFileName(APath)));
  while DirectoryPath <> '' do
  begin
    Candidate := IncludeTrailingPathDelimiter(DirectoryPath) +
      'harness' + PathDelim + 'assert.js';
    if FileExists(Candidate) and
       DirectoryExists(IncludeTrailingPathDelimiter(DirectoryPath) +
         'test') then
      Exit(DirectoryPath);
    ParentDirectory := ExtractFileDir(DirectoryPath);
    if (ParentDirectory = '') or (ParentDirectory = DirectoryPath) then
      Break;
    DirectoryPath := ParentDirectory;
  end;
end;

function TTest262ModuleContentProvider.HarnessFilePath(
  const ASuiteRoot, AName: string): string;
var
  BundledPath: string;
begin
  if (AName = '$262.js') or (AName = 'goccia-global-shim.js') then
  begin
    BundledPath := ExpandFileName('scripts' + PathDelim +
      'test262_harness' + PathDelim + AName);
    if FileExists(BundledPath) then
      Exit(BundledPath);
  end;
  Result := IncludeTrailingPathDelimiter(ASuiteRoot) + 'harness' +
    PathDelim + AName;
end;

function TTest262ModuleContentProvider.ShouldPrependHarness(
  const APath: string): Boolean;
var
  AbsolutePath: string;
  SuiteRoot: string;
  TestRoot: string;
begin
  if LowerCase(ExtractFileExt(APath)) <> '.js' then
    Exit(False);
  if EndsStr('_FIXTURE.js', ExtractFileName(APath)) then
    Exit(False);
  SuiteRoot := FindSuiteRoot(APath);
  if SuiteRoot = '' then
    Exit(False);
  AbsolutePath := ExpandFileName(APath);
  TestRoot := IncludeTrailingPathDelimiter(SuiteRoot) + 'test' + PathDelim;
  Result := StartsStr(TestRoot, AbsolutePath);
end;

function TTest262ModuleContentProvider.BuildHarnessPrelude(
  const APath, ASource: string): string;
var
  I: Integer;
  IncludeName: string;
  Includes: TStringList;
  Raw: Boolean;
  SuiteRoot: string;
begin
  Result := '';
  SuiteRoot := FindSuiteRoot(APath);
  if SuiteRoot = '' then
    Exit;
  Includes := TStringList.Create;
  try
    Includes.Add('$262.js');
    Includes.Add('sta.js');
    Includes.Add('assert.js');
    ReadTest262Frontmatter(ASource, Includes, Raw);
    if Raw then
      Exit;
    for I := 0 to Includes.Count - 1 do
    begin
      IncludeName := Includes[I];
      if Result <> '' then
        Result := Result + sLineBreak;
      Result := Result + ReadUTF8FileText(
        HarnessFilePath(SuiteRoot, IncludeName));
    end;
    if Result <> '' then
      Result := Result + sLineBreak + ReadUTF8FileText(
        HarnessFilePath(SuiteRoot, 'goccia-global-shim.js'));
  finally
    Includes.Free;
  end;
end;

function TTest262ModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  HarnessPrelude: string;
  OriginalContent: TGocciaModuleContent;
begin
  OriginalContent := inherited LoadContent(APath);
  if not ShouldPrependHarness(APath) then
    Exit(OriginalContent);
  try
    HarnessPrelude := BuildHarnessPrelude(APath, OriginalContent.Text);
    if HarnessPrelude = '' then
      Exit(OriginalContent);
    Result := TGocciaModuleContent.Create(HarnessPrelude + sLineBreak +
      OriginalContent.Text, OriginalContent.LastModified);
  except
    OriginalContent.Free;
    raise;
  end;
  OriginalContent.Free;
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
