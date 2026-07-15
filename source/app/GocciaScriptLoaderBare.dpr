program GocciaScriptLoaderBare;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  StrUtils,
  SyncObjs,
  SysUtils,

  CLI.Parser,
  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Builtins.Atomics,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.CLI.Options,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.HostEnvironment,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.Scope,
  Goccia.Scope.Redeclaration,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.StackLimit,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

type
  TBarePrintHost = class
  public
    function Print(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareExecutionMode = (bemInterpreted, bemBytecode);

  TBareOptions = record
    Compatibility: TGocciaCompatibilityFlags;
    LabelStatementsEnabled: Boolean;
    ForInLoopsEnabled: Boolean;
    ExperimentalJSModuleSourceEnabled: Boolean;
    WarningUnsupportedFeatures: Boolean;
    StrictTypes: Boolean;
    UnsafeFunctionConstructor: Boolean;
    UnsafeShadowRealm: Boolean;
    Deterministic: Boolean;
    Test262Host: Boolean;
    Test262AgentCanSuspend: Boolean;
    Print: Boolean;
    Mode: TBareExecutionMode;
    SourceType: TGocciaSourceType;
    SourceTypeExplicit: Boolean;
    FileName: string;
    SourceName: string;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
    StackSize: Integer;
    ProfileModePresent: Boolean;
    ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
    ProfileOutputPath: string;
  end;

  TBareTest262Realm = class;
  TBareTest262AgentHost = class;
  TBareTest262AgentThread = class;
  TBareTest262Host = class;

  TGocciaTest262IsHTMLDDAValue = class(TGocciaObjectValue)
  public
    function HasHTMLDDAInternalSlot: Boolean; override;
    function TypeOf: string; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
  end;

  TBareTest262EvalHost = class
  private
    FEngine: TGocciaEngine;
  public
    constructor Create(const AEngine: TGocciaEngine);
    function Eval(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareTest262Realm = class
  private
    FEngine: TGocciaEngine;
    FExecutor: TGocciaExecutor;
    FEvalHost: TBareTest262EvalHost;
    FTest262Host: TBareTest262Host;
    FSource: TStringList;
  public
    constructor Create(const AOptions: TBareOptions;
      const AParentEnvironment: TGocciaHostEnvironment);
    destructor Destroy; override;
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GlobalObject: TGocciaObjectValue;
    property Engine: TGocciaEngine read FEngine;
  end;

  TBareTest262Host = class
  private
    FAgentHost: TBareTest262AgentHost;
    FHostEnvironment: TGocciaHostEnvironment;
    FOptions: TBareOptions;
    FRealms: TObjectList<TBareTest262Realm>;
  public
    constructor Create(const AOptions: TBareOptions);
    destructor Destroy; override;
    procedure ConfigureHostEnvironment(
      const AParent: TGocciaHostEnvironment);
    function CreateAgentObject: TGocciaObjectValue;
    function CreateRealm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    property AgentHost: TBareTest262AgentHost read FAgentHost;
    property HostEnvironment: TGocciaHostEnvironment read FHostEnvironment;
  end;

  TBareTest262AgentHost = class
  private
    FBroadcastGeneration: Integer;
    FBroadcastValue: TGocciaValue;
    FLock: TRTLCriticalSection;
    FOptions: TBareOptions;
    FReports: TStringList;
    FShuttingDown: Boolean;
    FTest262Host: TBareTest262Host;
    FWorkers: TObjectList<TBareTest262AgentThread>;
    procedure PumpCurrentThreadWork;
  public
    constructor Create(const AOptions: TBareOptions;
      const ATest262Host: TBareTest262Host);
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

  TBareTest262AgentThread = class(TThread)
  private
    FHost: TBareTest262Host;
    FOptions: TBareOptions;
    FSourceText: string;
    procedure WaitForAsyncAgentWork;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHost: TBareTest262Host;
      const AOptions: TBareOptions; const ASourceText: string);
  end;

  TBareTest262ModuleContentProvider = class(TGocciaFileSystemModuleContentProvider)
  private
    function BuildHarnessPrelude(const APath: string;
      const ASource: UTF8String): UTF8String;
    function FindSuiteRoot(const APath: string): string;
    function HarnessFilePath(const ASuiteRoot, AName: string): string;
    function ShouldPrependHarness(const APath: string): Boolean;
  public
    function LoadContent(const APath: string): TGocciaModuleContent; override;
  end;

const
  BARE_PRINT_GLOBAL_NAME = 'print';
  TEST262_TIMEOUT_MARKER = 'GocciaTest262:Timeout';
  TEST262_TIMEOUT_EXIT_CODE = 124;
  // Finite default call-stack depth for the bare loader.  Without a bound,
  // genuinely unbounded recursion runs until OOM or the (optional) instruction
  // limit instead of throwing RangeError like a conforming engine.  Proper
  // tail calls reuse their frame, so this limit only fences off deep *non-tail*
  // recursion; the value is generous to avoid clipping legitimate recursion.
  BARE_DEFAULT_MAX_STACK_DEPTH = 10000;

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor; const AOptions: TBareOptions;
  const AParentEnvironment: TGocciaHostEnvironment = nil); forward;
function CreateExecutorForMode(
  const AMode: TBareExecutionMode): TGocciaExecutor; forward;

procedure InstallTest262EvalGlobal(const AEngine: TGocciaEngine;
  const AEvalHost: TBareTest262EvalHost);
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
  const ATest262Host: TBareTest262Host;
  const ATest262EvalHost: TBareTest262EvalHost);
var
  Test262Obj: TGocciaObjectValue;
begin
  AEngine.GocciaGlobal.AssignProperty('test262Host',
    TGocciaBooleanLiteralValue.TrueValue);
  Test262Obj := TGocciaObjectValue.Create;
  Test262Obj.AssignProperty('createRealm',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ATest262Host.CreateRealm, 'createRealm', 0));
  Test262Obj.AssignProperty('evalScript',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ATest262EvalHost.EvalScript, 'evalScript', 1));
  Test262Obj.AssignProperty('abstractModuleSourcePrototype',
    TGocciaModuleSourceValue.SharedPrototype);
  Test262Obj.AssignProperty('isHTMLDDA', TGocciaTest262IsHTMLDDAValue.Create);
  Test262Obj.AssignProperty('agent', ATest262Host.CreateAgentObject);
  AEngine.GocciaGlobal.AssignProperty('test262', Test262Obj);
end;

function TGocciaTest262IsHTMLDDAValue.HasHTMLDDAInternalSlot: Boolean;
begin
  Result := True;
end;

function TGocciaTest262IsHTMLDDAValue.TypeOf: string;
begin
  Result := 'undefined';
end;

function TGocciaTest262IsHTMLDDAValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TBarePrintHost.Print(const AArgs: TGocciaArgumentsCollection;
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

constructor TBareTest262EvalHost.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TBareTest262EvalHost.Eval(const AArgs: TGocciaArgumentsCollection;
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
  EvalSource := TStringList.Create;
  try
    EvalSource.Text := SourceText;
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
            EvalScope := FEngine.Interpreter.GlobalScope.CreateChild(skFunction,
              'StrictTest262Eval')
          else
            EvalScope := FEngine.Interpreter.GlobalScope.CreateChild(skBlock,
              'Test262Eval');
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

function TBareTest262EvalHost.EvalScript(
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
  ScriptSource := TStringList.Create;
  try
    ScriptSource.Text := SourceText;
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

constructor TBareTest262Realm.Create(const AOptions: TBareOptions;
  const AParentEnvironment: TGocciaHostEnvironment);
var
  ChildOptions: TBareOptions;
begin
  inherited Create;
  FSource := TStringList.Create;
  FExecutor := CreateExecutorForMode(AOptions.Mode);
  FEngine := TGocciaEngine.Create('<test262-realm>', FSource, FExecutor);
  ChildOptions := AOptions;
  ChildOptions.SourceType := stScript;
  ChildOptions.Test262Host := True;
  ConfigureEngine(FEngine, FExecutor, ChildOptions, AParentEnvironment);

  FEvalHost := TBareTest262EvalHost.Create(FEngine);
  FTest262Host := TBareTest262Host.Create(ChildOptions);
  FTest262Host.ConfigureHostEnvironment(FEngine.HostEnvironment);
  FEngine.RefreshGlobalThis;
  InstallTest262HostGlobals(FEngine, FTest262Host, FEvalHost);
  InstallTest262EvalGlobal(FEngine, FEvalHost);
  FEngine.SuspendRealmExecutionContext;
end;

destructor TBareTest262Realm.Destroy;
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

function TBareTest262Realm.GlobalObject: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue(FEngine.Realm.GlobalObject);
end;

function TBareTest262Realm.EvalScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FEvalHost.EvalScript(AArgs, AThisValue);
end;

constructor TBareTest262Host.Create(const AOptions: TBareOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FAgentHost := TBareTest262AgentHost.Create(AOptions, Self);
  FRealms := TObjectList<TBareTest262Realm>.Create(True);
end;

destructor TBareTest262Host.Destroy;
begin
  FAgentHost.Free;
  FRealms.Free;
  FHostEnvironment.Free;
  inherited;
end;

procedure TBareTest262Host.ConfigureHostEnvironment(
  const AParent: TGocciaHostEnvironment);
begin
  FreeAndNil(FHostEnvironment);
  FHostEnvironment := TGocciaHostEnvironment.Create;
  FHostEnvironment.ConfigureAsChildOf(AParent);
end;

function TBareTest262Host.CreateAgentObject: TGocciaObjectValue;
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

function TBareTest262Host.CreateRealm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Realm: TBareTest262Realm;
  RealmRecord: TGocciaObjectValue;
begin
  Realm := TBareTest262Realm.Create(FOptions, FHostEnvironment);
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

constructor TBareTest262AgentHost.Create(const AOptions: TBareOptions;
  const ATest262Host: TBareTest262Host);
begin
  inherited Create;
  FOptions := AOptions;
  FTest262Host := ATest262Host;
  FReports := TStringList.Create;
  FWorkers := TObjectList<TBareTest262AgentThread>.Create(True);
  FBroadcastGeneration := 0;
  FBroadcastValue := nil;
  FShuttingDown := False;
  InitCriticalSection(FLock);
end;

destructor TBareTest262AgentHost.Destroy;
var
  Worker: TBareTest262AgentThread;
begin
  EnterCriticalSection(FLock);
  try
    FShuttingDown := True;
  finally
    LeaveCriticalSection(FLock);
  end;

  for Worker in FWorkers do
    Worker.WaitFor;

  EnterCriticalSection(FLock);
  try
    if Assigned(FBroadcastValue) and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveQueuedRoot(FBroadcastValue);
    FBroadcastValue := nil;
  finally
    LeaveCriticalSection(FLock);
  end;
  FWorkers.Free;
  DoneCriticalSection(FLock);
  FReports.Free;
  inherited;
end;

procedure TBareTest262AgentHost.PumpCurrentThreadWork;
var
  Queue: TGocciaMicrotaskQueue;
begin
  PumpAtomicsWaitAsyncCompletions;
  Queue := TGocciaMicrotaskQueue.Instance;
  if Assigned(Queue) and Queue.HasPending then
    Queue.DrainOneJob;
end;

function TBareTest262AgentHost.Broadcast(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  EnterCriticalSection(FLock);
  try
    if Assigned(FBroadcastValue) and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveQueuedRoot(FBroadcastValue);
    FBroadcastValue := Value;
    if Assigned(FBroadcastValue) and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddQueuedRoot(FBroadcastValue);
    Inc(FBroadcastGeneration);
  finally
    LeaveCriticalSection(FLock);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TBareTest262AgentHost.GetReport(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ReportText: string;
begin
  PumpCurrentThreadWork;
  EnterCriticalSection(FLock);
  try
    if FReports.Count = 0 then
      Exit(TGocciaNullLiteralValue.NullValue);
    ReportText := FReports[0];
    FReports.Delete(0);
  finally
    LeaveCriticalSection(FLock);
  end;
  Result := TGocciaStringLiteralValue.Create(ReportText);
end;

function TBareTest262AgentHost.Leaving(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  PumpCurrentThreadWork;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TBareTest262AgentHost.MonotonicNow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(GetTickCount64);
end;

function TBareTest262AgentHost.ReceiveBroadcast(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  BroadcastValue: TGocciaValue;
  Callback: TGocciaValue;
  CallbackArgs: TGocciaArgumentsCollection;
begin
  if (AArgs.Length = 0) or (not AArgs.GetElement(0).IsCallable) then
    ThrowTypeError('$262.agent.receiveBroadcast callback must be callable');

  Callback := AArgs.GetElement(0);
  BroadcastValue := nil;
  repeat
    EnterCriticalSection(FLock);
    try
      if FShuttingDown then
        Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
      if FBroadcastGeneration > 0 then
      begin
        BroadcastValue := FBroadcastValue;
        Break;
      end;
    finally
      LeaveCriticalSection(FLock);
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

function TBareTest262AgentHost.Report(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ReportText: string;
begin
  if AArgs.Length > 0 then
    ReportText := AArgs.GetElement(0).ToStringLiteral.Value
  else
    ReportText := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  EnterCriticalSection(FLock);
  try
    FReports.Add(ReportText);
  finally
    LeaveCriticalSection(FLock);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TBareTest262AgentHost.SleepAgent(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Deadline: QWord;
  Milliseconds: Int64;
  NowMilliseconds: QWord;
  Remaining: Int64;
begin
  if AArgs.Length = 0 then
    Milliseconds := 0
  else
    Milliseconds := ToInt64Value(AArgs.GetElement(0));
  if Milliseconds < 0 then
    Milliseconds := 0;

  Deadline := GetTickCount64 + QWord(Milliseconds);
  repeat
    PumpCurrentThreadWork;
    CheckInstructionLimit;
    CheckExecutionTimeout;
    NowMilliseconds := GetTickCount64;
    if NowMilliseconds >= Deadline then
      Break;
    Remaining := Int64(Deadline - NowMilliseconds);
    if Remaining > 5 then
      Sleep(5)
    else
      Sleep(LongWord(Remaining));
  until False;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TBareTest262AgentHost.Start(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  SourceText: string;
  Worker: TBareTest262AgentThread;
begin
  if (AArgs.Length = 0) or
     (not (AArgs.GetElement(0) is TGocciaStringLiteralValue)) then
    ThrowTypeError('$262.agent.start source must be a string');

  SourceText := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  EnterCriticalSection(FLock);
  try
    if FShuttingDown then
      Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
    Worker := TBareTest262AgentThread.Create(FTest262Host, FOptions, SourceText);
    FWorkers.Add(Worker);
    Worker.Start;
  finally
    LeaveCriticalSection(FLock);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

constructor TBareTest262AgentThread.Create(const AHost: TBareTest262Host;
  const AOptions: TBareOptions; const ASourceText: string);
const
  AGENT_STACK_SIZE = 8 * 1024 * 1024;
begin
  inherited Create(True, AGENT_STACK_SIZE);
  FreeOnTerminate := False;
  FHost := AHost;
  FOptions := AOptions;
  FSourceText := ASourceText;
end;

procedure TBareTest262AgentThread.WaitForAsyncAgentWork;
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

procedure TBareTest262AgentThread.Execute;
var
  AgentOptions: TBareOptions;
  Engine: TGocciaEngine;
  EvalHost: TBareTest262EvalHost;
  Executor: TGocciaExecutor;
  Source: TStringList;
begin
  InitThreadRuntime(False, FOptions.MaxMemoryBytes);
  StartExecutionTimeout(FOptions.TimeoutMs);
  StartInstructionLimit(FOptions.MaxInstructions);
  Source := TStringList.Create;
  try
    Source.Text := ReadUTF8FileText(ExpandFileName('scripts' + PathDelim +
      'test262_harness' + PathDelim + '$262.js')) + LineEnding + FSourceText;
    Executor := CreateExecutorForMode(FOptions.Mode);
    try
      Engine := TGocciaEngine.Create('<test262-agent>', Source, Executor);
      try
        AgentOptions := FOptions;
        AgentOptions.SourceType := stScript;
        AgentOptions.Test262Host := True;
        ConfigureEngine(Engine, Executor, AgentOptions,
          FHost.HostEnvironment);
        EvalHost := TBareTest262EvalHost.Create(Engine);
        try
          InstallTest262HostGlobals(Engine, FHost, EvalHost);
          Engine.RefreshGlobalThis;
          InstallTest262EvalGlobal(Engine, EvalHost);
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
     (((Result[1] = '''') and (Result[Length(Result)] = '''')) or
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
  I: Integer;
  Item: string;
  ListText: string;
  Parts: TStringArray;
begin
  ListText := Trim(AValue);
  if ListText = '' then
    Exit;

  if (ListText[1] = '[') then
  begin
    Delete(ListText, 1, 1);
    I := Pos(']', ListText);
    if I > 0 then
      ListText := Copy(ListText, 1, I - 1);
  end;

  Parts := ListText.Split([',']);
  for Item in Parts do
    AddTest262ListItem(AItems, Item);
end;

procedure ReadTest262Frontmatter(const ASource: UTF8String;
  const AIncludes: TStrings; out ARaw: Boolean);
var
  Block: string;
  ClosePos: SizeInt;
  ColonPos: SizeInt;
  Key: string;
  Line: string;
  Lines: TStringList;
  OpenPos: SizeInt;
  Trimmed: string;
  Value: string;
  InFlags: Boolean;
  InIncludes: Boolean;
begin
  ARaw := False;
  OpenPos := Pos('/*---', string(ASource));
  if OpenPos <= 0 then
    Exit;
  ClosePos := PosEx('---*/', string(ASource), OpenPos + Length('/*---'));
  if ClosePos <= OpenPos then
    Exit;

  Block := Copy(string(ASource), OpenPos + Length('/*---'),
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
        begin
          AddTest262InlineList(AIncludes, '');
          ARaw := Pos('raw', Value) > 0;
        end
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

function Test262SourceCanSuspendAgent(const APath: string): Boolean;
var
  Block: string;
  ClosePos: SizeInt;
  ColonPos: SizeInt;
  InFlags: Boolean;
  Key: string;
  Line: string;
  Lines: TStringList;
  OpenPos: SizeInt;
  Source: UTF8String;
  Trimmed: string;
  Value: string;
begin
  Result := True;
  if (APath = '') or (not FileExists(APath)) then
    Exit;

  Source := ReadUTF8FileText(APath);
  OpenPos := Pos('/*---', string(Source));
  if OpenPos <= 0 then
    Exit;
  ClosePos := PosEx('---*/', string(Source), OpenPos + Length('/*---'));
  if ClosePos <= OpenPos then
    Exit;

  Block := Copy(string(Source), OpenPos + Length('/*---'),
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
        Value := NormalizeTest262ListItem(Trim(Copy(Trimmed, 2, MaxInt)));
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

function TBareTest262ModuleContentProvider.FindSuiteRoot(
  const APath: string): string;
var
  Candidate: string;
  Dir: string;
  ParentDir: string;
begin
  Result := '';
  Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(APath)));
  while Dir <> '' do
  begin
    Candidate := IncludeTrailingPathDelimiter(Dir) + 'harness' + PathDelim +
      'assert.js';
    if FileExists(Candidate) and
       DirectoryExists(IncludeTrailingPathDelimiter(Dir) + 'test') then
      Exit(Dir);

    ParentDir := ExtractFileDir(Dir);
    if (ParentDir = '') or (ParentDir = Dir) then
      Break;
    Dir := ParentDir;
  end;
end;

function TBareTest262ModuleContentProvider.HarnessFilePath(
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

  Result := IncludeTrailingPathDelimiter(ASuiteRoot) + 'harness' + PathDelim +
    AName;
end;

function TBareTest262ModuleContentProvider.ShouldPrependHarness(
  const APath: string): Boolean;
var
  AbsPath: string;
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

  AbsPath := ExpandFileName(APath);
  TestRoot := IncludeTrailingPathDelimiter(SuiteRoot) + 'test' + PathDelim;
  Result := StartsStr(TestRoot, AbsPath);
end;

function TBareTest262ModuleContentProvider.BuildHarnessPrelude(
  const APath: string; const ASource: UTF8String): UTF8String;
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
        Result := Result + LineEnding;
      Result := Result + ReadUTF8FileText(
        HarnessFilePath(SuiteRoot, IncludeName));
    end;
    if Result <> '' then
      Result := Result + LineEnding + ReadUTF8FileText(
        HarnessFilePath(SuiteRoot, 'goccia-global-shim.js'));
  finally
    Includes.Free;
  end;
end;

function TBareTest262ModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  HarnessPrelude: UTF8String;
  OriginalContent: TGocciaModuleContent;
begin
  OriginalContent := inherited LoadContent(APath);
  if not ShouldPrependHarness(APath) then
    Exit(OriginalContent);

  try
    HarnessPrelude := BuildHarnessPrelude(APath, OriginalContent.Text);
    if HarnessPrelude = '' then
      Exit(OriginalContent);

    Result := TGocciaModuleContent.Create(HarnessPrelude + LineEnding +
      OriginalContent.Text, OriginalContent.LastModified);
  except
    OriginalContent.Free;
    raise;
  end;
  OriginalContent.Free;
end;

procedure PrintUsage;
var
  Flag: TGocciaCompatibility;
  Descriptor: TGocciaCompatibilityFlagDescriptor;
begin
  WriteLn('Usage: GocciaScriptLoaderBare [file|-] [options]');
  WriteLn('');
  WriteLn('Options:');
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
  begin
    Descriptor := CompatibilityFlagDescriptor(Flag);
    WriteLn(Format('  --%-28s %s', [Descriptor.OptionName,
      Descriptor.HelpText]));
  end;
  WriteLn('  --strict-types                Enforce type annotations at runtime');
  WriteLn('  --warning-unsupported-features');
  WriteLn('                                Warn and recover for unsupported/default-disabled syntax');
  WriteLn('  --mode=interpreted|bytecode   Execution mode (default: interpreted)');
  WriteLn('  --source-type=script|module   Load entry as script source or module source (.mjs infers module)');
  WriteLn('  --source-name=PATH            Name stdin source as PATH for diagnostics and module resolution');
  WriteLn('  --unsafe-function-constructor Enable dynamic Function constructor');
  WriteLn('  --unsafe-shadowrealm          Enable the ShadowRealm constructor');
  WriteLn('  --deterministic               Use fixed script-visible time, UTC, and seeded randomness');
  WriteLn('  --test262-host                Expose Goccia.test262 host hooks for test262');
  WriteLn('  --print                       Print the script''s last value (incl. undefined)');
  WriteLn('  --timeout=MS                  Per-file cooperative timeout in milliseconds');
  WriteLn('  --max-memory=BYTES            GC heap byte limit (RangeError on exceed)');
  WriteLn('  --max-instructions=N          Maximum bytecode steps before aborting');
  WriteLn('  --stack-size=N                Maximum call stack depth (0 = no limit)');
  WriteLn('  --profile=opcodes|functions|all');
  WriteLn('                                Enable bytecode VM profiling (forces bytecode mode)');
  WriteLn('  --profile-output=PATH         Write profiler JSON to PATH');
  WriteLn('  --help                        Show this help');
end;

procedure ParseMode(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'interpreted' then
    AOptions.Mode := bemInterpreted
  else if AValue = 'bytecode' then
    AOptions.Mode := bemBytecode
  else
    raise Exception.Create('Invalid --mode value: ' + AValue);
end;

procedure ParseSourceType(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'script' then
    AOptions.SourceType := stScript
  else if AValue = 'module' then
    AOptions.SourceType := stModule
  else
    raise Exception.Create('Invalid --source-type value: ' + AValue);
  AOptions.SourceTypeExplicit := True;
end;

procedure ParseTimeout(const AValue: string; var AOptions: TBareOptions);
var
  Parsed: Integer;
begin
  if not TryStrToInt(AValue, Parsed) then
    raise Exception.Create('Invalid --timeout value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--timeout must be 0 or greater');
  AOptions.TimeoutMs := Parsed;
end;

procedure ParseMaxMemory(const AValue: string; var AOptions: TBareOptions);
var
  Parsed: Int64;
begin
  if not TryStrToInt64(AValue, Parsed) then
    raise Exception.Create('Invalid --max-memory value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--max-memory must be 0 or greater');
  AOptions.MaxMemoryBytes := Parsed;
end;

procedure ParseMaxInstructions(const AValue: string;
  var AOptions: TBareOptions);
var
  Parsed: Int64;
begin
  if not TryStrToInt64(AValue, Parsed) then
    raise Exception.Create('Invalid --max-instructions value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--max-instructions must be 0 or greater');
  AOptions.MaxInstructions := Parsed;
end;

procedure ParseStackSize(const AValue: string; var AOptions: TBareOptions);
var
  Parsed: Integer;
begin
  if not TryStrToInt(AValue, Parsed) then
    raise Exception.Create('Invalid --stack-size value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--stack-size must be 0 or greater');
  AOptions.StackSize := Parsed;
end;

procedure ParseProfileMode(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'opcodes' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmOpcodes
  else if AValue = 'functions' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmFunctions
  else if AValue = 'all' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmAll
  else
    raise Exception.Create('Invalid --profile value: ' + AValue);
  AOptions.ProfileModePresent := True;
  AOptions.Mode := bemBytecode;
end;

procedure InitializeProfiler(const AOptions: TBareOptions);
begin
  if not AOptions.ProfileModePresent then
    Exit;

  TGocciaProfiler.Initialize;
  TGocciaProfiler.Instance.Enabled := True;
  case AOptions.ProfileMode of
    Goccia.CLI.Options.pmOpcodes:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes];
    Goccia.CLI.Options.pmFunctions:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmFunctions];
    Goccia.CLI.Options.pmAll:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes,
        Goccia.Profiler.pmFunctions];
  end;
end;

procedure WriteProfilerReport(const AOptions: TBareOptions);
begin
  if (not AOptions.ProfileModePresent) or
     (AOptions.ProfileOutputPath = '') or
     (not Assigned(TGocciaProfiler.Instance)) then
    Exit;

  WriteProfileJSON(TGocciaProfiler.Instance, AOptions.ProfileOutputPath);
end;

procedure ParseOptions(out AOptions: TBareOptions);
var
  Arguments: TCommandLineArguments;
  I: Integer;
  Arg: string;
  SourceMetadataPath: string;
begin
  AOptions.Compatibility := [];
  AOptions.LabelStatementsEnabled := False;
  AOptions.ForInLoopsEnabled := False;
  AOptions.ExperimentalJSModuleSourceEnabled := False;
  AOptions.WarningUnsupportedFeatures := False;
  AOptions.StrictTypes := False;
  AOptions.UnsafeFunctionConstructor := False;
  AOptions.UnsafeShadowRealm := False;
  AOptions.Deterministic := False;
  AOptions.Test262Host := False;
  AOptions.Test262AgentCanSuspend := True;
  AOptions.Print := False;
  AOptions.Mode := bemInterpreted;
  AOptions.SourceType := stScript;
  AOptions.SourceTypeExplicit := False;
  AOptions.FileName := STDIN_PATH_MARKER;
  AOptions.SourceName := '';
  AOptions.TimeoutMs := 0;
  AOptions.MaxMemoryBytes := 0;
  AOptions.MaxInstructions := 0;
  AOptions.StackSize := BARE_DEFAULT_MAX_STACK_DEPTH;
  AOptions.ProfileModePresent := False;
  AOptions.ProfileMode := Goccia.CLI.Options.pmAll;
  AOptions.ProfileOutputPath := '';

  Arguments := GetCommandLineArguments;
  for I := 0 to High(Arguments) do
  begin
    Arg := Arguments[I];
    if Arg = '--help' then
    begin
      PrintUsage;
      Halt(0);
    end
    else if Arg = '--compat-label' then
      AOptions.LabelStatementsEnabled := True
    else if Arg = '--compat-for-in-loop' then
      AOptions.ForInLoopsEnabled := True
    else if Arg = '--experimental-js-module-source' then
      AOptions.ExperimentalJSModuleSourceEnabled := True
    else if TryApplyCompatibilityFlagArg(Arg, AOptions.Compatibility) then
    begin
      { handled by source compatibility flag registry }
    end
    else if Arg = '--warning-unsupported-features' then
      AOptions.WarningUnsupportedFeatures := True
    else if Arg = '--strict-types' then
      AOptions.StrictTypes := True
    else if Arg = '--unsafe-function-constructor' then
      AOptions.UnsafeFunctionConstructor := True
    else if Arg = '--unsafe-shadowrealm' then
      AOptions.UnsafeShadowRealm := True
    else if Arg = '--deterministic' then
      AOptions.Deterministic := True
    else if Arg = '--test262-host' then
      AOptions.Test262Host := True
    else if Arg = '--print' then
      AOptions.Print := True
    else if Copy(Arg, 1, Length('--mode=')) = '--mode=' then
      ParseMode(Copy(Arg, Length('--mode=') + 1, MaxInt), AOptions)
    else if Copy(Arg, 1, Length('--source-type=')) = '--source-type=' then
      ParseSourceType(Copy(Arg, Length('--source-type=') + 1, MaxInt),
        AOptions)
    else if Copy(Arg, 1, Length('--source-name=')) = '--source-name=' then
      AOptions.SourceName := Copy(Arg, Length('--source-name=') + 1, MaxInt)
    else if Copy(Arg, 1, Length('--timeout=')) = '--timeout=' then
      ParseTimeout(Copy(Arg, Length('--timeout=') + 1, MaxInt), AOptions)
    else if Copy(Arg, 1, Length('--max-memory=')) = '--max-memory=' then
      ParseMaxMemory(Copy(Arg, Length('--max-memory=') + 1, MaxInt), AOptions)
    else if Copy(Arg, 1, Length('--max-instructions=')) = '--max-instructions=' then
      ParseMaxInstructions(Copy(Arg, Length('--max-instructions=') + 1, MaxInt),
        AOptions)
    else if Copy(Arg, 1, Length('--stack-size=')) = '--stack-size=' then
      ParseStackSize(Copy(Arg, Length('--stack-size=') + 1, MaxInt), AOptions)
    else if Copy(Arg, 1, Length('--profile=')) = '--profile=' then
      ParseProfileMode(Copy(Arg, Length('--profile=') + 1, MaxInt), AOptions)
    else if Copy(Arg, 1, Length('--profile-output=')) = '--profile-output=' then
      AOptions.ProfileOutputPath := Copy(Arg, Length('--profile-output=') + 1,
        MaxInt)
    else if Copy(Arg, 1, 2) = '--' then
      raise Exception.Create('Unknown option: ' + Arg)
    else if AOptions.FileName = STDIN_PATH_MARKER then
      AOptions.FileName := Arg
    else
      raise Exception.Create('Unexpected argument: ' + Arg);
  end;

  if (not AOptions.SourceTypeExplicit) and
     IsModuleSourceFileName(AOptions.FileName) then
    AOptions.SourceType := stModule;

  if AOptions.ProfileModePresent then
    AOptions.Mode := bemBytecode;

  if (AOptions.ProfileOutputPath <> '') and not AOptions.ProfileModePresent then
    raise Exception.Create(
      '--profile-output requires --profile=opcodes|functions|all');

  if AOptions.Test262Host then
  begin
    if AOptions.SourceName <> '' then
      SourceMetadataPath := AOptions.SourceName
    else
      SourceMetadataPath := AOptions.FileName;
    AOptions.Test262AgentCanSuspend :=
      Test262SourceCanSuspendAgent(SourceMetadataPath);
  end;
end;

function ReadBareSource(const AFileName: string): TStringList;
begin
  if IsStdinPath(AFileName) then
    Result := ReadSourceFromText(Input)
  else
    Result := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
end;

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor; const AOptions: TBareOptions;
  const AParentEnvironment: TGocciaHostEnvironment = nil);
begin
  if Assigned(AParentEnvironment) then
    AEngine.HostEnvironment.ConfigureAsChildOf(AParentEnvironment)
  else if AOptions.Deterministic then
    AEngine.HostEnvironment.UseDeterministicProfile;
  AEngine.Compatibility := AOptions.Compatibility;
  AEngine.LabelStatementsEnabled := AOptions.LabelStatementsEnabled;
  AEngine.ForInLoopsEnabled := AOptions.ForInLoopsEnabled;
  AEngine.ExperimentalJSModuleSourceEnabled :=
    AOptions.ExperimentalJSModuleSourceEnabled;
  AEngine.WarningUnsupportedFeatures := AOptions.WarningUnsupportedFeatures;
  AEngine.StrictTypes := AOptions.StrictTypes;
  AEngine.SourceType := AOptions.SourceType;
  AEngine.FunctionConstructor.Enabled := AOptions.UnsafeFunctionConstructor;
  if AOptions.UnsafeShadowRealm then
    EnableShadowRealm(AEngine);
  SetAtomicsAgentCanSuspend(AOptions.Test262AgentCanSuspend);
  if AOptions.Test262Host then
    AEngine.ModuleLoader.SetContentProvider(
      TBareTest262ModuleContentProvider.Create, True);
  if AExecutor is TGocciaBytecodeExecutor then
    TGocciaBytecodeExecutor(AExecutor).GlobalBackedTopLevel :=
      AOptions.SourceType = stScript;
end;

procedure RegisterBareGlobals(const AEngine: TGocciaEngine;
  const APrintHost: TBarePrintHost; const ATest262Host: TBareTest262Host;
  const ATest262EvalHost: TBareTest262EvalHost;
  const AOptions: TBareOptions);
begin
  AEngine.RegisterGlobal(BARE_PRINT_GLOBAL_NAME,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(APrintHost.Print,
      BARE_PRINT_GLOBAL_NAME, -1));
  if AOptions.Test262Host then
    InstallTest262HostGlobals(AEngine, ATest262Host, ATest262EvalHost);
  AEngine.RefreshGlobalThis;
  if AOptions.Test262Host then
    InstallTest262EvalGlobal(AEngine, ATest262EvalHost);
end;

procedure PrintResult(const AResult: TGocciaValue; const APrint: Boolean);
begin
  { Mirrors `node -p` / `bun --print` / `deno eval -p`: silent by default,
    prints the bare value (incl. `undefined`) only when --print is set. }
  if not APrint then
    Exit;
  if not Assigned(AResult) then
    Exit;
  WriteLn(AResult.ToStringLiteral.Value);
end;

function CreateExecutorForMode(
  const AMode: TBareExecutionMode): TGocciaExecutor;
begin
  case AMode of
    bemInterpreted: Result := TGocciaInterpreterExecutor.Create;
    bemBytecode: Result := TGocciaBytecodeExecutor.Create;
  end;
end;

function RunBare(const AOptions: TBareOptions): Integer;
var
  DisplayName: string;
  Engine: TGocciaEngine;
  Executor: TGocciaExecutor;
  GC: TGarbageCollector;
  PrintHost: TBarePrintHost;
  ScriptResult: TGocciaScriptResult;
  Source: TStringList;
  Test262EvalHost: TBareTest262EvalHost;
  Test262Host: TBareTest262Host;
begin
  Result := 0;
  Source := ReadBareSource(AOptions.FileName);
  try
    InitializeProfiler(AOptions);
    if AOptions.SourceName <> '' then
      DisplayName := AOptions.SourceName
    else if IsStdinPath(AOptions.FileName) then
      DisplayName := STDIN_FILE_NAME
    else
      DisplayName := AOptions.FileName;

    { Engine-side bounds.  StartExecutionTimeout / StartInstructionLimit
      are threadvar-backed and order-independent vs. engine creation, so
      they stay here.  --max-memory must be applied AFTER Engine.Create —
      the engine constructor calls TGarbageCollector.Initialize, which
      lazy-creates the GC singleton with FMaxBytes := FSuggestedMaxBytes
      and would otherwise overwrite any pre-engine value. }
    StartExecutionTimeout(AOptions.TimeoutMs);
    StartInstructionLimit(AOptions.MaxInstructions);
    SetMaxStackDepth(AOptions.StackSize);

    PrintHost := TBarePrintHost.Create;
    try
      Test262Host := TBareTest262Host.Create(AOptions);
      try
        Executor := CreateExecutorForMode(AOptions.Mode);
        try
          Engine := TGocciaEngine.Create(DisplayName, Source, Executor);
          try
            ConfigureEngine(Engine, Executor, AOptions);
            Test262Host.ConfigureHostEnvironment(Engine.HostEnvironment);
            Test262EvalHost := TBareTest262EvalHost.Create(Engine);
            try

              GC := TGarbageCollector.Instance;
              if Assigned(GC) and (AOptions.MaxMemoryBytes > 0) then
                GC.MaxBytes := AOptions.MaxMemoryBytes;

              RegisterBareGlobals(Engine, PrintHost, Test262Host,
                Test262EvalHost, AOptions);
              try
                ScriptResult := Engine.Execute;
                PrintResult(ScriptResult.Result, AOptions.Print);
              except
                on E: EGocciaBytecodeThrow do
                begin
                  WriteLn(StdErr, FormatThrowDetail(E.ThrownValue,
                    DisplayName, Source, IsColorTerminal));
                  Result := 1;
                end;
                on E: TGocciaThrowValue do
                begin
                  WriteLn(StdErr, FormatThrowDetail(E.Value, DisplayName,
                    Source, IsColorTerminal, E.Suggestion));
                  Result := 1;
                end;
              end;
            finally
              Test262EvalHost.Free;
            end;
          finally
            Engine.Free;
          end;
        finally
          Executor.Free;
        end;
      finally
        Test262Host.Free;
      end;
    finally
      PrintHost.Free;
    end;
  finally
    try
      WriteProfilerReport(AOptions);
    finally
      if Assigned(TGocciaProfiler.Instance) then
        TGocciaProfiler.Shutdown;
      Source.Free;
    end;
  end;
end;

var
  Options: TBareOptions;
begin
  Options := Default(TBareOptions);
  try
    ParseOptions(Options);
    ExitCode := RunBare(Options);
  except
    on E: TGocciaTimeoutError do
    begin
      if Options.Test262Host then
      begin
        WriteLn(StdErr, TEST262_TIMEOUT_MARKER, ':', E.DurationMs);
        ExitCode := TEST262_TIMEOUT_EXIT_CODE;
      end
      else
      begin
        WriteLn(StdErr, 'Error: ', E.Message);
        ExitCode := 1;
      end;
    end;
    on E: TGocciaError do
    begin
      WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
      ExitCode := 1;
    end;
    on E: Exception do
    begin
      WriteLn(StdErr, 'Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
