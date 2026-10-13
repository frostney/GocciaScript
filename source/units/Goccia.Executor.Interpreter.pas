unit Goccia.Executor.Interpreter;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node,
  Goccia.Evaluator.Context,
  Goccia.Executor,
  Goccia.Interpreter,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaInterpreterModule = class(TGocciaCompiledModule)
  private
    FProgram: TGocciaProgram;
  public
    constructor Create(const AProgram: TGocciaProgram);
    property Prog: TGocciaProgram read FProgram;
  end;

  TGocciaInterpreterExecutor = class(TGocciaExecutor)
  private
    FInterpreter: TGocciaInterpreter;
  public
    constructor Create;
    procedure Initialize(const AGlobalScope: TGocciaScope;
      const AModuleLoader: TGocciaModuleLoader;
      const ASourcePath: string); override;
    function ExecuteProgram(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    function ExecuteDynamicFunction(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    function EvaluateGlobalEval(const AProgram: TGocciaProgram;
      const ASourcePath: string; const AStrictEval: Boolean;
      const ADeclarations: TGocciaGlobalEvalDeclarations;
      var AStage: TGocciaGlobalEvalStage): TGocciaValue; override;
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext;
      out AProgramConsumed: Boolean): TGocciaValue; override;
    procedure InstantiateModuleFunctions(const AProgram: TGocciaProgram;
      var AContext: TGocciaEvaluationContext); override;
    function CompileModule(
      const AProgram: TGocciaProgram): TGocciaCompiledModule; override;
    function RunCompiledModule(
      const AModule: TGocciaCompiledModule): TGocciaValue; override;
    function RunCompiledModuleInScope(
      const AModule: TGocciaCompiledModule;
      const AScope: TGocciaScope): TGocciaValue; override;
    property Interpreter: TGocciaInterpreter read FInterpreter
      write FInterpreter;
  end;

implementation

uses
  TimingUtils,

  Goccia.AST.Statements,
  Goccia.ControlFlow,
  Goccia.Evaluator,
  Goccia.ExecutionContext,
  Goccia.GarbageCollector,
  Goccia.Scope.Redeclaration;

{ TGocciaInterpreterExecutor }

constructor TGocciaInterpreterExecutor.Create;
begin
  inherited Create;
  FInterpreter := nil;
end;

procedure TGocciaInterpreterExecutor.Initialize(
  const AGlobalScope: TGocciaScope; const AModuleLoader: TGocciaModuleLoader;
  const ASourcePath: string);
begin
  inherited;
  AModuleLoader.EvaluateModuleBody := EvaluateModuleBody;
  AModuleLoader.InstantiateModuleFunctions := InstantiateModuleFunctions;
end;

procedure TGocciaInterpreterExecutor.InstantiateModuleFunctions(
  const AProgram: TGocciaProgram; var AContext: TGocciaEvaluationContext);
begin
  FInterpreter.InstantiateModuleFunctions(AProgram, AContext);
end;

function TGocciaInterpreterExecutor.ExecuteProgram(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Start: Int64;
begin
  Start := GetNanoseconds;
  Result := FInterpreter.Execute(AProgram);
  ExecuteTimeNanoseconds := GetNanoseconds - Start;
end;

function TGocciaInterpreterExecutor.ExecuteDynamicFunction(
  const AProgram: TGocciaProgram): TGocciaValue;
begin
  Result := FInterpreter.Execute(AProgram);
end;

// ES2026 §19.2.1.1 PerformEval steps 17-33, evaluated by the interpreter: the
// eval gets a fresh declarative environment under the global one, which is
// also its variable environment unless the eval is strict. The engine has run
// the static checks; the evaluator makes its own declaration lists.
function TGocciaInterpreterExecutor.EvaluateGlobalEval(
  const AProgram: TGocciaProgram; const ASourcePath: string;
  const AStrictEval: Boolean;
  const ADeclarations: TGocciaGlobalEvalDeclarations;
  var AStage: TGocciaGlobalEvalStage): TGocciaValue;
var
  EvalScope, VarScope: TGocciaScope;
  EvalContext: TGocciaEvaluationContext;
begin
  AStage := gesRun;
  if AStrictEval then
    EvalScope := FGlobalScope.CreateChild(skFunction, 'StrictGlobalEval')
  else
    EvalScope := FGlobalScope.CreateChild(skBlock, 'GlobalEval');
  EvalScope.ThisValue := FGlobalScope.ThisValue;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(EvalScope);
  try
    EvalContext := FInterpreter.CreateEvaluationContext;
    EvalContext.Scope := EvalScope;
    EvalContext.CurrentFilePath := ASourcePath;
    EvalContext.NonStrictMode := not AStrictEval;
    if AStrictEval then
      VarScope := EvalScope
    else
      VarScope := FGlobalScope;
    Result := EvaluateEvalProgram(AProgram, EvalContext, VarScope, EvalScope,
      AStrictEval, False, nil, False, False, False, False, True);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(EvalScope);
  end;
end;

function TGocciaInterpreterExecutor.EvaluateModuleBody(
  const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext;
  out AProgramConsumed: Boolean): TGocciaValue;
begin
  Result := FInterpreter.EvaluateModuleBody(
    AProgram, AContext, AProgramConsumed);
end;

constructor TGocciaInterpreterModule.Create(const AProgram: TGocciaProgram);
begin
  inherited Create;
  FProgram := AProgram;
end;

function TGocciaInterpreterExecutor.CompileModule(
  const AProgram: TGocciaProgram): TGocciaCompiledModule;
begin
  Result := TGocciaInterpreterModule.Create(AProgram);
end;

function TGocciaInterpreterExecutor.RunCompiledModule(
  const AModule: TGocciaCompiledModule): TGocciaValue;
begin
  Result := FInterpreter.Execute(TGocciaInterpreterModule(AModule).Prog);
  TGocciaInterpreterModule(AModule).FProgram := nil;
end;

function TGocciaInterpreterExecutor.RunCompiledModuleInScope(
  const AModule: TGocciaCompiledModule;
  const AScope: TGocciaScope): TGocciaValue;
var
  Context: TGocciaEvaluationContext;
  ProgramConsumed: Boolean;
begin
  Context := FInterpreter.CreateEvaluationContext;
  Context.Scope := AScope;
  Context.Realm := FRealm;
  Context.CurrentFilePath := FSourcePath;
  Context.NonStrictMode := AScope.NonStrictMode;
  Context.CompatibilityNonStrictMode := AScope.EffectiveNonStrictMode;
  Result := EvaluateModuleBody(
    TGocciaInterpreterModule(AModule).Prog, Context, ProgramConsumed);
  TGocciaInterpreterModule(AModule).FProgram := nil;
end;

end.
