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
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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

function TGocciaInterpreterExecutor.EvaluateModuleBody(
  const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  I: Integer;
  CF: TGocciaControlFlow;
  ExecutionContext: TGocciaExecutionContextScope;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  PredeclareModuleLexicalDeclarations(AProgram, AContext.Scope);
  if FInterpreter.VarEnabled then
    HoistVarDeclarations(AProgram.Body, AContext.Scope, False);
  if FInterpreter.FunctionEnabled then
    HoistFunctionDeclarations(AProgram.Body, AContext);
  ExecutionContext := nil;
  if Assigned(AContext.Realm) then
    ExecutionContext := TGocciaExecutionContextScope.Create(
      CreateExecutionContext(AContext.Realm, AContext.Scope,
        AContext.CurrentFilePath, AProgram));
  try
    for I := 0 to AProgram.Body.Count - 1 do
      if (AProgram.Body[I] is TGocciaImportDeclaration) or
         (AProgram.Body[I] is TGocciaReExportDeclaration) then
      begin
        CF := EvaluateStatement(AProgram.Body[I], AContext);
        CF := CF.UpdateEmpty(Result);
        if CF.Kind = cfkReturn then Exit(CF.Value);
      end;

    for I := 0 to AProgram.Body.Count - 1 do
    begin
      if (AProgram.Body[I] is TGocciaImportDeclaration) or
         (AProgram.Body[I] is TGocciaReExportDeclaration) then
        Continue;
      CF := EvaluateStatement(AProgram.Body[I], AContext);
      CF := CF.UpdateEmpty(Result);
      Result := CF.Value;
      if CF.Kind = cfkReturn then Exit;
    end;
  finally
    if Assigned(ExecutionContext) then
      ExecutionContext.Free;
  end;
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
begin
  Context := FInterpreter.CreateEvaluationContext;
  Context.Scope := AScope;
  Context.Realm := FRealm;
  Context.CurrentFilePath := FSourcePath;
  Context.NonStrictMode := AScope.NonStrictMode;
  Context.CompatibilityNonStrictMode := AScope.EffectiveNonStrictMode;
  Result := EvaluateModuleBody(
    TGocciaInterpreterModule(AModule).Prog, Context);
  TGocciaInterpreterModule(AModule).FProgram := nil;
end;

end.
