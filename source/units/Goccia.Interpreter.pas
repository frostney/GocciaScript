unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Classes,
  Math,
  SysUtils,

  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.ControlFlow,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaInterpreter = class
  private
    FGlobalScope: TGocciaGlobalScope;
    FFileName: string;
    FSourceLines: TStringList;
    FASIEnabled: Boolean;
    FJSXEnabled: Boolean;
    FVarEnabled: Boolean;
    FFunctionEnabled: Boolean;
    FTraditionalForLoopsEnabled: Boolean;
    FWhileLoopsEnabled: Boolean;
    FLooseEqualityEnabled: Boolean;
    FNonStrictModeEnabled: Boolean;
    FArgumentsObjectEnabled: Boolean;
    FStrictTypesEnabled: Boolean;
    FModuleLoader: TGocciaModuleLoader;
    FOwnsModuleLoader: Boolean;
    FRealm: TGocciaRealm;

    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetGlobalModules: TOrderedStringMap<TGocciaModule>;
    function GetResolver: TGocciaModuleResolver;
    procedure SetASIEnabled(const AValue: Boolean);
    procedure SetJSXEnabled(const AValue: Boolean);
    procedure SetVarEnabled(const AValue: Boolean);
    procedure SetFunctionEnabled(const AValue: Boolean);
    procedure SetTraditionalForLoopsEnabled(const AValue: Boolean);
    procedure SetWhileLoopsEnabled(const AValue: Boolean);
    procedure SetLooseEqualityEnabled(const AValue: Boolean);
    procedure SetNonStrictModeEnabled(const AValue: Boolean);
    procedure SetArgumentsObjectEnabled(const AValue: Boolean);
    procedure SetStrictTypesEnabled(const AValue: Boolean);
    procedure SetResolver(const AValue: TGocciaModuleResolver);
  public
    function CreateEvaluationContext: TGocciaEvaluationContext;
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext;
      out AProgramConsumed: Boolean): TGocciaValue;
    constructor Create(const AFileName: string; const ASourceLines: TStringList); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader); overload;
    destructor Destroy; override;
    function Execute(const AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const AModulePath, AImportingFilePath: string): TGocciaModule;
    function LoadModuleSourceValue(
      const AModulePath, AImportingFilePath: string): TGocciaValue;
    function LoadDeferredModuleNamespaceValue(
      const AModulePath, AImportingFilePath: string): TGocciaValue;

    property ASIEnabled: Boolean read FASIEnabled write SetASIEnabled;
    property VarEnabled: Boolean read FVarEnabled write SetVarEnabled;
    property FunctionEnabled: Boolean read FFunctionEnabled write SetFunctionEnabled;
    property TraditionalForLoopsEnabled: Boolean
      read FTraditionalForLoopsEnabled write SetTraditionalForLoopsEnabled;
    property WhileLoopsEnabled: Boolean
      read FWhileLoopsEnabled write SetWhileLoopsEnabled;
    property LooseEqualityEnabled: Boolean
      read FLooseEqualityEnabled write SetLooseEqualityEnabled;
    property NonStrictModeEnabled: Boolean
      read FNonStrictModeEnabled write SetNonStrictModeEnabled;
    property ArgumentsObjectEnabled: Boolean
      read FArgumentsObjectEnabled write SetArgumentsObjectEnabled;
    property StrictTypesEnabled: Boolean read FStrictTypesEnabled
      write SetStrictTypesEnabled;
    property GlobalScope: TGocciaGlobalScope read FGlobalScope;
    property Realm: TGocciaRealm read FRealm write FRealm;
    property JSXEnabled: Boolean read FJSXEnabled write SetJSXEnabled;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property ModuleLoader: TGocciaModuleLoader read FModuleLoader;
    property Resolver: TGocciaModuleResolver read GetResolver write SetResolver;
    property GlobalModules: TOrderedStringMap<TGocciaModule> read GetGlobalModules;
  end;


implementation

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Statements,
  Goccia.CapabilityAudit,
  Goccia.Constants.ErrorNames,
  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.Generator.Continuation,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

type
  TGocciaInterpreterAsyncModuleEvaluation = class(TGocciaObjectValue)
  private
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FContext: TGocciaEvaluationContext;
    FContinuation: TGocciaGeneratorContinuation;
    FProgram: TGocciaProgram;
    FPromise: TGocciaPromiseValue;
    FSettled: Boolean;
    procedure AttachAwait(const ASuspension: EGocciaAsyncAwaitSuspend);
    procedure RejectWithException(const AException: Exception);
    procedure Resume(const AKind: TGocciaGeneratorResumeKind;
      const AValue: TGocciaValue);
  public
    constructor Create(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext);
    destructor Destroy; override;
    function Start: TGocciaPromiseValue;
    function FulfillAwait(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RejectAwait(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

procedure RejectPromiseWithException(const APromise: TGocciaPromiseValue;
  const AException: Exception);
begin
  if AException is EGocciaBytecodeThrow then
    APromise.Reject(EGocciaBytecodeThrow(AException).ThrownValue)
  else if AException is TGocciaThrowValue then
    APromise.Reject(TGocciaThrowValue(AException).Value)
  else if AException is TGocciaTypeError then
    APromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
  else if AException is TGocciaReferenceError then
    APromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
  else if AException is TGocciaSyntaxError then
    APromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
  else
    APromise.Reject(CreateErrorObject(ERROR_NAME, AException.Message));
end;

{ TGocciaInterpreterAsyncModuleEvaluation }

constructor TGocciaInterpreterAsyncModuleEvaluation.Create(
  const AProgram: TGocciaProgram; const AContext: TGocciaEvaluationContext);
var
  I: Integer;
begin
  inherited Create(nil);
  FProgram := AProgram;
  FContext := AContext;
  FPromise := TGocciaPromiseValue.Create;
  FBodyStatements := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to FProgram.Body.Count - 1 do
    if not ((FProgram.Body[I] is TGocciaImportDeclaration) or
            (FProgram.Body[I] is TGocciaReExportDeclaration)) then
      FBodyStatements.Add(FProgram.Body[I]);
  FContinuation := TGocciaGeneratorContinuation.Create(
    FBodyStatements, FContext.Scope, FContext);
end;

destructor TGocciaInterpreterAsyncModuleEvaluation.Destroy;
begin
  FContinuation.Free;
  FBodyStatements.Free;
  FProgram.Free;
  inherited;
end;

procedure TGocciaInterpreterAsyncModuleEvaluation.AttachAwait(
  const ASuspension: EGocciaAsyncAwaitSuspend);
var
  FulfillHandler: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaNativeFunctionValue;
begin
  FulfillHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillAwait, '<module-await-fulfill>', 1);
  FulfillHandler.CapturedRoot := Self;
  RejectHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    RejectAwait, '<module-await-reject>', 1);
  RejectHandler.CapturedRoot := Self;
  ASuspension.Promise.InvokeThen(FulfillHandler, RejectHandler);
end;

procedure TGocciaInterpreterAsyncModuleEvaluation.RejectWithException(
  const AException: Exception);
begin
  if FSettled then
    Exit;
  FSettled := True;
  RejectPromiseWithException(FPromise, AException);
end;

procedure TGocciaInterpreterAsyncModuleEvaluation.Resume(
  const AKind: TGocciaGeneratorResumeKind; const AValue: TGocciaValue);
var
  Done: Boolean;
  ExecutionContext: TGocciaExecutionContextScope;
begin
  if FSettled then
    Exit;

  ExecutionContext := nil;
  if Assigned(FContext.Realm) then
    ExecutionContext := TGocciaExecutionContextScope.Create(
      CreateExecutionContext(FContext.Realm, FContext.Scope,
        FContext.CurrentFilePath, FProgram));
  PushAsyncAwaitSuspension;
  try
    try
      FContinuation.Resume(AKind, AValue, Done);
      if Done then
      begin
        FSettled := True;
        FPromise.Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    except
      on E: EGocciaAsyncAwaitSuspend do
        AttachAwait(E);
      on E: EGocciaCapabilityAuditDeliveryError do
        raise;
      on E: Exception do
        RejectWithException(E);
    end;
  finally
    PopAsyncAwaitSuspension;
    if Assigned(ExecutionContext) then
      ExecutionContext.Free;
  end;
end;

function TGocciaInterpreterAsyncModuleEvaluation.Start: TGocciaPromiseValue;
begin
  Resume(grkNext, TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := FPromise;
end;

function TGocciaInterpreterAsyncModuleEvaluation.FulfillAwait(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Resume(grkNext, AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaInterpreterAsyncModuleEvaluation.RejectAwait(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Resume(grkThrow, AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaInterpreterAsyncModuleEvaluation.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
end;

{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  Create(AFileName, ASourceLines, nil);
end;

constructor TGocciaInterpreter.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const AModuleLoader: TGocciaModuleLoader);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FRealm := CurrentRealm;
  FGlobalScope := TGocciaGlobalScope.Create;
  // Seed the error channel at the root: every scope created under the
  // global scope inherits OnError at construction, so closures whose
  // lexical chain was built before any EvaluateStatements propagation
  // (class bodies, static methods) can still throw structured errors.
  // Without this, those paths called a NIL method pointer — natively an
  // access violation that happened to be swallowed as a generic error,
  // under WASM an uncatchable call_indirect trap.
  FGlobalScope.OnError := ThrowError;
  if Assigned(AModuleLoader) then
  begin
    FModuleLoader := AModuleLoader;
    FOwnsModuleLoader := False;
  end
  else
  begin
    FModuleLoader := TGocciaModuleLoader.Create(AFileName);
    FOwnsModuleLoader := True;
  end;
  FModuleLoader.BindRuntime(FGlobalScope, ThrowError);
  FModuleLoader.EvaluateModuleBody := EvaluateModuleBody;
  FGlobalScope.LoadModule := LoadModule;
  FGlobalScope.LoadModuleSource := LoadModuleSourceValue;
  FGlobalScope.LoadDeferredModule := LoadDeferredModuleNamespaceValue;
  FGlobalScope.ResolveModuleURL := FModuleLoader.ResolveModuleURL;
end;

destructor TGocciaInterpreter.Destroy;
begin
  if Assigned(FModuleLoader) and (not FOwnsModuleLoader) then
    FModuleLoader.EvaluateModuleBody := nil;

  if not Assigned(TGarbageCollector.Instance) then
    FGlobalScope.Free;
  if FOwnsModuleLoader then
    FModuleLoader.Free;

  inherited;
end;


function TGocciaInterpreter.CreateEvaluationContext: TGocciaEvaluationContext;
begin
  Result := Default(TGocciaEvaluationContext);
  if Assigned(FRealm) then
    Result.Realm := FRealm
  else
    Result.Realm := CurrentRealm;
  Result.Scope := FGlobalScope;
  Result.OnError := ThrowError;
  Result.LoadModule := LoadModule;
  Result.LoadModuleSource := LoadModuleSourceValue;
  Result.LoadDeferredModule := LoadDeferredModuleNamespaceValue;
  Result.ResolveModuleURL := FModuleLoader.ResolveModuleURL;
  Result.CurrentFilePath := FFileName;
  Result.CoverageEnabled := Assigned(TGocciaCoverageTracker.Instance)
    and TGocciaCoverageTracker.Instance.Enabled;
  Result.StrictTypes := FStrictTypesEnabled;
  Result.NonStrictMode := FNonStrictModeEnabled;
  Result.CompatibilityNonStrictMode := FNonStrictModeEnabled;
  if Assigned(FGlobalScope) then
    FGlobalScope.ArgumentsObjectEnabled := FArgumentsObjectEnabled;
end;

function TGocciaInterpreter.Execute(const AProgram: TGocciaProgram): TGocciaValue;
var
  I: Integer;
  CF: TGocciaControlFlow;
  Context: TGocciaEvaluationContext;
  ExecutionContext: TGocciaExecutionContextScope;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  Context := CreateEvaluationContext;
  Context.NonStrictMode := Context.NonStrictMode and
    not HasUseStrictDirective(AProgram);

  PredeclareScriptLexicalDeclarations(AProgram, FGlobalScope);
  if FVarEnabled then
    HoistVarDeclarations(AProgram.Body, FGlobalScope, Context);
  if FFunctionEnabled then
    HoistFunctionDeclarations(AProgram.Body, Context);

  ExecutionContext := nil;
  if Assigned(Context.Realm) then
    ExecutionContext := TGocciaExecutionContextScope.Create(
      CreateExecutionContext(Context.Realm, Context.Scope,
        Context.CurrentFilePath, AProgram));
  try
    for I := 0 to AProgram.Body.Count - 1 do
    begin
      CF := EvaluateStatement(AProgram.Body[I], Context);
      CF := CF.UpdateEmpty(Result);
      Result := CF.Value;
      if CF.Kind = cfkReturn then Exit;
    end;
  finally
    if Assigned(ExecutionContext) then
      ExecutionContext.Free;
  end;
end;

function TGocciaInterpreter.LoadModule(const AModulePath, AImportingFilePath: string): TGocciaModule;
begin
  Result := FModuleLoader.LoadModule(AModulePath, AImportingFilePath);
end;

function TGocciaInterpreter.LoadModuleSourceValue(
  const AModulePath, AImportingFilePath: string): TGocciaValue;
begin
  Result := FModuleLoader.LoadModuleSourceValue(AModulePath, AImportingFilePath);
end;

function TGocciaInterpreter.LoadDeferredModuleNamespaceValue(
  const AModulePath, AImportingFilePath: string): TGocciaValue;
begin
  Result := FModuleLoader.LoadDeferredModuleNamespaceValue(AModulePath,
    AImportingFilePath);
end;

function TGocciaInterpreter.GetContentProvider: TGocciaModuleContentProvider;
begin
  Result := FModuleLoader.ContentProvider;
end;

function TGocciaInterpreter.GetGlobalModules: TOrderedStringMap<TGocciaModule>;
begin
  Result := FModuleLoader.GlobalModules;
end;

function TGocciaInterpreter.GetResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaInterpreter.SetASIEnabled(const AValue: Boolean);
begin
  FASIEnabled := AValue;
  FModuleLoader.ASIEnabled := AValue;
end;

procedure TGocciaInterpreter.SetJSXEnabled(const AValue: Boolean);
begin
  FJSXEnabled := AValue;
  FModuleLoader.JSXEnabled := AValue;
end;

procedure TGocciaInterpreter.SetVarEnabled(const AValue: Boolean);
begin
  FVarEnabled := AValue;
  FModuleLoader.VarEnabled := AValue;
end;

procedure TGocciaInterpreter.SetFunctionEnabled(const AValue: Boolean);
begin
  FFunctionEnabled := AValue;
  FModuleLoader.FunctionEnabled := AValue;
end;

procedure TGocciaInterpreter.SetTraditionalForLoopsEnabled(const AValue: Boolean);
begin
  FTraditionalForLoopsEnabled := AValue;
  FModuleLoader.TraditionalForLoopsEnabled := AValue;
end;

procedure TGocciaInterpreter.SetWhileLoopsEnabled(const AValue: Boolean);
begin
  FWhileLoopsEnabled := AValue;
  FModuleLoader.WhileLoopsEnabled := AValue;
end;

procedure TGocciaInterpreter.SetLooseEqualityEnabled(const AValue: Boolean);
begin
  FLooseEqualityEnabled := AValue;
  FModuleLoader.LooseEqualityEnabled := AValue;
end;

procedure TGocciaInterpreter.SetNonStrictModeEnabled(const AValue: Boolean);
begin
  FNonStrictModeEnabled := AValue;
  if Assigned(FGlobalScope) then
    FGlobalScope.NonStrictMode := AValue;
  FModuleLoader.NonStrictModeEnabled := AValue;
end;

procedure TGocciaInterpreter.SetArgumentsObjectEnabled(const AValue: Boolean);
begin
  FArgumentsObjectEnabled := AValue;
  if Assigned(FGlobalScope) then
    FGlobalScope.ArgumentsObjectEnabled := AValue;
end;

procedure TGocciaInterpreter.SetStrictTypesEnabled(const AValue: Boolean);
begin
  FStrictTypesEnabled := AValue;
  if Assigned(FGlobalScope) then
    FGlobalScope.StrictTypes := AValue;
  FModuleLoader.StrictTypesEnabled := AValue;
end;

procedure TGocciaInterpreter.SetResolver(const AValue: TGocciaModuleResolver);
begin
  if Assigned(AValue) and (AValue <> FModuleLoader.Resolver) then
    raise Exception.Create(
      'TGocciaInterpreter resolver is owned by the module loader. ' +
      'Create the interpreter with a configured TGocciaModuleLoader instead.');
end;

function TGocciaInterpreter.EvaluateModuleBody(
  const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext;
  out AProgramConsumed: Boolean): TGocciaValue;
var
  AsyncEvaluation: TGocciaInterpreterAsyncModuleEvaluation;
  I: Integer;
  CF: TGocciaControlFlow;
  ExecutionContext: TGocciaExecutionContextScope;
begin
  AProgramConsumed := False;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not AContext.ModuleEnvironmentInitialized then
  begin
    PredeclareModuleLexicalDeclarations(AProgram, AContext.Scope);
    if FVarEnabled then
      HoistVarDeclarations(AProgram.Body, AContext.Scope, AContext);
    if FFunctionEnabled then
      HoistFunctionDeclarations(AProgram.Body, AContext);
  end;
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

    if AProgram.HasTopLevelAwait then
    begin
      AsyncEvaluation := TGocciaInterpreterAsyncModuleEvaluation.Create(
        AProgram, AContext);
      AProgramConsumed := True;
      Result := AsyncEvaluation.Start;
      if Assigned(AContext.CurrentModule) and
         (Result is TGocciaPromiseValue) then
        AContext.CurrentModule.EvaluationPromise := Result;
      Exit;
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

procedure TGocciaInterpreter.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.
