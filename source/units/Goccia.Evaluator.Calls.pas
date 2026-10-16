// The tree-walk interpreter's call paths for AST-backed functions.
//
// TGocciaFunctionValue (Goccia.Values.FunctionValue) holds a function's
// parameters, body and closure, and does the value-level part of a call:
// rooting, the call scope and the function execution context. Running the
// body is the evaluator's job, so it lives here and is registered with
// RegisterFunctionCallPaths when this unit is linked. Goccia.Evaluator, the
// only unit that creates these functions, uses this unit, so a program that
// can create one always has the paths registered.
unit Goccia.Evaluator.Calls;

{$I Goccia.inc}

interface

uses
  Goccia.EvaluationContext,
  Goccia.Scope,
  Goccia.Values.FunctionValue;

{ True when a parameter has an initializer, directly or inside a destructuring
  pattern: ES2026 §10.2.11 FunctionDeclarationInstantiation step 30 then gives
  the body a separate var environment. }
function FunctionHasParameterExpressions(
  const AFunction: TGocciaFunctionValue): Boolean;
{ Creates every parameter binding in its TDZ before any initializer runs, so
  an initializer that reads a later parameter throws a ReferenceError. }
procedure PredeclareFunctionParameterBindings(
  const AFunction: TGocciaFunctionValue; const ACallScope: TGocciaScope);
{ Names a direct eval inside a parameter initializer may not declare with
  `var`: the parameters, and `arguments` when the call creates it. }
function BuildFunctionParameterEvalRejectNames(
  const AFunction: TGocciaFunctionValue;
  const AIncludeArgumentsObject: Boolean): TGocciaEvalRejectNameArray;

implementation

uses
  Generics.Collections,
  SysUtils,

  UnicodeStringList,

  Goccia.Arguments.Collection,
  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.Constants,
  Goccia.Constants.ErrorNames,
  Goccia.ControlFlow,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator,
  Goccia.GarbageCollector,
  Goccia.Generator.Continuation,
  Goccia.Realm,
  Goccia.Scope.Declarations,
  Goccia.Types.Enforcement,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

type
  TGocciaAsyncFunctionEvaluation = class(TGocciaObjectValue)
  private
    FFunction: TGocciaFunctionValue;
    FPromise: TGocciaPromiseValue;
    FContinuation: TGocciaGeneratorContinuation;
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FSyntheticReturn: TGocciaReturnStatement;
    FCallScope: TGocciaScope;
    FRealm: TGocciaRealm;
    FSettled: Boolean;
    procedure AttachAwait(const ASuspension: EGocciaAsyncAwaitSuspend);
    function RejectWithException(const AException: Exception): Boolean;
    procedure Resume(const AKind: TGocciaGeneratorResumeKind;
      const AValue: TGocciaValue);
  public
    constructor Create(const AFunction: TGocciaFunctionValue;
      const ABodyStatements: TObjectList<TGocciaASTNode>;
      const ASyntheticReturn: TGocciaReturnStatement;
      const ACallScope: TGocciaScope;
      const AContext: TGocciaEvaluationContext);
    destructor Destroy; override;
    function Start: TGocciaPromiseValue;
    function FulfillAwait(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RejectAwait(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

{ True when AException is a script-level failure and the promise now carries
  it. False means the exception is not the guest's to observe, and the caller
  — which is inside the handler that owns it — must let it continue with a
  bare `raise`.

  The bare raise at the call site is the whole point of the Boolean. This
  used to end in `raise AException`, which re-raises the very object the RTL
  is already unwinding: the original handler frees it on the way out, so the
  second raise carries a dangling reference and the async path surfaced a
  spurious "Access violation" instead of the limit that actually fired. }
function TryRejectAsyncPromiseWithException(
  const APromise: TGocciaPromiseValue;
  const AException: Exception): Boolean;
var
  ThrownVal: TGocciaValue;
begin
  Result := True;
  { A JS throw that leaves the VM as EGocciaBytecodeThrow is a guest
    completion carrying the thrown value, exactly like TGocciaThrowValue;
    treating it as an engine fault would re-raise it into the microtask queue
    and leave the async function's promise forever pending. UnwrapThrownValue
    handles both boundary classes, identity preserved. }
  if UnwrapThrownValue(AException, ThrownVal) then
    APromise.Reject(ThrownVal)
  else if AException is TGocciaTypeError then
    APromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
  else if AException is TGocciaReferenceError then
    APromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
  else if AException is TGocciaSyntaxError then
    APromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
  else if AException is TGocciaRuntimeError then
    APromise.Reject(CreateErrorObject(ERROR_NAME, AException.Message))
  else
    Result := False;
end;

{ TGocciaAsyncFunctionEvaluation }

constructor TGocciaAsyncFunctionEvaluation.Create(
  const AFunction: TGocciaFunctionValue;
  const ABodyStatements: TObjectList<TGocciaASTNode>;
  const ASyntheticReturn: TGocciaReturnStatement;
  const ACallScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext);
begin
  inherited Create(nil);
  FFunction := AFunction;
  FPromise := TGocciaPromiseValue.Create;
  FBodyStatements := ABodyStatements;
  FSyntheticReturn := ASyntheticReturn;
  FCallScope := ACallScope;
  FRealm := AContext.Realm;
  FContinuation := TGocciaGeneratorContinuation.Create(
    FBodyStatements, ACallScope, AContext);
end;

destructor TGocciaAsyncFunctionEvaluation.Destroy;
begin
  FContinuation.Free;
  FSyntheticReturn.Free;
  FBodyStatements.Free;
  inherited;
end;

procedure TGocciaAsyncFunctionEvaluation.AttachAwait(
  const ASuspension: EGocciaAsyncAwaitSuspend);
var
  GC: TGarbageCollector;
  FulfillHandler: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaNativeFunctionValue;
begin
  GC := TGarbageCollector.Instance;
  FulfillHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillAwait, '<async-await-fulfill>', 1);
  if Assigned(GC) then
    GC.AddTempRoot(FulfillHandler);
  try
    FulfillHandler.CapturedRoot := Self;
    RejectHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      RejectAwait, '<async-await-reject>', 1);
    if Assigned(GC) then
      GC.AddTempRoot(RejectHandler);
    try
      RejectHandler.CapturedRoot := Self;
      ASuspension.Promise.InvokeThen(FulfillHandler, RejectHandler);
    finally
      if Assigned(GC) then
        GC.RemoveTempRoot(RejectHandler);
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(FulfillHandler);
  end;
end;

function TGocciaAsyncFunctionEvaluation.RejectWithException(
  const AException: Exception): Boolean;
begin
  if FSettled then
    Exit(True);
  Result := TryRejectAsyncPromiseWithException(FPromise, AException);
  { Only a rejection settles the evaluation. Marking it settled for an
    exception that keeps unwinding would leave a promise that can never be
    resolved and silence the later resume paths. }
  if Result then
    FSettled := True;
end;

procedure TGocciaAsyncFunctionEvaluation.Resume(
  const AKind: TGocciaGeneratorResumeKind; const AValue: TGocciaValue);
var
  Done: Boolean;
  ResumeValue: TGocciaValue;
  PreviousRealm: TGocciaRealm;
  RealmSwitched: Boolean;
begin
  if FSettled then
    Exit;

  PreviousRealm := CurrentRealm;
  RealmSwitched := Assigned(FRealm) and (FRealm <> PreviousRealm);
  if RealmSwitched then
    SetCurrentRealm(FRealm);
  PushCurrentFunctionExecutionContext(FCallScope, FFunction);
  PushAsyncAwaitSuspension;
  try
    try
      ResumeValue := FContinuation.Resume(AKind, AValue, Done);
      if Done then
      begin
        FSettled := True;
        FPromise.Resolve(ResumeValue);
      end;
    except
      on E: EGocciaAsyncAwaitSuspend do
        AttachAwait(E);
      on E: Exception do
        if not RejectWithException(E) then
          raise;
    end;
  finally
    PopAsyncAwaitSuspension;
    PopCurrentFunctionExecutionContext;
    if RealmSwitched then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function TGocciaAsyncFunctionEvaluation.Start: TGocciaPromiseValue;
begin
  Resume(grkNext, TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := FPromise;
end;

function TGocciaAsyncFunctionEvaluation.FulfillAwait(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Resume(grkNext, AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaAsyncFunctionEvaluation.RejectAwait(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Resume(grkThrow, AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaAsyncFunctionEvaluation.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FFunction) then
    FFunction.MarkReferences;
  { Resume publishes FFunction and FCallScope to the function-execution-context
    facade, which holds both as raw pointers for the length of the resumption
    (see the rooting note on GCurrentFunctionContextStack in Goccia.Realm.pas).
    FContinuation marks the same scope, but that is its bookkeeping, not this
    one's: mark it here so the facade's contract does not depend on the
    continuation's internals. }
  if Assigned(FCallScope) then
    FCallScope.MarkReferences;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
end;

function DestructuringPatternHasParameterExpression(
  const APattern: TGocciaDestructuringPattern): Boolean;
var
  I: Integer;
  ArrayPattern: TGocciaArrayDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
begin
  if not Assigned(APattern) then
    Exit(False);

  if APattern is TGocciaAssignmentDestructuringPattern then
    Exit(True);

  if APattern is TGocciaRestDestructuringPattern then
    Exit(DestructuringPatternHasParameterExpression(
      TGocciaRestDestructuringPattern(APattern).Argument));

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      if DestructuringPatternHasParameterExpression(ArrayPattern.Elements[I]) then
        Exit(True);
    Exit(False);
  end;

  if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
      if DestructuringPatternHasParameterExpression(
        ObjectPattern.Properties[I].Pattern) then
        Exit(True);
  end;

  Result := False;
end;

function FunctionHasParameterExpressions(
  const AFunction: TGocciaFunctionValue): Boolean;
var
  Parameters: TGocciaParameterArray;
  I: Integer;
begin
  Parameters := AFunction.Parameters;
  for I := 0 to High(Parameters) do
    if Assigned(Parameters[I].DefaultValue) or
       (Parameters[I].IsPattern and
        DestructuringPatternHasParameterExpression(Parameters[I].Pattern)) then
      Exit(True);
  Result := False;
end;

procedure PredeclareFunctionParameterBindings(
  const AFunction: TGocciaFunctionValue; const ACallScope: TGocciaScope);
var
  Parameters: TGocciaParameterArray;
  I, J: Integer;
  Names: TUnicodeStringList;
begin
  Parameters := AFunction.Parameters;
  Names := TUnicodeStringList.Create;
  try
    for I := 0 to High(Parameters) do
    begin
      Names.Clear;
      if Parameters[I].IsPattern then
        CollectPatternBindingNames(Parameters[I].Pattern, Names, True)
      else if Parameters[I].Name <> '' then
        Names.Add(Parameters[I].Name);

      for J := 0 to Names.Count - 1 do
        if not ACallScope.ContainsOwnLexicalBinding(Names[J]) then
          ACallScope.PredeclareLexicalBinding(Names[J], dtParameter);
    end;
  finally
    Names.Free;
  end;
end;

function BuildFunctionParameterEvalRejectNames(
  const AFunction: TGocciaFunctionValue;
  const AIncludeArgumentsObject: Boolean): TGocciaEvalRejectNameArray;
var
  Parameters: TGocciaParameterArray;
  Names: TUnicodeStringList;
  I, J: Integer;
  procedure AddName(const AName: string);
  begin
    if (AName <> '') and (Names.IndexOf(AName) < 0) then
      Names.Add(AName);
  end;
begin
  Parameters := AFunction.Parameters;
  Names := TUnicodeStringList.Create;
  try
    if AIncludeArgumentsObject then
      AddName(IDENTIFIER_ARGUMENTS);
    for I := 0 to High(Parameters) do
    begin
      if Parameters[I].IsPattern then
        CollectPatternBindingNames(Parameters[I].Pattern, Names, True)
      else
        AddName(Parameters[I].Name);
    end;

    SetLength(Result, Names.Count);
    for J := 0 to Names.Count - 1 do
      Result[J] := Names[J];
  finally
    Names.Free;
  end;
end;

procedure PrepareCallContext(const AFunction: TGocciaFunctionValue;
  const ACallScope: TGocciaScope;
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  var AContext: TGocciaEvaluationContext; out ABodyScope: TGocciaScope;
  var ABodyScopeRooted: Boolean);
var
  Parameters: TGocciaParameterArray;
  Closure: TGocciaScope;
  I, J: Integer;
  CompatibilityNonStrictMode: Boolean;
  ArgumentsObjectEnabled: Boolean;
  EvalRejectNames, SavedEvalRejectNames: TGocciaEvalRejectNameArray;
  ReturnValue: TGocciaValue;
  ParamTypeHint: TGocciaLocalType;
  ParameterNames: array of string;
  HasParamExpressions: Boolean;
  GC: TGarbageCollector;
  function EvaluateParameterDefault(
    const AExpression: TGocciaExpression): TGocciaValue;
  var
    SavedRejectArgumentsVarDeclaration: Boolean;
  begin
    SavedRejectArgumentsVarDeclaration :=
      AContext.RejectArgumentsVarDeclarationInEval;
    SavedEvalRejectNames := AContext.RejectVarDeclarationNamesInEval;
    AContext.RejectArgumentsVarDeclarationInEval :=
      ArgumentsObjectEnabled and AFunction.CreatesArgumentsObject;
    AContext.RejectVarDeclarationNamesInEval := EvalRejectNames;
    try
      Result := EvaluateExpression(AExpression, AContext);
    finally
      AContext.RejectArgumentsVarDeclarationInEval :=
        SavedRejectArgumentsVarDeclaration;
      AContext.RejectVarDeclarationNamesInEval := SavedEvalRejectNames;
    end;
  end;
  function CreateArgumentsObjectForCall: TGocciaValue;
  var
    ParameterIndex: Integer;
  begin
    if AContext.NonStrictMode and AFunction.IsSimpleParams then
    begin
      SetLength(ParameterNames, Length(Parameters));
      for ParameterIndex := 0 to High(Parameters) do
        ParameterNames[ParameterIndex] := Parameters[ParameterIndex].Name;
      Exit(CreateMappedArgumentsObject(AArguments, ParameterNames,
        ACallScope, AFunction));
    end;
    Result := CreateUnmappedArgumentsObject(AArguments);
  end;
begin
  Parameters := AFunction.Parameters;
  Closure := AFunction.Closure;
  ABodyScopeRooted := False;
  FillChar(AContext, SizeOf(AContext), 0);
  AContext.Realm := CurrentRealm;
  AContext.Scope := Closure;
  AContext.OnError := Closure.OnError;
  AContext.LoadModule := Closure.LoadModule;
  AContext.LoadModuleSource := Closure.LoadModuleSource;
  AContext.ResolveModuleURL := Closure.ResolveModuleURL;
  AContext.CurrentFilePath := AFunction.SourceFilePath;
  AContext.CoverageEnabled := AFunction.TrackCoverage and
    (TGocciaCoverageTracker.Instance <> nil) and
    TGocciaCoverageTracker.Instance.Enabled;
  AContext.StrictTypes := Closure.EffectiveStrictTypes;
  CompatibilityNonStrictMode := Closure.EffectiveNonStrictMode;
  ArgumentsObjectEnabled := Closure.EffectiveArgumentsObjectEnabled;
  AContext.NonStrictMode := CompatibilityNonStrictMode and
    not AFunction.StrictCode;
  AContext.CompatibilityNonStrictMode := CompatibilityNonStrictMode;
  AContext.HideFunctionSourceText := AFunction.HideNestedFunctionSourceText;
  AContext.DisposalTracker := nil;
  HasParamExpressions := FunctionHasParameterExpressions(AFunction);
  if HasParamExpressions then
    EvalRejectNames := BuildFunctionParameterEvalRejectNames(AFunction,
      ArgumentsObjectEnabled and AFunction.CreatesArgumentsObject and
      not ParameterListBindsName(Parameters, IDENTIFIER_ARGUMENTS))
  else
    EvalRejectNames := nil;

  AFunction.RecordCoverageCall;

  AFunction.BindThis(ACallScope, AThisValue);
  AContext.Scope := ACallScope;

  if ArgumentsObjectEnabled and AFunction.CreatesArgumentsObject and
     not ParameterListBindsName(Parameters, IDENTIFIER_ARGUMENTS) and
     not ACallScope.ContainsOwnLexicalBinding(IDENTIFIER_ARGUMENTS) then
    ACallScope.DefineVariableBinding(IDENTIFIER_ARGUMENTS,
      CreateArgumentsObjectForCall, True);

  if HasParamExpressions then
    PredeclareFunctionParameterBindings(AFunction, ACallScope);

  if AFunction.IsSimpleParams then
  begin
    for I := 0 to Length(Parameters) - 1 do
    begin
      if I < AArguments.Length then
        ACallScope.DefineLexicalBinding(Parameters[I].Name,
          AArguments.GetElement(I), dtParameter)
      else
        ACallScope.DefineLexicalBinding(Parameters[I].Name,
          TGocciaUndefinedLiteralValue.UndefinedValue, dtParameter);
    end;
  end
  else
  begin
    for I := 0 to Length(Parameters) - 1 do
    begin
      if Parameters[I].IsRest then
      begin
        ReturnValue := TGocciaArrayValue.Create;
        if I < AArguments.Length then
          for J := I to AArguments.Length - 1 do
            TGocciaArrayValue(ReturnValue).Elements.Add(AArguments.GetElement(J));
        if Parameters[I].IsPattern then
        begin
          AContext.Scope := ACallScope;
          AssignPattern(Parameters[I].Pattern, ReturnValue, AContext, True);
        end
        else
          ACallScope.DefineLexicalBinding(Parameters[I].Name, ReturnValue,
            dtParameter);

        if AContext.StrictTypes and (Parameters[I].TypeAnnotation <> '') then
        begin
          ParamTypeHint := TypeAnnotationToLocalType(Parameters[I].TypeAnnotation);
          if (ParamTypeHint <> sltUntyped) and not Parameters[I].IsPattern then
            ACallScope.SetOwnBindingTypeHint(Parameters[I].Name, ParamTypeHint);
        end;

        Break;
      end
      else if Parameters[I].IsPattern then
      begin
        if I < AArguments.Length then
          ReturnValue := AArguments.GetElement(I)
        else
          ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if Assigned(Parameters[I].DefaultValue) and
           (ReturnValue is TGocciaUndefinedLiteralValue) then
          ReturnValue := EvaluateParameterDefault(Parameters[I].DefaultValue);

        if AContext.StrictTypes
          and (Parameters[I].TypeAnnotation <> '')
          and not Parameters[I].IsOptional
          and not Assigned(Parameters[I].DefaultValue) then
        begin
          ParamTypeHint := TypeAnnotationToLocalType(Parameters[I].TypeAnnotation);
          if ParamTypeHint <> sltUntyped then
            EnforceStrictType(ReturnValue, ParamTypeHint);
        end;

        AContext.Scope := ACallScope;
        AssignPattern(Parameters[I].Pattern, ReturnValue, AContext, True);
      end
      else
      begin
        if I < AArguments.Length then
          ReturnValue := AArguments.GetElement(I)
        else
          ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if Assigned(Parameters[I].DefaultValue) and
           (ReturnValue is TGocciaUndefinedLiteralValue) then
          ReturnValue := EvaluateParameterDefault(Parameters[I].DefaultValue);
        ACallScope.DefineLexicalBinding(Parameters[I].Name, ReturnValue,
          dtParameter);
      end;
    end;
  end;

  ABodyScope := ACallScope;
  if HasParamExpressions then
  begin
    ABodyScope := ACallScope.CreateChild(skFunction, AFunction.Name + ':body');
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(ABodyScope);
      ABodyScopeRooted := True;
    end;
  end;

  AContext.Scope := ABodyScope;
  if Assigned(AContext.OnError) and not Assigned(ABodyScope.OnError) then
    ABodyScope.OnError := AContext.OnError;

  if AContext.StrictTypes then
  begin
    for I := 0 to Length(Parameters) - 1 do
    begin
      if Parameters[I].TypeAnnotation = '' then
        Continue;
      if Parameters[I].IsRest then
        Continue;
      if Parameters[I].IsOptional then
        Continue;
      if Assigned(Parameters[I].DefaultValue) then
        Continue;
      if Parameters[I].IsPattern then
        Continue;
      ParamTypeHint := TypeAnnotationToLocalType(Parameters[I].TypeAnnotation);
      if ParamTypeHint = sltUntyped then
        Continue;
      EnforceStrictType(ACallScope.GetValue(Parameters[I].Name), ParamTypeHint);
      ACallScope.SetOwnBindingTypeHint(Parameters[I].Name, ParamTypeHint);
    end;
  end;
end;

{ Runs AFunction's body in ACallScope, which the caller created, rooted and
  published as the running function's context. }
function ExecuteFunctionBody(const AFunction: TGocciaFunctionValue;
  const ACallScope: TGocciaScope;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  BodyStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
  CF: TGocciaControlFlow;
  Context: TGocciaEvaluationContext;
  BodyScope: TGocciaScope;
  PreviousContinuation: TGocciaGeneratorContinuation;
  PreviousRealm: TGocciaRealm;
  RealmSwitched: Boolean;
  GC: TGarbageCollector;
  BodyScopeRooted: Boolean;
begin
  BodyStatements := AFunction.BodyStatements;
  GC := TGarbageCollector.Instance;
  BodyScopeRooted := False;
  PreviousRealm := CurrentRealm;
  RealmSwitched := Assigned(AFunction.CreationRealm) and
    (AFunction.CreationRealm <> PreviousRealm);
  if RealmSwitched then
    SetCurrentRealm(AFunction.CreationRealm);
  PreviousContinuation := SuspendCurrentGeneratorContinuation;
  try
    PrepareCallContext(AFunction, ACallScope, AArguments, AThisValue, Context,
      BodyScope, BodyScopeRooted);

  // Expression-body fast path: an expression body cannot contain var,
  // function, or lexical declarations, so the hoisting passes below are
  // no-ops for it. Evaluate the single expression directly and skip them.
  if AFunction.IsExpressionBody and (BodyStatements.Count = 1) then
  begin
    Result := EvaluateExpression(TGocciaExpression(BodyStatements[0]), Context);
    Exit;
  end;

  // Hoist var declarations to function scope
  HoistVarDeclarations(BodyStatements, BodyScope, Context);

  // Function-body lexical declarations are in TDZ before hoisted function
  // declarations are created, so closures capture the uninitialized binding.
  PredeclareFunctionBodyLexicalDeclarations(BodyStatements, BodyScope);

  // Hoist function declarations (both name and value) to function scope
  HoistFunctionDeclarations(BodyStatements, Context);

  // Single-return fast path: (x) => { return expr; } — evaluate the return
  // expression directly, bypassing the statement loop
  if (BodyStatements.Count = 1) and (BodyStatements[0] is TGocciaReturnStatement) then
  begin
    if TGocciaReturnStatement(BodyStatements[0]).Value <> nil then
      Result := EvaluateExpression(TGocciaExpression(TGocciaReturnStatement(BodyStatements[0]).Value), Context)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  for I := 0 to BodyStatements.Count - 1 do
  begin
    if BodyStatements[I] is TGocciaExpression then
      CF := TGocciaControlFlow.Normal(EvaluateExpression(TGocciaExpression(BodyStatements[I]), Context))
    else
      CF := EvaluateStatement(TGocciaStatement(BodyStatements[I]), Context);
    if CF.Kind = cfkReturn then
    begin
      if CF.Value = nil then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue
      else
        Result := CF.Value;
      Exit;
    end;
    if CF.Kind = cfkBreak then
      ThrowSyntaxError(SErrorIllegalBreakStatement, SSuggestExpressionExpected);
    if CF.Kind = cfkContinue then
      ThrowSyntaxError(SErrorIllegalContinueStatement, SSuggestExpressionExpected);
  end;
  finally
    if BodyScopeRooted and Assigned(GC) then
      GC.PopActiveRoot;
    RestoreCurrentGeneratorContinuation(PreviousContinuation);
    if RealmSwitched then
      SetCurrentRealm(PreviousRealm);
  end;
end;

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function StartAsyncFunction(const AFunction: TGocciaFunctionValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  BodyStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
  Context: TGocciaEvaluationContext;
  CallScope: TGocciaScope;
  BodyScope: TGocciaScope;
  PreviousContinuation: TGocciaGeneratorContinuation;
  PreviousRealm: TGocciaRealm;
  RealmSwitched: Boolean;
  GC: TGarbageCollector;
  BodyScopeRooted: Boolean;
  SelfRooted: Boolean;
  CallScopeRooted: Boolean;
  FunctionContextPushed: Boolean;
  AsyncEvaluationRooted: Boolean;
  AsyncEvaluation: TGocciaAsyncFunctionEvaluation;
  AsyncBodyStatements: TObjectList<TGocciaASTNode>;
  SyntheticReturn: TGocciaReturnStatement;
  RejectedPromise: TGocciaPromiseValue;
  Rejected: Boolean;
begin
  BodyStatements := AFunction.BodyStatements;
  GC := TGarbageCollector.Instance;
  BodyScopeRooted := False;
  SelfRooted := False;
  CallScopeRooted := False;
  FunctionContextPushed := False;
  AsyncEvaluationRooted := False;
  AsyncEvaluation := nil;
  AsyncBodyStatements := nil;
  SyntheticReturn := nil;
  CallScope := nil;
  PreviousContinuation := nil;
  PreviousRealm := CurrentRealm;
  RealmSwitched := False;

  try
  try
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(AFunction);
      SelfRooted := True;
    end;

    CallScope := AFunction.CreateCallScope;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(CallScope);
      CallScopeRooted := True;
    end;

    PushCurrentFunctionExecutionContext(CallScope, AFunction);
    FunctionContextPushed := True;

    RealmSwitched := Assigned(AFunction.CreationRealm) and
      (AFunction.CreationRealm <> PreviousRealm);
    if RealmSwitched then
      SetCurrentRealm(AFunction.CreationRealm);
    PreviousContinuation := SuspendCurrentGeneratorContinuation;

    PrepareCallContext(AFunction, CallScope, AArguments, AThisValue, Context,
      BodyScope, BodyScopeRooted);

    // An expression body has no declarations to hoist; skip the no-op passes.
    if not (AFunction.IsExpressionBody and (BodyStatements.Count = 1)) then
    begin
      HoistVarDeclarations(BodyStatements, BodyScope, Context);
      PredeclareFunctionBodyLexicalDeclarations(BodyStatements, BodyScope);
      HoistFunctionDeclarations(BodyStatements, Context);
    end;

    AsyncBodyStatements := TObjectList<TGocciaASTNode>.Create(False);
    if AFunction.IsExpressionBody and (BodyStatements.Count = 1) then
    begin
      SyntheticReturn := TGocciaReturnStatement.Create(
        TGocciaExpression(BodyStatements[0]), BodyStatements[0].Span);
      AsyncBodyStatements.Add(SyntheticReturn);
    end
    else
      for I := 0 to BodyStatements.Count - 1 do
        AsyncBodyStatements.Add(BodyStatements[I]);

    AsyncEvaluation := TGocciaAsyncFunctionEvaluation.Create(AFunction,
      AsyncBodyStatements, SyntheticReturn, BodyScope, Context);
    AsyncBodyStatements := nil;
    SyntheticReturn := nil;
    if Assigned(GC) then
    begin
      GC.AddTempRoot(AsyncEvaluation);
      AsyncEvaluationRooted := True;
    end;
    try
      Result := AsyncEvaluation.Start;
    finally
      if AsyncEvaluationRooted and Assigned(GC) then
        GC.RemoveTempRoot(AsyncEvaluation);
    end;
  except
    on E: Exception do
    begin
      RejectedPromise := TGocciaPromiseValue.Create;
      if Assigned(GC) then
        GC.AddTempRoot(RejectedPromise);
      try
        Rejected := TryRejectAsyncPromiseWithException(RejectedPromise, E);
        if Rejected then
          Result := RejectedPromise;
      finally
        if Assigned(GC) then
          GC.RemoveTempRoot(RejectedPromise);
      end;
      { Outside the temp-root frame: FPC rejects a re-raise inside a nested
        try/finally, and the root has to come off either way. }
      if not Rejected then
        raise;
    end;
  end;

  finally
    if Assigned(AsyncBodyStatements) then
      AsyncBodyStatements.Free;
    if Assigned(SyntheticReturn) then
      SyntheticReturn.Free;
    if BodyScopeRooted and Assigned(GC) then
      GC.PopActiveRoot;
    RestoreCurrentGeneratorContinuation(PreviousContinuation);
    if RealmSwitched then
      SetCurrentRealm(PreviousRealm);
    if FunctionContextPushed then
      PopCurrentFunctionExecutionContext;
    if Assigned(GC) then
    begin
      if CallScopeRooted then
        GC.PopActiveRoot;
      if SelfRooted then
        GC.PopActiveRoot;
    end;
  end;
end;

initialization
  RegisterFunctionCallPaths(ExecuteFunctionBody, StartAsyncFunction);

end.
