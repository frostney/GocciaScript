// This unit defines TGocciaFunctionValue for user-defined functions.
// TGocciaArrowFunctionValue handles arrow functions (lexical this).
// TGocciaMethodValue handles class methods (with super/owning class).
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Evaluator.Context,
  Goccia.Scope,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

type
  TGocciaFunctionValue = class(TGocciaFunctionBase)
  protected
    FName: string;
    FParameters: TGocciaParameterArray;
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FClosure: TGocciaScope;
    FSourceFilePath: string;
    FSourceLine: Integer;
    FSourceText: string;
    FIsExpressionBody: Boolean;
    FIsSimpleParams: Boolean;
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
    function GetSourceText: string; override;
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); virtual;
    function CreateCallScope: TGocciaScope; virtual;
    function CreatesArgumentsObject: Boolean; virtual;
    function HasParameterExpressions: Boolean;
    procedure PredeclareParameterBindings(const ACallScope: TGocciaScope);
    function BuildParameterEvalVarDeclarationRejectNames(
      const AIncludeArgumentsObject: Boolean): TGocciaEvalRejectNameArray;
    function ExecuteBody(const ACallScope: TGocciaScope; const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallAsync(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CallWithNewTarget(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; const ANewTarget: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;

    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function ConstructWithReceiver(const AArguments: TGocciaArgumentsCollection;
      const AReceiver: TGocciaValue; const ANewTarget: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
    procedure SetInferredName(const AName: string);

    property Parameters: TGocciaParameterArray read FParameters;
    property BodyStatements: TObjectList<TGocciaASTNode> read FBodyStatements;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName write FName;
    property IsExpressionBody: Boolean read FIsExpressionBody write FIsExpressionBody;
    property SourceFilePath: string read FSourceFilePath write FSourceFilePath;
    property SourceLine: Integer read FSourceLine write FSourceLine;
    property SourceText: string read FSourceText write FSourceText;
  end;

  TGocciaArrowFunctionValue = class(TGocciaFunctionValue)
  protected
    function CreateCallScope: TGocciaScope; override;
    function CreatesArgumentsObject: Boolean; override;
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); override;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue; // TGocciaClassValue stored as TGocciaValue to avoid circular dependency
    FLastSuperConstructorCalled: Boolean;
  protected
    function CreateCallScope: TGocciaScope; override;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
    function CallWithThisValue(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; out AFinalThisValue: TGocciaValue;
      const ANewTarget: TGocciaValue = nil): TGocciaValue;
    procedure MarkReferences; override;

    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass write FOwningClass;
    property LastSuperConstructorCalled: Boolean
      read FLastSuperConstructorCalled;
  end;

implementation

uses
  Classes,
  SysUtils,

  Goccia.AST.BindingPatterns,
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
  Goccia.Types.Enforcement,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.ToObject;

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
    procedure RejectWithException(const AException: Exception);
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

procedure RejectAsyncPromiseWithException(const APromise: TGocciaPromiseValue;
  const AException: Exception);
begin
  if AException is TGocciaThrowValue then
    APromise.Reject(TGocciaThrowValue(AException).Value)
  else if AException is TGocciaTypeError then
    APromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
  else if AException is TGocciaReferenceError then
    APromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
  else if AException is TGocciaSyntaxError then
    APromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
  else if AException is TGocciaRuntimeError then
    APromise.Reject(CreateErrorObject(ERROR_NAME, AException.Message))
  else
    raise AException;
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

procedure TGocciaAsyncFunctionEvaluation.RejectWithException(
  const AException: Exception);
begin
  if FSettled then
    Exit;
  FSettled := True;
  RejectAsyncPromiseWithException(FPromise, AException);
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
        RejectWithException(E);
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
  if Assigned(FPromise) then
    FPromise.MarkReferences;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
end;

{ TGocciaFunctionValue }

constructor TGocciaFunctionValue.Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string = '');
var
  I: Integer;
begin
  FParameters := AParameters;
  FBodyStatements := ABodyStatements;
  FClosure := AClosure;
  FName := AName;

  // Pre-compute whether all parameters are simple named params (no rest, no destructuring, no defaults)
  FIsSimpleParams := True;
  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest or FParameters[I].IsPattern or Assigned(FParameters[I].DefaultValue) then
    begin
      FIsSimpleParams := False;
      Break;
    end;
  end;

  inherited Create;
end;

destructor TGocciaFunctionValue.Destroy;
begin
  FBodyStatements.Free;
  inherited;
end;

procedure TGocciaFunctionValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark the closure scope
  if Assigned(FClosure) then
    FClosure.MarkReferences;
end;

procedure TGocciaFunctionValue.BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue);
var
  Root: TGocciaScope;
begin
  if FStrictThis then
    ACallScope.ThisValue := AThisValue
  else if not Assigned(AThisValue) or
          (AThisValue is TGocciaUndefinedLiteralValue) or
          (AThisValue is TGocciaNullLiteralValue) then
  begin
    Root := FClosure;
    while Assigned(Root.Parent) do
      Root := Root.Parent;
    ACallScope.ThisValue := Root.ThisValue;
  end
  else
    ACallScope.ThisValue := CoerceNonStrictThis(AThisValue, nil);
end;

function TGocciaFunctionValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaCallScope.Create(FClosure, FName, Length(FParameters) + 2);
end;

function TGocciaFunctionValue.CreatesArgumentsObject: Boolean;
begin
  Result := True;
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

function TGocciaFunctionValue.HasParameterExpressions: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FParameters) do
    if Assigned(FParameters[I].DefaultValue) or
       (FParameters[I].IsPattern and
        DestructuringPatternHasParameterExpression(FParameters[I].Pattern)) then
      Exit(True);
  Result := False;
end;

procedure TGocciaFunctionValue.PredeclareParameterBindings(
  const ACallScope: TGocciaScope);
var
  I, J: Integer;
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    Names.CaseSensitive := True;
    for I := 0 to High(FParameters) do
    begin
      Names.Clear;
      if FParameters[I].IsPattern then
        CollectPatternBindingNames(FParameters[I].Pattern, Names, True)
      else if FParameters[I].Name <> '' then
        Names.Add(FParameters[I].Name);

      for J := 0 to Names.Count - 1 do
        if not ACallScope.ContainsOwnLexicalBinding(Names[J]) then
          ACallScope.PredeclareLexicalBinding(Names[J], dtParameter);
    end;
  finally
    Names.Free;
  end;
end;

function TGocciaFunctionValue.BuildParameterEvalVarDeclarationRejectNames(
  const AIncludeArgumentsObject: Boolean): TGocciaEvalRejectNameArray;
var
  Names: TStringList;
  I, J: Integer;
  procedure AddName(const AName: string);
  begin
    if (AName <> '') and (Names.IndexOf(AName) < 0) then
      Names.Add(AName);
  end;
begin
  Names := TStringList.Create;
  try
    Names.CaseSensitive := True;
    if AIncludeArgumentsObject then
      AddName(IDENTIFIER_ARGUMENTS);
    for I := 0 to High(FParameters) do
    begin
      if FParameters[I].IsPattern then
        CollectPatternBindingNames(FParameters[I].Pattern, Names, True)
      else
        AddName(FParameters[I].Name);
    end;

    SetLength(Result, Names.Count);
    for J := 0 to Names.Count - 1 do
      Result[J] := Names[J];
  finally
    Names.Free;
  end;
end;

function TGocciaFunctionValue.ExecuteBody(const ACallScope: TGocciaScope; const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I, J: Integer;
  CompatibilityNonStrictMode: Boolean;
  ArgumentsObjectEnabled: Boolean;
  EvalRejectNames, SavedEvalRejectNames: TGocciaEvalRejectNameArray;
  ReturnValue: TGocciaValue;
  CF: TGocciaControlFlow;
  Context: TGocciaEvaluationContext;
  ParamTypeHint: TGocciaLocalType;
  ParameterNames: array of string;
  BodyScope: TGocciaScope;
  HasParamExpressions: Boolean;
  PreviousContinuation: TGocciaGeneratorContinuation;
  PreviousRealm: TGocciaRealm;
  RealmSwitched: Boolean;
  GC: TGarbageCollector;
  BodyScopeRooted: Boolean;
  function EvaluateParameterDefault(
    const AExpression: TGocciaExpression): TGocciaValue;
  var
    SavedRejectArgumentsVarDeclaration: Boolean;
  begin
    SavedRejectArgumentsVarDeclaration :=
      Context.RejectArgumentsVarDeclarationInEval;
    SavedEvalRejectNames := Context.RejectVarDeclarationNamesInEval;
    Context.RejectArgumentsVarDeclarationInEval :=
      ArgumentsObjectEnabled and CreatesArgumentsObject;
    Context.RejectVarDeclarationNamesInEval := EvalRejectNames;
    try
      Result := EvaluateExpression(AExpression, Context);
    finally
      Context.RejectArgumentsVarDeclarationInEval :=
        SavedRejectArgumentsVarDeclaration;
      Context.RejectVarDeclarationNamesInEval := SavedEvalRejectNames;
    end;
  end;
  function CreateArgumentsObjectForCall: TGocciaValue;
  var
    ParameterIndex: Integer;
  begin
    if Context.NonStrictMode and FIsSimpleParams then
    begin
      SetLength(ParameterNames, Length(FParameters));
      for ParameterIndex := 0 to High(FParameters) do
        ParameterNames[ParameterIndex] := FParameters[ParameterIndex].Name;
      Exit(CreateMappedArgumentsObject(AArguments, ParameterNames,
        ACallScope, Self));
    end;
    Result := CreateUnmappedArgumentsObject(AArguments);
  end;
begin
  GC := TGarbageCollector.Instance;
  BodyScopeRooted := False;
  PreviousRealm := CurrentRealm;
  RealmSwitched := Assigned(CreationRealm) and (CreationRealm <> PreviousRealm);
  if RealmSwitched then
    SetCurrentRealm(CreationRealm);
  PreviousContinuation := SuspendCurrentGeneratorContinuation;
  try
  // Set up evaluation context — inherit OnError, LoadModule and the
  // strict-types flag from the closure scope so the body sees the
  // same enforcement setting as the surrounding lexical scope.
  // EffectiveStrictTypes walks to the root scope so closures observe
  // updates made by TGocciaEngine.SetStrictTypes after the closure's
  // lexical scope was created.
  FillChar(Context, SizeOf(Context), 0);
  Context.Realm := CurrentRealm;
  Context.Scope := FClosure;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := FClosure.LoadModule;
  Context.LoadModuleSource := FClosure.LoadModuleSource;
  Context.CurrentFilePath := FSourceFilePath;
  Context.CoverageEnabled := Assigned(TGocciaCoverageTracker.Instance)
    and TGocciaCoverageTracker.Instance.Enabled;
  Context.StrictTypes := FClosure.EffectiveStrictTypes;
  CompatibilityNonStrictMode := FClosure.EffectiveNonStrictMode;
  ArgumentsObjectEnabled := FClosure.EffectiveArgumentsObjectEnabled;
  Context.NonStrictMode := CompatibilityNonStrictMode and not FStrictCode;
  Context.CompatibilityNonStrictMode := CompatibilityNonStrictMode;
  Context.DisposalTracker := nil;
  HasParamExpressions := HasParameterExpressions;
  // EvalRejectNames is only read while evaluating a parameter default, so
  // only build it when a parameter actually has a default or pattern
  // expression. Simple-parameter functions skip the allocation and the
  // O(n^2) name dedup entirely.
  if HasParamExpressions then
    EvalRejectNames := BuildParameterEvalVarDeclarationRejectNames(
      ArgumentsObjectEnabled and CreatesArgumentsObject and
      not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS))
  else
    EvalRejectNames := nil;

  // Record coverage hit on the declaration line (get/set/constructor/method)
  if Context.CoverageEnabled and (FSourceLine > 0) and (FSourceFilePath <> '') then
    TGocciaCoverageTracker.Instance.RecordLineHit(FSourceFilePath, FSourceLine);

  // Bind this via virtual dispatch (arrow vs non-arrow)
  BindThis(ACallScope, AThisValue);
  Context.Scope := ACallScope;

  if ArgumentsObjectEnabled and CreatesArgumentsObject and
     not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS) and
     not ACallScope.ContainsOwnLexicalBinding(IDENTIFIER_ARGUMENTS) then
    ACallScope.DefineVariableBinding(IDENTIFIER_ARGUMENTS,
      CreateArgumentsObjectForCall, True);

  if HasParamExpressions then
    PredeclareParameterBindings(ACallScope);

  // Bind parameters - fast path for simple named params (no rest/destructuring/defaults)
  if FIsSimpleParams then
  begin
    for I := 0 to Length(FParameters) - 1 do
    begin
      if I < AArguments.Length then
        ACallScope.DefineLexicalBinding(FParameters[I].Name, AArguments.GetElement(I), dtParameter)
      else
        ACallScope.DefineLexicalBinding(FParameters[I].Name, TGocciaUndefinedLiteralValue.UndefinedValue, dtParameter);
    end;
  end
  else
  begin
    // Full parameter binding with rest, destructuring, and defaults
    for I := 0 to Length(FParameters) - 1 do
    begin
      if FParameters[I].IsRest then
      begin
        ReturnValue := TGocciaArrayValue.Create;
        if I < AArguments.Length then
          for J := I to AArguments.Length - 1 do
            TGocciaArrayValue(ReturnValue).Elements.Add(AArguments.GetElement(J));
        if FParameters[I].IsPattern then
        begin
          Context.Scope := ACallScope;
          AssignPattern(FParameters[I].Pattern, ReturnValue, Context, True);
        end
        else
          ACallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue,
            dtParameter);

        // Strict-types: the rest parameter annotation describes the rest
        // array's type (e.g. (...nums: number[])).  Record the type hint on
        // the binding so subsequent reassignments to a non-matching value
        // throw under --strict-types.  The rest array itself is whatever it
        // is — skip initial enforcement, matching the bytecode side which
        // skips IsRest in EmitParameterTypeChecks.
        if Context.StrictTypes and (FParameters[I].TypeAnnotation <> '') then
        begin
          ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
          if (ParamTypeHint <> sltUntyped) and not FParameters[I].IsPattern then
            ACallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
        end;

        Break;
      end
      else if FParameters[I].IsPattern then
      begin
        if I < AArguments.Length then
          ReturnValue := AArguments.GetElement(I)
        else
          ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if Assigned(FParameters[I].DefaultValue) and
           (ReturnValue is TGocciaUndefinedLiteralValue) then
          ReturnValue := EvaluateParameterDefault(FParameters[I].DefaultValue);

        // Strict-types enforcement on destructured parameters: enforce on the
        // raw pre-destructured value, matching the bytecode side which checks
        // __paramN before EmitDestructuringParameters.  Defaults and optionals
        // skip enforcement (parity with the named-parameter loop below).
        if Context.StrictTypes
          and (FParameters[I].TypeAnnotation <> '')
          and not FParameters[I].IsOptional
          and not Assigned(FParameters[I].DefaultValue) then
        begin
          ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
          if ParamTypeHint <> sltUntyped then
            EnforceStrictType(ReturnValue, ParamTypeHint);
        end;

        Context.Scope := ACallScope;
        AssignPattern(FParameters[I].Pattern, ReturnValue, Context, True);
      end
      else
      begin
        if I < AArguments.Length then
          ReturnValue := AArguments.GetElement(I)
        else
          ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if Assigned(FParameters[I].DefaultValue) and
           (ReturnValue is TGocciaUndefinedLiteralValue) then
          ReturnValue := EvaluateParameterDefault(FParameters[I].DefaultValue);
        ACallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue, dtParameter);
      end;
    end;
  end;

  BodyScope := ACallScope;
  if HasParamExpressions then
  begin
    BodyScope := ACallScope.CreateChild(skFunction, FName + ':body');
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(BodyScope);
      BodyScopeRooted := True;
    end;
  end;

  Context.Scope := BodyScope;
  if Assigned(Context.OnError) and not Assigned(BodyScope.OnError) then
    BodyScope.OnError := Context.OnError;

  // Strict-types enforcement on parameters: when --strict-types is on,
  // type-annotated parameters reject incompatible argument values, and
  // the recorded TypeHint guards subsequent reassignments to the
  // parameter binding inside the body.  Mirrors the bytecode side's
  // ApplyParameterTypeAnnotations + EmitParameterTypeChecks.
  if Context.StrictTypes then
  begin
    for I := 0 to Length(FParameters) - 1 do
    begin
      if FParameters[I].TypeAnnotation = '' then
        Continue;
      if FParameters[I].IsRest then
        Continue;
      if FParameters[I].IsOptional then
        Continue;
      if Assigned(FParameters[I].DefaultValue) then
        Continue;
      if FParameters[I].IsPattern then
        Continue;
      ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
      if ParamTypeHint = sltUntyped then
        Continue;
      EnforceStrictType(ACallScope.GetValue(FParameters[I].Name),
        ParamTypeHint);
      ACallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
    end;
  end;

  // Expression-body fast path: an expression body cannot contain var,
  // function, or lexical declarations, so the hoisting passes below are
  // no-ops for it. Evaluate the single expression directly and skip them.
  if FIsExpressionBody and (FBodyStatements.Count = 1) then
  begin
    Result := EvaluateExpression(TGocciaExpression(FBodyStatements[0]), Context);
    Exit;
  end;

  // Hoist var declarations to function scope
  HoistVarDeclarations(FBodyStatements, BodyScope, Context);

  // Function-body lexical declarations are in TDZ before hoisted function
  // declarations are created, so closures capture the uninitialized binding.
  PredeclareFunctionBodyLexicalDeclarations(FBodyStatements, BodyScope);

  // Hoist function declarations (both name and value) to function scope
  HoistFunctionDeclarations(FBodyStatements, Context);

  // Single-return fast path: (x) => { return expr; } — evaluate the return
  // expression directly, bypassing the statement loop
  if (FBodyStatements.Count = 1) and (FBodyStatements[0] is TGocciaReturnStatement) then
  begin
    if TGocciaReturnStatement(FBodyStatements[0]).Value <> nil then
      Result := EvaluateExpression(TGocciaExpression(TGocciaReturnStatement(FBodyStatements[0]).Value), Context)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  for I := 0 to FBodyStatements.Count - 1 do
  begin
    if FBodyStatements[I] is TGocciaExpression then
      CF := TGocciaControlFlow.Normal(EvaluateExpression(TGocciaExpression(FBodyStatements[I]), Context))
    else
      CF := EvaluateStatement(TGocciaStatement(FBodyStatements[I]), Context);
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

function TGocciaFunctionValue.CallAsync(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  I, J: Integer;
  CompatibilityNonStrictMode: Boolean;
  ArgumentsObjectEnabled: Boolean;
  EvalRejectNames, SavedEvalRejectNames: TGocciaEvalRejectNameArray;
  ReturnValue: TGocciaValue;
  Context: TGocciaEvaluationContext;
  ParamTypeHint: TGocciaLocalType;
  ParameterNames: array of string;
  CallScope: TGocciaScope;
  BodyScope: TGocciaScope;
  HasParamExpressions: Boolean;
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
  function EvaluateParameterDefault(
    const AExpression: TGocciaExpression): TGocciaValue;
  var
    SavedRejectArgumentsVarDeclaration: Boolean;
  begin
    SavedRejectArgumentsVarDeclaration :=
      Context.RejectArgumentsVarDeclarationInEval;
    SavedEvalRejectNames := Context.RejectVarDeclarationNamesInEval;
    Context.RejectArgumentsVarDeclarationInEval :=
      ArgumentsObjectEnabled and CreatesArgumentsObject;
    Context.RejectVarDeclarationNamesInEval := EvalRejectNames;
    try
      Result := EvaluateExpression(AExpression, Context);
    finally
      Context.RejectArgumentsVarDeclarationInEval :=
        SavedRejectArgumentsVarDeclaration;
      Context.RejectVarDeclarationNamesInEval := SavedEvalRejectNames;
    end;
  end;
  function CreateArgumentsObjectForCall: TGocciaValue;
  var
    ParameterIndex: Integer;
  begin
    if Context.NonStrictMode and FIsSimpleParams then
    begin
      SetLength(ParameterNames, Length(FParameters));
      for ParameterIndex := 0 to High(FParameters) do
        ParameterNames[ParameterIndex] := FParameters[ParameterIndex].Name;
      Exit(CreateMappedArgumentsObject(AArguments, ParameterNames,
        CallScope, Self));
    end;
    Result := CreateUnmappedArgumentsObject(AArguments);
  end;
begin
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
      GC.PushActiveRoot(Self);
      SelfRooted := True;
    end;

    CallScope := CreateCallScope;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(CallScope);
      CallScopeRooted := True;
    end;

    PushCurrentFunctionExecutionContext(CallScope, Self);
    FunctionContextPushed := True;

    RealmSwitched := Assigned(CreationRealm) and
      (CreationRealm <> PreviousRealm);
    if RealmSwitched then
      SetCurrentRealm(CreationRealm);
    PreviousContinuation := SuspendCurrentGeneratorContinuation;

    FillChar(Context, SizeOf(Context), 0);
    Context.Realm := CurrentRealm;
    Context.Scope := FClosure;
    Context.OnError := FClosure.OnError;
    Context.LoadModule := FClosure.LoadModule;
    Context.LoadModuleSource := FClosure.LoadModuleSource;
    Context.CurrentFilePath := FSourceFilePath;
    Context.CoverageEnabled := Assigned(TGocciaCoverageTracker.Instance)
      and TGocciaCoverageTracker.Instance.Enabled;
    Context.StrictTypes := FClosure.EffectiveStrictTypes;
    CompatibilityNonStrictMode := FClosure.EffectiveNonStrictMode;
    ArgumentsObjectEnabled := FClosure.EffectiveArgumentsObjectEnabled;
    Context.NonStrictMode := CompatibilityNonStrictMode and not FStrictCode;
    Context.CompatibilityNonStrictMode := CompatibilityNonStrictMode;
    Context.DisposalTracker := nil;
    HasParamExpressions := HasParameterExpressions;
    if HasParamExpressions then
      EvalRejectNames := BuildParameterEvalVarDeclarationRejectNames(
        ArgumentsObjectEnabled and CreatesArgumentsObject and
        not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS))
    else
      EvalRejectNames := nil;

    if Context.CoverageEnabled and (FSourceLine > 0) and
       (FSourceFilePath <> '') then
      TGocciaCoverageTracker.Instance.RecordLineHit(FSourceFilePath,
        FSourceLine);

    BindThis(CallScope, AThisValue);
    Context.Scope := CallScope;

    if ArgumentsObjectEnabled and CreatesArgumentsObject and
       not ParameterListBindsName(FParameters, IDENTIFIER_ARGUMENTS) and
       not CallScope.ContainsOwnLexicalBinding(IDENTIFIER_ARGUMENTS) then
      CallScope.DefineVariableBinding(IDENTIFIER_ARGUMENTS,
        CreateArgumentsObjectForCall, True);

    if HasParamExpressions then
      PredeclareParameterBindings(CallScope);

    if FIsSimpleParams then
    begin
      for I := 0 to Length(FParameters) - 1 do
      begin
        if I < AArguments.Length then
          CallScope.DefineLexicalBinding(FParameters[I].Name,
            AArguments.GetElement(I), dtParameter)
        else
          CallScope.DefineLexicalBinding(FParameters[I].Name,
            TGocciaUndefinedLiteralValue.UndefinedValue, dtParameter);
      end;
    end
    else
    begin
      for I := 0 to Length(FParameters) - 1 do
      begin
        if FParameters[I].IsRest then
        begin
          ReturnValue := TGocciaArrayValue.Create;
          if I < AArguments.Length then
            for J := I to AArguments.Length - 1 do
              TGocciaArrayValue(ReturnValue).Elements.Add(
                AArguments.GetElement(J));
          if FParameters[I].IsPattern then
          begin
            Context.Scope := CallScope;
            AssignPattern(FParameters[I].Pattern, ReturnValue, Context, True);
          end
          else
            CallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue,
              dtParameter);

          if Context.StrictTypes and
             (FParameters[I].TypeAnnotation <> '') then
          begin
            ParamTypeHint :=
              TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
            if (ParamTypeHint <> sltUntyped) and
               not FParameters[I].IsPattern then
              CallScope.SetOwnBindingTypeHint(FParameters[I].Name,
                ParamTypeHint);
          end;

          Break;
        end
        else if FParameters[I].IsPattern then
        begin
          if I < AArguments.Length then
            ReturnValue := AArguments.GetElement(I)
          else
            ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          if Assigned(FParameters[I].DefaultValue) and
             (ReturnValue is TGocciaUndefinedLiteralValue) then
            ReturnValue :=
              EvaluateParameterDefault(FParameters[I].DefaultValue);

          if Context.StrictTypes
            and (FParameters[I].TypeAnnotation <> '')
            and not FParameters[I].IsOptional
            and not Assigned(FParameters[I].DefaultValue) then
          begin
            ParamTypeHint :=
              TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
            if ParamTypeHint <> sltUntyped then
              EnforceStrictType(ReturnValue, ParamTypeHint);
          end;

          Context.Scope := CallScope;
          AssignPattern(FParameters[I].Pattern, ReturnValue, Context, True);
        end
        else
        begin
          if I < AArguments.Length then
            ReturnValue := AArguments.GetElement(I)
          else
            ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          if Assigned(FParameters[I].DefaultValue) and
             (ReturnValue is TGocciaUndefinedLiteralValue) then
            ReturnValue :=
              EvaluateParameterDefault(FParameters[I].DefaultValue);
          CallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue,
            dtParameter);
        end;
      end;
    end;

    BodyScope := CallScope;
    if HasParamExpressions then
    begin
      BodyScope := CallScope.CreateChild(skFunction, FName + ':body');
      if Assigned(GC) then
      begin
        GC.PushActiveRoot(BodyScope);
        BodyScopeRooted := True;
      end;
    end;

    Context.Scope := BodyScope;
    if Assigned(Context.OnError) and not Assigned(BodyScope.OnError) then
      BodyScope.OnError := Context.OnError;

    if Context.StrictTypes then
    begin
      for I := 0 to Length(FParameters) - 1 do
      begin
        if FParameters[I].TypeAnnotation = '' then
          Continue;
        if FParameters[I].IsRest then
          Continue;
        if FParameters[I].IsOptional then
          Continue;
        if Assigned(FParameters[I].DefaultValue) then
          Continue;
        if FParameters[I].IsPattern then
          Continue;
        ParamTypeHint :=
          TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
        if ParamTypeHint = sltUntyped then
          Continue;
        EnforceStrictType(CallScope.GetValue(FParameters[I].Name),
          ParamTypeHint);
        CallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
      end;
    end;

    // An expression body has no declarations to hoist; skip the no-op passes.
    if not (FIsExpressionBody and (FBodyStatements.Count = 1)) then
    begin
      HoistVarDeclarations(FBodyStatements, BodyScope, Context);
      PredeclareFunctionBodyLexicalDeclarations(FBodyStatements, BodyScope);
      HoistFunctionDeclarations(FBodyStatements, Context);
    end;

    AsyncBodyStatements := TObjectList<TGocciaASTNode>.Create(False);
    if FIsExpressionBody and (FBodyStatements.Count = 1) then
    begin
      SyntheticReturn := TGocciaReturnStatement.Create(
        TGocciaExpression(FBodyStatements[0]), FBodyStatements[0].Line,
        FBodyStatements[0].Column);
      AsyncBodyStatements.Add(SyntheticReturn);
    end
    else
      for I := 0 to FBodyStatements.Count - 1 do
        AsyncBodyStatements.Add(FBodyStatements[I]);

    AsyncEvaluation := TGocciaAsyncFunctionEvaluation.Create(Self,
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
        RejectAsyncPromiseWithException(RejectedPromise, E);
        Result := RejectedPromise;
      finally
        if Assigned(GC) then
          GC.RemoveTempRoot(RejectedPromise);
      end;
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

function TGocciaFunctionValue.CallWithNewTarget(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
  GC: TGarbageCollector;
  SelfRooted: Boolean;
  CallScopeRooted: Boolean;
  FunctionContextPushed: Boolean;
begin
  GC := TGarbageCollector.Instance;
  SelfRooted := False;
  CallScopeRooted := False;
  FunctionContextPushed := False;
  CallScope := nil;
  if Assigned(GC) then
  begin
    GC.PushActiveRoot(Self);
    SelfRooted := True;
  end;
  try
    CallScope := CreateCallScope;
    if Assigned(ANewTarget) and (CallScope is TGocciaCallScope) then
      TGocciaCallScope(CallScope).NewTarget := ANewTarget;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(CallScope);
      CallScopeRooted := True;
    end;
    PushCurrentFunctionExecutionContext(CallScope, Self);
    FunctionContextPushed := True;
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
  finally
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

function TGocciaFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CallWithNewTarget(AArguments, AThisValue, nil);
end;

function TGocciaFunctionValue.ConstructWithReceiver(
  const AArguments: TGocciaArgumentsCollection; const AReceiver: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := CallWithNewTarget(AArguments, AReceiver, ANewTarget);
end;

function TGocciaFunctionValue.GetFunctionLength: Integer;
var
  I: Integer;
begin
  // ECMAScript: length is the number of formal parameters before the first
  // one with a default value or rest parameter
  Result := 0;
  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest then Break;
    if Assigned(FParameters[I].DefaultValue) then Break;
    Inc(Result);
  end;
end;

function TGocciaFunctionValue.GetFunctionName: string;
begin
  Result := FName;
end;

function TGocciaFunctionValue.GetSourceText: string;
begin
  Result := FSourceText;
end;

procedure TGocciaFunctionValue.SetInferredName(const AName: string);
begin
  if FName = '' then
    FName := AName;
end;

{ TGocciaArrowFunctionValue }

function TGocciaArrowFunctionValue.CreatesArgumentsObject: Boolean;
begin
  Result := False;
end;

function TGocciaArrowFunctionValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaArrowCallScope.Create(FClosure, FName, Length(FParameters) + 2);
end;

procedure TGocciaArrowFunctionValue.BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue);
begin
  ACallScope.ThisValue := FClosure.ThisValue;
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABodyStatements, AClosure, AName);
  FSuperClass := ASuperClass;
  FLastSuperConstructorCalled := False;
end;

function TGocciaMethodValue.CallWithThisValue(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  out AFinalThisValue: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
  GC: TGarbageCollector;
  SelfRooted: Boolean;
  CallScopeRooted: Boolean;
  FunctionContextPushed: Boolean;
begin
  AFinalThisValue := AThisValue;
  GC := TGarbageCollector.Instance;
  SelfRooted := False;
  CallScopeRooted := False;
  FunctionContextPushed := False;
  CallScope := nil;
  if Assigned(GC) then
  begin
    GC.PushActiveRoot(Self);
    SelfRooted := True;
  end;
  try
    CallScope := CreateCallScope;
    if Assigned(ANewTarget) and (CallScope is TGocciaMethodCallScope) then
      TGocciaMethodCallScope(CallScope).NewTarget := ANewTarget;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(CallScope);
      CallScopeRooted := True;
    end;
    PushCurrentFunctionExecutionContext(CallScope, Self);
    FunctionContextPushed := True;
    if CallScope is TGocciaMethodCallScope then
      TGocciaMethodCallScope(CallScope).SuperConstructorCalled := False;
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
    if (CallScope is TGocciaMethodCallScope) and
       Assigned(TGocciaMethodCallScope(CallScope).SuperClass) and
       not TGocciaMethodCallScope(CallScope).SuperConstructorCalled then
      AFinalThisValue := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      AFinalThisValue := CallScope.ThisValue;
    if CallScope is TGocciaMethodCallScope then
      FLastSuperConstructorCalled :=
        TGocciaMethodCallScope(CallScope).SuperConstructorCalled
    else
      FLastSuperConstructorCalled := False;
  finally
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

function TGocciaMethodValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaMethodCallScope.Create(FClosure, FName, FSuperClass, FOwningClass, Length(FParameters) + 2);
end;

procedure TGocciaMethodValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype + closure

  // Mark method-specific references
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;
  if Assigned(FOwningClass) then
    FOwningClass.MarkReferences;
end;

end.
