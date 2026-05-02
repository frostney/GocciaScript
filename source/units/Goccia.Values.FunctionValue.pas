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
    function ExecuteBody(const ACallScope: TGocciaScope; const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;

    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
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
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); override;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue; // TGocciaClassValue stored as TGocciaValue to avoid circular dependency
  protected
    function CreateCallScope: TGocciaScope; override;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
    function CallWithThisValue(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; out AFinalThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;

    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass write FOwningClass;
  end;

implementation

uses
  SysUtils,

  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.ControlFlow,
  Goccia.Coverage,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.GarbageCollector,
  Goccia.Types.Enforcement,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper;

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
begin
  // Non-arrow function: use call-site this (shorthand methods, class methods, getters/setters)
  ACallScope.ThisValue := AThisValue;
end;

function TGocciaFunctionValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaCallScope.Create(FClosure, FName, Length(FParameters) + 2);
end;

function TGocciaFunctionValue.ExecuteBody(const ACallScope: TGocciaScope; const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I, J: Integer;
  ReturnValue: TGocciaValue;
  CF: TGocciaControlFlow;
  Context: TGocciaEvaluationContext;
  ParamTypeHint: TGocciaLocalType;
begin
  // Set up evaluation context — inherit OnError, LoadModule and the
  // strict-types flag from the closure scope so the body sees the
  // same enforcement setting as the surrounding lexical scope.
  // EffectiveStrictTypes walks to the root scope so closures observe
  // updates made by TGocciaEngine.SetStrictTypes after the closure's
  // lexical scope was created.
  Context.Scope := FClosure;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := FClosure.LoadModule;
  Context.CurrentFilePath := FSourceFilePath;
  Context.CoverageEnabled := Assigned(TGocciaCoverageTracker.Instance)
    and TGocciaCoverageTracker.Instance.Enabled;
  Context.StrictTypes := FClosure.EffectiveStrictTypes;
  Context.DisposalTracker := nil;

  // Record coverage hit on the declaration line (get/set/constructor/method)
  if Context.CoverageEnabled and (FSourceLine > 0) and (FSourceFilePath <> '') then
    TGocciaCoverageTracker.Instance.RecordLineHit(FSourceFilePath, FSourceLine);

  // Bind this via virtual dispatch (arrow vs non-arrow)
  BindThis(ACallScope, AThisValue);

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
        ACallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue, dtParameter);

        // Strict-types: the rest parameter annotation describes the rest
        // array's type (e.g. (...nums: number[])).  Record the type hint on
        // the binding so subsequent reassignments to a non-matching value
        // throw under --strict-types.  The rest array itself is whatever it
        // is — skip initial enforcement, matching the bytecode side which
        // skips IsRest in EmitParameterTypeChecks.
        if Context.StrictTypes and (FParameters[I].TypeAnnotation <> '') then
        begin
          ParamTypeHint := TypeAnnotationToLocalType(FParameters[I].TypeAnnotation);
          if ParamTypeHint <> sltUntyped then
            ACallScope.SetOwnBindingTypeHint(FParameters[I].Name, ParamTypeHint);
        end;

        Break;
      end
      else if FParameters[I].IsPattern then
      begin
        if I < AArguments.Length then
          ReturnValue := AArguments.GetElement(I)
        else if Assigned(FParameters[I].DefaultValue) then
          ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context)
        else
          ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;

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
          ACallScope.DefineLexicalBinding(FParameters[I].Name, AArguments.GetElement(I), dtParameter)
        else
        begin
          if Assigned(FParameters[I].DefaultValue) then
          begin
            ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context);
            ACallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue, dtParameter);
          end
          else
            ACallScope.DefineLexicalBinding(FParameters[I].Name, TGocciaUndefinedLiteralValue.UndefinedValue, dtParameter);
        end;
      end;
    end;
  end;

  Context.Scope := ACallScope;
  if Assigned(Context.OnError) and not Assigned(ACallScope.OnError) then
    ACallScope.OnError := Context.OnError;

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

  // Hoist var declarations to function scope
  HoistVarDeclarations(FBodyStatements, ACallScope);

  // Hoist function declarations (both name and value) to function scope
  HoistFunctionDeclarations(FBodyStatements, Context);

  // Expression-body fast path: expression bodies cannot contain return/break
  if FIsExpressionBody and (FBodyStatements.Count = 1) then
  begin
    Result := EvaluateExpression(TGocciaExpression(FBodyStatements[0]), Context);
    Exit;
  end;

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
end;

function TGocciaFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
begin
  CallScope := CreateCallScope;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PushActiveRoot(CallScope);
  try
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PopActiveRoot;
  end;
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

procedure TGocciaArrowFunctionValue.BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue);
var
  ClosureScope: TGocciaScope;
begin
  // Arrow functions always inherit 'this' from their lexical (closure) scope,
  // per ECMAScript spec. They never use the call-site AThisValue.
  ClosureScope := FClosure;
  while Assigned(ClosureScope) do
  begin
    if Assigned(ClosureScope.ThisValue) and not (ClosureScope.ThisValue is TGocciaUndefinedLiteralValue) then
    begin
      ACallScope.ThisValue := ClosureScope.ThisValue;
      Break;
    end;
    ClosureScope := ClosureScope.Parent;
  end;
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABodyStatements, AClosure, AName);
  FSuperClass := ASuperClass;
end;

function TGocciaMethodValue.CallWithThisValue(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  out AFinalThisValue: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
begin
  AFinalThisValue := AThisValue;
  CallScope := CreateCallScope;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PushActiveRoot(CallScope);
  try
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
    AFinalThisValue := CallScope.ThisValue;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PopActiveRoot;
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
