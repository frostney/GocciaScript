// This unit defines TGocciaFunctionValue for user-defined functions.
// TGocciaArrowFunctionValue handles arrow functions (lexical this).
// TGocciaMethodValue handles class methods (with super/owning class).
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.AST.Node, Goccia.AST.Statements, Goccia.AST.Expressions, Goccia.Scope,
  Goccia.Error, Goccia.Logger, Goccia.Values.Error, Goccia.Values.ObjectValue, Goccia.Values.FunctionBase,
  Goccia.Arguments.Collection, Generics.Collections, Classes, Math, SysUtils,
  Goccia.Values.Primitives, Goccia.Token;

type
  TGocciaFunctionValue = class(TGocciaFunctionBase)
  protected
    FName: string;
    FParameters: TGocciaParameterArray;
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FClosure: TGocciaScope;
    FIsExpressionBody: Boolean;
    FIsSimpleParams: Boolean;
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
    procedure BindThis(CallScope: TGocciaScope; ThisValue: TGocciaValue); virtual;
    function ExecuteBody(CallScope: TGocciaScope; Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;

    function Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; override;
    function CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue; virtual;
    procedure GCMarkReferences; override;

    property Parameters: TGocciaParameterArray read FParameters;
    property BodyStatements: TObjectList<TGocciaASTNode> read FBodyStatements;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName write FName;
    property IsExpressionBody: Boolean read FIsExpressionBody write FIsExpressionBody;
  end;

  TGocciaArrowFunctionValue = class(TGocciaFunctionValue)
  protected
    procedure BindThis(CallScope: TGocciaScope; ThisValue: TGocciaValue); override;
  public
    function CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue; override;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue; // TGocciaClassValue stored as TGocciaValue to avoid circular dependency
  public
    constructor Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string; ASuperClass: TGocciaValue = nil);
    procedure GCMarkReferences; override;

    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass write FOwningClass;
  end;

implementation

uses
  Goccia.Evaluator, Goccia.Values.ClassHelper, Goccia.Values.ArrayValue, Goccia.GarbageCollector;

{ TGocciaFunctionValue }

constructor TGocciaFunctionValue.Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string = '');
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

procedure TGocciaFunctionValue.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark the closure scope
  if Assigned(FClosure) then
    FClosure.GCMarkReferences;
end;

procedure TGocciaFunctionValue.BindThis(CallScope: TGocciaScope; ThisValue: TGocciaValue);
begin
  // Non-arrow function: use call-site this (shorthand methods, class methods, getters/setters)
  CallScope.ThisValue := ThisValue;
  CallScope.DefineLexicalBinding('this', ThisValue, dtUnknown);
end;

function TGocciaFunctionValue.ExecuteBody(CallScope: TGocciaScope; Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I, J: Integer;
  ReturnValue: TGocciaValue;
  Method: TGocciaMethodValue;
  Context: TGocciaEvaluationContext;
begin
  // Set up evaluation context — inherit OnError from the closure scope
  Context.Scope := FClosure;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := nil;

  // Bind this via virtual dispatch (arrow vs non-arrow)
  BindThis(CallScope, ThisValue);

  // If this is a method with a superclass, set up super handling
  if Self is TGocciaMethodValue then
  begin
    Method := TGocciaMethodValue(Self);
    if Assigned(Method.SuperClass) and not (Method.SuperClass is TGocciaUndefinedLiteralValue) then
      CallScope.DefineLexicalBinding('__super__', Method.SuperClass, dtUnknown);

    // Bind the owning class so private field access resolves to the correct class
    if Assigned(Method.OwningClass) then
      CallScope.DefineLexicalBinding('__owning_class__', Method.OwningClass, dtUnknown);
  end;

  // Bind parameters - fast path for simple named params (no rest/destructuring/defaults)
  if FIsSimpleParams then
  begin
    for I := 0 to Length(FParameters) - 1 do
    begin
      if I < Arguments.Length then
        CallScope.DefineLexicalBinding(FParameters[I].Name, Arguments.GetElement(I), dtParameter)
      else
        CallScope.DefineLexicalBinding(FParameters[I].Name, TGocciaUndefinedLiteralValue.UndefinedValue, dtParameter);
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
        if I < Arguments.Length then
          for J := I to Arguments.Length - 1 do
            TGocciaArrayValue(ReturnValue).Elements.Add(Arguments.GetElement(J));
        CallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue, dtParameter);
        Break;
      end
      else if FParameters[I].IsPattern then
      begin
        if I < Arguments.Length then
          ReturnValue := Arguments.GetElement(I)
        else if Assigned(FParameters[I].DefaultValue) then
          ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context)
        else
          ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        Context.Scope := CallScope;
        AssignPattern(FParameters[I].Pattern, ReturnValue, Context, True);
      end
      else
      begin
        if I < Arguments.Length then
          CallScope.DefineLexicalBinding(FParameters[I].Name, Arguments.GetElement(I), dtParameter)
        else
        begin
          if Assigned(FParameters[I].DefaultValue) then
          begin
            ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context);
            CallScope.DefineLexicalBinding(FParameters[I].Name, ReturnValue, dtParameter);
          end
          else
            CallScope.DefineLexicalBinding(FParameters[I].Name, TGocciaUndefinedLiteralValue.UndefinedValue, dtParameter);
        end;
      end;
    end;
  end;

  // Execute function body statements directly
  try
    Context.Scope := CallScope;
    ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;

    ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    for I := 0 to FBodyStatements.Count - 1 do
    begin
      try
        ReturnValue := Evaluate(FBodyStatements[I], Context);
      except
        on E: TGocciaReturnValue do
        begin
          raise;
        end;
        on E: TGocciaThrowValue do
        begin
          // Let thrown values bubble up for try-catch handling
          raise;
        end;
        on E: Exception do
        begin
          raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
        end;
      end;
    end;

    // Expression body (e.g. (x) => x * 2) implicitly returns the last expression value.
    // Block body (e.g. (x) => { x * 2; }) without explicit return always returns undefined.
    if FIsExpressionBody then
      Result := ReturnValue
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: TGocciaReturnValue do
    begin
      if E.Value = nil then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue
      else
      begin
        // Create a new instance of the same type
        if E.Value is TGocciaNumberLiteralValue then
          Result := TGocciaNumberLiteralValue.Create(TGocciaNumberLiteralValue(E.Value).Value)
        else if E.Value is TGocciaStringLiteralValue then
          Result := TGocciaStringLiteralValue.Create(TGocciaStringLiteralValue(E.Value).Value)
        else if E.Value is TGocciaBooleanLiteralValue then
        begin
          if TGocciaBooleanLiteralValue(E.Value).Value then
            Result := TGocciaBooleanLiteralValue.TrueValue
          else
            Result := TGocciaBooleanLiteralValue.FalseValue;
        end
        else if E.Value is TGocciaUndefinedLiteralValue then
          Result := TGocciaUndefinedLiteralValue.UndefinedValue
        else if E.Value is TGocciaObjectValue then
          Result := E.Value
        else
          Result := E.Value;
      end;
    end;
    on E: TGocciaThrowValue do raise;
    on E: TGocciaBreakSignal do raise;
    on E: TGocciaTypeError do raise;
    on E: TGocciaReferenceError do raise;
    on E: TGocciaRuntimeError do raise;
    on E: TGocciaError do raise;
    on E: Exception do
    begin
      Logger.Error('FunctionValue.Call: Caught unexpected exception: %s', [E.Message]);
      raise;
    end;
  end;
end;

function TGocciaFunctionValue.Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
begin
  // Create call scope
  CallScope := FClosure.CreateChild(skFunction, FName, Length(FParameters) + 2);

  // Register with GC as an active scope (protects it from collection during execution)
  if Assigned(TGocciaGC.Instance) then
    TGocciaGC.Instance.PushActiveScope(CallScope);
  try
    Result := ExecuteBody(CallScope, Arguments, ThisValue);
  finally
    // Pop active scope from GC stack - the scope may still be alive
    // if captured by closures; the GC will determine reachability
    if Assigned(TGocciaGC.Instance) then
      TGocciaGC.Instance.PopActiveScope;
  end;
end;

function TGocciaFunctionValue.CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue;
var
  ClonedStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  // Create a copy of the statements - we can't share the same reference
  // since both functions would try to free the same object
  ClonedStatements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
  for I := 0 to FBodyStatements.Count - 1 do
    ClonedStatements.Add(FBodyStatements[I]);

  Result := TGocciaFunctionValue.Create(FParameters, ClonedStatements, NewScope, FName);
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

{ TGocciaArrowFunctionValue }

procedure TGocciaArrowFunctionValue.BindThis(CallScope: TGocciaScope; ThisValue: TGocciaValue);
var
  ClosureScope: TGocciaScope;
begin
  // Arrow functions always inherit 'this' from their lexical (closure) scope,
  // per ECMAScript spec. They never use the call-site ThisValue.
  ClosureScope := FClosure;
  while Assigned(ClosureScope) do
  begin
    if Assigned(ClosureScope.ThisValue) and not (ClosureScope.ThisValue is TGocciaUndefinedLiteralValue) then
    begin
      CallScope.ThisValue := ClosureScope.ThisValue;
      CallScope.DefineLexicalBinding('this', ClosureScope.ThisValue, dtUnknown);
      Break;
    end;
    ClosureScope := ClosureScope.Parent;
  end;
end;

function TGocciaArrowFunctionValue.CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue;
var
  ClonedStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  ClonedStatements := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to FBodyStatements.Count - 1 do
    ClonedStatements.Add(FBodyStatements[I]);

  Result := TGocciaArrowFunctionValue.Create(FParameters, ClonedStatements, NewScope, FName);
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string; ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABodyStatements, AClosure, AName);
  FSuperClass := ASuperClass;
end;

procedure TGocciaMethodValue.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype + closure

  // Mark method-specific references
  if Assigned(FSuperClass) then
    FSuperClass.GCMarkReferences;
  if Assigned(FOwningClass) then
    FOwningClass.GCMarkReferences;
end;

end.
