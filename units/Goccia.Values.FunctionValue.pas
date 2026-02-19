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
    FIsExpressionBody: Boolean;
    FIsSimpleParams: Boolean;
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); virtual;
    function CreateCallScope: TGocciaScope; virtual;
    function ExecuteBody(const ACallScope: TGocciaScope; const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;

    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue; virtual;
    procedure MarkReferences; override;

    property Parameters: TGocciaParameterArray read FParameters;
    property BodyStatements: TObjectList<TGocciaASTNode> read FBodyStatements;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName write FName;
    property IsExpressionBody: Boolean read FIsExpressionBody write FIsExpressionBody;
  end;

  TGocciaArrowFunctionValue = class(TGocciaFunctionValue)
  protected
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); override;
  public
    function CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue; override;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue; // TGocciaClassValue stored as TGocciaValue to avoid circular dependency
  protected
    function CreateCallScope: TGocciaScope; override;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
    procedure MarkReferences; override;

    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass write FOwningClass;
  end;

implementation

uses
  SysUtils,

  Goccia.Error,
  Goccia.Evaluator,
  Goccia.GarbageCollector,
  Goccia.Interfaces,
  Goccia.Logger,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.Error;

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
  Context: TGocciaEvaluationContext;
begin
  // Set up evaluation context — inherit OnError from the closure scope
  Context.Scope := FClosure;
  Context.OnError := FClosure.OnError;
  Context.LoadModule := nil;

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

  // Execute function body statements directly
  try
    Context.Scope := ACallScope;
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
        Result := E.Value;
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

function TGocciaFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
begin
  // Create call scope via virtual dispatch (TGocciaMethodValue creates TGocciaMethodCallScope)
  CallScope := CreateCallScope;

  // Register with GC as an active scope (protects it from collection during execution)
  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.PushActiveScope(CallScope);
  try
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
  finally
    // Pop active scope from GC stack - the scope may still be alive
    // if captured by closures; the GC will determine reachability
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.PopActiveScope;
  end;
end;

function TGocciaFunctionValue.CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue;
var
  ClonedStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  // Create a copy of the statements - we can't share the same reference
  // since both functions would try to free the same object
  ClonedStatements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
  for I := 0 to FBodyStatements.Count - 1 do
    ClonedStatements.Add(FBodyStatements[I]);

  Result := TGocciaFunctionValue.Create(FParameters, ClonedStatements, ANewScope, FName);
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

function TGocciaArrowFunctionValue.CloneWithNewScope(const ANewScope: TGocciaScope): TGocciaFunctionValue;
var
  ClonedStatements: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  ClonedStatements := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to FBodyStatements.Count - 1 do
    ClonedStatements.Add(FBodyStatements[I]);

  Result := TGocciaArrowFunctionValue.Create(FParameters, ClonedStatements, ANewScope, FName);
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABodyStatements, AClosure, AName);
  FSuperClass := ASuperClass;
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
