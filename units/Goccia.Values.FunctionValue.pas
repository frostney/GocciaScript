// TODO: Rename to Goccia.Values.UserFunction?
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
    FIsArrow: Boolean;
    FIsExpressionBody: Boolean;
    FIsSimpleParams: Boolean;
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;

    function Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; override;
    function CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue;
    procedure GCMarkReferences; override;

    property Parameters: TGocciaParameterArray read FParameters;
    property BodyStatements: TObjectList<TGocciaASTNode> read FBodyStatements;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName write FName;
    property IsArrow: Boolean read FIsArrow write FIsArrow;
    property IsExpressionBody: Boolean read FIsExpressionBody write FIsExpressionBody;
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

function TGocciaFunctionValue.Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I, J: Integer;
  ReturnValue: TGocciaValue;
  Method: TGocciaMethodValue;
  Context: TGocciaEvaluationContext;
  CallScope: TGocciaScope;
  ClosureScope: TGocciaScope;
begin
  // Create call scope
  CallScope := TGocciaScope.Create(FClosure, skFunction, FName, Length(FParameters) + 2);

  // Register with GC as an active scope (protects it from collection during execution)
  if Assigned(TGocciaGC.Instance) then
    TGocciaGC.Instance.PushActiveScope(CallScope);
  try
    // Set up evaluation context for default parameter evaluation
    Context.Scope := FClosure;
    // Use global context as fallback for OnError when not explicitly provided
    if Assigned(GlobalEvaluationContext.OnError) then
    begin
      Context.OnError := GlobalEvaluationContext.OnError;
      Context.LoadModule := GlobalEvaluationContext.LoadModule;
    end
    else
    begin
      Context.OnError := nil;
      Context.LoadModule := nil;
    end;

    // Set up the call scope
    // Arrow functions: if called as a method (ThisValue is provided), use the call-site this.
    // If called standalone (ThisValue is undefined), inherit 'this' from the closure scope.
    if FIsArrow and (not Assigned(ThisValue) or (ThisValue is TGocciaUndefinedLiteralValue)) then
    begin
      // Standalone call of an arrow function: inherit 'this' from enclosing lexical scope
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
    end
    else
    begin
      // Regular function call, or arrow function called as a method (obj.method())
      CallScope.ThisValue := ThisValue;
      CallScope.DefineLexicalBinding('this', ThisValue, dtUnknown);
    end;

    // If this is a method with a superclass, set up super handling
    if Self is TGocciaMethodValue then
    begin
      Method := TGocciaMethodValue(Self);
      if Assigned(Method.SuperClass) and not (Method.SuperClass is TGocciaUndefinedLiteralValue) then
      begin
        // Set up special 'super' binding in the method scope - TODO: This should be a specialised scope
        CallScope.DefineLexicalBinding('__super__', Method.SuperClass, dtUnknown);
      end;

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
            Result := TGocciaBooleanLiteralValue.Create(TGocciaBooleanLiteralValue(E.Value).Value)
          else if E.Value is TGocciaUndefinedLiteralValue then
            Result := TGocciaUndefinedLiteralValue.UndefinedValue
          else if E.Value is TGocciaObjectValue then
            Result := E.Value
          else
            Result := E.Value;
        end;
      end;
      on E: Exception do
      begin
        Logger.Error('FunctionValue.Call: Caught unexpected exception: %s', [E.Message]);
        raise;
      end;
    end;

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
