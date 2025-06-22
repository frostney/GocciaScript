// TODO: Rename to Goccia.Values.UserFunction?
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.Values.Base, Goccia.AST.Node, Goccia.AST.Statements, Goccia.AST.Expressions, Goccia.Scope,
  Goccia.Error, Goccia.Logger, Goccia.Values.Error, Goccia.Values.ObjectValue,
  Generics.Collections, Classes, Math, SysUtils,
  Goccia.Values.NumberValue,
  Goccia.Values.StringValue,
  Goccia.Values.BooleanValue,
  Goccia.Values.UndefinedValue;

type
  TGocciaFunctionValue = class(TGocciaObjectValue, IGocciaCallable)
  protected
    FName: string;
    FParameters: TGocciaParameterArray;
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FClosure: TGocciaScope;
  public
    constructor Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue;
    property Parameters: TGocciaParameterArray read FParameters;
    property BodyStatements: TObjectList<TGocciaASTNode> read FBodyStatements;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
  public
    constructor Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string; ASuperClass: TGocciaValue = nil);
    function ToString: string; override;
    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
  end;

implementation

uses
  Goccia.Evaluator;

{ TGocciaFunctionValue }

constructor TGocciaFunctionValue.Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string = '');
begin
  FParameters := AParameters;
  FBodyStatements := ABodyStatements;
  FClosure := AClosure;
  FName := AName;

  inherited Create;
end;

destructor TGocciaFunctionValue.Destroy;
begin
  FBodyStatements.Free;
  inherited;
end;

function TGocciaFunctionValue.ToString: string;
begin
  if FName <> '' then
    Result := Format('[Function: %s]', [FName])
  else
    Result := '[Function]';
end;

function TGocciaFunctionValue.ToBoolean: Boolean;
begin
  Result := True;
end;

function TGocciaFunctionValue.ToNumber: Double;
begin
  Result := NaN;
end;

function TGocciaFunctionValue.TypeName: string;
begin
  Result := 'function';
end;

function TGocciaFunctionValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  ReturnValue: TGocciaValue;
  Method: TGocciaMethodValue;
  Context: TGocciaEvaluationContext;
  CallScope: TGocciaScope;
begin
  Logger.Debug('FunctionValue.Call: Entering');
  Logger.Debug('  Function type: %s', [Self.ClassName]);
  Logger.Debug('  Arguments.Count: %d', [Arguments.Count]);
  Logger.Debug('  ThisValue type: %s', [ThisValue.ClassName]);

  // Create call scope
  CallScope := TGocciaScope.Create(FClosure, skFunction, Format('Type: FunctionCall, Name: %s', [FName]));
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
    CallScope.ThisValue := ThisValue;

    // If this is a method with a superclass, set up super handling
    if Self is TGocciaMethodValue then
    begin
      Method := TGocciaMethodValue(Self);
      if Assigned(Method.SuperClass) and not (Method.SuperClass is TGocciaUndefinedValue) then
      begin
        Logger.Debug('FunctionValue.Call: Method has superclass: %s', [Method.SuperClass.ToString]);
        // Set up special 'super' binding in the method scope
        CallScope.DefineBuiltin('__super__', Method.SuperClass);
      end;
    end;

    // Bind parameters
    for I := 0 to Length(FParameters) - 1 do
    begin
      if FParameters[I].IsPattern then
      begin
        // Handle destructuring parameter
        Logger.Debug('Binding destructuring parameter %d', [I]);

        // Get the argument value or default
        if I < Arguments.Count then
          ReturnValue := Arguments[I]
        else if Assigned(FParameters[I].DefaultValue) then
        begin
          Logger.Debug('  No argument provided, using default value for destructuring');
          ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context);
        end
        else
        begin
          Logger.Debug('  No argument provided, using undefined for destructuring');
          ReturnValue := TGocciaUndefinedValue.Create;
        end;

        // Bind the destructuring pattern using existing pattern assignment logic
        Context.Scope := CallScope;
        AssignPattern(FParameters[I].Pattern, ReturnValue, Context);
      end
      else
      begin
        // Handle simple named parameter
        Logger.Debug('Binding parameter %d: %s', [I, FParameters[I].Name]);
        if I < Arguments.Count then
        begin
          Logger.Debug('  Argument value type: %s, toString: %s', [Arguments[I].ClassName, Arguments[I].ToString]);
          CallScope.DefineBuiltin(FParameters[I].Name, Arguments[I])
        end
        else
        begin
          // Check if there's a default value
          if Assigned(FParameters[I].DefaultValue) then
          begin
            Logger.Debug('  No argument provided, using default value');
            // Evaluate the default value in the function's closure scope
            ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context);
            CallScope.DefineBuiltin(FParameters[I].Name, ReturnValue);
          end
          else
          begin
            Logger.Debug('  No argument provided, setting to undefined');
            CallScope.DefineBuiltin(FParameters[I].Name, TGocciaUndefinedValue.Create);
          end;
        end;
      end;
    end;

    // Execute function body statements directly
    try
      Context.Scope := CallScope;
      ReturnValue := TGocciaUndefinedValue.Create;

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

      if ReturnValue = nil then
        ReturnValue := TGocciaUndefinedValue.Create;
      Result := ReturnValue;
    except
      on E: TGocciaReturnValue do
      begin
        Logger.Debug('FunctionValue.Call: Caught TGocciaReturnValue');
        if E.Value = nil then
        begin
          Logger.Debug('FunctionValue.Call: E.Value is nil, creating undefined value');
          Result := TGocciaUndefinedValue.Create;
        end
        else
        begin
          Logger.Debug('FunctionValue.Call: E.Value type: %s', [E.Value.ClassName]);
          // Create a new instance of the same type
          if E.Value is TGocciaNumberValue then
            Result := TGocciaNumberValue.Create(TGocciaNumberValue(E.Value).Value)
          else if E.Value is TGocciaStringValue then
            Result := TGocciaStringValue.Create(TGocciaStringValue(E.Value).Value)
          else if E.Value is TGocciaBooleanValue then
            Result := TGocciaBooleanValue.Create(TGocciaBooleanValue(E.Value).Value)
          else if E.Value is TGocciaUndefinedValue then
            Result := TGocciaUndefinedValue.Create
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

    Logger.Debug('FunctionValue.Call: Exiting');
    Logger.Debug('  Result type: %s', [Result.ClassName]);
    Logger.Debug('  Result toString: %s', [Result.ToString]);
  finally
    // Don't free CallScope if it might be referenced by closures
    // TODO: Implement proper reference counting for scopes
    // CallScope.Free;
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

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(AParameters: TGocciaParameterArray; ABodyStatements: TObjectList<TGocciaASTNode>; AClosure: TGocciaScope; const AName: string; ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABodyStatements, AClosure, AName);
  FSuperClass := ASuperClass;
end;

function TGocciaMethodValue.ToString: string;
begin
  if FName <> '' then
    Result := Format('[Method: %s]', [FName])
  else
    Result := '[Method]';
end;

end.
