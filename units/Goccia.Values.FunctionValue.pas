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
  TGocciaBlockValue = class
  private
    FStatements: TObjectList<TGocciaASTNode>;
    FScope: TGocciaScope;
  public
    constructor Create(AStatements: TObjectList<TGocciaASTNode>; AScope: TGocciaScope);
    destructor Destroy; override;
    function Execute(ACallScope: TGocciaScope): TGocciaValue;
    property Statements: TObjectList<TGocciaASTNode> read FStatements;
    property Scope: TGocciaScope read FScope;
  end;

  TGocciaFunctionValue = class(TGocciaObjectValue, IGocciaCallable)
  protected
    FName: string;
    FParameters: TGocciaParameterArray;
    FBody: TGocciaBlockValue;
    FClosure: TGocciaScope;
  public
    constructor Create(AParameters: TGocciaParameterArray; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaBlockValue read FBody;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
  public
    constructor Create(AParameters: TGocciaParameterArray; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string; ASuperClass: TGocciaValue = nil);
    function ToString: string; override;
    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
  end;

implementation

uses
  Goccia.Evaluator;

{ TGocciaBlockValue }

constructor TGocciaBlockValue.Create(AStatements: TObjectList<TGocciaASTNode>; AScope: TGocciaScope);
begin
  FStatements := AStatements;
  FScope := AScope;
end;

destructor TGocciaBlockValue.Destroy;
begin
  FStatements.Free;
  inherited;
end;

function TGocciaBlockValue.Execute(ACallScope: TGocciaScope): TGocciaValue;
var
  I: Integer;
  Context: TGocciaEvaluationContext;
  LastValue: TGocciaValue;
begin
  Context.Scope := ACallScope;
  LastValue := TGocciaUndefinedValue.Create;

  for I := 0 to FStatements.Count - 1 do
  begin
    try
      LastValue := Evaluate(FStatements[I], Context);
    except
      on E: TGocciaReturnValue do
      begin
        raise;
      end;
      on E: Exception do
      begin
        raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
      end;
    end;
  end;

  if LastValue = nil then
    LastValue := TGocciaUndefinedValue.Create;
  Result := LastValue;
end;

{ TGocciaFunctionValue }

constructor TGocciaFunctionValue.Create(AParameters: TGocciaParameterArray; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string = '');
begin
  FParameters := AParameters;
  FBody := ABody;
  FClosure := AClosure;
  FName := AName;

  inherited Create;
end;

destructor TGocciaFunctionValue.Destroy;
begin
  FBody.Free;
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
    Context.OnError := nil; // TODO: Pass proper error handler
    Context.LoadModule := nil; // TODO: Pass proper module loader

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
        CallScope.SetValue('__super__', Method.SuperClass);
      end;
    end;

    // Bind parameters
    for I := 0 to Length(FParameters) - 1 do
    begin
      Logger.Debug('Binding parameter %d: %s', [I, FParameters[I].Name]);
      if I < Arguments.Count then
      begin
        Logger.Debug('  Argument value type: %s, toString: %s', [Arguments[I].ClassName, Arguments[I].ToString]);
        CallScope.SetValue(FParameters[I].Name, Arguments[I])
      end
      else
      begin
        // Check if there's a default value
        if Assigned(FParameters[I].DefaultValue) then
        begin
          Logger.Debug('  No argument provided, using default value');
          // Evaluate the default value in the function's closure scope
          ReturnValue := EvaluateExpression(FParameters[I].DefaultValue, Context);
          CallScope.SetValue(FParameters[I].Name, ReturnValue);
        end
        else
        begin
          Logger.Debug('  No argument provided, setting to undefined');
          CallScope.SetValue(FParameters[I].Name, TGocciaUndefinedValue.Create);
        end;
      end;
    end;

    // Execute function body
    try
      ReturnValue := FBody.Execute(CallScope);
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
    CallScope.Free;
  end;
end;

function TGocciaFunctionValue.CloneWithNewScope(NewScope: TGocciaScope): TGocciaFunctionValue;
begin
  Result := TGocciaFunctionValue.Create(FParameters, FBody, NewScope, FName);
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(AParameters: TGocciaParameterArray; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string; ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABody, AClosure, AName);
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
