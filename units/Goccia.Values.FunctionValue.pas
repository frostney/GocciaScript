// TODO: Rename to Goccia.Values.UserFunction?
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.Values.Base, Goccia.AST.Node, Goccia.AST.Statements, Goccia.Scope,
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
    FParameters: TStringList;
    FBody: TGocciaBlockValue;
    FClosure: TGocciaScope;
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    property Parameters: TStringList read FParameters;
    property Body: TGocciaBlockValue read FBody;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string);
    function ToString: string; override;
  end;

  TGocciaFunctionInvocation = class
  private
    FFunction: TGocciaFunctionValue;
    FArguments: TObjectList<TGocciaValue>;
    FThisValue: TGocciaValue;
    FCallScope: TGocciaScope;
  public
    constructor Create(AFunction: TGocciaFunctionValue; AArguments: TObjectList<TGocciaValue>; AThisValue: TGocciaValue);
    destructor Destroy; override;
    function Execute: TGocciaValue;
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

constructor TGocciaFunctionValue.Create(AParameters: TStringList; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string = '');
begin
  FParameters := AParameters;
  FBody := ABody;
  FClosure := AClosure;
  FName := AName;

  WriteLn('Parameters in FunctionValue: ' + IntToStr(FParameters.Count));

  inherited Create;
end;

destructor TGocciaFunctionValue.Destroy;
begin
  FParameters.Free;
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
  Invocation: TGocciaFunctionInvocation;
begin
  Invocation := TGocciaFunctionInvocation.Create(Self, Arguments, ThisValue);
  try
    Result := Invocation.Execute;
  finally
    Invocation.Free;
  end;
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(AParameters: TStringList; ABody: TGocciaBlockValue; AClosure: TGocciaScope; const AName: string);
begin
  inherited Create(AParameters, ABody, AClosure, AName);
end;

function TGocciaMethodValue.ToString: string;
begin
  if FName <> '' then
    Result := Format('[Method: %s]', [FName])
  else
    Result := '[Method]';
end;

{ TGocciaFunctionInvocation }

constructor TGocciaFunctionInvocation.Create(AFunction: TGocciaFunctionValue; AArguments: TObjectList<TGocciaValue>; AThisValue: TGocciaValue);
begin
  FFunction := AFunction;
  FArguments := AArguments;
  FThisValue := AThisValue;
  FCallScope := TGocciaScope.Create(FFunction.Closure, skFunction, Format('Type: FunctionInvocation, Name: %s', [AFunction.Name]));
end;

destructor TGocciaFunctionInvocation.Destroy;
begin
  FCallScope.Free;
  inherited;
end;

function TGocciaFunctionInvocation.Execute: TGocciaValue;
var
  I: Integer;
  ReturnValue: TGocciaValue;
begin
  TGocciaLogger.Debug('FunctionInvocation.Execute: Entering');
  TGocciaLogger.Debug('  Function type: %s', [FFunction.ClassName]);
  TGocciaLogger.Debug('  Arguments.Count: %d', [FArguments.Count]);
  TGocciaLogger.Debug('  ThisValue type: %s', [FThisValue.ClassName]);

  // Set up the call scope
  FCallScope.ThisValue := FThisValue;

  // Bind parameters
  for I := 0 to FFunction.Parameters.Count - 1 do
  begin
    TGocciaLogger.Debug('Binding parameter %d: %s', [I, FFunction.Parameters[I]]);
    if I < FArguments.Count then
    begin
      TGocciaLogger.Debug('  Argument value type: %s, ToString: %s', [FArguments[I].ClassName, FArguments[I].ToString]);
      FCallScope.SetValue(FFunction.Parameters[I], FArguments[I])
    end
    else
    begin
      TGocciaLogger.Debug('  No argument provided, setting to undefined');
      FCallScope.SetValue(FFunction.Parameters[I], TGocciaUndefinedValue.Create);
    end;
  end;

  // Execute function body
  try
    ReturnValue := FFunction.Body.Execute(FCallScope);
    if ReturnValue = nil then
      ReturnValue := TGocciaUndefinedValue.Create;
    Result := ReturnValue;
  except
    on E: TGocciaReturnValue do
    begin
      TGocciaLogger.Debug('FunctionInvocation.Execute: Caught TGocciaReturnValue');
      if E.Value = nil then
      begin
        TGocciaLogger.Debug('FunctionInvocation.Execute: E.Value is nil, creating undefined value');
        Result := TGocciaUndefinedValue.Create;
      end
      else
      begin
        TGocciaLogger.Debug('FunctionInvocation.Execute: E.Value type: %s', [E.Value.ClassName]);
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
      TGocciaLogger.Debug('FunctionInvocation.Execute: Caught unexpected exception: %s', [E.Message]);
      raise;
    end;
  end;

  TGocciaLogger.Debug('FunctionInvocation.Execute: Exiting');
  TGocciaLogger.Debug('  Result type: %s', [Result.ClassName]);
  TGocciaLogger.Debug('  Result ToString: %s', [Result.ToString]);
end;

end.
