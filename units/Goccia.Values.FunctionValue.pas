// TODO: Rename to Goccia.Values.UserFunction?
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.AST.Node, Goccia.AST.Statements, Goccia.Scope,
  Goccia.Error, Goccia.Logger, Goccia.Values.Undefined, Goccia.Values.Error,
  Generics.Collections, Classes, Math, SysUtils;

type
  TGocciaFunctionValue = class(TGocciaValue)
  private
    FName: string;
  protected
    FParameters: TStringList;
    FBody: TGocciaBlockStatement;
    FClosure: TGocciaScope;
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaBlockStatement; AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;
    function ToString: string; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue; virtual;
    property Parameters: TStringList read FParameters;
    property Body: TGocciaBlockStatement read FBody;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FMethodName: string;
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaBlockStatement; AClosure: TGocciaScope; const AMethodName: string);
    function ToString: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue; override;
    property MethodName: string read FMethodName;
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

{ TGocciaFunctionValue }

constructor TGocciaFunctionValue.Create(AParameters: TStringList; ABody: TGocciaBlockStatement; AClosure: TGocciaScope; const AName: string = '');
begin
  FParameters := AParameters;
  FBody := ABody;
  FClosure := AClosure;
  FName := AName;
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

constructor TGocciaMethodValue.Create(AParameters: TStringList; ABody: TGocciaBlockStatement; AClosure: TGocciaScope; const AMethodName: string);
begin
  inherited Create(AParameters, ABody, AClosure, AMethodName);
  FMethodName := AMethodName;
end;

function TGocciaMethodValue.ToString: string;
begin
  if Name <> '' then
    Result := Format('[Method: %s]', [Name])
  else
    Result := '[Method]';
end;

function TGocciaMethodValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
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

{ TGocciaFunctionInvocation }

constructor TGocciaFunctionInvocation.Create(AFunction: TGocciaFunctionValue; AArguments: TObjectList<TGocciaValue>; AThisValue: TGocciaValue);
begin
  FFunction := AFunction;
  FArguments := AArguments;
  FThisValue := AThisValue;
  FCallScope := FFunction.Closure.CreateChild;
end;

destructor TGocciaFunctionInvocation.Destroy;
begin
  FCallScope.Free;
  inherited;
end;

function TGocciaFunctionInvocation.Execute: TGocciaValue;
var
  I: Integer;
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
    if I < FArguments.Count then
      FCallScope.SetValue(FFunction.Parameters[I], FArguments[I])
    else
      FCallScope.SetValue(FFunction.Parameters[I], TGocciaUndefinedValue.Create);
  end;

  // Execute function body
  try
    Result := TGocciaUndefinedValue.Create;
    for I := 0 to FFunction.Body.Statements.Count - 1 do
      Result := FFunction.Body.Statements[I].Execute(FCallScope);
  except
    on E: TGocciaReturnValue do
      Result := E.Value;
  end;

  TGocciaLogger.Debug('FunctionInvocation.Execute: Exiting');
  TGocciaLogger.Debug('  Result type: %s', [Result.ClassName]);
  TGocciaLogger.Debug('  Result ToString: %s', [Result.ToString]);
end;

end.
