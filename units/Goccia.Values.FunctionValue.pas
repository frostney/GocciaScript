// TODO: Rename to Goccia.Values.UserFunction?
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.AST.Node, Goccia.Scope, Generics.Collections, Classes, SysUtils, Math,
  Goccia.Values.Undefined, Goccia.Interfaces, Goccia.Logger;

type
  TGocciaFunctionValue = class(TGocciaValue)
  private
    FName: string;
  protected
    FParameters: TStringList;
    FBody: TGocciaASTNode;
    FClosure: TGocciaScope;
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaASTNode;
      AClosure: TGocciaScope; const AName: string = '');
    function ToString: string; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue; Interpreter: IGocciaInterpreter): TGocciaValue; virtual;
    property Parameters: TStringList read FParameters;
    property Body: TGocciaASTNode read FBody;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaASTNode;
      AClosure: TGocciaScope; const AName: string = '');
    function ToString: string; override;
    function TypeName: string; override;
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue; Interpreter: IGocciaInterpreter): TGocciaValue; override;
  end;

  TGocciaFunctionInvocation = class
  private
    FParameters: TStringList;
    FInterpreter: IGocciaInterpreter;
    FCallScope: TGocciaScope;
    FBody: TGocciaASTNode;
  public
    constructor Create(AInterpreter: IGocciaInterpreter; ACallScope: TGocciaScope; AParameters: TStringList; ABody: TGocciaASTNode);
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    property Parameters: TStringList read FParameters;
    property Body: TGocciaASTNode read FBody;
    property CallScope: TGocciaScope read FCallScope;
  end;

implementation

uses
  Goccia.Interpreter;

constructor TGocciaFunctionValue.Create(AParameters: TStringList;
  ABody: TGocciaASTNode; AClosure: TGocciaScope; const AName: string);
begin
  FParameters := AParameters;
  FBody := ABody;
  FClosure := AClosure;
  FName := AName;
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

function TGocciaFunctionValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue; Interpreter: IGocciaInterpreter): TGocciaValue;
begin
  Result := TGocciaFunctionInvocation.Create(Interpreter, FClosure, Parameters, Body).Call(Arguments, ThisValue);
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(AParameters: TStringList;
  ABody: TGocciaASTNode; AClosure: TGocciaScope; const AName: string);
begin
  inherited Create(AParameters, ABody, AClosure, AName);
end;

function TGocciaMethodValue.ToString: string;
begin
  if Name <> '' then
    Result := Format('[Method: %s]', [Name])
  else
    Result := '[Method]';
end;

function TGocciaMethodValue.TypeName: string;
begin
  Result := 'method';
end;

function TGocciaMethodValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue; Interpreter: IGocciaInterpreter): TGocciaValue;
begin
  TGocciaLogger.Debug('TGocciaMethodValue.Call: Entered');
  TGocciaLogger.Debug('  ThisValue: %s', [ThisValue.ToString]);
  TGocciaLogger.Debug('  Arguments.Count: %d', [Arguments.Count]);
  TGocciaLogger.Debug('  Closure address: %d', [NativeInt(FClosure)]);
  TGocciaLogger.Debug('  Parameters: %s', [Parameters.Text]);
  TGocciaLogger.Debug('  Body: %s', [Body.ToString]);
  TGocciaLogger.Debug('  CallScope address: %d', [NativeInt(FClosure.CreateChild)]);
  Result := TGocciaFunctionInvocation.Create(Interpreter, FClosure, Parameters, Body).Call(Arguments, ThisValue);
  TGocciaLogger.Debug('TGocciaMethodValue.Call: Exiting');
  TGocciaLogger.Debug('  Result: %s', [Result.ToString]);
end;

constructor TGocciaFunctionInvocation.Create(AInterpreter: IGocciaInterpreter; ACallScope: TGocciaScope; AParameters: TStringList; ABody: TGocciaASTNode);
begin
  FInterpreter := AInterpreter;
  FCallScope := ACallScope;
  FParameters := AParameters;
  FBody := ABody;
end;

function TGocciaFunctionInvocation.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  ReturnValue: TGocciaValue;
begin
  TGocciaLogger.Debug('TGocciaFunctionInvocation.Call: Entered');
  TGocciaLogger.Debug('  ThisValue: %s', [ThisValue.ToString]);
  TGocciaLogger.Debug('  Arguments.Count: %d', [Arguments.Count]);
  TGocciaLogger.Debug('  Parameters: %s', [FParameters.Text]);
  TGocciaLogger.Debug('  Body: %s', [FBody.ClassName]);
  TGocciaLogger.Debug('  CallScope address: %d', [NativeUInt(FCallScope)]);

  try
    // Set the this value in the call scope
    FCallScope.ThisValue := ThisValue;

    // Bind parameters to the call scope
    for I := 0 to FParameters.Count - 1 do
    begin
      if I < Arguments.Count then
        FCallScope.SetValue(FParameters[I], Arguments[I])
      else
        FCallScope.SetValue(FParameters[I], TGocciaUndefinedValue.Create);
    end;

    // Execute the function body
    try
      ReturnValue := FInterpreter.Evaluate(FBody);
      Result := ReturnValue;
    except
      on E: TGocciaReturnValue do
      begin
        Result := E.Value;
      end;
    end;
  finally
    CallScope.Free;
  end;

  TGocciaLogger.Debug('TGocciaFunctionInvocation.Call: Exiting');
  TGocciaLogger.Debug('  Result: %s', [Result.ToString]);
end;

end.
