// TODO: Rename to Goccia.Values.UserFunction?
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.AST.Node, Goccia.Scope, Generics.Collections, Classes, SysUtils, Math,
  Goccia.Values.Undefined, Goccia.Interfaces;

type
  TGocciaFunctionValue = class(TGocciaValue)
  private
    FParameters: TStringList;
    FBody: TGocciaASTNode;
    FClosure: TGocciaScope;
    FName: string;
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
    function Call(Arguments: TObjectList<TGocciaVAlue>; ThisValue: TGocciaValue): TGocciaValue;

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
  WriteLn('TGocciaMethodValue.Call: Entered');
  WriteLn('  ThisValue: ', ThisValue.ToString);
  WriteLn('  Arguments.Count: ', Arguments.Count);
  WriteLn('  Closure address: ', NativeInt(FClosure));
  WriteLn('  Parameters: ', Parameters.Text);
  WriteLn('  Body: ', Body.ToString);
  WriteLn('  CallScope address: ', NativeInt(FClosure.CreateChild));
  Result := TGocciaFunctionInvocation.Create(Interpreter, FClosure, Parameters, Body).Call(Arguments, ThisValue);
  WriteLn('TGocciaMethodValue.Call: Exiting');
  WriteLn('  Result: ', Result.ToString);
end;

constructor TGocciaFunctionInvocation.Create(AInterpreter: IGocciaInterpreter; ACallScope: TGocciaScope; AParameters: TStringList; ABody: TGocciaASTNode);
begin
  FInterpreter := AInterpreter;
  FCallScope := ACallScope;
  FParameters := AParameters;
  FBody := ABody;
end;

function TGocciaFunctionInvocation.Call(Arguments: TObjectList<TGocciaVAlue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
begin
  WriteLn('TGocciaFunctionInvocation.Call: Entered');
  WriteLn('  ThisValue: ', ThisValue.ToString);
  WriteLn('  Arguments.Count: ', Arguments.Count);
  WriteLn('  Parameters: ', FParameters.Text);
  WriteLn('  Body: ', FBody.ToString);
  WriteLn('  CallScope address: ', NativeInt(FCallScope));

  Result := TGocciaUndefinedValue.Create;

  FCallScope.ThisValue := ThisValue;

  try
    for I := 0 to Arguments.Count - 1 do
    begin
      if I < Arguments.Count then
        FCallScope.SetValue(FParameters[I], Arguments[I])
      else
        FCallScope.SetValue(FParameters[I], TGocciaUndefinedValue.Create);
    end;

    Result := FInterpreter.Evaluate(FBody);
  finally
    FCallScope.Free;
  end;

  WriteLn('TGocciaFunctionInvocation.Call: Exiting');
  WriteLn('  Result: ', Result.ToString);
end;

end.
