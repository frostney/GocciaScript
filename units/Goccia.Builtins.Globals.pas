unit Goccia.Builtins.Globals;

{$I Goccia.inc}

interface

uses
  SysUtils, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.FunctionValue, Goccia.Values.UndefinedValue, Generics.Collections, Math, Goccia.Builtins.Base, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, Goccia.Values.Base, Goccia.Scope, Goccia.Error;

type
  TGocciaGlobals = class(TGocciaBuiltin)
  protected
    function ParseFloat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ParseInt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function IsNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function IsFinite(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
  end;

implementation

constructor TGocciaGlobals.Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  // Global constants
  AScope.SetValue('NaN', TGocciaNumberValue.Create(Math.NaN));
  AScope.SetValue('Infinity', TGocciaNumberValue.Create(Math.Infinity));

  // Global functions
  AScope.SetValue('parseFloat', TGocciaNativeFunctionValue.Create(ParseFloat, 'parseFloat', 1));
  AScope.SetValue('parseInt', TGocciaNativeFunctionValue.Create(ParseInt, 'parseInt', 1));
  AScope.SetValue('isNaN', TGocciaNativeFunctionValue.Create(IsNaN, 'isNaN', 1));
  AScope.SetValue('isFinite', TGocciaNativeFunctionValue.Create(IsFinite, 'isFinite', 1));
end;

function TGocciaGlobals.ParseFloat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaNumberValue.Create(NaN);
    Exit;
  end;

  if Args[0] is TGocciaNumberValue then
  begin
    Result := Args[0];
    Exit;
  end;

  if (Args[0].ToString = 'NaN') or (Args[0].ToString = 'Infinity') then
  begin
    Result := TGocciaNumberValue.Create(NaN);
    Exit;
  end;

  if not TryStrToFloat(Args[0].ToString, Value) then
  begin
    Result := TGocciaNumberValue.Create(NaN);
  end;

  Result := TGocciaNumberValue.Create(Value);
end;

function TGocciaGlobals.ParseInt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Integer;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaNumberValue.Create(NaN);
    Exit;
  end;

  if Args[0] is TGocciaNumberValue then
  begin
    Result := Args[0];
    Exit;
  end;

  if Args[0].ToString = 'NaN' then
  begin
    Result := TGocciaNumberValue.Create(NaN);
    Exit;
  end;

  if Args[0].ToString = 'Infinity' then
  begin
    Result := TGocciaNumberValue.Create(Infinity);
    Exit;
  end;

  if not TryStrToInt(Args[0].ToString, Value) then
  begin
    Result := TGocciaNumberValue.Create(NaN);
  end;

  Result := TGocciaNumberValue.Create(Value);
end;

function TGocciaGlobals.IsNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanValue.Create(Math.IsNaN(StrToFloat(Args[0].ToString)));
end;

function TGocciaGlobals.IsFinite(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanValue.Create(not IsInfinite(StrToFloat(Args[0].ToString)));
end;

end.
