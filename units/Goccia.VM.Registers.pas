unit Goccia.VM.Registers;

{$I Goccia.inc}
{$POINTERMATH ON}

interface

uses
  Math,

  Goccia.Values.HoleValue,
  Goccia.Values.Primitives;

type
  TGocciaRegisterKind = (
    grkUndefined,
    grkNull,
    grkHole,
    grkBoolean,
    grkInt,
    grkFloat,
    grkObject
  );

  TGocciaRegister = record
    Kind: TGocciaRegisterKind;
    case Integer of
      0: (BoolValue: Boolean);
      1: (IntValue: Int64);
      2: (FloatValue: Double);
      3: (ObjectValue: TGocciaValue);
  end;

  PGocciaRegister = ^TGocciaRegister;
  TGocciaRegisterArray = array of TGocciaRegister;

function RegisterUndefined: TGocciaRegister; inline;
function RegisterNull: TGocciaRegister; inline;
function RegisterHole: TGocciaRegister; inline;
function RegisterBoolean(const AValue: Boolean): TGocciaRegister; inline;
function RegisterInt(const AValue: Int64): TGocciaRegister; inline;
function RegisterFloat(const AValue: Double): TGocciaRegister; inline;
function RegisterObject(const AValue: TGocciaValue): TGocciaRegister; inline;
function ValueToRegister(const AValue: TGocciaValue): TGocciaRegister; inline;
function RegisterToValue(const ARegister: TGocciaRegister): TGocciaValue; inline;
function RegisterToDouble(const ARegister: TGocciaRegister): Double; inline;
function RegisterToBoolean(const ARegister: TGocciaRegister): Boolean; inline;
function RegisterIsNumericScalar(const ARegister: TGocciaRegister): Boolean; inline;
procedure MarkRegisterReferences(const ARegister: TGocciaRegister); inline;

implementation

function RegisterUndefined: TGocciaRegister; inline;
begin
  Result.Kind := grkUndefined;
end;

function RegisterNull: TGocciaRegister; inline;
begin
  Result.Kind := grkNull;
end;

function RegisterHole: TGocciaRegister; inline;
begin
  Result.Kind := grkHole;
end;

function RegisterBoolean(const AValue: Boolean): TGocciaRegister; inline;
begin
  Result.Kind := grkBoolean;
  Result.BoolValue := AValue;
end;

function RegisterInt(const AValue: Int64): TGocciaRegister; inline;
begin
  Result.Kind := grkInt;
  Result.IntValue := AValue;
end;

function RegisterFloat(const AValue: Double): TGocciaRegister; inline;
begin
  Result.Kind := grkFloat;
  Result.FloatValue := AValue;
end;

function RegisterObject(const AValue: TGocciaValue): TGocciaRegister; inline;
begin
  Result.Kind := grkObject;
  Result.ObjectValue := AValue;
end;

function ValueToRegister(const AValue: TGocciaValue): TGocciaRegister; inline;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(RegisterUndefined);
  if AValue is TGocciaNullLiteralValue then
    Exit(RegisterNull);
  if AValue = TGocciaHoleValue.HoleValue then
    Exit(RegisterHole);
  if AValue is TGocciaBooleanLiteralValue then
    Exit(RegisterBoolean(TGocciaBooleanLiteralValue(AValue).Value));
  if AValue is TGocciaNumberLiteralValue then
  begin
    if (not TGocciaNumberLiteralValue(AValue).IsNaN) and
       (not TGocciaNumberLiteralValue(AValue).IsInfinite) and
       (not TGocciaNumberLiteralValue(AValue).IsNegativeZero) and
       (Frac(TGocciaNumberLiteralValue(AValue).Value) = 0.0) and
       (TGocciaNumberLiteralValue(AValue).Value >= Low(LongInt)) and
       (TGocciaNumberLiteralValue(AValue).Value <= High(LongInt)) then
      Exit(RegisterInt(Trunc(TGocciaNumberLiteralValue(AValue).Value)));
    Exit(RegisterFloat(TGocciaNumberLiteralValue(AValue).Value));
  end;
  Result := RegisterObject(AValue);
end;

function RegisterToValue(const ARegister: TGocciaRegister): TGocciaValue; inline;
begin
  case ARegister.Kind of
    grkUndefined:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    grkNull:
      Result := TGocciaNullLiteralValue.NullValue;
    grkHole:
      Result := TGocciaHoleValue.HoleValue;
    grkBoolean:
      if ARegister.BoolValue then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    grkInt:
      if ARegister.IntValue = 0 then
        Result := TGocciaNumberLiteralValue.ZeroValue
      else if ARegister.IntValue = 1 then
        Result := TGocciaNumberLiteralValue.OneValue
      else
        Result := TGocciaNumberLiteralValue.Create(ARegister.IntValue);
    grkFloat:
      Result := TGocciaNumberLiteralValue.Create(ARegister.FloatValue);
    grkObject:
      if Assigned(ARegister.ObjectValue) then
        Result := ARegister.ObjectValue
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function RegisterToDouble(const ARegister: TGocciaRegister): Double; inline;
begin
  case ARegister.Kind of
    grkInt:
      Result := ARegister.IntValue;
    grkFloat:
      Result := ARegister.FloatValue;
  else
    Result := RegisterToValue(ARegister).ToNumberLiteral.Value;
  end;
end;

function RegisterToBoolean(const ARegister: TGocciaRegister): Boolean; inline;
begin
  case ARegister.Kind of
    grkUndefined, grkNull, grkHole:
      Result := False;
    grkBoolean:
      Result := ARegister.BoolValue;
    grkInt:
      Result := ARegister.IntValue <> 0;
    grkFloat:
      Result := (ARegister.FloatValue <> 0.0) and (not IsNaN(ARegister.FloatValue));
    grkObject:
      Result := RegisterToValue(ARegister).ToBooleanLiteral.Value;
  else
    Result := False;
  end;
end;

function RegisterIsNumericScalar(const ARegister: TGocciaRegister): Boolean; inline;
begin
  Result := ARegister.Kind in [grkInt, grkFloat];
end;

procedure MarkRegisterReferences(const ARegister: TGocciaRegister); inline;
begin
  if (ARegister.Kind = grkObject) and Assigned(ARegister.ObjectValue) then
    ARegister.ObjectValue.MarkReferences;
end;

end.
