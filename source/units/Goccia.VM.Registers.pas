unit Goccia.VM.Registers;

{$I Goccia.inc}
{$POINTERMATH ON}

interface

uses
  Math,

  NumberBits,

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

function RegisterUndefined: TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterNull: TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterHole: TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterBoolean(const AValue: Boolean): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterInt(const AValue: Int64): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterFloat(const AValue: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterFromDouble(const AValue: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterObject(const AValue: TGocciaValue): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function ValueToRegister(const AValue: TGocciaValue): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
function RegisterToValue(const ARegister: TGocciaRegister): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
function RegisterToDouble(const ARegister: TGocciaRegister): Double; {$IFDEF FPC}inline;{$ENDIF}
function RegisterToBoolean(const ARegister: TGocciaRegister): Boolean; {$IFDEF FPC}inline;{$ENDIF}
function RegisterIsNumericScalar(const ARegister: TGocciaRegister): Boolean; {$IFDEF FPC}inline;{$ENDIF}
procedure MarkRegisterReferences(const ARegister: TGocciaRegister); {$IFDEF FPC}inline;{$ENDIF}

implementation

function RegisterUndefined: TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkUndefined;
end;

function RegisterNull: TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkNull;
end;

function RegisterHole: TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkHole;
end;

function RegisterBoolean(const AValue: Boolean): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkBoolean;
  Result.BoolValue := AValue;
end;

function RegisterInt(const AValue: Int64): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkInt;
  Result.IntValue := AValue;
end;

function RegisterFloat(const AValue: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkFloat;
  Result.FloatValue := AValue;
end;

function RegisterFromDouble(const AValue: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  // Build a register directly from a raw Double without ever allocating a heap
  // TGocciaNumberLiteralValue. Mirrors the number branch of VMValueToRegisterFast:
  // exact integers in LongInt range become grkInt (so downstream scalar opcodes and
  // the Zero/One singletons engage on later boxing), and -0.0 stays float to keep
  // its sign bit. NaN/Infinity/non-integers stay float.
  if AValue = 0.0 then
  begin
    if NumberBits.IsNegativeZero(AValue) then
      Exit(RegisterFloat(AValue)); // -0.0: preserve the sign bit as a float
    Exit(RegisterInt(0));
  end;
  if AValue = 1.0 then
    Exit(RegisterInt(1));
  if (not IsNaN(AValue)) and (not IsInfinite(AValue)) and
     (Frac(AValue) = 0.0) and
     (AValue >= Low(LongInt)) and (AValue <= High(LongInt)) then
    Exit(RegisterInt(Trunc(AValue)));
  Result := RegisterFloat(AValue);
end;

function RegisterObject(const AValue: TGocciaValue): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.Kind := grkObject;
  Result.ObjectValue := AValue;
end;

function ValueToRegister(const AValue: TGocciaValue): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
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

function RegisterToValue(const ARegister: TGocciaRegister): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
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

function RegisterToDouble(const ARegister: TGocciaRegister): Double; {$IFDEF FPC}inline;{$ENDIF}
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

function RegisterToBoolean(const ARegister: TGocciaRegister): Boolean; {$IFDEF FPC}inline;{$ENDIF}
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

function RegisterIsNumericScalar(const ARegister: TGocciaRegister): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := ARegister.Kind in [grkInt, grkFloat];
end;

procedure MarkRegisterReferences(const ARegister: TGocciaRegister); {$IFDEF FPC}inline;{$ENDIF}
begin
  if (ARegister.Kind = grkObject) and Assigned(ARegister.ObjectValue) then
    ARegister.ObjectValue.MarkReferences;
end;

end.
