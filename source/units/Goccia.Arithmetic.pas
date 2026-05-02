unit Goccia.Arithmetic;

{$I Goccia.inc}

interface

uses
  Goccia.Token,
  Goccia.Values.Primitives;

function EvaluateAddition(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateSubtraction(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateMultiplication(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateDivision(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateModulo(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateExponentiation(const ALeft, ARight: TGocciaValue): TGocciaValue;

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue;
function CompoundOperations(const ACurrentValue, ANewValue: TGocciaValue;
  const AOperator: TGocciaTokenType): TGocciaValue;

function IsStrictEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
function IsNotStrictEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
function IsSameValue(const ALeft, ARight: TGocciaValue): Boolean; inline;
function IsSameValueZero(const ALeft, ARight: TGocciaValue): Boolean; inline;

function GreaterThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
function GreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
function LessThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
function LessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;

implementation

uses
  Math,

  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

const
  RELATION_LESS = -1;
  RELATION_EQUAL = 0;
  RELATION_GREATER = 1;
  RELATION_UNORDERED = 2;

type
  TGocciaNumberEqualityKind = (
    nekStrict,
    nekSameValue,
    nekSameValueZero
  );

function NumberValue(const AValue: Double): TGocciaNumberLiteralValue; inline;
var
  Bits: Int64 absolute AValue;
begin
  if Math.IsNaN(AValue) then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  if Math.IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
  end;
  if AValue = 0.0 then
  begin
    if Bits < 0 then
      Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
    Exit(TGocciaNumberLiteralValue.ZeroValue);
  end;
  if AValue = 1.0 then
    Exit(TGocciaNumberLiteralValue.OneValue);
  Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function InfinityWithSign(const APositive: Boolean): TGocciaNumberLiteralValue; inline;
begin
  if APositive then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
end;

// ES2026 §6.1.6.2 BigInt mixed-type arithmetic errors
procedure CheckBigIntMixedTypes(const ALeft, ARight: TGocciaValue); inline;
begin
  if (ALeft is TGocciaBigIntValue) xor (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntMixedTypes, SSuggestBigIntNoMixedArithmetic);
end;

procedure ToPrimitiveOperands(const ALeft, ARight: TGocciaValue;
  out APrimitiveLeft, APrimitiveRight: TGocciaValue); inline;
begin
  APrimitiveLeft := ToPrimitive(ALeft);
  APrimitiveRight := ToPrimitive(ARight);
end;

function ToNumericPair(const ALeft, ARight: TGocciaValue;
  out ALeftNum, ARightNum: TGocciaNumberLiteralValue): Boolean; inline;
begin
  if (ALeft is TGocciaSymbolValue) or (ARight is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
  CheckBigIntMixedTypes(ALeft, ARight);
  ALeftNum := ALeft.ToNumberLiteral;
  ARightNum := ARight.ToNumberLiteral;
  Result := not (ALeftNum.IsNaN or ARightNum.IsNaN);
end;

function IsActualZero(const ANum: TGocciaNumberLiteralValue): Boolean; inline;
begin
  Result := (ANum.Value = 0) and not ANum.IsNaN and not ANum.IsInfinite;
end;

// ES2026 §13.15.3 ApplyStringOrNumericBinaryOperator(lval, opText, rval)
function EvaluateAddition(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);

  if (PrimLeft is TGocciaSymbolValue) or (PrimRight is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);

  if (PrimLeft is TGocciaStringLiteralValue) or
     (PrimRight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue.Create(
      PrimLeft.ToStringLiteral.Value + PrimRight.ToStringLiteral.Value));

  // ES2026 §6.1.6.2.1 BigInt::add
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Add(
        TGocciaBigIntValue(PrimRight).Value)));

  CheckBigIntMixedTypes(PrimLeft, PrimRight);
  LeftNum := PrimLeft.ToNumberLiteral;
  RightNum := PrimRight.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite and RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinity = RightNum.IsInfinity then
        Exit(InfinityWithSign(LeftNum.IsInfinity));
      Exit(TGocciaNumberLiteralValue.NaNValue);
    end;
    if LeftNum.IsInfinite then
      Exit(InfinityWithSign(LeftNum.IsInfinity));
    Exit(InfinityWithSign(RightNum.IsInfinity));
  end;

  Result := NumberValue(LeftNum.Value + RightNum.Value);
end;

function EvaluateSubtraction(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);

  // ES2026 §6.1.6.2.2 BigInt::subtract
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Subtract(
        TGocciaBigIntValue(PrimRight).Value)));

  if not ToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite and RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinity <> RightNum.IsInfinity then
        Exit(InfinityWithSign(LeftNum.IsInfinity));
      Exit(TGocciaNumberLiteralValue.NaNValue);
    end;
    if LeftNum.IsInfinite then
      Exit(InfinityWithSign(LeftNum.IsInfinity));
    Exit(InfinityWithSign(not RightNum.IsInfinity));
  end;

  Result := NumberValue(LeftNum.Value - RightNum.Value);
end;

function EvaluateMultiplication(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftZero, RightZero: Boolean;
  SameSign: Boolean;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);

  // ES2026 §6.1.6.2.3 BigInt::multiply
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Multiply(
        TGocciaBigIntValue(PrimRight).Value)));

  if not ToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    LeftZero := (not LeftNum.IsInfinite) and (LeftNum.Value = 0);
    RightZero := (not RightNum.IsInfinite) and (RightNum.Value = 0);
    if LeftZero or RightZero then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    SameSign := LeftNum.IsInfinity = RightNum.IsInfinity;
    if not LeftNum.IsInfinite then
      SameSign := (LeftNum.Value > 0) = RightNum.IsInfinity
    else if not RightNum.IsInfinite then
      SameSign := LeftNum.IsInfinity = (RightNum.Value > 0);
    Exit(InfinityWithSign(SameSign));
  end;

  Result := NumberValue(LeftNum.Value * RightNum.Value);
end;

function EvaluateDivision(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  SameSign: Boolean;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);

  // ES2026 §6.1.6.2.6 BigInt::divide
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    if TGocciaBigIntValue(PrimRight).Value.IsZero then
      ThrowRangeError(SErrorBigIntDivisionByZero);
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Divide(
        TGocciaBigIntValue(PrimRight).Value)));
  end;

  if not ToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite then
  begin
    if RightNum.IsInfinite then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    SameSign := LeftNum.IsInfinity =
      ((RightNum.Value > 0) or
       ((RightNum.Value = 0) and not RightNum.IsNegativeZero));
    Exit(InfinityWithSign(SameSign));
  end;

  if RightNum.IsInfinite then
  begin
    SameSign := (LeftNum.Value > 0) or
      ((LeftNum.Value = 0) and not LeftNum.IsNegativeZero);
    if SameSign = RightNum.IsInfinity then
      Exit(TGocciaNumberLiteralValue.ZeroValue);
    Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
  end;

  if RightNum.Value = 0 then
  begin
    if LeftNum.Value = 0 then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    if LeftNum.Value > 0 then
    begin
      if RightNum.IsNegativeZero then
        Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    end;
    if RightNum.IsNegativeZero then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
  end;

  Result := NumberValue(LeftNum.Value / RightNum.Value);
end;

function EvaluateModulo(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);

  // ES2026 §6.1.6.2.7 BigInt::remainder
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    if TGocciaBigIntValue(PrimRight).Value.IsZero then
      ThrowRangeError(SErrorBigIntDivisionByZero);
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Modulo(
        TGocciaBigIntValue(PrimRight).Value)));
  end;

  if not ToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  if RightNum.IsInfinite then
    Exit(NumberValue(LeftNum.Value));
  if RightNum.Value = 0 then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  Result := NumberValue(
    LeftNum.Value - RightNum.Value * Trunc(LeftNum.Value / RightNum.Value));
end;

function EvaluateExponentiation(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);

  // ES2026 §6.1.6.2.8 BigInt::exponentiate
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    if TGocciaBigIntValue(PrimRight).Value.IsNegative then
      ThrowRangeError(SErrorBigIntNegativeExponent);
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Power(
        TGocciaBigIntValue(PrimRight).Value)));
  end;

  if not ToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
  begin
    if IsActualZero(RightNum) then
      Exit(TGocciaNumberLiteralValue.OneValue);
    Exit(TGocciaNumberLiteralValue.NaNValue);
  end;

  if IsActualZero(RightNum) then
    Exit(TGocciaNumberLiteralValue.OneValue);

  if RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite or (Abs(LeftNum.Value) > 1) then
    begin
      if RightNum.IsInfinity then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      Exit(TGocciaNumberLiteralValue.ZeroValue);
    end;
    if Abs(LeftNum.Value) = 1 then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    if RightNum.IsInfinity then
      Exit(TGocciaNumberLiteralValue.ZeroValue);
    Exit(TGocciaNumberLiteralValue.InfinityValue);
  end;

  if LeftNum.IsInfinite then
  begin
    if RightNum.Value > 0 then
    begin
      if LeftNum.IsInfinity then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      if Frac(RightNum.Value) <> 0 then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      if Frac(RightNum.Value / 2) = 0 then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
    end;
    if LeftNum.IsNegativeInfinity and (Frac(RightNum.Value) = 0) and
       (Frac(RightNum.Value / 2) <> 0) then
      Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
    Exit(TGocciaNumberLiteralValue.ZeroValue);
  end;

  Result := NumberValue(Power(LeftNum.Value, RightNum.Value));
end;

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseAnd(
        TGocciaBigIntValue(PrimRight).Value)));
  CheckBigIntMixedTypes(PrimLeft, PrimRight);
  Result := NumberValue(ToInt32Value(PrimLeft) and ToInt32Value(PrimRight));
end;

function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseOr(
        TGocciaBigIntValue(PrimRight).Value)));
  CheckBigIntMixedTypes(PrimLeft, PrimRight);
  Result := NumberValue(ToInt32Value(PrimLeft) or ToInt32Value(PrimRight));
end;

function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseXor(
        TGocciaBigIntValue(PrimRight).Value)));
  CheckBigIntMixedTypes(PrimLeft, PrimRight);
  Result := NumberValue(ToInt32Value(PrimLeft) xor ToInt32Value(PrimRight));
end;

// ES2026 §6.1.6.2.9 BigInt::leftShift
function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.ShiftLeft(
        TGocciaBigIntValue(PrimRight).Value.ToInt64)));
  CheckBigIntMixedTypes(PrimLeft, PrimRight);
  Result := NumberValue(
    ToInt32Value(PrimLeft) shl (ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.10 BigInt::signedRightShift
function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.ShiftRight(
        TGocciaBigIntValue(PrimRight).Value.ToInt64)));
  CheckBigIntMixedTypes(PrimLeft, PrimRight);
  Result := NumberValue(
    SarLongint(ToInt32Value(PrimLeft), ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::unsignedRightShift
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  ToPrimitiveOperands(ALeft, ARight, PrimLeft, PrimRight);
  if (PrimLeft is TGocciaBigIntValue) or (PrimRight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntUnsignedRightShift,
      SSuggestBigIntNoMixedArithmetic);
  Result := NumberValue(
    ToUint32Value(PrimLeft) shr (ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::bitwiseNOT
function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue;
var
  PrimOperand: TGocciaValue;
begin
  PrimOperand := ToPrimitive(AOperand);
  if PrimOperand is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimOperand).Value.BitwiseNot));
  Result := NumberValue(not ToInt32Value(PrimOperand));
end;

function CompoundOperations(const ACurrentValue, ANewValue: TGocciaValue;
  const AOperator: TGocciaTokenType): TGocciaValue;
begin
  case AOperator of
    gttPlusAssign:
      Result := EvaluateAddition(ACurrentValue, ANewValue);
    gttMinusAssign:
      Result := EvaluateSubtraction(ACurrentValue, ANewValue);
    gttStarAssign:
      Result := EvaluateMultiplication(ACurrentValue, ANewValue);
    gttSlashAssign:
      Result := EvaluateDivision(ACurrentValue, ANewValue);
    gttPercentAssign:
      Result := EvaluateModulo(ACurrentValue, ANewValue);
    gttPowerAssign:
      Result := EvaluateExponentiation(ACurrentValue, ANewValue);
    gttBitwiseAndAssign:
      Result := EvaluateBitwiseAnd(ACurrentValue, ANewValue);
    gttBitwiseOrAssign:
      Result := EvaluateBitwiseOr(ACurrentValue, ANewValue);
    gttBitwiseXorAssign:
      Result := EvaluateBitwiseXor(ACurrentValue, ANewValue);
    gttLeftShiftAssign:
      Result := EvaluateLeftShift(ACurrentValue, ANewValue);
    gttRightShiftAssign:
      Result := EvaluateRightShift(ACurrentValue, ANewValue);
    gttUnsignedRightShiftAssign:
      Result := EvaluateUnsignedRightShift(ACurrentValue, ANewValue);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function NumberValuesEqual(const ALeft, ARight: TGocciaNumberLiteralValue;
  const AKind: TGocciaNumberEqualityKind): Boolean; inline;
begin
  if ALeft.IsNaN or ARight.IsNaN then
  begin
    if AKind = nekStrict then
      Exit(False);
    Exit(ALeft.IsNaN and ARight.IsNaN);
  end;

  if ALeft.IsInfinite or ARight.IsInfinite then
  begin
    if ALeft.IsInfinite and ARight.IsInfinite then
      Exit(ALeft.IsInfinity = ARight.IsInfinity);
    Exit(False);
  end;

  if (ALeft.Value = 0) and (ARight.Value = 0) then
  begin
    if AKind = nekSameValue then
      Exit(ALeft.IsNegativeZero = ARight.IsNegativeZero);
    Exit(True);
  end;

  Result := ALeft.Value = ARight.Value;
end;

function ValuesEqual(const ALeft, ARight: TGocciaValue;
  const ANumberKind: TGocciaNumberEqualityKind): Boolean; inline;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) and
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(True);

  if (ALeft is TGocciaNullLiteralValue) and
     (ARight is TGocciaNullLiteralValue) then
    Exit(True);

  if (ALeft is TGocciaBooleanLiteralValue) and
     (ARight is TGocciaBooleanLiteralValue) then
    Exit(TGocciaBooleanLiteralValue(ALeft).Value =
      TGocciaBooleanLiteralValue(ARight).Value);

  if (ALeft is TGocciaNumberLiteralValue) and
     (ARight is TGocciaNumberLiteralValue) then
    Exit(NumberValuesEqual(TGocciaNumberLiteralValue(ALeft),
      TGocciaNumberLiteralValue(ARight), ANumberKind));

  if (ALeft is TGocciaStringLiteralValue) and
     (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value =
      TGocciaStringLiteralValue(ARight).Value);

  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue(ALeft).Value.Equal(
      TGocciaBigIntValue(ARight).Value));

  Result := ALeft = ARight;
end;

// ES2026 §7.2.15 IsStrictlyEqual(x, y)
function IsStrictEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  Result := ValuesEqual(ALeft, ARight, nekStrict);
end;

function IsNotStrictEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  Result := not IsStrictEqual(ALeft, ARight);
end;

// ES2026 §7.2.10 SameValue(x, y)
function IsSameValue(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  Result := ValuesEqual(ALeft, ARight, nekSameValue);
end;

// ES2026 §7.2.11 SameValueZero(x, y)
function IsSameValueZero(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  Result := ValuesEqual(ALeft, ARight, nekSameValueZero);
end;

function NormalizeRelation(const ACompare: Integer): Integer; inline;
begin
  if ACompare < 0 then
    Exit(RELATION_LESS);
  if ACompare > 0 then
    Exit(RELATION_GREATER);
  Result := RELATION_EQUAL;
end;

function CompareNumberValues(const ALeftNum, ARightNum: TGocciaNumberLiteralValue): Integer; inline;
begin
  if ALeftNum.IsNaN or ARightNum.IsNaN then
    Exit(RELATION_UNORDERED);

  if ALeftNum.IsInfinity then
  begin
    if ARightNum.IsInfinity then
      Exit(RELATION_EQUAL);
    Exit(RELATION_GREATER);
  end;
  if ALeftNum.IsNegativeInfinity then
  begin
    if ARightNum.IsNegativeInfinity then
      Exit(RELATION_EQUAL);
    Exit(RELATION_LESS);
  end;
  if ARightNum.IsInfinity then
    Exit(RELATION_LESS);
  if ARightNum.IsNegativeInfinity then
    Exit(RELATION_GREATER);

  if ALeftNum.Value < ARightNum.Value then
    Exit(RELATION_LESS);
  if ALeftNum.Value > ARightNum.Value then
    Exit(RELATION_GREATER);
  Result := RELATION_EQUAL;
end;

// ES2026 §7.2.14 IsLessThan(x, y, LeftFirst)
function CompareBigIntAndNumber(const ABigInt: TGocciaBigIntValue;
  const ANumber: TGocciaNumberLiteralValue): Integer; inline;
var
  NumVal, FloorVal: Double;
  NumAsBigInt: TBigInteger;
begin
  if ANumber.IsNaN then
    Exit(RELATION_UNORDERED);
  if ANumber.IsInfinity then
    Exit(RELATION_LESS);
  if ANumber.IsNegativeInfinity then
    Exit(RELATION_GREATER);

  NumVal := ANumber.Value;
  if Frac(NumVal) <> 0 then
  begin
    FloorVal := System.Int(NumVal);
    NumAsBigInt := TBigInteger.FromDouble(FloorVal);
    Result := NormalizeRelation(ABigInt.Value.Compare(NumAsBigInt));
    if NumVal > 0 then
    begin
      if Result <= 0 then
        Result := RELATION_LESS
      else
        Result := RELATION_GREATER;
    end
    else
    begin
      if Result >= 0 then
        Result := RELATION_GREATER
      else
        Result := RELATION_LESS;
    end;
    Exit;
  end;

  NumAsBigInt := TBigInteger.FromDouble(NumVal);
  Result := NormalizeRelation(ABigInt.Value.Compare(NumAsBigInt));
end;

function CompareStringValues(const ALeft, ARight: string): Integer; inline;
begin
  if ALeft < ARight then
    Exit(RELATION_LESS);
  if ALeft > ARight then
    Exit(RELATION_GREATER);
  Result := RELATION_EQUAL;
end;

function CompareRelationalValues(const ALeft, ARight: TGocciaValue): Integer; inline;
var
  Cmp: Integer;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(RELATION_UNORDERED);

  if (ALeft is TGocciaNullLiteralValue) and
     (ARight is TGocciaNullLiteralValue) then
    Exit(RELATION_EQUAL);

  if (ALeft is TGocciaStringLiteralValue) and
     (ARight is TGocciaStringLiteralValue) then
    Exit(CompareStringValues(TGocciaStringLiteralValue(ALeft).Value,
      TGocciaStringLiteralValue(ARight).Value));

  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
    Exit(NormalizeRelation(TGocciaBigIntValue(ALeft).Value.Compare(
      TGocciaBigIntValue(ARight).Value)));

  if (ALeft is TGocciaBigIntValue) and
     (ARight is TGocciaNumberLiteralValue) then
    Exit(CompareBigIntAndNumber(TGocciaBigIntValue(ALeft),
      TGocciaNumberLiteralValue(ARight)));

  if (ALeft is TGocciaNumberLiteralValue) and
     (ARight is TGocciaBigIntValue) then
  begin
    Cmp := CompareBigIntAndNumber(TGocciaBigIntValue(ARight),
      TGocciaNumberLiteralValue(ALeft));
    if Cmp = RELATION_UNORDERED then
      Exit(RELATION_UNORDERED);
    Exit(-Cmp);
  end;

  Result := CompareNumberValues(ALeft.ToNumberLiteral, ARight.ToNumberLiteral);
end;

function LessThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  Result := CompareRelationalValues(ALeft, ARight) = RELATION_LESS;
end;

function GreaterThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  Result := CompareRelationalValues(ALeft, ARight) = RELATION_GREATER;
end;

function LessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
var
  Cmp: Integer;
begin
  Cmp := CompareRelationalValues(ALeft, ARight);
  Result := (Cmp = RELATION_LESS) or (Cmp = RELATION_EQUAL);
end;

function GreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
var
  Cmp: Integer;
begin
  Cmp := CompareRelationalValues(ALeft, ARight);
  Result := (Cmp = RELATION_GREATER) or (Cmp = RELATION_EQUAL);
end;

end.
