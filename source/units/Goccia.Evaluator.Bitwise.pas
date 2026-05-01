unit Goccia.Evaluator.Bitwise;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue;

implementation

uses
  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ToPrimitive;

procedure CheckBigIntMixed(const ALeft, ARight: TGocciaValue); inline;
begin
  if (ALeft is TGocciaBigIntValue) xor (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntMixedTypes, SSuggestBigIntNoMixedArithmetic);
end;

// All Number-side bitwise operators must route operands through
// ToInt32Value / ToUint32Value (Goccia.Utils) rather than calling
// Trunc directly.  Trunc(NaN) on FPC's x86_64 backend returns
// Int64.MinValue (the SSE cvttsd2si "indefinite" sentinel) where on
// aarch64 it saturates to 0; using the spec-compliant helpers gives
// us platform-uniform semantics: NaN, ±Infinity, and ±0 all coerce
// to 0 before any cast.
//
// Operands also go through ToPrimitive first so boxed BigInts (e.g.
// `Object(1n)`) unbox to their primitive BigInt and take the BigInt
// branch instead of being silently coerced to Number 0/-1 via the
// boxed object's ToNumberLiteral path.  Per ES2026 §13.15.3
// EvaluateBinaryArithmeticExpression / §6.1.6.2 BigInt operations,
// the "is BigInt?" check applies to the post-ToPrimitive value.

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseAnd(TGocciaBigIntValue(PrimRight).Value));
    Exit;
  end;
  CheckBigIntMixed(PrimLeft, PrimRight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(PrimLeft) and ToInt32Value(PrimRight));
end;

function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseOr(TGocciaBigIntValue(PrimRight).Value));
    Exit;
  end;
  CheckBigIntMixed(PrimLeft, PrimRight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(PrimLeft) or ToInt32Value(PrimRight));
end;

function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseXor(TGocciaBigIntValue(PrimRight).Value));
    Exit;
  end;
  CheckBigIntMixed(PrimLeft, PrimRight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(PrimLeft) xor ToInt32Value(PrimRight));
end;

// ES2026 §6.1.6.2.9 BigInt::leftShift
function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.ShiftLeft(
        TGocciaBigIntValue(PrimRight).Value.ToInt64));
    Exit;
  end;
  CheckBigIntMixed(PrimLeft, PrimRight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(PrimLeft) shl (ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.10 BigInt::signedRightShift
function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.ShiftRight(
        TGocciaBigIntValue(PrimRight).Value.ToInt64));
    Exit;
  end;
  CheckBigIntMixed(PrimLeft, PrimRight);
  Result := TGocciaNumberLiteralValue.Create(
    SarLongint(ToInt32Value(PrimLeft), ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::unsignedRightShift — always throws
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) or (PrimRight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntUnsignedRightShift, SSuggestBigIntNoMixedArithmetic);
  Result := TGocciaNumberLiteralValue.Create(
    ToUint32Value(PrimLeft) shr (ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::bitwiseNOT — ~x = -(x + 1)
function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue;
var
  PrimOperand: TGocciaValue;
begin
  PrimOperand := ToPrimitive(AOperand);
  if PrimOperand is TGocciaBigIntValue then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimOperand).Value.BitwiseNot);
    Exit;
  end;
  Result := TGocciaNumberLiteralValue.Create(not ToInt32Value(PrimOperand));
end;

end.
