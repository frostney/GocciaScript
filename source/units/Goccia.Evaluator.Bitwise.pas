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
  Goccia.Values.ErrorHelper;

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

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.BitwiseAnd(TGocciaBigIntValue(ARight).Value));
    Exit;
  end;
  CheckBigIntMixed(ALeft, ARight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(ALeft) and ToInt32Value(ARight));
end;

function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.BitwiseOr(TGocciaBigIntValue(ARight).Value));
    Exit;
  end;
  CheckBigIntMixed(ALeft, ARight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(ALeft) or ToInt32Value(ARight));
end;

function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.BitwiseXor(TGocciaBigIntValue(ARight).Value));
    Exit;
  end;
  CheckBigIntMixed(ALeft, ARight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(ALeft) xor ToInt32Value(ARight));
end;

// ES2026 §6.1.6.2.9 BigInt::leftShift
function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.ShiftLeft(
        TGocciaBigIntValue(ARight).Value.ToInt64));
    Exit;
  end;
  CheckBigIntMixed(ALeft, ARight);
  Result := TGocciaNumberLiteralValue.Create(
    ToInt32Value(ALeft) shl (ToUint32Value(ARight) and 31));
end;

// ES2026 §6.1.6.2.10 BigInt::signedRightShift
function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.ShiftRight(
        TGocciaBigIntValue(ARight).Value.ToInt64));
    Exit;
  end;
  CheckBigIntMixed(ALeft, ARight);
  Result := TGocciaNumberLiteralValue.Create(
    SarLongint(ToInt32Value(ALeft), ToUint32Value(ARight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::unsignedRightShift — always throws
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) or (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntUnsignedRightShift, SSuggestBigIntNoMixedArithmetic);
  Result := TGocciaNumberLiteralValue.Create(
    ToUint32Value(ALeft) shr (ToUint32Value(ARight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::bitwiseNOT — ~x = -(x + 1)
function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue;
begin
  if AOperand is TGocciaBigIntValue then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(AOperand).Value.BitwiseNot);
    Exit;
  end;
  Result := TGocciaNumberLiteralValue.Create(not ToInt32Value(AOperand));
end;

end.
