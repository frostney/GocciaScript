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
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper;

procedure CheckBigIntMixed(const ALeft, ARight: TGocciaValue); inline;
begin
  if (ALeft is TGocciaBigIntValue) xor (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntMixedTypes, SSuggestBigIntNoMixedArithmetic);
end;

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.BitwiseAnd(TGocciaBigIntValue(ARight).Value));
    Exit;
  end;
  CheckBigIntMixed(ALeft, ARight);
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) and Trunc(ARight.ToNumberLiteral.Value));
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
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) or Trunc(ARight.ToNumberLiteral.Value));
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
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) xor Trunc(ARight.ToNumberLiteral.Value));
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
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) shl (Trunc(ARight.ToNumberLiteral.Value) and 31));
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
    SarLongint(Int32(Trunc(ALeft.ToNumberLiteral.Value)),
               Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::unsignedRightShift — always throws
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  if (ALeft is TGocciaBigIntValue) or (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntUnsignedRightShift, SSuggestBigIntNoMixedArithmetic);
  Result := TGocciaNumberLiteralValue.Create(Cardinal(Trunc(ALeft.ToNumberLiteral.Value)) shr (Trunc(ARight.ToNumberLiteral.Value) and 31));
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
  Result := TGocciaNumberLiteralValue.Create(not Trunc(AOperand.ToNumberLiteral.Value));
end;

end.
