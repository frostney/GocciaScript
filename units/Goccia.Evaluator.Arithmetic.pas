unit Goccia.Evaluator.Arithmetic;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Token, Math, SysUtils;

function EvaluateAddition(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateSubtraction(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateModulo(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue;
function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;

implementation

uses Goccia.Values.ClassHelper, Goccia.Values.ToPrimitive, Goccia.Evaluator.Bitwise;

type
  TGocciaNumericBinaryOp = function(A, B: Double): Double;

function DoSubtract(A, B: Double): Double;
begin
  Result := A - B;
end;

function DoMultiply(A, B: Double): Double;
begin
  Result := A * B;
end;

function DoPower(A, B: Double): Double;
begin
  Result := Power(A, B);
end;

function EvaluateSimpleNumericBinaryOp(Left, Right: TGocciaValue; AOp: TGocciaNumericBinaryOp): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Left.ToNumberLiteral;
  RightNum := Right.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(AOp(LeftNum.Value, RightNum.Value));
end;

function EvaluateAddition(Left, Right: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  // Step 1: Convert both operands to primitives (ECMAScript ToPrimitive)
  PrimLeft := ToPrimitive(Left);
  PrimRight := ToPrimitive(Right);

  // Step 2: If either primitive is a string, concatenate
  if (PrimLeft is TGocciaStringLiteralValue) or (PrimRight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue.Create(PrimLeft.ToStringLiteral.Value + PrimRight.ToStringLiteral.Value);
    Exit;
  end;

  // Step 3: Otherwise, convert both to numbers and add
  LeftNum := PrimLeft.ToNumberLiteral;
  RightNum := PrimRight.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value + RightNum.Value);
end;

function EvaluateSubtraction(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateSimpleNumericBinaryOp(Left, Right, @DoSubtract);
end;

function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateSimpleNumericBinaryOp(Left, Right, @DoMultiply);
end;

function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Left.ToNumberLiteral;
  RightNum := Right.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if RightNum.Value = 0 then
  begin
    if LeftNum.Value = 0 then
      Result := TGocciaNumberLiteralValue.NaNValue    // 0 / 0 = NaN
    else if LeftNum.Value > 0 then
      Result := TGocciaNumberLiteralValue.InfinityValue   // positive / 0 = +Infinity
    else
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue // negative / 0 = -Infinity
  end
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value / RightNum.Value);
end;

function EvaluateModulo(Left, Right: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Left.ToNumberLiteral;
  RightNum := Right.ToNumberLiteral;

  // NaN propagation
  if LeftNum.IsNaN or RightNum.IsNaN then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Division by zero produces NaN
  if RightNum.Value = 0 then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Infinity % anything = NaN
  if LeftNum.IsInfinity or LeftNum.IsNegativeInfinity then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // anything % Infinity = anything
  if RightNum.IsInfinity or RightNum.IsNegativeInfinity then
  begin
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value);
    Exit;
  end;

  // ECMAScript uses floating-point modulo (not integer)
  Result := TGocciaNumberLiteralValue.Create(
    LeftNum.Value - RightNum.Value * Trunc(LeftNum.Value / RightNum.Value));
end;

function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateSimpleNumericBinaryOp(Left, Right, @DoPower);
end;

function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;
begin
  case Operator of
    gttPlusAssign:
      Result := EvaluateAddition(CurrentValue, NewValue);
    gttMinusAssign:
      Result := EvaluateSubtraction(CurrentValue, NewValue);
    gttStarAssign:
      Result := EvaluateMultiplication(CurrentValue, NewValue);
    gttSlashAssign:
      Result := EvaluateDivision(CurrentValue, NewValue);
    gttPercentAssign:
      Result := EvaluateModulo(CurrentValue, NewValue);
    gttPowerAssign:
      Result := EvaluateExponentiation(CurrentValue, NewValue);
    gttBitwiseAndAssign:
      Result := EvaluateBitwiseAnd(CurrentValue, NewValue);
    gttBitwiseOrAssign:
      Result := EvaluateBitwiseOr(CurrentValue, NewValue);
    gttBitwiseXorAssign:
      Result := EvaluateBitwiseXor(CurrentValue, NewValue);
    gttLeftShiftAssign:
      Result := EvaluateLeftShift(CurrentValue, NewValue);
    gttRightShiftAssign:
      Result := EvaluateRightShift(CurrentValue, NewValue);
    gttUnsignedRightShiftAssign:
      Result := EvaluateUnsignedRightShift(CurrentValue, NewValue);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

end.
