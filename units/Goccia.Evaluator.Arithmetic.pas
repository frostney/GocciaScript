unit Goccia.Evaluator.Arithmetic;

{$I Goccia.inc}

interface

uses
  Math,
  SysUtils,

  Goccia.Token,
  Goccia.Values.Primitives;

function EvaluateAddition(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateSubtraction(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateMultiplication(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateDivision(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateModulo(const ALeft, ARight: TGocciaValue): TGocciaValue;
function EvaluateExponentiation(const ALeft, ARight: TGocciaValue): TGocciaValue;
function PerformCompoundOperation(const ACurrentValue, ANewValue: TGocciaValue; const AOperator: TGocciaTokenType): TGocciaValue;

implementation

uses
  Goccia.Evaluator.Bitwise,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

type
  TGocciaNumericBinaryOp = function(const A, B: Double): Double;

function DoSubtract(const A, B: Double): Double;
begin
  Result := A - B;
end;

function DoMultiply(const A, B: Double): Double;
begin
  Result := A * B;
end;

function DoPower(const A, B: Double): Double;
begin
  Result := Power(A, B);
end;

function EvaluateSimpleNumericBinaryOp(const ALeft, ARight: TGocciaValue; const AOp: TGocciaNumericBinaryOp): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if (ALeft is TGocciaSymbolValue) or (ARight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a number');

  LeftNum := ALeft.ToNumberLiteral;
  RightNum := ARight.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(AOp(LeftNum.Value, RightNum.Value));
end;

function EvaluateAddition(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  // Step 1: Convert both operands to primitives (ECMAScript ToPrimitive)
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  // ECMAScript: Symbols cannot be implicitly converted to strings or numbers
  if (PrimLeft is TGocciaSymbolValue) or (PrimRight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a string');

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

function EvaluateSubtraction(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateSimpleNumericBinaryOp(ALeft, ARight, @DoSubtract);
end;

function EvaluateMultiplication(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateSimpleNumericBinaryOp(ALeft, ARight, @DoMultiply);
end;

function EvaluateDivision(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if (ALeft is TGocciaSymbolValue) or (ARight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a number');

  LeftNum := ALeft.ToNumberLiteral;
  RightNum := ARight.ToNumberLiteral;

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

function EvaluateModulo(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if (ALeft is TGocciaSymbolValue) or (ARight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a number');

  LeftNum := ALeft.ToNumberLiteral;
  RightNum := ARight.ToNumberLiteral;

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

function EvaluateExponentiation(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateSimpleNumericBinaryOp(ALeft, ARight, @DoPower);
end;

function PerformCompoundOperation(const ACurrentValue, ANewValue: TGocciaValue; const AOperator: TGocciaTokenType): TGocciaValue;
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

end.
