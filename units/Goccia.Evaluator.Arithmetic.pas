unit Goccia.Evaluator.Arithmetic;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Token, Math, SysUtils;

function EvaluateAddition(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateSubtraction(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateModulo(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue; inline;
function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;

implementation

uses Goccia.Values.ClassHelper, Goccia.Values.ToPrimitive;

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
  Result := TGocciaNumberLiteralValue.Create(Left.ToNumberLiteral.Value - Right.ToNumberLiteral.Value);
end;

function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Left.ToNumberLiteral.Value * Right.ToNumberLiteral.Value);
end;

function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: Double;
begin
  LeftNum := Left.ToNumberLiteral.Value;
  RightNum := Right.ToNumberLiteral.Value;

  if RightNum = 0 then
  begin
    if LeftNum = 0 then
      Result := TGocciaNumberLiteralValue.NaNValue    // 0 / 0 = NaN
    else if LeftNum > 0 then
      Result := TGocciaNumberLiteralValue.InfinityValue   // positive / 0 = +Infinity
    else
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue; // negative / 0 = -Infinity
  end
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum / RightNum);
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
  Result := TGocciaNumberLiteralValue.Create(Power(Left.ToNumberLiteral.Value, Right.ToNumberLiteral.Value));
end;

function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;
begin
  case Operator of
    gttPlusAssign:
      Result := EvaluateAddition(CurrentValue, NewValue);
    gttMinusAssign:
      Result := TGocciaNumberLiteralValue.Create(CurrentValue.ToNumberLiteral.Value - NewValue.ToNumberLiteral.Value);
    gttStarAssign:
      Result := TGocciaNumberLiteralValue.Create(CurrentValue.ToNumberLiteral.Value * NewValue.ToNumberLiteral.Value);
    gttSlashAssign:
      begin
        if NewValue.ToNumberLiteral.Value = 0 then
        begin
          if CurrentValue.ToNumberLiteral.Value = 0 then
            Result := TGocciaNumberLiteralValue.NaNValue    // 0 /= 0 = NaN
          else if CurrentValue.ToNumberLiteral.Value > 0 then
            Result := TGocciaNumberLiteralValue.InfinityValue   // positive /= 0 = +Infinity
          else
            Result := TGocciaNumberLiteralValue.NegativeInfinityValue; // negative /= 0 = -Infinity
        end
        else
          Result := TGocciaNumberLiteralValue.Create(CurrentValue.ToNumberLiteral.Value / NewValue.ToNumberLiteral.Value);
      end;
    gttPercentAssign:
      Result := EvaluateModulo(CurrentValue, NewValue);
    gttPowerAssign:
      Result := TGocciaNumberLiteralValue.Create(Power(CurrentValue.ToNumberLiteral.Value, NewValue.ToNumberLiteral.Value));
    gttBitwiseAndAssign:
      Result := TGocciaNumberLiteralValue.Create(Trunc(CurrentValue.ToNumberLiteral.Value) and Trunc(NewValue.ToNumberLiteral.Value));
    gttBitwiseOrAssign:
      Result := TGocciaNumberLiteralValue.Create(Trunc(CurrentValue.ToNumberLiteral.Value) or Trunc(NewValue.ToNumberLiteral.Value));
    gttBitwiseXorAssign:
      Result := TGocciaNumberLiteralValue.Create(Trunc(CurrentValue.ToNumberLiteral.Value) xor Trunc(NewValue.ToNumberLiteral.Value));
    gttLeftShiftAssign:
      Result := TGocciaNumberLiteralValue.Create(Trunc(CurrentValue.ToNumberLiteral.Value) shl (Trunc(NewValue.ToNumberLiteral.Value) and 31));
    gttRightShiftAssign:
      Result := TGocciaNumberLiteralValue.Create(Trunc(CurrentValue.ToNumberLiteral.Value) shr (Trunc(NewValue.ToNumberLiteral.Value) and 31));
    gttUnsignedRightShiftAssign:
      Result := TGocciaNumberLiteralValue.Create(Cardinal(Trunc(CurrentValue.ToNumberLiteral.Value)) shr (Trunc(NewValue.ToNumberLiteral.Value) and 31));
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

end.
