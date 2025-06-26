unit Goccia.Evaluator.Arithmetic;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Values.Primitives, Goccia.Token, Math, SysUtils;

function EvaluateAddition(Left, Right: TGocciaValue): TGocciaValue;
function EvaluateSubtraction(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateModulo(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue; inline;
function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;

implementation

uses Goccia.Values.ClassHelper;

function EvaluateAddition(Left, Right: TGocciaValue): TGocciaValue;
begin
  if (Left is TGocciaStringLiteralValue) or (Right is TGocciaStringLiteralValue) then
    Result := TGocciaStringLiteralValue.Create(Left.ToStringLiteral.Value + Right.ToStringLiteral.Value)
  else if (Left is TGocciaNumberLiteralValue) and (Right is TGocciaNumberLiteralValue) then
    Result := TGocciaNumberLiteralValue.Create(
      TGocciaNumberLiteralValue(Left).Value + TGocciaNumberLiteralValue(Right).Value)
  else if (Left is TGocciaNumberLiteralValue) or (Right is TGocciaNumberLiteralValue) then
    Result := TGocciaNumberLiteralValue.Create(Left.ToNumberLiteral.Value + Right.ToNumberLiteral.Value)
  else
    Result := TGocciaStringLiteralValue.Create(Left.ToStringLiteral.Value + Right.ToStringLiteral.Value);
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
begin
  Result := TGocciaNumberLiteralValue.Create(
    Trunc(Left.ToNumberLiteral.Value) mod Trunc(Right.ToNumberLiteral.Value));
end;

function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Power(Left.ToNumberLiteral.Value, Right.ToNumberLiteral.Value));
end;

function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;
begin
  case Operator of
    gttPlusAssign:
      begin
        if (CurrentValue is TGocciaStringLiteralValue) or (NewValue is TGocciaStringLiteralValue) then
          Result := TGocciaStringLiteralValue.Create(CurrentValue.ToString + NewValue.ToString)
        else
          Result := TGocciaNumberLiteralValue.Create(CurrentValue.ToNumberLiteral.Value + NewValue.ToNumberLiteral.Value);
      end;
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
      Result := TGocciaNumberLiteralValue.Create(
        Trunc(CurrentValue.ToNumberLiteral.Value) mod Trunc(NewValue.ToNumberLiteral.Value));
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
    Result := TGocciaUndefinedLiteralValue.Create;
  end;
end;

end.
