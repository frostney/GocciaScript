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

function EvaluateAddition(Left, Right: TGocciaValue): TGocciaValue;
begin
  if (Left is TGocciaStringLiteral) or (Right is TGocciaStringLiteral) then
    Result := TGocciaStringLiteral.Create(Left.ToString + Right.ToString)
  else if (Left is TGocciaNumberLiteral) and (Right is TGocciaNumberLiteral) then
    Result := TGocciaNumberLiteral.Create(
      TGocciaNumberLiteral(Left).Value + TGocciaNumberLiteral(Right).Value)
  else if (Left is TGocciaNumberLiteral) or (Right is TGocciaNumberLiteral) then
    Result := TGocciaNumberLiteral.Create(Left.ToNumber + Right.ToNumber)
  else
    Result := TGocciaStringLiteral.Create(Left.ToString + Right.ToString);
end;

function EvaluateSubtraction(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteral.Create(Left.ToNumber - Right.ToNumber);
end;

function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteral.Create(Left.ToNumber * Right.ToNumber);
end;

function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: Double;
begin
  LeftNum := Left.ToNumber;
  RightNum := Right.ToNumber;

  if RightNum = 0 then
  begin
    if LeftNum = 0 then
      Result := TGocciaNumberLiteral.CreateNaN    // 0 / 0 = NaN
    else if LeftNum > 0 then
      Result := TGocciaNumberLiteral.Create(Infinity)   // positive / 0 = +Infinity
    else
      Result := TGocciaNumberLiteral.Create(-Infinity); // negative / 0 = -Infinity
  end
  else
    Result := TGocciaNumberLiteral.Create(LeftNum / RightNum);
end;

function EvaluateModulo(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteral.Create(
    Trunc(Left.ToNumber) mod Trunc(Right.ToNumber));
end;

function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteral.Create(Power(Left.ToNumber, Right.ToNumber));
end;

function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;
begin
  case Operator of
    gttPlusAssign:
      begin
        if (CurrentValue is TGocciaStringLiteral) or (NewValue is TGocciaStringLiteral) then
          Result := TGocciaStringLiteral.Create(CurrentValue.ToString + NewValue.ToString)
        else
          Result := TGocciaNumberLiteral.Create(CurrentValue.ToNumber + NewValue.ToNumber);
      end;
    gttMinusAssign:
      Result := TGocciaNumberLiteral.Create(CurrentValue.ToNumber - NewValue.ToNumber);
    gttStarAssign:
      Result := TGocciaNumberLiteral.Create(CurrentValue.ToNumber * NewValue.ToNumber);
    gttSlashAssign:
      begin
        if NewValue.ToNumber = 0 then
        begin
          if CurrentValue.ToNumber = 0 then
            Result := TGocciaNumberLiteral.CreateNaN    // 0 /= 0 = NaN
          else if CurrentValue.ToNumber > 0 then
            Result := TGocciaNumberLiteral.Create(Infinity)   // positive /= 0 = +Infinity
          else
            Result := TGocciaNumberLiteral.Create(-Infinity); // negative /= 0 = -Infinity
        end
        else
          Result := TGocciaNumberLiteral.Create(CurrentValue.ToNumber / NewValue.ToNumber);
      end;
    gttPercentAssign:
      Result := TGocciaNumberLiteral.Create(
        Trunc(CurrentValue.ToNumber) mod Trunc(NewValue.ToNumber));
    gttPowerAssign:
      Result := TGocciaNumberLiteral.Create(Power(CurrentValue.ToNumber, NewValue.ToNumber));
    gttBitwiseAndAssign:
      Result := TGocciaNumberLiteral.Create(Trunc(CurrentValue.ToNumber) and Trunc(NewValue.ToNumber));
    gttBitwiseOrAssign:
      Result := TGocciaNumberLiteral.Create(Trunc(CurrentValue.ToNumber) or Trunc(NewValue.ToNumber));
    gttBitwiseXorAssign:
      Result := TGocciaNumberLiteral.Create(Trunc(CurrentValue.ToNumber) xor Trunc(NewValue.ToNumber));
    gttLeftShiftAssign:
      Result := TGocciaNumberLiteral.Create(Trunc(CurrentValue.ToNumber) shl (Trunc(NewValue.ToNumber) and 31));
    gttRightShiftAssign:
      Result := TGocciaNumberLiteral.Create(Trunc(CurrentValue.ToNumber) shr (Trunc(NewValue.ToNumber) and 31));
    gttUnsignedRightShiftAssign:
      Result := TGocciaNumberLiteral.Create(Cardinal(Trunc(CurrentValue.ToNumber)) shr (Trunc(NewValue.ToNumber) and 31));
  else
    Result := TGocciaUndefinedLiteral.Create;
  end;
end;

end.
