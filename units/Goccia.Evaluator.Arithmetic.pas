unit Goccia.Evaluator.Arithmetic;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.NumberValue, Goccia.Token, Goccia.Values.StringValue, Goccia.Values.UndefinedValue,
  Math, SysUtils;

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
  if (Left is TGocciaStringValue) or (Right is TGocciaStringValue) then
    Result := TGocciaStringValue.Create(Left.ToString + Right.ToString)
  else if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
    Result := TGocciaNumberValue.Create(
      TGocciaNumberValue(Left).Value + TGocciaNumberValue(Right).Value)
  else if (Left is TGocciaNumberValue) or (Right is TGocciaNumberValue) then
    Result := TGocciaNumberValue.Create(Left.ToNumber + Right.ToNumber)
  else
    Result := TGocciaStringValue.Create(Left.ToString + Right.ToString);
end;

function EvaluateSubtraction(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Left.ToNumber - Right.ToNumber);
end;

function EvaluateMultiplication(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Left.ToNumber * Right.ToNumber);
end;

function EvaluateDivision(Left, Right: TGocciaValue): TGocciaValue;
var
  RightNum: Double;
begin
  RightNum := Right.ToNumber;
  if RightNum = 0 then
    Result := TGocciaNumberValue.Create(Infinity)
  else
    Result := TGocciaNumberValue.Create(Left.ToNumber / RightNum);
end;

function EvaluateModulo(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(
    Trunc(Left.ToNumber) mod Trunc(Right.ToNumber));
end;

function EvaluateExponentiation(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Power(Left.ToNumber, Right.ToNumber));
end;

function PerformCompoundOperation(CurrentValue, NewValue: TGocciaValue; Operator: TGocciaTokenType): TGocciaValue;
begin
  case Operator of
    gttPlusAssign:
      begin
        if (CurrentValue is TGocciaStringValue) or (NewValue is TGocciaStringValue) then
          Result := TGocciaStringValue.Create(CurrentValue.ToString + NewValue.ToString)
        else
          Result := TGocciaNumberValue.Create(CurrentValue.ToNumber + NewValue.ToNumber);
      end;
    gttMinusAssign:
      Result := TGocciaNumberValue.Create(CurrentValue.ToNumber - NewValue.ToNumber);
    gttStarAssign:
      Result := TGocciaNumberValue.Create(CurrentValue.ToNumber * NewValue.ToNumber);
    gttSlashAssign:
      begin
        if NewValue.ToNumber = 0 then
          Result := TGocciaNumberValue.Create(Infinity)
        else
          Result := TGocciaNumberValue.Create(CurrentValue.ToNumber / NewValue.ToNumber);
      end;
    gttPercentAssign:
      Result := TGocciaNumberValue.Create(
        Trunc(CurrentValue.ToNumber) mod Trunc(NewValue.ToNumber));
    gttPowerAssign:
      Result := TGocciaNumberValue.Create(Power(CurrentValue.ToNumber, NewValue.ToNumber));
    gttBitwiseAndAssign:
      Result := TGocciaNumberValue.Create(Trunc(CurrentValue.ToNumber) and Trunc(NewValue.ToNumber));
    gttBitwiseOrAssign:
      Result := TGocciaNumberValue.Create(Trunc(CurrentValue.ToNumber) or Trunc(NewValue.ToNumber));
    gttBitwiseXorAssign:
      Result := TGocciaNumberValue.Create(Trunc(CurrentValue.ToNumber) xor Trunc(NewValue.ToNumber));
    gttLeftShiftAssign:
      Result := TGocciaNumberValue.Create(Trunc(CurrentValue.ToNumber) shl (Trunc(NewValue.ToNumber) and 31));
    gttRightShiftAssign:
      Result := TGocciaNumberValue.Create(Trunc(CurrentValue.ToNumber) shr (Trunc(NewValue.ToNumber) and 31));
    gttUnsignedRightShiftAssign:
      Result := TGocciaNumberValue.Create(Cardinal(Trunc(CurrentValue.ToNumber)) shr (Trunc(NewValue.ToNumber) and 31));
  else
    Result := TGocciaUndefinedValue.Create;
  end;
end;

end.