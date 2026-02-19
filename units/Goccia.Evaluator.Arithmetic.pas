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

function InfinityWithSign(const APositive: Boolean): TGocciaNumberLiteralValue; inline;
begin
  if APositive then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
end;

function ToNumericPair(const ALeft, ARight: TGocciaValue;
  out ALeftNum, ARightNum: TGocciaNumberLiteralValue): Boolean;
begin
  if (ALeft is TGocciaSymbolValue) or (ARight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a number');
  ALeftNum := ALeft.ToNumberLiteral;
  ARightNum := ARight.ToNumberLiteral;
  Result := not (ALeftNum.IsNaN or ARightNum.IsNaN);
end;

function EvaluateAddition(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaSymbolValue) or (PrimRight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a string');

  if (PrimLeft is TGocciaStringLiteralValue) or (PrimRight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue.Create(PrimLeft.ToStringLiteral.Value + PrimRight.ToStringLiteral.Value);
    Exit;
  end;

  LeftNum := PrimLeft.ToNumberLiteral;
  RightNum := PrimRight.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite and RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinity = RightNum.IsInfinity then
        Result := InfinityWithSign(LeftNum.IsInfinity)
      else
        Result := TGocciaNumberLiteralValue.NaNValue;
    end
    else if LeftNum.IsInfinite then
      Result := InfinityWithSign(LeftNum.IsInfinity)
    else
      Result := InfinityWithSign(RightNum.IsInfinity);
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(LeftNum.Value + RightNum.Value);
end;

function EvaluateSubtraction(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if not ToNumericPair(ALeft, ARight, LeftNum, RightNum) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite and RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinity <> RightNum.IsInfinity then
        Result := InfinityWithSign(LeftNum.IsInfinity)
      else
        Result := TGocciaNumberLiteralValue.NaNValue;
    end
    else if LeftNum.IsInfinite then
      Result := InfinityWithSign(LeftNum.IsInfinity)
    else
      Result := InfinityWithSign(not RightNum.IsInfinity);
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(LeftNum.Value - RightNum.Value);
end;

function EvaluateMultiplication(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftZero, RightZero: Boolean;
  SameSign: Boolean;
begin
  if not ToNumericPair(ALeft, ARight, LeftNum, RightNum) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    LeftZero := (not LeftNum.IsInfinite) and (LeftNum.Value = 0);
    RightZero := (not RightNum.IsInfinite) and (RightNum.Value = 0);
    if LeftZero or RightZero then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;
    SameSign := LeftNum.IsInfinity = RightNum.IsInfinity;
    if not LeftNum.IsInfinite then
      SameSign := (LeftNum.Value > 0) = RightNum.IsInfinity
    else if not RightNum.IsInfinite then
      SameSign := LeftNum.IsInfinity = (RightNum.Value > 0);
    Result := InfinityWithSign(SameSign);
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(LeftNum.Value * RightNum.Value);
end;

function EvaluateDivision(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  SameSign: Boolean;
begin
  if not ToNumericPair(ALeft, ARight, LeftNum, RightNum) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if LeftNum.IsInfinite then
  begin
    if RightNum.IsInfinite then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;
    SameSign := LeftNum.IsInfinity = ((RightNum.Value > 0) or ((RightNum.Value = 0) and not RightNum.IsNegativeZero));
    Result := InfinityWithSign(SameSign);
    Exit;
  end;

  if RightNum.IsInfinite then
  begin
    SameSign := (LeftNum.Value > 0) or ((LeftNum.Value = 0) and not LeftNum.IsNegativeZero);
    if SameSign = RightNum.IsInfinity then
      Result := TGocciaNumberLiteralValue.ZeroValue
    else
      Result := TGocciaNumberLiteralValue.NegativeZeroValue;
    Exit;
  end;

  if RightNum.Value = 0 then
  begin
    if LeftNum.Value = 0 then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if LeftNum.Value > 0 then
    begin
      if RightNum.IsNegativeZero then
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue
      else
        Result := TGocciaNumberLiteralValue.InfinityValue;
    end
    else
    begin
      if RightNum.IsNegativeZero then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
    end;
  end
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value / RightNum.Value);
end;

function EvaluateModulo(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if not ToNumericPair(ALeft, ARight, LeftNum, RightNum) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if LeftNum.IsInfinite then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if RightNum.IsInfinite then
  begin
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value);
    Exit;
  end;

  if RightNum.Value = 0 then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(
    LeftNum.Value - RightNum.Value * Trunc(LeftNum.Value / RightNum.Value));
end;

function IsActualZero(const ANum: TGocciaNumberLiteralValue): Boolean; inline;
begin
  Result := (ANum.Value = 0) and not ANum.IsNaN and not ANum.IsInfinite;
end;

function EvaluateExponentiation(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if not ToNumericPair(ALeft, ARight, LeftNum, RightNum) then
  begin
    if IsActualZero(RightNum) then
      Result := TGocciaNumberLiteralValue.OneValue
    else
      Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  if IsActualZero(RightNum) then
  begin
    Result := TGocciaNumberLiteralValue.OneValue;
    Exit;
  end;

  if RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite or (Abs(LeftNum.Value) > 1) then
    begin
      if RightNum.IsInfinity then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else
        Result := TGocciaNumberLiteralValue.ZeroValue;
    end
    else if Abs(LeftNum.Value) = 1 then
      Result := TGocciaNumberLiteralValue.NaNValue
    else
    begin
      if RightNum.IsInfinity then
        Result := TGocciaNumberLiteralValue.ZeroValue
      else
        Result := TGocciaNumberLiteralValue.InfinityValue;
    end;
    Exit;
  end;

  if LeftNum.IsInfinite then
  begin
    if RightNum.Value > 0 then
    begin
      if LeftNum.IsInfinity then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else if Frac(RightNum.Value) <> 0 then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else if Frac(RightNum.Value / 2) = 0 then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
    end
    else
    begin
      if LeftNum.IsNegativeInfinity and (Frac(RightNum.Value) = 0) and (Frac(RightNum.Value / 2) <> 0) then
        Result := TGocciaNumberLiteralValue.NegativeZeroValue
      else
        Result := TGocciaNumberLiteralValue.ZeroValue;
    end;
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(Power(LeftNum.Value, RightNum.Value));
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
