unit Goccia.Values.ArithmeticOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Values.Primitives, Goccia.Values.Constants,
  SysUtils, Math;

type
  TGocciaArithmetic = class helper for TGocciaValue
  public
    function Add(Other: TGocciaValue): TGocciaValue;
    function Subtract(Other: TGocciaValue): TGocciaValue;
    function Multiply(Other: TGocciaValue): TGocciaValue;
    function Divide(Other: TGocciaValue): TGocciaValue;
    function Modulo(Other: TGocciaValue): TGocciaValue;
    function Power(Other: TGocciaValue): TGocciaValue;

    function BitwiseAnd(Other: TGocciaValue): TGocciaValue;
    function BitwiseOr(Other: TGocciaValue): TGocciaValue;
    function BitwiseXor(Other: TGocciaValue): TGocciaValue;
    function LeftShift(Other: TGocciaValue): TGocciaValue;
    function RightShift(Other: TGocciaValue): TGocciaValue;
    function UnsignedRightShift(Other: TGocciaValue): TGocciaValue;
    function BitwiseNot: TGocciaValue;

    function IsEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsNotEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsLessThan(Other: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsGreaterThan(Other: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsLessThanOrEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsGreaterThanOrEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
  end;

implementation

uses
  Goccia.Values.TypeCoercion;

{ TGocciaArithmetic }

function TGocciaArithmetic.Add(Other: TGocciaValue): TGocciaValue;
var
  LeftStr, RightStr: TGocciaStringLiteralValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  // ECMAScript Addition: If either operand is string, do string concatenation
  if (Self is TGocciaStringLiteralValue) or (Other is TGocciaStringLiteralValue) then
  begin
    LeftStr := Self.ToStringLiteral;
    RightStr := Other.ToStringLiteral;
    Result := TGocciaStringLiteralValue.Create(LeftStr.Value + RightStr.Value);
  end
  else
  begin
    // Numeric addition
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    // Handle special cases
    if LeftNum.IsNaN or RightNum.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if LeftNum.IsInfinity then
    begin
      if RightNum.IsNegativeInfinity then
        Result := TGocciaNumberLiteralValue.NaNValue
      else
        Result := TGocciaNumberLiteralValue.InfinityValue;
    end
    else if LeftNum.IsNegativeInfinity then
    begin
      if RightNum.IsInfinity then
        Result := TGocciaNumberLiteralValue.NaNValue
      else
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
    end
    else if RightNum.IsInfinity then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else if RightNum.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue
    else
      Result := TGocciaNumberLiteralValue.Create(LeftNum.Value + RightNum.Value);
  end;
end;

function TGocciaArithmetic.Subtract(Other: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Self.ToNumberLiteral;
  RightNum := Other.ToNumberLiteral;

  // Handle special cases
  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if LeftNum.IsInfinity and RightNum.IsInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if LeftNum.IsNegativeInfinity and RightNum.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if LeftNum.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if LeftNum.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if RightNum.IsInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if RightNum.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value - RightNum.Value);
end;

function TGocciaArithmetic.Multiply(Other: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Self.ToNumberLiteral;
  RightNum := Other.ToNumberLiteral;

  // Handle special cases
  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if (LeftNum.IsInfinity or LeftNum.IsNegativeInfinity) and (RightNum.Value = 0) then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if (RightNum.IsInfinity or RightNum.IsNegativeInfinity) and (LeftNum.Value = 0) then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if LeftNum.IsInfinity then
  begin
    if RightNum.Value < 0 then
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue
    else
      Result := TGocciaNumberLiteralValue.InfinityValue;
  end
  else if LeftNum.IsNegativeInfinity then
  begin
    if RightNum.Value < 0 then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
  end
  else if RightNum.IsInfinity then
  begin
    if LeftNum.Value < 0 then
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue
    else
      Result := TGocciaNumberLiteralValue.InfinityValue;
  end
  else if RightNum.IsNegativeInfinity then
  begin
    if LeftNum.Value < 0 then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
  end
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value * RightNum.Value);
end;

function TGocciaArithmetic.Divide(Other: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Self.ToNumberLiteral;
  RightNum := Other.ToNumberLiteral;

  // Handle special cases
  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if RightNum.Value = 0 then
  begin
    if LeftNum.Value = 0 then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if LeftNum.Value > 0 then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
  end
  else if LeftNum.IsInfinity then
  begin
    if RightNum.IsInfinity or RightNum.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if RightNum.Value < 0 then
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue
    else
      Result := TGocciaNumberLiteralValue.InfinityValue;
  end
  else if LeftNum.IsNegativeInfinity then
  begin
    if RightNum.IsInfinity or RightNum.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if RightNum.Value < 0 then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
  end
  else if RightNum.IsInfinity or RightNum.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else
    Result := TGocciaNumberLiteralValue.Create(LeftNum.Value / RightNum.Value);
end;

function TGocciaArithmetic.Modulo(Other: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  ModResult: Double;
begin
  LeftNum := Self.ToNumberLiteral;
  RightNum := Other.ToNumberLiteral;

  // Handle special cases according to ECMAScript spec
  if LeftNum.IsNaN or RightNum.IsNaN or
     LeftNum.IsInfinity or LeftNum.IsNegativeInfinity or
     RightNum.Value = 0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if RightNum.IsInfinity or RightNum.IsNegativeInfinity then
    Result := LeftNum
  else if LeftNum.Value = 0 then
    Result := LeftNum
  else
  begin
    ModResult := Fmod(LeftNum.Value, RightNum.Value);
    Result := TGocciaNumberLiteralValue.Create(ModResult);
  end;
end;

function TGocciaArithmetic.Power(Other: TGocciaValue): TGocciaValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  LeftNum := Self.ToNumberLiteral;
  RightNum := Other.ToNumberLiteral;

  // Handle special cases
  if LeftNum.IsNaN or RightNum.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Power(LeftNum.Value, RightNum.Value));
end;

function TGocciaArithmetic.BitwiseAnd(Other: TGocciaValue): TGocciaValue;
var
  LeftInt, RightInt: Integer;
begin
  LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
  RightInt := Trunc(Other.ToNumberLiteral.Value) and $7FFFFFFF;
  Result := TGocciaNumberLiteralValue.Create(LeftInt and RightInt);
end;

function TGocciaArithmetic.BitwiseOr(Other: TGocciaValue): TGocciaValue;
var
  LeftInt, RightInt: Integer;
begin
  LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
  RightInt := Trunc(Other.ToNumberLiteral.Value) and $7FFFFFFF;
  Result := TGocciaNumberLiteralValue.Create(LeftInt or RightInt);
end;

function TGocciaArithmetic.BitwiseXor(Other: TGocciaValue): TGocciaValue;
var
  LeftInt, RightInt: Integer;
begin
  LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
  RightInt := Trunc(Other.ToNumberLiteral.Value) and $7FFFFFFF;
  Result := TGocciaNumberLiteralValue.Create(LeftInt xor RightInt);
end;

function TGocciaArithmetic.LeftShift(Other: TGocciaValue): TGocciaValue;
var
  LeftInt, RightInt: Integer;
begin
  LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
  RightInt := Trunc(Other.ToNumberLiteral.Value) and $1F; // Only use lower 5 bits
  Result := TGocciaNumberLiteralValue.Create(LeftInt shl RightInt);
end;

function TGocciaArithmetic.RightShift(Other: TGocciaValue): TGocciaValue;
var
  LeftInt, RightInt: Integer;
begin
  LeftInt := Trunc(Self.ToNumberLiteral.Value);
  RightInt := Trunc(Other.ToNumberLiteral.Value) and $1F; // Only use lower 5 bits
  Result := TGocciaNumberLiteralValue.Create(LeftInt shr RightInt);
end;

function TGocciaArithmetic.UnsignedRightShift(Other: TGocciaValue): TGocciaValue;
var
  LeftInt, RightInt: Cardinal;
begin
  LeftInt := Cardinal(Trunc(Self.ToNumberLiteral.Value));
  RightInt := Cardinal(Trunc(Other.ToNumberLiteral.Value)) and $1F; // Only use lower 5 bits
  Result := TGocciaNumberLiteralValue.Create(LeftInt shr RightInt);
end;

function TGocciaArithmetic.BitwiseNot: TGocciaValue;
var
  IntValue: Integer;
begin
  IntValue := Trunc(Self.ToNumberLiteral.Value);
  Result := TGocciaNumberLiteralValue.Create(not IntValue);
end;

function TGocciaArithmetic.IsEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
begin
  // Strict equality comparison
  if (Self is TGocciaUndefinedLiteralValue) and (Other is TGocciaUndefinedLiteralValue) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else if (Self is TGocciaNullLiteralValue) and (Other is TGocciaNullLiteralValue) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else if (Self is TGocciaBooleanLiteralValue) and (Other is TGocciaBooleanLiteralValue) then
    Result := TGocciaBooleanLiteralValue.Create(
      TGocciaBooleanLiteralValue(Self).Value = TGocciaBooleanLiteralValue(Other).Value)
  else if (Self is TGocciaNumberLiteralValue) and (Other is TGocciaNumberLiteralValue) then
  begin
    // Handle NaN case
    if TGocciaNumberLiteralValue(Self).IsNaN or TGocciaNumberLiteralValue(Other).IsNaN then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.Create(
        TGocciaNumberLiteralValue(Self).Value = TGocciaNumberLiteralValue(Other).Value);
  end
  else if (Self is TGocciaStringLiteralValue) and (Other is TGocciaStringLiteralValue) then
    Result := TGocciaBooleanLiteralValue.Create(
      TGocciaStringLiteralValue(Self).Value = TGocciaStringLiteralValue(Other).Value)
  else
    Result := TGocciaBooleanLiteralValue.Create(Self = Other); // Reference equality
end;

function TGocciaArithmetic.IsNotEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(not IsEqual(Other).Value);
end;

function TGocciaArithmetic.IsLessThan(Other: TGocciaValue): TGocciaBooleanLiteralValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftStr, RightStr: TGocciaStringLiteralValue;
begin
  if (Self is TGocciaStringLiteralValue) and (Other is TGocciaStringLiteralValue) then
  begin
    LeftStr := TGocciaStringLiteralValue(Self);
    RightStr := TGocciaStringLiteralValue(Other);
    Result := TGocciaBooleanLiteralValue.Create(LeftStr.Value < RightStr.Value);
  end
  else
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    if LeftNum.IsNaN or RightNum.IsNaN then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.Create(LeftNum.Value < RightNum.Value);
  end;
end;

function TGocciaArithmetic.IsGreaterThan(Other: TGocciaValue): TGocciaBooleanLiteralValue;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftStr, RightStr: TGocciaStringLiteralValue;
begin
  if (Self is TGocciaStringLiteralValue) and (Other is TGocciaStringLiteralValue) then
  begin
    LeftStr := TGocciaStringLiteralValue(Self);
    RightStr := TGocciaStringLiteralValue(Other);
    Result := TGocciaBooleanLiteralValue.Create(LeftStr.Value > RightStr.Value);
  end
  else
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    if LeftNum.IsNaN or RightNum.IsNaN then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.Create(LeftNum.Value > RightNum.Value);
  end;
end;

function TGocciaArithmetic.IsLessThanOrEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    IsLessThan(Other).Value or IsEqual(Other).Value);
end;

function TGocciaArithmetic.IsGreaterThanOrEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    IsGreaterThan(Other).Value or IsEqual(Other).Value);
end;

end.
