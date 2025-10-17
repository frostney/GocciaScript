unit Goccia.Values.ClassHelper;

{$I Goccia.inc}

interface

uses
  SysUtils, Classes, Math, Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.BooleanObjectValue, Goccia.Values.NumberObjectValue, Goccia.Values.StringObjectValue, Goccia.Values.Interfaces, Goccia.Values.Constants;

type

  TGocciaValueHelper = class helper for TGocciaValue
  public
    // Type coercion helpers
    // function ToBooleanLiteral: TGocciaBooleanLiteralValue;
    // function ToNumberLiteral: TGocciaNumberLiteralValue;
    // function ToStringLiteral: TGocciaStringLiteralValue;

    function ToStringObject: TGocciaStringObjectValue;
    function ToBooleanObject: TGocciaBooleanObjectValue;
    function ToNumberObject: TGocciaNumberObjectValue;

    // Boxing helpers
    function Box: TGocciaObjectValue;

    // Arithmetic helpers
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

  // function TGocciaValueHelper.ToBooleanLiteral: TGocciaBooleanLiteralValue;
  // var
  //   NumberValue: TGocciaNumberLiteralValue;
  // begin
  //   // Copies value
  //   if Self is TGocciaBooleanLiteralValue then
  //   begin
  //     Result := TGocciaBooleanLiteralValue.Create((Self as TGocciaBooleanLiteralValue).Value);
  //     Exit;
  //   end;

  //   if Self is TGocciaNumberLiteralValue then
  //   begin
  //     NumberValue := (Self as TGocciaNumberLiteralValue);

  //     if NumberValue.IsNaN then
  //       Result := TGocciaBooleanLiteralValue.Create(False)
  //     else if NumberValue.IsInfinity then
  //       Result := TGocciaBooleanLiteralValue.Create(True)
  //     else if NumberValue.IsNegativeInfinity then
  //       Result := TGocciaBooleanLiteralValue.Create(False)
  //     else
  //       Result := TGocciaBooleanLiteralValue.Create(NumberValue.Value > ZERO_VALUE);
  //     Exit;
  //   end;

  //   if Self is TGocciaStringLiteralValue then
  //   begin
  //     Result := TGocciaBooleanLiteralValue.Create((Self as TGocciaStringLiteralValue).Value <> EMPTY_STRING);
  //     Exit;
  //   end;

  //   if Self is TGocciaNullLiteralValue then
  //   begin
  //     Result := TGocciaBooleanLiteralValue.Create(False);
  //     Exit;
  //   end;

  //   if Self is TGocciaUndefinedLiteralValue then
  //   begin
  //     Result := TGocciaBooleanLiteralValue.Create(False);
  //     Exit;
  //   end;

  //   Result := TGocciaBooleanLiteralValue.Create(False);
  // end;

  // function TGocciaValueHelper.ToNumberLiteral: TGocciaNumberLiteralValue;
  // var
  //   NumberValue: TGocciaNumberLiteralValue;
  // begin
  //   if Self is TGocciaNumberLiteralValue then
  //   begin
  //     NumberValue := (Self as TGocciaNumberLiteralValue);

  //     if NumberValue.IsNaN then
  //       Result := TGocciaNumberLiteralValue.NaNValue
  //     else if NumberValue.IsInfinity then
  //       Result := TGocciaNumberLiteralValue.InfinityValue
  //     else if NumberValue.IsNegativeInfinity then
  //       Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  //     else
  //       Result := TGocciaNumberLiteralValue.Create(NumberValue.Value);
  //     Exit;
  //   end;

  //   if Self is TGocciaBooleanLiteralValue then
  //   begin
  //     if (Self as TGocciaBooleanLiteralValue).Value then
  //       Result := TGocciaNumberLiteralValue.OneValue
  //     else
  //       Result := TGocciaNumberLiteralValue.ZeroValue;
  //     Exit;
  //   end;

  //   if Self is TGocciaStringLiteralValue then
  //   begin
  //     Result := TGocciaNumberLiteralValue.Create(StrToFloat((Self as TGocciaStringLiteralValue).Value));
  //     Exit;
  //   end;

  //   if Self is TGocciaNullLiteralValue then
  //   begin
  //     Result := TGocciaNumberLiteralValue.Create(ZERO_VALUE);
  //     Exit;
  //   end;

  //   if Self is TGocciaUndefinedLiteralValue then
  //   begin
  //     Result := TGocciaNumberLiteralValue.NaNValue;
  //     Exit;
  //   end;

  //   if Self is TGocciaObjectValue then
  //   begin
  //     if Self is TGocciaStringObjectValue then
  //     begin
  //       Result := TGocciaNumberLiteralValue.Create(StrToFloat((Self as TGocciaStringObjectValue).Primitive.Value));
  //       Exit;
  //     end;

  //     if Self is TGocciaBooleanObjectValue then
  //     begin
  //       if (Self as TGocciaBooleanObjectValue).Primitive.Value then
  //         Result := TGocciaNumberLiteralValue.OneValue
  //       else
  //         Result := TGocciaNumberLiteralValue.ZeroValue;
  //       Exit;
  //     end;

  //     if Self is TGocciaNumberObjectValue then
  //     begin
  //       Result := (Self as TGocciaNumberObjectValue).Primitive;
  //       Exit;
  //     end;

  //     Result := TGocciaNumberLiteralValue.NaNValue;
  //     Exit;
  //   end;

  //   Result := TGocciaNumberLiteralValue.NaNValue;
  // end;

  // function TGocciaValueHelper.ToStringLiteral: TGocciaStringLiteralValue;
  // var NumberValue: TGocciaNumberLiteralValue;
  // begin
  //   if Self is TGocciaStringLiteralValue then
  //   begin
  //     Result := TGocciaStringLiteralValue.Create((Self as TGocciaStringLiteralValue).Value);
  //     Exit;
  //   end;

  //   if Self is TGocciaNumberLiteralValue then
  //   begin
  //     NumberValue := (Self as TGocciaNumberLiteralValue);

  //     if NumberValue.IsNaN then
  //       Result := TGocciaStringLiteralValue.Create(NAN_LITERAL)
  //     else if NumberValue.IsInfinity then
  //       Result := TGocciaStringLiteralValue.Create(INFINITY_LITERAL)
  //     else if NumberValue.IsNegativeInfinity then
  //       Result := TGocciaStringLiteralValue.Create(NEGATIVE_INFINITY_LITERAL)
  //     else
  //       Result := TGocciaStringLiteralValue.Create(FloatToStr(NumberValue.Value));
  //     Exit;
  //   end;

  //   if Self is TGocciaBooleanLiteralValue then
  //   begin
  //     if (Self as TGocciaBooleanLiteralValue).Value then
  //       Result := TGocciaStringLiteralValue.Create(BOOLEAN_TRUE_LITERAL)
  //     else
  //       Result := TGocciaStringLiteralValue.Create(BOOLEAN_FALSE_LITERAL);
  //     Exit;
  //   end;

  //   if Self is TGocciaNullLiteralValue then
  //   begin
  //     Result := TGocciaStringLiteralValue.Create(NULL_LITERAL);
  //     Exit;
  //   end;

  //   if Self is TGocciaUndefinedLiteralValue then
  //   begin
  //     Result := TGocciaStringLiteralValue.Create(UNDEFINED_LITERAL);
  //     Exit;
  //   end;

  //   if Self is TGocciaObjectValue then
  //   begin
  //     if Self is TGocciaStringObjectValue then
  //     begin
  //       Result := TGocciaStringLiteralValue.Create((Self as TGocciaStringObjectValue).Primitive.Value);
  //       Exit;
  //     end;

  //     if Self is TGocciaBooleanObjectValue then
  //     begin
  //       Result := TGocciaStringLiteralValue.Create(BOOLEAN_TRUE_LITERAL);
  //       Exit;
  //     end;

  //     if Self is TGocciaNumberObjectValue then
  //     begin
  //       Result := TGocciaStringLiteralValue.Create(FloatToStr((Self as TGocciaNumberObjectValue).Primitive.Value));
  //       Exit;
  //     end;

  //     Result := TGocciaStringLiteralValue.Create('[' + (Self as TGocciaObjectValue).TypeName + ' ' + (Self as TGocciaObjectValue).ToStringTag + ']');
  //     Exit;
  //   end;

  //   Result := TGocciaStringLiteralValue.Create(EMPTY_STRING);
  // end;

  function TGocciaValueHelper.ToNumberObject: TGocciaNumberObjectValue;
  begin
    Result := TGocciaNumberObjectValue.Create(Self.ToNumberLiteral);
  end;

  function TGocciaValueHelper.ToStringObject: TGocciaStringObjectValue;
  begin
    Result := TGocciaStringObjectValue.Create(Self.ToStringLiteral);
  end;

  function TGocciaValueHelper.ToBooleanObject: TGocciaBooleanObjectValue;
  begin
    Result := TGocciaBooleanObjectValue.Create(Self.ToBooleanLiteral);
  end;


  function TGocciaValueHelper.Box: TGocciaObjectValue;
  begin
    if Self is TGocciaBooleanLiteralValue then
    begin
      Result := TGocciaBooleanObjectValue.Create(Self.ToBooleanLiteral);
      Exit;
    end;

    if Self is TGocciaNumberLiteralValue then
    begin
      Result := TGocciaNumberObjectValue.Create(Self.ToNumberLiteral);
      Exit;
    end;

    if Self is TGocciaStringLiteralValue then
    begin
      Result := TGocciaStringObjectValue.Create(Self.ToStringLiteral);
      Exit;
    end;

    Result := nil;
  end;



  function TGocciaValueHelper.Add(Other: TGocciaValue): TGocciaValue;
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

  function TGocciaValueHelper.Subtract(Other: TGocciaValue): TGocciaValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    // Handle special cases
    if (LeftNum.IsNaN or RightNum.IsNaN) then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if (LeftNum.IsInfinity and RightNum.IsInfinity) then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if (LeftNum.IsNegativeInfinity and RightNum.IsNegativeInfinity) then
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

  function TGocciaValueHelper.Multiply(Other: TGocciaValue): TGocciaValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    // Handle special cases
    if LeftNum.IsNaN or RightNum.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if ((LeftNum.IsInfinity or LeftNum.IsNegativeInfinity) and (RightNum.Value = 0)) then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if ((RightNum.IsInfinity or RightNum.IsNegativeInfinity) and (LeftNum.Value = 0)) then
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

  function TGocciaValueHelper.Divide(Other: TGocciaValue): TGocciaValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    // Handle special cases
    if (LeftNum.IsNaN or RightNum.IsNaN) then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if (RightNum.Value = 0) then
    begin
      if (LeftNum.Value = 0) then
        Result := TGocciaNumberLiteralValue.NaNValue
      else if (LeftNum.Value > 0) then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
    end
    else if LeftNum.IsInfinity then
    begin
      if (RightNum.IsInfinity or RightNum.IsNegativeInfinity) then
        Result := TGocciaNumberLiteralValue.NaNValue
      else if (RightNum.Value < 0) then
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue
      else
        Result := TGocciaNumberLiteralValue.InfinityValue;
    end
    else if LeftNum.IsNegativeInfinity then
    begin
      if (RightNum.IsInfinity or RightNum.IsNegativeInfinity) then
        Result := TGocciaNumberLiteralValue.NaNValue
      else if (RightNum.Value < 0) then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
    end
    else if (RightNum.IsInfinity or RightNum.IsNegativeInfinity) then
      Result := TGocciaNumberLiteralValue.ZeroValue
    else
      Result := TGocciaNumberLiteralValue.Create(LeftNum.Value / RightNum.Value);
  end;

  function TGocciaValueHelper.Modulo(Other: TGocciaValue): TGocciaValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
    ModResult: Double;
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    // Handle special cases according to ECMAScript spec
    if (LeftNum.IsNaN or RightNum.IsNaN or
      LeftNum.IsInfinity or LeftNum.IsNegativeInfinity or
      (RightNum.Value = 0)) then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if (RightNum.IsInfinity or RightNum.IsNegativeInfinity) then
      Result := LeftNum
    else if (LeftNum.Value = 0) then
      Result := LeftNum
    else
    begin
      ModResult := Fmod(LeftNum.Value, RightNum.Value);
      Result := TGocciaNumberLiteralValue.Create(ModResult);
    end;
  end;

  function TGocciaValueHelper.Power(Other: TGocciaValue): TGocciaValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
    Base, Exponent: Extended;
  begin
    LeftNum := Self.ToNumberLiteral;
    RightNum := Other.ToNumberLiteral;

    // Handle special cases
    if (LeftNum.IsNaN or RightNum.IsNaN) then
      Result := TGocciaNumberLiteralValue.NaNValue
    else
    begin
      Base := LeftNum.Value;
      Exponent := RightNum.Value;
      Result := TGocciaNumberLiteralValue.Create(Math.Power(Base, Exponent));
    end;
  end;

  function TGocciaValueHelper.BitwiseAnd(Other: TGocciaValue): TGocciaValue;
  var
    LeftInt, RightInt: Integer;
  begin
    LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
    RightInt := Trunc(Other.ToNumberLiteral.Value) and $7FFFFFFF;
    Result := TGocciaNumberLiteralValue.Create(LeftInt and RightInt);
  end;

  function TGocciaValueHelper.BitwiseOr(Other: TGocciaValue): TGocciaValue;
  var
    LeftInt, RightInt: Integer;
  begin
    LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
    RightInt := Trunc(Other.ToNumberLiteral.Value) and $7FFFFFFF;
    Result := TGocciaNumberLiteralValue.Create(LeftInt or RightInt);
  end;

  function TGocciaValueHelper.BitwiseXor(Other: TGocciaValue): TGocciaValue;
  var
    LeftInt, RightInt: Integer;
  begin
    LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
    RightInt := Trunc(Other.ToNumberLiteral.Value) and $7FFFFFFF;
    Result := TGocciaNumberLiteralValue.Create(LeftInt xor RightInt);
  end;

  function TGocciaValueHelper.LeftShift(Other: TGocciaValue): TGocciaValue;
  var
    LeftInt, RightInt: Integer;
  begin
    LeftInt := Trunc(Self.ToNumberLiteral.Value) and $7FFFFFFF;
    RightInt := Trunc(Other.ToNumberLiteral.Value) and $1F; // Only use lower 5 bits
    Result := TGocciaNumberLiteralValue.Create(LeftInt shl RightInt);
  end;

  function TGocciaValueHelper.RightShift(Other: TGocciaValue): TGocciaValue;
  var
    LeftInt, RightInt: Integer;
  begin
    LeftInt := Trunc(Self.ToNumberLiteral.Value);
    RightInt := Trunc(Other.ToNumberLiteral.Value) and $1F; // Only use lower 5 bits
    Result := TGocciaNumberLiteralValue.Create(LeftInt shr RightInt);
  end;

  function TGocciaValueHelper.UnsignedRightShift(Other: TGocciaValue): TGocciaValue;
  var
    LeftInt, RightInt: Cardinal;
  begin
    LeftInt := Cardinal(Trunc(Self.ToNumberLiteral.Value));
    RightInt := Cardinal(Trunc(Other.ToNumberLiteral.Value)) and $1F; // Only use lower 5 bits
    Result := TGocciaNumberLiteralValue.Create(LeftInt shr RightInt);
  end;

  function TGocciaValueHelper.BitwiseNot: TGocciaValue;
  var
    IntValue: Integer;
  begin
    IntValue := Trunc(Self.ToNumberLiteral.Value);
    Result := TGocciaNumberLiteralValue.Create(not IntValue);
  end;

  function TGocciaValueHelper.IsEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
  begin
    // Strict equality comparison
    if (Self is TGocciaUndefinedLiteralValue) and (Other is TGocciaUndefinedLiteralValue) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else if (Self is TGocciaNullLiteralValue) and (Other is TGocciaNullLiteralValue) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else if (Self is TGocciaBooleanLiteralValue) and (Other is TGocciaBooleanLiteralValue) then
      if TGocciaBooleanLiteralValue(Self).Value = TGocciaBooleanLiteralValue(Other).Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue
    else if (Self is TGocciaNumberLiteralValue) and (Other is TGocciaNumberLiteralValue) then
    begin
      // Handle NaN case
      if TGocciaNumberLiteralValue(Self).IsNaN or TGocciaNumberLiteralValue(Other).IsNaN then
        Result := TGocciaBooleanLiteralValue.FalseValue
      else
        if TGocciaNumberLiteralValue(Self).Value = TGocciaNumberLiteralValue(Other).Value then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else if (Self is TGocciaStringLiteralValue) and (Other is TGocciaStringLiteralValue) then
      if TGocciaStringLiteralValue(Self).Value = TGocciaStringLiteralValue(Other).Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue
    else
      if Self = Other then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue; // Reference equality
  end;

  function TGocciaValueHelper.IsNotEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    EqualResult: TGocciaBooleanLiteralValue;
  begin
    EqualResult := IsEqual(Other);
    Result := TGocciaBooleanLiteralValue.Create(not EqualResult.Value);
  end;

  function TGocciaValueHelper.IsLessThan(Other: TGocciaValue): TGocciaBooleanLiteralValue;
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

      if (LeftNum.IsNaN or RightNum.IsNaN) then
        Result := TGocciaBooleanLiteralValue.FalseValue
      else if LeftNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // +Infinity is not less than anything
      else if LeftNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.Create(not (RightNum.IsNegativeInfinity))  // -Infinity < anything except -Infinity
      else if RightNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.TrueValue  // Anything < +Infinity
      else if RightNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // Nothing < -Infinity
      else
        Result := TGocciaBooleanLiteralValue.Create(LeftNum.Value < RightNum.Value);
    end;
  end;

  function TGocciaValueHelper.IsGreaterThan(Other: TGocciaValue): TGocciaBooleanLiteralValue;
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

      if (LeftNum.IsNaN or RightNum.IsNaN) then
        Result := TGocciaBooleanLiteralValue.FalseValue
      else if LeftNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.Create(not (RightNum.IsInfinity))  // +Infinity > anything except +Infinity
      else if LeftNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // -Infinity is not greater than anything
      else if RightNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // Nothing > +Infinity
      else if RightNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.TrueValue  // Anything > -Infinity
      else
        Result := TGocciaBooleanLiteralValue.Create(LeftNum.Value > RightNum.Value);
    end;
  end;

  function TGocciaValueHelper.IsLessThanOrEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    LessResult, EqualResult: TGocciaBooleanLiteralValue;
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    // For numeric comparisons, check NaN first
    if (Self is TGocciaNumberLiteralValue) and (Other is TGocciaNumberLiteralValue) then
    begin
      LeftNum := TGocciaNumberLiteralValue(Self);
      RightNum := TGocciaNumberLiteralValue(Other);
      if LeftNum.IsNaN or RightNum.IsNaN then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
    
    LessResult := IsLessThan(Other);
    EqualResult := IsEqual(Other);
    Result := TGocciaBooleanLiteralValue.Create(LessResult.Value or EqualResult.Value);
  end;

  function TGocciaValueHelper.IsGreaterThanOrEqual(Other: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    GreaterResult, EqualResult: TGocciaBooleanLiteralValue;
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    // For numeric comparisons, check NaN first
    if (Self is TGocciaNumberLiteralValue) and (Other is TGocciaNumberLiteralValue) then
    begin
      LeftNum := TGocciaNumberLiteralValue(Self);
      RightNum := TGocciaNumberLiteralValue(Other);
      if LeftNum.IsNaN or RightNum.IsNaN then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
    
    GreaterResult := IsGreaterThan(Other);
    EqualResult := IsEqual(Other);
    Result := TGocciaBooleanLiteralValue.Create(GreaterResult.Value or EqualResult.Value);
  end;

end.
