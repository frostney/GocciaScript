unit Goccia.Values.ClassHelper;

{$I Goccia.inc}

interface

uses
  Goccia.Values.BigIntObjectValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.StringObjectValue;

type

  TGocciaValueHelper = class helper for TGocciaValue
  public
    function ToStringObject: TGocciaStringObjectValue;
    function ToBooleanObject: TGocciaBooleanObjectValue;
    function ToNumberObject: TGocciaNumberObjectValue;

    function Box: TGocciaObjectValue;

    function IsEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsNotEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsLessThan(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsGreaterThan(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsLessThanOrEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
    function IsGreaterThanOrEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  end;


implementation

uses
  Math,

  BigInteger,

  Goccia.Values.BigIntValue;

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

    if Self is TGocciaBigIntValue then
    begin
      Result := TGocciaBigIntObjectValue.Create(Self);
      Exit;
    end;

    Result := nil;
  end;
  function TGocciaValueHelper.IsEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  begin
    // Strict equality comparison
    if (Self is TGocciaUndefinedLiteralValue) and (AOther is TGocciaUndefinedLiteralValue) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else if (Self is TGocciaNullLiteralValue) and (AOther is TGocciaNullLiteralValue) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else if (Self is TGocciaBooleanLiteralValue) and (AOther is TGocciaBooleanLiteralValue) then
      if TGocciaBooleanLiteralValue(Self).Value = TGocciaBooleanLiteralValue(AOther).Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue
    else if (Self is TGocciaNumberLiteralValue) and (AOther is TGocciaNumberLiteralValue) then
    begin
      // Handle NaN case
      if TGocciaNumberLiteralValue(Self).IsNaN or TGocciaNumberLiteralValue(AOther).IsNaN then
        Result := TGocciaBooleanLiteralValue.FalseValue
      else
        if TGocciaNumberLiteralValue(Self).Value = TGocciaNumberLiteralValue(AOther).Value then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else if (Self is TGocciaStringLiteralValue) and (AOther is TGocciaStringLiteralValue) then
      if TGocciaStringLiteralValue(Self).Value = TGocciaStringLiteralValue(AOther).Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue
    // ES2026 §7.2.16 BigInt strict equality
    else if (Self is TGocciaBigIntValue) and (AOther is TGocciaBigIntValue) then
      if TGocciaBigIntValue(Self).Value.Equal(TGocciaBigIntValue(AOther).Value) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue
    else
      if Self = AOther then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue; // Reference equality
  end;

  function TGocciaValueHelper.IsNotEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    EqualResult: TGocciaBooleanLiteralValue;
  begin
    EqualResult := IsEqual(AOther);
    if EqualResult.Value then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.TrueValue;
  end;

  function TGocciaValueHelper.IsLessThan(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
    LeftStr, RightStr: TGocciaStringLiteralValue;
  begin
    if (Self is TGocciaStringLiteralValue) and (AOther is TGocciaStringLiteralValue) then
    begin
      LeftStr := TGocciaStringLiteralValue(Self);
      RightStr := TGocciaStringLiteralValue(AOther);
      if LeftStr.Value < RightStr.Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
    begin
      LeftNum := Self.ToNumberLiteral;
      RightNum := AOther.ToNumberLiteral;

      if (LeftNum.IsNaN or RightNum.IsNaN) then
        Result := TGocciaBooleanLiteralValue.FalseValue
      else if LeftNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // +Infinity is not less than anything
      else if LeftNum.IsNegativeInfinity then
      begin
        if RightNum.IsNegativeInfinity then
          Result := TGocciaBooleanLiteralValue.FalseValue
        else
          Result := TGocciaBooleanLiteralValue.TrueValue;  // -Infinity < anything except -Infinity
      end
      else if RightNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.TrueValue  // Anything < +Infinity
      else if RightNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // Nothing < -Infinity
      else if LeftNum.Value < RightNum.Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  end;

  function TGocciaValueHelper.IsGreaterThan(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    LeftNum, RightNum: TGocciaNumberLiteralValue;
    LeftStr, RightStr: TGocciaStringLiteralValue;
  begin
    if (Self is TGocciaStringLiteralValue) and (AOther is TGocciaStringLiteralValue) then
    begin
      LeftStr := TGocciaStringLiteralValue(Self);
      RightStr := TGocciaStringLiteralValue(AOther);
      if LeftStr.Value > RightStr.Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
    begin
      LeftNum := Self.ToNumberLiteral;
      RightNum := AOther.ToNumberLiteral;

      if (LeftNum.IsNaN or RightNum.IsNaN) then
        Result := TGocciaBooleanLiteralValue.FalseValue
      else if LeftNum.IsInfinity then
      begin
        if RightNum.IsInfinity then
          Result := TGocciaBooleanLiteralValue.FalseValue
        else
          Result := TGocciaBooleanLiteralValue.TrueValue;  // +Infinity > anything except +Infinity
      end
      else if LeftNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // -Infinity is not greater than anything
      else if RightNum.IsInfinity then
        Result := TGocciaBooleanLiteralValue.FalseValue  // Nothing > +Infinity
      else if RightNum.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.TrueValue  // Anything > -Infinity
      else if LeftNum.Value > RightNum.Value then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  end;

  function TGocciaValueHelper.IsLessThanOrEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    LessResult, EqualResult: TGocciaBooleanLiteralValue;
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    // For numeric comparisons, check NaN first
    if (Self is TGocciaNumberLiteralValue) and (AOther is TGocciaNumberLiteralValue) then
    begin
      LeftNum := TGocciaNumberLiteralValue(Self);
      RightNum := TGocciaNumberLiteralValue(AOther);
      if LeftNum.IsNaN or RightNum.IsNaN then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
    
    LessResult := IsLessThan(AOther);
    EqualResult := IsEqual(AOther);
    if LessResult.Value or EqualResult.Value then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;

  function TGocciaValueHelper.IsGreaterThanOrEqual(const AOther: TGocciaValue): TGocciaBooleanLiteralValue;
  var
    GreaterResult, EqualResult: TGocciaBooleanLiteralValue;
    LeftNum, RightNum: TGocciaNumberLiteralValue;
  begin
    // For numeric comparisons, check NaN first
    if (Self is TGocciaNumberLiteralValue) and (AOther is TGocciaNumberLiteralValue) then
    begin
      LeftNum := TGocciaNumberLiteralValue(Self);
      RightNum := TGocciaNumberLiteralValue(AOther);
      if LeftNum.IsNaN or RightNum.IsNaN then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
    
    GreaterResult := IsGreaterThan(AOther);
    EqualResult := IsEqual(AOther);
    if GreaterResult.Value or EqualResult.Value then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;

end.
