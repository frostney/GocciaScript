unit Goccia.Values.TypeCoercion;

{$I Goccia.inc}

interface

uses
  SysUtils, Classes, Goccia.Values.Core, Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.BooleanObjectValue, Goccia.Values.NumberObjectValue, Goccia.Values.StringObjectValue, Goccia.Values.Interfaces, Goccia.Values.Constants;

type

  TGocciaTypeCoercion = class helper for TGocciaValue
  public
    function ToBooleanLiteral: TGocciaBooleanLiteralValue;
    function ToNumberLiteral: TGocciaNumberLiteralValue;
    function ToStringLiteral: TGocciaStringLiteralValue;

    function ToStringObject: TGocciaStringObjectValue;
    function ToBooleanObject: TGocciaBooleanObjectValue;
    function ToNumberObject: TGocciaNumberObjectValue;
  end;


implementation

  function TGocciaTypeCoercion.ToBooleanLiteral: TGocciaBooleanLiteralValue;
  var
    NumberValue: TGocciaNumberLiteralValue;
  begin
    // Copies value
    if Self is TGocciaBooleanLiteralValue then
    begin
      Result := TGocciaBooleanLiteralValue.Create((Self as TGocciaBooleanLiteralValue).Value);
      Exit;
    end;

    if Self is TGocciaNumberLiteralValue then
    begin
      NumberValue := (Self as TGocciaNumberLiteralValue);

      if NumberValue.IsNaN then
        Result := TGocciaBooleanLiteralValue.Create(False)
      else if NumberValue.IsInfinity then
        Result := TGocciaBooleanLiteralValue.Create(True)
      else if NumberValue.IsNegativeInfinity then
        Result := TGocciaBooleanLiteralValue.Create(False)
      else
        Result := TGocciaBooleanLiteralValue.Create(NumberValue.Value > ZERO_VALUE);
      Exit;
    end;

    if Self is TGocciaStringLiteralValue then
    begin
      Result := TGocciaBooleanLiteralValue.Create((Self as TGocciaStringLiteralValue).Value <> EMPTY_STRING);
      Exit;
    end;

    if Self is TGocciaNullLiteralValue then
    begin
      Result := TGocciaBooleanLiteralValue.Create(False);
      Exit;
    end;

    if Self is TGocciaUndefinedLiteralValue then
    begin
      Result := TGocciaBooleanLiteralValue.Create(False);
      Exit;
    end;

    Result := TGocciaBooleanLiteralValue.Create(False);
  end;

  function TGocciaTypeCoercion.ToNumberLiteral: TGocciaNumberLiteralValue;
  var
    NumberValue: TGocciaNumberLiteralValue;
  begin
    if Self is TGocciaNumberLiteralValue then
    begin
      NumberValue := (Self as TGocciaNumberLiteralValue);

      if NumberValue.IsNaN then
        Result := TGocciaNumberLiteralValue.NaNValue
      else if NumberValue.IsInfinity then
        Result := TGocciaNumberLiteralValue.InfinityValue
      else if NumberValue.IsNegativeInfinity then
        Result := TGocciaNumberLiteralValue.NegativeInfinityValue
      else
        Result := TGocciaNumberLiteralValue.Create(NumberValue.Value);
      Exit;
    end;

    if Self is TGocciaBooleanLiteralValue then
    begin
      if (Self as TGocciaBooleanLiteralValue).Value then
        Result := TGocciaNumberLiteralValue.OneValue
      else
        Result := TGocciaNumberLiteralValue.ZeroValue;
      Exit;
    end;

    if Self is TGocciaStringLiteralValue then
    begin
      Result := TGocciaNumberLiteralValue.Create(StrToFloat((Self as TGocciaStringLiteralValue).Value));
      Exit;
    end;

    if Self is TGocciaNullLiteralValue then
    begin
      Result := TGocciaNumberLiteralValue.Create(ZERO_VALUE);
      Exit;
    end;

    if Self is TGocciaUndefinedLiteralValue then
    begin
      Result := TGocciaNumberLiteralValue.Create(ZERO_VALUE);
      Exit;
    end;

    if Self is TGocciaObjectValue then
    begin
      if Self is TGocciaStringObjectValue then
      begin
        Result := TGocciaNumberLiteralValue.Create(StrToFloat((Self as TGocciaStringObjectValue).Primitive.Value));
        Exit;
      end;

      if Self is TGocciaBooleanObjectValue then
      begin
        if (Self as TGocciaBooleanObjectValue).Primitive.Value then
          Result := TGocciaNumberLiteralValue.OneValue
        else
          Result := TGocciaNumberLiteralValue.ZeroValue;
        Exit;
      end;

      if Self is TGocciaNumberObjectValue then
      begin
        Result := (Self as TGocciaNumberObjectValue).Primitive;
        Exit;
      end;

      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    Result := TGocciaNumberLiteralValue.NaNValue;
  end;

  function TGocciaTypeCoercion.ToStringLiteral: TGocciaStringLiteralValue;
  var NumberValue: TGocciaNumberLiteralValue;
  begin
    if Self is TGocciaStringLiteralValue then
    begin
      Result := TGocciaStringLiteralValue.Create((Self as TGocciaStringLiteralValue).Value);
      Exit;
    end;

    if Self is TGocciaNumberLiteralValue then
    begin
      NumberValue := (Self as TGocciaNumberLiteralValue);

      if NumberValue.IsNaN then
        Result := TGocciaStringLiteralValue.Create(NAN_LITERAL)
      else if NumberValue.IsInfinity then
        Result := TGocciaStringLiteralValue.Create(INFINITY_LITERAL)
      else if NumberValue.IsNegativeInfinity then
        Result := TGocciaStringLiteralValue.Create(NEGATIVE_INFINITY_LITERAL)
      else
        Result := TGocciaStringLiteralValue.Create(FloatToStr(NumberValue.Value));
      Exit;
    end;

    if Self is TGocciaBooleanLiteralValue then
    begin
      if (Self as TGocciaBooleanLiteralValue).Value then
        Result := TGocciaStringLiteralValue.Create(BOOLEAN_TRUE_LITERAL)
      else
        Result := TGocciaStringLiteralValue.Create(BOOLEAN_FALSE_LITERAL);
      Exit;
    end;

    if Self is TGocciaNullLiteralValue then
    begin
      Result := TGocciaStringLiteralValue.Create(NULL_LITERAL);
      Exit;
    end;

    if Self is TGocciaUndefinedLiteralValue then
    begin
      Result := TGocciaStringLiteralValue.Create(UNDEFINED_LITERAL);
      Exit;
    end;

    if Self is TGocciaObjectValue then
    begin
      if Self is TGocciaStringObjectValue then
      begin
        Result := TGocciaStringLiteralValue.Create((Self as TGocciaStringObjectValue).Primitive.Value);
        Exit;
      end;

      if Self is TGocciaBooleanObjectValue then
      begin
        Result := TGocciaStringLiteralValue.Create(BOOLEAN_TRUE_LITERAL);
        Exit;
      end;

      if Self is TGocciaNumberObjectValue then
      begin
        Result := TGocciaStringLiteralValue.Create(FloatToStr((Self as TGocciaNumberObjectValue).Primitive.Value));
        Exit;
      end;

      Result := TGocciaStringLiteralValue.Create('[' + (Self as TGocciaObjectValue).TypeName + ' ' + (Self as TGocciaObjectValue).ToStringTag + ']');
      Exit;
    end;

    Result := TGocciaStringLiteralValue.Create(EMPTY_STRING);
  end;

  function TGocciaTypeCoercion.ToNumberObject: TGocciaNumberObjectValue;
  begin
    Result := TGocciaNumberObjectValue.Create(Self.ToNumberLiteral);
  end;

  function TGocciaTypeCoercion.ToStringObject: TGocciaStringObjectValue;
  begin
    Result := TGocciaStringObjectValue.Create(Self.ToStringLiteral);
  end;

  function TGocciaTypeCoercion.ToBooleanObject: TGocciaBooleanObjectValue;
  begin
    Result := TGocciaBooleanObjectValue.Create(Self.ToBooleanLiteral);
  end;

end.