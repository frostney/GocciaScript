unit Goccia.Float16;

{$I Goccia.inc}

interface

// ES2026 §5.2.5 Mathematical operations — IEEE 754-2019 binary16 (half-precision)
function DoubleToFloat16(const AValue: Double): Word;
function Float16ToDouble(const AValue: Word): Double;

implementation

uses
  Math;

const
  FLOAT16_SIGN_BIT          = Word($8000);
  FLOAT16_EXPONENT_MASK     = Word($7C00);
  FLOAT16_MANTISSA_MASK     = Word($03FF);
  FLOAT16_INFINITY          = Word($7C00);
  FLOAT16_NAN               = Word($7E00);
  FLOAT16_MANTISSA_BITS     = 10;
  FLOAT16_EXPONENT_BIAS     = 15;
  FLOAT16_MAX_EXPONENT      = 15;
  FLOAT16_MIN_EXPONENT      = -14;

  FLOAT64_EXPONENT_BIAS     = 1023;
  FLOAT64_MANTISSA_BITS     = 52;
  FLOAT64_EXPONENT_MASK     = Int64($7FF0000000000000);
  FLOAT64_MANTISSA_MASK     = Int64($000FFFFFFFFFFFFF);
  FLOAT64_SIGN_BIT          = Int64($8000000000000000);

  MANTISSA_SHIFT            = FLOAT64_MANTISSA_BITS - FLOAT16_MANTISSA_BITS; // 42

// ES2026 §25.1.2.12 SetValueInBuffer — Float16 serialization
function DoubleToFloat16(const AValue: Double): Word;
var
  Bits: Int64 absolute AValue;
  Sign: Word;
  Exponent: Integer;
  Mantissa: Int64;
  ShiftAmount: Integer;
  RoundBit, StickyBits: Int64;
begin
  Sign := Word((Bits shr 48) and $8000);
  Exponent := Integer((Bits shr FLOAT64_MANTISSA_BITS) and $7FF) - FLOAT64_EXPONENT_BIAS;
  Mantissa := Bits and FLOAT64_MANTISSA_MASK;

  // NaN
  if Exponent = (FLOAT64_EXPONENT_BIAS + 1) then
  begin
    if Mantissa <> 0 then
      Result := Sign or FLOAT16_NAN
    else
      Result := Sign or FLOAT16_INFINITY;
    Exit;
  end;

  // Add implicit leading 1 for normalized doubles
  if Exponent > -FLOAT64_EXPONENT_BIAS then
    Mantissa := Mantissa or (Int64(1) shl FLOAT64_MANTISSA_BITS)
  else
  begin
    // Subnormal double — renormalize
    Exponent := -FLOAT64_EXPONENT_BIAS + 1;
    // Mantissa has no implicit 1
  end;

  // Overflow — too large for float16
  if Exponent > FLOAT16_MAX_EXPONENT then
  begin
    Result := Sign or FLOAT16_INFINITY;
    Exit;
  end;

  // Rebias for float16
  Exponent := Exponent + FLOAT16_EXPONENT_BIAS;

  if Exponent <= 0 then
  begin
    // Subnormal float16 — shift mantissa right by additional (1 - exponent) bits
    ShiftAmount := MANTISSA_SHIFT + (1 - Exponent);
    if ShiftAmount >= 64 then
    begin
      // Too small — rounds to zero
      Result := Sign;
      Exit;
    end;

    // Extract round bit and sticky bits, then shift
    RoundBit := (Mantissa shr (ShiftAmount - 1)) and 1;
    if ShiftAmount > 1 then
      StickyBits := Mantissa and ((Int64(1) shl (ShiftAmount - 1)) - 1)
    else
      StickyBits := 0;
    Mantissa := Mantissa shr ShiftAmount;

    // Round to nearest even
    if (RoundBit <> 0) and ((StickyBits <> 0) or ((Mantissa and 1) <> 0)) then
      Mantissa := Mantissa + 1;

    Result := Sign or Word(Mantissa);
    Exit;
  end;

  // Normal float16
  RoundBit := (Mantissa shr (MANTISSA_SHIFT - 1)) and 1;
  StickyBits := Mantissa and ((Int64(1) shl (MANTISSA_SHIFT - 1)) - 1);
  Mantissa := Mantissa shr MANTISSA_SHIFT;

  // Remove implicit leading 1
  Mantissa := Mantissa and FLOAT16_MANTISSA_MASK;

  // Round to nearest even
  if (RoundBit <> 0) and ((StickyBits <> 0) or ((Mantissa and 1) <> 0)) then
  begin
    Mantissa := Mantissa + 1;
    if Mantissa > FLOAT16_MANTISSA_MASK then
    begin
      Mantissa := 0;
      Exponent := Exponent + 1;
      if Exponent > 2 * FLOAT16_EXPONENT_BIAS then
      begin
        Result := Sign or FLOAT16_INFINITY;
        Exit;
      end;
    end;
  end;

  Result := Sign or Word(Exponent shl FLOAT16_MANTISSA_BITS) or Word(Mantissa);
end;

// ES2026 §25.1.2.11 GetValueFromBuffer — Float16 deserialization
function Float16ToDouble(const AValue: Word): Double;
var
  Sign: Integer;
  Exponent: Integer;
  Mantissa: Integer;
begin
  Sign := (AValue shr 15) and 1;
  Exponent := (AValue shr FLOAT16_MANTISSA_BITS) and $1F;
  Mantissa := AValue and Integer(FLOAT16_MANTISSA_MASK);

  if Exponent = 31 then
  begin
    // Infinity or NaN
    if Mantissa = 0 then
    begin
      if Sign = 1 then
        Result := NegInfinity
      else
        Result := Infinity;
    end
    else
      Result := NaN;
    Exit;
  end;

  if Exponent = 0 then
  begin
    if Mantissa = 0 then
    begin
      // ±0
      if Sign = 1 then
        Result := -0.0
      else
        Result := 0.0;
    end
    else
    begin
      // Subnormal: value = (-1)^sign * 2^(-14) * (mantissa / 1024)
      Result := Mantissa / 1024.0;
      Result := Result * Power(2, -14);
      if Sign = 1 then
        Result := -Result;
    end;
    Exit;
  end;

  // Normal: value = (-1)^sign * 2^(exponent - 15) * (1 + mantissa / 1024)
  Result := 1.0 + Mantissa / 1024.0;
  Result := Result * Power(2, Exponent - FLOAT16_EXPONENT_BIAS);
  if Sign = 1 then
    Result := -Result;
end;

end.
