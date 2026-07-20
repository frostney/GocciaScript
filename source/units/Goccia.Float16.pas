unit Goccia.Float16;

{$I Goccia.inc}

interface

// ES2026 §5.2.5 Mathematical operations — IEEE 754-2019 binary16 (half-precision)
function DoubleToFloat16(const AValue: Double): Word;
function Float16ToDouble(const AValue: Word): Double;

implementation

uses
  NumberBits;

const
  FLOAT16_SIGN_BIT          = Word($8000);
  FLOAT16_EXPONENT_MASK     = Word($7C00);
  FLOAT16_MANTISSA_MASK     = Word($03FF);
  FLOAT16_INFINITY          = Word($7C00);
  FLOAT16_MANTISSA_BITS     = 10;
  FLOAT16_EXPONENT_BIAS     = 15;
  FLOAT16_MAX_EXPONENT      = 15;
  FLOAT16_MIN_EXPONENT      = -14;

  FLOAT64_EXPONENT_BIAS     = 1023;
  FLOAT64_MANTISSA_BITS     = 52;
  FLOAT64_MANTISSA_MASK     = UInt64($000FFFFFFFFFFFFF);

  MANTISSA_SHIFT            = FLOAT64_MANTISSA_BITS - FLOAT16_MANTISSA_BITS; // 42

// ES2026 §25.1.2.12 SetValueInBuffer — Float16 serialization
function DoubleToFloat16(const AValue: Double): Word;
var
  Bits: UInt64;
  Sign: Word;
  Exponent: Integer;
  Mantissa: UInt64;
  ShiftAmount: Integer;
  RoundBit, StickyBits: UInt64;
begin
  Bits := DoubleToBits(AValue);
  Sign := Word((Bits shr 48) and $8000);
  Exponent := Integer((Bits shr FLOAT64_MANTISSA_BITS) and $7FF) - FLOAT64_EXPONENT_BIAS;
  Mantissa := Bits and FLOAT64_MANTISSA_MASK;

  // NaN
  if Exponent = (FLOAT64_EXPONENT_BIAS + 1) then
  begin
    if Mantissa <> 0 then
      Result := CANONICAL_FLOAT16_NAN_BITS
    else
      Result := Sign or FLOAT16_INFINITY;
    Exit;
  end;

  // Add implicit leading 1 for normalized doubles
  if Exponent > -FLOAT64_EXPONENT_BIAS then
    Mantissa := Mantissa or (UInt64(1) shl FLOAT64_MANTISSA_BITS)
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
      StickyBits := Mantissa and ((UInt64(1) shl (ShiftAmount - 1)) - 1)
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
  StickyBits := Mantissa and ((UInt64(1) shl (MANTISSA_SHIFT - 1)) - 1);
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
  SignBits: UInt64;
  Exponent: Integer;
  Mantissa: Integer;
  LeadingBit: Integer;
  DoubleExponent: Integer;
  DoubleMantissa: UInt64;
  Bits: UInt64;
begin
  SignBits := UInt64(AValue and FLOAT16_SIGN_BIT) shl 48;
  Exponent := (AValue shr FLOAT16_MANTISSA_BITS) and $1F;
  Mantissa := AValue and Integer(FLOAT16_MANTISSA_MASK);

  if Exponent = 31 then
  begin
    if Mantissa = 0 then
      Result := BitsToDouble(SignBits or UInt64($7FF0000000000000))
    else
      Result := BitsToDouble(CANONICAL_FLOAT64_NAN_BITS);
    Exit;
  end;

  if Exponent = 0 then
  begin
    if Mantissa = 0 then
      Result := BitsToDouble(SignBits)
    else
    begin
      LeadingBit := 9;
      while (Mantissa and (1 shl LeadingBit)) = 0 do
        Dec(LeadingBit);
      DoubleExponent := LeadingBit - 24 + FLOAT64_EXPONENT_BIAS;
      DoubleMantissa := UInt64(Mantissa - (1 shl LeadingBit))
        shl (FLOAT64_MANTISSA_BITS - LeadingBit);
      Bits := SignBits or (UInt64(DoubleExponent) shl FLOAT64_MANTISSA_BITS)
        or DoubleMantissa;
      Result := BitsToDouble(Bits);
    end;
    Exit;
  end;

  DoubleExponent := Exponent - FLOAT16_EXPONENT_BIAS + FLOAT64_EXPONENT_BIAS;
  Bits := SignBits or (UInt64(DoubleExponent) shl FLOAT64_MANTISSA_BITS)
    or (UInt64(Mantissa) shl MANTISSA_SHIFT);
  Result := BitsToDouble(Bits);
end;

end.
