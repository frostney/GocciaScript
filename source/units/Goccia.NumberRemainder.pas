unit Goccia.NumberRemainder;

{$I Goccia.inc}

interface

function NumberRemainder(const ANumerator, ADenominator: Double): Double;

implementation

uses
  NumberBits;

const
  SIGN_BIT: UInt64 = UInt64(1) shl 63;
  EXPONENT_MASK: UInt64 = $7FF;
  INFINITY_BITS_SHIFTED: UInt64 = UInt64($7FF) shl 53;
  FRACTION_MASK: UInt64 = $000FFFFFFFFFFFFF;
  HIDDEN_BIT: UInt64 = $0010000000000000;

{ Copyright (C) 2005-2020 Rich Felker, et al.

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.

  Bit-level port of musl libc src/math/fmod.c, blob
  6849722bac50e477b9210f57f0271dcdabd8ae0c. }

// ES2026 §6.1.6.1.6 Number::remainder(n, d)
function NumberRemainder(const ANumerator, ADenominator: Double): Double;
var
  NumeratorBits, DenominatorBits, ShiftedBits: UInt64;
  NumeratorExponent, DenominatorExponent: Integer;
  NumeratorSign: UInt64;
begin
  NumeratorBits := DoubleToBits(ANumerator);
  DenominatorBits := DoubleToBits(ADenominator);
  NumeratorExponent := Integer((NumeratorBits shr 52) and EXPONENT_MASK);
  DenominatorExponent := Integer((DenominatorBits shr 52) and EXPONENT_MASK);
  NumeratorSign := NumeratorBits and SIGN_BIT;

  // ES2026 §6.1.6.1.6 steps 1-3: invalid operands produce NaN.
  if ((DenominatorBits shl 1) = 0) or
     (NumeratorExponent = Integer(EXPONENT_MASK)) or
     ((DenominatorBits shl 1) > INFINITY_BITS_SHIFTED) then
    Exit(BitsToDouble(CANONICAL_FLOAT64_NAN_BITS));

  // ES2026 §6.1.6.1.6 steps 4-6: preserve n or its signed zero.
  if (NumeratorBits shl 1) <= (DenominatorBits shl 1) then
  begin
    if (NumeratorBits shl 1) = (DenominatorBits shl 1) then
      Exit(BitsToDouble(NumeratorSign));
    Exit(ANumerator);
  end;

  if NumeratorExponent = 0 then
  begin
    ShiftedBits := NumeratorBits shl 12;
    while (ShiftedBits shr 63) = 0 do
    begin
      Dec(NumeratorExponent);
      ShiftedBits := ShiftedBits shl 1;
    end;
    NumeratorBits := NumeratorBits shl (1 - NumeratorExponent);
  end
  else
    NumeratorBits := (NumeratorBits and FRACTION_MASK) or HIDDEN_BIT;

  if DenominatorExponent = 0 then
  begin
    ShiftedBits := DenominatorBits shl 12;
    while (ShiftedBits shr 63) = 0 do
    begin
      Dec(DenominatorExponent);
      ShiftedBits := ShiftedBits shl 1;
    end;
    DenominatorBits := DenominatorBits shl (1 - DenominatorExponent);
  end
  else
    DenominatorBits := (DenominatorBits and FRACTION_MASK) or HIDDEN_BIT;

  while NumeratorExponent > DenominatorExponent do
  begin
    if NumeratorBits >= DenominatorBits then
    begin
      if NumeratorBits = DenominatorBits then
        Exit(BitsToDouble(NumeratorSign));
      NumeratorBits := NumeratorBits - DenominatorBits;
    end;
    NumeratorBits := NumeratorBits shl 1;
    Dec(NumeratorExponent);
  end;

  if NumeratorBits >= DenominatorBits then
  begin
    if NumeratorBits = DenominatorBits then
      Exit(BitsToDouble(NumeratorSign));
    NumeratorBits := NumeratorBits - DenominatorBits;
  end;

  while (NumeratorBits shr 52) = 0 do
  begin
    NumeratorBits := NumeratorBits shl 1;
    Dec(NumeratorExponent);
  end;

  if NumeratorExponent > 0 then
    NumeratorBits := (NumeratorBits - HIDDEN_BIT) or
      (UInt64(NumeratorExponent) shl 52)
  else
    NumeratorBits := NumeratorBits shr (1 - NumeratorExponent);

  Result := BitsToDouble(NumeratorBits or NumeratorSign);
end;

end.
