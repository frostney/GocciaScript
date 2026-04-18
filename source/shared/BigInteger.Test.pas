program BigInteger.Test;

{$I Shared.inc}

uses
  Math,
  SysUtils,

  BigInteger,
  TestingPascalLibrary;

type
  TBigIntegerTests = class(TTestSuite)
  private
    { Constructors — Zero, One, NegativeOne }
    procedure TestZeroIsZero;
    procedure TestZeroIsNotNegative;
    procedure TestOneIsOne;
    procedure TestOneIsPositive;
    procedure TestNegativeOneIsMinusOne;
    procedure TestNegativeOneIsNegative;

    { FromInt64 }
    procedure TestFromInt64Zero;
    procedure TestFromInt64Positive;
    procedure TestFromInt64Negative;
    procedure TestFromInt64MaxInt;
    procedure TestFromInt64MinInt;
    procedure TestFromInt64LargePositive;
    procedure TestFromInt64LargeNegative;

    { FromDouble }
    procedure TestFromDoubleZero;
    procedure TestFromDoublePositive;
    procedure TestFromDoubleNegative;
    procedure TestFromDoubleLargeValue;
    procedure TestFromDoubleNaN;
    procedure TestFromDoubleInfinity;
    procedure TestFromDoubleNonInteger;
    procedure TestFromDoubleMaxSafeInteger;
    procedure TestFromDoubleNegMaxSafeInteger;

    { FromDecimalString }
    procedure TestFromDecimalStringZero;
    procedure TestFromDecimalStringPositive;
    procedure TestFromDecimalStringNegative;
    procedure TestFromDecimalStringLeadingPlus;
    procedure TestFromDecimalStringLargeNumber;
    procedure TestFromDecimalStringEmpty;
    procedure TestFromDecimalStringInvalidChar;
    procedure TestFromDecimalStringSignOnly;

    { FromHexString }
    procedure TestFromHexStringSmall;
    procedure TestFromHexStringUpperCase;
    procedure TestFromHexStringLarge;
    procedure TestFromHexStringEmpty;
    procedure TestFromHexStringInvalidChar;

    { FromBinaryString }
    procedure TestFromBinaryStringSmall;
    procedure TestFromBinaryStringByte;
    procedure TestFromBinaryStringEmpty;
    procedure TestFromBinaryStringInvalidChar;

    { FromOctalString }
    procedure TestFromOctalStringSmall;
    procedure TestFromOctalStringLarger;
    procedure TestFromOctalStringEmpty;
    procedure TestFromOctalStringInvalidChar;

    { Predicates }
    procedure TestIsZeroTrue;
    procedure TestIsZeroFalse;
    procedure TestIsNegativeTrue;
    procedure TestIsNegativeFalse;
    procedure TestIsPositiveTrue;
    procedure TestIsPositiveFalse;
    procedure TestIsOneTrue;
    procedure TestIsOneFalse;
    procedure TestIsMinusOneTrue;
    procedure TestIsMinusOneFalse;
    procedure TestBitLength;
    procedure TestBitLengthZero;
    procedure TestGetBit;

    { Conversions }
    procedure TestToDoubleZero;
    procedure TestToDoublePositive;
    procedure TestToDoubleNegative;
    procedure TestToInt64Zero;
    procedure TestToInt64Positive;
    procedure TestToInt64Negative;
    procedure TestToInt64Large;
    procedure TestToStringZero;
    procedure TestToStringPositive;
    procedure TestToStringNegative;
    procedure TestToRadixStringBinary;
    procedure TestToRadixStringHex;
    procedure TestToRadixStringOctal;
    procedure TestToRadixStringBase36;
    procedure TestToRadixStringInvalidRadix;

    { Unary operations }
    procedure TestNegatePositive;
    procedure TestNegateNegative;
    procedure TestNegateZero;
    procedure TestAbsValuePositive;
    procedure TestAbsValueNegative;
    procedure TestAbsValueZero;

    { Addition }
    procedure TestAddBothPositive;
    procedure TestAddBothNegative;
    procedure TestAddPositiveNegative;
    procedure TestAddNegativePositive;
    procedure TestAddZeroLeft;
    procedure TestAddZeroRight;
    procedure TestAddCancelToZero;
    procedure TestAddCarry;

    { Subtraction }
    procedure TestSubtractBasic;
    procedure TestSubtractNegativeResult;
    procedure TestSubtractFromZero;
    procedure TestSubtractToZero;

    { Multiplication }
    procedure TestMultiplyBasic;
    procedure TestMultiplyByZero;
    procedure TestMultiplyNegatives;
    procedure TestMultiplyMixedSign;
    procedure TestMultiplyLarge;

    { Division }
    procedure TestDivideExact;
    procedure TestDivideWithRemainder;
    procedure TestDivideNegative;
    procedure TestDivideTruncatesTowardZero;
    procedure TestDivideByZero;
    procedure TestDivideZeroByNonZero;
    procedure TestDivideSmallerByLarger;

    { Modulo }
    procedure TestModuloBasic;
    procedure TestModuloNegativeDividend;
    procedure TestModuloZeroDividend;
    procedure TestModuloByZero;
    procedure TestModuloSmallerByLarger;

    { Power }
    procedure TestPowerBasic;
    procedure TestPowerOfZero;
    procedure TestPowerToZero;
    procedure TestPowerNegativeExponent;
    procedure TestPowerLarge;

    { Bitwise AND }
    procedure TestBitwiseAndBasic;
    procedure TestBitwiseAndWithZero;
    procedure TestBitwiseAndNegative;

    { Bitwise OR }
    procedure TestBitwiseOrBasic;
    procedure TestBitwiseOrWithZero;
    procedure TestBitwiseOrNegative;

    { Bitwise XOR }
    procedure TestBitwiseXorBasic;
    procedure TestBitwiseXorSelf;
    procedure TestBitwiseXorNegative;

    { Bitwise NOT }
    procedure TestBitwiseNotPositive;
    procedure TestBitwiseNotNegative;
    procedure TestBitwiseNotZero;

    { Shift left }
    procedure TestShiftLeftBasic;
    procedure TestShiftLeftZeroCount;
    procedure TestShiftLeftZeroValue;
    procedure TestShiftLeftLarge;

    { Shift right }
    procedure TestShiftRightBasic;
    procedure TestShiftRightZeroCount;
    procedure TestShiftRightAllBits;
    procedure TestShiftRightNegativeFloors;

    { Comparison }
    procedure TestCompareEqual;
    procedure TestCompareLess;
    procedure TestCompareGreater;
    procedure TestCompareNegatives;
    procedure TestCompareMixedSign;
    procedure TestEqualTrue;
    procedure TestEqualFalse;

    { AsIntN / AsUintN }
    procedure TestAsIntN8;
    procedure TestAsIntN8Negative;
    procedure TestAsIntN8Overflow;
    procedure TestAsUintN8;
    procedure TestAsUintN8Large;
    procedure TestAsUintNZeroBits;
    procedure TestAsIntNZeroBits;

    { Roundtrip }
    procedure TestRoundtripFromInt64;
    procedure TestRoundtripDecimalString;
    procedure TestRoundtripDivModConsistency;
    procedure TestRoundtripFromDoubleToInt64;
  public
    procedure SetupTests; override;
  end;

procedure TBigIntegerTests.SetupTests;
begin
  { Constructors }
  Test('Zero.IsZero is True', TestZeroIsZero);
  Test('Zero is not negative', TestZeroIsNotNegative);
  Test('One.IsOne is True', TestOneIsOne);
  Test('One is positive', TestOneIsPositive);
  Test('NegativeOne.IsMinusOne is True', TestNegativeOneIsMinusOne);
  Test('NegativeOne is negative', TestNegativeOneIsNegative);

  { FromInt64 }
  Test('FromInt64(0) produces zero', TestFromInt64Zero);
  Test('FromInt64 positive value', TestFromInt64Positive);
  Test('FromInt64 negative value', TestFromInt64Negative);
  Test('FromInt64 max 32-bit', TestFromInt64MaxInt);
  Test('FromInt64 min Int64', TestFromInt64MinInt);
  Test('FromInt64 large positive (>32-bit)', TestFromInt64LargePositive);
  Test('FromInt64 large negative (>32-bit)', TestFromInt64LargeNegative);

  { FromDouble }
  Test('FromDouble(0.0) produces zero', TestFromDoubleZero);
  Test('FromDouble positive integer', TestFromDoublePositive);
  Test('FromDouble negative integer', TestFromDoubleNegative);
  Test('FromDouble large value (epoch ns)', TestFromDoubleLargeValue);
  Test('FromDouble NaN raises', TestFromDoubleNaN);
  Test('FromDouble Infinity raises', TestFromDoubleInfinity);
  Test('FromDouble non-integer raises', TestFromDoubleNonInteger);
  Test('FromDouble MAX_SAFE_INTEGER', TestFromDoubleMaxSafeInteger);
  Test('FromDouble -MAX_SAFE_INTEGER', TestFromDoubleNegMaxSafeInteger);

  { FromDecimalString }
  Test('FromDecimalString "0"', TestFromDecimalStringZero);
  Test('FromDecimalString positive', TestFromDecimalStringPositive);
  Test('FromDecimalString negative', TestFromDecimalStringNegative);
  Test('FromDecimalString leading plus', TestFromDecimalStringLeadingPlus);
  Test('FromDecimalString large number', TestFromDecimalStringLargeNumber);
  Test('FromDecimalString empty raises', TestFromDecimalStringEmpty);
  Test('FromDecimalString invalid char raises', TestFromDecimalStringInvalidChar);
  Test('FromDecimalString sign only raises', TestFromDecimalStringSignOnly);

  { FromHexString }
  Test('FromHexString small value', TestFromHexStringSmall);
  Test('FromHexString upper case', TestFromHexStringUpperCase);
  Test('FromHexString large value', TestFromHexStringLarge);
  Test('FromHexString empty raises', TestFromHexStringEmpty);
  Test('FromHexString invalid char raises', TestFromHexStringInvalidChar);

  { FromBinaryString }
  Test('FromBinaryString small', TestFromBinaryStringSmall);
  Test('FromBinaryString byte', TestFromBinaryStringByte);
  Test('FromBinaryString empty raises', TestFromBinaryStringEmpty);
  Test('FromBinaryString invalid char raises', TestFromBinaryStringInvalidChar);

  { FromOctalString }
  Test('FromOctalString small', TestFromOctalStringSmall);
  Test('FromOctalString larger', TestFromOctalStringLarger);
  Test('FromOctalString empty raises', TestFromOctalStringEmpty);
  Test('FromOctalString invalid char raises', TestFromOctalStringInvalidChar);

  { Predicates }
  Test('IsZero true for zero', TestIsZeroTrue);
  Test('IsZero false for non-zero', TestIsZeroFalse);
  Test('IsNegative true for negative', TestIsNegativeTrue);
  Test('IsNegative false for positive', TestIsNegativeFalse);
  Test('IsPositive true for positive', TestIsPositiveTrue);
  Test('IsPositive false for negative', TestIsPositiveFalse);
  Test('IsOne true for one', TestIsOneTrue);
  Test('IsOne false for other', TestIsOneFalse);
  Test('IsMinusOne true for -1', TestIsMinusOneTrue);
  Test('IsMinusOne false for other', TestIsMinusOneFalse);
  Test('BitLength of non-zero', TestBitLength);
  Test('BitLength of zero', TestBitLengthZero);
  Test('GetBit reads individual bits', TestGetBit);

  { Conversions }
  Test('ToDouble zero', TestToDoubleZero);
  Test('ToDouble positive', TestToDoublePositive);
  Test('ToDouble negative', TestToDoubleNegative);
  Test('ToInt64 zero', TestToInt64Zero);
  Test('ToInt64 positive', TestToInt64Positive);
  Test('ToInt64 negative', TestToInt64Negative);
  Test('ToInt64 large', TestToInt64Large);
  Test('ToString zero', TestToStringZero);
  Test('ToString positive', TestToStringPositive);
  Test('ToString negative', TestToStringNegative);
  Test('ToRadixString binary', TestToRadixStringBinary);
  Test('ToRadixString hex', TestToRadixStringHex);
  Test('ToRadixString octal', TestToRadixStringOctal);
  Test('ToRadixString base 36', TestToRadixStringBase36);
  Test('ToRadixString invalid radix raises', TestToRadixStringInvalidRadix);

  { Unary }
  Test('Negate positive', TestNegatePositive);
  Test('Negate negative', TestNegateNegative);
  Test('Negate zero', TestNegateZero);
  Test('AbsValue positive', TestAbsValuePositive);
  Test('AbsValue negative', TestAbsValueNegative);
  Test('AbsValue zero', TestAbsValueZero);

  { Addition }
  Test('Add both positive', TestAddBothPositive);
  Test('Add both negative', TestAddBothNegative);
  Test('Add positive + negative', TestAddPositiveNegative);
  Test('Add negative + positive', TestAddNegativePositive);
  Test('Add zero + x', TestAddZeroLeft);
  Test('Add x + zero', TestAddZeroRight);
  Test('Add cancels to zero', TestAddCancelToZero);
  Test('Add with carry', TestAddCarry);

  { Subtraction }
  Test('Subtract basic', TestSubtractBasic);
  Test('Subtract yields negative', TestSubtractNegativeResult);
  Test('Subtract from zero', TestSubtractFromZero);
  Test('Subtract to zero', TestSubtractToZero);

  { Multiplication }
  Test('Multiply basic', TestMultiplyBasic);
  Test('Multiply by zero', TestMultiplyByZero);
  Test('Multiply two negatives', TestMultiplyNegatives);
  Test('Multiply mixed signs', TestMultiplyMixedSign);
  Test('Multiply large values', TestMultiplyLarge);

  { Division }
  Test('Divide exact', TestDivideExact);
  Test('Divide with remainder (truncated)', TestDivideWithRemainder);
  Test('Divide negative values', TestDivideNegative);
  Test('Divide truncates toward zero', TestDivideTruncatesTowardZero);
  Test('Divide by zero raises', TestDivideByZero);
  Test('Divide zero by non-zero', TestDivideZeroByNonZero);
  Test('Divide smaller by larger yields zero', TestDivideSmallerByLarger);

  { Modulo }
  Test('Modulo basic', TestModuloBasic);
  Test('Modulo sign follows dividend', TestModuloNegativeDividend);
  Test('Modulo of zero', TestModuloZeroDividend);
  Test('Modulo by zero raises', TestModuloByZero);
  Test('Modulo smaller by larger returns self', TestModuloSmallerByLarger);

  { Power }
  Test('Power basic', TestPowerBasic);
  Test('Power of zero', TestPowerOfZero);
  Test('Power to zero yields one', TestPowerToZero);
  Test('Power negative exponent raises', TestPowerNegativeExponent);
  Test('Power large exponent', TestPowerLarge);

  { Bitwise AND }
  Test('Bitwise AND basic', TestBitwiseAndBasic);
  Test('Bitwise AND with zero', TestBitwiseAndWithZero);
  Test('Bitwise AND negative (two''s complement)', TestBitwiseAndNegative);

  { Bitwise OR }
  Test('Bitwise OR basic', TestBitwiseOrBasic);
  Test('Bitwise OR with zero', TestBitwiseOrWithZero);
  Test('Bitwise OR negative', TestBitwiseOrNegative);

  { Bitwise XOR }
  Test('Bitwise XOR basic', TestBitwiseXorBasic);
  Test('Bitwise XOR self yields zero', TestBitwiseXorSelf);
  Test('Bitwise XOR negative', TestBitwiseXorNegative);

  { Bitwise NOT }
  Test('Bitwise NOT positive', TestBitwiseNotPositive);
  Test('Bitwise NOT negative', TestBitwiseNotNegative);
  Test('Bitwise NOT zero', TestBitwiseNotZero);

  { Shift left }
  Test('ShiftLeft basic', TestShiftLeftBasic);
  Test('ShiftLeft zero count', TestShiftLeftZeroCount);
  Test('ShiftLeft zero value', TestShiftLeftZeroValue);
  Test('ShiftLeft large count', TestShiftLeftLarge);

  { Shift right }
  Test('ShiftRight basic', TestShiftRightBasic);
  Test('ShiftRight zero count', TestShiftRightZeroCount);
  Test('ShiftRight all bits', TestShiftRightAllBits);
  Test('ShiftRight negative floors toward -inf', TestShiftRightNegativeFloors);

  { Comparison }
  Test('Compare equal', TestCompareEqual);
  Test('Compare less', TestCompareLess);
  Test('Compare greater', TestCompareGreater);
  Test('Compare negatives', TestCompareNegatives);
  Test('Compare mixed sign', TestCompareMixedSign);
  Test('Equal true', TestEqualTrue);
  Test('Equal false', TestEqualFalse);

  { AsIntN / AsUintN }
  Test('AsIntN(8) positive in range', TestAsIntN8);
  Test('AsIntN(8) negative value', TestAsIntN8Negative);
  Test('AsIntN(8) overflow wraps', TestAsIntN8Overflow);
  Test('AsUintN(8) basic', TestAsUintN8);
  Test('AsUintN(8) large value masks', TestAsUintN8Large);
  Test('AsUintN(0) yields zero', TestAsUintNZeroBits);
  Test('AsIntN(0) yields zero', TestAsIntNZeroBits);

  { Roundtrip }
  Test('Roundtrip FromInt64 -> ToInt64', TestRoundtripFromInt64);
  Test('Roundtrip FromDecimalString -> ToString', TestRoundtripDecimalString);
  Test('Roundtrip a = (a/b)*b + (a mod b)', TestRoundtripDivModConsistency);
  Test('Roundtrip FromDouble -> ToInt64 for large values', TestRoundtripFromDoubleToInt64);
end;

{ ── Constructors ─────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestZeroIsZero;
begin
  Expect<Boolean>(TBigInteger.Zero.IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestZeroIsNotNegative;
begin
  Expect<Boolean>(TBigInteger.Zero.IsNegative).ToBe(False);
end;

procedure TBigIntegerTests.TestOneIsOne;
begin
  Expect<Boolean>(TBigInteger.One.IsOne).ToBe(True);
end;

procedure TBigIntegerTests.TestOneIsPositive;
begin
  Expect<Boolean>(TBigInteger.One.IsPositive).ToBe(True);
end;

procedure TBigIntegerTests.TestNegativeOneIsMinusOne;
begin
  Expect<Boolean>(TBigInteger.NegativeOne.IsMinusOne).ToBe(True);
end;

procedure TBigIntegerTests.TestNegativeOneIsNegative;
begin
  Expect<Boolean>(TBigInteger.NegativeOne.IsNegative).ToBe(True);
end;

{ ── FromInt64 ────────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestFromInt64Zero;
begin
  Expect<Boolean>(TBigInteger.FromInt64(0).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestFromInt64Positive;
begin
  Expect<String>(TBigInteger.FromInt64(42).ToString).ToBe('42');
end;

procedure TBigIntegerTests.TestFromInt64Negative;
begin
  Expect<String>(TBigInteger.FromInt64(-42).ToString).ToBe('-42');
end;

procedure TBigIntegerTests.TestFromInt64MaxInt;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromInt64(2147483647); // MaxInt32
  Expect<Int64>(B.ToInt64).ToBe(2147483647);
end;

procedure TBigIntegerTests.TestFromInt64MinInt;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromInt64(Low(Int64));
  Expect<Boolean>(B.IsNegative).ToBe(True);
  Expect<String>(B.ToString).ToBe('-9223372036854775808');
end;

procedure TBigIntegerTests.TestFromInt64LargePositive;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromInt64(Int64(5000000000)); // > 2^32
  Expect<Int64>(B.ToInt64).ToBe(Int64(5000000000));
end;

procedure TBigIntegerTests.TestFromInt64LargeNegative;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromInt64(Int64(-5000000000));
  Expect<Int64>(B.ToInt64).ToBe(Int64(-5000000000));
end;

{ ── FromDouble ───────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestFromDoubleZero;
begin
  Expect<Boolean>(TBigInteger.FromDouble(0.0).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDoublePositive;
begin
  Expect<String>(TBigInteger.FromDouble(123.0).ToString).ToBe('123');
end;

procedure TBigIntegerTests.TestFromDoubleNegative;
begin
  Expect<String>(TBigInteger.FromDouble(-456.0).ToString).ToBe('-456');
end;

procedure TBigIntegerTests.TestFromDoubleLargeValue;
var
  B: TBigInteger;
begin
  // Epoch nanoseconds magnitude — the value that triggered the AArch64 bug
  B := TBigInteger.FromDouble(1718451045123.0);
  Expect<Int64>(B.ToInt64).ToBe(Int64(1718451045123));
end;

procedure TBigIntegerTests.TestFromDoubleNaN;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromDouble(NaN);
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDoubleInfinity;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromDouble(Infinity);
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDoubleNonInteger;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromDouble(1.5);
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDoubleMaxSafeInteger;
var
  B: TBigInteger;
begin
  // 2^53 = 9007199254740992
  B := TBigInteger.FromDouble(9007199254740992.0);
  Expect<String>(B.ToString).ToBe('9007199254740992');
end;

procedure TBigIntegerTests.TestFromDoubleNegMaxSafeInteger;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromDouble(-9007199254740992.0);
  Expect<String>(B.ToString).ToBe('-9007199254740992');
end;

{ ── FromDecimalString ────────────────────────────────────────────── }

procedure TBigIntegerTests.TestFromDecimalStringZero;
begin
  Expect<Boolean>(TBigInteger.FromDecimalString('0').IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDecimalStringPositive;
begin
  Expect<String>(TBigInteger.FromDecimalString('12345').ToString).ToBe('12345');
end;

procedure TBigIntegerTests.TestFromDecimalStringNegative;
begin
  Expect<String>(TBigInteger.FromDecimalString('-9999').ToString).ToBe('-9999');
end;

procedure TBigIntegerTests.TestFromDecimalStringLeadingPlus;
begin
  Expect<String>(TBigInteger.FromDecimalString('+42').ToString).ToBe('42');
end;

procedure TBigIntegerTests.TestFromDecimalStringLargeNumber;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromDecimalString('123456789012345678901234567890');
  Expect<String>(B.ToString).ToBe('123456789012345678901234567890');
end;

procedure TBigIntegerTests.TestFromDecimalStringEmpty;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromDecimalString('');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDecimalStringInvalidChar;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromDecimalString('12x34');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromDecimalStringSignOnly;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromDecimalString('-');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

{ ── FromHexString ────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestFromHexStringSmall;
begin
  Expect<String>(TBigInteger.FromHexString('ff').ToString).ToBe('255');
end;

procedure TBigIntegerTests.TestFromHexStringUpperCase;
begin
  Expect<String>(TBigInteger.FromHexString('FF').ToString).ToBe('255');
end;

procedure TBigIntegerTests.TestFromHexStringLarge;
begin
  Expect<String>(TBigInteger.FromHexString('100000000').ToString).ToBe('4294967296');
end;

procedure TBigIntegerTests.TestFromHexStringEmpty;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromHexString('');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromHexStringInvalidChar;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromHexString('GG');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

{ ── FromBinaryString ─────────────────────────────────────────────── }

procedure TBigIntegerTests.TestFromBinaryStringSmall;
begin
  Expect<String>(TBigInteger.FromBinaryString('1010').ToString).ToBe('10');
end;

procedure TBigIntegerTests.TestFromBinaryStringByte;
begin
  Expect<String>(TBigInteger.FromBinaryString('11111111').ToString).ToBe('255');
end;

procedure TBigIntegerTests.TestFromBinaryStringEmpty;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromBinaryString('');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromBinaryStringInvalidChar;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromBinaryString('102');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

{ ── FromOctalString ──────────────────────────────────────────────── }

procedure TBigIntegerTests.TestFromOctalStringSmall;
begin
  Expect<String>(TBigInteger.FromOctalString('17').ToString).ToBe('15');
end;

procedure TBigIntegerTests.TestFromOctalStringLarger;
begin
  Expect<String>(TBigInteger.FromOctalString('777').ToString).ToBe('511');
end;

procedure TBigIntegerTests.TestFromOctalStringEmpty;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromOctalString('');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestFromOctalStringInvalidChar;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromOctalString('89');
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

{ ── Predicates ───────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestIsZeroTrue;
begin
  Expect<Boolean>(TBigInteger.FromInt64(0).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestIsZeroFalse;
begin
  Expect<Boolean>(TBigInteger.FromInt64(1).IsZero).ToBe(False);
end;

procedure TBigIntegerTests.TestIsNegativeTrue;
begin
  Expect<Boolean>(TBigInteger.FromInt64(-5).IsNegative).ToBe(True);
end;

procedure TBigIntegerTests.TestIsNegativeFalse;
begin
  Expect<Boolean>(TBigInteger.FromInt64(5).IsNegative).ToBe(False);
end;

procedure TBigIntegerTests.TestIsPositiveTrue;
begin
  Expect<Boolean>(TBigInteger.FromInt64(5).IsPositive).ToBe(True);
end;

procedure TBigIntegerTests.TestIsPositiveFalse;
begin
  Expect<Boolean>(TBigInteger.FromInt64(-5).IsPositive).ToBe(False);
end;

procedure TBigIntegerTests.TestIsOneTrue;
begin
  Expect<Boolean>(TBigInteger.One.IsOne).ToBe(True);
end;

procedure TBigIntegerTests.TestIsOneFalse;
begin
  Expect<Boolean>(TBigInteger.FromInt64(2).IsOne).ToBe(False);
end;

procedure TBigIntegerTests.TestIsMinusOneTrue;
begin
  Expect<Boolean>(TBigInteger.NegativeOne.IsMinusOne).ToBe(True);
end;

procedure TBigIntegerTests.TestIsMinusOneFalse;
begin
  Expect<Boolean>(TBigInteger.FromInt64(-2).IsMinusOne).ToBe(False);
end;

procedure TBigIntegerTests.TestBitLength;
begin
  Expect<Integer>(TBigInteger.FromInt64(255).BitLength).ToBe(8);
  Expect<Integer>(TBigInteger.FromInt64(256).BitLength).ToBe(9);
  Expect<Integer>(TBigInteger.FromInt64(1).BitLength).ToBe(1);
end;

procedure TBigIntegerTests.TestBitLengthZero;
begin
  Expect<Integer>(TBigInteger.Zero.BitLength).ToBe(0);
end;

procedure TBigIntegerTests.TestGetBit;
var
  B: TBigInteger;
begin
  B := TBigInteger.FromInt64(5); // binary 101
  Expect<Boolean>(B.GetBit(0)).ToBe(True);
  Expect<Boolean>(B.GetBit(1)).ToBe(False);
  Expect<Boolean>(B.GetBit(2)).ToBe(True);
  Expect<Boolean>(B.GetBit(3)).ToBe(False);
  Expect<Boolean>(B.GetBit(100)).ToBe(False);
end;

{ ── Conversions ──────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestToDoubleZero;
begin
  Expect<Boolean>(TBigInteger.Zero.ToDouble = 0.0).ToBe(True);
end;

procedure TBigIntegerTests.TestToDoublePositive;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).ToDouble = 42.0).ToBe(True);
end;

procedure TBigIntegerTests.TestToDoubleNegative;
begin
  Expect<Boolean>(TBigInteger.FromInt64(-42).ToDouble = -42.0).ToBe(True);
end;

procedure TBigIntegerTests.TestToInt64Zero;
begin
  Expect<Int64>(TBigInteger.Zero.ToInt64).ToBe(Int64(0));
end;

procedure TBigIntegerTests.TestToInt64Positive;
begin
  Expect<Int64>(TBigInteger.FromInt64(12345).ToInt64).ToBe(Int64(12345));
end;

procedure TBigIntegerTests.TestToInt64Negative;
begin
  Expect<Int64>(TBigInteger.FromInt64(-12345).ToInt64).ToBe(Int64(-12345));
end;

procedure TBigIntegerTests.TestToInt64Large;
begin
  Expect<Int64>(TBigInteger.FromInt64(Int64(4294967296)).ToInt64).ToBe(Int64(4294967296));
end;

procedure TBigIntegerTests.TestToStringZero;
begin
  Expect<String>(TBigInteger.Zero.ToString).ToBe('0');
end;

procedure TBigIntegerTests.TestToStringPositive;
begin
  Expect<String>(TBigInteger.FromInt64(12345).ToString).ToBe('12345');
end;

procedure TBigIntegerTests.TestToStringNegative;
begin
  Expect<String>(TBigInteger.FromInt64(-12345).ToString).ToBe('-12345');
end;

procedure TBigIntegerTests.TestToRadixStringBinary;
begin
  Expect<String>(TBigInteger.FromInt64(10).ToRadixString(2)).ToBe('1010');
end;

procedure TBigIntegerTests.TestToRadixStringHex;
begin
  Expect<String>(TBigInteger.FromInt64(255).ToRadixString(16)).ToBe('ff');
end;

procedure TBigIntegerTests.TestToRadixStringOctal;
begin
  Expect<String>(TBigInteger.FromInt64(8).ToRadixString(8)).ToBe('10');
end;

procedure TBigIntegerTests.TestToRadixStringBase36;
begin
  Expect<String>(TBigInteger.FromInt64(35).ToRadixString(36)).ToBe('z');
end;

procedure TBigIntegerTests.TestToRadixStringInvalidRadix;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromInt64(1).ToRadixString(1);
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

{ ── Unary ────────────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestNegatePositive;
begin
  Expect<String>(TBigInteger.FromInt64(5).Negate.ToString).ToBe('-5');
end;

procedure TBigIntegerTests.TestNegateNegative;
begin
  Expect<String>(TBigInteger.FromInt64(-5).Negate.ToString).ToBe('5');
end;

procedure TBigIntegerTests.TestNegateZero;
begin
  Expect<Boolean>(TBigInteger.Zero.Negate.IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestAbsValuePositive;
begin
  Expect<String>(TBigInteger.FromInt64(5).AbsValue.ToString).ToBe('5');
end;

procedure TBigIntegerTests.TestAbsValueNegative;
begin
  Expect<String>(TBigInteger.FromInt64(-5).AbsValue.ToString).ToBe('5');
end;

procedure TBigIntegerTests.TestAbsValueZero;
begin
  Expect<Boolean>(TBigInteger.Zero.AbsValue.IsZero).ToBe(True);
end;

{ ── Addition ─────────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestAddBothPositive;
begin
  Expect<String>(TBigInteger.FromInt64(3).Add(TBigInteger.FromInt64(4)).ToString).ToBe('7');
end;

procedure TBigIntegerTests.TestAddBothNegative;
begin
  Expect<String>(TBigInteger.FromInt64(-3).Add(TBigInteger.FromInt64(-4)).ToString).ToBe('-7');
end;

procedure TBigIntegerTests.TestAddPositiveNegative;
begin
  Expect<String>(TBigInteger.FromInt64(10).Add(TBigInteger.FromInt64(-3)).ToString).ToBe('7');
end;

procedure TBigIntegerTests.TestAddNegativePositive;
begin
  Expect<String>(TBigInteger.FromInt64(-10).Add(TBigInteger.FromInt64(3)).ToString).ToBe('-7');
end;

procedure TBigIntegerTests.TestAddZeroLeft;
begin
  Expect<String>(TBigInteger.Zero.Add(TBigInteger.FromInt64(5)).ToString).ToBe('5');
end;

procedure TBigIntegerTests.TestAddZeroRight;
begin
  Expect<String>(TBigInteger.FromInt64(5).Add(TBigInteger.Zero).ToString).ToBe('5');
end;

procedure TBigIntegerTests.TestAddCancelToZero;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).Add(TBigInteger.FromInt64(-42)).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestAddCarry;
var
  A, B: TBigInteger;
begin
  // 2^32 - 1 + 1 = 2^32 (forces carry across limb boundary)
  A := TBigInteger.FromInt64(Int64(4294967295));
  B := TBigInteger.FromInt64(1);
  Expect<String>(A.Add(B).ToString).ToBe('4294967296');
end;

{ ── Subtraction ──────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestSubtractBasic;
begin
  Expect<String>(TBigInteger.FromInt64(10).Subtract(TBigInteger.FromInt64(3)).ToString).ToBe('7');
end;

procedure TBigIntegerTests.TestSubtractNegativeResult;
begin
  Expect<String>(TBigInteger.FromInt64(3).Subtract(TBigInteger.FromInt64(10)).ToString).ToBe('-7');
end;

procedure TBigIntegerTests.TestSubtractFromZero;
begin
  Expect<String>(TBigInteger.Zero.Subtract(TBigInteger.FromInt64(5)).ToString).ToBe('-5');
end;

procedure TBigIntegerTests.TestSubtractToZero;
begin
  Expect<Boolean>(TBigInteger.FromInt64(7).Subtract(TBigInteger.FromInt64(7)).IsZero).ToBe(True);
end;

{ ── Multiplication ───────────────────────────────────────────────── }

procedure TBigIntegerTests.TestMultiplyBasic;
begin
  Expect<String>(TBigInteger.FromInt64(6).Multiply(TBigInteger.FromInt64(7)).ToString).ToBe('42');
end;

procedure TBigIntegerTests.TestMultiplyByZero;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).Multiply(TBigInteger.Zero).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestMultiplyNegatives;
begin
  Expect<String>(TBigInteger.FromInt64(-3).Multiply(TBigInteger.FromInt64(-4)).ToString).ToBe('12');
end;

procedure TBigIntegerTests.TestMultiplyMixedSign;
begin
  Expect<String>(TBigInteger.FromInt64(3).Multiply(TBigInteger.FromInt64(-4)).ToString).ToBe('-12');
end;

procedure TBigIntegerTests.TestMultiplyLarge;
var
  A, B: TBigInteger;
begin
  // 1000000 * 1000000 = 10^12
  A := TBigInteger.FromInt64(1000000);
  B := TBigInteger.FromInt64(1000000);
  Expect<String>(A.Multiply(B).ToString).ToBe('1000000000000');
end;

{ ── Division ─────────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestDivideExact;
begin
  Expect<String>(TBigInteger.FromInt64(42).Divide(TBigInteger.FromInt64(6)).ToString).ToBe('7');
end;

procedure TBigIntegerTests.TestDivideWithRemainder;
begin
  // 7 / 2 = 3 (truncated)
  Expect<String>(TBigInteger.FromInt64(7).Divide(TBigInteger.FromInt64(2)).ToString).ToBe('3');
end;

procedure TBigIntegerTests.TestDivideNegative;
begin
  Expect<String>(TBigInteger.FromInt64(-42).Divide(TBigInteger.FromInt64(6)).ToString).ToBe('-7');
end;

procedure TBigIntegerTests.TestDivideTruncatesTowardZero;
begin
  // -7 / 2 = -3 (truncates toward zero, not -4)
  Expect<String>(TBigInteger.FromInt64(-7).Divide(TBigInteger.FromInt64(2)).ToString).ToBe('-3');
end;

procedure TBigIntegerTests.TestDivideByZero;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromInt64(1).Divide(TBigInteger.Zero);
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestDivideZeroByNonZero;
begin
  Expect<Boolean>(TBigInteger.Zero.Divide(TBigInteger.FromInt64(5)).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestDivideSmallerByLarger;
begin
  Expect<Boolean>(TBigInteger.FromInt64(3).Divide(TBigInteger.FromInt64(10)).IsZero).ToBe(True);
end;

{ ── Modulo ───────────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestModuloBasic;
begin
  Expect<String>(TBigInteger.FromInt64(7).Modulo(TBigInteger.FromInt64(3)).ToString).ToBe('1');
end;

procedure TBigIntegerTests.TestModuloNegativeDividend;
begin
  // -7 % 3 = -1 (sign follows dividend per ES2026)
  Expect<String>(TBigInteger.FromInt64(-7).Modulo(TBigInteger.FromInt64(3)).ToString).ToBe('-1');
end;

procedure TBigIntegerTests.TestModuloZeroDividend;
begin
  Expect<Boolean>(TBigInteger.Zero.Modulo(TBigInteger.FromInt64(5)).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestModuloByZero;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromInt64(1).Modulo(TBigInteger.Zero);
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestModuloSmallerByLarger;
begin
  // 3 % 10 = 3 (returns self)
  Expect<String>(TBigInteger.FromInt64(3).Modulo(TBigInteger.FromInt64(10)).ToString).ToBe('3');
end;

{ ── Power ────────────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestPowerBasic;
begin
  Expect<String>(TBigInteger.FromInt64(2).Power(TBigInteger.FromInt64(10)).ToString).ToBe('1024');
end;

procedure TBigIntegerTests.TestPowerOfZero;
begin
  Expect<Boolean>(TBigInteger.Zero.Power(TBigInteger.FromInt64(5)).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestPowerToZero;
begin
  Expect<Boolean>(TBigInteger.FromInt64(99).Power(TBigInteger.Zero).IsOne).ToBe(True);
end;

procedure TBigIntegerTests.TestPowerNegativeExponent;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TBigInteger.FromInt64(2).Power(TBigInteger.FromInt64(-1));
  except
    Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBigIntegerTests.TestPowerLarge;
begin
  // 10^18 = 1000000000000000000
  Expect<String>(TBigInteger.FromInt64(10).Power(TBigInteger.FromInt64(18)).ToString).ToBe('1000000000000000000');
end;

{ ── Bitwise AND ──────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestBitwiseAndBasic;
begin
  // 0b1100 & 0b1010 = 0b1000 = 8
  Expect<String>(TBigInteger.FromInt64(12).BitwiseAnd(TBigInteger.FromInt64(10)).ToString).ToBe('8');
end;

procedure TBigIntegerTests.TestBitwiseAndWithZero;
begin
  Expect<Boolean>(TBigInteger.FromInt64(255).BitwiseAnd(TBigInteger.Zero).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestBitwiseAndNegative;
begin
  // -1 & 255 = 255 (two's complement: -1 is all 1s)
  Expect<String>(TBigInteger.FromInt64(-1).BitwiseAnd(TBigInteger.FromInt64(255)).ToString).ToBe('255');
end;

{ ── Bitwise OR ───────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestBitwiseOrBasic;
begin
  // 0b1100 | 0b1010 = 0b1110 = 14
  Expect<String>(TBigInteger.FromInt64(12).BitwiseOr(TBigInteger.FromInt64(10)).ToString).ToBe('14');
end;

procedure TBigIntegerTests.TestBitwiseOrWithZero;
begin
  Expect<String>(TBigInteger.FromInt64(42).BitwiseOr(TBigInteger.Zero).ToString).ToBe('42');
end;

procedure TBigIntegerTests.TestBitwiseOrNegative;
begin
  // -1 | 0 = -1
  Expect<String>(TBigInteger.FromInt64(-1).BitwiseOr(TBigInteger.Zero).ToString).ToBe('-1');
end;

{ ── Bitwise XOR ──────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestBitwiseXorBasic;
begin
  // 0b1100 ^ 0b1010 = 0b0110 = 6
  Expect<String>(TBigInteger.FromInt64(12).BitwiseXor(TBigInteger.FromInt64(10)).ToString).ToBe('6');
end;

procedure TBigIntegerTests.TestBitwiseXorSelf;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).BitwiseXor(TBigInteger.FromInt64(42)).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestBitwiseXorNegative;
begin
  // -1 ^ 0 = -1
  Expect<String>(TBigInteger.FromInt64(-1).BitwiseXor(TBigInteger.Zero).ToString).ToBe('-1');
end;

{ ── Bitwise NOT ──────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestBitwiseNotPositive;
begin
  // ~0 = -1
  Expect<String>(TBigInteger.Zero.BitwiseNot.ToString).ToBe('-1');
end;

procedure TBigIntegerTests.TestBitwiseNotNegative;
begin
  // ~(-1) = 0
  Expect<Boolean>(TBigInteger.FromInt64(-1).BitwiseNot.IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestBitwiseNotZero;
begin
  // ~5 = -6
  Expect<String>(TBigInteger.FromInt64(5).BitwiseNot.ToString).ToBe('-6');
end;

{ ── Shift left ───────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestShiftLeftBasic;
begin
  // 1 << 8 = 256
  Expect<String>(TBigInteger.FromInt64(1).ShiftLeft(8).ToString).ToBe('256');
end;

procedure TBigIntegerTests.TestShiftLeftZeroCount;
begin
  Expect<String>(TBigInteger.FromInt64(42).ShiftLeft(0).ToString).ToBe('42');
end;

procedure TBigIntegerTests.TestShiftLeftZeroValue;
begin
  Expect<Boolean>(TBigInteger.Zero.ShiftLeft(10).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestShiftLeftLarge;
begin
  // 1 << 64 = 18446744073709551616
  Expect<String>(TBigInteger.FromInt64(1).ShiftLeft(64).ToString).ToBe('18446744073709551616');
end;

{ ── Shift right ──────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestShiftRightBasic;
begin
  // 256 >> 4 = 16
  Expect<String>(TBigInteger.FromInt64(256).ShiftRight(4).ToString).ToBe('16');
end;

procedure TBigIntegerTests.TestShiftRightZeroCount;
begin
  Expect<String>(TBigInteger.FromInt64(42).ShiftRight(0).ToString).ToBe('42');
end;

procedure TBigIntegerTests.TestShiftRightAllBits;
begin
  // 255 >> 100 = 0 (all bits shifted out for positive)
  Expect<Boolean>(TBigInteger.FromInt64(255).ShiftRight(100).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestShiftRightNegativeFloors;
begin
  // -1 >> 1 = -1 (floor division semantics)
  Expect<String>(TBigInteger.FromInt64(-1).ShiftRight(1).ToString).ToBe('-1');
  // -3 >> 1 = -2 (floor: -1.5 rounds to -2)
  Expect<String>(TBigInteger.FromInt64(-3).ShiftRight(1).ToString).ToBe('-2');
end;

{ ── Comparison ───────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestCompareEqual;
begin
  Expect<Integer>(TBigInteger.FromInt64(42).Compare(TBigInteger.FromInt64(42))).ToBe(0);
end;

procedure TBigIntegerTests.TestCompareLess;
begin
  Expect<Integer>(TBigInteger.FromInt64(3).Compare(TBigInteger.FromInt64(5))).ToBe(-1);
end;

procedure TBigIntegerTests.TestCompareGreater;
begin
  Expect<Integer>(TBigInteger.FromInt64(5).Compare(TBigInteger.FromInt64(3))).ToBe(1);
end;

procedure TBigIntegerTests.TestCompareNegatives;
begin
  Expect<Integer>(TBigInteger.FromInt64(-5).Compare(TBigInteger.FromInt64(-3))).ToBe(-1);
  Expect<Integer>(TBigInteger.FromInt64(-3).Compare(TBigInteger.FromInt64(-5))).ToBe(1);
end;

procedure TBigIntegerTests.TestCompareMixedSign;
begin
  Expect<Integer>(TBigInteger.FromInt64(-1).Compare(TBigInteger.FromInt64(1))).ToBe(-1);
  Expect<Integer>(TBigInteger.FromInt64(1).Compare(TBigInteger.FromInt64(-1))).ToBe(1);
end;

procedure TBigIntegerTests.TestEqualTrue;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).Equal(TBigInteger.FromInt64(42))).ToBe(True);
end;

procedure TBigIntegerTests.TestEqualFalse;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).Equal(TBigInteger.FromInt64(43))).ToBe(False);
end;

{ ── AsIntN / AsUintN ─────────────────────────────────────────────── }

procedure TBigIntegerTests.TestAsIntN8;
begin
  // 127 fits in signed 8-bit
  Expect<String>(TBigInteger.FromInt64(127).AsIntN(8).ToString).ToBe('127');
end;

procedure TBigIntegerTests.TestAsIntN8Negative;
begin
  // -128 is the minimum signed 8-bit value
  Expect<String>(TBigInteger.FromInt64(-128).AsIntN(8).ToString).ToBe('-128');
end;

procedure TBigIntegerTests.TestAsIntN8Overflow;
begin
  // 128 overflows signed 8-bit → wraps to -128
  Expect<String>(TBigInteger.FromInt64(128).AsIntN(8).ToString).ToBe('-128');
  // 255 → -1
  Expect<String>(TBigInteger.FromInt64(255).AsIntN(8).ToString).ToBe('-1');
end;

procedure TBigIntegerTests.TestAsUintN8;
begin
  Expect<String>(TBigInteger.FromInt64(200).AsUintN(8).ToString).ToBe('200');
end;

procedure TBigIntegerTests.TestAsUintN8Large;
begin
  // 256 mod 256 = 0
  Expect<String>(TBigInteger.FromInt64(256).AsUintN(8).ToString).ToBe('0');
  // 257 mod 256 = 1
  Expect<String>(TBigInteger.FromInt64(257).AsUintN(8).ToString).ToBe('1');
end;

procedure TBigIntegerTests.TestAsUintNZeroBits;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).AsUintN(0).IsZero).ToBe(True);
end;

procedure TBigIntegerTests.TestAsIntNZeroBits;
begin
  Expect<Boolean>(TBigInteger.FromInt64(42).AsIntN(0).IsZero).ToBe(True);
end;

{ ── Roundtrips ───────────────────────────────────────────────────── }

procedure TBigIntegerTests.TestRoundtripFromInt64;
var
  Values: array[0..5] of Int64;
  I: Integer;
begin
  Values[0] := 0;
  Values[1] := 1;
  Values[2] := -1;
  Values[3] := 2147483647;
  Values[4] := Int64(-2147483648);
  Values[5] := Int64(1718451045123);

  for I := 0 to High(Values) do
    Expect<Int64>(TBigInteger.FromInt64(Values[I]).ToInt64).ToBe(Values[I]);
end;

procedure TBigIntegerTests.TestRoundtripDecimalString;
var
  S: string;
begin
  S := '99999999999999999999';
  Expect<String>(TBigInteger.FromDecimalString(S).ToString).ToBe(S);
end;

procedure TBigIntegerTests.TestRoundtripDivModConsistency;
var
  A, B, Q, R, Reconstructed: TBigInteger;
begin
  // Verify: a = (a / b) * b + (a mod b) for several cases
  A := TBigInteger.FromInt64(100);
  B := TBigInteger.FromInt64(7);
  Q := A.Divide(B);
  R := A.Modulo(B);
  Reconstructed := Q.Multiply(B).Add(R);
  Expect<Boolean>(A.Equal(Reconstructed)).ToBe(True);

  // Negative dividend
  A := TBigInteger.FromInt64(-100);
  Q := A.Divide(B);
  R := A.Modulo(B);
  Reconstructed := Q.Multiply(B).Add(R);
  Expect<Boolean>(A.Equal(Reconstructed)).ToBe(True);
end;

procedure TBigIntegerTests.TestRoundtripFromDoubleToInt64;
var
  Values: array[0..3] of Int64;
  I: Integer;
  D: Double;
begin
  Values[0] := Int64(1718451045123);
  Values[1] := Int64(9007199254740992);
  Values[2] := Int64(-9007199254740992);
  Values[3] := Int64(4294967296);

  for I := 0 to High(Values) do
  begin
    D := Values[I];
    Expect<Int64>(TBigInteger.FromDouble(D).ToInt64).ToBe(Values[I]);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TBigIntegerTests.Create('BigInteger'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
