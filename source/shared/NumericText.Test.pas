program NumericText.Test;

{$I Shared.inc}

uses
  Classes,
  Math,
  SysUtils,

  NumberBits,
  NumericText,
  TestingPascalLibrary;

type
  TNumericTextTests = class(TTestSuite)
  private
    procedure TestWhitespaceAndEmpty;
    procedure TestSignedInfinity;
    procedure TestUInt64Parsing;
    procedure TestNonDecimalIntegerLiterals;
    procedure TestNonDecimalRejections;
    procedure TestDecimalLiterals;
    procedure TestDecimalRoundingBoundaries;
    procedure TestShortestNumberFormatting;
    procedure TestSignedZero;
    procedure TestInvalidInput;
  public
    procedure SetupTests; override;
  end;

function IsNegativeZero(const AValue: Double): Boolean;
begin
  Result := NumberBits.IsNegativeZero(AValue);
end;

function IsPositiveZero(const AValue: Double): Boolean;
begin
  Result := (AValue = 0.0) and not NumberBits.IsNegativeZero(AValue);
end;

procedure TNumericTextTests.SetupTests;
begin
  Test('whitespace and empty input convert to +0',
    TestWhitespaceAndEmpty);
  Test('signed Infinity converts; abbreviations do not',
    TestSignedInfinity);
  Test('unsigned 64-bit parsing uses the native RTL',
    TestUInt64Parsing);
  Test('hex, binary, and octal literals convert in either letter case',
    TestNonDecimalIntegerLiterals);
  Test('non-decimal literals reject signs, empty digits, and bad radix digits',
    TestNonDecimalRejections);
  Test('decimal literals convert and overflow to signed Infinity',
    TestDecimalLiterals);
  Test('decimal conversion rounds binary64 boundaries ties-to-even',
    TestDecimalRoundingBoundaries);
  Test('number formatting emits ECMAScript shortest decimals',
    TestShortestNumberFormatting);
  Test('negative-zero strings preserve -0',
    TestSignedZero);
  Test('inputs that are not StringNumericLiterals convert to NaN',
    TestInvalidInput);
end;

procedure TNumericTextTests.TestUInt64Parsing;
var
  Value: UInt64;
begin
  Expect<Boolean>(TryTextToUInt64('$FFFFFFFFFFFFFFFF', Value)).ToBe(True);
  Expect<UInt64>(Value).ToBe(High(UInt64));
  Expect<Boolean>(TryTextToUInt64('$10000000000000000', Value)).ToBe(False);
  Expect<Boolean>(TryTextToUInt64('$not-hex', Value)).ToBe(False);
end;

procedure TNumericTextTests.TestWhitespaceAndEmpty;
begin
  Expect<Boolean>(IsPositiveZero(StringToNumber(''))).ToBe(True);
  Expect<Boolean>(IsPositiveZero(StringToNumber('   '))).ToBe(True);
  Expect<Boolean>(IsPositiveZero(StringToNumber(#9#10#13' '))).ToBe(True);
  Expect<Double>(StringToNumber('  42  ')).ToBe(42.0);
  Expect<Double>(StringToNumber(#9' 42 '#10)).ToBe(42.0);
end;

procedure TNumericTextTests.TestSignedInfinity;
begin
  Expect<Boolean>(StringToNumber('Infinity') = Infinity).ToBe(True);
  Expect<Boolean>(StringToNumber('+Infinity') = Infinity).ToBe(True);
  Expect<Boolean>(StringToNumber('-Infinity') = NegInfinity).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('Inf'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('Infinityy'))).ToBe(True);
end;

procedure TNumericTextTests.TestNonDecimalIntegerLiterals;
begin
  Expect<Double>(StringToNumber('0xFF')).ToBe(255.0);
  Expect<Double>(StringToNumber('0X10')).ToBe(16.0);
  Expect<Double>(StringToNumber('0b101')).ToBe(5.0);
  Expect<Double>(StringToNumber('0B11')).ToBe(3.0);
  Expect<Double>(StringToNumber('0o17')).ToBe(15.0);
  Expect<Double>(StringToNumber('0O20')).ToBe(16.0);
  // Large hex must round to the nearest Double, not wrap a fixed-width integer.
  Expect<Double>(StringToNumber('0xABCDEF12345')).ToBe(11806310474565.0);
end;

procedure TNumericTextTests.TestNonDecimalRejections;
begin
  Expect<Boolean>(IsNan(StringToNumber('+0x10'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('-0x10'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('0x'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('0b'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('0o'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('0b12'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('0o18'))).ToBe(True);
end;

procedure TNumericTextTests.TestDecimalLiterals;
begin
  Expect<Double>(StringToNumber('00123')).ToBe(123.0);
  Expect<Double>(StringToNumber('.5e3')).ToBe(500.0);
  Expect<Double>(StringToNumber('5.')).ToBe(5.0);
  Expect<Double>(StringToNumber('+.5')).ToBe(0.5);
  Expect<Boolean>(StringToNumber('1e400') = Infinity).ToBe(True);
  Expect<Boolean>(StringToNumber('-1e400') = NegInfinity).ToBe(True);
end;

procedure TNumericTextTests.TestDecimalRoundingBoundaries;
begin
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber('0.1'))).ToBe(
    UInt64($3FB999999999999A));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '0.30000000000000004'))).ToBe(UInt64($3FD3333333333334));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber('0.30e-143'))).ToBe(
    UInt64($2222BB017A5BA7D4));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '9007199254740993'))).ToBe(UInt64($4340000000000000));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '2.4703282292062327e-324'))).ToBe(UInt64($0000000000000000));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '2.4703282292062328e-324'))).ToBe(UInt64($0000000000000001));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '4.9406564584124654e-324'))).ToBe(UInt64($0000000000000001));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '2.2250738585072011e-308'))).ToBe(UInt64($000FFFFFFFFFFFFF));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '2.2250738585072014e-308'))).ToBe(UInt64($0010000000000000));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '1.7976931348623157e308'))).ToBe(UInt64($7FEFFFFFFFFFFFFF));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '1.7976931348623158e308'))).ToBe(UInt64($7FEFFFFFFFFFFFFF));
  Expect<UInt64>(DoubleToBits(DecimalTextToNumber(
    '1.7976931348623159e308'))).ToBe(UInt64($7FF0000000000000));
end;

procedure TNumericTextTests.TestShortestNumberFormatting;
begin
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($0000000000000001)))).ToBe('5e-324');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($000FFFFFFFFFFFFF)))).ToBe('2.225073858507201e-308');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($0010000000000000)))).ToBe('2.2250738585072014e-308');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($3FB999999999999A)))).ToBe('0.1');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($3FD3333333333334)))).ToBe('0.30000000000000004');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($4340000000000000)))).ToBe('9007199254740992');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($7FEFFFFFFFFFFFFF)))).ToBe('1.7976931348623157e+308');
  Expect<string>(NumberToString(BitsToDouble(
    UInt64($8000000000000000)))).ToBe('0');
end;

procedure TNumericTextTests.TestSignedZero;
begin
  Expect<Boolean>(IsNegativeZero(StringToNumber('-0'))).ToBe(True);
  Expect<Boolean>(IsNegativeZero(StringToNumber('-0.00'))).ToBe(True);
  Expect<Boolean>(IsPositiveZero(StringToNumber('+0'))).ToBe(True);
  Expect<Boolean>(IsPositiveZero(StringToNumber('0'))).ToBe(True);
end;

procedure TNumericTextTests.TestInvalidInput;
begin
  Expect<Boolean>(IsNan(StringToNumber('abc'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('.'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('+'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('-'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('1_000'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('1,000'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('1.5d'))).ToBe(True);
  Expect<Boolean>(IsNan(StringToNumber('0x1p4'))).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TNumericTextTests.Create('NumericText'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
