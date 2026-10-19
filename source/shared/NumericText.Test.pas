program NumericText.Test;

{$I Shared.inc}

uses
  Classes,
  Math,
  SysUtils,

  AllocationCounter,
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
    procedure TestFastPathMatchesExactConversion;
    procedure TestShortDecimalAllocations;
    procedure TestEveryRoundingModeConvertsToNearest;
    procedure TestShortestNumberFormatting;
    procedure TestIntegerFormatting;
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
  Test('the fast path converts as the exact conversion does',
    TestFastPathMatchesExactConversion);
  Test('every floating-point rounding mode converts to nearest, ties to even',
    TestEveryRoundingModeConvertsToNearest);
  Test('a short decimal converts without per-digit allocations',
    TestShortDecimalAllocations);
  Test('number formatting emits ECMAScript shortest decimals',
    TestShortestNumberFormatting);
  Test('integer formatting matches the RTL across the Int64 range',
    TestIntegerFormatting);
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

procedure ExpectSameAsExact(const AText: string);
begin
  Expect<string>(AText + ' -> ' + IntToHex(DoubleToBits(
    DecimalTextToNumber(AText)), 16)).ToBe(AText + ' -> ' + IntToHex(
    DoubleToBits(ExactDecimalTextToNumber(AText)), 16));
end;

// A fixed-seed linear congruential step (Knuth's MMIX constants); the
// arithmetic wraps by design.
{$PUSH}{$Q-}{$R-}
function NextPseudoRandom(var AState: UInt64; const ABound: Integer): Integer;
begin
  AState := AState * UInt64(6364136223846793005) +
    UInt64(1442695040888963407);
  Result := Integer((AState shr 33) mod UInt64(ABound));
end;
{$POP}

procedure TNumericTextTests.TestFastPathMatchesExactConversion;
const
  BOUNDARIES: array[0..21] of string = (
    '1', '-1', '1.5', '0.1', '123.456', '-0.000001', '1e22', '1e23',
    '1e-22', '1e-23', '9007199254740992', '9007199254740993',
    '9007199254740992e22', '9007199254740992e-22', '9007199254740991e15',
    '9007199254740991e16', '1e15', '1e16', '7e15', '9999999999999999999',
    '99999999999999999999', '4.35e-20');
var
  I, DigitCount, Exponent, DigitIndex: Integer;
  State: UInt64;
  Digits: string;
begin
  for I := Low(BOUNDARIES) to High(BOUNDARIES) do
    ExpectSameAsExact(BOUNDARIES[I]);

  // Every digit count the fast path reads, and every exponent around it.
  State := 1785;
  for I := 1 to 5000 do
  begin
    DigitCount := 1 + NextPseudoRandom(State, 20);
    Exponent := NextPseudoRandom(State, 61) - 30;
    SetLength(Digits, DigitCount);
    for DigitIndex := 1 to DigitCount do
      Digits[DigitIndex] := Char(Ord('0') + NextPseudoRandom(State, 10));
    if Digits[1] = '0' then
      Digits[1] := '1';
    ExpectSameAsExact(Digits + 'e' + IntToStr(Exponent));
    ExpectSameAsExact(Copy(Digits, 1, (DigitCount + 1) div 2) + '.' +
      Copy(Digits, (DigitCount + 1) div 2 + 1, DigitCount) + 'e' +
      IntToStr(Exponent));
  end;
end;

procedure TNumericTextTests.TestShortDecimalAllocations;
var
  Value: Double;
  Allocations: Int64;
begin
  // A source literal such as `1` used to cost a BigInteger long division:
  // more than a hundred allocations for one digit.
  StartCountingAllocations;
  Value := DecimalTextToNumber('1');
  Value := Value + DecimalTextToNumber('30000');
  {$IF DEFINED(GOCCIA_CPU_X86_64) OR DEFINED(GOCCIA_CPU_AARCH64)}
  // A fraction takes the fast path only where one division rounds once.
  Value := Value + DecimalTextToNumber('1.25');
  Allocations := StopCountingAllocations;
  Expect<Double>(Value).ToBe(30002.25);
  {$ELSE}
  Allocations := StopCountingAllocations;
  Expect<Double>(Value).ToBe(30001);
  {$ENDIF}
  if Allocations > 6 then
    Fail(Format('%d allocations; expected at most 6', [Allocations]));
end;

// The conversion's result must not depend on the floating-point rounding
// mode a host left set.
procedure TNumericTextTests.TestEveryRoundingModeConvertsToNearest;
const
  INPUTS: array[0..5] of string = ('0.1', '1.25', '3.14159', '1e22',
    '123456789012345e-17', '9007199254740993');
  MODES: array[0..3] of TFPURoundingMode = (rmNearest, rmDown, rmUp,
    rmTruncate);
var
  Expected: array[0..5] of UInt64;
  Previous: TFPURoundingMode;
  I, M: Integer;
begin
  for I := 0 to High(INPUTS) do
    Expected[I] := DoubleToBits(ExactDecimalTextToNumber(INPUTS[I]));
  Previous := GetRoundMode;
  try
    for M := 0 to High(MODES) do
    begin
      SetRoundMode(MODES[M]);
      for I := 0 to High(INPUTS) do
        if DoubleToBits(DecimalTextToNumber(INPUTS[I])) <> Expected[I] then
          Fail(Format('%s under rounding mode %d', [INPUTS[I], Ord(MODES[M])]));
    end;
  finally
    SetRoundMode(Previous);
  end;
  Expect<Boolean>(True).ToBe(True);
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

procedure TNumericTextTests.TestIntegerFormatting;
const
  BOUNDARIES: array[0..17] of Int64 = (
    0, 1, -1, 9, 10, -10, 99, 100, 2147483647, -2147483648, 2147483648,
    4294967295, 4294967296, 9007199254740991, -9007199254740991,
    999999999999999999, High(Int64), Low(Int64));
var
  I: Integer;
  Power: Int64;
begin
  Expect<string>(IntegerToString(0)).ToBe('0');
  Expect<string>(IntegerToString(-1)).ToBe('-1');
  Expect<string>(IntegerToString(1234567890)).ToBe('1234567890');
  Expect<string>(IntegerToString(High(Int64))).ToBe('9223372036854775807');
  Expect<string>(IntegerToString(Low(Int64))).ToBe('-9223372036854775808');

  for I := Low(BOUNDARIES) to High(BOUNDARIES) do
    Expect<string>(IntegerToString(BOUNDARIES[I])).ToBe(
      string(IntToStr(BOUNDARIES[I])));

  // Every digit count, on both sides of each power of ten.
  Power := 1;
  for I := 1 to 18 do
  begin
    Power := Power * 10;
    Expect<string>(IntegerToString(Power - 1)).ToBe(string(IntToStr(Power - 1)));
    Expect<string>(IntegerToString(Power)).ToBe(string(IntToStr(Power)));
    Expect<string>(IntegerToString(-Power)).ToBe(string(IntToStr(-Power)));
    Expect<string>(IntegerToString(-Power - 1)).ToBe(
      string(IntToStr(-Power - 1)));
  end;
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
