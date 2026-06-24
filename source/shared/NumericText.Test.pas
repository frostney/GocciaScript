program NumericText.Test;

{$I Shared.inc}

uses
  Classes,
  Math,
  SysUtils,

  NumericText,
  TestingPascalLibrary;

type
  TNumericTextTests = class(TTestSuite)
  private
    procedure TestWhitespaceAndEmpty;
    procedure TestSignedInfinity;
    procedure TestNonDecimalIntegerLiterals;
    procedure TestNonDecimalRejections;
    procedure TestDecimalLiterals;
    procedure TestSignedZero;
    procedure TestInvalidInput;
  public
    procedure SetupTests; override;
  end;

function IsNegativeZero(const AValue: Double): Boolean;
var
  Value: Double;
  Bits: Int64 absolute Value;
begin
  Value := AValue;
  Result := (Value = 0.0) and (Bits < 0);
end;

function IsPositiveZero(const AValue: Double): Boolean;
var
  Value: Double;
  Bits: Int64 absolute Value;
begin
  Value := AValue;
  Result := (Value = 0.0) and (Bits >= 0);
end;

procedure TNumericTextTests.SetupTests;
begin
  Test('whitespace and empty input convert to +0',
    TestWhitespaceAndEmpty);
  Test('signed Infinity converts; abbreviations do not',
    TestSignedInfinity);
  Test('hex, binary, and octal literals convert in either letter case',
    TestNonDecimalIntegerLiterals);
  Test('non-decimal literals reject signs, empty digits, and bad radix digits',
    TestNonDecimalRejections);
  Test('decimal literals convert and overflow to signed Infinity',
    TestDecimalLiterals);
  Test('negative-zero strings preserve -0',
    TestSignedZero);
  Test('inputs that are not StringNumericLiterals convert to NaN',
    TestInvalidInput);
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
