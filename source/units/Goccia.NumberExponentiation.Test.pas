program Goccia.NumberExponentiation.Test;

{$I Goccia.inc}

uses
  Math,

  NumberBits,
  TestingPascalLibrary,

  Goccia.NumberExponentiation;

type
  TNumberExponentiationTests = class(TTestSuite)
  private
    procedure TestOrdinaryValues;
    procedure TestNaNPrecedence;
    procedure TestInfiniteBases;
    procedure TestSignedZeroBases;
    procedure TestInfiniteExponents;
    procedure TestNegativeFractionalBase;
  public
    procedure SetupTests; override;
  end;

procedure TNumberExponentiationTests.SetupTests;
begin
  Test('ordinary finite values use native binary64 power', TestOrdinaryValues);
  Test('zero exponent takes precedence over a NaN base', TestNaNPrecedence);
  Test('infinite bases preserve odd exponent signs', TestInfiniteBases);
  Test('signed-zero bases preserve odd exponent signs', TestSignedZeroBases);
  Test('infinite exponents compare the base magnitude to one',
    TestInfiniteExponents);
  Test('negative bases reject fractional exponents',
    TestNegativeFractionalBase);
end;

procedure TNumberExponentiationTests.TestOrdinaryValues;
begin
  Expect<Double>(NumberExponentiation(2.0, 10.0)).ToBe(1024.0);
  Expect<Double>(NumberExponentiation(-2.0, 3.0)).ToBe(-8.0);
end;

procedure TNumberExponentiationTests.TestNaNPrecedence;
begin
  Expect<Double>(NumberExponentiation(NaN, 0.0)).ToBe(1.0);
  Expect<Boolean>(IsNan(NumberExponentiation(1.0, NaN))).ToBe(True);
end;

procedure TNumberExponentiationTests.TestInfiniteBases;
begin
  Expect<Double>(NumberExponentiation(NegInfinity, 3.0)).ToBe(NegInfinity);
  Expect<Double>(NumberExponentiation(NegInfinity, 2.0)).ToBe(Infinity);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(NegInfinity, -3.0))).ToBe(
    UInt64(1) shl 63);
end;

procedure TNumberExponentiationTests.TestSignedZeroBases;
var
  NegativeZero: Double;
begin
  NegativeZero := BitsToDouble(UInt64(1) shl 63);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(NegativeZero, 3.0))).ToBe(
    UInt64(1) shl 63);
  Expect<Double>(NumberExponentiation(NegativeZero, -3.0)).ToBe(NegInfinity);
  Expect<Double>(NumberExponentiation(NegativeZero, 2.0)).ToBe(0.0);
end;

procedure TNumberExponentiationTests.TestInfiniteExponents;
begin
  Expect<Double>(NumberExponentiation(2.0, Infinity)).ToBe(Infinity);
  Expect<Double>(NumberExponentiation(2.0, NegInfinity)).ToBe(0.0);
  Expect<Double>(NumberExponentiation(0.5, Infinity)).ToBe(0.0);
  Expect<Boolean>(IsNan(NumberExponentiation(-1.0, Infinity))).ToBe(True);
end;

procedure TNumberExponentiationTests.TestNegativeFractionalBase;
begin
  Expect<Boolean>(IsNan(NumberExponentiation(-2.0, 0.5))).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(
    TNumberExponentiationTests.Create('Goccia.NumberExponentiation'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
