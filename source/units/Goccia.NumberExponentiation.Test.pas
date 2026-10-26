program Goccia.NumberExponentiation.Test;

{$I Goccia.inc}

uses
  Math,

  NumberBits,
  TestingPascalLibrary,

  Goccia.FloatingPoint,
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
    procedure TestCorrectlyRoundedResults;
    procedure TestNegativeBaseLargeIntegralExponent;
    procedure TestSubnormalAndOverflowingResults;
  public
    procedure SetupTests; override;
  end;

procedure TNumberExponentiationTests.SetupTests;
begin
  Test('ordinary finite values', TestOrdinaryValues);
  Test('zero exponent takes precedence over a NaN base', TestNaNPrecedence);
  Test('infinite bases preserve odd exponent signs', TestInfiniteBases);
  Test('signed-zero bases preserve odd exponent signs', TestSignedZeroBases);
  Test('infinite exponents compare the base magnitude to one',
    TestInfiniteExponents);
  Test('negative bases reject fractional exponents',
    TestNegativeFractionalBase);
  Test('finite results are correctly rounded', TestCorrectlyRoundedResults);
  Test('negative bases keep the sign for integral exponents of 2^31 or more',
    TestNegativeBaseLargeIntegralExponent);
  Test('results round once into the subnormal range and overflow to infinity',
    TestSubnormalAndOverflowingResults);
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

// Expected bit patterns are Node 24's results, each the correctly rounded
// value.
procedure TNumberExponentiationTests.TestCorrectlyRoundedResults;
begin
  Expect<UInt64>(DoubleToBits(NumberExponentiation(
    BitsToDouble($4415AF1D78B58C40), 0.5))).ToBe($4202A05F20000000);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(10.0, -5.0))).ToBe(
    $3EE4F8B588E368F1);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(
    BitsToDouble($3FF199999999999A), 30.0))).ToBe($4031730C06EF9D91);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(10.0, 308.0))).ToBe(
    $7FE1CCF385EBC8A0);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(
    BitsToDouble($16687E92154EF7AC), 1.5))).ToBe($01A56E1FC2F8F359);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(2.0,
    BitsToDouble($3FB999999999999A)))).ToBe($3FF125FBEE250664);
  Expect<Double>(NumberExponentiation(9.0, 0.5)).ToBe(3.0);
  Expect<Double>(NumberExponentiation(3.0, 33.0)).ToBe(5559060566555523.0);
end;

procedure TNumberExponentiationTests.TestNegativeBaseLargeIntegralExponent;
begin
  Expect<Double>(NumberExponentiation(-2.0, 2147483648.0)).ToBe(Infinity);
  Expect<Double>(NumberExponentiation(-3.0, 2147483649.0)).ToBe(NegInfinity);
  Expect<UInt64>(DoubleToBits(NumberExponentiation(-0.5, 2147483649.0))).ToBe(
    UInt64(1) shl 63);
  Expect<Double>(NumberExponentiation(-1.0, 1e300)).ToBe(1.0);
end;

// Under the engine's floating-point environment, where an overflow gives
// infinity instead of raising.
procedure TNumberExponentiationTests.TestSubnormalAndOverflowingResults;
var
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Expect<UInt64>(DoubleToBits(NumberExponentiation(2.0, -1074.0))).ToBe(1);
    Expect<UInt64>(DoubleToBits(NumberExponentiation(0.5, 1075.0))).ToBe(0);
    Expect<UInt64>(DoubleToBits(NumberExponentiation(-2.0, -1075.0))).ToBe(
      UInt64(1) shl 63);
    Expect<Double>(NumberExponentiation(10.0, 309.0)).ToBe(Infinity);
    Expect<Double>(NumberExponentiation(-10.0, 309.0)).ToBe(NegInfinity);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TNumberExponentiationTests.Create('Goccia.NumberExponentiation'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
