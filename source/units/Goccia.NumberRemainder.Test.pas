program Goccia.NumberRemainder.Test;

{$I Goccia.inc}

uses
  Math,

  NumberBits,
  TestingPascalLibrary,

  Goccia.NumberRemainder;

type
  TNumberRemainderTests = class(TTestSuite)
  private
    procedure TestFiniteValues;
    procedure TestHugeQuotient;
    procedure TestSignedZero;
    procedure TestSpecialValues;
    procedure TestSubnormalValues;
  public
    procedure SetupTests; override;
  end;

procedure TNumberRemainderTests.SetupTests;
begin
  Test('finite operands use truncated division semantics', TestFiniteValues);
  Test('huge quotient retains the exact binary64 remainder', TestHugeQuotient);
  Test('zero remainder preserves the dividend sign', TestSignedZero);
  Test('NaN, infinity, and zero follow Number remainder', TestSpecialValues);
  Test('subnormal inputs and outputs retain exact bits', TestSubnormalValues);
end;

procedure TNumberRemainderTests.TestFiniteValues;
begin
  Expect<Double>(NumberRemainder(5.5, 2.0)).ToBe(1.5);
  Expect<Double>(NumberRemainder(-5.5, 2.0)).ToBe(-1.5);
  Expect<Double>(NumberRemainder(5.5, -2.0)).ToBe(1.5);
end;

procedure TNumberRemainderTests.TestHugeQuotient;
begin
  Expect<Double>(NumberRemainder(1.0E308, 3.0)).ToBe(2.0);
end;

procedure TNumberRemainderTests.TestSignedZero;
const
  NEGATIVE_ZERO_BITS: UInt64 = UInt64(1) shl 63;
begin
  Expect<UInt64>(DoubleToBits(NumberRemainder(-4.0, 2.0))).ToBe(
    NEGATIVE_ZERO_BITS);
  Expect<UInt64>(DoubleToBits(NumberRemainder(4.0, 2.0))).ToBe(UInt64(0));
end;

procedure TNumberRemainderTests.TestSpecialValues;
begin
  Expect<Boolean>(IsNan(NumberRemainder(Infinity, 1.0))).ToBe(True);
  Expect<Boolean>(IsNan(NumberRemainder(1.0, 0.0))).ToBe(True);
  Expect<Boolean>(IsNan(NumberRemainder(NaN, 1.0))).ToBe(True);
  Expect<Double>(NumberRemainder(7.0, Infinity)).ToBe(7.0);
end;

procedure TNumberRemainderTests.TestSubnormalValues;
const
  MINIMUM_SUBNORMAL_BITS: UInt64 = 1;
  THREE_SUBNORMAL_BITS: UInt64 = 3;
begin
  Expect<UInt64>(DoubleToBits(NumberRemainder(
    BitsToDouble(THREE_SUBNORMAL_BITS),
    BitsToDouble(UInt64(2))))).ToBe(MINIMUM_SUBNORMAL_BITS);
end;

begin
  TestRunnerProgram.AddSuite(
    TNumberRemainderTests.Create('Goccia.NumberRemainder'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
