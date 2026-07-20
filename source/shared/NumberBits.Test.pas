program NumberBits.Test;

{$I Shared.inc}

uses
  NumberBits,
  TestingPascalLibrary;

type
  TNumberBitsTests = class(TTestSuite)
  private
    procedure TestRoundTrip;
    procedure TestSingleRoundTrip;
    procedure TestSignedZero;
    procedure TestCanonicalNaNPatterns;
  public
    procedure SetupTests; override;
  end;

procedure TNumberBitsTests.SetupTests;
begin
  Test('binary64 bit conversions round-trip', TestRoundTrip);
  Test('binary32 bit conversions round-trip', TestSingleRoundTrip);
  Test('signed zero is identified by its binary64 representation',
    TestSignedZero);
  Test('canonical NaN patterns are positive quiet NaNs',
    TestCanonicalNaNPatterns);
end;

procedure TNumberBitsTests.TestRoundTrip;
const
  VALUE_BITS: UInt64 = UInt64($C0454000) shl 32;
begin
  Expect<UInt64>(DoubleToBits(BitsToDouble(VALUE_BITS))).ToBe(VALUE_BITS);
end;

procedure TNumberBitsTests.TestSingleRoundTrip;
const
  VALUE_BITS: UInt32 = $C22A0000;
begin
  Expect<UInt32>(SingleToBits(BitsToSingle(VALUE_BITS))).ToBe(VALUE_BITS);
end;

procedure TNumberBitsTests.TestSignedZero;
const
  NEGATIVE_ZERO_BITS: UInt64 = UInt64(1) shl 63;
begin
  Expect<Boolean>(IsNegativeZero(BitsToDouble(NEGATIVE_ZERO_BITS))).ToBe(True);
  Expect<Boolean>(IsNegativeZero(0.0)).ToBe(False);
end;

procedure TNumberBitsTests.TestCanonicalNaNPatterns;
begin
  Expect<UInt16>(CANONICAL_FLOAT16_NAN_BITS).ToBe(UInt16($7E00));
  Expect<UInt32>(CANONICAL_FLOAT32_NAN_BITS).ToBe(UInt32($7FC00000));
  Expect<UInt64>(CANONICAL_FLOAT64_NAN_BITS).ToBe(
    UInt64($7FF8000000000000));
end;

begin
  TestRunnerProgram.AddSuite(TNumberBitsTests.Create('NumberBits'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
