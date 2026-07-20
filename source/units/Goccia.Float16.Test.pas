program Goccia.Float16.Test;

{$I Goccia.inc}

uses
  NumberBits,
  TestingPascalLibrary,

  Goccia.Float16;

type
  TFloat16Tests = class(TTestSuite)
  private
    procedure TestAllRawValuesRoundTrip;
    procedure TestCanonicalNaN;
    procedure TestBoundaryRounding;
    procedure TestSignedZero;
  public
    procedure SetupTests; override;
  end;

procedure TFloat16Tests.SetupTests;
begin
  Test('all binary16 values round-trip exactly', TestAllRawValuesRoundTrip);
  Test('NaN serialization uses the canonical binary16 payload', TestCanonicalNaN);
  Test('boundary values round to nearest with ties to even', TestBoundaryRounding);
  Test('signed zero survives both conversions', TestSignedZero);
end;

procedure TFloat16Tests.TestAllRawValuesRoundTrip;
var
  RawValue: UInt32;
  Expected: Word;
begin
  for RawValue := 0 to $FFFF do
  begin
    if ((RawValue and $7C00) = $7C00) and ((RawValue and $03FF) <> 0) then
      Expected := Word(CANONICAL_FLOAT16_NAN_BITS)
    else
      Expected := Word(RawValue);
    Expect<Word>(DoubleToFloat16(Float16ToDouble(Word(RawValue))))
      .ToBe(Expected);
  end;
end;

procedure TFloat16Tests.TestCanonicalNaN;
begin
  Expect<Word>(DoubleToFloat16(BitsToDouble(UInt64($7FF0000000000001))))
    .ToBe(Word(CANONICAL_FLOAT16_NAN_BITS));
  Expect<UInt64>(DoubleToBits(Float16ToDouble(Word($7FFF))))
    .ToBe(CANONICAL_FLOAT64_NAN_BITS);
end;

procedure TFloat16Tests.TestBoundaryRounding;
begin
  Expect<Word>(DoubleToFloat16(1.00048828125)).ToBe(Word($3C00));
  Expect<Word>(DoubleToFloat16(1.00146484375)).ToBe(Word($3C02));
  Expect<Word>(DoubleToFloat16(65520.0)).ToBe(Word($7C00));
  Expect<Word>(DoubleToFloat16(5.9604644775390625E-8)).ToBe(Word($0001));
  Expect<Word>(DoubleToFloat16(2.98023223876953125E-8)).ToBe(Word($0000));
end;

procedure TFloat16Tests.TestSignedZero;
var
  NegativeZero: Double;
begin
  NegativeZero := BitsToDouble(UInt64(1) shl 63);
  Expect<Word>(DoubleToFloat16(NegativeZero)).ToBe(Word($8000));
  Expect<Boolean>(IsNegativeZero(Float16ToDouble(Word($8000)))).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TFloat16Tests.Create('Goccia.Float16'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
