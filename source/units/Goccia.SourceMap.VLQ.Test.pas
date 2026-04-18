program Goccia.SourceMap.VLQ.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.SourceMap.VLQ;

type
  TVLQTests = class(TTestSuite)
  private
    procedure TestEncodeZero;
    procedure TestEncodePositiveSmall;
    procedure TestEncodeNegativeSmall;
    procedure TestEncodeLargePositive;
    procedure TestEncodeLargeNegative;
    procedure TestDecodeKnownVectors;
    procedure TestRoundTripValues;
    procedure TestDecodeMultipleValues;
  public
    procedure SetupTests; override;
  end;

procedure TVLQTests.SetupTests;
begin
  Test('Encode zero produces "A"', TestEncodeZero);
  Test('Encode small positive values', TestEncodePositiveSmall);
  Test('Encode small negative values', TestEncodeNegativeSmall);
  Test('Encode large positive values', TestEncodeLargePositive);
  Test('Encode large negative values', TestEncodeLargeNegative);
  Test('Decode known spec test vectors', TestDecodeKnownVectors);
  Test('Round-trip encode/decode for range of values', TestRoundTripValues);
  Test('Decode multiple concatenated values', TestDecodeMultipleValues);
end;

// TC39 Source Map Spec: 0 encodes as 'A' (sextet 0b000000)
procedure TVLQTests.TestEncodeZero;
begin
  Expect<string>(EncodeVLQ(0)).ToBe('A');
end;

// TC39 Source Map Spec: 1 encodes as 'C' (sextet 0b000010)
// 2 encodes as 'E', 15 encodes as 'e'
procedure TVLQTests.TestEncodePositiveSmall;
begin
  Expect<string>(EncodeVLQ(1)).ToBe('C');
  Expect<string>(EncodeVLQ(2)).ToBe('E');
  Expect<string>(EncodeVLQ(15)).ToBe('e');
end;

// TC39 Source Map Spec: -1 encodes as 'D' (sextet 0b000011, sign bit set)
procedure TVLQTests.TestEncodeNegativeSmall;
begin
  Expect<string>(EncodeVLQ(-1)).ToBe('D');
  Expect<string>(EncodeVLQ(-2)).ToBe('F');
  Expect<string>(EncodeVLQ(-15)).ToBe('f');
end;

// 16 requires two sextets: continuation bit + overflow
// TC39 Source Map Spec: 16 encodes as 'gB'
procedure TVLQTests.TestEncodeLargePositive;
begin
  Expect<string>(EncodeVLQ(16)).ToBe('gB');
  Expect<string>(EncodeVLQ(100)).ToBe('oG');
  Expect<string>(EncodeVLQ(1000)).ToBe('w+B');
end;

procedure TVLQTests.TestEncodeLargeNegative;
begin
  Expect<string>(EncodeVLQ(-16)).ToBe('hB');
  Expect<string>(EncodeVLQ(-100)).ToBe('pG');
end;

procedure TVLQTests.TestDecodeKnownVectors;
var
  Pos: Integer;
begin
  // 'A' = 0
  Pos := 1;
  Expect<Integer>(DecodeVLQ('A', Pos)).ToBe(0);
  Expect<Integer>(Pos).ToBe(2);

  // 'C' = 1
  Pos := 1;
  Expect<Integer>(DecodeVLQ('C', Pos)).ToBe(1);
  Expect<Integer>(Pos).ToBe(2);

  // 'D' = -1
  Pos := 1;
  Expect<Integer>(DecodeVLQ('D', Pos)).ToBe(-1);
  Expect<Integer>(Pos).ToBe(2);

  // 'gB' = 16 (two sextets)
  Pos := 1;
  Expect<Integer>(DecodeVLQ('gB', Pos)).ToBe(16);
  Expect<Integer>(Pos).ToBe(3);
end;

// Round-trip: encode then decode should return the original value
procedure TVLQTests.TestRoundTripValues;
var
  I, Pos: Integer;
  Encoded: string;
begin
  for I := -500 to 500 do
  begin
    Encoded := EncodeVLQ(I);
    Pos := 1;
    Expect<Integer>(DecodeVLQ(Encoded, Pos)).ToBe(I);
  end;
end;

// Decoding should correctly advance the position through multiple values
procedure TVLQTests.TestDecodeMultipleValues;
var
  Pos: Integer;
  Combined: string;
begin
  Combined := EncodeVLQ(0) + EncodeVLQ(5) + EncodeVLQ(-3) + EncodeVLQ(100);
  Pos := 1;
  Expect<Integer>(DecodeVLQ(Combined, Pos)).ToBe(0);
  Expect<Integer>(DecodeVLQ(Combined, Pos)).ToBe(5);
  Expect<Integer>(DecodeVLQ(Combined, Pos)).ToBe(-3);
  Expect<Integer>(DecodeVLQ(Combined, Pos)).ToBe(100);
  Expect<Integer>(Pos).ToBe(Length(Combined) + 1);
end;

begin
  TestRunnerProgram.AddSuite(TVLQTests.Create('SourceMap VLQ'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
