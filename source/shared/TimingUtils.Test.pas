program TimingUtils.Test;

{$I Shared.inc}

uses
  SysUtils,

  TestingPascalLibrary,
  TimingUtils;

type
  TTimingUtilsTests = class(TTestSuite)
  private
    procedure TestGetNanosecondsIsPositive;
    procedure TestGetMillisecondsIsPositive;
    procedure TestGetNanosecondsIsMonotonic;
    procedure TestGetNanosecondsResolvesBelowAMicrosecond;
    procedure TestGetMillisecondsConsistentWithNanoseconds;
    procedure TestGetEpochNanosecondsIsPositive;
    procedure TestGetEpochNanosecondsIsInReasonableRange;
    procedure TestFormatDurationZeroNs;
    procedure TestFormatDurationSmallNs;
    procedure TestFormatDurationMicroseconds;
    procedure TestFormatDurationMilliseconds;
    procedure TestFormatDurationSeconds;
    procedure TestFormatDurationBelowMicrosecondThreshold;
    procedure TestFormatDurationAtMicrosecondThreshold;
    procedure TestFormatDurationBelowMillisecondThreshold;
    procedure TestFormatDurationAtMillisecondThreshold;
    procedure TestFormatDurationBelowSecondThreshold;
    procedure TestFormatDurationAtSecondThreshold;
    procedure TestFormatDurationLargeValue;
    procedure TestFormatDurationMatchesFormat;
  public
    procedure SetupTests; override;
  end;

procedure TTimingUtilsTests.SetupTests;
begin
  Test('GetNanoseconds returns positive value', TestGetNanosecondsIsPositive);
  Test('GetMilliseconds returns positive value', TestGetMillisecondsIsPositive);
  Test('GetNanoseconds is monotonically non-decreasing', TestGetNanosecondsIsMonotonic);
  Test('GetNanoseconds resolves intervals shorter than a microsecond on macOS',
    TestGetNanosecondsResolvesBelowAMicrosecond);
  Test('GetMilliseconds is consistent with GetNanoseconds', TestGetMillisecondsConsistentWithNanoseconds);
  Test('GetEpochNanoseconds returns positive value', TestGetEpochNanosecondsIsPositive);
  Test('GetEpochNanoseconds is in a reasonable range (after 2020)', TestGetEpochNanosecondsIsInReasonableRange);
  Test('FormatDuration with 0 ns returns 0ns', TestFormatDurationZeroNs);
  Test('FormatDuration with small ns value', TestFormatDurationSmallNs);
  Test('FormatDuration with microsecond range', TestFormatDurationMicroseconds);
  Test('FormatDuration with millisecond range', TestFormatDurationMilliseconds);
  Test('FormatDuration with seconds range', TestFormatDurationSeconds);
  Test('FormatDuration below microsecond threshold stays in ns', TestFormatDurationBelowMicrosecondThreshold);
  Test('FormatDuration at microsecond threshold (500 ns)', TestFormatDurationAtMicrosecondThreshold);
  Test('FormatDuration below millisecond threshold stays in us', TestFormatDurationBelowMillisecondThreshold);
  Test('FormatDuration at millisecond threshold (500000 ns)', TestFormatDurationAtMillisecondThreshold);
  Test('FormatDuration below second threshold stays in ms', TestFormatDurationBelowSecondThreshold);
  Test('FormatDuration at second threshold (10s)', TestFormatDurationAtSecondThreshold);
  Test('FormatDuration with large value (60s)', TestFormatDurationLargeValue);
  Test('FormatDuration writes what Format writes for the same Double',
    TestFormatDurationMatchesFormat);
end;

// The Format-based formatting FormatDuration reproduces without Format,
// with the same Double arithmetic.
function ReferenceFormatDuration(const ANanoseconds: Int64): string;
var
  Us, Ms, S: Double;
begin
  Us := ANanoseconds / 1000.0;
  Ms := Us / 1000.0;
  S := Ms / 1000.0;
  if S >= 10.0 then
    Result := SysUtils.Format('%.2fs', [S])
  else if Ms >= 0.5 then
    Result := SysUtils.Format('%.2fms', [Ms])
  else if Us >= 0.5 then
    Result := SysUtils.Format('%.2f' + #$00B5 + 's', [Us])
  else
    Result := SysUtils.Format('%dns', [ANanoseconds]);
end;

procedure TTimingUtilsTests.TestGetNanosecondsIsPositive;
begin
  Expect<Boolean>(GetNanoseconds > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestGetMillisecondsIsPositive;
begin
  Expect<Boolean>(GetMilliseconds > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestGetNanosecondsIsMonotonic;
var
  A, B: Int64;
begin
  A := GetNanoseconds;
  B := GetNanoseconds;
  Expect<Boolean>(B >= A).ToBe(True);
end;

procedure TTimingUtilsTests.TestGetNanosecondsResolvesBelowAMicrosecond;
{$IFDEF DARWIN}
const
  READINGS = 100000;
  NANOSECONDS_PER_MICROSECOND = 1000;
var
  I: Integer;
  Previous, Current, Elapsed: Int64;
  FinerThanMicrosecond: Boolean;
begin
  // A benchmark sample is the difference of two readings. On a clock that
  // advances in whole microseconds every difference is a multiple of 1000 ns,
  // whatever offset the readings carry, so a sample shorter than a
  // microsecond measures as 0 or 1000 ns. Darwin's CLOCK_MONOTONIC is such a
  // clock; the one GetNanoseconds reads there must not be.
  FinerThanMicrosecond := False;
  Previous := GetNanoseconds;
  for I := 1 to READINGS do
  begin
    Current := GetNanoseconds;
    Elapsed := Current - Previous;
    if Elapsed mod NANOSECONDS_PER_MICROSECOND <> 0 then
    begin
      FinerThanMicrosecond := True;
      Break;
    end;
    Previous := Current;
  end;
  Expect<Boolean>(FinerThanMicrosecond).ToBe(True);
end;
{$ELSE}
begin
  // The clocks read on Linux and Windows are not chosen by this unit for
  // their resolution, which depends on the system, so nothing is required of
  // them here.
  Expect<Boolean>(GetNanoseconds > 0).ToBe(True);
end;
{$ENDIF}

procedure TTimingUtilsTests.TestGetMillisecondsConsistentWithNanoseconds;
var
  Ms, Ns: Int64;
begin
  // GetMilliseconds should be approximately GetNanoseconds / 1_000_000
  // We get ms first, then ns; ms value should not exceed ns/1_000_000 by much
  Ms := GetMilliseconds;
  Ns := GetNanoseconds;
  // Since ns is sampled after ms, ns/1_000_000 >= ms (modulo small timing)
  Expect<Boolean>(Ns div 1000000 >= Ms - 1).ToBe(True);
end;

procedure TTimingUtilsTests.TestGetEpochNanosecondsIsPositive;
begin
  Expect<Boolean>(GetEpochNanoseconds > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestGetEpochNanosecondsIsInReasonableRange;
var
  EpochNs: Int64;
  // 2020-01-01T00:00:00Z in nanoseconds since Unix epoch
  // 1577836800 seconds * 1_000_000_000
  MinEpochNs: Int64;
begin
  MinEpochNs := Int64(1577836800) * Int64(1000000000);
  EpochNs := GetEpochNanoseconds;
  Expect<Boolean>(EpochNs > MinEpochNs).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationZeroNs;
begin
  Expect<string>(FormatDuration(0)).ToBe('0ns');
end;

procedure TTimingUtilsTests.TestFormatDurationSmallNs;
begin
  Expect<string>(FormatDuration(42)).ToBe('42ns');
end;

procedure TTimingUtilsTests.TestFormatDurationMicroseconds;
var
  S: string;
begin
  // 1500 ns = 1.5 us => microsecond range
  S := FormatDuration(1500);
  Expect<Boolean>(Pos(#$00B5 + 's', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationMilliseconds;
var
  S: string;
begin
  // 5_000_000 ns = 5 ms
  S := FormatDuration(5000000);
  Expect<Boolean>(Pos('ms', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationSeconds;
var
  S: string;
begin
  // 15_000_000_000 ns = 15 s => should show "s" but not as part of "ms" or "us"
  S := FormatDuration(Int64(15000000000));
  Expect<Boolean>(Pos('s', S) > 0).ToBe(True);
  Expect<Boolean>(Pos('ms', S) = 0).ToBe(True);
  Expect<Boolean>(Pos(#$00B5, S) = 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationBelowMicrosecondThreshold;
begin
  // 499 ns => Us = 0.499, below 0.5 threshold => stays in ns format
  Expect<string>(FormatDuration(499)).ToBe('499ns');
end;

procedure TTimingUtilsTests.TestFormatDurationAtMicrosecondThreshold;
var
  S: string;
begin
  // 500 ns => Us = 0.5, exactly at threshold => microsecond format
  S := FormatDuration(500);
  Expect<Boolean>(Pos(#$00B5 + 's', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationBelowMillisecondThreshold;
var
  S: string;
begin
  // 499_999 ns => Ms = 0.499999, below 0.5 => stays in microsecond format
  S := FormatDuration(499999);
  Expect<Boolean>(Pos(#$00B5 + 's', S) > 0).ToBe(True);
  Expect<Boolean>(Pos('ms', S) = 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationAtMillisecondThreshold;
var
  S: string;
begin
  // 500_000 ns => Ms = 0.5, exactly at threshold => millisecond format
  S := FormatDuration(500000);
  Expect<Boolean>(Pos('ms', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationBelowSecondThreshold;
var
  S: string;
begin
  // 9_990_000_000 ns => S = 9.99 < 10.0 => stays in ms format
  S := FormatDuration(Int64(9990000000));
  Expect<Boolean>(Pos('ms', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationAtSecondThreshold;
var
  S: string;
begin
  // 10_000_000_000 ns => S = 10.0 => seconds format
  S := FormatDuration(Int64(10000000000));
  Expect<Boolean>(Pos('ms', S) = 0).ToBe(True);
  Expect<Boolean>(Pos(#$00B5, S) = 0).ToBe(True);
  Expect<Boolean>(Pos('s', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationLargeValue;
var
  S: string;
begin
  // 60_000_000_000 ns = 60 s
  S := FormatDuration(Int64(60000000000));
  Expect<Boolean>(Pos('s', S) > 0).ToBe(True);
  Expect<Boolean>(Pos('60.00s', S) > 0).ToBe(True);
end;

procedure TTimingUtilsTests.TestFormatDurationMatchesFormat;
const
  // Unit boundaries, values either side of a rounding tie in each unit, and
  // ties themselves, which FormatDuration hands to Format.
  SAMPLES: array[0..37] of Int64 = (
    -1500, -1, 0, 1, 499, 500, 501, 504, 505, 506, 994, 995, 996, 1004, 1005,
    9995, 99995, 499994, 499995, 499999, 500000, 504999, 505000, 505001,
    1004999, 1005000, 1005001, 9995000, 9999994999, 9999995000, 9999999999,
    10000000000, 10004999999, 10005000000, 10005000001, 99999995000000,
    100000000000000, 123456789012345678);
var
  I: Integer;
  Value: Int64;
  Mismatches: Integer;
begin
  Mismatches := 0;
  for I := Low(SAMPLES) to High(SAMPLES) do
    if FormatDuration(SAMPLES[I]) <> ReferenceFormatDuration(SAMPLES[I]) then
      Inc(Mismatches);
  // A deterministic sweep across every unit.
  Value := 1;
  while Value < Int64(200000000000000) do
  begin
    if FormatDuration(Value) <> ReferenceFormatDuration(Value) then
      Inc(Mismatches);
    Value := Value * 3 + 7;
  end;
  // Every value through the microsecond unit and into milliseconds, where the
  // unit values carry the precision of the 1000.0 constants.
  for I := 0 to 1100000 do
    if FormatDuration(I) <> ReferenceFormatDuration(I) then
      Inc(Mismatches);
  for I := 0 to 20000 do
    if FormatDuration(Int64(I) * 49999) <>
       ReferenceFormatDuration(Int64(I) * 49999) then
      Inc(Mismatches);
  Expect<Integer>(Mismatches).ToBe(0);
end;

begin
  TestRunnerProgram.AddSuite(TTimingUtilsTests.Create('TimingUtils'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
