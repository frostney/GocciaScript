program TimeZoneInformationFile.Test;

{$I Shared.inc}

uses
  SysUtils,

  TestingPascalLibrary,
  TimeZoneInformationFile;

type
  TTimeZoneInformationFileTests = class(TTestSuite)
  private
    procedure TestParsesNorthernRule;
    procedure TestParsesQuotedNamesAndExtendedTimes;
    procedure TestParsesRuleWithoutDaylightSaving;
    procedure TestParsesHalfHourOffsets;
    procedure TestRejectsMalformedRules;
    procedure TestNorthernRuleOffsets;
    procedure TestSouthernRuleOffsets;
    procedure TestNegativeDaylightSavingOffsets;
    procedure TestAllYearDaylightSaving;
    procedure TestJulianDaySkipsLeapDay;
    procedure TestZeroBasedDayCountsLeapDay;
    procedure TestNextAndPreviousTransitions;
    procedure TestFarFutureOffset;
    procedure TestReadsFooterRule;
    procedure TestReadsVersionFourFile;
    procedure TestEmptyFooterLeavesNoRule;
  public
    procedure SetupTests; override;
  end;

const
  LOS_ANGELES_RULE = 'PST8PDT,M3.2.0,M11.1.0';
  SYDNEY_RULE = 'AEST-10AEDT,M10.1.0,M4.1.0/3';
  DUBLIN_RULE = 'IST-1GMT0,M10.5.0,M3.5.0/1';
  { RFC 9636 §3.3.1 example: permanently 4 hours west of UTC. }
  ALL_YEAR_DAYLIGHT_RULE = 'XXX3EDT4,0/0,J365/23';
  EPOCH_2038_01_15_NOON = 2147169600;
  EPOCH_2038_07_01_NOON = 2161598400;
  EPOCH_2040_01_01 = 2208988800;
  EPOCH_2040_02_29 = 2214086400;
  EPOCH_2040_03_01 = 2214172800;
  EPOCH_2040_03_11_1000 = 2215072800;
  EPOCH_2040_07_01 = 2224713600;
  EPOCH_2040_11_04_0900 = 2235632400;
  EPOCH_2040_12_31_NOON = 2240568000;
  { 275760-09-13T00:00:00Z, the largest Temporal instant. }
  EPOCH_MAX_INSTANT = 8640000000000;

function ParseRule(const AText: string): TTimeZoneRule;
begin
  if not TryParseTimeZoneRule(AText, Result) then
    raise Exception.Create('Could not parse ' + AText);
end;

{ A TZif file with one local time type, no transitions, the given version
  and, when AHasFooter, a footer holding AFooter between its newlines. }
function BuildTimeZoneInformationFile(const AVersion: Char;
  const AFooter: string; const AHasFooter: Boolean): TBytes;
var
  Bytes: array of Byte;
  Count: Integer;

  procedure Add(const AValue: Byte);
  begin
    Bytes[Count] := AValue;
    Inc(Count);
  end;

  procedure AddInt32(const AValue: Integer);
  begin
    Add(Byte((AValue shr 24) and $FF));
    Add(Byte((AValue shr 16) and $FF));
    Add(Byte((AValue shr 8) and $FF));
    Add(Byte(AValue and $FF));
  end;

  procedure AddBlock;
  var
    I: Integer;
  begin
    Add(Ord('T'));
    Add(Ord('Z'));
    Add(Ord('i'));
    Add(Ord('f'));
    Add(Ord(AVersion));
    for I := 1 to 15 do
      Add(0);
    AddInt32(0); // isutcnt
    AddInt32(0); // isstdcnt
    AddInt32(0); // leapcnt
    AddInt32(0); // timecnt
    AddInt32(1); // typecnt
    AddInt32(4); // charcnt
    AddInt32(-28800); // utoff
    Add(0); // isdst
    Add(0); // desigidx
    Add(Ord('P'));
    Add(Ord('S'));
    Add(Ord('T'));
    Add(0);
  end;

var
  I: Integer;
begin
  SetLength(Bytes, 2 * 64 + Length(AFooter) + 2);
  Count := 0;
  AddBlock;
  AddBlock;
  if AHasFooter then
  begin
    Add(10);
    for I := 1 to Length(AFooter) do
      Add(Ord(AFooter[I]));
    Add(10);
  end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Bytes[I];
end;

procedure TTimeZoneInformationFileTests.SetupTests;
begin
  Test('parses a northern-hemisphere rule', TestParsesNorthernRule);
  Test('parses quoted names and RFC 9636 extended times',
    TestParsesQuotedNamesAndExtendedTimes);
  Test('parses a rule without daylight saving time',
    TestParsesRuleWithoutDaylightSaving);
  Test('parses half-hour offsets', TestParsesHalfHourOffsets);
  Test('rejects malformed rules', TestRejectsMalformedRules);
  Test('a northern rule has daylight saving time in July',
    TestNorthernRuleOffsets);
  Test('a southern rule has daylight saving time in January',
    TestSouthernRuleOffsets);
  Test('a negative daylight-saving rule has its higher offset in summer',
    TestNegativeDaylightSavingOffsets);
  Test('an all-year daylight-saving rule never shows standard time',
    TestAllYearDaylightSaving);
  Test('Jn never counts February 29', TestJulianDaySkipsLeapDay);
  Test('n counts February 29', TestZeroBasedDayCountsLeapDay);
  Test('finds the next and previous rule transitions',
    TestNextAndPreviousTransitions);
  Test('applies the rule at the largest Temporal instant', TestFarFutureOffset);
  Test('reads the footer rule of a TZif file', TestReadsFooterRule);
  Test('reads a version 4 TZif file through its 64-bit block',
    TestReadsVersionFourFile);
  Test('an empty or missing footer leaves no rule', TestEmptyFooterLeavesNoRule);
end;

procedure TTimeZoneInformationFileTests.TestParsesNorthernRule;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule(LOS_ANGELES_RULE);
  Expect<Boolean>(Rule.Present).ToBe(True);
  Expect<Boolean>(Rule.HasDaylightSaving).ToBe(True);
  Expect<Integer>(Rule.StandardOffsetSeconds).ToBe(-28800);
  Expect<Integer>(Rule.DaylightOffsetSeconds).ToBe(-25200);
  Expect<Boolean>(Rule.DaylightStart.Kind = tzrdMonthWeekDay).ToBe(True);
  Expect<Integer>(Rule.DaylightStart.Month).ToBe(3);
  Expect<Integer>(Rule.DaylightStart.Week).ToBe(2);
  Expect<Integer>(Rule.DaylightStart.Day).ToBe(0);
  Expect<Integer>(Rule.DaylightStart.TimeSeconds).ToBe(7200);
  Expect<Integer>(Rule.DaylightEnd.Month).ToBe(11);
end;

procedure TTimeZoneInformationFileTests.TestParsesQuotedNamesAndExtendedTimes;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule('<-03>3<-02>,M3.5.0/-2,M10.5.0/-1');
  Expect<Integer>(Rule.StandardOffsetSeconds).ToBe(-10800);
  Expect<Integer>(Rule.DaylightOffsetSeconds).ToBe(-7200);
  Expect<Integer>(Rule.DaylightStart.TimeSeconds).ToBe(-7200);
  Expect<Integer>(Rule.DaylightEnd.TimeSeconds).ToBe(-3600);

  Rule := ParseRule('<-04>4<-03>,M9.1.6/24,M4.1.6/24');
  Expect<Integer>(Rule.DaylightStart.TimeSeconds).ToBe(86400);
  Expect<Boolean>(TryParseTimeZoneRule('EST5EDT,M3.2.0/167,M11.1.0/-167',
    Rule)).ToBe(True);
end;

procedure TTimeZoneInformationFileTests.TestParsesRuleWithoutDaylightSaving;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule('JST-9');
  Expect<Boolean>(Rule.Present).ToBe(True);
  Expect<Boolean>(Rule.HasDaylightSaving).ToBe(False);
  Expect<Integer>(Rule.StandardOffsetSeconds).ToBe(32400);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_07_01)).ToBe(32400);

  Rule := ParseRule('<+0545>-5:45');
  Expect<Integer>(Rule.StandardOffsetSeconds).ToBe(20700);
end;

procedure TTimeZoneInformationFileTests.TestParsesHalfHourOffsets;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule('<+1030>-10:30<+11>-11,M10.1.0,M4.1.0');
  Expect<Integer>(Rule.StandardOffsetSeconds).ToBe(37800);
  Expect<Integer>(Rule.DaylightOffsetSeconds).ToBe(39600);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_07_01)).ToBe(37800);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_01_01)).ToBe(39600);
end;

procedure TTimeZoneInformationFileTests.TestRejectsMalformedRules;
var
  Rule: TTimeZoneRule;
begin
  Expect<Boolean>(TryParseTimeZoneRule('', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule(':America/New_York', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PS8', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,M3.2.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,M13.2.0,M11.1.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,M3.6.0,M11.1.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,M3.2.7,M11.1.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,J0,M11.1.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,366,M11.1.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,M3.2.0/168,M11.1.0', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST25', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('PST8PDT,M3.2.0,M11.1.0x', Rule)).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneRule('<-03', Rule)).ToBe(False);
  Expect<Boolean>(Rule.Present).ToBe(False);
end;

procedure TTimeZoneInformationFileTests.TestNorthernRuleOffsets;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule(LOS_ANGELES_RULE);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2038_01_15_NOON)).ToBe(-28800);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2038_07_01_NOON)).ToBe(-25200);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_03_11_1000 - 1)).ToBe(-28800);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_03_11_1000)).ToBe(-25200);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_11_04_0900 - 1)).ToBe(-25200);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_11_04_0900)).ToBe(-28800);
end;

procedure TTimeZoneInformationFileTests.TestSouthernRuleOffsets;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule(SYDNEY_RULE);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2038_01_15_NOON)).ToBe(39600);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2038_07_01_NOON)).ToBe(36000);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_12_31_NOON)).ToBe(39600);
end;

procedure TTimeZoneInformationFileTests.TestNegativeDaylightSavingOffsets;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule(DUBLIN_RULE);
  Expect<Integer>(Rule.StandardOffsetSeconds).ToBe(3600);
  Expect<Integer>(Rule.DaylightOffsetSeconds).ToBe(0);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2038_01_15_NOON)).ToBe(0);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2038_07_01_NOON)).ToBe(3600);
end;

procedure TTimeZoneInformationFileTests.TestAllYearDaylightSaving;
var
  Rule: TTimeZoneRule;
  Transition: Int64;
begin
  Rule := ParseRule(ALL_YEAR_DAYLIGHT_RULE);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_01_01)).ToBe(-14400);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_01_01 + 3 * 3600)).ToBe(-14400);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_07_01)).ToBe(-14400);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_2040_12_31_NOON)).ToBe(-14400);
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_07_01, True,
    Transition)).ToBe(False);
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_07_01, False,
    Transition)).ToBe(False);
end;

procedure TTimeZoneInformationFileTests.TestJulianDaySkipsLeapDay;
var
  Rule: TTimeZoneRule;
  Transition: Int64;
begin
  // J60 is March 1 in every year, including leap year 2040.
  Rule := ParseRule('AAA0BBB,J60/0,J300/0');
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_01_01, True,
    Transition)).ToBe(True);
  Expect<Boolean>(Transition = EPOCH_2040_03_01).ToBe(True);
end;

procedure TTimeZoneInformationFileTests.TestZeroBasedDayCountsLeapDay;
var
  Rule: TTimeZoneRule;
  Transition: Int64;
begin
  // Zero-based day 59 is February 29 in leap year 2040.
  Rule := ParseRule('AAA0BBB,59/0,300/0');
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_01_01, True,
    Transition)).ToBe(True);
  Expect<Boolean>(Transition = EPOCH_2040_02_29).ToBe(True);
end;

procedure TTimeZoneInformationFileTests.TestNextAndPreviousTransitions;
var
  Rule: TTimeZoneRule;
  Transition: Int64;
begin
  Rule := ParseRule(LOS_ANGELES_RULE);
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_01_01, True,
    Transition)).ToBe(True);
  Expect<Boolean>(Transition = EPOCH_2040_03_11_1000).ToBe(True);
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_03_11_1000, True,
    Transition)).ToBe(True);
  Expect<Boolean>(Transition = EPOCH_2040_11_04_0900).ToBe(True);
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_11_04_0900, False,
    Transition)).ToBe(True);
  Expect<Boolean>(Transition = EPOCH_2040_03_11_1000).ToBe(True);
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_11_04_0900 + 1, False,
    Transition)).ToBe(True);
  Expect<Boolean>(Transition = EPOCH_2040_11_04_0900).ToBe(True);

  Rule := ParseRule('JST-9');
  Expect<Boolean>(TryGetTimeZoneRuleTransition(Rule, EPOCH_2040_01_01, True,
    Transition)).ToBe(False);
end;

procedure TTimeZoneInformationFileTests.TestFarFutureOffset;
var
  Rule: TTimeZoneRule;
begin
  Rule := ParseRule(LOS_ANGELES_RULE);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_MAX_INSTANT)).ToBe(-25200);
  Rule := ParseRule(SYDNEY_RULE);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, EPOCH_MAX_INSTANT)).ToBe(36000);
  Expect<Integer>(TimeZoneRuleOffsetSeconds(Rule, -EPOCH_MAX_INSTANT)).ToBe(36000);
end;

procedure TTimeZoneInformationFileTests.TestReadsFooterRule;
var
  Data: TTimeZoneInformationData;
begin
  Expect<Boolean>(TryParseTimeZoneInformationFile('Test/Zone',
    BuildTimeZoneInformationFile('2', LOS_ANGELES_RULE, True), Data)).ToBe(True);
  Expect<Boolean>(Data.Rule.Present).ToBe(True);
  Expect<Integer>(Data.Rule.DaylightOffsetSeconds).ToBe(-25200);
  Expect<Integer>(Length(Data.Transitions)).ToBe(0);
  Expect<Integer>(Data.DefaultOffsetSeconds).ToBe(-28800);
end;

procedure TTimeZoneInformationFileTests.TestReadsVersionFourFile;
var
  Data: TTimeZoneInformationData;
begin
  Expect<Boolean>(TryParseTimeZoneInformationFile('Test/Zone',
    BuildTimeZoneInformationFile('4', LOS_ANGELES_RULE, True), Data)).ToBe(True);
  Expect<Boolean>(Data.Rule.Present).ToBe(True);
end;

procedure TTimeZoneInformationFileTests.TestEmptyFooterLeavesNoRule;
var
  Data: TTimeZoneInformationData;
begin
  Expect<Boolean>(TryParseTimeZoneInformationFile('Test/Zone',
    BuildTimeZoneInformationFile('2', '', True), Data)).ToBe(True);
  Expect<Boolean>(Data.Rule.Present).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneInformationFile('Test/Zone',
    BuildTimeZoneInformationFile('2', '', False), Data)).ToBe(True);
  Expect<Boolean>(Data.Rule.Present).ToBe(False);
  Expect<Boolean>(TryParseTimeZoneInformationFile('Test/Zone',
    BuildTimeZoneInformationFile('2', 'PST8PDT,M3.2.0', True), Data)).ToBe(True);
  Expect<Boolean>(Data.Rule.Present).ToBe(False);
end;

begin
  TestRunnerProgram.AddSuite(TTimeZoneInformationFileTests.Create(
    'TimeZoneInformationFile'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
