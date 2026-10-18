program Goccia.Bytecode.Debug.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,
  TimingUtils,

  Goccia.Bytecode.Debug,
  Goccia.TestSetup;

type
  TDebugInfoTests = class(TTestSuite)
  private
    procedure TestPositionIsTheLastEntryAtOrBeforeThePC;
    procedure TestPositionBeforeTheFirstEntryIsZero;
    procedure TestEntriesOutOfOrderStillResolve;
    procedure TestLookupsTakeLogarithmicTime;
  public
    procedure SetupTests; override;
  end;

procedure TDebugInfoTests.SetupTests;
begin
  Test('A PC resolves to the last entry at or before it',
    TestPositionIsTheLastEntryAtOrBeforeThePC);
  Test('A PC before the first entry has no position',
    TestPositionBeforeTheFirstEntryIsZero);
  Test('Entries added out of PC order still resolve',
    TestEntriesOutOfOrderStillResolve);
  Test('Position lookups take time logarithmic in the map',
    TestLookupsTakeLogarithmicTime);
end;

procedure TDebugInfoTests.TestPositionIsTheLastEntryAtOrBeforeThePC;
var
  Info: TGocciaDebugInfo;
begin
  Info := TGocciaDebugInfo.Create('<test>');
  try
    Info.AddLineMapping(0, 1, 1);
    Info.AddLineMapping(0, 2, 5);
    Info.AddLineMapping(4, 3, 7);
    Info.AddLineMapping(9, 4, 2);
    Info.AddLineMapping(9, 5, 3);

    // The second entry at PC 0 is kept beside the first, and a lookup at
    // that PC reaches the later one.
    Expect<Integer>(Info.LineMapCount).ToBe(4);
    Expect<Cardinal>(Info.GetLineForPC(0)).ToBe(2);
    Expect<Word>(Info.GetColumnForPC(0)).ToBe(5);
    Expect<Cardinal>(Info.GetLineForPC(3)).ToBe(2);
    Expect<Cardinal>(Info.GetLineForPC(4)).ToBe(3);
    Expect<Word>(Info.GetColumnForPC(8)).ToBe(7);
    // The last entry at a PC supersedes the one before it.
    Expect<Cardinal>(Info.GetLineForPC(9)).ToBe(5);
    Expect<Word>(Info.GetColumnForPC(9)).ToBe(3);
    Expect<Cardinal>(Info.GetLineForPC(1000)).ToBe(5);
  finally
    Info.Free;
  end;
end;

procedure TDebugInfoTests.TestPositionBeforeTheFirstEntryIsZero;
var
  Info: TGocciaDebugInfo;
begin
  Info := TGocciaDebugInfo.Create('<test>');
  try
    Expect<Cardinal>(Info.GetLineForPC(0)).ToBe(0);
    Info.AddLineMapping(3, 7, 2);
    Expect<Cardinal>(Info.GetLineForPC(2)).ToBe(0);
    Expect<Word>(Info.GetColumnForPC(2)).ToBe(0);
    Expect<Cardinal>(Info.GetLineForPC(3)).ToBe(7);
  finally
    Info.Free;
  end;
end;

{ A map whose PCs go back (bytecode loaded from a file can hold one) is read
  as before the lookup was a binary search: the newest entry at or before
  the PC wins. }
procedure TDebugInfoTests.TestEntriesOutOfOrderStillResolve;
var
  Info: TGocciaDebugInfo;
begin
  Info := TGocciaDebugInfo.Create('<test>');
  try
    Info.AddLineMapping(0, 1, 1);
    Info.AddLineMapping(10, 2, 1);
    Info.AddLineMapping(5, 3, 1);
    Info.AddLineMapping(20, 4, 1);

    Expect<Cardinal>(Info.GetLineForPC(4)).ToBe(1);
    Expect<Cardinal>(Info.GetLineForPC(7)).ToBe(3);
    Expect<Cardinal>(Info.GetLineForPC(12)).ToBe(3);
    Expect<Cardinal>(Info.GetLineForPC(25)).ToBe(4);
  finally
    Info.Free;
  end;
end;

{ Every instruction of a function looking up its own position once, as a
  module of many import declarations does, against building the map on the
  same machine. A scan per lookup would take about ENTRY_COUNT / 2 steps
  each, thousands of times the build; a binary search takes about
  log2(ENTRY_COUNT). }
procedure TDebugInfoTests.TestLookupsTakeLogarithmicTime;
const
  ENTRY_COUNT = 50000;
  LOOKUP_TO_BUILD_RATIO = 50;
  SLACK_NANOSECONDS = 20 * 1000 * 1000;
var
  Info: TGocciaDebugInfo;
  BuildTime, LookupTime, Start: Int64;
  I: Integer;
  Checksum: Int64;
begin
  Info := TGocciaDebugInfo.Create('<test>');
  try
    Start := GetNanoseconds;
    for I := 0 to ENTRY_COUNT - 1 do
      Info.AddLineMapping(UInt32(I) * 3, UInt32(I) + 1, 1);
    BuildTime := GetNanoseconds - Start;

    Checksum := 0;
    Start := GetNanoseconds;
    for I := 0 to ENTRY_COUNT - 1 do
      Inc(Checksum, Info.GetLineForPC(UInt32(I) * 3 + 1));
    LookupTime := GetNanoseconds - Start;

    Expect<Int64>(Checksum).ToBe(Int64(ENTRY_COUNT) * (ENTRY_COUNT + 1) div 2);
    if LookupTime > LOOKUP_TO_BUILD_RATIO * BuildTime + SLACK_NANOSECONDS then
      Fail(Format('%d lookups took %d ms against %d ms to build the map',
        [ENTRY_COUNT, LookupTime div 1000000, BuildTime div 1000000]));
    Expect<Boolean>(LookupTime <=
      LOOKUP_TO_BUILD_RATIO * BuildTime + SLACK_NANOSECONDS).ToBe(True);
  finally
    Info.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TDebugInfoTests.Create('Bytecode debug info'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
