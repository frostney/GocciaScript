program Goccia.OriginMap.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.GarbageCollector,
  Goccia.OriginMap,
  Goccia.TestSetup;

type
  TOriginMapTests = class(TTestSuite)
  private
    procedure TestExactInsideRun;
    procedure TestContiguousCopiesMerge;
    procedure TestNonContiguousCopiesDoNot;
    procedure TestBoundaryStartTakesWhatFollows;
    procedure TestBoundaryEndTakesWhatPrecedes;
    procedure TestGapWidensOutward;
    procedure TestGapAtTheEndReachesTheOriginalLength;
    procedure TestBeforeTheFirstRun;
    procedure TestAnchorGivesAnExactEdge;
    procedure TestRedundantAnchorIsDropped;
    procedure TestAppendShiftedRebases;
    procedure TestEmptyMapMapsNothing;
    procedure TestResetDetachesStorage;
  public
    procedure SetupTests; override;
  end;

procedure TOriginMapTests.SetupTests;
begin
  Test('An offset inside a copied run is exact', TestExactInsideRun);
  Test('Contiguous copies merge into one run', TestContiguousCopiesMerge);
  Test('Copies that skip input do not merge', TestNonContiguousCopiesDoNot);
  Test('A shared boundary starts at what follows',
    TestBoundaryStartTakesWhatFollows);
  Test('A shared boundary ends at what precedes',
    TestBoundaryEndTakesWhatPrecedes);
  Test('An offset in a gap widens outward', TestGapWidensOutward);
  Test('A gap at the end reaches the original length',
    TestGapAtTheEndReachesTheOriginalLength);
  Test('An offset before the first run answers the start of the file',
    TestBeforeTheFirstRun);
  Test('An anchor makes a rewritten construct edge exact',
    TestAnchorGivesAnExactEdge);
  Test('An anchor that repeats the run before it is dropped',
    TestRedundantAnchorIsDropped);
  Test('AppendShifted rebases another list', TestAppendShiftedRebases);
  Test('An empty map maps nothing', TestEmptyMapMapsNothing);
  Test('Reset detaches storage rather than emptying it',
    TestResetDetachesStorage);
end;

// Generated `ab__cd` from original `abcd`: two copies with a synthesized gap.
function TwoRunsWithAGap: TGocciaOriginMap;
var
  Runs: TGocciaOriginRuns;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 0, 2);
  Runs.NoteCopy(4, 2, 2);
  Result := TGocciaOriginMap.Create(Runs, 4);
end;

procedure TOriginMapTests.TestExactInsideRun;
var
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Map := TwoRunsWithAGap;
  try
    Expect<Boolean>(Map.Map(1, obStart, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(1);
    Expect<Boolean>(Map.Map(5, obEnd, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(3);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestContiguousCopiesMerge;
var
  Runs: TGocciaOriginRuns;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 10, 1);
  Runs.NoteCopy(1, 11, 1);
  Runs.NoteCopy(2, 12, 1);
  Expect<Integer>(Runs.Count).ToBe(1);
  Expect<Integer>(Runs.Items[0].Length).ToBe(3);
end;

procedure TOriginMapTests.TestNonContiguousCopiesDoNot;
var
  Runs: TGocciaOriginRuns;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 0, 2);
  // Generated continues, original skips: the transformer dropped something.
  Runs.NoteCopy(2, 5, 2);
  Expect<Integer>(Runs.Count).ToBe(2);
end;

procedure TOriginMapTests.TestBoundaryStartTakesWhatFollows;
var
  Runs: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 0, 2);
  Runs.NoteCopy(2, 7, 2);
  Map := TGocciaOriginMap.Create(Runs, 9);
  try
    // Generated offset 2 ends the first run and begins the second. A range
    // that starts there starts at the second.
    Expect<Boolean>(Map.Map(2, obStart, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(7);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestBoundaryEndTakesWhatPrecedes;
var
  Runs: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 0, 2);
  Runs.NoteCopy(2, 7, 2);
  Map := TGocciaOriginMap.Create(Runs, 9);
  try
    Expect<Boolean>(Map.Map(2, obEnd, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(2);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestGapWidensOutward;
var
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Map := TwoRunsWithAGap;
  try
    // Generated offset 3 is inside the synthesized `__`, which has no
    // original text. A start rounds back to where copying stopped, an end
    // forward to where it resumed, so the range covers the construct.
    Expect<Boolean>(Map.Map(3, obStart, Original)).ToBe(False);
    Expect<Integer>(Original).ToBe(2);
    Expect<Boolean>(Map.Map(3, obEnd, Original)).ToBe(False);
    Expect<Integer>(Original).ToBe(2);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestGapAtTheEndReachesTheOriginalLength;
var
  Runs: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 0, 2);
  Map := TGocciaOriginMap.Create(Runs, 9);
  try
    Expect<Boolean>(Map.Map(6, obEnd, Original)).ToBe(False);
    Expect<Integer>(Original).ToBe(9);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestBeforeTheFirstRun;
var
  Runs: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Runs.Reset;
  Runs.NoteCopy(4, 4, 2);
  Map := TGocciaOriginMap.Create(Runs, 9);
  try
    Expect<Boolean>(Map.Map(1, obStart, Original)).ToBe(False);
    Expect<Integer>(Original).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestAnchorGivesAnExactEdge;
var
  Runs: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Runs.Reset;
  // `<div/>` at original 0..6 became 24 characters of factory call, then a
  // copied `;`. Both edges of the rewrite are anchored.
  Runs.NoteAnchor(0, 0);
  Runs.NoteAnchor(24, 6);
  Runs.NoteCopy(24, 6, 1);
  Map := TGocciaOriginMap.Create(Runs, 7);
  try
    Expect<Boolean>(Map.Map(0, obStart, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(0);
    Expect<Boolean>(Map.Map(24, obEnd, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(6);
    Expect<Boolean>(Map.Map(25, obEnd, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(7);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestRedundantAnchorIsDropped;
var
  Runs: TGocciaOriginRuns;
begin
  Runs.Reset;
  Runs.NoteCopy(0, 0, 4);
  Runs.NoteAnchor(4, 4);
  Expect<Integer>(Runs.Count).ToBe(1);
end;

procedure TOriginMapTests.TestAppendShiftedRebases;
var
  Inner, Outer: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Inner.Reset;
  Inner.NoteCopy(0, 0, 3);

  Outer.Reset;
  Outer.NoteCopy(0, 0, 10);
  Outer.AppendShifted(Inner, 20, 40);

  Map := TGocciaOriginMap.Create(Outer, 60);
  try
    Expect<Integer>(Outer.Count).ToBe(2);
    Expect<Boolean>(Map.Map(21, obStart, Original)).ToBe(True);
    Expect<Integer>(Original).ToBe(41);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestEmptyMapMapsNothing;
var
  Runs: TGocciaOriginRuns;
  Map: TGocciaOriginMap;
  Original: Integer;
begin
  Runs.Reset;
  Map := TGocciaOriginMap.Create(Runs, 0);
  try
    Expect<Boolean>(Map.Map(3, obStart, Original)).ToBe(False);
    Expect<Integer>(Original).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TOriginMapTests.TestResetDetachesStorage;
var
  Held, Building: TGocciaOriginRuns;
begin
  Building.Reset;
  Building.NoteCopy(0, 100, 4);

  // The array is reference counted and not copied on write. Handing it on and
  // then continuing to build must not rewrite what was handed on.
  Held := Building;
  Building.Reset;
  Building.NoteCopy(0, 200, 4);

  Expect<Integer>(Held.Count).ToBe(1);
  Expect<Integer>(Held.Items[0].OriginalStart).ToBe(100);
  Expect<Integer>(Building.Items[0].OriginalStart).ToBe(200);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TOriginMapTests.Create('OriginMap'));
    RunGocciaTests;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
