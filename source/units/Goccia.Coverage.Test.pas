program Goccia.Coverage.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Coverage,
  Goccia.TestSetup;

type
  TTestCoverage = class(TTestSuite)
  private
    function MakeWorker(const AFilePath: string): TGocciaCoverageTracker;
  public
    procedure SetupTests; override;

    procedure TestMergeSumsStatementHitsByLocation;
    procedure TestMergeSumsBranchHitCounts;
    procedure TestMergeSumsFunctionHitCounts;
    procedure TestMergeRetainsZeroHitSites;
    procedure TestMergeAdoptsUnknownFiles;
    procedure TestRegisteringASiteTwiceReturnsOneIndex;
    procedure TestLineCountIsTheMostAmongStatementsStartingOnIt;
    procedure TestLinesAreOnlyThoseAStatementStartsOn;
    procedure TestBranchPathsCountAsFoundAndHit;
    procedure TestOrderFollowsSourcePositionsEnclosingFirst;
    procedure TestVirtualPathsAreLeftVerbatim;
    procedure TestCanonicalPathUsesForwardSlashes;
    procedure TestEntryAndImportSpellingsShareOneRecord;
    procedure TestResolvedSourcePathSurvivesCanonicalization;
    procedure TestCanonicalPathIsRepoRelativeUnderARepository;
  end;

const
  WORKER_FILE = 'shared.js';

function Loc(const AStartLine, AStartColumn, AEndLine, AEndColumn: Integer): TGocciaCoverageLoc;
begin
  Result.StartLine := AStartLine;
  Result.StartColumn := AStartColumn;
  Result.EndLine := AEndLine;
  Result.EndColumn := AEndColumn;
end;

function Locs2(const A, B: TGocciaCoverageLoc): TGocciaCoverageLocArray;
begin
  SetLength(Result, 2);
  Result[0] := A;
  Result[1] := B;
end;

procedure TTestCoverage.SetupTests;
begin
  Test('MergeFrom sums statement hits by location, not by index',
    TestMergeSumsStatementHitsByLocation);
  Test('MergeFrom sums branch hit counts', TestMergeSumsBranchHitCounts);
  Test('MergeFrom sums function hit counts', TestMergeSumsFunctionHitCounts);
  Test('MergeFrom retains zero-hit statements, branch paths and functions',
    TestMergeRetainsZeroHitSites);
  Test('MergeFrom adopts unknown files', TestMergeAdoptsUnknownFiles);
  Test('registering a site twice returns one index',
    TestRegisteringASiteTwiceReturnsOneIndex);
  Test('a line counts the most among the statements starting on it',
    TestLineCountIsTheMostAmongStatementsStartingOnIt);
  Test('only lines a statement starts on are listed',
    TestLinesAreOnlyThoseAStatementStartsOn);
  Test('every path of a branch is found, and hit when its count is above 0',
    TestBranchPathsCountAsFoundAndHit);
  Test('sites are ordered by source position, an enclosing site first',
    TestOrderFollowsSourcePositionsEnclosingFirst);
  Test('virtual paths are left verbatim', TestVirtualPathsAreLeftVerbatim);
  Test('canonical paths use forward slashes',
    TestCanonicalPathUsesForwardSlashes);
  Test('entry and import spellings share one record',
    TestEntryAndImportSpellingsShareOneRecord);
  Test('resolved source path survives canonicalization',
    TestResolvedSourcePathSurvivesCanonicalization);
  Test('canonical path is repo-relative under a repository',
    TestCanonicalPathIsRepoRelativeUnderARepository);
end;

{ A temporary file the canonicalizer can actually stat.  Returned as the
  absolute path; callers derive the other spellings they need from it.

  Deliberately created under the working directory rather than the system
  temp directory: canonicalization is textual (ExpandFileName), so it does
  not collapse spellings that differ by a symlinked ancestor, and on macOS
  the temp directory is reached through the /var -> /private/var symlink. }
function MakeTempSourceFile: string;
var
  Lines: TextFile;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) +
    Format('goccia-cov-%d.js', [Random(1000000)]);
  AssignFile(Lines, Result);
  Rewrite(Lines);
  try
    WriteLn(Lines, 'const a = 1;');
  finally
    CloseFile(Lines);
  end;
end;

function TTestCoverage.MakeWorker(const AFilePath: string): TGocciaCoverageTracker;
begin
  Result := TGocciaCoverageTracker.Create;
  Result.RegisterSourceFile(AFilePath);
end;

{ Each worker registers the same statements in its own order and counts them several times. A merge that matched
  by index would add the wrong statements together; one that counted a hit per worker would collapse the totals
  to the number of workers that reached the statement. }
procedure TTestCoverage.TestMergeSumsStatementHitsByLocation;
var
  Main, WorkerOne, WorkerTwo: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
  I, First, Second: Integer;
begin
  Main := MakeWorker(WORKER_FILE);
  try
    WorkerOne := MakeWorker(WORKER_FILE);
    try
      WorkerTwo := MakeWorker(WORKER_FILE);
      try
        { worker one meets line 3 first, worker two line 9 }
        First := WorkerOne.RegisterStatement(WORKER_FILE, Loc(3, 0, 3, 10));
        Second := WorkerOne.RegisterStatement(WORKER_FILE, Loc(9, 2, 9, 8));
        for I := 1 to 4 do
          WorkerOne.RecordStatementHit(WORKER_FILE, First);
        WorkerOne.RecordStatementHit(WORKER_FILE, Second);

        Second := WorkerTwo.RegisterStatement(WORKER_FILE, Loc(9, 2, 9, 8));
        First := WorkerTwo.RegisterStatement(WORKER_FILE, Loc(3, 0, 3, 10));
        for I := 1 to 7 do
          WorkerTwo.RecordStatementHit(WORKER_FILE, First);
        WorkerTwo.RecordStatementHit(WORKER_FILE, Second);
        WorkerTwo.RecordStatementHit(WORKER_FILE, Second);

        Main.MergeFrom(WorkerOne);
        Main.MergeFrom(WorkerTwo);

        Merged := Main.GetFileCoverage(WORKER_FILE);
        Expect<Integer>(Merged.StatementCount).ToBe(2);
        for I := 0 to Merged.StatementCount - 1 do
          if Merged.Statements[I].Loc.StartLine = 3 then
            Expect<Int64>(Merged.Statements[I].HitCount).ToBe(11)
          else
            Expect<Int64>(Merged.Statements[I].HitCount).ToBe(3);
      finally
        WorkerTwo.Free;
      end;
    finally
      WorkerOne.Free;
    end;
  finally
    Main.Free;
  end;
end;

procedure TTestCoverage.TestMergeSumsBranchHitCounts;
var
  Main, WorkerOne, WorkerTwo: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
  Branch, I: Integer;
  Paths: TGocciaCoverageLocArray;
begin
  Main := MakeWorker(WORKER_FILE);
  try
    WorkerOne := MakeWorker(WORKER_FILE);
    try
      WorkerTwo := MakeWorker(WORKER_FILE);
      try
        Paths := Locs2(Loc(2, 4, 2, 5), Loc(2, 8, 2, 9));
        Branch := WorkerOne.RegisterBranch(WORKER_FILE, 'cond-expr', Loc(2, 0, 2, 9), Paths);
        for I := 1 to 3 do
          WorkerOne.RecordBranchHit(WORKER_FILE, Branch, 0);
        WorkerOne.RecordBranchHit(WORKER_FILE, Branch, 1);
        Branch := WorkerTwo.RegisterBranch(WORKER_FILE, 'cond-expr', Loc(2, 0, 2, 9), Paths);
        for I := 1 to 2 do
          WorkerTwo.RecordBranchHit(WORKER_FILE, Branch, 0);

        Main.MergeFrom(WorkerOne);
        Main.MergeFrom(WorkerTwo);

        Merged := Main.GetFileCoverage(WORKER_FILE);
        Expect<Integer>(Merged.BranchCount).ToBe(1);
        Expect<Integer>(Merged.BranchesFound).ToBe(2);
        Expect<Integer>(Merged.BranchesHit).ToBe(2);
        Expect<Int64>(Merged.Branches[0].HitCounts[0]).ToBe(5);
        Expect<Int64>(Merged.Branches[0].HitCounts[1]).ToBe(1);
        Expect<string>(Merged.Branches[0].BranchType).ToBe('cond-expr');
      finally
        WorkerTwo.Free;
      end;
    finally
      WorkerOne.Free;
    end;
  finally
    Main.Free;
  end;
end;

procedure TTestCoverage.TestMergeSumsFunctionHitCounts;
var
  Main, WorkerOne, WorkerTwo: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
  Index, I: Integer;
begin
  Main := MakeWorker(WORKER_FILE);
  try
    WorkerOne := MakeWorker(WORKER_FILE);
    try
      WorkerTwo := MakeWorker(WORKER_FILE);
      try
        Index := WorkerOne.RegisterFunction(WORKER_FILE, 'step', Loc(1, 9, 1, 13), Loc(1, 16, 1, 30));
        for I := 1 to 3 do
          WorkerOne.RecordFunctionHit(WORKER_FILE, Index);
        Index := WorkerTwo.RegisterFunction(WORKER_FILE, 'step', Loc(1, 9, 1, 13), Loc(1, 16, 1, 30));
        for I := 1 to 4 do
          WorkerTwo.RecordFunctionHit(WORKER_FILE, Index);

        Main.MergeFrom(WorkerOne);
        Main.MergeFrom(WorkerTwo);

        Merged := Main.GetFileCoverage(WORKER_FILE);
        Expect<Integer>(Merged.FunctionsFound).ToBe(1);
        Expect<Integer>(Merged.FunctionsHit).ToBe(1);
        Expect<Int64>(Merged.Functions[0].HitCount).ToBe(7);
        Expect<string>(Merged.Functions[0].Name).ToBe('step');
      finally
        WorkerTwo.Free;
      end;
    finally
      WorkerOne.Free;
    end;
  finally
    Main.Free;
  end;
end;

{ Statements, branch paths and functions that no run reached must survive the merge with zero hits so the report
  keeps its shape. }
procedure TTestCoverage.TestMergeRetainsZeroHitSites;
var
  Main, Worker: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
  Branch: Integer;
begin
  Main := MakeWorker(WORKER_FILE);
  try
    Worker := MakeWorker(WORKER_FILE);
    try
      Worker.RegisterFunction(WORKER_FILE, 'neverCalled', Loc(4, 0, 4, 11), Loc(4, 14, 4, 20));
      Worker.RegisterStatement(WORKER_FILE, Loc(5, 2, 5, 9));
      Branch := Worker.RegisterBranch(WORKER_FILE, 'if', Loc(2, 0, 3, 1),
        Locs2(Loc(2, 0, 3, 1), Loc(0, 0, 0, 0)));
      Worker.RecordBranchHit(WORKER_FILE, Branch, 0);

      Main.MergeFrom(Worker);

      Merged := Main.GetFileCoverage(WORKER_FILE);
      Expect<Integer>(Merged.FunctionsFound).ToBe(1);
      Expect<Integer>(Merged.FunctionsHit).ToBe(0);
      Expect<Integer>(Merged.StatementsFound).ToBe(1);
      Expect<Integer>(Merged.StatementsHit).ToBe(0);
      Expect<Integer>(Merged.BranchesFound).ToBe(2);
      Expect<Integer>(Merged.BranchesHit).ToBe(1);
    finally
      Worker.Free;
    end;
  finally
    Main.Free;
  end;
end;

procedure TTestCoverage.TestMergeAdoptsUnknownFiles;
var
  Main, Worker: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
  Index: Integer;
begin
  Main := TGocciaCoverageTracker.Create;
  try
    Worker := MakeWorker('imported.js');
    try
      Index := Worker.RegisterStatement('imported.js', Loc(2, 0, 2, 7));
      Worker.RecordStatementHit('imported.js', Index);
      Worker.RecordStatementHit('imported.js', Index);

      Main.MergeFrom(Worker);

      Merged := Main.GetFileCoverage('imported.js');
      Expect<Boolean>(Merged <> nil).ToBe(True);
      Expect<Int64>(Merged.Statements[0].HitCount).ToBe(2);
    finally
      Worker.Free;
    end;
  finally
    Main.Free;
  end;
end;

{ Compiling a site again (the code of a finally block runs through two paths) must count into the same statement. }
procedure TTestCoverage.TestRegisteringASiteTwiceReturnsOneIndex;
var
  FileCov: TGocciaFileCoverage;
  First, Second: Integer;
begin
  FileCov := TGocciaFileCoverage.Create(WORKER_FILE);
  try
    First := FileCov.RegisterStatement(Loc(1, 0, 1, 5));
    Second := FileCov.RegisterStatement(Loc(1, 0, 1, 5));
    Expect<Integer>(Second).ToBe(First);
    Expect<Integer>(FileCov.RegisterStatement(Loc(1, 1, 1, 5))).ToBe(First + 1);
    Expect<Integer>(FileCov.StatementCount).ToBe(2);
    First := FileCov.RegisterFunction('', Loc(1, 0, 1, 1), Loc(1, 4, 1, 9));
    Second := FileCov.RegisterFunction('', Loc(1, 0, 1, 1), Loc(1, 4, 1, 9));
    Expect<Integer>(Second).ToBe(First);
    Expect<Integer>(FileCov.FunctionCount).ToBe(1);
  finally
    FileCov.Free;
  end;
end;

{ `const f = () => 1;` called five times: the declarator's initializer runs once, the arrow's body five times, and
  both start on line 1, which counts 5 (istanbul-lib-coverage getLineCoverage). }
procedure TTestCoverage.TestLineCountIsTheMostAmongStatementsStartingOnIt;
var
  FileCov: TGocciaFileCoverage;
  Lines: TGocciaCoverageLineArray;
  Init, Body, Loop, I: Integer;
begin
  FileCov := TGocciaFileCoverage.Create(WORKER_FILE);
  try
    Init := FileCov.RegisterStatement(Loc(1, 10, 1, 17));
    Body := FileCov.RegisterStatement(Loc(1, 16, 1, 17));
    Loop := FileCov.RegisterStatement(Loc(2, 0, 2, 30));
    FileCov.RecordStatementHit(Init);
    for I := 1 to 5 do
      FileCov.RecordStatementHit(Body);
    FileCov.RecordStatementHit(Loop);

    Lines := FileCov.LineCoverage;
    Expect<Integer>(Length(Lines)).ToBe(2);
    Expect<Integer>(Lines[0].Line).ToBe(1);
    Expect<Int64>(Lines[0].HitCount).ToBe(5);
    Expect<Integer>(Lines[1].Line).ToBe(2);
    Expect<Int64>(Lines[1].HitCount).ToBe(1);
    Expect<Integer>(FileCov.LinesFound).ToBe(2);
    Expect<Integer>(FileCov.LinesHit).ToBe(2);
  finally
    FileCov.Free;
  end;
end;

{ A line no statement starts on (the rest of a multi-line statement, a blank line) is not listed, whatever runs on
  it; a line whose statements all count 0 is listed with 0. }
procedure TTestCoverage.TestLinesAreOnlyThoseAStatementStartsOn;
var
  FileCov: TGocciaFileCoverage;
  Lines: TGocciaCoverageLineArray;
begin
  FileCov := TGocciaFileCoverage.Create(WORKER_FILE);
  try
    FileCov.RecordStatementHit(FileCov.RegisterStatement(Loc(2, 0, 5, 1)));
    FileCov.RegisterStatement(Loc(7, 0, 7, 4));
    FileCov.RegisterStatement(Loc(7, 6, 7, 9));

    Lines := FileCov.LineCoverage;
    Expect<Integer>(Length(Lines)).ToBe(2);
    Expect<Integer>(Lines[0].Line).ToBe(2);
    Expect<Int64>(Lines[0].HitCount).ToBe(1);
    Expect<Integer>(Lines[1].Line).ToBe(7);
    Expect<Int64>(Lines[1].HitCount).ToBe(0);
    Expect<Integer>(FileCov.LinesFound).ToBe(2);
    Expect<Integer>(FileCov.LinesHit).ToBe(1);
  finally
    FileCov.Free;
  end;
end;

procedure TTestCoverage.TestBranchPathsCountAsFoundAndHit;
var
  FileCov: TGocciaFileCoverage;
  Branch: Integer;
  Paths: TGocciaCoverageLocArray;
begin
  FileCov := TGocciaFileCoverage.Create(WORKER_FILE);
  try
    SetLength(Paths, 3);
    Paths[0] := Loc(1, 0, 1, 1);
    Paths[1] := Loc(1, 5, 1, 6);
    Paths[2] := Loc(1, 10, 1, 11);
    Branch := FileCov.RegisterBranch('binary-expr', Loc(1, 0, 1, 11), Paths);
    FileCov.RecordBranchHit(Branch, 0);
    FileCov.RecordBranchHit(Branch, 1);
    { a path out of range is ignored, not counted }
    FileCov.RecordBranchHit(Branch, 3);
    Expect<Integer>(FileCov.BranchesFound).ToBe(3);
    Expect<Integer>(FileCov.BranchesHit).ToBe(2);
  finally
    FileCov.Free;
  end;
end;

procedure TTestCoverage.TestOrderFollowsSourcePositionsEnclosingFirst;
var
  FileCov: TGocciaFileCoverage;
  Order: TGocciaCoverageIndexArray;
  Inner, Outer, Later: Integer;
begin
  FileCov := TGocciaFileCoverage.Create(WORKER_FILE);
  try
    { registered in the order the compiler meets them: a function hoisted ahead of the statement around it }
    Later := FileCov.RegisterStatement(Loc(5, 0, 5, 4));
    Inner := FileCov.RegisterStatement(Loc(2, 2, 2, 8));
    Outer := FileCov.RegisterStatement(Loc(2, 2, 4, 3));
    Order := FileCov.StatementOrder;
    Expect<Integer>(Length(Order)).ToBe(3);
    Expect<Integer>(Order[0]).ToBe(Outer);
    Expect<Integer>(Order[1]).ToBe(Inner);
    Expect<Integer>(Order[2]).ToBe(Later);
  finally
    FileCov.Free;
  end;
end;

{ `<stdin>` and multifile section names identify sources that were never on
  disk.  Expanding them would invent a bogus absolute path, so they must pass
  through untouched. }
procedure TTestCoverage.TestVirtualPathsAreLeftVerbatim;
begin
  Expect<string>(CanonicalCoveragePath('<stdin>')).ToBe('<stdin>');
  Expect<string>(CanonicalCoveragePath('<stdin>[part1]')).ToBe('<stdin>[part1]');
  Expect<string>(CanonicalCoveragePath('')).ToBe('');
end;

{ genhtml and Codecov mishandle backslash separators in an `SF:` record, so
  every emitted path uses '/' whatever the platform produced. }
procedure TTestCoverage.TestCanonicalPathUsesForwardSlashes;
var
  SourceFile, Canonical: string;
begin
  SourceFile := MakeTempSourceFile;
  try
    Canonical := CanonicalCoveragePath(SourceFile);
    Expect<Boolean>(Pos('\', Canonical) > 0).ToBe(False);
    Expect<Boolean>(Canonical <> '').ToBe(True);
  finally
    DeleteFile(SourceFile);
  end;
end;

{ The regression behind issue #1094: a file reached both as the entry point
  (keyed by the spelling as typed) and as an import (keyed by its resolved
  absolute path) produced two records whose hits were never added together. }
procedure TTestCoverage.TestEntryAndImportSpellingsShareOneRecord;
var
  Tracker: TGocciaCoverageTracker;
  SourceFile, RelativeSpelling: string;
  Index: Integer;
begin
  SourceFile := MakeTempSourceFile;
  try
    { A relative spelling of the same file, as a command line would supply. }
    RelativeSpelling := '.' + PathDelim + ExtractFileName(SourceFile);
    Expect<string>(CanonicalCoveragePath(RelativeSpelling))
      .ToBe(CanonicalCoveragePath(SourceFile));

    Tracker := TGocciaCoverageTracker.Create;
    try
      Tracker.RegisterSourceFile(RelativeSpelling);
      Index := Tracker.RegisterStatement(RelativeSpelling, Loc(1, 0, 1, 5));
      Tracker.RecordStatementHit(RelativeSpelling, Index);
      { Same physical file, absolute spelling — the import route. }
      Tracker.RecordStatementHit(SourceFile, Index);

      Expect<Integer>(Tracker.Files.Count).ToBe(1);
      Expect<Int64>(
        Tracker.GetFileCoverage(SourceFile).Statements[Index].HitCount).ToBe(2);
    finally
      Tracker.Free;
    end;
  finally
    DeleteFile(SourceFile);
  end;
end;

{ Under a repository root the canonical form drops the root prefix, which is
  what Codecov matches against and what genhtml resolves. }
procedure TTestCoverage.TestCanonicalPathIsRepoRelativeUnderARepository;
var
  Absolute, Canonical, Expected, Root, RootPrefix, SourceFile: string;
begin
  SourceFile := MakeTempSourceFile;
  try
    Absolute := ExpandFileName(SourceFile);
    Canonical := CanonicalCoveragePath(SourceFile);
    Root := FindRepositoryRoot(ExtractFileDir(Absolute));
    if Root <> '' then
    begin
      { The expected key is derived from the repository root rather than
        assumed to be the basename. MakeTempSourceFile writes into
        GetCurrentDir, so the file only sits directly in the root when the
        suite happens to be launched from there; run from a subdirectory the
        canonical key is `<subdir>/<name>`, which is exactly what
        CanonicalCoveragePath is supposed to produce. Asserting the basename
        made this test pass or fail on the caller's working directory instead
        of on the behaviour under test. }
      RootPrefix := IncludeTrailingPathDelimiter(Root);
      Expected := NormalizeCoveragePathSeparators(
        Copy(Absolute, Length(RootPrefix) + 1, MaxInt));
      Expect<string>(Canonical).ToBe(Expected);
      { And it really is relative: the root prefix is gone, and what is left
        cannot be read as an absolute path. }
      Expect<Boolean>(Canonical = NormalizeCoveragePathSeparators(Absolute))
        .ToBe(False);
      Expect<Boolean>(Canonical <> '').ToBe(True);
      Expect<Boolean>(Canonical[1] = '/').ToBe(False);
    end
    else
      { Outside any repository there is no root to be relative to. }
      Expect<string>(Canonical).ToBe(
        NormalizeCoveragePathSeparators(Absolute));
  finally
    DeleteFile(SourceFile);
  end;
end;

{ A canonical key may be repo-relative and so unresolvable from the process's
  working directory.  Reporters read source through the remembered native
  path instead, which must survive canonicalization. }
procedure TTestCoverage.TestResolvedSourcePathSurvivesCanonicalization;
var
  Tracker: TGocciaCoverageTracker;
  SourceFile, Canonical: string;
begin
  SourceFile := MakeTempSourceFile;
  try
    Tracker := TGocciaCoverageTracker.Create;
    try
      Tracker.RegisterSourceFile(SourceFile);
      Canonical := CanonicalCoveragePath(SourceFile);
      Expect<Boolean>(
        FileExists(Tracker.ResolvedSourcePath(Canonical))).ToBe(True);
      { Unknown keys fall back to themselves rather than returning ''. }
      Expect<string>(Tracker.ResolvedSourcePath('<stdin>')).ToBe('<stdin>');
    finally
      Tracker.Free;
    end;
  finally
    DeleteFile(SourceFile);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestCoverage.Create('Coverage'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
