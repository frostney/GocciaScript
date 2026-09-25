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
    function MakeWorker(const AFilePath: string;
      const AExecutableLines: Integer): TGocciaCoverageTracker;
  public
    procedure SetupTests; override;

    procedure TestMergeSumsLineHitCounts;
    procedure TestMergeSumsBranchHitCounts;
    procedure TestMergeSumsFunctionHitCounts;
    procedure TestMergeRetainsZeroHitBranchesAndFunctions;
    procedure TestMergeAdoptsUnknownFilesAndExecutableLines;
    procedure TestVirtualPathsAreLeftVerbatim;
    procedure TestCanonicalPathUsesForwardSlashes;
    procedure TestEntryAndImportSpellingsShareOneRecord;
    procedure TestResolvedSourcePathSurvivesCanonicalization;
    procedure TestCanonicalPathIsRepoRelativeUnderARepository;
  end;

const
  WORKER_FILE = 'shared.js';

procedure TTestCoverage.SetupTests;
begin
  Test('MergeFrom sums line hit counts', TestMergeSumsLineHitCounts);
  Test('MergeFrom sums branch hit counts', TestMergeSumsBranchHitCounts);
  Test('MergeFrom sums function hit counts', TestMergeSumsFunctionHitCounts);
  Test('MergeFrom retains zero-hit branches and functions',
    TestMergeRetainsZeroHitBranchesAndFunctions);
  Test('MergeFrom adopts unknown files and executable line counts',
    TestMergeAdoptsUnknownFilesAndExecutableLines);
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

function TTestCoverage.MakeWorker(const AFilePath: string;
  const AExecutableLines: Integer): TGocciaCoverageTracker;
begin
  Result := TGocciaCoverageTracker.Create;
  Result.RegisterSourceFile(AFilePath, AExecutableLines);
end;

{ Each worker records the same line several times. A merge that records one
  hit per covered line collapses the totals to "number of workers that touched
  the line", which is the parallel-run regression this guards. }
procedure TTestCoverage.TestMergeSumsLineHitCounts;
var
  Main, WorkerOne, WorkerTwo: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
  I: Integer;
begin
  Main := MakeWorker(WORKER_FILE, 10);
  try
    WorkerOne := MakeWorker(WORKER_FILE, 10);
    try
      WorkerTwo := MakeWorker(WORKER_FILE, 10);
      try
        for I := 1 to 4 do
          WorkerOne.GetFileCoverage(WORKER_FILE).RecordLineHit(3);
        for I := 1 to 7 do
          WorkerTwo.GetFileCoverage(WORKER_FILE).RecordLineHit(3);
        WorkerTwo.GetFileCoverage(WORKER_FILE).RecordLineHit(9);

        Main.MergeFrom(WorkerOne);
        Main.MergeFrom(WorkerTwo);

        Merged := Main.GetFileCoverage(WORKER_FILE);
        Expect<Integer>(Merged.GetLineHitCount(3)).ToBe(11);
        Expect<Integer>(Merged.GetLineHitCount(9)).ToBe(1);
        Expect<Integer>(Merged.GetLineHitCount(4)).ToBe(0);
        Expect<Integer>(Merged.LinesHit).ToBe(2);
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
  I: Integer;
begin
  Main := MakeWorker(WORKER_FILE, 10);
  try
    WorkerOne := MakeWorker(WORKER_FILE, 10);
    try
      WorkerTwo := MakeWorker(WORKER_FILE, 10);
      try
        for I := 1 to 3 do
          WorkerOne.RecordBranchHit(WORKER_FILE, 2, 5, 0);
        WorkerOne.RecordBranchHit(WORKER_FILE, 2, 5, 1);
        for I := 1 to 2 do
          WorkerTwo.RecordBranchHit(WORKER_FILE, 2, 5, 0);

        Main.MergeFrom(WorkerOne);
        Main.MergeFrom(WorkerTwo);

        Merged := Main.GetFileCoverage(WORKER_FILE);
        Expect<Integer>(Merged.BranchesFound).ToBe(2);
        Expect<Integer>(Merged.BranchesHit).ToBe(2);
        for I := 0 to Merged.Branches.Count - 1 do
          if Merged.Branches[I].BranchIndex = 0 then
            Expect<Integer>(Merged.Branches[I].HitCount).ToBe(5)
          else
            Expect<Integer>(Merged.Branches[I].HitCount).ToBe(1);
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
  I: Integer;
begin
  Main := MakeWorker(WORKER_FILE, 10);
  try
    WorkerOne := MakeWorker(WORKER_FILE, 10);
    try
      WorkerTwo := MakeWorker(WORKER_FILE, 10);
      try
        for I := 1 to 3 do
          WorkerOne.RecordFunctionHit(WORKER_FILE, 'step', 1, 0);
        for I := 1 to 4 do
          WorkerTwo.RecordFunctionHit(WORKER_FILE, 'step', 1, 0);

        Main.MergeFrom(WorkerOne);
        Main.MergeFrom(WorkerTwo);

        Merged := Main.GetFileCoverage(WORKER_FILE);
        Expect<Integer>(Merged.FunctionsFound).ToBe(1);
        Expect<Integer>(Merged.FunctionsHit).ToBe(1);
        Expect<Integer>(Merged.Functions[0].HitCount).ToBe(7);
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

{ Uncalled functions and untaken branch arms must survive the merge with zero
  hits so the report keeps its shape. }
procedure TTestCoverage.TestMergeRetainsZeroHitBranchesAndFunctions;
var
  Main, Worker: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
begin
  Main := MakeWorker(WORKER_FILE, 10);
  try
    Worker := MakeWorker(WORKER_FILE, 10);
    try
      Worker.RegisterFunction(WORKER_FILE, 'neverCalled', 4, 0);
      Worker.RecordBranchHit(WORKER_FILE, 2, 5, 0);

      Main.MergeFrom(Worker);

      Merged := Main.GetFileCoverage(WORKER_FILE);
      Expect<Integer>(Merged.FunctionsFound).ToBe(1);
      Expect<Integer>(Merged.FunctionsHit).ToBe(0);
      Expect<Integer>(Merged.BranchesFound).ToBe(2);
      Expect<Integer>(Merged.BranchesHit).ToBe(1);
    finally
      Worker.Free;
    end;
  finally
    Main.Free;
  end;
end;

procedure TTestCoverage.TestMergeAdoptsUnknownFilesAndExecutableLines;
var
  Main, Worker: TGocciaCoverageTracker;
  Merged: TGocciaFileCoverage;
begin
  Main := TGocciaCoverageTracker.Create;
  try
    Worker := MakeWorker('imported.js', 12);
    try
      Worker.GetFileCoverage('imported.js').RecordLineHit(2);
      Worker.GetFileCoverage('imported.js').RecordLineHit(2);

      Main.MergeFrom(Worker);

      Merged := Main.GetFileCoverage('imported.js');
      Expect<Boolean>(Merged <> nil).ToBe(True);
      Expect<Integer>(Merged.ExecutableLines).ToBe(12);
      Expect<Integer>(Merged.GetLineHitCount(2)).ToBe(2);
    finally
      Worker.Free;
    end;
  finally
    Main.Free;
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
begin
  SourceFile := MakeTempSourceFile;
  try
    { A relative spelling of the same file, as a command line would supply. }
    RelativeSpelling := '.' + PathDelim + ExtractFileName(SourceFile);
    Expect<string>(CanonicalCoveragePath(RelativeSpelling))
      .ToBe(CanonicalCoveragePath(SourceFile));

    Tracker := TGocciaCoverageTracker.Create;
    try
      Tracker.RegisterSourceFile(RelativeSpelling, 3);
      Tracker.RecordLineHit(RelativeSpelling, 1);
      { Same physical file, absolute spelling — the import route. }
      Tracker.RecordLineHit(SourceFile, 1);

      Expect<Integer>(Tracker.Files.Count).ToBe(1);
      Expect<Integer>(
        Tracker.GetFileCoverage(SourceFile).GetLineHitCount(1)).ToBe(2);
      Expect<Integer>(
        Tracker.GetFileCoverage(RelativeSpelling).ExecutableLines).ToBe(3);
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
      Tracker.RegisterSourceFile(SourceFile, 1);
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
