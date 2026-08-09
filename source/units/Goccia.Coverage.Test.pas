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

begin
  TestRunnerProgram.AddSuite(TTestCoverage.Create('Coverage'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
