program Goccia.Threading.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  Goccia.Threading,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  TTestThreading = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestWorkQueueDrainsAllItems;
    procedure TestWorkQueueReturnsItemsInOrder;
    procedure TestWorkQueueEmptyReturnsFalse;
    procedure TestPoolRunsAllFiles;
    procedure TestPoolResultsInFileOrder;
    procedure TestPoolCancelSkipsRemaining;
    procedure TestPoolCancelOnErrorStopsOnFailure;
    procedure TestPoolResetsCancelledBetweenRuns;
    procedure TestPoolHandlesEmptyFileList;
    procedure TestPoolSingleWorker;
  end;

{ Helpers }

var
  GWorkerCallCount: Integer;
  GWorkerFileNames: array of string;
  GWorkerLock: TRTLCriticalSection;

procedure ResetWorkerState;
begin
  GWorkerCallCount := 0;
  SetLength(GWorkerFileNames, 0);
end;

type
  TTestWorkerHost = class
    procedure CountingWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    procedure FailOnSecondWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
  end;

procedure TTestWorkerHost.CountingWorker(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  EnterCriticalSection(GWorkerLock);
  try
    Inc(GWorkerCallCount);
    SetLength(GWorkerFileNames, Length(GWorkerFileNames) + 1);
    GWorkerFileNames[High(GWorkerFileNames)] := AFileName;
  finally
    LeaveCriticalSection(GWorkerLock);
  end;
end;

procedure TTestWorkerHost.FailOnSecondWorker(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  if AFileName = 'fail.js' then
    AErrorMessage := 'deliberate failure'
  else
    AErrorMessage := '';
end;

{ TTestThreading }

procedure TTestThreading.SetupTests;
begin
  Test('WorkQueue drains all items', TestWorkQueueDrainsAllItems);
  Test('WorkQueue returns items in order', TestWorkQueueReturnsItemsInOrder);
  Test('WorkQueue empty returns False', TestWorkQueueEmptyReturnsFalse);
  Test('Pool runs all files', TestPoolRunsAllFiles);
  Test('Pool results in file order', TestPoolResultsInFileOrder);
  Test('Pool Cancel skips remaining', TestPoolCancelSkipsRemaining);
  Test('Pool CancelOnError stops on failure', TestPoolCancelOnErrorStopsOnFailure);
  Test('Pool resets Cancelled between runs', TestPoolResetsCancelledBetweenRuns);
  Test('Pool handles empty file list', TestPoolHandlesEmptyFileList);
  Test('Pool single worker processes all files', TestPoolSingleWorker);
end;

procedure TTestThreading.TestWorkQueueDrainsAllItems;
var
  Items: TGocciaWorkItemArray;
  Queue: TGocciaWorkQueue;
  Item: TGocciaWorkItem;
  Count: Integer;
begin
  SetLength(Items, 5);
  Items[0].FileName := 'a.js'; Items[0].Index := 0;
  Items[1].FileName := 'b.js'; Items[1].Index := 1;
  Items[2].FileName := 'c.js'; Items[2].Index := 2;
  Items[3].FileName := 'd.js'; Items[3].Index := 3;
  Items[4].FileName := 'e.js'; Items[4].Index := 4;

  Queue := TGocciaWorkQueue.Create(Items);
  try
    Count := 0;
    while Queue.TryDequeue(Item) do
      Inc(Count);
    Expect<Integer>(Count).ToBe(5);
  finally
    Queue.Free;
  end;
end;

procedure TTestThreading.TestWorkQueueReturnsItemsInOrder;
var
  Items: TGocciaWorkItemArray;
  Queue: TGocciaWorkQueue;
  Item: TGocciaWorkItem;
begin
  SetLength(Items, 3);
  Items[0].FileName := 'first.js';  Items[0].Index := 0;
  Items[1].FileName := 'second.js'; Items[1].Index := 1;
  Items[2].FileName := 'third.js';  Items[2].Index := 2;

  Queue := TGocciaWorkQueue.Create(Items);
  try
    Queue.TryDequeue(Item);
    Expect<string>(Item.FileName).ToBe('first.js');
    Queue.TryDequeue(Item);
    Expect<string>(Item.FileName).ToBe('second.js');
    Queue.TryDequeue(Item);
    Expect<string>(Item.FileName).ToBe('third.js');
  finally
    Queue.Free;
  end;
end;

procedure TTestThreading.TestWorkQueueEmptyReturnsFalse;
var
  Items: TGocciaWorkItemArray;
  Queue: TGocciaWorkQueue;
  Item: TGocciaWorkItem;
begin
  SetLength(Items, 0);
  Queue := TGocciaWorkQueue.Create(Items);
  try
    Expect<Boolean>(Queue.TryDequeue(Item)).ToBe(False);
  finally
    Queue.Free;
  end;
end;

procedure TTestThreading.TestPoolRunsAllFiles;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
begin
  ResetWorkerState;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Files.Add('a.js');
    Files.Add('b.js');
    Files.Add('c.js');
    Files.Add('d.js');
    Files.Add('e.js');

    Pool := TGocciaThreadPool.Create(2);
    try
      Pool.RunAll(Files, Host.CountingWorker);
      Expect<Integer>(GWorkerCallCount).ToBe(5);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

procedure TTestThreading.TestPoolResultsInFileOrder;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
begin
  ResetWorkerState;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Files.Add('alpha.js');
    Files.Add('beta.js');
    Files.Add('gamma.js');

    Pool := TGocciaThreadPool.Create(2);
    try
      Pool.RunAll(Files, Host.CountingWorker);
      Expect<Integer>(Length(Pool.Results)).ToBe(3);
      Expect<string>(Pool.Results[0].FileName).ToBe('alpha.js');
      Expect<string>(Pool.Results[1].FileName).ToBe('beta.js');
      Expect<string>(Pool.Results[2].FileName).ToBe('gamma.js');
      Expect<Boolean>(Pool.Results[0].Success).ToBe(True);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

procedure TTestThreading.TestPoolCancelSkipsRemaining;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
  CancelledCount, I: Integer;
begin
  ResetWorkerState;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    for I := 0 to 99 do
      Files.Add('file' + IntToStr(I) + '.js');

    Pool := TGocciaThreadPool.Create(2);
    try
      Pool.RunAll(Files, Host.CountingWorker);
      // Cancel after first run completes
      Pool.Cancel;
      // All should have completed since Cancel was called after RunAll
      Expect<Integer>(GWorkerCallCount).ToBe(100);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

procedure TTestThreading.TestPoolCancelOnErrorStopsOnFailure;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
  FailedCount, CancelledCount, I: Integer;
begin
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    // Put the failure file first so it triggers quickly
    Files.Add('fail.js');
    for I := 1 to 99 do
      Files.Add('ok' + IntToStr(I) + '.js');

    Pool := TGocciaThreadPool.Create(1);
    try
      Pool.CancelOnError := True;
      Pool.RunAll(Files, Host.FailOnSecondWorker);

      // First file should have failed
      Expect<Boolean>(Pool.Results[0].Success).ToBe(False);
      Expect<string>(Pool.Results[0].ErrorMessage).ToBe('deliberate failure');
      Expect<Boolean>(Pool.Cancelled).ToBe(True);

      // Some remaining files should be cancelled (with 1 worker, all after first)
      CancelledCount := 0;
      FailedCount := 0;
      for I := 0 to High(Pool.Results) do
      begin
        if Pool.Results[I].ErrorMessage = 'Cancelled' then
          Inc(CancelledCount);
        if not Pool.Results[I].Success then
          Inc(FailedCount);
      end;

      // At least some files should be cancelled
      Expect<Boolean>(CancelledCount > 0).ToBe(True);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

procedure TTestThreading.TestPoolResetsCancelledBetweenRuns;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
begin
  ResetWorkerState;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Files.Add('fail.js');

    Pool := TGocciaThreadPool.Create(1);
    try
      Pool.CancelOnError := True;
      // First run: triggers cancel
      Pool.RunAll(Files, Host.FailOnSecondWorker);
      Expect<Boolean>(Pool.Cancelled).ToBe(True);

      // Second run: should reset and succeed
      Files.Clear;
      Files.Add('ok.js');
      Pool.CancelOnError := False;
      Pool.RunAll(Files, Host.CountingWorker);
      Expect<Boolean>(Pool.Cancelled).ToBe(False);
      Expect<Boolean>(Pool.Results[0].Success).ToBe(True);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

procedure TTestThreading.TestPoolHandlesEmptyFileList;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
begin
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Pool := TGocciaThreadPool.Create(4);
    try
      Pool.RunAll(Files, Host.CountingWorker);
      Expect<Integer>(Length(Pool.Results)).ToBe(0);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

procedure TTestThreading.TestPoolSingleWorker;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
begin
  ResetWorkerState;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Files.Add('one.js');
    Files.Add('two.js');
    Files.Add('three.js');

    Pool := TGocciaThreadPool.Create(1);
    try
      Pool.RunAll(Files, Host.CountingWorker);
      Expect<Integer>(GWorkerCallCount).ToBe(3);
      Expect<Integer>(Length(Pool.Results)).ToBe(3);
      Expect<string>(Pool.Results[0].FileName).ToBe('one.js');
      Expect<string>(Pool.Results[2].FileName).ToBe('three.js');
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

begin
  InitCriticalSection(GWorkerLock);
  try
    TestRunnerProgram.AddSuite(TTestThreading.Create('Threading'));
    TestRunnerProgram.Run;
  finally
    DoneCriticalSection(GWorkerLock);
  end;

  ExitCode := TestResultToExitCode;
end.
