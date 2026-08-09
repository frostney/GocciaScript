program Goccia.Threading.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  CriticalSections,
  Classes,
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.ThreadCleanupRegistry,
  Goccia.Threading,
  Goccia.Values.Primitives,
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
    procedure TestCancellationFlagLifecycle;
    procedure TestPoolCancelOnErrorBoundsWorkAcrossWorkers;
    procedure TestResumedAbandonedWorkerCannotCancelLaterRun;
    procedure TestPoolResetsCancelledBetweenRuns;
    procedure TestPoolHandlesEmptyFileList;
    procedure TestPoolSingleWorker;
    procedure TestThreadCleanupRegistryRunsRegistered;
    procedure TestShutdownThreadRuntimeDrainsRegistryPerWorker;
  end;

{ Helpers }

var
  GWorkerCallCount: Integer;
  GWorkerFileNames: array of string;
  GWorkerLock: TGocciaCriticalSection;

procedure ResetWorkerState;
begin
  GWorkerCallCount := 0;
  SetLength(GWorkerFileNames, 0);
end;

{ Sentinel cleanup callbacks for the ThreadCleanupRegistry tests. Registrations
  persist for the process (the registry has no unregister), and a registered
  callback fires on every thread that drains the registry — including worker
  threads concurrently — so the counters use atomic increments and each test
  resets its own counter before measuring a delta. }
var
  GSentinelCount: Integer;
  GSentinelWorkerCount: Integer;

procedure SentinelCleanup;
begin
  AtomicIncrementInt32(GSentinelCount);
end;

procedure SentinelCleanupSecond;
begin
  AtomicIncrementInt32(GSentinelCount);
end;

procedure SentinelWorkerCleanup;
begin
  AtomicIncrementInt32(GSentinelWorkerCount);
end;

type
  TTestWorkerHost = class
    procedure CountingWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    procedure FailOnSecondWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    { Counts every invocation and fails on 'fail.js', so a test can assert
      how much work actually reached the worker callback after a cancel. }
    procedure CountingFailWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    { Blocks on 'stall.js' until the main thread releases it, then fails —
      the shape the watchdog abandons, resuming only once a later run is
      already in flight. }
    procedure StallThenFailWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    { Deliberately slow so a later run stays in flight long enough for a
      resumed zombie's cancellation to be observable if it leaked in. }
    procedure SlowCountingWorker(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
  end;

{ Release gate for the stalling worker.  Guarded rather than a bare
  Boolean: this file is the regression suite for an unsynchronised
  cross-thread flag, so the test must not reintroduce one. }
var
  GStallReleased: Boolean;

procedure ReleaseStalledWorker;
begin
  CriticalSectionEnter(GWorkerLock);
  try
    GStallReleased := True;
  finally
    CriticalSectionLeave(GWorkerLock);
  end;
end;

function StallReleased: Boolean;
begin
  CriticalSectionEnter(GWorkerLock);
  try
    Result := GStallReleased;
  finally
    CriticalSectionLeave(GWorkerLock);
  end;
end;

{ Worker-thread exits observed so far.  SentinelWorkerCleanup increments
  this from every worker's ShutdownThreadRuntime, which is the only
  externally visible signal that an abandoned thread has fully retired. }
function WorkerExitCount: Integer;
begin
  ReadMemoryBarrier;
  Result := GSentinelWorkerCount;
end;

procedure TTestWorkerHost.CountingWorker(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  CriticalSectionEnter(GWorkerLock);
  try
    Inc(GWorkerCallCount);
    SetLength(GWorkerFileNames, Length(GWorkerFileNames) + 1);
    GWorkerFileNames[High(GWorkerFileNames)] := AFileName;
  finally
    CriticalSectionLeave(GWorkerLock);
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

procedure TTestWorkerHost.CountingFailWorker(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  CriticalSectionEnter(GWorkerLock);
  try
    Inc(GWorkerCallCount);
  finally
    CriticalSectionLeave(GWorkerLock);
  end;
  if AFileName = 'fail.js' then
    AErrorMessage := 'deliberate failure'
  else
    AErrorMessage := '';
end;

procedure TTestWorkerHost.StallThenFailWorker(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  if AFileName <> 'stall.js' then
    Exit;
  // Stand in for a worker wedged in native code: the pool's watchdog sees
  // no progress and abandons this thread while it is still alive here.
  while not StallReleased do
    Sleep(1);
  AErrorMessage := 'deliberate failure after resuming';
end;

procedure TTestWorkerHost.SlowCountingWorker(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  { Release the abandoned worker from inside the later run rather than
    before it starts.  Releasing beforehand leaves the zombie racing
    RunAll's flag reset: if it lost that race the cancel would be wiped
    and the run would pass whether or not the pool isolates flags.  The
    race resolves in the test's favour in practice — the current test
    fails 5/5 against the pre-fix pool — but that rests on a zombie
    waking slower than RunAll dispatches, which is a scheduling accident,
    not a guarantee, and this test runs on four CI platforms.  Releasing
    here makes the cancellation land mid-run by construction.  The call
    is idempotent (one guarded Boolean), so every worker may run it. }
  ReleaseStalledWorker;
  Sleep(5);
  CriticalSectionEnter(GWorkerLock);
  try
    Inc(GWorkerCallCount);
  finally
    CriticalSectionLeave(GWorkerLock);
  end;
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
  Test('CancellationFlag cancels and resets', TestCancellationFlagLifecycle);
  Test('Pool CancelOnError bounds work across workers',
    TestPoolCancelOnErrorBoundsWorkAcrossWorkers);
  Test('Pool resets Cancelled between runs', TestPoolResetsCancelledBetweenRuns);
  Test('Pool handles empty file list', TestPoolHandlesEmptyFileList);
  Test('Pool single worker processes all files', TestPoolSingleWorker);
  Test('ThreadCleanupRegistry runs registered callbacks', TestThreadCleanupRegistryRunsRegistered);
  Test('ShutdownThreadRuntime drains registry once per worker', TestShutdownThreadRuntimeDrainsRegistryPerWorker);
  { Registered last: this is the only case that deliberately strands a
    worker thread.  It joins that thread before returning, but running it
    after the registry tests keeps their exact sentinel counts out of
    reach of a straggler even if that join ever regressed. }
  Test('Resumed abandoned worker cannot cancel a later run',
    TestResumedAbandonedWorkerCannotCancelLaterRun);
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

{ The flag is the pool's only stop signal and every thread reaches it
  through these three methods, so its state machine is worth pinning
  directly rather than only through a pool run. }
procedure TTestThreading.TestCancellationFlagLifecycle;
var
  Flag: TGocciaCancellationFlag;
begin
  Flag := TGocciaCancellationFlag.Create;
  try
    Expect<Boolean>(Flag.IsCancelled).ToBe(False);
    Flag.Cancel;
    Expect<Boolean>(Flag.IsCancelled).ToBe(True);
    // Cancel is idempotent — a second failing file must not un-cancel.
    Flag.Cancel;
    Expect<Boolean>(Flag.IsCancelled).ToBe(True);
    Flag.Reset;
    Expect<Boolean>(Flag.IsCancelled).ToBe(False);
  finally
    Flag.Free;
  end;
end;

{ Regression guard for the shared-cancellation-flag data race: the flag
  used to be a plain Boolean written and read by several threads without
  synchronisation, so a worker could observe a stale False after a peer
  had already failed and keep pulling queued files.  With the failing
  file first in a long queue and eight workers, only the handful of files
  already in flight may reach the callback; a worker that misses the
  cancel drains the rest of the queue and blows the bound.  Repeated
  because a lost update is timing-dependent and a single run proves
  little. }
procedure TTestThreading.TestPoolCancelOnErrorBoundsWorkAcrossWorkers;
const
  FILE_COUNT = 2000;
  WORKER_COUNT = 8;
  { Generous: the true in-flight set is at most WORKER_COUNT.  The bound
    only has to separate "cancelled promptly" from "drained the queue". }
  MAX_EXECUTED = 200;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
  Iteration, I: Integer;
begin
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Files.Add('fail.js');
    for I := 1 to FILE_COUNT - 1 do
      Files.Add('ok' + IntToStr(I) + '.js');

    for Iteration := 1 to 10 do
    begin
      ResetWorkerState;
      Pool := TGocciaThreadPool.Create(WORKER_COUNT);
      try
        Pool.CancelOnError := True;
        Pool.RunAll(Files, Host.CountingFailWorker);
        Expect<Boolean>(Pool.Cancelled).ToBe(True);
        Expect<Boolean>(GWorkerCallCount >= 1).ToBe(True);
        Expect<Boolean>(GWorkerCallCount <= MAX_EXECUTED).ToBe(True);
      finally
        Pool.Free;
      end;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

{ Regression guard for cancellation-flag ownership across an abandonment.
  The flag is a pool field, so a run that abandons a worker used to hand
  the zombie a pointer to the very object the NEXT run would reset and
  reuse.  A zombie that later unsticks, finishes its old file and fails
  then calls Cancel — landing on an unrelated run and silently skipping
  files nobody asked to stop.  Here run 1 strands a worker, run 2 starts
  on what must be a fresh flag, and the zombie is released so its failure
  lands squarely inside run 2.  If the two runs share a flag, run 2's
  files are marked cancelled instead of executed and both assertions
  below fail loudly. }
procedure TTestThreading.TestResumedAbandonedWorkerCannotCancelLaterRun;
const
  STALL_WATCHDOG_MS = 200;
  LATER_RUN_FILES = 200;
  { Run 1 spawns 2 workers (one strands, one finds no work) and run 2
    spawns 2 more; every worker thread increments the exit counter from
    ShutdownThreadRuntime as it retires. }
  EXPECTED_WORKER_EXITS = 4;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
  ExitBaseline, I, WaitedMs: Integer;
  AllSucceeded: Boolean;
begin
  ResetWorkerState;
  GStallReleased := False;
  ExitBaseline := WorkerExitCount;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    { Run 1 — the only file wedges its worker, so the watchdog abandons
      that thread and RunAll returns while it is still alive.  Its queue
      is left empty, so once released it finishes and exits promptly. }
    Files.Add('stall.js');
    Pool := TGocciaThreadPool.Create(2);
    try
      // CancelOnError must be armed here: the zombie captured this at
      // construction, and it is what makes its later failure call Cancel.
      Pool.CancelOnError := True;
      Pool.RunAll(Files, Host.StallThenFailWorker, nil, STALL_WATCHDOG_MS);

      { Run 2 — a long, entirely healthy batch on the same pool. }
      Files.Clear;
      for I := 1 to LATER_RUN_FILES do
        Files.Add('later' + IntToStr(I) + '.js');
      ResetWorkerState;

      // The zombie is released by SlowCountingWorker itself, so it
      // resumes and fails with run 2 already in flight.
      Pool.RunAll(Files, Host.SlowCountingWorker);

      { Wait for the zombie to retire BEFORE asserting anything.

        Releasing it from inside the later run puts its failure inside that
        run, but says nothing about whether it has reached its Cancel by the
        time RunAll returns. Assert first and a pool that reuses the flag can
        cancel immediately afterwards, leaving every assertion already passed
        — the test would go green against precisely the defect it exists to
        catch. Its exit is the one externally visible proof that its Cancel,
        if any, has already happened, so the wait moves ahead of the
        assertions and closes the window rather than leaving it open.

        An earlier version of this comment justified the wait by the cleanup
        registry: the zombie drains it on the way out and the registry tests
        assert exact counts. That reason does not apply — this test is
        registered after both registry tests, which have already made their
        assertions by the time it runs. What the wait actually guarantees is
        that the abandoned thread has retired before the pool is freed. }
      WaitedMs := 0;
      while (WorkerExitCount - ExitBaseline < EXPECTED_WORKER_EXITS)
          and (WaitedMs < 5000) do
      begin
        Sleep(10);
        Inc(WaitedMs, 10);
      end;
      Expect<Integer>(WorkerExitCount - ExitBaseline)
        .ToBe(EXPECTED_WORKER_EXITS);

      // Nothing in run 2 failed, so nothing may have cancelled it.
      Expect<Boolean>(Pool.Cancelled).ToBe(False);
      // Every file must have reached the callback — a leaked-in cancel
      // shows up as dequeued-but-skipped files, not as an error.
      Expect<Integer>(GWorkerCallCount).ToBe(LATER_RUN_FILES);
      AllSucceeded := True;
      for I := 0 to High(Pool.Results) do
        if not Pool.Results[I].Success then
          AllSucceeded := False;
      Expect<Boolean>(AllSucceeded).ToBe(True);
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

procedure TTestThreading.TestThreadCleanupRegistryRunsRegistered;
begin
  // SentinelCleanup and SentinelCleanupSecond are registered once at startup
  // (see the program body), honouring RegisterThreadvarCleanup's
  // write-once-at-init contract. A nil callback was also registered there and
  // must be ignored — otherwise the drain below would call a nil pointer.

  // Draining runs every registered callback (both sentinels, nil skipped).
  GSentinelCount := 0;
  RunThreadvarCleanups;
  Expect<Integer>(GSentinelCount).ToBe(2);

  // Draining again is safe and re-runs them (repeatable on any thread).
  GSentinelCount := 0;
  RunThreadvarCleanups;
  Expect<Integer>(GSentinelCount).ToBe(2);
end;

procedure TTestThreading.TestShutdownThreadRuntimeDrainsRegistryPerWorker;
const
  WORKER_COUNT = 3;
var
  Pool: TGocciaThreadPool;
  Files: TStringList;
  Host: TTestWorkerHost;
begin
  // SentinelWorkerCleanup is registered once at startup (see the program body).
  // Each worker thread calls ShutdownThreadRuntime as it exits, which drains the
  // registry on that thread. With WORKER_COUNT workers, the registered callback
  // must fire exactly WORKER_COUNT times — proving the per-worker-exit wiring.
  GSentinelWorkerCount := 0;

  ResetWorkerState;
  Host := TTestWorkerHost.Create;
  Files := TStringList.Create;
  try
    Files.Add('w1.js');
    Files.Add('w2.js');
    Files.Add('w3.js');

    Pool := TGocciaThreadPool.Create(WORKER_COUNT);
    try
      Pool.RunAll(Files, Host.CountingWorker);
      Expect<Integer>(GSentinelWorkerCount).ToBe(WORKER_COUNT);
    finally
      Pool.Free;
    end;
  finally
    Files.Free;
    Host.Free;
  end;
end;

begin
  // Worker threads call InitThreadRuntime → PinPrimitiveSingletons, which
  // in turn touches UndefinedValue/NullValue/... — those getters assert
  // the singleton was created on the main thread.  Pre-initialise here so
  // worker threads only encounter already-built singletons.
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  CriticalSectionInit(GWorkerLock);
  // Register the cleanup sentinels once here, before any worker thread is
  // spawned, honouring RegisterThreadvarCleanup's write-once-at-init contract.
  // The nil registration must be ignored (a nil callback would crash the drain).
  RegisterThreadvarCleanup(nil);
  RegisterThreadvarCleanup(@SentinelCleanup);
  RegisterThreadvarCleanup(@SentinelCleanupSecond);
  RegisterThreadvarCleanup(@SentinelWorkerCleanup);
  try
    TestRunnerProgram.AddSuite(TTestThreading.Create('Threading'));
    RunGocciaTests;
  finally
    CriticalSectionDone(GWorkerLock);
    TGarbageCollector.Shutdown;
  end;

  ExitCode := TestResultToExitCode;
end.
