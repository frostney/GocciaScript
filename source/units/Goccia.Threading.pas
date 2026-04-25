{ Thread pool for parallel file execution.

  Provides TGocciaThreadPool which dispatches file-level work items to a
  configurable number of worker threads. Each worker initialises its own
  thread-local runtime (GC, CallStack, MicrotaskQueue) and shuts it down
  on completion, so there is zero shared mutable state between workers.

  Workers pull files from a shared queue so that fast workers naturally
  pick up more files, keeping all cores busy even when file execution
  times vary widely.

  Results are collected in submission order so callers can print output
  deterministically. }

unit Goccia.Threading;

{$I Goccia.inc}

interface

uses
  Classes,
  SyncObjs;

threadvar
  { True on worker threads spawned by TGocciaThreadPool.  Code that can
    run on either the main thread or a worker should check this flag
    before calling WriteLn, because FPC's standard I/O is not
    thread-safe — concurrent WriteLn calls corrupt the shared Output
    TextRec buffer and cause access violations. }
  GIsWorkerThread: Boolean;

type
  { Callback executed on each worker thread for a single file.
    Implementations must NOT call WriteLn directly — capture output in
    AConsoleOutput instead. }
  TGocciaWorkerProc = procedure(const AFileName: string;
    const AIndex: Integer; out AConsoleOutput: string;
    out AErrorMessage: string; AData: Pointer) of object;

  { Per-file result collected by the thread pool. }
  TGocciaWorkerResult = record
    Index: Integer;
    FileName: string;
    Success: Boolean;
    ErrorMessage: string;
    ConsoleOutput: string;
    Data: Pointer;
  end;

  TGocciaWorkerResultArray = array of TGocciaWorkerResult;

  { Internal work item queued for worker threads. }
  TGocciaWorkItem = record
    FileName: string;
    Index: Integer;
  end;

  TGocciaWorkItemArray = array of TGocciaWorkItem;

  { Shared work queue — workers pull the next item under a lock. }
  TGocciaWorkQueue = class
  private
    FItems: TGocciaWorkItemArray;
    FNextIndex: Integer;
    FLock: TRTLCriticalSection;
  public
    constructor Create(const AItems: TGocciaWorkItemArray);
    destructor Destroy; override;
    { Returns True and fills AItem if work remains, False when exhausted. }
    function TryDequeue(out AItem: TGocciaWorkItem): Boolean;
  end;

  TGocciaFileWorker = class(TThread)
  private
    FQueue: TGocciaWorkQueue;
    FResults: TGocciaWorkerResultArray;
    FResultCount: Integer;
    FWorkerProc: TGocciaWorkerProc;
    FCancelled: PBoolean;
    FCancelOnError: Boolean;
    FEnableCoverage: Boolean;
    FMaxBytes: Int64;
    FCoverageTracker: TObject;  // TGocciaCoverageTracker, kept alive for merge
    FData: Pointer;
    // Timestamp (nanoseconds) of this worker's last progress event —
    // updated on dequeue, before the worker callback, and after it
    // returns.  Read by the pool's watchdog to detect a worker that is
    // stuck on a single file.  Aligned 64-bit loads/stores are atomic on
    // our target platforms; a slightly stale read only delays detection
    // by one poll cycle, which is acceptable for a best-effort watchdog.
    FLastActivityNs: Int64;
    // Index of the file the worker is currently processing, or -1 when
    // idle (between dequeue calls).  Naturally-aligned Integer loads are
    // atomic on aarch64/x86_64 — a torn read is impossible.  The watchdog
    // reads this on a stalled worker so it can name the file the worker
    // got stuck on, instead of just reporting "some worker".
    FCurrentFileIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TGocciaWorkQueue;
      AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean;
      ACancelOnError: Boolean; AEnableCoverage: Boolean; AMaxBytes: Int64;
      AData: Pointer);
    property Results: TGocciaWorkerResultArray read FResults;
    property ResultCount: Integer read FResultCount;
    property CoverageTracker: TObject read FCoverageTracker;
    property LastActivityNs: Int64 read FLastActivityNs;
    property CurrentFileIndex: Integer read FCurrentFileIndex;
  end;

  { Thread pool that dispatches files to workers via a shared queue.
    Call RunAll to execute, then read Results in original file order. }
  TGocciaThreadPool = class
  private
    FWorkerCount: Integer;
    FResults: TGocciaWorkerResultArray;
    FWorkers: array of TGocciaFileWorker;
    // Parallel to FWorkers — True when the pool's watchdog gave up on
    // a stalled worker and let the rest of the queue continue without
    // it.  Abandoned workers are leaked (never Free'd or WaitFor'd) so
    // the OS reclaims their memory and any held mutexes on process
    // exit; the alternative of TerminateThread is unsafe across FPC
    // platforms and would corrupt the shared GC's bookkeeping anyway.
    FAbandoned: array of Boolean;
    FCancelled: Boolean;
    FCancelOnError: Boolean;
    FEnableCoverage: Boolean;
    FMaxBytes: Int64;
  public
    constructor Create(AWorkerCount: Integer);

    { Execute AFiles across worker threads. Blocks until all complete.
      AWorkerProc is called on each worker thread for each file.
      AData is an opaque pointer passed through to each worker call.
      AWatchdogMs is the maximum time (in milliseconds) that any single
      worker may go without making progress — i.e. the longest it can
      sit on one file before the pool cancels remaining work.  0 means
      no watchdog (wait forever).  Because the bound is per-worker and
      per-file rather than total, it stays constant no matter how many
      files are queued. }
    procedure RunAll(const AFiles: TStringList;
      AWorkerProc: TGocciaWorkerProc; AData: Pointer = nil;
      AWatchdogMs: Integer = 0);

    { Signal workers to skip remaining files. }
    procedure Cancel;

    { Merge per-worker coverage into the main thread's tracker.
      Only meaningful when EnableCoverage is True. }
    procedure MergeCoverageInto(ATarget: TObject);

    property Results: TGocciaWorkerResultArray read FResults;
    property WorkerCount: Integer read FWorkerCount;
    property Cancelled: Boolean read FCancelled;
    { When True, the first worker error automatically cancels remaining files. }
    property CancelOnError: Boolean read FCancelOnError write FCancelOnError;
    { When True, each worker initialises a per-thread coverage tracker.
      After RunAll, use MergeCoverageInto to collect results. }
    property EnableCoverage: Boolean read FEnableCoverage write FEnableCoverage;
    { GC memory ceiling propagated to each worker thread.
      0 means workers use the auto-detected default. }
    property MaxBytes: Int64 read FMaxBytes write FMaxBytes;
  end;

  { Initialise the thread-local runtime for the calling thread.
    Must be called at the start of each worker thread.
    Set AEnableCoverage to initialise a per-thread coverage tracker.
    AMaxBytes overrides the worker's GC memory ceiling (0 = use default). }
  procedure InitThreadRuntime(AEnableCoverage: Boolean = False;
    AMaxBytes: Int64 = 0);

  { Shut down the thread-local runtime for the calling thread.
    Must be called before the worker thread exits. }
  procedure ShutdownThreadRuntime;

implementation

uses
  Math,
  SysUtils,

  TimingUtils,

  Goccia.Builtins.DisposableStack,
  Goccia.Builtins.Semver,
  Goccia.CallStack,
  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.MicrotaskQueue,
  Goccia.Temporal.TimeZone,
  Goccia.Values.Primitives;

{ Thread runtime lifecycle }

procedure InitThreadRuntime(AEnableCoverage: Boolean; AMaxBytes: Int64);
begin
  GIsWorkerThread := True;
  TGarbageCollector.Initialize;
  // Disable automatic GC on worker threads. Shared immutable objects
  // (primitive singletons, shared prototypes) have a single FGCMark field
  // written by the mark phase — running GC on multiple threads would race
  // on that field. Each worker runs a single file and then shuts down, so
  // skipping collection is acceptable: all objects are freed in bulk when
  // the thread-local GC is destroyed.
  TGarbageCollector.Instance.Enabled := False;
  // Propagate the memory ceiling from the main thread so that --max-memory
  // is honoured on workers. Without this, workers use the auto-detected
  // default and ignore the CLI override.
  if AMaxBytes > 0 then
    TGarbageCollector.Instance.MaxBytes := AMaxBytes;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;
  PinPrimitiveSingletons;
  if AEnableCoverage then
  begin
    TGocciaCoverageTracker.Initialize;
    TGocciaCoverageTracker.Instance.Enabled := True;
  end;
end;

procedure ShutdownThreadRuntime;
begin
  // Coverage tracker is NOT shut down here — the main thread reads it
  // after workers complete, then merges into the main tracker.
  ClearImportMetaCache;
  ClearDisposableStackSlotMap;
  ClearSemverHosts;
  ClearTimeZoneCache;
  TGocciaMicrotaskQueue.Shutdown;
  TGocciaCallStack.Shutdown;
  TGarbageCollector.Shutdown;
end;

{ TGocciaWorkQueue }

constructor TGocciaWorkQueue.Create(const AItems: TGocciaWorkItemArray);
begin
  inherited Create;
  FItems := AItems;
  FNextIndex := 0;
  InitCriticalSection(FLock);
end;

destructor TGocciaWorkQueue.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited;
end;

function TGocciaWorkQueue.TryDequeue(out AItem: TGocciaWorkItem): Boolean;
var
  Idx: Integer;
begin
  EnterCriticalSection(FLock);
  try
    if FNextIndex < Length(FItems) then
    begin
      Idx := FNextIndex;
      Inc(FNextIndex);
      AItem := FItems[Idx];
      Result := True;
    end
    else
      Result := False;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TGocciaFileWorker }

constructor TGocciaFileWorker.Create(AQueue: TGocciaWorkQueue;
  AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean;
  ACancelOnError: Boolean; AEnableCoverage: Boolean; AMaxBytes: Int64;
  AData: Pointer);
const
  WORKER_STACK_SIZE = 8 * 1024 * 1024; // 8 MB — match main thread
begin
  inherited Create(True, WORKER_STACK_SIZE);
  FreeOnTerminate := False;
  FQueue := AQueue;
  FWorkerProc := AWorkerProc;
  FCancelled := ACancelled;
  FCancelOnError := ACancelOnError;
  FEnableCoverage := AEnableCoverage;
  FMaxBytes := AMaxBytes;
  FCoverageTracker := nil;
  FData := AData;
  FResultCount := 0;
  FLastActivityNs := GetNanoseconds;
  FCurrentFileIndex := -1;
  // Pre-allocate generously; actual count tracked by FResultCount.
  SetLength(FResults, 0);
end;

procedure TGocciaFileWorker.Execute;
var
  Item: TGocciaWorkItem;
  ConsoleOut, ErrorMsg: string;
  Idx: Integer;
begin
  FLastActivityNs := GetNanoseconds;
  InitThreadRuntime(FEnableCoverage, FMaxBytes);
  try
    while FQueue.TryDequeue(Item) do
    begin
      // Publish activity and the current file index on every dequeue so
      // the watchdog sees forward progress (and can name the stalled
      // file) even when individual files run fast.
      FCurrentFileIndex := Item.Index;
      FLastActivityNs := GetNanoseconds;

      if FCancelled^ then
      begin
        Idx := FResultCount;
        Inc(FResultCount);
        if Idx >= Length(FResults) then
          SetLength(FResults, Max(8, Length(FResults) * 2));
        FResults[Idx].Index := Item.Index;
        FResults[Idx].FileName := Item.FileName;
        FResults[Idx].Success := False;
        FResults[Idx].ErrorMessage := 'Cancelled';
        FResults[Idx].ConsoleOutput := '';
        FResults[Idx].Data := nil;
        Continue;
      end;

      ConsoleOut := '';
      ErrorMsg := '';
      Idx := FResultCount;
      Inc(FResultCount);
      if Idx >= Length(FResults) then
        SetLength(FResults, Max(8, Length(FResults) * 2));
      FResults[Idx].Index := Item.Index;
      FResults[Idx].FileName := Item.FileName;
      FResults[Idx].Data := FData;
      try
        FWorkerProc(Item.FileName, Item.Index,
          ConsoleOut, ErrorMsg, FData);
        FResults[Idx].Success := ErrorMsg = '';
        FResults[Idx].ErrorMessage := ErrorMsg;
        FResults[Idx].ConsoleOutput := ConsoleOut;
      except
        on E: Exception do
        begin
          FResults[Idx].Success := False;
          FResults[Idx].ErrorMessage := E.ClassName + ': ' + E.Message;
          FResults[Idx].ConsoleOutput := ConsoleOut;
        end;
      end;

      // Publish activity on every completion so the watchdog has a
      // fresh reading even while the worker drains the remainder of a
      // long queue.  Mark the worker idle (no current file) so a
      // between-files snapshot does not falsely accuse a fast worker.
      FCurrentFileIndex := -1;
      FLastActivityNs := GetNanoseconds;

      { Cancel remaining files across all workers on first error. }
      if FCancelOnError and (not FResults[Idx].Success) then
        FCancelled^ := True;
    end;
  finally
    // Refresh the timestamp as we exit the work loop so the watchdog
    // does not interpret a slow thread-runtime shutdown as a hang.
    FLastActivityNs := GetNanoseconds;
    { Detach the coverage tracker before shutting down the runtime so
      the main thread can read it after the worker completes. }
    if FEnableCoverage then
      FCoverageTracker := TGocciaCoverageTracker.Instance;
    ShutdownThreadRuntime;
  end;
end;

{ TGocciaThreadPool }

constructor TGocciaThreadPool.Create(AWorkerCount: Integer);
begin
  inherited Create;
  FWorkerCount := Max(1, AWorkerCount);
  FCancelled := False;
  FCancelOnError := False;
  FEnableCoverage := False;
  FMaxBytes := 0;
end;

procedure TGocciaThreadPool.RunAll(const AFiles: TStringList;
  AWorkerProc: TGocciaWorkerProc; AData: Pointer; AWatchdogMs: Integer);
var
  AllItems: TGocciaWorkItemArray;
  Queue: TGocciaWorkQueue;
  FileCount, I, J, StalledIdx: Integer;
  StalledCount, SnapshotCount: Integer;
  NowNs: Int64;
  AllDone, AnyAbandoned: Boolean;
  StalledFiles: TGocciaWorkerResultArray;
  SnapshotResults: TGocciaWorkerResultArray;
begin
  FCancelled := False;
  AnyAbandoned := False;
  FileCount := AFiles.Count;
  if FileCount = 0 then
  begin
    SetLength(FResults, 0);
    SetLength(FWorkers, 0);
    SetLength(FAbandoned, 0);
    Exit;
  end;

  // Build the shared work item array
  SetLength(AllItems, FileCount);
  for I := 0 to FileCount - 1 do
  begin
    AllItems[I].FileName := AFiles[I];
    AllItems[I].Index := I;
  end;

  Queue := TGocciaWorkQueue.Create(AllItems);
  try
    // Create and start workers. Track how many started so we can
    // wait/free only those if a later Create/Start raises.
    SetLength(FWorkers, FWorkerCount);
    SetLength(FAbandoned, FWorkerCount);
    for I := 0 to FWorkerCount - 1 do
    begin
      FWorkers[I] := nil;
      FAbandoned[I] := False;
    end;

    for I := 0 to FWorkerCount - 1 do
    begin
      FWorkers[I] := TGocciaFileWorker.Create(Queue, AWorkerProc,
        @FCancelled, FCancelOnError, FEnableCoverage, FMaxBytes, AData);
      FWorkers[I].Start;
    end;
  finally
    StalledCount := 0;
    SetLength(StalledFiles, 0);

    if AWatchdogMs > 0 then
    begin
      // Per-worker-idle watchdog: poll until every worker is either
      // finished or has been abandoned.  When a worker goes
      // AWatchdogMs without publishing a progress timestamp we mark
      // only that worker as abandoned and record its current file as
      // a TIMEOUT result.  The other workers keep draining the queue,
      // so a single bad test no longer aborts the entire run.  Because
      // the bound is per-worker and per-file rather than total, a
      // huge batch does not loosen the watchdog the way a
      // total-elapsed formula would.
      repeat
        NowNs := GetNanoseconds;
        // First pass: mark any newly stalled worker as abandoned.
        for I := 0 to FWorkerCount - 1 do
          if Assigned(FWorkers[I]) and (not FAbandoned[I])
              and (not FWorkers[I].Finished) then
            if ((NowNs - FWorkers[I].LastActivityNs) div 1000000)
                >= AWatchdogMs then
            begin
              J := FWorkers[I].CurrentFileIndex;
              SetLength(StalledFiles, StalledCount + 1);
              StalledFiles[StalledCount].Data := nil;
              StalledFiles[StalledCount].Success := False;
              StalledFiles[StalledCount].ConsoleOutput := '';
              if (J >= 0) and (J < FileCount) then
              begin
                StalledFiles[StalledCount].Index := J;
                StalledFiles[StalledCount].FileName := AFiles[J];
                StalledFiles[StalledCount].ErrorMessage := Format(
                  'TIMEOUT (worker stalled in native code, no progress for %dms)',
                  [AWatchdogMs]);
                WriteLn(StdErr, 'Warning: worker #', I,
                  ' stalled in native code on file: ', AFiles[J],
                  ' (no progress for ', AWatchdogMs,
                  'ms). Abandoning worker; remaining files continue.');
              end
              else
              begin
                // No current file index — record a sentinel slot at
                // index 0 so we still surface the abandonment in
                // logs, but don't overwrite a real result.
                StalledFiles[StalledCount].Index := -1;
                StalledFiles[StalledCount].FileName := '<unknown>';
                StalledFiles[StalledCount].ErrorMessage := Format(
                  'TIMEOUT (worker stalled, no current file index, no progress for %dms)',
                  [AWatchdogMs]);
                WriteLn(StdErr, 'Warning: worker #', I,
                  ' stalled with no current file index (no progress for ',
                  AWatchdogMs, 'ms). Abandoning worker.');
              end;
              Inc(StalledCount);
              FAbandoned[I] := True;
              AnyAbandoned := True;
            end;

        // Second pass: are all workers done (finished or abandoned)?
        AllDone := True;
        for I := 0 to FWorkerCount - 1 do
          if Assigned(FWorkers[I]) and (not FAbandoned[I])
              and (not FWorkers[I].Finished) then
          begin
            AllDone := False;
            Break;
          end;
        if not AllDone then
          Sleep(100);
      until AllDone;
    end;

    // Wait on the workers we still own.  Abandoned workers are
    // leaked: they may still be stuck inside native code that holds
    // shared resources, so WaitFor would block forever and Free is
    // a use-after-free.  The OS reclaims them on process exit.
    for I := 0 to FWorkerCount - 1 do
      if Assigned(FWorkers[I]) and not FAbandoned[I] then
        FWorkers[I].WaitFor;

    if AnyAbandoned then
      WriteLn(StdErr, 'Warning: ', StalledCount,
        ' worker(s) abandoned. Leaking their queue/state; ',
        'they will be reclaimed on process exit.');

    // Only free the queue when no abandoned workers remain — they
    // still hold a reference to it via FQueue.
    if not AnyAbandoned then
      Queue.Free;
  end;

  // Collect results in original file order
  SetLength(FResults, FileCount);
  // Initialise all slots so we can detect missing results
  for I := 0 to FileCount - 1 do
  begin
    FResults[I].Index := I;
    FResults[I].FileName := AFiles[I];
    FResults[I].Success := False;
    FResults[I].ErrorMessage := '';
    FResults[I].ConsoleOutput := '';
    FResults[I].Data := nil;
  end;

  for I := 0 to FWorkerCount - 1 do
  begin
    if not Assigned(FWorkers[I]) then Continue;

    if FAbandoned[I] then
    begin
      // Snapshot the abandoned worker's results array by reference so
      // any subsequent SetLength on the worker side doesn't pull the
      // memory out from under us.  Skip the in-progress slot
      // (ResultCount-1) because Success/ErrorMessage on that slot are
      // not yet written by the worker.  All earlier slots were fully
      // populated before the worker entered the call that stalled.
      SnapshotResults := FWorkers[I].Results;
      SnapshotCount := FWorkers[I].ResultCount;
      if SnapshotCount > Length(SnapshotResults) then
        SnapshotCount := Length(SnapshotResults);
      for J := 0 to SnapshotCount - 2 do
      begin
        StalledIdx := SnapshotResults[J].Index;
        if (StalledIdx >= 0) and (StalledIdx < FileCount) then
          FResults[StalledIdx] := SnapshotResults[J];
      end;
      // Don't Free or merge coverage from abandoned workers — the
      // thread is still alive in native code.
      Continue;
    end;

    // Check for unhandled exceptions in the worker thread
    if Assigned(FWorkers[I].FatalException) then
      WriteLn(StdErr, 'Worker thread ', I, ' fatal: ',
        Exception(FWorkers[I].FatalException).Message);

    for J := 0 to FWorkers[I].ResultCount - 1 do
      FResults[FWorkers[I].Results[J].Index] := FWorkers[I].Results[J];

    { Don't free workers yet if coverage is enabled — MergeCoverageInto
      needs access to their coverage trackers. }
    if not FEnableCoverage then
    begin
      FWorkers[I].Free;
      FWorkers[I] := nil;
    end;
  end;

  // Apply stalled-file overrides last so the watchdog's TIMEOUT
  // message wins over any stale empty entry left by initialisation.
  for I := 0 to StalledCount - 1 do
  begin
    StalledIdx := StalledFiles[I].Index;
    if (StalledIdx >= 0) and (StalledIdx < FileCount) then
      FResults[StalledIdx] := StalledFiles[I];
  end;
end;

procedure TGocciaThreadPool.Cancel;
begin
  FCancelled := True;
end;

procedure TGocciaThreadPool.MergeCoverageInto(ATarget: TObject);
var
  I: Integer;
  WorkerTracker: TGocciaCoverageTracker;
begin
  if not FEnableCoverage then Exit;
  for I := 0 to High(FWorkers) do
  begin
    // Abandoned workers are still alive in native code — leave them
    // alone.  Their CoverageTracker is also unset (the assignment
    // happens in the worker's finally block, which abandoned workers
    // never reach).
    if (I < Length(FAbandoned)) and FAbandoned[I] then Continue;
    if Assigned(FWorkers[I]) and Assigned(FWorkers[I].CoverageTracker) then
    begin
      WorkerTracker := TGocciaCoverageTracker(FWorkers[I].CoverageTracker);
      TGocciaCoverageTracker(ATarget).MergeFrom(WorkerTracker);
      WorkerTracker.Free;
    end;
    FreeAndNil(FWorkers[I]);
  end;
end;

end.
