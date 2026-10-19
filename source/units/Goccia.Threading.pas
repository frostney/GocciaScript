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

// GIsWorkerThread moved to the dependency-free Goccia.Threading.Flags
// unit so the value layer can import the flag without this unit's
// TThread closure; the pool still sets it per worker
// (InitThreadRuntime).

uses
  Classes,
  SyncObjs,

  CriticalSections,

  Goccia.CLI.JSON.Reporter,
  Goccia.Threading.Flags;

const
  { Error message recorded for a file that was dequeued after the pool had
    already been cancelled, i.e. one that never ran. Callers that cancel
    deliberately (CancelOnError) need to tell these apart from genuine
    failures, so the spelling is shared rather than duplicated. }
  GOCCIA_POOL_CANCELLED_MESSAGE = 'Cancelled';

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

  { Points in a worker's loop where the test seam below can stop it. }
  TGocciaWorkerCheckpoint = (
    { The worker has taken a file and published it as its current file. }
    wcDequeued,
    { The worker callback for the current file has returned. }
    wcFileReturned,
    { The worker has recorded the file's result and is between files. }
    wcFileRecorded);

  { Test seam, nil outside tests. The checkpoint hook runs on a worker at
    each TGocciaWorkerCheckpoint; AWorker is that worker. The stall hook is
    asked by the watchdog about every running worker on every poll, and a
    worker it answers True for is abandoned at once, whatever the time, so
    a test can abandon a worker at a chosen point and then let it run on. }
  TGocciaWorkerCheckpointHook = procedure(const AWorker: TObject;
    const ACheckpoint: TGocciaWorkerCheckpoint; const AFileIndex: Integer);
  TGocciaWatchdogStallHook = function(const AWorker: TObject): Boolean;

  { Shared work queue — workers pull the next item under a lock. }
  TGocciaWorkQueue = class
  private
    FItems: TGocciaWorkItemArray;
    FNextIndex: Integer;
    FLock: TGocciaCriticalSection;
  public
    constructor Create(const AItems: TGocciaWorkItemArray);
    destructor Destroy; override;
    { Returns True and fills AItem if work remains, False when exhausted. }
    function TryDequeue(out AItem: TGocciaWorkItem): Boolean;
  end;

  { Shared cancellation flag — the pool's stop signal, read and written
    from the main thread and from every worker.  The Boolean itself is
    private and reachable only through these three methods, so there is
    no way to touch it outside the lock; a plain shared Boolean let a
    worker observe a stale False after a peer had already cancelled and
    run further queued files.  The lock is the same critical-section
    idiom TGocciaWorkQueue uses, and it is taken once per dequeued file
    — nothing next to the cost of running one. }
  TGocciaCancellationFlag = class
  private
    FCancelled: Boolean;
    FLock: TGocciaCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    { True once Cancel has been called and Reset has not. }
    function IsCancelled: Boolean;
    procedure Cancel;
    { Clears the flag for a fresh run. Main thread only, before workers start. }
    procedure Reset;
  end;

  TGocciaFileWorker = class(TThread)
  private
    FQueue: TGocciaWorkQueue;
    { Guards FResults, FResultCount, FCurrentFileIndex and FAbandoned
      between this worker and the pool's watchdog: the watchdog abandons a
      worker and copies its finished results in one step under it, and the
      worker takes a file, and records a result, only under it and only
      while it is not abandoned. }
    FStateLock: TGocciaCriticalSection;
    { Set by the watchdog. From then on the run has reported this worker's
      files, so the worker records no result and takes no further file. }
    FAbandoned: Boolean;
    FResults: TGocciaWorkerResultArray;
    FResultCount: Integer;
    FWorkerProc: TGocciaWorkerProc;
    { Not owned — the pool owns this flag and outlives the worker, except
      for abandoned workers, for which the pool deliberately leaks it. }
    FCancelFlag: TGocciaCancellationFlag;
    FCancelOnError: Boolean;
    FEnableCoverage: Boolean;
    FResetRuntimeBetweenItems: Boolean;
    FMaxBytes: Int64;
    FCoverageTracker: TObject;  // TGocciaCoverageTracker, kept alive for merge
    FMemoryStats: TCLIJSONMemoryStats;
    FData: Pointer;
    // Timestamp (nanoseconds) of this worker's last progress event —
    // updated on dequeue, before the worker callback, and after it
    // returns.  Read by the pool's watchdog to detect a worker that is
    // stuck on a single file.  Aligned 64-bit loads/stores are atomic on
    // our target platforms; a slightly stale read only delays detection
    // by one poll cycle, which is acceptable for a best-effort watchdog.
    FLastActivityNs: Int64;
    // Index of the file the worker is currently processing, or -1 when
    // idle (between dequeue calls).  The watchdog reads it under
    // FStateLock when it abandons the worker, so it can name the file the
    // worker got stuck on instead of just reporting "some worker".
    FCurrentFileIndex: Integer;
    procedure TouchActivity;
    function TakeNextItem(out AItem: TGocciaWorkItem): Boolean;
    function StillOwned: Boolean;
    function RecordResult(const AResult: TGocciaWorkerResult): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TGocciaWorkQueue;
      AWorkerProc: TGocciaWorkerProc; ACancelFlag: TGocciaCancellationFlag;
      ACancelOnError: Boolean; AEnableCoverage: Boolean; AMaxBytes: Int64;
      AResetRuntimeBetweenItems: Boolean; AData: Pointer);
    destructor Destroy; override;
    { Main thread, watchdog only. Abandons the worker unless it has made
      progress since the watchdog read AObservedActivityNs, and returns,
      from the same critical section, its results array (the first
      ACompletedCount entries are the files it finished) and the file it is
      on (-1 between files). It allocates nothing, so it cannot fail half
      way, and the worker never touches the array again. }
    function TryAbandon(const AObservedActivityNs: Int64;
      out ACompleted: TGocciaWorkerResultArray;
      out ACompletedCount, ACurrentFileIndex: Integer): Boolean;
    { Read only after the worker has finished (WaitFor). }
    property Results: TGocciaWorkerResultArray read FResults;
    property ResultCount: Integer read FResultCount;
    property CoverageTracker: TObject read FCoverageTracker;
    property MemoryStats: TCLIJSONMemoryStats read FMemoryStats;
    property LastActivityNs: Int64 read FLastActivityNs;
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
    { The stop signal handed to every worker of the CURRENT run.  Owned
      here and freed with the pool, except once a worker has been
      abandoned: that thread is still alive and still reads the flag it
      was given, so the flag is leaked to it and the next run starts on a
      replacement.  One flag therefore never spans an abandonment. }
    FCancelFlag: TGocciaCancellationFlag;
    { True while FCancelFlag belongs to an abandoned worker rather than to
      this pool.  Set when a run abandons, cleared when the next run mints
      a replacement — so the destructor frees exactly the flags the pool
      still owns and never one a zombie is reading. }
    FCancelFlagLeaked: Boolean;
    FAnyAbandoned: Boolean;
    FCancelOnError: Boolean;
    FEnableCoverage: Boolean;
    FResetRuntimeBetweenItems: Boolean;
    FMaxBytes: Int64;
    FMemoryStats: TCLIJSONMemoryStats;
    function GetCancelled: Boolean;
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;

    { Execute AFiles across worker threads. Blocks until all complete.
      AWorkerProc is called on each worker thread for each file.
      AData is an opaque pointer passed through to each worker call.
      AWatchdogMs is the maximum time (in milliseconds) that any single
      worker may go without making progress — i.e. the longest it can
      sit on one file before the pool abandons that worker.  The file it
      was on gets a TIMEOUT result, the other workers drain the queue,
      and the abandoned worker takes no further file and records nothing;
      a file no worker took keeps an empty, unsuccessful result.  0 means
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
    property MemoryStats: TCLIJSONMemoryStats read FMemoryStats;
    property WorkerCount: Integer read FWorkerCount;
    { True when the last RunAll abandoned a worker. Such a worker takes no
      further file and records nothing, but it may still be inside the
      worker callback for the file it was on, so whatever that callback
      writes to (AData, host state) must stay allocated for the rest of
      the process. }
    property AnyWorkerAbandoned: Boolean read FAnyAbandoned;
    { This run's stop signal, exposed so a worker callback can cancel the queue
      without holding a reference to the pool itself. That distinction is a
      lifetime one, not a convenience: the pool is freed while an abandoned
      worker is still running, so a callback that cached the pool could call
      into freed memory, whereas the flag is leaked rather than freed the
      moment any worker is abandoned (see Destroy) — precisely the case in
      which a zombie can still reach for it. Read it before RunAll; RunAll
      replaces the flag when the previous run leaked one, so a pool that is
      being reused must be re-read rather than cached across runs. }
    property CancelFlag: TGocciaCancellationFlag read FCancelFlag;
    { Reads the shared flag under its lock, so callers never see a torn
      or stale value while workers are still running. }
    property Cancelled: Boolean read GetCancelled;
    { When True, the first worker error automatically cancels remaining files. }
    property CancelOnError: Boolean read FCancelOnError write FCancelOnError;
    { When True, each worker initialises a per-thread coverage tracker.
      After RunAll, use MergeCoverageInto to collect results. }
    property EnableCoverage: Boolean read FEnableCoverage write FEnableCoverage;
    { Recreate the complete thread-local runtime after each work item. This is
      intended for long-lived native harnesses whose cases require heap and
      singleton isolation in addition to a fresh engine. }
    property ResetRuntimeBetweenItems: Boolean
      read FResetRuntimeBetweenItems write FResetRuntimeBetweenItems;
    { GC memory ceiling propagated to each worker thread.
      0 means workers use the auto-detected default. }
    property MaxBytes: Int64 read FMaxBytes write FMaxBytes;
  end;

  { Installs the test seam's hooks; pass nil for both outside tests. Set
    them only while no pool is running. }
  procedure SetThreadPoolHooksForTesting(
    const ACheckpoint: TGocciaWorkerCheckpointHook;
    const AStalled: TGocciaWatchdogStallHook);

  { Initialise the thread-local runtime for the calling thread.
    Must be called at the start of each worker thread.
    Set AEnableCoverage to initialise a per-thread coverage tracker.
    AMaxBytes overrides the worker's GC memory ceiling (0 = use default). }
  procedure InitThreadRuntime(AEnableCoverage: Boolean = False;
    AMaxBytes: Int64 = 0);

  { Shut down the thread-local runtime for the calling thread: drain the
    threadvar cleanups, then free the microtask queue, the call stack and the
    collector, which destroys every value it still manages. A worker thread
    calls it before it exits; TGocciaApplication.RunApplication calls it on
    the main thread once the application has finished. No engine may still
    be alive on the thread. }
  procedure ShutdownThreadRuntime;

  { ShutdownThreadRuntime for the main thread once the application has
    finished, unless a thread pool abandoned a stalled worker. An abandoned
    worker is still running and still reads the process-wide values the main
    thread's collector manages (the primitive singletons, the well-known
    symbols, the shared prototypes), so freeing them would pull them from
    under it; the process then exits with them still allocated instead. }
  procedure ShutdownMainThreadRuntime;

  { Ends the process at once with AStatus, running no unit finalization and
    flushing nothing. For a process whose heap the engine no longer vouches
    for, or whose worker threads are still running: Halt would run code on
    that heap and tear the RTL down under those threads. Declared against the
    C runtime (TerminateProcess on Windows) rather than routed through the
    RTL for that reason. }
  procedure TerminateProcessNow(AStatus: LongInt);

  { When a thread pool has abandoned a worker, flushes Output and ErrOutput
    and ends the process with AExitCode through TerminateProcessNow;
    otherwise returns. A host calls it once its report is written and
    before it frees anything: the abandoned worker may still be inside its
    callback, which reads the host's objects and the process-wide runtime,
    so neither the host's teardown nor unit finalization may run under it. }
  procedure EndProcessIfWorkerAbandoned(const AExitCode: LongInt);

implementation

uses
  Math,
  SysUtils,

  TimingUtils,

  Goccia.CallStack,
  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.Primitives;

{ Thread runtime lifecycle }

procedure InitThreadRuntime(AEnableCoverage: Boolean; AMaxBytes: Int64);
begin
  GIsWorkerThread := True;
  TGarbageCollector.Initialize;
  // Disable automatic GC on worker threads. A worker collects only when asked:
  // through Goccia.gc() or a host's explicit Collect (GocciaTestRunner
  // collects after every file a worker runs), both under the global collector
  // lock. Shutting the runtime down reclaims whatever is left.
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

var
  { Set by the main thread when a pool abandons a worker; never cleared,
    because an abandoned worker is never joined. }
  GWorkerAbandoned: Boolean = False;
  GCheckpointHook: TGocciaWorkerCheckpointHook = nil;
  GStallHook: TGocciaWatchdogStallHook = nil;

procedure SetThreadPoolHooksForTesting(
  const ACheckpoint: TGocciaWorkerCheckpointHook;
  const AStalled: TGocciaWatchdogStallHook);
begin
  GCheckpointHook := ACheckpoint;
  GStallHook := AStalled;
end;

procedure WorkerCheckpoint(const AWorker: TObject;
  const ACheckpoint: TGocciaWorkerCheckpoint; const AFileIndex: Integer);
begin
  if Assigned(GCheckpointHook) then
    GCheckpointHook(AWorker, ACheckpoint, AFileIndex);
end;

{$IFDEF WINDOWS}
{ TerminateProcess rather than ExitProcess: ExitProcess first terminates the
  peer threads and THEN runs DLL process-detach handlers — a detach handler
  needing a lock a just-terminated worker held deadlocks the exit. Terminate
  skips detach handlers entirely, which is the whole contract here. }
function GetCurrentProcess: THandle; stdcall;
  external 'kernel32' name 'GetCurrentProcess';
function TerminateProcess(AProcess: THandle; AExitCode: LongWord): LongBool;
  stdcall; external 'kernel32' name 'TerminateProcess';

procedure TerminateProcessNow(AStatus: LongInt);
begin
  TerminateProcess(GetCurrentProcess, LongWord(AStatus));
end;
{$ELSE}
procedure TerminateProcessNow(AStatus: LongInt); cdecl; external name '_exit';
{$ENDIF}

procedure EndProcessIfWorkerAbandoned(const AExitCode: LongInt);
begin
  if not GWorkerAbandoned then
    Exit;
  // Flushing is best effort (a closed or full stream raises); nothing may
  // keep the process from ending here.
  try
    try
      Flush(Output);
    except
    end;
    try
      Flush(ErrOutput);
    except
    end;
  finally
    TerminateProcessNow(AExitCode);
  end;
end;

{ A diagnostic the watchdog writes must not fail the run: a closed or full
  stderr would otherwise raise in the middle of an abandonment. }
procedure WarnBestEffort(const AText: string);
begin
  try
    WriteLn(ErrOutput, AText);
  except
  end;
end;

procedure ShutdownMainThreadRuntime;
begin
  if not GWorkerAbandoned then
    ShutdownThreadRuntime;
end;

procedure ShutdownThreadRuntime;
begin
  // Coverage tracker is NOT shut down here — the main thread reads it
  // after workers complete, then merges into the main tracker.
  //
  // Drain the engine-wide threadvar-cleanup registry. Every per-thread managed
  // cache and memo registers its release proc in Goccia.ThreadCleanupRegistry
  // from its own unit initialization — the builtins' and value types' cached
  // member-definition arrays, plus the import.meta, Atomics-waiter,
  // disposable-stack, semver-host, timezone, regex-input and is-ASCII caches —
  // so this single drain releases all of this worker thread's copies (the
  // registry's own finalization does the same on the main thread). Run it
  // before the ordered object-lifecycle shutdowns below so caches that touch
  // the collector (e.g. import.meta unpinning) are released while the GC is
  // still alive.
  RunThreadvarCleanups;
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
  CriticalSectionInit(FLock);
end;

destructor TGocciaWorkQueue.Destroy;
begin
  CriticalSectionDone(FLock);
  inherited;
end;

function TGocciaWorkQueue.TryDequeue(out AItem: TGocciaWorkItem): Boolean;
var
  Idx: Integer;
begin
  CriticalSectionEnter(FLock);
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
    CriticalSectionLeave(FLock);
  end;
end;

{ TGocciaCancellationFlag }

constructor TGocciaCancellationFlag.Create;
begin
  inherited Create;
  FCancelled := False;
  CriticalSectionInit(FLock);
end;

destructor TGocciaCancellationFlag.Destroy;
begin
  CriticalSectionDone(FLock);
  inherited;
end;

function TGocciaCancellationFlag.IsCancelled: Boolean;
begin
  CriticalSectionEnter(FLock);
  try
    Result := FCancelled;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaCancellationFlag.Cancel;
begin
  CriticalSectionEnter(FLock);
  try
    FCancelled := True;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaCancellationFlag.Reset;
begin
  CriticalSectionEnter(FLock);
  try
    FCancelled := False;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

{ TGocciaFileWorker }

constructor TGocciaFileWorker.Create(AQueue: TGocciaWorkQueue;
  AWorkerProc: TGocciaWorkerProc; ACancelFlag: TGocciaCancellationFlag;
  ACancelOnError: Boolean; AEnableCoverage: Boolean; AMaxBytes: Int64;
  AResetRuntimeBetweenItems: Boolean; AData: Pointer);
const
  WORKER_STACK_SIZE = 8 * 1024 * 1024; // 8 MB — match main thread
begin
  inherited Create(True, WORKER_STACK_SIZE);
  FreeOnTerminate := False;
  FQueue := AQueue;
  FWorkerProc := AWorkerProc;
  FCancelFlag := ACancelFlag;
  FCancelOnError := ACancelOnError;
  FEnableCoverage := AEnableCoverage;
  FResetRuntimeBetweenItems := AResetRuntimeBetweenItems;
  FMaxBytes := AMaxBytes;
  FCoverageTracker := nil;
  FMemoryStats := DefaultCLIJSONMemoryStats;
  FData := AData;
  FResultCount := 0;
  FLastActivityNs := GetNanoseconds;
  FCurrentFileIndex := -1;
  FAbandoned := False;
  CriticalSectionInit(FStateLock);
  // Grown on demand; the actual count is tracked by FResultCount.
  SetLength(FResults, 0);
end;

destructor TGocciaFileWorker.Destroy;
begin
  // Only a worker that was not abandoned is ever freed, after WaitFor.
  CriticalSectionDone(FStateLock);
  inherited;
end;

// Every write of FLastActivityNs is under FStateLock, so the watchdog's
// check that the worker made no progress and its abandonment are one step.
procedure TGocciaFileWorker.TouchActivity;
begin
  CriticalSectionEnter(FStateLock);
  try
    FLastActivityNs := GetNanoseconds;
  finally
    CriticalSectionLeave(FStateLock);
  end;
end;

function TGocciaFileWorker.TakeNextItem(out AItem: TGocciaWorkItem): Boolean;
begin
  CriticalSectionEnter(FStateLock);
  try
    Result := (not FAbandoned) and FQueue.TryDequeue(AItem);
    if Result then
    begin
      // Publish activity and the current file index on every dequeue so
      // the watchdog sees forward progress (and can name the stalled
      // file) even when individual files run fast.
      FCurrentFileIndex := AItem.Index;
      FLastActivityNs := GetNanoseconds;
    end;
  finally
    CriticalSectionLeave(FStateLock);
  end;
end;

function TGocciaFileWorker.StillOwned: Boolean;
begin
  CriticalSectionEnter(FStateLock);
  try
    Result := not FAbandoned;
  finally
    CriticalSectionLeave(FStateLock);
  end;
end;

function TGocciaFileWorker.RecordResult(
  const AResult: TGocciaWorkerResult): Boolean;
begin
  CriticalSectionEnter(FStateLock);
  try
    Result := not FAbandoned;
    if not Result then
      Exit;
    if FResultCount >= Length(FResults) then
      SetLength(FResults, Max(8, Length(FResults) * 2));
    FResults[FResultCount] := AResult;
    Inc(FResultCount);
    // Publish activity on every completion so the watchdog has a fresh
    // reading even while the worker drains the remainder of a long
    // queue, and mark the worker idle so an abandonment between files
    // names no file.
    FCurrentFileIndex := -1;
    FLastActivityNs := GetNanoseconds;
  finally
    CriticalSectionLeave(FStateLock);
  end;
end;

function TGocciaFileWorker.TryAbandon(const AObservedActivityNs: Int64;
  out ACompleted: TGocciaWorkerResultArray;
  out ACompletedCount, ACurrentFileIndex: Integer): Boolean;
begin
  ACompleted := nil;
  ACompletedCount := 0;
  ACurrentFileIndex := -1;
  CriticalSectionEnter(FStateLock);
  try
    Result := FLastActivityNs = AObservedActivityNs;
    if not Result then
      Exit;
    // A reference, not a copy: nothing here can fail once the worker is
    // abandoned.
    ACompleted := FResults;
    ACompletedCount := FResultCount;
    ACurrentFileIndex := FCurrentFileIndex;
    FAbandoned := True;
  finally
    CriticalSectionLeave(FStateLock);
  end;
end;

procedure TGocciaFileWorker.Execute;
var
  Item: TGocciaWorkItem;
  ItemResult: TGocciaWorkerResult;
  ConsoleOut, ErrorMsg: string;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
begin
  TouchActivity;
  InitThreadRuntime(FEnableCoverage, FMaxBytes);
  BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
  try
    // A worker the watchdog abandoned takes no further file and records
    // no further result: by then the run has reported its files, and the
    // host may already have released what a later file would write to.
    while TakeNextItem(Item) do
    begin
      WorkerCheckpoint(Self, wcDequeued, Item.Index);
      ItemResult.Index := Item.Index;
      ItemResult.FileName := Item.FileName;
      ItemResult.ConsoleOutput := '';

      if FCancelFlag.IsCancelled then
      begin
        ItemResult.Success := False;
        ItemResult.ErrorMessage := GOCCIA_POOL_CANCELLED_MESSAGE;
        ItemResult.Data := nil;
        if not RecordResult(ItemResult) then
          Break;
        Continue;
      end;

      // Abandoned after taking the file: the run reported it as timed
      // out, so it is not started now.
      if not StillOwned then
        Break;

      ConsoleOut := '';
      ErrorMsg := '';
      ItemResult.Data := FData;
      try
        FWorkerProc(Item.FileName, Item.Index,
          ConsoleOut, ErrorMsg, FData);
        WorkerCheckpoint(Self, wcFileReturned, Item.Index);
        ItemResult.Success := ErrorMsg = '';
        ItemResult.ErrorMessage := ErrorMsg;
        ItemResult.ConsoleOutput := ConsoleOut;
      except
        on E: Exception do
        begin
          ItemResult.Success := False;
          ItemResult.ErrorMessage := E.ClassName + ': ' + E.Message;
          ItemResult.ConsoleOutput := ConsoleOut;
        end;
      end;

      if not RecordResult(ItemResult) then
        Break;
      WorkerCheckpoint(Self, wcFileRecorded, Item.Index);

      { Cancel remaining files across all workers on first error. }
      if FCancelOnError and (not ItemResult.Success) then
        FCancelFlag.Cancel;

      if FResetRuntimeBetweenItems then
      begin
        FMemoryStats := CombineCLIJSONMemoryStats(FMemoryStats,
          FinishCLIJSONMemoryMeasurement(MemoryMeasurement), True);
        ShutdownThreadRuntime;
        InitThreadRuntime(FEnableCoverage, FMaxBytes);
        BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      end;
    end;
  finally
    // Refresh the timestamp as we exit the work loop so the watchdog
    // does not interpret a slow thread-runtime shutdown as a hang.
    TouchActivity;
    FMemoryStats := CombineCLIJSONMemoryStats(FMemoryStats,
      FinishCLIJSONMemoryMeasurement(MemoryMeasurement), True);
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
  FCancelFlag := TGocciaCancellationFlag.Create;
  FCancelFlagLeaked := False;
  FCancelOnError := False;
  FEnableCoverage := False;
  FResetRuntimeBetweenItems := False;
  FMaxBytes := 0;
  FMemoryStats := DefaultCLIJSONMemoryStats;
end;

destructor TGocciaThreadPool.Destroy;
begin
  // Same rule as the work queue: an abandoned worker is still running and
  // still calls IsCancelled on the flag it holds, so freeing that one
  // would be a use-after-free.  Leak it and let the OS reclaim it on
  // process exit.  Any flag replaced by a later run was already leaked
  // under this same rule, so every owned flag is freed exactly once.
  if not FCancelFlagLeaked then
    FCancelFlag.Free;
  FCancelFlag := nil;
  inherited;
end;

function TGocciaThreadPool.GetCancelled: Boolean;
begin
  Result := FCancelFlag.IsCancelled;
end;

procedure TGocciaThreadPool.RunAll(const AFiles: TStringList;
  AWorkerProc: TGocciaWorkerProc; AData: Pointer; AWatchdogMs: Integer);
var
  AllItems: TGocciaWorkItemArray;
  Queue: TGocciaWorkQueue;
  FileCount, I, J, StalledIdx: Integer;
  StalledCount: Integer;
  NowNs, ObservedNs: Int64;
  AllDone: Boolean;
  StalledFiles: TGocciaWorkerResultArray;
  { The finished results of abandoned workers, copied when each was
    abandoned. Owned by this thread: an abandoned worker never writes them. }
  AbandonedResults: TGocciaWorkerResultArray;
  Completed: TGocciaWorkerResultArray;
  CompletedCount: Integer;
begin
  if FCancelFlagLeaked then
  begin
    // A previous run abandoned a worker that is still alive and still
    // reads the flag it was handed.  That flag now belongs to the zombie
    // alone — resetting it here would re-arm a signal the zombie can
    // still fire: if it ever unsticks, finishes its old file and fails,
    // its Cancel would land on THIS run and skip files that were never
    // asked to stop.  Give it up for good (it is leaked, never freed)
    // and dispatch this run on a fresh flag we own.
    FCancelFlag := TGocciaCancellationFlag.Create;
    FCancelFlagLeaked := False;
  end
  else
    FCancelFlag.Reset;
  FMemoryStats := DefaultCLIJSONMemoryStats;
  FAnyAbandoned := False;
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
        FCancelFlag, FCancelOnError, FEnableCoverage, FMaxBytes,
        FResetRuntimeBetweenItems, AData);
      FWorkers[I].Start;
    end;
  finally
    StalledCount := 0;
    SetLength(StalledFiles, 0);
    SetLength(AbandonedResults, 0);

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
          begin
            ObservedNs := FWorkers[I].LastActivityNs;
            if ((((NowNs - ObservedNs) div 1000000) >= AWatchdogMs) or
                (Assigned(GStallHook) and GStallHook(FWorkers[I]))) and
                FWorkers[I].TryAbandon(ObservedNs, Completed,
                  CompletedCount, J) then
            begin
              // Publish the abandonment before anything that can fail
              // (allocation, output): from here on the worker, its queue
              // and this run's cancellation flag belong to no one, and
              // every path out of RunAll, and the host's teardown, must
              // know it.
              FAbandoned[I] := True;
              FAnyAbandoned := True;
              FCancelFlagLeaked := True;
              GWorkerAbandoned := True;
              AbandonedResults := Concat(AbandonedResults,
                Copy(Completed, 0, CompletedCount));
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
                WarnBestEffort('Warning: worker #' + IntToStr(I) +
                  ' stalled in native code on file: ' + AFiles[J] +
                  ' (no progress for ' + IntToStr(AWatchdogMs) +
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
                WarnBestEffort('Warning: worker #' + IntToStr(I) +
                  ' stalled with no current file index (no progress for ' +
                  IntToStr(AWatchdogMs) + 'ms). Abandoning worker.');
              end;
              Inc(StalledCount);
            end;
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

    if FAnyAbandoned then
      WarnBestEffort('Warning: ' + IntToStr(StalledCount) +
        ' worker(s) abandoned. Leaking their queue/state; ' +
        'they will be reclaimed on process exit.');

    // Only free the queue when no abandoned workers remain — they
    // still hold a reference to it via FQueue.  Each run builds its own
    // queue, so a leaked one belongs to that run's zombie and can never
    // reach a later run.  The cancellation flag is different: it is a
    // pool field that every run would otherwise share, so the
    // abandonment marked it leaked for THIS run (FCancelFlagLeaked) and
    // the next RunAll replaces it with a fresh instance.
    if not FAnyAbandoned then
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

    // An abandoned worker's finished results were copied when it was
    // abandoned (AbandonedResults below); the thread is still alive, so it
    // is neither freed nor merged for coverage.
    if FAbandoned[I] then
      Continue;

    // Check for unhandled exceptions in the worker thread
    if Assigned(FWorkers[I].FatalException) then
      WriteLn(ErrOutput, 'Worker thread ', I, ' fatal: ',
        Exception(FWorkers[I].FatalException).Message);

    for J := 0 to FWorkers[I].ResultCount - 1 do
      FResults[FWorkers[I].Results[J].Index] := FWorkers[I].Results[J];

    FMemoryStats := CombineCLIJSONMemoryStats(
      FMemoryStats, FWorkers[I].MemoryStats, True);

    { Don't free workers yet if coverage is enabled — MergeCoverageInto
      needs access to their coverage trackers. }
    if not FEnableCoverage then
    begin
      FWorkers[I].Free;
      FWorkers[I] := nil;
    end;
  end;

  for I := 0 to High(AbandonedResults) do
  begin
    StalledIdx := AbandonedResults[I].Index;
    if (StalledIdx >= 0) and (StalledIdx < FileCount) then
      FResults[StalledIdx] := AbandonedResults[I];
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
  FCancelFlag.Cancel;
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
