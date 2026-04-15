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
    FCoverageTracker: TObject;  // TGocciaCoverageTracker, kept alive for merge
    FData: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TGocciaWorkQueue;
      AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean;
      ACancelOnError: Boolean; AEnableCoverage: Boolean; AData: Pointer);
    property Results: TGocciaWorkerResultArray read FResults;
    property ResultCount: Integer read FResultCount;
    property CoverageTracker: TObject read FCoverageTracker;
  end;

  { Thread pool that dispatches files to workers via a shared queue.
    Call RunAll to execute, then read Results in original file order. }
  TGocciaThreadPool = class
  private
    FWorkerCount: Integer;
    FResults: TGocciaWorkerResultArray;
    FWorkers: array of TGocciaFileWorker;
    FCancelled: Boolean;
    FCancelOnError: Boolean;
    FEnableCoverage: Boolean;
  public
    constructor Create(AWorkerCount: Integer);

    { Execute AFiles across worker threads. Blocks until all complete.
      AWorkerProc is called on each worker thread for each file.
      AData is an opaque pointer passed through to each worker call. }
    procedure RunAll(const AFiles: TStringList;
      AWorkerProc: TGocciaWorkerProc; AData: Pointer = nil);

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
  end;

  { Initialise the thread-local runtime for the calling thread.
    Must be called at the start of each worker thread.
    Set AEnableCoverage to initialise a per-thread coverage tracker. }
  procedure InitThreadRuntime(AEnableCoverage: Boolean = False);

  { Shut down the thread-local runtime for the calling thread.
    Must be called before the worker thread exits. }
  procedure ShutdownThreadRuntime;

implementation

uses
  Math,
  SysUtils,

  Goccia.CallStack,
  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.Primitives;

{ Thread runtime lifecycle }

procedure InitThreadRuntime(AEnableCoverage: Boolean);
begin
  TGarbageCollector.Initialize;
  // Disable automatic GC on worker threads. Shared immutable objects
  // (primitive singletons, shared prototypes) have a single FGCMark field
  // written by the mark phase — running GC on multiple threads would race
  // on that field. Each worker runs a single file and then shuts down, so
  // skipping collection is acceptable: all objects are freed in bulk when
  // the thread-local GC is destroyed.
  TGarbageCollector.Instance.Enabled := False;
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
  ACancelOnError: Boolean; AEnableCoverage: Boolean; AData: Pointer);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FQueue := AQueue;
  FWorkerProc := AWorkerProc;
  FCancelled := ACancelled;
  FCancelOnError := ACancelOnError;
  FEnableCoverage := AEnableCoverage;
  FCoverageTracker := nil;
  FData := AData;
  FResultCount := 0;
  // Pre-allocate generously; actual count tracked by FResultCount.
  SetLength(FResults, 0);
end;

procedure TGocciaFileWorker.Execute;
var
  Item: TGocciaWorkItem;
  ConsoleOut, ErrorMsg: string;
  Idx: Integer;
begin
  InitThreadRuntime(FEnableCoverage);
  try
    while FQueue.TryDequeue(Item) do
    begin
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

      { Cancel remaining files across all workers on first error. }
      if FCancelOnError and (not FResults[Idx].Success) then
        FCancelled^ := True;
    end;
  finally
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
end;

procedure TGocciaThreadPool.RunAll(const AFiles: TStringList;
  AWorkerProc: TGocciaWorkerProc; AData: Pointer);
var
  AllItems: TGocciaWorkItemArray;
  Queue: TGocciaWorkQueue;
  FileCount, I, J: Integer;
begin
  FCancelled := False;
  FileCount := AFiles.Count;
  if FileCount = 0 then
  begin
    SetLength(FResults, 0);
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
    for I := 0 to FWorkerCount - 1 do
      FWorkers[I] := nil;

    for I := 0 to FWorkerCount - 1 do
    begin
      FWorkers[I] := TGocciaFileWorker.Create(Queue, AWorkerProc,
        @FCancelled, FCancelOnError, FEnableCoverage, AData);
      FWorkers[I].Start;
    end;
  finally
    // Wait for all started workers before freeing the queue
    for I := 0 to FWorkerCount - 1 do
      if Assigned(FWorkers[I]) then
        FWorkers[I].WaitFor;
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
