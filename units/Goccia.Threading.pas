{ Thread pool for parallel file execution.

  Provides TGocciaThreadPool which dispatches file-level work items to a
  configurable number of worker threads. Each worker initialises its own
  thread-local runtime (GC, CallStack, MicrotaskQueue) and shuts it down
  on completion, so there is zero shared mutable state between workers.

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

  TGocciaFileWorker = class(TThread)
  private
    FWorkItems: TGocciaWorkItemArray;
    FResults: TGocciaWorkerResultArray;
    FWorkerProc: TGocciaWorkerProc;
    FCancelled: PBoolean;
    FCancelOnError: Boolean;
    FEnableCoverage: Boolean;
    FCoverageTracker: TObject;  // TGocciaCoverageTracker, kept alive for merge
    FData: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AWorkItems: TGocciaWorkItemArray;
      AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean;
      ACancelOnError: Boolean; AEnableCoverage: Boolean; AData: Pointer);
    property Results: TGocciaWorkerResultArray read FResults;
    property CoverageTracker: TObject read FCoverageTracker;
  end;

  { Simple thread pool that partitions files across N workers.
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
    { When True, the first worker error automatically cancels remaining files. }
    property CancelOnError: Boolean read FCancelOnError write FCancelOnError;
    { When True, each worker initialises a per-thread coverage tracker.
      After RunAll, use MergeCoverage to collect results. }
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

{ TGocciaFileWorker }

constructor TGocciaFileWorker.Create(const AWorkItems: TGocciaWorkItemArray;
  AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean;
  ACancelOnError: Boolean; AEnableCoverage: Boolean; AData: Pointer);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FWorkItems := AWorkItems;
  FWorkerProc := AWorkerProc;
  FCancelled := ACancelled;
  FCancelOnError := ACancelOnError;
  FEnableCoverage := AEnableCoverage;
  FCoverageTracker := nil;
  FData := AData;
  SetLength(FResults, Length(FWorkItems));
end;

procedure TGocciaFileWorker.Execute;
var
  I: Integer;
  ConsoleOut, ErrorMsg: string;
begin
  InitThreadRuntime(FEnableCoverage);
  try
    for I := 0 to High(FWorkItems) do
    begin
      if FCancelled^ then
      begin
        FResults[I].Index := FWorkItems[I].Index;
        FResults[I].FileName := FWorkItems[I].FileName;
        FResults[I].Success := False;
        FResults[I].ErrorMessage := 'Cancelled';
        FResults[I].ConsoleOutput := '';
        FResults[I].Data := nil;
        Continue;
      end;

      ConsoleOut := '';
      ErrorMsg := '';
      FResults[I].Index := FWorkItems[I].Index;
      FResults[I].FileName := FWorkItems[I].FileName;
      FResults[I].Data := FData;
      try
        FWorkerProc(FWorkItems[I].FileName, FWorkItems[I].Index,
          ConsoleOut, ErrorMsg, FData);
        FResults[I].Success := ErrorMsg = '';
        FResults[I].ErrorMessage := ErrorMsg;
        FResults[I].ConsoleOutput := ConsoleOut;
      except
        on E: Exception do
        begin
          FResults[I].Success := False;
          FResults[I].ErrorMessage := E.ClassName + ': ' + E.Message;
          FResults[I].ConsoleOutput := ConsoleOut;
        end;
      end;

      { Cancel remaining files across all workers on first error. }
      if FCancelOnError and (not FResults[I].Success) then
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
  WorkItems: array of TGocciaWorkItemArray;
  FileCount, I, WorkerIdx, ResultIdx: Integer;
begin
  FileCount := AFiles.Count;
  if FileCount = 0 then
  begin
    SetLength(FResults, 0);
    Exit;
  end;

  // Partition files round-robin across workers
  SetLength(WorkItems, FWorkerCount);
  for I := 0 to FWorkerCount - 1 do
    SetLength(WorkItems[I], 0);

  for I := 0 to FileCount - 1 do
  begin
    WorkerIdx := I mod FWorkerCount;
    SetLength(WorkItems[WorkerIdx], Length(WorkItems[WorkerIdx]) + 1);
    WorkItems[WorkerIdx][High(WorkItems[WorkerIdx])].FileName := AFiles[I];
    WorkItems[WorkerIdx][High(WorkItems[WorkerIdx])].Index := I;
  end;

  // Create and start workers
  SetLength(FWorkers, FWorkerCount);
  for I := 0 to FWorkerCount - 1 do
  begin
    if Length(WorkItems[I]) > 0 then
    begin
      FWorkers[I] := TGocciaFileWorker.Create(WorkItems[I], AWorkerProc,
        @FCancelled, FCancelOnError, FEnableCoverage, AData);
      FWorkers[I].Start;
    end
    else
      FWorkers[I] := nil;
  end;

  // Wait for all workers
  for I := 0 to FWorkerCount - 1 do
    if Assigned(FWorkers[I]) then
      FWorkers[I].WaitFor;

  // Collect results in original file order
  SetLength(FResults, FileCount);
  for I := 0 to FWorkerCount - 1 do
  begin
    if Assigned(FWorkers[I]) then
    begin
      // Check for unhandled exceptions in the worker thread
      if Assigned(FWorkers[I].FatalException) then
      begin
        WriteLn(StdErr, 'Worker thread ', I, ' fatal: ',
          Exception(FWorkers[I].FatalException).Message);
      end;
      for ResultIdx := 0 to High(FWorkers[I].Results) do
      begin
        if FWorkers[I].Results[ResultIdx].FileName <> '' then
          FResults[FWorkers[I].Results[ResultIdx].Index] := FWorkers[I].Results[ResultIdx]
        else
        begin
          { Worker died before processing this slot — fill from work items. }
          FResults[WorkItems[I][ResultIdx].Index].Index := WorkItems[I][ResultIdx].Index;
          FResults[WorkItems[I][ResultIdx].Index].FileName := WorkItems[I][ResultIdx].FileName;
          FResults[WorkItems[I][ResultIdx].Index].Success := False;
          if Assigned(FWorkers[I].FatalException) then
            FResults[WorkItems[I][ResultIdx].Index].ErrorMessage :=
              Exception(FWorkers[I].FatalException).Message
          else
            FResults[WorkItems[I][ResultIdx].Index].ErrorMessage := 'Worker terminated unexpectedly';
          FResults[WorkItems[I][ResultIdx].Index].ConsoleOutput := '';
          FResults[WorkItems[I][ResultIdx].Index].Data := nil;
        end;
      end;
      { Don't free workers yet if coverage is enabled — MergeCoverageInto
        needs access to their coverage trackers. }
      if not FEnableCoverage then
      begin
        FWorkers[I].Free;
        FWorkers[I] := nil;
      end;
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
