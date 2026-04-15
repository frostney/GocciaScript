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
    FData: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AWorkItems: TGocciaWorkItemArray;
      AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean;
      AData: Pointer);
    property Results: TGocciaWorkerResultArray read FResults;
  end;

  { Simple thread pool that partitions files across N workers.
    Call RunAll to execute, then read Results in original file order. }
  TGocciaThreadPool = class
  private
    FWorkerCount: Integer;
    FResults: TGocciaWorkerResultArray;
    FCancelled: Boolean;
  public
    constructor Create(AWorkerCount: Integer);

    { Execute AFiles across worker threads. Blocks until all complete.
      AWorkerProc is called on each worker thread for each file.
      AData is an opaque pointer passed through to each worker call. }
    procedure RunAll(const AFiles: TStringList;
      AWorkerProc: TGocciaWorkerProc; AData: Pointer = nil);

    { Signal workers to skip remaining files. }
    procedure Cancel;

    property Results: TGocciaWorkerResultArray read FResults;
    property WorkerCount: Integer read FWorkerCount;
    property Cancelled: Boolean read FCancelled;
  end;

  { Initialise the thread-local runtime for the calling thread.
    Must be called at the start of each worker thread. }
  procedure InitThreadRuntime;

  { Shut down the thread-local runtime for the calling thread.
    Must be called before the worker thread exits. }
  procedure ShutdownThreadRuntime;

implementation

uses
  Math,
  SysUtils,

  Goccia.CallStack,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.Primitives;

{ Thread runtime lifecycle }

procedure InitThreadRuntime;
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
end;

procedure ShutdownThreadRuntime;
begin
  TGocciaMicrotaskQueue.Shutdown;
  TGocciaCallStack.Shutdown;
  TGarbageCollector.Shutdown;
end;

{ TGocciaFileWorker }

constructor TGocciaFileWorker.Create(const AWorkItems: TGocciaWorkItemArray;
  AWorkerProc: TGocciaWorkerProc; ACancelled: PBoolean; AData: Pointer);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FWorkItems := AWorkItems;
  FWorkerProc := AWorkerProc;
  FCancelled := ACancelled;
  FData := AData;
  SetLength(FResults, Length(FWorkItems));
end;

procedure TGocciaFileWorker.Execute;
var
  I: Integer;
  ConsoleOut, ErrorMsg: string;
begin
  InitThreadRuntime;
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
    end;
  finally
    ShutdownThreadRuntime;
  end;
end;

{ TGocciaThreadPool }

constructor TGocciaThreadPool.Create(AWorkerCount: Integer);
begin
  inherited Create;
  FWorkerCount := Max(1, AWorkerCount);
  FCancelled := False;
end;

procedure TGocciaThreadPool.RunAll(const AFiles: TStringList;
  AWorkerProc: TGocciaWorkerProc; AData: Pointer);
var
  Workers: array of TGocciaFileWorker;
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
  SetLength(Workers, FWorkerCount);
  for I := 0 to FWorkerCount - 1 do
  begin
    if Length(WorkItems[I]) > 0 then
    begin
      Workers[I] := TGocciaFileWorker.Create(WorkItems[I], AWorkerProc,
        @FCancelled, AData);
      Workers[I].Start;
    end
    else
      Workers[I] := nil;
  end;

  // Wait for all workers
  for I := 0 to FWorkerCount - 1 do
    if Assigned(Workers[I]) then
      Workers[I].WaitFor;

  // Collect results in original file order
  SetLength(FResults, FileCount);
  for I := 0 to FWorkerCount - 1 do
  begin
    if Assigned(Workers[I]) then
    begin
      // Check for unhandled exceptions in the worker thread
      if Assigned(Workers[I].FatalException) then
      begin
        WriteLn(StdErr, 'Worker thread ', I, ' fatal: ',
          Exception(Workers[I].FatalException).Message);
      end;
      for ResultIdx := 0 to High(Workers[I].Results) do
      begin
        if Workers[I].Results[ResultIdx].FileName <> '' then
          FResults[Workers[I].Results[ResultIdx].Index] := Workers[I].Results[ResultIdx]
        else
        begin
          { Worker died before processing this slot — fill from work items. }
          FResults[WorkItems[I][ResultIdx].Index].Index := WorkItems[I][ResultIdx].Index;
          FResults[WorkItems[I][ResultIdx].Index].FileName := WorkItems[I][ResultIdx].FileName;
          FResults[WorkItems[I][ResultIdx].Index].Success := False;
          if Assigned(Workers[I].FatalException) then
            FResults[WorkItems[I][ResultIdx].Index].ErrorMessage :=
              Exception(Workers[I].FatalException).Message
          else
            FResults[WorkItems[I][ResultIdx].Index].ErrorMessage := 'Worker terminated unexpectedly';
          FResults[WorkItems[I][ResultIdx].Index].ConsoleOutput := '';
          FResults[WorkItems[I][ResultIdx].Index].Data := nil;
        end;
      end;
      Workers[I].Free;
    end;
  end;
end;

procedure TGocciaThreadPool.Cancel;
begin
  FCancelled := True;
end;

end.
