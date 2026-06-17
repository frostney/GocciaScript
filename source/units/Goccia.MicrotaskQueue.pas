unit Goccia.MicrotaskQueue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Values.Primitives;

type
  TPromiseReactionType = (prtFulfill, prtReject, prtThenableResolve);

  TGocciaMicrotask = record
    Handler: TGocciaValue;
    ResultPromise: TGocciaValue;
    Value: TGocciaValue;
    ReactionType: TPromiseReactionType;
  end;

  TGocciaMicrotaskQueue = class
  private
    FQueue: TList<TGocciaMicrotask>;
    FFinalizationQueue: TList<TGocciaMicrotask>;
    FHead: Integer;
    FFinalizationHead: Integer;
    procedure AddQueuedRoots(const AMicrotask: TGocciaMicrotask);
    procedure RemoveQueuedRoots(const AMicrotask: TGocciaMicrotask);
    procedure ExecuteTask(const ATask: TGocciaMicrotask);
    procedure CompactQueueIfEmpty;
    procedure CompactFinalizationQueueIfEmpty;
  public
    class function Instance: TGocciaMicrotaskQueue;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(const AMicrotask: TGocciaMicrotask);
    procedure EnqueueFinalizationCleanup(const AMicrotask: TGocciaMicrotask);
    function DrainOneJob: Boolean;
    procedure DrainQueue;
    procedure ClearQueue;
    function HasPending: Boolean;
  end;

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.ErrorNames,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

threadvar
  MicrotaskQueueThreadInstance: TGocciaMicrotaskQueue;

procedure RejectPromiseWithException(const APromise: TGocciaPromiseValue;
  const AException: Exception);
begin
  if not Assigned(APromise) then
    raise AException;

  if AException is EGocciaBytecodeThrow then
    APromise.Reject(EGocciaBytecodeThrow(AException).ThrownValue)
  else if AException is TGocciaThrowValue then
    APromise.Reject(TGocciaThrowValue(AException).Value)
  else if AException is TGocciaTypeError then
    APromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
  else if AException is TGocciaReferenceError then
    APromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
  else if AException is TGocciaSyntaxError then
    APromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
  else
    APromise.Reject(CreateErrorObject(ERROR_NAME, AException.Message));
end;

procedure ExecutePromiseResolveThenableJob(const APromise: TGocciaPromiseValue;
  const AThenable, AThenMethod: TGocciaValue);
var
  ThenArgs: TGocciaArgumentsCollection;
  ResolveFn: TGocciaNativeFunctionValue;
  RejectFn: TGocciaNativeFunctionValue;
  GC: TGarbageCollector;
begin
  if not Assigned(APromise) then
    Exit;

  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    APromise.DoResolve, '', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    APromise.DoReject, '', 1);
  ResolveFn.CapturedRoot := APromise;
  RejectFn.CapturedRoot := APromise;
  ThenArgs := TGocciaArgumentsCollection.Create([ResolveFn, RejectFn]);
  GC := TGarbageCollector.Instance;
  try
    if Assigned(GC) then
    begin
      GC.AddTempRoot(AThenable);
      GC.AddTempRoot(AThenMethod);
      GC.AddTempRoot(ResolveFn);
      GC.AddTempRoot(RejectFn);
    end;
    try
      try
        DispatchCall(AThenMethod, ThenArgs, AThenable);
      except
        on E: TGocciaTimeoutError do
          raise;
        on E: TGocciaInstructionLimitError do
          raise;
        on E: Exception do
          RejectPromiseWithException(APromise, E);
      end;
    finally
      if Assigned(GC) then
      begin
        GC.RemoveTempRoot(RejectFn);
        GC.RemoveTempRoot(ResolveFn);
        GC.RemoveTempRoot(AThenMethod);
        GC.RemoveTempRoot(AThenable);
      end;
    end;
  finally
    ThenArgs.Free;
  end;
end;

class function TGocciaMicrotaskQueue.Instance: TGocciaMicrotaskQueue;
begin
  Result := MicrotaskQueueThreadInstance;
end;

class procedure TGocciaMicrotaskQueue.Initialize;
begin
  if not Assigned(MicrotaskQueueThreadInstance) then
    MicrotaskQueueThreadInstance := TGocciaMicrotaskQueue.Create;
end;

class procedure TGocciaMicrotaskQueue.Shutdown;
begin
  FreeAndNil(MicrotaskQueueThreadInstance);
end;

constructor TGocciaMicrotaskQueue.Create;
begin
  FQueue := TList<TGocciaMicrotask>.Create;
  FFinalizationQueue := TList<TGocciaMicrotask>.Create;
  FHead := 0;
  FFinalizationHead := 0;
end;

destructor TGocciaMicrotaskQueue.Destroy;
begin
  FFinalizationQueue.Free;
  FQueue.Free;
  inherited;
end;

procedure TGocciaMicrotaskQueue.Enqueue(const AMicrotask: TGocciaMicrotask);
begin
  AddQueuedRoots(AMicrotask);
  FQueue.Add(AMicrotask);
end;

procedure TGocciaMicrotaskQueue.EnqueueFinalizationCleanup(
  const AMicrotask: TGocciaMicrotask);
begin
  AddQueuedRoots(AMicrotask);
  FFinalizationQueue.Add(AMicrotask);
end;

procedure TGocciaMicrotaskQueue.AddQueuedRoots(
  const AMicrotask: TGocciaMicrotask);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  if Assigned(AMicrotask.Handler) then
    GC.AddQueuedRoot(AMicrotask.Handler);
  if Assigned(AMicrotask.Value) then
    GC.AddQueuedRoot(AMicrotask.Value);
  if Assigned(AMicrotask.ResultPromise) then
    GC.AddQueuedRoot(AMicrotask.ResultPromise);
end;

procedure TGocciaMicrotaskQueue.RemoveQueuedRoots(
  const AMicrotask: TGocciaMicrotask);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  if Assigned(AMicrotask.Handler) then
    GC.RemoveQueuedRoot(AMicrotask.Handler);
  if Assigned(AMicrotask.Value) then
    GC.RemoveQueuedRoot(AMicrotask.Value);
  if Assigned(AMicrotask.ResultPromise) then
    GC.RemoveQueuedRoot(AMicrotask.ResultPromise);
end;

procedure TGocciaMicrotaskQueue.ExecuteTask(
  const ATask: TGocciaMicrotask);
var
  Promise: TGocciaPromiseValue;
  HandlerResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  Promise := TGocciaPromiseValue(ATask.ResultPromise);

  if ATask.ReactionType = prtThenableResolve then
  begin
    if Assigned(Promise) then
    begin
      if Assigned(ATask.Handler) then
        ExecutePromiseResolveThenableJob(Promise, ATask.Value, ATask.Handler)
      else if ATask.Value is TGocciaPromiseValue then
        Promise.SubscribeTo(TGocciaPromiseValue(ATask.Value));
    end;
    Exit;
  end;

  if Assigned(ATask.Handler) and ATask.Handler.IsCallable then
  begin
    CallArgs := TGocciaArgumentsCollection.Create([ATask.Value]);
    try
      try
        HandlerResult := TGocciaFunctionBase(ATask.Handler).Call(
          CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        if Assigned(Promise) then
          Promise.Resolve(HandlerResult);
      except
        on E: EGocciaBytecodeThrow do
          if Assigned(Promise) then
            Promise.Reject(E.ThrownValue)
          else
            raise;
        on E: TGocciaThrowValue do
          if Assigned(Promise) then
            Promise.Reject(E.Value)
          else
            raise;
      end;
    finally
      CallArgs.Free;
    end;
  end
  else
  begin
    if Assigned(Promise) then
    begin
      case ATask.ReactionType of
        prtFulfill: Promise.Resolve(ATask.Value);
        prtReject: Promise.Reject(ATask.Value);
        prtThenableResolve:;
      end;
    end;
  end;
end;

procedure TGocciaMicrotaskQueue.CompactQueueIfEmpty;
begin
  if FHead >= FQueue.Count then
  begin
    FQueue.Clear;
    FHead := 0;
  end;
end;

procedure TGocciaMicrotaskQueue.CompactFinalizationQueueIfEmpty;
begin
  if FFinalizationHead >= FFinalizationQueue.Count then
  begin
    FFinalizationQueue.Clear;
    FFinalizationHead := 0;
  end;
end;

function TGocciaMicrotaskQueue.DrainOneJob: Boolean;
var
  Task: TGocciaMicrotask;
begin
  Result := False;
  if FHead < FQueue.Count then
  begin
    CheckExecutionTimeout;
    CheckInstructionLimit;
    Task := FQueue[FHead];
    Inc(FHead);
    try
      ExecuteTask(Task);
    finally
      RemoveQueuedRoots(Task);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.ClearKeptObjects;
    end;
    CompactQueueIfEmpty;
    Exit(True);
  end;

  if FFinalizationHead < FFinalizationQueue.Count then
  begin
    CheckExecutionTimeout;
    CheckInstructionLimit;
    Task := FFinalizationQueue[FFinalizationHead];
    Inc(FFinalizationHead);
    try
      ExecuteTask(Task);
    finally
      RemoveQueuedRoots(Task);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.ClearKeptObjects;
    end;
    CompactFinalizationQueueIfEmpty;
    Exit(True);
  end;
end;

procedure TGocciaMicrotaskQueue.DrainQueue;
var
  Task: TGocciaMicrotask;
begin
  // Advance the head index BEFORE running each task so that recursive
  // DrainQueue calls (e.g. when a handler awaits a settled promise, which
  // drains the microtask queue during AwaitValue) only see
  // remaining and newly-enqueued tasks rather than re-executing the in-flight
  // one. A previous implementation kept tasks in the queue until a final
  // Clear at the end, so any nested drain re-ran every already-processed
  // task, producing infinite recursion when a microtask handler eventually
  // re-entered the same path (observed as SIGSEGV from stack overflow with
  // async-generator yields of rejected promises).
  //
  // Using a head index instead of TList.Delete(0) keeps each pop O(1);
  // shifting the whole list per task would otherwise be O(n^2) for large
  // microtask bursts (Promise-heavy fan-outs, await-loops in async iterators).
  // Once FHead catches up to Count we compact by clearing the underlying list
  // so the buffer does not grow unboundedly across drains.
  while (FHead < FQueue.Count) or
        (FFinalizationHead < FFinalizationQueue.Count) do
  begin
    while FHead < FQueue.Count do
    begin
      CheckExecutionTimeout;
      CheckInstructionLimit;
      Task := FQueue[FHead];
      Inc(FHead);
      try
        ExecuteTask(Task);
      finally
        RemoveQueuedRoots(Task);
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.ClearKeptObjects;
      end;
    end;
    CompactQueueIfEmpty;

    if FFinalizationHead < FFinalizationQueue.Count then
    begin
      CheckExecutionTimeout;
      CheckInstructionLimit;
      Task := FFinalizationQueue[FFinalizationHead];
      Inc(FFinalizationHead);
      try
        ExecuteTask(Task);
      finally
        RemoveQueuedRoots(Task);
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.ClearKeptObjects;
      end;
      CompactFinalizationQueueIfEmpty;
    end;
  end;

  // Queue is logically empty (FHead caught up to Count). Compact the list so
  // already-processed records do not retain Pascal-side references (e.g. via
  // the underlying TList<TGocciaMicrotask> array) any longer than necessary
  // and so FHead/Count cannot drift unbounded across many drains.
  if FHead >= FQueue.Count then
    CompactQueueIfEmpty;
end;

procedure TGocciaMicrotaskQueue.ClearQueue;
var
  I: Integer;
  Task: TGocciaMicrotask;
begin
  for I := FHead to FQueue.Count - 1 do
  begin
    Task := FQueue[I];
    RemoveQueuedRoots(Task);
  end;
  for I := FFinalizationHead to FFinalizationQueue.Count - 1 do
  begin
    Task := FFinalizationQueue[I];
    RemoveQueuedRoots(Task);
  end;
  FQueue.Clear;
  FFinalizationQueue.Clear;
  FHead := 0;
  FFinalizationHead := 0;
end;

function TGocciaMicrotaskQueue.HasPending: Boolean;
begin
  Result := (FHead < FQueue.Count) or
    (FFinalizationHead < FFinalizationQueue.Count);
end;

end.
