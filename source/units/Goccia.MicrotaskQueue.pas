unit Goccia.MicrotaskQueue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AsyncContext,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TPromiseReactionType = (prtFulfill, prtReject, prtThenableResolve);

  TGocciaMicrotaskJob = class
  public
    procedure Execute; virtual; abstract;
    function Run(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure CaptureRoots(const AContainer: TGocciaObjectValue); virtual;
  end;

  TGocciaMicrotask = record
    Handler: TGocciaValue;
    ResultPromise: TGocciaValue;
    Value: TGocciaValue;
    ReactionType: TPromiseReactionType;
    { The async context this job runs under. Never set by the code that builds
      the record — Enqueue always overwrites it — so no enqueue site can leave
      it holding whatever the stack frame happened to contain. }
    Context: TGocciaAsyncContextSnapshot;
  end;

  TGocciaMicrotaskQueue = class
  private
    FQueue: TList<TGocciaMicrotask>;
    FFinalizationQueue: TList<TGocciaMicrotask>;
    FJobs: TDictionary<TGocciaValue, TGocciaMicrotaskJob>;
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

    { Enqueue captures the async context in effect right now. Use
      EnqueueWithContext for a job whose context was captured earlier — a
      promise reaction registered on a promise that was still pending runs
      under the context of its registration, not of its settlement. }
    procedure Enqueue(const AMicrotask: TGocciaMicrotask);
    procedure EnqueueWithContext(const AMicrotask: TGocciaMicrotask;
      const AContext: TGocciaAsyncContextSnapshot);
    procedure EnqueueJob(const AJob: TGocciaMicrotaskJob);
    procedure EnqueueFinalizationCleanup(const AMicrotask: TGocciaMicrotask);
    function DrainOneJob: Boolean;
    procedure DrainQueue;
    procedure ClearQueue;
    function HasPending: Boolean;
  end;

implementation

uses
  SysUtils,

  Goccia.Builtins.Atomics,
  Goccia.CapabilityAudit,
  Goccia.Constants.ErrorNames,
  Goccia.EngineFault,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

threadvar
  MicrotaskQueueThreadInstance: TGocciaMicrotaskQueue;

type
  TThenableResolvingFunctions = class(TGocciaObjectValue)
  private
    FPromise: TGocciaPromiseValue;
    FAlreadyResolved: Boolean;
  public
    constructor Create(const APromise: TGocciaPromiseValue);
    function Resolve(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Reject(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure RejectException(const AException: Exception);
    procedure MarkReferences; override;
  end;

{ TGocciaMicrotaskJob }

function TGocciaMicrotaskJob.Run(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Execute;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaMicrotaskJob.CaptureRoots(
  const AContainer: TGocciaObjectValue);
begin
end;

constructor TThenableResolvingFunctions.Create(
  const APromise: TGocciaPromiseValue);
begin
  inherited Create(nil);
  FPromise := APromise;
  FAlreadyResolved := False;
end;

function TThenableResolvingFunctions.Resolve(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then
    Exit;
  FAlreadyResolved := True;
  if AArgs.Length > 0 then
    FPromise.Resolve(AArgs.GetElement(0))
  else
    FPromise.Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TThenableResolvingFunctions.Reject(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then
    Exit;
  FAlreadyResolved := True;
  if AArgs.Length > 0 then
    FPromise.Reject(AArgs.GetElement(0))
  else
    FPromise.Reject(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TThenableResolvingFunctions.RejectException(
  const AException: Exception);
var
  RejectArgs: TGocciaArgumentsCollection;
  Reason: TGocciaValue;
begin
  if AException is EGocciaBytecodeThrow then
    Reason := EGocciaBytecodeThrow(AException).ThrownValue
  else if AException is TGocciaThrowValue then
    Reason := TGocciaThrowValue(AException).Value
  else if AException is TGocciaTypeError then
    Reason := CreateErrorObject(TYPE_ERROR_NAME, AException.Message)
  else if AException is TGocciaReferenceError then
    Reason := CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message)
  else if AException is TGocciaSyntaxError then
    Reason := CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message)
  else
    Reason := CreateErrorObject(ERROR_NAME, AException.Message);

  RejectArgs := TGocciaArgumentsCollection.Create([Reason]);
  try
    Reject(RejectArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    RejectArgs.Free;
  end;
end;

procedure TThenableResolvingFunctions.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
end;

procedure RejectPromiseWithException(const APromise: TGocciaPromiseValue;
  const AException: Exception);
begin
  { Precondition, shared with the sibling reject helpers in Goccia.Values.Await
    and Goccia.Interpreter: invoke this only from inside the `on E: Exception`
    handler that caught AException. With no promise to reject there is nothing
    to absorb the exception into, so it keeps unwinding to the host. `raise
    AException` re-raised the object by name, which starts a second propagation
    of an exception the enclosing handler still owns and frees on exit (the
    dangling re-raise behind the async-path access violations); a bare `raise`
    will not compile outside a lexical handler, so AcquireExceptionObject takes
    a reference to the in-flight exception and re-raises that same object. }
  if not Assigned(APromise) then
    raise Exception(AcquireExceptionObject);

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
  ResolvingFunctions: TThenableResolvingFunctions;
  ResolveFn: TGocciaNativeFunctionValue;
  RejectFn: TGocciaNativeFunctionValue;
  GC: TGarbageCollector;
begin
  if not Assigned(APromise) then
    Exit;

  ResolvingFunctions := TThenableResolvingFunctions.Create(APromise);
  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ResolvingFunctions.Resolve, '', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ResolvingFunctions.Reject, '', 1);
  ResolveFn.CapturedRoot := ResolvingFunctions;
  RejectFn.CapturedRoot := ResolvingFunctions;
  ThenArgs := TGocciaArgumentsCollection.Create([ResolveFn, RejectFn]);
  GC := TGarbageCollector.Instance;
  try
    if Assigned(GC) then
    begin
      GC.AddTempRoot(ResolvingFunctions);
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
        on E: TGocciaMemoryLimitError do
          raise;
        on E: EGocciaCapabilityAuditDeliveryError do
          raise;
        on E: Exception do
        begin
          if IsEngineIntegrityFault(E) then
            raise;
          ResolvingFunctions.RejectException(E);
        end;
      end;
    finally
      if Assigned(GC) then
      begin
        GC.RemoveTempRoot(RejectFn);
        GC.RemoveTempRoot(ResolveFn);
        GC.RemoveTempRoot(AThenMethod);
        GC.RemoveTempRoot(AThenable);
        GC.RemoveTempRoot(ResolvingFunctions);
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
  FJobs := TDictionary<TGocciaValue, TGocciaMicrotaskJob>.Create;
  FHead := 0;
  FFinalizationHead := 0;
end;

destructor TGocciaMicrotaskQueue.Destroy;
begin
  ClearQueue;
  FJobs.Free;
  FFinalizationQueue.Free;
  FQueue.Free;
  inherited;
end;

procedure TGocciaMicrotaskQueue.Enqueue(const AMicrotask: TGocciaMicrotask);
begin
  EnqueueWithContext(AMicrotask, CurrentAsyncContext);
end;

procedure TGocciaMicrotaskQueue.EnqueueWithContext(
  const AMicrotask: TGocciaMicrotask;
  const AContext: TGocciaAsyncContextSnapshot);
var
  Task: TGocciaMicrotask;
begin
  Task := AMicrotask;
  Task.Context := AContext;
  AddQueuedRoots(Task);
  FQueue.Add(Task);
end;

procedure TGocciaMicrotaskQueue.EnqueueJob(const AJob: TGocciaMicrotaskJob);
var
  Task: TGocciaMicrotask;
  Handler: TGocciaNativeFunctionValue;
  Roots: TGocciaObjectValue;
  RootsRoot: TGocciaTempRoot;
  HandlerRoot: TGocciaTempRoot;
  JobOwnedByQueue: Boolean;
begin
  InitializeTempRoot(RootsRoot);
  InitializeTempRoot(HandlerRoot);
  JobOwnedByQueue := False;
  try
    Roots := TGocciaObjectValue.Create(nil);
    AddTempRootIfNeeded(RootsRoot, Roots);
    AJob.CaptureRoots(Roots);
    Handler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      AJob.Run, '', 0);
    AddTempRootIfNeeded(HandlerRoot, Handler);
    Handler.CapturedRoot := Roots;
    FJobs.Add(Handler, AJob);
    Task.Handler := Handler;
    Task.ResultPromise := nil;
    Task.Value := nil;
    Task.ReactionType := prtFulfill;
    try
      Enqueue(Task);
      JobOwnedByQueue := True;
    except
      FJobs.Remove(Handler);
      raise;
    end;
  finally
    RemoveTempRootIfNeeded(HandlerRoot);
    RemoveTempRootIfNeeded(RootsRoot);
    if not JobOwnedByQueue then
      AJob.Free;
  end;
end;

procedure TGocciaMicrotaskQueue.EnqueueFinalizationCleanup(
  const AMicrotask: TGocciaMicrotask);
var
  Task: TGocciaMicrotask;
begin
  Task := AMicrotask;
  Task.Context := CurrentAsyncContext;
  AddQueuedRoots(Task);
  FFinalizationQueue.Add(Task);
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
  if Assigned(AMicrotask.Context) then
    GC.AddQueuedRoot(AMicrotask.Context);
end;

procedure TGocciaMicrotaskQueue.RemoveQueuedRoots(
  const AMicrotask: TGocciaMicrotask);
var
  GC: TGarbageCollector;
  Job: TGocciaMicrotaskJob;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    if Assigned(AMicrotask.Handler) then
      GC.RemoveQueuedRoot(AMicrotask.Handler);
    if Assigned(AMicrotask.Value) then
      GC.RemoveQueuedRoot(AMicrotask.Value);
    if Assigned(AMicrotask.ResultPromise) then
      GC.RemoveQueuedRoot(AMicrotask.ResultPromise);
    if Assigned(AMicrotask.Context) then
      GC.RemoveQueuedRoot(AMicrotask.Context);
  end;
  if Assigned(AMicrotask.Handler) and
     FJobs.TryGetValue(AMicrotask.Handler, Job) then
  begin
    FJobs.Remove(AMicrotask.Handler);
    Job.Free;
  end;
end;

procedure TGocciaMicrotaskQueue.ExecuteTask(
  const ATask: TGocciaMicrotask);
var
  Promise: TGocciaPromiseValue;
  Capability: TGocciaPromiseReactionCapability;
  HandlerResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  PreviousContext: TGocciaAsyncContextSnapshot;
  procedure ResolveResult(const AValue: TGocciaValue);
  begin
    if Assigned(Capability) then
      Capability.Resolve(AValue)
    else if Assigned(Promise) then
      Promise.Resolve(AValue);
  end;

  procedure RejectResult(const AValue: TGocciaValue);
  begin
    if Assigned(Capability) then
      Capability.Reject(AValue)
    else if Assigned(Promise) then
      Promise.Reject(AValue);
  end;

  procedure RejectExceptionResult(const AException: Exception);
  begin
    if AException is TGocciaTypeError then
      RejectResult(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
    else if AException is TGocciaReferenceError then
      RejectResult(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
    else if AException is TGocciaSyntaxError then
      RejectResult(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
    else
      RejectResult(CreateErrorObject(ERROR_NAME, AException.Message));
  end;
begin
  { Every job runs under the async context that was in effect where its
    continuation was created, and leaves the enclosing context untouched. The
    restore is what keeps a nested drain — an `await` inside a handler pumps
    this queue re-entrantly — from leaking one continuation's bindings into
    the frame that drained it. }
  PreviousContext := CurrentAsyncContext;
  SetCurrentAsyncContext(ATask.Context);
  try

  Promise := nil;
  Capability := nil;
  if ATask.ResultPromise is TGocciaPromiseValue then
    Promise := TGocciaPromiseValue(ATask.ResultPromise)
  else if ATask.ResultPromise is TGocciaPromiseReactionCapability then
    Capability := TGocciaPromiseReactionCapability(ATask.ResultPromise);

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
        HandlerResult := DispatchCall(ATask.Handler, CallArgs,
          TGocciaUndefinedLiteralValue.UndefinedValue);
        ResolveResult(HandlerResult);
      except
        on E: EGocciaBytecodeThrow do
          if Assigned(Promise) or Assigned(Capability) then
            RejectResult(E.ThrownValue)
          else
            raise;
        on E: TGocciaThrowValue do
          if Assigned(Promise) or Assigned(Capability) then
            RejectResult(E.Value)
          else
            raise;
        on E: TGocciaTimeoutError do
          raise;
        on E: TGocciaInstructionLimitError do
          raise;
        on E: TGocciaMemoryLimitError do
          raise;
        on E: EGocciaCapabilityAuditDeliveryError do
          raise;
        on E: TGocciaTypeError do
          if Assigned(Promise) or Assigned(Capability) then
            RejectExceptionResult(E)
          else
            raise;
        on E: TGocciaReferenceError do
          if Assigned(Promise) or Assigned(Capability) then
            RejectExceptionResult(E)
          else
            raise;
        on E: TGocciaSyntaxError do
          if Assigned(Promise) or Assigned(Capability) then
            RejectExceptionResult(E)
          else
            raise;
        on E: Exception do
        begin
          if IsEngineIntegrityFault(E) then
            raise;
          if Assigned(Promise) or Assigned(Capability) then
            RejectExceptionResult(E)
          else
            raise;
        end;
      end;
    finally
      CallArgs.Free;
    end;
  end
  else
  begin
    if Assigned(Promise) or Assigned(Capability) then
    begin
      case ATask.ReactionType of
        prtFulfill: ResolveResult(ATask.Value);
        prtReject: RejectResult(ATask.Value);
        prtThenableResolve:;
      end;
    end;
  end;

  finally
    SetCurrentAsyncContext(PreviousContext);
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
      PumpAtomicsWaitAsyncCompletions;
      if (TGarbageCollector.Instance <> nil) then
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
      PumpAtomicsWaitAsyncCompletions;
      if (TGarbageCollector.Instance <> nil) then
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
        PumpAtomicsWaitAsyncCompletions;
        if (TGarbageCollector.Instance <> nil) then
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
        PumpAtomicsWaitAsyncCompletions;
        if (TGarbageCollector.Instance <> nil) then
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
