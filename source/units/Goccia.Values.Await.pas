unit Goccia.Values.Await;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function AwaitValue(const AValue: TGocciaValue): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.Builtins.Atomics,
  Goccia.Constants.ErrorNames,
  Goccia.EngineFault,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FetchManager,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.MicrotaskQueue,
  Goccia.Timeout,
  Goccia.Timers,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

procedure DrainAwaitMicrotasks;
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  if Assigned(Queue) and Queue.HasPending then
    Queue.DrainOneJob;
end;

procedure DrainMicrotasksUntilPromiseSettled(
  const APromise: TGocciaPromiseValue);
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  while Assigned(APromise) and (APromise.State = gpsPending) and
        Assigned(Queue) and Queue.HasPending do
    Queue.DrainOneJob;
end;

{ A real-mode timer is the one continuation an await can be waiting on that no
  amount of microtask draining will produce. GocciaScript has no host event
  loop to hand control back to, so the awaiting frame runs the timer queue
  itself: the virtual clock jumps to the next due timer, the timer fires, its
  microtasks drain, and the loop asks again. No real time passes.

  Three things it deliberately does not do. Under fake timers it runs nothing —
  a suite that turned the clock over to `vi` decides when timers run, and an
  await that silently advanced it would take that decision away. It runs
  nothing for a queue belonging to another realm, so a ShadowRealm child's await
  cannot execute its parent's callbacks. And an exception a callback throws does
  not surface here: it is parked for the host, because in Node the awaiting
  frame is not the one that sees it. }
procedure RunTimersUntilPromiseSettled(const APromise: TGocciaPromiseValue);
var
  Iterations: Integer;
begin
  Iterations := 0;
  while Assigned(APromise) and (APromise.State = gpsPending) and
        (Iterations < TIMER_LOOP_LIMIT) do
  begin
    CheckExecutionTimeout;
    CheckInstructionLimit;
    if not RunOneRealTimer then
      Exit;
    Inc(Iterations);
    DrainMicrotasksUntilPromiseSettled(APromise);
  end;
  { The budget ran out with timers still runnable: name that rather than let it
    reach the caller as an ordinary unsettled promise. }
  if Assigned(APromise) and (APromise.State = gpsPending) and
     (Iterations >= TIMER_LOOP_LIMIT) and HasRunnableRealTimers then
    RaiseRealTimerLoopLimit;
end;

procedure RejectPromiseWithException(const APromise: TGocciaPromiseValue;
  const AException: Exception);
begin
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

// ES2026 §27.7.5.3 Await(value)
function AwaitValue(const AValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  PromiseRooted: Boolean;
begin
  Promise := nil;
  PromiseRooted := False;

  try
    if AValue is TGocciaPromiseValue then
    begin
      Promise := TGocciaPromiseValue(AValue);
      if (TGarbageCollector.Instance <> nil) then
      begin
        TGarbageCollector.Instance.AddTempRoot(Promise);
        PromiseRooted := True;
      end;
    end
    else
    begin
      Promise := TGocciaPromiseValue.Create;
      if (TGarbageCollector.Instance <> nil) then
      begin
        TGarbageCollector.Instance.AddTempRoot(Promise);
        PromiseRooted := True;
      end;
      try
        Promise.Resolve(AValue);
      except
        on E: TGocciaTimeoutError do
          raise;
        on E: TGocciaInstructionLimitError do
          raise;
        on E: TGocciaMemoryLimitError do
          raise;
        on E: Exception do
        begin
          if IsEngineIntegrityFault(E) then
            raise;
          RejectPromiseWithException(Promise, E);
        end;
      end;
    end;

    if Promise.State <> gpsPending then
    begin
      // ES2026 §27.7.5.3 step 3-4: Even already-settled promises introduce
      // a microtask boundary (the continuation is a PromiseReactionJob)
      DrainAwaitMicrotasks;
      if Promise.State = gpsFulfilled then
        Result := Promise.PromiseResult
      else
        raise TGocciaThrowValue.Create(Promise.PromiseResult);
      Exit;
    end;

    if Promise.State = gpsPending then
      DrainMicrotasksUntilPromiseSettled(Promise);

    WaitForFetchPromise(Promise);
    if Promise.State = gpsPending then
      WaitForAtomicsPromise(Promise);
    if Promise.State = gpsPending then
      DrainMicrotasksUntilPromiseSettled(Promise);
    if Promise.State = gpsPending then
      RunTimersUntilPromiseSettled(Promise);

    if Promise.State = gpsFulfilled then
      Result := Promise.PromiseResult
    else if Promise.State = gpsRejected then
      raise TGocciaThrowValue.Create(Promise.PromiseResult)
    else
      ThrowTypeError(SErrorAwaitPromiseUnsettled, SSuggestAwaitMicrotaskDrain);
  finally
    if PromiseRooted and (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;
end;

end.
