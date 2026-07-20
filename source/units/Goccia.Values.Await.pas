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
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FetchManager,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Timeout,
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
        on E: Exception do
          RejectPromiseWithException(Promise, E);
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
