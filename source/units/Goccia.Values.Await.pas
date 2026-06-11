unit Goccia.Values.Await;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function AwaitValue(const AValue: TGocciaValue): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Atomics,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
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
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
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
  ThenMethod: TGocciaValue;
  Promise: TGocciaPromiseValue;
  PromiseRooted: Boolean;
  ThenArgs: TGocciaArgumentsCollection;
begin
  Promise := nil;
  PromiseRooted := False;

  try
    if AValue is TGocciaPromiseValue then
    begin
      Promise := TGocciaPromiseValue(AValue);
      if Assigned(TGarbageCollector.Instance) then
      begin
        TGarbageCollector.Instance.AddTempRoot(Promise);
        PromiseRooted := True;
      end;
    end
    // ES2026 §25.6.4.6.1 PromiseResolve: If value is a thenable (object with
    // callable .then), wrap it in a Promise via resolve
    else if AValue is TGocciaObjectValue then
    begin
      Promise := TGocciaPromiseValue.Create;
      if Assigned(TGarbageCollector.Instance) then
      begin
        TGarbageCollector.Instance.AddTempRoot(Promise);
        PromiseRooted := True;
      end;

      try
        ThenMethod := AValue.GetProperty(PROP_THEN);
        if Assigned(ThenMethod) and not (ThenMethod is TGocciaUndefinedLiteralValue) and ThenMethod.IsCallable then
        begin
          ThenArgs := TGocciaArgumentsCollection.Create([
            TGocciaNativeFunctionValue.Create(Promise.DoResolve, 'resolve', 1),
            TGocciaNativeFunctionValue.Create(Promise.DoReject, 'reject', 1)
          ]);
          try
            // ES2026 §25.6.4.6.1 step 9: If Call(then, ...) throws, reject promise
            try
              TGocciaFunctionBase(ThenMethod).Call(ThenArgs, AValue);
            except
              on E: TGocciaTimeoutError do
                raise;
              on E: TGocciaInstructionLimitError do
                raise;
              on E: Exception do
                RejectPromiseWithException(Promise, E);
            end;
          finally
            ThenArgs.Free;
          end;
        end
        else
        begin
          // ES2026 §27.7.5.3 step 2: Await wraps the value in Promise.resolve(),
          // introducing a microtask boundary even for non-thenable objects
          DrainAwaitMicrotasks;
          Result := AValue;
          Exit;
        end;
      except
        on E: TGocciaTimeoutError do
          raise;
        on E: TGocciaInstructionLimitError do
          raise;
        on E: Exception do
          RejectPromiseWithException(Promise, E);
      end;
    end
    else
    begin
      // ES2026 §27.7.5.3 step 2: Await wraps the value in Promise.resolve(),
      // introducing a microtask boundary even for primitive values
      DrainAwaitMicrotasks;
      Result := AValue;
      Exit;
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
    if PromiseRooted and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;
end;

end.
