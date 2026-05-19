unit Goccia.Values.Await;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function AwaitValue(const AValue: TGocciaValue): TGocciaValue;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FetchManager,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue;

procedure DrainAwaitMicrotasks;
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  if Assigned(Queue) and Queue.HasPending then
    Queue.DrainQueue;
end;

// ES2026 §27.7.5.3 Await(value)
function AwaitValue(const AValue: TGocciaValue): TGocciaValue;
var
  ThenMethod: TGocciaValue;
  Promise: TGocciaPromiseValue;
  ThenArgs: TGocciaArgumentsCollection;
begin
  if AValue is TGocciaPromiseValue then
  begin
    Promise := TGocciaPromiseValue(AValue);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(Promise);
  end
  // ES2026 §25.6.4.6.1 PromiseResolve: If value is a thenable (object with
  // callable .then), wrap it in a Promise via resolve
  else if AValue is TGocciaObjectValue then
  begin
    ThenMethod := AValue.GetProperty(PROP_THEN);
    if Assigned(ThenMethod) and not (ThenMethod is TGocciaUndefinedLiteralValue) and ThenMethod.IsCallable then
    begin
      Promise := TGocciaPromiseValue.Create;
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(Promise);
      ThenArgs := TGocciaArgumentsCollection.Create([
        TGocciaNativeFunctionValue.Create(Promise.DoResolve, 'resolve', 1),
        TGocciaNativeFunctionValue.Create(Promise.DoReject, 'reject', 1)
      ]);
      try
        // ES2026 §25.6.4.6.1 step 9: If Call(then, ...) throws, reject promise
        try
          TGocciaFunctionBase(ThenMethod).Call(ThenArgs, AValue);
        except
          on E: TGocciaThrowValue do
            Promise.Reject(E.Value);
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
  end
  else
  begin
    // ES2026 §27.7.5.3 step 2: Await wraps the value in Promise.resolve(),
    // introducing a microtask boundary even for primitive values
    DrainAwaitMicrotasks;
    Result := AValue;
    Exit;
  end;

  try
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

    WaitForFetchPromise(Promise);

    if Promise.State = gpsFulfilled then
      Result := Promise.PromiseResult
    else if Promise.State = gpsRejected then
      raise TGocciaThrowValue.Create(Promise.PromiseResult)
    else
      ThrowTypeError(SErrorAwaitPromiseUnsettled, SSuggestAwaitMicrotaskDrain);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;
end;

end.
