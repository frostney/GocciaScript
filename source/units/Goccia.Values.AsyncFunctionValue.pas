unit Goccia.Values.AsyncFunctionValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Scope,
  Goccia.Values.FunctionValue,
  Goccia.Values.Primitives;

type
  TGocciaAsyncFunctionValue = class(TGocciaFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncArrowFunctionValue = class(TGocciaArrowFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncMethodValue = class(TGocciaMethodValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.PromiseValue;

procedure RejectPromiseWithException(const APromise: TGocciaPromiseValue;
  const AException: Exception);
begin
  if AException is TGocciaThrowValue then
    APromise.Reject(TGocciaThrowValue(AException).Value)
  else if AException is TGocciaTypeError then
    APromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
  else if AException is TGocciaReferenceError then
    APromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
  else if AException is TGocciaSyntaxError then
    APromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
  else if AException is TGocciaRuntimeError then
    APromise.Reject(CreateErrorObject(ERROR_NAME, AException.Message))
  else
    raise AException;
end;

{ TGocciaAsyncFunctionValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PushActiveRoot(CallScope);
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: Exception do
          RejectPromiseWithException(Promise, E);
      end;
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.PopActiveRoot;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

{ TGocciaAsyncArrowFunctionValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncArrowFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PushActiveRoot(CallScope);
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: Exception do
          RejectPromiseWithException(Promise, E);
      end;
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.PopActiveRoot;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

{ TGocciaAsyncMethodValue }

// ES2026 §27.7.5.1 AsyncFunctionStart(promiseCapability, asyncFunctionBody)
function TGocciaAsyncMethodValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  CallScope: TGocciaScope;
  BodyResult: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    CallScope := CreateCallScope;

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PushActiveRoot(CallScope);
    try
      try
        BodyResult := ExecuteBody(CallScope, AArguments, AThisValue);
        Promise.Resolve(BodyResult);
      except
        on E: Exception do
          RejectPromiseWithException(Promise, E);
      end;
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.PopActiveRoot;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

end.
