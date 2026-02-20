unit Goccia.Builtins.GlobalPromise;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaGlobalPromise = class(TGocciaBuiltin)
  private
    FPromiseConstructor: TGocciaNativeFunctionValue;

    function PromiseConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseResolve(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseReject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAllSettled(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseRace(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAny(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ExtractPromiseArray(const AArgs: TGocciaArgumentsCollection): TGocciaArrayValue;
    function PromiseWithResolvers(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseTry(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue;

type
  TPromiseAllState = class(TGocciaObjectValue)
  private
    FResults: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
    procedure MarkReferences; override;
    property Results: TGocciaArrayValue read FResults;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaPromiseValue read FResultPromise;
    property Settled: Boolean read FSettled write FSettled;
  end;

  TPromiseAllHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
  public
    constructor Create(const AState: TPromiseAllState);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllSettledFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllSettledRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseRaceHandler = class(TGocciaObjectValue)
  private
    FResultPromise: TGocciaPromiseValue;
    FIsResolve: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue; const AIsResolve: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAnyState = class(TGocciaObjectValue)
  private
    FErrors: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
    procedure MarkReferences; override;
    property Errors: TGocciaArrayValue read FErrors;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaPromiseValue read FResultPromise;
    property Settled: Boolean read FSettled write FSettled;
  end;

  TPromiseAnyFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAnyState;
  public
    constructor Create(const AState: TPromiseAnyState);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAnyRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAnyState;
    FIndex: Integer;
  public
    constructor Create(const AState: TPromiseAnyState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

{ Helper: wrap value as Promise if not already one }
function WrapAsPromise(const AValue: TGocciaValue): TGocciaPromiseValue;
var
  P: TGocciaPromiseValue;
begin
  if AValue is TGocciaPromiseValue then
    Result := TGocciaPromiseValue(AValue)
  else
  begin
    P := TGocciaPromiseValue.Create;
    P.Resolve(AValue);
    Result := P;
  end;
end;

{ TPromiseAllState }

constructor TPromiseAllState.Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FRemaining := ACount;
  FSettled := False;
  FResults := TGocciaArrayValue.Create;
  for I := 0 to ACount - 1 do
    FResults.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TPromiseAllState.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResults) then FResults.MarkReferences;
  if Assigned(FResultPromise) then FResultPromise.MarkReferences;
end;

{ TPromiseAllHandler }

constructor TPromiseAllHandler.Create(const AState: TPromiseAllState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Results.Elements[FIndex] := AArgs.GetElement(0);
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAllRejectHandler }

constructor TPromiseAllRejectHandler.Create(const AState: TPromiseAllState);
begin
  inherited Create(nil);
  FState := AState;
end;

function TPromiseAllRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Settled := True;
  FState.ResultPromise.Reject(AArgs.GetElement(0));
end;

procedure TPromiseAllRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAllSettledFulfillHandler }

constructor TPromiseAllSettledFulfillHandler.Create(const AState: TPromiseAllState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllSettledFulfillHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  Entry := TGocciaObjectValue.Create;
  Entry.AssignProperty('status', TGocciaStringLiteralValue.Create('fulfilled'));
  Entry.AssignProperty('value', AArgs.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllSettledFulfillHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAllSettledRejectHandler }

constructor TPromiseAllSettledRejectHandler.Create(const AState: TPromiseAllState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAllSettledRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  Entry := TGocciaObjectValue.Create;
  Entry.AssignProperty('status', TGocciaStringLiteralValue.Create('rejected'));
  Entry.AssignProperty('reason', AArgs.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    FState.ResultPromise.Resolve(FState.Results);
  end;
end;

procedure TPromiseAllSettledRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseRaceHandler }

constructor TPromiseRaceHandler.Create(const AResultPromise: TGocciaPromiseValue; const AIsResolve: Boolean);
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FIsResolve := AIsResolve;
end;

function TPromiseRaceHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FIsResolve then
    FResultPromise.Resolve(AArgs.GetElement(0))
  else
    FResultPromise.Reject(AArgs.GetElement(0));
end;

procedure TPromiseRaceHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResultPromise) then FResultPromise.MarkReferences;
end;

{ TPromiseAnyState }

constructor TPromiseAnyState.Create(const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FRemaining := ACount;
  FSettled := False;
  FErrors := TGocciaArrayValue.Create;
  for I := 0 to ACount - 1 do
    FErrors.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TPromiseAnyState.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FErrors) then FErrors.MarkReferences;
  if Assigned(FResultPromise) then FResultPromise.MarkReferences;
end;

{ TPromiseAnyFulfillHandler }

constructor TPromiseAnyFulfillHandler.Create(const AState: TPromiseAnyState);
begin
  inherited Create(nil);
  FState := AState;
end;

function TPromiseAnyFulfillHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Settled := True;
  FState.ResultPromise.Resolve(AArgs.GetElement(0));
end;

procedure TPromiseAnyFulfillHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TPromiseAnyRejectHandler }

constructor TPromiseAnyRejectHandler.Create(const AState: TPromiseAnyState; const AIndex: Integer);
begin
  inherited Create(nil);
  FState := AState;
  FIndex := AIndex;
end;

function TPromiseAnyRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FState.Settled then Exit;

  FState.Errors.Elements[FIndex] := AArgs.GetElement(0);
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject('AggregateError',
      'All promises were rejected');
    ErrorObj.AssignProperty('errors', FState.Errors);
    FState.ResultPromise.Reject(ErrorObj);
  end;
end;

procedure TPromiseAnyRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
end;

{ TGocciaGlobalPromise }

constructor TGocciaGlobalPromise.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FPromiseConstructor := TGocciaNativeFunctionValue.Create(PromiseConstructorFn, 'Promise', 1);

  TGocciaPromiseValue.ExposePrototype(FPromiseConstructor);

  FPromiseConstructor.AssignProperty('resolve',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseResolve, 'resolve', 1));
  FPromiseConstructor.AssignProperty('reject',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseReject, 'reject', 1));
  FPromiseConstructor.AssignProperty('all',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseAll, 'all', 1));
  FPromiseConstructor.AssignProperty('allSettled',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseAllSettled, 'allSettled', 1));
  FPromiseConstructor.AssignProperty('race',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseRace, 'race', 1));
  FPromiseConstructor.AssignProperty('any',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseAny, 'any', 1));
  FPromiseConstructor.AssignProperty('withResolvers',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseWithResolvers, 'withResolvers', 0));
  FPromiseConstructor.AssignProperty('try',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(PromiseTry, 'try', 1));

  AScope.DefineLexicalBinding(AName, FPromiseConstructor, dtLet);
end;

{ Promise ( executor ) — §27.2.3.1
  1. If NewTarget is undefined, throw a TypeError exception.
  2. If IsCallable(executor) is false, throw a TypeError exception.
  3. Let promise be ? OrdinaryCreateFromConstructor(NewTarget, "%Promise.prototype%").
  4. Set promise.[[PromiseState]] to pending.
  5. Set promise.[[PromiseFulfillReactions]] to a new empty List.
  6. Set promise.[[PromiseRejectReactions]] to a new empty List.
  7. Let resolvingFunctions be CreateResolvingFunctions(promise).
  8. Let completion be Completion(Call(executor, undefined,
     « resolvingFunctions.[[Resolve]], resolvingFunctions.[[Reject]] »)).
  9. If completion is an abrupt completion, perform
     ? Call(resolvingFunctions.[[Reject]], undefined, « completion.[[Value]] »).
  10. Return promise. }
function TGocciaGlobalPromise.PromiseConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Executor: TGocciaValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ExecutorArgs, RejectArgs: TGocciaArgumentsCollection;
begin
  { Step 2: If IsCallable(executor) is false, throw a TypeError }
  if AArgs.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise resolver undefined is not a function');

  Executor := AArgs.GetElement(0);
  if not Executor.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise resolver is not a function');

  { Steps 3-4: Let promise = OrdinaryCreateFromConstructor; set state to pending }
  Promise := TGocciaPromiseValue.Create;
  { Step 7: Let resolvingFunctions = CreateResolvingFunctions(promise) }
  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoResolve, 'resolve', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoReject, 'reject', 1);

  { Step 8: Let completion = Call(executor, undefined, « resolve, reject ») }
  ExecutorArgs := TGocciaArgumentsCollection.Create([ResolveFn, RejectFn]);
  try
    try
      TGocciaFunctionBase(Executor).Call(ExecutorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    except
      on E: TGocciaThrowValue do
      begin
        { Step 9: If abrupt completion, Call(reject, undefined, « reason ») }
        RejectArgs := TGocciaArgumentsCollection.Create([E.Value]);
        try
          Promise.DoReject(RejectArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          RejectArgs.Free;
        end;
      end;
    end;
  finally
    ExecutorArgs.Free;
  end;

  { Step 10: Return promise }
  Result := Promise;
end;

{ Promise.resolve ( x ) — §27.2.4.7
  1. Let C be the this value.
  2. If Type(x) is Object and x has a [[PromiseState]] internal slot:
     a. Let xConstructor be ? Get(x, "constructor").
     b. If SameValue(xConstructor, C) is true, return x.
  3. Let promiseCapability be ? NewPromiseCapability(C).
  4. Perform ? Call(promiseCapability.[[Resolve]], undefined, « x »).
  5. Return promiseCapability.[[Promise]]. }
function TGocciaGlobalPromise.PromiseResolve(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
  P: TGocciaPromiseValue;
begin
  Value := AArgs.GetElement(0);

  { Step 2: If x is already a Promise, return it directly }
  if Value is TGocciaPromiseValue then
  begin
    Result := Value;
    Exit;
  end;

  { Steps 3-4: Let promiseCapability = NewPromiseCapability; Call(resolve, x) }
  P := TGocciaPromiseValue.Create;
  P.Resolve(Value);
  { Step 5: Return promiseCapability.[[Promise]] }
  Result := P;
end;

{ Promise.reject ( r ) — §27.2.4.6
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Perform ? Call(promiseCapability.[[Reject]], undefined, « r »).
  4. Return promiseCapability.[[Promise]]. }
function TGocciaGlobalPromise.PromiseReject(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  P: TGocciaPromiseValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  P := TGocciaPromiseValue.Create;
  { Step 3: Call(reject, undefined, « r ») }
  P.Reject(AArgs.GetElement(0));
  { Step 4: Return promiseCapability.[[Promise]] }
  Result := P;
end;

function TGocciaGlobalPromise.ExtractPromiseArray(const AArgs: TGocciaArgumentsCollection): TGocciaArrayValue;
var
  Iterable: TGocciaValue;
  Str: string;
  I: Integer;
begin
  Iterable := AArgs.GetElement(0);
  if Iterable is TGocciaArrayValue then
    Result := TGocciaArrayValue(Iterable)
  else if Iterable is TGocciaStringLiteralValue then
  begin
    Str := TGocciaStringLiteralValue(Iterable).Value;
    Result := TGocciaArrayValue.Create;
    TGocciaGarbageCollector.Instance.AddTempRoot(Result);
    try
      for I := 1 to Length(Str) do
        Result.Elements.Add(TGocciaStringLiteralValue.Create(Str[I]));
    finally
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Result);
    end;
  end
  else if Iterable is TGocciaSetValue then
    Result := TGocciaSetValue(Iterable).ToArray
  else if Iterable is TGocciaMapValue then
    Result := TGocciaMapValue(Iterable).ToArray
  else
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(
      Iterable.ToStringLiteral.Value + ' is not iterable');
    Result := nil;
  end;
end;

{ Promise.all ( iterable ) — §27.2.4.1
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Let promiseResolve be ? GetPromiseResolve(C).
  4. Let iteratorRecord be ? GetIterator(iterable, sync).
  5. Let result be Completion(PerformPromiseAll(iteratorRecord, C,
     promiseCapability, promiseResolve)).
  6. If result is an abrupt completion, reject and return.
  7. Return promiseCapability.[[Promise]].

  PerformPromiseAll creates per-element resolve handlers that decrement
  a remainingElementsCount; when it reaches 0, the result array is resolved. }
function TGocciaGlobalPromise.PromiseAll(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  State: TPromiseAllState;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  FulfillHandler: TPromiseAllHandler;
  RejectHandler: TPromiseAllRejectHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  ResultPromise := TGocciaPromiseValue.Create;
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

  { Empty iterable: resolve immediately with empty array }
  if InputArray.Elements.Count = 0 then
  begin
    ResultPromise.Resolve(TGocciaArrayValue.Create);
    Result := ResultPromise;
    Exit;
  end;

  { Step 5: PerformPromiseAll — set up per-element handlers }
  State := TPromiseAllState.Create(ResultPromise, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    FulfillHandler := TPromiseAllHandler.Create(State, I);
    RejectHandler := TPromiseAllRejectHandler.Create(State);

    { Invoke then(onFulfilled, onRejected) on wrapped promise }
    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FulfillHandler.Invoke, 'all-resolve', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'all-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := ResultPromise;
end;

{ Promise.allSettled ( iterable ) — §27.2.4.2
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Let promiseResolve be ? GetPromiseResolve(C).
  4. Let iteratorRecord be ? GetIterator(iterable, sync).
  5. Let result be Completion(PerformPromiseAllSettled(iteratorRecord, C,
     promiseCapability, promiseResolve)).
  6. If result is an abrupt completion, reject and return.
  7. Return promiseCapability.[[Promise]].

  PerformPromiseAllSettled creates per-element fulfill/reject handlers that
  record status+value/reason objects; when remainingElementsCount
  reaches 0, the result array is resolved (never rejects). }
function TGocciaGlobalPromise.PromiseAllSettled(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  State: TPromiseAllState;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  FulfillHandler: TPromiseAllSettledFulfillHandler;
  RejectHandler: TPromiseAllSettledRejectHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  ResultPromise := TGocciaPromiseValue.Create;
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

  { Empty iterable: resolve immediately with empty array }
  if InputArray.Elements.Count = 0 then
  begin
    ResultPromise.Resolve(TGocciaArrayValue.Create);
    Result := ResultPromise;
    Exit;
  end;

  { Step 5: PerformPromiseAllSettled — set up per-element handlers }
  State := TPromiseAllState.Create(ResultPromise, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    FulfillHandler := TPromiseAllSettledFulfillHandler.Create(State, I);
    RejectHandler := TPromiseAllSettledRejectHandler.Create(State, I);

    { Invoke then(onFulfilled, onRejected) — both handlers record status }
    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FulfillHandler.Invoke, 'allSettled-fulfill', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'allSettled-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := ResultPromise;
end;

{ Promise.race ( iterable ) — §27.2.4.5
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Let promiseResolve be ? GetPromiseResolve(C).
  4. Let iteratorRecord be ? GetIterator(iterable, sync).
  5. Let result be Completion(PerformPromiseRace(iteratorRecord, C,
     promiseCapability, promiseResolve)).
  6. If result is an abrupt completion, reject and return.
  7. Return promiseCapability.[[Promise]].

  PerformPromiseRace: for each promise, invoke then(resolve, reject) —
  the first to settle wins (resolves or rejects the result promise). }
function TGocciaGlobalPromise.PromiseRace(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  ResolveHandler, RejectHandler: TPromiseRaceHandler;
  ThenArgs: TGocciaArgumentsCollection;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  ResultPromise := TGocciaPromiseValue.Create;
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

  { Step 5: PerformPromiseRace — first to settle wins }
  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    ResolveHandler := TPromiseRaceHandler.Create(ResultPromise, True);
    RejectHandler := TPromiseRaceHandler.Create(ResultPromise, False);

    { Invoke then(resolve, reject) — first settlement propagates }
    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(ResolveHandler.Invoke, 'race-resolve', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'race-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := ResultPromise;
end;

{ Promise.any ( iterable ) — §27.2.4.3
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Let promiseResolve be ? GetPromiseResolve(C).
  4. Let iteratorRecord be ? GetIterator(iterable, sync).
  5. Let result be Completion(PerformPromiseAny(iteratorRecord, C,
     promiseCapability, promiseResolve)).
  6. If result is an abrupt completion, reject and return.
  7. Return promiseCapability.[[Promise]].

  PerformPromiseAny: collect rejections in an errors array; first fulfillment
  wins. If all reject, reject with AggregateError containing all reasons. }
function TGocciaGlobalPromise.PromiseAny(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  InputArray: TGocciaArrayValue;
  ResultPromise: TGocciaPromiseValue;
  State: TPromiseAnyState;
  I: Integer;
  Wrapped: TGocciaPromiseValue;
  FulfillHandler: TPromiseAnyFulfillHandler;
  RejectHandler: TPromiseAnyRejectHandler;
  ThenArgs: TGocciaArgumentsCollection;
  ErrorObj: TGocciaObjectValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  ResultPromise := TGocciaPromiseValue.Create;
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      ResultPromise.Reject(E.Value);
      Result := ResultPromise;
      Exit;
    end;
  end;

  { Empty iterable: reject with AggregateError (all zero promises rejected) }
  if InputArray.Elements.Count = 0 then
  begin
    ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject('AggregateError',
      'All promises were rejected');
    ErrorObj.AssignProperty('errors', TGocciaArrayValue.Create);
    ResultPromise.Reject(ErrorObj);
    Result := ResultPromise;
    Exit;
  end;

  { Step 5: PerformPromiseAny — first fulfillment wins }
  State := TPromiseAnyState.Create(ResultPromise, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    Wrapped := WrapAsPromise(InputArray.Elements[I]);
    FulfillHandler := TPromiseAnyFulfillHandler.Create(State);
    RejectHandler := TPromiseAnyRejectHandler.Create(State, I);

    { Invoke then(onFulfilled, onRejected) — fulfill resolves, reject collects }
    ThenArgs := TGocciaArgumentsCollection.Create([
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FulfillHandler.Invoke, 'any-fulfill', 1),
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RejectHandler.Invoke, 'any-reject', 1)
    ]);
    try
      TGocciaFunctionBase(Wrapped.GetProperty('then')).Call(ThenArgs, Wrapped);
    finally
      ThenArgs.Free;
    end;
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := ResultPromise;
end;

{ Promise.withResolvers ( ) — §27.2.4.8
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Let obj be OrdinaryObjectCreate(%Object.prototype%).
  4. Perform ! CreateDataPropertyOrThrow(obj, "promise", promiseCapability.[[Promise]]).
  5. Perform ! CreateDataPropertyOrThrow(obj, "resolve", promiseCapability.[[Resolve]]).
  6. Perform ! CreateDataPropertyOrThrow(obj, "reject", promiseCapability.[[Reject]]).
  7. Return obj. }
function TGocciaGlobalPromise.PromiseWithResolvers(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Promise := TGocciaPromiseValue.Create;
  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoResolve, 'resolve', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoReject, 'reject', 1);

  { Steps 3-6: Create result object with promise, resolve, reject properties }
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.AssignProperty('promise', Promise);
  ResultObj.AssignProperty('resolve', ResolveFn);
  ResultObj.AssignProperty('reject', RejectFn);

  { Step 7: Return obj }
  Result := ResultObj;
end;

{ Promise.try ( callbackfn, ...args ) — §27.2.4.9 (ES2025+)
  1. Let C be the this value.
  2. If IsCallable(callbackfn) is false, throw a TypeError exception.
  3. Let promiseCapability be ? NewPromiseCapability(C).
  4. Let status be Completion(Call(callbackfn, undefined, args)).
  5. If status is an abrupt completion, then
     a. Perform ? Call(promiseCapability.[[Reject]], undefined, « status.[[Value]] »).
  6. Else,
     a. Perform ? Call(promiseCapability.[[Resolve]], undefined, « status.[[Value]] »).
  7. Return promiseCapability.[[Promise]]. }
function TGocciaGlobalPromise.PromiseTry(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Callback: TGocciaValue;
  CallbackResult: TGocciaValue;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  { Step 3: Let promiseCapability = NewPromiseCapability(C) }
  Promise := TGocciaPromiseValue.Create;

  { Step 2: If IsCallable(callbackfn) is false, throw a TypeError }
  if AArgs.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.try requires a callback function');

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError('Promise.try requires a callback function');

  { Step 4: Let status = Call(callbackfn, undefined) }
  try
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      CallbackResult := TGocciaFunctionBase(Callback).Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      EmptyArgs.Free;
    end;
    { Step 6: Normal completion — Call(resolve, status.[[Value]]) }
    Promise.Resolve(CallbackResult);
  except
    on E: TGocciaThrowValue do
      { Step 5: Abrupt completion — Call(reject, status.[[Value]]) }
      Promise.Reject(E.Value);
    on E: Exception do
      Promise.Reject(CreateErrorObject('Error', E.Message));
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Promise;
end;

end.
