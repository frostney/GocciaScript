unit Goccia.Builtins.GlobalPromise;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalPromise = class(TGocciaBuiltin)
  private
    FPromiseConstructor: TGocciaNativeFunctionValue;
    FPromiseIntrinsicProto: TGocciaObjectValue;

    function ExtractPromiseArray(const AArgs: TGocciaArgumentsCollection): TGocciaArrayValue;
    function PromiseConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  published
    function PromiseConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseSpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseResolve(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseReject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAllSettled(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseRace(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAny(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseWithResolvers(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseTry(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.VM.Exception;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

type
  TPromiseCapability = record
    Promise: TGocciaValue;
    Resolve: TGocciaValue;
    Reject: TGocciaValue;
  end;

  TPromiseCapabilityExecutor = class(TGocciaObjectValue)
  private
    FResolve: TGocciaValue;
    FReject: TGocciaValue;
  public
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
    property Resolve: TGocciaValue read FResolve;
    property Reject: TGocciaValue read FReject;
  end;

  TPromiseAllState = class(TGocciaObjectValue)
  private
    FResults: TGocciaArrayValue;
    FRemaining: Integer;
    FResultPromise: TGocciaValue;
    FResolve: TGocciaValue;
    FReject: TGocciaValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise, AResolve, AReject: TGocciaValue;
      const ACount: Integer);
    procedure MarkReferences; override;
    property Results: TGocciaArrayValue read FResults;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaValue read FResultPromise;
    property Resolve: TGocciaValue read FResolve;
    property Reject: TGocciaValue read FReject;
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
    FResultPromise: TGocciaValue;
    FResolve: TGocciaValue;
    FReject: TGocciaValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise, AResolve, AReject: TGocciaValue;
      const ACount: Integer);
    procedure MarkReferences; override;
    property Errors: TGocciaArrayValue read FErrors;
    property Remaining: Integer read FRemaining write FRemaining;
    property ResultPromise: TGocciaValue read FResultPromise;
    property Resolve: TGocciaValue read FResolve;
    property Reject: TGocciaValue read FReject;
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
function TPromiseCapabilityExecutor.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  FResolve := AArgs.GetElement(0);
  FReject := AArgs.GetElement(1);
end;

procedure TPromiseCapabilityExecutor.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResolve) then
    FResolve.MarkReferences;
  if Assigned(FReject) then
    FReject.MarkReferences;
end;

procedure CallPromiseCapability(const AFunction, AValue: TGocciaValue);
var
  Args: TGocciaArgumentsCollection;
begin
  if not Assigned(AFunction) or not AFunction.IsCallable then
    Exit;

  Args := TGocciaArgumentsCollection.Create([AValue]);
  try
    DispatchCall(AFunction, Args, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Args.Free;
  end;
end;

function NewPromiseCapability(const AConstructor: TGocciaValue;
  const ANativeConstructor: TGocciaNativeFunctionValue): TPromiseCapability;
var
  Promise: TGocciaPromiseValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ExecutorHost: TPromiseCapabilityExecutor;
  ExecutorFn: TGocciaNativeFunctionValue;
  ConstructArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
begin
  if AConstructor = ANativeConstructor then
  begin
    Promise := TGocciaPromiseValue.Create;
    ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Promise.DoResolve, 'resolve', 1);
    RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Promise.DoReject, 'reject', 1);
    ResolveFn.CapturedRoot := Promise;
    RejectFn.CapturedRoot := Promise;
    Result.Promise := Promise;
    Result.Resolve := ResolveFn;
    Result.Reject := RejectFn;
    Exit;
  end;

  if not Assigned(AConstructor) or not AConstructor.IsConstructable then
    ThrowTypeError(SErrorValueNotConstructor, SSuggestNotConstructorType);

  ExecutorHost := TPromiseCapabilityExecutor.Create;
  ExecutorFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ExecutorHost.Invoke, '', 2);
  ExecutorFn.CapturedRoot := ExecutorHost;
  ConstructArgs := TGocciaArgumentsCollection.Create([ExecutorFn]);
  GC := TGarbageCollector.Instance;
  try
    if Assigned(GC) then
    begin
      GC.AddTempRoot(ExecutorHost);
      GC.AddTempRoot(ExecutorFn);
    end;

    Result.Promise := ConstructValue(AConstructor, ConstructArgs, AConstructor);
    Result.Resolve := ExecutorHost.Resolve;
    Result.Reject := ExecutorHost.Reject;

    if not Assigned(Result.Resolve) or not Result.Resolve.IsCallable or
       not Assigned(Result.Reject) or not Result.Reject.IsCallable then
      ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);
  finally
    if Assigned(GC) then
    begin
      GC.RemoveTempRoot(ExecutorFn);
      GC.RemoveTempRoot(ExecutorHost);
    end;
    ConstructArgs.Free;
  end;
end;

function PromiseResolveForConstructor(const AConstructor,
  AValue: TGocciaValue): TGocciaValue;
var
  ResolveMethod: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  if not (AConstructor is TGocciaObjectValue) then
    ThrowTypeError(SErrorValueNotConstructor, SSuggestNotConstructorType);

  ResolveMethod := TGocciaObjectValue(AConstructor).GetProperty(PROP_RESOLVE);
  if not Assigned(ResolveMethod) or not ResolveMethod.IsCallable then
    ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

  Args := TGocciaArgumentsCollection.Create([AValue]);
  try
    Result := DispatchCall(ResolveMethod, Args, AConstructor);
  finally
    Args.Free;
  end;
end;

procedure InvokePromiseLikeThen(const APromiseLike, AOnFulfilled,
  AOnRejected: TGocciaValue);
var
  ThenMethod: TGocciaValue;
  ThenArgs: TGocciaArgumentsCollection;
begin
  if not (APromiseLike is TGocciaObjectValue) then
    ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

  ThenMethod := TGocciaObjectValue(APromiseLike).GetProperty(PROP_THEN);
  if not Assigned(ThenMethod) or not ThenMethod.IsCallable then
    ThrowTypeError(SErrorThenNotFunction, SSuggestPromiseThisType);

  ThenArgs := TGocciaArgumentsCollection.Create([AOnFulfilled, AOnRejected]);
  try
    DispatchCall(ThenMethod, ThenArgs, APromiseLike);
  finally
    ThenArgs.Free;
  end;
end;

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

constructor TPromiseAllState.Create(const AResultPromise, AResolve,
  AReject: TGocciaValue; const ACount: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FResolve := AResolve;
  FReject := AReject;
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
  if Assigned(FResolve) then FResolve.MarkReferences;
  if Assigned(FReject) then FReject.MarkReferences;
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
    CallPromiseCapability(FState.Resolve, FState.Results);
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
  CallPromiseCapability(FState.Reject, AArgs.GetElement(0));
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

  Entry := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Entry.CreateDataPropertyOrThrow('status',
    TGocciaStringLiteralValue.Create('fulfilled'));
  Entry.CreateDataPropertyOrThrow(PROP_VALUE, AArgs.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    CallPromiseCapability(FState.Resolve, FState.Results);
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

  Entry := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Entry.CreateDataPropertyOrThrow('status',
    TGocciaStringLiteralValue.Create('rejected'));
  Entry.CreateDataPropertyOrThrow('reason', AArgs.GetElement(0));
  FState.Results.Elements[FIndex] := Entry;
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    CallPromiseCapability(FState.Resolve, FState.Results);
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

constructor TPromiseAnyState.Create(const AResultPromise, AResolve,
  AReject: TGocciaValue; const ACount: Integer);
var
  I: Integer;
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FResolve := AResolve;
  FReject := AReject;
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
  if Assigned(FResolve) then FResolve.MarkReferences;
  if Assigned(FReject) then FReject.MarkReferences;
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
  CallPromiseCapability(FState.Resolve, AArgs.GetElement(0));
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
    ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject(AGGREGATE_ERROR_NAME,
      SErrorAllPromisesRejected);
    ErrorObj.AssignProperty(PROP_ERRORS, FState.Errors);
    CallPromiseCapability(FState.Reject, ErrorObj);
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
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FPromiseConstructor := TGocciaNativeFunctionValue.Create(PromiseConstructorFn, 'Promise', 1);
  FPromiseConstructor.ConstructCallback := PromiseConstruct;
  FPromiseConstructor.DefineSymbolProperty(TGocciaSymbolValue.WellKnownSpecies,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        PromiseSpeciesGetter, 'get [Symbol.species]', 0),
      nil, [pfConfigurable]));

  TGocciaPromiseValue.ExposePrototype(FPromiseConstructor);
  FPromiseIntrinsicProto := TGocciaObjectValue(FPromiseConstructor.GetProperty(PROP_PROTOTYPE));
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(PromiseResolve, 1, gmkStaticMethod);
    Members.AddMethod(PromiseReject, 1, gmkStaticMethod);
    Members.AddMethod(PromiseAll, 1, gmkStaticMethod);
    Members.AddMethod(PromiseAllSettled, 1, gmkStaticMethod);
    Members.AddMethod(PromiseRace, 1, gmkStaticMethod);
    Members.AddMethod(PromiseAny, 1, gmkStaticMethod);
    Members.AddMethod(PromiseWithResolvers, 0, gmkStaticMethod);
    Members.AddMethod(PromiseTry, 1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FPromiseConstructor, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FPromiseConstructor, dtLet, True);
end;

// ES2026 §27.2.4.8 get Promise [ @@species ]
function TGocciaGlobalPromise.PromiseSpeciesGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
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
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseResolverUndefinedNotFunction, SSuggestPromiseResolver);

  Executor := AArgs.GetElement(0);
  if not Executor.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

  { Steps 3-4: Let promise = OrdinaryCreateFromConstructor; set state to pending }
  Promise := TGocciaPromiseValue.Create;
  { Step 7: Let resolvingFunctions = CreateResolvingFunctions(promise) }
  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoResolve, 'resolve', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoReject, 'reject', 1);
  ResolveFn.CapturedRoot := Promise;
  RejectFn.CapturedRoot := Promise;

  { Step 8: Let completion = Call(executor, undefined, « resolve, reject ») }
  ExecutorArgs := TGocciaArgumentsCollection.Create([ResolveFn, RejectFn]);
  try
    try
      InvokeCallable(Executor, ExecutorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    except
      on E: EGocciaBytecodeThrow do
      begin
        RejectArgs := TGocciaArgumentsCollection.Create([E.ThrownValue]);
        try
          Promise.DoReject(RejectArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          RejectArgs.Free;
        end;
      end;
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

function TGocciaGlobalPromise.PromiseConstruct(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Executor: TGocciaValue;
  ResolveFn, RejectFn: TGocciaNativeFunctionValue;
  ExecutorArgs, RejectArgs: TGocciaArgumentsCollection;
  Proto: TGocciaObjectValue;
begin
  if AArgs.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseResolverUndefinedNotFunction, SSuggestPromiseResolver);

  Executor := AArgs.GetElement(0);
  if not Executor.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

  Proto := GetProtoFromConstructorWithIntrinsic(ANewTarget, FPromiseIntrinsicProto);

  Promise := TGocciaPromiseValue.Create;
  Promise.Prototype := Proto;

  ResolveFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoResolve, 'resolve', 1);
  RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(Promise.DoReject, 'reject', 1);
  ResolveFn.CapturedRoot := Promise;
  RejectFn.CapturedRoot := Promise;

  ExecutorArgs := TGocciaArgumentsCollection.Create([ResolveFn, RejectFn]);
  try
    try
      InvokeCallable(Executor, ExecutorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    except
      on E: EGocciaBytecodeThrow do
      begin
        RejectArgs := TGocciaArgumentsCollection.Create([E.ThrownValue]);
        try
          Promise.DoReject(RejectArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          RejectArgs.Free;
        end;
      end;
      on E: TGocciaThrowValue do
      begin
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
    TGarbageCollector.Instance.AddTempRoot(Result);
    try
      for I := 1 to Length(Str) do
        Result.Elements.Add(TGocciaStringLiteralValue.Create(Str[I]));
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Result);
    end;
  end
  else if Iterable is TGocciaSetValue then
    Result := TGocciaSetValue(Iterable).ToArray
  else if Iterable is TGocciaMapValue then
    Result := TGocciaMapValue(Iterable).ToArray
  else
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(
      Iterable.ToStringLiteral.Value + ' is not iterable', SSuggestNotIterable);
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
  Capability: TPromiseCapability;
  State: TPromiseAllState;
  I: Integer;
  PromiseLike: TGocciaValue;
  FulfillHandler: TPromiseAllHandler;
  FulfillFn: TGocciaNativeFunctionValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      CallPromiseCapability(Capability.Reject, E.Value);
      Result := Capability.Promise;
      Exit;
    end;
  end;

  { Empty iterable: resolve immediately with empty array }
  if InputArray.Elements.Count = 0 then
  begin
    CallPromiseCapability(Capability.Resolve, TGocciaArrayValue.Create);
    Result := Capability.Promise;
    Exit;
  end;

  { Step 5: PerformPromiseAll — set up per-element handlers }
  State := TPromiseAllState.Create(Capability.Promise, Capability.Resolve,
    Capability.Reject, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    PromiseLike := PromiseResolveForConstructor(AThisValue, InputArray.Elements[I]);
    FulfillHandler := TPromiseAllHandler.Create(State, I);
    FulfillFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FulfillHandler.Invoke, 'all-resolve', 1);
    FulfillFn.CapturedRoot := FulfillHandler;
    InvokePromiseLikeThen(PromiseLike, FulfillFn, Capability.Reject);
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
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
  Capability: TPromiseCapability;
  State: TPromiseAllState;
  I: Integer;
  PromiseLike: TGocciaValue;
  FulfillHandler: TPromiseAllSettledFulfillHandler;
  RejectHandler: TPromiseAllSettledRejectHandler;
  FulfillFn, RejectFn: TGocciaNativeFunctionValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      CallPromiseCapability(Capability.Reject, E.Value);
      Result := Capability.Promise;
      Exit;
    end;
  end;

  { Empty iterable: resolve immediately with empty array }
  if InputArray.Elements.Count = 0 then
  begin
    CallPromiseCapability(Capability.Resolve, TGocciaArrayValue.Create);
    Result := Capability.Promise;
    Exit;
  end;

  { Step 5: PerformPromiseAllSettled — set up per-element handlers }
  State := TPromiseAllState.Create(Capability.Promise, Capability.Resolve,
    Capability.Reject, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    PromiseLike := PromiseResolveForConstructor(AThisValue, InputArray.Elements[I]);
    FulfillHandler := TPromiseAllSettledFulfillHandler.Create(State, I);
    RejectHandler := TPromiseAllSettledRejectHandler.Create(State, I);

    { Invoke then(onFulfilled, onRejected) — both handlers record status }
    FulfillFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      FulfillHandler.Invoke, 'allSettled-fulfill', 1);
    RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      RejectHandler.Invoke, 'allSettled-reject', 1);
    FulfillFn.CapturedRoot := FulfillHandler;
    RejectFn.CapturedRoot := RejectHandler;
    InvokePromiseLikeThen(PromiseLike, FulfillFn, RejectFn);
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
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
  Capability: TPromiseCapability;
  I: Integer;
  PromiseLike: TGocciaValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      CallPromiseCapability(Capability.Reject, E.Value);
      Result := Capability.Promise;
      Exit;
    end;
  end;

  { Step 5: PerformPromiseRace — first to settle wins }
  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    PromiseLike := PromiseResolveForConstructor(AThisValue, InputArray.Elements[I]);
    InvokePromiseLikeThen(PromiseLike, Capability.Resolve, Capability.Reject);
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
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
  Capability: TPromiseCapability;
  State: TPromiseAnyState;
  I: Integer;
  PromiseLike: TGocciaValue;
  RejectHandler: TPromiseAnyRejectHandler;
  RejectFn: TGocciaNativeFunctionValue;
  ErrorObj: TGocciaObjectValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  { Step 4: Let iteratorRecord = GetIterator(iterable) }
  try
    InputArray := ExtractPromiseArray(AArgs);
  except
    on E: TGocciaThrowValue do
    begin
      { Step 6: If abrupt, Call(reject, reason) }
      CallPromiseCapability(Capability.Reject, E.Value);
      Result := Capability.Promise;
      Exit;
    end;
  end;

  { Empty iterable: reject with AggregateError (all zero promises rejected) }
  if InputArray.Elements.Count = 0 then
  begin
    ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject(AGGREGATE_ERROR_NAME,
      SErrorAllPromisesRejected);
    ErrorObj.AssignProperty(PROP_ERRORS, TGocciaArrayValue.Create);
    CallPromiseCapability(Capability.Reject, ErrorObj);
    Result := Capability.Promise;
    Exit;
  end;

  { Step 5: PerformPromiseAny — first fulfillment wins }
  State := TPromiseAnyState.Create(Capability.Promise, Capability.Resolve,
    Capability.Reject, InputArray.Elements.Count);

  for I := 0 to InputArray.Elements.Count - 1 do
  begin
    { Wrap each element as a Promise via PromiseResolve(C, nextValue) }
    PromiseLike := PromiseResolveForConstructor(AThisValue, InputArray.Elements[I]);
    RejectHandler := TPromiseAnyRejectHandler.Create(State, I);

    { Invoke then(onFulfilled, onRejected) — fulfill resolves, reject collects }
    RejectFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      RejectHandler.Invoke, 'any-reject', 1);
    RejectFn.CapturedRoot := RejectHandler;
    InvokePromiseLikeThen(PromiseLike, Capability.Resolve, RejectFn);
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
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
  ResolveFn.CapturedRoot := Promise;
  RejectFn.CapturedRoot := Promise;

  { Steps 3-6: Create result object with promise, resolve, reject properties }
  ResultObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  ResultObj.CreateDataPropertyOrThrow('promise', Promise);
  ResultObj.CreateDataPropertyOrThrow('resolve', ResolveFn);
  ResultObj.CreateDataPropertyOrThrow('reject', RejectFn);

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
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseTryRequiresCallback, SSuggestCallbackRequired);

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseTryRequiresCallback, SSuggestCallbackRequired);

  { Step 4: Let status = Call(callbackfn, undefined) }
  try
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      CallbackResult := InvokeCallable(Callback, EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      EmptyArgs.Free;
    end;
    { Step 6: Normal completion — Call(resolve, status.[[Value]]) }
    Promise.Resolve(CallbackResult);
  except
    on E: EGocciaBytecodeThrow do
      Promise.Reject(E.ThrownValue);
    on E: TGocciaThrowValue do
      { Step 5: Abrupt completion — Call(reject, status.[[Value]]) }
      Promise.Reject(E.Value);
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: Exception do
      Promise.Reject(CreateErrorObject(ERROR_NAME, E.Message));
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Promise;
end;

end.
