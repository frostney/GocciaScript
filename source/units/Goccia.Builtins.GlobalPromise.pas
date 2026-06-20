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
    function PromiseKeyed(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; const AAllSettled: Boolean): TGocciaValue;
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
    function PromiseAllKeyed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseAllSettledKeyed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseWithResolvers(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseTry(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Arithmetic,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Realm,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.ProxyValue,
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

  TPromiseKeyRecord = record
    Name: string;
    Symbol: TGocciaSymbolValue;
    IsSymbol: Boolean;
  end;

  TPromiseKeyRecordArray = array of TPromiseKeyRecord;

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
    FAlreadyCalled: Boolean;
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
    FAlreadyCalled: Boolean;
  public
    constructor Create(const AState: TPromiseAllState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseAllSettledRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseAllState;
    FIndex: Integer;
    FAlreadyCalled: Boolean;
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
    FAlreadyCalled: Boolean;
  public
    constructor Create(const AState: TPromiseAnyState; const AIndex: Integer);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseKeyedState = class(TGocciaObjectValue)
  private
    FResultObject: TGocciaObjectValue;
    FRemaining: Integer;
    FResolve: TGocciaValue;
    FReject: TGocciaValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultObject: TGocciaObjectValue;
      const AResolve, AReject: TGocciaValue; const ACount: Integer);
    procedure MarkReferences; override;
    property ResultObject: TGocciaObjectValue read FResultObject;
    property Remaining: Integer read FRemaining write FRemaining;
    property Resolve: TGocciaValue read FResolve;
    property Reject: TGocciaValue read FReject;
    property Settled: Boolean read FSettled write FSettled;
  end;

  TPromiseKeyedFulfillHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseKeyedState;
    FName: string;
    FSymbol: TGocciaSymbolValue;
    FIsSymbol: Boolean;
    FAllSettled: Boolean;
    FAlreadyCalled: Boolean;
  public
    constructor Create(const AState: TPromiseKeyedState; const AName: string;
      const ASymbol: TGocciaSymbolValue; const AIsSymbol, AAllSettled: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TPromiseKeyedRejectHandler = class(TGocciaObjectValue)
  private
    FState: TPromiseKeyedState;
    FName: string;
    FSymbol: TGocciaSymbolValue;
    FIsSymbol: Boolean;
    FAllSettled: Boolean;
    FAlreadyCalled: Boolean;
  public
    constructor Create(const AState: TPromiseKeyedState; const AName: string;
      const ASymbol: TGocciaSymbolValue; const AIsSymbol, AAllSettled: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

{ Helper: wrap value as Promise if not already one }
function TPromiseCapabilityExecutor.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
  function HasNonUndefinedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
  end;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if HasNonUndefinedValue(FResolve) or HasNonUndefinedValue(FReject) then
    ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

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

function RejectPromiseCapabilityWithReason(const ACapability: TPromiseCapability;
  const AReason: TGocciaValue): TGocciaValue;
begin
  CallPromiseCapability(ACapability.Reject, AReason);
  Result := ACapability.Promise;
end;

function PromiseRejectionReasonFromException(
  const AException: Exception): TGocciaValue;
begin
  if AException is EGocciaBytecodeThrow then
    Result := EGocciaBytecodeThrow(AException).ThrownValue
  else if AException is TGocciaThrowValue then
    Result := TGocciaThrowValue(AException).Value
  else if AException is TGocciaTypeError then
    Result := CreateErrorObject(TYPE_ERROR_NAME, AException.Message)
  else if AException is TGocciaReferenceError then
    Result := CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message)
  else if AException is TGocciaSyntaxError then
    Result := CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message)
  else if AException is TGocciaRuntimeError then
    Result := CreateErrorObject(ERROR_NAME, AException.Message)
  else
    raise AException;
end;

function RejectPromiseCapabilityWithException(
  const ACapability: TPromiseCapability;
  const AException: Exception): TGocciaValue;
begin
  Result := RejectPromiseCapabilityWithReason(ACapability,
    PromiseRejectionReasonFromException(AException));
end;

function CreateAnonymousPromiseBuiltinFunction(
  const AFunction: TGocciaNativeFunctionCallback;
  const ACapturedRoot: TGocciaValue; const AArity: Integer): TGocciaNativeFunctionValue;
begin
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(AFunction, '',
    AArity);
  Result.CapturedRoot := ACapturedRoot;
  Result.DefineProperty(PROP_LENGTH, TGocciaPropertyDescriptorData.Create(
    TGocciaNumberLiteralValue.Create(AArity), [pfConfigurable]));
  Result.DefineProperty(PROP_NAME, TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(''), [pfConfigurable]));
end;

function CreatePromiseElementFunction(
  const AFunction: TGocciaNativeFunctionCallback;
  const ACapturedRoot: TGocciaValue): TGocciaNativeFunctionValue;
begin
  Result := CreateAnonymousPromiseBuiltinFunction(AFunction, ACapturedRoot, 1);
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
    ResolveFn := CreateAnonymousPromiseBuiltinFunction(Promise.DoResolve,
      Promise, 1);
    RejectFn := CreateAnonymousPromiseBuiltinFunction(Promise.DoReject,
      Promise, 1);
    Result.Promise := Promise;
    Result.Resolve := ResolveFn;
    Result.Reject := RejectFn;
    Exit;
  end;

  if not Assigned(AConstructor) or not AConstructor.IsConstructable then
    ThrowTypeError(SErrorValueNotConstructor, SSuggestNotConstructorType);

  ExecutorHost := TPromiseCapabilityExecutor.Create;
  ExecutorFn := CreateAnonymousPromiseBuiltinFunction(ExecutorHost.Invoke,
    ExecutorHost, 2);
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

function GetPromiseResolveMethod(const AConstructor: TGocciaValue): TGocciaValue;
begin
  if not (AConstructor is TGocciaObjectValue) then
    ThrowTypeError(SErrorValueNotConstructor, SSuggestNotConstructorType);

  Result := TGocciaObjectValue(AConstructor).GetProperty(PROP_RESOLVE);
  if not Assigned(Result) or not Result.IsCallable then
    ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);
end;

function CallPromiseResolveMethod(const APromiseResolve, AConstructor,
  AValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create([AValue]);
  try
    Result := DispatchCall(APromiseResolve, Args, AConstructor);
  finally
    Args.Free;
  end;
end;

function GetPromiseIterator(const AIterable: TGocciaValue): TGocciaIteratorValue;
begin
  Result := GetIteratorFromValue(AIterable);
  if not Assigned(Result) then
    Goccia.Values.ErrorHelper.ThrowTypeError(
      AIterable.ToStringLiteral.Value + ' is not iterable',
      SSuggestNotIterable);
end;

function PromiseDefaultPrototypeForNewTarget(const ANewTarget: TGocciaValue;
  const AIntrinsicDefault: TGocciaObjectValue): TGocciaObjectValue;
var
  ConstructorPrototype, ConstructorValue, ProtoValue: TGocciaValue;
  FallbackRealm: TGocciaRealm;
  GlobalScope: TGocciaScope;
begin
  if ANewTarget is TGocciaObjectValue then
    ProtoValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE)
  else
    ProtoValue := nil;

  if ProtoValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ProtoValue));

  FallbackRealm := nil;
  if ANewTarget is TGocciaFunctionBase then
    FallbackRealm := TGocciaFunctionBase(ANewTarget).CreationRealm;

  if Assigned(FallbackRealm) and
     (FallbackRealm.GlobalEnv is TGocciaScope) then
  begin
    GlobalScope := TGocciaScope(FallbackRealm.GlobalEnv);
    if GlobalScope.TryGetBindingValue('Promise', ConstructorValue) and
       (ConstructorValue is TGocciaObjectValue) then
    begin
      ConstructorPrototype :=
        TGocciaObjectValue(ConstructorValue).GetProperty(PROP_PROTOTYPE);
      if ConstructorPrototype is TGocciaObjectValue then
        Exit(TGocciaObjectValue(ConstructorPrototype));
    end;
  end;

  Result := AIntrinsicDefault;
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
  FAlreadyCalled := False;
end;

function TPromiseAllHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyCalled then Exit;
  FAlreadyCalled := True;
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
  FAlreadyCalled := False;
end;

function TPromiseAllSettledFulfillHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyCalled then Exit;
  FAlreadyCalled := True;
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
  FAlreadyCalled := False;
end;

function TPromiseAllSettledRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entry: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyCalled then Exit;
  FAlreadyCalled := True;
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
  FAlreadyCalled := False;
end;

function TPromiseAnyRejectHandler.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyCalled then Exit;
  FAlreadyCalled := True;
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

{ TPromiseKeyedState }

constructor TPromiseKeyedState.Create(const AResultObject: TGocciaObjectValue;
  const AResolve, AReject: TGocciaValue; const ACount: Integer);
begin
  inherited Create(nil);
  FResultObject := AResultObject;
  FResolve := AResolve;
  FReject := AReject;
  FRemaining := ACount;
  FSettled := False;
end;

procedure TPromiseKeyedState.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FResultObject) then FResultObject.MarkReferences;
  if Assigned(FResolve) then FResolve.MarkReferences;
  if Assigned(FReject) then FReject.MarkReferences;
end;

procedure SetKeyedResultValue(const AObject: TGocciaObjectValue;
  const AName: string; const ASymbol: TGocciaSymbolValue;
  const AIsSymbol: Boolean; const AValue: TGocciaValue);
begin
  if AIsSymbol then
    AObject.AssignSymbolProperty(ASymbol, AValue)
  else
    AObject.AssignProperty(AName, AValue);
end;

function CreateAllSettledEntry(const AStatus, APropertyName: string;
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Result.CreateDataPropertyOrThrow('status',
    TGocciaStringLiteralValue.Create(AStatus));
  Result.CreateDataPropertyOrThrow(APropertyName, AValue);
end;

{ TPromiseKeyedFulfillHandler }

constructor TPromiseKeyedFulfillHandler.Create(
  const AState: TPromiseKeyedState; const AName: string;
  const ASymbol: TGocciaSymbolValue; const AIsSymbol, AAllSettled: Boolean);
begin
  inherited Create(nil);
  FState := AState;
  FName := AName;
  FSymbol := ASymbol;
  FIsSymbol := AIsSymbol;
  FAllSettled := AAllSettled;
  FAlreadyCalled := False;
end;

function TPromiseKeyedFulfillHandler.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyCalled then Exit;
  FAlreadyCalled := True;
  if FState.Settled then Exit;

  if FAllSettled then
    Value := CreateAllSettledEntry('fulfilled', PROP_VALUE, AArgs.GetElement(0))
  else
    Value := AArgs.GetElement(0);
  SetKeyedResultValue(FState.ResultObject, FName, FSymbol, FIsSymbol, Value);
  FState.Remaining := FState.Remaining - 1;

  if FState.Remaining = 0 then
  begin
    FState.Settled := True;
    CallPromiseCapability(FState.Resolve, FState.ResultObject);
  end;
end;

procedure TPromiseKeyedFulfillHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
  if Assigned(FSymbol) then FSymbol.MarkReferences;
end;

{ TPromiseKeyedRejectHandler }

constructor TPromiseKeyedRejectHandler.Create(
  const AState: TPromiseKeyedState; const AName: string;
  const ASymbol: TGocciaSymbolValue; const AIsSymbol, AAllSettled: Boolean);
begin
  inherited Create(nil);
  FState := AState;
  FName := AName;
  FSymbol := ASymbol;
  FIsSymbol := AIsSymbol;
  FAllSettled := AAllSettled;
  FAlreadyCalled := False;
end;

function TPromiseKeyedRejectHandler.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyCalled then Exit;
  FAlreadyCalled := True;
  if FState.Settled then Exit;

  if FAllSettled then
  begin
    Value := CreateAllSettledEntry('rejected', 'reason', AArgs.GetElement(0));
    SetKeyedResultValue(FState.ResultObject, FName, FSymbol, FIsSymbol, Value);
    FState.Remaining := FState.Remaining - 1;

    if FState.Remaining = 0 then
    begin
      FState.Settled := True;
      CallPromiseCapability(FState.Resolve, FState.ResultObject);
    end;
  end
  else
  begin
    FState.Settled := True;
    CallPromiseCapability(FState.Reject, AArgs.GetElement(0));
  end;
end;

procedure TPromiseKeyedRejectHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FState) then FState.MarkReferences;
  if Assigned(FSymbol) then FSymbol.MarkReferences;
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
  FPromiseConstructor.DefineProperty(PROP_LENGTH,
    TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(1),
      [pfConfigurable]));
  FPromiseConstructor.DefineProperty(PROP_NAME,
    TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create('Promise'),
      [pfConfigurable]));
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
    Members.AddMethod(PromiseAllKeyed, 1, gmkStaticMethod);
    Members.AddMethod(PromiseAllSettledKeyed, 1, gmkStaticMethod);
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
  Goccia.Values.ErrorHelper.ThrowTypeError(
    'Promise constructor cannot be invoked without ''new''',
    SSuggestPromiseResolver);

  { Step 2: If IsCallable(executor) is false, throw a TypeError }
  if AArgs.Length = 0 then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseResolverUndefinedNotFunction, SSuggestPromiseResolver);

  Executor := AArgs.GetElement(0);
  if not Executor.IsCallable then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

  { Steps 3-4: Let promise = OrdinaryCreateFromConstructor; set state to pending }
  Promise := TGocciaPromiseValue.Create;
  { Step 7: Let resolvingFunctions = CreateResolvingFunctions(promise) }
  ResolveFn := CreateAnonymousPromiseBuiltinFunction(Promise.DoResolve,
    Promise, 1);
  RejectFn := CreateAnonymousPromiseBuiltinFunction(Promise.DoReject,
    Promise, 1);

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

  Proto := PromiseDefaultPrototypeForNewTarget(ANewTarget,
    FPromiseIntrinsicProto);

  Promise := TGocciaPromiseValue.Create;
  Promise.Prototype := Proto;

  ResolveFn := CreateAnonymousPromiseBuiltinFunction(Promise.DoResolve,
    Promise, 1);
  RejectFn := CreateAnonymousPromiseBuiltinFunction(Promise.DoReject,
    Promise, 1);

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
  ValueConstructor: TGocciaValue;
  Capability: TPromiseCapability;
  ResolveArgs: TGocciaArgumentsCollection;
begin
  if not (AThisValue is TGocciaObjectValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorValueNotConstructor,
      SSuggestNotConstructorType);

  Value := AArgs.GetElement(0);

  { Step 2: If x is already a Promise created by C, return it directly. }
  if Value is TGocciaPromiseValue then
  begin
    ValueConstructor := TGocciaPromiseValue(Value).GetProperty(PROP_CONSTRUCTOR);
    if IsSameValue(ValueConstructor, AThisValue) then
    begin
      Result := Value;
      Exit;
    end;
  end;

  { Steps 3-4: Let promiseCapability = NewPromiseCapability; Call(resolve, x) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  ResolveArgs := TGocciaArgumentsCollection.Create([Value]);
  try
    DispatchCall(Capability.Resolve, ResolveArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    ResolveArgs.Free;
  end;
  { Step 5: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
end;

{ Promise.reject ( r ) — §27.2.4.6
  1. Let C be the this value.
  2. Let promiseCapability be ? NewPromiseCapability(C).
  3. Perform ? Call(promiseCapability.[[Reject]], undefined, « r »).
  4. Return promiseCapability.[[Promise]]. }
function TGocciaGlobalPromise.PromiseReject(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Capability: TPromiseCapability;
  RejectArgs: TGocciaArgumentsCollection;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  { Step 3: Call(reject, undefined, « r ») }
  RejectArgs := TGocciaArgumentsCollection.Create([AArgs.GetElement(0)]);
  try
    DispatchCall(Capability.Reject, RejectArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    RejectArgs.Free;
  end;
  { Step 4: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
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

function GetPromiseKeyedOwnKeys(
  const AObject: TGocciaObjectValue): TPromiseKeyRecordArray;
var
  KeyValues: TArray<TGocciaValue>;
  StringKeys: TArray<string>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
  I, Index: Integer;
begin
  if AObject is TGocciaProxyValue then
  begin
    KeyValues := TGocciaProxyValue(AObject).GetOwnPropertyKeyValues;
    SetLength(Result, Length(KeyValues));
    for I := 0 to High(KeyValues) do
    begin
      if KeyValues[I] is TGocciaSymbolValue then
      begin
        Result[I].IsSymbol := True;
        Result[I].Symbol := TGocciaSymbolValue(KeyValues[I]);
        Result[I].Name := '';
      end
      else
      begin
        Result[I].IsSymbol := False;
        Result[I].Symbol := nil;
        Result[I].Name := KeyValues[I].ToStringLiteral.Value;
      end;
    end;
    Exit;
  end;

  StringKeys := AObject.GetOwnPropertyKeys;
  SymbolKeys := AObject.GetOwnSymbols;
  SetLength(Result, Length(StringKeys) + Length(SymbolKeys));
  Index := 0;
  for I := 0 to High(StringKeys) do
  begin
    Result[Index].IsSymbol := False;
    Result[Index].Symbol := nil;
    Result[Index].Name := StringKeys[I];
    Inc(Index);
  end;
  for I := 0 to High(SymbolKeys) do
  begin
    Result[Index].IsSymbol := True;
    Result[Index].Symbol := SymbolKeys[I];
    Result[Index].Name := '';
    Inc(Index);
  end;
end;

function GetPromiseKeyedDescriptor(const AObject: TGocciaObjectValue;
  const AKey: TPromiseKeyRecord): TGocciaPropertyDescriptor;
begin
  if AKey.IsSymbol then
    Result := AObject.GetOwnSymbolPropertyDescriptor(AKey.Symbol)
  else
    Result := AObject.GetOwnPropertyDescriptor(AKey.Name);
end;

function GetPromiseKeyedValue(const AObject: TGocciaObjectValue;
  const AKey: TPromiseKeyRecord): TGocciaValue;
begin
  if AKey.IsSymbol then
    Result := AObject.GetSymbolProperty(AKey.Symbol)
  else
    Result := AObject.GetProperty(AKey.Name);
end;

procedure CreatePromiseKeyedPlaceholder(const AObject: TGocciaObjectValue;
  const AKey: TPromiseKeyRecord);
begin
  if AKey.IsSymbol then
    AObject.CreateDataPropertyOrThrow(AKey.Symbol,
      TGocciaUndefinedLiteralValue.UndefinedValue)
  else
    AObject.CreateDataPropertyOrThrow(AKey.Name,
      TGocciaUndefinedLiteralValue.UndefinedValue);
end;

// TC39 await-dictionary proposal: Promise.allKeyed / allSettledKeyed.
function TGocciaGlobalPromise.PromiseKeyed(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  const AAllSettled: Boolean): TGocciaValue;
var
  Capability: TPromiseCapability;
  InputValue: TGocciaValue;
  InputObject: TGocciaObjectValue;
  ResultObject: TGocciaObjectValue;
  State: TPromiseKeyedState;
  Keys: TPromiseKeyRecordArray;
  Descriptor: TGocciaPropertyDescriptor;
  PromiseLike: TGocciaValue;
  FulfillHandler: TPromiseKeyedFulfillHandler;
  RejectHandler: TPromiseKeyedRejectHandler;
  FulfillFn, RejectFn: TGocciaNativeFunctionValue;
  ResultRoot, StateRoot: TGocciaTempRoot;
  I: Integer;
begin
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(StateRoot);
  ResultObject := nil;
  State := nil;
  try
    try
      InputValue := AArgs.GetElement(0);
      if not (InputValue is TGocciaObjectValue) then
        Goccia.Values.ErrorHelper.ThrowTypeError(
          SErrorCannotConvertValueToObject, SSuggestObjectArgType);
      InputObject := TGocciaObjectValue(InputValue);

      ResultObject := TGocciaObjectValue.Create(nil);
      AddTempRootIfNeeded(ResultRoot, ResultObject);
      State := TPromiseKeyedState.Create(ResultObject, Capability.Resolve,
        Capability.Reject, 1);
      AddTempRootIfNeeded(StateRoot, State);

      Keys := GetPromiseKeyedOwnKeys(InputObject);
      for I := 0 to High(Keys) do
      begin
        Descriptor := GetPromiseKeyedDescriptor(InputObject, Keys[I]);
        if not Assigned(Descriptor) or not Descriptor.Enumerable then
          Continue;

        CreatePromiseKeyedPlaceholder(ResultObject, Keys[I]);
        State.Remaining := State.Remaining + 1;

        PromiseLike := PromiseResolveForConstructor(AThisValue,
          GetPromiseKeyedValue(InputObject, Keys[I]));
        FulfillHandler := TPromiseKeyedFulfillHandler.Create(State,
          Keys[I].Name, Keys[I].Symbol, Keys[I].IsSymbol, AAllSettled);
        RejectHandler := TPromiseKeyedRejectHandler.Create(State,
          Keys[I].Name, Keys[I].Symbol, Keys[I].IsSymbol, AAllSettled);
        FulfillFn := CreatePromiseElementFunction(FulfillHandler.Invoke,
          FulfillHandler);
        RejectFn := CreatePromiseElementFunction(RejectHandler.Invoke,
          RejectHandler);
        InvokePromiseLikeThen(PromiseLike, FulfillFn, RejectFn);
      end;

      State.Remaining := State.Remaining - 1;
      if State.Remaining = 0 then
      begin
        State.Settled := True;
        CallPromiseCapability(State.Resolve, ResultObject);
      end;
    except
      on E: EGocciaBytecodeThrow do
      begin
        Result := RejectPromiseCapabilityWithReason(Capability, E.ThrownValue);
        Exit;
      end;
      on E: TGocciaThrowValue do
      begin
        Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
        Exit;
      end;
    end;
  finally
    RemoveTempRootIfNeeded(StateRoot);
    RemoveTempRootIfNeeded(ResultRoot);
  end;

  Result := Capability.Promise;
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
  Capability: TPromiseCapability;
  State: TPromiseAllState;
  Iterator: TGocciaIteratorValue;
  PromiseResolve, Iterable, NextValue, PromiseLike: TGocciaValue;
  FulfillHandler: TPromiseAllHandler;
  FulfillFn: TGocciaNativeFunctionValue;
  Done: Boolean;
  Index: Integer;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  try
    PromiseResolve := GetPromiseResolveMethod(AThisValue);
    Iterable := AArgs.GetElement(0);
    Iterator := GetPromiseIterator(Iterable);
    State := TPromiseAllState.Create(Capability.Promise, Capability.Resolve,
      Capability.Reject, 0);
    State.Remaining := 1;
    Index := 0;

    while True do
    begin
      try
        NextValue := Iterator.DirectNext(Done);
      except
        on E: Exception do
        begin
          Result := RejectPromiseCapabilityWithException(Capability, E);
          Exit;
        end;
      end;

      if Done then
      begin
        State.Remaining := State.Remaining - 1;
        if State.Remaining = 0 then
          CallPromiseCapability(State.Resolve, State.Results);
        Result := Capability.Promise;
        Exit;
      end;

      try
        State.Results.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        PromiseLike := CallPromiseResolveMethod(PromiseResolve, AThisValue,
          NextValue);
        FulfillHandler := TPromiseAllHandler.Create(State, Index);
        FulfillFn := CreatePromiseElementFunction(FulfillHandler.Invoke,
          FulfillHandler);
        State.Remaining := State.Remaining + 1;
        InvokePromiseLikeThen(PromiseLike, FulfillFn, Capability.Reject);
      except
        on E: EGocciaBytecodeThrow do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithReason(Capability,
            E.ThrownValue);
          Exit;
        end;
        on E: TGocciaThrowValue do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
          Exit;
        end;
      end;

      Inc(Index);
    end;
  except
    on E: EGocciaBytecodeThrow do
    begin
      Result := RejectPromiseCapabilityWithReason(Capability, E.ThrownValue);
      Exit;
    end;
    on E: TGocciaThrowValue do
    begin
      Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
      Exit;
    end;
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
  Capability: TPromiseCapability;
  State: TPromiseAllState;
  Iterator: TGocciaIteratorValue;
  PromiseResolve, Iterable, NextValue, PromiseLike: TGocciaValue;
  FulfillHandler: TPromiseAllSettledFulfillHandler;
  RejectHandler: TPromiseAllSettledRejectHandler;
  FulfillFn, RejectFn: TGocciaNativeFunctionValue;
  Done: Boolean;
  Index: Integer;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  try
    PromiseResolve := GetPromiseResolveMethod(AThisValue);
    Iterable := AArgs.GetElement(0);
    Iterator := GetPromiseIterator(Iterable);
    State := TPromiseAllState.Create(Capability.Promise, Capability.Resolve,
      Capability.Reject, 0);
    State.Remaining := 1;
    Index := 0;

    while True do
    begin
      try
        NextValue := Iterator.DirectNext(Done);
      except
        on E: EGocciaBytecodeThrow do
        begin
          Result := RejectPromiseCapabilityWithReason(Capability,
            E.ThrownValue);
          Exit;
        end;
        on E: TGocciaThrowValue do
        begin
          Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
          Exit;
        end;
      end;

      if Done then
      begin
        State.Remaining := State.Remaining - 1;
        if State.Remaining = 0 then
          CallPromiseCapability(State.Resolve, State.Results);
        Result := Capability.Promise;
        Exit;
      end;

      try
        State.Results.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        PromiseLike := CallPromiseResolveMethod(PromiseResolve, AThisValue,
          NextValue);
        FulfillHandler := TPromiseAllSettledFulfillHandler.Create(State,
          Index);
        RejectHandler := TPromiseAllSettledRejectHandler.Create(State, Index);

        FulfillFn := CreatePromiseElementFunction(FulfillHandler.Invoke,
          FulfillHandler);
        RejectFn := CreatePromiseElementFunction(RejectHandler.Invoke,
          RejectHandler);
        State.Remaining := State.Remaining + 1;
        InvokePromiseLikeThen(PromiseLike, FulfillFn, RejectFn);
      except
        on E: EGocciaBytecodeThrow do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithReason(Capability,
            E.ThrownValue);
          Exit;
        end;
        on E: TGocciaThrowValue do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
          Exit;
        end;
      end;

      Inc(Index);
    end;
  except
    on E: EGocciaBytecodeThrow do
    begin
      Result := RejectPromiseCapabilityWithReason(Capability, E.ThrownValue);
      Exit;
    end;
    on E: TGocciaThrowValue do
    begin
      Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
      Exit;
    end;
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
  Capability: TPromiseCapability;
  Iterator: TGocciaIteratorValue;
  PromiseResolve, Iterable, NextValue, PromiseLike: TGocciaValue;
  Done: Boolean;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  try
    PromiseResolve := GetPromiseResolveMethod(AThisValue);
    Iterable := AArgs.GetElement(0);
    Iterator := GetPromiseIterator(Iterable);

    while True do
    begin
      try
        NextValue := Iterator.DirectNext(Done);
      except
        on E: EGocciaBytecodeThrow do
        begin
          Result := RejectPromiseCapabilityWithReason(Capability,
            E.ThrownValue);
          Exit;
        end;
        on E: TGocciaThrowValue do
        begin
          Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
          Exit;
        end;
      end;

      if Done then
      begin
        Result := Capability.Promise;
        Exit;
      end;

      try
        PromiseLike := CallPromiseResolveMethod(PromiseResolve, AThisValue,
          NextValue);
        InvokePromiseLikeThen(PromiseLike, Capability.Resolve,
          Capability.Reject);
      except
        on E: Exception do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithException(Capability, E);
          Exit;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result := RejectPromiseCapabilityWithException(Capability, E);
      Exit;
    end;
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
  Capability: TPromiseCapability;
  State: TPromiseAnyState;
  Iterator: TGocciaIteratorValue;
  PromiseResolve, Iterable, NextValue, PromiseLike: TGocciaValue;
  RejectHandler: TPromiseAnyRejectHandler;
  RejectFn: TGocciaNativeFunctionValue;
  ErrorObj: TGocciaObjectValue;
  Done: Boolean;
  Index: Integer;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);
  try
    PromiseResolve := GetPromiseResolveMethod(AThisValue);
    Iterable := AArgs.GetElement(0);
    Iterator := GetPromiseIterator(Iterable);
    State := TPromiseAnyState.Create(Capability.Promise, Capability.Resolve,
      Capability.Reject, 0);
    State.Remaining := 1;
    Index := 0;

    while True do
    begin
      try
        NextValue := Iterator.DirectNext(Done);
      except
        on E: EGocciaBytecodeThrow do
        begin
          Result := RejectPromiseCapabilityWithReason(Capability,
            E.ThrownValue);
          Exit;
        end;
        on E: TGocciaThrowValue do
        begin
          Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
          Exit;
        end;
      end;

      if Done then
      begin
        State.Remaining := State.Remaining - 1;
        if State.Remaining = 0 then
        begin
          ErrorObj := Goccia.Values.ErrorHelper.CreateErrorObject(
            AGGREGATE_ERROR_NAME, SErrorAllPromisesRejected);
          ErrorObj.AssignProperty(PROP_ERRORS, State.Errors);
          CallPromiseCapability(State.Reject, ErrorObj);
        end;
        Result := Capability.Promise;
        Exit;
      end;

      try
        State.Errors.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        PromiseLike := CallPromiseResolveMethod(PromiseResolve, AThisValue,
          NextValue);
        RejectHandler := TPromiseAnyRejectHandler.Create(State, Index);

        RejectFn := CreatePromiseElementFunction(RejectHandler.Invoke,
          RejectHandler);
        State.Remaining := State.Remaining + 1;
        InvokePromiseLikeThen(PromiseLike, Capability.Resolve, RejectFn);
      except
        on E: EGocciaBytecodeThrow do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithReason(Capability,
            E.ThrownValue);
          Exit;
        end;
        on E: TGocciaThrowValue do
        begin
          CloseIteratorPreservingError(Iterator);
          Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
          Exit;
        end;
      end;

      Inc(Index);
    end;
  except
    on E: EGocciaBytecodeThrow do
    begin
      Result := RejectPromiseCapabilityWithReason(Capability, E.ThrownValue);
      Exit;
    end;
    on E: TGocciaThrowValue do
    begin
      Result := RejectPromiseCapabilityWithReason(Capability, E.Value);
      Exit;
    end;
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
end;

function TGocciaGlobalPromise.PromiseAllKeyed(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := PromiseKeyed(AArgs, AThisValue, False);
end;

function TGocciaGlobalPromise.PromiseAllSettledKeyed(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := PromiseKeyed(AArgs, AThisValue, True);
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
  Capability: TPromiseCapability;
  ResultObj: TGocciaObjectValue;
begin
  { Step 2: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);

  { Steps 3-6: Create result object with promise, resolve, reject properties }
  ResultObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  ResultObj.CreateDataPropertyOrThrow('promise', Capability.Promise);
  ResultObj.CreateDataPropertyOrThrow('resolve', Capability.Resolve);
  ResultObj.CreateDataPropertyOrThrow('reject', Capability.Reject);

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
  Capability: TPromiseCapability;
  Callback: TGocciaValue;
  CallbackResult: TGocciaValue;
  CallbackArgs: TGocciaArgumentsCollection;
  CallbackArgCount: Integer;
  I: Integer;
begin
  { Step 3: Let promiseCapability = NewPromiseCapability(C) }
  Capability := NewPromiseCapability(AThisValue, FPromiseConstructor);

  { Step 4: Let status = Call(callbackfn, undefined, args) }
  Callback := AArgs.GetElement(0);
  try
    if AArgs.Length > 0 then
      CallbackArgCount := AArgs.Length - 1
    else
      CallbackArgCount := 0;
    CallbackArgs := TGocciaArgumentsCollection.CreateWithCapacity(
      CallbackArgCount);
    try
      for I := 1 to AArgs.Length - 1 do
        CallbackArgs.Add(AArgs.GetElement(I));
      CallbackResult := InvokeCallable(Callback, CallbackArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallbackArgs.Free;
    end;
    { Step 6: Normal completion — Call(resolve, status.[[Value]]) }
    CallPromiseCapability(Capability.Resolve, CallbackResult);
  except
    on E: EGocciaBytecodeThrow do
      CallPromiseCapability(Capability.Reject, E.ThrownValue);
    on E: TGocciaThrowValue do
      { Step 5: Abrupt completion — Call(reject, status.[[Value]]) }
      CallPromiseCapability(Capability.Reject, E.Value);
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: Exception do
      CallPromiseCapability(Capability.Reject, CreateErrorObject(ERROR_NAME,
        E.Message));
  end;

  { Step 7: Return promiseCapability.[[Promise]] }
  Result := Capability.Promise;
end;

end.
