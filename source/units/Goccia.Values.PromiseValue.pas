unit Goccia.Values.PromiseValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaPromiseState = (gpsPending, gpsFulfilled, gpsRejected);

  TGocciaPromiseValue = class;
  TGocciaPromiseReactionCapability = class;

  TGocciaPromiseReaction = record
    OnFulfilled: TGocciaValue;
    OnRejected: TGocciaValue;
    ResultPromise: TGocciaValue;
  end;

  TGocciaPromiseFinallyWrapper = class(TGocciaObjectValue)
  private
    FOnFinally: TGocciaValue;
    FConstructor: TGocciaValue;
    FIsFulfill: Boolean;
  public
    constructor Create(const AOnFinally, AConstructor: TGocciaValue;
      const AIsFulfill: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TGocciaPromiseReactionCapability = class(TGocciaObjectValue)
  private
    FPromise: TGocciaValue;
    FResolve: TGocciaValue;
    FReject: TGocciaValue;
    procedure CallCapability(const AFunction, AValue: TGocciaValue);
  public
    constructor Create(const APromise, AResolve, AReject: TGocciaValue);
    procedure Resolve(const AValue: TGocciaValue);
    procedure Reject(const AValue: TGocciaValue);
    procedure MarkReferences; override;
    property Promise: TGocciaValue read FPromise;
  end;

  TGocciaPromiseValue = class(TGocciaObjectValue)
  private
    FState: TGocciaPromiseState;
    FResult: TGocciaValue;
    FReactions: TList<TGocciaPromiseReaction>;
    FAlreadyResolved: Boolean;

    procedure TriggerReactions;
    procedure InitializePrototype;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure Resolve(const AValue: TGocciaValue);
    procedure Reject(const AReason: TGocciaValue);
    procedure SubscribeTo(const APromise: TGocciaPromiseValue);

    function DoResolve(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DoReject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;
    function BuiltinTagFallback: Boolean; override;

    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    function InvokeThen(const AOnFulfilled, AOnRejected: TGocciaValue): TGocciaPromiseValue;
    function InvokeFinally(const AOnFinally: TGocciaValue): TGocciaPromiseValue;

    property State: TGocciaPromiseState read FState;
    property PromiseResult: TGocciaValue read FResult;
  published
    function PromiseThen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseCatch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PromiseFinally(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

function GetPromiseIntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;

implementation

uses
  Goccia.Arithmetic,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.VM.Exception;

var
  GPromiseSharedSlot: TGocciaRealmOwnedSlotId;
  GPromiseDefaultConstructorSlot: TGocciaRealmSlotId;

function GetPromiseSharedForRealm(
  const ARealm: TGocciaRealm): TGocciaSharedPrototype; inline;
begin
  if Assigned(ARealm) then
    Result := TGocciaSharedPrototype(ARealm.GetOwnedSlot(GPromiseSharedSlot))
  else
    Result := nil;
end;

function GetPromiseShared: TGocciaSharedPrototype; inline;
begin
  Result := GetPromiseSharedForRealm(CurrentRealm);
end;

function GetPromiseDefaultConstructorForRealm(
  const ARealm: TGocciaRealm): TGocciaValue; inline;
begin
  if Assigned(ARealm) then
    Result := TGocciaValue(ARealm.GetSlot(GPromiseDefaultConstructorSlot))
  else
    Result := nil;
end;

procedure SetPromiseDefaultConstructorForRealm(const ARealm: TGocciaRealm;
  const AConstructor: TGocciaValue); inline;
begin
  if Assigned(ARealm) then
    ARealm.SetSlot(GPromiseDefaultConstructorSlot, AConstructor);
end;

function GetPromiseIntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetPromiseSharedForRealm(ARealm);
  if Assigned(Shared) then
    Result := Shared.Prototype
  else
    Result := nil;
end;

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
    FInvoked: Boolean;
  public
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
    property Resolve: TGocciaValue read FResolve;
    property Reject: TGocciaValue read FReject;
  end;

  TGocciaFinallyPassthrough = class(TGocciaObjectValue)
  private
    FOriginalValue: TGocciaValue;
    FIsFulfill: Boolean;
  public
    constructor Create(const AOriginalValue: TGocciaValue; const AIsFulfill: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

function TPromiseCapabilityExecutor.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FInvoked then
    ThrowTypeError(SErrorPromiseResolverNotFunction, SSuggestPromiseResolver);

  FInvoked := True;
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

function NewPromiseCapability(
  const AConstructor: TGocciaValue): TPromiseCapability;
var
  ExecutorHost: TPromiseCapabilityExecutor;
  ExecutorFn: TGocciaNativeFunctionValue;
  ConstructArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
begin
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

    Result.Promise := ConstructValue(AConstructor, ConstructArgs,
      AConstructor);
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

function GetPromiseDefaultConstructor: TGocciaValue;
begin
  Result := GetPromiseDefaultConstructorForRealm(CurrentRealm);
  if not Assigned(Result) and Assigned(GetPromiseShared) then
    Result := GetPromiseShared.Prototype.GetProperty(PROP_CONSTRUCTOR);
end;

function PromiseSpeciesConstructor(
  const APromise: TGocciaObjectValue): TGocciaValue;
var
  ConstructorValue, SpeciesValue: TGocciaValue;
begin
  Result := GetPromiseDefaultConstructor;
  ConstructorValue := APromise.GetProperty(PROP_CONSTRUCTOR);
  if ConstructorValue is TGocciaUndefinedLiteralValue then
    Exit;
  if not (ConstructorValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);

  SpeciesValue := TGocciaObjectValue(ConstructorValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies);
  if (SpeciesValue is TGocciaUndefinedLiteralValue) or
     (SpeciesValue is TGocciaNullLiteralValue) then
    Exit;
  if SpeciesValue.IsConstructable then
    Exit(SpeciesValue);

  ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);
end;

function PromiseResolveForConstructor(const AConstructor,
  AValue: TGocciaValue): TGocciaValue;
var
  ValueConstructor: TGocciaValue;
  Capability: TPromiseCapability;
  ResolveArgs: TGocciaArgumentsCollection;
begin
  if not (AConstructor is TGocciaObjectValue) then
    ThrowTypeError(SErrorValueNotConstructor, SSuggestNotConstructorType);

  if AValue is TGocciaPromiseValue then
  begin
    ValueConstructor := TGocciaPromiseValue(AValue).GetProperty(
      PROP_CONSTRUCTOR);
    if IsSameValue(ValueConstructor, AConstructor) then
      Exit(AValue);
  end;

  Capability := NewPromiseCapability(AConstructor);
  ResolveArgs := TGocciaArgumentsCollection.Create([AValue]);
  try
    DispatchCall(Capability.Resolve, ResolveArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    ResolveArgs.Free;
  end;
  Result := Capability.Promise;
end;

function InvokeThenMethod(const AReceiver, AOnFulfilled,
  AOnRejected: TGocciaValue): TGocciaValue;
var
  ReceiverObject: TGocciaObjectValue;
  ThenMethod: TGocciaValue;
  ThenArgs: TGocciaArgumentsCollection;
begin
  ReceiverObject := ToObject(AReceiver);
  ThenMethod := ReceiverObject.GetPropertyWithContext(PROP_THEN, AReceiver);
  if not Assigned(ThenMethod) or not ThenMethod.IsCallable then
    ThrowTypeError(SErrorThenNotFunction, SSuggestPromiseThisType);

  ThenArgs := TGocciaArgumentsCollection.Create([AOnFulfilled, AOnRejected]);
  try
    Result := DispatchCall(ThenMethod, ThenArgs, AReceiver);
  finally
    ThenArgs.Free;
  end;
end;

function InvokeThenMethodOneArg(const AReceiver,
  AOnFulfilled: TGocciaValue): TGocciaValue;
var
  ReceiverObject: TGocciaObjectValue;
  ThenMethod: TGocciaValue;
  ThenArgs: TGocciaArgumentsCollection;
begin
  ReceiverObject := ToObject(AReceiver);
  ThenMethod := ReceiverObject.GetPropertyWithContext(PROP_THEN, AReceiver);
  if not Assigned(ThenMethod) or not ThenMethod.IsCallable then
    ThrowTypeError(SErrorThenNotFunction, SSuggestPromiseThisType);

  ThenArgs := TGocciaArgumentsCollection.Create([AOnFulfilled]);
  try
    Result := DispatchCall(ThenMethod, ThenArgs, AReceiver);
  finally
    ThenArgs.Free;
  end;
end;

{ TGocciaPromiseReactionCapability }

constructor TGocciaPromiseReactionCapability.Create(
  const APromise, AResolve, AReject: TGocciaValue);
begin
  inherited Create(nil);
  FPromise := APromise;
  FResolve := AResolve;
  FReject := AReject;
end;

procedure TGocciaPromiseReactionCapability.CallCapability(
  const AFunction, AValue: TGocciaValue);
var
  Args: TGocciaArgumentsCollection;
begin
  if not Assigned(AFunction) or not AFunction.IsCallable then Exit;
  Args := TGocciaArgumentsCollection.Create([AValue]);
  try
    DispatchCall(AFunction, Args, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Args.Free;
  end;
end;

procedure TGocciaPromiseReactionCapability.Resolve(const AValue: TGocciaValue);
begin
  CallCapability(FResolve, AValue);
end;

procedure TGocciaPromiseReactionCapability.Reject(const AValue: TGocciaValue);
begin
  CallCapability(FReject, AValue);
end;

procedure TGocciaPromiseReactionCapability.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
  if Assigned(FResolve) then
    FResolve.MarkReferences;
  if Assigned(FReject) then
    FReject.MarkReferences;
end;

{ TGocciaFinallyPassthrough }

constructor TGocciaFinallyPassthrough.Create(const AOriginalValue: TGocciaValue; const AIsFulfill: Boolean);
begin
  inherited Create(nil);
  FOriginalValue := AOriginalValue;
  FIsFulfill := AIsFulfill;
end;

function TGocciaFinallyPassthrough.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if FIsFulfill then
    Result := FOriginalValue
  else
    raise TGocciaThrowValue.Create(FOriginalValue);
end;

procedure TGocciaFinallyPassthrough.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FOriginalValue) then
    FOriginalValue.MarkReferences;
end;

{ TGocciaPromiseFinallyWrapper }

constructor TGocciaPromiseFinallyWrapper.Create(const AOnFinally,
  AConstructor: TGocciaValue; const AIsFulfill: Boolean);
begin
  inherited Create(nil);
  FOnFinally := AOnFinally;
  FConstructor := AConstructor;
  FIsFulfill := AIsFulfill;
end;

function TGocciaPromiseFinallyWrapper.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  OriginalValue, CallbackResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallbackPromise: TGocciaValue;
  Passthrough: TGocciaFinallyPassthrough;
  PassthroughFn: TGocciaNativeFunctionValue;
begin
  OriginalValue := AArgs.GetElement(0);

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    CallbackResult := DispatchCall(FOnFinally, CallArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallArgs.Free;
  end;

  CallbackPromise := PromiseResolveForConstructor(FConstructor,
    CallbackResult);
  Passthrough := TGocciaFinallyPassthrough.Create(OriginalValue, FIsFulfill);
  PassthroughFn := CreateAnonymousPromiseBuiltinFunction(Passthrough.Invoke,
    Passthrough, 0);
  Result := InvokeThenMethodOneArg(CallbackPromise, PassthroughFn);
end;

procedure TGocciaPromiseFinallyWrapper.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FOnFinally) then
    FOnFinally.MarkReferences;
  if Assigned(FConstructor) then
    FConstructor.MarkReferences;
end;

{ TGocciaPromiseValue }

constructor TGocciaPromiseValue.Create;
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FState := gpsPending;
  FResult := TGocciaUndefinedLiteralValue.UndefinedValue;
  FAlreadyResolved := False;
  InitializePrototype;
  Shared := GetPromiseShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaPromiseValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetPromiseShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GPromiseSharedSlot, Shared);
  if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    TGocciaObjectValue.InitializeSharedPrototype;
  Shared.Prototype.Prototype := TGocciaObjectValue.SharedObjectPrototype;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(PromiseThen, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(PromiseCatch, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(PromiseFinally, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Promise'),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaPromiseValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetPromiseShared;
  if not Assigned(Shared) then
  begin
    TGocciaPromiseValue.Create;
    Shared := GetPromiseShared;
  end;
  if Assigned(Shared) then
  begin
    SetPromiseDefaultConstructorForRealm(CurrentRealm, AConstructor);
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
    AConstructor.DefineProperty(PROP_PROTOTYPE,
      TGocciaPropertyDescriptorData.Create(Shared.Prototype, []));
  end;
end;

destructor TGocciaPromiseValue.Destroy;
begin
  FReactions.Free;
  inherited;
end;

procedure TGocciaPromiseValue.MarkReferences;
var
  I: Integer;
  Reaction: TGocciaPromiseReaction;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FResult) then
    FResult.MarkReferences;

  if Assigned(FReactions) then
    for I := 0 to FReactions.Count - 1 do
    begin
      Reaction := FReactions[I];
      if Assigned(Reaction.OnFulfilled) then
        Reaction.OnFulfilled.MarkReferences;
      if Assigned(Reaction.OnRejected) then
        Reaction.OnRejected.MarkReferences;
      if Assigned(Reaction.ResultPromise) then
        Reaction.ResultPromise.MarkReferences;
    end;
end;

// ES2026 §27.2.1.3.2 Promise Resolve Functions ( resolution )
procedure TGocciaPromiseValue.Resolve(const AValue: TGocciaValue);
var
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
  ThenMethod: TGocciaValue;
begin
  if FState <> gpsPending then Exit;

  if AValue = Self then
  begin
    FState := gpsRejected;
    FResult := Goccia.Values.ErrorHelper.CreateErrorObject(TYPE_ERROR_NAME,
      SErrorPromiseChainingCycle);
    TriggerReactions;
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    try
      // ES2026 §27.2.1.3.2 step 9: Let then be Get(resolution, "then").
      ThenMethod := TGocciaObjectValue(AValue).GetProperty(PROP_THEN);
    except
      on E: EGocciaBytecodeThrow do
      begin
        Reject(E.ThrownValue);
        Exit;
      end;
      on E: TGocciaThrowValue do
      begin
        Reject(E.Value);
        Exit;
      end;
    end;

    // ES2026 §27.2.1.3.2 steps 11-15: If then is callable, resolve via the thenable.
    if Assigned(ThenMethod) and ThenMethod.IsCallable then
    begin
      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) then
      begin
        Task.Handler := ThenMethod;
        Task.Value := AValue;
        Task.ResultPromise := Self;
        Task.ReactionType := prtThenableResolve;
        Queue.Enqueue(Task);
      end;
      Exit;
    end;
  end;

  FState := gpsFulfilled;
  FResult := AValue;
  TriggerReactions;
end;

procedure TGocciaPromiseValue.Reject(const AReason: TGocciaValue);
begin
  if FState <> gpsPending then Exit;

  FState := gpsRejected;
  FResult := AReason;
  TriggerReactions;
end;

procedure TGocciaPromiseValue.SubscribeTo(const APromise: TGocciaPromiseValue);
var
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  case APromise.FState of
    gpsFulfilled, gpsRejected:
    begin
      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) then
      begin
        Task.Handler := nil;
        Task.Value := APromise.FResult;
        Task.ResultPromise := Self;
        if APromise.FState = gpsFulfilled then
          Task.ReactionType := prtFulfill
        else
          Task.ReactionType := prtReject;
        Queue.Enqueue(Task);
      end;
    end;
    gpsPending:
    begin
      Reaction.OnFulfilled := nil;
      Reaction.OnRejected := nil;
      Reaction.ResultPromise := Self;
      if not Assigned(APromise.FReactions) then
        APromise.FReactions := TList<TGocciaPromiseReaction>.Create;
      APromise.FReactions.Add(Reaction);
    end;
  end;
end;

procedure TGocciaPromiseValue.TriggerReactions;
var
  I: Integer;
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  if not Assigned(FReactions) or (FReactions.Count = 0) then Exit;
  Queue := TGocciaMicrotaskQueue.Instance;
  if not Assigned(Queue) then Exit;

  for I := 0 to FReactions.Count - 1 do
  begin
    Reaction := FReactions[I];

    case FState of
      gpsFulfilled:
      begin
        Task.Handler := Reaction.OnFulfilled;
        Task.Value := FResult;
        Task.ResultPromise := Reaction.ResultPromise;
        Task.ReactionType := prtFulfill;
        Queue.Enqueue(Task);
      end;
      gpsRejected:
      begin
        Task.Handler := Reaction.OnRejected;
        Task.Value := FResult;
        Task.ResultPromise := Reaction.ResultPromise;
        Task.ReactionType := prtReject;
        Queue.Enqueue(Task);
      end;
    end;
  end;

  FReactions.Clear;
end;

function TGocciaPromiseValue.DoResolve(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then Exit;
  FAlreadyResolved := True;

  if AArgs.Length > 0 then
    Resolve(AArgs.GetElement(0))
  else
    Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaPromiseValue.DoReject(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FAlreadyResolved then Exit;
  FAlreadyResolved := True;

  if AArgs.Length > 0 then
    Reject(AArgs.GetElement(0))
  else
    Reject(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

{ Public helper — core then logic without TGocciaArgumentsCollection overhead }

function PerformPromiseThen(const APromise: TGocciaPromiseValue;
  const AOnFulfilled, AOnRejected: TGocciaValue;
  const ACapability: TPromiseCapability): TGocciaValue;
var
  CapabilityHost: TGocciaPromiseReactionCapability;
  Reaction: TGocciaPromiseReaction;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  CapabilityHost := TGocciaPromiseReactionCapability.Create(
    ACapability.Promise, ACapability.Resolve, ACapability.Reject);
  Queue := TGocciaMicrotaskQueue.Instance;

  case APromise.FState of
    gpsPending:
    begin
      Reaction.OnFulfilled := AOnFulfilled;
      Reaction.OnRejected := AOnRejected;
      Reaction.ResultPromise := CapabilityHost;
      if not Assigned(APromise.FReactions) then
        APromise.FReactions := TList<TGocciaPromiseReaction>.Create;
      APromise.FReactions.Add(Reaction);
    end;
    gpsFulfilled:
    begin
      Task.Handler := AOnFulfilled;
      Task.Value := APromise.FResult;
      Task.ResultPromise := CapabilityHost;
      Task.ReactionType := prtFulfill;
      if Assigned(Queue) then
        Queue.Enqueue(Task);
    end;
    gpsRejected:
    begin
      Task.Handler := AOnRejected;
      Task.Value := APromise.FResult;
      Task.ResultPromise := CapabilityHost;
      Task.ReactionType := prtReject;
      if Assigned(Queue) then
        Queue.Enqueue(Task);
    end;
  end;

  Result := ACapability.Promise;
end;

function TGocciaPromiseValue.InvokeThen(
  const AOnFulfilled, AOnRejected: TGocciaValue): TGocciaPromiseValue;
var
  Capability: TPromiseCapability;
  ChildPromise: TGocciaPromiseValue;
begin
  ChildPromise := TGocciaPromiseValue.Create;
  Capability.Promise := ChildPromise;
  Capability.Resolve := CreateAnonymousPromiseBuiltinFunction(
    ChildPromise.DoResolve, ChildPromise, 1);
  Capability.Reject := CreateAnonymousPromiseBuiltinFunction(
    ChildPromise.DoReject, ChildPromise, 1);
  Result := TGocciaPromiseValue(PerformPromiseThen(Self, AOnFulfilled,
    AOnRejected, Capability));
end;

{ Public helper — core finally logic without TGocciaArgumentsCollection overhead }

function TGocciaPromiseValue.InvokeFinally(
  const AOnFinally: TGocciaValue): TGocciaPromiseValue;
var
  ConstructorValue: TGocciaValue;
  WrapFulfilled, WrapRejected: TGocciaPromiseFinallyWrapper;
  WrapFulfilledFn, WrapRejectedFn: TGocciaNativeFunctionValue;
begin
  if not Assigned(AOnFinally) or not AOnFinally.IsCallable then
    Exit(InvokeThen(nil, nil));

  ConstructorValue := PromiseSpeciesConstructor(Self);
  WrapFulfilled := TGocciaPromiseFinallyWrapper.Create(AOnFinally,
    ConstructorValue, True);
  WrapRejected := TGocciaPromiseFinallyWrapper.Create(AOnFinally,
    ConstructorValue, False);
  WrapFulfilledFn := CreateAnonymousPromiseBuiltinFunction(
    WrapFulfilled.Invoke, WrapFulfilled, 1);
  WrapRejectedFn := CreateAnonymousPromiseBuiltinFunction(
    WrapRejected.Invoke, WrapRejected, 1);
  Result := InvokeThen(WrapFulfilledFn, WrapRejectedFn);
end;

{ Prototype methods }

function TGocciaPromiseValue.PromiseThen(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  P: TGocciaPromiseValue;
  OnFulfilled, OnRejected: TGocciaValue;
  ConstructorValue: TGocciaValue;
  Capability: TPromiseCapability;
begin
  if not (AThisValue is TGocciaPromiseValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseThenNonPromise, SSuggestPromiseThisType);

  P := TGocciaPromiseValue(AThisValue);
  ConstructorValue := PromiseSpeciesConstructor(P);
  Capability := NewPromiseCapability(ConstructorValue);

  OnFulfilled := nil;
  OnRejected := nil;
  if (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable then
    OnFulfilled := AArgs.GetElement(0);
  if (AArgs.Length > 1) and AArgs.GetElement(1).IsCallable then
    OnRejected := AArgs.GetElement(1);

  Result := PerformPromiseThen(P, OnFulfilled, OnRejected, Capability);
end;

function TGocciaPromiseValue.PromiseCatch(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ThenArgs: TGocciaArgumentsCollection;
begin
  ThenArgs := TGocciaArgumentsCollection.Create([
    TGocciaUndefinedLiteralValue.UndefinedValue,
    AArgs.GetElement(0)
  ]);
  try
    Result := InvokeThenMethod(AThisValue, ThenArgs.GetElement(0),
      ThenArgs.GetElement(1));
  finally
    ThenArgs.Free;
  end;
end;

function TGocciaPromiseValue.PromiseFinally(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  OnFinally: TGocciaValue;
  ConstructorValue: TGocciaValue;
  ThenFinally, CatchFinally: TGocciaValue;
  WrapFulfilled, WrapRejected: TGocciaPromiseFinallyWrapper;
begin
  if not (AThisValue is TGocciaObjectValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorPromiseFinallyNonPromise, SSuggestPromiseThisType);

  ConstructorValue := PromiseSpeciesConstructor(TGocciaObjectValue(AThisValue));
  OnFinally := AArgs.GetElement(0);
  if Assigned(OnFinally) and OnFinally.IsCallable then
  begin
    WrapFulfilled := TGocciaPromiseFinallyWrapper.Create(OnFinally,
      ConstructorValue, True);
    WrapRejected := TGocciaPromiseFinallyWrapper.Create(OnFinally,
      ConstructorValue, False);
    ThenFinally := CreateAnonymousPromiseBuiltinFunction(
      WrapFulfilled.Invoke, WrapFulfilled, 1);
    CatchFinally := CreateAnonymousPromiseBuiltinFunction(
      WrapRejected.Invoke, WrapRejected, 1);
  end
  else
  begin
    ThenFinally := OnFinally;
    CatchFinally := OnFinally;
  end;

  Result := InvokeThenMethod(AThisValue, ThenFinally, CatchFinally);
end;

function TGocciaPromiseValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaPromiseValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaPromiseValue.ToStringTag: string;
begin
  Result := 'Promise';
end;

function TGocciaPromiseValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
end;

initialization
  GPromiseSharedSlot := RegisterRealmOwnedSlot('Promise.shared');
  GPromiseDefaultConstructorSlot := RegisterRealmSlot('Promise.defaultConstructor');

end.
