{ The `node:async_hooks` namespace: AsyncLocalStorage and AsyncResource.

  Both are thin wrappers over Goccia.AsyncContext, which owns the snapshot
  mechanism and the propagation seams. Nothing here knows how a continuation
  is created; it only derives snapshots and installs them around a call.

  See docs/adr/0112-native-async-local-storage.md for the decision and the
  scope cuts. }

unit Goccia.Builtins.AsyncHooks;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

{ Returns the namespace object. AHostToken receives an opaque handle for the
  per-namespace host state; pass it to ReleaseAsyncHooksHost when the module
  registration that owns this namespace goes away. }
function CreateAsyncHooksNamespace(out AHostToken: TObject): TGocciaObjectValue;
procedure ReleaseAsyncHooksHost(const AHostToken: TObject);
procedure ClearAsyncHooksHosts;

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.AsyncContext,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved,
  Goccia.ObjectModel,
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

const
  ASYNC_LOCAL_STORAGE_NAME = 'AsyncLocalStorage';
  ASYNC_RESOURCE_NAME = 'AsyncResource';
  PROP_DEFAULT_VALUE = 'defaultValue';
  { Node reports an unnamed AsyncLocalStorage as the empty string, not as
    undefined; probed against Node v24.0.1. }
  UNNAMED_STORAGE = '';
  { AsyncResource ids exist so that code written for Node can read them. They
    are unique per resource and never recycled; nothing in GocciaScript acts
    on them, and there is no async-hooks callback surface to relate them to. }
  FIRST_ASYNC_ID = 1;
  { Every bind-family member returns a function Node calls `bound`, with the
    target's own length. The snapshot runner takes (callback, ...args), so its
    length is one however long the callback is. }
  BOUND_FUNCTION_NAME = 'bound';
  SNAPSHOT_RUNNER_LENGTH = 1;

type
  TGocciaAsyncHooksHostList = TObjectList<TObject>;

  { One AsyncLocalStorage instance. Identity is the snapshot key, so the
    instance object itself is what a snapshot binds a store to. }
  TGocciaAsyncLocalStorageValue = class(TGocciaObjectValue)
  private
    FDefaultValue: TGocciaValue;
    FStorageName: string;
  public
    procedure MarkReferences; override;
    property DefaultValue: TGocciaValue read FDefaultValue write FDefaultValue;
    property StorageName: string read FStorageName write FStorageName;
  end;

  { An AsyncResource captures the async context once, at construction, and
    replays it on demand. }
  TGocciaAsyncResourceValue = class(TGocciaObjectValue)
  private
    FContext: TGocciaAsyncContextSnapshot;
    FAsyncId: Double;
  public
    procedure MarkReferences; override;
    property Context: TGocciaAsyncContextSnapshot read FContext write FContext;
    property AsyncId: Double read FAsyncId write FAsyncId;
  end;

  { A callable bound to one snapshot. Backs AsyncResource.prototype.bind,
    AsyncResource.bind, AsyncLocalStorage.bind and the function returned by
    AsyncLocalStorage.snapshot. }
  TGocciaAsyncBoundFunction = class(TGocciaObjectValue)
  private
    FTarget: TGocciaValue;
    FBoundThis: TGocciaValue;
    FContext: TGocciaAsyncContextSnapshot;
    FUsesCallerTarget: Boolean;
  public
    constructor Create(const ATarget, ABoundThis: TGocciaValue;
      const AContext: TGocciaAsyncContextSnapshot;
      const AUsesCallerTarget: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TGocciaAsyncHooksNamespaceHost = class
  private
    FAsyncLocalStoragePrototype: TGocciaObjectValue;
    FAsyncResourcePrototype: TGocciaObjectValue;
    FNextAsyncId: Double;

    function RequireStorage(
      const AThisValue: TGocciaValue): TGocciaAsyncLocalStorageValue;
    function RequireResource(
      const AThisValue: TGocciaValue): TGocciaAsyncResourceValue;
    function CallInContext(const ACallback, AThisValue: TGocciaValue;
      const AArgs: TGocciaArgumentsCollection; const AFirstArgument: Integer;
      const AContext: TGocciaAsyncContextSnapshot): TGocciaValue;
    function CreateBoundFunction(const ATarget, ABoundThis: TGocciaValue;
      const AContext: TGocciaAsyncContextSnapshot;
      const AUsesCallerTarget: Boolean): TGocciaValue;
    function ReceiverArgument(const AArgs: TGocciaArgumentsCollection;
      const AIndex: Integer): TGocciaValue;
  public
    constructor Create;

    function StorageConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageRun(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageGetStore(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageEnterWith(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageExit(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageDisable(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageGetName(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageStaticBind(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StorageStaticSnapshot(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function ResourceConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResourceRunInAsyncScope(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResourceBind(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResourceStaticBind(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResourceAsyncId(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResourceTriggerAsyncId(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResourceEmitDestroy(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

threadvar
  GAsyncHooksHosts: TGocciaAsyncHooksHostList;

{ Node's bind family runs validateFunction(fn) before it builds anything, so a
  non-callable is a TypeError at the bind call rather than a surprise at the
  first invocation. }
procedure RequireCallable(const AValue: TGocciaValue);
begin
  if not (Assigned(AValue) and AValue.IsCallable) then
    ThrowTypeError(SErrorAsyncHooksCallbackRequired, SSuggestCallbackRequired);
end;

{ The `length` a bound wrapper should report: the target's own, read through
  the ordinary property lookup so a bound or native target answers too. A
  snapshot runner has no target to ask. }
function CallableLength(const ATarget: TGocciaValue;
  const AUsesCallerTarget: Boolean): Integer;
var
  LengthValue: TGocciaValue;
  Reported: Double;
begin
  if AUsesCallerTarget then
    Exit(SNAPSHOT_RUNNER_LENGTH);
  if not (ATarget is TGocciaObjectValue) then
    Exit(0);

  LengthValue := TGocciaObjectValue(ATarget).GetProperty(PROP_LENGTH);
  if not (LengthValue is TGocciaNumberLiteralValue) then
    Exit(0);

  Reported := TGocciaNumberLiteralValue(LengthValue).Value;
  if (Reported <> Reported) or (Reported < 0) or (Reported > MaxInt) then
    Exit(0);
  Result := Trunc(Reported);
end;

{ TGocciaAsyncLocalStorageValue }

procedure TGocciaAsyncLocalStorageValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FDefaultValue) then
    FDefaultValue.MarkReferences;
end;

{ TGocciaAsyncResourceValue }

procedure TGocciaAsyncResourceValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FContext) then
    FContext.MarkReferences;
end;

{ TGocciaAsyncBoundFunction }

constructor TGocciaAsyncBoundFunction.Create(
  const ATarget, ABoundThis: TGocciaValue;
  const AContext: TGocciaAsyncContextSnapshot;
  const AUsesCallerTarget: Boolean);
begin
  inherited Create(nil);
  FTarget := ATarget;
  FBoundThis := ABoundThis;
  FContext := AContext;
  FUsesCallerTarget := AUsesCallerTarget;
end;

function TGocciaAsyncBoundFunction.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ContextToken, I: Integer;
  Receiver, Target: TGocciaValue;
begin
  { AsyncLocalStorage.snapshot() returns a function that takes the callback to
    run as its first argument; every other bound form closes over its target
    and forwards all arguments. }
  if FUsesCallerTarget then
    Target := AArgs.GetElement(0)
  else
    Target := FTarget;

  if not (Assigned(Target) and Target.IsCallable) then
    ThrowTypeError(SErrorAsyncHooksCallbackRequired, SSuggestCallbackRequired);

  (* Node forwards the call-site receiver whenever thisArg is undefined,
     whether it was omitted or written out — `bind` splits on
     `thisArg === undefined`, not on the argument count. So a bound function
     installed as an object method, `{ tag, run: resource.bind(fn) }`, sees the
     holder as `this`, and only an explicitly non-undefined thisArg displaces
     it. Probed against Node v24.0.1.

     A snapshot runner is not a bound function in that sense: Node implements
     snapshot() as `AsyncResource.bind((cb, ...args) => cb(...args))`, and the
     plain `cb(...)` inside it passes no receiver at all. Installing a runner
     as an object method must therefore not hand its holder to the callback. *)
  if FUsesCallerTarget then
    Receiver := TGocciaUndefinedLiteralValue.UndefinedValue
  else
  begin
    Receiver := FBoundThis;
    if not Assigned(Receiver) then
      Receiver := AThisValue;
  end;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    if FUsesCallerTarget then
      for I := 1 to AArgs.Length - 1 do
        CallArgs.Add(AArgs.GetElement(I))
    else
      for I := 0 to AArgs.Length - 1 do
        CallArgs.Add(AArgs.GetElement(I));

    ContextToken := EnterAsyncContext(FContext);
    try
      Result := DispatchCall(Target, CallArgs, Receiver);
    finally
      LeaveAsyncContext(ContextToken);
    end;
  finally
    CallArgs.Free;
  end;
end;

procedure TGocciaAsyncBoundFunction.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FTarget) then
    FTarget.MarkReferences;
  if Assigned(FBoundThis) then
    FBoundThis.MarkReferences;
  if Assigned(FContext) then
    FContext.MarkReferences;
end;

{ TGocciaAsyncHooksNamespaceHost }

constructor TGocciaAsyncHooksNamespaceHost.Create;
begin
  inherited Create;
  FNextAsyncId := FIRST_ASYNC_ID;
end;

function TGocciaAsyncHooksNamespaceHost.RequireStorage(
  const AThisValue: TGocciaValue): TGocciaAsyncLocalStorageValue;
begin
  if not (AThisValue is TGocciaAsyncLocalStorageValue) then
    ThrowTypeError(SErrorAsyncLocalStorageReceiver,
      SSuggestAsyncLocalStorageReceiver);
  Result := TGocciaAsyncLocalStorageValue(AThisValue);
end;

function TGocciaAsyncHooksNamespaceHost.RequireResource(
  const AThisValue: TGocciaValue): TGocciaAsyncResourceValue;
begin
  if not (AThisValue is TGocciaAsyncResourceValue) then
    ThrowTypeError(SErrorAsyncResourceReceiver, SSuggestAsyncResourceReceiver);
  Result := TGocciaAsyncResourceValue(AThisValue);
end;

{ Calls ACallback with AArgs[AFirstArgument..] under AContext, and restores the
  enclosing context on every exit path — including the throwing one, which is
  what keeps a rejected async callback from leaving its bindings behind. }
function TGocciaAsyncHooksNamespaceHost.CallInContext(
  const ACallback, AThisValue: TGocciaValue;
  const AArgs: TGocciaArgumentsCollection; const AFirstArgument: Integer;
  const AContext: TGocciaAsyncContextSnapshot): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ContextToken, I: Integer;
begin
  { Enter before anything else runs, and through the saved-snapshot stack.
    AContext was derived by the caller and is reachable from nothing until it
    is the current context, so any allocation in between — the argument
    collection, or the error object on the non-callable path — could collect
    it; and the snapshot it displaces has no other root either, so parking it
    in a local across the callback made a collection inside that callback free
    it and the restore write a dangling pointer. }
  ContextToken := EnterAsyncContext(AContext);
  try
    if not (Assigned(ACallback) and ACallback.IsCallable) then
      ThrowTypeError(SErrorAsyncHooksCallbackRequired,
        SSuggestCallbackRequired);

    CallArgs := TGocciaArgumentsCollection.Create;
    try
      for I := AFirstArgument to AArgs.Length - 1 do
        CallArgs.Add(AArgs.GetElement(I));
      Result := DispatchCall(ACallback, CallArgs, AThisValue);
    finally
      CallArgs.Free;
    end;
  finally
    LeaveAsyncContext(ContextToken);
  end;
end;

function TGocciaAsyncHooksNamespaceHost.CreateBoundFunction(
  const ATarget, ABoundThis: TGocciaValue;
  const AContext: TGocciaAsyncContextSnapshot;
  const AUsesCallerTarget: Boolean): TGocciaValue;
var
  Bound: TGocciaAsyncBoundFunction;
  BoundFunction: TGocciaNativeFunctionValue;
  BoundRoot: TGocciaTempRoot;
begin
  Bound := TGocciaAsyncBoundFunction.Create(ATarget, ABoundThis, AContext,
    AUsesCallerTarget);
  { Nothing refers to Bound until CapturedRoot does, and allocating the
    wrapper is a collection safe point. }
  InitializeTempRoot(BoundRoot);
  AddTempRootIfNeeded(BoundRoot, Bound);
  try
    { Node names every one of these `bound` — not `bound <fn>` — because the
      wrapper it returns is a function declared under that name, and copies
      the target's length onto it. Probed against Node v24.0.1. }
    BoundFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Bound.Invoke, BOUND_FUNCTION_NAME,
      CallableLength(ATarget, AUsesCallerTarget));
    BoundFunction.CapturedRoot := Bound;
    Result := BoundFunction;
  finally
    RemoveTempRootIfNeeded(BoundRoot);
  end;
end;

{ The receiver a bind-family member was given, or nil when it was omitted or
  written as undefined — the two cases Node treats alike. GetElement answers
  undefined for an absent index, so an Assigned check alone can never see the
  difference and the call-site-receiver fallback would be dead. }
function TGocciaAsyncHooksNamespaceHost.ReceiverArgument(
  const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer): TGocciaValue;
begin
  Result := AArgs.GetElement(AIndex);
  if Result is TGocciaUndefinedLiteralValue then
    Result := nil;
end;

function TGocciaAsyncHooksNamespaceHost.StorageConstructor(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  NameValue: TGocciaValue;
  Options: TGocciaObjectValue;
  Storage: TGocciaAsyncLocalStorageValue;
  StorageRoot: TGocciaTempRoot;
begin
  Storage := TGocciaAsyncLocalStorageValue.Create(FAsyncLocalStoragePrototype);
  Storage.DefaultValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  Storage.StorageName := UNNAMED_STORAGE;

  { Reading the options runs guest code when a member is an accessor, so the
    instance has to be rooted for the duration. }
  InitializeTempRoot(StorageRoot);
  AddTempRootIfNeeded(StorageRoot, Storage);
  try
    if AArgs.GetElement(0) is TGocciaObjectValue then
    begin
      Options := TGocciaObjectValue(AArgs.GetElement(0));
      if Options.HasProperty(PROP_DEFAULT_VALUE) then
        Storage.DefaultValue := Options.GetProperty(PROP_DEFAULT_VALUE);
      if Options.HasProperty(PROP_NAME) then
      begin
        NameValue := Options.GetProperty(PROP_NAME);
        if Assigned(NameValue) and
           not (NameValue is TGocciaUndefinedLiteralValue) then
          Storage.StorageName := NameValue.ToStringLiteral.Value;
      end;
    end;
  finally
    RemoveTempRootIfNeeded(StorageRoot);
  end;

  Result := Storage;
end;

{ Node: AsyncLocalStorage.prototype.run(store, callback[, ...args]).

  The store is bound for the synchronous part of the callback, and for every
  continuation created while it runs — those capture the derived snapshot at
  the point they are created and carry it themselves. }
function TGocciaAsyncHooksNamespaceHost.StorageRun(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  RUN_FIRST_EXTRA_ARGUMENT = 2;
var
  Storage: TGocciaAsyncLocalStorageValue;
begin
  Storage := RequireStorage(AThisValue);
  Result := CallInContext(AArgs.GetElement(1),
    TGocciaUndefinedLiteralValue.UndefinedValue, AArgs,
    RUN_FIRST_EXTRA_ARGUMENT,
    DeriveAsyncContext(CurrentAsyncContext, Storage, AArgs.GetElement(0)));
end;

{ Node: AsyncLocalStorage.prototype.getStore().

  Purely a lookup in the current frame: a binding wins even when its value is
  undefined (`run(undefined, ...)` reports undefined, not the default), and the
  default value stands in only when there is no binding at all. There is no
  per-instance enabled flag to consult — see StorageDisable. }
function TGocciaAsyncHooksNamespaceHost.StorageGetStore(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Snapshot: TGocciaAsyncContextSnapshot;
  Storage: TGocciaAsyncLocalStorageValue;
  Store: TGocciaValue;
begin
  Storage := RequireStorage(AThisValue);
  Snapshot := CurrentAsyncContext;
  if Assigned(Snapshot) and Snapshot.TryGetStore(Storage, Store) then
    Result := Store
  else
    Result := Storage.DefaultValue;
end;

{ Node: AsyncLocalStorage.prototype.enterWith(store). Unlike run there is no
  scope to leave, so the binding survives until whatever installed the current
  context restores it — the enclosing run, the enclosing microtask, or nothing
  at all at the top level. }
function TGocciaAsyncHooksNamespaceHost.StorageEnterWith(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Storage: TGocciaAsyncLocalStorageValue;
begin
  Storage := RequireStorage(AThisValue);
  SetCurrentAsyncContext(DeriveAsyncContext(CurrentAsyncContext, Storage,
    AArgs.GetElement(0)));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ Node: AsyncLocalStorage.prototype.exit(callback[, ...args]). Probed against
  Node v24.0.1: exit binds undefined rather than dropping the binding, so
  getStore reports undefined inside the callback even when the instance was
  constructed with a defaultValue. }
function TGocciaAsyncHooksNamespaceHost.StorageExit(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  EXIT_FIRST_EXTRA_ARGUMENT = 1;
var
  Storage: TGocciaAsyncLocalStorageValue;
begin
  Storage := RequireStorage(AThisValue);
  Result := CallInContext(AArgs.GetElement(0),
    TGocciaUndefinedLiteralValue.UndefinedValue, AArgs,
    EXIT_FIRST_EXTRA_ARGUMENT,
    DeriveAsyncContext(CurrentAsyncContext, Storage,
      TGocciaUndefinedLiteralValue.UndefinedValue));
end;

{ Node: AsyncLocalStorage.prototype.disable().

  Deletes the binding from the CURRENT frame and nothing more. Node v24 is the
  AsyncContextFrame model, and every observable consequence follows from that
  one edit: getStore reports the default value afterwards because no binding is
  left here; a run or enterWith re-binds; and a continuation captured before
  the disable still reports the store IT captured, because disable never
  reached that frame.

  An earlier per-instance `disabled` flag reproduced the first two and got the
  third wrong in both directions — it masked already-captured bindings, and it
  made exit() on a disabled instance report the default value where Node
  reports undefined. Both were probed against Node v24.0.1. }
function TGocciaAsyncHooksNamespaceHost.StorageDisable(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Storage: TGocciaAsyncLocalStorageValue;
begin
  Storage := RequireStorage(AThisValue);
  SetCurrentAsyncContext(
    DeriveAsyncContextWithout(CurrentAsyncContext, Storage));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaAsyncHooksNamespaceHost.StorageGetName(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    RequireStorage(AThisValue).StorageName);
end;

{ Node: AsyncLocalStorage.bind(fn) — binds fn to the current context. Node
  routes it through AsyncResource.bind, so the same validateFunction check
  rejects a non-callable here rather than at the first call. }
function TGocciaAsyncHooksNamespaceHost.StorageStaticBind(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireCallable(AArgs.GetElement(0));
  Result := CreateBoundFunction(AArgs.GetElement(0), nil,
    CurrentAsyncContext, False);
end;

{ Node: AsyncLocalStorage.snapshot() — returns (fn, ...args) => fn(...args)
  run under the context current at the time of the snapshot call. }
function TGocciaAsyncHooksNamespaceHost.StorageStaticSnapshot(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateBoundFunction(nil, nil, CurrentAsyncContext, True);
end;

function TGocciaAsyncHooksNamespaceHost.ResourceConstructor(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Resource: TGocciaAsyncResourceValue;
begin
  Resource := TGocciaAsyncResourceValue.Create(FAsyncResourcePrototype);
  Resource.Context := CurrentAsyncContext;
  Resource.AsyncId := FNextAsyncId;
  FNextAsyncId := FNextAsyncId + 1;
  Result := Resource;
end;

{ Node: asyncResource.runInAsyncScope(fn[, thisArg, ...args]).

  Unlike bind, an omitted thisArg here is NOT replaced by the call-site
  receiver — Node applies fn with whatever was passed, so an omitted one means
  `this` is undefined. GetElement already answers undefined for an absent
  index, which is that value. }
function TGocciaAsyncHooksNamespaceHost.ResourceRunInAsyncScope(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  RUN_IN_SCOPE_FIRST_EXTRA_ARGUMENT = 2;
var
  Resource: TGocciaAsyncResourceValue;
begin
  Resource := RequireResource(AThisValue);
  Result := CallInContext(AArgs.GetElement(0), AArgs.GetElement(1), AArgs,
    RUN_IN_SCOPE_FIRST_EXTRA_ARGUMENT, Resource.Context);
end;

{ Node: asyncResource.bind(fn[, thisArg]) — the resource's captured context.
  An undefined thisArg leaves the call-site receiver in place; see
  TGocciaAsyncBoundFunction.Invoke. }
function TGocciaAsyncHooksNamespaceHost.ResourceBind(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  BIND_THIS_ARGUMENT = 1;
begin
  RequireCallable(AArgs.GetElement(0));
  Result := CreateBoundFunction(AArgs.GetElement(0),
    ReceiverArgument(AArgs, BIND_THIS_ARGUMENT),
    RequireResource(AThisValue).Context, False);
end;

{ Node: AsyncResource.bind(fn[, type, thisArg]) — the context current at the
  call, which is what constructing a resource here and binding to it would
  capture anyway. }
function TGocciaAsyncHooksNamespaceHost.ResourceStaticBind(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  STATIC_BIND_THIS_ARGUMENT = 2;
begin
  RequireCallable(AArgs.GetElement(0));
  Result := CreateBoundFunction(AArgs.GetElement(0),
    ReceiverArgument(AArgs, STATIC_BIND_THIS_ARGUMENT), CurrentAsyncContext,
    False);
end;

function TGocciaAsyncHooksNamespaceHost.ResourceAsyncId(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(
    RequireResource(AThisValue).AsyncId);
end;

{ GocciaScript has no async-hooks callback surface and therefore no notion of
  the resource that triggered another, so a resource reports itself. }
function TGocciaAsyncHooksNamespaceHost.ResourceTriggerAsyncId(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(
    RequireResource(AThisValue).AsyncId);
end;

{ There are no destroy hooks to emit to; Node returns the resource so the call
  chains, and so does this. }
function TGocciaAsyncHooksNamespaceHost.ResourceEmitDestroy(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireResource(AThisValue);
end;

function CreateAsyncHooksNamespace(out AHostToken: TObject): TGocciaObjectValue;
var
  DefaultExport, NamespaceObject: TGocciaObjectValue;
  Host: TGocciaAsyncHooksNamespaceHost;
  Members: TGocciaMemberCollection;
  MemberDefinitions: TArray<TGocciaMemberDefinition>;
  StorageConstructorValue, ResourceConstructorValue: TGocciaNativeFunctionValue;
begin
  Host := TGocciaAsyncHooksNamespaceHost.Create;
  if not Assigned(GAsyncHooksHosts) then
    GAsyncHooksHosts := TGocciaAsyncHooksHostList.Create(True);
  GAsyncHooksHosts.Add(Host);
  AHostToken := Host;

  Host.FAsyncLocalStoragePrototype := TGocciaObjectValue.Create(
    TGocciaObjectValue.SharedObjectPrototype);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('run', Host.StorageRun, 2);
    Members.AddNamedMethod('getStore', Host.StorageGetStore, 0);
    Members.AddNamedMethod('enterWith', Host.StorageEnterWith, 1);
    Members.AddNamedMethod('exit', Host.StorageExit, 1);
    Members.AddNamedMethod('disable', Host.StorageDisable, 0);
    Members.AddAccessor(PROP_NAME, Host.StorageGetName, nil,
      [pfConfigurable]);
    MemberDefinitions := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Host.FAsyncLocalStoragePrototype,
    MemberDefinitions);

  Host.FAsyncResourcePrototype := TGocciaObjectValue.Create(
    TGocciaObjectValue.SharedObjectPrototype);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('runInAsyncScope', Host.ResourceRunInAsyncScope, 1);
    Members.AddNamedMethod('bind', Host.ResourceBind, 1);
    Members.AddNamedMethod('asyncId', Host.ResourceAsyncId, 0);
    Members.AddNamedMethod('triggerAsyncId', Host.ResourceTriggerAsyncId, 0);
    Members.AddNamedMethod('emitDestroy', Host.ResourceEmitDestroy, 0);
    MemberDefinitions := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Host.FAsyncResourcePrototype, MemberDefinitions);

  StorageConstructorValue := TGocciaNativeFunctionValue.Create(
    Host.StorageConstructor, ASYNC_LOCAL_STORAGE_NAME, 0);
  StorageConstructorValue.AssignProperty(PROP_PROTOTYPE,
    Host.FAsyncLocalStoragePrototype);
  Host.FAsyncLocalStoragePrototype.AssignProperty(PROP_CONSTRUCTOR,
    StorageConstructorValue);

  ResourceConstructorValue := TGocciaNativeFunctionValue.Create(
    Host.ResourceConstructor, ASYNC_RESOURCE_NAME, 1);
  ResourceConstructorValue.AssignProperty(PROP_PROTOTYPE,
    Host.FAsyncResourcePrototype);
  Host.FAsyncResourcePrototype.AssignProperty(PROP_CONSTRUCTOR,
    ResourceConstructorValue);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('bind', Host.StorageStaticBind, 1,
      gmkStaticMethod);
    Members.AddNamedMethod('snapshot', Host.StorageStaticSnapshot, 0,
      gmkStaticMethod);
    MemberDefinitions := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(StorageConstructorValue, MemberDefinitions);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('bind', Host.ResourceStaticBind, 1,
      gmkStaticMethod);
    MemberDefinitions := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(ResourceConstructorValue, MemberDefinitions);

  NamespaceObject := TGocciaObjectValue.Create(
    TGocciaObjectValue.SharedObjectPrototype);
  NamespaceObject.AssignProperty(ASYNC_LOCAL_STORAGE_NAME,
    StorageConstructorValue);
  NamespaceObject.AssignProperty(ASYNC_RESOURCE_NAME,
    ResourceConstructorValue);

  { Node's CommonJS module object is the default export of `node:async_hooks`
    under ESM, so `import async_hooks from "node:async_hooks"` reaches the same
    two constructors as the named imports. }
  DefaultExport := TGocciaObjectValue.Create(
    TGocciaObjectValue.SharedObjectPrototype);
  DefaultExport.AssignProperty(ASYNC_LOCAL_STORAGE_NAME,
    StorageConstructorValue);
  DefaultExport.AssignProperty(ASYNC_RESOURCE_NAME, ResourceConstructorValue);
  NamespaceObject.AssignProperty(KEYWORD_DEFAULT, DefaultExport);

  Result := NamespaceObject;
end;

{ Drops one namespace's host. The list owns its entries, so Remove frees it;
  an unknown or already-released token is ignored so a double detach is safe. }
procedure ReleaseAsyncHooksHost(const AHostToken: TObject);
begin
  if not (Assigned(AHostToken) and Assigned(GAsyncHooksHosts)) then
    Exit;
  GAsyncHooksHosts.Remove(AHostToken);
end;

{ Thread teardown: releases whatever survived, for a host that never detached. }
procedure ClearAsyncHooksHosts;
begin
  FreeAndNil(GAsyncHooksHosts);
end;

initialization
  RegisterThreadvarCleanup(ClearAsyncHooksHosts);

end.
