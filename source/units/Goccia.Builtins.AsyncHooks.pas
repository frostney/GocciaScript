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

function CreateAsyncHooksNamespace: TGocciaObjectValue;
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

type
  TGocciaAsyncHooksHostList = TObjectList<TObject>;

  { One AsyncLocalStorage instance. Identity is the snapshot key, so the
    instance object itself is what a snapshot binds a store to. }
  TGocciaAsyncLocalStorageValue = class(TGocciaObjectValue)
  private
    FDefaultValue: TGocciaValue;
    FDisabled: Boolean;
    FStorageName: string;
  public
    procedure MarkReferences; override;
    property DefaultValue: TGocciaValue read FDefaultValue write FDefaultValue;
    property Disabled: Boolean read FDisabled write FDisabled;
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
  I: Integer;
  PreviousContext: TGocciaAsyncContextSnapshot;
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

  Receiver := FBoundThis;
  if not Assigned(Receiver) then
    Receiver := AThisValue;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    if FUsesCallerTarget then
      for I := 1 to AArgs.Length - 1 do
        CallArgs.Add(AArgs.GetElement(I))
    else
      for I := 0 to AArgs.Length - 1 do
        CallArgs.Add(AArgs.GetElement(I));

    PreviousContext := CurrentAsyncContext;
    SetCurrentAsyncContext(FContext);
    try
      Result := DispatchCall(Target, CallArgs, Receiver);
    finally
      SetCurrentAsyncContext(PreviousContext);
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
  I: Integer;
  PreviousContext: TGocciaAsyncContextSnapshot;
begin
  { Install before anything else runs. AContext was derived by the caller and
    is reachable from nothing until it is the current context, so any
    allocation in between — building the argument collection, or the error
    object on the non-callable path — could collect it at a safe point. }
  PreviousContext := CurrentAsyncContext;
  SetCurrentAsyncContext(AContext);
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
    SetCurrentAsyncContext(PreviousContext);
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
    BoundFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Bound.Invoke, '', 0);
    BoundFunction.CapturedRoot := Bound;
    Result := BoundFunction;
  finally
    RemoveTempRootIfNeeded(BoundRoot);
  end;
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
  { Node re-enables a disabled instance on run and on enterWith. }
  Storage.Disabled := False;
  Result := CallInContext(AArgs.GetElement(1),
    TGocciaUndefinedLiteralValue.UndefinedValue, AArgs,
    RUN_FIRST_EXTRA_ARGUMENT,
    DeriveAsyncContext(CurrentAsyncContext, Storage, AArgs.GetElement(0)));
end;

{ Node: AsyncLocalStorage.prototype.getStore(). A bound store wins even when it
  is undefined — `run(undefined, ...)` reports undefined, not the default. }
function TGocciaAsyncHooksNamespaceHost.StorageGetStore(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Snapshot: TGocciaAsyncContextSnapshot;
  Storage: TGocciaAsyncLocalStorageValue;
  Store: TGocciaValue;
begin
  Storage := RequireStorage(AThisValue);
  if Storage.Disabled then
    Exit(Storage.DefaultValue);

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
  Storage.Disabled := False;
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

{ Node: AsyncLocalStorage.prototype.disable(). getStore reports the default
  value until run or enterWith re-enables the instance. }
function TGocciaAsyncHooksNamespaceHost.StorageDisable(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Storage: TGocciaAsyncLocalStorageValue;
begin
  Storage := RequireStorage(AThisValue);
  Storage.Disabled := True;
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

{ Node: AsyncLocalStorage.bind(fn) — binds fn to the current context. }
function TGocciaAsyncHooksNamespaceHost.StorageStaticBind(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
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

{ Node: asyncResource.runInAsyncScope(fn[, thisArg, ...args]). }
function TGocciaAsyncHooksNamespaceHost.ResourceRunInAsyncScope(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  RUN_IN_SCOPE_FIRST_EXTRA_ARGUMENT = 2;
var
  Receiver: TGocciaValue;
  Resource: TGocciaAsyncResourceValue;
begin
  Resource := RequireResource(AThisValue);
  Receiver := AArgs.GetElement(1);
  if not Assigned(Receiver) then
    Receiver := TGocciaUndefinedLiteralValue.UndefinedValue;
  Result := CallInContext(AArgs.GetElement(0), Receiver, AArgs,
    RUN_IN_SCOPE_FIRST_EXTRA_ARGUMENT, Resource.Context);
end;

{ Node: asyncResource.bind(fn[, thisArg]) — the resource's captured context. }
function TGocciaAsyncHooksNamespaceHost.ResourceBind(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateBoundFunction(AArgs.GetElement(0), AArgs.GetElement(1),
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
  Result := CreateBoundFunction(AArgs.GetElement(0),
    AArgs.GetElement(STATIC_BIND_THIS_ARGUMENT), CurrentAsyncContext, False);
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

function CreateAsyncHooksNamespace: TGocciaObjectValue;
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

procedure ClearAsyncHooksHosts;
begin
  FreeAndNil(GAsyncHooksHosts);
end;

initialization
  RegisterThreadvarCleanup(ClearAsyncHooksHosts);

end.
