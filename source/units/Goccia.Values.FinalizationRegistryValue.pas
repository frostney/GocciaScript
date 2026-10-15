unit Goccia.Values.FinalizationRegistryValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.MicrotaskQueue,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFinalizationRegistryCell = record
    Target: TGocciaValue;
    HeldValue: TGocciaValue;
    UnregisterToken: TGocciaValue;
    HasUnregisterToken: Boolean;
  end;

  TGocciaFinalizationRegistryValue = class(TGocciaInstanceValue)
  private
    FCleanupCallback: TGocciaValue;
    { [[Cells]] whose target is still alive. }
    FCells: TList<TGocciaFinalizationRegistryCell>;
    { [[Cells]] whose target was collected: [[WeakRefTarget]] is empty and the
      cleanup job that removes the cell has not run yet. They are still cells
      of the registry (ES2026 §9.9.3 empties the target and leaves the cell
      in place), so unregister must find them, and a cell it removes here
      never reaches the callback. Kept apart from FCells so a cleanup job
      takes the next one in O(1). }
    FPendingCells: TList<TGocciaFinalizationRegistryCell>;
    { The native function every cleanup job of this registry calls. Made on
      the first register, because a collection is no place to allocate. Its
      CapturedRoot is the registry, so a queued job keeps the registry and
      its pending held values alive until it runs. }
    FCleanupJob: TGocciaValue;
    { The microtask scope the registry was created in. A collection can run
      while a nested engine is executing, and the cleanup job still belongs to
      the engine that created the registry. }
    FOwnerScope: TGocciaMicrotaskScopeId;
    procedure EnqueueCleanup;
    function RunCleanupJob(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InitializePrototype;
  public
    function FinalizationRegistryRegister(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FinalizationRegistryUnregister(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    procedure SweepWeakReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class function GetSharedPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue;

    property CleanupCallback: TGocciaValue read FCleanupCallback;
  end;

implementation

uses
  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.WeakReferenceSupport;

var
  GFinalizationRegistrySharedSlot: TGocciaRealmOwnedSlotId;

function GetFinalizationRegistryShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GFinalizationRegistrySharedSlot))
  else
    Result := nil;
end;

constructor TGocciaFinalizationRegistryValue.Create(
  const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FCleanupCallback := nil;
  FCells := TList<TGocciaFinalizationRegistryCell>.Create;
  FPendingCells := TList<TGocciaFinalizationRegistryCell>.Create;
  FCleanupJob := nil;
  FOwnerScope := CurrentMicrotaskScope;
  InitializePrototype;
  Shared := GetFinalizationRegistryShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaFinalizationRegistryValue.Destroy;
begin
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.UnregisterWeakContainer(Self);
  FCells.Free;
  FPendingCells.Free;
  inherited;
end;

procedure TGocciaFinalizationRegistryValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetFinalizationRegistryShared <> nil) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GFinalizationRegistrySharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('register', FinalizationRegistryRegister, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
    Members.AddNamedMethod('unregister', FinalizationRegistryUnregister, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_FINALIZATION_REGISTRY),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaFinalizationRegistryValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetFinalizationRegistryShared;
  if not Assigned(Shared) then
  begin
    TGocciaFinalizationRegistryValue.Create;
    Shared := GetFinalizationRegistryShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

class function TGocciaFinalizationRegistryValue.GetSharedPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := GetSharedPrototypeFromOwnedSlot(ARealm,
    GFinalizationRegistrySharedSlot);
end;

function TGocciaFinalizationRegistryValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_FINALIZATION_REGISTRY;
end;

// ES2026 §26.2.1.1 FinalizationRegistry(cleanupCallback)
procedure TGocciaFinalizationRegistryValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
begin
  FCleanupCallback := AArguments.GetElement(0);
  if not Assigned(FCleanupCallback) or not FCleanupCallback.IsCallable then
    ThrowTypeError(SErrorFinalizationRegistryCleanupNotCallable,
      SSuggestCallbackRequired);
end;

procedure TGocciaFinalizationRegistryValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCleanupCallback) then
    FCleanupCallback.MarkReferences;
  if Assigned(FCleanupJob) then
    FCleanupJob.MarkReferences;
  for I := 0 to FCells.Count - 1 do
    if Assigned(FCells[I].HeldValue) then
      FCells[I].HeldValue.MarkReferences;
  for I := 0 to FPendingCells.Count - 1 do
    if Assigned(FPendingCells[I].HeldValue) then
      FPendingCells[I].HeldValue.MarkReferences;
end;

// One job per emptied cell, each running CleanupFinalizationRegistry for one
// cell (ES2026 §9.12 lets an implementation stop after any cell), so the
// microtasks a callback queues run before the next callback, as they did when
// each job called the callback directly.
procedure TGocciaFinalizationRegistryValue.EnqueueCleanup;
var
  Task: TGocciaMicrotask;
begin
  if (TGocciaMicrotaskQueue.Instance = nil) or not Assigned(FCleanupJob) then
    Exit;
  Task.Handler := FCleanupJob;
  Task.ResultPromise := nil;
  Task.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  Task.ReactionType := prtFulfill;
  TGocciaMicrotaskQueue.Instance.EnqueueFinalizationCleanup(Task,
    FOwnerScope);
end;

// ES2026 §9.12 CleanupFinalizationRegistry, for at most one cell. The cell may
// be gone already: unregister removes an emptied cell too, and then its job
// has nothing to do.
function TGocciaFinalizationRegistryValue.RunCleanupJob(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  HeldValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FPendingCells.Count = 0 then
    Exit;
  // Steps 3.a-3.b: Choose a cell whose target is empty and remove it.
  HeldValue := FPendingCells[0].HeldValue;
  FPendingCells.Delete(0);
  // Step 3.c: Perform ? HostCallJobCallback(callback, undefined, « heldValue »).
  CallArgs := TGocciaArgumentsCollection.Create([HeldValue]);
  try
    InvokeCallable(FCleanupCallback, CallArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallArgs.Free;
  end;
end;

procedure TGocciaFinalizationRegistryValue.SweepWeakReferences;
var
  I: Integer;
  Cell: TGocciaFinalizationRegistryCell;
begin
  for I := FCells.Count - 1 downto 0 do
  begin
    Cell := FCells[I];
    if Cell.HasUnregisterToken and Assigned(Cell.UnregisterToken) and
       not Cell.UnregisterToken.GCMarked then
    begin
      Cell.UnregisterToken := nil;
      Cell.HasUnregisterToken := False;
      FCells[I] := Cell;
    end;
    if Assigned(Cell.Target) and not Cell.Target.GCMarked then
    begin
      // ES2026 §9.9.3: set cell.[[WeakRefTarget]] to empty and enqueue the
      // cleanup job. The cell stays a cell of this registry until the job
      // removes it.
      Cell.Target := nil;
      FPendingCells.Add(Cell);
      FCells.Delete(I);
      EnqueueCleanup;
    end;
  end;

  // A pending cell's token is weak too, and unregister compares against it.
  for I := 0 to FPendingCells.Count - 1 do
  begin
    Cell := FPendingCells[I];
    if Cell.HasUnregisterToken and Assigned(Cell.UnregisterToken) and
       not Cell.UnregisterToken.GCMarked then
    begin
      Cell.UnregisterToken := nil;
      Cell.HasUnregisterToken := False;
      FPendingCells[I] := Cell;
    end;
  end;
end;

// ES2026 §26.2.3.2 FinalizationRegistry.prototype.register(target, heldValue [ , unregisterToken ])
function TGocciaFinalizationRegistryValue.FinalizationRegistryRegister(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Registry: TGocciaFinalizationRegistryValue;
  Target, HeldValue, UnregisterToken: TGocciaValue;
  Cell: TGocciaFinalizationRegistryCell;
  Job: TGocciaNativeFunctionValue;
begin
  if not (AThisValue is TGocciaFinalizationRegistryValue) then
    ThrowTypeError(SErrorFinalizationRegistryRegisterNonRegistry,
      SSuggestFinalizationRegistryThisType);

  Registry := TGocciaFinalizationRegistryValue(AThisValue);
  Target := AArgs.GetElement(0);
  RequireCanBeHeldWeakly(Target, 'FinalizationRegistry.prototype.register');
  HeldValue := AArgs.GetElement(1);
  if IsSameValue(Target, HeldValue) then
    ThrowTypeError(SErrorFinalizationRegistryTargetHeldValueSame,
      SSuggestFinalizationRegistryHeldValue);

  UnregisterToken := AArgs.GetElement(2);
  Cell.Target := Target;
  Cell.HeldValue := HeldValue;
  Cell.UnregisterToken := nil;
  Cell.HasUnregisterToken := False;

  if not (UnregisterToken is TGocciaUndefinedLiteralValue) then
  begin
    RequireCanBeHeldWeakly(UnregisterToken,
      'FinalizationRegistry.prototype.register');
    Cell.UnregisterToken := UnregisterToken;
    Cell.HasUnregisterToken := True;
  end;

  if not Assigned(Registry.FCleanupJob) then
  begin
    Job := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Registry.RunCleanupJob, '', 1);
    Job.CapturedRoot := Registry;
    Registry.FCleanupJob := Job;
  end;

  // Count this registry as a live weak container on its first cell.
  if (Registry.FCells.Count = 0) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.RegisterWeakContainer(Registry);
  Registry.FCells.Add(Cell);
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddKeptObject(Target);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §26.2.3.3 FinalizationRegistry.prototype.unregister(unregisterToken)
function TGocciaFinalizationRegistryValue.FinalizationRegistryUnregister(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Registry: TGocciaFinalizationRegistryValue;
  UnregisterToken: TGocciaValue;
  I: Integer;
  Removed: Boolean;
begin
  if not (AThisValue is TGocciaFinalizationRegistryValue) then
    ThrowTypeError(SErrorFinalizationRegistryUnregisterNonRegistry,
      SSuggestFinalizationRegistryThisType);

  Registry := TGocciaFinalizationRegistryValue(AThisValue);
  UnregisterToken := AArgs.GetElement(0);
  RequireCanBeHeldWeakly(UnregisterToken,
    'FinalizationRegistry.prototype.unregister');

  Removed := False;
  for I := Registry.FCells.Count - 1 downto 0 do
    if Registry.FCells[I].HasUnregisterToken and
       IsSameValue(Registry.FCells[I].UnregisterToken, UnregisterToken) then
    begin
      Registry.FCells.Delete(I);
      Removed := True;
    end;
  for I := Registry.FPendingCells.Count - 1 downto 0 do
    if Registry.FPendingCells[I].HasUnregisterToken and
       IsSameValue(Registry.FPendingCells[I].UnregisterToken,
         UnregisterToken) then
    begin
      Registry.FPendingCells.Delete(I);
      Removed := True;
    end;

  if Removed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

initialization
  GFinalizationRegistrySharedSlot :=
    RegisterRealmOwnedSlot('FinalizationRegistry.shared');

end.
