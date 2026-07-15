unit Goccia.Values.FinalizationRegistryValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
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
    FCells: TList<TGocciaFinalizationRegistryCell>;
    procedure EnqueueCleanup(const AHeldValue: TGocciaValue);
    procedure InitializePrototype;
  public
    function FinalizationRegistryRegister(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FinalizationRegistryUnregister(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function ToStringTag: string; override;
    function BuiltinTagFallback: Boolean; override;

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
  Goccia.MicrotaskQueue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.WeakReferenceSupport;

var
  GFinalizationRegistrySharedSlot: TGocciaRealmOwnedSlotId;

function GetFinalizationRegistryShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
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
  InitializePrototype;
  Shared := GetFinalizationRegistryShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaFinalizationRegistryValue.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnregisterWeakContainer(Self);
  FCells.Free;
  inherited;
end;

procedure TGocciaFinalizationRegistryValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetFinalizationRegistryShared) then Exit;

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

function TGocciaFinalizationRegistryValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
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
  for I := 0 to FCells.Count - 1 do
    if Assigned(FCells[I].HeldValue) then
      FCells[I].HeldValue.MarkReferences;
end;

procedure TGocciaFinalizationRegistryValue.EnqueueCleanup(
  const AHeldValue: TGocciaValue);
var
  Task: TGocciaMicrotask;
begin
  if not Assigned(TGocciaMicrotaskQueue.Instance) then
    Exit;
  Task.Handler := FCleanupCallback;
  Task.ResultPromise := nil;
  Task.Value := AHeldValue;
  Task.ReactionType := prtFulfill;
  TGocciaMicrotaskQueue.Instance.EnqueueFinalizationCleanup(Task);
end;

procedure TGocciaFinalizationRegistryValue.SweepWeakReferences;
var
  I: Integer;
  Cell: TGocciaFinalizationRegistryCell;
begin
  for I := FCells.Count - 1 downto 0 do
  begin
    Cell := FCells[I];
    if Assigned(Cell.Target) and not Cell.Target.GCMarked then
    begin
      EnqueueCleanup(Cell.HeldValue);
      FCells.Delete(I);
    end
    else if Cell.HasUnregisterToken and Assigned(Cell.UnregisterToken) and
            not Cell.UnregisterToken.GCMarked then
    begin
      Cell.UnregisterToken := nil;
      Cell.HasUnregisterToken := False;
      FCells[I] := Cell;
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

  // Count this registry as a live weak container on its first cell.
  if (Registry.FCells.Count = 0) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RegisterWeakContainer(Registry);
  Registry.FCells.Add(Cell);
  if Assigned(TGarbageCollector.Instance) then
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

  if Removed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

initialization
  GFinalizationRegistrySharedSlot :=
    RegisterRealmOwnedSlot('FinalizationRegistry.shared');

end.
