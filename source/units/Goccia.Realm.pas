unit Goccia.Realm;

{$I Goccia.inc}

// Per-engine realm storage for built-in intrinsic objects (e.g. Array.prototype,
// Object.prototype, Map.prototype, ...).  These objects are mutable from JS:
// userland code can install or delete properties on them, and ECMAScript spec
// requires that mutations on one realm do not leak into another realm.
//
// Historically the GocciaScript engine cached these prototype objects in
// unit-level threadvars, which meant the prototype state survived engine
// teardown and re-init.  test262 conformance tests therefore tried to undo
// every mutation through a JS harness (see scripts/test262_harness/
// prototypeIsolation.js); that harness cannot reverse non-configurable
// property additions and does not cover every intrinsic.
//
// TGocciaRealm holds the mutable per-realm state explicitly.  TGocciaEngine
// owns one TGocciaRealm at a time and replaces it via TGocciaEngine.ResetRealm
// to produce a fresh realm without rebuilding the engine.  See issue #403.
//
// Slot model
// ----------
// Each value unit that needs per-realm state registers a slot id at unit
// initialization via RegisterRealmSlot.  At runtime the unit reads / writes
// CurrentRealm.GetSlot(SlotId) / .SetSlot(SlotId, Value).  The realm tracks
// every TGCManagedObject ever stored in a slot and unpins them all when it is
// freed, so callers do not need to pair PinObject / UnpinObject across the
// realm boundary themselves.
//
// CurrentRealm is a thread-local pointer that the engine assigns on
// Initialize and ResetRealm.  It is never nil-and-rebuilt mid-flight: we are
// not relying on lazy reinitialisation of cleared threadvars to produce a
// fresh realm.  Replacing the pointer simply tells the worker thread which
// realm now owns the intrinsics it should look up.

interface

uses
  Goccia.GarbageCollector;

type
  TGocciaRealmSlotId = type Integer;
  TGocciaRealmOwnedSlotId = type Integer;

  TGocciaRealm = class
  private
    FSlots: array of TGCManagedObject;
    // Tracks every object ever stored in a slot so the realm can unpin them
    // wholesale on Free.  We keep history (not just the current slot value)
    // because some units register multiple distinct objects per slot over
    // their lifetime (e.g. lazy method-host swap).
    FPinned: array of TGCManagedObject;
    FPinnedCount: Integer;
    // Owned plain TObjects (not GC-managed).  The realm calls .Free on each
    // of them on destruction.  Used by helper wrappers like
    // TGocciaSharedPrototype that bundle a per-realm prototype with bookkeeping
    // and need their lifetime tied to the realm.
    FOwnedSlots: array of TObject;
    procedure RememberPin(const AObject: TGCManagedObject);
  public
    constructor Create;
    destructor Destroy; override;

    function GetSlot(const ASlotId: TGocciaRealmSlotId): TGCManagedObject;
    procedure SetSlot(const ASlotId: TGocciaRealmSlotId; const AValue: TGCManagedObject);
    function HasSlot(const ASlotId: TGocciaRealmSlotId): Boolean;

    function GetOwnedSlot(const ASlotId: TGocciaRealmOwnedSlotId): TObject;
    procedure SetOwnedSlot(const ASlotId: TGocciaRealmOwnedSlotId; const AValue: TObject);
  end;

// Allocates a new slot id.  Call from the initialization section of a unit
// that owns per-realm state and store the result in a unit-local constant.
function RegisterRealmSlot(const AName: string): TGocciaRealmSlotId;

// Allocates a new owned-object slot id.  The realm calls .Free on the stored
// object when the realm is destroyed.  Use for plain TObject helpers (e.g.
// TGocciaSharedPrototype wrappers) that aren't GC-managed but need
// realm-bounded lifetime.
function RegisterRealmOwnedSlot(const AName: string): TGocciaRealmOwnedSlotId;

// Returns the realm currently active on this worker thread, or nil if no
// engine is initialized.  Production code paths that rely on intrinsics
// (value constructors, ExposePrototype, InitializePrototype, ...) require a
// realm to be present.
function CurrentRealm: TGocciaRealm; inline;

// Set by TGocciaEngine.Initialize and TGocciaEngine.ResetRealm.  Tests and
// other host code generally should not call this directly.
procedure SetCurrentRealm(const ARealm: TGocciaRealm);

implementation

uses
  SysUtils;

threadvar
  GCurrentRealm: TGocciaRealm;

var
  GSlotCount: Integer;
  GSlotNames: array of string;
  GOwnedSlotCount: Integer;
  GOwnedSlotNames: array of string;
  GSlotLock: TRTLCriticalSection;

function RegisterRealmSlot(const AName: string): TGocciaRealmSlotId;
begin
  EnterCriticalSection(GSlotLock);
  try
    Result := GSlotCount;
    Inc(GSlotCount);
    SetLength(GSlotNames, GSlotCount);
    GSlotNames[Result] := AName;
  finally
    LeaveCriticalSection(GSlotLock);
  end;
end;

function RegisterRealmOwnedSlot(const AName: string): TGocciaRealmOwnedSlotId;
begin
  EnterCriticalSection(GSlotLock);
  try
    Result := GOwnedSlotCount;
    Inc(GOwnedSlotCount);
    SetLength(GOwnedSlotNames, GOwnedSlotCount);
    GOwnedSlotNames[Result] := AName;
  finally
    LeaveCriticalSection(GSlotLock);
  end;
end;

function CurrentRealm: TGocciaRealm;
begin
  Result := GCurrentRealm;
end;

procedure SetCurrentRealm(const ARealm: TGocciaRealm);
begin
  GCurrentRealm := ARealm;
end;

{ TGocciaRealm }

constructor TGocciaRealm.Create;
begin
  inherited Create;
  // Slot count is monotonic; size the array to the count seen so far.  Slots
  // registered after this realm exists will trigger a grow on first GetSlot /
  // SetSlot (see HasSlot below); in practice all slots are registered at unit
  // init time, before any realm is created, so the grow path is rare.
  SetLength(FSlots, GSlotCount);
  SetLength(FOwnedSlots, GOwnedSlotCount);
  FPinnedCount := 0;
end;

destructor TGocciaRealm.Destroy;
var
  I: Integer;
begin
  // Free the plain TObjects this realm owns first.  Their destructors may need
  // to ask the GC to unpin objects they pinned (e.g. TGocciaSharedPrototype),
  // which must happen before our own GC unpinning loop runs - otherwise an
  // unpin called from a destructor after we've cleared FPinned would be a
  // double-free in spirit.  This ordering is also why owned-slot tear-down
  // happens before pinned-slot tear-down.
  for I := 0 to High(FOwnedSlots) do
  begin
    if Assigned(FOwnedSlots[I]) then
    begin
      FOwnedSlots[I].Free;
      FOwnedSlots[I] := nil;
    end;
  end;
  FOwnedSlots := nil;

  // Unpin everything this realm asked the GC to keep alive.  After this the
  // GC is free to collect the prototype graph from this realm.
  if Assigned(TGarbageCollector.Instance) then
  begin
    for I := 0 to FPinnedCount - 1 do
      TGarbageCollector.Instance.UnpinObject(FPinned[I]);
  end;
  FPinned := nil;
  FSlots := nil;
  inherited;
end;

procedure TGocciaRealm.RememberPin(const AObject: TGCManagedObject);
begin
  if not Assigned(AObject) then
    Exit;
  if FPinnedCount = Length(FPinned) then
  begin
    if Length(FPinned) = 0 then
      SetLength(FPinned, 16)
    else
      SetLength(FPinned, Length(FPinned) * 2);
  end;
  FPinned[FPinnedCount] := AObject;
  Inc(FPinnedCount);
end;

function TGocciaRealm.HasSlot(const ASlotId: TGocciaRealmSlotId): Boolean;
begin
  Result := (ASlotId >= 0) and (ASlotId < Length(FSlots))
    and Assigned(FSlots[ASlotId]);
end;

function TGocciaRealm.GetSlot(const ASlotId: TGocciaRealmSlotId): TGCManagedObject;
begin
  if (ASlotId >= 0) and (ASlotId < Length(FSlots)) then
    Result := FSlots[ASlotId]
  else
    Result := nil;
end;

procedure TGocciaRealm.SetSlot(const ASlotId: TGocciaRealmSlotId; const AValue: TGCManagedObject);
begin
  if ASlotId < 0 then
    raise Exception.CreateFmt('TGocciaRealm.SetSlot: invalid slot id %d', [ASlotId]);

  // Slot registered after this realm was constructed - grow the storage.
  if ASlotId >= Length(FSlots) then
    SetLength(FSlots, ASlotId + 1);

  FSlots[ASlotId] := AValue;
  if Assigned(AValue) and Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(AValue);
    RememberPin(AValue);
  end;
end;

function TGocciaRealm.GetOwnedSlot(const ASlotId: TGocciaRealmOwnedSlotId): TObject;
begin
  if (ASlotId >= 0) and (ASlotId < Length(FOwnedSlots)) then
    Result := FOwnedSlots[ASlotId]
  else
    Result := nil;
end;

procedure TGocciaRealm.SetOwnedSlot(const ASlotId: TGocciaRealmOwnedSlotId; const AValue: TObject);
var
  Existing: TObject;
begin
  if ASlotId < 0 then
    raise Exception.CreateFmt('TGocciaRealm.SetOwnedSlot: invalid slot id %d', [ASlotId]);

  // Owned slot registered after this realm was constructed - grow.
  if ASlotId >= Length(FOwnedSlots) then
    SetLength(FOwnedSlots, ASlotId + 1);

  Existing := FOwnedSlots[ASlotId];
  if Existing = AValue then
    Exit;

  FOwnedSlots[ASlotId] := AValue;

  // Realm owns the lifetime of any previous slot value - free it now.  Callers
  // should not reuse the old reference after a SetOwnedSlot replacement.
  if Assigned(Existing) then
    Existing.Free;
end;

initialization
  InitCriticalSection(GSlotLock);
  GSlotCount := 0;
  GOwnedSlotCount := 0;

finalization
  DoneCriticalSection(GSlotLock);

end.
