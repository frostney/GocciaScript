unit Goccia.Realm;

{$I Goccia.inc}

// ECMA-262 Realm Record storage.  The realm owns per-realm intrinsics
// (e.g. Array.prototype, Object.prototype, Map.prototype, ...), its global
// object/environment links, template object map, loaded-module host state, and
// host-defined payload.
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
// CurrentRealm is a compatibility facade over the active execution context.
// New execution paths should push/pop TGocciaExecutionContext records; the
// facade remains because many value units lookup realm-scoped intrinsics
// through this unit.

interface

uses
  Classes,

  Goccia.GarbageCollector;

type
  TGocciaRealmHostFinalize = procedure(const AHostDefined: TObject) of object;
  TGocciaRealmIdentity = type QWord;
  TGocciaRealmSlotId = type Integer;
  TGocciaRealmOwnedSlotId = type Integer;

  TGocciaRealmIntrinsics = class
  end;

  TGocciaRealm = class
  private
    FAgentSignifier: string;
    FGlobalObject: TGCManagedObject;
    FGlobalEnv: TGCManagedObject;
    FHostDefined: TObject;
    FHostFinalize: TGocciaRealmHostFinalize;
    FIdentity: TGocciaRealmIdentity;
    FIntrinsics: TGocciaRealmIntrinsics;
    FLoadedModules: TObject;
    FSlots: array of TGCManagedObject;
    FTemplateMap: TStringList;
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
    function GetTemplateMapCount: Integer;
  public
    constructor Create(const AAgentSignifier: string = '');
    destructor Destroy; override;

    function GetSlot(const ASlotId: TGocciaRealmSlotId): TGCManagedObject;
    procedure SetSlot(const ASlotId: TGocciaRealmSlotId; const AValue: TGCManagedObject);
    function HasSlot(const ASlotId: TGocciaRealmSlotId): Boolean;

    function GetOwnedSlot(const ASlotId: TGocciaRealmOwnedSlotId): TObject;
    procedure SetOwnedSlot(const ASlotId: TGocciaRealmOwnedSlotId; const AValue: TObject);

    function GetTemplateObject(const AKey: string): TGCManagedObject;
    procedure SetTemplateObject(const AKey: string; const AValue: TGCManagedObject);

    property AgentSignifier: string read FAgentSignifier write FAgentSignifier;
    property Intrinsics: TGocciaRealmIntrinsics read FIntrinsics;
    property GlobalObject: TGCManagedObject read FGlobalObject write FGlobalObject;
    property GlobalEnv: TGCManagedObject read FGlobalEnv write FGlobalEnv;
    property TemplateMapCount: Integer read GetTemplateMapCount;
    property LoadedModules: TObject read FLoadedModules write FLoadedModules;
    property HostDefined: TObject read FHostDefined write FHostDefined;
    property HostFinalize: TGocciaRealmHostFinalize read FHostFinalize write FHostFinalize;
    property Identity: TGocciaRealmIdentity read FIdentity;
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

procedure PushCurrentFunctionExecutionContext(const AScope: TObject;
  const AFunctionValue: TObject);
procedure PopCurrentFunctionExecutionContext;
function HasCurrentFunctionExecutionContext: Boolean;
function CurrentFunctionExecutionContextScope: TObject;
function CurrentFunctionExecutionContextValue: TObject;

implementation

uses
  SysUtils;

threadvar
  GCurrentRealm: TGocciaRealm;
  GCurrentFunctionContextStack: array of record
    Scope: TObject;
    FunctionValue: TObject;
  end;
  GCurrentFunctionContextStackCount: Integer;

var
  GSlotCount: Integer;
  GSlotNames: array of string;
  GOwnedSlotCount: Integer;
  GOwnedSlotNames: array of string;
  GRealmIdentityCounter: TGocciaRealmIdentity;
  GSlotLock: TRTLCriticalSection;

function NextRealmIdentity: TGocciaRealmIdentity;
begin
  EnterCriticalSection(GSlotLock);
  try
    Inc(GRealmIdentityCounter);
    Result := GRealmIdentityCounter;
  finally
    LeaveCriticalSection(GSlotLock);
  end;
end;

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

procedure PushCurrentFunctionExecutionContext(const AScope: TObject;
  const AFunctionValue: TObject);
begin
  if not Assigned(AFunctionValue) then
    raise Exception.Create('Function execution context requires a function value.');

  if GCurrentFunctionContextStackCount >=
     Length(GCurrentFunctionContextStack) then
    SetLength(GCurrentFunctionContextStack,
      GCurrentFunctionContextStackCount * 2 + 8);

  GCurrentFunctionContextStack[GCurrentFunctionContextStackCount].Scope :=
    AScope;
  GCurrentFunctionContextStack[GCurrentFunctionContextStackCount].FunctionValue :=
    AFunctionValue;
  Inc(GCurrentFunctionContextStackCount);
end;

procedure PopCurrentFunctionExecutionContext;
begin
  if GCurrentFunctionContextStackCount <= 0 then
    raise Exception.Create('Function execution context stack underflow.');

  Dec(GCurrentFunctionContextStackCount);
  FillChar(GCurrentFunctionContextStack[GCurrentFunctionContextStackCount],
    SizeOf(GCurrentFunctionContextStack[GCurrentFunctionContextStackCount]),
    0);
end;

function HasCurrentFunctionExecutionContext: Boolean;
begin
  Result := GCurrentFunctionContextStackCount > 0;
end;

function CurrentFunctionExecutionContextScope: TObject;
begin
  if GCurrentFunctionContextStackCount > 0 then
    Result :=
      GCurrentFunctionContextStack[GCurrentFunctionContextStackCount - 1].Scope
  else
    Result := nil;
end;

function CurrentFunctionExecutionContextValue: TObject;
begin
  if GCurrentFunctionContextStackCount > 0 then
    Result :=
      GCurrentFunctionContextStack[GCurrentFunctionContextStackCount - 1].FunctionValue
  else
    Result := nil;
end;

{ TGocciaRealm }

constructor TGocciaRealm.Create(const AAgentSignifier: string);
begin
  inherited Create;
  FAgentSignifier := AAgentSignifier;
  FIdentity := NextRealmIdentity;
  FIntrinsics := TGocciaRealmIntrinsics.Create;
  FTemplateMap := TStringList.Create;
  FTemplateMap.Sorted := True;
  FTemplateMap.Duplicates := dupError;
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
  if Assigned(FHostDefined) and Assigned(FHostFinalize) then
    FHostFinalize(FHostDefined);
  FHostDefined := nil;
  FLoadedModules := nil;
  FGlobalObject := nil;
  FGlobalEnv := nil;

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
  FTemplateMap.Free;
  FIntrinsics.Free;
  inherited;
end;

procedure TGocciaRealm.RememberPin(const AObject: TGCManagedObject);
var
  I: Integer;
begin
  if not Assigned(AObject) then
    Exit;
  // PinObject is set-based, so the same object pinned twice still occupies a
  // single set entry — but FPinned is append-only.  Dedupe defensively so
  // Destroy issues exactly one UnpinObject per pinned object: harmless under
  // current usage (Remove on a missing key is a no-op), but keeps the count
  // honest if a future caller lands a refcounted pin model.
  for I := 0 to FPinnedCount - 1 do
    if FPinned[I] = AObject then
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

function TGocciaRealm.GetTemplateMapCount: Integer;
begin
  Result := FTemplateMap.Count;
end;

function TGocciaRealm.GetTemplateObject(const AKey: string): TGCManagedObject;
var
  Index: Integer;
begin
  if FTemplateMap.Find(AKey, Index) then
    Result := TGCManagedObject(FTemplateMap.Objects[Index])
  else
    Result := nil;
end;

procedure TGocciaRealm.SetTemplateObject(const AKey: string;
  const AValue: TGCManagedObject);
var
  Index: Integer;
begin
  if AKey = '' then
    raise Exception.Create('TGocciaRealm.SetTemplateObject: key is empty.');
  if FTemplateMap.Find(AKey, Index) then
    FTemplateMap.Objects[Index] := AValue
  else
    FTemplateMap.AddObject(AKey, AValue);

  if Assigned(AValue) and Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(AValue);
    RememberPin(AValue);
  end;
end;

initialization
  InitCriticalSection(GSlotLock);
  GSlotCount := 0;
  GOwnedSlotCount := 0;
  GRealmIdentityCounter := 0;

finalization
  SetLength(GCurrentFunctionContextStack, 0);
  GCurrentFunctionContextStackCount := 0;
  DoneCriticalSection(GSlotLock);

end.
