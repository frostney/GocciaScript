unit Goccia.Values.Shape;

{$I Goccia.inc}

// Shape-lite: per-realm interned identities for object property layouts.
//
// A shape records the sequence of string property keys appended to a
// property map — keys only, no prototype pointer, no attribute flags, no
// values. Two maps with the same shape pointer store the same key at the
// same entry index, which is the invariant the VM's shape-validated inline
// caches read through. The invariant holds because shaped maps are
// append-only: the first delete or clear flips the map to dictionary mode
// (a process-wide sentinel shape that never matches a cached shape), and
// entry compaction only ever runs on maps that already left shaped mode.
//
// Shapes are interned per realm (TGocciaShapeTable in a realm-owned slot):
// no cross-thread locking, freed at engine tear-down with the realm.
// Property maps remember the realm pointer and non-reused identity token
// they were created under; if another realm asks to materialize a shape for
// that map, the map leaves shaped mode before any foreign shape table can
// receive its layout. Within an engine's lifetime shapes are never freed,
// and function templates never outlive their engine, so cache entries may
// validate by raw pointer identity without a version stamp.

interface

uses
  Generics.Collections,

  OrderedStringMap,

  Goccia.Realm,
  Goccia.Values.ObjectPropertyDescriptor;

const
  // Objects appending more string keys than this keep a PREFIX shape: the
  // first SHAPE_TRANSITION_DEPTH_LIMIT keys stay cacheable (entry indices
  // below the shape's depth remain valid), only the suffix goes uncached.
  SHAPE_TRANSITION_DEPTH_LIMIT = 64;
  // Ceiling on interned shapes per realm; programs whose dynamic keys
  // explode the transition tree degrade gracefully — layouts keep their
  // longest already-interned prefix instead of losing caching entirely.
  // Both saturation kinds are counted by the profiler (--profile=opcodes,
  // "Shape Saturation" section) because they otherwise degrade silently.
  SHAPE_TABLE_CAPACITY_LIMIT = 32768;

type
  TGocciaShapeTable = class;

  TGocciaShape = class
  private
    FTable: TGocciaShapeTable;
    FParent: TGocciaShape;
    FKey: string;
    FDepth: Integer;
    // Most shapes have exactly one successor (objects built in a fixed key
    // order), so the first child is held inline and matched by string
    // compare — pointer-equal for constant-pool keys — before any hashing.
    // FChildren is only allocated for the second distinct successor.
    FSoleChildKey: string;
    FSoleChild: TGocciaShape;
    FChildren: TOrderedStringMap<TGocciaShape>;
  public
    constructor Create(const ATable: TGocciaShapeTable;
      const AParent: TGocciaShape; const AKey: string);
    destructor Destroy; override;
    // Interned child shape for appending AKey; nil when a transition limit
    // is reached (the caller flips to dictionary mode).
    function Transition(const AKey: string): TGocciaShape;
    property Parent: TGocciaShape read FParent;
    property Key: string read FKey;
    property Depth: Integer read FDepth;
  end;

  TGocciaShapeTable = class
  private
    FRootShape: TGocciaShape;
    FShapes: TObjectList<TGocciaShape>;
    function CreateShape(const AParent: TGocciaShape;
      const AKey: string): TGocciaShape;
  public
    constructor Create;
    destructor Destroy; override;
    property RootShape: TGocciaShape read FRootShape;
  end;

  // The Decision-A mutation funnel, computed lazily: shapes are derived
  // from the map's own entry sequence on demand (EnsureShape), so property
  // appends pay nothing — no hook, no transition, no realm lookup at
  // object construction. Staleness is benign by construction: while a map
  // is append-only, an earlier shape is a true prefix description of the
  // layout, so a stale Shape read by the cache hit path can only produce a
  // miss, never a wrong entry index. Only Remove and Clear — the
  // operations that break the append-only invariant — are intercepted,
  // and they live on the map itself so no mutation path can bypass them.
  TGocciaShapedPropertyMap = class(TGocciaPropertyMap)
  private
    FOwnerRealm: TGocciaRealm;
    FOwnerRealmIdentity: TGocciaRealmIdentity;
    FShape: TGocciaShape;
    FShapeEntryCount: Integer;
    function OwnerRealmIsCurrent: Boolean; {$IFDEF FPC}inline;{$ENDIF}
  public
    constructor Create; overload;
    constructor Create(AInitialCapacity: Integer); overload;
    function Remove(const AKey: string): Boolean; override;
    procedure Clear; override;
    // Recompute the shape to cover all current entries (resuming from the
    // stale prefix shape, so each appended key is transitioned exactly
    // once per map). Returns nil for an empty layout and the dictionary
    // sentinel for maps that left shaped mode; cache fills reject both.
    // When a transition limit is hit the longest interned prefix is kept
    // instead: the result's Depth is then smaller than Count, so callers
    // proving PRESENCE must guard EntryIndex < Depth and callers proving
    // ABSENCE must require Depth = Count. Maps owned by another realm switch
    // to dictionary mode here, keeping foreign realm shape tables and raw
    // pointer cache identities separated without adding a cache-hit check.
    function EnsureShape: TGocciaShape;
    // Raw last-computed shape for the cache hit path: one field load, may
    // lag behind the live layout (benign — see unit header).
    property Shape: TGocciaShape read FShape;
  end;

// Process-wide immutable sentinel compared by identity only; never part of
// any realm's transition tree.
function DictionaryShapeSentinel: TGocciaShape; {$IFDEF FPC}inline;{$ENDIF}

// The current realm's shape table, created on first use through the
// realm-owned slot. nil when no realm is current (engine bootstrap edges);
// callers degrade to dictionary mode.
function CurrentRealmShapeTable: TGocciaShapeTable;

implementation

uses
  Goccia.Profiler;

var
  GShapeTableSlot: TGocciaRealmOwnedSlotId;
  GDictionaryShape: TGocciaShape;

function DictionaryShapeSentinel: TGocciaShape; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := GDictionaryShape;
end;

function CurrentRealmShapeTable: TGocciaShapeTable;
var
  Stored: TObject;
begin
  if (CurrentRealm = nil) then
    Exit(nil);
  Stored := CurrentRealm.GetOwnedSlot(GShapeTableSlot);
  if not Assigned(Stored) then
  begin
    Stored := TGocciaShapeTable.Create;
    CurrentRealm.SetOwnedSlot(GShapeTableSlot, Stored);
  end;
  Result := TGocciaShapeTable(Stored);
end;

{ TGocciaShape }

constructor TGocciaShape.Create(const ATable: TGocciaShapeTable;
  const AParent: TGocciaShape; const AKey: string);
begin
  inherited Create;
  FTable := ATable;
  FParent := AParent;
  FKey := AKey;
  if Assigned(AParent) then
    FDepth := AParent.FDepth + 1
  else
    FDepth := 0;
  FChildren := nil;
end;

destructor TGocciaShape.Destroy;
begin
  // Child shapes are owned by the table's shape list, not by FChildren.
  FChildren.Free;
  inherited;
end;

function TGocciaShape.Transition(const AKey: string): TGocciaShape;
begin
  if Assigned(FSoleChild) and (FSoleChildKey = AKey) then
    Exit(FSoleChild);
  if Assigned(FChildren) and FChildren.TryGetValue(AKey, Result) then
    Exit;

  if (not Assigned(FTable)) or
     (FDepth >= SHAPE_TRANSITION_DEPTH_LIMIT) or
     (FTable.FShapes.Count >= SHAPE_TABLE_CAPACITY_LIMIT) then
    Exit(nil);

  Result := FTable.CreateShape(Self, AKey);
  if not Assigned(FSoleChild) then
  begin
    FSoleChild := Result;
    FSoleChildKey := AKey;
  end
  else
  begin
    if not Assigned(FChildren) then
      FChildren := TOrderedStringMap<TGocciaShape>.Create;
    FChildren.Add(AKey, Result);
  end;
end;

{ TGocciaShapeTable }

constructor TGocciaShapeTable.Create;
begin
  inherited Create;
  FShapes := TObjectList<TGocciaShape>.Create(True);
  FRootShape := CreateShape(nil, '');
end;

destructor TGocciaShapeTable.Destroy;
begin
  FShapes.Free;
  inherited;
end;

function TGocciaShapeTable.CreateShape(const AParent: TGocciaShape;
  const AKey: string): TGocciaShape;
begin
  Result := TGocciaShape.Create(Self, AParent, AKey);
  FShapes.Add(Result);
end;

{ TGocciaShapedPropertyMap }

constructor TGocciaShapedPropertyMap.Create;
begin
  Create(0);
end;

constructor TGocciaShapedPropertyMap.Create(AInitialCapacity: Integer);
begin
  inherited Create(AInitialCapacity);
  FOwnerRealm := CurrentRealm;
  if Assigned(FOwnerRealm) then
    FOwnerRealmIdentity := FOwnerRealm.Identity
  else
    FOwnerRealmIdentity := 0;
  FShape := nil;
  FShapeEntryCount := 0;
end;

function TGocciaShapedPropertyMap.OwnerRealmIsCurrent: Boolean;
var
  ActiveRealm: TGocciaRealm;
begin
  ActiveRealm := CurrentRealm;
  Result := Assigned(ActiveRealm) and
    (ActiveRealm = FOwnerRealm) and
    (ActiveRealm.Identity = FOwnerRealmIdentity);
end;

function TGocciaShapedPropertyMap.EnsureShape: TGocciaShape;
var
  Table: TGocciaShapeTable;
  Walk, Next: TGocciaShape;
  Profiler: TGocciaProfiler;
  I: Integer;
begin
  if FShape = GDictionaryShape then
    Exit(GDictionaryShape);
  if not OwnerRealmIsCurrent then
  begin
    FShape := GDictionaryShape;
    Exit(GDictionaryShape);
  end;
  // Depth-capped maps are permanently in prefix mode; skip the staleness
  // walk (it could only fail at the cap again).
  if Assigned(FShape) and
     (FShape.Depth >= SHAPE_TRANSITION_DEPTH_LIMIT) then
    Exit(FShape);
  if FShapeEntryCount = CountFast then
    Exit(FShape);

  Walk := FShape;
  if not Assigned(Walk) then
  begin
    Table := CurrentRealmShapeTable;
    if not Assigned(Table) then
    begin
      FShape := GDictionaryShape;
      Exit(GDictionaryShape);
    end;
    Walk := Table.RootShape;
  end;

  // While shaped, the map is append-only: no tombstones, so entry indices
  // 0..Count-1 are exactly the appended keys in order, and the stale shape
  // covers the first FShapeEntryCount of them.
  for I := FShapeEntryCount to CountFast - 1 do
  begin
    Next := Walk.Transition(KeyAtEntry(I));
    if not Assigned(Next) then
    begin
      // Transition limit reached: keep the longest interned prefix. The
      // prefix is a true description of entries 0..Walk.Depth-1, so
      // Depth-guarded cache fills stay sound; entries beyond it (and
      // absence proofs, which need full coverage) go uncached. Table-cap
      // prefixes retry on later calls — already-interned transitions
      // remain usable even when the table cannot grow.
      Profiler := TGocciaProfiler.Instance;
      if Assigned(Profiler) and Profiler.Enabled then
      begin
        if Walk.Depth >= SHAPE_TRANSITION_DEPTH_LIMIT then
          Profiler.RecordShapeDepthLimit
        else
          Profiler.RecordShapeTableSaturation;
      end;
      FShape := Walk;
      FShapeEntryCount := I;
      Exit(Walk);
    end;
    Walk := Next;
  end;

  FShape := Walk;
  FShapeEntryCount := CountFast;
  Result := Walk;
end;

function TGocciaShapedPropertyMap.Remove(const AKey: string): Boolean;
begin
  Result := inherited Remove(AKey);
  if Result then
    FShape := GDictionaryShape;
end;

procedure TGocciaShapedPropertyMap.Clear;
begin
  inherited Clear;
  FShape := GDictionaryShape;
end;

initialization
  GShapeTableSlot := RegisterRealmOwnedSlot('ShapeTable');
  GDictionaryShape := TGocciaShape.Create(nil, nil, '');

finalization
  GDictionaryShape.Free;

end.
