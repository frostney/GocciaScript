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
// Within an engine's lifetime shapes are never freed, and function
// templates never outlive their engine, so cache entries may validate by
// raw pointer identity without a version stamp.

interface

uses
  Generics.Collections,

  OrderedStringMap,

  Goccia.Realm,
  Goccia.Values.ObjectPropertyDescriptor;

const
  // Objects appending more string keys than this stop being shape-tracked
  // (dictionary-style usage); the transition chain would otherwise grow one
  // interned shape per key.
  SHAPE_TRANSITION_DEPTH_LIMIT = 64;
  // Hard ceiling on interned shapes per realm; programs whose dynamic keys
  // explode the transition tree degrade to dictionary mode instead of
  // growing realm memory without bound.
  SHAPE_TABLE_CAPACITY_LIMIT = 4096;

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

  // The Decision-A mutation funnel: every property-map write path
  // (DefineProperty, AssignProperty, freeze/seal rewrites, direct Add call
  // sites) reaches TOrderedStringMap.Add, so layout tracking lives on the
  // map itself and cannot be bypassed.
  TGocciaShapedPropertyMap = class(TGocciaPropertyMap)
  private
    FShape: TGocciaShape;
  protected
    procedure AfterNewEntryAdded(const AKey: string); override;
  public
    function Remove(const AKey: string): Boolean; override;
    procedure Clear; override;
    // nil = empty layout (no string keys yet); DictionaryShapeSentinel =
    // dictionary mode; anything else = interned shape of this realm.
    property Shape: TGocciaShape read FShape;
  end;

// Process-wide immutable sentinel compared by identity only; never part of
// any realm's transition tree.
function DictionaryShapeSentinel: TGocciaShape; inline;

// The current realm's shape table, created on first use through the
// realm-owned slot. nil when no realm is current (engine bootstrap edges);
// callers degrade to dictionary mode.
function CurrentRealmShapeTable: TGocciaShapeTable;

implementation

var
  GShapeTableSlot: TGocciaRealmOwnedSlotId;
  GDictionaryShape: TGocciaShape;

function DictionaryShapeSentinel: TGocciaShape;
begin
  Result := GDictionaryShape;
end;

function CurrentRealmShapeTable: TGocciaShapeTable;
var
  Stored: TObject;
begin
  if not Assigned(CurrentRealm) then
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

procedure TGocciaShapedPropertyMap.AfterNewEntryAdded(const AKey: string);
var
  Table: TGocciaShapeTable;
  NextShape: TGocciaShape;
begin
  if FShape = GDictionaryShape then
    Exit;

  if not Assigned(FShape) then
  begin
    Table := CurrentRealmShapeTable;
    if not Assigned(Table) then
    begin
      FShape := GDictionaryShape;
      Exit;
    end;
    NextShape := Table.RootShape.Transition(AKey);
  end
  else
    NextShape := FShape.Transition(AKey);

  if Assigned(NextShape) then
    FShape := NextShape
  else
    FShape := GDictionaryShape;
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
