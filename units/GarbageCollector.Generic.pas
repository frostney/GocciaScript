unit GarbageCollector.Generic;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  GarbageCollector.Managed;

type
  TGCManagedObjectList = TObjectList<TGCManagedObject>;
  TGCRootMarker = procedure of object;

  TGenericGarbageCollector = class
  private
    FManagedObjects: TGCManagedObjectList;
    FPinnedObjects: TDictionary<TGCManagedObject, Boolean>;
    FTempRoots: TDictionary<TGCManagedObject, Boolean>;

    FExternalRootMarker: TGCRootMarker;

    function GetManagedObjectCount: Integer;
  protected
    FAllocationsSinceLastGC: Integer;
    FGCThreshold: Integer;
    FEnabled: Boolean;
    FCollecting: Boolean;
    FTotalCollected: Int64;
    FTotalCollections: Integer;

    procedure ClearObjectMarks;
    procedure MarkRoots; virtual;
    procedure SweepObjects;
    procedure FreeAllManagedObjects;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterObject(const AObject: TGCManagedObject);
    procedure UnregisterObject(const AObject: TGCManagedObject);
    procedure PinObject(const AObject: TGCManagedObject);
    procedure UnpinObject(const AObject: TGCManagedObject);
    procedure AddTempRoot(const AObject: TGCManagedObject);
    procedure RemoveTempRoot(const AObject: TGCManagedObject);
    function IsTempRoot(const AObject: TGCManagedObject): Boolean;

    procedure SetExternalRootMarker(const AMarker: TGCRootMarker);

    procedure Collect; virtual;
    procedure CollectIfNeeded;

    property Enabled: Boolean read FEnabled write FEnabled;
    property Threshold: Integer read FGCThreshold write FGCThreshold;
    property TotalCollected: Int64 read FTotalCollected;
    property TotalCollections: Integer read FTotalCollections;
    property ManagedObjectCount: Integer read GetManagedObjectCount;
  end;

const
  DEFAULT_GC_THRESHOLD = 10000;

implementation

{ TGenericGarbageCollector }

constructor TGenericGarbageCollector.Create;
begin
  inherited Create;
  FManagedObjects := TGCManagedObjectList.Create(False);
  FPinnedObjects := TDictionary<TGCManagedObject, Boolean>.Create;
  FTempRoots := TDictionary<TGCManagedObject, Boolean>.Create;
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
  FExternalRootMarker := nil;
end;

destructor TGenericGarbageCollector.Destroy;
begin
  FManagedObjects.Free;
  FPinnedObjects.Free;
  FTempRoots.Free;
  inherited;
end;

procedure TGenericGarbageCollector.RegisterObject(
  const AObject: TGCManagedObject);
begin
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGenericGarbageCollector.UnregisterObject(
  const AObject: TGCManagedObject);
begin
  FManagedObjects.Remove(AObject);
end;

procedure TGenericGarbageCollector.PinObject(const AObject: TGCManagedObject);
begin
  if Assigned(AObject) and not FPinnedObjects.ContainsKey(AObject) then
    FPinnedObjects.Add(AObject, True);
end;

procedure TGenericGarbageCollector.UnpinObject(
  const AObject: TGCManagedObject);
begin
  FPinnedObjects.Remove(AObject);
end;

procedure TGenericGarbageCollector.AddTempRoot(
  const AObject: TGCManagedObject);
begin
  if Assigned(AObject) and not FTempRoots.ContainsKey(AObject) then
    FTempRoots.Add(AObject, True);
end;

procedure TGenericGarbageCollector.RemoveTempRoot(
  const AObject: TGCManagedObject);
begin
  FTempRoots.Remove(AObject);
end;

function TGenericGarbageCollector.IsTempRoot(
  const AObject: TGCManagedObject): Boolean;
begin
  Result := Assigned(AObject) and FTempRoots.ContainsKey(AObject);
end;

procedure TGenericGarbageCollector.SetExternalRootMarker(
  const AMarker: TGCRootMarker);
begin
  FExternalRootMarker := AMarker;
end;

procedure TGenericGarbageCollector.MarkRoots;
var
  Obj: TGCManagedObject;
begin
  for Obj in FPinnedObjects.Keys do
    Obj.MarkReferences;

  for Obj in FTempRoots.Keys do
    Obj.MarkReferences;

  if Assigned(FExternalRootMarker) then
    FExternalRootMarker();
end;

procedure TGenericGarbageCollector.SweepObjects;
var
  I, WriteIdx: Integer;
  Collected: Integer;
begin
  Collected := 0;
  WriteIdx := 0;

  for I := 0 to FManagedObjects.Count - 1 do
  begin
    if FManagedObjects[I].GCMarked then
    begin
      FManagedObjects[WriteIdx] := FManagedObjects[I];
      Inc(WriteIdx);
    end
    else
    begin
      FManagedObjects[I].Free;
      Inc(Collected);
    end;
  end;

  FManagedObjects.Count := WriteIdx;
  FTotalCollected := FTotalCollected + Collected;
end;

procedure TGenericGarbageCollector.FreeAllManagedObjects;
var
  I: Integer;
begin
  for I := 0 to FManagedObjects.Count - 1 do
    FManagedObjects[I].Free;
  FManagedObjects.Count := 0;
end;

procedure TGenericGarbageCollector.ClearObjectMarks;
var
  I: Integer;
begin
  for I := 0 to FManagedObjects.Count - 1 do
    FManagedObjects[I].GCMarked := False;
end;

procedure TGenericGarbageCollector.Collect;
begin
  if FCollecting then Exit;
  FCollecting := True;
  try
    ClearObjectMarks;
    MarkRoots;
    SweepObjects;
    FAllocationsSinceLastGC := 0;
    Inc(FTotalCollections);
  finally
    FCollecting := False;
  end;
end;

procedure TGenericGarbageCollector.CollectIfNeeded;
begin
  if FEnabled and (FAllocationsSinceLastGC >= FGCThreshold) and
    not FCollecting then
    Collect;
end;

function TGenericGarbageCollector.GetManagedObjectCount: Integer;
begin
  Result := FManagedObjects.Count;
end;

end.
