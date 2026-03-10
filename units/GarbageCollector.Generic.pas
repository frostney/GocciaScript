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
  protected class var
    FInstance: TGenericGarbageCollector;
  private
    FManagedObjects: TGCManagedObjectList;
    FPinnedObjects: TDictionary<TGCManagedObject, Boolean>;
    FTempRoots: TDictionary<TGCManagedObject, Boolean>;

    FExternalRootMarkers: array of TGCRootMarker;

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
    class function Instance: TGenericGarbageCollector; inline;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    function AllocateObject(const AObject: TGCManagedObject): TGCManagedObject;
    procedure RegisterObject(const AObject: TGCManagedObject);
    procedure UnregisterObject(const AObject: TGCManagedObject);
    procedure PinObject(const AObject: TGCManagedObject);
    procedure UnpinObject(const AObject: TGCManagedObject);
    procedure AddTempRoot(const AObject: TGCManagedObject);
    procedure RemoveTempRoot(const AObject: TGCManagedObject);
    function IsTempRoot(const AObject: TGCManagedObject): Boolean;

    procedure SetExternalRootMarker(const AMarker: TGCRootMarker);
    procedure AddExternalRootMarker(const AMarker: TGCRootMarker);
    procedure RemoveExternalRootMarker(const AMarker: TGCRootMarker);

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

uses
  SysUtils;

{ TGenericGarbageCollector }

class function TGenericGarbageCollector.Instance: TGenericGarbageCollector;
begin
  Result := FInstance;
end;

class procedure TGenericGarbageCollector.Shutdown;
begin
  FreeAndNil(FInstance);
end;

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
  SetLength(FExternalRootMarkers, 0);
end;

destructor TGenericGarbageCollector.Destroy;
begin
  FManagedObjects.Free;
  FPinnedObjects.Free;
  FTempRoots.Free;
  inherited;
end;

function TGenericGarbageCollector.AllocateObject(
  const AObject: TGCManagedObject): TGCManagedObject;
begin
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
  Result := AObject;
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
  SetLength(FExternalRootMarkers, 0);
  if Assigned(AMarker) then
    AddExternalRootMarker(AMarker);
end;

procedure TGenericGarbageCollector.AddExternalRootMarker(
  const AMarker: TGCRootMarker);
var
  Len: Integer;
begin
  Len := Length(FExternalRootMarkers);
  SetLength(FExternalRootMarkers, Len + 1);
  FExternalRootMarkers[Len] := AMarker;
end;

procedure TGenericGarbageCollector.RemoveExternalRootMarker(
  const AMarker: TGCRootMarker);
var
  I, WriteIdx: Integer;
begin
  WriteIdx := 0;
  for I := 0 to Length(FExternalRootMarkers) - 1 do
  begin
    if (TMethod(FExternalRootMarkers[I]).Code <> TMethod(AMarker).Code) or
       (TMethod(FExternalRootMarkers[I]).Data <> TMethod(AMarker).Data) then
    begin
      FExternalRootMarkers[WriteIdx] := FExternalRootMarkers[I];
      Inc(WriteIdx);
    end;
  end;
  SetLength(FExternalRootMarkers, WriteIdx);
end;

procedure TGenericGarbageCollector.MarkRoots;
var
  Obj: TGCManagedObject;
  I: Integer;
begin
  for Obj in FPinnedObjects.Keys do
    Obj.MarkReferences;

  for Obj in FTempRoots.Keys do
    Obj.MarkReferences;

  for I := 0 to Length(FExternalRootMarkers) - 1 do
    FExternalRootMarkers[I]();
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
