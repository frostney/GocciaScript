unit Souffle.GarbageCollector;

{$I Souffle.inc}

interface

uses
  Generics.Collections,

  Souffle.Heap;

type
  TSouffleHeapObjectList = TObjectList<TSouffleHeapObject>;
  TSouffleGCRootMarker = procedure of object;

  TSouffleGarbageCollector = class
  private class var
    FInstance: TSouffleGarbageCollector;
  private
    FManagedObjects: TSouffleHeapObjectList;
    FPinnedObjects: TDictionary<TSouffleHeapObject, Boolean>;
    FTempRoots: TDictionary<TSouffleHeapObject, Boolean>;

    FAllocationsSinceLastGC: Integer;
    FGCThreshold: Integer;
    FEnabled: Boolean;
    FCollecting: Boolean;
    FTotalCollected: Int64;
    FTotalCollections: Integer;

    FExternalRootMarker: TSouffleGCRootMarker;

    procedure MarkPhase;
    procedure SweepPhase;
    function GetManagedObjectCount: Integer;
  public
    class function Instance: TSouffleGarbageCollector;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    function AllocateObject(const AObject: TSouffleHeapObject): TSouffleHeapObject;
    procedure PinObject(const AObject: TSouffleHeapObject);
    procedure UnpinObject(const AObject: TSouffleHeapObject);
    procedure AddTempRoot(const AObject: TSouffleHeapObject);
    procedure RemoveTempRoot(const AObject: TSouffleHeapObject);

    procedure SetExternalRootMarker(const AMarker: TSouffleGCRootMarker);

    procedure Collect;
    procedure CollectIfNeeded;

    property Enabled: Boolean read FEnabled write FEnabled;
    property Threshold: Integer read FGCThreshold write FGCThreshold;
    property TotalCollected: Int64 read FTotalCollected;
    property TotalCollections: Integer read FTotalCollections;
    property ManagedObjectCount: Integer read GetManagedObjectCount;
  end;

implementation

uses
  SysUtils;

const
  DEFAULT_GC_THRESHOLD = 10000;

{ TSouffleGarbageCollector }

class function TSouffleGarbageCollector.Instance: TSouffleGarbageCollector;
begin
  Result := FInstance;
end;

class procedure TSouffleGarbageCollector.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TSouffleGarbageCollector.Create;
end;

class procedure TSouffleGarbageCollector.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TSouffleGarbageCollector.Create;
begin
  FManagedObjects := TSouffleHeapObjectList.Create(False);
  FPinnedObjects := TDictionary<TSouffleHeapObject, Boolean>.Create;
  FTempRoots := TDictionary<TSouffleHeapObject, Boolean>.Create;
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
  FExternalRootMarker := nil;
end;

destructor TSouffleGarbageCollector.Destroy;
var
  I: Integer;
begin
  for I := 0 to FManagedObjects.Count - 1 do
    FManagedObjects[I].Free;
  FManagedObjects.Free;
  FPinnedObjects.Free;
  FTempRoots.Free;
  inherited;
end;

function TSouffleGarbageCollector.AllocateObject(
  const AObject: TSouffleHeapObject): TSouffleHeapObject;
begin
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
  Result := AObject;
end;

procedure TSouffleGarbageCollector.PinObject(
  const AObject: TSouffleHeapObject);
begin
  if Assigned(AObject) and not FPinnedObjects.ContainsKey(AObject) then
    FPinnedObjects.Add(AObject, True);
end;

procedure TSouffleGarbageCollector.UnpinObject(
  const AObject: TSouffleHeapObject);
begin
  FPinnedObjects.Remove(AObject);
end;

procedure TSouffleGarbageCollector.AddTempRoot(
  const AObject: TSouffleHeapObject);
begin
  if Assigned(AObject) and not FTempRoots.ContainsKey(AObject) then
    FTempRoots.Add(AObject, True);
end;

procedure TSouffleGarbageCollector.RemoveTempRoot(
  const AObject: TSouffleHeapObject);
begin
  FTempRoots.Remove(AObject);
end;

procedure TSouffleGarbageCollector.SetExternalRootMarker(
  const AMarker: TSouffleGCRootMarker);
begin
  FExternalRootMarker := AMarker;
end;

procedure TSouffleGarbageCollector.MarkPhase;
var
  I: Integer;
  Obj: TSouffleHeapObject;
begin
  for I := 0 to FManagedObjects.Count - 1 do
    FManagedObjects[I].GCMarked := False;

  for Obj in FPinnedObjects.Keys do
    Obj.MarkReferences;

  for Obj in FTempRoots.Keys do
    Obj.MarkReferences;

  if Assigned(FExternalRootMarker) then
    FExternalRootMarker();
end;

procedure TSouffleGarbageCollector.SweepPhase;
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

procedure TSouffleGarbageCollector.Collect;
begin
  if FCollecting then Exit;
  FCollecting := True;
  try
    MarkPhase;
    SweepPhase;
    FAllocationsSinceLastGC := 0;
    Inc(FTotalCollections);
  finally
    FCollecting := False;
  end;
end;

procedure TSouffleGarbageCollector.CollectIfNeeded;
begin
  if FEnabled and (FAllocationsSinceLastGC >= FGCThreshold) and not FCollecting then
    Collect;
end;

function TSouffleGarbageCollector.GetManagedObjectCount: Integer;
begin
  Result := FManagedObjects.Count;
end;

end.
