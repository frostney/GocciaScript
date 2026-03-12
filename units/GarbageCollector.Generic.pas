unit GarbageCollector.Generic;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  GarbageCollector.Managed;

type
  TGCManagedObjectList = TObjectList<TGCManagedObject>;
  TGCRootMarker = procedure of object;

  TGarbageCollector = class
  private class var
    FInstance: TGarbageCollector;
  private
    FManagedObjects: TGCManagedObjectList;
    FPinnedObjects: TDictionary<TGCManagedObject, Boolean>;
    FTempRoots: TDictionary<TGCManagedObject, Boolean>;
    FRootObjects: TDictionary<TGCManagedObject, Boolean>;
    FActiveRootStack: TGCManagedObjectList;

    FExternalRootMarkers: array of TGCRootMarker;

    FAllocationsSinceLastGC: Integer;
    FGCThreshold: Integer;
    FEnabled: Boolean;
    FCollecting: Boolean;
    FTotalCollected: Int64;
    FTotalCollections: Integer;
    FNilSlots: Integer;

    function GetManagedObjectCount: Integer;
    function GetWatermark: Integer; inline;
  protected
    procedure MarkRoots; virtual;
    procedure SweepObjects;
  public
    class function Instance: TGarbageCollector; inline;
    class procedure Initialize;
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

    procedure AddRootObject(const AObject: TGCManagedObject);
    procedure RemoveRootObject(const AObject: TGCManagedObject);
    procedure PushActiveRoot(const AObject: TGCManagedObject);
    procedure PopActiveRoot;

    procedure AddExternalRootMarker(const AMarker: TGCRootMarker);
    procedure RemoveExternalRootMarker(const AMarker: TGCRootMarker);

    // Full mark-and-sweep of all managed objects. Unconditional — ignores
    // the Enabled flag and always runs. Use when a clean heap is required
    // (e.g. between script executions or before benchmark measurement).
    procedure Collect; virtual;

    // Automatic collection: calls Collect only when Enabled is True and
    // allocations since the last collection exceed the Threshold.
    procedure CollectIfNeeded; overload;

    // Same as CollectIfNeeded, but temporarily pushes AProtect onto the
    // active root stack so a stack-held object survives the collection.
    procedure CollectIfNeeded(const AProtect: TGCManagedObject); overload;

    // Young-generation collection: runs a full mark from roots, then
    // sweeps only objects allocated after AWatermark. Objects before
    // the watermark survive regardless of mark state, preserving
    // old-to-young references. Cheaper than Collect because the sweep
    // phase only frees young garbage while skipping old objects.
    procedure CollectYoung(const AWatermark: Integer);

    property Enabled: Boolean read FEnabled write FEnabled;
    property Threshold: Integer read FGCThreshold write FGCThreshold;
    property TotalCollected: Int64 read FTotalCollected;
    property TotalCollections: Integer read FTotalCollections;
    property ManagedObjectCount: Integer read GetManagedObjectCount;

    // Current position in the managed objects list. Capture before a
    // measurement phase and pass to CollectYoung for efficient
    // between-round collections.
    property Watermark: Integer read GetWatermark;
  end;

const
  DEFAULT_GC_THRESHOLD = 10000;

implementation

{$IFDEF GC_DEBUG}
uses
  SysUtils;
{$ENDIF}

{ TGarbageCollector }

class function TGarbageCollector.Instance: TGarbageCollector;
begin
  Result := FInstance;
end;

class procedure TGarbageCollector.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGarbageCollector.Create;
end;

class procedure TGarbageCollector.Shutdown;
begin
  FInstance.Free;
  FInstance := nil;
end;

constructor TGarbageCollector.Create;
begin
  inherited Create;
  FManagedObjects := TGCManagedObjectList.Create(False);
  FPinnedObjects := TDictionary<TGCManagedObject, Boolean>.Create;
  FTempRoots := TDictionary<TGCManagedObject, Boolean>.Create;
  FRootObjects := TDictionary<TGCManagedObject, Boolean>.Create;
  FActiveRootStack := TGCManagedObjectList.Create(False);
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
  FNilSlots := 0;
  SetLength(FExternalRootMarkers, 0);
end;

destructor TGarbageCollector.Destroy;
begin
  FManagedObjects.Free;
  FPinnedObjects.Free;
  FTempRoots.Free;
  FRootObjects.Free;
  FActiveRootStack.Free;
  inherited;
end;

function TGarbageCollector.AllocateObject(
  const AObject: TGCManagedObject): TGCManagedObject;
begin
  AObject.GCIndex := FManagedObjects.Count;
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
  Result := AObject;
end;

procedure TGarbageCollector.RegisterObject(
  const AObject: TGCManagedObject);
begin
  AObject.GCIndex := FManagedObjects.Count;
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGarbageCollector.UnregisterObject(
  const AObject: TGCManagedObject);
var
  Idx: Integer;
begin
  Idx := AObject.GCIndex;
  if (Idx >= 0) and (Idx < FManagedObjects.Count) and
     (FManagedObjects[Idx] = AObject) then
  begin
    FManagedObjects[Idx] := nil;
    AObject.GCIndex := -1;
    Inc(FNilSlots);
  end;
end;

procedure TGarbageCollector.PinObject(const AObject: TGCManagedObject);
begin
  if Assigned(AObject) and not FPinnedObjects.ContainsKey(AObject) then
    FPinnedObjects.Add(AObject, True);
end;

procedure TGarbageCollector.UnpinObject(
  const AObject: TGCManagedObject);
begin
  FPinnedObjects.Remove(AObject);
end;

procedure TGarbageCollector.AddTempRoot(
  const AObject: TGCManagedObject);
begin
  if Assigned(AObject) and not FTempRoots.ContainsKey(AObject) then
    FTempRoots.Add(AObject, True);
end;

procedure TGarbageCollector.RemoveTempRoot(
  const AObject: TGCManagedObject);
begin
  FTempRoots.Remove(AObject);
end;

function TGarbageCollector.IsTempRoot(
  const AObject: TGCManagedObject): Boolean;
begin
  Result := Assigned(AObject) and FTempRoots.ContainsKey(AObject);
end;

procedure TGarbageCollector.AddRootObject(
  const AObject: TGCManagedObject);
begin
  if not FRootObjects.ContainsKey(AObject) then
    FRootObjects.Add(AObject, True);
end;

procedure TGarbageCollector.RemoveRootObject(
  const AObject: TGCManagedObject);
begin
  FRootObjects.Remove(AObject);
end;

procedure TGarbageCollector.PushActiveRoot(
  const AObject: TGCManagedObject);
begin
  FActiveRootStack.Add(AObject);
end;

procedure TGarbageCollector.PopActiveRoot;
begin
  if FActiveRootStack.Count > 0 then
    FActiveRootStack.Delete(FActiveRootStack.Count - 1);
end;

procedure TGarbageCollector.AddExternalRootMarker(
  const AMarker: TGCRootMarker);
var
  Len: Integer;
begin
  Len := Length(FExternalRootMarkers);
  SetLength(FExternalRootMarkers, Len + 1);
  FExternalRootMarkers[Len] := AMarker;
end;

procedure TGarbageCollector.RemoveExternalRootMarker(
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

procedure TGarbageCollector.MarkRoots;
var
  Obj: TGCManagedObject;
  I: Integer;
begin
  for Obj in FPinnedObjects.Keys do
    Obj.MarkReferences;

  for Obj in FTempRoots.Keys do
    Obj.MarkReferences;

  for Obj in FRootObjects.Keys do
    Obj.MarkReferences;

  for I := 0 to FActiveRootStack.Count - 1 do
    FActiveRootStack[I].MarkReferences;

  for I := 0 to Length(FExternalRootMarkers) - 1 do
    FExternalRootMarkers[I]();
end;

procedure TGarbageCollector.SweepObjects;
var
  I, WriteIdx: Integer;
  Collected: Integer;
  Obj: TGCManagedObject;
begin
  Collected := 0;
  WriteIdx := 0;

  for I := 0 to FManagedObjects.Count - 1 do
  begin
    Obj := FManagedObjects[I];
    if Obj = nil then
      Continue;
    if Obj.GCMarked then
    begin
      Obj.GCIndex := WriteIdx;
      FManagedObjects[WriteIdx] := Obj;
      Inc(WriteIdx);
    end
    else
    begin
      Obj.GCIndex := -1;
      Obj.Free;
      Inc(Collected);
    end;
  end;

  FManagedObjects.Count := WriteIdx;
  if FManagedObjects.Capacity > 4 * WriteIdx + 256 then
    FManagedObjects.Capacity := WriteIdx + (WriteIdx div 2);
  FNilSlots := 0;
  FTotalCollected := FTotalCollected + Collected;
end;

procedure TGarbageCollector.Collect;
var
  BeforeCount: Integer;
begin
  if FCollecting then Exit;
  FCollecting := True;
  try
    BeforeCount := FManagedObjects.Count - FNilSlots;
    TGCManagedObject.AdvanceMark;
    MarkRoots;
    SweepObjects;
    FAllocationsSinceLastGC := 0;
    Inc(FTotalCollections);
    {$IFDEF GC_DEBUG}
    WriteLn(Format('[GC] Collect: %d -> %d objects (%d freed)',
      [BeforeCount, FManagedObjects.Count, BeforeCount - FManagedObjects.Count]));
    {$ENDIF}
  finally
    FCollecting := False;
  end;
end;

procedure TGarbageCollector.CollectIfNeeded;
begin
  if FEnabled and (FAllocationsSinceLastGC >= FGCThreshold) and
    not FCollecting then
    Collect;
end;

procedure TGarbageCollector.CollectIfNeeded(
  const AProtect: TGCManagedObject);
begin
  if not FEnabled or (FAllocationsSinceLastGC < FGCThreshold) or
     FCollecting then
    Exit;
  if Assigned(AProtect) then
    FActiveRootStack.Add(AProtect);
  try
    Collect;
  finally
    if Assigned(AProtect) then
      FActiveRootStack.Delete(FActiveRootStack.Count - 1);
  end;
end;

procedure TGarbageCollector.CollectYoung(const AWatermark: Integer);
var
  I, WriteIdx, Collected: Integer;
  Obj: TGCManagedObject;
  EffectiveWatermark: Integer;
begin
  if FCollecting then Exit;
  FCollecting := True;
  try
    EffectiveWatermark := AWatermark;
    if EffectiveWatermark < 0 then
      EffectiveWatermark := 0;
    if EffectiveWatermark > FManagedObjects.Count then
      EffectiveWatermark := FManagedObjects.Count;

    TGCManagedObject.AdvanceMark;
    MarkRoots;

    Collected := 0;
    WriteIdx := 0;

    for I := 0 to FManagedObjects.Count - 1 do
    begin
      Obj := FManagedObjects[I];
      if Obj = nil then
        Continue;
      if Obj.GCMarked or (I < EffectiveWatermark) then
      begin
        Obj.GCIndex := WriteIdx;
        FManagedObjects[WriteIdx] := Obj;
        Inc(WriteIdx);
      end
      else
      begin
        Obj.GCIndex := -1;
        Obj.Free;
        Inc(Collected);
      end;
    end;

    FManagedObjects.Count := WriteIdx;
    if FManagedObjects.Capacity > 4 * WriteIdx + 256 then
      FManagedObjects.Capacity := WriteIdx + (WriteIdx div 2);
    FNilSlots := 0;
    FAllocationsSinceLastGC := 0;
    FTotalCollected := FTotalCollected + Collected;
    Inc(FTotalCollections);
    {$IFDEF GC_DEBUG}
    WriteLn(Format('[GC] CollectYoung(wm=%d): %d total, %d young, %d freed, %d surviving',
      [AWatermark, EffectiveWatermark + (FManagedObjects.Count - EffectiveWatermark) + Collected,
       FManagedObjects.Count - EffectiveWatermark + Collected, Collected, FManagedObjects.Count]));
    {$ENDIF}
  finally
    FCollecting := False;
  end;
end;

function TGarbageCollector.GetWatermark: Integer;
begin
  Result := FManagedObjects.Count;
end;

function TGarbageCollector.GetManagedObjectCount: Integer;
begin
  Result := FManagedObjects.Count - FNilSlots;
end;

end.
