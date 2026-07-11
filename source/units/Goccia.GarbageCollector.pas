unit Goccia.GarbageCollector;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  HashMap,
  MemoryDetection;

type
  TGCManagedObject = class
  private
    FGCMark: Cardinal;
    FGCIndex: Integer;
    function GetGCMarked: Boolean; inline;
    procedure SetGCMarked(const AValue: Boolean); inline;
  public
    procedure BeforeDestruction; override;
    class procedure AdvanceMark; static; inline;
    procedure MarkReferences; virtual;
    function TraceWeakReferences: Boolean; virtual;
    procedure SweepWeakReferences; virtual;
    // Called by the GC sweep instead of Free. Default calls Free.
    // Override to return the object to a pool instead of deallocating.
    procedure Recycle; virtual;
    property GCMarked: Boolean read GetGCMarked write SetGCMarked;
    property GCIndex: Integer read FGCIndex write FGCIndex;
  end;

  TGCManagedObjectList = TObjectList<TGCManagedObject>;
  TGCObjectSet = THashMap<TGCManagedObject, Boolean>;
  TGCObjectRefCounts = THashMap<TGCManagedObject, Integer>;

  // Initialize stack-local roots with InitializeTempRoot before first use.
  TGocciaTempRoot = record
    ObjectValue: TGCManagedObject;
    Added: Boolean;
  end;

  TGocciaActiveRootFrame = record
  private
    FCount: Integer;
  public
    procedure Initialize; inline;
    procedure Add(const AObject: TGCManagedObject);
    procedure Clear;
  end;

  TGarbageCollector = class
  private
    FManagedObjects: TGCManagedObjectList;
    FPinnedObjects: TGCObjectSet;
    FTempRoots: TGCObjectSet;
    FQueuedRoots: TGCObjectRefCounts;
    FKeptObjects: TGCObjectSet;
    FRootObjects: TGCObjectSet;
    FActiveRootStack: TGCManagedObjectList;
    // Weak containers (WeakMap/WeakSet/WeakRef/FinalizationRegistry) that have
    // held weak data: a container joins on its first weak insertion and leaves
    // only when it is itself collected, so an emptied-but-live container stays
    // in the set. Lets Collect/CollectYoung skip the weak passes while empty.
    FWeakContainers: TGCObjectSet;

    FAllocationsSinceLastGC: Integer;
    FGCThreshold: Integer;
    FEnabled: Boolean;
    FCollecting: Boolean;
    FTotalCollected: Int64;
    FTotalCollections: Integer;

    FBytesAllocated: Int64;
    FPeakBytesAllocated: Int64;
    FTotalBytesAllocated: Int64;
    FMaxBytes: Int64;
    FSuggestedMaxBytes: Int64;
    FMemoryLimitFiring: Boolean;

    {$IFDEF GC_TIMING}
    FTotalMarkTimeNs: Int64;
    FTotalSweepTimeNs: Int64;
    FTotalGCTimeNs: Int64;
    FMaxMarkTimeNs: Int64;
    FMaxSweepTimeNs: Int64;
    {$ENDIF}

    function GetManagedObjectCount: Integer;
    function GetWatermark: Integer; inline;
    procedure ClearActiveRootEntries(const AObject: TGCManagedObject);
  protected
    procedure MarkRoots; virtual;
    procedure TraceWeakReferences;
    procedure SweepWeakReferences;
    procedure SweepObjects;
  public
    class function Instance: TGarbageCollector; inline;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterObject(const AObject: TGCManagedObject);
    procedure UnregisterObject(const AObject: TGCManagedObject);
    procedure RegisterWeakContainer(const AObject: TGCManagedObject);
    procedure UnregisterWeakContainer(const AObject: TGCManagedObject);
    procedure PinObject(const AObject: TGCManagedObject);
    procedure UnpinObject(const AObject: TGCManagedObject);
    procedure AddTempRoot(const AObject: TGCManagedObject);
    procedure RemoveTempRoot(const AObject: TGCManagedObject);
    function IsTempRoot(const AObject: TGCManagedObject): Boolean;
    procedure AddQueuedRoot(const AObject: TGCManagedObject);
    procedure RemoveQueuedRoot(const AObject: TGCManagedObject);
    procedure AddKeptObject(const AObject: TGCManagedObject);
    procedure ClearKeptObjects;

    procedure AddRootObject(const AObject: TGCManagedObject);
    procedure RemoveRootObject(const AObject: TGCManagedObject);
    procedure PushActiveRoot(const AObject: TGCManagedObject);
    procedure PopActiveRoot;

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

    function NeedsMemoryPressureCollection: Boolean;
    procedure CollectForMemoryPressure(const AProtect: TGCManagedObject);

    // Young-generation collection: marks from real roots, then sweeps
    // only objects allocated after AWatermark. Old objects are retained
    // even when unmarked, but they are not pre-marked because old objects
    // can acquire young references between the watermark capture and this
    // collection. Tracing through old roots keeps those young objects live.
    procedure CollectYoung(const AWatermark: Integer);
    procedure ResetPeakBytesAllocated;

    {$IFDEF GC_TIMING}
    procedure PrintTimingSummary;
    {$ENDIF}

    property Enabled: Boolean read FEnabled write FEnabled;
    property Threshold: Integer read FGCThreshold write FGCThreshold;
    property TotalCollected: Int64 read FTotalCollected;
    property TotalCollections: Integer read FTotalCollections;
    property ManagedObjectCount: Integer read GetManagedObjectCount;

    // Byte-level memory tracking. BytesAllocated is the approximate
    // number of bytes currently tracked by the GC (InstanceSize per
    // registered object). Set MaxBytes to a positive value to impose
    // a ceiling; allocations that exceed it raise a RangeError.
    property BytesAllocated: Int64 read FBytesAllocated;
    property PeakBytesAllocated: Int64 read FPeakBytesAllocated;
    property TotalBytesAllocated: Int64 read FTotalBytesAllocated;
    property MaxBytes: Int64 read FMaxBytes write FMaxBytes;
    property SuggestedMaxBytes: Int64 read FSuggestedMaxBytes;
    property MemoryLimitFiring: Boolean read FMemoryLimitFiring write FMemoryLimitFiring;

    // Current position in the managed objects list. Capture before a
    // measurement phase and pass to CollectYoung for efficient
    // between-round collections.
    property Watermark: Integer read GetWatermark;
  end;

const
  DEFAULT_GC_THRESHOLD = 10000;

  DEFAULT_MAX_BYTES   = 512 * 1024 * 1024;         { 512 MB fallback }
  MAX_BYTES_CAP_64BIT = Int64(8192) * 1024 * 1024;  { 8 GB cap for 64-bit }
  MAX_BYTES_CAP_32BIT = 700 * 1024 * 1024;          { 700 MB cap for 32-bit }
  MEMORY_PRESSURE_COLLECTION_MIN_RESERVE = 16 * 1024;
  MEMORY_PRESSURE_COLLECTION_MAX_RESERVE = 1024 * 1024;

function DetectDefaultMaxBytes: Int64;
procedure InitializeTempRoot(var ARoot: TGocciaTempRoot); inline;
procedure AddTempRootIfNeeded(var ARoot: TGocciaTempRoot;
  const AObject: TGCManagedObject);
procedure RemoveTempRootIfNeeded(var ARoot: TGocciaTempRoot);

implementation

uses
  SyncObjs
  {$IF DEFINED(GC_DEBUG) OR DEFINED(GC_TIMING)}
  ,
  SysUtils
  {$IFDEF GC_TIMING}, TimingUtils{$ENDIF}
  {$ENDIF};

function DetectDefaultMaxBytes: Int64;
var
  PhysMem, Cap: Int64;
begin
  {$IF SizeOf(Pointer) >= 8}
  Cap := MAX_BYTES_CAP_64BIT;
  {$ELSE}
  Cap := MAX_BYTES_CAP_32BIT;
  {$ENDIF}
  PhysMem := GetAvailableMemoryBytes;
  if PhysMem > 0 then
  begin
    Result := PhysMem div 2;
    if Result > Cap then
      Result := Cap;
  end
  else
    Result := DEFAULT_MAX_BYTES;
end;

var
  GCCurrentMark: Cardinal;
  GCCollectLock: TRTLCriticalSection;

threadvar
  GCThreadInstance: TGarbageCollector;

{ TGCManagedObject }

class procedure TGCManagedObject.AdvanceMark;
begin
  Inc(GCCurrentMark);
  if GCCurrentMark = 0 then
    GCCurrentMark := 1;
end;

function TGCManagedObject.GetGCMarked: Boolean;
begin
  Result := FGCMark = GCCurrentMark;
end;

procedure TGCManagedObject.SetGCMarked(const AValue: Boolean);
begin
  if AValue then
    FGCMark := GCCurrentMark;
end;

procedure TGCManagedObject.MarkReferences;
begin
  FGCMark := GCCurrentMark;
end;

function TGCManagedObject.TraceWeakReferences: Boolean;
begin
  Result := False;
end;

procedure TGCManagedObject.SweepWeakReferences;
begin
end;

procedure TGCManagedObject.Recycle;
begin
  Free;
end;

procedure TGCManagedObject.BeforeDestruction;
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.UnregisterObject(Self);
  inherited;
end;

procedure InitializeTempRoot(var ARoot: TGocciaTempRoot);
begin
  ARoot.ObjectValue := nil;
  ARoot.Added := False;
end;

procedure AddTempRootIfNeeded(var ARoot: TGocciaTempRoot;
  const AObject: TGCManagedObject);
var
  GC: TGarbageCollector;
begin
  if ARoot.Added then
  begin
    if ARoot.ObjectValue = AObject then
      Exit;
    RemoveTempRootIfNeeded(ARoot);
  end;

  ARoot.ObjectValue := AObject;
  ARoot.Added := False;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and Assigned(AObject) and not GC.IsTempRoot(AObject) then
  begin
    GC.AddTempRoot(AObject);
    ARoot.Added := True;
  end;
end;

procedure RemoveTempRootIfNeeded(var ARoot: TGocciaTempRoot);
var
  GC: TGarbageCollector;
begin
  if ARoot.Added then
  begin
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.RemoveTempRoot(ARoot.ObjectValue);
  end;
  ARoot.ObjectValue := nil;
  ARoot.Added := False;
end;

{ TGocciaActiveRootFrame }

procedure TGocciaActiveRootFrame.Initialize;
begin
  FCount := 0;
end;

procedure TGocciaActiveRootFrame.Add(const AObject: TGCManagedObject);
var
  GC: TGarbageCollector;
begin
  if not Assigned(AObject) then
    Exit;
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  GC.PushActiveRoot(AObject);
  Inc(FCount);
end;

procedure TGocciaActiveRootFrame.Clear;
var
  GC: TGarbageCollector;
begin
  if FCount <= 0 then
    Exit;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    while FCount > 0 do
    begin
      GC.PopActiveRoot;
      Dec(FCount);
    end
  else
    FCount := 0;
end;

{ TGarbageCollector }

class function TGarbageCollector.Instance: TGarbageCollector;
begin
  Result := GCThreadInstance;
end;

class procedure TGarbageCollector.Initialize;
begin
  if not Assigned(GCThreadInstance) then
  begin
    GCThreadInstance := TGarbageCollector.Create;
    if GCCurrentMark = 0 then
      GCCurrentMark := 1;
  end;
end;

class procedure TGarbageCollector.Shutdown;
begin
  GCThreadInstance.Free;
  GCThreadInstance := nil;
end;

constructor TGarbageCollector.Create;
begin
  inherited Create;
  FManagedObjects := TGCManagedObjectList.Create(False);
  FPinnedObjects := TGCObjectSet.Create;
  FTempRoots := TGCObjectSet.Create;
  FQueuedRoots := TGCObjectRefCounts.Create;
  FKeptObjects := TGCObjectSet.Create;
  FRootObjects := TGCObjectSet.Create;
  FActiveRootStack := TGCManagedObjectList.Create(False);
  FWeakContainers := TGCObjectSet.Create;
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
  FBytesAllocated := 0;
  FPeakBytesAllocated := 0;
  FTotalBytesAllocated := 0;
  FSuggestedMaxBytes := DetectDefaultMaxBytes;
  FMaxBytes := FSuggestedMaxBytes;
  FMemoryLimitFiring := False;
  {$IFDEF GC_TIMING}
  FTotalMarkTimeNs := 0;
  FTotalSweepTimeNs := 0;
  FTotalGCTimeNs := 0;
  FMaxMarkTimeNs := 0;
  FMaxSweepTimeNs := 0;
  {$ENDIF}
end;

destructor TGarbageCollector.Destroy;
begin
  {$IFDEF GC_TIMING}
  PrintTimingSummary;
  {$ENDIF}
  FManagedObjects.Free;
  FPinnedObjects.Free;
  FTempRoots.Free;
  FQueuedRoots.Free;
  FKeptObjects.Free;
  FRootObjects.Free;
  FActiveRootStack.Free;
  FWeakContainers.Free;
  inherited;
end;

procedure TGarbageCollector.RegisterObject(
  const AObject: TGCManagedObject);
begin
  AObject.GCIndex := FManagedObjects.Count;
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
  Inc(FBytesAllocated, AObject.InstanceSize);
  Inc(FTotalBytesAllocated, AObject.InstanceSize);
  if FBytesAllocated > FPeakBytesAllocated then
    FPeakBytesAllocated := FBytesAllocated;
end;

procedure TGarbageCollector.UnregisterObject(
  const AObject: TGCManagedObject);
var
  Idx: Integer;
begin
  if not Assigned(AObject) then
    Exit;
  if not Assigned(FManagedObjects) then
    Exit;

  Idx := AObject.GCIndex;
  if FCollecting and
     ((Idx < 0) or (Idx >= FManagedObjects.Count) or
      (FManagedObjects[Idx] <> AObject)) then
    Exit;

  if Assigned(FPinnedObjects) then
    FPinnedObjects.Remove(AObject);
  if Assigned(FTempRoots) then
    FTempRoots.Remove(AObject);
  if Assigned(FQueuedRoots) then
    FQueuedRoots.Remove(AObject);
  if Assigned(FKeptObjects) then
    FKeptObjects.Remove(AObject);
  if Assigned(FRootObjects) then
    FRootObjects.Remove(AObject);
  ClearActiveRootEntries(AObject);

  if (Idx >= 0) and (Idx < FManagedObjects.Count) and
     (FManagedObjects[Idx] = AObject) then
  begin
    FManagedObjects[Idx] := nil;
    AObject.GCIndex := -1;
    Dec(FBytesAllocated, AObject.InstanceSize);
  end;
end;

procedure TGarbageCollector.RegisterWeakContainer(
  const AObject: TGCManagedObject);
begin
  if Assigned(AObject) then
    FWeakContainers.Add(AObject, True);
end;

// Weak containers unregister from their own destructors, not from
// UnregisterObject: UnregisterObject early-exits while collecting for an
// object already being swept (its GCIndex is -1), so centralizing the removal
// there would leave a swept weak container dangling in the set.
procedure TGarbageCollector.UnregisterWeakContainer(
  const AObject: TGCManagedObject);
begin
  if Assigned(FWeakContainers) then
    FWeakContainers.Remove(AObject);
end;

procedure TGarbageCollector.ClearActiveRootEntries(
  const AObject: TGCManagedObject);
var
  I: Integer;
begin
  if not Assigned(FActiveRootStack) then
    Exit;
  for I := FActiveRootStack.Count - 1 downto 0 do
    if FActiveRootStack[I] = AObject then
      FActiveRootStack[I] := nil;
end;

procedure TGarbageCollector.PinObject(const AObject: TGCManagedObject);
begin
  if Assigned(AObject) then
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
  if Assigned(AObject) then
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

procedure TGarbageCollector.AddQueuedRoot(
  const AObject: TGCManagedObject);
var
  Count: Integer;
begin
  if not Assigned(AObject) then
    Exit;
  if FQueuedRoots.TryGetValue(AObject, Count) then
    FQueuedRoots.AddOrSetValue(AObject, Count + 1)
  else
    FQueuedRoots.Add(AObject, 1);
end;

procedure TGarbageCollector.RemoveQueuedRoot(
  const AObject: TGCManagedObject);
var
  Count: Integer;
begin
  if not Assigned(AObject) then
    Exit;
  if not FQueuedRoots.TryGetValue(AObject, Count) then
    Exit;
  if Count <= 1 then
    FQueuedRoots.Remove(AObject)
  else
    FQueuedRoots.AddOrSetValue(AObject, Count - 1);
end;

procedure TGarbageCollector.AddKeptObject(
  const AObject: TGCManagedObject);
begin
  if Assigned(AObject) then
    FKeptObjects.Add(AObject, True);
end;

procedure TGarbageCollector.ClearKeptObjects;
begin
  FKeptObjects.Clear;
end;

procedure TGarbageCollector.AddRootObject(
  const AObject: TGCManagedObject);
begin
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

procedure TGarbageCollector.MarkRoots;
var
  Pair: TGCObjectSet.TKeyValuePair;
  QueuedPair: TGCObjectRefCounts.TKeyValuePair;
  I: Integer;
begin
  for Pair in FPinnedObjects do
    Pair.Key.MarkReferences;

  for Pair in FTempRoots do
    Pair.Key.MarkReferences;

  for QueuedPair in FQueuedRoots do
    QueuedPair.Key.MarkReferences;

  for Pair in FKeptObjects do
    Pair.Key.MarkReferences;

  for Pair in FRootObjects do
    Pair.Key.MarkReferences;

  for I := 0 to FActiveRootStack.Count - 1 do
    if Assigned(FActiveRootStack[I]) then
      FActiveRootStack[I].MarkReferences;
end;

procedure TGarbageCollector.TraceWeakReferences;
var
  Changed: Boolean;
  I: Integer;
  Obj: TGCManagedObject;
begin
  repeat
    Changed := False;
    for I := 0 to FManagedObjects.Count - 1 do
    begin
      Obj := FManagedObjects[I];
      if Assigned(Obj) and Obj.GCMarked then
        Changed := Obj.TraceWeakReferences or Changed;
    end;
  until not Changed;
end;

procedure TGarbageCollector.SweepWeakReferences;
var
  I: Integer;
  Obj: TGCManagedObject;
begin
  for I := 0 to FManagedObjects.Count - 1 do
  begin
    Obj := FManagedObjects[I];
    if Assigned(Obj) and Obj.GCMarked then
      Obj.SweepWeakReferences;
  end;
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
      Dec(FBytesAllocated, Obj.InstanceSize);
      Obj.GCIndex := -1;
      Obj.Recycle;
      Inc(Collected);
    end;
  end;

  FManagedObjects.Count := WriteIdx;
  if FManagedObjects.Capacity > 4 * WriteIdx + 256 then
    FManagedObjects.Capacity := WriteIdx + (WriteIdx div 2);
  FTotalCollected := FTotalCollected + Collected;
end;

procedure TGarbageCollector.Collect;
var
  BeforeCount: Integer;
  {$IFDEF GC_TIMING}
  StartNs, AfterMarkNs, EndNs: Int64;
  MarkNs, SweepNs, TotalNs: Int64;
  {$ENDIF}
begin
  EnterCriticalSection(GCCollectLock);
  try
    if FCollecting then Exit;
    FCollecting := True;
    try
      BeforeCount := FManagedObjects.Count;
      TGCManagedObject.AdvanceMark;
      {$IFDEF GC_TIMING}
      StartNs := GetNanoseconds;
      {$ENDIF}
      MarkRoots;
      // The weak-reference passes are no-ops on every non-weak object, so
      // skip both full-heap walks entirely while no weak container is tracked.
      // A populated-then-emptied container stays tracked while live, so the
      // passes still run for it (see docs/garbage-collector.md).
      if FWeakContainers.Count > 0 then
      begin
        TraceWeakReferences;
        SweepWeakReferences;
      end;
      {$IFDEF GC_TIMING}
      AfterMarkNs := GetNanoseconds;
      {$ENDIF}
      SweepObjects;
      FAllocationsSinceLastGC := 0;

      // Adaptive threshold: next collection after allocating as many
      // objects as survived, amortizing collection cost to O(1) per
      // allocation. Small heaps keep the default minimum.
      FGCThreshold := FManagedObjects.Count;
      if FGCThreshold < DEFAULT_GC_THRESHOLD then
        FGCThreshold := DEFAULT_GC_THRESHOLD;

      Inc(FTotalCollections);
      {$IFDEF GC_TIMING}
      EndNs := GetNanoseconds;
      MarkNs := AfterMarkNs - StartNs;
      SweepNs := EndNs - AfterMarkNs;
      TotalNs := EndNs - StartNs;
      FTotalMarkTimeNs := FTotalMarkTimeNs + MarkNs;
      FTotalSweepTimeNs := FTotalSweepTimeNs + SweepNs;
      FTotalGCTimeNs := FTotalGCTimeNs + TotalNs;
      if MarkNs > FMaxMarkTimeNs then
        FMaxMarkTimeNs := MarkNs;
      if SweepNs > FMaxSweepTimeNs then
        FMaxSweepTimeNs := SweepNs;
      WriteLn(Format('[GC] Collect: mark=%s sweep=%s total=%s (%d before, %d after)',
        [FormatDuration(MarkNs), FormatDuration(SweepNs), FormatDuration(TotalNs),
         BeforeCount, FManagedObjects.Count]));
      {$ENDIF}
      {$IFDEF GC_DEBUG}
      WriteLn(Format('[GC] Collect: %d -> %d objects (%d freed)',
        [BeforeCount, FManagedObjects.Count, BeforeCount - FManagedObjects.Count]));
      {$ENDIF}
    finally
      FCollecting := False;
    end;
  finally
    LeaveCriticalSection(GCCollectLock);
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

function TGarbageCollector.NeedsMemoryPressureCollection: Boolean;
var
  Reserve: Int64;
begin
  Result := False;
  // Explicit pressure checks remain active when routine threshold GC is
  // disabled, as it is during bytecode execution and benchmark measurement.
  if (FMaxBytes <= 0) or FCollecting or FMemoryLimitFiring then
    Exit;

  Reserve := FMaxBytes div 8;
  if Reserve < MEMORY_PRESSURE_COLLECTION_MIN_RESERVE then
    Reserve := MEMORY_PRESSURE_COLLECTION_MIN_RESERVE;
  if Reserve > MEMORY_PRESSURE_COLLECTION_MAX_RESERVE then
    Reserve := MEMORY_PRESSURE_COLLECTION_MAX_RESERVE;
  if Reserve >= FMaxBytes then
    Reserve := FMaxBytes div 2;

  Result := FBytesAllocated >= (FMaxBytes - Reserve);
end;

procedure TGarbageCollector.CollectForMemoryPressure(
  const AProtect: TGCManagedObject);
var
  WasFiring: Boolean;
begin
  if not NeedsMemoryPressureCollection then
    Exit;

  if Assigned(AProtect) then
    FActiveRootStack.Add(AProtect);
  WasFiring := FMemoryLimitFiring;
  FMemoryLimitFiring := True;
  try
    Collect;
  finally
    FMemoryLimitFiring := WasFiring;
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
  EnterCriticalSection(GCCollectLock);
  try
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
      if FWeakContainers.Count > 0 then
      begin
        TraceWeakReferences;
        SweepWeakReferences;
      end;

      Collected := 0;
      WriteIdx := EffectiveWatermark;

      for I := EffectiveWatermark to FManagedObjects.Count - 1 do
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
          Dec(FBytesAllocated, Obj.InstanceSize);
          Obj.GCIndex := -1;
          Obj.Recycle;
          Inc(Collected);
        end;
      end;

      FManagedObjects.Count := WriteIdx;
      if FManagedObjects.Capacity > 4 * WriteIdx + 256 then
        FManagedObjects.Capacity := WriteIdx + (WriteIdx div 2);
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
  finally
    LeaveCriticalSection(GCCollectLock);
  end;
end;

procedure TGarbageCollector.ResetPeakBytesAllocated;
begin
  FPeakBytesAllocated := FBytesAllocated;
end;

{$IFDEF GC_TIMING}
procedure TGarbageCollector.PrintTimingSummary;
var
  AvgMarkNs, AvgSweepNs, AvgGCNs: Int64;
begin
  WriteLn('[GC] === Timing Summary ===');
  WriteLn(Format('[GC] Total collections: %d', [FTotalCollections]));
  WriteLn(Format('[GC] Total GC time: %s (mark=%s, sweep=%s)',
    [FormatDuration(FTotalGCTimeNs), FormatDuration(FTotalMarkTimeNs),
     FormatDuration(FTotalSweepTimeNs)]));
  if FTotalCollections > 0 then
  begin
    AvgMarkNs := FTotalMarkTimeNs div FTotalCollections;
    AvgSweepNs := FTotalSweepTimeNs div FTotalCollections;
    AvgGCNs := FTotalGCTimeNs div FTotalCollections;
    WriteLn(Format('[GC] Average per collection: %s (mark=%s, sweep=%s)',
      [FormatDuration(AvgGCNs), FormatDuration(AvgMarkNs),
       FormatDuration(AvgSweepNs)]));
    WriteLn(Format('[GC] Max single collection: mark=%s, sweep=%s',
      [FormatDuration(FMaxMarkTimeNs), FormatDuration(FMaxSweepTimeNs)]));
  end;
  WriteLn(Format('[GC] Total objects collected: %d', [FTotalCollected]));
end;
{$ENDIF}

function TGarbageCollector.GetWatermark: Integer;
begin
  Result := FManagedObjects.Count;
end;

function TGarbageCollector.GetManagedObjectCount: Integer;
begin
  Result := FManagedObjects.Count;
end;

initialization
  InitCriticalSection(GCCollectLock);
  GCCurrentMark := 1;

finalization
  DoneCriticalSection(GCCollectLock);

end.
