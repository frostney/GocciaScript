unit Goccia.GarbageCollector;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  HashMap;

type
  TGCManagedObject = class
  private
    FGCMark: Cardinal;
    FGCIndex: Integer;
    function GetGCMarked: Boolean; inline;
    procedure SetGCMarked(const AValue: Boolean); inline;
  public
    class procedure AdvanceMark; static; inline;
    procedure MarkReferences; virtual;
    // Called by the GC sweep instead of Free. Default calls Free.
    // Override to return the object to a pool instead of deallocating.
    procedure Recycle; virtual;
    property GCMarked: Boolean read GetGCMarked write SetGCMarked;
    property GCIndex: Integer read FGCIndex write FGCIndex;
  end;

  TGCManagedObjectList = TObjectList<TGCManagedObject>;
  TGCObjectSet = THashMap<TGCManagedObject, Boolean>;

  TGarbageCollector = class
  private
    FManagedObjects: TGCManagedObjectList;
    FPinnedObjects: TGCObjectSet;
    FTempRoots: TGCObjectSet;
    FRootObjects: TGCObjectSet;
    FActiveRootStack: TGCManagedObjectList;

    FAllocationsSinceLastGC: Integer;
    FGCThreshold: Integer;
    FEnabled: Boolean;
    FCollecting: Boolean;
    FTotalCollected: Int64;
    FTotalCollections: Integer;

    FBytesAllocated: Int64;
    FMaxBytes: Int64;
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
  protected
    procedure MarkRoots; virtual;
    procedure SweepObjects;
  public
    class function Instance: TGarbageCollector; inline;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

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

    // Young-generation collection: pre-marks objects before AWatermark
    // as surviving, then runs mark-and-sweep. MarkReferences on old
    // objects short-circuits via "if GCMarked then Exit", and the sweep
    // only walks objects after the watermark. Both mark and sweep are
    // O(young) instead of O(all).
    // Constraint: old objects must not acquire new references to young
    // objects between the watermark capture and the CollectYoung call.
    // Safe for benchmark measurement where old objects are read-only.
    procedure CollectYoung(const AWatermark: Integer);

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
    property MaxBytes: Int64 read FMaxBytes write FMaxBytes;
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

function DetectDefaultMaxBytes: Int64;

implementation

{$IFDEF UNIX}
function libc_sysconf(Name: Integer): Int64; cdecl; external 'c' name 'sysconf';
{$ENDIF}

{$IFDEF MSWINDOWS}
uses
  Windows
  {$IF DEFINED(GC_DEBUG)}, SysUtils{$ENDIF}
  {$IFDEF GC_TIMING}, TimingUtils{$ENDIF};
{$ELSE}
  {$IF DEFINED(GC_DEBUG) OR DEFINED(GC_TIMING)}
uses
  SysUtils
  {$IFDEF GC_TIMING},
  TimingUtils{$ENDIF};
  {$ENDIF}
{$ENDIF}

{ Returns the total physical memory in bytes, or 0 if detection fails. }
function GetPhysicalMemoryBytes: Int64;
{$IFDEF UNIX}
const
  {$IFDEF DARWIN}
  SC_PHYS_PAGES = 200;
  SC_PAGESIZE   = 29;
  {$ELSE}
  SC_PHYS_PAGES = 85;  { Linux }
  SC_PAGESIZE   = 30;
  {$ENDIF}
var
  Pages, PageSize: Int64;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  MemStatus: TMemoryStatusEx;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Pages := libc_sysconf(SC_PHYS_PAGES);
  PageSize := libc_sysconf(SC_PAGESIZE);
  if (Pages > 0) and (PageSize > 0) then
    Result := Pages * PageSize
  else
    Result := 0;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := Int64(MemStatus.ullTotalPhys)
  else
    Result := 0;
  {$ENDIF}
end;

function DetectDefaultMaxBytes: Int64;
var
  PhysMem, Cap: Int64;
begin
  {$IF SizeOf(Pointer) >= 8}
  Cap := MAX_BYTES_CAP_64BIT;
  {$ELSE}
  Cap := MAX_BYTES_CAP_32BIT;
  {$ENDIF}
  PhysMem := GetPhysicalMemoryBytes;
  if PhysMem > 0 then
  begin
    Result := PhysMem div 2;
    if Result > Cap then
      Result := Cap;
  end
  else
    Result := DEFAULT_MAX_BYTES;
end;

threadvar
  GCCurrentMark: Cardinal;
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

procedure TGCManagedObject.Recycle;
begin
  Free;
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
  FRootObjects := TGCObjectSet.Create;
  FActiveRootStack := TGCManagedObjectList.Create(False);
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
  FBytesAllocated := 0;
  FMaxBytes := DetectDefaultMaxBytes;
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
  FRootObjects.Free;
  FActiveRootStack.Free;
  inherited;
end;

procedure TGarbageCollector.RegisterObject(
  const AObject: TGCManagedObject);
begin
  AObject.GCIndex := FManagedObjects.Count;
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
  Inc(FBytesAllocated, AObject.InstanceSize);
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
    Dec(FBytesAllocated, AObject.InstanceSize);
  end;
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
  I: Integer;
begin
  for Pair in FPinnedObjects do
    Pair.Key.MarkReferences;

  for Pair in FTempRoots do
    Pair.Key.MarkReferences;

  for Pair in FRootObjects do
    Pair.Key.MarkReferences;

  for I := 0 to FActiveRootStack.Count - 1 do
    FActiveRootStack[I].MarkReferences;
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
  if FCollecting then Exit;
  FCollecting := True;
  try
    BeforeCount := FManagedObjects.Count;
    TGCManagedObject.AdvanceMark;
    {$IFDEF GC_TIMING}
    StartNs := GetNanoseconds;
    {$ENDIF}
    MarkRoots;
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

    for I := 0 to EffectiveWatermark - 1 do
    begin
      Obj := FManagedObjects[I];
      if Assigned(Obj) then
        Obj.GCMarked := True;
    end;

    MarkRoots;

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
  GCCurrentMark := 1;

end.
