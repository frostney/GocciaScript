program Goccia.GarbageCollector.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  Classes,
  SysUtils,

  Goccia.GarbageCollector,
  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.ThreadPolls,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TChildManaged = class(TGCManagedObject)
  public
    destructor Destroy; override;
  end;

  { A real TGocciaValue, so a descriptor can hold it, that reports its own
    sweep. Created with a nil prototype: nothing here reads a property, and a
    standalone object keeps the suite free of engine setup. }
  TCountedObjectValue = class(TGocciaObjectValue)
  public
    destructor Destroy; override;
  end;

  TParentManaged = class(TGCManagedObject)
  private
    FChild: TChildManaged;
  public
    destructor Destroy; override;
    procedure MarkReferences; override;
    property Child: TChildManaged read FChild write FChild;
  end;

  { A foreign thread releasing external bytes against the MAIN thread's
    collector — the shape of a TGocciaErrorObjectValue charged on collector A
    but destroyed on worker thread B, which is the one cross-thread entry
    point into a collector. Deliberately does NOT initialize its own
    thread-local runtime: the error destructor releases through the reserving
    collector pointer, not through this thread's TGarbageCollector.Instance. }
  TCrossThreadReleaser = class(TThread)
  private
    FCollector: TGarbageCollector;
    FChunkBytes: Int64;
    FIterations: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACollector: TGarbageCollector;
      const AChunkBytes: Int64; const AIterations: Integer);
  end;

  { Starts, then stops, a collector of its own and records what this thread's
    TGarbageCollector.Instance and its GThreadPolls.Collector mirror were at
    each step. }
  TOwnCollectorThread = class(TThread)
  public
    NoneAtStart: Boolean;
    OwnAfterInitialize: TGarbageCollector;
    MirrorAfterInitialize: Boolean;
    OwnCountGrowth: Integer;
    NoneAfterShutdown: Boolean;
  protected
    procedure Execute; override;
  end;

  TTestGarbageCollector = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestCollectYoungTracesOldToYoungReferences;
    procedure TestDestroyFreesEveryManagedObject;
    procedure TestReservationCollectsAndRetries;
    procedure TestReservationCollectsBeyondPressureReserve;
    procedure TestReservationRefusesWhatCollectionCannotFit;
    procedure TestRepeatedRefusalCollectsOnlyOnce;
    procedure TestTryCollectGrantsAlreadyFittingRequestWithoutWalking;
    procedure TestDataDescriptorPushRootsProtectsValue;
    procedure TestAccessorDescriptorPushRootsProtectsBothHalves;
    procedure TestInnerFrameClearLeavesOuterFrameRootsIntact;
    procedure TestActiveRootStackGrowsPastInitialCapacity;
    procedure TestPushesOutsideTheGuardingTryLeakOnRaise;
    procedure TestCrossThreadReleaseKeepsAccountingExact;
    procedure TestRegisterObjectAdvancesEveryByteCounter;
    procedure TestEachThreadHasItsOwnCollector;
    procedure TestHeapTriggerKeepsGarbageNearTheCeiling;
    procedure TestHeapTriggerBacksOffAboveSurvivors;
    procedure TestHeapTriggerReturnsAfterUnownedHeapIsFreed;
    procedure TestHeapGrowthTriggersUnderAFarCeiling;
    procedure TestHeapGrowthTriggerDoesNotRepeatOverRetainedHeap;
    procedure TestNativeGrowthSamplesTheHeap;
    procedure TestNonCollectingChargeNeverCollectsOrLatches;
    procedure TestTrackedTriggerBacksOffAboveSurvivors;
    procedure TestTrackedTriggerReturnsAfterTrackedBytesAreReleased;
  end;

var
  GParentDestructorCount: Integer;
  GChildDestructorCount: Integer;
  GCountedValueDestructorCount: Integer;

destructor TChildManaged.Destroy;
begin
  Inc(GChildDestructorCount);
  inherited;
end;

destructor TParentManaged.Destroy;
begin
  Inc(GParentDestructorCount);
  inherited;
end;

destructor TCountedObjectValue.Destroy;
begin
  Inc(GCountedValueDestructorCount);
  inherited;
end;

procedure TParentManaged.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FChild) then
    FChild.MarkReferences;
end;

constructor TCrossThreadReleaser.Create(const ACollector: TGarbageCollector;
  const AChunkBytes: Int64; const AIterations: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCollector := ACollector;
  FChunkBytes := AChunkBytes;
  FIterations := AIterations;
end;

procedure TCrossThreadReleaser.Execute;
var
  I: Integer;
begin
  for I := 1 to FIterations do
    FCollector.ReleaseExternalBytes(FChunkBytes);
end;

procedure TOwnCollectorThread.Execute;
var
  Before: Integer;
begin
  NoneAtStart := (TGarbageCollector.Instance = nil) and
    (GThreadPolls.Collector = nil);
  TGarbageCollector.Initialize;
  try
    OwnAfterInitialize := TGarbageCollector.Instance;
    MirrorAfterInitialize := GThreadPolls.Collector = Pointer(OwnAfterInitialize);
    Before := OwnAfterInitialize.ManagedObjectCount;
    TGocciaNumberLiteralValue.Create(1.5);
    OwnCountGrowth := OwnAfterInitialize.ManagedObjectCount - Before;
  finally
    TGarbageCollector.Shutdown;
  end;
  NoneAfterShutdown := (TGarbageCollector.Instance = nil) and
    (GThreadPolls.Collector = nil);
end;

procedure TTestGarbageCollector.SetupTests;
begin
  Test('CollectYoung traces references from old rooted objects',
    TestCollectYoungTracesOldToYoungReferences);
  Test('Destroying a collector frees every object it still manages',
    TestDestroyFreesEveryManagedObject);
  Test('External reservation collects and retries',
    TestReservationCollectsAndRetries);
  Test('External reservation collects past the pressure reserve',
    TestReservationCollectsBeyondPressureReserve);
  Test('External reservation still refuses what a collection cannot fit',
    TestReservationRefusesWhatCollectionCannotFit);
  Test('Repeated refusal of the same size collects only once',
    TestRepeatedRefusalCollectsOnlyOnce);
  Test('An already-fitting request is granted without walking the heap',
    TestTryCollectGrantsAlreadyFittingRequestWithoutWalking);
  Test('Data descriptor PushRoots keeps its value alive across a collection',
    TestDataDescriptorPushRootsProtectsValue);
  Test('Accessor descriptor PushRoots keeps getter and setter alive',
    TestAccessorDescriptorPushRootsProtectsBothHalves);
  Test('Clearing an inner frame leaves an outer frame''s roots intact',
    TestInnerFrameClearLeavesOuterFrameRootsIntact);
  Test('The active-root stack grows past its initial capacity and releases',
    TestActiveRootStackGrowsPastInitialCapacity);
  Test('A frame''s pushes must sit inside the try that clears it',
    TestPushesOutsideTheGuardingTryLeakOnRaise);
  Test('Cross-thread releases keep the byte ledger exact under churn',
    TestCrossThreadReleaseKeepsAccountingExact);
  Test('Registering an object advances the live, total and peak byte counters',
    TestRegisterObjectAdvancesEveryByteCounter);
  Test('Each thread allocates into its own collector, from Initialize to Shutdown',
    TestEachThreadHasItsOwnCollector);
  { Heap-triggered collection reads the FPC heap manager's status; other
    compilers report no heap total, so the trigger is inert there. }
  {$IFDEF FPC}
  Test('Heap growth triggers collection before garbage outgrows the ceiling',
    TestHeapTriggerKeepsGarbageNearTheCeiling);
  Test('Heap-triggered collection backs off when the heap it cannot reclaim exceeds the ceiling',
    TestHeapTriggerBacksOffAboveSurvivors);
  Test('The heap trigger comes back down once heap it could not reclaim is freed',
    TestHeapTriggerReturnsAfterUnownedHeapIsFreed);
  Test('Heap growth over what the last collection left triggers a collection under a far ceiling',
    TestHeapGrowthTriggersUnderAFarCeiling);
  Test('The growth trigger does not repeat over heap a collection cannot reclaim',
    TestHeapGrowthTriggerDoesNotRepeatOverRetainedHeap);
  Test('Reported native growth samples the heap between registrations',
    TestNativeGrowthSamplesTheHeap);
  {$ENDIF}
  Test('A non-collecting charge neither collects nor latches pressure',
    TestNonCollectingChargeNeverCollectsOrLatches);
  Test('The tracked trigger backs off above a live set inside the reserve',
    TestTrackedTriggerBacksOffAboveSurvivors);
  Test('Released tracked bytes bring the tracked trigger back to the line',
    TestTrackedTriggerReturnsAfterTrackedBytesAreReleased);
end;

const
  { Registrations between two simulated safe points. The VM checks every
    1024 instructions, so a checkpoint per few dozen allocations is the
    realistic shape. }
  CHECKPOINT_INTERVAL = 64;

{ Allocates ACount unreachable objects, passing a pressure checkpoint every
  CHECKPOINT_INTERVAL of them, and returns the highest heap-manager in-use
  total observed at a checkpoint. }
function ChurnGarbage(const AGC: TGarbageCollector;
  const ACount: Integer): Int64;
var
  I: Integer;
  Heap: Int64;
begin
  Result := CurrentHeapBytes;
  for I := 1 to ACount do
  begin
    TGocciaObjectValue.Create;
    if I mod CHECKPOINT_INTERVAL = 0 then
    begin
      Heap := CurrentHeapBytes;
      if Heap > Result then
        Result := Heap;
      AGC.CollectForMemoryPressure(nil);
    end;
  end;
end;

procedure TTestGarbageCollector.TestHeapTriggerKeepsGarbageNearTheCeiling;
const
  MIN_CEILING_ABOVE_HEAP = 8 * 1024 * 1024;
  GARBAGE_OBJECTS = 400000;
  { Sampling drift (HEAP_SAMPLE_INTERVAL registrations) plus the allocation
    between two checkpoints, with room for allocator rounding. }
  SLACK = 2 * 1024 * 1024;
var
  CollectionsBefore, HeapCollectionsBefore: Integer;
  GC: TGarbageCollector;
  HeapAtStart: Int64;
  Headroom: Int64;
  PeakHeap: Int64;
  PreviousMaxBytes: Int64;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  HeapAtStart := CurrentHeapBytes;
  { The headroom must stay above two pressure reserves (a reserve is an eighth
    of the ceiling), or the back-off for heap the collector cannot reclaim
    would engage and this would test that instead. Matching the starting heap
    keeps it there however large the heap already is. }
  Headroom := MIN_CEILING_ABOVE_HEAP;
  if Headroom < HeapAtStart then
    Headroom := HeapAtStart;
  { The ceiling sits well above what the heap already holds. Each garbage
    object is charged its InstanceSize but costs the heap manager more than
    twice that (its property map and allocator rounding are uncharged), so a
    trigger on BytesAllocated alone lets the heap run past twice the ceiling
    before the first collection. }
  GC.MaxBytes := HeapAtStart + Headroom;
  try
    CollectionsBefore := GC.TotalCollections;
    HeapCollectionsBefore := GC.HeapTriggeredCollections;

    PeakHeap := ChurnGarbage(GC, GARBAGE_OBJECTS);

    Expect<Boolean>(GC.HeapTriggeredCollections > HeapCollectionsBefore)
      .ToBe(True);
    Expect<Boolean>(GC.TotalCollections > CollectionsBefore).ToBe(True);
    Expect<Boolean>(PeakHeap <= GC.MaxBytes + SLACK).ToBe(True);
  finally
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestHeapTriggerBacksOffAboveSurvivors;
const
  CEILING_ABOVE_HEAP = 4 * 1024 * 1024;
  { Heap the collector cannot reclaim: more than the ceiling allows on its
    own, as an engine-held buffer or a large AST would be. }
  RETAINED_BYTES = 12 * 1024 * 1024;
  GARBAGE_OBJECTS = 200000;
  { Without the back-off the trigger would sit one pressure reserve (512 KiB
    at this ceiling) above the retained heap and collect every few thousand
    allocations: about seventy collections for this churn. }
  MAX_HEAP_COLLECTIONS = 12;
  SLACK = 2 * 1024 * 1024;
var
  GC: TGarbageCollector;
  HeapAfterRetaining: Int64;
  HeapCollectionsBefore: Integer;
  PeakHeap: Int64;
  PreviousMaxBytes: Int64;
  Retained: Pointer;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  GC.MaxBytes := CurrentHeapBytes + CEILING_ABOVE_HEAP;
  GetMem(Retained, RETAINED_BYTES);
  try
    FillChar(Retained^, RETAINED_BYTES, 0);
    GC.Collect;
    HeapAfterRetaining := CurrentHeapBytes;
    Expect<Boolean>(HeapAfterRetaining > GC.MaxBytes).ToBe(True);

    HeapCollectionsBefore := GC.HeapTriggeredCollections;
    PeakHeap := ChurnGarbage(GC, GARBAGE_OBJECTS);

    { Collecting cannot get under the ceiling, so it runs once per growth
      step of half what the last collection left rather than at every
      checkpoint, and the heap peaks near 1.5x that. Refusal still compares
      BytesAllocated, which stays far below the ceiling: nothing is refused. }
    Expect<Boolean>(GC.HeapTriggeredCollections > HeapCollectionsBefore)
      .ToBe(True);
    Expect<Boolean>(GC.HeapTriggeredCollections - HeapCollectionsBefore <=
      MAX_HEAP_COLLECTIONS).ToBe(True);
    Expect<Boolean>(PeakHeap <= HeapAfterRetaining +
      HeapAfterRetaining div 2 + SLACK).ToBe(True);
  finally
    FreeMem(Retained);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestHeapTriggerReturnsAfterUnownedHeapIsFreed;
const
  CEILING_ABOVE_HEAP = 8 * 1024 * 1024;
  RETAINED_BYTES = 24 * 1024 * 1024;
  GARBAGE_OBJECTS = 200000;
  SLACK = 2 * 1024 * 1024;
var
  GC: TGarbageCollector;
  PeakHeap: Int64;
  PreviousMaxBytes: Int64;
  Retained: Pointer;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  GC.MaxBytes := CurrentHeapBytes + CEILING_ABOVE_HEAP;
  GetMem(Retained, RETAINED_BYTES);
  try
    FillChar(Retained^, RETAINED_BYTES, 0);
    { This collection records a baseline far above the ceiling, so the
      trigger backs off above it. }
    GC.Collect;
    FreeMem(Retained);
    Retained := nil;
    { The baseline no longer holds once the unowned block is gone. The next
      heap sample sees that, and the trigger is back below the ceiling
      without waiting for a collection to re-measure it. }
    PeakHeap := ChurnGarbage(GC, GARBAGE_OBJECTS);
    Expect<Boolean>(PeakHeap <= GC.MaxBytes + SLACK).ToBe(True);
  finally
    if Assigned(Retained) then
      FreeMem(Retained);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestHeapGrowthTriggersUnderAFarCeiling;
const
  { Far enough that the ceiling's own trigger never comes into reach. }
  FAR_CEILING = Int64(64) * 1024 * 1024 * 1024;
  GARBAGE_OBJECTS = 1500000;
  SLACK = 2 * 1024 * 1024;
var
  GC: TGarbageCollector;
  Growth: Int64;
  HeapAfterCollect: Int64;
  HeapCollectionsBefore: Integer;
  PeakHeap: Int64;
  PreviousMaxBytes: Int64;
begin
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := GC.MaxBytes;
  GC.MaxBytes := FAR_CEILING;
  try
    GC.Collect;
    HeapAfterCollect := CurrentHeapBytes;
    HeapCollectionsBefore := GC.HeapTriggeredCollections;

    { Far more garbage than the growth floor. Before the growth trigger
      nothing collected here, and the heap grew by all of it. }
    PeakHeap := ChurnGarbage(GC, GARBAGE_OBJECTS);

    Growth := HeapAfterCollect * (HEAP_GROWTH_TRIGGER_FACTOR - 1);
    if Growth < HEAP_GROWTH_TRIGGER_MIN_BYTES then
      Growth := HEAP_GROWTH_TRIGGER_MIN_BYTES;
    Expect<Boolean>(GC.HeapTriggeredCollections > HeapCollectionsBefore)
      .ToBe(True);
    Expect<Boolean>(PeakHeap <= HeapAfterCollect + Growth + SLACK).ToBe(True);
  finally
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestHeapGrowthTriggerDoesNotRepeatOverRetainedHeap;
const
  FAR_CEILING = Int64(64) * 1024 * 1024 * 1024;
  { Heap the collector does not own and cannot reclaim, large enough that
    the factor, not the floor, sets the step. }
  RETAINED_BYTES = 48 * 1024 * 1024;
  GARBAGE_OBJECTS = 1500000;
  { A trigger with no record of the last collection would fire at every
    checkpoint once the retained heap passed it: 23,437 checkpoints here.
    The factor needs the garbage to reach several times the retained heap. }
  MAX_HEAP_COLLECTIONS = 4;
var
  GC: TGarbageCollector;
  HeapCollectionsBefore: Integer;
  PreviousMaxBytes: Int64;
  Retained: Pointer;
begin
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := GC.MaxBytes;
  GC.MaxBytes := FAR_CEILING;
  GetMem(Retained, RETAINED_BYTES);
  try
    FillChar(Retained^, RETAINED_BYTES, 0);
    GC.Collect;
    HeapCollectionsBefore := GC.HeapTriggeredCollections;
    ChurnGarbage(GC, GARBAGE_OBJECTS);
    Expect<Boolean>(GC.HeapTriggeredCollections - HeapCollectionsBefore <=
      MAX_HEAP_COLLECTIONS).ToBe(True);
  finally
    FreeMem(Retained);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestNativeGrowthSamplesTheHeap;
const
  CEILING_ABOVE_HEAP = 16 * 1024 * 1024;
  BLOCK_BYTES = 1024 * 1024;
  BLOCK_COUNT = 32;
var
  Blocks: array[0..BLOCK_COUNT - 1] of Pointer;
  GC: TGarbageCollector;
  I: Integer;
  PreviousMaxBytes: Int64;
  Latched: Boolean;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  GC.MaxBytes := CurrentHeapBytes + CEILING_ABOVE_HEAP;
  for I := 0 to BLOCK_COUNT - 1 do
    Blocks[I] := nil;
  Latched := False;
  try
    { Element storage grows the heap without registering anything: twice
      the headroom, reported as it is allocated, and no object created. The
      registration count alone would not sample the heap at all. }
    for I := 0 to BLOCK_COUNT - 1 do
    begin
      { The gate reports a request before the store allocates it. }
      GC.NoteNativeGrowth(BLOCK_BYTES);
      GetMem(Blocks[I], BLOCK_BYTES);
      FillChar(Blocks[I]^, BLOCK_BYTES, 0);
      if GC.ExternalPressurePending then
      begin
        Latched := True;
        Break;
      end;
    end;
    Expect<Boolean>(Latched).ToBe(True);
    { It latches, never collects: the checkpoint does that. }
    Expect<Boolean>(CurrentHeapBytes <= GC.MaxBytes + BLOCK_BYTES).ToBe(True);
  finally
    for I := 0 to BLOCK_COUNT - 1 do
      if Assigned(Blocks[I]) then
        FreeMem(Blocks[I]);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;

  { One request larger than the headroom, reported before it is allocated:
    the sample counts it as in use, or it would see the heap from before the
    request, reset the count, and leave the allocation unsampled. }
  GC.MaxBytes := CurrentHeapBytes + CEILING_ABOVE_HEAP;
  try
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(False);
    GC.NoteNativeGrowth(2 * CEILING_ABOVE_HEAP);
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(True);
  finally
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestNonCollectingChargeNeverCollectsOrLatches;
const
  HEADROOM_BYTES = 64 * 1024;
  CHARGE_BYTES = 1024;
var
  BaselineBytes, ChargedBytes, PreviousMaxBytes: Int64;
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GChildDestructorCount := 0;
  PreviousMaxBytes := GC.MaxBytes;
  ChargedBytes := 0;
  try
    { Unrooted garbage that a collection would reclaim, so a charge that
      collected before refusing could succeed. }
    GC.RegisterObject(TChildManaged.Create);
    BaselineBytes := GC.BytesAllocated;
    GC.MaxBytes := BaselineBytes + HEADROOM_BYTES;
    CollectionsBefore := GC.TotalCollections;

    { The VM charges stack growth from inside call setup, where a collection
      would sweep values held only in Pascal locals: a refusal must not walk
      the heap. }
    Expect<Boolean>(GC.TryChargeExternalBytes(HEADROOM_BYTES + 1)).ToBe(False);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore);
    Expect<Integer>(GChildDestructorCount).ToBe(0);
    Expect<Int64>(GC.BytesAllocated).ToBe(BaselineBytes);

    { A charge that fits is counted, inside the pressure reserve too, without
      arming a collection: a generator charges its frame at every await. }
    GC.MaxBytes := BaselineBytes + CHARGE_BYTES;
    Expect<Boolean>(GC.TryChargeExternalBytes(CHARGE_BYTES)).ToBe(True);
    ChargedBytes := CHARGE_BYTES;
    Expect<Int64>(GC.BytesAllocated - BaselineBytes).ToBe(CHARGE_BYTES);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(True);
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(False);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore);
  finally
    if ChargedBytes > 0 then
      GC.ReleaseExternalBytes(ChargedBytes);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestTrackedTriggerBacksOffAboveSurvivors;
const
  CEILING_HEADROOM = 4 * 1024 * 1024;
  SMALL_CHARGE = 1024;
  GARBAGE_OBJECTS = 500;
var
  BaselineBytes, Ceiling, Charged, Garbage, Line, Request, Reserve,
    Survivors, Trigger: Int64;
  CollectionsBefore, I: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;

  procedure Charge(const ABytes: Int64);
  begin
    Expect<Boolean>(GC.TryReserveExternalBytes(ABytes)).ToBe(True);
    Inc(Charged, ABytes);
  end;

begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  Charged := 0;
  try
    BaselineBytes := GC.BytesAllocated;
    Ceiling := BaselineBytes + CEILING_HEADROOM;
    GC.MaxBytes := Ceiling;
    Reserve := GC.MemoryPressureReserve;
    Line := Ceiling - Reserve;
    { The live set must be able to sit below the line for the reset arm. }
    Expect<Boolean>(Line > BaselineBytes).ToBe(True);

    { External bytes are released explicitly, never by a collection, so this
      ballast is a live set no sweep can reclaim: a quarter of the reserve
      past the line, the shape of a script whose working set fits but sits
      close to its ceiling. }
    Charge(Line + Reserve div 4 - BaselineBytes);
    GC.Collect;
    Survivors := GC.BytesAllocated;
    Expect<Boolean>(Survivors >= Line).ToBe(True);

    { That collection could not get below the line, and nothing has been
      allocated since, so another one could free nothing. Without the
      back-off every checkpoint collected here (once per 1,024 VM
      instructions) and every charged reservation latched a collection. }
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);
    Charge(SMALL_CHARGE);
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(False);
    CollectionsBefore := GC.TotalCollections;
    GC.CollectForMemoryPressure(nil);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore);

    { Collections still follow allocation: halfway from the survivors to the
      ceiling, the trigger fires again, and a charge that reaches it latches
      the checkpoint's collection. }
    Trigger := Survivors + (Ceiling - Survivors) div 2;
    Charge(Trigger - 1 - GC.BytesAllocated);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(False);
    Charge(1);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(True);
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(True);
    GC.CollectForMemoryPressure(nil);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore + 1);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);

    { The back-off only delays the checkpoint. A reservation that does not
      fit still forces a collection before it is refused, so garbage made
      while the trigger is backed off is found and the request fits. }
    GChildDestructorCount := 0;
    Garbage := GC.BytesAllocated;
    for I := 1 to GARBAGE_OBJECTS do
      GC.RegisterObject(TChildManaged.Create);
    Garbage := GC.BytesAllocated - Garbage;
    Request := Ceiling - GC.BytesAllocated + Garbage div 2;
    Charge(Request);
    Expect<Integer>(GChildDestructorCount).ToBe(GARBAGE_OBJECTS);
    Expect<Boolean>(GC.BytesAllocated <= Ceiling).ToBe(True);
    { And the ceiling holds: what no collection can make room for is refused. }
    Expect<Boolean>(GC.TryReserveExternalBytes(
      Ceiling - GC.BytesAllocated + 1)).ToBe(False);
    Expect<Boolean>(GC.BytesAllocated <= Ceiling).ToBe(True);

    { Once a collection gets the live set back under the line, the trigger
      is the line again: reaching it latches at once. }
    GC.ReleaseExternalBytes(Charged);
    Charged := 0;
    GC.Collect;
    Expect<Boolean>(GC.BytesAllocated < Line).ToBe(True);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);
    Charge(Line - GC.BytesAllocated);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(True);
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(True);
  finally
    if Charged > 0 then
      GC.ReleaseExternalBytes(Charged);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestTrackedTriggerReturnsAfterTrackedBytesAreReleased;
const
  CEILING_HEADROOM = 4 * 1024 * 1024;
var
  BaselineBytes, Ceiling, Charged, Line, Reserve, Survivors: Int64;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;

  procedure Charge(const ABytes: Int64);
  begin
    Expect<Boolean>(GC.TryReserveExternalBytes(ABytes)).ToBe(True);
    Inc(Charged, ABytes);
  end;

  procedure Release(const ABytes: Int64);
  begin
    GC.ReleaseExternalBytes(ABytes);
    Dec(Charged, ABytes);
  end;

begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  Charged := 0;
  try
    BaselineBytes := GC.BytesAllocated;
    Ceiling := BaselineBytes + CEILING_HEADROOM;
    GC.MaxBytes := Ceiling;
    Reserve := GC.MemoryPressureReserve;
    Line := Ceiling - Reserve;
    Expect<Boolean>(Line - Reserve div 4 > BaselineBytes).ToBe(True);

    { A collection that cannot get below the line backs the trigger off. }
    Charge(Line + Reserve div 4 - BaselineBytes);
    GC.Collect;
    Survivors := GC.BytesAllocated;
    Expect<Boolean>(Survivors >= Line).ToBe(True);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);

    { The script then releases half its ballast without a collection, and
      grows back before any checkpoint runs. The release stays on record:
      the trigger is the line again, not halfway from the old survivors to
      the ceiling. }
    Release(Survivors - (Line - Reserve div 4));
    Expect<Boolean>(GC.BytesAllocated < Line).ToBe(True);
    Charge(Line - 1 - GC.BytesAllocated);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);
    Charge(1);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(True);
  finally
    if Charged > 0 then
      GC.ReleaseExternalBytes(Charged);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
  end;
end;

procedure TTestGarbageCollector.TestDataDescriptorPushRootsProtectsValue;
var
  Descriptor: TGocciaPropertyDescriptorData;
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  { The descriptor is a plain class, not a managed object, so it is not itself
    a root: without PushRoots the value it points at is unreachable the moment
    the only other reference to it is a Pascal local. }
  Descriptor := TGocciaPropertyDescriptorData.Create(
    TCountedObjectValue.Create, [pfWritable, pfEnumerable, pfConfigurable]);
  try
    Roots.Initialize;
    try
      Descriptor.PushRoots(Roots);
      GC.Collect;
      Expect<Integer>(GCountedValueDestructorCount).ToBe(0);
    finally
      Roots.Clear;
    end;

    { Clearing twice must be a no-op, not a second release: the frames this
      primitive serves are cleared in nested finally blocks during unwind. }
    Roots.Clear;

    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(1);
  finally
    Descriptor.Free;
  end;
end;

procedure TTestGarbageCollector.TestAccessorDescriptorPushRootsProtectsBothHalves;
var
  Descriptor: TGocciaPropertyDescriptorAccessor;
  GC: TGarbageCollector;
  GetterOnly: TGocciaPropertyDescriptorAccessor;
  Roots: TGocciaActiveRootFrame;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  Descriptor := TGocciaPropertyDescriptorAccessor.Create(
    TCountedObjectValue.Create, TCountedObjectValue.Create, [pfConfigurable]);
  { A setter-less accessor is the ordinary case, and PushRoots must drop the
    nil half rather than push it. }
  GetterOnly := TGocciaPropertyDescriptorAccessor.Create(
    TCountedObjectValue.Create, nil, [pfConfigurable]);
  try
    Roots.Initialize;
    try
      Descriptor.PushRoots(Roots);
      GetterOnly.PushRoots(Roots);
      GC.Collect;
      Expect<Integer>(GCountedValueDestructorCount).ToBe(0);
    finally
      Roots.Clear;
    end;

    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(3);
  finally
    Descriptor.Free;
    GetterOnly.Free;
  end;
end;

procedure TTestGarbageCollector.TestInnerFrameClearLeavesOuterFrameRootsIntact;
var
  GC: TGarbageCollector;
  InnerRoots: TGocciaActiveRootFrame;
  InnerValue: TCountedObjectValue;
  OuterRoots: TGocciaActiveRootFrame;
  OuterValue: TCountedObjectValue;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  { Both frames are Initialized at the same depth and then added to in
    interleaved order — a strictly harder shape than the promise combinators,
    where the inner frame is merely Initialized before the outer has pushed.
    Releasing to a recorded base depth would make the inner Clear take the
    outer frame's entry with it; releasing the frame's own count cannot. }
  OuterRoots.Initialize;
  InnerRoots.Initialize;
  try
    OuterValue := TCountedObjectValue.Create;
    OuterRoots.Add(OuterValue);
    InnerValue := TCountedObjectValue.Create;
    InnerRoots.Add(InnerValue);

    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(0);

    InnerRoots.Clear;
    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(1);

    { Re-adding after a Clear rolls back against the current depth, so a frame
      reused across loop iterations stays correct. }
    InnerRoots.Add(OuterValue);
    InnerRoots.Clear;
    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(1);
  finally
    { Innermost first: if anything above raises before the mid-test Clear, the
      inner frame's entry is still on top of the outer frame's, and releasing
      the outer one first would take the inner entry with it. Clear is
      idempotent — it exits on a zero count — so the mid-test Clears above cost
      this one nothing, and a leaked inner entry would otherwise outlive the
      test on the active-root stack. }
    InnerRoots.Clear;
    OuterRoots.Clear;
  end;

  GC.Collect;
  Expect<Integer>(GCountedValueDestructorCount).ToBe(2);
end;

procedure TTestGarbageCollector.TestActiveRootStackGrowsPastInitialCapacity;
const
  { Two doublings past the stack's initial 256-slot capacity, so the growth
    path itself is under test, not just the fast path the other tests touch. }
  PUSH_COUNT = 600;
var
  GC: TGarbageCollector;
  I: Integer;
  Roots: TGocciaActiveRootFrame;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  Roots.Initialize;
  try
    for I := 1 to PUSH_COUNT do
      Roots.Add(TCountedObjectValue.Create);

    { Every pushed value must survive a collection while the frame holds it —
      a growth bug that dropped or duplicated slots would surface here. }
    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(0);
  finally
    Roots.Clear;
  end;

  GC.Collect;
  Expect<Integer>(GCountedValueDestructorCount).ToBe(PUSH_COUNT);
end;

{ Stands in for the allocation the benchmark builtin used to perform between
  its pushes and its try — anything on that line that can raise turns the
  pushes above it into a permanent leak. }
procedure RaiseAfterPush;
begin
  raise Exception.Create('between the push and the guarded region');
end;

procedure TTestGarbageCollector.TestPushesOutsideTheGuardingTryLeakOnRaise;
var
  BaseDepth: Integer;
  GC: TGarbageCollector;
  Raised: Boolean;
  Roots: TGocciaActiveRootFrame;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;
  BaseDepth := GC.ActiveRootDepth;

  { The shape the builtins must not write: a push, then something that can
    raise, then the try whose finally would have cleared the frame. A frame is
    a stack record, so an escape here is not a recoverable leak — the entries
    stay on the collector's stack for the life of the thread and pin whatever
    they point at. Asserting on the depth rather than on a sweep count is what
    makes this cheap: no collection is needed to see the imbalance. }
  Raised := False;
  Roots.Initialize;
  try
    Roots.Add(TCountedObjectValue.Create);
    RaiseAfterPush;
  except
    on E: Exception do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
  Expect<Integer>(GC.ActiveRootDepth).ToBe(BaseDepth + 1);

  { And the shape they must, modelled on the benchmark builtin the fix came
    from: a push, then a call that can raise (there, the arguments collection
    whose construction the pushes used to precede), then a second push — all
    inside the try. Moving the first push and RaiseAfterPush above the `try`
    reproduces the original defect and this assertion fails, which is what the
    first half above only demonstrates in isolation. Without the intervening
    raising call the two shapes are indistinguishable: a raise *inside* the try
    unwinds through the finally either way. }
  Roots.Clear;
  Expect<Integer>(GC.ActiveRootDepth).ToBe(BaseDepth);

  Raised := False;
  Roots.Initialize;
  try
    try
      Roots.Add(TCountedObjectValue.Create);
      RaiseAfterPush;
      Roots.Add(TCountedObjectValue.Create);
    finally
      Roots.Clear;
    end;
  except
    on E: Exception do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
  Expect<Integer>(GC.ActiveRootDepth).ToBe(BaseDepth);

  GC.Collect;
  Expect<Integer>(GCountedValueDestructorCount).ToBe(2);
end;

procedure TTestGarbageCollector.TestRegisterObjectAdvancesEveryByteCounter;
var
  GC: TGarbageCollector;
  LiveBefore, TotalBefore: Int64;
  Managed: TChildManaged;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GC.ResetPeakBytesAllocated;
  LiveBefore := GC.BytesAllocated;
  TotalBefore := GC.TotalBytesAllocated;
  Expect<Int64>(GC.PeakBytesAllocated).ToBe(LiveBefore);

  Managed := TChildManaged.Create;
  GC.RegisterObject(Managed);
  Expect<Int64>(GC.BytesAllocated - LiveBefore).ToBe(Managed.InstanceSize);
  Expect<Int64>(GC.TotalBytesAllocated - TotalBefore).ToBe(
    Managed.InstanceSize);
  Expect<Int64>(GC.PeakBytesAllocated).ToBe(GC.BytesAllocated);

  // Collecting it lowers the live count only: the total is cumulative and the
  // peak keeps the high-water mark.
  GC.Collect;
  Expect<Int64>(GC.BytesAllocated).ToBe(LiveBefore);
  Expect<Int64>(GC.TotalBytesAllocated - TotalBefore).ToBe(
    TChildManaged.InstanceSize);
  Expect<Int64>(GC.PeakBytesAllocated - LiveBefore).ToBe(
    TChildManaged.InstanceSize);
end;

procedure TTestGarbageCollector.TestReservationCollectsAndRetries;
var
  BaselineBytes: Int64;
  GarbageBytes: Int64;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
  Reserved: Boolean;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GChildDestructorCount := 0;
  BaselineBytes := GC.BytesAllocated;

  GC.RegisterObject(TChildManaged.Create);
  GarbageBytes := GC.BytesAllocated - BaselineBytes;
  PreviousMaxBytes := GC.MaxBytes;
  GC.MaxBytes := BaselineBytes + GarbageBytes;
  Reserved := False;
  try
    Reserved := GC.TryReserveExternalBytes(GarbageBytes);
    Expect<Boolean>(Reserved).ToBe(True);
    Expect<Integer>(GChildDestructorCount).ToBe(1);
  finally
    if Reserved then
      GC.ReleaseExternalBytes(GarbageBytes);
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

procedure TTestGarbageCollector.TestReservationCollectsBeyondPressureReserve;
const
  { Ballast and headroom are chosen so the live set parks well clear of the
    pressure trigger while the reservation itself is several times the
    reserve — the shape the last-resort path used to refuse outright. }
  BUDGET_HEADROOM_BYTES = 8 * 1024 * 1024;
  BALLAST_BYTES = 4 * 1024 * 1024;
  GARBAGE_OBJECT_COUNT = 4000;
var
  BaselineBytes: Int64;
  BallastReserved: Boolean;
  GC: TGarbageCollector;
  I: Integer;
  PreviousMaxBytes: Int64;
  Reservation: Int64;
  Reserved: Boolean;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;
  PreviousMaxBytes := GC.MaxBytes;
  BallastReserved := False;
  Reservation := 0;
  Reserved := False;
  try
    GC.MaxBytes := BaselineBytes + BUDGET_HEADROOM_BYTES;
    { External bytes are released explicitly, never by a collection, so the
      ballast is a live set no sweep can reclaim. }
    BallastReserved := GC.TryReserveExternalBytes(BALLAST_BYTES);
    Expect<Boolean>(BallastReserved).ToBe(True);

    GChildDestructorCount := 0;
    for I := 1 to GARBAGE_OBJECT_COUNT do
      GC.RegisterObject(TChildManaged.Create);

    { The probe only means anything inside the band: no pressure is latched
      and the live set has not crossed the reserve, so the heuristic on its
      own would have declined to collect and the reservation would have been
      refused with every one of those objects still reclaimable. }
    Expect<Boolean>(GC.ExternalPressurePending).ToBe(False);
    Expect<Boolean>(GC.NeedsMemoryPressureCollection).ToBe(False);

    Reservation := GC.MaxBytes - BaselineBytes - BALLAST_BYTES;
    Expect<Boolean>(Reservation > GC.MaxBytes div 8).ToBe(True);

    Reserved := GC.TryReserveExternalBytes(Reservation);
    Expect<Boolean>(Reserved).ToBe(True);
    Expect<Integer>(GChildDestructorCount).ToBe(GARBAGE_OBJECT_COUNT);
  finally
    if Reserved then
      GC.ReleaseExternalBytes(Reservation);
    if BallastReserved then
      GC.ReleaseExternalBytes(BALLAST_BYTES);
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

procedure TTestGarbageCollector.TestReservationRefusesWhatCollectionCannotFit;
const
  BUDGET_HEADROOM_BYTES = 4 * 1024 * 1024;
var
  BaselineBytes: Int64;
  BallastReserved: Boolean;
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;
  PreviousMaxBytes := GC.MaxBytes;
  BallastReserved := False;
  try
    GC.MaxBytes := BaselineBytes + BUDGET_HEADROOM_BYTES;
    { Larger than the whole budget: no collection could make room for it, so
      it is refused without walking the heap first. The collection count is
      what pins that second half — without it the assertion still passes if
      the over-budget guard is removed and the refusal comes from the retry. }
    CollectionsBefore := GC.TotalCollections;
    Expect<Boolean>(GC.TryReserveExternalBytes(GC.MaxBytes + 1)).ToBe(False);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore);

    { Fits the budget on its own, but not beside a live set a collection
      cannot reclaim. This is the refusal the forced collection must still
      reach after finding nothing to free. }
    BallastReserved := GC.TryReserveExternalBytes(BUDGET_HEADROOM_BYTES div 2);
    Expect<Boolean>(BallastReserved).ToBe(True);
    Expect<Boolean>(
      GC.TryReserveExternalBytes(BUDGET_HEADROOM_BYTES)).ToBe(False);
  finally
    if BallastReserved then
      GC.ReleaseExternalBytes(BUDGET_HEADROOM_BYTES div 2);
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

{ Pins the grant-without-walking arm of TryCollectForLimitedBytes: a request
  that already fits must return True without a collection. The arm exists for
  a cross-thread interleaving — a foreign release landing between a caller's
  failed fit test and this call can make the request fit, and the force
  predicate would otherwise read that as "nothing to force" and refuse a
  grantable request — but the contract it establishes is directly observable
  single-threaded, which is what this asserts. }
procedure TTestGarbageCollector.TestTryCollectGrantsAlreadyFittingRequestWithoutWalking;
const
  BUDGET_HEADROOM_BYTES = 1024 * 1024;
var
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  PreviousMaxBytes := GC.MaxBytes;
  try
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    CollectionsBefore := GC.TotalCollections;
    Expect<Boolean>(
      GC.TryCollectForLimitedBytes(BUDGET_HEADROOM_BYTES div 2)).ToBe(True);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore);
  finally
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

procedure TTestGarbageCollector.TestRepeatedRefusalCollectsOnlyOnce;
const
  BUDGET_HEADROOM_BYTES = 8 * 1024 * 1024;
  BALLAST_BYTES = 4 * 1024 * 1024;
  GARBAGE_OBJECT_COUNT = 4000;
  { Over the budget beside the ballast, so no collection can ever fit it,
    but under the budget itself so it is not short-circuited as impossible. }
  UNFITTABLE_BYTES = 6 * 1024 * 1024;
  RETRY_COUNT = 16;
var
  BaselineBytes: Int64;
  BallastReserved: Boolean;
  CollectionsAfterFirst: Integer;
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  I: Integer;
  PreviousMaxBytes: Int64;
  Reservation: Int64;
  Reserved: Boolean;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;
  PreviousMaxBytes := GC.MaxBytes;
  BallastReserved := False;
  Reservation := 0;
  Reserved := False;
  try
    GC.MaxBytes := BaselineBytes + BUDGET_HEADROOM_BYTES;
    BallastReserved := GC.TryReserveExternalBytes(BALLAST_BYTES);
    Expect<Boolean>(BallastReserved).ToBe(True);
    for I := 1 to GARBAGE_OBJECT_COUNT do
      GC.RegisterObject(TChildManaged.Create);

    { The first attempt is entitled to its collection: nothing is on record
      yet, and the heap really does hold reclaimable garbage. }
    CollectionsBefore := GC.TotalCollections;
    Expect<Boolean>(
      GC.TryReserveExternalBytes(UNFITTABLE_BYTES)).ToBe(False);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsBefore + 1);

    { Every retry of a size that collection already proved unfittable must be
      answered from the recorded floor. Without the damper this is one full
      mark-and-sweep per iteration, which is the amplification a guest gets by
      catching the RangeError and trying again. }
    CollectionsAfterFirst := GC.TotalCollections;
    for I := 1 to RETRY_COUNT do
      Expect<Boolean>(
        GC.TryReserveExternalBytes(UNFITTABLE_BYTES)).ToBe(False);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsAfterFirst);

    { The floor is a statement about a size, not a blanket stop. Fresh garbage
      plus a request the floor does not rule out collects again and succeeds,
      so ordinary progress never inherits the damper. }
    for I := 1 to GARBAGE_OBJECT_COUNT do
      GC.RegisterObject(TChildManaged.Create);
    Reservation := GC.MaxBytes - BaselineBytes - BALLAST_BYTES;
    Reserved := GC.TryReserveExternalBytes(Reservation);
    Expect<Boolean>(Reserved).ToBe(True);
    Expect<Integer>(GC.TotalCollections).ToBe(CollectionsAfterFirst + 1);
  finally
    if Reserved then
      GC.ReleaseExternalBytes(Reservation);
    if BallastReserved then
      GC.ReleaseExternalBytes(BALLAST_BYTES);
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

{ #1592: a collector that goes away destroys what it still manages, roots,
  pins and temp roots included, instead of leaking it. A thread's runtime
  shutdown and the main thread's end of run rely on this. }
procedure TTestGarbageCollector.TestDestroyFreesEveryManagedObject;
var
  GC: TGarbageCollector;
  Parent: TParentManaged;
  Pinned, TempRooted: TChildManaged;
begin
  GParentDestructorCount := 0;
  GChildDestructorCount := 0;
  GC := TGarbageCollector.Create;
  try
    Parent := TParentManaged.Create;
    GC.RegisterObject(Parent);
    GC.AddRootObject(Parent);
    Parent.Child := TChildManaged.Create;
    GC.RegisterObject(Parent.Child);
    Pinned := TChildManaged.Create;
    GC.RegisterObject(Pinned);
    GC.PinObject(Pinned);
    TempRooted := TChildManaged.Create;
    GC.RegisterObject(TempRooted);
    GC.AddTempRoot(TempRooted);

    GC.Collect;
    Expect<Integer>(GC.ManagedObjectCount).ToBe(4);
    Expect<Integer>(GParentDestructorCount).ToBe(0);
    Expect<Integer>(GChildDestructorCount).ToBe(0);
  finally
    GC.Free;
  end;

  Expect<Integer>(GParentDestructorCount).ToBe(1);
  Expect<Integer>(GChildDestructorCount).ToBe(3);
end;

procedure TTestGarbageCollector.TestCollectYoungTracesOldToYoungReferences;
var
  GC: TGarbageCollector;
  Parent: TParentManaged;
  Child: TChildManaged;
  Watermark: Integer;
begin
  GParentDestructorCount := 0;
  GChildDestructorCount := 0;
  GC := TGarbageCollector.Instance;

  Parent := TParentManaged.Create;
  GC.RegisterObject(Parent);
  GC.AddRootObject(Parent);
  try
    GC.Collect;
    Watermark := GC.Watermark;

    Child := TChildManaged.Create;
    GC.RegisterObject(Child);
    Parent.Child := Child;

    GC.CollectYoung(Watermark);

    Expect<Integer>(GChildDestructorCount).ToBe(0);

    Parent.Child := nil;
    GC.RemoveRootObject(Parent);
    GC.Collect;

    Expect<Integer>(GParentDestructorCount).ToBe(1);
    Expect<Integer>(GChildDestructorCount).ToBe(1);
  finally
    if GParentDestructorCount = 0 then
      GC.RemoveRootObject(Parent);
  end;
end;

{ The tear this pins was real: before the accounting lock, an error object
  charged on collector A but destroyed on worker thread B drove A's
  FBytesAllocated concurrently with A's own allocations, and a lost update
  skewed the total --max-memory is checked against. The worker here releases
  a pre-charged reservation in small chunks while the owner thread churns
  registrations and full collections against the same counters; any lost
  update, torn 64-bit access, or deadlock between the collect and accounting
  locks surfaces as an exact-balance failure (or a hang) below. }
procedure TTestGarbageCollector.TestCrossThreadReleaseKeepsAccountingExact;
const
  CHUNK_BYTES = 64;
  { Sized so the releaser spans the owner-side churn (allocations plus 20
    full collections) rather than finishing in its opening moments — the
    exact-balance assertion only bites while the two sides actually overlap. }
  RELEASE_ITERATIONS = 200000;
  CHURN_ITERATIONS = 20000;
  CHURN_COLLECT_INTERVAL = 1000;
var
  BaselineBytes: Int64;
  GC: TGarbageCollector;
  I: Integer;
  Releaser: TCrossThreadReleaser;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;

  { Charge everything the worker will release, up front, on this (the
    reserving) thread — the error-excerpt shape: reserved on A, released
    piecemeal from B. }
  Expect<Boolean>(GC.TryReserveExternalBytes(
    Int64(CHUNK_BYTES) * RELEASE_ITERATIONS)).ToBe(True);

  Releaser := TCrossThreadReleaser.Create(GC, CHUNK_BYTES,
    RELEASE_ITERATIONS);
  try
    Releaser.Start;
    for I := 1 to CHURN_ITERATIONS do
    begin
      GC.RegisterObject(TChildManaged.Create);
      if I mod CHURN_COLLECT_INTERVAL = 0 then
        GC.Collect;
    end;
    Releaser.WaitFor;
  finally
    Releaser.Free;
  end;

  { Sweep the remaining churn garbage; the ledger must balance to the byte. }
  GC.Collect;
  Expect<Int64>(GC.BytesAllocated).ToBe(BaselineBytes);
end;

procedure TTestGarbageCollector.TestEachThreadHasItsOwnCollector;
var
  MainCollector: TGarbageCollector;
  MainCount: Integer;
  Worker: TOwnCollectorThread;
begin
  MainCollector := TGarbageCollector.Instance;
  MainCount := MainCollector.ManagedObjectCount;
  Worker := TOwnCollectorThread.Create(True);
  try
    Worker.Start;
    Worker.WaitFor;
    Expect<Boolean>(Worker.NoneAtStart).ToBe(True);
    Expect<Boolean>(Assigned(Worker.OwnAfterInitialize)).ToBe(True);
    Expect<Boolean>(Worker.OwnAfterInitialize = MainCollector).ToBe(False);
    Expect<Boolean>(Worker.MirrorAfterInitialize).ToBe(True);
    Expect<Integer>(Worker.OwnCountGrowth).ToBe(1);
    Expect<Boolean>(Worker.NoneAfterShutdown).ToBe(True);
  finally
    Worker.Free;
  end;

  // The worker's allocation, Initialize and Shutdown left this thread's
  // collector alone, and this thread still allocates into it.
  Expect<Boolean>(TGarbageCollector.Instance = MainCollector).ToBe(True);
  Expect<Boolean>(GThreadPolls.Collector = Pointer(MainCollector)).ToBe(True);
  Expect<Integer>(MainCollector.ManagedObjectCount).ToBe(MainCount);
  TGocciaNumberLiteralValue.Create(2.5);
  Expect<Integer>(MainCollector.ManagedObjectCount).ToBe(MainCount + 1);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestGarbageCollector.Create('GarbageCollector'));
    RunGocciaTests;
  finally
    TGarbageCollector.Shutdown;
  end;
  ExitCode := TestResultToExitCode;
end.
