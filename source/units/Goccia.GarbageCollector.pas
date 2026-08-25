unit Goccia.GarbageCollector;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  CriticalSections,
  HashMap,
  MemoryDetection;

type
  TGarbageCollector = class;

  // A live object that contributes roots the collector cannot otherwise see.
  // Registration is driven by AfterConstruction/BeforeDestruction so it happens
  // once per instance regardless of which constructor ran, including subclass
  // constructors that do not chain to an inherited one. TInterfacedObject stays
  // the ancestor so descendants keep the ancestry they already had.
  TGCRootSource = class(TInterfacedObject)
  private
    // Position in the owner's root-source list, for O(1) unregistration.
    FRootSourceIndex: Integer;
    // The collector this instance registered with. Thread-local collectors mean
    // an instance must unregister from the same one, and a collector that is
    // destroyed first nils this so unregistration cannot reach freed memory.
    FRootSourceOwner: TGarbageCollector;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure MarkRootReferences; virtual; abstract;
    // The collector this source is still registered with, or nil once that
    // collector has been destroyed.
    //
    // A holder that caches a root source across engines has to key on this
    // rather than on the collector it remembers separately. A
    // Shutdown/Initialize pair can put the next thread-local collector at the
    // address the previous one occupied, and a bare pointer compare against a
    // remembered address then reports "same collector" for a source that is
    // registered with the dead one — so the source is never rebuilt and
    // nothing it publishes is ever marked again. Reading it here cannot report
    // a stale match: the collector's destructor nils this field on every
    // source it still owns.
    function RegisteredCollector: TGarbageCollector;
  end;

  TGCManagedObject = class
  private
    FGCMark: Cardinal;
    FGCIndex: Integer;
    function GetGCMarked: Boolean;
    procedure SetGCMarked(const AValue: Boolean);
  public
    procedure BeforeDestruction; override;
    class procedure AdvanceMark; static; {$IFDEF FPC}inline;{$ENDIF}
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
  // Backing store for the active-root stack only. A bare array rather than
  // TGCManagedObjectList because that stack is pushed and popped once per
  // rooted evaluator temporary: TObjectList pays a virtual Notify dispatch per
  // Add and per Delete, and Delete additionally moves the tail, none of which
  // buys anything for a non-owning LIFO stack whose element type is a plain
  // class reference.
  TGCManagedObjectArray = array of TGCManagedObject;
  TGCRootSourceList = TList<TGCRootSource>;
  TGCObjectSet = THashMap<TGCManagedObject, Boolean>;
  TGCObjectRefCounts = THashMap<TGCManagedObject, Integer>;

  // Initialize stack-local roots with InitializeTempRoot before first use.
  TGocciaTempRoot = record
    ObjectValue: TGCManagedObject;
    Added: Boolean;
  end;

  // A stack-local group of GC roots. Initialize before first use, Add every
  // value that must survive a collecting safe point, and Clear in a finally so
  // the exception path is covered. Add tolerates nil and tolerates the same
  // object twice, which is what makes it the right tool for evaluator
  // temporaries where two locals can hold one object.
  //
  // Release is a count rollback, not a per-entry pop: Clear releases exactly
  // the top FCount entries of the collector's active-root stack in one
  // assignment. That is what the pop loop it replaces did, entry for entry, so
  // frames that share a base depth keep behaving as they do today — in the
  // promise combinators the inner per-iteration frame is Initialized before
  // the outer frame has pushed anything, so its recorded base depth sits
  // below the outer frame's entries, and releasing to that base depth instead
  // would let the inner frame's Clear take the outer frame's roots with it.
  //
  // FBaseDepth is therefore a floor, not a target: Clear never unwinds past
  // the depth the frame was Initialized at, so a frame cannot eat an enclosing
  // frame's roots even if the stack is left unbalanced beneath it. Clear is
  // idempotent — the second call has no entries to release and returns without
  // touching the stack, which is what makes clearing a frame in an inner
  // finally and again in an outer one safe during exception unwind.
  TGocciaActiveRootFrame = record
  private
    // Resolved once in Initialize: Add runs per rooted temporary on the
    // property-store path, and re-reading the collector threadvar there costs
    // more than the push itself. nil means "no collector on this thread", and
    // the frame stays a no-op for its whole lifetime.
    //
    // Invariant: a frame must not outlive the collector it was Initialized
    // against. Clear dereferences this cached pointer, so a frame still live
    // across TGarbageCollector.Shutdown would touch freed memory — and an
    // Assigned check could not save it, because the stale pointer stays
    // non-nil. Every current Shutdown site (thread runtime teardown, app
    // mains, test suite teardowns) runs with no frame live; keep it that way.
    FCollector: TGarbageCollector;
    FBaseDepth: Integer;
    FCount: Integer;
  public
    procedure Initialize; {$IFDEF FPC}inline;{$ENDIF}
    procedure Add(const AObject: TGCManagedObject);
    procedure Clear;
  end;

  TGarbageCollector = class
  private
    FManagedObjects: TGCManagedObjectList;
    FRootSources: TGCRootSourceList;
    FPinnedObjects: TGCObjectSet;
    FTempRoots: TGCObjectSet;
    FQueuedRoots: TGCObjectRefCounts;
    FKeptObjects: TGCObjectSet;
    FRootObjects: TGCObjectSet;
    FActiveRootStack: TGCManagedObjectArray;
    FActiveRootCount: Integer;
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

    // Short-lived leaf lock for the byte counters a FOREIGN thread can drive:
    // FBytesAllocated, FExternalBytes, FForcedCollectFloor and
    // FPeakBytesAllocated. The one cross-thread entry point into a collector
    // is ReleaseExternalBytes through an error object's reserving-collector
    // pointer (TGocciaErrorObjectValue.Destroy); everything else reaches a
    // collector through the thread-local Instance, so FManagedObjects and the
    // root sets are owner-thread-confined and need no lock at all. Keeping the
    // counters on their own per-collector lock is what lets one worker's full
    // mark-and-sweep run without stalling any other worker's per-allocation
    // RegisterObject/UnregisterObject — and keeps those allocations off a
    // shared cache line entirely: each worker contends only with the rare
    // cross-thread release actually aimed at its collector.
    //
    // Readers: every write to these counters goes through this lock, so a
    // reader only has to avoid observing a half-applied cross-thread write.
    // On 64-bit an aligned Int64 load cannot tear, so readers take nothing and
    // BytesAllocated stays a bare field load on the per-allocation path; on
    // 32-bit ReleaseExternalBytes splits its write into two stores and readers
    // go through GetBytesAllocated, which takes this lock. Code that already
    // holds the lock reads the fields directly — GetBytesAllocated must never
    // be called from inside a section.
    //
    // Lock order: the global GCCollectLock may be held when taking an
    // accounting lock (a sweep's aggregate byte settle, a swept destructor's
    // cross-collector release). NEVER take GCCollectLock — or a second
    // accounting lock — while holding one: the reserve path drops this lock
    // before it collects (TryReserveExternalBytes ->
    // TryCollectForLimitedBytes), and sweeps settle their bytes outside the
    // Recycle calls, for exactly this reason. Nothing may run guest code,
    // destructors, or any other lock acquisition inside an accounting section.
    //
    // Lifetime: a collector field, freed in Destroy. A cross-thread release
    // entering it is safe on the same terms as the field accesses it guards —
    // the GC lifecycle invariant that an error object is freed before its
    // reserving collector shuts down (see
    // TGocciaErrorObjectValue.FErrorSourceExcerptCollector); a lock that
    // outlived the collector could not protect the freed fields anyway.
    FAccountingLock: TGocciaCriticalSection;
    FBytesAllocated: Int64;
    FExternalBytes: Int64;
    FExternalBytesAllocatedSinceGC: Int64;
    FPeakBytesAllocated: Int64;
    FTotalBytesAllocated: Int64;
    FMaxBytes: Int64;
    FSuggestedMaxBytes: Int64;
    FMemoryLimitFiring: Boolean;
    FExternalPressurePending: Boolean;
    FMemoryPressureCountdown: PInteger;
    // Bytes still live after the most recent forced collection that failed to
    // make room, or -1 when no such observation is on record. Absent an
    // intervening collection or counter drop, no collection gets below the
    // level the last forced one left, so this bounds what a repeat attempt
    // could possibly reclaim — a bounded heuristic, not an invariant: objects
    // that merely become unreachable move no counter, and the guest-side
    // escape is Goccia.gc(), which clears the floor. Shared by the charged
    // reservation path and the uncharged growth gate, for the reason
    // ShouldForceLimitCollection states.
    FForcedCollectFloor: Int64;

    {$IFDEF GC_TIMING}
    FTotalMarkTimeNs: Int64;
    FTotalSweepTimeNs: Int64;
    FTotalGCTimeNs: Int64;
    FMaxMarkTimeNs: Int64;
    FMaxSweepTimeNs: Int64;
    {$ENDIF}

    function GetManagedObjectCount: Integer;
    function GetWatermark: Integer; {$IFDEF FPC}inline;{$ENDIF}
    // Untorn read of the live-byte total for callers that hold no lock — see
    // the reader rule at FAccountingLock. Never call it while holding the
    // accounting lock; read FBytesAllocated directly there.
    function GetBytesAllocated: Int64; {$IFDEF FPC}inline;{$ENDIF}
    // Pressure predicate over an already-read live total, so the charge path
    // can decide from the value it just committed instead of re-reading it
    // through the locked accessor with the lock held.
    function NeedsMemoryPressureCollection(
      const ABytesAllocated: Int64): Boolean; overload;
    procedure ClearActiveRootEntries(const AObject: TGCManagedObject);
    procedure GrowActiveRootStack;
    // Reads the live total and the forced-collect floor, so the caller must
    // hold FAccountingLock across the decision it feeds.
    function ShouldForceLimitCollection(const ABytes: Int64): Boolean;
    // The one --max-memory fit predicate (overflow guard + ceiling test).
    // Caller must hold FAccountingLock ("Locked" suffix).
    function FitsWithinLimitLocked(const ABytes: Int64): Boolean; {$IFDEF FPC}inline;{$ENDIF}
    // Fit-test-and-charge in one atomic step: applies the reservation to the
    // byte totals and latches external pressure only when ABytes fits, so no
    // caller can commit a charge past the ceiling on a stale earlier test.
    // Caller must hold FAccountingLock ("Locked" suffix).
    function TryChargeExternalBytesLocked(const ABytes: Int64): Boolean;
  protected
    procedure MarkRoots; virtual;
    procedure TraceWeakReferences;
    procedure SweepWeakReferences;
    procedure SweepObjects;
  public
    class function Instance: TGarbageCollector; {$IFDEF FPC}inline;{$ENDIF}
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterObject(const AObject: TGCManagedObject);
    procedure UnregisterObject(const AObject: TGCManagedObject);
    procedure RegisterRootSource(const ASource: TGCRootSource);
    procedure UnregisterRootSource(const ASource: TGCRootSource);
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
    procedure PopActiveRoot; {$IFDEF FPC}inline;{$ENDIF}

    // Release the top ACount active roots in one step, never unwinding below
    // AFloorDepth. Equivalent to ACount PopActiveRoot calls at O(1) instead of
    // O(ACount); the floor is the guard described at TGocciaActiveRootFrame.
    procedure ReleaseActiveRoots(const ACount: Integer;
      const AFloorDepth: Integer);

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

    function NeedsMemoryPressureCollection: Boolean; overload;

    // Collects when pressure has been latched by an external reservation or
    // when the live set has crossed the reserve below the ceiling. AForce
    // skips that heuristic and always collects; it exists for the last-resort
    // site in TryReserveExternalBytes, which must not refuse a reservation
    // before a collection has actually been attempted.
    procedure CollectForMemoryPressure(const AProtect: TGCManagedObject;
      const AForce: Boolean = False);

    // Young-generation collection: marks from real roots, then sweeps
    // only objects allocated after AWatermark. Old objects are retained
    // even when unmarked, but they are not pre-marked because old objects
    // can acquire young references between the watermark capture and this
    // collection. Tracing through old roots keeps those young objects live.
    procedure CollectYoung(const AWatermark: Integer);
    procedure ResetPeakBytesAllocated;

    // Forces one collection when a request of ABytes does not currently fit
    // under MaxBytes and a collection could plausibly change that, then
    // re-tests the fit. Returns True only when ABytes fits afterwards.
    //
    // Charges nothing: the charged path (TryReserveExternalBytes) adds the
    // bytes itself once this returns True, and the uncharged growth gate
    // (Goccia.MemoryLimit) permits the growth without ever charging.
    // False also covers the shapes no collection could help — a request
    // larger than the whole budget, a repeat of a request the last forced
    // collection already refused, and re-entrant calls from inside the
    // collector — none of which walk the heap.
    //
    // AProtect is for callers whose stack-held value nothing else roots. The
    // growth gate passes nil deliberately: its callers have already opened a
    // TGocciaActiveRootFrame over the store's owner and pending value before
    // consulting the budget, so the window is the caller's, not this one's.
    // See ADR 0110.
    function TryCollectForLimitedBytes(const ABytes: Int64;
      const AProtect: TGCManagedObject = nil): Boolean;
    function TryReserveExternalBytes(const ABytes: Int64;
      const AProtect: TGCManagedObject = nil): Boolean;
    procedure ReleaseExternalBytes(const ABytes: Int64);
    function ExchangeMemoryPressureCountdown(
      const ACountdown: PInteger): PInteger;

    {$IFDEF GC_TIMING}
    procedure PrintTimingSummary;
    {$ENDIF}

    property Enabled: Boolean read FEnabled write FEnabled;
    property Threshold: Integer read FGCThreshold write FGCThreshold;
    property TotalCollected: Int64 read FTotalCollected;
    property TotalCollections: Integer read FTotalCollections;
    property ManagedObjectCount: Integer read GetManagedObjectCount;

    // Current depth of the active-root stack. Captured by
    // TGocciaActiveRootFrame.Initialize as the floor its Clear may not
    // unwind past.
    property ActiveRootDepth: Integer read FActiveRootCount;

    // Byte-level memory tracking. BytesAllocated is the approximate
    // number of bytes currently tracked by the GC (InstanceSize per
    // registered object). Set MaxBytes to a positive value to impose
    // a ceiling; allocations that exceed it raise a RangeError.
    //
    // BytesAllocated reads through an accessor because it is the one counter a
    // foreign thread writes (ReleaseExternalBytes). The peak and the lifetime
    // total are written only by the owning thread, so no reader of theirs can
    // catch a half-applied write and they stay direct field reads.
    property BytesAllocated: Int64 read GetBytesAllocated;
    property PeakBytesAllocated: Int64 read FPeakBytesAllocated;
    property TotalBytesAllocated: Int64 read FTotalBytesAllocated;
    property MaxBytes: Int64 read FMaxBytes write FMaxBytes;
    property SuggestedMaxBytes: Int64 read FSuggestedMaxBytes;
    property MemoryLimitFiring: Boolean read FMemoryLimitFiring write FMemoryLimitFiring;
    property ExternalPressurePending: Boolean
      read FExternalPressurePending;

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
  MEMORY_PRESSURE_COLLECTION_MAX_RESERVE = 16 * 1024 * 1024;
  EXTERNAL_MEMORY_PRESSURE_ALLOCATION_INTERVAL = 256 * 1024 * 1024;

function DetectDefaultMaxBytes: Int64;
procedure InitializeTempRoot(var ARoot: TGocciaTempRoot); {$IFDEF FPC}inline;{$ENDIF}
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

const
  // Slots pre-allocated for the active-root stack, and the unit it doubles
  // from. Deep enough that ordinary evaluator nesting never reallocates.
  ACTIVE_ROOT_STACK_INITIAL_CAPACITY = 256;

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
  // Global collection coordination only. Collect and CollectYoung serialize
  // across ALL collectors because the mark epoch (GCCurrentMark) is
  // process-global and some intrinsic objects are shared: two concurrent
  // collections advancing the epoch mid-mark would unmark each other's live
  // sets and sweep reachable objects. Recursive, because a sweep destructor
  // can re-enter collection paths (Collect -> SweepObjects -> Recycle ->
  // destructor). The byte counters are NOT guarded here — they live under the
  // per-collector FAccountingLock (see its declaration for the lock order).
  GCCollectLock: TGocciaCriticalSection;

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

{ TGCRootSource }

procedure TGCRootSource.AfterConstruction;
begin
  inherited;
  FRootSourceIndex := -1;
  FRootSourceOwner := TGarbageCollector.Instance;
  if Assigned(FRootSourceOwner) then
    FRootSourceOwner.RegisterRootSource(Self);
end;

procedure TGCRootSource.BeforeDestruction;
begin
  if Assigned(FRootSourceOwner) then
  begin
    FRootSourceOwner.UnregisterRootSource(Self);
    FRootSourceOwner := nil;
  end;
  inherited;
end;

function TGCRootSource.RegisteredCollector: TGarbageCollector;
begin
  Result := FRootSourceOwner;
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
  FCollector := TGarbageCollector.Instance;
  if Assigned(FCollector) then
    FBaseDepth := FCollector.ActiveRootDepth
  else
    FBaseDepth := 0;
  FCount := 0;
end;

procedure TGocciaActiveRootFrame.Add(const AObject: TGCManagedObject);
begin
  if not Assigned(AObject) or not Assigned(FCollector) then
    Exit;
  FCollector.PushActiveRoot(AObject);
  Inc(FCount);
end;

procedure TGocciaActiveRootFrame.Clear;
begin
  // FCount > 0 implies a collector: Add is the only thing that increments it,
  // and it only does so after pushing onto an assigned collector.
  if FCount <= 0 then
    Exit;
  FCollector.ReleaseActiveRoots(FCount, FBaseDepth);
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
  CriticalSectionInit(FAccountingLock);
  FManagedObjects := TGCManagedObjectList.Create(False);
  FRootSources := TGCRootSourceList.Create;
  FPinnedObjects := TGCObjectSet.Create;
  FTempRoots := TGCObjectSet.Create;
  FQueuedRoots := TGCObjectRefCounts.Create;
  FKeptObjects := TGCObjectSet.Create;
  FRootObjects := TGCObjectSet.Create;
  SetLength(FActiveRootStack, ACTIVE_ROOT_STACK_INITIAL_CAPACITY);
  FActiveRootCount := 0;
  FWeakContainers := TGCObjectSet.Create;
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
  FBytesAllocated := 0;
  FExternalBytes := 0;
  FExternalBytesAllocatedSinceGC := 0;
  FPeakBytesAllocated := 0;
  FTotalBytesAllocated := 0;
  FSuggestedMaxBytes := DetectDefaultMaxBytes;
  FMaxBytes := FSuggestedMaxBytes;
  FMemoryLimitFiring := False;
  FExternalPressurePending := False;
  FMemoryPressureCountdown := nil;
  FForcedCollectFloor := -1;
  {$IFDEF GC_TIMING}
  FTotalMarkTimeNs := 0;
  FTotalSweepTimeNs := 0;
  FTotalGCTimeNs := 0;
  FMaxMarkTimeNs := 0;
  FMaxSweepTimeNs := 0;
  {$ENDIF}
end;

destructor TGarbageCollector.Destroy;
var
  I: Integer;
begin
  {$IFDEF GC_TIMING}
  PrintTimingSummary;
  {$ENDIF}
  // Root sources are not owned and can outlive the collector (a pooled argument
  // collection freed during engine tear-down, for instance). Drop their
  // back-pointer so their destructors do not unregister into freed memory.
  for I := 0 to FRootSources.Count - 1 do
  begin
    FRootSources[I].FRootSourceOwner := nil;
    FRootSources[I].FRootSourceIndex := -1;
  end;
  FRootSources.Free;
  FManagedObjects.Free;
  FPinnedObjects.Free;
  FTempRoots.Free;
  FQueuedRoots.Free;
  FKeptObjects.Free;
  FRootObjects.Free;
  FActiveRootCount := 0;
  SetLength(FActiveRootStack, 0);
  FWeakContainers.Free;
  CriticalSectionDone(FAccountingLock);
  inherited;
end;

procedure TGarbageCollector.RegisterObject(
  const AObject: TGCManagedObject);
begin
  // The list and the allocation counter are owner-thread-confined; the byte
  // totals race a cross-thread ReleaseExternalBytes and take the accounting
  // lock — see FAccountingLock for the full confinement and ordering rules.
  AObject.GCIndex := FManagedObjects.Count;
  FManagedObjects.Add(AObject);
  Inc(FAllocationsSinceLastGC);
  CriticalSectionEnter(FAccountingLock);
  try
    Inc(FBytesAllocated, AObject.InstanceSize);
    Inc(FTotalBytesAllocated, AObject.InstanceSize);
    if FBytesAllocated > FPeakBytesAllocated then
      FPeakBytesAllocated := FBytesAllocated;
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
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

  // The list and root sets are owner-thread-confined (this runs either on the
  // owner thread via BeforeDestruction -> the thread-local Instance, or inside
  // this collector's own sweep), so they need no lock; the Dec and the floor
  // invalidation it feeds race a cross-thread ReleaseExternalBytes and take
  // the accounting lock — see FAccountingLock.
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
    CriticalSectionEnter(FAccountingLock);
    try
      Dec(FBytesAllocated, AObject.InstanceSize);
      // Same invalidation as ReleaseExternalBytes: dropping below the recorded
      // floor means that observation no longer bounds what a collection could
      // reclaim and must not go on suppressing one.
      if (FForcedCollectFloor >= 0) and (FBytesAllocated < FForcedCollectFloor) then
        FForcedCollectFloor := -1;
    finally
      CriticalSectionLeave(FAccountingLock);
    end;
  end;
end;

procedure TGarbageCollector.RegisterRootSource(
  const ASource: TGCRootSource);
begin
  ASource.FRootSourceIndex := FRootSources.Count;
  FRootSources.Add(ASource);
end;

// Removal swaps the last entry into the vacated slot rather than leaving a nil
// behind. Root sources are created and destroyed far more often than managed
// objects — one per builtin invocation — and nothing depends on their order or
// on a stable index across a collection, so there is no compaction pass to
// piggyback on and nil slots would grow without bound while automatic
// collection is disabled (as it is throughout bytecode execution).
procedure TGarbageCollector.UnregisterRootSource(
  const ASource: TGCRootSource);
var
  Idx, LastIdx: Integer;
  Last: TGCRootSource;
begin
  if not Assigned(FRootSources) then
    Exit;

  Idx := ASource.FRootSourceIndex;
  if (Idx < 0) or (Idx >= FRootSources.Count) or
     (FRootSources[Idx] <> ASource) then
    Exit;

  LastIdx := FRootSources.Count - 1;
  if Idx <> LastIdx then
  begin
    Last := FRootSources[LastIdx];
    FRootSources[Idx] := Last;
    Last.FRootSourceIndex := Idx;
  end;
  FRootSources.Delete(LastIdx);
  ASource.FRootSourceIndex := -1;
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
  for I := FActiveRootCount - 1 downto 0 do
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

procedure TGarbageCollector.GrowActiveRootStack;
begin
  if Length(FActiveRootStack) = 0 then
    SetLength(FActiveRootStack, ACTIVE_ROOT_STACK_INITIAL_CAPACITY)
  else
    SetLength(FActiveRootStack, Length(FActiveRootStack) * 2);
end;

procedure TGarbageCollector.PushActiveRoot(
  const AObject: TGCManagedObject);
begin
  if FActiveRootCount = Length(FActiveRootStack) then
    GrowActiveRootStack;
  FActiveRootStack[FActiveRootCount] := AObject;
  Inc(FActiveRootCount);
end;

procedure TGarbageCollector.PopActiveRoot;
begin
  if FActiveRootCount > 0 then
    Dec(FActiveRootCount);
end;

procedure TGarbageCollector.ReleaseActiveRoots(const ACount: Integer;
  const AFloorDepth: Integer);
var
  Target: Integer;
begin
  if ACount <= 0 then
    Exit;
  Target := FActiveRootCount - ACount;
  if Target < AFloorDepth then
    Target := AFloorDepth;
  if Target < 0 then
    Target := 0;
  if Target < FActiveRootCount then
    FActiveRootCount := Target;
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

  for I := 0 to FActiveRootCount - 1 do
    if Assigned(FActiveRootStack[I]) then
      FActiveRootStack[I].MarkReferences;

  for I := 0 to FRootSources.Count - 1 do
    FRootSources[I].MarkRootReferences;
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
  FreedBytes: Int64;
  Obj: TGCManagedObject;
begin
  Collected := 0;
  WriteIdx := 0;
  FreedBytes := 0;

  // Freed bytes accumulate locally and settle under the accounting lock once,
  // in the finally below, instead of Dec-ing per object: Recycle runs
  // destructors, and a swept error object's destructor takes an accounting
  // lock to release its excerpt bytes against ANOTHER thread's collector — so
  // no accounting section may be open across a Recycle call. A nested
  // destructor freeing a still-registered object goes through the full
  // UnregisterObject path (its own locked Dec) and nils its slot, which this
  // loop then skips — no byte is counted twice. InstanceSize must be read
  // before Recycle, while the object is still alive. The settle sits in a
  // finally so a destructor exception cannot strand the bytes of the objects
  // already recycled — the old per-object Dec kept completed frees accounted
  // on that path, and this must too.
  try
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
        Inc(FreedBytes, Obj.InstanceSize);
        Obj.GCIndex := -1;
        Obj.Recycle;
        Inc(Collected);
      end;
    end;
  finally
    if FreedBytes > 0 then
    begin
      CriticalSectionEnter(FAccountingLock);
      try
        Dec(FBytesAllocated, FreedBytes);
      finally
        CriticalSectionLeave(FAccountingLock);
      end;
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
  CriticalSectionEnter(GCCollectLock);
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
      FExternalBytesAllocatedSinceGC := 0;
      FExternalPressurePending := False;
      // This collection supersedes whatever the last forced one observed, so
      // the next failing reservation is entitled to force again. The floor is
      // part of the accounting family a cross-thread release reads and
      // conditionally invalidates, so the write goes under the accounting
      // lock (either order with a concurrent conditional -1 lands on -1).
      CriticalSectionEnter(FAccountingLock);
      try
        FForcedCollectFloor := -1;
      finally
        CriticalSectionLeave(FAccountingLock);
      end;

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
    CriticalSectionLeave(GCCollectLock);
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
    PushActiveRoot(AProtect);
  try
    Collect;
  finally
    if Assigned(AProtect) then
      PopActiveRoot;
  end;
end;

function TGarbageCollector.GetBytesAllocated: Int64;
begin
  {$IF SizeOf(Pointer) >= 8}
  Result := FBytesAllocated;
  {$ELSE}
  // A 32-bit target splits the 64-bit write in a cross-thread
  // ReleaseExternalBytes into two stores, and a load that lands between them
  // yields a total no memory-limit decision may be made on. No 64-bit
  // interlocked load is available: FPC 3.2.2 declares those only under CPU64,
  // and CI builds i386-win32. Nothing between the two calls can raise, so the
  // load needs no exception frame — which also keeps this accessor inlinable.
  CriticalSectionEnter(FAccountingLock);
  Result := FBytesAllocated;
  CriticalSectionLeave(FAccountingLock);
  {$ENDIF}
end;

function TGarbageCollector.NeedsMemoryPressureCollection: Boolean;
begin
  // Guards before the counter read: on 32-bit GetBytesAllocated takes the
  // accounting lock, and the periodic VM/interpreter pressure polls must stay
  // free when no limit is set or a collection is already underway.
  if (FMaxBytes <= 0) or FCollecting or FMemoryLimitFiring then
    Exit(False);
  Result := NeedsMemoryPressureCollection(GetBytesAllocated);
end;

function TGarbageCollector.NeedsMemoryPressureCollection(
  const ABytesAllocated: Int64): Boolean;
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

  Result := ABytesAllocated >= (FMaxBytes - Reserve);
end;

procedure TGarbageCollector.CollectForMemoryPressure(
  const AProtect: TGCManagedObject; const AForce: Boolean);
var
  WasFiring: Boolean;
begin
  if not AForce and not FExternalPressurePending and
     not NeedsMemoryPressureCollection then
    Exit;

  if Assigned(AProtect) then
    PushActiveRoot(AProtect);
  WasFiring := FMemoryLimitFiring;
  FMemoryLimitFiring := True;
  try
    Collect;
  finally
    FMemoryLimitFiring := WasFiring;
    if Assigned(AProtect) then
      PopActiveRoot;
  end;
end;

procedure TGarbageCollector.CollectYoung(const AWatermark: Integer);
var
  I, WriteIdx, Collected: Integer;
  FreedBytes: Int64;
  Obj: TGCManagedObject;
  EffectiveWatermark: Integer;
begin
  CriticalSectionEnter(GCCollectLock);
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
      FreedBytes := 0;

      // Deferred settle in a finally — same reasoning as SweepObjects:
      // Recycle may take an accounting lock for a cross-collector release,
      // so none may be held across it, and a destructor exception must not
      // strand the bytes of the objects already recycled.
      try
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
            Inc(FreedBytes, Obj.InstanceSize);
            Obj.GCIndex := -1;
            Obj.Recycle;
            Inc(Collected);
          end;
        end;
      finally
        if FreedBytes > 0 then
        begin
          CriticalSectionEnter(FAccountingLock);
          try
            Dec(FBytesAllocated, FreedBytes);
          finally
            CriticalSectionLeave(FAccountingLock);
          end;
        end;
      end;

      FManagedObjects.Count := WriteIdx;
      if FManagedObjects.Capacity > 4 * WriteIdx + 256 then
        FManagedObjects.Capacity := WriteIdx + (WriteIdx div 2);
      FAllocationsSinceLastGC := 0;
      FExternalBytesAllocatedSinceGC := 0;
      FExternalPressurePending := False;
      // Same locked reset as Collect: either order with a concurrent
      // conditional invalidation lands on -1.
      CriticalSectionEnter(FAccountingLock);
      try
        FForcedCollectFloor := -1;
      finally
        CriticalSectionLeave(FAccountingLock);
      end;
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
    CriticalSectionLeave(GCCollectLock);
  end;
end;

procedure TGarbageCollector.ResetPeakBytesAllocated;
begin
  // Reads FBytesAllocated and writes the sibling peak total, so take the same
  // lock the mutators use: a concurrent cross-thread ReleaseExternalBytes would
  // otherwise let this observe a torn 64-bit total (on 32-bit targets) or race
  // the peak update in RegisterObject/TryReserveExternalBytes.
  CriticalSectionEnter(FAccountingLock);
  try
    FPeakBytesAllocated := FBytesAllocated;
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
end;

function TGarbageCollector.ShouldForceLimitCollection(
  const ABytes: Int64): Boolean;
begin
  // Last resort: a reservation — or a gated growth — is only refused once a
  // collection has actually been attempted. The pressure heuristic cannot
  // decide this on its own: it triggers at a fixed reserve below the ceiling,
  // so a request larger than that reserve used to be refused with reclaimable
  // garbage still on the heap whenever the live set sat below the trigger.
  //
  // Two shapes are refused without walking the heap, because for them no
  // collection could change the answer. A request larger than the whole budget
  // never fits. And once a forced collection has left FForcedCollectFloor
  // bytes live, no later collection gets the heap below that level, so a
  // request that does not fit beside the floor cannot be made to fit either —
  // which is what keeps a guest that retries a refused request at O(1) per
  // attempt instead of a full mark-and-sweep each time. The floor is per
  // request size, so a smaller request that the floor does not rule out still
  // forces its collection.
  //
  // The floor is shared by both refusal paths on purpose. It records a fact
  // about the heap ("the last forced collection left this many bytes live"),
  // not about the caller, and both paths force the same full collection and
  // then apply the same fit test, so a level that defeated one defeats the
  // other at the same request size. A second, gate-private floor would only
  // buy each path the right to re-learn what the other just proved.
  Result := not FCollecting and not FMemoryLimitFiring and
    (FMaxBytes > 0) and (ABytes <= FMaxBytes) and
    (FBytesAllocated <= High(Int64) - ABytes) and
    (FBytesAllocated + ABytes > FMaxBytes) and
    ((FForcedCollectFloor < 0) or
     (FForcedCollectFloor <= FMaxBytes - ABytes));
end;

function TGarbageCollector.TryCollectForLimitedBytes(const ABytes: Int64;
  const AProtect: TGCManagedObject): Boolean;
var
  ShouldForce: Boolean;
begin
  // Only the owner thread reaches this method (via TryReserveExternalBytes or
  // directly from RequireNativeBytes in Goccia.MemoryLimit), so the only
  // concurrent party is a cross-thread ReleaseExternalBytes — which mutates
  // FBytesAllocated and FForcedCollectFloor under the accounting lock. Each
  // counter read/write sequence here therefore takes the accounting lock; an
  // unlocked fit test could see a torn 64-bit total (on 32-bit targets), and
  // an unlocked floor write could resurrect a level a release just invalidated
  // back to -1 — the fit test and the floor write share one section so a
  // release lands either before the test (and is seen) or after the write (and
  // its conditional invalidation then clears the floor if the drop warrants
  // it). The accounting lock is NOT held across the collection itself: Collect
  // takes the collect lock, and the lock order forbids collect-under-accounting
  // (see FAccountingLock). A release sneaking in between the predicate and the
  // collection only frees room — the post-collection fit test decides.
  CriticalSectionEnter(FAccountingLock);
  try
    // A cross-thread release landing after the caller's failed fit test can
    // have made the request fit with no collection needed. The force predicate
    // reads that as "nothing to force", which the callers would take for a
    // refusal — so grant the now-fitting request here instead: the contract is
    // "True when ABytes fits afterwards", and it already does. (Under the old
    // single-lock design this interleaving was unreachable; the split makes
    // the explicit grant necessary.)
    if FitsWithinLimitLocked(ABytes) then
      Exit(True);
    ShouldForce := ShouldForceLimitCollection(ABytes);
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
  if not ShouldForce then
    Exit(False);
  CollectForMemoryPressure(AProtect, True);
  CriticalSectionEnter(FAccountingLock);
  try
    Result := FitsWithinLimitLocked(ABytes);
    // Record the level this collection could not get below, so a retry of a
    // request it already refused skips the walk that just proved fruitless.
    // Collect clears this again — as do CollectYoung and the conditional
    // invalidations on any counter drop (ReleaseExternalBytes,
    // UnregisterObject) — so any ordinary collection re-arms forcing.
    // The per-request-size term is pinned by the damper test's third arm in
    // Goccia.GarbageCollector.Test.pas (a size-independent floor fails it).
    if not Result then
      FForcedCollectFloor := FBytesAllocated;
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
end;

function TGarbageCollector.FitsWithinLimitLocked(
  const ABytes: Int64): Boolean;
begin
  Result := (FBytesAllocated <= High(Int64) - ABytes) and
    ((FMaxBytes <= 0) or (FBytesAllocated + ABytes <= FMaxBytes));
end;

function TGarbageCollector.TryChargeExternalBytesLocked(
  const ABytes: Int64): Boolean;
begin
  Result := FitsWithinLimitLocked(ABytes);
  if not Result then
    Exit;
  Inc(FBytesAllocated, ABytes);
  Inc(FExternalBytes, ABytes);
  Inc(FExternalBytesAllocatedSinceGC, ABytes);
  Inc(FTotalBytesAllocated, ABytes);
  if FBytesAllocated > FPeakBytesAllocated then
    FPeakBytesAllocated := FBytesAllocated;
  if (FExternalBytesAllocatedSinceGC >=
      EXTERNAL_MEMORY_PRESSURE_ALLOCATION_INTERVAL) or
     NeedsMemoryPressureCollection(FBytesAllocated) then
  begin
    FExternalPressurePending := True;
    if Assigned(FMemoryPressureCountdown) then
      FMemoryPressureCountdown^ := 0;
  end;
end;

function TGarbageCollector.TryReserveExternalBytes(
  const ABytes: Int64; const AProtect: TGCManagedObject): Boolean;
begin
  if ABytes <= 0 then
    Exit(True);
  // External-byte accounting is a read-modify-write on FBytesAllocated and
  // FExternalBytes that a cross-thread error destructor can drive concurrently
  // through ReleaseExternalBytes on this same (reserving) collector, so both
  // attempts run the atomic fit-test-and-charge under the accounting lock. The
  // collection retry sits BETWEEN the two locked sections rather than inside
  // one: Collect takes the collect lock, and the lock order forbids acquiring
  // it under the accounting lock (see FAccountingLock). The second attempt
  // re-tests rather than trusting the collection's verdict, so nothing that
  // moved the counters in the window — a concurrent release only frees room,
  // but a re-entrant same-thread charge from a sweep destructor would not —
  // can push the committed total past the ceiling.
  CriticalSectionEnter(FAccountingLock);
  try
    Result := TryChargeExternalBytesLocked(ABytes);
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
  if Result then
    Exit;
  if not TryCollectForLimitedBytes(ABytes, AProtect) then
    Exit(False);
  CriticalSectionEnter(FAccountingLock);
  try
    Result := TryChargeExternalBytesLocked(ABytes);
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
end;

procedure TGarbageCollector.ReleaseExternalBytes(const ABytes: Int64);
begin
  if ABytes <= 0 then
    Exit;
  // This is the one entry point a FOREIGN thread has into a collector: it runs
  // on the reserving collector, which may be owned by another thread (an error
  // object charged on thread A can be destroyed on thread B, which then
  // releases here against collector A). The accounting lock serializes it with
  // the owner's counter updates so the two never tear
  // FBytesAllocated/FExternalBytes — see FAccountingLock.
  CriticalSectionEnter(FAccountingLock);
  try
    if ABytes >= FExternalBytes then
    begin
      Dec(FBytesAllocated, FExternalBytes);
      FExternalBytes := 0;
    end
    else
    begin
      Dec(FExternalBytes, ABytes);
      Dec(FBytesAllocated, ABytes);
    end;
    // Released bytes take the heap below the level the last forced collection
    // observed, so that observation no longer bounds what a collection could
    // reclaim and must not go on suppressing one.
    if (FForcedCollectFloor >= 0) and (FBytesAllocated < FForcedCollectFloor) then
      FForcedCollectFloor := -1;
  finally
    CriticalSectionLeave(FAccountingLock);
  end;
end;

function TGarbageCollector.ExchangeMemoryPressureCountdown(
  const ACountdown: PInteger): PInteger;
begin
  Result := FMemoryPressureCountdown;
  FMemoryPressureCountdown := ACountdown;
  if FExternalPressurePending and Assigned(FMemoryPressureCountdown) then
    FMemoryPressureCountdown^ := 0;
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
  CriticalSectionInit(GCCollectLock);
  GCCurrentMark := 1;

finalization
  CriticalSectionDone(GCCollectLock);

end.
