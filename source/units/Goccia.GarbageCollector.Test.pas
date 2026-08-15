program Goccia.GarbageCollector.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  Goccia.GarbageCollector,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  TChildManaged = class(TGCManagedObject)
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

  TTestGarbageCollector = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestCollectYoungTracesOldToYoungReferences;
    procedure TestReservationCollectsAndRetries;
    procedure TestReservationCollectsBeyondPressureReserve;
    procedure TestReservationRefusesWhatCollectionCannotFit;
    procedure TestRepeatedRefusalCollectsOnlyOnce;
  end;

var
  GParentDestructorCount: Integer;
  GChildDestructorCount: Integer;

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

procedure TParentManaged.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FChild) then
    FChild.MarkReferences;
end;

procedure TTestGarbageCollector.SetupTests;
begin
  Test('CollectYoung traces references from old rooted objects',
    TestCollectYoungTracesOldToYoungReferences);
  Test('External reservation collects and retries',
    TestReservationCollectsAndRetries);
  Test('External reservation collects past the pressure reserve',
    TestReservationCollectsBeyondPressureReserve);
  Test('External reservation still refuses what a collection cannot fit',
    TestReservationRefusesWhatCollectionCannotFit);
  Test('Repeated refusal of the same size collects only once',
    TestRepeatedRefusalCollectsOnlyOnce);
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
