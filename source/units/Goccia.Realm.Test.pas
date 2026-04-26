program Goccia.Realm.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Realm,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  // Plain TObject helper used to observe SetOwnedSlot lifetime semantics.
  // Increments GOwnedDestructorCount in its destructor so tests can verify
  // that the realm freed it.
  TCountingOwned = class
  public
    destructor Destroy; override;
  end;

  // Minimal TGCManagedObject used to populate slots without dragging in the
  // value system.  Increments GManagedDestructorCount in its destructor so
  // tests can verify that an unpinned object becomes collectible.
  TCountingManaged = class(TGCManagedObject)
  public
    destructor Destroy; override;
  end;

var
  GOwnedDestructorCount: Integer;
  GManagedDestructorCount: Integer;

destructor TCountingOwned.Destroy;
begin
  Inc(GOwnedDestructorCount);
  inherited;
end;

destructor TCountingManaged.Destroy;
begin
  Inc(GManagedDestructorCount);
  inherited;
end;

type
  TTestRealm = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestRegisterSlotReturnsDistinctIds;
    procedure TestRegisterOwnedSlotReturnsDistinctIds;
    procedure TestUnsetSlotReturnsNil;
    procedure TestHasSlotReflectsState;
    procedure TestSetSlotRoundtrip;
    procedure TestUnsetOwnedSlotReturnsNil;
    procedure TestSetOwnedSlotRoundtrip;
    procedure TestSetOwnedSlotReplacesPreviousAndFreesIt;
    procedure TestSetOwnedSlotSameValueIsNoOp;
    procedure TestDestructorFreesOwnedSlots;
    procedure TestDestructorUnpinsSlotObjects;
    procedure TestSlotIdGrowsArrayLazily;
    procedure TestThreadLocalityOfCurrentRealm;
  end;

procedure TTestRealm.SetupTests;
begin
  Test('RegisterRealmSlot returns distinct ids',
    TestRegisterSlotReturnsDistinctIds);
  Test('RegisterRealmOwnedSlot returns distinct ids',
    TestRegisterOwnedSlotReturnsDistinctIds);
  Test('GetSlot on an unset slot returns nil', TestUnsetSlotReturnsNil);
  Test('HasSlot returns false before set, true after set',
    TestHasSlotReflectsState);
  Test('SetSlot/GetSlot roundtrips the stored object', TestSetSlotRoundtrip);
  Test('GetOwnedSlot on an unset slot returns nil',
    TestUnsetOwnedSlotReturnsNil);
  Test('SetOwnedSlot/GetOwnedSlot roundtrips the stored object',
    TestSetOwnedSlotRoundtrip);
  Test('SetOwnedSlot replaces previous owner and frees it',
    TestSetOwnedSlotReplacesPreviousAndFreesIt);
  Test('SetOwnedSlot with the same value is a no-op',
    TestSetOwnedSlotSameValueIsNoOp);
  Test('Destructor frees every owned slot', TestDestructorFreesOwnedSlots);
  Test('Destructor unpins slot objects so the GC can collect them',
    TestDestructorUnpinsSlotObjects);
  Test('SetSlot grows the slot array when the id was registered later',
    TestSlotIdGrowsArrayLazily);
  Test('CurrentRealm is thread-local',
    TestThreadLocalityOfCurrentRealm);
end;

procedure TTestRealm.TestRegisterSlotReturnsDistinctIds;
var
  A, B: TGocciaRealmSlotId;
begin
  A := RegisterRealmSlot('test/slot/a');
  B := RegisterRealmSlot('test/slot/b');
  Expect<Boolean>(Integer(A) <> Integer(B)).ToBe(True);
  Expect<Boolean>(Integer(A) >= 0).ToBe(True);
  Expect<Boolean>(Integer(B) >= 0).ToBe(True);
end;

procedure TTestRealm.TestRegisterOwnedSlotReturnsDistinctIds;
var
  A, B: TGocciaRealmOwnedSlotId;
begin
  A := RegisterRealmOwnedSlot('test/owned/a');
  B := RegisterRealmOwnedSlot('test/owned/b');
  Expect<Boolean>(Integer(A) <> Integer(B)).ToBe(True);
  Expect<Boolean>(Integer(A) >= 0).ToBe(True);
  Expect<Boolean>(Integer(B) >= 0).ToBe(True);
end;

procedure TTestRealm.TestUnsetSlotReturnsNil;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmSlotId;
begin
  Slot := RegisterRealmSlot('test/unset/slot');
  Realm := TGocciaRealm.Create;
  try
    Expect<Boolean>(Realm.GetSlot(Slot) = nil).ToBe(True);
  finally
    Realm.Free;
  end;
end;

procedure TTestRealm.TestHasSlotReflectsState;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmSlotId;
  Obj: TCountingManaged;
begin
  Slot := RegisterRealmSlot('test/hasslot');
  Realm := TGocciaRealm.Create;
  Obj := nil;
  try
    Expect<Boolean>(Realm.HasSlot(Slot)).ToBe(False);

    // Don't register with the GC: we Free Obj manually below and registering
    // it would leave a dangling pointer in FManagedObjects that confuses any
    // later sweep run by another test in this suite.
    Obj := TCountingManaged.Create;
    Realm.SetSlot(Slot, Obj);

    Expect<Boolean>(Realm.HasSlot(Slot)).ToBe(True);
  finally
    Realm.Free;
    if Assigned(Obj) then
      Obj.Free;
  end;
end;

procedure TTestRealm.TestSetSlotRoundtrip;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmSlotId;
  Obj: TCountingManaged;
begin
  Slot := RegisterRealmSlot('test/roundtrip');
  Realm := TGocciaRealm.Create;
  Obj := TCountingManaged.Create;
  try
    Realm.SetSlot(Slot, Obj);
    Expect<Boolean>(Realm.GetSlot(Slot) = Obj).ToBe(True);
  finally
    Realm.Free;
    Obj.Free;
  end;
end;

procedure TTestRealm.TestUnsetOwnedSlotReturnsNil;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmOwnedSlotId;
begin
  Slot := RegisterRealmOwnedSlot('test/owned/unset');
  Realm := TGocciaRealm.Create;
  try
    Expect<Boolean>(Realm.GetOwnedSlot(Slot) = nil).ToBe(True);
  finally
    Realm.Free;
  end;
end;

procedure TTestRealm.TestSetOwnedSlotRoundtrip;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmOwnedSlotId;
  Owned: TCountingOwned;
begin
  Slot := RegisterRealmOwnedSlot('test/owned/roundtrip');
  Realm := TGocciaRealm.Create;
  Owned := TCountingOwned.Create;
  try
    Realm.SetOwnedSlot(Slot, Owned);
    Expect<Boolean>(Realm.GetOwnedSlot(Slot) = Owned).ToBe(True);
  finally
    Realm.Free;
  end;
end;

procedure TTestRealm.TestSetOwnedSlotReplacesPreviousAndFreesIt;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmOwnedSlotId;
  First, Second: TCountingOwned;
  CountAfterReplace: Integer;
begin
  Slot := RegisterRealmOwnedSlot('test/owned/replace');
  Realm := TGocciaRealm.Create;
  First := TCountingOwned.Create;
  Second := TCountingOwned.Create;
  try
    GOwnedDestructorCount := 0;
    Realm.SetOwnedSlot(Slot, First);

    // Replacing must call Free on the previous owner.
    Realm.SetOwnedSlot(Slot, Second);
    CountAfterReplace := GOwnedDestructorCount;
    Expect<Integer>(CountAfterReplace).ToBe(1);
    Expect<Boolean>(Realm.GetOwnedSlot(Slot) = Second).ToBe(True);
  finally
    // Realm.Free will Free Second.
    Realm.Free;
  end;
  Expect<Integer>(GOwnedDestructorCount).ToBe(2);
end;

procedure TTestRealm.TestSetOwnedSlotSameValueIsNoOp;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmOwnedSlotId;
  Owned: TCountingOwned;
begin
  Slot := RegisterRealmOwnedSlot('test/owned/idempotent');
  Realm := TGocciaRealm.Create;
  Owned := TCountingOwned.Create;
  try
    GOwnedDestructorCount := 0;
    Realm.SetOwnedSlot(Slot, Owned);
    // Setting to the same instance must not free it.
    Realm.SetOwnedSlot(Slot, Owned);
    Expect<Integer>(GOwnedDestructorCount).ToBe(0);
    Expect<Boolean>(Realm.GetOwnedSlot(Slot) = Owned).ToBe(True);
  finally
    Realm.Free;
  end;
  Expect<Integer>(GOwnedDestructorCount).ToBe(1);
end;

procedure TTestRealm.TestDestructorFreesOwnedSlots;
var
  Realm: TGocciaRealm;
  SlotA, SlotB: TGocciaRealmOwnedSlotId;
  OwnedA, OwnedB: TCountingOwned;
begin
  SlotA := RegisterRealmOwnedSlot('test/owned/destroy/a');
  SlotB := RegisterRealmOwnedSlot('test/owned/destroy/b');
  Realm := TGocciaRealm.Create;
  OwnedA := TCountingOwned.Create;
  OwnedB := TCountingOwned.Create;
  GOwnedDestructorCount := 0;
  Realm.SetOwnedSlot(SlotA, OwnedA);
  Realm.SetOwnedSlot(SlotB, OwnedB);
  Realm.Free;
  Expect<Integer>(GOwnedDestructorCount).ToBe(2);
end;

procedure TTestRealm.TestDestructorUnpinsSlotObjects;
var
  Realm: TGocciaRealm;
  Slot: TGocciaRealmSlotId;
  Obj: TCountingManaged;
  StartCount: Integer;
begin
  Slot := RegisterRealmSlot('test/unpin');
  Realm := TGocciaRealm.Create;
  Obj := TCountingManaged.Create;
  TGarbageCollector.Instance.RegisterObject(Obj);
  Realm.SetSlot(Slot, Obj);

  // Pinned objects survive Collect; once the realm is gone they should not.
  // Capture a baseline first - other tests in this suite share the global
  // destructor counter through manually-freed helpers.
  StartCount := GManagedDestructorCount;
  TGarbageCollector.Instance.Collect;
  Expect<Integer>(GManagedDestructorCount).ToBe(StartCount);

  Realm.Free;
  TGarbageCollector.Instance.Collect;
  // Sweep should have freed Obj since nothing else references it.
  Expect<Integer>(GManagedDestructorCount).ToBe(StartCount + 1);
end;

procedure TTestRealm.TestSlotIdGrowsArrayLazily;
var
  Realm: TGocciaRealm;
  LateSlot: TGocciaRealmSlotId;
  Obj: TCountingManaged;
begin
  // Register the slot AFTER realm creation so SetSlot must grow FSlots.
  Realm := TGocciaRealm.Create;
  LateSlot := RegisterRealmSlot('test/late/slot');
  Obj := TCountingManaged.Create;
  try
    Realm.SetSlot(LateSlot, Obj);
    Expect<Boolean>(Realm.GetSlot(LateSlot) = Obj).ToBe(True);
  finally
    Realm.Free;
    Obj.Free;
  end;
end;

type
  TRealmProbeThread = class(TThread)
  private
    FObservedRealm: TGocciaRealm;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property ObservedRealm: TGocciaRealm read FObservedRealm;
  end;

constructor TRealmProbeThread.Create;
begin
  FObservedRealm := TGocciaRealm(Pointer(IntPtr($DEADBEEF))); // sentinel
  inherited Create(False);
end;

procedure TRealmProbeThread.Execute;
begin
  FObservedRealm := CurrentRealm;
end;

procedure TTestRealm.TestThreadLocalityOfCurrentRealm;
var
  MainRealm, PreviousRealm: TGocciaRealm;
  Probe: TRealmProbeThread;
begin
  PreviousRealm := CurrentRealm;
  MainRealm := TGocciaRealm.Create;
  try
    SetCurrentRealm(MainRealm);
    Expect<Boolean>(CurrentRealm = MainRealm).ToBe(True);

    Probe := TRealmProbeThread.Create;
    try
      Probe.WaitFor;
      // The probe thread inherits a separate threadvar slot, so it sees the
      // initial value (nil) rather than MainRealm.
      Expect<Boolean>(Probe.ObservedRealm = nil).ToBe(True);
    finally
      Probe.Free;
    end;

    Expect<Boolean>(CurrentRealm = MainRealm).ToBe(True);
  finally
    SetCurrentRealm(PreviousRealm);
    MainRealm.Free;
  end;
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestRealm.Create('Realm'));
    TestRunnerProgram.Run;
  finally
    TGarbageCollector.Shutdown;
  end;
  ExitCode := TestResultToExitCode;
end.
