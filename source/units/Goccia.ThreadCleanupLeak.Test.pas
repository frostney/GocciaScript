{ Leak-regression gate for issue #885: managed threadvars must be released when
  a thread tears down its runtime.

  FPC does not auto-finalize managed threadvars at thread exit, so the per-thread
  member-definition arrays that every builtin and value type caches
  (FStaticMembers / FPrototypeMembers : TArray<TGocciaMemberDefinition>, whose
  records hold strings) stay allocated unless something clears them.
  ShutdownThreadRuntime drains Goccia.ThreadCleanupRegistry on each worker thread,
  and the registry's finalization drains it on the main thread, so the registered
  ClearThreadvarMembers procs release those arrays on whichever thread tears down.

  This test populates the current thread's member-definition threadvars by
  building a throwaway engine, then asserts that draining the registry reclaims a
  meaningful amount of live heap. If the drain regresses to a no-op (cleanups not
  registered, or ShutdownThreadRuntime stops draining), nothing is reclaimed and
  the assertion fails. The companion test in Goccia.Threading.Test proves
  ShutdownThreadRuntime drains the registry once per worker exit, so together they
  cover the worker-thread teardown path. Runs in CI as a Pascal unit test. }

program Goccia.ThreadCleanupLeak.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  TextSemantics,

  Goccia.Builtins.Atomics,
  Goccia.Builtins.DisposableStack,
  Goccia.Builtins.Semver,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.RegExp.VM,
  Goccia.Temporal.TimeZone,
  Goccia.ThreadCleanupRegistry,
  Goccia.Threading.Init,
  Goccia.Values.Primitives,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  TLeakTests = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestDrainReclaimsMemberDefinitionThreadvars;
    procedure TestRegistryDrainIsIdempotent;
    procedure TestMigratedCacheCleanupsAreRegistered;
  end;

procedure TLeakTests.SetupTests;
begin
  // Idempotency runs first so the eagerly-created GPublishedGetterHosts list is
  // still live for its first FreeAndNil — the reclaim test would otherwise have
  // already drained it, leaving the object-reference container cleanups
  // unexercised. EnsureSharedPrototypesInitialized rebuilds a fresh engine on
  // every call, so the reclaim test below re-populates the member-definition
  // threadvars regardless of order. #892
  Test('registry drain is idempotent for object-reference container cleanups (#892)',
    TestRegistryDrainIsIdempotent);
  Test('draining the registry reclaims per-thread member-definition threadvars',
    TestDrainReclaimsMemberDefinitionThreadvars);
  Test('each migrated per-thread cache/memo cleanup is registered with the registry',
    TestMigratedCacheCleanupsAreRegistered);
end;

procedure TLeakTests.TestDrainReclaimsMemberDefinitionThreadvars;
const
  // One engine populates ~64 member-definition arrays (records with strings);
  // their combined heap is far above this floor, while collector/allocator noise
  // around a single Collect stays well below it.
  MIN_RECLAIMED_BYTES = 8 * 1024;
var
  Populated, Drained: Int64;
begin
  // Building a throwaway engine registers every builtin and value type, which
  // populates this thread's FStaticMembers / FPrototypeMembers threadvars. The
  // engine objects themselves are freed inside the call; the member-definition
  // arrays are threadvars and outlive it (the leak this issue is about).
  EnsureSharedPrototypesInitialized;
  TGarbageCollector.Instance.Collect;
  Populated := Int64(GetHeapStatus.TotalAllocated);

  // Draining the registry runs every ClearThreadvarMembers, releasing those
  // managed arrays on this thread — exactly what ShutdownThreadRuntime does on a
  // worker thread and what the registry finalization does on the main thread.
  RunThreadvarCleanups;
  TGarbageCollector.Instance.Collect;
  Drained := Int64(GetHeapStatus.TotalAllocated);

  Expect<Boolean>((Populated - Drained) >= MIN_RECLAIMED_BYTES).ToBe(True);
end;

procedure TLeakTests.TestRegistryDrainIsIdempotent;
const
  // A second drain over already-nil'd threadvars reclaims ~nothing; this tolerance
  // absorbs collector/heap-manager bookkeeping noise around the extra Collect so
  // the idempotency check does not flake on exact heap-total equality, while still
  // catching a double-free re-allocation or a real leak.
  MAX_IDEMPOTENT_DRAIN_NOISE_BYTES = 1024;
var
  AfterFirstDrain, AfterSecondDrain: Int64;
begin
  // #892 routes object-reference container threadvars through the registry with
  // FreeAndNil-based cleanups (e.g. Goccia.ObjectModel's published-getter host
  // list, which is created eagerly in that unit's initialization, and the
  // Goccia.Compiler.Statements working-state lists). RunThreadvarCleanups runs
  // on both the worker-exit and main-thread-finalization paths and is documented
  // as safe to call repeatedly, so a second drain must be a no-op rather than a
  // double-free. (A non-idempotent `.Free` without nil would fault here on the
  // eagerly-created published-getter host list.)
  EnsureSharedPrototypesInitialized;
  RunThreadvarCleanups;
  TGarbageCollector.Instance.Collect;
  AfterFirstDrain := Int64(GetHeapStatus.TotalAllocated);

  // Second drain over the now-cleared (nil'd) threadvars must not crash or grow.
  RunThreadvarCleanups;
  TGarbageCollector.Instance.Collect;
  AfterSecondDrain := Int64(GetHeapStatus.TotalAllocated);

  Expect<Boolean>(
    (AfterSecondDrain - AfterFirstDrain) <= MAX_IDEMPOTENT_DRAIN_NOISE_BYTES).ToBe(True);
end;

procedure TLeakTests.TestMigratedCacheCleanupsAreRegistered;
begin
  // Issue #893: each per-thread cache/memo that ShutdownThreadRuntime used to
  // clear by an explicit call must instead register its clear with
  // Goccia.ThreadCleanupRegistry from its owning unit's initialization (those
  // units are pulled in via this program's uses clause). If a unit drops its
  // RegisterThreadvarCleanup call, that threadvar silently leaks again on every
  // worker-thread exit; pinning each registration here makes that regress
  // loudly. (TestDrainReclaimsMemberDefinitionThreadvars proves the drain then
  // runs the registered cleanups and reclaims their heap.)
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ClearImportMetaCache)).ToBe(True);
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ShutdownAtomicsWaitersForCurrentThread)).ToBe(True);
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ClearDisposableStackSlotMap)).ToBe(True);
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ClearSemverHosts)).ToBe(True);
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ClearTimeZoneCache)).ToBe(True);
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ClearRegExpInputMemo)).ToBe(True);
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ClearAsciiMemo)).ToBe(True);
end;

begin
  // EnsureSharedPrototypesInitialized builds singletons whose getters assert they
  // were created on the main thread; pre-build them here first.
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    TestRunnerProgram.AddSuite(TLeakTests.Create('ThreadCleanupLeak'));
    TestRunnerProgram.Run;
  finally
    TGarbageCollector.Shutdown;
  end;

  ExitCode := TestResultToExitCode;
end.
