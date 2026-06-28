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

  Goccia.GarbageCollector,
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
  end;

procedure TLeakTests.SetupTests;
begin
  Test('draining the registry reclaims per-thread member-definition threadvars',
    TestDrainReclaimsMemberDefinitionThreadvars);
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
