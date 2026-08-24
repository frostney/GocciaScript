program Goccia.MemoryLimit.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.CapabilityAudit,
  Goccia.Diagnostics.SourceRegistry,
  Goccia.Engine,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.MemoryLimit,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.Sandbox.Context,
  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.Shape;

type
  { A managed value that reports its own sweep. Recycle, not Destroy: the sweep
    calls Recycle, so a value freed some other way — by its owner, at teardown —
    is not miscounted as collected. }
  TCanaryValue = class(TGocciaObjectValue)
  public
    procedure Recycle; override;
  end;

  { The same, counted separately, for the value the control test deliberately
    never hands to a store. Two classes rather than one counter with a flag so
    "protected" and "must not be protected" can never be confused for each
    other in an assertion. }
  TControlValue = class(TGocciaObjectValue)
  public
    procedure Recycle; override;
  end;

  { A property map whose growth gate collects before deciding. It overrides only
    the budget decision, so the rooting under test is the production
    TGocciaShapedPropertyMap.RequireStorageBytes and not something the test put
    there: the collection happens inside the window that override opens, at
    exactly the point the gate takes it.

    Two arms, and the difference between them is what the H4 layer added.
    GCollectDuringPropertyStore is the SEAM: it collects unconditionally,
    needs no budget arithmetic, and pins the rooting deterministically on
    every pointer width in microseconds. GTightCeilingDuringPropertyStore is
    the REAL gate: it arms a ceiling this growth cannot fit and then calls the
    production consult, so the collection is taken by RequireNativeBytes
    itself. The seam proves the window is wide enough; the real arm proves the
    production gate collects inside it. Neither substitutes for the other —
    a seam that stopped matching the gate would still pass, and a real-gate
    run alone could not tell "collected and everything survived" from "never
    collected". }
  TCollectingPropertyMap = class(TGocciaShapedPropertyMap)
  private
    FGateCalls: Integer;
    FForcedCollections: Integer;
  protected
    procedure ConsultStorageBudget(const ABytes: Int64); override;
  public
    { Number of times the growth gate has been consulted. Read by the tests so
      a run that never reached the gate fails instead of passing vacuously. }
    property GateCalls: Integer read FGateCalls;
    { Collections taken by the production gate itself, under the tight-ceiling
      arm. Counted around the inherited consult so the number cannot include a
      collection the test injected. }
    property ForcedCollections: Integer read FForcedCollections;
  end;

  { An ordinary object whose property storage is the collecting map above. }
  TGatedStoreObjectValue = class(TGocciaObjectValue)
  private
    FGatedProperties: TCollectingPropertyMap;
  public
    constructor CreateGated;
    property GatedProperties: TCollectingPropertyMap read FGatedProperties;
  end;

  { The element-storage twin of TCollectingPropertyMap, with the same two arms:
    an element list whose growth gate collects before deciding. It likewise
    overrides only the budget decision, so what is under test is the production
    TGocciaElementList.RequireStorageBytes and not something the test put
    there. }
  TCollectingElementList = class(TGocciaElementList)
  private
    FGateCalls: Integer;
    FForcedCollections: Integer;
  protected
    procedure ConsultStorageBudget(const ABytes: Int64); override;
  public
    { Number of times the growth gate has been consulted. Read by the tests so
      a run that never reached the gate fails instead of passing vacuously. }
    property GateCalls: Integer read FGateCalls;
    { Collections taken by the production gate itself, under the tight-ceiling
      arm. }
    property ForcedCollections: Integer read FForcedCollections;
  end;

  { An ordinary array whose dense element storage is the collecting list. }
  TGatedElementsArrayValue = class(TGocciaArrayValue)
  private
    FGatedElements: TCollectingElementList;
  public
    constructor CreateGated;
    property GatedElements: TCollectingElementList read FGatedElements;
  end;

  { Raises at a deterministic index insertion, standing in for an allocator
    failure during the registry's commit phase. }
  TFailingDiagnosticSourceScope = class(TGocciaDiagnosticSourceScope)
  private
    FFailAtIndex: Integer;
  protected
    procedure AfterIndexCommit(const AIndex: Integer); override;
  public
    constructor Create(const AFailAtIndex: Integer);
    property FailAtIndex: Integer read FFailAtIndex write FFailAtIndex;
  end;

  TMemoryLimitTests = class(TTestSuite)
  private
    { The class the sandbox injection points raise, or nil for "raise nothing".
      Set by SandboxFaultEscapesScript for the duration of one run. }
    FSandboxFaultClass: ExceptClass;
    { Arms FaultEscapesScript to install the fetch runtime extension, which is
      what puts Response in scope, and to give the engine an audit sink that
      refuses delivery so any capability the script exercises raises. }
    FInstallFailingAuditSink: Boolean;
    { Set by the witness native below. FaultEscapesScript clears it before each
      run, so it answers one question about one script: did guest code keep
      running after a fault it should never have been able to observe? }
    FSwallowedFaultReported: Boolean;
    { The refusing sink itself. EmitCapabilityAudit wraps whatever it raises in
      EGocciaCapabilityAuditDeliveryError. }
    procedure FailingAuditSink(const AEvent: TGocciaCapabilityAuditEvent);
    { The native the integrity-fault tests call. Raises the same class a virtual
      call through a collected value raises under `$OBJECTCHECKS ON`, which is
      how a real unrooted-temporary bug arrives. }
    function RaiseIntegrityFault(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    { The witness the integrity-fault scripts call from inside their `catch`.
      Being called at all means the catch block ran, which is the swallow the
      guards exist to prevent — so the tests assert it stayed silent. Raising
      here instead would be indistinguishable from the fault under test. }
    function ReportSwallowedFault(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    { Raises whatever FSandboxFaultClass names. The refusal comes from the gate
      itself rather than a hand-built instance, so the test exercises the same
      object a real over-budget allocation produces. }
    procedure RaiseConfiguredFault;
    { Root-clamp hook, installed on the sandbox filesystem after the extension
      has taken its own. It fires from inside TGocciaSandboxFsJob.Execute, so it
      injects the fault at the job's completion boundary. }
    procedure SandboxRootClamp(const APath, ABase, ACanonicalPath: string);
    { Guest-callable native, reached through an options-object getter, so it
      injects the same fault at the synchronous fs.promises argument boundary
      that RejectedPromiseFromException guards. }
    function RaiseSandboxFault(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    { Runs ASource under a budget that cannot fit the over-budget allocation and
      with that native bound as a global, and answers whether AFaultClass
      reached the host instead of the script. Any other escaping exception is
      not an answer to that question and propagates. }
    function FaultEscapesScript(const ASource: string;
      const AExecutor: TGocciaExecutor; const AName: string;
      const AFaultClass: ExceptClass): Boolean;
    { The same question for a module running against the sandbox runtime
      extension. Both sandbox injection points are armed to raise
      AInjectedClass; pass nil to arm neither and let the ordinary filesystem
      error through. }
    function SandboxFaultEscapesScript(const ASource: string;
      const AExecutor: TGocciaExecutor; const AName: string;
      const AEscapingClass, AInjectedClass: ExceptClass): Boolean;
    procedure TestGateRefusesOverBudgetRequest;
    procedure TestGatePermitsInBudgetRequest;
    procedure StoreCanaries(const ATarget: TGatedStoreObjectValue;
      const AFirstIndex, ACount: Integer);
    function StoreCanariesUntilGateFires(
      const ATarget: TGatedStoreObjectValue;
      const ALimit: Integer): Integer;
    function StoreCanariesUntilBucketRehash(
      const ATarget: TGatedStoreObjectValue;
      const ALimit: Integer): Integer;
    procedure TestGateCollectsBeforeRefusing;
    procedure TestRepeatedRefusalCollectsOnlyOnce;
    procedure TestGatedGrowthRootsNativeOnlyValues;
    procedure TestRealGatedGrowthRootsNativeOnlyValues;
    procedure TestGatedGrowthRootsAnUnrootedMapOwner;
    procedure TestGatedBucketRehashRootsThePendingValue;
    procedure TestGatedCompactionRootsThePendingValue;
    procedure TestGatedWindowCollectionSweepsAnUnhandedValue;
    function HoleExtendCanaries(const ATarget: TGatedElementsArrayValue;
      const ACount: Integer): Integer;
    procedure TestElementGateCollectsBeforeRefusing;
    procedure TestGatedHoleExtendRootsTheIncomingValue;
    procedure TestRealGatedHoleExtendRootsTheIncomingValue;
    procedure TestGatedHoleExtendRootsAnUnrootedArray;
    procedure TestGatedHoleExtendRootsTheDescriptorValue;
    procedure TestGatedElementWindowSweepsAnUnhandedValue;
    procedure TestSyncCatchCannotSwallowRefusal;
    procedure TestAsyncFunctionCatchCannotSwallowRefusal;
    procedure TestPromiseExecutorCatchCannotSwallowRefusal;
    procedure TestAsyncGeneratorReturnCannotSwallowRefusal;
    procedure TestIteratorCloseCannotSwallowRefusal;
    procedure TestScriptErrorsStayCatchable;
    procedure TestSyncCatchCannotSwallowIntegrityFault;
    procedure TestAsyncFunctionCatchCannotSwallowIntegrityFault;
    procedure TestSandboxFsPromiseCannotSwallowRefusal;
    procedure TestSandboxFsPromiseCannotSwallowIntegrityFault;
    procedure TestSandboxFsPromiseErrorsStayCatchable;
    procedure TestResponseJsonCannotSwallowAuditDeliveryFailure;
    procedure TestDiagnosticIdentityFailureFailsClosed;
    procedure TestDiagnosticCanonicalIdentityKeepsHostOwnership;
    procedure TestDiagnosticSourceAccountingUsesRetainedBytes;
    procedure TestDiagnosticRegistryCommitRollsBack;
  protected
    procedure BeforeEach; override;
  public
    procedure SetupTests; override;
  end;

const
  { 100e6 pointer slots is 800 MB on a 64-bit target — far past any budget
    this suite installs, so the gate refuses regardless of what the test
    process has already allocated. }
  OVER_BUDGET_SOURCE_TAIL = 'const a = new Array(100000000); a.length;';

  { Headroom over live usage rather than an absolute ceiling: the budget is
    process-global and the suite shares it with everything already allocated,
    so a fixed number would refuse the engine's own setup on a busy run. }
  BUDGET_HEADROOM_BYTES = 64 * 1024 * 1024;

  { Name and message for the injected integrity fault. Nothing asserts on the
    message — the class reaching the host is the whole signal. It matches the
    text FPC produces for a real object check so the intent is legible in a
    stack trace. }
  INTEGRITY_FAULT_GLOBAL_NAME = '__gocciaRaiseIntegrityFault';
  INTEGRITY_FAULT_MESSAGE = 'Object reference is Nil';

  { Name of the witness native the integrity-fault scripts call from their
    `catch` blocks, so a swallow reports itself instead of only showing up as
    the absence of an escaping fault. }
  SWALLOW_WITNESS_GLOBAL_NAME = '__gocciaReportSwallowedFault';

  { Upper bound on the properties the canary tests store while waiting for the
    growth gate to fire. The entry array first clears GATED_GROWTH_MIN_BYTES at
    62 entries on a 64-bit target and later on a 32-bit one, so this is well
    clear of both while still turning a gate that stopped firing into a failed
    assertion rather than a hung test. }
  GATE_PROBE_LIMIT = 512;

  { The bucket array is Int32 slots and Grow doubles it, so the transient a
    rehash reports is old + 2*old. The first rehash whose transient clears
    GATED_GROWTH_MIN_BYTES (4096) is 512 -> 1024 buckets, at (512 + 1024) * 4 =
    6144 bytes; every smaller rehash is under the threshold and is never gated.
    A map that has reached this capacity has therefore been through a gated
    Grow — on every pointer width, since SizeOf(Int32) does not vary. Reaching
    it takes roughly 359 properties at the map's 70% load factor, so the probe
    limit below has to be well clear of that. }
  BUCKET_REHASH_GATED_CAPACITY = 1024;
  BUCKET_PROBE_LIMIT = 2048;

  { The compaction test stores this many properties and then deletes all but
    every COMPACTION_LIVE_STRIDE'th. DeletedSlotsNeedCompaction is
    FDeletedCount > FCount, so three dead entries per live one puts the next
    store on the Compact path, and compacting 400 entries down to 100 reports
    (400 + 100) * SizeOf(TEntry) — comfortably over the gate's threshold at
    either pointer width. }
  COMPACTION_TOTAL_PROPERTIES = 400;
  COMPACTION_LIVE_STRIDE = 4;

  { How many hole-extending element stores the element canaries perform. The
    element gate has no small-block threshold to clear — every extension past
    the current element count is reported — so one store would be enough; a
    handful runs the window repeatedly and leaves several already-stored
    canaries behind for the owner push to keep alive. }
  ELEMENT_CANARY_STORES = 8;

  { Stride between the indices those stores use. Anything above 1 leaves a hole
    behind every write, which is what puts the store on the extension path
    rather than the append fast path (an append at exactly the element count
    never reaches the gate at all). }
  ELEMENT_CANARY_STRIDE = 2;

  { Name of the native the sandbox tests reach through an options getter. }
  SANDBOX_FAULT_GLOBAL_NAME = '__gocciaRaiseSandboxFault';

  { A path that escapes the sandbox root, so normalising it clamps and calls the
    root-clamp hook. The clamp happens inside the queued filesystem job rather
    than at the call, which is what puts the injected fault on the job's
    completion boundary. }
  SANDBOX_CLAMPING_PATH = '../../missing.txt';

  { The queued filesystem job normalises the escaping path, the clamp hook
    raises, and the job's completion boundary decides whether the guest's
    `catch` sees it. }
  SANDBOX_JOB_SOURCE =
    'import fs from "fs";' + sLineBreak +
    'try {' + sLineBreak +
    '  await fs.promises.readFile("' + SANDBOX_CLAMPING_PATH + '", "utf8");' +
    sLineBreak +
    '} catch (e) {}';

  { The options getter runs while fs.promises.readFile is still validating its
    arguments, before anything is queued, so the fault arrives at the
    synchronous boundary that RejectedPromiseFromException guards. }
  SANDBOX_ARGUMENT_SOURCE =
    'import fs from "fs";' + sLineBreak +
    'try {' + sLineBreak +
    '  await fs.promises.readFile("/present.txt", {' + sLineBreak +
    '    get encoding() { return ' + SANDBOX_FAULT_GLOBAL_NAME + '(); },' +
    sLineBreak +
    '  });' + sLineBreak +
    '} catch (e) {}';

var
  { Number of TCanaryValue instances the sweep has reclaimed. Reset by each
    test that reads it. }
  GCanarySweepCount: Integer;
  { The same for TControlValue, the never-handed-out value. }
  GControlSweepCount: Integer;
  { Arms TCollectingPropertyMap. Off by default so the setup stores the canary
    tests perform before the interesting one do not collect. }
  GCollectDuringPropertyStore: Boolean;
  { The same for TCollectingElementList. Separate from the flag above so a
    property-store gate reached incidentally from an element test — an array
    index that lands in the property map — cannot collect and make an element
    assertion pass or fail for the other layer's reason. }
  GCollectDuringElementStore: Boolean;
  { Arms the real-gate arm of TCollectingPropertyMap: instead of injecting a
    collection, install a ceiling the growth cannot fit and let the production
    RequireNativeBytes be the thing that collects. Off by default for the same
    reason the seam flags are. }
  GTightCeilingDuringPropertyStore: Boolean;
  { The same for TCollectingElementList. }
  GTightCeilingDuringElementStore: Boolean;

procedure TMemoryLimitTests.BeforeEach;
begin
  inherited BeforeEach;
  { The gate reads its budget off the collector, and a program with no
    collector is unbounded by construction — without this the direct gate
    tests would assert against a disabled budget and pass vacuously. }
  TGarbageCollector.Initialize;
end;

constructor TFailingDiagnosticSourceScope.Create(
  const AFailAtIndex: Integer);
begin
  inherited Create;
  FFailAtIndex := AFailAtIndex;
end;

procedure TFailingDiagnosticSourceScope.AfterIndexCommit(
  const AIndex: Integer);
begin
  inherited AfterIndexCommit(AIndex);
  if AIndex = FFailAtIndex then
    raise EOutOfMemory.Create('injected diagnostic registry allocation failure');
end;

procedure TMemoryLimitTests.SetupTests;
begin
  Test('Diagnostic file identity failure disables source disclosure',
    TestDiagnosticIdentityFailureFailsClosed);
  Test('Diagnostic canonical identity preserves first host ownership',
    TestDiagnosticCanonicalIdentityKeepsHostOwnership);
  Test('Diagnostic source caps and reservations use retained UTF-16 bytes',
    TestDiagnosticSourceAccountingUsesRetainedBytes);
  Test('Diagnostic registry commit rolls back indexes and reservation',
    TestDiagnosticRegistryCommitRollsBack);
  Test('The gate refuses a request larger than the remaining budget',
    TestGateRefusesOverBudgetRequest);
  Test('The gate permits a request that fits the remaining budget',
    TestGatePermitsInBudgetRequest);
  Test('The gate collects and re-tests before refusing',
    TestGateCollectsBeforeRefusing);
  Test('A repeated refusal of the same size collects only once',
    TestRepeatedRefusalCollectsOnlyOnce);
  Test('A gated growth keeps the values a store is carrying alive across a ' +
    'collection taken there', TestGatedGrowthRootsNativeOnlyValues);
  Test('The real gate''s own collection keeps those values alive too',
    TestRealGatedGrowthRootsNativeOnlyValues);
  Test('A gated growth keeps an object no caller roots alive with it',
    TestGatedGrowthRootsAnUnrootedMapOwner);
  Test('A gated bucket rehash keeps the pending value alive',
    TestGatedBucketRehashRootsThePendingValue);
  Test('A gated compaction keeps the pending value alive',
    TestGatedCompactionRootsThePendingValue);
  Test('That same collection sweeps a value the store was never handed',
    TestGatedWindowCollectionSweepsAnUnhandedValue);
  Test('The element gate collects and re-tests before refusing',
    TestElementGateCollectsBeforeRefusing);
  Test('A gated hole extension keeps the element being stored alive across ' +
    'a collection taken there', TestGatedHoleExtendRootsTheIncomingValue);
  Test('The real element gate''s own collection keeps it alive too',
    TestRealGatedHoleExtendRootsTheIncomingValue);
  Test('A gated hole extension keeps an array no caller roots alive with it',
    TestGatedHoleExtendRootsAnUnrootedArray);
  Test('A gated hole extension keeps the descriptor value its caller reads ' +
    'back afterwards alive', TestGatedHoleExtendRootsTheDescriptorValue);
  Test('That same collection sweeps a value the element store was never handed',
    TestGatedElementWindowSweepsAnUnhandedValue);
  Test('Script try/catch cannot swallow a refusal',
    TestSyncCatchCannotSwallowRefusal);
  Test('An async function body cannot swallow a refusal',
    TestAsyncFunctionCatchCannotSwallowRefusal);
  Test('A promise executor cannot swallow a refusal',
    TestPromiseExecutorCatchCannotSwallowRefusal);
  Test('An async generator return/finally cannot swallow a refusal',
    TestAsyncGeneratorReturnCannotSwallowRefusal);
  Test('Ordinary script errors stay catchable', TestScriptErrorsStayCatchable);
  Test('Script try/catch cannot swallow an engine-integrity fault',
    TestSyncCatchCannotSwallowIntegrityFault);
  Test('A guest iterator return() cannot swallow a refusal while closing',
    TestIteratorCloseCannotSwallowRefusal);
  Test('An async function body cannot swallow an engine-integrity fault',
    TestAsyncFunctionCatchCannotSwallowIntegrityFault);
  Test('A sandbox fs promise cannot swallow a refusal',
    TestSandboxFsPromiseCannotSwallowRefusal);
  Test('A sandbox fs promise cannot swallow an engine-integrity fault',
    TestSandboxFsPromiseCannotSwallowIntegrityFault);
  Test('Sandbox filesystem errors stay catchable',
    TestSandboxFsPromiseErrorsStayCatchable);
  Test('Response.json cannot swallow an audit delivery failure',
    TestResponseJsonCannotSwallowAuditDeliveryFailure);
end;

procedure TCanaryValue.Recycle;
begin
  Inc(GCanarySweepCount);
  inherited;
end;

procedure TControlValue.Recycle;
begin
  Inc(GControlSweepCount);
  inherited;
end;

{ The tight-ceiling arm, shared by both gates as prose because Pascal cannot
  share it as code — `inherited` is not something a method pointer can carry.

  ArmTightCeiling installs a ceiling one byte below what the growth asks for,
  which is the only shape that makes the gate collect: the request is over what
  is left of the budget, so the fit test fails, and inside the whole budget, so
  ShouldForceLimitCollection does not rule it out. The test plants reclaimable
  garbage first, so the forced collection gets BytesAllocated back under the
  armed ceiling and the gate then PERMITS — the store completes, and what the
  test reads afterwards is a map or list the production gate collected inside
  and then let grow.

  ReleaseTightCeiling restores the budget whatever happened, and answers how
  many collections ran inside the window. A refusal that escaped without the
  restore would leave every later test running against a one-byte budget. }
procedure ArmTightCeiling(const ABytes: Int64;
  out APreviousMaxBytes: Int64; out ACollectionsBefore: Integer);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  { One object's worth of guaranteed reclaim, created unreferenced and counted
    into the baseline below. The armed ceiling is one byte under the request,
    so the forced collection has to find SOMETHING or the gate refuses and the
    store raises instead of growing — and a gate consulted repeatedly (the
    element list is gated on every extension) would otherwise find the heap
    already swept clean by its own previous call. Planting it here rather than
    in the tests makes each gate call independent of the last. }
  TGocciaObjectValue.Create;
  APreviousMaxBytes := GC.MaxBytes;
  ACollectionsBefore := GC.TotalCollections;
  GC.MaxBytes := GC.BytesAllocated + ABytes - 1;
end;

function ReleaseTightCeiling(const APreviousMaxBytes: Int64;
  const ACollectionsBefore: Integer): Integer;
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  Result := GC.TotalCollections - ACollectionsBefore;
  GC.MaxBytes := APreviousMaxBytes;
end;

procedure TCollectingPropertyMap.ConsultStorageBudget(const ABytes: Int64);
var
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  Inc(FGateCalls);
  if GTightCeilingDuringPropertyStore then
  begin
    ArmTightCeiling(ABytes, PreviousMaxBytes, CollectionsBefore);
    try
      inherited ConsultStorageBudget(ABytes);
    finally
      Inc(FForcedCollections,
        ReleaseTightCeiling(PreviousMaxBytes, CollectionsBefore));
    end;
    Exit;
  end;
  if GCollectDuringPropertyStore then
  begin
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.Collect;
  end;
  inherited ConsultStorageBudget(ABytes);
end;

procedure TCollectingElementList.ConsultStorageBudget(const ABytes: Int64);
var
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  Inc(FGateCalls);
  if GTightCeilingDuringElementStore then
  begin
    ArmTightCeiling(ABytes, PreviousMaxBytes, CollectionsBefore);
    try
      inherited ConsultStorageBudget(ABytes);
    finally
      Inc(FForcedCollections,
        ReleaseTightCeiling(PreviousMaxBytes, CollectionsBefore));
    end;
    Exit;
  end;
  if GCollectDuringElementStore then
  begin
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.Collect;
  end;
  inherited ConsultStorageBudget(ABytes);
end;

constructor TGatedElementsArrayValue.CreateGated;
begin
  inherited Create;
  { The base constructor already built an ordinary element list; swap it for
    the collecting one before anything has been stored in it, and give it the
    same owner the real one gets. }
  FElements.Free;
  FGatedElements := TCollectingElementList.Create(False);
  FGatedElements.Owner := Self;
  FElements := FGatedElements;
end;

constructor TGatedStoreObjectValue.CreateGated;
begin
  inherited Create;
  { The base constructor already built an ordinary shaped map; swap it for the
    collecting one before anything has been stored in it, and give it the same
    owner the real one gets. }
  FProperties.Free;
  FGatedProperties := TCollectingPropertyMap.Create;
  FGatedProperties.Owner := Self;
  FProperties := FGatedProperties;
end;

procedure TMemoryLimitTests.FailingAuditSink(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  raise Exception.Create('audit sink refused ' + AEvent.Subject);
end;

function TMemoryLimitTests.RaiseIntegrityFault(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  raise EObjectCheck.Create(INTEGRITY_FAULT_MESSAGE);
end;

function TMemoryLimitTests.ReportSwallowedFault(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  FSwallowedFaultReported := True;
end;

procedure TMemoryLimitTests.RaiseConfiguredFault;
begin
  if FSandboxFaultClass = nil then
    Exit;
  if FSandboxFaultClass = TGocciaMemoryLimitError then
    { Ask the gate for more than the budget the caller installed, so the test
      asserts on the exception the engine really raises rather than one the
      test built. }
    RequireNativeBytes(Int64(BUDGET_HEADROOM_BYTES) * 4)
  else
    raise EObjectCheck.Create(INTEGRITY_FAULT_MESSAGE);
end;

procedure TMemoryLimitTests.SandboxRootClamp(const APath, ABase,
  ACanonicalPath: string);
begin
  RaiseConfiguredFault;
end;

function TMemoryLimitTests.RaiseSandboxFault(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  RaiseConfiguredFault;
end;

function TMemoryLimitTests.FaultEscapesScript(const ASource: string;
  const AExecutor: TGocciaExecutor; const AName: string;
  const AFaultClass: ExceptClass): Boolean;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  Result := False;
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := TGocciaEngine.Create(AName, Source, AExecutor);
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := 0;
  FSwallowedFaultReported := False;
  try
    Engine.InjectGlobal(INTEGRITY_FAULT_GLOBAL_NAME,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RaiseIntegrityFault,
        INTEGRITY_FAULT_GLOBAL_NAME, 0));
    Engine.InjectGlobal(SWALLOW_WITNESS_GLOBAL_NAME,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(ReportSwallowedFault,
        SWALLOW_WITNESS_GLOBAL_NAME, 0));
    if FInstallFailingAuditSink then
    begin
      AttachRuntime(Engine).Install(TGocciaFetchRuntimeExtension.Create);
      Engine.CapabilityAuditSink := FailingAuditSink;
    end;
    if Assigned(GC) then
    begin
      PreviousMaxBytes := GC.MaxBytes;
      GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    end;
    try
      Engine.Execute;
    except
      on E: Exception do
        if E.InheritsFrom(AFaultClass) then
          Result := True
        else
          raise;
    end;
  finally
    if Assigned(GC) then
      GC.MaxBytes := PreviousMaxBytes;
    Engine.Free;
    AExecutor.Free;
    Source.Free;
  end;
end;

function TMemoryLimitTests.SandboxFaultEscapesScript(const ASource: string;
  const AExecutor: TGocciaExecutor; const AName: string;
  const AEscapingClass, AInjectedClass: ExceptClass): Boolean;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Runtime: TGocciaRuntimeCore;
  Context: TGocciaSandboxContext;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  Result := False;
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := TGocciaEngine.Create(AName, Source, AExecutor);
  Context := TGocciaSandboxContext.Create;
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := 0;
  FSandboxFaultClass := AInjectedClass;
  try
    Engine.SourceType := stModule;
    Engine.InjectGlobal(SANDBOX_FAULT_GLOBAL_NAME,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RaiseSandboxFault,
        SANDBOX_FAULT_GLOBAL_NAME, 0));
    Runtime := AttachRuntime(Engine);
    Runtime.Install(TGocciaSandboxRuntimeExtension.Create(Context));
    { After Install, so the extension's own audit-emitting hook is the one being
      replaced rather than the other way round. }
    Context.Fs.RootClampCallback := SandboxRootClamp;

    if Assigned(GC) then
    begin
      PreviousMaxBytes := GC.MaxBytes;
      GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    end;
    try
      Engine.Execute;
    except
      on E: Exception do
        if E.InheritsFrom(AEscapingClass) then
          Result := True
        else
          raise;
    end;
  finally
    FSandboxFaultClass := nil;
    if Assigned(GC) then
      GC.MaxBytes := PreviousMaxBytes;
    Engine.Free;
    Context.Free;
    AExecutor.Free;
    Source.Free;
  end;
end;

procedure TMemoryLimitTests.TestGateRefusesOverBudgetRequest;
var
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
  Raised: Boolean;
begin
  GC := TGarbageCollector.Instance;
  Expect<Boolean>(Assigned(GC)).ToBe(True);
  PreviousMaxBytes := GC.MaxBytes;
  try
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    { The precondition the zero-collection claim below rests on, asserted
      rather than assumed: this request does not merely exceed what is LEFT of
      the budget, it exceeds the whole of it. }
    Expect<Boolean>(
      Int64(BUDGET_HEADROOM_BYTES) * 4 > GC.MaxBytes).ToBe(True);
    Expect<Boolean>(CanAllocateNativeBytes(
      Int64(BUDGET_HEADROOM_BYTES) * 4)).ToBe(False);
    CollectionsBefore := GC.TotalCollections;
    Raised := False;
    try
      RequireNativeBytes(Int64(BUDGET_HEADROOM_BYTES) * 4);
    except
      on TGocciaMemoryLimitError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);

    { The half of the contract the collecting gate does NOT change. A request
      larger than the whole budget can never fit however much is reclaimed, so
      it is refused on the arithmetic alone and never walks the heap — the
      cheap answer a runaway `new Array(1e8)` gets, and the reason a guest
      cannot turn the ceiling into a mark-and-sweep generator. }
    Expect<Integer>(GC.TotalCollections - CollectionsBefore).ToBe(0);
  finally
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

procedure TMemoryLimitTests.TestGatePermitsInBudgetRequest;
var
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
begin
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := GC.MaxBytes;
  try
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    Expect<Boolean>(CanAllocateNativeBytes(1024)).ToBe(True);
    { A non-positive request is never a budget question. }
    Expect<Boolean>(CanAllocateNativeBytes(0)).ToBe(True);
    { A JS-controlled length can multiply into a total that wraps; a wrapped
      total must not read as comfortably in budget. }
    Expect<Boolean>(CanAllocateNativeBytes(High(Int64))).ToBe(False);
    RequireNativeBytes(1024);
  finally
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

procedure TMemoryLimitTests.TestGateCollectsBeforeRefusing;
var
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
  Raised: Boolean;
  Request: Int64;
begin
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := GC.MaxBytes;
  try
    { Clears any floor an earlier test's refusal recorded, so this one measures
      the gate's own behaviour rather than the damper's. }
    GC.Collect;
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    { Exactly the whole budget. That is the one request size that is both
      inside the budget — so the gate is obliged to try — and impossible to
      make room for, since fitting it would need the live set at zero. A
      refusal that collects first, deterministically, on every pointer width. }
    Request := GC.MaxBytes;
    CollectionsBefore := GC.TotalCollections;
    Raised := False;
    try
      RequireNativeBytes(Request);
    except
      on TGocciaMemoryLimitError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);

    { The expectation this layer exists to flip. The gate used to answer from
      the byte counter and never walk the heap, so whether a storage doubling
      was refused depended on how much collectable garbage happened to be
      resident. It now forces one collection and re-tests before refusing —
      once, not per attempt; TestRepeatedRefusalCollectsOnlyOnce pins that
      half. The rooting the canary tests below pin is what makes this safe,
      and it does not move. }
    Expect<Integer>(GC.TotalCollections - CollectionsBefore).ToBe(1);
  finally
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

procedure TMemoryLimitTests.TestRepeatedRefusalCollectsOnlyOnce;
const
  { Enough attempts that a per-attempt mark-and-sweep would be unmistakable in
    the count; the assertion is exact, so one extra walk fails it. }
  RETRY_ATTEMPTS = 8;
var
  CollectionsBefore: Integer;
  GC: TGarbageCollector;
  I: Integer;
  PreviousMaxBytes: Int64;
  Refusals: Integer;
  Request: Int64;
begin
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := GC.MaxBytes;
  try
    GC.Collect;
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    Request := GC.MaxBytes;

    { The damper, from the guest's side. A script that catches the catchable
      RangeError and retries — or a native builder that asks again after a
      refusal — must not be able to bill the engine a full mark-and-sweep per
      attempt. The first forced collection records the level it could not get
      below, and every later attempt at the same size is answered from that
      record without walking the heap. }
    CollectionsBefore := GC.TotalCollections;
    Refusals := 0;
    for I := 1 to RETRY_ATTEMPTS do
      try
        RequireNativeBytes(Request);
      except
        on TGocciaMemoryLimitError do
          Inc(Refusals);
      end;
    { Every attempt still refuses: the damper skips the walk, never the
      refusal. }
    Expect<Integer>(Refusals).ToBe(RETRY_ATTEMPTS);
    Expect<Integer>(GC.TotalCollections - CollectionsBefore).ToBe(1);

    { And the damper suppresses a proven-hopeless repeat, not the mechanism: an
      ordinary collection clears the record, so the next attempt forces its own
      collection again. Without this the first refusal at a given size would
      disarm the gate for the rest of the run. }
    CollectionsBefore := GC.TotalCollections;
    GC.Collect;
    try
      RequireNativeBytes(Request);
    except
      on TGocciaMemoryLimitError do;
    end;
    { Two: the explicit collection, and the one the re-armed gate forced. }
    Expect<Integer>(GC.TotalCollections - CollectionsBefore).ToBe(2);
  finally
    GC.MaxBytes := PreviousMaxBytes;
  end;
end;

{ Stores canary-valued properties into ATarget until its growth gate has fired
  at least once, and answers how many were stored. Each value is created and
  handed straight to DefineProperty, so between the two it exists only in a
  Pascal local and in a descriptor no map holds yet — the native-unrooted shape,
  and the one no evaluator-side frame can cover.

  A loop rather than a fixed property count because how many entries it takes to
  clear GATED_GROWTH_MIN_BYTES depends on SizeOf(TEntry), which differs by
  pointer width. }
function TMemoryLimitTests.StoreCanariesUntilGateFires(
  const ATarget: TGatedStoreObjectValue; const ALimit: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALimit - 1 do
  begin
    StoreCanaries(ATarget, I, 1);
    Inc(Result);
    if ATarget.GatedProperties.GateCalls > 0 then
      Exit;
  end;
end;

{ Keeps storing until the bucket array has been rehashed to the first capacity
  whose rehash is large enough to be gated. The loop above stops at the FIRST
  gate call, which is always an entry-array growth — the bucket array is Int32
  slots and stays under the threshold until far more properties have been
  stored — so reaching Grow's gate needs its own probe. }
function TMemoryLimitTests.StoreCanariesUntilBucketRehash(
  const ATarget: TGatedStoreObjectValue; const ALimit: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALimit - 1 do
  begin
    StoreCanaries(ATarget, I, 1);
    Inc(Result);
    if ATarget.GatedProperties.Capacity >= BUCKET_REHASH_GATED_CAPACITY then
      Exit;
  end;
end;

procedure TMemoryLimitTests.StoreCanaries(
  const ATarget: TGatedStoreObjectValue;
  const AFirstIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AFirstIndex to AFirstIndex + ACount - 1 do
    ATarget.DefineProperty('canary' + IntToStr(I),
      TGocciaPropertyDescriptorData.Create(TCanaryValue.Create,
        [pfEnumerable, pfConfigurable, pfWritable]));
end;

procedure TMemoryLimitTests.TestGatedGrowthRootsNativeOnlyValues;
var
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
  Stored: Integer;
  Target: TGatedStoreObjectValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedStoreObjectValue.CreateGated;
  Roots.Initialize;
  try
    { The object being stored into is the caller's to keep alive, not the store
      path's, so root it the way a real caller's frame would. That leaves
      exactly one unrooted thing in the picture: the values. }
    Roots.Add(Target);
    GC.Collect;
    GCanarySweepCount := 0;

    GCollectDuringPropertyStore := True;
    try
      Stored := StoreCanariesUntilGateFires(Target, GATE_PROBE_LIMIT);
    finally
      GCollectDuringPropertyStore := False;
    end;

    { The gate really was reached — without this the run below could pass
      because nothing ever collected. }
    Expect<Boolean>(Target.GatedProperties.GateCalls > 0).ToBe(True);
    { Every value handed to the store survived: the ones already in the map
      because the gate roots the map's owner, and the one the gating Add had
      not stored yet because the gate roots the pending descriptor. }
    Expect<Integer>(GCanarySweepCount).ToBe(0);
    Expect<Boolean>(
      Target.GetProperty('canary0') is TCanaryValue).ToBe(True);
  finally
    Roots.Clear;
  end;

  { Nothing roots the object now, so it and every canary it holds go on the
    next collection — the protection is scoped to the growth window, not a
    leak. }
  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

procedure TMemoryLimitTests.TestGatedGrowthRootsAnUnrootedMapOwner;
var
  GC: TGarbageCollector;
  Stored: Integer;
  Target: TGatedStoreObjectValue;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCanarySweepCount := 0;

  { No caller-side root anywhere: the object exists only in this bare Pascal
    local. That is the shape the gate's owner push exists for — a native
    builder part-way through populating an object it has not handed to anyone
    yet, so nothing in the engine can see it and no evaluator frame covers it.
    The test above roots the object itself, which leaves the owner push
    unexercised; here it is the only thing standing between the collection and
    both the object and every canary already in its map. }
  Target := TGatedStoreObjectValue.CreateGated;
  GCollectDuringPropertyStore := True;
  try
    Stored := StoreCanariesUntilGateFires(Target, GATE_PROBE_LIMIT);
  finally
    GCollectDuringPropertyStore := False;
  end;

  Expect<Boolean>(Target.GatedProperties.GateCalls > 0).ToBe(True);
  Expect<Integer>(GCanarySweepCount).ToBe(0);
  { Reading a property back would fault rather than fail if the object itself
    had been swept, so assert on the canaries first and on the object second. }
  Expect<Boolean>(Target.GetProperty('canary0') is TCanaryValue).ToBe(True);

  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

procedure TMemoryLimitTests.TestGatedBucketRehashRootsThePendingValue;
var
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
  Stored: Integer;
  Target: TGatedStoreObjectValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedStoreObjectValue.CreateGated;
  Roots.Initialize;
  try
    Roots.Add(Target);
    GC.Collect;
    GCanarySweepCount := 0;

    { The entry-array growth point is the one the other tests reach; this one
      drives the map far enough that Grow's bucket rehash is gated too. Both
      paths receive the pending value from the same Add, and a rehash that
      stopped passing it on would leave the store's own value unrooted across
      the collection while everything already in the map stayed safe — a hole
      no test that stops at the first gate call can see. }
    GCollectDuringPropertyStore := True;
    try
      Stored := StoreCanariesUntilBucketRehash(Target, BUCKET_PROBE_LIMIT);
    finally
      GCollectDuringPropertyStore := False;
    end;

    Expect<Integer>(Target.GatedProperties.Capacity).ToBe(
      BUCKET_REHASH_GATED_CAPACITY);
    Expect<Integer>(GCanarySweepCount).ToBe(0);
  finally
    Roots.Clear;
  end;

  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

procedure TMemoryLimitTests.TestGatedCompactionRootsThePendingValue;
var
  GateCallsBefore: Integer;
  GC: TGarbageCollector;
  I: Integer;
  Roots: TGocciaActiveRootFrame;
  Target: TGatedStoreObjectValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedStoreObjectValue.CreateGated;
  Roots.Initialize;
  try
    Roots.Add(Target);

    { Built with the collection disarmed, so the only collection in this test is
      the one the store under test takes. The properties that will be deleted
      hold the pinned undefined singleton rather than canaries: a deleted
      canary would become garbage and be swept for reasons that have nothing to
      do with the rooting under test. }
    for I := 0 to COMPACTION_TOTAL_PROPERTIES - 1 do
      if I mod COMPACTION_LIVE_STRIDE = 0 then
        StoreCanaries(Target, I, 1)
      else
        Target.DefineProperty('filler' + IntToStr(I),
          TGocciaPropertyDescriptorData.Create(
            TGocciaUndefinedLiteralValue.UndefinedValue,
            [pfEnumerable, pfConfigurable, pfWritable]));

    for I := 0 to COMPACTION_TOTAL_PROPERTIES - 1 do
      if I mod COMPACTION_LIVE_STRIDE <> 0 then
        Target.DeleteProperty('filler' + IntToStr(I));

    GC.Collect;
    GCanarySweepCount := 0;
    GateCallsBefore := Target.GatedProperties.GateCalls;

    { Dead entries now outnumber live ones, so the next store compacts before
      it appends — the third gated growth point, and the only one that shrinks
      rather than grows. Compact reports the two entry arrays it holds at once,
      and it must carry the pending value the same way Grow does. }
    GCollectDuringPropertyStore := True;
    try
      StoreCanaries(Target, COMPACTION_TOTAL_PROPERTIES, 1);
    finally
      GCollectDuringPropertyStore := False;
    end;

    Expect<Boolean>(
      Target.GatedProperties.GateCalls > GateCallsBefore).ToBe(True);
    Expect<Integer>(Target.GatedProperties.DeletedCount).ToBe(0);
    Expect<Integer>(GCanarySweepCount).ToBe(0);
  finally
    Roots.Clear;
  end;
end;

procedure TMemoryLimitTests.TestGatedWindowCollectionSweepsAnUnhandedValue;
var
  Control: TControlValue;
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
  Target: TGatedStoreObjectValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedStoreObjectValue.CreateGated;
  Roots.Initialize;
  try
    Roots.Add(Target);
    GC.Collect;
    GCanarySweepCount := 0;
    GControlSweepCount := 0;

    { The vacuity control for the test above: same object, same stores, same
      collection at the same gate — but this value is never handed to the
      store, so nothing is entitled to root it and the collection must take it.
      Without this, canaries that survived because the collection never ran
      would read exactly like canaries the gate protected.

      Never-handed-out is the shape that answers the question. A value returned
      to a caller and then dropped survives for an unrelated reason — the
      interpreter's own pressure checkpoints protect the current expression
      result — so it would prove nothing about the store. }
    Control := TControlValue.Create;
    Expect<Boolean>(Assigned(Control)).ToBe(True);

    GCollectDuringPropertyStore := True;
    try
      StoreCanariesUntilGateFires(Target, GATE_PROBE_LIMIT);
    finally
      GCollectDuringPropertyStore := False;
    end;

    Expect<Boolean>(Target.GatedProperties.GateCalls > 0).ToBe(True);
    Expect<Integer>(GControlSweepCount).ToBe(1);
    Expect<Integer>(GCanarySweepCount).ToBe(0);
  finally
    Roots.Clear;
  end;
end;

procedure TMemoryLimitTests.TestRealGatedGrowthRootsNativeOnlyValues;
var
  Control: TControlValue;
  GC: TGarbageCollector;
  Stored: Integer;
  Target: TGatedStoreObjectValue;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCanarySweepCount := 0;
  GControlSweepCount := 0;

  { The seam version of this test injects the collection, which pins the
    rooting deterministically but says nothing about whether the production
    gate takes one. This runs the same stores against a ceiling the growth
    cannot fit, so RequireNativeBytes itself is what collects — the H4
    behaviour, exercised end to end through the real budget arithmetic,
    inside the real rooted window.

    Target is deliberately held only in this Pascal local — no frame, no
    external root. The gate's own Owner push is the sole thing keeping the
    object and its stored canaries alive across the collection the gate
    takes, which is the production bare-local-builder shape, and what makes
    removing the owner push fail THIS test and not only the seam variant.

    The vacuity control is folded in rather than split off: the collection
    here is the gate's, not the test's, so "did it actually run" and "did it
    spare only what it was entitled to" are one question. }
  Target := TGatedStoreObjectValue.CreateGated;
  Control := TControlValue.Create;
  Expect<Boolean>(Assigned(Control)).ToBe(True);

  GTightCeilingDuringPropertyStore := True;
  try
    Stored := StoreCanariesUntilGateFires(Target, GATE_PROBE_LIMIT);
  finally
    GTightCeilingDuringPropertyStore := False;
  end;

  Expect<Boolean>(Target.GatedProperties.GateCalls > 0).ToBe(True);
  { The production gate collected, and the store still completed: it made
    room and permitted rather than refusing. }
  Expect<Boolean>(Target.GatedProperties.ForcedCollections > 0).ToBe(True);
  { The value the gating Add had not stored yet, and every value already in
    the map, survived a collection the engine itself decided to take. }
  Expect<Integer>(GCanarySweepCount).ToBe(0);
  { And it really was a sweep: the one value nothing handed to the store is
    gone. }
  Expect<Integer>(GControlSweepCount).ToBe(1);
  Expect<Boolean>(
    Target.GetProperty('canary0') is TCanaryValue).ToBe(True);

  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

{ Stores ACount canary values into ATarget at a stride that leaves a hole
  behind every write, and answers how many it stored. Each value is created and
  handed straight to SetElement, so between the two it exists only in a Pascal
  local — the native-unrooted shape, and the one no evaluator-side frame covers.

  A strided index rather than a sequential one because an append at exactly the
  element count takes the list's Add fast path and never reaches the gate.

  Every other store goes through SetProperty with a stringified index — the
  route the evaluator's property store takes into an array — so the SetProperty
  extension arm's pending-value threading is pinned by the same canaries as
  SetElement's, not merely argued equivalent to it. (AssignProperty is the
  wrong vehicle here: for a not-yet-existing index it funnels into
  DefineProperty's dense-create arm, which the descriptor canary already
  pins.) }
function TMemoryLimitTests.HoleExtendCanaries(
  const ATarget: TGatedElementsArrayValue; const ACount: Integer): Integer;
var
  I: Integer;
  Index: Integer;
begin
  Result := 0;
  for I := 0 to ACount - 1 do
  begin
    Index := ATarget.Elements.Count + ELEMENT_CANARY_STRIDE - 1;
    if I mod 2 = 0 then
      ATarget.SetElement(Index, TCanaryValue.Create)
    else
      ATarget.SetProperty(IntToStr(Index), TCanaryValue.Create);
    Inc(Result);
  end;
end;

procedure TMemoryLimitTests.TestElementGateCollectsBeforeRefusing;
var
  CollectionsBefore: Integer;
  Elements: TGocciaElementList;
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
  Raised: Boolean;
  RequestedSlots: Int64;
begin
  GC := TGarbageCollector.Instance;
  Elements := TGocciaElementList.Create(False);
  PreviousMaxBytes := GC.MaxBytes;
  try
    GC.Collect;
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    { As many pointer slots as the whole budget could hold, so the request the
      gate is handed is the whole budget rounded down — the element-side twin
      of the property gate's request above, and inside the budget on every
      pointer width because the slot count is derived from SizeOf(Pointer)
      rather than assumed. Reported before a single slot is allocated, which is
      what the refusal has to happen for. }
    RequestedSlots := GC.MaxBytes div SizeOf(Pointer);
    CollectionsBefore := GC.TotalCollections;
    Raised := False;
    try
      ExtendElementsWithHoles(Elements, RequestedSlots, nil);
    except
      on TGocciaMemoryLimitError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    { Refused before allocating: the list is untouched, not partly extended. }
    Expect<Integer>(Elements.Count).ToBe(0);

    { The element-side twin of the pinned assertion the property gate carries,
      and it flips for the same reason: the gate forces one collection and
      re-tests before refusing. The rooting the canary tests below pin is what
      makes that safe, and it does not move. }
    Expect<Integer>(GC.TotalCollections - CollectionsBefore).ToBe(1);
  finally
    GC.MaxBytes := PreviousMaxBytes;
    Elements.Free;
  end;
end;

procedure TMemoryLimitTests.TestGatedHoleExtendRootsTheIncomingValue;
var
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
  Stored: Integer;
  Target: TGatedElementsArrayValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedElementsArrayValue.CreateGated;
  Roots.Initialize;
  try
    { The array being stored into is the caller's to keep alive, not the store
      path's, so root it the way a real caller's frame would. That leaves
      exactly one unrooted thing in the picture: the element being stored. }
    Roots.Add(Target);
    GC.Collect;
    GCanarySweepCount := 0;

    GCollectDuringElementStore := True;
    try
      Stored := HoleExtendCanaries(Target, ELEMENT_CANARY_STORES);
    finally
      GCollectDuringElementStore := False;
    end;

    { The gate really was reached — without this the run below could pass
      because nothing ever collected. }
    Expect<Boolean>(Target.GatedElements.GateCalls > 0).ToBe(True);
    { Every value handed to the store survived: the ones already in the list
      because the gate roots the list's owner, and the one the gating store had
      not written yet because the gate roots the pending value. }
    Expect<Integer>(GCanarySweepCount).ToBe(0);
    Expect<Boolean>(
      Target.GetElement(ELEMENT_CANARY_STRIDE - 1) is TCanaryValue).ToBe(True);
  finally
    Roots.Clear;
  end;

  { Nothing roots the array now, so it and every canary it holds go on the next
    collection — the protection is scoped to the growth window, not a leak. }
  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

procedure TMemoryLimitTests.TestGatedHoleExtendRootsAnUnrootedArray;
var
  GC: TGarbageCollector;
  Stored: Integer;
  Target: TGatedElementsArrayValue;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCanarySweepCount := 0;

  { No caller-side root anywhere: the array exists only in this bare Pascal
    local. That is the shape the gate's owner push exists for, and it is not a
    contrived one — TGocciaArrayValue.InitializeNativeFromArguments extends the
    element list with holes while the instance is still inside its own
    construction, so `new Array(n)` reaches the gate from every one of the
    engine's construct sites with the array reachable from nothing at all. The
    test above roots the array itself, which leaves the owner push unexercised;
    here it is the only thing standing between the collection and both the
    array and every canary already in its list. }
  Target := TGatedElementsArrayValue.CreateGated;
  GCollectDuringElementStore := True;
  try
    Stored := HoleExtendCanaries(Target, ELEMENT_CANARY_STORES);
  finally
    GCollectDuringElementStore := False;
  end;

  Expect<Boolean>(Target.GatedElements.GateCalls > 0).ToBe(True);
  Expect<Integer>(GCanarySweepCount).ToBe(0);
  { Reading an element back would fault rather than fail if the array itself
    had been swept, so assert on the canaries first and on the array second. }
  Expect<Boolean>(
    Target.GetElement(ELEMENT_CANARY_STRIDE - 1) is TCanaryValue).ToBe(True);

  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

procedure TMemoryLimitTests.TestGatedHoleExtendRootsTheDescriptorValue;
var
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
  Target: TGatedElementsArrayValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedElementsArrayValue.CreateGated;
  Roots.Initialize;
  try
    Roots.Add(Target);
    GC.Collect;
    GCanarySweepCount := 0;

    { The store family above hands the gate a value it holds in a Pascal local.
      This one is worse: ArrayDefineOwnProperty's dense-create arm reads the
      descriptor's value back out AFTER the extension, and a property
      descriptor is a plain class the collector does not trace — so a
      collection taken at the gate would free the value and leave the read
      storing a dangling pointer into the element list, a use-after-free that
      outlives the store rather than one that ends with it. Reading the field
      before the gate would not help: the hazard is the object's lifetime, not
      when the field is loaded. }
    GCollectDuringElementStore := True;
    try
      Target.DefineProperty(IntToStr(ELEMENT_CANARY_STRIDE),
        TGocciaPropertyDescriptorData.Create(TCanaryValue.Create,
          [pfEnumerable, pfConfigurable, pfWritable]));
    finally
      GCollectDuringElementStore := False;
    end;

    Expect<Boolean>(Target.GatedElements.GateCalls > 0).ToBe(True);
    Expect<Integer>(GCanarySweepCount).ToBe(0);
    { And the value that reached the slot is the one the descriptor carried,
      not a stale pointer that happens to still read as a canary. }
    Expect<Boolean>(
      Target.GetElement(ELEMENT_CANARY_STRIDE) is TCanaryValue).ToBe(True);
  finally
    Roots.Clear;
  end;

  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(1);
end;

procedure TMemoryLimitTests.TestGatedElementWindowSweepsAnUnhandedValue;
var
  Control: TControlValue;
  GC: TGarbageCollector;
  Roots: TGocciaActiveRootFrame;
  Target: TGatedElementsArrayValue;
begin
  GC := TGarbageCollector.Instance;
  Target := TGatedElementsArrayValue.CreateGated;
  Roots.Initialize;
  try
    Roots.Add(Target);
    GC.Collect;
    GCanarySweepCount := 0;
    GControlSweepCount := 0;

    { The vacuity control for the element tests above: same array, same stores,
      same collection at the same gate — but this value is never handed to the
      store, so nothing is entitled to root it and the collection must take it.
      Without this, canaries that survived because the collection never ran
      would read exactly like canaries the gate protected. }
    Control := TControlValue.Create;
    Expect<Boolean>(Assigned(Control)).ToBe(True);

    GCollectDuringElementStore := True;
    try
      HoleExtendCanaries(Target, ELEMENT_CANARY_STORES);
    finally
      GCollectDuringElementStore := False;
    end;

    Expect<Boolean>(Target.GatedElements.GateCalls > 0).ToBe(True);
    Expect<Integer>(GControlSweepCount).ToBe(1);
    Expect<Integer>(GCanarySweepCount).ToBe(0);
  finally
    Roots.Clear;
  end;
end;

procedure TMemoryLimitTests.TestRealGatedHoleExtendRootsTheIncomingValue;
var
  Control: TControlValue;
  GC: TGarbageCollector;
  Stored: Integer;
  Target: TGatedElementsArrayValue;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCanarySweepCount := 0;
  GControlSweepCount := 0;

  { The element-side twin of the real-gate property test: the same stores the
    seam version makes, but against a ceiling each extension cannot fit, so
    the collection is the production RequireNativeBytes'. Every one of these
    stores is gated — the element gate has no small-block threshold — so this
    runs the forced-collect-and-re-test path repeatedly rather than once.

    As in the property twin, Target lives only in this Pascal local: the
    gate's Owner push is the sole root across each collection, so removing
    that push fails this test, not only the seam variant. }
  Target := TGatedElementsArrayValue.CreateGated;
  Control := TControlValue.Create;
  Expect<Boolean>(Assigned(Control)).ToBe(True);

  GTightCeilingDuringElementStore := True;
  try
    Stored := HoleExtendCanaries(Target, ELEMENT_CANARY_STORES);
  finally
    GTightCeilingDuringElementStore := False;
  end;

  Expect<Boolean>(Target.GatedElements.GateCalls > 0).ToBe(True);
  Expect<Boolean>(Target.GatedElements.ForcedCollections > 0).ToBe(True);
  Expect<Integer>(GCanarySweepCount).ToBe(0);
  Expect<Integer>(GControlSweepCount).ToBe(1);
  Expect<Boolean>(
    Target.GetElement(ELEMENT_CANARY_STRIDE - 1) is TCanaryValue).ToBe(True);

  GC.Collect;
  Expect<Integer>(GCanarySweepCount).ToBe(Stored);
end;

procedure TMemoryLimitTests.TestSyncCatchCannotSwallowRefusal;
const
  SourceText =
    'let caught = false;' + sLineBreak +
    'try {' + sLineBreak +
    '  ' + OVER_BUDGET_SOURCE_TAIL + sLineBreak +
    '} catch (e) {' + sLineBreak +
    '  caught = true;' + sLineBreak +
    '}';
begin
  { Nothing but an escaping TGocciaMemoryLimitError can produce True here:
    had the catch block absorbed the refusal, the script would have run to
    completion and Execute would have returned normally. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'memory-limit-sync-interpreted.js', TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'memory-limit-sync-bytecode.js', TGocciaMemoryLimitError)).ToBe(True);
end;

procedure TMemoryLimitTests.TestAsyncFunctionCatchCannotSwallowRefusal;
const
  SourceText =
    'const grow = async () => {' + sLineBreak +
    '  ' + OVER_BUDGET_SOURCE_TAIL + sLineBreak +
    '};' + sLineBreak +
    'const main = async () => {' + sLineBreak +
    '  try {' + sLineBreak +
    '    await grow();' + sLineBreak +
    '  } catch (e) {}' + sLineBreak +
    '};' + sLineBreak +
    'main();';
begin
  { Regression guard for the async path specifically: a refusal escaping an
    async function body used to be converted into a spurious access
    violation, which the guest could then catch as an ordinary Error. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'memory-limit-async-interpreted.js', TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'memory-limit-async-bytecode.js', TGocciaMemoryLimitError)).ToBe(True);
end;

procedure TMemoryLimitTests.TestPromiseExecutorCatchCannotSwallowRefusal;
const
  SourceText =
    'try {' + sLineBreak +
    '  new Promise((resolve) => {' + sLineBreak +
    '    ' + OVER_BUDGET_SOURCE_TAIL + sLineBreak +
    '    resolve(1);' + sLineBreak +
    '  }).catch(() => {});' + sLineBreak +
    '} catch (e) {}';
begin
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'memory-limit-executor-interpreted.js', TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'memory-limit-executor-bytecode.js', TGocciaMemoryLimitError)).ToBe(True);
end;

procedure TMemoryLimitTests.TestAsyncGeneratorReturnCannotSwallowRefusal;
const
  { `for await ... break` runs the async generator's `.return()`, which
    executes the body's `finally` as guest code. The refusal raised there
    reaches the generator resume handler on the return path. Before the fix
    that handler had no limit arm: the interpreter folded the ceiling into a
    catchable rejection (guest caught it and kept running) while the VM was
    fatal — a swallow AND a mode divergence. Both executors must now be fatal. }
  SourceText =
    'const obj = { async *g() {' + sLineBreak +
    '  try { yield 1; } finally { ' + OVER_BUDGET_SOURCE_TAIL + ' }' +
    sLineBreak +
    '} };' + sLineBreak +
    'const main = async () => {' + sLineBreak +
    '  try {' + sLineBreak +
    '    for await (const v of obj.g()) { break; }' + sLineBreak +
    '  } catch (e) {}' + sLineBreak +
    '};' + sLineBreak +
    'main();';
begin
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'memory-limit-async-generator-interpreted.js', TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'memory-limit-async-generator-bytecode.js', TGocciaMemoryLimitError)).ToBe(True);
end;

procedure TMemoryLimitTests.TestIteratorCloseCannotSwallowRefusal;
const
  { The close path, which is the one place the engine deliberately throws
    errors away. A `for..of` body that throws closes the iterator, and
    ES2026 §7.4.11 IteratorClose step 5 says the body's completion wins over
    anything `return()` raises — so the engine's Close*PreservingError helpers
    swallow whatever comes out of `return()`.

    That rule is written over Completion Records: it is about one guest
    completion displacing another. A host resource ceiling never becomes a
    Completion Record, so suppressing it there is not the spec's instruction,
    it is a hole — and precisely the shape the ceiling contract exists to
    forbid, since a guest could refuse allocations for free by growing inside
    a return() reached from a throwing loop body. Both executors must let it
    through. }
  SourceText =
    'const iterable = {' + sLineBreak +
    '  [Symbol.iterator]() {' + sLineBreak +
    '    return {' + sLineBreak +
    '      next: () => ({ value: 1, done: false }),' + sLineBreak +
    '      return: () => { ' + OVER_BUDGET_SOURCE_TAIL + ' }' + sLineBreak +
    '    };' + sLineBreak +
    '  }' + sLineBreak +
    '};' + sLineBreak +
    'try {' + sLineBreak +
    '  for (const v of iterable) { throw new Error("body"); }' + sLineBreak +
    '} catch (e) {}';
begin
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'memory-limit-iterator-close-interpreted.js',
    TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'memory-limit-iterator-close-bytecode.js',
    TGocciaMemoryLimitError)).ToBe(True);
end;

procedure TMemoryLimitTests.TestScriptErrorsStayCatchable;
const
  SourceText =
    'let caught = false;' + sLineBreak +
    'try {' + sLineBreak +
    '  null.property;' + sLineBreak +
    '} catch (e) {' + sLineBreak +
    '  caught = true;' + sLineBreak +
    '}';
begin
  { The counterweight to the guards above: the same budget must not turn an
    ordinary in-language error into a host-level failure. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'memory-limit-script-error-interpreted.js', TGocciaMemoryLimitError)).ToBe(False);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'memory-limit-script-error-bytecode.js', TGocciaMemoryLimitError)).ToBe(False);
end;

procedure TMemoryLimitTests.TestSyncCatchCannotSwallowIntegrityFault;
const
  SourceText =
    'try {' + sLineBreak +
    '  ' + INTEGRITY_FAULT_GLOBAL_NAME + '();' + sLineBreak +
    '} catch (e) {' + sLineBreak +
    '  ' + SWALLOW_WITNESS_GLOBAL_NAME + '();' + sLineBreak +
    '}';
begin
  { An engine-integrity fault is not a JavaScript error and must not become
    one. The interpreter used to fold every unlisted Pascal exception into a
    catchable Error object, which turned a use-after-free — the shape an
    unrooted evaluator temporary produces under memory pressure — into
    `catch (e)` and let the script keep running on corrupted state. Both
    executors must now unwind to the host.

    The escaping EObjectCheck is the load-bearing assertion; the witness is the
    other half of the same question, and it distinguishes the two ways this can
    fail — a fault that never reached the host because the guest caught it,
    versus one that never fired at all. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'integrity-fault-sync-interpreted.js', EObjectCheck)).ToBe(True);
  Expect<Boolean>(FSwallowedFaultReported).ToBe(False);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'integrity-fault-sync-bytecode.js', EObjectCheck)).ToBe(True);
  Expect<Boolean>(FSwallowedFaultReported).ToBe(False);
end;

procedure TMemoryLimitTests.TestAsyncFunctionCatchCannotSwallowIntegrityFault;
const
  SourceText =
    'const fault = async () => {' + sLineBreak +
    '  ' + INTEGRITY_FAULT_GLOBAL_NAME + '();' + sLineBreak +
    '};' + sLineBreak +
    'const main = async () => {' + sLineBreak +
    '  try {' + sLineBreak +
    '    await fault();' + sLineBreak +
    '  } catch (e) {' + sLineBreak +
    '    ' + SWALLOW_WITNESS_GLOBAL_NAME + '();' + sLineBreak +
    '  }' + sLineBreak +
    '};' + sLineBreak +
    'main();';
begin
  { The await and promise-reaction boundaries convert a Pascal exception into a
    rejection, which `catch` then absorbs — the same swallow by a different
    route. The witness matters more here than on the synchronous path: a throw
    from inside this catch block would only become another rejection nothing
    observes, so a host-side flag is the only way the swallow can report
    itself. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'integrity-fault-async-interpreted.js', EObjectCheck)).ToBe(True);
  Expect<Boolean>(FSwallowedFaultReported).ToBe(False);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'integrity-fault-async-bytecode.js', EObjectCheck)).ToBe(True);
  Expect<Boolean>(FSwallowedFaultReported).ToBe(False);
end;

procedure TMemoryLimitTests.TestSandboxFsPromiseCannotSwallowRefusal;
begin
  { fs.promises hands the guest a promise, and a promise is a `catch` away from
    absorbing whatever settles it. Both sandbox boundaries used to convert every
    Pascal exception into a rejection, so the budget stopped being a ceiling the
    moment a script wrapped its filesystem calls in try/catch. }
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_JOB_SOURCE,
    TGocciaInterpreterExecutor.Create, 'sandbox-refusal-job-interpreted.js',
    TGocciaMemoryLimitError, TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_JOB_SOURCE,
    TGocciaBytecodeExecutor.Create, 'sandbox-refusal-job-bytecode.js',
    TGocciaMemoryLimitError, TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_ARGUMENT_SOURCE,
    TGocciaInterpreterExecutor.Create,
    'sandbox-refusal-argument-interpreted.js',
    TGocciaMemoryLimitError, TGocciaMemoryLimitError)).ToBe(True);
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_ARGUMENT_SOURCE,
    TGocciaBytecodeExecutor.Create, 'sandbox-refusal-argument-bytecode.js',
    TGocciaMemoryLimitError, TGocciaMemoryLimitError)).ToBe(True);
end;

procedure TMemoryLimitTests.TestSandboxFsPromiseCannotSwallowIntegrityFault;
begin
  { The stronger half of the same guard: a use-after-free reaching either
    sandbox boundary must not become a rejected promise the guest catches and
    carries on from. }
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_JOB_SOURCE,
    TGocciaInterpreterExecutor.Create, 'sandbox-integrity-job-interpreted.js',
    EObjectCheck, EObjectCheck)).ToBe(True);
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_JOB_SOURCE,
    TGocciaBytecodeExecutor.Create, 'sandbox-integrity-job-bytecode.js',
    EObjectCheck, EObjectCheck)).ToBe(True);
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_ARGUMENT_SOURCE,
    TGocciaInterpreterExecutor.Create,
    'sandbox-integrity-argument-interpreted.js',
    EObjectCheck, EObjectCheck)).ToBe(True);
  Expect<Boolean>(SandboxFaultEscapesScript(SANDBOX_ARGUMENT_SOURCE,
    TGocciaBytecodeExecutor.Create, 'sandbox-integrity-argument-bytecode.js',
    EObjectCheck, EObjectCheck)).ToBe(True);
end;

procedure TMemoryLimitTests.TestSandboxFsPromiseErrorsStayCatchable;
const
  { Nothing is armed here, so the only thing that can settle the promise is the
    ENOENT the virtual filesystem produces. The script asserts it arrived as a
    Node-shaped error object; if it did not, the throw escapes as an exception
    that is not the class under test and the helper propagates it. }
  SourceText =
    'import fs from "fs";' + sLineBreak +
    'let caught = null;' + sLineBreak +
    'try {' + sLineBreak +
    '  await fs.promises.readFile("/missing.txt", "utf8");' + sLineBreak +
    '} catch (e) {' + sLineBreak +
    '  caught = e;' + sLineBreak +
    '}' + sLineBreak +
    'if (caught === null || caught.code !== "ENOENT") {' + sLineBreak +
    '  throw new Error("sandbox fs rejection was not catchable");' +
    sLineBreak +
    '}';
begin
  { The counterweight to the two guards above: hardening the boundary must not
    cost the sandbox its ordinary, guest-visible filesystem errors. Asserting
    against Exception rather than a specific class makes any escape at all a
    failure. }
  Expect<Boolean>(SandboxFaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create, 'sandbox-catchable-interpreted.js',
    Exception, nil)).ToBe(False);
  Expect<Boolean>(SandboxFaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create, 'sandbox-catchable-bytecode.js',
    Exception, nil)).ToBe(False);
end;

procedure TMemoryLimitTests.TestResponseJsonCannotSwallowAuditDeliveryFailure;
const
  { ES2026 §27.2.1.3.2 step 9 has Resolve read `then` off the parsed body, and
    that read walks the prototype chain, so a getter on Object.prototype runs
    guest code inside Response.json's conversion arm. The getter calls the
    Function constructor, which is capability-audited; with a sink that cannot
    deliver, EmitCapabilityAudit raises from there. }
  SourceText =
    'Object.defineProperty(Object.prototype, "then", {' + sLineBreak +
    '  get: () => { Function("return 1"); },' + sLineBreak +
    '  configurable: true,' + sLineBreak +
    '});' + sLineBreak +
    'new Response(''{"a":1}'').json();';
begin
  { The arm around the parse rejects with a SyntaxError, so before the guard it
    reported an undeliverable audit record as a malformed body and the host
    heard nothing at all. Being the only guarded block in this change that runs
    guest code, it is also the only one where omitting the capability-audit
    class from the allowlist was observable — which is why the allowlist is now
    stated once, in Goccia.UncatchableFault.pas, rather than per boundary. }
  FInstallFailingAuditSink := True;
  try
    Expect<Boolean>(FaultEscapesScript(SourceText,
      TGocciaInterpreterExecutor.Create, 'response-json-audit-interpreted.js',
      EGocciaCapabilityAuditDeliveryError)).ToBe(True);
    Expect<Boolean>(FaultEscapesScript(SourceText,
      TGocciaBytecodeExecutor.Create, 'response-json-audit-bytecode.js',
      EGocciaCapabilityAuditDeliveryError)).ToBe(True);
  finally
    FInstallFailingAuditSink := False;
  end;
end;

procedure TMemoryLimitTests.TestDiagnosticIdentityFailureFailsClosed;
var
  FirstLine: Integer;
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
begin
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    { A filesystem content provider explicitly requires a handle-derived
      identity. Blank means that lookup failed; retaining it under a lexical
      spelling would let a later alias bypass host ownership. }
    Scope.Register('unidentified-host.js', 'HOST_SECRET', True, '', True);
    Expect<Int64>(Scope.RetainedBytes).ToBe(0);

    { Once any filesystem identity is unresolved, no source in this scope is
      disclosed: a later successfully-identified path could still alias the
      unknown host file. Locations remain available outside this registry. }
    Scope.Register('apparently-guest.js', 'GUEST_SOURCE', False,
      '#test:guest-file', True);
    Expect<Boolean>(Scope.TryGetGuestWindow('apparently-guest.js', 1, 0, 0,
      Window, FirstLine)).ToBe(False);
    Expect<Integer>(Window.Count).ToBe(0);
  finally
    Window.Free;
    Scope.Free;
  end;
end;

procedure TMemoryLimitTests.TestDiagnosticCanonicalIdentityKeepsHostOwnership;
var
  FirstLine: Integer;
  RetainedAfterHost: Int64;
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
begin
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    Scope.Register('host-spelling.js', 'HOST_ALIAS_SECRET', True,
      '#test:volume-and-file-index', True);
    RetainedAfterHost := Scope.RetainedBytes;
    Expect<Boolean>(RetainedAfterHost > 0).ToBe(True);

    { The same opened-file identity under another spelling is not enrolled as
      a second guest entry. First registration wins, so the host classification
      is durable across symlinks, junctions, hardlinks, and case aliases. }
    Scope.Register('guest-alias.js', 'HOST_ALIAS_SECRET', False,
      '#test:volume-and-file-index', True);
    Expect<Int64>(Scope.RetainedBytes).ToBe(RetainedAfterHost);
    Expect<Boolean>(Scope.TryGetGuestWindow('guest-alias.js', 1, 0, 0,
      Window, FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow('host-spelling.js', 1, 0, 0,
      Window, FirstLine)).ToBe(False);
  finally
    Window.Free;
    Scope.Free;
  end;
end;

procedure TMemoryLimitTests.TestDiagnosticSourceAccountingUsesRetainedBytes;
var
  AccountedBytes, BaselineBytes: Int64;
  GC: TGarbageCollector;
  ManyLines: TStringList;
  ManyLinesText, OneUnit, TwoUnits: string;
  PreviousMaxBytes: Int64;
  Probe, Scope: TGocciaDiagnosticSourceScope;
  I: Integer;
begin
  OneUnit := UnicodeString(WideChar($00E9));
  TwoUnits := UnicodeString(WideChar($D83D)) + WideChar($DE00);
  Expect<Integer>(Length(OneUnit)).ToBe(1);
  Expect<Integer>(Length(TwoUnits)).ToBe(2);
  Expect<Int64>(DiagnosticStringRetainedBytes(TwoUnits) -
    DiagnosticStringRetainedBytes(OneUnit)).ToBe(SizeOf(Char));

  { Measure the exact retained representation once, then prove the same figure
    is both the registry counter and the collector reservation. }
  GC := TGarbageCollector.Instance;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;
  Scope := TGocciaDiagnosticSourceScope.Create;
  try
    Scope.Register('unicode.js', OneUnit + TwoUnits + sLineBreak + OneUnit,
      False, '#test:unicode-source', True);
    AccountedBytes := Scope.RetainedBytes;
    Expect<Boolean>(AccountedBytes >
      DiagnosticStringRetainedBytes(OneUnit + TwoUnits + sLineBreak +
        OneUnit)).ToBe(True);
    Expect<Int64>(GC.BytesAllocated - BaselineBytes).ToBe(AccountedBytes);
  finally
    Scope.Free;
  end;
  Expect<Int64>(GC.BytesAllocated).ToBe(BaselineBytes);

  { One byte less than the measured retained figure must refuse the source and
    must not leave any uncharged representation behind. }
  Probe := TGocciaDiagnosticSourceScope.Create;
  try
    Probe.Register('probe.js', OneUnit + TwoUnits + sLineBreak + OneUnit,
      False, '#test:probe-source', True);
    AccountedBytes := Probe.RetainedBytes;
  finally
    Probe.Free;
  end;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;
  PreviousMaxBytes := GC.MaxBytes;
  Scope := TGocciaDiagnosticSourceScope.Create;
  try
    GC.MaxBytes := BaselineBytes + AccountedBytes - 1;
    Scope.Register('refused.js', OneUnit + TwoUnits + sLineBreak + OneUnit,
      False, '#test:refused-source', True);
    Expect<Int64>(Scope.RetainedBytes).ToBe(0);
  finally
    Scope.Free;
    GC.MaxBytes := PreviousMaxBytes;
  end;

  { UTF-16 content length alone is below the per-module cap, but splitting
    many short lines retains a header plus container slots for every line. The
    actual representation crosses the cap and must therefore be rejected. }
  ManyLines := TStringList.Create;
  try
    for I := 1 to 20000 do
      ManyLines.Add(OneUnit);
    ManyLinesText := ManyLines.Text;
  finally
    ManyLines.Free;
  end;
  Expect<Boolean>(DiagnosticStringRetainedBytes(ManyLinesText) <
    GOCCIA_DIAGNOSTIC_SOURCE_CAP_BYTES).ToBe(True);
  Scope := TGocciaDiagnosticSourceScope.Create;
  try
    Scope.Register('many-lines.js', ManyLinesText, False,
      '#test:many-lines-source', True);
    Expect<Int64>(Scope.RetainedBytes).ToBe(0);
  finally
    Scope.Free;
  end;
end;

procedure TMemoryLimitTests.TestDiagnosticRegistryCommitRollsBack;
var
  BaselineBytes: Int64;
  FirstLine: Integer;
  GC: TGarbageCollector;
  Raised: Boolean;
  Scope: TFailingDiagnosticSourceScope;
  Window: TStringList;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  BaselineBytes := GC.BytesAllocated;
  Scope := TFailingDiagnosticSourceScope.Create(2);
  Window := TStringList.Create;
  try
    Raised := False;
    try
      Scope.Register('transaction.js', 'const transaction = true;', False,
        '#test:transaction-source', True);
    except
      on E: EOutOfMemory do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Int64>(Scope.RetainedBytes).ToBe(0);
    Expect<Int64>(GC.BytesAllocated).ToBe(BaselineBytes);
    Expect<Boolean>(Scope.TryGetGuestWindow('transaction.js', 1, 0, 0,
      Window, FirstLine)).ToBe(False);

    { A retry of the same identity succeeds after disarming the injected
      failure, proving the failed transaction left no inaccessible map entry. }
    Scope.FailAtIndex := 0;
    Scope.Register('transaction.js', 'const transaction = true;', False,
      '#test:transaction-source', True);
    Expect<Boolean>(Scope.RetainedBytes > 0).ToBe(True);
    Expect<Boolean>(Scope.TryGetGuestWindow('transaction.js', 1, 0, 0,
      Window, FirstLine)).ToBe(True);
  finally
    Window.Free;
    Scope.Free;
  end;
  Expect<Int64>(GC.BytesAllocated).ToBe(BaselineBytes);
end;

begin
  { The gated-growth tests collect explicitly, and some of them run before this
    program has built its first engine. The primitive singletons are reachable
    only from class variables the collector cannot see, and nothing pins them
    until TGocciaEngine.PinSingletons runs during engine construction — so a
    collection on a fresh collector sweeps UndefinedValue out from under every
    class variable still pointing at it. Production never has that window:
    every site that collects postdates engine construction, and the pins the
    engine takes are process-wide and are NOT released by realm teardown (the
    realm only unpins what it stored in a slot). Taking the same pins here, once
    and for the life of the program, gives this program the floor an engine
    would have given it — same set the engine pins, HoleValue included. }
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  TGarbageCollector.Instance.PinObject(TGocciaHoleValue.HoleValue);

  TestRunnerProgram.AddSuite(TMemoryLimitTests.Create('Memory limit'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
