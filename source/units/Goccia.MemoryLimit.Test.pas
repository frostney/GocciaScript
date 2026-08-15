program Goccia.MemoryLimit.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.CapabilityAudit,
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
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
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
    procedure TestSyncCatchCannotSwallowRefusal;
    procedure TestAsyncFunctionCatchCannotSwallowRefusal;
    procedure TestPromiseExecutorCatchCannotSwallowRefusal;
    procedure TestAsyncGeneratorReturnCannotSwallowRefusal;
    procedure TestScriptErrorsStayCatchable;
    procedure TestSyncCatchCannotSwallowIntegrityFault;
    procedure TestAsyncFunctionCatchCannotSwallowIntegrityFault;
    procedure TestSandboxFsPromiseCannotSwallowRefusal;
    procedure TestSandboxFsPromiseCannotSwallowIntegrityFault;
    procedure TestSandboxFsPromiseErrorsStayCatchable;
    procedure TestResponseJsonCannotSwallowAuditDeliveryFailure;
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

procedure TMemoryLimitTests.BeforeEach;
begin
  inherited BeforeEach;
  { The gate reads its budget off the collector, and a program with no
    collector is unbounded by construction — without this the direct gate
    tests would assert against a disabled budget and pass vacuously. }
  TGarbageCollector.Initialize;
end;

procedure TMemoryLimitTests.SetupTests;
begin
  Test('The gate refuses a request larger than the remaining budget',
    TestGateRefusesOverBudgetRequest);
  Test('The gate permits a request that fits the remaining budget',
    TestGatePermitsInBudgetRequest);
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
  GC: TGarbageCollector;
  PreviousMaxBytes: Int64;
  Raised: Boolean;
begin
  GC := TGarbageCollector.Instance;
  Expect<Boolean>(Assigned(GC)).ToBe(True);
  PreviousMaxBytes := GC.MaxBytes;
  try
    GC.MaxBytes := GC.BytesAllocated + BUDGET_HEADROOM_BYTES;
    Expect<Boolean>(CanAllocateNativeBytes(
      Int64(BUDGET_HEADROOM_BYTES) * 4)).ToBe(False);
    Raised := False;
    try
      RequireNativeBytes(Int64(BUDGET_HEADROOM_BYTES) * 4);
    except
      on TGocciaMemoryLimitError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
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

begin
  TestRunnerProgram.AddSuite(TMemoryLimitTests.Create('Memory limit'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
