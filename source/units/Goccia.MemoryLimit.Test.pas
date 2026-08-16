program Goccia.MemoryLimit.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.MemoryLimit,
  Goccia.TestSetup,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TMemoryLimitTests = class(TTestSuite)
  private
    { The native the integrity-fault tests call. Raises the same class a virtual
      call through a collected value raises under `$OBJECTCHECKS ON`, which is
      how a real unrooted-temporary bug arrives. }
    function RaiseIntegrityFault(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    { Runs ASource under a budget that cannot fit the over-budget allocation and
      with that native bound as a global, and answers whether AFaultClass
      reached the host instead of the script. Any other escaping exception is
      not an answer to that question and propagates. }
    function FaultEscapesScript(const ASource: string;
      const AExecutor: TGocciaExecutor; const AName: string;
      const AFaultClass: ExceptClass): Boolean;
    procedure TestGateRefusesOverBudgetRequest;
    procedure TestGatePermitsInBudgetRequest;
    procedure TestSyncCatchCannotSwallowRefusal;
    procedure TestAsyncFunctionCatchCannotSwallowRefusal;
    procedure TestPromiseExecutorCatchCannotSwallowRefusal;
    procedure TestAsyncGeneratorReturnCannotSwallowRefusal;
    procedure TestScriptErrorsStayCatchable;
    procedure TestSyncCatchCannotSwallowIntegrityFault;
    procedure TestAsyncFunctionCatchCannotSwallowIntegrityFault;
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
end;

function TMemoryLimitTests.RaiseIntegrityFault(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  raise EObjectCheck.Create(INTEGRITY_FAULT_MESSAGE);
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
  try
    Engine.InjectGlobal(INTEGRITY_FAULT_GLOBAL_NAME,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RaiseIntegrityFault,
        INTEGRITY_FAULT_GLOBAL_NAME, 0));
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
    'let caught = false;' + sLineBreak +
    'try {' + sLineBreak +
    '  ' + INTEGRITY_FAULT_GLOBAL_NAME + '();' + sLineBreak +
    '} catch (e) {' + sLineBreak +
    '  caught = true;' + sLineBreak +
    '}';
begin
  { An engine-integrity fault is not a JavaScript error and must not become
    one. The interpreter used to fold every unlisted Pascal exception into a
    catchable Error object, which turned a use-after-free — the shape an
    unrooted evaluator temporary produces under memory pressure — into
    `catch (e)` and let the script keep running on corrupted state. Both
    executors must now unwind to the host. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'integrity-fault-sync-interpreted.js', EObjectCheck)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'integrity-fault-sync-bytecode.js', EObjectCheck)).ToBe(True);
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
    '  } catch (e) {}' + sLineBreak +
    '};' + sLineBreak +
    'main();';
begin
  { The await and promise-reaction boundaries convert a Pascal exception into a
    rejection, which `catch` then absorbs — the same swallow by a different
    route. }
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaInterpreterExecutor.Create,
    'integrity-fault-async-interpreted.js', EObjectCheck)).ToBe(True);
  Expect<Boolean>(FaultEscapesScript(SourceText,
    TGocciaBytecodeExecutor.Create,
    'integrity-fault-async-bytecode.js', EObjectCheck)).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TMemoryLimitTests.Create('Memory limit'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
