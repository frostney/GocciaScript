program Goccia.ExecutionAbort.Test;

{ A host's abort from another thread (ADR 0174): the polls it arms, the
  handshake that keeps a request from being undone, the handle's lifetime, and
  runs of real scripts on worker threads that another thread stops: a bytecode
  loop, a native loop, the engine's own waits, a microtask chain, with pending
  promises left behind, and a worker that runs again afterwards. The last case
  repeats a run under a collection before every reservation. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SyncObjs,
  SysUtils,

  HTTPTypes,
  TestingPascalLibrary,
  TimingUtils,

  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.ExecutionAbort,
  Goccia.FetchTransport,
  Goccia.GarbageCollector,
  Goccia.CLI.JSON.Reporter,
  Goccia.InstructionLimit,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Threading,
  Goccia.Threading.Init,
  Goccia.ThreadCleanupRegistry,
  Goccia.ThreadPolls,
  Goccia.Timeout,
  Goccia.UncatchableFault;

const
  { A script that was stopped ends well inside this; one that was not runs
    until its own limit. }
  PROMPT_END_MS = 3000;
  LET_IT_RUN_MS = 60;
  JOIN_MS = 20000;
  API_HOST = 'api.example';
  RACE_TRIALS = 60;
  POLL_CAP = 100000000;

  { Each runs until something stops it. }
  SCRIPT_BYTECODE_LOOP =
    'let n = 0;' + LineEnding +
    'while (true) { n = n + 1; }';
  SCRIPT_ITERATOR =
    'const it = { [Symbol.iterator]: () => ({ next: () => ({' +
    ' done: false, value: 1 }) }) };' + LineEnding +
    'for (const x of it) {}';
  SCRIPT_NATIVE_LOOP =
    'new Array(2 ** 31).reverse();';
  SCRIPT_ATOMICS_WAIT =
    'const view = new Int32Array(new SharedArrayBuffer(4));' + LineEnding +
    'Atomics.wait(view, 0, 0);';
  SCRIPT_ATOMICS_WAIT_ASYNC =
    'const view = new Int32Array(new SharedArrayBuffer(4));' + LineEnding +
    'await Atomics.waitAsync(view, 0, 0, 30000).value;';
  SCRIPT_FETCH =
    'await fetch("https://api.example/never");';
  SCRIPT_MICROTASK_CHAIN =
    'const again = () => Promise.resolve().then(again);' + LineEnding +
    'again();' + LineEnding +
    'await new Promise(() => {});';
  SCRIPT_PENDING_PROMISES =
    'const pending = [new Promise(() => {}), new Promise(() => {})];' +
    LineEnding +
    'Promise.all(pending).then(() => {});' + LineEnding +
    'Promise.race(pending).then(() => {});' + LineEnding +
    'const held = { list: [], map: new Map() };' + LineEnding +
    'const it = { [Symbol.iterator]: () => ({ next: () => {' + LineEnding +
    '  held.list.push({ a: [1, 2, 3] });' + LineEnding +
    '  held.map.set(held.list.length, { b: "x" + held.list.length });' +
    LineEnding +
    '  return { done: false, value: 1 };' + LineEnding +
    '} }) };' + LineEnding +
    'for (const x of it) {}';
  SCRIPT_QUICK = 'export const answer = 6 * 7;';

type
  TSilentTransport = class(TInterfacedObject, IGocciaFetchTransport)
  private
    FResponders: array of IGocciaFetchResponder;
  public
    procedure Fetch(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
    destructor Destroy; override;
  end;

  { Runs SCRIPT on its own thread the way a host's worker does: its own
    runtime, an armed handle, the run, ClearAbort, and then a quick script on
    a fresh engine to show the worker is usable again. }
  TAbortableRun = class(TThread)
  private
    FScript: string;
    FWithFetch: Boolean;
    FWhileLoops: Boolean;
    FMaxInstructions: Int64;
    FDeadlineMs: Integer;
    FStress: Boolean;
    FRequestOnStart: Boolean;
    FReady: TEvent;
    FSecondScript: string;
    function RunOnce(const AScript: string; const AArm: Boolean;
      const AWithFetch: Boolean): string;
  protected
    procedure Execute; override;
  public
    Handle: IGocciaAbortHandle;
    Outcome: string;
    Elapsed: Int64;
    SecondOutcome: string;
    Quarantined: Integer;
    constructor Create(const AScript: string);
    destructor Destroy; override;
    procedure WaitUntilRunning;
    property WithFetch: Boolean read FWithFetch write FWithFetch;
    property WhileLoops: Boolean read FWhileLoops write FWhileLoops;
    property MaxInstructions: Int64 read FMaxInstructions write FMaxInstructions;
    property DeadlineMs: Integer read FDeadlineMs write FDeadlineMs;
    property Stress: Boolean read FStress write FStress;
    property RequestOnStart: Boolean read FRequestOnStart write FRequestOnStart;
    property SecondScript: string read FSecondScript write FSecondScript;
  end;

  { Polls the way a native loop and the VM do until the abort ends it. }
  TPollMode = (pmInstructions, pmChurnScopes, pmNativeLoopOnly);

  TPollingThread = class(TThread)
  private
    FReady: TEvent;
    FMode: TPollMode;
  protected
    procedure Execute; override;
  public
    Handle: IGocciaAbortHandle;
    Polls: Int64;
    Raised: string;
    Elapsed: Int64;
    constructor Create(const AMode: TPollMode);
    destructor Destroy; override;
    procedure WaitUntilArmed;
  end;

  TRequestingThread = class(TThread)
  private
    FHandle: IGocciaAbortHandle;
    FDelayMs: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHandle: IGocciaAbortHandle;
      const ADelayMs: Integer);
  end;

  TCleanupThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Handle: IGocciaAbortHandle;
    DisarmedByCleanup: Boolean;
    ArmedBefore: Boolean;
    constructor Create;
  end;

  TExecutionAbortTests = class(TTestSuite)
  private
    function PollsUntilAbort(const AViaTimeout: Boolean;
      out AClassName: string): Int64;
    function StopsScript(const AScript: string; const ARun: TAbortableRun;
      out AElapsed: Int64): string;
    procedure TestArmingTurnsBothPollsOn;
    procedure TestUnrequestedAbortChangesNothing;
    procedure TestRequestEndsTheInstructionPoll;
    procedure TestRequestEndsTheTimeoutPoll;
    procedure TestRequestIsAnUncatchableInstructionLimitFamilyError;
    procedure TestReportersNameTheAbortApartFromTheLimit;
    procedure TestScopeChurnCannotUndoARequest;
    procedure TestStartAndClearBudgetsCannotUndoARequest;
    procedure TestRequestOutranksASpentBudgetAndAnExpiredDeadline;
    procedure TestClearAbortDisarmsAndKeepsBudgets;
    procedure TestLateRequestDoesNotReachTheNextRun;
    procedure TestRequestAfterThreadCleanupIsHarmless;
    procedure TestAbortOnlyHasNoDeadlineToReport;
    procedure TestCaptureAgainDetachesThePreviousHandle;
    procedure TestRacingRecomputeNeverLosesTheRequest;
    procedure TestBytecodeLoopIsStopped;
    procedure TestIteratorLoopIsStopped;
    procedure TestNativeLoopIsStopped;
    procedure TestAtomicsWaitIsStopped;
    procedure TestAtomicsWaitAsyncIsStopped;
    procedure TestFetchWaitIsStopped;
    procedure TestMicrotaskChainIsStopped;
    procedure TestPendingPromisesAreLeftSafeToFree;
    procedure TestWorkerRunsAgainAfterAnAbort;
    procedure TestAbortBeforeTheRunStartsEndsItAtOnce;
    procedure TestStoppedRunUnderCollectionBeforeEveryReservation;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

{ TSilentTransport }

procedure TSilentTransport.Fetch(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
begin
  SetLength(FResponders, Length(FResponders) + 1);
  FResponders[High(FResponders)] := AResponder;
end;

destructor TSilentTransport.Destroy;
begin
  FResponders := nil;
  inherited Destroy;
end;

{ TAbortableRun }

constructor TAbortableRun.Create(const AScript: string);
begin
  FScript := AScript;
  FReady := TEvent.Create(nil, True, False, '');
  FSecondScript := SCRIPT_QUICK;
  inherited Create(True);
end;

destructor TAbortableRun.Destroy;
begin
  FReady.Free;
  inherited Destroy;
end;

procedure TAbortableRun.WaitUntilRunning;
begin
  FReady.WaitFor(JOIN_MS);
end;

{ Returns the class name of what ended the run, or the module's `answer`
  export. AArm arms the thread after the engine exists and before it runs. }
function TAbortableRun.RunOnce(const AScript: string; const AArm: Boolean;
  const AWithFetch: Boolean): string;
var
  Source: TStringList;
  Runtime: TGocciaRuntime;
  Capabilities: TGocciaCapabilities;
  Started: Int64;
begin
  Result := '';
  Source := TStringList.Create;
  Source.Text := AScript;
  if AWithFetch then
    Capabilities := TGocciaCapabilities.None.Allow(gcNet, API_HOST)
  else
    Capabilities := TGocciaCapabilities.None;
  Runtime := TGocciaRuntime.Create('main.js', Source, Capabilities);
  try
    Runtime.Engine.SourceType := stModule;
    if FWhileLoops then
      Runtime.Engine.Compatibility := [cfWhileLoops];
    if AWithFetch then
    begin
      Runtime.Install(TGocciaFetchRuntimeExtension.Create);
      Runtime.Engine.FetchTransport := TSilentTransport.Create;
    end;
    if AArm then
    begin
      Handle := CaptureAbortHandle;
      if FDeadlineMs > 0 then
        StartExecutionTimeout(FDeadlineMs);
      if FMaxInstructions > 0 then
        StartInstructionLimit(FMaxInstructions);
      if FRequestOnStart then
        Handle.Request;
      FReady.SetEvent;
    end;
    Started := GetMilliseconds;
    try
      Runtime.Execute;
      Result := 'finished';
    except
      on E: Exception do
        Result := E.ClassName;
    end;
    if AArm then
    begin
      Elapsed := GetMilliseconds - Started;
      ClearExecutionTimeout;
      ClearInstructionLimit;
      ClearAbort;
    end;
  finally
    Runtime.Free;
    Source.Free;
  end;
end;

procedure TAbortableRun.Execute;
var
  GC: TGarbageCollector;
begin
  InitThreadRuntime;
  try
    if FStress then
    begin
      GC := TGarbageCollector.Instance;
      { As Goccia.GCStress.Test: kept in quarantine, or freed under memcheck. }
      GC.QuarantineSwept := GetEnvironmentVariable('GOCCIA_GC_STRESS_FREE') <> '1';
      GC.CollectBeforeEveryReservation := True;
    end;
    Outcome := RunOnce(FScript, True, FWithFetch);
    if FStress then
    begin
      GC := TGarbageCollector.Instance;
      GC.CollectBeforeEveryReservation := False;
      GC.Collect;
      GC.QuarantineSwept := False;
      Quarantined := TGarbageCollector.QuarantineViolations;
    end;
    { The same thread, a fresh engine, nothing armed. }
    SecondOutcome := RunOnce(FSecondScript, False, False);
  finally
    FReady.SetEvent;
    ShutdownThreadRuntime;
  end;
end;

{ TPollingThread }

constructor TPollingThread.Create(const AMode: TPollMode);
begin
  FMode := AMode;
  FReady := TEvent.Create(nil, True, False, '');
  inherited Create(True);
end;

destructor TPollingThread.Destroy;
begin
  FReady.Free;
  inherited Destroy;
end;

procedure TPollingThread.WaitUntilArmed;
begin
  FReady.WaitFor(JOIN_MS);
end;

procedure TPollingThread.Execute;
var
  Started: Int64;
begin
  Handle := CaptureAbortHandle;
  FReady.SetEvent;
  Started := GetMilliseconds;
  try
    try
      while Polls < POLL_CAP do
      begin
        case FMode of
          pmChurnScopes:
            begin
              // Every push and pop recomputes the limit the request lowered.
              PushInstructionLimitScope(1000000000);
              PollInstructionLimit(CaptureInstructionLimitState);
              PopInstructionLimitScope;
            end;
          pmInstructions:
            PollInstructionLimit(CaptureInstructionLimitState);
        end;
        CheckExecutionTimeout;
        Inc(Polls);
      end;
      Raised := 'never';
    except
      on E: Exception do
        Raised := E.ClassName;
    end;
  finally
    Elapsed := GetMilliseconds - Started;
    ClearAbort;
    ClearInstructionLimit;
    ClearExecutionTimeout;
  end;
end;

{ TRequestingThread }

constructor TRequestingThread.Create(const AHandle: IGocciaAbortHandle;
  const ADelayMs: Integer);
begin
  FHandle := AHandle;
  FDelayMs := ADelayMs;
  inherited Create(False);
end;

procedure TRequestingThread.Execute;
begin
  if FDelayMs > 0 then
    Sleep(FDelayMs);
  RequestAbort(FHandle);
  FHandle := nil;
end;

{ TCleanupThread }

constructor TCleanupThread.Create;
begin
  inherited Create(True);
end;

procedure TCleanupThread.Execute;
begin
  Handle := CaptureAbortHandle;
  ArmedBefore := GThreadPolls.TimeoutArmed and
    GThreadPolls.InstructionLimitActive;
  // What ShutdownThreadRuntime does as a worker exits without ClearAbort.
  RunThreadvarCleanups;
  DisarmedByCleanup := (GThreadPolls.Any = 0) and not AbortIsArmed;
end;

{ TExecutionAbortTests }

procedure TExecutionAbortTests.SetupTests;
begin
  Test('capturing a handle turns the instruction and timeout polls on',
    TestArmingTurnsBothPollsOn);
  Test('an armed handle nobody requested changes nothing',
    TestUnrequestedAbortChangesNothing);
  Test('a request from another thread ends the instruction poll',
    TestRequestEndsTheInstructionPoll);
  Test('a request from another thread ends the timeout poll',
    TestRequestEndsTheTimeoutPoll);
  Test('the abort error is uncatchable and part of the limit family',
    TestRequestIsAnUncatchableInstructionLimitFamilyError);
  Test('the JSON reporter names an abort apart from the instruction limit',
    TestReportersNameTheAbortApartFromTheLimit);
  Test('scope pushes and pops cannot undo a request',
    TestScopeChurnCannotUndoARequest);
  Test('starting and clearing the budgets cannot undo a request',
    TestStartAndClearBudgetsCannotUndoARequest);
  Test('a request outranks a spent budget and an expired deadline',
    TestRequestOutranksASpentBudgetAndAnExpiredDeadline);
  Test('ClearAbort disarms the polls and keeps the host''s budgets',
    TestClearAbortDisarmsAndKeepsBudgets);
  Test('a late request on a cleared handle does not reach the next run',
    TestLateRequestDoesNotReachTheNextRun);
  Test('a request after the thread''s cleanup is harmless',
    TestRequestAfterThreadCleanupIsHarmless);
  Test('an abort-only thread has no deadline to report or to hit',
    TestAbortOnlyHasNoDeadlineToReport);
  Test('capturing again detaches the previous handle',
    TestCaptureAgainDetachesThePreviousHandle);
  Test('a recompute racing with a request never loses it',
    TestRacingRecomputeNeverLosesTheRequest);
  Test('a bytecode loop with no call is stopped', TestBytecodeLoopIsStopped);
  Test('an endless for-of over a custom iterator is stopped',
    TestIteratorLoopIsStopped);
  Test('a native loop over a huge sparse array is stopped',
    TestNativeLoopIsStopped);
  Test('Atomics.wait is stopped', TestAtomicsWaitIsStopped);
  Test('an Atomics.waitAsync on the idle wait is stopped',
    TestAtomicsWaitAsyncIsStopped);
  Test('a fetch nobody answers is stopped', TestFetchWaitIsStopped);
  Test('a microtask chain that never ends is stopped',
    TestMicrotaskChainIsStopped);
  Test('a run stopped with pending promises and live data is safe to free',
    TestPendingPromisesAreLeftSafeToFree);
  Test('the worker runs a fresh engine after an abort',
    TestWorkerRunsAgainAfterAnAbort);
  Test('a request made before the run starts ends it at once',
    TestAbortBeforeTheRunStartsEndsItAtOnce);
  Test('a stopped run under a collection before every reservation',
    TestStoppedRunUnderCollectionBeforeEveryReservation);
end;

procedure TExecutionAbortTests.BeforeEach;
begin
  inherited BeforeEach;
  ClearAbort;
  ClearInstructionLimit;
  ClearExecutionTimeout;
end;

procedure TExecutionAbortTests.AfterEach;
begin
  ClearAbort;
  ClearInstructionLimit;
  ClearExecutionTimeout;
  inherited AfterEach;
end;

procedure TExecutionAbortTests.TestArmingTurnsBothPollsOn;
var
  Handle: IGocciaAbortHandle;
begin
  Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);
  Handle := CaptureAbortHandle;
  Expect<Boolean>(AbortIsArmed).ToBe(True);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
  Expect<Boolean>(GThreadPolls.InstructionLimitActive).ToBe(True);
  Expect<Boolean>(Handle.IsRequested).ToBe(False);
  ClearAbort;
  Expect<Boolean>(AbortIsArmed).ToBe(False);
  Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);
end;

procedure TExecutionAbortTests.TestUnrequestedAbortChangesNothing;
var
  Handle: IGocciaAbortHandle;
  I: Integer;
begin
  Handle := CaptureAbortHandle;
  for I := 1 to 20000 do
  begin
    PollInstructionLimit(CaptureInstructionLimitState);
    CheckExecutionTimeout;
    CheckExecutionTimeoutNow;
    CheckInstructionLimit;
  end;
  Expect<Boolean>(Handle.IsRequested).ToBe(False);
end;

{ Polls until the abort ends it, on a thread of its own that a second thread
  requests from. Returns the polls it made and the class it raised. }
function TExecutionAbortTests.PollsUntilAbort(const AViaTimeout: Boolean;
  out AClassName: string): Int64;
var
  Poller: TPollingThread;
  Requester: TRequestingThread;
begin
  Poller := TPollingThread.Create(pmInstructions);
  try
    Poller.Start;
    Poller.WaitUntilArmed;
    Requester := TRequestingThread.Create(Poller.Handle, LET_IT_RUN_MS);
    try
      Poller.WaitFor;
      Requester.WaitFor;
    finally
      Requester.Free;
    end;
    AClassName := Poller.Raised;
    Result := Poller.Polls;
    Expect<Boolean>(Poller.Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Poller.Free;
  end;
end;

procedure TExecutionAbortTests.TestRequestEndsTheInstructionPoll;
var
  Name: string;
begin
  PollsUntilAbort(False, Name);
  Expect<string>(Name).ToBe(TGocciaAbortError.ClassName);
end;

procedure TExecutionAbortTests.TestRequestEndsTheTimeoutPoll;
var
  Poller: TPollingThread;
begin
  { A thread that polls only the native loops' check, as one stuck in a
    native loop does; the request lowers the instruction limit too, but this
    thread never reads it. }
  Poller := TPollingThread.Create(pmNativeLoopOnly);
  try
    Poller.Start;
    Poller.WaitUntilArmed;
    RequestAbort(Poller.Handle);
    Poller.WaitFor;
    Expect<string>(Poller.Raised).ToBe(TGocciaAbortError.ClassName);
  finally
    Poller.Free;
  end;
end;

procedure TExecutionAbortTests.TestRequestIsAnUncatchableInstructionLimitFamilyError;
var
  Handle: IGocciaAbortHandle;
  Raised: Exception;
begin
  Handle := CaptureAbortHandle;
  Handle.Request;
  Raised := nil;
  try
    CheckInstructionLimit;
  except
    on E: Exception do
      Raised := Exception(AcquireExceptionObject);
  end;
  try
    Expect<Boolean>(Raised is TGocciaAbortError).ToBe(True);
    Expect<Boolean>(Raised is TGocciaInstructionLimitError).ToBe(True);
    Expect<Boolean>(IsUncatchableFault(Raised)).ToBe(True);
  finally
    Raised.Free;
  end;
  // The timeout poll raises the same class.
  Raised := nil;
  try
    CheckExecutionTimeoutNow;
  except
    on E: Exception do
      Raised := Exception(AcquireExceptionObject);
  end;
  try
    Expect<Boolean>(Raised is TGocciaAbortError).ToBe(True);
  finally
    Raised.Free;
  end;
end;

procedure TExecutionAbortTests.TestReportersNameTheAbortApartFromTheLimit;
var
  Abort: Exception;
  Limit: Exception;
begin
  Abort := TGocciaAbortError.Create('Execution aborted by the host');
  Limit := TGocciaInstructionLimitError.Create('limit');
  try
    Expect<string>(ExceptionToCLIJSONErrorInfo(Abort).ErrorType)
      .ToBe('RunAbortedError');
    Expect<string>(ExceptionToCLIJSONErrorInfo(Limit).ErrorType)
      .ToBe('InstructionLimitError');
  finally
    Abort.Free;
    Limit.Free;
  end;
end;

procedure TExecutionAbortTests.TestScopeChurnCannotUndoARequest;
var
  Poller: TPollingThread;
begin
  Poller := TPollingThread.Create(pmChurnScopes);
  try
    Poller.Start;
    Poller.WaitUntilArmed;
    Sleep(LET_IT_RUN_MS);
    RequestAbort(Poller.Handle);
    Poller.WaitFor;
    Expect<string>(Poller.Raised).ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Poller.Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Poller.Free;
  end;
end;

procedure TExecutionAbortTests.TestStartAndClearBudgetsCannotUndoARequest;
var
  Handle: IGocciaAbortHandle;
  Name: string;
begin
  Handle := CaptureAbortHandle;
  Handle.Request;
  StartInstructionLimit(1000000);
  ClearInstructionLimit;
  StartInstructionLimit(0);
  PushInstructionLimitScope(500);
  PopInstructionLimitScope;
  Name := '';
  try
    CheckInstructionLimit;
  except
    on E: Exception do
      Name := E.ClassName;
  end;
  Expect<string>(Name).ToBe(TGocciaAbortError.ClassName);
  Expect<Boolean>(GThreadPolls.InstructionLimitActive).ToBe(True);
end;

procedure TExecutionAbortTests.TestRequestOutranksASpentBudgetAndAnExpiredDeadline;
var
  Handle: IGocciaAbortHandle;
  Name: string;
begin
  Handle := CaptureAbortHandle;
  StartInstructionLimit(1);
  IncrementInstructionCounter;
  IncrementInstructionCounter;
  StartExecutionTimeout(1);
  Sleep(10);
  // Spent and expired, not requested: the budget's own error.
  Name := '';
  try
    CheckInstructionLimit;
  except
    on E: Exception do
      Name := E.ClassName;
  end;
  Expect<string>(Name).ToBe(TGocciaInstructionLimitError.ClassName);
  Handle.Request;
  Name := '';
  try
    CheckInstructionLimit;
  except
    on E: Exception do
      Name := E.ClassName;
  end;
  Expect<string>(Name).ToBe(TGocciaAbortError.ClassName);
  Name := '';
  try
    CheckExecutionTimeoutNow;
  except
    on E: Exception do
      Name := E.ClassName;
  end;
  Expect<string>(Name).ToBe(TGocciaAbortError.ClassName);
end;

procedure TExecutionAbortTests.TestClearAbortDisarmsAndKeepsBudgets;
var
  Handle: IGocciaAbortHandle;
  Name: string;
begin
  StartInstructionLimit(50);
  StartExecutionTimeout(60000);
  Handle := CaptureAbortHandle;
  Handle.Request;
  ClearAbort;
  { The requested handle no longer governs this thread, and the host's own
    budget and deadline still do. }
  CheckInstructionLimit;
  CheckExecutionTimeoutNow;
  Expect<Boolean>(GThreadPolls.InstructionLimitActive).ToBe(True);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
  Name := '';
  try
    while True do
    begin
      PollInstructionLimit(CaptureInstructionLimitState);
    end;
  except
    on E: Exception do
      Name := E.ClassName;
  end;
  Expect<string>(Name).ToBe(TGocciaInstructionLimitError.ClassName);
  Expect<Boolean>(Handle.IsRequested).ToBe(True);
end;

procedure TExecutionAbortTests.TestLateRequestDoesNotReachTheNextRun;
var
  First, Second: IGocciaAbortHandle;
  I: Integer;
begin
  First := CaptureAbortHandle;
  ClearAbort;
  Second := CaptureAbortHandle;
  First.Request;
  for I := 1 to 20000 do
  begin
    PollInstructionLimit(CaptureInstructionLimitState);
    CheckExecutionTimeout;
    CheckInstructionLimit;
  end;
  Expect<Boolean>(Second.IsRequested).ToBe(False);
  Second.Request;
  try
    CheckInstructionLimit;
    Fail('the second run''s own request should have raised');
  except
    on TGocciaAbortError do
      ;
  end;
end;

procedure TExecutionAbortTests.TestRequestAfterThreadCleanupIsHarmless;
var
  Worker: TCleanupThread;
  Handle: IGocciaAbortHandle;
begin
  Worker := TCleanupThread.Create;
  try
    Worker.Start;
    Worker.WaitFor;
    Expect<Boolean>(Worker.ArmedBefore).ToBe(True);
    Expect<Boolean>(Worker.DisarmedByCleanup).ToBe(True);
    Handle := Worker.Handle;
    // The thread has ended; its record is gone. This must not write there.
    Handle.Request;
    Handle.Request;
    Expect<Boolean>(Handle.IsRequested).ToBe(True);
  finally
    Worker.Free;
  end;
end;

procedure TExecutionAbortTests.TestAbortOnlyHasNoDeadlineToReport;
var
  Handle: IGocciaAbortHandle;
  I: Integer;
begin
  Handle := CaptureAbortHandle;
  Expect<Integer>(RemainingExecutionTimeoutMilliseconds).ToBe(0);
  for I := 1 to 5000 do
    CheckExecutionTimeoutNow;
  { A real deadline on top reports itself and ends with the abort's cleanup. }
  StartExecutionTimeout(60000);
  Expect<Boolean>(RemainingExecutionTimeoutMilliseconds > 0).ToBe(True);
  ClearExecutionTimeout;
  Expect<Integer>(RemainingExecutionTimeoutMilliseconds).ToBe(0);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
end;

procedure TExecutionAbortTests.TestCaptureAgainDetachesThePreviousHandle;
var
  First, Second: IGocciaAbortHandle;
  Name: string;
begin
  First := CaptureAbortHandle;
  Second := CaptureAbortHandle;
  First.Request;
  Name := '';
  try
    CheckInstructionLimit;
    CheckExecutionTimeoutNow;
  except
    on E: Exception do
      Name := E.ClassName;
  end;
  Expect<string>(Name).ToBe('');
  Second.Request;
  try
    CheckInstructionLimit;
    Fail('the current handle''s request should have raised');
  except
    on TGocciaAbortError do
      ;
  end;
end;

procedure TExecutionAbortTests.TestRacingRecomputeNeverLosesTheRequest;
var
  Trial: Integer;
  Poller: TPollingThread;
  Requester: TRequestingThread;
  Lost: Integer;
begin
  Lost := 0;
  for Trial := 1 to RACE_TRIALS do
  begin
    Poller := TPollingThread.Create(pmChurnScopes);
    try
      Poller.Start;
      Poller.WaitUntilArmed;
      // From a request right away to one a few milliseconds in, so that it
      // lands at every point of the push, poll and pop cycle.
      Requester := TRequestingThread.Create(Poller.Handle, Trial mod 4);
      try
        Poller.WaitFor;
        Requester.WaitFor;
      finally
        Requester.Free;
      end;
      if (Poller.Raised <> TGocciaAbortError.ClassName) or
         (Poller.Elapsed >= PROMPT_END_MS) then
        Inc(Lost);
    finally
      Poller.Free;
    end;
  end;
  Expect<Integer>(Lost).ToBe(0);
end;

{ Runs ARun's script on its thread, requests the abort once it is running,
  and returns what ended it. }
function TExecutionAbortTests.StopsScript(const AScript: string;
  const ARun: TAbortableRun; out AElapsed: Int64): string;
var
  Requester: TRequestingThread;
begin
  ARun.Start;
  ARun.WaitUntilRunning;
  Requester := TRequestingThread.Create(ARun.Handle, LET_IT_RUN_MS);
  try
    ARun.WaitFor;
    Requester.WaitFor;
  finally
    Requester.Free;
  end;
  AElapsed := ARun.Elapsed;
  Result := ARun.Outcome;
end;

procedure TExecutionAbortTests.TestBytecodeLoopIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_BYTECODE_LOOP);
  try
    Run.WhileLoops := True;
    Expect<string>(StopsScript(SCRIPT_BYTECODE_LOOP, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
    Expect<string>(Run.SecondOutcome).ToBe('finished');
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestIteratorLoopIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_ITERATOR);
  try
    Expect<string>(StopsScript(SCRIPT_ITERATOR, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestNativeLoopIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  { No bytecode runs inside the call: only the native loop's own poll can
    see the request. }
  Run := TAbortableRun.Create(SCRIPT_NATIVE_LOOP);
  try
    Expect<string>(StopsScript(SCRIPT_NATIVE_LOOP, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
    Expect<string>(Run.SecondOutcome).ToBe('finished');
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestAtomicsWaitIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_ATOMICS_WAIT);
  try
    Expect<string>(StopsScript(SCRIPT_ATOMICS_WAIT, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestAtomicsWaitAsyncIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_ATOMICS_WAIT_ASYNC);
  try
    Expect<string>(StopsScript(SCRIPT_ATOMICS_WAIT_ASYNC, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestFetchWaitIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_FETCH);
  try
    Run.WithFetch := True;
    Expect<string>(StopsScript(SCRIPT_FETCH, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
    Expect<string>(Run.SecondOutcome).ToBe('finished');
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestMicrotaskChainIsStopped;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_MICROTASK_CHAIN);
  try
    Expect<string>(StopsScript(SCRIPT_MICROTASK_CHAIN, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestPendingPromisesAreLeftSafeToFree;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_PENDING_PROMISES);
  try
    Run.Stress := False;
    Expect<string>(StopsScript(SCRIPT_PENDING_PROMISES, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    { Freeing the runtime, collecting, shutting the thread's collector down
      and a fresh engine afterwards all ran on the worker. }
    Expect<string>(Run.SecondOutcome).ToBe('finished');
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestWorkerRunsAgainAfterAnAbort;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_ITERATOR);
  try
    Run.SecondScript := 'export const answer = [1, 2, 3].map((x) => x * 2)' +
      '.reduce((a, b) => a + b, 0);';
    Expect<string>(StopsScript(SCRIPT_ITERATOR, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<string>(Run.SecondOutcome).ToBe('finished');
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestAbortBeforeTheRunStartsEndsItAtOnce;
var
  Run: TAbortableRun;
begin
  Run := TAbortableRun.Create(SCRIPT_ITERATOR);
  try
    Run.RequestOnStart := True;
    Run.Start;
    Run.WaitFor;
    Expect<string>(Run.Outcome).ToBe(TGocciaAbortError.ClassName);
    Expect<Boolean>(Run.Elapsed < PROMPT_END_MS).ToBe(True);
  finally
    Run.Free;
  end;
end;

procedure TExecutionAbortTests.TestStoppedRunUnderCollectionBeforeEveryReservation;
var
  Run: TAbortableRun;
  Elapsed: Int64;
begin
  Run := TAbortableRun.Create(SCRIPT_PENDING_PROMISES);
  try
    Run.Stress := True;
    Expect<string>(StopsScript(SCRIPT_PENDING_PROMISES, Run, Elapsed))
      .ToBe(TGocciaAbortError.ClassName);
    Expect<string>(Run.SecondOutcome).ToBe('finished');
    Expect<Integer>(Run.Quarantined).ToBe(0);
  finally
    Run.Free;
  end;
end;

begin
  EnsureSharedPrototypesInitialized;
  TestRunnerProgram.AddSuite(TExecutionAbortTests.Create('Execution abort'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
