{ Deterministic virtual timer queue.

  GocciaScript has no host event loop and never waits on wall time. A timer
  registered here is a record in an ordered queue with a due time on a virtual
  clock, and it runs only when something advances that clock:

    - under fake timers, one of the `vi` advance members (see
      Goccia.Builtins.Timers), and
    - under real timers, the engine itself, at the two points where it would
      otherwise have nothing left to do: an `await` whose promise is still
      pending (Goccia.Values.Await) and the end-of-run idle drain
      (Goccia.RuntimeExtensions.Timers).

  Either way no real time passes: the clock jumps to each timer's due time.

  The ordering, the due-time arithmetic and the loop guard are modelled on
  @sinonjs/fake-timers, which is what Vitest's fake timers wrap, and every rule
  in here was probed against the pinned Vitest 4.1.10 rather than read off its
  documentation. See docs/adr/0113-deterministic-virtual-timer-queue.md.

  The queue is shared machinery: it sits below both executors, so the
  interpreter and the bytecode VM get identical behaviour by construction. }

unit Goccia.Timers;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.AsyncContext,
  Goccia.GarbageCollector,
  Goccia.HostEnvironment,
  Goccia.Values.Primitives;

const
  { @sinonjs/fake-timers aborts a runAll() that keeps finding new timers, and
    Vitest configures the limit at 10000. The message is reproduced verbatim
    because suites assert on it. }
  TIMER_LOOP_LIMIT = 10000;
  TIMER_LOOP_LIMIT_MESSAGE =
    'Aborting after running 10000 timers, assuming an infinite loop!';
  { Node clamps a delay above the signed 32-bit range to 1ms, and so does the
    fake clock. }
  TIMER_MAX_DELAY = 2147483647.0;
  NEGATIVE_TICKS_MESSAGE = 'Negative ticks are not supported';

type
  TGocciaTimerKind = (gtkTimeout, gtkInterval);

  { One scheduled callback. Owned by the queue's list, which frees it. }
  TGocciaTimerEntry = class
  public
    Id: Double;
    Kind: TGocciaTimerKind;
    Callback: TGocciaValue;
    Args: TArray<TGocciaValue>;
    Delay: Double;
    IntervalDelay: Double;
    CallAt: Double;
    CreatedAt: Double;
    { The async context in effect where the timer was registered. A timer
      callback is a continuation, so it runs under that context rather than
      under whatever the code advancing the clock happens to hold. }
    Context: TGocciaAsyncContextSnapshot;
  end;

  TGocciaTimerEntryList = TObjectList<TGocciaTimerEntry>;

  TGocciaTimerQueue = class;

  { Publishes the queue's live values to the collector: a pending timer's
    callback, its arguments and its captured context are reachable from nothing
    else. }
  TGocciaTimerRoots = class(TGCRootSource)
  private
    FQueue: TGocciaTimerQueue;
    FCollector: TGarbageCollector;
  public
    procedure MarkRootReferences; override;
    property Collector: TGarbageCollector read FCollector;
  end;

  TGocciaTimerQueue = class
  private
    FTimers: TGocciaTimerEntryList;
    FInFlight: TGocciaTimerEntry;
    FRoots: TGocciaTimerRoots;
    FNextId: Double;

    FNow: Double;
    FStart: Double;
    FAdjusted: Double;
    FDuringTick: Boolean;
    FFaking: Boolean;
    FMockedDateOnly: Boolean;
    FMockedDate: Double;

    FHostEnvironment: TGocciaHostEnvironment;

    FPendingThrow: TGocciaValue;
    FHasPendingThrow: Boolean;

    procedure EnsureRoots;
    procedure PublishClock;
    procedure SetNow(const AValue: Double);
    function IndexOfId(const AId: Double): Integer;
    function IsEarlier(const ALeft, ARight: TGocciaTimerEntry): Boolean;
    function FirstTimerInRange(const AFrom, ATo: Double): TGocciaTimerEntry;
    function FirstTimer: TGocciaTimerEntry;
    function LastTimer: TGocciaTimerEntry;
    procedure CallTimer(const ATimer: TGocciaTimerEntry;
      const ACaptureThrows: Boolean);
    procedure CapturePendingThrow(const AValue: TGocciaValue);
    procedure RaisePendingThrow;
    procedure DrainMicrotasks;
    function DoTick(const AMilliseconds: Double;
      const AAsync: Boolean): Double;
    function DoNext(const AAsync, ACaptureThrows: Boolean): Double;
  public
    class function Instance: TGocciaTimerQueue;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    { Registration. ADelay and AInterval are raw JavaScript numbers; the
      normalisation the fake clock applies happens here. }
    function AddTimer(const AKind: TGocciaTimerKind;
      const ACallback: TGocciaValue; const AArgs: TArray<TGocciaValue>;
      const ADelay: Double): Double;
    procedure ClearTimer(const AId: Double);
    { The `vi.clearAllTimers` semantics: fake-timer mode only, and it rewinds
      the clock as well. DiscardTimers is the engine-side counterpart — it
      drops the queue in either mode and leaves the clock alone, for a host
      tearing a run down. }
    procedure ClearAllTimers;
    procedure DiscardTimers;
    function CountTimers: Integer;
    function HasPendingTimers: Boolean;

    { Advancing. Every member returns the new virtual now, in milliseconds.
      AAsync drains the microtask queue between timers, which is what makes an
      `Async` advance member observe the promise callbacks a timer queued. }
    function Tick(const AMilliseconds: Double;
      const AAsync: Boolean): Double;
    function AdvanceToNextTimer(const AAsync: Boolean): Double;
    function RunAllTimers(const AAsync: Boolean): Double;
    function RunPendingTimers(const AAsync: Boolean): Double;

    { Runs the next due timer and drains microtasks after it. Real-timer mode
      only; returns False when nothing ran. This is what the engine calls where
      a host event loop would have taken over. }
    function RunOneRealTimer: Boolean;
    { Bounded real-timer drain for the end of a run. Stops at the loop limit
      rather than raising: an uncleared interval must not turn a passing file
      into a failing one on the way out. }
    procedure DrainRealTimers;

    { Fake-timer mode. }
    procedure BeginFakeTimers(const ANowMilliseconds: Double);
    procedure EndFakeTimers;
    procedure SetSystemTime(const AEpochMilliseconds: Double);
    procedure ClearMockedDate;
    function RealEpochMilliseconds: Double;

    { The engine whose Date, Temporal.Now and performance this queue drives
      while a clock is mocked. Assigned by the runtime extension. }
    property HostEnvironment: TGocciaHostEnvironment read FHostEnvironment
      write FHostEnvironment;

    property Faking: Boolean read FFaking;
    property MockedDateOnly: Boolean read FMockedDateOnly;
    property MockedDate: Double read FMockedDate;
    property NowMilliseconds: Double read FNow;
  end;

{ Runs the next due real-mode timer on this thread's queue, if there is one.
  False when nothing ran — no queue yet, fake timers on, or nothing pending.
  This is the seam the engine's promise waits call: a real-mode timer is a
  continuation no amount of microtask draining will produce. }
function RunOneRealTimer: Boolean;

implementation

uses
  Math,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.CapabilityAudit,
  Goccia.Constants.ErrorNames,
  Goccia.EngineFault,
  Goccia.Error,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.MicrotaskQueue,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.VM.Exception;

const
  NANOSECONDS_PER_MILLISECOND = 1000000.0;

threadvar
  TimerQueueThreadInstance: TGocciaTimerQueue;

{ TGocciaTimerRoots }

procedure TGocciaTimerRoots.MarkRootReferences;

  procedure MarkTimer(const ATimer: TGocciaTimerEntry);
  var
    I: Integer;
  begin
    if not Assigned(ATimer) then
      Exit;
    if Assigned(ATimer.Callback) then
      ATimer.Callback.MarkReferences;
    for I := Low(ATimer.Args) to High(ATimer.Args) do
      if Assigned(ATimer.Args[I]) then
        ATimer.Args[I].MarkReferences;
    if Assigned(ATimer.Context) then
      ATimer.Context.MarkReferences;
  end;

var
  I: Integer;
begin
  if not Assigned(FQueue) then
    Exit;
  for I := 0 to FQueue.FTimers.Count - 1 do
    MarkTimer(FQueue.FTimers[I]);
  MarkTimer(FQueue.FInFlight);
  if Assigned(FQueue.FPendingThrow) then
    FQueue.FPendingThrow.MarkReferences;
end;

{ TGocciaTimerQueue }

class function TGocciaTimerQueue.Instance: TGocciaTimerQueue;
begin
  Result := TimerQueueThreadInstance;
end;

class procedure TGocciaTimerQueue.Initialize;
begin
  if not Assigned(TimerQueueThreadInstance) then
    TimerQueueThreadInstance := TGocciaTimerQueue.Create;
end;

class procedure TGocciaTimerQueue.Shutdown;
begin
  FreeAndNil(TimerQueueThreadInstance);
end;

constructor TGocciaTimerQueue.Create;
begin
  inherited Create;
  FTimers := TGocciaTimerEntryList.Create(True);
  FNextId := 1;
  FNow := 0;
  FStart := 0;
  FAdjusted := 0;
end;

destructor TGocciaTimerQueue.Destroy;
begin
  { FHostEnvironment is deliberately not touched. The queue is a thread
    singleton and outlives the engines that point it at their host environment,
    so by teardown the pointer may name a freed one; clearing the override is
    the detaching extension's job, while its engine is still alive. }
  FreeAndNil(FRoots);
  FTimers.Free;
  inherited;
end;

{ The root source registers with whichever collector was current when it was
  built, so a thread whose collector was replaced between engines needs a fresh
  one — the same rule the async-context roots follow. }
procedure TGocciaTimerQueue.EnsureRoots;
var
  Collector: TGarbageCollector;
begin
  Collector := TGarbageCollector.Instance;
  if Assigned(FRoots) and (FRoots.Collector = Collector) then
    Exit;

  FreeAndNil(FRoots);
  if not Assigned(Collector) then
    Exit;

  FRoots := TGocciaTimerRoots.Create;
  FRoots.FQueue := Self;
  FRoots.FCollector := Collector;
end;

{ The mocked clock reaches JavaScript through the engine's host environment,
  which is what Date, Temporal.Now and performance already read. Overriding
  there rather than patching a global keeps every reader consistent and leaves
  the Date shim untouched. }
procedure TGocciaTimerQueue.PublishClock;
begin
  if not Assigned(FHostEnvironment) then
    Exit;

  if FFaking then
    { Monotonic time excludes whatever setSystemTime jumped the wall clock by,
      so performance.now() measures elapsed virtual time rather than the
      simulated date. }
    FHostEnvironment.OverrideClock(
      True, Round(FNow * NANOSECONDS_PER_MILLISECOND),
      True, Round((FNow - FAdjusted - FStart) * NANOSECONDS_PER_MILLISECOND))
  else if FMockedDateOnly then
    { setSystemTime outside useFakeTimers freezes the date and nothing else,
      exactly as Vitest's Date-only mock does. }
    FHostEnvironment.OverrideClock(
      True, Round(FMockedDate * NANOSECONDS_PER_MILLISECOND), False, 0)
  else
    FHostEnvironment.ClearClockOverride;
end;

procedure TGocciaTimerQueue.SetNow(const AValue: Double);
begin
  FNow := AValue;
  PublishClock;
end;

function TGocciaTimerQueue.RealEpochMilliseconds: Double;
begin
  if Assigned(FHostEnvironment) then
    Result := FHostEnvironment.RealEpochNanoseconds / NANOSECONDS_PER_MILLISECOND
  else
    Result := 0;
  Result := Int(Result);
end;

function TGocciaTimerQueue.IndexOfId(const AId: Double): Integer;
var
  I: Integer;
begin
  for I := 0 to FTimers.Count - 1 do
    if FTimers[I].Id = AId then
      Exit(I);
  Result := -1;
end;

{ The fake clock's ordering: due time, then registration order, then id. Two
  timers due at the same instant therefore fire in the order they were
  scheduled, and setSystemTime — which shifts every due time and creation time
  by the same amount — cannot reorder them. }
function TGocciaTimerQueue.IsEarlier(
  const ALeft, ARight: TGocciaTimerEntry): Boolean;
begin
  if ALeft.CallAt <> ARight.CallAt then
    Exit(ALeft.CallAt < ARight.CallAt);
  if ALeft.CreatedAt <> ARight.CreatedAt then
    Exit(ALeft.CreatedAt < ARight.CreatedAt);
  Result := ALeft.Id < ARight.Id;
end;

function TGocciaTimerQueue.FirstTimerInRange(
  const AFrom, ATo: Double): TGocciaTimerEntry;
var
  Candidate: TGocciaTimerEntry;
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTimers.Count - 1 do
  begin
    Candidate := FTimers[I];
    if (Candidate.CallAt < AFrom) or (Candidate.CallAt > ATo) then
      Continue;
    if (Result = nil) or IsEarlier(Candidate, Result) then
      Result := Candidate;
  end;
end;

function TGocciaTimerQueue.FirstTimer: TGocciaTimerEntry;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTimers.Count - 1 do
    if (Result = nil) or IsEarlier(FTimers[I], Result) then
      Result := FTimers[I];
end;

function TGocciaTimerQueue.LastTimer: TGocciaTimerEntry;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTimers.Count - 1 do
    if (Result = nil) or IsEarlier(Result, FTimers[I]) then
      Result := FTimers[I];
end;

function NormalizedDelay(const AValue: Double): Double;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit(0);
  Result := Int(AValue);
  if Result > TIMER_MAX_DELAY then
    Result := 1;
  if Result < 0 then
    Result := 0;
end;

function TGocciaTimerQueue.AddTimer(const AKind: TGocciaTimerKind;
  const ACallback: TGocciaValue; const AArgs: TArray<TGocciaValue>;
  const ADelay: Double): Double;
var
  Entry: TGocciaTimerEntry;
begin
  EnsureRoots;

  Entry := TGocciaTimerEntry.Create;
  Entry.Id := FNextId;
  FNextId := FNextId + 1;
  Entry.Kind := AKind;
  Entry.Callback := ACallback;
  Entry.Args := AArgs;
  Entry.Delay := NormalizedDelay(ADelay);
  if AKind = gtkInterval then
    Entry.IntervalDelay := Entry.Delay
  else
    Entry.IntervalDelay := 0;
  Entry.CreatedAt := FNow;
  { A zero delay means "the next turn". Scheduled from inside a running timer
    that is one virtual millisecond later, which is what keeps a chain of
    zero-delay timers from all collapsing onto the instant the chain started
    and looping forever inside one advance. }
  if Entry.Delay <> 0 then
    Entry.CallAt := FNow + Entry.Delay
  else if FDuringTick then
    Entry.CallAt := FNow + 1
  else
    Entry.CallAt := FNow;
  Entry.Context := CurrentAsyncContext;

  FTimers.Add(Entry);
  Result := Entry.Id;
end;

procedure TGocciaTimerQueue.ClearTimer(const AId: Double);
var
  Index: Integer;
begin
  { A falsy id is ignored rather than reported: clearTimeout(undefined) is
    common in cleanup paths and does nothing everywhere else either. }
  if IsNan(AId) or (AId = 0) then
    Exit;
  Index := IndexOfId(AId);
  if Index >= 0 then
    FTimers.Delete(Index);
end;

{ Drops the queue and rewinds the clock to the instant fake timers were
  installed — both halves, because the fake clock's reset does both and a suite
  can see either. Outside fake timers it does nothing rather than reporting an
  error, which is also what Vitest does. }
procedure TGocciaTimerQueue.ClearAllTimers;
begin
  if not FFaking then
    Exit;
  FTimers.Clear;
  FPendingThrow := nil;
  FHasPendingThrow := False;
  SetNow(FStart);
end;

procedure TGocciaTimerQueue.DiscardTimers;
begin
  FTimers.Clear;
  FPendingThrow := nil;
  FHasPendingThrow := False;
end;

function TGocciaTimerQueue.CountTimers: Integer;
begin
  Result := FTimers.Count;
end;

function TGocciaTimerQueue.HasPendingTimers: Boolean;
begin
  Result := FTimers.Count > 0;
end;

procedure TGocciaTimerQueue.DrainMicrotasks;
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  if Assigned(Queue) and Queue.HasPending then
    Queue.DrainQueue;
end;

{ A *tick* records the first exception it produced, keeps running the remaining
  timers, and rethrows once it is over — a suite can observe both halves, so
  both are reproduced. Stepping to a single timer does not: the fake clock's
  `next` has no handler of its own, which is why `runAllTimers` and
  `advanceTimersToNextTimer` stop at the throwing timer and leave the rest
  pending. Probed for each member separately, because the three do not agree. }
procedure TGocciaTimerQueue.CapturePendingThrow(const AValue: TGocciaValue);
begin
  if FHasPendingThrow then
    Exit;
  FPendingThrow := AValue;
  FHasPendingThrow := True;
end;

procedure TGocciaTimerQueue.RaisePendingThrow;
var
  Value: TGocciaValue;
begin
  if not FHasPendingThrow then
    Exit;
  Value := FPendingThrow;
  FPendingThrow := nil;
  FHasPendingThrow := False;
  raise TGocciaThrowValue.Create(Value);
end;

procedure TGocciaTimerQueue.CallTimer(const ATimer: TGocciaTimerEntry;
  const ACaptureThrows: Boolean);
var
  CallArgs: TGocciaArgumentsCollection;
  ContextToken: Integer;
  I, Index: Integer;
  Owned: TGocciaTimerEntry;
begin
  Owned := nil;
  if ATimer.Kind = gtkInterval then
    { An interval reschedules from its previous due time, not from now, so a
      long-running callback cannot make the interval drift. }
    ATimer.CallAt := ATimer.CallAt + ATimer.IntervalDelay
  else
  begin
    Index := IndexOfId(ATimer.Id);
    if Index >= 0 then
    begin
      Owned := FTimers.Extract(FTimers[Index]);
      FInFlight := Owned;
    end;
  end;

  try
    ContextToken := EnterAsyncContext(ATimer.Context);
    try
      if Assigned(ATimer.Callback) and ATimer.Callback.IsCallable then
      begin
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          for I := Low(ATimer.Args) to High(ATimer.Args) do
            CallArgs.Add(ATimer.Args[I]);
          if ACaptureThrows then
          begin
            try
              DispatchCall(ATimer.Callback, CallArgs,
                TGocciaUndefinedLiteralValue.UndefinedValue);
            except
              on E: EGocciaBytecodeThrow do
                CapturePendingThrow(E.ThrownValue);
              on E: TGocciaThrowValue do
                CapturePendingThrow(E.Value);
              on E: TGocciaTimeoutError do
                raise;
              on E: TGocciaInstructionLimitError do
                raise;
              on E: TGocciaMemoryLimitError do
                raise;
              on E: EGocciaCapabilityAuditDeliveryError do
                raise;
              on E: TGocciaTypeError do
                CapturePendingThrow(
                  CreateErrorObject(TYPE_ERROR_NAME, E.Message));
              on E: TGocciaReferenceError do
                CapturePendingThrow(
                  CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
              on E: TGocciaSyntaxError do
                CapturePendingThrow(
                  CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
              on E: Exception do
              begin
                if IsEngineIntegrityFault(E) then
                  raise;
                CapturePendingThrow(CreateErrorObject(ERROR_NAME, E.Message));
              end;
            end;
          end
          else
            DispatchCall(ATimer.Callback, CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          CallArgs.Free;
        end;
      end;
    finally
      LeaveAsyncContext(ContextToken);
    end;
  finally
    FInFlight := nil;
    Owned.Free;
  end;
end;

{ Mirrors @sinonjs/fake-timers' doTick. The lagging `Previous` bound is what
  makes a timer scheduled during the tick eligible on the following iteration,
  and the trailing re-check is what picks up timers a callback scheduled
  strictly inside the remaining range. }
function TGocciaTimerQueue.DoTick(const AMilliseconds: Double;
  const AAsync: Boolean): Double;
var
  OldNow, Previous, TickFrom, TickTo: Double;
  Timer: TGocciaTimerEntry;
  WasDuringTick: Boolean;
begin
  if AMilliseconds < 0 then
    ThrowTypeError(NEGATIVE_TICKS_MESSAGE);

  TickTo := FNow + Int(AMilliseconds);
  TickFrom := FNow;
  Previous := FNow;
  WasDuringTick := FDuringTick;
  FDuringTick := True;
  try
    if AAsync then
      DrainMicrotasks;

    Timer := FirstTimerInRange(TickFrom, TickTo);
    while Assigned(Timer) and (TickFrom <= TickTo) do
    begin
      CheckExecutionTimeout;
      CheckInstructionLimit;

      TickFrom := Timer.CallAt;
      SetNow(Timer.CallAt);
      OldNow := FNow;
      CallTimer(Timer, True);
      if AAsync then
        DrainMicrotasks;

      { A setSystemTime inside the callback moved the wall clock under us; the
        window this tick is walking moves with it so the remaining timers, whose
        due times were shifted by the same amount, stay in range. }
      if OldNow <> FNow then
      begin
        TickFrom := TickFrom + (FNow - OldNow);
        TickTo := TickTo + (FNow - OldNow);
        Previous := Previous + (FNow - OldNow);
      end;

      Timer := FirstTimerInRange(Previous, TickTo);
      Previous := TickFrom;
    end;
  finally
    FDuringTick := WasDuringTick;
  end;

  Timer := FirstTimerInRange(TickFrom, TickTo);
  if Assigned(Timer) then
    DoTick(TickTo - FNow, AAsync)
  else
    SetNow(TickTo);

  Result := FNow;
end;

function TGocciaTimerQueue.Tick(const AMilliseconds: Double;
  const AAsync: Boolean): Double;
begin
  Result := DoTick(AMilliseconds, AAsync);
  RaisePendingThrow;
end;

function TGocciaTimerQueue.DoNext(const AAsync,
  ACaptureThrows: Boolean): Double;
var
  Timer: TGocciaTimerEntry;
  WasDuringTick: Boolean;
begin
  Timer := FirstTimer;
  if not Assigned(Timer) then
    Exit(FNow);

  WasDuringTick := FDuringTick;
  FDuringTick := True;
  try
    SetNow(Timer.CallAt);
    CallTimer(Timer, ACaptureThrows);
    if AAsync then
      DrainMicrotasks;
  finally
    FDuringTick := WasDuringTick;
  end;
  Result := FNow;
end;

{ Vitest follows the clock's own `next` with a zero-length tick so that every
  timer due at the instant it landed on fires, not just the first one. The step
  itself lets an exception out, so a throwing timer leaves the rest of that
  instant pending. }
function TGocciaTimerQueue.AdvanceToNextTimer(const AAsync: Boolean): Double;
begin
  DoNext(AAsync, False);
  DoTick(0, AAsync);
  RaisePendingThrow;
  Result := FNow;
end;

{ Steps one timer at a time, so a throwing callback aborts the run and leaves
  everything behind it pending — the fake clock's runAll has no handler of its
  own either. }
function TGocciaTimerQueue.RunAllTimers(const AAsync: Boolean): Double;
var
  I: Integer;
begin
  for I := 0 to TIMER_LOOP_LIMIT - 1 do
  begin
    if FTimers.Count = 0 then
      Exit(FNow);
    CheckExecutionTimeout;
    CheckInstructionLimit;
    DoNext(AAsync, False);
  end;
  ThrowError(TIMER_LOOP_LIMIT_MESSAGE);
  Result := FNow;
end;

{ "Only pending" means the timers that exist when the call is made: the clock
  advances to the latest of their due times, which fires anything that becomes
  due on the way — including a timer one of them scheduled inside that window —
  and leaves anything scheduled beyond it pending. }
function TGocciaTimerQueue.RunPendingTimers(const AAsync: Boolean): Double;
var
  Timer: TGocciaTimerEntry;
begin
  Timer := LastTimer;
  if not Assigned(Timer) then
    Exit(FNow);
  Result := Tick(Timer.CallAt - FNow, AAsync);
end;

function TGocciaTimerQueue.RunOneRealTimer: Boolean;
begin
  Result := False;
  if FFaking or (FTimers.Count = 0) then
    Exit;
  { Capture-and-rethrow rather than letting the exception out of the step: the
    caller is an engine wait, not a suite's advance member, and a timer that
    threw must not leave the queue mid-step. }
  DoNext(True, True);
  RaisePendingThrow;
  Result := True;
end;

procedure TGocciaTimerQueue.DrainRealTimers;
var
  I: Integer;
begin
  if FFaking then
    Exit;
  for I := 0 to TIMER_LOOP_LIMIT - 1 do
    if not RunOneRealTimer then
      Exit;
end;

procedure TGocciaTimerQueue.BeginFakeTimers(const ANowMilliseconds: Double);
begin
  EnsureRoots;
  { Re-enabling installs a fresh clock: whatever was scheduled against the
    previous one is discarded rather than carried over, which is what a second
    useFakeTimers() does in Vitest. }
  FTimers.Clear;
  { A recorded exception belongs to the advance that recorded it. An advance
    always rethrows and clears, so this only matters when one was cut short by
    a timeout or an instruction limit — installing a clock must not inherit
    that. }
  FPendingThrow := nil;
  FHasPendingThrow := False;
  FFaking := True;
  FMockedDateOnly := False;
  FNow := ANowMilliseconds;
  FStart := ANowMilliseconds;
  FAdjusted := 0;
  PublishClock;
end;

procedure TGocciaTimerQueue.EndFakeTimers;
begin
  FTimers.Clear;
  FPendingThrow := nil;
  FHasPendingThrow := False;
  FFaking := False;
  FMockedDateOnly := False;
  FNow := 0;
  FStart := 0;
  FAdjusted := 0;
  PublishClock;
end;

procedure TGocciaTimerQueue.SetSystemTime(const AEpochMilliseconds: Double);
var
  Difference: Double;
  I: Integer;
begin
  if not FFaking then
  begin
    FMockedDateOnly := True;
    FMockedDate := AEpochMilliseconds;
    PublishClock;
    Exit;
  end;

  Difference := AEpochMilliseconds - FNow;
  FAdjusted := FAdjusted + Difference;
  FNow := AEpochMilliseconds;
  { Every pending timer keeps the delay it was scheduled with: moving the wall
    clock is not the same as letting time pass. }
  for I := 0 to FTimers.Count - 1 do
  begin
    FTimers[I].CreatedAt := FTimers[I].CreatedAt + Difference;
    FTimers[I].CallAt := FTimers[I].CallAt + Difference;
  end;
  PublishClock;
end;

procedure TGocciaTimerQueue.ClearMockedDate;
begin
  FMockedDateOnly := False;
  PublishClock;
end;

function RunOneRealTimer: Boolean;
var
  Queue: TGocciaTimerQueue;
begin
  Queue := TGocciaTimerQueue.Instance;
  Result := Assigned(Queue) and Queue.RunOneRealTimer;
end;

procedure CleanupTimerQueueThreadState;
begin
  TGocciaTimerQueue.Shutdown;
end;

initialization
  RegisterThreadvarCleanup(CleanupTimerQueueThreadState);

end.
