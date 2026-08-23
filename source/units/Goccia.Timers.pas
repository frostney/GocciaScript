{ Deterministic virtual timer queue.

  GocciaScript has no host event loop and never waits on wall time. A timer
  registered here is a record in an ordered queue with a due time on a virtual
  clock, and it runs only when something advances that clock:

    - under fake timers, one of the `vi` advance members (see
      Goccia.Builtins.Timers), and
    - under real timers, the engine itself, wherever it would otherwise have
      nothing left to do: an `await` whose promise is still pending
      (Goccia.Values.Await and Goccia.FetchManager), the end of each test
      (Goccia.Builtins.TestingLibrary), and the engine's own idle point
      (Goccia.RuntimeExtensions.Timers).

  Either way no real time passes: the clock jumps to each timer's due time.
  Work that is really outstanding still outranks it — a fetch in flight is
  polled before the clock is allowed to move — and an exception a real-mode
  callback throws is parked for the host rather than raised at whichever frame
  happened to be waiting, which is not the one Node would report it at.

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
    because suites assert on it — built from the constant so the two cannot
    drift apart. }
  TIMER_LOOP_LIMIT = 10000;
  { Node clamps a delay above the signed 32-bit range to 1ms, and so does the
    fake clock. }
  TIMER_MAX_DELAY = 2147483647.0;
  NEGATIVE_TICKS_MESSAGE = 'Negative ticks are not supported';
  { One advance may legitimately fire a great many timers — a 10ms interval
    advanced by an hour fires 360000 — so this bound is far above anything a
    real suite reaches and exists only to stop the one shape that cannot
    terminate: a timer whose period is zero, which reschedules itself at the
    instant it just ran so the clock can never move past it. Vitest hangs
    forever on that shape; aborting is the one place this deliberately does
    better than the oracle. }
  TIMER_TICK_LOOP_LIMIT = 1000000;
  TICK_LOOP_LIMIT_MESSAGE =
    'Aborting after firing %d timers in a single advance: a timer keeps ' +
    'rescheduling itself at the current instant, so the clock cannot move ' +
    'past it. A setInterval with a period of 0 does this.';
  { Vitest lets a non-finite system time through and leaves Date.now() reporting
    NaN. GocciaScript refuses it instead: the mocked clock reaches JavaScript as
    an Int64 nanosecond count on the host environment, so there is no NaN to
    propagate, and every arithmetic consumer of the virtual clock — range tests,
    due-time shifting — silently stops working once one is admitted. }
  NON_FINITE_SYSTEM_TIME_MESSAGE =
    'The system time must be a finite number of milliseconds or a valid Date.';
  NON_FINITE_SYSTEM_TIME_SUGGESTION =
    'Pass a finite epoch value, as in setSystemTime(0) or ' +
    'setSystemTime(new Date("2020-01-01")).';
  { A real-mode drain that hits the bound has to say so in its own words: the
    fake-clock message names an advance member the program never called. }
  REAL_TIMER_LOOP_LIMIT_MESSAGE =
    'Aborting after running %d timers, assuming an infinite loop of ' +
    'self-rescheduling timers. Clear the timer, or drive it with ' +
    'vi.useFakeTimers() so the test decides when it runs.';

type
  TGocciaTimerKind = (gtkTimeout, gtkInterval);

  { One advance operation's recorded exception.

    The slot has to be per-operation rather than per-queue. A tick drains the
    microtask queue between timers, and guest code reached from there can start
    another advance or another engine wait re-entrantly; with one queue-wide
    slot the outer tick's recorded exception was raised at the inner site, and
    the outer tick then believed it had succeeded. Each operation pushes its own
    slot and the enclosing ones stay saved — and stay marked — until it pops. }
  TGocciaTimerThrowSlot = record
    Value: TGocciaValue;
    HasValue: Boolean;
  end;

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
    { How many of this entry's callback frames are on the stack. A depth, not a
      flag: an explicit nested advance can re-enter the same interval entry (the
      fake-clock selectors do not skip a dispatching entry), so a Boolean cleared
      by the inner frame would let a clear from the still-running outer callback
      free the entry that frame is still holding. An advance reached from inside
      that callback — through the microtask drain, or through an engine wait an
      `await` in it started — must not pick the same entry again: an interval
      stays in the queue while it runs, and the real-mode step chooses the
      earliest timer without a due-time window, so it re-entered the same
      callback until the stack ran out. }
    DispatchDepth: Integer;
    { Cancelled while its own callback was running. An interval stays in the
      queue while it runs — that is what lets an explicit nested advance
      re-enter it, as Vitest allows — so `clearInterval(id)` called from inside
      that very callback would otherwise delete and free the entry the
      dispatcher is still holding. It is moved aside and marked instead, and
      freed when the callback returns. }
    Cleared: Boolean;
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
  public
    procedure MarkRootReferences; override;
  end;

  TGocciaTimerQueue = class
  private
    FTimers: TGocciaTimerEntryList;
    { Timeout entries whose callbacks are on the stack. They are extracted from
      FTimers before dispatch, so this is what keeps them marked — and it is a
      stack, not a slot, because a nested advance can start another one. }
    FInFlight: TGocciaTimerEntryList;
    FRoots: TGocciaTimerRoots;
    FNextId: Double;

    FNow: Double;
    FStart: Double;
    FAdjusted: Double;
    { Sub-millisecond remainder carried between advances, in nanoseconds. An
      advance moves the clock by whole milliseconds and banks the fraction, so
      two half-millisecond advances move it by one — probed: tick(1.5) then
      tick(0.5) lands on 2 and fires a timer due there. }
    FNanos: Double;
    FDuringTick: Boolean;
    FFaking: Boolean;
    FMockedDateOnly: Boolean;
    FMockedDate: Double;

    FHostEnvironment: TGocciaHostEnvironment;
    FOwnerRealm: TObject;

    FThrow: TGocciaTimerThrowSlot;
    FSavedThrows: TArray<TGocciaTimerThrowSlot>;
    FSavedThrowCount: Integer;
    { The value of an exception this queue is in the middle of raising. Held
      until the next advance starts rather than released at the raise: the
      exception object does not root what it carries, and the unwind runs
      arbitrary `finally` blocks that can collect. }
    FRaisedThrow: TGocciaValue;
    { An exception from a real-mode timer. Not raised at whichever frame
      happened to be waiting — in Node that frame is unaffected and the error is
      an uncaught top-level one — so it is parked here for the host to report. }
    FUncaughtError: TGocciaValue;
    FHasUncaughtError: Boolean;

    procedure EnsureRoots;
    procedure PublishClock;
    procedure SetNow(const AValue: Double);
    function IndexOfId(const AId: Double): Integer;
    function IsEarlier(const ALeft, ARight: TGocciaTimerEntry): Boolean;
    function IsDispatchable(const ATimer: TGocciaTimerEntry): Boolean;
    function FirstTimerInRange(const AFrom, ATo: Double): TGocciaTimerEntry;
    function FirstTimer: TGocciaTimerEntry;
    function FirstRealTimer(const ATimeoutsOnly: Boolean): TGocciaTimerEntry;
    function LastTimer: TGocciaTimerEntry;
    procedure RetireEntry(const AIndex: Integer);
    procedure RetireAllEntries;
    procedure CallTimer(const ATimer: TGocciaTimerEntry;
      const ACaptureThrows: Boolean);
    procedure CapturePendingThrow(const AValue: TGocciaValue);
    function PushThrowSlot: Integer;
    function TakeThrowSlot(const AToken: Integer;
      out AValue: TGocciaValue): Boolean;
    procedure RaiseThrown(const AValue: TGocciaValue);
    procedure DrainMicrotasks;
    function DoTick(const AMilliseconds: Double;
      const AAsync: Boolean): Double;
    function DoNext(const AAsync, ACaptureThrows: Boolean): Double;
    function RunOneRealTimerOfKind(const ATimeoutsOnly: Boolean): Boolean;
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
      a host event loop would have taken over. An exception the callback threw
      is parked in the uncaught slot rather than raised here. }
    function RunOneRealTimer: Boolean;
    { Bounded real-timer drain for the end of a run.

      Timeouts only, and it never raises. An uncleared interval is by
      construction infinite, so running it here would burn the whole budget and
      finish no sooner; and a shutdown path that threw would turn a passing file
      into a failing one. Whatever a callback threw is parked in the uncaught
      slot for the host to report. }
    procedure DrainRealTimers;

    { An exception a real-mode timer callback threw, if any, cleared by the
      read. The host attributes it: the test runner fails the test that was
      running, or the file when none was. }
    function TakeUncaughtError(out AValue: TGocciaValue): Boolean;
    function HasUncaughtError: Boolean;

    { The engine boundary, in both directions.

      The queue is a thread singleton and outlives any one engine, but every
      JavaScript value it holds belongs to the realm that put it there. Anything
      left behind — a recorded exception, the value of one already raised, an
      uncaught error nobody collected — is published to the NEXT engine's
      collector by the root source, which then walks a value whose heap is gone.
      That is an access violation on the second test file in a process, and it
      is why this is separate from EndFakeTimers: `vi.useRealTimers()` must not
      discard an uncaught error the runner has not reported yet. }
    procedure ResetForEngine;

    { Fake-timer mode. }
    procedure BeginFakeTimers(const ANowMilliseconds: Double);
    procedure EndFakeTimers;
    procedure SetSystemTime(const AEpochMilliseconds: Double);
    procedure ClearMockedDate;
    function RealEpochMilliseconds: Double;

    { The engine whose Date, Temporal.Now and performance this queue drives
      while a clock is mocked. Assigned by the runtime extension.

      Both are cleared on detach while that engine is still alive: the queue is
      a thread singleton that outlives any one engine, so a stale pointer here
      would have PublishClock writing into freed memory. }
    property HostEnvironment: TGocciaHostEnvironment read FHostEnvironment
      write FHostEnvironment;
    { The realm this queue's timers belong to, as an opaque handle compared by
      identity against Goccia.Realm's current realm. A ShadowRealm child runs on
      the same thread and shares this singleton, so without the check the
      child's `await` would run the parent realm's callbacks with the child's
      realm installed — parent code observing child intrinsics. }
    property OwnerRealm: TObject read FOwnerRealm write FOwnerRealm;

    property Faking: Boolean read FFaking;
    property MockedDateOnly: Boolean read FMockedDateOnly;
    property MockedDate: Double read FMockedDate;
    property NowMilliseconds: Double read FNow;
  end;

{ The loop-limit message the fake clock produces, built from the constant. }
function TimerLoopLimitMessage: string;

{ Runs the next due real-mode timer on this thread's queue, if there is one.
  False when nothing ran — no queue yet, fake timers on, nothing pending, or the
  queue belongs to a realm other than the one currently executing. This is the
  seam the engine's promise waits call: a real-mode timer is a continuation no
  amount of microtask draining will produce. }
function RunOneRealTimer: Boolean;

{ True when this thread's queue still holds real-mode timers the current realm
  may run. Lets a caller tell "the bound stopped me" from "there was nothing
  left", which is the difference between a timer diagnosis and an ordinary
  unsettled promise. }
function HasRunnableRealTimers: Boolean;

{ Drops every timer on this thread's queue. Between-test isolation: a drain the
  runner abandoned must not leave callbacks that fire inside the next test. }
procedure DiscardRealTimers;

{ Bounded real-mode drain the host runs where a program would otherwise be
  finished. Returns False when the bound stopped it with timers still runnable. }
function DrainRealTimersForHost: Boolean;

{ An exception a real-mode timer threw, for the host to attribute. }
function TakeUncaughtTimerError(out AValue: TGocciaValue): Boolean;

{ Reports a wait that spent its whole timer budget with timers still runnable.
  Lives here so callers that cannot reach the error helpers — the fetch manager
  compiles on a lane without them — can still name the real cause instead of
  letting it surface as an ordinary unsettled promise. }
procedure RaiseRealTimerLoopLimit;

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
  Goccia.Realm,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.VM.Exception;

const
  NANOSECONDS_PER_MILLISECOND = 1000000.0;
  { Largest magnitude, in whole milliseconds, whose nanosecond count still fits
    in the signed Int64 the host clock is published as (High(Int64) div 1e6).
    A finite JavaScript date can legitimately reach 8.64e15 ms, which is far
    beyond this, so a conversion is range-checked and rejected rather than
    silently overflowing Int64 inside Round. }
  MAX_CLOCK_MILLISECONDS = 9223372036854.0;
  CLOCK_OUT_OF_RANGE_MESSAGE =
    'The resulting time is too large to represent as a nanosecond clock.';
  CLOCK_OUT_OF_RANGE_SUGGESTION =
    'Keep the mocked time and any advance within about 9.2e12 milliseconds of ' +
    'the epoch.';

threadvar
  TimerQueueThreadInstance: TGocciaTimerQueue;

{ Rejects a millisecond clock value that the Int64 nanosecond clock cannot
  represent. Kept separate from the conversion so a caller can preflight a
  target value BEFORE mutating any timer state: PublishClock converts FNow /
  FMockedDate on every state change, so a value that only fails at conversion
  time would otherwise be committed first and poison every later publish. }
procedure RequireClockInRange(const AMilliseconds: Double);
begin
  if IsNan(AMilliseconds) or IsInfinite(AMilliseconds) or
     (Abs(AMilliseconds) > MAX_CLOCK_MILLISECONDS) then
    ThrowRangeError(CLOCK_OUT_OF_RANGE_MESSAGE, CLOCK_OUT_OF_RANGE_SUGGESTION);
end;

{ Converts a millisecond clock value to the Int64 nanosecond count the host
  environment is published with, rejecting a value that would overflow Int64
  with a RangeError rather than wrapping. }
function ClockMillisecondsToNanoseconds(const AMilliseconds: Double): Int64;
begin
  RequireClockInRange(AMilliseconds);
  Result := Round(AMilliseconds * NANOSECONDS_PER_MILLISECOND);
end;

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
  for I := 0 to FQueue.FInFlight.Count - 1 do
    MarkTimer(FQueue.FInFlight[I]);
  { Every enclosing advance's recorded exception, not just the innermost one:
    an outer tick's value has to survive the whole re-entrant window its
    microtask drain opened. }
  if FQueue.FThrow.HasValue and Assigned(FQueue.FThrow.Value) then
    FQueue.FThrow.Value.MarkReferences;
  for I := 0 to FQueue.FSavedThrowCount - 1 do
    if FQueue.FSavedThrows[I].HasValue and
       Assigned(FQueue.FSavedThrows[I].Value) then
      FQueue.FSavedThrows[I].Value.MarkReferences;
  if Assigned(FQueue.FRaisedThrow) then
    FQueue.FRaisedThrow.MarkReferences;
  if Assigned(FQueue.FUncaughtError) then
    FQueue.FUncaughtError.MarkReferences;
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
  FInFlight := TGocciaTimerEntryList.Create(True);
  FNextId := 1;
  FNow := 0;
  FStart := 0;
  FAdjusted := 0;
  FNanos := 0;
end;

destructor TGocciaTimerQueue.Destroy;
begin
  { FHostEnvironment is deliberately not touched. The queue is a thread
    singleton and outlives the engines that point it at their host environment,
    so by teardown the pointer may name a freed one; clearing the override is
    the detaching extension's job, while its engine is still alive. }
  FreeAndNil(FRoots);
  FInFlight.Free;
  FTimers.Free;
  inherited;
end;

{ The root source registers with whichever collector was current when it was
  built, so a thread whose collector was replaced between engines needs a fresh
  one — the same rule the async-context roots follow.

  Two properties here are load-bearing.

  The identity test asks the source which collector it is *registered with*.
  Comparing against a separately remembered collector pointer is unsound: a
  Shutdown/Initialize pair can put the next thread-local collector at the
  address the previous one had, and the source then stays registered with the
  dead collector while this believes it is current. The collector's destructor
  nils the registration on every source it owns, so this test cannot match a
  destroyed one.

  And every caller must reach this BEFORE the value it wants published becomes
  reachable only from the queue. AddTimer calls it first, while the callback and
  arguments are still held by the caller's argument collection, so there is no
  window in which an entry is in FTimers with no root source to mark it. }
procedure TGocciaTimerQueue.EnsureRoots;
var
  Collector: TGarbageCollector;
begin
  Collector := TGarbageCollector.Instance;
  if Assigned(Collector) and Assigned(FRoots) and
     (FRoots.RegisteredCollector = Collector) then
    Exit;

  FreeAndNil(FRoots);
  if not Assigned(Collector) then
    Exit;

  FRoots := TGocciaTimerRoots.Create;
  FRoots.FQueue := Self;
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
      True, ClockMillisecondsToNanoseconds(FNow),
      True, ClockMillisecondsToNanoseconds(FNow - FAdjusted - FStart))
  else if FMockedDateOnly then
    { setSystemTime outside useFakeTimers freezes the date and nothing else,
      exactly as Vitest's Date-only mock does. }
    FHostEnvironment.OverrideClock(
      True, ClockMillisecondsToNanoseconds(FMockedDate), False, 0)
  else
    FHostEnvironment.ClearClockOverride;
end;

procedure TGocciaTimerQueue.SetNow(const AValue: Double);
begin
  { Central choke point for every fake-clock advance: DoNext (and thus
    AdvanceToNextTimer / RunAllTimers), the DoTick loop, and ClearAllTimers all
    publish through here. Preflight the value BEFORE assigning FNow so a timer
    due beyond the representable range (e.g. MAX_CLOCK_MILLISECONDS + 1) is
    rejected while timer state is still intact, rather than poisoning FNow and
    having PublishClock throw only at conversion time. Validate both halves
    PublishClock converts while faking — the wall value and its monotonic
    counterpart (FNow - FAdjusted - FStart) — since either can fall out of range
    independently. The DoTick preflight is preserved so a rejected bulk advance
    still avoids running any timer. }
  if FFaking then
  begin
    RequireClockInRange(AValue);
    RequireClockInRange(AValue - FAdjusted - FStart);
  end;
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

procedure RequireFiniteEpoch(const AValue: Double);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    ThrowTypeError(NON_FINITE_SYSTEM_TIME_MESSAGE,
      NON_FINITE_SYSTEM_TIME_SUGGESTION);
end;

function TimerLoopLimitMessage: string;
begin
  Result := Format('Aborting after running %d timers, assuming an infinite ' +
    'loop!', [TIMER_LOOP_LIMIT]);
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

{ A due time that is not a finite number can never select an entry. The
  in-flight check is deliberately NOT here: re-entering a running timer is
  something the fake clock permits, and a suite that calls an advance member
  from inside a timer callback gets that nesting under Vitest too (probed: an
  interval whose callback advances the clock re-enters itself three deep). Only
  the real-mode selector excludes it, because only that one picks the earliest
  timer with no window to bound the nesting. }
function TGocciaTimerQueue.IsDispatchable(
  const ATimer: TGocciaTimerEntry): Boolean;
begin
  Result := Assigned(ATimer) and
    (not IsNan(ATimer.CallAt)) and (not IsInfinite(ATimer.CallAt));
end;

{ The range test is written as an explicit "inside" rather than as a negated
  "outside". Every comparison against a NaN bound is false, so the negated form
  skipped nothing and reported an arbitrary timer as due — which is how a NaN
  clock turned into an unbounded DoTick recursion. Non-finite bounds now select
  nothing at all. }
function TGocciaTimerQueue.FirstTimerInRange(
  const AFrom, ATo: Double): TGocciaTimerEntry;
var
  Candidate: TGocciaTimerEntry;
  I: Integer;
begin
  Result := nil;
  if IsNan(AFrom) or IsNan(ATo) or IsInfinite(AFrom) or IsInfinite(ATo) then
    Exit;
  for I := 0 to FTimers.Count - 1 do
  begin
    Candidate := FTimers[I];
    if not IsDispatchable(Candidate) then
      Continue;
    if not ((Candidate.CallAt >= AFrom) and (Candidate.CallAt <= ATo)) then
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
    if IsDispatchable(FTimers[I]) and
       ((Result = nil) or IsEarlier(FTimers[I], Result)) then
      Result := FTimers[I];
end;

{ The real-mode selector, and the only one that skips a timer already on the
  stack.

  Real mode has no window to pick within: it takes the earliest timer whatever
  its due time, because the whole point is to jump the clock to it. An interval
  stays in the queue while its callback runs, so the moment that callback
  reached an engine wait — an `await`, or a microtask drain that led to one —
  this selector handed back the very same entry and the callback re-entered
  itself, again and again, until the stack ran out. The fake-clock paths need no
  such check: their range or their explicit advance bounds the nesting.

  ATimeoutsOnly additionally skips intervals, for the end-of-run drain: an
  uncleared interval is by construction never exhausted, so running it there
  would spend the whole budget and finish no sooner than skipping it. }
function TGocciaTimerQueue.FirstRealTimer(
  const ATimeoutsOnly: Boolean): TGocciaTimerEntry;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTimers.Count - 1 do
  begin
    if FTimers[I].DispatchDepth > 0 then
      Continue;
    if ATimeoutsOnly and (FTimers[I].Kind <> gtkTimeout) then
      Continue;
    if not IsDispatchable(FTimers[I]) then
      Continue;
    if (Result = nil) or IsEarlier(FTimers[I], Result) then
      Result := FTimers[I];
  end;
end;

function TGocciaTimerQueue.LastTimer: TGocciaTimerEntry;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTimers.Count - 1 do
    if IsDispatchable(FTimers[I]) and
       ((Result = nil) or IsEarlier(Result, FTimers[I])) then
      Result := FTimers[I];
end;

{ The truncation is not a shortcut — it is what the fake clock does. It computes
  a due time with `parseInt(delay)`, which stringifies and cuts at the decimal
  point, so a fractional delay loses its fraction. Probed rather than assumed,
  because the opposite is the obvious guess: under Vitest 4.1.10
  `setTimeout(fn, 1.5)` followed by `advanceTimersByTime(1)` fires the timer,
  and a delay of 0.4 is due immediately. Fractions survive on the *advance*
  side instead, through FNanos. }
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
    RetireEntry(Index);
end;

{ Removes the entry at AIndex from the pending list, freeing it — unless its own
  callback is on the stack, in which case it is moved to the in-flight list and
  marked, so the dispatcher's frame keeps a live object to return through. A
  callback that cancels its own interval once it has seen enough is by far the
  common case, and it used to free the entry the dispatcher was about to touch
  on the way out. }
procedure TGocciaTimerQueue.RetireEntry(const AIndex: Integer);
var
  Entry: TGocciaTimerEntry;
begin
  Entry := FTimers[AIndex];
  if Entry.DispatchDepth = 0 then
  begin
    FTimers.Delete(AIndex);
    Exit;
  end;
  Entry.Cleared := True;
  FInFlight.Add(FTimers.Extract(Entry));
end;

{ Empties the pending list without freeing anything a dispatcher still holds. }
procedure TGocciaTimerQueue.RetireAllEntries;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
    RetireEntry(I);
end;

{ Drops the queue and rewinds the clock to the instant fake timers were
  installed — both halves, because the fake clock's reset does both and a suite
  can see either. Outside fake timers it does nothing rather than reporting an
  error, which is also what Vitest does. }
procedure TGocciaTimerQueue.ClearAllTimers;
begin
  if not FFaking then
    Exit;
  RetireAllEntries;
  FNanos := 0;
  SetNow(FStart);
end;

procedure TGocciaTimerQueue.DiscardTimers;
begin
  { Drop the queue but keep any uncaught error: the engine's own idle teardown
    (DiscardRuntimePending) runs this before an interpreted run's test runner
    gets to call TakeUncaughtTimerError, so clearing it here loses a throwing
    module-scope timer and lets the file pass. The error is cleared on read
    (TakeUncaughtError) or at the engine boundary (ResetForEngine), which is the
    point at which a leftover value would otherwise reach the next engine. }
  RetireAllEntries;
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
  if FThrow.HasValue then
    Exit;
  FThrow.Value := AValue;
  FThrow.HasValue := True;
end;

{ Opens a fresh slot for one advance operation and saves the enclosing one.
  Returns the depth to hand back to TakeThrowSlot. }
function TGocciaTimerQueue.PushThrowSlot: Integer;
begin
  { The value of an exception raised by the previous advance is released here
    rather than at the raise itself: by now that exception has been caught or
    has left the engine, and nothing else roots what it carried. }
  FRaisedThrow := nil;

  if FSavedThrowCount >= Length(FSavedThrows) then
    SetLength(FSavedThrows, FSavedThrowCount * 2 + 8);
  Result := FSavedThrowCount;
  FSavedThrows[Result] := FThrow;
  Inc(FSavedThrowCount);
  FThrow.Value := nil;
  FThrow.HasValue := False;
end;

{ Closes the slot AToken opened, restoring the enclosing one, and reports
  whatever this operation recorded. The token is a depth rather than a pop
  count, so an operation unwound by an exception cannot pop past its caller. }
function TGocciaTimerQueue.TakeThrowSlot(const AToken: Integer;
  out AValue: TGocciaValue): Boolean;
var
  I: Integer;
begin
  AValue := nil;
  Result := False;
  if (AToken < 0) or (AToken >= FSavedThrowCount) then
    Exit;

  Result := FThrow.HasValue;
  AValue := FThrow.Value;

  FThrow := FSavedThrows[AToken];
  for I := AToken to FSavedThrowCount - 1 do
  begin
    FSavedThrows[I].Value := nil;
    FSavedThrows[I].HasValue := False;
  end;
  FSavedThrowCount := AToken;
end;

{ The value stays in a marked field across the raise. A TGocciaThrowValue does
  not root what it carries, and unwinding runs arbitrary `finally` blocks that
  can allocate — so releasing the field first left the in-flight value
  collectable. It is dropped at the next PushThrowSlot instead. }
procedure TGocciaTimerQueue.RaiseThrown(const AValue: TGocciaValue);
begin
  FRaisedThrow := AValue;
  raise TGocciaThrowValue.Create(AValue);
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
      { Onto the in-flight stack, not into a single slot: a nested advance can
        put another timeout in flight, and a slot would leave the outer one
        unmarked for the rest of its own callback. }
      FInFlight.Add(Owned);
    end;
  end;

  Inc(ATimer.DispatchDepth);
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
    Dec(ATimer.DispatchDepth);
    { Frees it: the in-flight list owns its entries. A timeout always lands
      here; an interval only when its own callback cancelled it. The cleared
      interval is freed only once the last frame holding it unwinds, so a nested
      re-entry that clears the entry cannot free it out from under the outer
      frame still on the stack. }
    if Assigned(Owned) then
      FInFlight.Remove(Owned)
    else if ATimer.Cleared and (ATimer.DispatchDepth = 0) then
      FInFlight.Remove(ATimer);
  end;
end;

{ Mirrors @sinonjs/fake-timers' doTick. The lagging `Previous` bound is what
  makes a timer scheduled during the tick eligible on the following iteration,
  and the trailing re-check is what picks up timers a callback scheduled
  strictly inside the remaining range. }
function TGocciaTimerQueue.DoTick(const AMilliseconds: Double;
  const AAsync: Boolean): Double;
var
  Fired: Integer;
  NanosTotal, OldNow, Previous, TickFrom, TickTo: Double;
  Timer: TGocciaTimerEntry;
  WasDuringTick: Boolean;
begin
  { Ordered as the fake clock orders it: the non-finite refusal first, because
    `NaN < 0` is false and a NaN would otherwise reach the arithmetic below and
    turn every range test and the trailing re-check into nonsense. }
  if IsNan(AMilliseconds) or IsInfinite(AMilliseconds) then
    ThrowTypeError(NON_FINITE_SYSTEM_TIME_MESSAGE,
      'Advance the timers by a finite number of milliseconds.');
  if AMilliseconds < 0 then
    ThrowTypeError(NEGATIVE_TICKS_MESSAGE);
  if IsNan(FNow) or IsInfinite(FNow) then
    ThrowTypeError(NON_FINITE_SYSTEM_TIME_MESSAGE,
      NON_FINITE_SYSTEM_TIME_SUGGESTION);

  { Whole milliseconds move the clock; the fraction is banked. Two advances of
    half a millisecond therefore move it by one and fire a timer due there,
    which a per-advance truncation would never do. }
  NanosTotal := FNanos + Round(Frac(AMilliseconds) * NANOSECONDS_PER_MILLISECOND);
  TickTo := FNow + Int(AMilliseconds);
  if NanosTotal >= NANOSECONDS_PER_MILLISECOND then
  begin
    TickTo := TickTo + 1;
    NanosTotal := NanosTotal - NANOSECONDS_PER_MILLISECOND;
  end;
  { Reject a target the published clock cannot represent before banking the
    fraction or firing a single timer, so a rejected advance leaves the queue
    unchanged rather than half-updated. Every intermediate SetNow lands between
    FNow and TickTo, so both endpoints being in range covers them. The
    monotonic value shifts with TickTo by the same FAdjusted+FStart offset. }
  if FFaking then
  begin
    RequireClockInRange(TickTo);
    RequireClockInRange(TickTo - FAdjusted - FStart);
  end;
  FNanos := NanosTotal;

  TickFrom := FNow;
  Previous := FNow;
  Fired := 0;
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
      Inc(Fired);
      if Fired > TIMER_TICK_LOOP_LIMIT then
        ThrowError(Format(TICK_LOOP_LIMIT_MESSAGE, [TIMER_TICK_LOOP_LIMIT]));

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

  { Timers a callback scheduled strictly inside the remaining range still have
    to run. The recursion terminates because it can only be entered with a timer
    due at or before TickTo, and each pass either fires one or moves the clock
    to TickTo. }
  Timer := FirstTimerInRange(TickFrom, TickTo);
  if Assigned(Timer) then
    DoTick(TickTo - FNow, AAsync)
  else
    SetNow(TickTo);

  Result := FNow;
end;

function TGocciaTimerQueue.Tick(const AMilliseconds: Double;
  const AAsync: Boolean): Double;
var
  Thrown: TGocciaValue;
  HasThrown: Boolean;
  Token: Integer;
begin
  Token := PushThrowSlot;
  try
    Result := DoTick(AMilliseconds, AAsync);
  finally
    { Runs on the exception path too, so an operation cut short by a timeout or
      an instruction limit still restores its caller's slot instead of leaving
      its own recorded value to surface at an unrelated advance. That hard fault
      wins: the recorded value is simply dropped. }
    HasThrown := TakeThrowSlot(Token, Thrown);
  end;
  if HasThrown then
    RaiseThrown(Thrown);
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
var
  Thrown: TGocciaValue;
  HasThrown: Boolean;
  Token: Integer;
begin
  Token := PushThrowSlot;
  try
    DoNext(AAsync, False);
    DoTick(0, AAsync);
  finally
    HasThrown := TakeThrowSlot(Token, Thrown);
  end;
  if HasThrown then
    RaiseThrown(Thrown);
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
    if not Assigned(FirstTimer) then
      Exit(FNow);
    CheckExecutionTimeout;
    CheckInstructionLimit;
    DoNext(AAsync, False);
  end;
  ThrowError(TimerLoopLimitMessage);
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

{ One real-mode step.

  The exception a callback throws does NOT come out here. This is reached from
  an engine wait — an `await`, or the runner draining a test's returned promise
  — and in Node a timer callback that throws is an uncaught top-level error
  while the frame that happened to be waiting carries on untouched. Raising it
  at the wait made it catchable by an unrelated `try` around the `await`, and
  left the awaited promise pending on top of that. It is parked instead, for the
  host to attribute and report. }
function TGocciaTimerQueue.RunOneRealTimerOfKind(
  const ATimeoutsOnly: Boolean): Boolean;
var
  Thrown: TGocciaValue;
  HasThrown: Boolean;
  Timer: TGocciaTimerEntry;
  Token: Integer;
  WasDuringTick: Boolean;
begin
  Result := False;
  if FFaking then
    Exit;
  Timer := FirstRealTimer(ATimeoutsOnly);
  if not Assigned(Timer) then
    Exit;

  Token := PushThrowSlot;
  try
    WasDuringTick := FDuringTick;
    FDuringTick := True;
    try
      SetNow(Timer.CallAt);
      CallTimer(Timer, True);
      DrainMicrotasks;
    finally
      FDuringTick := WasDuringTick;
    end;
  finally
    HasThrown := TakeThrowSlot(Token, Thrown);
  end;

  if HasThrown and not FHasUncaughtError then
  begin
    FUncaughtError := Thrown;
    FHasUncaughtError := True;
  end;
  Result := True;
end;

function TGocciaTimerQueue.RunOneRealTimer: Boolean;
begin
  Result := RunOneRealTimerOfKind(False);
end;

procedure TGocciaTimerQueue.DrainRealTimers;
var
  I: Integer;
begin
  if FFaking then
    Exit;
  for I := 0 to TIMER_LOOP_LIMIT - 1 do
  begin
    CheckExecutionTimeout;
    CheckInstructionLimit;
    if not RunOneRealTimerOfKind(True) then
      Exit;
  end;
end;

function TGocciaTimerQueue.TakeUncaughtError(
  out AValue: TGocciaValue): Boolean;
begin
  AValue := FUncaughtError;
  Result := FHasUncaughtError;
  FUncaughtError := nil;
  FHasUncaughtError := False;
end;

function TGocciaTimerQueue.HasUncaughtError: Boolean;
begin
  Result := FHasUncaughtError;
end;

procedure TGocciaTimerQueue.BeginFakeTimers(const ANowMilliseconds: Double);
begin
  RequireFiniteEpoch(ANowMilliseconds);
  { Reject a start the published clock cannot represent before retiring the
    previous queue or installing the fresh clock, so a rejected useFakeTimers()
    leaves whatever was in place untouched. The monotonic value starts at 0. }
  RequireClockInRange(ANowMilliseconds);
  EnsureRoots;
  { Re-enabling installs a fresh clock: whatever was scheduled against the
    previous one is discarded rather than carried over, which is what a second
    useFakeTimers() does in Vitest. }
  RetireAllEntries;
  FFaking := True;
  FMockedDateOnly := False;
  FNow := ANowMilliseconds;
  FStart := ANowMilliseconds;
  FAdjusted := 0;
  FNanos := 0;
  PublishClock;
end;

procedure TGocciaTimerQueue.ResetForEngine;
var
  I: Integer;
begin
  EndFakeTimers;
  FInFlight.Clear;
  FThrow.Value := nil;
  FThrow.HasValue := False;
  for I := 0 to FSavedThrowCount - 1 do
  begin
    FSavedThrows[I].Value := nil;
    FSavedThrows[I].HasValue := False;
  end;
  FSavedThrowCount := 0;
  FRaisedThrow := nil;
  FUncaughtError := nil;
  FHasUncaughtError := False;
  FNextId := 1;
end;

procedure TGocciaTimerQueue.EndFakeTimers;
begin
  RetireAllEntries;
  FFaking := False;
  FMockedDateOnly := False;
  FNow := 0;
  FStart := 0;
  FAdjusted := 0;
  FNanos := 0;
  PublishClock;
end;

{ The finite check lives here rather than only at the JavaScript boundary
  because there are two boundaries: the Vitest shim's `vi.setSystemTime`, and
  `goccia:timers`' own `setSystemTime`, which converts its argument and calls
  straight through. Guarding only the shim left the second one able to install a
  NaN clock — after which every due-time comparison is false, the range test
  selects arbitrarily, and the trailing re-check in DoTick recurses on NaN until
  the stack is gone. }
procedure TGocciaTimerQueue.SetSystemTime(const AEpochMilliseconds: Double);
var
  Difference: Double;
  I: Integer;
begin
  RequireFiniteEpoch(AEpochMilliseconds);
  { Preflight the published wall clock before touching any state: both branches
    below publish AEpochMilliseconds (as FMockedDate or as FNow), and the
    faking branch leaves the monotonic value FNow-FAdjusted-FStart unchanged.
    Rejecting an out-of-range target here keeps the queue exactly as it was
    rather than leaving FNow/FMockedDate poisoned for every later publish. }
  RequireClockInRange(AEpochMilliseconds);

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

{ The queue this thread holds, but only when the realm currently executing is
  the one whose timers it carries.

  A ShadowRealm child engine runs on the same thread and shares this singleton.
  Without the check, an `await` inside the child drained the PARENT realm's
  timer callbacks with the child's realm installed as current, so parent code
  ran against child intrinsics — a realm-isolation break, and one that no error
  reports. A queue with no owner recorded is one no timer extension attached to;
  it has no timers either, so the guard costs nothing there. }
function OwningQueueForCurrentRealm: TGocciaTimerQueue;
begin
  Result := TGocciaTimerQueue.Instance;
  if not Assigned(Result) then
    Exit;
  if Result.OwnerRealm <> TObject(CurrentRealm) then
    Result := nil;
end;

function RunOneRealTimer: Boolean;
var
  Queue: TGocciaTimerQueue;
begin
  Queue := OwningQueueForCurrentRealm;
  Result := Assigned(Queue) and Queue.RunOneRealTimer;
end;

function HasRunnableRealTimers: Boolean;
var
  Queue: TGocciaTimerQueue;
begin
  Queue := OwningQueueForCurrentRealm;
  Result := Assigned(Queue) and (not Queue.Faking) and
    Assigned(Queue.FirstRealTimer(False));
end;

procedure DiscardRealTimers;
var
  Queue: TGocciaTimerQueue;
begin
  Queue := TGocciaTimerQueue.Instance;
  { Fake timers are left alone. A suite that installs a clock in beforeEach and
    schedules against it across a test owns that queue, and Vitest does not
    reset it between tests either — only real-mode leftovers, which nothing is
    waiting on, are dropped. }
  if Assigned(Queue) and (not Queue.Faking) then
    Queue.DiscardTimers;
end;

function DrainRealTimersForHost: Boolean;
var
  Queue: TGocciaTimerQueue;
begin
  Queue := OwningQueueForCurrentRealm;
  if not Assigned(Queue) then
    Exit(True);
  Queue.DrainRealTimers;
  Result := not Assigned(Queue.FirstRealTimer(True));
end;

function TakeUncaughtTimerError(out AValue: TGocciaValue): Boolean;
var
  Queue: TGocciaTimerQueue;
begin
  AValue := nil;
  Queue := TGocciaTimerQueue.Instance;
  Result := Assigned(Queue) and Queue.TakeUncaughtError(AValue);
end;

procedure RaiseRealTimerLoopLimit;
begin
  ThrowError(Format(REAL_TIMER_LOOP_LIMIT_MESSAGE, [TIMER_LOOP_LIMIT]));
end;

procedure CleanupTimerQueueThreadState;
begin
  TGocciaTimerQueue.Shutdown;
end;

initialization
  RegisterThreadvarCleanup(CleanupTimerQueueThreadState);

end.
