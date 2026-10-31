unit Goccia.Timeout;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  { Identifies which scope of execution a timeout applies to.
    The test runner pushes tsDescribe / tsTest around suite and
    test execution; the engine and other apps push tsFile around
    a whole script. The poll point checks whichever deadline is
    soonest and reports its scope on expiry. }
  TGocciaTimeoutScope = (tsFile, tsDescribe, tsTest);

  TGocciaTimeoutError = class(Exception)
  private
    FScope: TGocciaTimeoutScope;
    FDurationMs: Integer;
  public
    constructor Create(const AScope: TGocciaTimeoutScope;
      const ADurationMs: Integer);
    property Scope: TGocciaTimeoutScope read FScope;
    property DurationMs: Integer read FDurationMs;
  end;

  { What a timeout poll reads, kept in one record so that one thread-variable
    lookup reaches all of it: every reference to a thread variable is a lookup
    (a pthread_getspecific call on POSIX targets). Only the scope procedures
    below change it, on the thread that owns it. A handle to it is valid only
    on that thread. }
  TGocciaTimeoutPollState = record
  private
    DeadlineNs: Int64;       // 0 means no scope on the stack has a deadline
    CheckCounter: Integer;
    // A poll reads the clock when CheckCounter and CheckMask is 0: every time
    // while the soonest deadline is short, once in 1024 polls otherwise.
    CheckMask: Integer;
    DurationMs: Integer;
    Scope: TGocciaTimeoutScope;
  end;
  PGocciaTimeoutPollState = ^TGocciaTimeoutPollState;

{ File-scope (back-compat) entry points. StartExecutionTimeout resets
  the stack and pushes a single tsFile entry; ClearExecutionTimeout
  drops every active scope. Existing callers in script-loader / REPL
  / bundler / test-runner-file that paired Start..Clear keep working
  unchanged. }
procedure StartExecutionTimeout(const ATimeoutMilliseconds: Integer);
procedure ClearExecutionTimeout;

{ Nested-scope entry points. Each Push must be paired with exactly
  one Pop. A duration of 0 is allowed (and the corresponding scope
  contributes no deadline) so that callers can push unconditionally
  and balance is preserved. }
procedure PushTimeoutScope(const AScope: TGocciaTimeoutScope;
  const ATimeoutMilliseconds: Integer);
procedure PopTimeoutScope;

{ Polled by the bytecode VM and other long-running loops. Raises
  TGocciaTimeoutError tagged with the soonest-deadline scope when
  the deadline has elapsed.

  Chokepoint rule for native loops: any native loop whose iteration count
  scales with a JS-controlled value (array index/length, string length,
  regex subject length, element count) must poll this between iterations —
  the dispatch-loop polls cannot fire while a single native call runs.
  Self-throttling makes an unmasked per-iteration call cheap (1023 of 1024
  calls are a counter increment); loops too hot even for that should use a
  small local mask (and 255), never larger — local masks COMPOSE with the
  internal 1/1024 throttle, so a 4096 local mask would stretch real clock
  reads to one per ~4.2M iterations and let deadlines overshoot by seconds.
  Loops that mutate JS-visible state must restore consistency before the
  raise propagates (see ExtendElementsWithHoles in Goccia.Values.HoleValue
  for the rollback pattern). }
procedure CheckExecutionTimeout;
{ CheckExecutionTimeout through the calling thread's poll state, for a caller
  that already holds it (Goccia.ThreadPolls keeps it beside the TimeoutArmed
  flag): the same throttle and the same error, without a thread-variable
  lookup. }
procedure PollExecutionTimeout(
  const AState: PGocciaTimeoutPollState); {$IFDEF FPC}inline;{$ENDIF}
{ The slow path of a poll: reads the clock and raises when the soonest
  deadline has passed. It is in the interface so that PollExecutionTimeout can
  be inlined into other units; call PollExecutionTimeout instead. }
procedure CheckExecutionTimeoutDeadline(const AState: PGocciaTimeoutPollState);
{ CheckExecutionTimeout without the self-throttle: the clock is read on every
  call. For a loop that sleeps or blocks between iterations, where one clock
  read in 1024 would overshoot the deadline by the length of 1023 waits, and
  for a point that must not let work finished after the deadline through. }
procedure CheckExecutionTimeoutNow;
function RemainingExecutionTimeoutMilliseconds: Integer;

implementation

uses
  TimingUtils,

  Goccia.ThreadCleanupRegistry,
  Goccia.ThreadPolls;

const
  TIMEOUT_CHECK_INTERVAL = 1024;
  TIMEOUT_ALWAYS_CHECK_THRESHOLD_MS = 16;
  // Initial capacity reserved on first push; the stack grows on demand
  // so deeply-nested describe blocks no longer hit a fixed ceiling.
  TIMEOUT_INITIAL_CAPACITY = 16;

type
  TTimeoutStackEntry = record
    Scope: TGocciaTimeoutScope;
    DurationMs: Integer;
    DeadlineNs: Int64;       // 0 means "no deadline at this scope"
  end;

threadvar
  GTimeoutStack: array of TTimeoutStackEntry;
  GTimeoutDepth: Integer;
  GTimeoutPoll: TGocciaTimeoutPollState;

constructor TGocciaTimeoutError.Create(const AScope: TGocciaTimeoutScope;
  const ADurationMs: Integer);
const
  ScopeNames: array[TGocciaTimeoutScope] of string =
    ('file', 'describe', 'test');
begin
  inherited CreateFmt('%s timed out after %dms', [ScopeNames[AScope],
    ADurationMs]);
  FScope := AScope;
  FDurationMs := ADurationMs;
end;

procedure RecomputeMinDeadline;
var
  Poll: PGocciaTimeoutPollState;
  I: Integer;
begin
  Poll := @GTimeoutPoll;
  Poll^.DeadlineNs := 0;
  Poll^.DurationMs := 0;
  for I := 0 to GTimeoutDepth - 1 do
  begin
    if GTimeoutStack[I].DeadlineNs = 0 then Continue;
    if (Poll^.DeadlineNs = 0) or
       (GTimeoutStack[I].DeadlineNs < Poll^.DeadlineNs) then
    begin
      Poll^.DeadlineNs := GTimeoutStack[I].DeadlineNs;
      Poll^.Scope := GTimeoutStack[I].Scope;
      Poll^.DurationMs := GTimeoutStack[I].DurationMs;
    end;
  end;
  if Poll^.DurationMs > TIMEOUT_ALWAYS_CHECK_THRESHOLD_MS then
    Poll^.CheckMask := TIMEOUT_CHECK_INTERVAL - 1
  else
    Poll^.CheckMask := 0;
  // The deadline changes here and nowhere else; this mirrors it for the
  // callers that poll through Goccia.ThreadPolls.
  GThreadPolls.TimeoutState := Poll;
  GThreadPolls.TimeoutArmed := Poll^.DeadlineNs <> 0;
end;

procedure PushTimeoutScope(const AScope: TGocciaTimeoutScope;
  const ATimeoutMilliseconds: Integer);
var
  NewCapacity: Integer;
begin
  if GTimeoutDepth >= Length(GTimeoutStack) then
  begin
    if Length(GTimeoutStack) = 0 then
      NewCapacity := TIMEOUT_INITIAL_CAPACITY
    else
      NewCapacity := Length(GTimeoutStack) * 2;
    SetLength(GTimeoutStack, NewCapacity);
  end;
  GTimeoutStack[GTimeoutDepth].Scope := AScope;
  GTimeoutStack[GTimeoutDepth].DurationMs := ATimeoutMilliseconds;
  if ATimeoutMilliseconds > 0 then
    GTimeoutStack[GTimeoutDepth].DeadlineNs := GetNanoseconds +
      Int64(ATimeoutMilliseconds) * 1000000
  else
    GTimeoutStack[GTimeoutDepth].DeadlineNs := 0;
  Inc(GTimeoutDepth);
  RecomputeMinDeadline;
end;

procedure PopTimeoutScope;
begin
  if GTimeoutDepth = 0 then Exit;
  Dec(GTimeoutDepth);
  RecomputeMinDeadline;
end;

procedure StartExecutionTimeout(const ATimeoutMilliseconds: Integer);
begin
  ClearExecutionTimeout;
  if ATimeoutMilliseconds > 0 then
    PushTimeoutScope(tsFile, ATimeoutMilliseconds);
end;

procedure ClearExecutionTimeout;
begin
  GTimeoutDepth := 0;
  RecomputeMinDeadline;
  GTimeoutPoll.CheckCounter := 0;
end;

procedure CheckExecutionTimeoutDeadline(const AState: PGocciaTimeoutPollState);
begin
  if GetNanoseconds >= AState^.DeadlineNs then
    raise TGocciaTimeoutError.Create(AState^.Scope, AState^.DurationMs);
end;

procedure PollExecutionTimeout(
  const AState: PGocciaTimeoutPollState); {$IFDEF FPC}inline;{$ENDIF}
begin
  if AState^.DeadlineNs <> 0 then
  begin
    Inc(AState^.CheckCounter);
    if (AState^.CheckCounter and AState^.CheckMask) = 0 then
      CheckExecutionTimeoutDeadline(AState);
  end;
end;

procedure CheckExecutionTimeout;
begin
  PollExecutionTimeout(@GTimeoutPoll);
end;

procedure CheckExecutionTimeoutNow;
var
  Poll: PGocciaTimeoutPollState;
begin
  Poll := @GTimeoutPoll;
  if Poll^.DeadlineNs <> 0 then
    CheckExecutionTimeoutDeadline(Poll);
end;

function RemainingExecutionTimeoutMilliseconds: Integer;
var
  Poll: PGocciaTimeoutPollState;
  RemainingNs: Int64;
begin
  Poll := @GTimeoutPoll;
  if Poll^.DeadlineNs = 0 then
    Exit(0);
  RemainingNs := Poll^.DeadlineNs - GetNanoseconds;
  if RemainingNs <= 0 then
    raise TGocciaTimeoutError.Create(Poll^.Scope, Poll^.DurationMs);
  if RemainingNs >= Int64(High(Integer)) * 1000000 then
    Exit(High(Integer));
  Result := Integer((RemainingNs + 999999) div 1000000);
end;

{ The calling thread's scope stack, released when its runtime shuts down. }
procedure ReleaseTimeoutStack;
begin
  SetLength(GTimeoutStack, 0);
  GTimeoutDepth := 0;
end;

initialization
  RegisterThreadvarCleanup(@ReleaseTimeoutStack);

end.
