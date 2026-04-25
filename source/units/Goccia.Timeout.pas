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
  the deadline has elapsed. }
procedure CheckExecutionTimeout;

implementation

uses
  TimingUtils;

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
  GMinDeadlineNs: Int64;     // 0 means no scope on the stack has a deadline
  GMinDeadlineScope: TGocciaTimeoutScope;
  GMinDeadlineMs: Integer;
  GCheckCounter: Integer;

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
  I: Integer;
begin
  GMinDeadlineNs := 0;
  GMinDeadlineMs := 0;
  for I := 0 to GTimeoutDepth - 1 do
  begin
    if GTimeoutStack[I].DeadlineNs = 0 then Continue;
    if (GMinDeadlineNs = 0) or
       (GTimeoutStack[I].DeadlineNs < GMinDeadlineNs) then
    begin
      GMinDeadlineNs := GTimeoutStack[I].DeadlineNs;
      GMinDeadlineScope := GTimeoutStack[I].Scope;
      GMinDeadlineMs := GTimeoutStack[I].DurationMs;
    end;
  end;
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
  GTimeoutDepth := 0;
  GMinDeadlineNs := 0;
  GMinDeadlineMs := 0;
  GCheckCounter := 0;
  if ATimeoutMilliseconds > 0 then
    PushTimeoutScope(tsFile, ATimeoutMilliseconds);
end;

procedure ClearExecutionTimeout;
begin
  GTimeoutDepth := 0;
  GMinDeadlineNs := 0;
  GMinDeadlineMs := 0;
  GCheckCounter := 0;
end;

procedure CheckExecutionTimeout;
begin
  if GMinDeadlineNs = 0 then Exit;

  Inc(GCheckCounter);
  if (GMinDeadlineMs > TIMEOUT_ALWAYS_CHECK_THRESHOLD_MS) and
     ((GCheckCounter and (TIMEOUT_CHECK_INTERVAL - 1)) <> 0) then
    Exit;

  if GetNanoseconds >= GMinDeadlineNs then
    raise TGocciaTimeoutError.Create(GMinDeadlineScope, GMinDeadlineMs);
end;

end.
