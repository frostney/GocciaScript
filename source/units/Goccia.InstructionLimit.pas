unit Goccia.InstructionLimit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaInstructionLimitError = class(Exception);

  { Raised on the thread a host has armed with Goccia.ExecutionAbort once
    another thread has requested the abort. A subclass, so that every
    boundary that lets the instruction limit keep unwinding (the uncatchable
    arms, Goccia.UncatchableFault, the sandbox's resource-limit
    classification) treats it the same way; a reporter that wants to say
    "aborted" tests for it first (ADR 0174). }
  TGocciaAbortError = class(TGocciaInstructionLimitError);

  { Non-owning handle to the calling thread's live instruction budget.
    Capture once at the bytecode-loop boundary, then pass it to
    PollInstructionLimit. Start/Clear mutate the same record, so a captured
    handle observes budget changes without resolving an FPC threadvar for
    every opcode. The handle is valid only on the thread that captured it. }
  TGocciaInstructionLimitState = record
  private
    MaxInstructions: Int64;
    InstructionCount: Int64;
    ScopeStarts: array of Int64;
    ScopeLimits: array of Int64;
    ScopeDepth: Integer;
    Active: Boolean;
    { The lowest InstructionCount at which the base budget or a scope's
      budget is spent, High(Int64) when none is set, so that a poll is one
      comparison. Start, Clear, Push and Pop recompute it. Code that lowers
      MaxInstructions or a scope's limit any other way must lower it too, and
      after the limit: a poll that reaches it checks every budget exactly and
      raises for a spent one, or recomputes it when none is spent. }
    NextLimitCount: Int64;
    { The request word of the abort handle that armed this thread, nil when
      none did (Goccia.ExecutionAbort). A request lowers NextLimitCount to 0
      from another thread; every recompute reads this word again after it
      stores, so that a recompute that raced with the request cannot undo it.
      While it is set, Active stays True, so the VM takes its instrumented
      dispatch path and the poll runs. }
    AbortRequested: PLongInt;
  end;
  PGocciaInstructionLimitState = ^TGocciaInstructionLimitState;

procedure StartInstructionLimit(const AMaxInstructions: Int64);
procedure ClearInstructionLimit;
procedure PushInstructionLimitScope(const AMaxInstructions: Int64);
procedure PopInstructionLimitScope;
function CaptureInstructionLimitState: PGocciaInstructionLimitState; {$IFDEF FPC}inline;{$ENDIF}
function InstructionLimitIsActive: Boolean; {$IFDEF FPC}inline;{$ENDIF}
procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
procedure CheckInstructionLimit;
{ Native work that no dispatched instruction counts: adds ACount to the
  budget (when one is active) and checks it, raising for a spent budget or a
  requested abort. Goccia.Timeout's CheckExecutionTimeout calls it from the
  slow path of a native loop's poll, one unit per element. }
procedure SpendInstructions(const ACount: Int64);
{ CheckInstructionLimit through a captured handle: no thread-variable lookup.
  Goccia.ThreadPolls keeps the handle beside the InstructionLimitActive
  flag. }
procedure CheckInstructionLimitState(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}
procedure PollInstructionLimit(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}
{ The slow path of the two above, taken once the count reaches
  NextLimitCount: checks every budget exactly and raises
  TGocciaInstructionLimitError for the first one spent. It is in the interface
  so that they can be inlined into other units; call them instead. }
procedure InstructionLimitReached(const AState: PGocciaInstructionLimitState);

{ The abort plumbing, for Goccia.ExecutionAbort alone; hosts use that unit.

  ArmInstructionAbort makes the calling thread's instruction poll observe the
  word AAbortRequested points to, and returns where a request has to write 0
  to stop a run that is inside the poll's single comparison. The word and the
  address stay valid until DisarmInstructionAbort, on the same thread, which
  also runs when the thread ends. Start/Clear and the scope procedures keep
  the poll active while the arm lasts. }
function ArmInstructionAbort(const AAbortRequested: PLongInt): PInt64;
procedure DisarmInstructionAbort;
{ Raises TGocciaAbortError. }
procedure RaiseAbort;

implementation

uses
  CriticalSections,

  Goccia.ThreadPolls,
  Goccia.Timeout;

threadvar
  GInstructionLimitState: TGocciaInstructionLimitState;

const
  ABORT_MESSAGE = 'Execution aborted by the host';

procedure RaiseAbort;
begin
  raise TGocciaAbortError.Create(ABORT_MESSAGE);
end;

procedure RecomputeNextLimitCount(const AState: PGocciaInstructionLimitState);
var
  Next, Candidate: Int64;
  I: Integer;
begin
  Next := High(Int64);
  if AState^.MaxInstructions > 0 then
    Next := AState^.MaxInstructions;
  for I := 0 to AState^.ScopeDepth - 1 do
    if AState^.ScopeLimits[I] > 0 then
    begin
      if AState^.ScopeStarts[I] > High(Int64) - AState^.ScopeLimits[I] then
        Candidate := High(Int64)
      else
        Candidate := AState^.ScopeStarts[I] + AState^.ScopeLimits[I];
      if Candidate < Next then
        Next := Candidate;
    end;
  AState^.NextLimitCount := Next;
  { A request that arrived while Next was being computed wrote 0 before this
    store, or will write it after the fence below; either way the word is
    visible here, or the 0 lands after this store. This is the engine thread's
    half of the handshake with Goccia.ExecutionAbort's Request. }
  if AState^.AbortRequested <> nil then
  begin
    FullMemoryBarrier;
    if AState^.AbortRequested^ <> 0 then
      AState^.NextLimitCount := 0;
  end;
end;

// Every assignment to GInstructionLimitState.Active goes through here, so the
// mirror that Goccia.ThreadPolls readers see cannot drift from it. Every
// change to a budget ends here too, so it recomputes NextLimitCount.
procedure SetInstructionLimitActive(const AActive: Boolean);
var
  Armed: Boolean;
begin
  // An armed abort keeps the poll running whether or not a budget is set.
  Armed := AActive or (GInstructionLimitState.AbortRequested <> nil);
  RecomputeNextLimitCount(@GInstructionLimitState);
  // A budget makes native loops spend it from their timeout poll.
  SetTimeoutBudgetArmed(AActive);
  GInstructionLimitState.Active := Armed;
  GThreadPolls.InstructionLimitState := @GInstructionLimitState;
  GThreadPolls.InstructionLimitActive := Armed;
end;

function ArmInstructionAbort(const AAbortRequested: PLongInt): PInt64;
begin
  GInstructionLimitState.AbortRequested := AAbortRequested;
  SetInstructionLimitActive((GInstructionLimitState.MaxInstructions > 0) or
    (GInstructionLimitState.ScopeDepth > 0));
  Result := @GInstructionLimitState.NextLimitCount;
end;

procedure DisarmInstructionAbort;
begin
  if GInstructionLimitState.AbortRequested = nil then
    Exit;
  GInstructionLimitState.AbortRequested := nil;
  SetInstructionLimitActive((GInstructionLimitState.MaxInstructions > 0) or
    (GInstructionLimitState.ScopeDepth > 0));
end;

procedure StartInstructionLimit(const AMaxInstructions: Int64);
begin
  GInstructionLimitState.ScopeDepth := 0;
  SetLength(GInstructionLimitState.ScopeStarts, 0);
  SetLength(GInstructionLimitState.ScopeLimits, 0);
  GInstructionLimitState.MaxInstructions := AMaxInstructions;
  GInstructionLimitState.InstructionCount := 0;
  SetInstructionLimitActive(AMaxInstructions > 0);
end;

procedure ClearInstructionLimit;
begin
  GInstructionLimitState.ScopeDepth := 0;
  SetLength(GInstructionLimitState.ScopeStarts, 0);
  SetLength(GInstructionLimitState.ScopeLimits, 0);
  GInstructionLimitState.MaxInstructions := 0;
  GInstructionLimitState.InstructionCount := 0;
  SetInstructionLimitActive(False);
end;

procedure PushInstructionLimitScope(const AMaxInstructions: Int64);
var
  Index: Integer;
begin
  if (GInstructionLimitState.ScopeDepth = 0) and
     (GInstructionLimitState.MaxInstructions <= 0) then
    GInstructionLimitState.InstructionCount := 0;
  Index := GInstructionLimitState.ScopeDepth;
  SetLength(GInstructionLimitState.ScopeStarts, Index + 1);
  SetLength(GInstructionLimitState.ScopeLimits, Index + 1);
  GInstructionLimitState.ScopeStarts[Index] :=
    GInstructionLimitState.InstructionCount;
  GInstructionLimitState.ScopeLimits[Index] := AMaxInstructions;
  Inc(GInstructionLimitState.ScopeDepth);
  SetInstructionLimitActive(True);
end;

procedure PopInstructionLimitScope;
begin
  if GInstructionLimitState.ScopeDepth = 0 then
    Exit;
  Dec(GInstructionLimitState.ScopeDepth);
  SetLength(GInstructionLimitState.ScopeStarts,
    GInstructionLimitState.ScopeDepth);
  SetLength(GInstructionLimitState.ScopeLimits,
    GInstructionLimitState.ScopeDepth);
  SetInstructionLimitActive((GInstructionLimitState.MaxInstructions > 0) or
    (GInstructionLimitState.ScopeDepth > 0));
end;

procedure RaiseInstructionLimit(const AMaxInstructions: Int64);
begin
  raise TGocciaInstructionLimitError.CreateFmt(
    'Execution exceeded instruction limit of %d', [AMaxInstructions]);
end;

procedure InstructionLimitReached(const AState: PGocciaInstructionLimitState);
var
  I: Integer;
begin
  // Ahead of the budgets: a request is a stop whatever else is spent.
  if (AState^.AbortRequested <> nil) and (AState^.AbortRequested^ <> 0) then
    RaiseAbort;
  for I := 0 to AState^.ScopeDepth - 1 do
    if (AState^.ScopeLimits[I] > 0) and
       (AState^.InstructionCount - AState^.ScopeStarts[I] >=
        AState^.ScopeLimits[I]) then
      RaiseInstructionLimit(AState^.ScopeLimits[I]);
  if (AState^.MaxInstructions > 0) and
     (AState^.InstructionCount >= AState^.MaxInstructions) then
    RaiseInstructionLimit(AState^.MaxInstructions);
  // Nothing is spent: NextLimitCount was lowered from outside, or this is a
  // thread whose state was never started, where it is still 0.
  RecomputeNextLimitCount(AState);
end;

procedure SpendInstructions(const ACount: Int64);
begin
  if GInstructionLimitState.Active then
    Inc(GInstructionLimitState.InstructionCount, ACount);
  CheckInstructionLimitState(@GInstructionLimitState);
end;

function CaptureInstructionLimitState: PGocciaInstructionLimitState;
begin
  Result := @GInstructionLimitState;
end;

function InstructionLimitIsActive: Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := GInstructionLimitState.Active;
end;

procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
begin
  if GInstructionLimitState.Active then
    Inc(GInstructionLimitState.InstructionCount);
end;

procedure CheckInstructionLimitState(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}
begin
  if AState^.InstructionCount >= AState^.NextLimitCount then
    InstructionLimitReached(AState);
end;

procedure CheckInstructionLimit;
begin
  CheckInstructionLimitState(@GInstructionLimitState);
end;

procedure PollInstructionLimit(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}
begin
  if AState^.Active then
  begin
    CheckInstructionLimitState(AState);
    Inc(AState^.InstructionCount);
  end;
end;

end.
