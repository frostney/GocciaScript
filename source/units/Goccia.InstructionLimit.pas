unit Goccia.InstructionLimit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaInstructionLimitError = class(Exception);

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

implementation

uses
  Goccia.ThreadPolls;

threadvar
  GInstructionLimitState: TGocciaInstructionLimitState;

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
end;

// Every assignment to GInstructionLimitState.Active goes through here, so the
// mirror that Goccia.ThreadPolls readers see cannot drift from it. Every
// change to a budget ends here too, so it recomputes NextLimitCount.
procedure SetInstructionLimitActive(const AActive: Boolean);
begin
  RecomputeNextLimitCount(@GInstructionLimitState);
  GInstructionLimitState.Active := AActive;
  GThreadPolls.InstructionLimitState := @GInstructionLimitState;
  GThreadPolls.InstructionLimitActive := AActive;
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
