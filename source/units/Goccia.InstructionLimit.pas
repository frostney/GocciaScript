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
  end;
  PGocciaInstructionLimitState = ^TGocciaInstructionLimitState;

procedure StartInstructionLimit(const AMaxInstructions: Int64);
procedure ClearInstructionLimit;
procedure PushInstructionLimitScope(const AMaxInstructions: Int64);
procedure PopInstructionLimitScope;
function CaptureInstructionLimitState: PGocciaInstructionLimitState; {$IFDEF FPC}inline;{$ENDIF}
procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
procedure CheckInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}
procedure PollInstructionLimit(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}

implementation

threadvar
  GInstructionLimitState: TGocciaInstructionLimitState;

procedure StartInstructionLimit(const AMaxInstructions: Int64);
begin
  GInstructionLimitState.ScopeDepth := 0;
  SetLength(GInstructionLimitState.ScopeStarts, 0);
  SetLength(GInstructionLimitState.ScopeLimits, 0);
  GInstructionLimitState.MaxInstructions := AMaxInstructions;
  GInstructionLimitState.InstructionCount := 0;
  GInstructionLimitState.Active := AMaxInstructions > 0;
end;

procedure ClearInstructionLimit;
begin
  GInstructionLimitState.ScopeDepth := 0;
  SetLength(GInstructionLimitState.ScopeStarts, 0);
  SetLength(GInstructionLimitState.ScopeLimits, 0);
  GInstructionLimitState.MaxInstructions := 0;
  GInstructionLimitState.InstructionCount := 0;
  GInstructionLimitState.Active := False;
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
  GInstructionLimitState.Active := True;
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
  GInstructionLimitState.Active :=
    (GInstructionLimitState.MaxInstructions > 0) or
    (GInstructionLimitState.ScopeDepth > 0);
end;

procedure RaiseInstructionLimit(const AMaxInstructions: Int64);
begin
  raise TGocciaInstructionLimitError.CreateFmt(
    'Execution exceeded instruction limit of %d', [AMaxInstructions]);
end;

procedure CheckScopedInstructionLimits;
var
  I: Integer;
begin
  for I := 0 to GInstructionLimitState.ScopeDepth - 1 do
    if (GInstructionLimitState.ScopeLimits[I] > 0) and
       (GInstructionLimitState.InstructionCount -
        GInstructionLimitState.ScopeStarts[I] >=
        GInstructionLimitState.ScopeLimits[I]) then
      RaiseInstructionLimit(GInstructionLimitState.ScopeLimits[I]);
end;

function CaptureInstructionLimitState: PGocciaInstructionLimitState;
begin
  Result := @GInstructionLimitState;
end;

procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
begin
  if GInstructionLimitState.Active then
    Inc(GInstructionLimitState.InstructionCount);
end;

procedure CheckInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}
begin
  if GInstructionLimitState.ScopeDepth > 0 then
    CheckScopedInstructionLimits;
  if (GInstructionLimitState.MaxInstructions > 0) and
     (GInstructionLimitState.InstructionCount >=
      GInstructionLimitState.MaxInstructions) then
    RaiseInstructionLimit(GInstructionLimitState.MaxInstructions);
end;

procedure PollInstructionLimit(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}
begin
  if AState.Active then
  begin
    if AState.ScopeDepth > 0 then
      CheckScopedInstructionLimits;
    if (AState.MaxInstructions > 0) and
       (AState.InstructionCount >= AState.MaxInstructions) then
      RaiseInstructionLimit(AState.MaxInstructions);
    Inc(AState.InstructionCount);
  end;
end;

end.
