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
  end;
  PGocciaInstructionLimitState = ^TGocciaInstructionLimitState;

procedure StartInstructionLimit(const AMaxInstructions: Int64);
procedure ClearInstructionLimit;
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
  GInstructionLimitState.MaxInstructions := AMaxInstructions;
  GInstructionLimitState.InstructionCount := 0;
end;

procedure ClearInstructionLimit;
begin
  GInstructionLimitState.MaxInstructions := 0;
  GInstructionLimitState.InstructionCount := 0;
end;

function CaptureInstructionLimitState: PGocciaInstructionLimitState;
begin
  Result := @GInstructionLimitState;
end;

procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
begin
  if GInstructionLimitState.MaxInstructions > 0 then
    Inc(GInstructionLimitState.InstructionCount);
end;

procedure CheckInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (GInstructionLimitState.MaxInstructions > 0) and
     (GInstructionLimitState.InstructionCount >=
      GInstructionLimitState.MaxInstructions) then
    raise TGocciaInstructionLimitError.CreateFmt(
      'Execution exceeded instruction limit of %d',
      [GInstructionLimitState.MaxInstructions]);
end;

procedure PollInstructionLimit(
  const AState: PGocciaInstructionLimitState); {$IFDEF FPC}inline;{$ENDIF}
begin
  if AState.MaxInstructions > 0 then
  begin
    if AState.InstructionCount >= AState.MaxInstructions then
      raise TGocciaInstructionLimitError.CreateFmt(
        'Execution exceeded instruction limit of %d',
        [AState.MaxInstructions]);
    Inc(AState.InstructionCount);
  end;
end;

end.
