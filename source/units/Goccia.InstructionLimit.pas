unit Goccia.InstructionLimit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaInstructionLimitError = class(Exception);

procedure StartInstructionLimit(const AMaxInstructions: Int64);
procedure ClearInstructionLimit;
procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
procedure CheckInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}
procedure PollInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}

implementation

threadvar
  GMaxInstructions: Int64;
  GInstructionCount: Int64;

procedure StartInstructionLimit(const AMaxInstructions: Int64);
begin
  GMaxInstructions := AMaxInstructions;
  GInstructionCount := 0;
end;

procedure ClearInstructionLimit;
begin
  GMaxInstructions := 0;
  GInstructionCount := 0;
end;

procedure IncrementInstructionCounter; {$IFDEF FPC}inline;{$ENDIF}
begin
  if GMaxInstructions > 0 then
    Inc(GInstructionCount);
end;

procedure CheckInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (GMaxInstructions > 0) and (GInstructionCount >= GMaxInstructions) then
    raise TGocciaInstructionLimitError.CreateFmt(
      'Execution exceeded instruction limit of %d', [GMaxInstructions]);
end;

procedure PollInstructionLimit; {$IFDEF FPC}inline;{$ENDIF}
begin
  if GMaxInstructions > 0 then
  begin
    if GInstructionCount >= GMaxInstructions then
      raise TGocciaInstructionLimitError.CreateFmt(
        'Execution exceeded instruction limit of %d', [GMaxInstructions]);
    Inc(GInstructionCount);
  end;
end;

end.
