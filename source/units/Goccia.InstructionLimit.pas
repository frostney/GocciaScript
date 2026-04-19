unit Goccia.InstructionLimit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaInstructionLimitError = class(Exception);

procedure StartInstructionLimit(const AMaxInstructions: Int64);
procedure ClearInstructionLimit;
procedure IncrementInstructionCounter; inline;
procedure CheckInstructionLimit; inline;

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

procedure IncrementInstructionCounter; inline;
begin
  Inc(GInstructionCount);
end;

procedure CheckInstructionLimit; inline;
begin
  if (GMaxInstructions > 0) and (GInstructionCount >= GMaxInstructions) then
    raise TGocciaInstructionLimitError.CreateFmt(
      'Execution exceeded instruction limit of %d', [GMaxInstructions]);
end;

end.
