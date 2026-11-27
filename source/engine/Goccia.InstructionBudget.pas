unit Goccia.InstructionBudget;

{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}

interface

type
  { Shared by the engines doing one job. Zero limit measures without capping. Each engine takes units in reserves
    and spends them locally, so the shared counter is touched once per reserve, not once per instruction. }
  IGocciaInstructionBudget = interface
    function Limit: Int64;
    { A capped counter includes the first refused unit and then stays fixed at Limit + 1. Otherwise it includes units
      engines hold in reserve, so it is exact once every engine of the job has ended; an uncapped count saturates at
      High(Int64). }
    function Count: QWord;
    { Count without Units an engine still holds in reserve: what that engine reports while others may still run }
    function CountWithout(Units: QWord): QWord;
    { A nonzero charge succeeds in full or is refused. A zero charge checks whether a cap has been exceeded. }
    function TryCharge(Units: QWord): Boolean;
    { At least MinUnits and at most MaxUnits; under a cap also at most an eighth of what is left, but MinUnits when that
      is more, so engines that share a nearly spent cap are refused late, not early. 0 and the cap spent when fewer than
      MinUnits are left. MinUnits is at least 1. }
    function Reserve(MinUnits, MaxUnits: QWord): QWord;
    { returns reserved units an engine did not spend; nothing once a cap was refused }
    procedure Refund(Units: QWord);
  end;

{ a budget for one job; Limit 0 measures without a cap, and a negative Limit raises EArgumentOutOfRangeException }
function NewInstructionBudget(Limit: Int64): IGocciaInstructionBudget;

implementation

uses
  SysUtils;

type
  { A lock rather than 64-bit atomics, which 32-bit targets lack: engines take it once per reserve, not per
    instruction. }
  TInstructionBudget = class(TInterfacedObject, IGocciaInstructionBudget)
  private
    FLimit: Int64;
    { a capped job: the units left, -1 once refused }
    FLeft: Int64;
    { an uncapped job: the units charged and reserved; FOverflowed once a reserve went past High(QWord), after which
      the total stays saturated }
    FUsed: QWord;
    FOverflowed: Boolean;
    FLock: TRTLCriticalSection;
  public
    constructor Create(ALimit: Int64);
    destructor Destroy; override;
    function Limit: Int64;
    function Count: QWord;
    function CountWithout(Units: QWord): QWord;
    function TryCharge(Units: QWord): Boolean;
    function Reserve(MinUnits, MaxUnits: QWord): QWord;
    procedure Refund(Units: QWord);
  end;

constructor TInstructionBudget.Create(ALimit: Int64);
begin
  inherited Create;
  InitCriticalSection(FLock);
  FLimit := ALimit;
  FLeft := ALimit;
  FUsed := 0;
  FOverflowed := False;
end;

destructor TInstructionBudget.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

function TInstructionBudget.Limit: Int64;
begin
  Result := FLimit;
end;

function TInstructionBudget.CountWithout(Units: QWord): QWord;
begin
  EnterCriticalSection(FLock);
  try
    if FLimit > 0 then
    begin
      if FLeft < 0 then Exit(QWord(FLimit) + 1);
      Result := QWord(FLimit - FLeft);
    end
    else Result := FUsed;
    if Result >= Units then Dec(Result, Units) else Result := 0;
    if (FLimit = 0) and (Result > QWord(High(Int64))) then Result := QWord(High(Int64));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TInstructionBudget.Count: QWord;
begin
  Result := CountWithout(0);
end;

function TInstructionBudget.TryCharge(Units: QWord): Boolean;
begin
  if Units = 0 then
  begin
    EnterCriticalSection(FLock);
    try
      Result := (FLimit = 0) or (FLeft >= 0);
    finally
      LeaveCriticalSection(FLock);
    end;
    Exit;
  end;
  Result := Reserve(Units, Units) = Units;
end;

function TInstructionBudget.Reserve(MinUnits, MaxUnits: QWord): QWord;
begin
  if MinUnits = 0 then MinUnits := 1;
  if MaxUnits < MinUnits then MaxUnits := MinUnits;
  EnterCriticalSection(FLock);
  try
    if FLimit > 0 then
    begin
      if FLeft < 0 then Exit(0);
      if MinUnits > QWord(FLeft) then
      begin
        FLeft := -1;
        Exit(0);
      end;
      Result := QWord(FLeft) div 8;
      if Result > MaxUnits then Result := MaxUnits;
      if Result < MinUnits then Result := MinUnits;
      Dec(FLeft, Int64(Result));
    end
    else
    begin
      Result := MaxUnits;
      if MaxUnits <= High(QWord) - FUsed then Inc(FUsed, MaxUnits)
      else
      begin
        FUsed := High(QWord);
        FOverflowed := True;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TInstructionBudget.Refund(Units: QWord);
begin
  if Units = 0 then Exit;
  EnterCriticalSection(FLock);
  try
    if FLimit > 0 then
    begin
      if FLeft < 0 then Exit;
      if Units > QWord(FLimit - FLeft) then Units := QWord(FLimit - FLeft);
      Inc(FLeft, Int64(Units));
    end
    { a total that overflowed lost track of what it held, so it stays saturated }
    else if FOverflowed then Exit
    else if Units >= FUsed then FUsed := 0
    else Dec(FUsed, Units);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function NewInstructionBudget(Limit: Int64): IGocciaInstructionBudget;
begin
  { checked before the object exists: a constructor that raised would free a lock it never made }
  if Limit < 0 then raise EArgumentOutOfRangeException.Create('Instruction budget cannot be negative');
  Result := TInstructionBudget.Create(Limit);
end;

end.
