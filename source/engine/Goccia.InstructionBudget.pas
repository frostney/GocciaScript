unit Goccia.InstructionBudget;

{$mode delphiunicode}{$H+}

interface

type
  { Shared by the engines doing one job. Zero limit measures without capping. Each engine takes units in reserves
    and spends them locally, so the shared counter is touched once per reserve, not once per instruction. }
  IGocciaInstructionBudget = interface
    function Limit: Int64;
    { A capped counter includes the first refused unit and then stays fixed at Limit + 1. Otherwise it includes units
      engines hold in reserve, so it is exact once every engine of the job has ended. }
    function Count: QWord;
    { A nonzero charge succeeds in full or is refused. A zero charge checks whether a cap has been exceeded. }
    function TryCharge(AUnits: QWord): Boolean;
    { At least AMin and at most AMax units; under a cap also at most an eighth of what is left (but AMin when that is
      less), so engines that share a nearly spent cap are refused late, not early. 0 and the cap spent when fewer than
      AMin are left. AMin is at least 1. }
    function Reserve(AMin, AMax: QWord): QWord;
    { returns reserved units an engine did not spend; nothing once a cap was refused }
    procedure Refund(AUnits: QWord);
  end;

function NewInstructionBudget(ALimit: Int64): IGocciaInstructionBudget;

implementation

uses
  SysUtils;

type
  { A lock rather than 64-bit atomics, which 32-bit targets lack: engines take it once per reserve, not per
    instruction. }
  TInstructionBudget = class(TInterfacedObject, IGocciaInstructionBudget)
  private
    FLimit: Int64;
    { Remaining units for a capped job (-1 after refusal), accumulated units otherwise. }
    FState: Int64;
    FLock: TRTLCriticalSection;
    function ReadState: Int64;
  public
    constructor Create(ALimit: Int64);
    destructor Destroy; override;
    function Limit: Int64;
    function Count: QWord;
    function TryCharge(AUnits: QWord): Boolean;
    function Reserve(AMin, AMax: QWord): QWord;
    procedure Refund(AUnits: QWord);
  end;

constructor TInstructionBudget.Create(ALimit: Int64);
begin
  inherited Create;
  if ALimit < 0 then raise EArgumentOutOfRangeException.Create('Instruction budget cannot be negative');
  FLimit := ALimit;
  FState := ALimit;
  InitCriticalSection(FLock);
end;

destructor TInstructionBudget.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

function TInstructionBudget.ReadState: Int64;
begin
  EnterCriticalSection(FLock);
  Result := FState;
  LeaveCriticalSection(FLock);
end;

function TInstructionBudget.Limit: Int64;
begin
  Result := FLimit;
end;

function TInstructionBudget.Count: QWord;
var State: Int64;
begin
  State := ReadState;
  if FLimit = 0 then Result := QWord(State)
  else if State >= 0 then Result := QWord(FLimit - State)
  else Result := QWord(FLimit) + 1;
end;

function TInstructionBudget.TryCharge(AUnits: QWord): Boolean;
begin
  if AUnits = 0 then Exit(ReadState >= 0);
  Result := Reserve(AUnits, AUnits) = AUnits;
end;

function TInstructionBudget.Reserve(AMin, AMax: QWord): QWord;
begin
  if AMin = 0 then AMin := 1;
  if AMax < AMin then AMax := AMin;
  EnterCriticalSection(FLock);
  try
    if FState < 0 then Exit(0);
    if FLimit > 0 then
    begin
      if AMin > QWord(FState) then
      begin
        FState := -1;
        Exit(0);
      end;
      Result := QWord(FState) div 8;
      if Result > AMax then Result := AMax;
      if Result < AMin then Result := AMin;
      Dec(FState, Int64(Result));
    end
    else
    begin
      Result := AMax;
      if AMax <= QWord(High(Int64) - FState) then Inc(FState, Int64(AMax)) else FState := High(Int64);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TInstructionBudget.Refund(AUnits: QWord);
begin
  if AUnits = 0 then Exit;
  EnterCriticalSection(FLock);
  try
    if FState < 0 then Exit;
    if FLimit > 0 then
    begin
      if AUnits > QWord(FLimit - FState) then AUnits := QWord(FLimit - FState);
      Inc(FState, Int64(AUnits));
    end
    else if FState = High(Int64) then Exit
    else if AUnits >= QWord(FState) then FState := 0
    else Dec(FState, Int64(AUnits));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function NewInstructionBudget(ALimit: Int64): IGocciaInstructionBudget;
begin
  Result := TInstructionBudget.Create(ALimit);
end;

end.
