unit Goccia.InstructionBudget;

{$mode delphiunicode}{$H+}

interface

type
  { Shared by the engines doing one job. Zero limit measures without capping. Each engine takes units in reserves
    and spends them locally, so the shared counter is touched once per reserve, not once per instruction. }
  IGocciaInstructionBudget = interface
    function Limit: Int64;
    { A capped counter includes the first refused unit and then stays fixed. It includes units engines hold in
      reserve, so it is exact once every engine of the job has ended. }
    function Count: QWord;
    { A nonzero charge succeeds in full or is refused. A zero charge checks whether a cap has been exceeded. }
    function TryCharge(AUnits: QWord): Boolean;
    { At least AMin and at most AMax units, as many as a cap leaves; 0 and the cap spent when fewer than AMin are
      left. AMin is at least 1. }
    function Reserve(AMin, AMax: QWord): QWord;
    { returns reserved units an engine did not spend; nothing once a cap was refused }
    procedure Refund(AUnits: QWord);
  end;

function NewInstructionBudget(ALimit: Int64): IGocciaInstructionBudget;

implementation

uses
  SysUtils;

type
  TInstructionBudget = class(TInterfacedObject, IGocciaInstructionBudget)
  private
    FLimit: Int64;
    { Remaining units for a capped job (-1 after refusal), accumulated units otherwise. }
    FState: Int64;
    function ReadState: Int64; inline;
  public
    constructor Create(ALimit: Int64);
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
end;

function TInstructionBudget.ReadState: Int64;
begin
  Result := InterlockedCompareExchange64(FState, 0, 0);
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
var State, Replacement: Int64;
begin
  if AMin = 0 then AMin := 1;
  if AMax < AMin then AMax := AMin;
  repeat
    State := ReadState;
    if State < 0 then Exit(0);
    if FLimit > 0 then
    begin
      if AMin > QWord(State) then
      begin
        Result := 0;
        Replacement := -1;
      end
      else
      begin
        if AMax <= QWord(State) then Result := AMax else Result := QWord(State);
        Replacement := State - Int64(Result);
      end;
    end
    else
    begin
      Result := AMax;
      if AMax <= QWord(High(Int64) - State) then Replacement := State + Int64(AMax)
      else Replacement := High(Int64);
    end;
  until InterlockedCompareExchange64(FState, Replacement, State) = State;
end;

procedure TInstructionBudget.Refund(AUnits: QWord);
var State, Replacement: Int64;
begin
  if AUnits = 0 then Exit;
  repeat
    State := ReadState;
    if State < 0 then Exit;
    if FLimit > 0 then
    begin
      if AUnits > QWord(FLimit - State) then AUnits := QWord(FLimit - State);
      Replacement := State + Int64(AUnits);
    end
    else if State = High(Int64) then Exit
    else if AUnits >= QWord(State) then Replacement := 0
    else Replacement := State - Int64(AUnits);
  until InterlockedCompareExchange64(FState, Replacement, State) = State;
end;

function NewInstructionBudget(ALimit: Int64): IGocciaInstructionBudget;
begin
  Result := TInstructionBudget.Create(ALimit);
end;

end.
