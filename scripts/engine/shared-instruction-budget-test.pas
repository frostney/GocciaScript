program SharedInstructionBudgetTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Goccia.Core, Goccia.Embed, Goccia.InstructionBudget, Goccia.VM;

type
  TCharger = class(TThread)
  private
    FBudget: IGocciaInstructionBudget;
    FCharged: QWord;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABudget: IGocciaInstructionBudget);
    property Charged: QWord read FCharged;
  end;

constructor TCharger.Create(const ABudget: IGocciaInstructionBudget);
begin
  inherited Create(True);
  FBudget := ABudget;
end;

procedure TCharger.Execute;
var I: Integer;
begin
  for I := 1 to 4000 do
  begin
    if not FBudget.TryCharge(3) then Exit;
    Inc(FCharged, 3);
  end;
end;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckConcurrentCharges(ALimit: Int64; AAccepted, ACount: QWord);
var
  Budget: IGocciaInstructionBudget;
  Chargers: array[0..3] of TCharger;
  I: Integer;
  Accepted: QWord;
begin
  Budget := NewInstructionBudget(ALimit);
  for I := Low(Chargers) to High(Chargers) do Chargers[I] := TCharger.Create(Budget);
  try
    for I := Low(Chargers) to High(Chargers) do Chargers[I].Start;
    Accepted := 0;
    for I := Low(Chargers) to High(Chargers) do
    begin
      Chargers[I].WaitFor;
      Check(Chargers[I].FatalException = nil, 'charge thread failed');
      Inc(Accepted, Chargers[I].Charged);
    end;
    Check((Accepted = AAccepted) and (Budget.Count = ACount), 'threads did not share one instruction counter');
  finally
    for I := Low(Chargers) to High(Chargers) do Chargers[I].Free;
  end;
end;

const
  { several thousand units, so two engines that share a cap both take several reserves }
  Entry = 'console.log("started"); const values = new Array(4096).fill(7); values[0];';

type
  { counts the entries that started, from any thread }
  TStartCounter = class
  public
    Started: Integer;
    procedure Console(const AMethod, ALine: UnicodeString);
  end;

procedure TStartCounter.Console(const AMethod, ALine: UnicodeString);
begin
  if ALine = 'started' then InterLockedIncrement(Started);
end;

var
  Starts: TStartCounter;

function RunEntry(const ABudget: IGocciaInstructionBudget): TGocciaRunResult;
var
  Engine: TGocciaEngine;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.OnConsole := Starts.Console;
    Engine.InstructionBudget := ABudget;
    Engine.MeasureInstructions := True;
    Engine.ResultForm := grfString;
    Result := Engine.Run('shared.mjs', Entry);
  finally
    Engine.Free;
  end;
end;

type
  TEntryThread = class(TThread)
  private
    FBudget: IGocciaInstructionBudget;
  protected
    procedure Execute; override;
  public
    RunResult: TGocciaRunResult;
    constructor Create(const ABudget: IGocciaInstructionBudget);
  end;

constructor TEntryThread.Create(const ABudget: IGocciaInstructionBudget);
begin
  inherited Create(True);
  FBudget := ABudget;
end;

procedure TEntryThread.Execute;
begin
  RunResult := RunEntry(FBudget);
end;

{ Two engines on two threads share one budget: uncapped (ALimit 0), capped with room for both, or capped below what
  both need, when both start, one is refused and the count is fixed past the cap. }
procedure CheckSharedEntries(ASharedLimit: Int64; AExpectedCount: QWord; ARefused: Boolean);
var
  Shared: IGocciaInstructionBudget;
  First, Second: TEntryThread;
begin
  Starts.Started := 0;
  Shared := NewInstructionBudget(ASharedLimit);
  First := TEntryThread.Create(Shared);
  Second := TEntryThread.Create(Shared);
  try
    First.Start;
    Second.Start;
    First.WaitFor;
    Second.WaitFor;
    Check((First.FatalException = nil) and (Second.FatalException = nil), 'a shared entry thread failed');
    Check(Shared.Count = AExpectedCount, 'entries did not charge the same budget: ' + IntToStr(Shared.Count));
    Check(Starts.Started = 2, 'an engine was refused before it did any work');
    { running at once, both can be refused near the cap; at least one is }
    if ARefused then
      Check((First.RunResult.Aborted = gaInstructionLimit) or (Second.RunResult.Aborted = gaInstructionLimit),
        'neither entry observed the spent shared limit')
    else
      Check(First.RunResult.Ok and (First.RunResult.ResultText = '7') and
        Second.RunResult.Ok and (Second.RunResult.ResultText = '7'), 'shared measurement changed an entry');
  finally
    Second.Free;
    First.Free;
  end;
end;

function NativeWork(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  Result := PollInstructionUnits(1000);
  AResult := MkInt(42);
end;

procedure CheckNativeCharges;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  ErrorText: UnicodeString;
  Shared: IGocciaInstructionBudget;
begin
  Engine := TGocciaEngine.Create;
  try
    Check(Engine.PrepareHost(ErrorText), ErrorText);
    Check(InjectGlobalBinding('nativeWork', NewNativeFunction(@NativeWork, 'nativeWork', 0)), 'could not install native test work');
    Shared := NewInstructionBudget(500);
    Engine.InstructionBudget := Shared;
    RunResult := Engine.Run('native.mjs', 'nativeWork();');
    Check((RunResult.Aborted = gaInstructionLimit) and (Shared.Count = 501) and
      RunResult.HasInstructionCount and (RunResult.InstructionCount = 501), 'native work did not charge the shared cap');
  finally
    Engine.Free;
  end;
end;

{ Reserves grant what a cap leaves, refunds return unspent units until a cap is refused, and an uncapped budget
  counts what was reserved and not returned. }
procedure CheckReserves;
var
  Budget: IGocciaInstructionBudget;
begin
  Budget := NewInstructionBudget(100);
  Check(Budget.Reserve(1, 1024) = 12, 'a reserve took more than an eighth of what the cap left');
  Check(Budget.Reserve(30, 1024) = 30, 'a reserve below its minimum was not raised to it');
  Check(Budget.Count = 42, 'reserved units were not counted');
  Budget.Refund(40);
  Check(Budget.Count = 2, 'a refund did not return unspent units');
  Check(Budget.Reserve(99, 99) = 0, 'a reserve larger than the remainder was granted');
  Check(Budget.Count = 101, 'a refused reserve did not fix the count past the cap');
  Budget.Refund(5);
  Check((Budget.Count = 101) and not Budget.TryCharge(0), 'a refund after refusal changed the count');
  Budget := NewInstructionBudget(0);
  Check(Budget.Reserve(1, 1024) = 1024, 'an uncapped reserve was not granted in full');
  Budget.Refund(1000);
  Check(Budget.Count = 24, 'an uncapped refund did not return unspent units');
end;

{ An engine's reported count leaves out the units it still holds in reserve. }
procedure CheckReportedCount(const AExpected: QWord);
var
  Shared: IGocciaInstructionBudget;
  RunResult: TGocciaRunResult;
begin
  Shared := NewInstructionBudget(0);
  RunResult := RunEntry(Shared);
  Check(RunResult.Ok and RunResult.HasInstructionCount and (RunResult.InstructionCount = AExpected) and
    (Shared.Count = AExpected), 'a shared run reported its reserve as work');
end;

var
  Budget: IGocciaInstructionBudget;
  OneEntry: TGocciaRunResult;
begin
  Starts := TStartCounter.Create;
  CheckReserves;
  CheckConcurrentCharges(12000, 12000, 12001);
  CheckConcurrentCharges(0, 48000, 48000);
  Budget := NewInstructionBudget(High(Int64));
  Check(Budget.TryCharge(QWord(High(Int64))) and Budget.TryCharge(0), 'an exact-limit charge did not succeed');
  Check(not Budget.TryCharge(High(QWord)), 'a charge larger than the signed range was accepted');
  Check(not Budget.TryCharge(0) and not Budget.TryCharge(1) and (Budget.Count = QWord(High(Int64)) + 1),
    'the first refused charge did not stay counted');
  Budget := NewInstructionBudget(0);
  Check(Budget.TryCharge(High(QWord)) and (Budget.Count = QWord(High(Int64))), 'uncapped count did not saturate');
  OneEntry := RunEntry(nil);
  Check(OneEntry.Ok and OneEntry.HasInstructionCount and (OneEntry.InstructionCount >= 64), 'single entry count unavailable');
  CheckReportedCount(OneEntry.InstructionCount);
  CheckSharedEntries(0, 2 * OneEntry.InstructionCount, False);
  CheckSharedEntries(Int64(2 * OneEntry.InstructionCount) + 3 * 1024, 2 * OneEntry.InstructionCount, False);
  CheckSharedEntries(Int64(OneEntry.InstructionCount + OneEntry.InstructionCount div 2),
    OneEntry.InstructionCount + OneEntry.InstructionCount div 2 + 1, True);
  CheckNativeCharges;
  WriteLn('Shared instruction budget: passed');
end.
