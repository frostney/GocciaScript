program Goccia.InstructionLimit.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.InstructionLimit,
  Goccia.NativeLimits,
  Goccia.TestSetup,
  Goccia.Timeout,
  Goccia.Values.ObjectValue;

type
  TInstructionLimitTests = class(TTestSuite)
  private
    procedure TestCapturedStateTracksLiveBudget;
    procedure TestNativeDepthRollsBackWhenBudgetCheckRaises;
    procedure TestNestedScopePreservesBaseBudget;
    procedure TestAllocationChecksAnExhaustedBudget;
    procedure TestEarliestBudgetTripsFirst;
    procedure TestScopeWithoutBudgetNeverTrips;
    procedure TestFreshThreadChecksNothing;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

  TFreshThreadCheck = class(TThread)
  public
    Raised: Boolean;
    procedure Execute; override;
  end;

procedure TFreshThreadCheck.Execute;
begin
  // A thread that never started or cleared a budget: its state is all zero.
  Raised := False;
  try
    CheckInstructionLimit;
    CheckInstructionLimit;
    PollInstructionLimit(CaptureInstructionLimitState);
  except
    on TGocciaInstructionLimitError do
      Raised := True;
  end;
end;

{ Polls through the captured handle until one raises, and returns how many
  polls passed before it and the message it raised with. }
function PollsBeforeLimit(const AState: PGocciaInstructionLimitState;
  const AMaxPolls: Integer; out AMessage: string): Integer;
begin
  Result := 0;
  AMessage := '';
  try
    while Result < AMaxPolls do
    begin
      PollInstructionLimit(AState);
      Inc(Result);
    end;
  except
    on E: TGocciaInstructionLimitError do
      AMessage := E.Message;
  end;
end;

procedure TInstructionLimitTests.BeforeEach;
begin
  inherited BeforeEach;
  ClearInstructionLimit;
end;

procedure TInstructionLimitTests.AfterEach;
begin
  ClearInstructionLimit;
  inherited AfterEach;
end;

procedure TInstructionLimitTests.SetupTests;
begin
  Test('Captured state tracks the live thread budget',
    TestCapturedStateTracksLiveBudget);
  Test('Nested scope preserves the base thread budget',
    TestNestedScopePreservesBaseBudget);
  Test('Native depth rolls back when a budget check raises',
    TestNativeDepthRollsBackWhenBudgetCheckRaises);
  Test('A value allocation checks an exhausted budget',
    TestAllocationChecksAnExhaustedBudget);
  Test('The budget spent first trips first, at its exact count',
    TestEarliestBudgetTripsFirst);
  Test('A scope without a budget counts and never trips',
    TestScopeWithoutBudgetNeverTrips);
  Test('A thread that never set a budget checks nothing',
    TestFreshThreadChecksNothing);
end;

procedure TInstructionLimitTests.TestEarliestBudgetTripsFirst;
var
  State: PGocciaInstructionLimitState;
  Message: string;
begin
  State := CaptureInstructionLimitState;

  // A scope inside a base budget, ending before it: the scope trips first,
  // after exactly its own number of polls.
  StartInstructionLimit(10);
  Expect<Integer>(PollsBeforeLimit(State, 4, Message)).ToBe(4);
  PushInstructionLimitScope(3);
  try
    Expect<Integer>(PollsBeforeLimit(State, 100, Message)).ToBe(3);
    Expect<string>(Message).ToBe('Execution exceeded instruction limit of 3');
  finally
    PopInstructionLimitScope;
  end;
  // Back in the base budget, which has 3 polls left.
  Expect<Integer>(PollsBeforeLimit(State, 100, Message)).ToBe(3);
  Expect<string>(Message).ToBe('Execution exceeded instruction limit of 10');

  // A scope that would end after the base budget: the base trips first.
  StartInstructionLimit(5);
  Expect<Integer>(PollsBeforeLimit(State, 2, Message)).ToBe(2);
  PushInstructionLimitScope(100);
  try
    Expect<Integer>(PollsBeforeLimit(State, 1000, Message)).ToBe(3);
    Expect<string>(Message).ToBe('Execution exceeded instruction limit of 5');
  finally
    PopInstructionLimitScope;
  end;

  // Nested scopes without a base budget: the inner one trips first, and the
  // outer one still trips at its own count once the inner one ends.
  ClearInstructionLimit;
  PushInstructionLimitScope(8);
  try
    Expect<Integer>(PollsBeforeLimit(State, 1, Message)).ToBe(1);
    PushInstructionLimitScope(2);
    try
      Expect<Integer>(PollsBeforeLimit(State, 100, Message)).ToBe(2);
      Expect<string>(Message).ToBe('Execution exceeded instruction limit of 2');
    finally
      PopInstructionLimitScope;
    end;
    Expect<Integer>(PollsBeforeLimit(State, 100, Message)).ToBe(5);
    Expect<string>(Message).ToBe('Execution exceeded instruction limit of 8');
  finally
    PopInstructionLimitScope;
  end;

  // A scope so large that its end does not fit in the counter never trips.
  StartInstructionLimit(High(Int64));
  IncrementInstructionCounter;
  PushInstructionLimitScope(High(Int64));
  try
    Expect<Integer>(PollsBeforeLimit(State, 1000, Message)).ToBe(1000);
    CheckInstructionLimit;
  finally
    PopInstructionLimitScope;
  end;
end;

procedure TInstructionLimitTests.TestScopeWithoutBudgetNeverTrips;
var
  State: PGocciaInstructionLimitState;
  Message: string;
begin
  // A host may push a scope for a run whatever its budget, 0 included: the
  // scope makes the state active, so the bytecode loop counts, but nothing
  // trips.
  State := CaptureInstructionLimitState;
  PushInstructionLimitScope(0);
  try
    Expect<Boolean>(InstructionLimitIsActive).ToBe(True);
    Expect<Integer>(PollsBeforeLimit(State, 10000, Message)).ToBe(10000);
    CheckInstructionLimit;

    // A budgeted scope inside it starts counting from where it is pushed.
    PushInstructionLimitScope(4);
    try
      Expect<Integer>(PollsBeforeLimit(State, 100, Message)).ToBe(4);
    finally
      PopInstructionLimitScope;
    end;
    Expect<Integer>(PollsBeforeLimit(State, 10000, Message)).ToBe(10000);
  finally
    PopInstructionLimitScope;
  end;
end;

procedure TInstructionLimitTests.TestFreshThreadChecksNothing;
var
  Worker: TFreshThreadCheck;
begin
  Worker := TFreshThreadCheck.Create(True);
  try
    Worker.Start;
    Worker.WaitFor;
    Expect<Boolean>(Worker.Raised).ToBe(False);
  finally
    Worker.Free;
  end;
end;

procedure TInstructionLimitTests.TestAllocationChecksAnExhaustedBudget;
var
  Value: TGocciaObjectValue;
  RaisedExpected: Boolean;
begin
  // With no budget, and nothing else armed on this thread, an allocation
  // checks nothing and succeeds.
  ClearExecutionTimeout;
  Value := TGocciaObjectValue.Create;
  Value.Free;

  // A budget of one step that is already spent: what the bytecode loop leaves
  // behind while it runs the last instruction it is allowed to run. A native
  // call made by that instruction must stop at its first allocation, and the
  // instruction limit is the only thing armed, so the allocation hook has to
  // look at it on its own account.
  StartInstructionLimit(1);
  IncrementInstructionCounter;
  RaisedExpected := False;
  try
    TGocciaObjectValue.Create;
  except
    on TGocciaInstructionLimitError do
      RaisedExpected := True;
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);

  // The same under a scope without a base budget.
  ClearInstructionLimit;
  PushInstructionLimitScope(1);
  try
    IncrementInstructionCounter;
    RaisedExpected := False;
    try
      TGocciaObjectValue.Create;
    except
      on TGocciaInstructionLimitError do
        RaisedExpected := True;
    end;
    Expect<Boolean>(RaisedExpected).ToBe(True);
  finally
    PopInstructionLimitScope;
  end;

  // Cleared again, allocation is unchecked again.
  ClearInstructionLimit;
  Value := TGocciaObjectValue.Create;
  Value.Free;
end;

procedure TInstructionLimitTests.TestCapturedStateTracksLiveBudget;
var
  State: PGocciaInstructionLimitState;
  RaisedExpected: Boolean;
begin
  State := CaptureInstructionLimitState;

  // A disabled budget remains a no-op through the captured handle.
  Expect<Boolean>(InstructionLimitIsActive).ToBe(False);
  PollInstructionLimit(State);

  // Starting after capture updates the same live state. Exactly two polls are
  // accepted for a budget of two; the next poll raises before incrementing.
  StartInstructionLimit(2);
  Expect<Boolean>(InstructionLimitIsActive).ToBe(True);
  PollInstructionLimit(State);
  PollInstructionLimit(State);
  RaisedExpected := False;
  try
    PollInstructionLimit(State);
  except
    on TGocciaInstructionLimitError do
      RaisedExpected := True;
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);

  // Clearing after capture must disable the same handle immediately.
  ClearInstructionLimit;
  Expect<Boolean>(InstructionLimitIsActive).ToBe(False);
  PollInstructionLimit(State);
end;

procedure TInstructionLimitTests.TestNestedScopePreservesBaseBudget;
var
  RaisedExpected: Boolean;
begin
  StartInstructionLimit(4);
  IncrementInstructionCounter;
  CheckInstructionLimit;

  PushInstructionLimitScope(2);
  try
    IncrementInstructionCounter;
    CheckInstructionLimit;
    IncrementInstructionCounter;
    RaisedExpected := False;
    try
      CheckInstructionLimit;
    except
      on TGocciaInstructionLimitError do
        RaisedExpected := True;
    end;
    Expect<Boolean>(RaisedExpected).ToBe(True);
  finally
    PopInstructionLimitScope;
  end;

  IncrementInstructionCounter;
  RaisedExpected := False;
  try
    CheckInstructionLimit;
  except
    on TGocciaInstructionLimitError do
      RaisedExpected := True;
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);
end;

procedure TInstructionLimitTests.TestNativeDepthRollsBackWhenBudgetCheckRaises;
var
  EnteredDepth: Integer;
  I: Integer;
  RaisedExpected: Boolean;
begin
  StartInstructionLimit(1);
  IncrementInstructionCounter;
  RaisedExpected := False;
  try
    EnterNativeDataDepth('instruction-limit test');
  except
    on TGocciaInstructionLimitError do
      RaisedExpected := True;
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);

  ClearInstructionLimit;
  EnteredDepth := 0;
  try
    for I := 1 to MAX_NATIVE_DATA_DEPTH do
    begin
      EnterNativeDataDepth('instruction-limit test');
      Inc(EnteredDepth);
    end;
    Expect<Integer>(EnteredDepth).ToBe(MAX_NATIVE_DATA_DEPTH);
  finally
    while EnteredDepth > 0 do
    begin
      LeaveNativeDataDepth;
      Dec(EnteredDepth);
    end;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TInstructionLimitTests.Create('Instruction limit'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
