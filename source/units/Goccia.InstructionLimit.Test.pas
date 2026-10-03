program Goccia.InstructionLimit.Test;

{$I Goccia.inc}

uses
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
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
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
