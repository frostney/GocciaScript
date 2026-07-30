program Goccia.InstructionLimit.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.InstructionLimit,
  Goccia.TestSetup;

type
  TInstructionLimitTests = class(TTestSuite)
  private
    procedure TestCapturedStateTracksLiveBudget;
    procedure TestNestedScopePreservesBaseBudget;
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
end;

procedure TInstructionLimitTests.TestCapturedStateTracksLiveBudget;
var
  State: PGocciaInstructionLimitState;
  RaisedExpected: Boolean;
begin
  State := CaptureInstructionLimitState;

  // A disabled budget remains a no-op through the captured handle.
  PollInstructionLimit(State);

  // Starting after capture updates the same live state. Exactly two polls are
  // accepted for a budget of two; the next poll raises before incrementing.
  StartInstructionLimit(2);
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
  PollInstructionLimit(State);
end;

procedure TInstructionLimitTests.TestNestedScopePreservesBaseBudget;
var
  RaisedExpected: Boolean;
begin
  StartInstructionLimit(3);
  IncrementInstructionCounter;
  CheckInstructionLimit;

  PushInstructionLimitScope(10);
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
end;

begin
  TestRunnerProgram.AddSuite(
    TInstructionLimitTests.Create('Instruction limit'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
