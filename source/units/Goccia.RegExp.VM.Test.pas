program Goccia.RegExp.VM.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.RegExp.VM;

type
  TRegExpVMTests = class(TTestSuite)
  private
    procedure TestShortSubjectGetsFloor;
    procedure TestBudgetGrowsWithSubject;
    procedure TestBudgetPast32BitProduct;
    procedure TestLongestSubject;
  public
    procedure SetupTests; override;
  end;

procedure TRegExpVMTests.SetupTests;
begin
  Test('a short subject gets the ten-million-step floor',
    TestShortSubjectGetsFloor);
  Test('the budget is one hundred steps per code unit above the floor',
    TestBudgetGrowsWithSubject);
  Test('a subject of 21,474,837 code units gets its full budget',
    TestBudgetPast32BitProduct);
  Test('the longest subject gets its full budget', TestLongestSubject);
end;

procedure TRegExpVMTests.TestShortSubjectGetsFloor;
begin
  Expect<Int64>(RegExpStepLimit(0)).ToBe(10000000);
  Expect<Int64>(RegExpStepLimit(1)).ToBe(10000000);
  Expect<Int64>(RegExpStepLimit(100000)).ToBe(10000000);
end;

procedure TRegExpVMTests.TestBudgetGrowsWithSubject;
begin
  Expect<Int64>(RegExpStepLimit(100001)).ToBe(10000100);
  Expect<Int64>(RegExpStepLimit(21474836)).ToBe(2147483600);
end;

procedure TRegExpVMTests.TestBudgetPast32BitProduct;
begin
  // 21,474,837 * 100 is the first product above High(Integer); 42,949,673 * 100
  // is the first one above High(Cardinal).
  Expect<Int64>(RegExpStepLimit(21474837)).ToBe(2147483700);
  Expect<Int64>(RegExpStepLimit(42949673)).ToBe(4294967300);
end;

procedure TRegExpVMTests.TestLongestSubject;
begin
  Expect<Int64>(RegExpStepLimit(High(Integer))).ToBe(214748364700);
end;

begin
  TestRunnerProgram.AddSuite(TRegExpVMTests.Create('Goccia.RegExp.VM'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
