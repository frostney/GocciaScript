program Goccia.RegExp.VM.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.RegExp.&Program,
  Goccia.RegExp.Compiler,
  Goccia.RegExp.VM;

type
  TRegExpVMTests = class(TTestSuite)
  private
    procedure TestShortSubjectGetsFloor;
    procedure TestBudgetGrowsWithSubject;
    procedure TestBudgetPast32BitProduct;
    procedure TestLongestSubject;
    procedure TestMatcherReleasesLargeBacktrackStack;
    procedure TestMatcherMatchesRepeatedly;
    procedure TestMatcherReleasesStackWhenLimitRaises;
    procedure TestOptionalCopiesSkipToTheEnd;
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
  Test('a matcher does not keep a large backtrack stack between matches',
    TestMatcherReleasesLargeBacktrackStack);
  Test('a matcher finds successive matches from the given start index',
    TestMatcherMatchesRepeatedly);
  Test('a matcher releases its buffers after a VM limit',
    TestMatcherReleasesStackWhenLimitRaises);
  Test('each optional copy of a{0,3} skips to the end of the repetition',
    TestOptionalCopiesSkipToTheEnd);
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

procedure TRegExpVMTests.TestMatcherReleasesLargeBacktrackStack;
var
  Matcher: TRegExpMatcher;
  Subject: string;
begin
  // (a|c)* pushes one backtrack entry per "a"; 5,000 of them exceed what a
  // matcher keeps for its next match.
  Subject := StringOfChar('a', 5000) + 'b';
  Matcher := TRegExpMatcher.Create(CompileRegExp('(a|c)*b', 'g'), Subject);
  try
    Expect<Boolean>(Matcher.Exec(0, False)).ToBe(True);
    Expect<Integer>(Matcher.Slot(1)).ToBe(5001);
    Expect<Boolean>(Matcher.RetainedBacktrackCapacity <= 1024).ToBe(True);
  finally
    Matcher.Free;
  end;
end;

procedure TRegExpVMTests.TestMatcherMatchesRepeatedly;
var
  Matcher: TRegExpMatcher;
begin
  Matcher := TRegExpMatcher.Create(CompileRegExp('a(\d)?', 'g'), 'xa1ya');
  try
    Expect<Boolean>(Matcher.Exec(0, False)).ToBe(True);
    Expect<Integer>(Matcher.Slot(0)).ToBe(1);
    Expect<Integer>(Matcher.Slot(3)).ToBe(3);
    Expect<Boolean>(Matcher.Exec(3, False)).ToBe(True);
    Expect<Integer>(Matcher.Slot(0)).ToBe(4);
    Expect<Integer>(Matcher.Slot(2)).ToBe(-1);
    Expect<Boolean>(Matcher.Exec(5, False)).ToBe(False);
    Expect<Boolean>(Matcher.Exec(1, True)).ToBe(True);
    Expect<Boolean>(Matcher.Exec(2, True)).ToBe(False);
  finally
    Matcher.Free;
  end;
end;

procedure TRegExpVMTests.TestMatcherReleasesStackWhenLimitRaises;
var
  Matcher: TRegExpMatcher;
  Raised: Boolean;
begin
  // (a|c)* leaves 2,000 backtrack entries below (b+)+$, whose exponential
  // backtracking on 30 "b"s and a "!" exceeds the step limit.
  Matcher := TRegExpMatcher.Create(CompileRegExp('(a|c)*(b+)+$', 'y'),
    StringOfChar('a', 2000) + StringOfChar('b', 30) + '!');
  try
    Raised := False;
    try
      Matcher.Exec(0, True);
    except
      on ERegExpRuntimeError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Matcher.ReleaseBuffers;
    Expect<Integer>(Matcher.RetainedBacktrackCapacity).ToBe(0);
    // The matcher still works after its buffers were released.
    Expect<Boolean>(Matcher.Exec(2030, False)).ToBe(False);
  finally
    Matcher.Free;
  end;
end;

procedure TRegExpVMTests.TestOptionalCopiesSkipToTheEnd;
var
  Code: TRegExpCodeArray;
  I, LastChar, Splits: Integer;
begin
  // A skipped iteration of a{0,3} must continue after the last copy, not
  // try the remaining copies one by one, which made a failed match walk
  // millions of copies for a{0,5000000}.
  Code := CompileRegExp('a{0,3}', '').Code;
  LastChar := -1;
  for I := 0 to High(Code) do
    if TRegExpOpCode(Code[I] and $FF) = RX_CHAR then
      LastChar := I;
  Splits := 0;
  for I := 0 to High(Code) do
    if TRegExpOpCode(Code[I] and $FF) = RX_SPLIT then
    begin
      Inc(Splits);
      Expect<Integer>(Integer(Code[I] shr 8)).ToBe(LastChar + 1);
    end;
  Expect<Integer>(Splits).ToBe(3);
end;

begin
  TestRunnerProgram.AddSuite(TRegExpVMTests.Create('Goccia.RegExp.VM'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
