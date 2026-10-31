program Goccia.ThreadPolls.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.InstructionLimit,
  Goccia.TestSetup,
  Goccia.ThreadPolls,
  Goccia.Timeout;

type
  TThreadPollsTests = class(TTestSuite)
  private
    procedure TestTimeoutMirrorFollowsEveryDeadlineChange;
    procedure TestInstructionLimitMirrorFollowsEveryActivation;
    procedure TestFlagsShareOneWord;
    procedure TestAnotherThreadHasItsOwnWord;
    procedure TestStatePointersReachTheChecks;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

  TArmingThread = class(TThread)
  public
    UnarmedAtStart: Boolean;
    ArmedInside: Boolean;
    ClearedInside: Boolean;
    procedure Execute; override;
  end;

procedure TArmingThread.Execute;
begin
  UnarmedAtStart := GThreadPolls.Any = 0;
  StartExecutionTimeout(60000);
  StartInstructionLimit(1000);
  ArmedInside := GThreadPolls.TimeoutArmed and
    GThreadPolls.InstructionLimitActive;
  ClearExecutionTimeout;
  ClearInstructionLimit;
  ClearedInside := GThreadPolls.Any = 0;
end;

procedure TThreadPollsTests.BeforeEach;
begin
  inherited BeforeEach;
  ClearExecutionTimeout;
  ClearInstructionLimit;
  GThreadPolls.ProfilingAllocations := False;
end;

procedure TThreadPollsTests.AfterEach;
begin
  ClearExecutionTimeout;
  ClearInstructionLimit;
  GThreadPolls.ProfilingAllocations := False;
  inherited AfterEach;
end;

procedure TThreadPollsTests.SetupTests;
begin
  Test('Timeout flag follows every change of the soonest deadline',
    TestTimeoutMirrorFollowsEveryDeadlineChange);
  Test('Instruction limit flag follows every activation and deactivation',
    TestInstructionLimitMirrorFollowsEveryActivation);
  Test('The three flags are read as one word',
    TestFlagsShareOneWord);
  Test('Arming on another thread leaves this thread unarmed',
    TestAnotherThreadHasItsOwnWord);
  Test('The state pointers beside the flags reach the armed checks',
    TestStatePointersReachTheChecks);
end;

procedure TThreadPollsTests.TestStatePointersReachTheChecks;
var
  Polls: PGocciaThreadPolls;
  RaisedExpected: Boolean;
  I: Integer;
begin
  // Read through a pointer to the record, as the VM does. FPC 3.2.2 at -O2
  // and above on i386 passes the wrong register to the out-of-line slow path
  // when an inlined check takes its state straight from the thread variable
  // (#1699 is the same defect on x86_64).
  Polls := @GThreadPolls;

  // The instruction-limit pointer is the same state the bytecode loop
  // captures, set whenever the flag is.
  StartInstructionLimit(2);
  Expect<Boolean>(Polls^.InstructionLimitActive).ToBe(True);
  Expect<Boolean>(Polls^.InstructionLimitState =
    Pointer(CaptureInstructionLimitState)).ToBe(True);
  IncrementInstructionCounter;
  CheckInstructionLimitState(
    PGocciaInstructionLimitState(Polls^.InstructionLimitState));
  IncrementInstructionCounter;
  RaisedExpected := False;
  try
    CheckInstructionLimitState(
      PGocciaInstructionLimitState(Polls^.InstructionLimitState));
  except
    on TGocciaInstructionLimitError do
      RaisedExpected := True;
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);
  ClearInstructionLimit;

  // A deadline far away: polls through the timeout pointer pass.
  StartExecutionTimeout(60000);
  Expect<Boolean>(Polls^.TimeoutArmed).ToBe(True);
  Expect<Boolean>(Polls^.TimeoutState <> nil).ToBe(True);
  PollExecutionTimeout(PGocciaTimeoutPollState(Polls^.TimeoutState));

  // A short deadline is read on every poll, so the first poll after it
  // passes raises, with the scope and the duration of that deadline.
  PushTimeoutScope(tsTest, 1);
  Sleep(20);
  RaisedExpected := False;
  try
    PollExecutionTimeout(PGocciaTimeoutPollState(Polls^.TimeoutState));
  except
    on E: TGocciaTimeoutError do
      RaisedExpected := (E.Scope = tsTest) and (E.DurationMs = 1);
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);

  // Once that scope ends, the outer deadline is the one polled again.
  PopTimeoutScope;
  PollExecutionTimeout(PGocciaTimeoutPollState(Polls^.TimeoutState));

  // A longer deadline is read once in 1024 polls, counted from the start, the
  // same through the pointer as through CheckExecutionTimeout.
  StartExecutionTimeout(20);
  Sleep(40);
  RaisedExpected := False;
  try
    for I := 1 to 1023 do
      if Odd(I) then
        PollExecutionTimeout(
          PGocciaTimeoutPollState(Polls^.TimeoutState))
      else
        CheckExecutionTimeout;
  except
    on TGocciaTimeoutError do
      RaisedExpected := True;
  end;
  Expect<Boolean>(RaisedExpected).ToBe(False);
  try
    PollExecutionTimeout(PGocciaTimeoutPollState(Polls^.TimeoutState));
  except
    on E: TGocciaTimeoutError do
      RaisedExpected := (E.Scope = tsFile) and (E.DurationMs = 20);
  end;
  Expect<Boolean>(RaisedExpected).ToBe(True);
  ClearExecutionTimeout;
end;

procedure TThreadPollsTests.TestTimeoutMirrorFollowsEveryDeadlineChange;
begin
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);

  StartExecutionTimeout(60000);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
  ClearExecutionTimeout;
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);

  // A duration of 0 starts no deadline.
  StartExecutionTimeout(0);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);

  // A scope without a duration is on the stack and contributes no deadline.
  PushTimeoutScope(tsDescribe, 0);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);
  PushTimeoutScope(tsTest, 60000);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
  PopTimeoutScope;
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);
  PopTimeoutScope;
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);

  // An outer deadline survives the end of an inner one.
  PushTimeoutScope(tsFile, 60000);
  PushTimeoutScope(tsTest, 30000);
  PopTimeoutScope;
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
  PopTimeoutScope;
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);

  // Restarting drops scopes that were still pushed.
  PushTimeoutScope(tsTest, 60000);
  StartExecutionTimeout(0);
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);
  PushTimeoutScope(tsTest, 60000);
  ClearExecutionTimeout;
  Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(False);
  Expect<Integer>(RemainingExecutionTimeoutMilliseconds).ToBe(0);
end;

procedure TThreadPollsTests.TestInstructionLimitMirrorFollowsEveryActivation;

  procedure ExpectMirror(const AExpected: Boolean);
  begin
    Expect<Boolean>(InstructionLimitIsActive).ToBe(AExpected);
    Expect<Boolean>(GThreadPolls.InstructionLimitActive).ToBe(AExpected);
  end;

begin
  ExpectMirror(False);

  StartInstructionLimit(10);
  ExpectMirror(True);
  ClearInstructionLimit;
  ExpectMirror(False);

  StartInstructionLimit(0);
  ExpectMirror(False);

  // A scope activates counting on its own, and its end deactivates it again
  // when no base budget is set.
  PushInstructionLimitScope(5);
  ExpectMirror(True);
  PopInstructionLimitScope;
  ExpectMirror(False);
  // Popping with nothing pushed changes nothing.
  PopInstructionLimitScope;
  ExpectMirror(False);

  // A base budget stays active after a scope inside it ends.
  StartInstructionLimit(10);
  PushInstructionLimitScope(5);
  PopInstructionLimitScope;
  ExpectMirror(True);

  // Restarting without a budget deactivates even with a scope still pushed.
  PushInstructionLimitScope(5);
  StartInstructionLimit(0);
  ExpectMirror(False);
  PushInstructionLimitScope(5);
  ClearInstructionLimit;
  ExpectMirror(False);
end;

procedure TThreadPollsTests.TestFlagsShareOneWord;
begin
  // Every flag sits inside Any: the record is the collector pointer, the two
  // poll-state pointers and one word, so a flag added outside the word would
  // grow it.
  Expect<Integer>(SizeOf(TGocciaThreadPolls))
    .ToBe(3 * SizeOf(Pointer) + SizeOf(UInt32));
  Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);

  StartExecutionTimeout(60000);
  Expect<Boolean>(GThreadPolls.Any <> 0).ToBe(True);
  ClearExecutionTimeout;
  Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);

  StartInstructionLimit(10);
  Expect<Boolean>(GThreadPolls.Any <> 0).ToBe(True);
  ClearInstructionLimit;
  Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);

  GThreadPolls.ProfilingAllocations := True;
  Expect<Boolean>(GThreadPolls.Any <> 0).ToBe(True);
  GThreadPolls.ProfilingAllocations := False;
  Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);
end;

procedure TThreadPollsTests.TestAnotherThreadHasItsOwnWord;
var
  Worker: TArmingThread;
begin
  Worker := TArmingThread.Create(True);
  try
    Worker.Start;
    Worker.WaitFor;
    Expect<Boolean>(Worker.UnarmedAtStart).ToBe(True);
    Expect<Boolean>(Worker.ArmedInside).ToBe(True);
    Expect<Boolean>(Worker.ClearedInside).ToBe(True);
    Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);
  finally
    Worker.Free;
  end;

  // And the other way round: this thread armed, a fresh thread not.
  StartExecutionTimeout(60000);
  Worker := TArmingThread.Create(True);
  try
    Worker.Start;
    Worker.WaitFor;
    Expect<Boolean>(Worker.UnarmedAtStart).ToBe(True);
    Expect<Boolean>(Worker.ClearedInside).ToBe(True);
    Expect<Boolean>(GThreadPolls.TimeoutArmed).ToBe(True);
  finally
    Worker.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TThreadPollsTests.Create('Goccia thread polls'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
