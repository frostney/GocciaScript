program ChildProcess.Test;

{$I Shared.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  DateUtils,
  Process,
  SysUtils,

  ChildProcess,
  TestingPascalLibrary;

type
  TChildProcessTests = class(TTestSuite)
  private
    FPath: string;
    procedure TestRunCommandStartsAChildProcess;
    {$IFDEF UNIX}
    procedure TestAChildDoesNotHoldTheParentsLock;
    procedure TestAChildDoesNotHoldAnotherChildsInput;
    procedure TestARunCommandChildHasOnlyStandardDescriptors;
    {$ENDIF}
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

{$IFDEF UNIX}
function StartChild(const AExecutable: string;
  const AArguments: array of string;
  const AOptions: TProcessOptions): TChildProcess;
var
  Argument: string;
begin
  Result := TChildProcess.Create(nil);
  Result.Executable := AExecutable;
  for Argument in AArguments do
    Result.Parameters.Add(Argument);
  Result.Options := AOptions;
  Result.Execute;
end;
{$ENDIF}

procedure TChildProcessTests.SetupTests;
begin
  Test('RunCommand starts its process as a TChildProcess',
    TestRunCommandStartsAChildProcess);
  {$IFDEF UNIX}
  Test('A child does not hold the flock of a file the parent had open',
    TestAChildDoesNotHoldTheParentsLock);
  Test('A child does not hold the write end of another child''s input',
    TestAChildDoesNotHoldAnotherChildsInput);
  Test('A RunCommand child has only its standard descriptors open',
    TestARunCommandChildHasOnlyStandardDescriptors);
  {$ENDIF}
end;

procedure TChildProcessTests.BeforeEach;
begin
  FPath := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('goccia-child-process-%d-%d.lock', [GetProcessID, Random(MaxInt)]);
end;

procedure TChildProcessTests.AfterEach;
begin
  DeleteFile(FPath);
end;

procedure TChildProcessTests.TestRunCommandStartsAChildProcess;
begin
  Expect<Boolean>(DefaultTProcess = TChildProcess).ToBe(True);
end;

{$IFDEF UNIX}
{ A TFileStream created with fmCreate holds an exclusive flock. A child that
  inherited its descriptor would keep the lock for as long as it runs after
  the parent closes the file, and the parent's exclusive opens would fail with
  EAGAIN. The forked child does hold it from the fork until it closes its
  descriptors just before exec, so the parent retries for up to a second,
  well short of the sleeper's five. }
procedure TChildProcessTests.TestAChildDoesNotHoldTheParentsLock;
const
  RETRY_LIMIT_MS = 1000;
  RETRY_INTERVAL_MS = 10;
var
  Held: TFileStream;
  Sleeper: TChildProcess;
  Started: TDateTime;
  Reopened: Boolean;
begin
  Sleeper := nil;
  Held := TFileStream.Create(FPath, fmCreate);
  try
    try
      Sleeper := StartChild('/bin/sleep', ['5'], []);
    finally
      Held.Free;
    end;
    Started := Now;
    repeat
      try
        TFileStream.Create(FPath, fmCreate).Free;
        Reopened := True;
      except
        on EFCreateError do
        begin
          Reopened := False;
          Sleep(RETRY_INTERVAL_MS);
        end;
      end;
    until Reopened or (MilliSecondsBetween(Now, Started) > RETRY_LIMIT_MS);
    Expect<Boolean>(Reopened).ToBe(True);
  finally
    if Assigned(Sleeper) then
    begin
      Sleeper.Terminate(0);
      Sleeper.WaitOnExit;
      Sleeper.Free;
    end;
  end;
end;

{ cat copies its input until end of file. A child started after it, while the
  parent still holds the write end of cat's input pipe, would hold that end
  too, and cat would see end of file only when that child exits. }
procedure TChildProcessTests.TestAChildDoesNotHoldAnotherChildsInput;
var
  Cat, Sleeper: TChildProcess;
  Closed: TDateTime;
  Elapsed: Int64;
begin
  Cat := StartChild('/bin/cat', [], [poUsePipes]);
  Sleeper := nil;
  try
    Sleeper := StartChild('/bin/sleep', ['5'], []);
    Closed := Now;
    Cat.CloseInput;
    Cat.WaitOnExit;
    Elapsed := MilliSecondsBetween(Now, Closed);
    Expect<Boolean>(Elapsed < 2500).ToBe(True);
  finally
    if Assigned(Sleeper) then
    begin
      Sleeper.Terminate(0);
      Sleeper.WaitOnExit;
      Sleeper.Free;
    end;
    Cat.Free;
  end;
end;

{ /dev/fd/<n> exists in a process exactly while its descriptor n is open. }
procedure TChildProcessTests.TestARunCommandChildHasOnlyStandardDescriptors;
var
  Held: TFileStream;
  Descriptor: THandle;
  Output: AnsiString;
begin
  Held := TFileStream.Create(FPath, fmCreate);
  try
    Descriptor := Held.Handle;
    Expect<Boolean>(Descriptor > 2).ToBe(True);
    Expect<Boolean>(RunCommand('/bin/sh', ['-c',
      Format('for n in 1 %d; do if [ -e /dev/fd/$n ]; then echo "$n open"; ' +
        'else echo "$n closed"; fi; done', [Descriptor])], Output)).ToBe(True);
  finally
    Held.Free;
  end;
  Expect<string>(Trim(string(Output))).ToBe(Format('1 open' + LineEnding + '%d closed',
    [Descriptor]));
end;
{$ENDIF}

begin
  Randomize;
  TestRunnerProgram.AddSuite(TChildProcessTests.Create('ChildProcess'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
