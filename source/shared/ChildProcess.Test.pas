program ChildProcess.Test;

{$I Shared.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  BaseUnix,
  {$ENDIF}
  Classes,
  Process,
  SysUtils,

  ChildProcess,
  TestingPascalLibrary,
  TimingUtils;

type
  TChildProcessTests = class(TTestSuite)
  private
    FPath: string;
    procedure TestRunCommandStartsAChildProcess;
    {$IFDEF UNIX}
    procedure TestAChildDoesNotHoldTheParentsLock;
    procedure TestAChildDoesNotHoldAnotherChildsInput;
    procedure TestARunCommandChildDoesNotSeeAParentDescriptor;
    procedure TestOneByOneCloseReachesAboveALoweredLimit;
    {$ENDIF}
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

{$IFDEF UNIX}
const
  // How long a reader may take to see end of file, or the parent to reopen
  // a file, once nothing else holds it.
  DEADLINE_MS = 2500;

type
  TChildProcessClass = class of TChildProcess;

  { Closes descriptors one by one, as on macOS and the BSDs, where there is no
    close_range. }
  TOneByOneChildProcess = class(TChildProcess)
  protected
    function CloseDescriptorsFrom(const AFirst: LongInt): Boolean; override;
  end;

function TOneByOneChildProcess.CloseDescriptorsFrom(
  const AFirst: LongInt): Boolean;
begin
  Result := False;
end;

function StartChild(const AExecutable: string;
  const AArguments: array of string; const AOptions: TProcessOptions;
  const AClass: TChildProcessClass = nil): TChildProcess;
var
  Argument: string;
begin
  if Assigned(AClass) then
    Result := AClass.Create(nil)
  else
    Result := TChildProcess.Create(nil);
  Result.Executable := AExecutable;
  for Argument in AArguments do
    Result.Parameters.Add(Argument);
  Result.Options := AOptions;
  Result.Execute;
end;

{ A cat reading a pipe the parent writes: it stays alive until the parent
  closes its input, so a test controls exactly how long it runs. }
function StartHolder(const AClass: TChildProcessClass = nil): TChildProcess;
begin
  Result := StartChild('/bin/cat', [], [poUsePipes], AClass);
end;

procedure StopHolder(var AHolder: TChildProcess);
begin
  if not Assigned(AHolder) then
    Exit;
  AHolder.CloseInput;
  AHolder.WaitOnExit;
  FreeAndNil(AHolder);
end;

{ Waits until AProcess exits or the deadline passes, and whether it exited. }
function ExitsWithin(const AProcess: TProcess;
  const AMilliseconds: Int64): Boolean;
var
  Started: Int64;
begin
  Started := GetMilliseconds;
  while AProcess.Running and (GetMilliseconds - Started < AMilliseconds) do
    Sleep(5);
  Result := not AProcess.Running;
end;

{ Whether a read of ADescriptor reaches end of file within the deadline. }
function ReachesEndOfFile(const ADescriptor: cint;
  const AMilliseconds: clong): Boolean;
var
  Poll: TPollFd;
  ReadByte: Byte;
begin
  Poll.fd := ADescriptor;
  Poll.events := POLLIN;
  Poll.revents := 0;
  Result := (FpPoll(@Poll, 1, AMilliseconds) = 1) and
    (FpRead(ADescriptor, ReadByte, 1) = 0);
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
  Test('A RunCommand child does not see a descriptor the parent has open',
    TestARunCommandChildDoesNotSeeAParentDescriptor);
  Test('Without close_range, a child closes a descriptor above a lowered limit',
    TestOneByOneCloseReachesAboveALoweredLimit);
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

{ build.pas and FPCSafeSEH start their compilers through RunCommand, which
  creates DefaultTProcess; this is also the check that runs on Windows. }
procedure TChildProcessTests.TestRunCommandStartsAChildProcess;
begin
  Expect<Boolean>(DefaultTProcess = TChildProcess).ToBe(True);
end;

{$IFDEF UNIX}
{ A TFileStream created with fmCreate holds an exclusive flock. A child that
  inherited its descriptor would keep the lock for as long as it runs after
  the parent closes the file, and the parent's exclusive opens would fail
  with EFCreateError. The forked child does hold it from the fork until it
  closes its descriptors just before exec, so the parent retries until the
  deadline, while the holder is still running. }
procedure TChildProcessTests.TestAChildDoesNotHoldTheParentsLock;
const
  RETRY_INTERVAL_MS = 10;
var
  Held: TFileStream;
  Holder: TChildProcess;
  Started: Int64;
  Reopened: Boolean;
begin
  Holder := nil;
  try
    Held := TFileStream.Create(FPath, fmCreate);
    try
      Holder := StartHolder;
    finally
      Held.Free;
    end;
    Started := GetMilliseconds;
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
    until Reopened or (GetMilliseconds - Started > DEADLINE_MS);
    Expect<Boolean>(Reopened).ToBe(True);
    Expect<Boolean>(Holder.Running).ToBe(True);
  finally
    StopHolder(Holder);
  end;
end;

{ cat copies its input until end of file. A child started after it, while the
  parent still holds the write end of cat's input pipe, would hold that end
  too, and cat would see end of file only when that child exits. }
procedure TChildProcessTests.TestAChildDoesNotHoldAnotherChildsInput;
var
  Cat, Holder: TChildProcess;
begin
  Holder := nil;
  Cat := StartChild('/bin/cat', [], [poUsePipes]);
  try
    Holder := StartHolder;
    Cat.CloseInput;
    Expect<Boolean>(ExitsWithin(Cat, DEADLINE_MS)).ToBe(True);
    Expect<Integer>(Cat.ExitStatus).ToBe(0);
    Expect<Boolean>(Holder.Running).ToBe(True);
  finally
    StopHolder(Holder);
    if Cat.Running then
    begin
      Cat.Terminate(1);
      Cat.WaitOnExit;
    end;
    Cat.Free;
  end;
end;

{ A shell can redirect to descriptor n (`true >&n`) exactly while it has n
  open. }
procedure TChildProcessTests.TestARunCommandChildDoesNotSeeAParentDescriptor;
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
      Format('for n in 1 %d; do if { true >&$n; } 2>/dev/null; then ' +
        'echo "$n open"; else echo "$n closed"; fi; done', [Descriptor])],
      Output)).ToBe(True);
  finally
    Held.Free;
  end;
  Expect<string>(Trim(string(Output))).ToBe(Format('1 open' + LineEnding + '%d closed',
    [Descriptor]));
end;

{ Lowering the soft descriptor limit leaves a descriptor opened above it open.
  Without close_range, as on macOS and the BSDs, the child must still close
  it: here the write end of a pipe at descriptor 200, under a limit of 64. }
procedure TChildProcessTests.TestOneByOneCloseReachesAboveALoweredLimit;
const
  HIGH_DESCRIPTOR = 200;
  LOWERED_LIMIT = 64;
var
  Ends: TFilDes;
  Saved, Lowered: TRLimit;
  Holder: TChildProcess;
begin
  Holder := nil;
  Expect<Integer>(FpPipe(Ends)).ToBe(0);
  Expect<Integer>(FpDup2(Ends[1], HIGH_DESCRIPTOR)).ToBe(HIGH_DESCRIPTOR);
  FpClose(Ends[1]);
  Expect<Integer>(FpGetRLimit(RLIMIT_NOFILE, @Saved)).ToBe(0);
  try
    Lowered := Saved;
    Lowered.rlim_cur := LOWERED_LIMIT;
    Expect<Integer>(FpSetRLimit(RLIMIT_NOFILE, @Lowered)).ToBe(0);
    try
      Holder := StartHolder(TOneByOneChildProcess);
    finally
      FpSetRLimit(RLIMIT_NOFILE, @Saved);
    end;
    FpClose(HIGH_DESCRIPTOR);
    Expect<Boolean>(ReachesEndOfFile(Ends[0], DEADLINE_MS)).ToBe(True);
    Expect<Boolean>(Holder.Running).ToBe(True);
  finally
    StopHolder(Holder);
    FpClose(HIGH_DESCRIPTOR);
    FpClose(Ends[0]);
  end;
end;
{$ENDIF}

begin
  Randomize;
  TestRunnerProgram.AddSuite(TChildProcessTests.Create('ChildProcess'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
