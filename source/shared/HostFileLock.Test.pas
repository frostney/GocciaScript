program HostFileLock.Test;

{$I Shared.inc}

uses
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  BaseUnix,
  UnixType,
  {$ELSEIF DEFINED(MSWINDOWS)}
  Windows,
  {$IFEND}
  SysUtils,

  HostFileLock,
  TestingPascalLibrary;

type
  THostFileLockTests = class(TTestSuite)
  private
    FDirectory: string;
    procedure TestAcquireHoldRelease;
    {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
    procedure TestALinkAtTheLockNameIsRefused;
    procedure TestAFlockErrorIsNotHeld;
    {$ELSEIF DEFINED(MSWINDOWS)}
    procedure TestALockFileExErrorIsNotHeld;
    {$IFEND}
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
{ flock(2) as a filesystem without file locks answers it. }
function UnsupportedFlock(AHandle, AOperation: cint): cint;
begin
  fpseterrno(ESysEINVAL);
  Result := -1;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
{ LockFileEx as a volume without byte-range locks answers it. }
function UnsupportedLockFileEx(AHandle: THandle; AFlags: DWORD;
  var AOverlapped: TOverlapped): BOOL;
begin
  SetLastError(ERROR_NOT_SUPPORTED);
  Result := False;
end;
{$IFEND}

procedure THostFileLockTests.SetupTests;
begin
  Test('A lock is held until it is released', TestAcquireHoldRelease);
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  Test('A symbolic link at the lock name is refused, not followed',
    TestALinkAtTheLockNameIsRefused);
  Test('A flock error other than contention fails at once',
    TestAFlockErrorIsNotHeld);
  {$ELSEIF DEFINED(MSWINDOWS)}
  Test('A LockFileEx error other than contention fails at once',
    TestALockFileExErrorIsNotHeld);
  {$IFEND}
end;

procedure THostFileLockTests.BeforeEach;
begin
  inherited BeforeEach;
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-hostfilelock-' + IntToStr(GetProcessID) + '-' +
    IntToStr(Random(MaxInt));
  ForceDirectories(FDirectory);
end;

procedure THostFileLockTests.AfterEach;
begin
  DeleteFile(FDirectory + PathDelim + 'x.lock');
  DeleteFile(FDirectory + PathDelim + 'link.lock');
  RemoveDir(FDirectory);
  inherited AfterEach;
end;

procedure THostFileLockTests.TestAcquireHoldRelease;
var
  First, Second: THostFileLock;
  Error, LockPath: string;
begin
  LockPath := FDirectory + PathDelim + 'x.lock';
  Expect<Boolean>(TryAcquireHostFileLock(LockPath, &644, First, Error) =
    hflAcquired).ToBe(True);
  try
    Expect<Boolean>(TryAcquireHostFileLock(LockPath, &644, Second, Error) =
      hflHeld).ToBe(True);
  finally
    ReleaseHostFileLock(First);
  end;
  Expect<Boolean>(TryAcquireHostFileLock(LockPath, &644, Second, Error) =
    hflAcquired).ToBe(True);
  ReleaseHostFileLock(Second);
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
procedure THostFileLockTests.TestALinkAtTheLockNameIsRefused;
var
  Lock: THostFileLock;
  Error, LinkPath, Target: string;
begin
  LinkPath := FDirectory + PathDelim + 'link.lock';
  Target := FDirectory + PathDelim + 'target';
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Target)),
    PAnsiChar(AnsiString(LinkPath)))).ToBe(0);
  Expect<Boolean>(TryAcquireHostFileLock(LinkPath, &644, Lock, Error) =
    hflFailed).ToBe(True);
  Expect<Boolean>(Pos('symbolic link', Error) > 0).ToBe(True);
  { The open did not follow the link to create its target. }
  Expect<Boolean>(FileExists(Target)).ToBe(False);
end;

procedure THostFileLockTests.TestAFlockErrorIsNotHeld;
var
  Lock: THostFileLock;
  Error, LockPath: string;
  Outcome: THostFileLockResult;
begin
  LockPath := FDirectory + PathDelim + 'x.lock';
  HostFileLockFlock := UnsupportedFlock;
  try
    Outcome := TryAcquireHostFileLock(LockPath, &644, Lock, Error);
  finally
    HostFileLockFlock := nil;
  end;
  Expect<Boolean>(Outcome = hflFailed).ToBe(True);
  Expect<Boolean>(Pos('cannot lock ' + LockPath, Error) = 1).ToBe(True);
end;
{$ELSEIF DEFINED(MSWINDOWS)}
procedure THostFileLockTests.TestALockFileExErrorIsNotHeld;
var
  Lock: THostFileLock;
  Error, LockPath: string;
  Outcome: THostFileLockResult;
begin
  LockPath := FDirectory + PathDelim + 'x.lock';
  HostFileLockLockFileEx := UnsupportedLockFileEx;
  try
    Outcome := TryAcquireHostFileLock(LockPath, &644, Lock, Error);
  finally
    HostFileLockLockFileEx := nil;
  end;
  Expect<Boolean>(Outcome = hflFailed).ToBe(True);
  Expect<Boolean>(Pos('cannot lock ' + LockPath, Error) = 1).ToBe(True);
end;
{$IFEND}

begin
  Randomize;
  TestRunnerProgram.AddSuite(THostFileLockTests.Create('HostFileLock'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
