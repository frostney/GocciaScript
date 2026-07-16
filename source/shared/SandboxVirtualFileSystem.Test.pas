program SandboxVirtualFileSystemTest;

{$I Goccia.inc}

uses
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,
  TimingUtils,

  Goccia.TestSetup;

type
  TDeterministicSandboxClock = class(TSandboxFsClock)
  private
    FCallCount: Integer;
    FValue: TDateTime;
  public
    function Now: TDateTime; override;
    property CallCount: Integer read FCallCount;
    property Value: TDateTime read FValue write FValue;
  end;

  TTestSandboxVirtualFileSystem = class(TTestSuite)
  private
    FClock: TDeterministicSandboxClock;
    FFs: TSandboxVirtualFileSystem;
    function TimeAt(const AMillisecond: Integer): TDateTime;
    procedure ExpectTime(const AActual, AExpected: TDateTime);
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;

    procedure TestTracksIndependentFileTimestamps;
    procedure TestCopyRenameAndForkMetadata;
    procedure TestForkDoesNotReadClock;
    procedure TestSnapshotReadsDoNotAdvanceAccessTime;
    procedure TestDirectoryEntryChangesUpdateParentMetadata;
    procedure TestRecursiveMkdirReportsFirstCreatedPath;
    procedure TestDefaultClockUsesUnixEpoch;
  end;

function TDeterministicSandboxClock.Now: TDateTime;
begin
  Inc(FCallCount);
  Result := FValue;
end;

procedure TTestSandboxVirtualFileSystem.SetupTests;
begin
  Test('Tracks independent file timestamps',
    TestTracksIndependentFileTimestamps);
  Test('Copy, rename, and fork preserve their timestamp contracts',
    TestCopyRenameAndForkMetadata);
  Test('Fork does not read the shared clock', TestForkDoesNotReadClock);
  Test('Snapshot reads do not advance access time',
    TestSnapshotReadsDoNotAdvanceAccessTime);
  Test('Directory entry changes update parent metadata',
    TestDirectoryEntryChangesUpdateParentMetadata);
  Test('Recursive mkdir reports its first created path',
    TestRecursiveMkdirReportsFirstCreatedPath);
  Test('Default clock tracks the Unix epoch', TestDefaultClockUsesUnixEpoch);
end;

procedure TTestSandboxVirtualFileSystem.BeforeEach;
begin
  FClock := TDeterministicSandboxClock.Create;
  FClock.Value := TimeAt(0);
  FFs := TSandboxVirtualFileSystem.Create(0, FClock);
end;

procedure TTestSandboxVirtualFileSystem.AfterEach;
begin
  FFs.Free;
  FClock.Free;
end;

function TTestSandboxVirtualFileSystem.TimeAt(
  const AMillisecond: Integer): TDateTime;
begin
  Result := EncodeDate(2026, 1, 1) +
    (AMillisecond / (24 * 60 * 60 * 1000));
end;

procedure TTestSandboxVirtualFileSystem.ExpectTime(const AActual,
  AExpected: TDateTime);
begin
  Expect<Boolean>(AActual = AExpected).ToBe(True);
end;

procedure TTestSandboxVirtualFileSystem.TestTracksIndependentFileTimestamps;
var
  CreatedStat: TSandboxFsStat;
  ReadStat: TSandboxFsStat;
  WrittenStat: TSandboxFsStat;
begin
  FClock.Value := TimeAt(10);
  FFs.WriteAllText('/file.txt', 'first');
  CreatedStat := FFs.Stat('/file.txt');
  ExpectTime(CreatedStat.AccessedAt, TimeAt(10));
  ExpectTime(CreatedStat.ModifiedAt, TimeAt(10));
  ExpectTime(CreatedStat.ChangedAt, TimeAt(10));
  ExpectTime(CreatedStat.BirthTime, TimeAt(10));

  FClock.Value := TimeAt(20);
  Expect<string>(FFs.ReadAllText('/file.txt')).ToBe('first');
  ReadStat := FFs.Stat('/file.txt');
  ExpectTime(ReadStat.AccessedAt, TimeAt(20));
  ExpectTime(ReadStat.ModifiedAt, TimeAt(10));
  ExpectTime(ReadStat.ChangedAt, TimeAt(10));
  ExpectTime(ReadStat.BirthTime, TimeAt(10));

  FClock.Value := TimeAt(30);
  FFs.WriteAllText('/file.txt', 'second');
  WrittenStat := FFs.Stat('/file.txt');
  ExpectTime(WrittenStat.AccessedAt, TimeAt(20));
  ExpectTime(WrittenStat.ModifiedAt, TimeAt(30));
  ExpectTime(WrittenStat.ChangedAt, TimeAt(30));
  ExpectTime(WrittenStat.BirthTime, TimeAt(10));

  FClock.Value := TimeAt(40);
  WrittenStat := FFs.Stat('/file.txt');
  ExpectTime(WrittenStat.AccessedAt, TimeAt(20));
  ExpectTime(WrittenStat.ModifiedAt, TimeAt(30));
  ExpectTime(WrittenStat.ChangedAt, TimeAt(30));
  ExpectTime(WrittenStat.BirthTime, TimeAt(10));
end;

procedure TTestSandboxVirtualFileSystem.TestCopyRenameAndForkMetadata;
var
  SourceBefore: TSandboxFsStat;
  SourceAfterCopy: TSandboxFsStat;
  CopyStat: TSandboxFsStat;
  RenamedStat: TSandboxFsStat;
  ForkedStat: TSandboxFsStat;
  Forked: TSandboxVirtualFileSystem;
begin
  FClock.Value := TimeAt(10);
  FFs.WriteAllText('/source.txt', 'source');
  FClock.Value := TimeAt(20);
  FFs.ReadAllText('/source.txt');
  SourceBefore := FFs.Stat('/source.txt');

  Forked := FFs.Fork;
  try
    ForkedStat := Forked.Stat('/source.txt');
    ExpectTime(ForkedStat.AccessedAt, SourceBefore.AccessedAt);
    ExpectTime(ForkedStat.ModifiedAt, SourceBefore.ModifiedAt);
    ExpectTime(ForkedStat.ChangedAt, SourceBefore.ChangedAt);
    ExpectTime(ForkedStat.BirthTime, SourceBefore.BirthTime);
  finally
    Forked.Free;
  end;

  FClock.Value := TimeAt(30);
  FFs.CopyPath('/source.txt', '/copy.txt');
  SourceAfterCopy := FFs.Stat('/source.txt');
  CopyStat := FFs.Stat('/copy.txt');
  ExpectTime(SourceAfterCopy.AccessedAt, TimeAt(30));
  ExpectTime(SourceAfterCopy.ModifiedAt, SourceBefore.ModifiedAt);
  ExpectTime(SourceAfterCopy.BirthTime, SourceBefore.BirthTime);
  ExpectTime(CopyStat.AccessedAt, TimeAt(30));
  ExpectTime(CopyStat.ModifiedAt, TimeAt(30));
  ExpectTime(CopyStat.ChangedAt, TimeAt(30));
  ExpectTime(CopyStat.BirthTime, TimeAt(30));

  FFs.CopyPath('/copy.txt', '/copy.txt');
  Expect<string>(FFs.ReadAllText('/copy.txt')).ToBe('source');

  FClock.Value := TimeAt(40);
  FFs.MovePath('/source.txt', '/renamed.txt');
  RenamedStat := FFs.Stat('/renamed.txt');
  ExpectTime(RenamedStat.AccessedAt, TimeAt(30));
  ExpectTime(RenamedStat.ModifiedAt, SourceBefore.ModifiedAt);
  ExpectTime(RenamedStat.ChangedAt, TimeAt(40));
  ExpectTime(RenamedStat.BirthTime, SourceBefore.BirthTime);
end;

procedure TTestSandboxVirtualFileSystem.TestForkDoesNotReadClock;
var
  CallsBeforeFork: Integer;
  Forked: TSandboxVirtualFileSystem;
begin
  FClock.Value := TimeAt(10);
  FFs.WriteAllText('/source.txt', 'source');
  CallsBeforeFork := FClock.CallCount;

  Forked := FFs.Fork;
  try
    Expect<Integer>(FClock.CallCount).ToBe(CallsBeforeFork);
  finally
    Forked.Free;
  end;
end;

procedure TTestSandboxVirtualFileSystem.TestSnapshotReadsDoNotAdvanceAccessTime;
var
  BeforeStat: TSandboxFsStat;
  AfterStat: TSandboxFsStat;
begin
  FClock.Value := TimeAt(10);
  FFs.WriteAllText('/file.txt', 'content');
  BeforeStat := FFs.Stat('/file.txt');

  FClock.Value := TimeAt(20);
  Expect<string>(FFs.SnapshotReadAllText('/file.txt')).ToBe('content');
  Expect<Integer>(Length(FFs.SnapshotReadAllBytes('/file.txt'))).ToBe(7);
  Expect<Integer>(Length(FFs.SnapshotList('/'))).ToBe(1);
  AfterStat := FFs.Stat('/file.txt');

  ExpectTime(AfterStat.AccessedAt, BeforeStat.AccessedAt);
  ExpectTime(FFs.Stat('/').AccessedAt, TimeAt(0));
end;

procedure TTestSandboxVirtualFileSystem.TestDirectoryEntryChangesUpdateParentMetadata;
var
  RootStat: TSandboxFsStat;
begin
  FClock.Value := TimeAt(10);
  FFs.MakeDirectory('/directory');
  RootStat := FFs.Stat('/');
  ExpectTime(RootStat.ModifiedAt, TimeAt(10));
  ExpectTime(RootStat.ChangedAt, TimeAt(10));

  FClock.Value := TimeAt(20);
  FFs.DeletePath('/directory');
  RootStat := FFs.Stat('/');
  ExpectTime(RootStat.ModifiedAt, TimeAt(20));
  ExpectTime(RootStat.ChangedAt, TimeAt(20));
end;

procedure TTestSandboxVirtualFileSystem.TestRecursiveMkdirReportsFirstCreatedPath;
begin
  FFs.MakeDirectory('/existing');
  Expect<string>(FFs.MakeDirectoryWithResult(
    '/existing/first/second', True)).ToBe('/existing/first');
  Expect<string>(FFs.MakeDirectoryWithResult(
    '/existing/first/second', True)).ToBe('');
end;

procedure TTestSandboxVirtualFileSystem.TestDefaultClockUsesUnixEpoch;
var
  DefaultFs: TSandboxVirtualFileSystem;
  BeforeMilliseconds: Int64;
  AfterMilliseconds: Int64;
  ActualMilliseconds: Double;
begin
  BeforeMilliseconds := GetEpochNanoseconds div 1000000;
  DefaultFs := TSandboxVirtualFileSystem.Create;
  try
    ActualMilliseconds := SandboxDateTimeToUnixMilliseconds(
      DefaultFs.Stat('/').BirthTime);
  finally
    DefaultFs.Free;
  end;
  AfterMilliseconds := GetEpochNanoseconds div 1000000;

  Expect<Boolean>((ActualMilliseconds >= BeforeMilliseconds - 1) and
    (ActualMilliseconds <= AfterMilliseconds + 1)).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(
    TTestSandboxVirtualFileSystem.Create('SandboxVirtualFileSystem'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
