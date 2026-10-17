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
    FClampCount: Integer;
    FLastClampPath: string;
    FLastClampBase: string;
    FLastCanonicalPath: string;
    function TimeAt(const AMillisecond: Integer): TDateTime;
    procedure ExpectTime(const AActual, AExpected: TDateTime);
    procedure HandleRootClamp(const APath, ABase,
      ACanonicalPath: string);
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;

    procedure TestTracksIndependentFileTimestamps;
    procedure TestCopyRenameAndForkMetadata;
    procedure TestForkDoesNotReadClock;
    procedure TestSnapshotReadsDoNotAdvanceAccessTime;
    procedure TestSnapshotSameBytesComparesInPlace;
    procedure TestDirectoryEntryChangesUpdateParentMetadata;
    procedure TestRecursiveMkdirReportsFirstCreatedPath;
    procedure TestDefaultClockUsesUnixEpoch;
    procedure TestDefaultQuotasAreBounded;
    procedure TestNodeQuotaRejectsAtomically;
    procedure TestRootClampCallback;
    procedure TestWholeFileWritesHoldExactlyTheirBytes;
    procedure TestHandleWritesStillGrowByDoubling;
    procedure TestWholeFileWriteRefusedByQuotaAllocatesNothing;
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
  Test('SnapshotSameBytes compares files in place',
    TestSnapshotSameBytesComparesInPlace);
  Test('Directory entry changes update parent metadata',
    TestDirectoryEntryChangesUpdateParentMetadata);
  Test('Recursive mkdir reports its first created path',
    TestRecursiveMkdirReportsFirstCreatedPath);
  Test('Default clock tracks the Unix epoch', TestDefaultClockUsesUnixEpoch);
  Test('Default quotas are bounded', TestDefaultQuotasAreBounded);
  Test('Node quota rejects atomically', TestNodeQuotaRejectsAtomically);
  Test('Root clamp callback reports one attempted escape',
    TestRootClampCallback);
  Test('Whole-file writes hold exactly their bytes',
    TestWholeFileWritesHoldExactlyTheirBytes);
  Test('Writes through a handle still grow by doubling',
    TestHandleWritesStillGrowByDoubling);
  Test('A whole-file write the quota refuses allocates nothing',
    TestWholeFileWriteRefusedByQuotaAllocatesNothing);
end;

procedure TTestSandboxVirtualFileSystem.BeforeEach;
begin
  FClock := TDeterministicSandboxClock.Create;
  FClock.Value := TimeAt(0);
  FFs := TSandboxVirtualFileSystem.Create(0, FClock);
  FClampCount := 0;
  FLastClampPath := '';
  FLastClampBase := '';
  FLastCanonicalPath := '';
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

procedure TTestSandboxVirtualFileSystem.HandleRootClamp(
  const APath, ABase, ACanonicalPath: string);
begin
  Inc(FClampCount);
  FLastClampPath := APath;
  FLastClampBase := ABase;
  FLastCanonicalPath := ACanonicalPath;
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

procedure TTestSandboxVirtualFileSystem.TestSnapshotSameBytesComparesInPlace;
var
  Bytes, Changed: TBytes;
  Other: TSandboxVirtualFileSystem;
  Size, Index: Integer;
  BeforeStat: TSandboxFsStat;
  Raised: Boolean;
begin
  Other := TSandboxVirtualFileSystem.Create(16 * 1024 * 1024, FClock);
  try
    FClock.Value := TimeAt(10);
    // Sizes on both sides of a machine word, so the word loop and the byte
    // tail are each reached, alone and together.
    for Size := 0 to 19 do
    begin
      SetLength(Bytes, Size);
      for Index := 0 to Size - 1 do
        Bytes[Index] := Byte(Index * 37 + 11);
      FFs.WriteAllBytes('/file.bin', Bytes);
      Other.WriteAllBytes('/other.bin', Bytes);
      Expect<Boolean>(FFs.SnapshotSameBytes('/file.bin', Other,
        '/other.bin')).ToBe(True);
      Expect<Boolean>(FFs.SnapshotSameBytes('/file.bin', FFs,
        '/file.bin')).ToBe(True);
      for Index := 0 to Size - 1 do
      begin
        Changed := Copy(Bytes);
        Changed[Index] := Changed[Index] xor $80;
        Other.WriteAllBytes('/other.bin', Changed);
        Expect<Boolean>(FFs.SnapshotSameBytes('/file.bin', Other,
          '/other.bin')).ToBe(False);
      end;
      Other.WriteAllBytes('/other.bin', Copy(Bytes, 0, Size div 2));
      if Size > 0 then
        Expect<Boolean>(FFs.SnapshotSameBytes('/file.bin', Other,
          '/other.bin')).ToBe(False);
    end;

    BeforeStat := FFs.Stat('/file.bin');
    FClock.Value := TimeAt(20);
    FFs.SnapshotSameBytes('/file.bin', Other, '/other.bin');
    ExpectTime(FFs.Stat('/file.bin').AccessedAt, BeforeStat.AccessedAt);

    FFs.MakeDirectory('/directory');
    Raised := False;
    try
      FFs.SnapshotSameBytes('/directory', Other, '/other.bin');
    except
      on E: ESandboxFsIsADirectory do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Other.Free;
  end;
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

procedure TTestSandboxVirtualFileSystem.TestDefaultQuotasAreBounded;
var
  DefaultFs: TSandboxVirtualFileSystem;
begin
  DefaultFs := TSandboxVirtualFileSystem.Create;
  try
    Expect<Int64>(DefaultFs.QuotaBytes).ToBe(16 * 1024 * 1024);
    Expect<Integer>(DefaultFs.NodeQuota).ToBe(4096);
  finally
    DefaultFs.Free;
  end;
end;

procedure TTestSandboxVirtualFileSystem.TestNodeQuotaRejectsAtomically;
var
  LimitedFs: TSandboxVirtualFileSystem;
  Raised: Boolean;
begin
  LimitedFs := TSandboxVirtualFileSystem.Create(0, 1, FClock);
  try
    LimitedFs.MakeDirectory('/first');
    Raised := False;
    try
      LimitedFs.MakeDirectory('/second');
    except
      on E: ESandboxFsQuotaExceeded do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Integer>(LimitedFs.NodeCount).ToBe(1);
    Expect<Boolean>(LimitedFs.Exists('/second')).ToBe(False);
  finally
    LimitedFs.Free;
  end;
end;

procedure TTestSandboxVirtualFileSystem.TestRootClampCallback;
begin
  FFs.RootClampCallback := HandleRootClamp;

  Expect<string>(FFs.Normalize('/work/..')).ToBe('/');
  Expect<Integer>(FClampCount).ToBe(0);

  Expect<string>(FFs.Normalize('../../secret.txt', '/work')).ToBe(
    '/secret.txt');
  Expect<Integer>(FClampCount).ToBe(1);
  Expect<string>(FLastClampPath).ToBe('../../secret.txt');
  Expect<string>(FLastClampBase).ToBe('/work');
  Expect<string>(FLastCanonicalPath).ToBe('/secret.txt');
end;

function FilledBytes(const ACount: Integer): TBytes;
begin
  Result := nil;
  SetLength(Result, ACount);
  if ACount > 0 then
    FillChar(Result[0], ACount, 65);
end;

procedure TTestSandboxVirtualFileSystem.TestWholeFileWritesHoldExactlyTheirBytes;
const
  SIZES: array[0..7] of Integer = (1, 63, 64, 65, 4096, 4097,
    1024 * 1024 + 1, 0);
var
  Index: Integer;
begin
  { A size just above a power of two is the case a doubling buffer rounds
    up the most. }
  for Index := Low(SIZES) to High(SIZES) do
  begin
    FFs.WriteAllBytes('/file.bin', FilledBytes(SIZES[Index]));
    Expect<Int64>(FFs.SnapshotAllocatedBytes('/file.bin')).ToBe(
      SIZES[Index]);
    Expect<Int64>(FFs.Stat('/file.bin').Size).ToBe(SIZES[Index]);
    Expect<Int64>(FFs.UsedBytes).ToBe(SIZES[Index]);
  end;

  { A shrinking rewrite releases the larger buffer. }
  FFs.WriteAllBytes('/file.bin', FilledBytes(100000));
  FFs.WriteAllBytes('/file.bin', FilledBytes(10));
  Expect<Int64>(FFs.SnapshotAllocatedBytes('/file.bin')).ToBe(10);
  Expect<Int64>(FFs.UsedBytes).ToBe(10);
  Expect<Integer>(Length(FFs.ReadAllBytes('/file.bin'))).ToBe(10);

  { 'h', 'é' (two bytes), 'llo'. }
  FFs.WriteAllText('/file.txt', 'h' + #$C3#$A9 + 'llo');
  Expect<Int64>(FFs.SnapshotAllocatedBytes('/file.txt')).ToBe(6);
  Expect<Int64>(FFs.UsedBytes).ToBe(16);
end;

procedure TTestSandboxVirtualFileSystem.TestHandleWritesStillGrowByDoubling;
var
  Handle: TSandboxFsFile;
  Data: TBytes;
begin
  FFs.WriteAllBytes('/file.bin', FilledBytes(65));
  FFs.AppendAllText('/file.bin', 'x');
  Expect<Int64>(FFs.SnapshotAllocatedBytes('/file.bin')).ToBe(130);
  FFs.AppendAllText('/file.bin', 'y');
  Expect<Int64>(FFs.SnapshotAllocatedBytes('/file.bin')).ToBe(130);
  Expect<Int64>(FFs.Stat('/file.bin').Size).ToBe(67);
  Expect<Int64>(FFs.UsedBytes).ToBe(67);

  Data := FilledBytes(100);
  Handle := FFs.Open('/stream.bin', omWrite);
  try
    Handle.Write(Data[0], Length(Data));
  finally
    Handle.Free;
  end;
  Expect<Int64>(FFs.SnapshotAllocatedBytes('/stream.bin')).ToBe(128);
  Expect<Int64>(FFs.Stat('/stream.bin').Size).ToBe(100);
  Expect<Int64>(FFs.UsedBytes).ToBe(167);
end;

procedure TTestSandboxVirtualFileSystem.TestWholeFileWriteRefusedByQuotaAllocatesNothing;
var
  LimitedFs: TSandboxVirtualFileSystem;
  Raised: Boolean;
begin
  LimitedFs := TSandboxVirtualFileSystem.Create(100, FClock);
  try
    LimitedFs.WriteAllBytes('/file.bin', FilledBytes(50));
    Raised := False;
    try
      LimitedFs.WriteAllBytes('/file.bin', FilledBytes(1000));
    except
      on E: ESandboxFsQuotaExceeded do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(LimitedFs.SnapshotAllocatedBytes('/file.bin') <
      1000).ToBe(True);
    Expect<Int64>(LimitedFs.UsedBytes).ToBe(0);

    LimitedFs.WriteAllBytes('/file.bin', FilledBytes(100));
    Expect<Int64>(LimitedFs.SnapshotAllocatedBytes('/file.bin')).ToBe(100);
    Expect<Int64>(LimitedFs.UsedBytes).ToBe(100);
  finally
    LimitedFs.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TTestSandboxVirtualFileSystem.Create('SandboxVirtualFileSystem'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
