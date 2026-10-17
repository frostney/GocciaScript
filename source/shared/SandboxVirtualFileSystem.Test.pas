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

  TSandboxFsProbe = procedure of object;

  TTestSandboxVirtualFileSystem = class(TTestSuite)
  private
    FProbePath: string;
    FProbeOther: string;
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
    procedure PopulateReadOnlyTree;
    function ProbeError(const AProbe: TSandboxFsProbe;
      const APath: string; const AOther: string = ''): string;
    procedure ExpectReadOnly(const AProbe: TSandboxFsProbe;
      const APath: string; const AOther: string = '';
      const AReportedPath: string = '');
    procedure ProbeWriteAllBytes;
    procedure ProbeWriteAllText;
    procedure ProbeAppendAllText;
    procedure ProbeOpenWrite;
    procedure ProbeOpenReadWrite;
    procedure ProbeOpenAppend;
    procedure ProbeMakeDirectory;
    procedure ProbeMakeDirectoryRecursive;
    procedure ProbeTouch;
    procedure ProbeDelete;
    procedure ProbeDeleteRecursive;
    procedure ProbeMove;
    procedure ProbeCopy;
    procedure ProbeCopyRecursive;
    procedure ProbeAddReadOnlyPath;
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
    procedure TestDefaultQuotasAreBounded;
    procedure TestNodeQuotaRejectsAtomically;
    procedure TestRootClampCallback;
    procedure TestReadOnlyPathsRefuseFileWrites;
    procedure TestReadOnlyPathsRefuseCreatingAndTouching;
    procedure TestReadOnlyPathsRefuseDeleting;
    procedure TestReadOnlyPathsRefuseMovingAndCopying;
    procedure TestReadOnlyPathsLeaveReadsAndOtherPaths;
    procedure TestReadOnlyPathsCoverPathsNotYetCreated;
    procedure TestReadOnlyPathsReachOpenHandlesAndForks;
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
  Test('Directory entry changes update parent metadata',
    TestDirectoryEntryChangesUpdateParentMetadata);
  Test('Recursive mkdir reports its first created path',
    TestRecursiveMkdirReportsFirstCreatedPath);
  Test('Default clock tracks the Unix epoch', TestDefaultClockUsesUnixEpoch);
  Test('Default quotas are bounded', TestDefaultQuotasAreBounded);
  Test('Node quota rejects atomically', TestNodeQuotaRejectsAtomically);
  Test('Root clamp callback reports one attempted escape',
    TestRootClampCallback);
  Test('Read-only paths refuse writes to existing and new files',
    TestReadOnlyPathsRefuseFileWrites);
  Test('Read-only paths refuse creating directories and touching',
    TestReadOnlyPathsRefuseCreatingAndTouching);
  Test('Read-only paths refuse deleting them, what they hold, and ' +
    'their holders', TestReadOnlyPathsRefuseDeleting);
  Test('Read-only paths refuse moving and copying at or into them',
    TestReadOnlyPathsRefuseMovingAndCopying);
  Test('Read-only paths leave reads and other paths alone',
    TestReadOnlyPathsLeaveReadsAndOtherPaths);
  Test('Read-only paths cover paths not created yet',
    TestReadOnlyPathsCoverPathsNotYetCreated);
  Test('Read-only paths reach open handles and forks',
    TestReadOnlyPathsReachOpenHandlesAndForks);
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

{ /data is read-only and holds a.txt and sub/b.txt; /work and /database.txt
  (which shares /data's prefix) stay writable, and /outer/inner, read-only,
  lies inside the writable /outer. }
procedure TTestSandboxVirtualFileSystem.PopulateReadOnlyTree;
begin
  FFs.MakeDirectory('/data/sub', True);
  FFs.WriteAllText('/data/a.txt', 'a');
  FFs.WriteAllText('/data/sub/b.txt', 'b');
  FFs.MakeDirectory('/work');
  FFs.WriteAllText('/work/w.txt', 'w');
  FFs.WriteAllText('/database.txt', 'not data');
  FFs.MakeDirectory('/outer/inner', True);
  FFs.WriteAllText('/outer/other.txt', 'o');
  FFs.WriteAllText('/outer/inner/f.txt', 'f');
  FFs.AddReadOnlyPath('/data');
  FFs.AddReadOnlyPath('/outer/inner');
end;

{ The class and message of the filesystem error the probe raised, or ''. }
function TTestSandboxVirtualFileSystem.ProbeError(
  const AProbe: TSandboxFsProbe; const APath, AOther: string): string;
begin
  FProbePath := APath;
  FProbeOther := AOther;
  Result := '';
  try
    AProbe;
  except
    on E: ESandboxFsError do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TTestSandboxVirtualFileSystem.ExpectReadOnly(
  const AProbe: TSandboxFsProbe; const APath, AOther,
  AReportedPath: string);
var
  Reported: string;
begin
  Reported := AReportedPath;
  if Reported = '' then
    Reported := APath;
  Expect<string>(ProbeError(AProbe, APath, AOther)).ToBe(
    'ESandboxFsReadOnly: ' + Reported + ': read-only file system');
end;

procedure TTestSandboxVirtualFileSystem.ProbeWriteAllBytes;
begin
  FFs.WriteAllBytes(FProbePath, TBytes.Create(1, 2, 3));
end;

procedure TTestSandboxVirtualFileSystem.ProbeWriteAllText;
begin
  FFs.WriteAllText(FProbePath, 'changed');
end;

procedure TTestSandboxVirtualFileSystem.ProbeAppendAllText;
begin
  FFs.AppendAllText(FProbePath, 'more');
end;

procedure TTestSandboxVirtualFileSystem.ProbeOpenWrite;
begin
  FFs.Open(FProbePath, omWrite).Free;
end;

procedure TTestSandboxVirtualFileSystem.ProbeOpenReadWrite;
begin
  FFs.Open(FProbePath, omReadWrite).Free;
end;

procedure TTestSandboxVirtualFileSystem.ProbeOpenAppend;
begin
  FFs.Open(FProbePath, omAppend).Free;
end;

procedure TTestSandboxVirtualFileSystem.ProbeMakeDirectory;
begin
  FFs.MakeDirectory(FProbePath);
end;

procedure TTestSandboxVirtualFileSystem.ProbeMakeDirectoryRecursive;
begin
  FFs.MakeDirectory(FProbePath, True);
end;

procedure TTestSandboxVirtualFileSystem.ProbeTouch;
begin
  FFs.Touch(FProbePath);
end;

procedure TTestSandboxVirtualFileSystem.ProbeDelete;
begin
  FFs.DeletePath(FProbePath);
end;

procedure TTestSandboxVirtualFileSystem.ProbeDeleteRecursive;
begin
  FFs.DeletePath(FProbePath, True);
end;

procedure TTestSandboxVirtualFileSystem.ProbeMove;
begin
  FFs.MovePath(FProbePath, FProbeOther);
end;

procedure TTestSandboxVirtualFileSystem.ProbeCopy;
begin
  FFs.CopyPath(FProbePath, FProbeOther);
end;

procedure TTestSandboxVirtualFileSystem.ProbeCopyRecursive;
begin
  FFs.CopyPath(FProbePath, FProbeOther, True);
end;

procedure TTestSandboxVirtualFileSystem.ProbeAddReadOnlyPath;
begin
  FFs.AddReadOnlyPath(FProbePath);
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsRefuseFileWrites;
const
  PATHS: array[0..4] of string = ('/data/a.txt', '/data/sub/b.txt',
    '/data/new.txt', '/data/missing/new.txt', '/outer/inner/f.txt');
var
  UsedBefore: Int64;
  NodesBefore: Integer;
  Index: Integer;
begin
  PopulateReadOnlyTree;
  UsedBefore := FFs.UsedBytes;
  NodesBefore := FFs.NodeCount;
  for Index := Low(PATHS) to High(PATHS) do
  begin
    ExpectReadOnly(ProbeWriteAllBytes, PATHS[Index]);
    ExpectReadOnly(ProbeWriteAllText, PATHS[Index]);
    ExpectReadOnly(ProbeAppendAllText, PATHS[Index]);
    ExpectReadOnly(ProbeOpenWrite, PATHS[Index]);
    ExpectReadOnly(ProbeOpenReadWrite, PATHS[Index]);
    ExpectReadOnly(ProbeOpenAppend, PATHS[Index]);
  end;
  { A spelling through '..' or a relative path is refused under its
    canonical name. }
  ExpectReadOnly(ProbeWriteAllText, '/work/../data/./a.txt', '',
    '/data/a.txt');
  ExpectReadOnly(ProbeWriteAllText, 'data/sub/../c.txt', '', '/data/c.txt');

  Expect<string>(FFs.ReadAllText('/data/a.txt')).ToBe('a');
  Expect<string>(FFs.ReadAllText('/data/sub/b.txt')).ToBe('b');
  Expect<string>(FFs.ReadAllText('/outer/inner/f.txt')).ToBe('f');
  Expect<Boolean>(FFs.Exists('/data/new.txt')).ToBe(False);
  Expect<Boolean>(FFs.Exists('/data/c.txt')).ToBe(False);
  Expect<Int64>(FFs.UsedBytes).ToBe(UsedBefore);
  Expect<Integer>(FFs.NodeCount).ToBe(NodesBefore);
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsRefuseCreatingAndTouching;
var
  NodesBefore: Integer;
  ModifiedBefore: TDateTime;
begin
  FClock.Value := TimeAt(10);
  PopulateReadOnlyTree;
  NodesBefore := FFs.NodeCount;
  ModifiedBefore := FFs.Stat('/data/a.txt').ModifiedAt;

  FClock.Value := TimeAt(20);
  ExpectReadOnly(ProbeMakeDirectory, '/data/new');
  ExpectReadOnly(ProbeMakeDirectoryRecursive, '/data/x/y/z');
  ExpectReadOnly(ProbeMakeDirectoryRecursive, '/outer/inner/deeper');
  ExpectReadOnly(ProbeTouch, '/data/a.txt');
  ExpectReadOnly(ProbeTouch, '/data/new.txt');
  ExpectReadOnly(ProbeTouch, '/data/sub');

  { Creating nothing is not a write: an existing directory still satisfies
    a recursive mkdir, and a plain one still reports that it exists. }
  FFs.MakeDirectory('/data', True);
  FFs.MakeDirectory('/data/sub', True);
  Expect<string>(ProbeError(ProbeMakeDirectory, '/data/sub')).ToBe(
    'ESandboxFsExists: /data/sub: already exists');
  { The writable part of a path is still created. }
  FFs.MakeDirectory('/outer/new', True);

  Expect<Integer>(FFs.NodeCount).ToBe(NodesBefore + 1);
  Expect<Boolean>(FFs.Exists('/data/x')).ToBe(False);
  ExpectTime(FFs.Stat('/data/a.txt').ModifiedAt, ModifiedBefore);
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsRefuseDeleting;
begin
  PopulateReadOnlyTree;
  ExpectReadOnly(ProbeDelete, '/data/a.txt');
  ExpectReadOnly(ProbeDeleteRecursive, '/data/sub');
  ExpectReadOnly(ProbeDeleteRecursive, '/data');
  ExpectReadOnly(ProbeDeleteRecursive, '/outer/inner');
  { A writable directory that holds a read-only one cannot be deleted
    either: the read-only one would go with it. }
  ExpectReadOnly(ProbeDeleteRecursive, '/outer');

  Expect<Boolean>(FFs.Exists('/data/a.txt')).ToBe(True);
  Expect<Boolean>(FFs.Exists('/data/sub/b.txt')).ToBe(True);
  Expect<Boolean>(FFs.Exists('/outer/inner/f.txt')).ToBe(True);
  Expect<Boolean>(FFs.Exists('/outer/other.txt')).ToBe(True);

  { Its writable contents can be. }
  FFs.DeletePath('/outer/other.txt');
  Expect<Boolean>(FFs.Exists('/outer/other.txt')).ToBe(False);
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsRefuseMovingAndCopying;
begin
  PopulateReadOnlyTree;
  { Moving out of, within, or into a read-only path. }
  ExpectReadOnly(ProbeMove, '/data/a.txt', '/work/a.txt');
  ExpectReadOnly(ProbeMove, '/data/a.txt', '/data/c.txt');
  ExpectReadOnly(ProbeMove, '/data', '/work/data');
  ExpectReadOnly(ProbeMove, '/work/w.txt', '/data/w.txt', '/data/w.txt');
  ExpectReadOnly(ProbeMove, '/work/w.txt', '/data/sub', '/data/sub/w.txt');
  ExpectReadOnly(ProbeMove, '/work/w.txt', '/data/a.txt', '/data/a.txt');
  { Moving a writable directory that holds a read-only one. }
  ExpectReadOnly(ProbeMove, '/outer', '/moved');
  { A missing parent under a read-only path is EROFS, as for a write. }
  ExpectReadOnly(ProbeMove, '/work/w.txt', '/data/missing/w.txt',
    '/data/missing/w.txt');
  ExpectReadOnly(ProbeCopy, '/work/w.txt', '/data/missing/w.txt',
    '/data/missing/w.txt');

  { Copying into a read-only path; reading one to copy it elsewhere is
    allowed. }
  ExpectReadOnly(ProbeCopy, '/work/w.txt', '/data/w.txt', '/data/w.txt');
  ExpectReadOnly(ProbeCopy, '/work/w.txt', '/data/a.txt', '/data/a.txt');
  ExpectReadOnly(ProbeCopy, '/work/w.txt', '/data', '/data/w.txt');
  ExpectReadOnly(ProbeCopyRecursive, '/work', '/data/work', '/data/work');
  FFs.CopyPath('/data', '/copied', True);
  FFs.CopyPath('/outer', '/outer-copy', True);

  Expect<string>(FFs.ReadAllText('/data/a.txt')).ToBe('a');
  Expect<Boolean>(FFs.Exists('/data/w.txt')).ToBe(False);
  Expect<Boolean>(FFs.Exists('/data/c.txt')).ToBe(False);
  Expect<Boolean>(FFs.Exists('/work/w.txt')).ToBe(True);
  Expect<Boolean>(FFs.Exists('/outer/inner/f.txt')).ToBe(True);
  { A copy is a new path, writable unless it is marked. }
  Expect<string>(FFs.ReadAllText('/copied/sub/b.txt')).ToBe('b');
  FFs.WriteAllText('/copied/sub/b.txt', 'changed');
  FFs.WriteAllText('/outer-copy/inner/f.txt', 'changed');

  FFs.MovePath('/work/w.txt', '/work/renamed.txt');
  Expect<Boolean>(FFs.Exists('/work/renamed.txt')).ToBe(True);
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsLeaveReadsAndOtherPaths;
var
  Handle: TSandboxFsFile;
begin
  PopulateReadOnlyTree;
  Expect<string>(FFs.ReadAllText('/data/a.txt')).ToBe('a');
  Expect<Integer>(Length(FFs.ReadAllBytes('/data/sub/b.txt'))).ToBe(1);
  Expect<Integer>(Length(FFs.List('/data'))).ToBe(2);
  Expect<Int64>(FFs.Stat('/data/a.txt').Size).ToBe(1);
  Expect<string>(FFs.SnapshotReadAllText('/data/a.txt')).ToBe('a');
  Handle := FFs.Open('/data/a.txt', omRead);
  try
    Expect<string>(Handle.ReadText(10)).ToBe('a');
  finally
    Handle.Free;
  end;

  Expect<Boolean>(FFs.IsReadOnly('/data')).ToBe(True);
  Expect<Boolean>(FFs.IsReadOnly('/data/sub/b.txt')).ToBe(True);
  Expect<Boolean>(FFs.IsReadOnly('/data/missing')).ToBe(True);
  Expect<Boolean>(FFs.IsReadOnly('/work/../data/a.txt')).ToBe(True);
  Expect<Boolean>(FFs.IsReadOnly('/')).ToBe(False);
  Expect<Boolean>(FFs.IsReadOnly('/dat')).ToBe(False);
  Expect<Boolean>(FFs.IsReadOnly('/database.txt')).ToBe(False);
  Expect<Boolean>(FFs.IsReadOnly('/outer')).ToBe(False);
  Expect<Boolean>(FFs.IsReadOnly('/outer/inner/f.txt')).ToBe(True);

  { A path that only shares the read-only path's prefix stays writable. }
  FFs.WriteAllText('/database.txt', 'changed');
  FFs.AppendAllText('/work/w.txt', '!');
  FFs.MakeDirectory('/data2');
  Expect<string>(FFs.ReadAllText('/database.txt')).ToBe('changed');
  Expect<string>(FFs.ReadAllText('/work/w.txt')).ToBe('w!');
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsCoverPathsNotYetCreated;
var
  RootFs: TSandboxVirtualFileSystem;
  Message: string;
begin
  FFs.AddReadOnlyPath('later/../later/');
  Expect<Boolean>(FFs.IsReadOnly('/later')).ToBe(True);
  ExpectReadOnly(ProbeMakeDirectory, '/later');
  ExpectReadOnly(ProbeMakeDirectoryRecursive, '/later/x');
  ExpectReadOnly(ProbeWriteAllText, '/later');
  FFs.MakeDirectory('/lat');
  Expect<Boolean>(FFs.Exists('/later')).ToBe(False);

  { A tree moved or copied to a missing ancestor of a mark would create
    entries under it. }
  FFs.AddReadOnlyPath('/up/deep/mark');
  FFs.MakeDirectory('/up');
  FFs.MakeDirectory('/tree/mark', True);
  FFs.WriteAllText('/tree/mark/f.txt', 'f');
  ExpectReadOnly(ProbeMove, '/tree', '/up/deep', '/up/deep');
  ExpectReadOnly(ProbeCopyRecursive, '/tree', '/up/deep', '/up/deep');
  Expect<Boolean>(FFs.Exists('/up/deep')).ToBe(False);
  Expect<Boolean>(FFs.Exists('/tree/mark/f.txt')).ToBe(True);
  { A file lands only at the destination, so it may go there, as a write
    may. }
  FFs.WriteAllText('/x.txt', 'x');
  FFs.CopyPath('/x.txt', '/up/deep');
  FFs.DeletePath('/up/deep');
  FFs.MovePath('/x.txt', '/up/deep');
  Expect<string>(FFs.ReadAllText('/up/deep')).ToBe('x');
  FFs.DeletePath('/up/deep');
  { Next to the mark is still writable. }
  FFs.MovePath('/tree', '/up/other');
  Expect<Boolean>(FFs.Exists('/up/other/mark/f.txt')).ToBe(True);

  { An empty path would normalize to the root. }
  Expect<string>(ProbeError(ProbeAddReadOnlyPath, '')).ToBe(
    'ESandboxFsInvalidPath: read-only path is empty');
  Expect<Boolean>(FFs.IsReadOnly('/lat')).ToBe(False);

  { Marking the root makes everything read-only. }
  RootFs := TSandboxVirtualFileSystem.Create(0, FClock);
  try
    RootFs.WriteAllText('/file.txt', 'x');
    RootFs.AddReadOnlyPath('/');
    Expect<Boolean>(RootFs.IsReadOnly('/anything')).ToBe(True);
    Message := '';
    try
      RootFs.MakeDirectory('/new');
    except
      on E: ESandboxFsReadOnly do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('/new: read-only file system');
    Message := '';
    try
      RootFs.DeletePath('/file.txt');
    except
      on E: ESandboxFsReadOnly do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('/file.txt: read-only file system');
  finally
    RootFs.Free;
  end;
end;

procedure TTestSandboxVirtualFileSystem.TestReadOnlyPathsReachOpenHandlesAndForks;
var
  Handle: TSandboxFsFile;
  Forked: TSandboxVirtualFileSystem;
  Data: TBytes;
  Message: string;
begin
  PopulateReadOnlyTree;
  { A handle opened before its path was marked stops writing once it is. }
  Handle := FFs.Open('/work/w.txt', omReadWrite);
  try
    FFs.AddReadOnlyPath('/work');
    Data := TBytes.Create(65);
    Message := '';
    try
      Handle.Write(Data[0], 1);
    except
      on E: ESandboxFsReadOnly do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('/work/w.txt: read-only file system');
    Message := '';
    try
      Handle.Truncate(0);
    except
      on E: ESandboxFsReadOnly do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('/work/w.txt: read-only file system');
  finally
    Handle.Free;
  end;
  Expect<string>(FFs.ReadAllText('/work/w.txt')).ToBe('w');

  Forked := FFs.Fork;
  try
    Expect<Boolean>(Forked.IsReadOnly('/data/a.txt')).ToBe(True);
    Expect<Boolean>(Forked.IsReadOnly('/work')).ToBe(True);
    Expect<Boolean>(Forked.IsReadOnly('/database.txt')).ToBe(False);
    Message := '';
    try
      Forked.WriteAllText('/data/a.txt', 'changed');
    except
      on E: ESandboxFsReadOnly do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('/data/a.txt: read-only file system');
  finally
    Forked.Free;
  end;
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
