program SandboxHostInputs.Test;

{$I Shared.inc}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  SandboxHostInputs,
  SandboxVirtualFileSystem,
  TestingPascalLibrary;

type
  { A sandbox clock that, once armed, moves a host directory away and puts a
    new, empty one in its place the first time the sandbox asks it the time:
    a copy-in asks when it writes a file it has read, so the swap lands in
    the middle of the copy. }
  TSwapDirectoryClock = class(TSandboxFsClock)
  private
    FDirectory: string;
    FArmed: Boolean;
  public
    procedure Arm(const ADirectory: string);
    function Now: TDateTime; override;
  end;

  { Records origins as CopyIn does, without copying: for an input no test
    can copy, such as a whole filesystem root. }
  TRecordingHostInputs = class(TSandboxHostInputs)
  public
    function RecordOrigin(const AHostPath, ASandboxPath: string;
      const AIsDirectory: Boolean;
      const APin: TSandboxHostPin): TSandboxHostOrigin;
  end;

  TSandboxHostInputsTests = class(TTestSuite)
  private
    FRoot: string;
    FFs: TSandboxVirtualFileSystem;
    FInputs: TSandboxHostInputs;
    function WriteHostFile(const ARelativePath, AText: string): string;
    function HostPath(const ARelativePath: string): string;
    function ReadHostText(const ARelativePath: string): string;
    function CopyError(const AHostPath, ASandboxPath: string): string;
    procedure TestFileTargets;
    procedure TestDirectoryTargets;
    procedure TestMissingHostPath;
    procedure TestSymlinksRejected;
    procedure TestEntryInsideInput;
    procedure TestLongestOriginWins;
    procedure TestWriteBackOnlyReadWriteOrigins;
    procedure TestWriteBackNewFilesInsideReadWriteDirectory;
    procedure TestWriteBackNeverDeletes;
    procedure TestWriteBackSkipsSymlinkedTarget;
    procedure TestWriteBackRefusesReplacedRoot;
    procedure TestWriteBackRefusesDirectorySwappedAfterPlan;
    procedure TestWriteBackRefusesFileInputDirectoryReplaced;
    procedure TestOutputFileRefusesReplacedDirectory;
    procedure TestOutputFileRefusesLeafSymlink;
    function PinUnderRoot(const ARelativePath: string): TSandboxHostPin;
    procedure TestPinRefusesPathOutsideRoot;
    procedure TestPinnedInputRefusesAncestorSwappedBeforeCopy;
    procedure TestPinnedInputWritesAlongItsRoute;
    procedure TestCommandLineOutputWritesAsNamed;
    procedure TestCommandLineOutputNeverWaitsOnAFifo;
    procedure TestPinnedInputReplacedByADirectoryWritesNothing;
    procedure TestInputReplacedDuringCopyWritesNothing;
    procedure TestPinnedInputReplacedDuringCopyWritesNothing;
    procedure TestConfigFileInputTurnedDirectoryWritesNothing;
    procedure TestUnpinnedInputStopsAllWriteBack;
    procedure TestDirectoryIdentityTellsDirectoriesApart;
    procedure TestUnavailableIdentityMatchesNothing;
    procedure TestOutputFileRefusesUnidentifiedDirectory;
    procedure TestUnidentifiedInputGetsNothingWritten;
    function FilesystemRoot: string;
    procedure TestInputDirectlyUnderARootHasTheRootAsItsDirectory;
    procedure TestInputPinnedAtARootWritesBackUnderIt;
    procedure TestPathBelowARootKeepsItsFirstCharacter;
    procedure TestCopiedRootMapsItsFilesBothWays;
    procedure TestPinAtARootItselfIdentifiesTheRoot;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

const
  { Room for the few small files the swap tests copy. }
  SWAP_TEST_QUOTA_BYTES = 1024 * 1024;

procedure TSwapDirectoryClock.Arm(const ADirectory: string);
begin
  FDirectory := ADirectory;
  FArmed := True;
end;

function TSwapDirectoryClock.Now: TDateTime;
begin
  if FArmed then
  begin
    FArmed := False;
    if not RenameFile(FDirectory, FDirectory + '.real') or
       not CreateDir(FDirectory) then
      raise Exception.Create('could not swap ' + FDirectory);
  end;
  Result := SysUtils.Now;
end;

procedure TSandboxHostInputsTests.SetupTests;
begin
  Test('A file lands at its target, or inside a directory target',
    TestFileTargets);
  Test('A directory''s contents land in its target', TestDirectoryTargets);
  Test('A missing host path is refused', TestMissingHostPath);
  Test('Symbolic links are refused, at the top and inside a directory',
    TestSymlinksRejected);
  Test('A host file inside a copied input maps to its sandbox path',
    TestEntryInsideInput);
  Test('The longest matching origin supplies a sandbox path',
    TestLongestOriginWins);
  Test('Write-back writes only read-write origins and reports the rest',
    TestWriteBackOnlyReadWriteOrigins);
  Test('Write-back writes new files inside a read-write directory',
    TestWriteBackNewFilesInsideReadWriteDirectory);
  Test('Write-back never deletes a host file', TestWriteBackNeverDeletes);
  Test('Write-back skips a target that became a symlink',
    TestWriteBackSkipsSymlinkedTarget);
  Test('Write-back writes nothing when an input was swapped for a link',
    TestWriteBackRefusesReplacedRoot);
  Test('Write-back does not follow a directory swapped after planning',
    TestWriteBackRefusesDirectorySwappedAfterPlan);
  Test('Write-back refuses a file input whose directory was replaced',
    TestWriteBackRefusesFileInputDirectoryReplaced);
  Test('An output file refuses a directory replaced after pinning',
    TestOutputFileRefusesReplacedDirectory);
  Test('An output file refuses a link at its own name',
    TestOutputFileRefusesLeafSymlink);
  Test('A pin refuses a path outside its root', TestPinRefusesPathOutsideRoot);
  Test('A pinned input refuses an ancestor swapped before it was copied',
    TestPinnedInputRefusesAncestorSwappedBeforeCopy);
  Test('A pinned input writes back along its route',
    TestPinnedInputWritesAlongItsRoute);
  Test('A command-line output is written as the user named it',
    TestCommandLineOutputWritesAsNamed);
  Test('A command-line output never waits on a FIFO',
    TestCommandLineOutputNeverWaitsOnAFifo);
  Test('A config input replaced by another directory gets nothing written',
    TestPinnedInputReplacedByADirectoryWritesNothing);
  Test('An input replaced while it was being copied gets nothing written',
    TestInputReplacedDuringCopyWritesNothing);
  Test('A config input replaced while it was being copied gets nothing written',
    TestPinnedInputReplacedDuringCopyWritesNothing);
  Test('A config file input that became a directory gets nothing written',
    TestConfigFileInputTurnedDirectoryWritesNothing);
  Test('An input that could not be pinned stops every input''s write-back',
    TestUnpinnedInputStopsAllWriteBack);
  Test('A directory identity tells two directories apart',
    TestDirectoryIdentityTellsDirectoriesApart);
  Test('An unavailable directory identity matches nothing',
    TestUnavailableIdentityMatchesNothing);
  Test('An output file refuses a directory that could not be identified',
    TestOutputFileRefusesUnidentifiedDirectory);
  Test('An input that could not be identified gets nothing written back',
    TestUnidentifiedInputGetsNothingWritten);
  Test('An input directly under a filesystem root has the root as its ' +
    'directory', TestInputDirectlyUnderARootHasTheRootAsItsDirectory);
  Test('An input pinned at a filesystem root writes back under it',
    TestInputPinnedAtARootWritesBackUnderIt);
  Test('A path below a filesystem root keeps its first character',
    TestPathBelowARootKeepsItsFirstCharacter);
  Test('A copied filesystem root maps its files both ways',
    TestCopiedRootMapsItsFilesBothWays);
  Test('A config pin at a filesystem root itself identifies the root',
    TestPinAtARootItselfIdentifiesTheRoot);
end;

procedure DeleteDirectoryTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if HostPathIsSymlink(EntryPath) then
        DeleteFile(EntryPath)
      else if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

procedure TSandboxHostInputsTests.BeforeEach;
begin
  inherited BeforeEach;
  FRoot := ExcludeTrailingPathDelimiter(ExpandFileName(
    IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-host-inputs-' +
    IntToStr(Random(MaxInt))));
  ForceDirectories(FRoot);
  FFs := TSandboxVirtualFileSystem.Create;
  FInputs := TSandboxHostInputs.Create(FFs);
end;

procedure TSandboxHostInputsTests.AfterEach;
begin
  FInputs.Free;
  FFs.Free;
  DeleteDirectoryTree(FRoot);
  inherited AfterEach;
end;

function TSandboxHostInputsTests.HostPath(const ARelativePath: string): string;
begin
  Result := FRoot + PathDelim + StringReplace(ARelativePath, '/', PathDelim,
    [rfReplaceAll]);
end;

function TSandboxHostInputsTests.WriteHostFile(const ARelativePath,
  AText: string): string;
begin
  Result := HostPath(ARelativePath);
  ForceDirectories(ExtractFileDir(Result));
  WriteUTF8FileText(Result, AText);
end;

function TSandboxHostInputsTests.ReadHostText(
  const ARelativePath: string): string;
begin
  Result := ReadUTF8FileText(HostPath(ARelativePath));
end;

function TSandboxHostInputsTests.CopyError(const AHostPath,
  ASandboxPath: string): string;
begin
  Result := '';
  try
    FInputs.CopyIn(AHostPath, ASandboxPath, False);
  except
    on E: ESandboxHostInputError do
      Result := E.Message;
  end;
end;

procedure TSandboxHostInputsTests.TestFileTargets;
begin
  WriteHostFile('task.txt', 'task');
  Expect<string>(FInputs.CopyIn(HostPath('task.txt'), '/task.txt', False))
    .ToBe('/task.txt');
  Expect<string>(FFs.ReadAllText('/task.txt')).ToBe('task');
  Expect<string>(FInputs.CopyIn(HostPath('task.txt'), '/inbox/', False))
    .ToBe('/inbox/task.txt');
  FFs.MakeDirectory('/existing');
  Expect<string>(FInputs.CopyIn(HostPath('task.txt'), '/existing', False))
    .ToBe('/existing/task.txt');
  Expect<string>(FInputs.CopyIn(HostPath('task.txt'), '/', False))
    .ToBe('/task.txt');
  Expect<string>(FInputs.CopyIn(HostPath('task.txt'), '/renamed.md', False))
    .ToBe('/renamed.md');
end;

procedure TSandboxHostInputsTests.TestDirectoryTargets;
begin
  WriteHostFile('src/main.js', 'main');
  WriteHostFile('src/lib/util.js', 'util');
  Expect<string>(FInputs.CopyIn(HostPath('src'), '/src', False))
    .ToBe('/src');
  Expect<string>(FFs.ReadAllText('/src/main.js')).ToBe('main');
  Expect<string>(FFs.ReadAllText('/src/lib/util.js')).ToBe('util');
  Expect<string>(FInputs.CopyIn(HostPath('src'), '/', False)).ToBe('/');
  Expect<Boolean>(FFs.IsFile('/main.js')).ToBe(True);
  Expect<Boolean>(FFs.IsFile('/lib/util.js')).ToBe(True);
end;

procedure TSandboxHostInputsTests.TestMissingHostPath;
begin
  Expect<string>(CopyError(HostPath('missing'), '/missing')).ToBe(
    'Copy path does not exist: ' + HostPath('missing'));
end;

procedure TSandboxHostInputsTests.TestSymlinksRejected;
begin
  {$IFDEF UNIX}
  WriteHostFile('real/secret.txt', 'secret');
  WriteHostFile('tree/ok.txt', 'ok');
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('real'))),
    PAnsiChar(AnsiString(HostPath('link'))))).ToBe(0);
  Expect<Integer>(FpSymlink(
    PAnsiChar(AnsiString(HostPath('real/secret.txt'))),
    PAnsiChar(AnsiString(HostPath('tree/leak.txt'))))).ToBe(0);
  Expect<string>(CopyError(HostPath('link'), '/link')).ToBe(
    'Copy path is a symlink (not supported): ' + HostPath('link'));
  Expect<string>(CopyError(HostPath('link') + PathDelim, '/link')).ToBe(
    'Copy path is a symlink (not supported): ' + HostPath('link'));
  Expect<string>(CopyError(HostPath('tree'), '/tree')).ToBe(
    'Copy path is a symlink (not supported): ' + HostPath('tree/leak.txt'));
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestEntryInsideInput;
var
  SandboxPath: string;
begin
  WriteHostFile('src/main.js', 'main');
  WriteHostFile('other.js', 'other');
  WriteHostFile('task.txt', 'task');
  FInputs.CopyIn(HostPath('src'), '/app', False);
  FInputs.CopyIn(HostPath('task.txt'), '/inbox/', False);
  Expect<Boolean>(FInputs.SandboxPathOfHostFile(HostPath('src/main.js'),
    SandboxPath)).ToBe(True);
  Expect<string>(SandboxPath).ToBe('/app/main.js');
  Expect<Boolean>(FInputs.SandboxPathOfHostFile(
    HostPath('src') + PathDelim + '..' + PathDelim + 'src' + PathDelim +
    'main.js', SandboxPath)).ToBe(True);
  Expect<string>(SandboxPath).ToBe('/app/main.js');
  Expect<Boolean>(FInputs.SandboxPathOfHostFile(HostPath('task.txt'),
    SandboxPath)).ToBe(True);
  Expect<string>(SandboxPath).ToBe('/inbox/task.txt');
  Expect<Boolean>(FInputs.SandboxPathOfHostFile(HostPath('other.js'),
    SandboxPath)).ToBe(False);
end;

procedure TSandboxHostInputsTests.TestLongestOriginWins;
var
  Origin: TSandboxHostOrigin;
  Path: string;
begin
  WriteHostFile('outer/a.txt', 'a');
  WriteHostFile('inner/b.txt', 'b');
  FInputs.CopyIn(HostPath('outer'), '/work', False);
  FInputs.CopyIn(HostPath('inner'), '/work/inner', True);
  Expect<Boolean>(FInputs.HostPathForSandboxPath('/work/inner/b.txt', Origin,
    Path)).ToBe(True);
  Expect<string>(Path).ToBe(HostPath('inner/b.txt'));
  Expect<Boolean>(Origin.ReadWrite).ToBe(True);
  Expect<Boolean>(FInputs.HostPathForSandboxPath('/work/a.txt', Origin,
    Path)).ToBe(True);
  Expect<string>(Path).ToBe(HostPath('outer/a.txt'));
  Expect<Boolean>(Origin.ReadWrite).ToBe(False);
  Expect<Boolean>(FInputs.HostPathForSandboxPath('/elsewhere.txt', Origin,
    Path)).ToBe(False);
end;

procedure TSandboxHostInputsTests.TestWriteBackOnlyReadWriteOrigins;
var
  Baseline: TSandboxVirtualFileSystem;
  Plan: TSandboxWriteBackPlan;
  Report: TStringList;
begin
  WriteHostFile('ro/a.txt', 'a');
  WriteHostFile('rw/b.txt', 'b');
  FInputs.CopyIn(HostPath('ro'), '/ro', False);
  FInputs.CopyIn(HostPath('rw'), '/rw', True);
  Expect<Boolean>(FInputs.HasReadWriteInput).ToBe(True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/ro/a.txt', 'A');
    FFs.WriteAllText('/rw/b.txt', 'B');
    FFs.WriteAllText('/scratch.txt', 's');
    Plan := FInputs.PlanWriteBack(Baseline);
    Expect<Integer>(Length(Plan)).ToBe(3);
    Expect<Boolean>(FInputs.ApplyWriteBack(Plan, Report)).ToBe(True);
    Expect<string>(ReadHostText('ro/a.txt')).ToBe('a');
    Expect<string>(ReadHostText('rw/b.txt')).ToBe('B');
    Expect<string>(Report.Text).ToBe(
      'write-back: /ro/a.txt was copied without write-back, skipped (use ' +
      '--copy-rw to write it back)' + sLineBreak +
      'write-back: /scratch.txt was not copied from the host, skipped' +
      sLineBreak +
      'write-back: ' + HostPath('rw/b.txt') + sLineBreak +
      'write-back: 1 file(s) written, 2 skipped' + sLineBreak);
  finally
    Report.Free;
    Baseline.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestWriteBackNewFilesInsideReadWriteDirectory;
var
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
begin
  WriteHostFile('out/keep.txt', 'keep');
  FInputs.CopyIn(HostPath('out'), '/out', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.MakeDirectory('/out/nested/deeper', True);
    FFs.WriteAllText('/out/nested/deeper/new.txt', 'new');
    FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline), Report);
    Expect<string>(ReadHostText('out/nested/deeper/new.txt')).ToBe('new');
    Expect<string>(ReadHostText('out/keep.txt')).ToBe('keep');
    Expect<string>(Report[Report.Count - 1]).ToBe(
      'write-back: 1 file(s) written, 0 skipped');
  finally
    Report.Free;
    Baseline.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestWriteBackNeverDeletes;
var
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
begin
  WriteHostFile('out/doomed.txt', 'still here');
  FInputs.CopyIn(HostPath('out'), '/out', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.DeletePath('/out/doomed.txt');
    Expect<Integer>(Length(FInputs.PlanWriteBack(Baseline))).ToBe(0);
    FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline), Report);
    Expect<string>(ReadHostText('out/doomed.txt')).ToBe('still here');
  finally
    Report.Free;
    Baseline.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestWriteBackSkipsSymlinkedTarget;
{$IFDEF UNIX}
var
  Baseline: TSandboxVirtualFileSystem;
  Plan: TSandboxWriteBackPlan;
  Report: TStringList;
{$ENDIF}
begin
  {$IFDEF UNIX}
  WriteHostFile('out/sub/file.txt', 'old');
  WriteHostFile('elsewhere/file.txt', 'untouched');
  FInputs.CopyIn(HostPath('out'), '/out', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    { After the copy, the host directory is swapped for a link that leads
      out of the input. }
    DeleteFile(HostPath('out/sub/file.txt'));
    RemoveDir(HostPath('out/sub'));
    Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('elsewhere'))),
      PAnsiChar(AnsiString(HostPath('out/sub'))))).ToBe(0);
    FFs.WriteAllText('/out/sub/file.txt', 'new');
    Plan := FInputs.PlanWriteBack(Baseline);
    Expect<Integer>(Length(Plan)).ToBe(1);
    Expect<Boolean>(Plan[0].Action = swaSkipOutside).ToBe(True);
    { Copy-in refuses links, so this one was planted during the run: the
      write is refused and the write-back fails. }
    Expect<Boolean>(FInputs.ApplyWriteBack(Plan, Report)).ToBe(False);
    Expect<string>(Report[0]).ToBe('write-back: ' + HostPath('out/sub/file.txt') +
      ' is a symlink or resolves outside its input; refused');
    Expect<string>(ReadHostText('elsewhere/file.txt')).ToBe('untouched');
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;


procedure TSandboxHostInputsTests.TestWriteBackRefusesReplacedRoot;
{$IFDEF UNIX}
var
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
{$ENDIF}
begin
  {$IFDEF UNIX}
  WriteHostFile('out/a.txt', 'orig');
  ForceDirectories(HostPath('target'));
  FInputs.CopyIn(HostPath('out'), '/out', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/a.txt', 'PWNED');
    FFs.WriteAllText('/out/b.txt', 'PWNED2');
    { The copied directory is moved away and a link to elsewhere takes its
      name while the run goes on. }
    Expect<Boolean>(RenameFile(HostPath('out'), HostPath('out.real')))
      .ToBe(True);
    Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('target'))),
      PAnsiChar(AnsiString(HostPath('out'))))).ToBe(0);
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('target/a.txt'))).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('target/b.txt'))).ToBe(False);
    Expect<string>(ReadHostText('out.real/a.txt')).ToBe('orig');
    Expect<Boolean>(Report.IndexOf('write-back: ' + HostPath('out') +
      ' was replaced during the run; nothing written') >= 0).ToBe(True);
    Expect<string>(Report[Report.Count - 1]).ToBe(
      'write-back: 0 file(s) written, 2 skipped');
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestWriteBackRefusesDirectorySwappedAfterPlan;
{$IFDEF UNIX}
var
  Baseline: TSandboxVirtualFileSystem;
  Plan: TSandboxWriteBackPlan;
  Report: TStringList;
{$ENDIF}
begin
  {$IFDEF UNIX}
  WriteHostFile('out/sub/s.txt', 'orig');
  ForceDirectories(HostPath('elsewhere'));
  FInputs.CopyIn(HostPath('out'), '/out', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/sub/s.txt', 'OVERWRITE');
    FFs.MakeDirectory('/out/fresh', True);
    FFs.WriteAllText('/out/fresh/new.txt', 'NEW');
    Plan := FInputs.PlanWriteBack(Baseline);
    Expect<Boolean>(Plan[0].Action = swaWrite).ToBe(True);
    Expect<Boolean>(Plan[1].Action = swaWrite).ToBe(True);
    { Between the plan and the write: a directory on the way becomes a
      link, and a directory the write would create appears as one. }
    DeleteFile(HostPath('out/sub/s.txt'));
    RemoveDir(HostPath('out/sub'));
    Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('elsewhere'))),
      PAnsiChar(AnsiString(HostPath('out/sub'))))).ToBe(0);
    Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('elsewhere'))),
      PAnsiChar(AnsiString(HostPath('out/fresh'))))).ToBe(0);
    Expect<Boolean>(FInputs.ApplyWriteBack(Plan, Report)).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('elsewhere/s.txt'))).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('elsewhere/new.txt'))).ToBe(False);
    Expect<string>(Report[Report.Count - 1]).ToBe(
      'write-back: 0 file(s) written, 2 skipped');
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestWriteBackRefusesFileInputDirectoryReplaced;
{$IFDEF UNIX}
var
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
{$ENDIF}
begin
  {$IFDEF UNIX}
  WriteHostFile('dir/f.txt', 'orig');
  WriteHostFile('target/f.txt', 'secret');
  FInputs.CopyIn(HostPath('dir/f.txt'), '/f.txt', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/f.txt', 'OVERWRITE');
    Expect<Boolean>(RenameFile(HostPath('dir'), HostPath('dir.real')))
      .ToBe(True);
    Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('target'))),
      PAnsiChar(AnsiString(HostPath('dir'))))).ToBe(0);
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<string>(ReadHostText('target/f.txt')).ToBe('secret');
    Expect<string>(ReadHostText('dir.real/f.txt')).ToBe('orig');
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

function TSandboxHostInputsTests.PinUnderRoot(
  const ARelativePath: string): TSandboxHostPin;
var
  Problem: string;
begin
  Expect<Boolean>(TryPinBeneath(FRoot, HostPath(ARelativePath), Result,
    Problem)).ToBe(True);
end;

procedure TSandboxHostInputsTests.TestOutputFileRefusesReplacedDirectory;
var
  Output: TSandboxHostOutputFile;
  Bytes: TBytes;
  Problem: string;
begin
  ForceDirectories(HostPath('dd'));
  ForceDirectories(HostPath('victim'));
  Bytes := TEncoding.UTF8.GetBytes('{}');
  Output := TSandboxHostOutputFile.Create(HostPath('dd/nested/diff.json'),
    PinUnderRoot('dd/nested/diff.json'));
  Expect<Boolean>(Output.Write(Bytes, Problem)).ToBe(True);
  Expect<string>(ReadHostText('dd/nested/diff.json')).ToBe('{}');
  {$IFDEF UNIX}
  Output := TSandboxHostOutputFile.Create(HostPath('dd/diff.json'),
    PinUnderRoot('dd/diff.json'));
  Expect<Boolean>(RenameFile(HostPath('dd'), HostPath('dd.real')))
    .ToBe(True);
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('victim'))),
    PAnsiChar(AnsiString(HostPath('dd'))))).ToBe(0);
  Expect<Boolean>(Output.Write(Bytes, Problem)).ToBe(False);
  Expect<string>(Problem).ToBe('dd is a symbolic link or not a directory');
  Expect<Boolean>(FileExists(HostPath('victim/diff.json'))).ToBe(False);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestOutputFileRefusesLeafSymlink;
{$IFDEF UNIX}
var
  Output: TSandboxHostOutputFile;
  Problem: string;
{$ENDIF}
begin
  {$IFDEF UNIX}
  WriteHostFile('victim.txt', 'keep');
  ForceDirectories(HostPath('dd'));
  Output := TSandboxHostOutputFile.Create(HostPath('dd/diff.json'),
    PinUnderRoot('dd/diff.json'));
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('victim.txt'))),
    PAnsiChar(AnsiString(HostPath('dd/diff.json'))))).ToBe(0);
  Expect<Boolean>(Output.Write(TEncoding.UTF8.GetBytes('{}'), Problem))
    .ToBe(False);
  Expect<string>(Problem).ToBe(
    'the target is a symbolic link or cannot be opened');
  Expect<string>(ReadHostText('victim.txt')).ToBe('keep');
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestPinRefusesPathOutsideRoot;
var
  Pin: TSandboxHostPin;
  Problem: string;
begin
  ForceDirectories(HostPath('config'));
  ForceDirectories(HostPath('outside'));
  Expect<Boolean>(TryPinBeneath(HostPath('config'), HostPath('outside/x'),
    Pin, Problem)).ToBe(False);
  { The root as the host spells it canonically (on Windows the long form of
    a short temp directory name). }
  Expect<string>(Problem).ToBe('is outside ' + ExcludeTrailingPathDelimiter(
    CanonicalHostPath(HostPath('config'))));
  Expect<Boolean>(TryPinBeneath(HostPath('config'),
    HostPath('config/new/deeper/x.json'), Pin, Problem)).ToBe(True);
  Expect<string>(Pin.Route).ToBe('new' + PathDelim + 'deeper' + PathDelim +
    'x.json');
  {$IFDEF UNIX}
  { A link inside the root that leads out is judged where it leads. }
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('outside'))),
    PAnsiChar(AnsiString(HostPath('config/away'))))).ToBe(0);
  Expect<Boolean>(TryPinBeneath(HostPath('config'), HostPath('config/away/x'),
    Pin, Problem)).ToBe(False);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestPinnedInputRefusesAncestorSwappedBeforeCopy;
{$IFDEF UNIX}
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
{$ENDIF}
begin
  {$IFDEF UNIX}
  { The config is checked and the input pinned; then, before the input is
    even copied (a long copy of another input), an ancestor on its route is
    swapped for a link out of the config's directory. }
  WriteHostFile('project/a/out/f.txt', 'orig');
  WriteHostFile('evil/out/f.txt', 'evil-orig');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/a/out'),
    Pin, Problem)).ToBe(True);
  Expect<Boolean>(RenameFile(HostPath('project/a'), HostPath('project/a.real')))
    .ToBe(True);
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('evil'))),
    PAnsiChar(AnsiString(HostPath('project/a'))))).ToBe(0);
  FInputs.CopyIn(HostPath('project/a/out'), '/out', True, Pin);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/f.txt', 'PWNED');
    FFs.WriteAllText('/out/planted.sh', 'echo planted');
    { The route led nowhere link-free just before the copy, so there is no
      directory to compare against: nothing is written, even if the route is
      restored before write-back. }
    Expect<Boolean>(RenameFile(HostPath('project/a'), HostPath('evil.link')))
      .ToBe(True);
    Expect<Boolean>(RenameFile(HostPath('project/a.real'),
      HostPath('project/a'))).ToBe(True);
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<string>(ReadHostText('evil/out/f.txt')).ToBe('evil-orig');
    Expect<Boolean>(FileExists(HostPath('evil/out/planted.sh'))).ToBe(False);
    Expect<string>(ReadHostText('project/a/out/f.txt')).ToBe('orig');
    Expect<Boolean>(FileExists(HostPath('project/a/out/planted.sh')))
      .ToBe(False);
    Expect<Boolean>(Report.IndexOf('write-back: ' + HostPath('project/a/out') +
      ' could not be pinned when it was copied (a is a symbolic link or not ' +
      'a directory); nothing written') >= 0).ToBe(True);
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestPinnedInputWritesAlongItsRoute;
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
begin
  WriteHostFile('project/a/out/f.txt', 'orig');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/a/out'),
    Pin, Problem)).ToBe(True);
  FInputs.CopyIn(HostPath('project/a/out'), '/out', True, Pin);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/f.txt', 'NEW');
    FFs.MakeDirectory('/out/sub', True);
    FFs.WriteAllText('/out/sub/g.txt', 'G');
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(True);
    Expect<string>(ReadHostText('project/a/out/f.txt')).ToBe('NEW');
    Expect<string>(ReadHostText('project/a/out/sub/g.txt')).ToBe('G');
  finally
    Report.Free;
    Baseline.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestCommandLineOutputWritesAsNamed;
begin
  { A regular file is replaced; a missing one is created. }
  WriteHostFile('out.json', 'old');
  WriteCommandLineOutputFile(HostPath('out.json'),
    TEncoding.UTF8.GetBytes('new'));
  Expect<string>(ReadHostText('out.json')).ToBe('new');
  WriteCommandLineOutputFile(HostPath('fresh.json'),
    TEncoding.UTF8.GetBytes('fresh'));
  Expect<string>(ReadHostText('fresh.json')).ToBe('fresh');
  {$IFDEF UNIX}
  { What the user named, even a device or a link, is written as named. }
  WriteCommandLineOutputFile('/dev/null', TEncoding.UTF8.GetBytes('x'));
  WriteHostFile('real.json', 'old');
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('real.json'))),
    PAnsiChar(AnsiString(HostPath('link.json'))))).ToBe(0);
  WriteCommandLineOutputFile(HostPath('link.json'),
    TEncoding.UTF8.GetBytes('through'));
  Expect<string>(ReadHostText('real.json')).ToBe('through');
  Expect<Boolean>(HostPathIsSymlink(HostPath('link.json'))).ToBe(True);
  {$ENDIF}
end;


procedure TSandboxHostInputsTests.TestCommandLineOutputNeverWaitsOnAFifo;
{$IFDEF UNIX}
var
  Message: string;
{$ENDIF}
begin
  {$IFDEF UNIX}
  { Opened for reading, a FIFO would wait for a writer; opened to write,
    for a reader. Nobody reads this one, so the write fails at once. }
  Expect<Integer>(FpMkfifo(PAnsiChar(AnsiString(HostPath('fifo'))), $1B6))
    .ToBe(0);
  Message := '';
  try
    WriteCommandLineOutputFile(HostPath('fifo'), TEncoding.UTF8.GetBytes('x'));
  except
    on E: EInOutError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe(HostPath('fifo') +
    ': no process is reading this FIFO');
  {$ELSE}
  { Windows has no FIFOs in the filesystem. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestPinnedInputReplacedByADirectoryWritesNothing;
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
begin
  WriteHostFile('project/out/f.txt', 'orig');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/out'),
    Pin, Problem)).ToBe(True);
  FInputs.CopyIn(HostPath('project/out'), '/out', True, Pin);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/f.txt', 'NEW');
    { During the run the input is moved away and a new, real directory takes
      its name: the route still leads to a directory, but not the one that
      was copied. }
    Expect<Boolean>(RenameFile(HostPath('project/out'),
      HostPath('project/out.real'))).ToBe(True);
    ForceDirectories(HostPath('project/out'));
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('project/out/f.txt'))).ToBe(False);
    Expect<string>(ReadHostText('project/out.real/f.txt')).ToBe('orig');
    Expect<Boolean>(Report.IndexOf('write-back: ' + HostPath('project/out') +
      ' was replaced during the run; nothing written') >= 0).ToBe(True);
  finally
    Report.Free;
    Baseline.Free;
  end;
end;


procedure TSandboxHostInputsTests.TestInputReplacedDuringCopyWritesNothing;
var
  Clock: TSwapDirectoryClock;
  Fs, Baseline: TSandboxVirtualFileSystem;
  Inputs: TSandboxHostInputs;
  Report: TStringList;
begin
  WriteHostFile('out/a.txt', 'orig');
  Clock := TSwapDirectoryClock.Create;
  Fs := TSandboxVirtualFileSystem.Create(SWAP_TEST_QUOTA_BYTES, Clock);
  Inputs := TSandboxHostInputs.Create(Fs);
  Baseline := nil;
  Report := TStringList.Create;
  try
    { The input is moved away and a new directory takes its name after the
      copy has begun: write-back must compare against the directory the copy
      started from, not the one that had the name when the copy ended. }
    { With /out already there, the first tick is the file write, which comes
      after the host file was read: the swap lands between reading the input
      and the end of the copy. }
    Fs.MakeDirectory('/out', True);
    Clock.Arm(HostPath('out'));
    {$IFDEF UNIX}
    Inputs.CopyIn(HostPath('out'), '/out', True);
    {$ELSE}
    { Windows will not rename a directory while the copy is listing it, so
      there the file is copied on its own and its directory is swapped once
      the file has been read and closed. }
    Inputs.CopyIn(HostPath('out/a.txt'), '/out/', True);
    {$ENDIF}
    Expect<Boolean>(DirectoryExists(HostPath('out.real'))).ToBe(True);
    Expect<string>(Fs.ReadAllText('/out/a.txt')).ToBe('orig');
    Baseline := Fs.Fork;
    Fs.WriteAllText('/out/a.txt', 'NEW');
    Expect<Boolean>(Inputs.ApplyWriteBack(Inputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('out/a.txt'))).ToBe(False);
    Expect<string>(ReadHostText('out.real/a.txt')).ToBe('orig');
    Expect<Boolean>(Report.IndexOf('write-back: ' + HostPath('out') +
      ' was replaced during the run; nothing written') >= 0).ToBe(True);
  finally
    Report.Free;
    Baseline.Free;
    Inputs.Free;
    Fs.Free;
    Clock.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestPinnedInputReplacedDuringCopyWritesNothing;
var
  Clock: TSwapDirectoryClock;
  Fs, Baseline: TSandboxVirtualFileSystem;
  Inputs: TSandboxHostInputs;
  Report: TStringList;
  Pin: TSandboxHostPin;
  Problem: string;
begin
  WriteHostFile('project/out/a.txt', 'orig');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/out'),
    Pin, Problem)).ToBe(True);
  Clock := TSwapDirectoryClock.Create;
  Fs := TSandboxVirtualFileSystem.Create(SWAP_TEST_QUOTA_BYTES, Clock);
  Inputs := TSandboxHostInputs.Create(Fs);
  Baseline := nil;
  Report := TStringList.Create;
  try
    Fs.MakeDirectory('/out', True);
    Clock.Arm(HostPath('project/out'));
    {$IFDEF UNIX}
    Inputs.CopyIn(HostPath('project/out'), '/out', True, Pin);
    {$ELSE}
    { As in TestInputReplacedDuringCopyWritesNothing: a file input, pinned at
      the same directory. }
    Inputs.CopyIn(HostPath('project/out/a.txt'), '/out/', True, Pin);
    {$ENDIF}
    Expect<Boolean>(DirectoryExists(HostPath('project/out.real'))).ToBe(True);
    Expect<string>(Fs.ReadAllText('/out/a.txt')).ToBe('orig');
    Baseline := Fs.Fork;
    Fs.WriteAllText('/out/a.txt', 'NEW');
    Expect<Boolean>(Inputs.ApplyWriteBack(Inputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('project/out/a.txt'))).ToBe(False);
    Expect<string>(ReadHostText('project/out.real/a.txt')).ToBe('orig');
    Expect<Boolean>(Report.IndexOf('write-back: ' + HostPath('project/out') +
      ' was replaced during the run; nothing written') >= 0).ToBe(True);
  finally
    Report.Free;
    Baseline.Free;
    Inputs.Free;
    Fs.Free;
    Clock.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestConfigFileInputTurnedDirectoryWritesNothing;
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
begin
  { Checked as a file, so pinned at its directory, project/a; by the copy it
    is a directory. Its paths must not be written under project/a. }
  WriteHostFile('project/a/out', 'a file');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/a'),
    Pin, Problem)).ToBe(True);
  Expect<Boolean>(DeleteFile(HostPath('project/a/out'))).ToBe(True);
  WriteHostFile('project/a/out/f.txt', 'orig');
  FInputs.CopyIn(HostPath('project/a/out'), '/out', True, Pin);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/f.txt', 'NEW');
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<Boolean>(FileExists(HostPath('project/a/f.txt'))).ToBe(False);
    Expect<string>(ReadHostText('project/a/out/f.txt')).ToBe('orig');
    Expect<Boolean>(Report.IndexOf('write-back: ' + HostPath('project/a/out') +
      ' could not be pinned when it was copied (it is not the directory its ' +
      'config entry was checked against); nothing written') >= 0).ToBe(True);
  finally
    Report.Free;
    Baseline.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestUnpinnedInputStopsAllWriteBack;
{$IFDEF UNIX}
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
{$ENDIF}
begin
  {$IFDEF UNIX}
  { One config input's route is a link from before the copy until after the
    run; another input is fine. A half-applied write-back is worse than
    none, so neither is written. }
  WriteHostFile('project/a/out/f.txt', 'orig');
  WriteHostFile('evil/out/f.txt', 'evil-orig');
  WriteHostFile('other/g.txt', 'orig');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/a/out'),
    Pin, Problem)).ToBe(True);
  Expect<Boolean>(RenameFile(HostPath('project/a'), HostPath('project/a.real')))
    .ToBe(True);
  Expect<Integer>(FpSymlink(PAnsiChar(AnsiString(HostPath('evil'))),
    PAnsiChar(AnsiString(HostPath('project/a'))))).ToBe(0);
  FInputs.CopyIn(HostPath('project/a/out'), '/out', True, Pin);
  FInputs.CopyIn(HostPath('other'), '/other', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/f.txt', 'PWNED');
    FFs.WriteAllText('/other/g.txt', 'NEW');
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    Expect<string>(ReadHostText('evil/out/f.txt')).ToBe('evil-orig');
    Expect<string>(ReadHostText('project/a.real/out/f.txt')).ToBe('orig');
    Expect<string>(ReadHostText('other/g.txt')).ToBe('orig');
    Expect<string>(Report[Report.Count - 1]).ToBe(
      'write-back: 0 file(s) written, 2 skipped');
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ELSE}
  { The scenario needs symbolic links, which Windows creates only with
    elevated rights. }
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TSandboxHostInputsTests.TestDirectoryIdentityTellsDirectoriesApart;
var
  First, Again, Other: THostDirectoryIdentity;
begin
  ForceDirectories(HostPath('one'));
  ForceDirectories(HostPath('two'));
  Expect<Boolean>(TryHostDirectoryIdentity(HostPath('one'), First)).ToBe(True);
  Expect<Boolean>(TryHostDirectoryIdentity(HostPath('one'), Again)).ToBe(True);
  Expect<Boolean>(TryHostDirectoryIdentity(HostPath('two'), Other)).ToBe(True);
  { POSIX stat and Windows FileIdInfo both answer on the test hosts. }
  Expect<Boolean>(First.Known).ToBe(True);
  Expect<Boolean>(First.Unavailable).ToBe(False);
  Expect<Boolean>(SameDirectoryIdentity(Again, First)).ToBe(True);
  Expect<Boolean>(SameDirectoryIdentity(Other, First)).ToBe(False);
  Expect<string>(HostDirectoryIdentityProblem(HostPath('one'), First)).ToBe('');
end;

procedure TSandboxHostInputsTests.TestUnavailableIdentityMatchesNothing;
var
  Unidentified, Known, Unknown: THostDirectoryIdentity;
begin
  ForceDirectories(HostPath('one'));
  Expect<Boolean>(TryHostDirectoryIdentity(HostPath('one'), Known)).ToBe(True);
  Unidentified := Default(THostDirectoryIdentity);
  Unidentified.Unavailable := True;
  Unknown := Default(THostDirectoryIdentity);
  { Unlike an identity the host cannot give at all (Known False), which
    matches anything, an unavailable one matches nothing, even itself. }
  Expect<Boolean>(SameDirectoryIdentity(Known, Unknown)).ToBe(True);
  Expect<Boolean>(SameDirectoryIdentity(Known, Unidentified)).ToBe(False);
  Expect<Boolean>(SameDirectoryIdentity(Unidentified, Known)).ToBe(False);
  Expect<Boolean>(SameDirectoryIdentity(Unidentified, Unidentified))
    .ToBe(False);
  Expect<Boolean>(Pos('Windows gives no file ID for ' + HostPath('one'),
    HostDirectoryIdentityProblem(HostPath('one'), Unidentified)) = 1)
    .ToBe(True);
end;

procedure TSandboxHostInputsTests.TestOutputFileRefusesUnidentifiedDirectory;
var
  Pin: TSandboxHostPin;
  Output: TSandboxHostOutputFile;
  Problem: string;
begin
  ForceDirectories(HostPath('dd'));
  Pin := PinUnderRoot('dd/diff.json');
  { As when GetFileInformationByHandleEx(FileIdInfo) fails for the root. }
  Pin.RootIdentity := Default(THostDirectoryIdentity);
  Pin.RootIdentity.Unavailable := True;
  Output := TSandboxHostOutputFile.Create(HostPath('dd/diff.json'), Pin);
  Expect<Boolean>(Output.Write(TEncoding.UTF8.GetBytes('{}'), Problem))
    .ToBe(False);
  Expect<Boolean>(Pos('GetFileInformationByHandleEx FileIdInfo failed',
    Problem) > 0).ToBe(True);
  Expect<Boolean>(FileExists(HostPath('dd/diff.json'))).ToBe(False);
end;

procedure TSandboxHostInputsTests.TestUnidentifiedInputGetsNothingWritten;
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
  I: Integer;
  Refused: Boolean;
begin
  WriteHostFile('project/out/f.txt', 'orig');
  WriteHostFile('other/g.txt', 'orig');
  Expect<Boolean>(TryPinBeneath(HostPath('project'), HostPath('project/out'),
    Pin, Problem)).ToBe(True);
  Pin.RootIdentity := Default(THostDirectoryIdentity);
  Pin.RootIdentity.Unavailable := True;
  FInputs.CopyIn(HostPath('project/out'), '/out', True, Pin);
  FInputs.CopyIn(HostPath('other'), '/other', True);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/out/f.txt', 'NEW');
    FFs.WriteAllText('/other/g.txt', 'NEW');
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(False);
    { The unidentified input is refused with the reason; the other input is
      still written. }
    Expect<string>(ReadHostText('project/out/f.txt')).ToBe('orig');
    Expect<string>(ReadHostText('other/g.txt')).ToBe('NEW');
    Refused := False;
    for I := 0 to Report.Count - 1 do
      if (Pos('write-back: ' + HostPath('project/out/f.txt') + ': Windows ' +
         'gives no file ID for ', Report[I]) = 1) then
        Refused := True;
    Expect<Boolean>(Refused).ToBe(True);
  finally
    Report.Free;
    Baseline.Free;
  end;
end;


{ The root of the filesystem the test files live on: `/`, or on Windows that
  drive's root, such as `C:\`. It is taken from the canonical path, which is
  the one a pin compares against, so a temporary directory that reaches
  another drive through a junction still lies under it. Nothing is written
  at the root itself; it is only the expected answer. }
function TSandboxHostInputsTests.FilesystemRoot: string;
var
  Canonical: string;
begin
  Canonical := CanonicalHostPath(FRoot);
  if Canonical = '' then
    Canonical := FRoot;
  Result := IncludeTrailingPathDelimiter(ExtractFileDrive(Canonical));
end;

procedure TSandboxHostInputsTests.TestInputDirectlyUnderARootHasTheRootAsItsDirectory;
var
  Origin: TSandboxHostOrigin;
begin
  { Stripping the separator from the root would leave `''` (or `C:`), which
    name the working directory (or the drive's): a file written back there
    would replace one the user never named. }
  Origin := Default(TSandboxHostOrigin);
  Origin.HostPath := FilesystemRoot + 'f.txt';
  Origin.IsDirectory := False;
  Expect<string>(OriginRootPath(Origin)).ToBe(FilesystemRoot);
  Origin.HostPath := FilesystemRoot;
  Origin.IsDirectory := True;
  Expect<string>(OriginRootPath(Origin)).ToBe(FilesystemRoot);
  Origin.HostPath := HostPath('in/f.txt');
  Origin.IsDirectory := False;
  Expect<string>(OriginRootPath(Origin)).ToBe(HostPath('in'));
end;

function TRecordingHostInputs.RecordOrigin(const AHostPath,
  ASandboxPath: string; const AIsDirectory: Boolean;
  const APin: TSandboxHostPin): TSandboxHostOrigin;
begin
  Result := TakeOrigin(AHostPath, ASandboxPath, AIsDirectory, True, APin);
  AddOrigin(Result);
end;

procedure TSandboxHostInputsTests.TestCopiedRootMapsItsFilesBothWays;
var
  Inputs: TRecordingHostInputs;
  Origin: TSandboxHostOrigin;
  Baseline: TSandboxVirtualFileSystem;
  Plan: TSandboxWriteBackPlan;
  Canonical, Below, SandboxPath, Path: string;
begin
  { As `--copy-rw /=/`: an entry file under the root runs at its path below
    the root, and a file the run changes maps back to the same host path. }
  WriteHostFile('in/d.js', 'entry');
  Canonical := CanonicalHostPath(HostPath('in/d.js'));
  Below := Copy(Canonical, Length(FilesystemRoot) + 1, MaxInt);
  Inputs := TRecordingHostInputs.Create(FFs);
  Baseline := nil;
  try
    Origin := Inputs.RecordOrigin(FilesystemRoot, '/', True,
      Default(TSandboxHostPin));
    Expect<string>(Origin.HostPath).ToBe(FilesystemRoot);
    Expect<string>(Origin.WriteRoot).ToBe(FilesystemRoot);
    Expect<string>(Origin.IdentityProblem).ToBe('');

    Expect<Boolean>(Inputs.SandboxPathOfHostFile(HostPath('in/d.js'),
      SandboxPath)).ToBe(True);
    Expect<string>(SandboxPath).ToBe('/' + StringReplace(Below, PathDelim,
      '/', [rfReplaceAll]));
    Expect<Boolean>(Inputs.HostPathForSandboxPath(SandboxPath, Origin, Path))
      .ToBe(True);
    Expect<string>(Path).ToBe(Canonical);

    Baseline := FFs.Fork;
    FFs.MakeDirectory(Copy(SandboxPath, 1, LastDelimiter('/', SandboxPath) -
      1), True);
    FFs.WriteAllText(SandboxPath, 'changed');
    Plan := Inputs.PlanWriteBack(Baseline);
    Expect<Integer>(Length(Plan)).ToBe(1);
    Expect<string>(Plan[0].HostPath).ToBe(Canonical);
    Expect<string>(Plan[0].RelativePath).ToBe(Below);
    Expect<Boolean>(Plan[0].Action = swaWrite).ToBe(True);
  finally
    Baseline.Free;
    Inputs.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestPinAtARootItselfIdentifiesTheRoot;
var
  Inputs: TRecordingHostInputs;
  Pin: TSandboxHostPin;
  Problem: string;
  Origin: TSandboxHostOrigin;
begin
  { A config at the root naming the root, or a file directly under it: the
    pin's route is empty and leads to the root itself. }
  Expect<Boolean>(TryPinBeneath(FilesystemRoot, FilesystemRoot, Pin,
    Problem)).ToBe(True);
  Expect<string>(Pin.Root).ToBe(FilesystemRoot);
  Expect<string>(Pin.Route).ToBe('');
  Inputs := TRecordingHostInputs.Create(FFs);
  try
    Origin := Inputs.RecordOrigin(FilesystemRoot, '/', True, Pin);
    Expect<string>(Origin.IdentityProblem).ToBe('');
    Origin := Inputs.RecordOrigin(FilesystemRoot + 'f.txt', '/f.txt', False,
      Pin);
    Expect<string>(Origin.IdentityProblem).ToBe('');
    Expect<string>(OriginRootPath(Origin)).ToBe(FilesystemRoot);
  finally
    Inputs.Free;
  end;
end;

procedure TSandboxHostInputsTests.TestPathBelowARootKeepsItsFirstCharacter;
begin
  { Copy-in looks up an entry file under a copied root, write-back writes
    under a root, and a pin records its route from one: counting a separator
    after the root as well as the root's own would drop `w` from `work`. }
  Expect<string>(HostPathBelow(FilesystemRoot + 'work' + PathDelim + 'd.js',
    FilesystemRoot)).ToBe('work' + PathDelim + 'd.js');
  Expect<string>(HostPathBelow(FilesystemRoot + 'f.txt', FilesystemRoot))
    .ToBe('f.txt');
  Expect<string>(HostPathBelow(FilesystemRoot, FilesystemRoot)).ToBe('');
  Expect<string>(HostPathBelow(HostPath('in/sub/f.txt'), HostPath('in')))
    .ToBe('sub' + PathDelim + 'f.txt');
  Expect<string>(HostPathBelow(HostPath('in'), HostPath('in'))).ToBe('');
end;

procedure TSandboxHostInputsTests.TestInputPinnedAtARootWritesBackUnderIt;
var
  Pin: TSandboxHostPin;
  Problem: string;
  Baseline: TSandboxVirtualFileSystem;
  Report: TStringList;
begin
  WriteHostFile('rw/f.txt', 'orig');
  Expect<Boolean>(TryPinBeneath(FilesystemRoot, HostPath('rw'), Pin,
    Problem)).ToBe(True);
  Expect<string>(Problem).ToBe('');
  Expect<string>(Pin.Root).ToBe(FilesystemRoot);
  Expect<string>(IncludeTrailingPathDelimiter(Pin.Root) + Pin.Route).ToBe(
    CanonicalHostPath(HostPath('rw')));
  FInputs.CopyIn(HostPath('rw/f.txt'), '/f.txt', True, Pin);
  Baseline := FFs.Fork;
  Report := TStringList.Create;
  try
    FFs.WriteAllText('/f.txt', 'NEW');
    Expect<Boolean>(FInputs.ApplyWriteBack(FInputs.PlanWriteBack(Baseline),
      Report)).ToBe(True);
    Expect<string>(ReadHostText('rw/f.txt')).ToBe('NEW');
  finally
    Report.Free;
    Baseline.Free;
  end;
end;

begin
  Randomize;
  TestRunnerProgram.AddSuite(TSandboxHostInputsTests.Create(
    'Sandbox Host Inputs'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
