program FileUtils.Test;

{$I Shared.inc}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  TestingPascalLibrary;

type
  TFileUtilsTests = class(TTestSuite)
  private
    FTempDir: string;
    procedure CreateTempFile(const ARelPath: string);
    procedure CreateTempDir(const ARelPath: string);

    procedure TestFindsFilesWithMatchingExtension;
    procedure TestIgnoresNonMatchingExtension;
    procedure TestRecursivelyFindsInSubdirectories;
    procedure TestMultipleExtensionsFilter;
    procedure TestEmptyDirectoryReturnsEmpty;
    procedure TestResultsAreSorted;
    procedure TestNoPartialExtensionMatch;
    procedure TestDirectoryWithOnlySubdirsReturnsEmpty;
    procedure TestDeeplyNestedThreeLevels;
    procedure TestSingleExtensionOverloadDelegatesToMulti;
    procedure TestTrailingPathDelimiterHandled;
    procedure TestMultipleFilesInMultipleSubdirs;
    procedure TestNoMatchingFilesAmongMany;
    procedure TestMixedExtensionsAcrossDepths;
    procedure TestIsAbsoluteHostPathRootedForms;
    procedure TestIsAbsoluteHostPathRelativeForms;
    procedure TestCanonicalHostPathIsUnknownForAMissingPath;
    procedure TestCanonicalHostPathIsStableForARealFile;
    procedure TestCanonicalHostPathFollowsASymlink;
    procedure TestReplaceHostFileCreatesWithPermissions;
    procedure TestReadSharedHostFileBytes;
    procedure TestHostPathIsRegularFile;
    procedure TestReadHostHandleBytesReadsFromTheStart;
    procedure TestRemoveHostTreeBeneathRemovesATree;
    procedure TestRemoveHostTreeBeneathDoesNotFollowLinks;
    procedure TestTryHostFileMode;
    procedure TestRemoveHostTreeBeneathRefusesALastDotDot;
    procedure TestRemoveHostTreeBeneathRemovesJunctionsItself;
    procedure TestReplaceWhileASharedReaderHoldsTheFile;
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
  end;

procedure TFileUtilsTests.SetupTests;
begin
  Test('Finds files with matching extension in flat directory', TestFindsFilesWithMatchingExtension);
  Test('Ignores files with non-matching extension', TestIgnoresNonMatchingExtension);
  Test('Recursively finds files in nested subdirectories', TestRecursivelyFindsInSubdirectories);
  Test('Multiple extensions filter works correctly', TestMultipleExtensionsFilter);
  Test('Empty directory returns empty list', TestEmptyDirectoryReturnsEmpty);
  Test('Results are sorted alphabetically by full path', TestResultsAreSorted);
  Test('No partial extension matches (.pas does not match .pas2)', TestNoPartialExtensionMatch);
  Test('Directory with only subdirs and no files returns empty', TestDirectoryWithOnlySubdirsReturnsEmpty);
  Test('Finds files in deeply nested directories (3+ levels)', TestDeeplyNestedThreeLevels);
  Test('Single extension overload delegates to multi-extension', TestSingleExtensionOverloadDelegatesToMulti);
  Test('Trailing path delimiter on directory is handled', TestTrailingPathDelimiterHandled);
  Test('Multiple files across multiple subdirectories', TestMultipleFilesInMultipleSubdirs);
  Test('No matching files among many non-matching returns empty', TestNoMatchingFilesAmongMany);
  Test('Mixed extensions across various depths', TestMixedExtensionsAcrossDepths);
  Test('IsAbsoluteHostPath accepts the platform''s rooted spellings',
    TestIsAbsoluteHostPathRootedForms);
  Test('IsAbsoluteHostPath rejects paths read against a working directory',
    TestIsAbsoluteHostPathRelativeForms);
  Test('CanonicalHostPath reports unknown for a path that does not exist',
    TestCanonicalHostPathIsUnknownForAMissingPath);
  Test('CanonicalHostPath is stable for a file that does exist',
    TestCanonicalHostPathIsStableForARealFile);
  { Creating a symlink needs an API this build only has on UNIX. }
  {$IFDEF UNIX}
  Test('CanonicalHostPath resolves a symlink to its target',
    TestCanonicalHostPathFollowsASymlink);
  {$ELSE}
  Skip('CanonicalHostPath resolves a symlink to its target',
    TestCanonicalHostPathFollowsASymlink,
    'creating a symlink is not available on this platform');
  {$ENDIF}
  {$IFDEF UNIX}
  Test('ReplaceHostFile creates the temporary with the given permissions',
    TestReplaceHostFileCreatesWithPermissions);
  {$ELSE}
  Skip('ReplaceHostFile creates the temporary with the given permissions',
    TestReplaceHostFileCreatesWithPermissions,
    'POSIX modes are not available on this platform');
  {$ENDIF}
  Test('ReadSharedHostFileBytes reads the whole file',
    TestReadSharedHostFileBytes);
  Test('HostPathIsRegularFile is true only for a regular file itself',
    TestHostPathIsRegularFile);
  Test('ReadHostHandleBytes reads the whole file through its handle',
    TestReadHostHandleBytesReadsFromTheStart);
  Test('RemoveHostTreeBeneath removes a tree and refuses to climb',
    TestRemoveHostTreeBeneathRemovesATree);
  Test('RemoveHostTreeBeneath refuses a path ending in ..',
    TestRemoveHostTreeBeneathRefusesALastDotDot);
  {$IFDEF MSWINDOWS}
  Test('RemoveHostTreeBeneath removes junctions without entering them',
    TestRemoveHostTreeBeneathRemovesJunctionsItself);
  {$ENDIF}
  {$IFDEF UNIX}
  Test('RemoveHostTreeBeneath removes links without following them',
    TestRemoveHostTreeBeneathDoesNotFollowLinks);
  Test('TryHostFileMode reads the permission bits', TestTryHostFileMode);
  {$ENDIF}
  { Share modes exist only on Windows; POSIX renames over open files. }
  {$IFDEF MSWINDOWS}
  Test('ReplaceHostFile replaces a file a shared reader holds open',
    TestReplaceWhileASharedReaderHoldsTheFile);
  {$ELSE}
  Skip('ReplaceHostFile replaces a file a shared reader holds open',
    TestReplaceWhileASharedReaderHoldsTheFile,
    'share modes exist only on Windows');
  {$ENDIF}
end;

procedure TFileUtilsTests.BeforeEach;
begin
  FTempDir := GetTempDir + 'goccia_fileutils_test_' + IntToStr(Random(MaxInt));
  ForceDirectories(FTempDir);
end;

procedure TFileUtilsTests.AfterEach;

  procedure RemoveTree(const ADir: string);
  var
    SR: TSearchRec;
    Path: string;
  begin
    if FindFirst(ADir + PathDelim + '*', faAnyFile, SR) = 0 then
    try
      repeat
        if (SR.Name = '.') or (SR.Name = '..') then
          Continue;
        Path := ADir + PathDelim + SR.Name;
        if (SR.Attr and faDirectory) <> 0 then
          RemoveTree(Path)
        else
          DeleteFile(Path);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
    RemoveDir(ADir);
  end;

begin
  if DirectoryExists(FTempDir) then
    RemoveTree(FTempDir);
end;

procedure TFileUtilsTests.CreateTempFile(const ARelPath: string);
var
  FullPath, Dir: string;
  F: TextFile;
begin
  FullPath := FTempDir + PathDelim + ARelPath;
  Dir := ExtractFileDir(FullPath);
  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);
  AssignFile(F, FullPath);
  Rewrite(F);
  CloseFile(F);
end;

procedure TFileUtilsTests.CreateTempDir(const ARelPath: string);
begin
  ForceDirectories(FTempDir + PathDelim + ARelPath);
end;

procedure TFileUtilsTests.TestFindsFilesWithMatchingExtension;
var
  Files: TStringList;
begin
  CreateTempFile('a.pas');
  CreateTempFile('b.pas');
  CreateTempFile('c.txt');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(2);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestIgnoresNonMatchingExtension;
var
  Files: TStringList;
begin
  CreateTempFile('readme.txt');
  CreateTempFile('notes.md');
  CreateTempFile('data.json');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(0);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestRecursivelyFindsInSubdirectories;
var
  Files: TStringList;
begin
  CreateTempFile('top.pas');
  CreateTempFile('sub' + PathDelim + 'nested.pas');
  CreateTempFile('sub' + PathDelim + 'deep' + PathDelim + 'deep.pas');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(3);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestMultipleExtensionsFilter;
var
  Files: TStringList;
begin
  CreateTempFile('unit.pas');
  CreateTempFile('project.dpr');
  CreateTempFile('include.inc');
  CreateTempFile('readme.txt');
  Files := FindAllFiles(FTempDir, ['.pas', '.dpr']);
  try
    Expect<Integer>(Files.Count).ToBe(2);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestEmptyDirectoryReturnsEmpty;
var
  Files: TStringList;
begin
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(0);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestResultsAreSorted;
var
  Files: TStringList;
  I: Integer;
  Sorted: Boolean;
begin
  CreateTempFile('z_file.pas');
  CreateTempFile('a_file.pas');
  CreateTempFile('m_file.pas');
  CreateTempFile('b_file.pas');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(4);
    Sorted := True;
    for I := 0 to Files.Count - 2 do
      if CompareStr(Files[I], Files[I + 1]) > 0 then
      begin
        Sorted := False;
        Break;
      end;
    Expect<Boolean>(Sorted).ToBe(True);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestNoPartialExtensionMatch;
var
  Files: TStringList;
begin
  // ExtractFileExt('file.pas2') returns '.pas2', not '.pas'
  CreateTempFile('file.pas');
  CreateTempFile('file.pas2');
  CreateTempFile('file.pascal');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(1);
    Expect<string>(ExtractFileName(Files[0])).ToBe('file.pas');
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestDirectoryWithOnlySubdirsReturnsEmpty;
var
  Files: TStringList;
begin
  CreateTempDir('subdir1');
  CreateTempDir('subdir2');
  CreateTempDir('subdir3');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(0);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestDeeplyNestedThreeLevels;
var
  Files: TStringList;
begin
  CreateTempFile('level1' + PathDelim + 'level2' + PathDelim + 'level3' + PathDelim + 'deep.pas');
  CreateTempFile('level1' + PathDelim + 'level2' + PathDelim + 'mid.pas');
  CreateTempFile('level1' + PathDelim + 'shallow.pas');
  CreateTempFile('root.pas');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(4);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestSingleExtensionOverloadDelegatesToMulti;
var
  SingleResult, MultiResult: TStringList;
  I: Integer;
begin
  CreateTempFile('a.js');
  CreateTempFile('b.js');
  CreateTempFile('c.txt');
  SingleResult := FindAllFiles(FTempDir, '.js');
  MultiResult := FindAllFiles(FTempDir, ['.js']);
  try
    Expect<Integer>(SingleResult.Count).ToBe(MultiResult.Count);
    Expect<Boolean>(SingleResult.Count > 0).ToBe(True);
    for I := 0 to SingleResult.Count - 1 do
      Expect<string>(SingleResult[I]).ToBe(MultiResult[I]);
  finally
    MultiResult.Free;
    SingleResult.Free;
  end;
end;

procedure TFileUtilsTests.TestTrailingPathDelimiterHandled;
var
  Files: TStringList;
begin
  CreateTempFile('test.pas');
  Files := FindAllFiles(FTempDir + PathDelim, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(1);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestMultipleFilesInMultipleSubdirs;
var
  Files: TStringList;
begin
  CreateTempFile('a' + PathDelim + 'one.js');
  CreateTempFile('a' + PathDelim + 'two.js');
  CreateTempFile('b' + PathDelim + 'three.js');
  CreateTempFile('b' + PathDelim + 'four.txt');
  CreateTempFile('c' + PathDelim + 'five.js');
  Files := FindAllFiles(FTempDir, '.js');
  try
    Expect<Integer>(Files.Count).ToBe(4);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestNoMatchingFilesAmongMany;
var
  Files: TStringList;
begin
  CreateTempFile('data.json');
  CreateTempFile('config.yaml');
  CreateTempFile('readme.md');
  CreateTempFile('sub' + PathDelim + 'style.css');
  Files := FindAllFiles(FTempDir, '.pas');
  try
    Expect<Integer>(Files.Count).ToBe(0);
  finally
    Files.Free;
  end;
end;

procedure TFileUtilsTests.TestMixedExtensionsAcrossDepths;
var
  Files: TStringList;
begin
  CreateTempFile('root.pas');
  CreateTempFile('root.dpr');
  CreateTempFile('root.txt');
  CreateTempFile('sub' + PathDelim + 'unit.pas');
  CreateTempFile('sub' + PathDelim + 'project.dpr');
  CreateTempFile('sub' + PathDelim + 'deep' + PathDelim + 'inner.pas');
  CreateTempFile('sub' + PathDelim + 'deep' + PathDelim + 'notes.txt');
  Files := FindAllFiles(FTempDir, ['.pas', '.dpr']);
  try
    Expect<Integer>(Files.Count).ToBe(5);
  finally
    Files.Free;
  end;
end;

{ A ceiling directory that is classified absolute is used verbatim; one that is
  not is anchored to the directory the setting came from
  (Goccia.Modules.Configuration AnchorCeilingDirectory), so misclassifying a
  drive-relative or backslash-prefixed path silently moves the capability
  boundary. The spellings are platform-specific, so the expectations are too. }
procedure TFileUtilsTests.TestIsAbsoluteHostPathRootedForms;
begin
  { A leading '/' roots a path on both platforms. }
  Expect<Boolean>(IsAbsoluteHostPath('/usr/local/lib')).ToBe(True);
  {$IFNDEF UNIX}
  Expect<Boolean>(IsAbsoluteHostPath('C:\packages')).ToBe(True);
  Expect<Boolean>(IsAbsoluteHostPath('c:/packages')).ToBe(True);
  Expect<Boolean>(IsAbsoluteHostPath('\\server\share\pkg')).ToBe(True);
  Expect<Boolean>(IsAbsoluteHostPath('\packages')).ToBe(True);
  {$ENDIF}
end;

procedure TFileUtilsTests.TestIsAbsoluteHostPathRelativeForms;
begin
  Expect<Boolean>(IsAbsoluteHostPath('')).ToBe(False);
  Expect<Boolean>(IsAbsoluteHostPath('packages')).ToBe(False);
  Expect<Boolean>(IsAbsoluteHostPath('./packages')).ToBe(False);
  Expect<Boolean>(IsAbsoluteHostPath('../packages')).ToBe(False);
  { Drive-relative: resolved against C:'s own working directory, not the root. }
  Expect<Boolean>(IsAbsoluteHostPath('C:packages')).ToBe(False);
  Expect<Boolean>(IsAbsoluteHostPath('C:')).ToBe(False);
  {$IFDEF UNIX}
  { A backslash is an ordinary filename character on UNIX. }
  Expect<Boolean>(IsAbsoluteHostPath('\packages')).ToBe(False);
  Expect<Boolean>(IsAbsoluteHostPath('C:\packages')).ToBe(False);
  {$ENDIF}
end;

procedure TFileUtilsTests.TestCanonicalHostPathIsUnknownForAMissingPath;
begin
  { '' is the "cannot answer" signal, not a path. Callers branch on it, so a
    name with nothing behind it must never come back as something. }
  Expect<string>(CanonicalHostPath('')).ToBe('');
  Expect<string>(CanonicalHostPath(FTempDir + PathDelim + 'absent.txt'))
    .ToBe('');
end;

procedure TFileUtilsTests.TestCanonicalHostPathIsStableForARealFile;
var
  Canonical: string;
begin
  CreateTempFile('present.txt');

  Canonical := CanonicalHostPath(FTempDir + PathDelim + 'present.txt');

  Expect<Boolean>(Canonical <> '').ToBe(True);
  { Canonicalizing an already-canonical path is the identity — the property the
    containment comparison relies on when neither side carries a link. }
  Expect<string>(CanonicalHostPath(Canonical)).ToBe(Canonical);
  Expect<string>(ExtractFileName(Canonical)).ToBe('present.txt');
end;

procedure TFileUtilsTests.TestCanonicalHostPathFollowsASymlink;
var
  LinkPath, TargetCanonical: string;
begin
  CreateTempDir('inner');
  CreateTempFile('inner' + PathDelim + 'target.txt');
  LinkPath := FTempDir + PathDelim + 'link.txt';
  TargetCanonical := CanonicalHostPath(
    FTempDir + PathDelim + 'inner' + PathDelim + 'target.txt');

  {$IFDEF UNIX}
  Expect<Boolean>(fpSymlink(PAnsiChar(AnsiString('inner' + PathDelim +
    'target.txt')), PAnsiChar(AnsiString(LinkPath))) = 0).ToBe(True);
  {$ENDIF}

  { The link and its target are two names for one file, and canonicalization is
    what collapses them — the whole reason a containment check can be phrased
    physically. }
  Expect<string>(CanonicalHostPath(LinkPath)).ToBe(TargetCanonical);
end;

procedure TFileUtilsTests.TestReadSharedHostFileBytes;
var
  Target, Error: string;
  Bytes: TBytes;
begin
  Target := FTempDir + PathDelim + 'store.json';
  Bytes := TEncoding.UTF8.GetBytes('{"version":1}');
  Expect<Boolean>(ReplaceHostFile(Target, Target + '.tmp', Bytes, Error))
    .ToBe(True);
  Expect<Integer>(Length(ReadSharedHostFileBytes(Target))).ToBe(Length(Bytes));
  Expect<Integer>(ReadSharedHostFileBytes(Target)[0]).ToBe(Ord('{'));
end;

procedure TFileUtilsTests.TestHostPathIsRegularFile;
var
  Target: string;
begin
  Target := FTempDir + PathDelim + 'regular.txt';
  CreateTempFile('regular.txt');
  Expect<Boolean>(HostPathIsRegularFile(Target)).ToBe(True);
  Expect<Boolean>(HostPathIsRegularFile(FTempDir)).ToBe(False);
  Expect<Boolean>(HostPathIsRegularFile(FTempDir + PathDelim + 'missing'))
    .ToBe(False);
  {$IFDEF UNIX}
  { A link to a regular file is not one. }
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Target)),
    PAnsiChar(AnsiString(FTempDir + PathDelim + 'link.txt')))).ToBe(0);
  Expect<Boolean>(HostPathIsRegularFile(FTempDir + PathDelim + 'link.txt'))
    .ToBe(False);
  {$ENDIF}
end;

procedure TFileUtilsTests.TestReadHostHandleBytesReadsFromTheStart;
var
  Target, Error: string;
  Handle: THandle;
  Bytes: TBytes;
begin
  Target := FTempDir + PathDelim + 'handle.bin';
  Expect<Boolean>(ReplaceHostFile(Target, Target + '.tmp',
    TEncoding.UTF8.GetBytes('0123456789'), Error)).ToBe(True);
  Handle := FileOpen(Target, fmOpenRead);
  Expect<Boolean>(Handle <> THandle(-1)).ToBe(True);
  try
    { A handle already read part way is read again from its start. }
    FileSeek(Handle, Int64(4), fsFromBeginning);
    Expect<Boolean>(ReadHostHandleBytes(Handle, Bytes, Error)).ToBe(True);
    Expect<string>(TEncoding.UTF8.GetString(Bytes)).ToBe('0123456789');
  finally
    FileClose(Handle);
  end;
end;

procedure TFileUtilsTests.TestRemoveHostTreeBeneathRemovesATree;
var
  Error: string;
begin
  CreateTempFile('cache' + PathDelim + 'a' + PathDelim + 'b' + PathDelim +
    'x.txt');
  CreateTempFile('cache' + PathDelim + 'a' + PathDelim + 'y.txt');
  CreateTempFile('cache' + PathDelim + 'keep.txt');
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache', 'a',
    Error)).ToBe(True);
  Expect<string>(Error).ToBe('');
  Expect<Boolean>(DirectoryExists(FTempDir + PathDelim + 'cache' + PathDelim +
    'a')).ToBe(False);
  Expect<Boolean>(FileExists(FTempDir + PathDelim + 'cache' + PathDelim +
    'keep.txt')).ToBe(True);
  { Nothing to remove is success. }
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache',
    'missing' + PathDelim + 'deeper', Error)).ToBe(True);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache',
    '..' + PathDelim + 'cache', Error)).ToBe(False);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache', '',
    Error)).ToBe(False);
end;

{ `a\..` names the root itself; removing it would delete everything. }
procedure TFileUtilsTests.TestRemoveHostTreeBeneathRefusesALastDotDot;
var
  Error: string;
begin
  CreateTempFile('cache' + PathDelim + 'a' + PathDelim + 'x.txt');
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache',
    'a' + PathDelim + '..', Error)).ToBe(False);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache',
    'a' + PathDelim + '.', Error)).ToBe(False);
  Expect<Boolean>(FileExists(FTempDir + PathDelim + 'cache' + PathDelim +
    'a' + PathDelim + 'x.txt')).ToBe(True);
end;

procedure TFileUtilsTests.TestRemoveHostTreeBeneathRemovesJunctionsItself;
{$IFDEF MSWINDOWS}
var
  Error, Outside: string;
begin
  CreateTempFile('outside' + PathDelim + 'secret.txt');
  CreateTempFile('cache' + PathDelim + 'pkg' + PathDelim + 'file.txt');
  Outside := FTempDir + PathDelim + 'outside';
  { A junction inside the tree, and one on the way to it. }
  Expect<Integer>(ExecuteProcess(GetEnvironmentVariable('ComSpec'),
    '/c mklink /J "' + FTempDir + PathDelim + 'cache' + PathDelim + 'pkg' +
    PathDelim + 'link" "' + Outside + '" >NUL')).ToBe(0);
  Expect<Integer>(ExecuteProcess(GetEnvironmentVariable('ComSpec'),
    '/c mklink /J "' + FTempDir + PathDelim + 'cache' + PathDelim + 'hop" "' +
    Outside + '" >NUL')).ToBe(0);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache',
    'hop' + PathDelim + 'secret.txt', Error)).ToBe(False);
  Expect<Boolean>(FileExists(Outside + PathDelim + 'secret.txt')).ToBe(True);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache', 'pkg',
    Error)).ToBe(True);
  Expect<string>(Error).ToBe('');
  Expect<Boolean>(FileExists(Outside + PathDelim + 'secret.txt')).ToBe(True);
  Expect<Boolean>(DirectoryExists(FTempDir + PathDelim + 'cache' + PathDelim +
    'pkg')).ToBe(False);
  { The junction removed as itself too. }
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache', 'hop',
    Error)).ToBe(True);
  Expect<Boolean>(FileExists(Outside + PathDelim + 'secret.txt')).ToBe(True);
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TFileUtilsTests.TestRemoveHostTreeBeneathDoesNotFollowLinks;
{$IFDEF UNIX}
var
  Error, Outside: string;
begin
  CreateTempFile('outside' + PathDelim + 'secret.txt');
  CreateTempFile('cache' + PathDelim + 'pkg' + PathDelim + 'file.txt');
  Outside := FTempDir + PathDelim + 'outside';
  { A link inside the tree, and one on the way to it. }
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Outside)),
    PAnsiChar(AnsiString(FTempDir + PathDelim + 'cache' + PathDelim + 'pkg' +
    PathDelim + 'link')))).ToBe(0);
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Outside)),
    PAnsiChar(AnsiString(FTempDir + PathDelim + 'cache' + PathDelim +
    'hop')))).ToBe(0);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache',
    'hop' + PathDelim + 'secret.txt', Error)).ToBe(False);
  Expect<Boolean>(FileExists(Outside + PathDelim + 'secret.txt')).ToBe(True);
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache', 'pkg',
    Error)).ToBe(True);
  Expect<Boolean>(FileExists(Outside + PathDelim + 'secret.txt')).ToBe(True);
  Expect<Boolean>(DirectoryExists(FTempDir + PathDelim + 'cache' + PathDelim +
    'pkg')).ToBe(False);
  { A root that is a link is refused. }
  Expect<Boolean>(RemoveHostTreeBeneath(FTempDir + PathDelim + 'cache' +
    PathDelim + 'hop', 'secret.txt', Error)).ToBe(False);
  Expect<Boolean>(FileExists(Outside + PathDelim + 'secret.txt')).ToBe(True);
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TFileUtilsTests.TestTryHostFileMode;
{$IFDEF UNIX}
var
  Mode: Cardinal;
begin
  CreateTempFile('mode.txt');
  fpChmod(PAnsiChar(AnsiString(FTempDir + PathDelim + 'mode.txt')), &640);
  Expect<Boolean>(TryHostFileMode(FTempDir + PathDelim + 'mode.txt', Mode))
    .ToBe(True);
  Expect<Integer>(Mode).ToBe(&640);
  Expect<Boolean>(TryHostFileMode(FTempDir + PathDelim + 'missing', Mode))
    .ToBe(False);
end;
{$ELSE}
begin
end;
{$ENDIF}

{ A run reads the trust store while another process's --trust replaces it.
  The reader opens it the way ReadSharedHostFileBytes does, sharing read,
  write, and delete, and the replace must still rename over it; a second
  shared reader must still open it meanwhile. }
procedure TFileUtilsTests.TestReplaceWhileASharedReaderHoldsTheFile;
{$IFDEF MSWINDOWS}
var
  Target, Error: string;
  Reader: THandle;
  Bytes: TBytes;
begin
  Target := FTempDir + PathDelim + 'store.json';
  Expect<Boolean>(ReplaceHostFile(Target, Target + '.tmp',
    TEncoding.UTF8.GetBytes('old'), Error)).ToBe(True);
  Reader := CreateFileW(PWideChar(UnicodeString(Target)), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Expect<Boolean>(Reader <> INVALID_HANDLE_VALUE).ToBe(True);
  try
    Expect<Integer>(Length(ReadSharedHostFileBytes(Target))).ToBe(3);
    Expect<Boolean>(ReplaceHostFile(Target, Target + '.tmp',
      TEncoding.UTF8.GetBytes('newer'), Error)).ToBe(True);
    Expect<string>(Error).ToBe('');
  finally
    CloseHandle(Reader);
  end;
  Bytes := ReadSharedHostFileBytes(Target);
  Expect<Integer>(Length(Bytes)).ToBe(5);
  Expect<Integer>(Bytes[0]).ToBe(Ord('n'));
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TFileUtilsTests.TestReplaceHostFileCreatesWithPermissions;
{$IFDEF UNIX}
const
  PRIVATE_FILE = &600;
  PERMISSION_BITS = &777;
var
  Target, Error: string;
  Info: Stat;
  PreviousMask: TMode;
begin
  Target := FTempDir + PathDelim + 'private.json';
  { With no umask, a 0666 temporary would be world-readable until renamed;
    the mode given is applied at creation instead. }
  PreviousMask := fpUmask(0);
  try
    Expect<Boolean>(ReplaceHostFile(Target, Target + '.tmp',
      TBytes.Create(Ord('x')), PRIVATE_FILE, Error)).ToBe(True);
  finally
    fpUmask(PreviousMask);
  end;
  Expect<Integer>(FpStat(Target, Info)).ToBe(0);
  Expect<Integer>(Info.st_mode and PERMISSION_BITS).ToBe(PRIVATE_FILE);
end;
{$ELSE}
begin
end;
{$ENDIF}

begin
  Randomize;
  TestRunnerProgram.AddSuite(TFileUtilsTests.Create('FileUtils'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
