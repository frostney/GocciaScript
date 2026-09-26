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
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
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
      'write-back: /ro/a.txt was copied read-only, skipped (use --copy-rw ' +
      'to write it back)' + sLineBreak +
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
    FInputs.ApplyWriteBack(Plan, Report);
    Expect<string>(ReadHostText('elsewhere/file.txt')).ToBe('untouched');
  finally
    Report.Free;
    Baseline.Free;
  end;
  {$ENDIF}
end;

begin
  Randomize;
  TestRunnerProgram.AddSuite(TSandboxHostInputsTests.Create(
    'Sandbox Host Inputs'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
