program HostOutputFiles.Test;

{$I Shared.inc}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  HostOutputFiles,
  TestingPascalLibrary;

type
  THostOutputFilesTests = class(TTestSuite)
  private
    FTempDir: string;
    FProject: string;
    FOutside: string;
    function Canonical(const APath: string): string;
    procedure Register(const APath: string);
    procedure WriteText(const APath, AText: string);
    function ReadText(const APath: string): string;
    function Refused(const APath: string): Boolean;
    procedure RemoveTree(const APath: string);

    procedure TestUnregisteredPathWritesAsBefore;
    procedure TestConfinedPathWrites;
    procedure TestConfinedPathReplacesContent;
    procedure TestFileInsideConfinedDirectory;
    procedure TestSwappedDirectoryIsRefused;
    procedure TestSymlinkLeafIsRefused;
    procedure TestReplacedRootIsRefused;
    procedure TestHardLinkedLeafIsRefused;
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
  end;

procedure THostOutputFilesTests.SetupTests;
begin
  Test('A path nobody registered is written as before',
    TestUnregisteredPathWritesAsBefore);
  Test('A confined output is written inside its directory',
    TestConfinedPathWrites);
  Test('A confined output replaces what the file held',
    TestConfinedPathReplacesContent);
  Test('A file directly inside a confined directory output is written',
    TestFileInsideConfinedDirectory);
  { Symbolic and hard links need an API this build only has on UNIX. }
  {$IFDEF UNIX}
  Test('A directory swapped for a link after the check is refused',
    TestSwappedDirectoryIsRefused);
  Test('A link at the output''s own name is refused',
    TestSymlinkLeafIsRefused);
  Test('A config directory replaced after the check is refused',
    TestReplacedRootIsRefused);
  Test('A hard-linked output is refused before it is truncated',
    TestHardLinkedLeafIsRefused);
  {$ELSE}
  Skip('A directory swapped for a link after the check is refused',
    TestSwappedDirectoryIsRefused, 'links are not available on this platform');
  Skip('A link at the output''s own name is refused',
    TestSymlinkLeafIsRefused, 'links are not available on this platform');
  Skip('A config directory replaced after the check is refused',
    TestReplacedRootIsRefused, 'links are not available on this platform');
  Skip('A hard-linked output is refused before it is truncated',
    TestHardLinkedLeafIsRefused, 'links are not available on this platform');
  {$ENDIF}
end;

procedure THostOutputFilesTests.BeforeEach;
begin
  ClearConfinedHostOutputs;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) +
    'goccia_host_output_' + IntToStr(Random(MaxInt));
  FProject := FTempDir + PathDelim + 'project';
  FOutside := FTempDir + PathDelim + 'outside';
  ForceDirectories(FProject + PathDelim + 'out');
  ForceDirectories(FOutside);
end;

procedure THostOutputFilesTests.AfterEach;
begin
  ClearConfinedHostOutputs;
  RemoveTree(FTempDir);
end;

{ Links are removed, never descended into, so a test's link to the outside
  directory cannot take its files with it. }
procedure THostOutputFilesTests.RemoveTree(const APath: string);
var
  Search: TSearchRec;
  Child: string;
begin
  if HostPathIsSymlink(APath) then
  begin
    DeleteFile(APath);
    Exit;
  end;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*',
       faAnyFile or faSymLink, Search) = 0 then
  try
    repeat
      if (Search.Name = '.') or (Search.Name = '..') then
        Continue;
      Child := IncludeTrailingPathDelimiter(APath) + Search.Name;
      if HostPathIsSymlink(Child) or ((Search.Attr and faDirectory) = 0) then
        DeleteFile(Child)
      else
        RemoveTree(Child);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(APath);
end;

function THostOutputFilesTests.Canonical(const APath: string): string;
var
  Directory: string;
begin
  { The leaf need not exist; its directory does. }
  Directory := CanonicalHostPath(ExtractFileDir(APath));
  Result := IncludeTrailingPathDelimiter(Directory) + ExtractFileName(APath);
end;

procedure THostOutputFilesTests.Register(const APath: string);
begin
  RegisterConfinedHostOutput(APath, Canonical(APath),
    CanonicalHostPath(FProject));
end;

procedure THostOutputFilesTests.WriteText(const APath, AText: string);
begin
  WriteHostOutputText(APath, AText);
end;

function THostOutputFilesTests.ReadText(const APath: string): string;
begin
  Result := ReadUTF8FileText(APath);
end;

function THostOutputFilesTests.Refused(const APath: string): Boolean;
begin
  Result := False;
  try
    WriteText(APath, 'WRITTEN');
  except
    on E: EHostOutputRefused do
      Result := True;
  end;
end;

procedure THostOutputFilesTests.TestUnregisteredPathWritesAsBefore;
var
  Path: string;
begin
  Path := FOutside + PathDelim + 'free.txt';
  WriteText(Path, 'free');
  Expect<Boolean>(IsConfinedHostOutput(Path)).ToBe(False);
  Expect<string>(ReadText(Path)).ToBe('free');
end;

procedure THostOutputFilesTests.TestConfinedPathWrites;
var
  Path: string;
begin
  Path := FProject + PathDelim + 'out' + PathDelim + 'report.json';
  Register(Path);
  Expect<Boolean>(IsConfinedHostOutput(Path)).ToBe(True);
  WriteText(Path, '{"ok":true}');
  Expect<string>(ReadText(Path)).ToBe('{"ok":true}');
end;

procedure THostOutputFilesTests.TestConfinedPathReplacesContent;
var
  Path: string;
begin
  Path := FProject + PathDelim + 'out' + PathDelim + 'report.json';
  WriteUTF8FileText(Path, 'a much longer previous content');
  Register(Path);
  WriteText(Path, 'short');
  Expect<string>(ReadText(Path)).ToBe('short');
end;

procedure THostOutputFilesTests.TestFileInsideConfinedDirectory;
var
  Directory: string;
begin
  Directory := FProject + PathDelim + 'out';
  RegisterConfinedHostOutput(Directory, CanonicalHostPath(Directory),
    CanonicalHostPath(FProject));
  WriteText(IncludeTrailingPathDelimiter(Directory) + 'main.gbc', 'bytes');
  Expect<string>(ReadText(Directory + PathDelim + 'main.gbc')).ToBe('bytes');
end;

procedure THostOutputFilesTests.TestSwappedDirectoryIsRefused;
{$IFDEF UNIX}
var
  Path, Directory: string;
begin
  Directory := FProject + PathDelim + 'out';
  Path := Directory + PathDelim + 'report.json';
  Register(Path);
  { After the check, out/ becomes a link to a directory elsewhere. }
  Expect<Boolean>(RenameFile(Directory, Directory + '.moved')).ToBe(True);
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(FOutside)),
    PAnsiChar(AnsiString(Directory)))).ToBe(0);
  Expect<Boolean>(Refused(Path)).ToBe(True);
  Expect<Boolean>(FileExists(FOutside + PathDelim + 'report.json'))
    .ToBe(False);
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure THostOutputFilesTests.TestSymlinkLeafIsRefused;
{$IFDEF UNIX}
var
  Path, Victim: string;
begin
  Path := FProject + PathDelim + 'out' + PathDelim + 'report.json';
  Victim := FOutside + PathDelim + 'victim.txt';
  WriteUTF8FileText(Victim, 'VICTIM');
  Register(Path);
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Victim)),
    PAnsiChar(AnsiString(Path)))).ToBe(0);
  Expect<Boolean>(Refused(Path)).ToBe(True);
  Expect<string>(ReadText(Victim)).ToBe('VICTIM');
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure THostOutputFilesTests.TestReplacedRootIsRefused;
{$IFDEF UNIX}
var
  Path: string;
begin
  Path := FProject + PathDelim + 'out' + PathDelim + 'report.json';
  Register(Path);
  { The whole project directory is moved aside and its name pointed at a
    directory with the same shape elsewhere. }
  ForceDirectories(FOutside + PathDelim + 'out');
  Expect<Boolean>(RenameFile(FProject, FProject + '.moved')).ToBe(True);
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(FOutside)),
    PAnsiChar(AnsiString(FProject)))).ToBe(0);
  Expect<Boolean>(Refused(Path)).ToBe(True);
  Expect<Boolean>(FileExists(FOutside + PathDelim + 'out' + PathDelim +
    'report.json')).ToBe(False);
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure THostOutputFilesTests.TestHardLinkedLeafIsRefused;
{$IFDEF UNIX}
var
  Path, Victim: string;
begin
  Path := FProject + PathDelim + 'out' + PathDelim + 'report.json';
  Victim := FOutside + PathDelim + 'victim.txt';
  WriteUTF8FileText(Victim, 'VICTIM');
  Register(Path);
  Expect<Integer>(fpLink(PAnsiChar(AnsiString(Victim)),
    PAnsiChar(AnsiString(Path)))).ToBe(0);
  Expect<Boolean>(Refused(Path)).ToBe(True);
  Expect<string>(ReadText(Victim)).ToBe('VICTIM');
end;
{$ELSE}
begin
end;
{$ENDIF}

begin
  Randomize;
  TestRunnerProgram.AddSuite(THostOutputFilesTests.Create('HostOutputFiles'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
