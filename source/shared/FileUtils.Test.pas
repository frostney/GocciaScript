program FileUtils.Test;

{$I Shared.inc}

uses
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

begin
  Randomize;
  TestRunnerProgram.AddSuite(TFileUtilsTests.Create('FileUtils'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
