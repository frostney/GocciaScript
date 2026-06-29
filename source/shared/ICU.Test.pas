{ Unit tests for the platform-independent ICU version discovery helpers in
  source/shared/ICU.pas.

  The Linux loader must pick up whatever ICU major is installed — including
  versions newer than any the engine has seen before, and ICU reachable only via
  LD_LIBRARY_PATH — with no code change. The version parsing and the directory
  scans are pure string and directory logic with no platform dependency, so they
  run on every platform and are pinned here, including majors above the old
  hard-coded 76 ceiling. }

program ICU.Test;

{$I Shared.inc}

uses
  SysUtils,

  ICU,
  TestingPascalLibrary;

const
  I18N_BASE = 'libicui18n.so';

type
  TICUTests = class(TTestSuite)
  private
    procedure TestParseMajorVersion;
    procedure TestHighestInDirHasNoCap;
    procedure TestHighestInDirListAcrossPaths;
  public
    procedure SetupTests; override;
  end;

procedure TouchFile(const APath: string);
var
  Handle: THandle;
begin
  Handle := FileCreate(APath);
  if Handle = THandle(-1) then
    raise Exception.CreateFmt('Failed to create test fixture file: %s', [APath]);
  FileClose(Handle);
end;

function MakeTempDir(const APrefix: string): string;
begin
  Result := GetTempFileName(GetTempDir, APrefix);
  DeleteFile(Result);
  if not ForceDirectories(Result) then
    raise Exception.CreateFmt('Failed to create test fixture directory: %s', [Result]);
end;

procedure TICUTests.SetupTests;
begin
  Test('ParseICUSoMajorVersion extracts the major from a versioned SONAME',
    TestParseMajorVersion);
  Test('HighestICUMajorVersionInDir returns the newest present major, uncapped',
    TestHighestInDirHasNoCap);
  Test('HighestICUMajorVersionInDirList scans every dir and skips empty segments',
    TestHighestInDirListAcrossPaths);
end;

procedure TICUTests.TestParseMajorVersion;
begin
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.77', I18N_BASE)).ToBe(77);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.100', I18N_BASE)).ToBe(100);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.76.1', I18N_BASE)).ToBe(76);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.70', I18N_BASE)).ToBe(70);
  // No numeric version, or an unrelated SONAME, yields 0.
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so', I18N_BASE)).ToBe(0);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.', I18N_BASE)).ToBe(0);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.x', I18N_BASE)).ToBe(0);
  Expect<Integer>(ParseICUSoMajorVersion('libicuuc.so.76', I18N_BASE)).ToBe(0);
end;

procedure TICUTests.TestHighestInDirHasNoCap;
var
  Dir: string;
begin
  Dir := MakeTempDir('gicu');
  try
    // No ICU library present.
    Expect<Integer>(HighestICUMajorVersionInDir(Dir, I18N_BASE)).ToBe(0);

    // A spread of majors, including ones above the old hard-coded 76 ceiling.
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.74');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.76');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.77');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.100');
    // A non-i18n SONAME and an unrelated file must be ignored.
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicuuc.so.100');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'unrelated.txt');

    Expect<Integer>(HighestICUMajorVersionInDir(Dir, I18N_BASE)).ToBe(100);
  finally
    DeleteFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.74');
    DeleteFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.76');
    DeleteFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.77');
    DeleteFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.100');
    DeleteFile(IncludeTrailingPathDelimiter(Dir) + 'libicuuc.so.100');
    DeleteFile(IncludeTrailingPathDelimiter(Dir) + 'unrelated.txt');
    RemoveDir(Dir);
  end;
end;

procedure TICUTests.TestHighestInDirListAcrossPaths;
var
  DirA, DirB: string;
begin
  DirA := MakeTempDir('gicua');
  DirB := MakeTempDir('gicub');
  try
    TouchFile(IncludeTrailingPathDelimiter(DirA) + 'libicui18n.so.76');
    TouchFile(IncludeTrailingPathDelimiter(DirB) + 'libicui18n.so.99');

    // Newest across the whole list; the trailing empty segment is skipped.
    Expect<Integer>(HighestICUMajorVersionInDirList(
      DirA + ':' + DirB + ':', ':', I18N_BASE)).ToBe(99);
    // An empty list discovers nothing.
    Expect<Integer>(HighestICUMajorVersionInDirList('', ':', I18N_BASE)).ToBe(0);
  finally
    DeleteFile(IncludeTrailingPathDelimiter(DirA) + 'libicui18n.so.76');
    DeleteFile(IncludeTrailingPathDelimiter(DirB) + 'libicui18n.so.99');
    RemoveDir(DirA);
    RemoveDir(DirB);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TICUTests.Create('ICU'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
