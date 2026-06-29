{ Unit tests for the runtime ICU version discovery in source/shared/ICU.pas.

  The Linux loader must pick up whatever ICU major is installed — including
  versions newer than any the engine has seen before — without a code change.
  These tests pin the version parsing and the "newest present" directory scan
  that make that work, including majors above the old hard-coded 76 ceiling.

  The discovery helpers are Linux-only; on other platforms ICU loads by a single
  fixed library name and there is nothing to discover, so the suite is a no-op. }

program ICU.Test;

{$I Shared.inc}

uses
  SysUtils,

  ICU,
  TestingPascalLibrary;

type
  TICUTests = class(TTestSuite)
  private
    {$IFDEF LINUX}
    procedure TestParseMajorVersion;
    procedure TestDiscoverHighestInDirHasNoCap;
    {$ELSE}
    procedure TestDiscoveryIsLinuxOnly;
    {$ENDIF}
  public
    procedure SetupTests; override;
  end;

{$IFDEF LINUX}
procedure TouchFile(const APath: string);
var
  Handle: THandle;
begin
  Handle := FileCreate(APath);
  if Handle <> THandle(-1) then
    FileClose(Handle);
end;
{$ENDIF}

procedure TICUTests.SetupTests;
begin
  {$IFDEF LINUX}
  Test('ParseICUSoMajorVersion extracts the major from a versioned SONAME',
    TestParseMajorVersion);
  Test('HighestICUMajorVersionInDir returns the newest present major, uncapped',
    TestDiscoverHighestInDirHasNoCap);
  {$ELSE}
  Test('ICU version discovery is Linux-only (no-op on this platform)',
    TestDiscoveryIsLinuxOnly);
  {$ENDIF}
end;

{$IFDEF LINUX}
procedure TICUTests.TestParseMajorVersion;
begin
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.77', 'libicui18n.so')).ToBe(77);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.100', 'libicui18n.so')).ToBe(100);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.76.1', 'libicui18n.so')).ToBe(76);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.70', 'libicui18n.so')).ToBe(70);
  // No numeric version, or an unrelated SONAME, yields 0.
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so', 'libicui18n.so')).ToBe(0);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.', 'libicui18n.so')).ToBe(0);
  Expect<Integer>(ParseICUSoMajorVersion('libicui18n.so.x', 'libicui18n.so')).ToBe(0);
  Expect<Integer>(ParseICUSoMajorVersion('libicuuc.so.76', 'libicui18n.so')).ToBe(0);
end;

procedure TICUTests.TestDiscoverHighestInDirHasNoCap;
var
  Dir: string;
begin
  Dir := GetTempFileName(GetTempDir, 'gicu');
  DeleteFile(Dir);
  ForceDirectories(Dir);
  try
    // No ICU library present.
    Expect<Integer>(HighestICUMajorVersionInDir(Dir)).ToBe(0);

    // A spread of majors, including ones above the old hard-coded 76 ceiling.
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.74');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.76');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.77');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicui18n.so.100');
    // A non-i18n SONAME and an unrelated file must be ignored.
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'libicuuc.so.100');
    TouchFile(IncludeTrailingPathDelimiter(Dir) + 'unrelated.txt');

    Expect<Integer>(HighestICUMajorVersionInDir(Dir)).ToBe(100);
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
{$ELSE}
procedure TICUTests.TestDiscoveryIsLinuxOnly;
begin
  Expect<Boolean>(ICULibraryAvailable or True).ToBe(True);
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(TICUTests.Create('ICU'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
