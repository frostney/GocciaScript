program Goccia.Packages.Address.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Packages.Address,
  Goccia.TestSetup;

type
  TProviderAddressTests = class(TTestSuite)
  private
    function Parses(const AText: string): Boolean;
    procedure TestParsesExactAddress;
    procedure TestParsesPrefixAndRootAddresses;
    procedure TestRejectsMalformedAddresses;
    procedure TestRefsAreTagsOrCommits;
    procedure TestPackageKeyNamesNoPath;
    procedure TestArtifactPathSafety;
    procedure TestDerivedURLAndCachePath;
  public
    procedure SetupTests; override;
  end;

procedure TProviderAddressTests.SetupTests;
begin
  Test('Parses an exact address', TestParsesExactAddress);
  Test('Parses prefix and root addresses', TestParsesPrefixAndRootAddresses);
  Test('Rejects malformed addresses', TestRejectsMalformedAddresses);
  Test('Refs are tags or commits', TestRefsAreTagsOrCommits);
  Test('A package key names no path', TestPackageKeyNamesNoPath);
  Test('Artifact paths are safe on every host', TestArtifactPathSafety);
  Test('URLs and cache paths are derived', TestDerivedURLAndCachePath);
end;

function TProviderAddressTests.Parses(const AText: string): Boolean;
var
  Address: TGocciaProviderAddress;
  Error: string;
begin
  Result := TryParseProviderAddress(AText, Address, Error);
end;

procedure TProviderAddressTests.TestParsesExactAddress;
var
  Address: TGocciaProviderAddress;
  Error: string;
begin
  Expect<Boolean>(TryParseProviderAddress(
    'github:frostney/GocciaScript-Raylib@v0.10.0/bindings/raylib.ts',
    Address, Error)).ToBe(True);
  Expect<string>(Address.Owner).ToBe('frostney');
  Expect<string>(Address.Repository).ToBe('GocciaScript-Raylib');
  Expect<string>(Address.Ref).ToBe('v0.10.0');
  Expect<string>(Address.Path).ToBe('bindings/raylib.ts');
  Expect<Boolean>(Address.IsPrefix).ToBe(False);
  Expect<string>(Address.PackageKey).ToBe(
    'github:frostney/GocciaScript-Raylib@v0.10.0');
  Expect<string>(Address.ScopeText).ToBe(
    'github:frostney/gocciascript-raylib');
end;

procedure TProviderAddressTests.TestParsesPrefixAndRootAddresses;
var
  Address: TGocciaProviderAddress;
  Error: string;
begin
  Expect<Boolean>(TryParseProviderAddress('github:o/r@v1/bindings/',
    Address, Error)).ToBe(True);
  Expect<string>(Address.Path).ToBe('bindings/');
  Expect<Boolean>(Address.IsPrefix).ToBe(True);
  Expect<Boolean>(TryParseProviderAddress('github:o/r@v1', Address,
    Error)).ToBe(True);
  Expect<string>(Address.Path).ToBe('');
  Expect<Boolean>(Address.IsPrefix).ToBe(True);
  Expect<Boolean>(TryParseProviderAddress('github:o/r@v1/', Address,
    Error)).ToBe(True);
  Expect<string>(Address.Path).ToBe('');
end;

procedure TProviderAddressTests.TestRejectsMalformedAddresses;
begin
  Expect<Boolean>(Parses('github:o/r')).ToBe(False);
  Expect<Boolean>(Parses('github:o@v1')).ToBe(False);
  Expect<Boolean>(Parses('github:/r@v1')).ToBe(False);
  Expect<Boolean>(Parses('github:o/r/x@v1')).ToBe(False);
  Expect<Boolean>(Parses('github:o/..@v1')).ToBe(False);
  Expect<Boolean>(Parses('GitHub:o/r@v1')).ToBe(False);
  Expect<Boolean>(Parses('https://github.com/o/r')).ToBe(False);
  Expect<Boolean>(Parses('github:o/r@v1/../x.ts')).ToBe(False);
  Expect<Boolean>(Parses('github:o/r@v1//x.ts')).ToBe(False);
  Expect<Boolean>(Parses('github:o/r@v1/a\b.ts')).ToBe(False);
  Expect<Boolean>(Parses('github:o/r@v1/c:/x.ts')).ToBe(False);
  Expect<Boolean>(IsProviderAddress('github:x')).ToBe(True);
  Expect<Boolean>(IsProviderAddress('./github:x')).ToBe(False);
end;

procedure TProviderAddressTests.TestRefsAreTagsOrCommits;
begin
  Expect<Boolean>(IsSafeProviderRef('v1.2.3')).ToBe(True);
  Expect<Boolean>(IsSafeProviderRef('1.0.0+build.5')).ToBe(True);
  Expect<Boolean>(IsSafeProviderRef(
    '0123456789abcdef0123456789abcdef01234567')).ToBe(True);
  { A ref cannot contain `/`: the path begins at the first one. }
  Expect<Boolean>(Parses('github:o/r@release/v1')).ToBe(True);
  Expect<Boolean>(IsSafeProviderRef('release/v1')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('-v1')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('.v1')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('v1.')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('v1..2')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('v1.lock')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('v1@{0}')).ToBe(False);
  Expect<Boolean>(IsSafeProviderRef('v1~1')).ToBe(False);
  Expect<Boolean>(IsCommitHash(
    '0123456789abcdef0123456789abcdef01234567')).ToBe(True);
  Expect<Boolean>(IsCommitHash(
    '0123456789ABCDEF0123456789abcdef01234567')).ToBe(False);
  Expect<Boolean>(IsCommitHash('0123456')).ToBe(False);
end;

procedure TProviderAddressTests.TestPackageKeyNamesNoPath;
var
  Address: TGocciaProviderAddress;
  Error: string;
begin
  Expect<Boolean>(TryParsePackageKey('github:o/r@v1', Address,
    Error)).ToBe(True);
  Expect<Boolean>(TryParsePackageKey('github:o/r@v1/x.ts', Address,
    Error)).ToBe(False);
end;

procedure TProviderAddressTests.TestArtifactPathSafety;
begin
  Expect<Boolean>(IsSafeArtifactPath('bindings/raylib.ts')).ToBe(True);
  Expect<Boolean>(IsSafeArtifactPath('native/linux-x86_64/libraylib.so'))
    .ToBe(True);
  Expect<Boolean>(IsSafeArtifactPath('@scope/x_y-z.json')).ToBe(True);
  Expect<Boolean>(IsSafeArtifactPath('')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('/abs.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a/../b.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a/./b.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a//b.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a\b.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a:b.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a b.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('a/b.')).ToBe(False);
  { Windows device names, with or without an extension. }
  Expect<Boolean>(IsSafeArtifactPath('con.ts')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('x/NUL')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('lpt1.txt')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('console.ts')).ToBe(True);
  { Package files never act as configuration or as a cache. }
  Expect<Boolean>(IsSafeArtifactPath('goccia.json')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('sub/Goccia.toml')).ToBe(False);
  Expect<Boolean>(IsSafeArtifactPath('goccia.json/x.ts')).ToBe(True);
  Expect<Boolean>(IsSafeArtifactPath('.goccia/x.ts')).ToBe(False);
end;

procedure TProviderAddressTests.TestDerivedURLAndCachePath;
begin
  Expect<string>(PackageArtifactURL('frostney', 'Raylib',
    '0123456789abcdef0123456789abcdef01234567', 'bindings/x.ts')).ToBe(
    'https://raw.githubusercontent.com/frostney/Raylib/' +
    '0123456789abcdef0123456789abcdef01234567/bindings/x.ts');
  Expect<string>(PackageCacheRelativeDirectory('Frostney', 'Raylib',
    '0123456789abcdef0123456789abcdef01234567')).ToBe(
    'packages/github/frostney/raylib/' +
    '0123456789abcdef0123456789abcdef01234567');
end;

begin
  TestRunnerProgram.AddSuite(TProviderAddressTests.Create(
    'Provider package addresses'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
