unit Goccia.Packages.Address;

{ Provider package addresses (ADR 0122).

  An import-map address `github:<owner>/<repo>@<ref>[/<path>]` names a
  provider package and, optionally, a path inside it. The ref is a tag or a
  40-character commit; branches are not accepted, because nothing can later
  prove which commit a moved branch pointed at. `<path>` is empty for the
  repository root, names a file for an exact import-map entry, and ends with
  `/` for a prefix entry.

  This unit is pure string work shared by the capability scopes, the
  lockfile, and the package store, so every place that validates a name
  validates it the same way. }

{$I Goccia.inc}

interface

const
  GITHUB_PROVIDER_PREFIX = 'github:';
  GITHUB_COMMIT_LENGTH = 40;
  SHA256_HEX_LENGTH = 64;
  { The only host a run fetches package files from. }
  GITHUB_RAW_HOST = 'raw.githubusercontent.com';
  PACKAGE_CACHE_DIRECTORY_NAME = '.goccia';
  PACKAGES_CACHE_DIRECTORY_NAME = 'packages';
  GITHUB_CACHE_DIRECTORY_NAME = 'github';

type
  TGocciaProviderAddress = record
    Owner: string;
    Repository: string;
    Ref: string;
    { '' for the repository root, `dir/` for a prefix, else a file. }
    Path: string;
    { `github:<owner>/<repo>@<ref>`, as written: the lockfile key. }
    function PackageKey: string;
    { `github:<owner>/<repo>`, lowercased: the import scope it needs. }
    function ScopeText: string;
    function IsPrefix: Boolean;
  end;

{ True for an address in the provider namespace, well formed or not. }
function IsProviderAddress(const AText: string): Boolean;

{ Parses a provider address. AError says what is wrong when it fails. }
function TryParseProviderAddress(const AText: string;
  out AAddress: TGocciaProviderAddress; out AError: string): Boolean;

{ Parses `github:<owner>/<repo>@<ref>` with no path: a lockfile key. }
function TryParsePackageKey(const AText: string;
  out AAddress: TGocciaProviderAddress; out AError: string): Boolean;

function IsSafeGitHubOwner(const AValue: string): Boolean;
function IsSafeGitHubRepository(const AValue: string): Boolean;
{ A tag name or a 40-character lowercase commit. No `/`, so the ref ends
  where the path begins. }
function IsSafeProviderRef(const AValue: string): Boolean;
function IsCommitHash(const AValue: string): Boolean;
function IsSHA256Hash(const AValue: string): Boolean;

{ A relative path inside a repository that is safe to materialize on every
  host filesystem: `/`-separated, no empty, `.`, or `..` segment, no
  backslash or colon, only ASCII letters, digits, `-`, `_`, `.`, and `@`, no
  segment ending in `.`, no Windows device name, and no `goccia.*` file or
  `.goccia` directory, which could otherwise act as configuration. }
function IsSafeArtifactPath(const APath: string): Boolean;

{ `packages/github/<owner>/<repo>/<commit>` below the cache directory,
  lowercased so case variants of one repository share an entry and cannot
  collide on a case-insensitive filesystem. `/`-separated. }
function PackageCacheRelativeDirectory(const AOwner, ARepository,
  ACommit: string): string;

{ The pinned URL of one package file. Neither the import map nor the
  lockfile can name a URL: it is always derived. }
function PackageArtifactURL(const AOwner, ARepository, ACommit,
  AArtifactPath: string): string;

implementation

uses
  SysUtils;

const
  MAX_OWNER_LENGTH = 39;
  MAX_REPOSITORY_LENGTH = 100;
  MAX_REF_LENGTH = 255;
  GITHUB_RAW_BASE_URL = 'https://' + GITHUB_RAW_HOST + '/';
  CONFIG_FILE_PREFIX = 'goccia.';
  WINDOWS_DEVICE_NAMES: array[0..21] of string = ('con', 'prn', 'aux', 'nul',
    'com1', 'com2', 'com3', 'com4', 'com5', 'com6', 'com7', 'com8', 'com9',
    'lpt1', 'lpt2', 'lpt3', 'lpt4', 'lpt5', 'lpt6', 'lpt7', 'lpt8', 'lpt9');

function IsASCIIAlphaNumeric(const AValue: Char): Boolean;
begin
  Result := ((AValue >= 'a') and (AValue <= 'z')) or
    ((AValue >= 'A') and (AValue <= 'Z')) or
    ((AValue >= '0') and (AValue <= '9'));
end;

function IsLowercaseHex(const AValue: string;
  const AExpectedLength: Integer): Boolean;
var
  I: Integer;
begin
  if Length(AValue) <> AExpectedLength then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not (((AValue[I] >= '0') and (AValue[I] <= '9')) or
      ((AValue[I] >= 'a') and (AValue[I] <= 'f'))) then
      Exit(False);
  Result := True;
end;

function IsCommitHash(const AValue: string): Boolean;
begin
  Result := IsLowercaseHex(AValue, GITHUB_COMMIT_LENGTH);
end;

function IsSHA256Hash(const AValue: string): Boolean;
begin
  Result := IsLowercaseHex(AValue, SHA256_HEX_LENGTH);
end;

function IsSafeGitHubOwner(const AValue: string): Boolean;
var
  I: Integer;
begin
  if (AValue = '') or (Length(AValue) > MAX_OWNER_LENGTH) or
     (AValue[1] = '-') then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not (IsASCIIAlphaNumeric(AValue[I]) or (AValue[I] = '-')) then
      Exit(False);
  Result := True;
end;

function IsSafeGitHubRepository(const AValue: string): Boolean;
var
  I: Integer;
begin
  if (AValue = '') or (Length(AValue) > MAX_REPOSITORY_LENGTH) or
     (AValue = '.') or (AValue = '..') then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not (IsASCIIAlphaNumeric(AValue[I]) or (AValue[I] = '-') or
      (AValue[I] = '_') or (AValue[I] = '.')) then
      Exit(False);
  Result := True;
end;

function IsSafeProviderRef(const AValue: string): Boolean;
var
  I: Integer;
begin
  if (AValue = '') or (Length(AValue) > MAX_REF_LENGTH) or
     (AValue[1] = '.') or (AValue[1] = '-') or
     (AValue[Length(AValue)] = '.') or (Pos('..', AValue) > 0) or
     SameText(Copy(AValue, Length(AValue) - 4, 5), '.lock') then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not (IsASCIIAlphaNumeric(AValue[I]) or (AValue[I] = '-') or
      (AValue[I] = '_') or (AValue[I] = '.') or (AValue[I] = '+')) then
      Exit(False);
  Result := True;
end;

function IsWindowsDeviceName(const ASegment: string): Boolean;
var
  Stem: string;
  DotIndex, I: Integer;
begin
  Stem := LowerCase(ASegment);
  DotIndex := Pos('.', Stem);
  if DotIndex > 0 then
    Stem := Copy(Stem, 1, DotIndex - 1);
  for I := Low(WINDOWS_DEVICE_NAMES) to High(WINDOWS_DEVICE_NAMES) do
    if Stem = WINDOWS_DEVICE_NAMES[I] then
      Exit(True);
  Result := False;
end;

function IsSafeArtifactSegment(const ASegment: string;
  const AIsLast: Boolean): Boolean;
begin
  Result := (ASegment <> '') and (ASegment <> '.') and (ASegment <> '..') and
    (ASegment[Length(ASegment)] <> '.') and
    not IsWindowsDeviceName(ASegment) and
    not SameText(ASegment, PACKAGE_CACHE_DIRECTORY_NAME) and
    not (AIsLast and SameText(Copy(ASegment, 1, Length(CONFIG_FILE_PREFIX)),
      CONFIG_FILE_PREFIX));
end;

function IsSafeArtifactPath(const APath: string): Boolean;
var
  I, SegmentStart: Integer;
begin
  if (APath = '') or (APath[1] = '/') then
    Exit(False);

  SegmentStart := 1;
  for I := 1 to Length(APath) + 1 do
    if (I > Length(APath)) or (APath[I] = '/') then
    begin
      if not IsSafeArtifactSegment(Copy(APath, SegmentStart, I - SegmentStart),
         I > Length(APath)) then
        Exit(False);
      SegmentStart := I + 1;
    end
    else if not (IsASCIIAlphaNumeric(APath[I]) or
      (APath[I] = '-') or (APath[I] = '_') or
      (APath[I] = '.') or (APath[I] = '@')) then
      Exit(False);

  Result := True;
end;

function IsProviderAddress(const AText: string): Boolean;
begin
  Result := SameText(Copy(AText, 1, Length(GITHUB_PROVIDER_PREFIX)),
    GITHUB_PROVIDER_PREFIX);
end;

function ParseAddress(const AText: string; const AAllowPath: Boolean;
  out AAddress: TGocciaProviderAddress; out AError: string): Boolean;
var
  AtIndex, SlashIndex: Integer;
  Repository, Rest, PathText: string;
begin
  Result := False;
  AAddress := Default(TGocciaProviderAddress);
  AError := '';
  if Copy(AText, 1, Length(GITHUB_PROVIDER_PREFIX)) <>
     GITHUB_PROVIDER_PREFIX then
  begin
    AError := 'the only provider is github: (lowercase)';
    Exit;
  end;
  Rest := Copy(AText, Length(GITHUB_PROVIDER_PREFIX) + 1, MaxInt);
  AtIndex := Pos('@', Rest);
  if AtIndex = 0 then
  begin
    AError := 'a provider address needs a tag or commit after "@"';
    Exit;
  end;
  Repository := Copy(Rest, 1, AtIndex - 1);
  Rest := Copy(Rest, AtIndex + 1, MaxInt);

  SlashIndex := Pos('/', Repository);
  if SlashIndex = 0 then
  begin
    AError := 'a provider address must be github:<owner>/<repo>@<ref>';
    Exit;
  end;
  AAddress.Owner := Copy(Repository, 1, SlashIndex - 1);
  AAddress.Repository := Copy(Repository, SlashIndex + 1, MaxInt);
  if not IsSafeGitHubOwner(AAddress.Owner) or
     not IsSafeGitHubRepository(AAddress.Repository) then
  begin
    AError := 'the GitHub owner or repository name is not valid';
    Exit;
  end;

  SlashIndex := Pos('/', Rest);
  if SlashIndex = 0 then
  begin
    AAddress.Ref := Rest;
    PathText := '';
  end
  else
  begin
    AAddress.Ref := Copy(Rest, 1, SlashIndex - 1);
    PathText := Copy(Rest, SlashIndex + 1, MaxInt);
  end;
  if not IsSafeProviderRef(AAddress.Ref) then
  begin
    AError := 'the ref must be a tag or a 40-character commit ' +
      '(letters, digits, ".", "_", "-", "+")';
    Exit;
  end;
  if (SlashIndex > 0) and not AAllowPath then
  begin
    AError := 'a package key names no path';
    Exit;
  end;
  if PathText <> '' then
  begin
    if PathText[Length(PathText)] = '/' then
    begin
      if not IsSafeArtifactPath(Copy(PathText, 1, Length(PathText) - 1)) then
      begin
        AError := 'the path inside the package is not a safe relative path';
        Exit;
      end;
    end
    else if not IsSafeArtifactPath(PathText) then
    begin
      AError := 'the path inside the package is not a safe relative path';
      Exit;
    end;
  end;
  AAddress.Path := PathText;
  Result := True;
end;

function TryParseProviderAddress(const AText: string;
  out AAddress: TGocciaProviderAddress; out AError: string): Boolean;
begin
  Result := ParseAddress(AText, True, AAddress, AError);
end;

function TryParsePackageKey(const AText: string;
  out AAddress: TGocciaProviderAddress; out AError: string): Boolean;
begin
  Result := ParseAddress(AText, False, AAddress, AError);
end;

function TGocciaProviderAddress.PackageKey: string;
begin
  Result := GITHUB_PROVIDER_PREFIX + Owner + '/' + Repository + '@' + Ref;
end;

function TGocciaProviderAddress.ScopeText: string;
begin
  Result := LowerCase(GITHUB_PROVIDER_PREFIX + Owner + '/' + Repository);
end;

function TGocciaProviderAddress.IsPrefix: Boolean;
begin
  Result := (Path = '') or (Path[Length(Path)] = '/');
end;

function PackageCacheRelativeDirectory(const AOwner, ARepository,
  ACommit: string): string;
begin
  Result := PACKAGES_CACHE_DIRECTORY_NAME + '/' + GITHUB_CACHE_DIRECTORY_NAME +
    '/' + LowerCase(AOwner) + '/' + LowerCase(ARepository) + '/' + ACommit;
end;

function PackageArtifactURL(const AOwner, ARepository, ACommit,
  AArtifactPath: string): string;
begin
  Result := GITHUB_RAW_BASE_URL + AOwner + '/' + ARepository + '/' + ACommit +
    '/' + AArtifactPath;
end;

end.
