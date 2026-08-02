unit Goccia.Modules.RemotePackages;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Modules.Resolver;

type
  EGocciaRemotePackageError = class(Exception);

  TGocciaProviderRemotePackageResolver = class(TGocciaRemotePackageResolver)
  private
    FCacheDirectory: string;
    function CacheRootForPackage(const AImportMapDirectory,
      ARepository, AResolvedReference: string): string;
    function MaterializeArtifact(const ARepository, AResolvedReference,
      AArtifactPath, AExpectedHash, APackageCacheRoot: string): string;
  protected
    function FetchArtifact(const AURL: string): TBytes; virtual;
  public
    constructor Create(const ACacheDirectory: string = '');
    function ResolvePackage(const AReference,
      AImportMapPath: string): string; override;
  end;

implementation

uses
  Classes,

  FileUtils,
  HTTPClient,
  SHA256,

  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.Platform,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  GITHUB_PROVIDER_PREFIX = 'github:';
  LOCKFILE_NAME = 'goccia.lock.json';
  LOCKFILE_SCHEMA_VERSION = 1;
  LOCKFILE_PACKAGES_PROPERTY = 'packages';
  LOCKFILE_VERSION_PROPERTY = 'version';
  LOCK_ENTRY_ARTIFACTS_PROPERTY = 'artifacts';
  LOCK_ENTRY_ENTRY_PROPERTY = 'entry';
  LOCK_ENTRY_RESOLVED_REFERENCE_PROPERTY = 'resolvedRef';
  ARTIFACT_HASH_PROPERTY = 'sha256';
  ARTIFACT_PLATFORM_PROPERTY = 'platform';
  DEFAULT_CACHE_DIRECTORY = '.goccia';
  PACKAGES_CACHE_DIRECTORY = 'packages';
  GITHUB_RAW_BASE_URL = 'https://raw.githubusercontent.com/';
  SHA256_HEX_LENGTH = 64;
  GITHUB_COMMIT_LENGTH = 40;

function ReadJSONFile(const APath: string): TGocciaValue;
var
  Parser: TGocciaJSONParser;
begin
  Parser := TGocciaJSONParser.Create;
  try
    Result := Parser.Parse(ReadUTF8FileText(APath));
  finally
    Parser.Free;
  end;
end;

function RequireObjectProperty(const AObject: TGocciaObjectValue;
  const APropertyName, AContext: string): TGocciaObjectValue;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(APropertyName);
  if not (Value is TGocciaObjectValue) then
    raise EGocciaRemotePackageError.CreateFmt(
      '%s property "%s" must be a JSON object.',
      [AContext, APropertyName]);
  Result := TGocciaObjectValue(Value);
end;

function RequireStringProperty(const AObject: TGocciaObjectValue;
  const APropertyName, AContext: string): string;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(APropertyName);
  if not (Value is TGocciaStringLiteralValue) then
    raise EGocciaRemotePackageError.CreateFmt(
      '%s property "%s" must be a string.',
      [AContext, APropertyName]);
  Result := TGocciaStringLiteralValue(Value).Value;
  if Result = '' then
    raise EGocciaRemotePackageError.CreateFmt(
      '%s property "%s" must not be empty.',
      [AContext, APropertyName]);
end;

function OptionalStringProperty(const AObject: TGocciaObjectValue;
  const APropertyName, AContext: string): string;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(APropertyName);
  if (not Assigned(Value)) or (Value is TGocciaUndefinedLiteralValue) then
    Exit('');
  if not (Value is TGocciaStringLiteralValue) then
    raise EGocciaRemotePackageError.CreateFmt(
      '%s property "%s" must be a string when present.',
      [AContext, APropertyName]);
  Result := TGocciaStringLiteralValue(Value).Value;
end;

function IsASCIIAlphaNumeric(const AValue: Char): Boolean;
begin
  Result := ((AValue >= 'a') and (AValue <= 'z')) or
    ((AValue >= 'A') and (AValue <= 'Z')) or
    ((AValue >= '0') and (AValue <= '9'));
end;

function IsSafeRepositoryPart(const AValue: string): Boolean;
var
  I: Integer;
begin
  if AValue = '' then
    Exit(False);
  if (AValue = '.') or (AValue = '..') then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not (IsASCIIAlphaNumeric(AValue[I]) or
      (AValue[I] = '-') or (AValue[I] = '_') or
      (AValue[I] = '.')) then
      Exit(False);
  Result := True;
end;

procedure ParseGitHubReference(const AReference: string;
  out ARepository: string);
var
  AtIndex, I, SlashIndex: Integer;
  OwnerName, RepositoryName, Selector: string;
begin
  if Copy(AReference, 1, Length(GITHUB_PROVIDER_PREFIX)) <>
     GITHUB_PROVIDER_PREFIX then
    raise EGocciaRemotePackageError.CreateFmt(
      'Unsupported remote package provider in "%s". The first slice supports github: references only.',
      [AReference]);

  AtIndex := 0;
  for I := Length(AReference) downto
    Length(GITHUB_PROVIDER_PREFIX) + 1 do
    if AReference[I] = '@' then
    begin
      AtIndex := I;
      Break;
    end;
  if AtIndex = 0 then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package reference "%s" must include a requested ref after "@".',
      [AReference]);

  ARepository := Copy(AReference, Length(GITHUB_PROVIDER_PREFIX) + 1,
    AtIndex - Length(GITHUB_PROVIDER_PREFIX) - 1);
  Selector := Copy(AReference, AtIndex + 1, MaxInt);
  SlashIndex := Pos('/', ARepository);
  if (SlashIndex <= 1) or (SlashIndex >= Length(ARepository)) or
     (Pos('/', Copy(ARepository, SlashIndex + 1, MaxInt)) > 0) or
     (Selector = '') then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package reference "%s" must use github:owner/repository@ref.',
      [AReference]);

  OwnerName := Copy(ARepository, 1, SlashIndex - 1);
  RepositoryName := Copy(ARepository, SlashIndex + 1, MaxInt);
  if not IsSafeRepositoryPart(OwnerName) or
     not IsSafeRepositoryPart(RepositoryName) then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package reference "%s" contains an invalid GitHub repository name.',
      [AReference]);
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

function IsSafeArtifactPath(const APath: string): Boolean;
var
  I, SegmentStart: Integer;
  Segment: string;
begin
  if (APath = '') or (APath[1] = '/') or
     (Pos('\', APath) > 0) or (Pos(':', APath) > 0) then
    Exit(False);

  SegmentStart := 1;
  for I := 1 to Length(APath) + 1 do
    if (I > Length(APath)) or (APath[I] = '/') then
    begin
      Segment := Copy(APath, SegmentStart, I - SegmentStart);
      if (Segment = '') or (Segment = '.') or (Segment = '..') then
        Exit(False);
      SegmentStart := I + 1;
    end
    else if not (IsASCIIAlphaNumeric(APath[I]) or
      (APath[I] = '-') or (APath[I] = '_') or
      (APath[I] = '.') or (APath[I] = '@')) then
      Exit(False);

  Result := True;
end;

function PlatformName: string;
begin
  Result := GetBuildOS + '-' + GetBuildArch;
end;

procedure EnsureDirectoryWithoutSymlinks(const ARoot,
  ARelativeDirectory: string);
var
  Candidate, Segment: string;
  I, SegmentStart: Integer;
begin
  Candidate := ExcludeTrailingPathDelimiter(ARoot);
  if HostPathIsSymlink(Candidate) then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package cache directory must not be a symbolic link: %s',
      [Candidate]);
  if not HostDirectoryExists(Candidate) and
     not ForceDirectories(Candidate) then
    raise EGocciaRemotePackageError.CreateFmt(
      'Could not create remote package cache directory: %s',
      [Candidate]);

  SegmentStart := 1;
  for I := 1 to Length(ARelativeDirectory) + 1 do
    if (I > Length(ARelativeDirectory)) or
       (ARelativeDirectory[I] = '/') then
    begin
      Segment := Copy(ARelativeDirectory, SegmentStart, I - SegmentStart);
      if Segment <> '' then
      begin
        Candidate := IncludeTrailingPathDelimiter(Candidate) + Segment;
        if HostPathIsSymlink(Candidate) then
          raise EGocciaRemotePackageError.CreateFmt(
            'Remote package cache path must not contain symbolic links: %s',
            [Candidate]);
        if not HostDirectoryExists(Candidate) and
           not CreateDir(Candidate) then
          raise EGocciaRemotePackageError.CreateFmt(
            'Could not create remote package cache directory: %s',
            [Candidate]);
      end;
      SegmentStart := I + 1;
    end;
end;

procedure WriteBytesAtomically(const APath: string; const ABytes: TBytes);
var
  Stream: TFileStream;
  TemporaryPath: string;
begin
  TemporaryPath := GetTempFileName(ExtractFileDir(APath), 'goc');
  Stream := TFileStream.Create(TemporaryPath, fmCreate);
  try
    if Length(ABytes) > 0 then
      Stream.WriteBuffer(ABytes[0], Length(ABytes));
  finally
    Stream.Free;
  end;

  try
    if HostFileExists(APath) and not DeleteFile(APath) then
      raise EGocciaRemotePackageError.CreateFmt(
        'Could not replace corrupt remote package cache artifact: %s',
        [APath]);
    if not RenameFile(TemporaryPath, APath) then
      raise EGocciaRemotePackageError.CreateFmt(
        'Could not commit remote package cache artifact: %s',
        [APath]);
  finally
    if HostFileExists(TemporaryPath) then
      DeleteFile(TemporaryPath);
  end;
end;

constructor TGocciaProviderRemotePackageResolver.Create(
  const ACacheDirectory: string);
begin
  inherited Create;
  FCacheDirectory := ACacheDirectory;
end;

function TGocciaProviderRemotePackageResolver.FetchArtifact(
  const AURL: string): TBytes;
var
  Headers: THTTPHeaders;
  Response: THTTPResponse;
begin
  SetLength(Headers, 0);
  Response := HTTPGet(AURL, Headers);
  if Response.StatusCode <> 200 then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package GET failed with HTTP %d for provider artifact.',
      [Response.StatusCode]);
  Result := Response.Body;
end;

function TGocciaProviderRemotePackageResolver.CacheRootForPackage(
  const AImportMapDirectory, ARepository,
  AResolvedReference: string): string;
var
  CacheBase: string;
begin
  if FCacheDirectory <> '' then
    CacheBase := ExpandHostFileName(FCacheDirectory)
  else
    CacheBase := IncludeTrailingPathDelimiter(AImportMapDirectory) +
      DEFAULT_CACHE_DIRECTORY;

  EnsureDirectoryWithoutSymlinks(CacheBase,
    PACKAGES_CACHE_DIRECTORY + '/github/' + ARepository + '/' +
    AResolvedReference);
  Result := IncludeTrailingPathDelimiter(CacheBase) +
    StringReplace(PACKAGES_CACHE_DIRECTORY + '/github/' + ARepository +
      '/' + AResolvedReference, '/', PathDelim, [rfReplaceAll]);
end;

function TGocciaProviderRemotePackageResolver.MaterializeArtifact(
  const ARepository, AResolvedReference, AArtifactPath,
  AExpectedHash, APackageCacheRoot: string): string;
var
  ArtifactBytes: TBytes;
  ArtifactDirectory, ArtifactURL, RelativeDirectory: string;
begin
  RelativeDirectory := ExtractFilePath(
    StringReplace(AArtifactPath, '/', PathDelim, [rfReplaceAll]));
  RelativeDirectory := StringReplace(
    ExcludeTrailingPathDelimiter(RelativeDirectory),
    PathDelim, '/', [rfReplaceAll]);
  EnsureDirectoryWithoutSymlinks(APackageCacheRoot, RelativeDirectory);

  Result := IncludeTrailingPathDelimiter(APackageCacheRoot) +
    StringReplace(AArtifactPath, '/', PathDelim, [rfReplaceAll]);
  if HostPathIsSymlink(Result) then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package cache artifact must not be a symbolic link: %s',
      [Result]);

  if HostFileExists(Result) then
  begin
    ArtifactBytes := ReadFileBytes(Result);
    if SHA256Hex(ArtifactBytes) = AExpectedHash then
      Exit;
  end;

  ArtifactURL := GITHUB_RAW_BASE_URL + ARepository + '/' +
    AResolvedReference + '/' + AArtifactPath;
  ArtifactBytes := FetchArtifact(ArtifactURL);
  if SHA256Hex(ArtifactBytes) <> AExpectedHash then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package artifact hash mismatch for "%s".',
      [AArtifactPath]);

  ArtifactDirectory := ExtractFileDir(Result);
  if not HostDirectoryExists(ArtifactDirectory) then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package cache directory is unavailable: %s',
      [ArtifactDirectory]);
  WriteBytesAtomically(Result, ArtifactBytes);
end;

function TGocciaProviderRemotePackageResolver.ResolvePackage(
  const AReference, AImportMapPath: string): string;
var
  ArtifactDescriptor, ArtifactsObject, LockEntry, LockObject,
    PackagesObject: TGocciaObjectValue;
  ArtifactHash, ArtifactPath, ArtifactPlatform, EntryPath,
    ImportMapDirectory, LockfilePath, PackageCacheRoot,
    Repository, ResolvedReference: string;
  ArtifactValue, LockValue, VersionValue: TGocciaValue;
  EntryMaterialized: Boolean;
begin
  ParseGitHubReference(AReference, Repository);
  ImportMapDirectory := ExtractFilePath(ExpandHostFileName(AImportMapPath));
  LockfilePath := IncludeTrailingPathDelimiter(ImportMapDirectory) +
    LOCKFILE_NAME;
  if not HostFileExists(LockfilePath) then
    raise EGocciaRemotePackageError.CreateFmt(
      'Remote package lockfile not found: %s',
      [LockfilePath]);

  LockValue := ReadJSONFile(LockfilePath);
  if not (LockValue is TGocciaObjectValue) then
    raise EGocciaRemotePackageError.Create(
      'Remote package lockfile must be a top-level JSON object.');
  if TGarbageCollector.Instance <> nil then
    TGarbageCollector.Instance.AddTempRoot(LockValue);
  try
    LockObject := TGocciaObjectValue(LockValue);
    VersionValue := LockObject.GetProperty(LOCKFILE_VERSION_PROPERTY);
    if not (VersionValue is TGocciaNumberLiteralValue) or
       (TGocciaNumberLiteralValue(VersionValue).Value <>
        LOCKFILE_SCHEMA_VERSION) then
      raise EGocciaRemotePackageError.CreateFmt(
        'Remote package lockfile version must be %d.',
        [LOCKFILE_SCHEMA_VERSION]);

    PackagesObject := RequireObjectProperty(LockObject,
      LOCKFILE_PACKAGES_PROPERTY, 'Remote package lockfile');
    ArtifactValue := PackagesObject.GetProperty(AReference);
    if not (ArtifactValue is TGocciaObjectValue) then
      raise EGocciaRemotePackageError.CreateFmt(
        'Remote package reference "%s" is not pinned in %s.',
        [AReference, LockfilePath]);
    LockEntry := TGocciaObjectValue(ArtifactValue);
    ResolvedReference := RequireStringProperty(LockEntry,
      LOCK_ENTRY_RESOLVED_REFERENCE_PROPERTY,
      'Remote package lock entry');
    if not IsLowercaseHex(ResolvedReference, GITHUB_COMMIT_LENGTH) then
      raise EGocciaRemotePackageError.Create(
        'Remote package resolvedRef must be a lowercase 40-character Git commit hash.');

    EntryPath := RequireStringProperty(LockEntry,
      LOCK_ENTRY_ENTRY_PROPERTY, 'Remote package lock entry');
    if not IsSafeArtifactPath(EntryPath) then
      raise EGocciaRemotePackageError.CreateFmt(
        'Remote package entry path is unsafe: %s', [EntryPath]);
    ArtifactsObject := RequireObjectProperty(LockEntry,
      LOCK_ENTRY_ARTIFACTS_PROPERTY, 'Remote package lock entry');
    PackageCacheRoot := CacheRootForPackage(ImportMapDirectory,
      Repository, ResolvedReference);

    EntryMaterialized := False;
    for ArtifactPath in ArtifactsObject.GetOwnPropertyKeys do
    begin
      if not IsSafeArtifactPath(ArtifactPath) then
        raise EGocciaRemotePackageError.CreateFmt(
          'Remote package artifact path is unsafe: %s', [ArtifactPath]);
      ArtifactValue := ArtifactsObject.GetProperty(ArtifactPath);
      if not (ArtifactValue is TGocciaObjectValue) then
        raise EGocciaRemotePackageError.CreateFmt(
          'Remote package artifact "%s" must be a JSON object.',
          [ArtifactPath]);
      ArtifactDescriptor := TGocciaObjectValue(ArtifactValue);
      ArtifactHash := RequireStringProperty(ArtifactDescriptor,
        ARTIFACT_HASH_PROPERTY, 'Remote package artifact');
      if not IsLowercaseHex(ArtifactHash, SHA256_HEX_LENGTH) then
        raise EGocciaRemotePackageError.CreateFmt(
          'Remote package artifact "%s" sha256 must be 64 lowercase hexadecimal characters.',
          [ArtifactPath]);
      ArtifactPlatform := OptionalStringProperty(ArtifactDescriptor,
        ARTIFACT_PLATFORM_PROPERTY, 'Remote package artifact');
      if (ArtifactPlatform <> '') and
         (ArtifactPlatform <> PlatformName) then
        Continue;

      MaterializeArtifact(Repository, ResolvedReference,
        ArtifactPath, ArtifactHash, PackageCacheRoot);
      if ArtifactPath = EntryPath then
        EntryMaterialized := True;
    end;

    if not EntryMaterialized then
      raise EGocciaRemotePackageError.CreateFmt(
        'Remote package entry "%s" is not declared for platform "%s".',
        [EntryPath, PlatformName]);
    Result := IncludeTrailingPathDelimiter(PackageCacheRoot) +
      StringReplace(EntryPath, '/', PathDelim, [rfReplaceAll]);
  finally
    if TGarbageCollector.Instance <> nil then
      TGarbageCollector.Instance.RemoveTempRoot(LockValue);
  end;
end;

end.
