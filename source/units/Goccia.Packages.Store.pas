unit Goccia.Packages.Store;

{ Materialized provider packages (ADR 0122).

  A run materializes a package when an import first resolves through it:
  every file the lockfile pins, for every platform, lands under
  `<import map directory>/.goccia/packages/github/<owner>/<repo>/<commit>/`.
  A cached file whose SHA-256 matches its pin is reused without network
  access; a missing or mismatching one is fetched from the pinned URL,
  verified, and only then committed, through a temporary created beside it
  and renamed into place. Nothing in the cache may be a symbolic link: the
  walk from `.goccia` down opens each directory without following one.

  The store also remembers every package it materialized, so the module
  loader and `FFI.open` can hash the exact bytes they are about to compile,
  read, or load against the pin (verify on load). A file inside a
  materialized package that the lockfile does not pin is refused outright.

  A run never resolves a ref and never writes the lockfile. }

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Packages.Address,
  Goccia.Packages.Lockfile,
  Goccia.Packages.Transport;

type
  { A package could not be materialized. Message names the package and the
    package-relative file only, so it is safe to show a script; Detail adds
    host-side context such as the lockfile path. }
  EGocciaProviderPackageError = class(Exception)
  private
    FDetail: string;
  public
    constructor CreateDetailed(const AMessage, ADetail: string);
    property Detail: string read FDetail;
  end;

  { The bytes about to be used from a materialized package do not match the
    lockfile, or the file is not pinned at all. }
  EGocciaProviderVerificationError = class(Exception);

  { Reports one provider decision for the capability audit log: a fetch, a
    cache or load verification, allowed or refused. }
  TGocciaProviderAuditHandler = procedure(const AAllowed: Boolean;
    const ASubject, AReason: string) of object;

  TGocciaMaterializedPackage = class
  private
    FAddress: TGocciaProviderAddress;
    FCanonicalRoot: string;
    FCommit: string;
    FKey: string;
    FLockPath: string;
    FPins: TDictionary<string, string>;
    FRoot: string;
  public
    constructor Create(const ALocked: TGocciaLockedPackage;
      const ALockPath, ARoot: string);
    destructor Destroy; override;
    { The pinned SHA-256 of APath, `/`-separated and relative to the
      package root. }
    function TryGetPin(const APath: string; out ASHA256: string): Boolean;

    property Address: TGocciaProviderAddress read FAddress;
    property CanonicalRoot: string read FCanonicalRoot;
    property Commit: string read FCommit;
    property Key: string read FKey;
    property LockPath: string read FLockPath;
    { The package root as resolution spells it: expanded, links not
      resolved, no trailing separator. }
    property Root: string read FRoot;
  end;

  TGocciaMaterializedPackageList = TObjectList<TGocciaMaterializedPackage>;

  TGocciaProviderPackageStore = class
  private
    FCachedOnly: Boolean;
    FLockfiles: TObjectDictionary<string, TGocciaLockfile>;
    FOnAudit: TGocciaProviderAuditHandler;
    FOwnsTransport: Boolean;
    FPackages: TGocciaMaterializedPackageList;
    FTransport: TGocciaProviderTransport;
    procedure Audit(const AAllowed: Boolean; const ASubject, AReason: string);
    function Lockfile(const ALockPath: string): TGocciaLockfile;
    procedure MaterializeArtifact(const ALocked: TGocciaLockedPackage;
      const ACacheDirectory, ARelativeDirectory: string;
      const AArtifact: TGocciaLockedArtifact);
    function FetchArtifact(const ALocked: TGocciaLockedPackage;
      const AArtifact: TGocciaLockedArtifact): TBytes;
  public
    { ATransport nil selects the HTTP transport. }
    constructor Create(const ATransport: TGocciaProviderTransport = nil;
      const AOwnsTransport: Boolean = True);
    destructor Destroy; override;

    { Materializes the package AAddress names, as pinned in the
      goccia.lock.json beside AImportMapPath, and remembers it. A package
      already materialized from that lockfile is returned as it is. }
    function Materialize(const AAddress: TGocciaProviderAddress;
      const AImportMapPath: string): TGocciaMaterializedPackage;

    { The materialized package whose root contains APath, with APath
      relative to that root (`/`-separated); nil when none does. APath is
      matched as spelled and, failing that, canonically. }
    function FindPackage(const APath: string;
      out ARelativePath: string): TGocciaMaterializedPackage;

    { Verify on load: when APath lies inside a materialized package, ABytes
      must be exactly the bytes the lockfile pins for it. Raises
      EGocciaProviderVerificationError otherwise. A path outside every
      package is left alone. }
    procedure VerifyContent(const APath: string; const ABytes: TBytes);

    { Whether APath lies inside a materialized package. }
    function OwnsPath(const APath: string): Boolean;

    { Refuse the network: a file missing from the cache, or not matching its
      pin, is an error instead of a fetch. }
    property CachedOnly: Boolean read FCachedOnly write FCachedOnly;
    property OnAudit: TGocciaProviderAuditHandler read FOnAudit write FOnAudit;
  end;

{ Runs of `/` in APath as the host's separator. }
function ToHostRelativePath(const APath: string): string;

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}

  FileUtils,
  SHA256,
  TextEncoding,

  Goccia.Capabilities;

const
  TEMPORARY_ARTIFACT_INFIX = '.goccia-download-';
  HTTP_STATUS_OK = 200;

function ToHostRelativePath(const APath: string): string;
begin
  Result := StringReplace(APath, '/', PathDelim, [rfReplaceAll]);
end;

function FromHostRelativePath(const APath: string): string;
begin
  Result := StringReplace(APath, PathDelim, '/', [rfReplaceAll]);
end;

{ An existing cache entry must be a regular file reached without a link. }
function IsRegularCacheFile(const APath: string): Boolean;
{$IFDEF UNIX}
var
  ErrorOffset: Integer;
  Info: Stat;
  PathBytes: TBytes;
begin
  Result := TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) and
    (fpLStat(PAnsiChar(@PathBytes[0]), Info) = 0) and
    fpS_ISREG(Info.st_mode);
end;
{$ELSE}
var
  Attributes: LongInt;
begin
  Attributes := FileGetAttr(APath);
  Result := (Attributes <> -1) and
    ((Attributes and (faDirectory or faSymLink)) = 0);
end;
{$ENDIF}

function PathEntryExists(const APath: string): Boolean;
begin
  Result := HostPathIsSymlink(APath) or HostFileExists(APath) or
    HostDirectoryExists(APath);
end;

{ EGocciaProviderPackageError }

constructor EGocciaProviderPackageError.CreateDetailed(const AMessage,
  ADetail: string);
begin
  inherited Create(AMessage);
  FDetail := ADetail;
end;

{ TGocciaMaterializedPackage }

constructor TGocciaMaterializedPackage.Create(
  const ALocked: TGocciaLockedPackage; const ALockPath, ARoot: string);
var
  I: Integer;
  Canonical: string;
begin
  inherited Create;
  FAddress := ALocked.Address;
  FKey := ALocked.Key;
  FCommit := ALocked.Commit;
  FLockPath := ALockPath;
  FRoot := ExcludeTrailingPathDelimiter(ExpandHostFileName(ARoot));
  Canonical := CanonicalCapabilityPath(FRoot);
  if Canonical <> '' then
    FCanonicalRoot := Canonical
  else
    FCanonicalRoot := FRoot;
  FPins := TDictionary<string, string>.Create;
  for I := 0 to ALocked.ArtifactCount - 1 do
    FPins.Add(ALocked.Artifact(I).Path, ALocked.Artifact(I).SHA256);
end;

destructor TGocciaMaterializedPackage.Destroy;
begin
  FPins.Free;
  inherited;
end;

function TGocciaMaterializedPackage.TryGetPin(const APath: string;
  out ASHA256: string): Boolean;
begin
  Result := FPins.TryGetValue(APath, ASHA256);
end;

{ TGocciaProviderPackageStore }

constructor TGocciaProviderPackageStore.Create(
  const ATransport: TGocciaProviderTransport; const AOwnsTransport: Boolean);
begin
  inherited Create;
  if Assigned(ATransport) then
  begin
    FTransport := ATransport;
    FOwnsTransport := AOwnsTransport;
  end
  else
  begin
    FTransport := TGocciaHTTPProviderTransport.Create;
    FOwnsTransport := True;
  end;
  FPackages := TGocciaMaterializedPackageList.Create(True);
  FLockfiles := TObjectDictionary<string, TGocciaLockfile>.Create(
    [doOwnsValues]);
end;

destructor TGocciaProviderPackageStore.Destroy;
begin
  FLockfiles.Free;
  FPackages.Free;
  if FOwnsTransport then
    FTransport.Free;
  inherited;
end;

procedure TGocciaProviderPackageStore.Audit(const AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if Assigned(FOnAudit) then
    FOnAudit(AAllowed, ASubject, AReason);
end;

function TGocciaProviderPackageStore.Lockfile(
  const ALockPath: string): TGocciaLockfile;
begin
  if FLockfiles.TryGetValue(ALockPath, Result) then
    Exit;
  try
    Result := LoadLockfile(ALockPath);
  except
    on E: EGocciaLockfileError do
      raise EGocciaProviderPackageError.CreateDetailed(
        'the provider lockfile ' + LOCKFILE_NAME + ' is missing or invalid',
        E.Message);
  end;
  FLockfiles.Add(ALockPath, Result);
end;

function TGocciaProviderPackageStore.FetchArtifact(
  const ALocked: TGocciaLockedPackage;
  const AArtifact: TGocciaLockedArtifact): TBytes;
var
  Response: TGocciaProviderResponse;
  URL: string;
begin
  URL := PackageArtifactURL(ALocked.Address.Owner, ALocked.Address.Repository,
    ALocked.Commit, AArtifact.Path);
  try
    Response := FTransport.Get(URL, GITHUB_RAW_HOST,
      PROVIDER_MAX_ARTIFACT_BYTES);
  except
    on E: EGocciaProviderTransportError do
    begin
      Audit(False, URL, E.Message);
      raise EGocciaProviderPackageError.CreateDetailed(Format(
        '%s: fetching %s failed', [ALocked.Key, AArtifact.Path]), E.Message);
    end;
  end;
  if Response.StatusCode <> HTTP_STATUS_OK then
  begin
    Audit(False, URL, Format('HTTP %d', [Response.StatusCode]));
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s: fetching %s failed with HTTP %d',
      [ALocked.Key, AArtifact.Path, Response.StatusCode]), URL);
  end;
  Result := Response.Body;
  if SHA256Hex(Result) <> AArtifact.SHA256 then
  begin
    Audit(False, URL, 'sha256 mismatch');
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s: %s does not match its SHA-256 in %s',
      [ALocked.Key, AArtifact.Path, LOCKFILE_NAME]), URL);
  end;
  Audit(True, URL, 'sha256 ok');
end;

procedure TGocciaProviderPackageStore.MaterializeArtifact(
  const ALocked: TGocciaLockedPackage;
  const ACacheDirectory, ARelativeDirectory: string;
  const AArtifact: TGocciaLockedArtifact);
var
  Bytes: TBytes;
  CacheIdentity: THostDirectoryIdentity;
  Candidate, ErrorMessage, HostRelative, Segment, Subject: string;
  I, SegmentStart: Integer;
  Mismatched: Boolean;
begin
  Mismatched := False;
  Subject := ALocked.Key + '/' + AArtifact.Path;
  HostRelative := ToHostRelativePath(ARelativeDirectory + '/' +
    AArtifact.Path);

  { No component below .goccia may be a link: a link would let the cache
    reach outside it, or a planted one hand a run bytes it never fetched. }
  Candidate := ExcludeTrailingPathDelimiter(ACacheDirectory);
  SegmentStart := 1;
  for I := 1 to Length(HostRelative) + 1 do
    if (I > Length(HostRelative)) or (HostRelative[I] = PathDelim) then
    begin
      Segment := Copy(HostRelative, SegmentStart, I - SegmentStart);
      SegmentStart := I + 1;
      Candidate := Candidate + PathDelim + Segment;
      if HostPathIsSymlink(Candidate) then
        raise EGocciaProviderPackageError.CreateDetailed(Format(
          '%s: the package cache must not contain symbolic links',
          [ALocked.Key]), Candidate);
      if not PathEntryExists(Candidate) then
        Break;
    end;

  Candidate := IncludeTrailingPathDelimiter(ACacheDirectory) + HostRelative;
  if PathEntryExists(Candidate) then
  begin
    if not IsRegularCacheFile(Candidate) then
      raise EGocciaProviderPackageError.CreateDetailed(Format(
        '%s: %s in the package cache is not a regular file',
        [ALocked.Key, AArtifact.Path]), Candidate);
    Bytes := ReadFileBytes(Candidate);
    if SHA256Hex(Bytes) = AArtifact.SHA256 then
    begin
      Audit(True, Subject, 'the cached file matches its pin');
      Exit;
    end;
    Audit(False, Subject, 'the cached file does not match its pin');
    Mismatched := True;
  end;

  if FCachedOnly and Mismatched then
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s: the cached %s does not match its pin, and --cached-only ' +
      'refuses the network', [ALocked.Key, AArtifact.Path]), Candidate);
  if FCachedOnly then
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s is not cached (%s), and --cached-only refuses the network',
      [ALocked.Key, AArtifact.Path]), Candidate);

  Bytes := FetchArtifact(ALocked, AArtifact);
  if not TryHostDirectoryIdentity(ACacheDirectory, CacheIdentity) then
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s: the package cache directory is unavailable', [ALocked.Key]),
      ACacheDirectory);
  { The temporary is named for this process and thread, so engines
    resolving one package in parallel never share one. The walk from the
    cache directory opens each directory without following a link. }
  if not ReplaceHostFileBeneath(ACacheDirectory, CacheIdentity, HostRelative,
     TEMPORARY_ARTIFACT_INFIX + IntToStr(GetProcessID) + '-' +
     IntToStr(PtrUInt(GetCurrentThreadId)), Bytes, ErrorMessage) then
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s: %s could not be written to the package cache',
      [ALocked.Key, AArtifact.Path]), ErrorMessage);
end;

function TGocciaProviderPackageStore.Materialize(
  const AAddress: TGocciaProviderAddress;
  const AImportMapPath: string): TGocciaMaterializedPackage;
var
  CacheDirectory, ImportMapDirectory, LockPath, RelativeDirectory: string;
  I: Integer;
  Known: TGocciaMaterializedPackage;
  Locked: TGocciaLockedPackage;
begin
  ImportMapDirectory := ExtractFilePath(ExpandHostFileName(AImportMapPath));
  LockPath := IncludeTrailingPathDelimiter(ImportMapDirectory) + LOCKFILE_NAME;
  for Known in FPackages do
    if (Known.Key = AAddress.PackageKey) and (Known.LockPath = LockPath) then
      Exit(Known);

  Locked := Lockfile(LockPath).FindPackage(AAddress.PackageKey);
  if not Assigned(Locked) then
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s is not pinned in %s', [AAddress.PackageKey, LOCKFILE_NAME]),
      LockPath);

  CacheDirectory := IncludeTrailingPathDelimiter(ImportMapDirectory) +
    PACKAGE_CACHE_DIRECTORY_NAME;
  if HostPathIsSymlink(CacheDirectory) then
    raise EGocciaProviderPackageError.CreateDetailed(Format(
      '%s: the package cache directory must not be a symbolic link',
      [Locked.Key]), CacheDirectory);
  if not HostDirectoryExists(CacheDirectory) then
  begin
    if FCachedOnly then
      raise EGocciaProviderPackageError.CreateDetailed(Format(
        '%s is not cached, and --cached-only refuses the network',
        [Locked.Key]), CacheDirectory);
    if not CreateDir(CacheDirectory) then
      raise EGocciaProviderPackageError.CreateDetailed(Format(
        '%s: the package cache directory could not be created',
        [Locked.Key]), CacheDirectory);
  end;

  RelativeDirectory := PackageCacheRelativeDirectory(Locked.Address.Owner,
    Locked.Address.Repository, Locked.Commit);
  for I := 0 to Locked.ArtifactCount - 1 do
    MaterializeArtifact(Locked, CacheDirectory, RelativeDirectory,
      Locked.Artifact(I));

  Result := TGocciaMaterializedPackage.Create(Locked, LockPath,
    IncludeTrailingPathDelimiter(CacheDirectory) +
    ToHostRelativePath(RelativeDirectory));
  FPackages.Add(Result);
end;

function RelativeWithin(const APath, ARoot: string;
  out ARelativePath: string): Boolean;
begin
  Result := (ARoot <> '') and IsPathWithinScope(APath, ARoot) and
    (Length(APath) > Length(ARoot));
  if Result then
    ARelativePath := FromHostRelativePath(Copy(APath, Length(ARoot) + 2,
      MaxInt));
end;

function TGocciaProviderPackageStore.FindPackage(const APath: string;
  out ARelativePath: string): TGocciaMaterializedPackage;
var
  Canonical, Expanded: string;
  Package: TGocciaMaterializedPackage;
begin
  ARelativePath := '';
  Result := nil;
  if (APath = '') or (FPackages.Count = 0) then
    Exit;
  Expanded := ExcludeTrailingPathDelimiter(ExpandHostFileName(APath));
  for Package in FPackages do
    if RelativeWithin(Expanded, Package.Root, ARelativePath) then
      Exit(Package);
  Canonical := CanonicalCapabilityPath(APath);
  for Package in FPackages do
    if RelativeWithin(Canonical, Package.CanonicalRoot, ARelativePath) then
      Exit(Package);
end;

function TGocciaProviderPackageStore.OwnsPath(const APath: string): Boolean;
var
  RelativePath: string;
begin
  Result := Assigned(FindPackage(APath, RelativePath));
end;

procedure TGocciaProviderPackageStore.VerifyContent(const APath: string;
  const ABytes: TBytes);
var
  Package: TGocciaMaterializedPackage;
  Pin, RelativePath, Subject: string;
begin
  Package := FindPackage(APath, RelativePath);
  if not Assigned(Package) then
    Exit;
  Subject := Package.Key + '/' + RelativePath;
  if not Package.TryGetPin(RelativePath, Pin) then
  begin
    Audit(False, Subject, 'the file is not pinned in ' + LOCKFILE_NAME);
    raise EGocciaProviderVerificationError.CreateFmt(
      'Provider package file %s is not pinned in %s', [Subject,
        LOCKFILE_NAME]);
  end;
  if SHA256Hex(ABytes) <> Pin then
  begin
    Audit(False, Subject, 'the loaded bytes do not match the pin');
    raise EGocciaProviderVerificationError.CreateFmt(
      'Provider package file %s changed after it was verified', [Subject]);
  end;
  Audit(True, Subject, 'the loaded bytes match the pin');
end;

end.
