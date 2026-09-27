unit Goccia.Packages.Install;

{ Install mode: creates and updates the pins of provider packages (ADR 0122).

  A run never resolves a ref or writes goccia.lock.json. This is the one
  place that does:

  - `--add <key>=github:<owner>/<repo>@<tag|commit>[/<path>]` adds or
    replaces an import-map entry and pins its package; the typed spec is the
    `import` grant for that package, for that invocation;
  - `--remove <key>` removes an entry and prunes what no entry needs;
  - `--install` re-derives every pin from the import map at its locked
    commit, fetching only what the cache lacks; `--frozen` refuses to change
    the lockfile, and `--check-refs` compares each pin with what its
    repository advertises;
  - `--update [key…]` re-resolves tag refs; a moved tag is an error unless
    `--accept-moved-tags`.

  A ref is a tag or a commit. A commit must be the tip of an advertised tag
  or branch when it is added, never a commit only `refs/pull/*` reaches:
  the raw host serves a commit that exists only in a fork under the
  upstream repository's name, so this is what ties a commit pin to the
  repository it names. Whenever a package's files must be fetched, its refs
  are listed again, so a lockfile edited to pin a fork commit is caught
  before its bytes are trusted. Bytes fetched for a pinned file must match
  the pin; only `--update` moves a pin to another commit.

  The order is safe: everything is fetched, hashed, and checked first; the
  cache is written next; then the lockfile and the import map, the import
  map last when entries are added and first when they are removed, so a
  failure leaves at worst a pin no entry uses, never an entry with no pin.
  Pruning removes the cache directories of pins the new lockfile dropped,
  without following links. One install at a time holds
  goccia.lock.json.lock. }

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  GitRefs,

  Goccia.Capabilities,
  Goccia.Packages.Address,
  Goccia.Packages.Crawl,
  Goccia.Packages.Lockfile,
  Goccia.Packages.Store,
  Goccia.Packages.Transport;

const
  INSTALL_EXIT_FAILURE = 1;
  INSTALL_EXIT_USAGE = 2;
  INSTALL_LOCK_SUFFIX = '.lock';
  DEFAULT_INSTALL_LOCK_TIMEOUT_MILLISECONDS = 10000;

type
  { Exit status 1: out of date, integrity, network, a moved tag, a deny, or
    a held install lock. Exit status 2: a usage error, or a malformed
    lockfile or import map. }
  EGocciaInstallError = class(Exception)
  private
    FExitCode: Integer;
  public
    constructor CreateWithCode(const AMessage: string;
      const AExitCode: Integer);
    property ExitCode: Integer read FExitCode;
  end;

  TGocciaInstallAdd = record
    Key: string;
    Spec: string;
  end;

  TGocciaInstallRequest = record
    Adds: array of TGocciaInstallAdd;
    Removes: array of string;
    Install: Boolean;
    Update: Boolean;
    { Import-map keys whose packages --update re-resolves; empty for all. }
    UpdateKeys: array of string;
    Frozen: Boolean;
    CheckRefs: Boolean;
    AcceptMovedTags: Boolean;
  end;

  TGocciaInstallLog = procedure(const ALine: string) of object;

  TGocciaImportMapState = record
    Exists: Boolean;
    Text: string;
    Entries: array of record
      Key: string;
      Value: string;
    end;
  end;

  TGocciaPackageWorkList = class;

  { How CheckRefs judges a package against its advertised refs. }
  TGocciaRefCheck = (
    { Pick the commit: a package the lockfile lacks, or --update of a tag. }
    grcResolve,
    { The pin is about to be trusted (a typed --add, or files to fetch): the
      tag must still name the locked commit, a commit pin must be a tip. }
    grcVerify,
    { --check-refs alone: as grcVerify, except that a commit pin that is no
      longer a tip is a warning, since branches advance. }
    grcReport);

  TGocciaPackageInstaller = class
  private
    FCacheDirectory: string;
    FCapabilities: TGocciaCapabilities;
    FEditable: Boolean;
    FImportMap: TGocciaImportMapState;
    FImportMapPath: string;
    FLockPath: string;
    FLockTimeoutMilliseconds: Integer;
    FNewLock: TGocciaLockfile;
    FOldLock: TGocciaLockfile;
    FOnAudit: TGocciaProviderAuditHandler;
    FOnInstallAudit: TGocciaProviderAuditHandler;
    FOnLog: TGocciaInstallLog;
    FProjectSpecifiers: TStringList;
    { Packages an entry was removed from, or replaced away from. }
    FReleasedPackages: TStringList;
    FRequest: TGocciaInstallRequest;
    FTransport: TGocciaProviderTransport;
    FWorks: TGocciaPackageWorkList;
    procedure Log(const ALine: string);
    procedure InstallAudit(const AAllowed: Boolean;
      const ASubject, AReason: string);
    function RefsGet(const AURL: string; out AStatusCode: Integer;
      out ABody: TBytes; out AError: string): Boolean;
    procedure ReadImportMap;
    procedure ReadOldLock;
    function ApplyEdits: Boolean;
    procedure CollectPackages;
    procedure CollectEntryPoints;
    procedure RefuseDenied;
    procedure CheckRefs(const AIndex: Integer; const ACheck: TGocciaRefCheck);
    procedure CrawlPackages;
    procedure BuildNewLock;
    procedure WriteCache;
    procedure WriteLock(const AChanged: Boolean);
    procedure WriteImportMap;
    procedure PruneCache;
    procedure ReportChanges;
    function SummaryLine: string;
    function DescribeDifferences: string;
  public
    { AImportMapPath is the import map to read (and, when AEditable, to
      write); goccia.lock.json and .goccia sit beside it. An import map that
      is a symbolic link is never written. ACapabilities are the grants from
      the command line and trusted config. }
    constructor Create(const AImportMapPath: string; const AEditable: Boolean;
      const ACapabilities: TGocciaCapabilities;
      const ATransport: TGocciaProviderTransport);
    destructor Destroy; override;
    procedure Run(const ARequest: TGocciaInstallRequest);

    property OnLog: TGocciaInstallLog read FOnLog write FOnLog;
    { import.provider: each grant and each request. }
    property OnAudit: TGocciaProviderAuditHandler read FOnAudit write FOnAudit;
    { import.provider.install: each change to a pin, or a refusal. }
    property OnInstallAudit: TGocciaProviderAuditHandler
      read FOnInstallAudit write FOnInstallAudit;
    { How long to wait for another install's lock before failing. }
    property LockTimeoutMilliseconds: Integer read FLockTimeoutMilliseconds
      write FLockTimeoutMilliseconds;
  end;

  { One package this run pins, with every import-map entry that names it. }
  TGocciaPackageWork = class
  public
    Key: string;
    Address: TGocciaProviderAddress;
    EntryKeys: TStringList;
    EntryPaths: TStringList;
    EntryPoints: TStringList;
    { A typed --add names it: the spec is its import grant. }
    Typed: Boolean;
    { This run re-derives its pins; otherwise its old pin is kept. }
    Affected: Boolean;
    { --update re-resolves its tag. }
    UpdateSelected: Boolean;
    { --frozen met a package the lockfile does not pin: reported, not
      resolved. }
    Unlocked: Boolean;
    Commit: string;
    RefKind: TGocciaLockRefKind;
    Old: TGocciaLockedPackage;
    Crawl: TGocciaPackageCrawl;
    { Paths whose bytes came from the verified cache, not the network. }
    Cached: TStringList;
    constructor Create;
    destructor Destroy; override;
    { Whether AKey is one of its import-map keys. }
    function HasEntry(const AKey: string): Boolean;
    { Whether the old pin applies at the commit this run pins. }
    function OldPinsCommit: Boolean;
  end;

  TGocciaPackageWorkList = class(TObjectList<TGocciaPackageWork>);

{ Splits `--add` AArgument, `<key>=github:…`. }
function TryParseAddArgument(const AArgument: string; out AKey, ASpec,
  AError: string): Boolean;

implementation

uses
  FileUtils,
  HostFileLock,
  SHA256,
  TextEncoding,

  Goccia.FileExtensions,
  Goccia.JSON.Utils,
  Goccia.Packages.ImportMapEdit;

const
  REFS_MAX_BYTES = 8 * 1024 * 1024;
  HTTP_STATUS_OK = 200;
  HTTP_STATUS_NOT_FOUND = 404;
  SHORT_COMMIT_LENGTH = 12;
  MAX_PROJECT_FILES = 20000;
  LOCK_RETRY_MILLISECONDS = 50;
  LOCK_FILE_MODE = &644;
  WRITE_TEMPORARY_INFIX = '.goccia-write-';
  PROJECT_SKIPPED_DIRECTORIES: array[0..2] of string = ('.goccia',
    'node_modules', '.git');

type
  { The crawl's fetcher for one package. The verified cache answers first.
    Otherwise the file is fetched, when the network is allowed, and must
    match its pin when the old lock pins it at this commit. }
  TGocciaPackageFetcher = class
  public
    Work: TGocciaPackageWork;
    CacheDirectory: string;
    Network: Boolean;
    Transport: TGocciaProviderTransport;
    OnAudit: TGocciaProviderAuditHandler;
    function Fetch(const APath: string; out ABytes: TBytes): Boolean;
  end;

constructor EGocciaInstallError.CreateWithCode(const AMessage: string;
  const AExitCode: Integer);
begin
  inherited Create(AMessage);
  FExitCode := AExitCode;
end;

procedure Fail(const AMessage: string);
begin
  raise EGocciaInstallError.CreateWithCode(AMessage, INSTALL_EXIT_FAILURE);
end;

procedure FailUsage(const AMessage: string);
begin
  raise EGocciaInstallError.CreateWithCode(AMessage, INSTALL_EXIT_USAGE);
end;

function ShortCommit(const ACommit: string): string;
begin
  Result := Copy(ACommit, 1, SHORT_COMMIT_LENGTH);
end;

{ An abbreviated or uppercase commit, which is never a ref. }
function LooksLikeCommit(const ARef: string): Boolean;
const
  MIN_ABBREVIATED_COMMIT_LENGTH = 7;
var
  I: Integer;
begin
  if (Length(ARef) < MIN_ABBREVIATED_COMMIT_LENGTH) or
     (Length(ARef) > GITHUB_COMMIT_LENGTH) then
    Exit(False);
  for I := 1 to Length(ARef) do
    if not CharInSet(ARef[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit(False);
  Result := True;
end;

function IsPrefixKey(const AKey: string): Boolean;
begin
  Result := (AKey <> '') and (AKey[Length(AKey)] = '/');
end;

function TryParseAddArgument(const AArgument: string; out AKey, ASpec,
  AError: string): Boolean;
var
  Address: TGocciaProviderAddress;
  SeparatorIndex: Integer;
begin
  AKey := '';
  ASpec := '';
  AError := '';
  SeparatorIndex := Pos('=', AArgument);
  if SeparatorIndex <= 1 then
  begin
    AError := Format('--add needs <key>=github:<owner>/<repo>@<ref>: "%s"',
      [AArgument]);
    Exit(False);
  end;
  AKey := Copy(AArgument, 1, SeparatorIndex - 1);
  ASpec := Copy(AArgument, SeparatorIndex + 1, MaxInt);
  if not TryParseProviderAddress(ASpec, Address, AError) then
  begin
    AError := Format('--add %s: %s', [AArgument, AError]);
    Exit(False);
  end;
  if IsPrefixKey(AKey) and not Address.IsPrefix then
  begin
    AError := Format('--add %s: the key ends with "/", so the address must ' +
      'name a directory ending with "/"', [AArgument]);
    Exit(False);
  end;
  Result := True;
end;

{ TGocciaPackageWork }

constructor TGocciaPackageWork.Create;
begin
  inherited Create;
  EntryKeys := TStringList.Create;
  EntryKeys.CaseSensitive := True;
  EntryPaths := TStringList.Create;
  EntryPaths.CaseSensitive := True;
  EntryPoints := TStringList.Create;
  EntryPoints.CaseSensitive := True;
  Cached := TStringList.Create;
  Cached.CaseSensitive := True;
end;

destructor TGocciaPackageWork.Destroy;
begin
  Crawl.Free;
  Cached.Free;
  EntryPoints.Free;
  EntryPaths.Free;
  EntryKeys.Free;
  inherited;
end;

function TGocciaPackageWork.HasEntry(const AKey: string): Boolean;
begin
  Result := EntryKeys.IndexOf(AKey) >= 0;
end;

function TGocciaPackageWork.OldPinsCommit: Boolean;
begin
  Result := Assigned(Old) and (Old.Commit = Commit);
end;

{ TGocciaPackageFetcher }

function TGocciaPackageFetcher.Fetch(const APath: string;
  out ABytes: TBytes): Boolean;
var
  Pin, URL: string;
  Pinned: Boolean;
  Response: TGocciaProviderResponse;
begin
  ABytes := nil;
  Pinned := Work.OldPinsCommit and Work.Old.FindArtifact(APath, Pin);
  if Pinned and ReadCacheFile(CacheDirectory, ToHostRelativePath(
     PackageCacheRelativeDirectory(Work.Address.Owner,
     Work.Address.Repository, Work.Commit) + '/' + APath), Work.Key,
     ABytes) and (SHA256Hex(ABytes) = Pin) then
  begin
    Work.Cached.Add(APath);
    Exit(True);
  end;
  ABytes := nil;
  if not Network then
    raise EGocciaCrawlNeedsFetch.CreateFmt('%s: %s is not cached',
      [Work.Key, APath]);
  URL := PackageArtifactURL(Work.Address.Owner, Work.Address.Repository,
    Work.Commit, APath);
  try
    Response := Transport.Get(URL, GITHUB_RAW_HOST,
      PROVIDER_MAX_ARTIFACT_BYTES);
  except
    on E: EGocciaProviderTransportError do
    begin
      if Assigned(OnAudit) then
        OnAudit(False, URL, E.Message);
      Fail(Format('%s: fetching %s failed: %s', [Work.Key, APath,
        E.Message]));
    end;
  end;
  if Assigned(OnAudit) then
    OnAudit(Response.StatusCode = HTTP_STATUS_OK, URL,
      Format('HTTP %d', [Response.StatusCode]));
  if (Response.StatusCode = HTTP_STATUS_NOT_FOUND) and not Pinned then
    Exit(False);
  if Response.StatusCode <> HTTP_STATUS_OK then
    Fail(Format('%s: fetching %s failed with HTTP %d', [Work.Key, APath,
      Response.StatusCode]));
  ABytes := Response.Body;
  { The commit is immutable, so bytes that differ from the pin are not an
    update: they are refused, and nothing is written. }
  if Pinned and (SHA256Hex(ABytes) <> Pin) then
  begin
    if Assigned(OnAudit) then
      OnAudit(False, URL, 'sha256 mismatch');
    Fail(Format('%s: %s does not match its pin in %s; the lockfile and the ' +
      'cache are unchanged', [Work.Key, APath, LOCKFILE_NAME]));
  end;
  Result := True;
end;

{ Every literal specifier in the project's own modules: a prefix entry pins
  the files its crawl reaches from the specifiers the project imports
  through it. Links and the cache, node_modules, and .git directories are
  not walked. }
function CollectProjectSpecifiers(const ARoot: string;
  const ALog: TGocciaInstallLog): TStringList;
var
  Files: Integer;

  function Skipped(const AName: string): Boolean;
  var
    I: Integer;
  begin
    for I := Low(PROJECT_SKIPPED_DIRECTORIES) to
        High(PROJECT_SKIPPED_DIRECTORIES) do
      if SameText(AName, PROJECT_SKIPPED_DIRECTORIES[I]) then
        Exit(True);
    Result := False;
  end;

  procedure Scan(const ADirectory: string);
  var
    SearchRecord: TSearchRec;
    Path, Source: string;
    References: TGocciaLiteralReferences;
    I: Integer;
  begin
    if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + '*', faAnyFile,
       SearchRecord) <> 0 then
      Exit;
    try
      repeat
        if (SearchRecord.Name = '.') or (SearchRecord.Name = '..') then
          Continue;
        Path := IncludeTrailingPathDelimiter(ADirectory) + SearchRecord.Name;
        if HostPathIsSymlink(Path) then
          Continue;
        if (SearchRecord.Attr and faDirectory) = faDirectory then
        begin
          if not Skipped(SearchRecord.Name) then
            Scan(Path);
          Continue;
        end;
        if not IsScriptExtension(ExtractFileExt(SearchRecord.Name)) then
          Continue;
        Inc(Files);
        if Files > MAX_PROJECT_FILES then
          Fail(Format('the project has more than %d modules to scan for ' +
            'prefix-entry imports', [MAX_PROJECT_FILES]));
        try
          Source := ReadUTF8FileText(Path);
          References := ExtractLiteralReferences(Source, Path);
        except
          on E: Exception do
          begin
            if Assigned(ALog) then
              ALog('Skipped ' + Path + ': ' + E.Message);
            Continue;
          end;
        end;
        for I := 0 to High(References) do
          if (References[I].Kind <> crkAsset) and
             (Result.IndexOf(References[I].Specifier) < 0) then
            Result.Add(References[I].Specifier);
      until FindNext(SearchRecord) <> 0;
    finally
      FindClose(SearchRecord);
    end;
  end;

begin
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  Files := 0;
  try
    Scan(ExcludeTrailingPathDelimiter(ARoot));
  except
    Result.Free;
    raise;
  end;
end;

{ The artifacts of APackage as `path=sha256` lines, for comparing pins. }
procedure ListArtifacts(const APackage: TGocciaLockedPackage;
  const ALines: TStringList);
var
  I: Integer;
begin
  ALines.Clear;
  if not Assigned(APackage) then
    Exit;
  for I := 0 to APackage.ArtifactCount - 1 do
    ALines.Values[APackage.Artifact(I).Path] := APackage.Artifact(I).SHA256;
end;

{ Files added, removed, and changed from AOld to ANew. }
procedure CountFileChanges(const AOld, ANew: TGocciaLockedPackage;
  out AAdded, ARemoved, AChanged: Integer);
var
  OldLines, NewLines: TStringList;
  I: Integer;
begin
  AAdded := 0;
  ARemoved := 0;
  AChanged := 0;
  OldLines := TStringList.Create;
  NewLines := TStringList.Create;
  try
    OldLines.CaseSensitive := True;
    NewLines.CaseSensitive := True;
    ListArtifacts(AOld, OldLines);
    ListArtifacts(ANew, NewLines);
    for I := 0 to NewLines.Count - 1 do
      if OldLines.IndexOfName(NewLines.Names[I]) < 0 then
        Inc(AAdded)
      else if OldLines.Values[NewLines.Names[I]] <>
        NewLines.ValueFromIndex[I] then
        Inc(AChanged);
    for I := 0 to OldLines.Count - 1 do
      if NewLines.IndexOfName(OldLines.Names[I]) < 0 then
        Inc(ARemoved);
  finally
    NewLines.Free;
    OldLines.Free;
  end;
end;

function SamePin(const AOld, ANew: TGocciaLockedPackage): Boolean;
var
  Added, Removed, Changed: Integer;
begin
  if not Assigned(AOld) or not Assigned(ANew) then
    Exit(False);
  CountFileChanges(AOld, ANew, Added, Removed, Changed);
  Result := (AOld.Commit = ANew.Commit) and (AOld.RefKind = ANew.RefKind) and
    (AOld.Key = ANew.Key) and (Added = 0) and (Removed = 0) and (Changed = 0);
end;

function CopyLockedPackage(const APackage: TGocciaLockedPackage;
  const AKey: string; const AAddress: TGocciaProviderAddress):
  TGocciaLockedPackage;
var
  I: Integer;
begin
  Result := TGocciaLockedPackage.Create(AKey, AAddress);
  Result.Commit := APackage.Commit;
  Result.RefKind := APackage.RefKind;
  for I := 0 to APackage.ArtifactCount - 1 do
    Result.AddArtifact(APackage.Artifact(I).Path,
      APackage.Artifact(I).SHA256);
end;

{ TGocciaPackageInstaller }

constructor TGocciaPackageInstaller.Create(const AImportMapPath: string;
  const AEditable: Boolean; const ACapabilities: TGocciaCapabilities;
  const ATransport: TGocciaProviderTransport);
begin
  inherited Create;
  FImportMapPath := ExpandHostFileName(AImportMapPath);
  FEditable := AEditable;
  FCapabilities := ACapabilities;
  FTransport := ATransport;
  FLockPath := IncludeTrailingPathDelimiter(ExtractFilePath(FImportMapPath)) +
    LOCKFILE_NAME;
  FCacheDirectory := PackageCacheDirectory(FImportMapPath);
  FLockTimeoutMilliseconds := DEFAULT_INSTALL_LOCK_TIMEOUT_MILLISECONDS;
  FReleasedPackages := TStringList.Create;
  FReleasedPackages.CaseSensitive := True;
end;

destructor TGocciaPackageInstaller.Destroy;
begin
  FReleasedPackages.Free;
  FWorks.Free;
  FProjectSpecifiers.Free;
  FNewLock.Free;
  FOldLock.Free;
  inherited;
end;

procedure TGocciaPackageInstaller.Log(const ALine: string);
begin
  if Assigned(FOnLog) then
    FOnLog(ALine);
end;

procedure TGocciaPackageInstaller.InstallAudit(const AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if Assigned(FOnInstallAudit) then
    FOnInstallAudit(AAllowed, ASubject, AReason);
end;

function TGocciaPackageInstaller.RefsGet(const AURL: string;
  out AStatusCode: Integer; out ABody: TBytes; out AError: string): Boolean;
var
  Response: TGocciaProviderResponse;
begin
  AStatusCode := 0;
  ABody := nil;
  AError := '';
  try
    Response := FTransport.Get(AURL, GITHUB_HOST, REFS_MAX_BYTES);
  except
    on E: EGocciaProviderTransportError do
    begin
      AError := E.Message;
      if Assigned(FOnAudit) then
        FOnAudit(False, AURL, E.Message);
      Exit(False);
    end;
  end;
  AStatusCode := Response.StatusCode;
  ABody := Response.Body;
  if Assigned(FOnAudit) then
    FOnAudit(Response.StatusCode = HTTP_STATUS_OK, AURL,
      Format('HTTP %d', [Response.StatusCode]));
  Result := True;
end;

{ Step 1: the import map as it is. Anything install mode cannot read, or
  that runs would reject, is a usage error. }
procedure TGocciaPackageInstaller.ReadImportMap;
var
  Entries: TGocciaImportMapEntries;
  Error: string;
  I: Integer;
begin
  FImportMap := Default(TGocciaImportMapState);
  FImportMap.Exists := HostFileExists(FImportMapPath);
  if HostPathIsSymlink(FImportMapPath) then
    { Replacing a link would write the file it points at, or turn it into a
      copy; either changes something the user did not name. }
    FEditable := False;
  if not FImportMap.Exists then
    Exit;
  try
    FImportMap.Text := ReadUTF8FileText(FImportMapPath);
  except
    on E: Exception do
      FailUsage(Format('cannot read %s: %s', [FImportMapPath, E.Message]));
  end;
  if not TryReadImportMapEntries(FImportMap.Text, Entries, Error) then
    FailUsage(Format('%s is not an import map install mode can read: %s',
      [FImportMapPath, Error]));
  SetLength(FImportMap.Entries, Length(Entries));
  for I := 0 to High(Entries) do
  begin
    if Entries[I].Key = '' then
      FailUsage(Format('%s has an import-map entry with an empty key',
        [FImportMapPath]));
    FImportMap.Entries[I].Key := Entries[I].Key;
    FImportMap.Entries[I].Value := Entries[I].Value;
  end;
end;

procedure TGocciaPackageInstaller.ReadOldLock;
begin
  if not HostFileExists(FLockPath) then
  begin
    FOldLock := TGocciaLockfile.Create;
    Exit;
  end;
  try
    FOldLock := LoadLockfile(FLockPath);
  except
    on E: EGocciaLockfileError do
      FailUsage(E.Message);
  end;
end;

{ Step 2: the import map as it will be. False when the edits cannot be
  written (an --import-map file, or a link): the lines to change are
  printed and nothing is written. }
function TGocciaPackageInstaller.ApplyEdits: Boolean;
var
  Address: TGocciaProviderAddress;
  Error: string;
  I, J, Index: Integer;

  function IndexOfKey(const AKey: string): Integer;
  var
    K: Integer;
  begin
    for K := 0 to High(FImportMap.Entries) do
      if FImportMap.Entries[K].Key = AKey then
        Exit(K);
    Result := -1;
  end;

begin
  for I := 0 to High(FRequest.Removes) do
  begin
    Index := IndexOfKey(FRequest.Removes[I]);
    if Index < 0 then
      FailUsage(Format('the import map %s has no entry %s', [FImportMapPath,
        QuoteJSONString(FRequest.Removes[I])]));
    if not IsProviderAddress(FImportMap.Entries[Index].Value) then
      FailUsage(Format('%s is not a provider entry; --remove removes only ' +
        'provider entries', [QuoteJSONString(FRequest.Removes[I])]));
  end;
  for I := 0 to High(FRequest.UpdateKeys) do
  begin
    Index := IndexOfKey(FRequest.UpdateKeys[I]);
    if (Index < 0) or not IsProviderAddress(FImportMap.Entries[Index].Value)
    then
      FailUsage(Format('the import map has no provider entry %s to update',
        [QuoteJSONString(FRequest.UpdateKeys[I])]));
  end;

  if not FEditable and ((Length(FRequest.Adds) > 0) or
     (Length(FRequest.Removes) > 0)) then
  begin
    for I := 0 to High(FRequest.Adds) do
      Log(Format('Add this entry to the "imports" of %s: %s: %s',
        [FImportMapPath, QuoteJSONString(FRequest.Adds[I].Key),
        QuoteJSONString(FRequest.Adds[I].Spec)]));
    for I := 0 to High(FRequest.Removes) do
      Log(Format('Remove the entry %s from the "imports" of %s',
        [QuoteJSONString(FRequest.Removes[I]), FImportMapPath]));
    Log('Then run GocciaRunner --install. Nothing was written.');
    Exit(False);
  end;

  for I := 0 to High(FRequest.Adds) do
  begin
    Index := IndexOfKey(FRequest.Adds[I].Key);
    if Index < 0 then
    begin
      SetLength(FImportMap.Entries, Length(FImportMap.Entries) + 1);
      Index := High(FImportMap.Entries);
      FImportMap.Entries[Index].Key := FRequest.Adds[I].Key;
    end
    else if FImportMap.Entries[Index].Value <> FRequest.Adds[I].Spec then
    begin
      Log(Format('Replace %s: %s -> %s', [QuoteJSONString(
        FRequest.Adds[I].Key), FImportMap.Entries[Index].Value,
        FRequest.Adds[I].Spec]));
      if TryParseProviderAddress(FImportMap.Entries[Index].Value, Address,
         Error) then
        FReleasedPackages.Add(Address.NormalizedPackageKey);
    end;
    FImportMap.Entries[Index].Value := FRequest.Adds[I].Spec;
  end;
  for I := 0 to High(FRequest.Removes) do
  begin
    Index := IndexOfKey(FRequest.Removes[I]);
    if TryParseProviderAddress(FImportMap.Entries[Index].Value, Address,
       Error) then
      FReleasedPackages.Add(Address.NormalizedPackageKey);
    for J := Index to High(FImportMap.Entries) - 1 do
      FImportMap.Entries[J] := FImportMap.Entries[J + 1];
    SetLength(FImportMap.Entries, Length(FImportMap.Entries) - 1);
  end;

  for I := 0 to High(FImportMap.Entries) do
    if IsProviderAddress(FImportMap.Entries[I].Value) and
       not TryParseProviderAddress(FImportMap.Entries[I].Value, Address,
         Error) then
      FailUsage(Format('import map entry %s: %s', [QuoteJSONString(
        FImportMap.Entries[I].Key), Error]));
  Result := True;
end;

{ Step 3: one work item per package the provider entries name. A package
  this run's request touches is re-derived from all of its entries; any
  other keeps its pin. }
procedure TGocciaPackageInstaller.CollectPackages;
var
  Address: TGocciaProviderAddress;
  Error: string;
  I, J: Integer;
  Work: TGocciaPackageWork;

  function FindWork(const AAddress: TGocciaProviderAddress): TGocciaPackageWork;
  var
    Candidate: TGocciaPackageWork;
  begin
    for Candidate in FWorks do
      if Candidate.Address.NormalizedPackageKey =
         AAddress.NormalizedPackageKey then
        Exit(Candidate);
    Result := nil;
  end;

begin
  FWorks := TGocciaPackageWorkList.Create(True);
  for I := 0 to High(FImportMap.Entries) do
  begin
    if not IsProviderAddress(FImportMap.Entries[I].Value) then
      Continue;
    TryParseProviderAddress(FImportMap.Entries[I].Value, Address, Error);
    Work := FindWork(Address);
    if not Assigned(Work) then
    begin
      Work := TGocciaPackageWork.Create;
      Work.Key := Address.PackageKey;
      Work.Address := Address;
      Work.Old := FOldLock.FindPackage(Work.Key);
      FWorks.Add(Work);
    end;
    Work.EntryKeys.Add(FImportMap.Entries[I].Key);
    Work.EntryPaths.Add(Address.Path);
    for J := 0 to High(FRequest.Adds) do
      if FRequest.Adds[J].Key = FImportMap.Entries[I].Key then
        Work.Typed := True;
  end;
  for Work in FWorks do
  begin
    Work.UpdateSelected := FRequest.Update and
      (Length(FRequest.UpdateKeys) = 0);
    for I := 0 to High(FRequest.UpdateKeys) do
      if Work.HasEntry(FRequest.UpdateKeys[I]) then
        Work.UpdateSelected := True;
    { A package that lost an entry is re-derived from the others. }
    Work.Affected := FRequest.Install or Work.Typed or Work.UpdateSelected or
      not Assigned(Work.Old) or
      (FReleasedPackages.IndexOf(Work.Address.NormalizedPackageKey) >= 0);
  end;
end;

{ Step 4: where each touched package's crawl starts. An exact entry starts
  at its path; a prefix entry at every specifier the project imports
  through it. A package nothing imports yet through its prefix entries is
  not pinned at all: it needs no grant and no network, and --frozen treats
  it as satisfied. }
procedure TGocciaPackageInstaller.CollectEntryPoints;
var
  Work: TGocciaPackageWork;
  I, J: Integer;
  Key, Specifier, Tail: string;
  NeedsScan: Boolean;
begin
  NeedsScan := False;
  for Work in FWorks do
    if Work.Affected then
      for I := 0 to Work.EntryKeys.Count - 1 do
        if IsPrefixKey(Work.EntryKeys[I]) then
          NeedsScan := True;
  if NeedsScan then
    FProjectSpecifiers := CollectProjectSpecifiers(
      ExtractFilePath(FImportMapPath), FOnLog);

  for I := FWorks.Count - 1 downto 0 do
  begin
    Work := FWorks[I];
    if not Work.Affected then
      Continue;
    for J := 0 to Work.EntryKeys.Count - 1 do
    begin
      Key := Work.EntryKeys[J];
      if not IsPrefixKey(Key) then
      begin
        if Work.EntryPoints.IndexOf(Work.EntryPaths[J]) < 0 then
          Work.EntryPoints.Add(Work.EntryPaths[J]);
        Continue;
      end;
      for Specifier in FProjectSpecifiers do
      begin
        if (Length(Specifier) <= Length(Key)) or
           (Copy(Specifier, 1, Length(Key)) <> Key) then
          Continue;
        Tail := Work.EntryPaths[J] + Copy(Specifier, Length(Key) + 1, MaxInt);
        if IsSafeArtifactPath(Tail) and (Work.EntryPoints.IndexOf(Tail) < 0) then
          Work.EntryPoints.Add(Tail);
      end;
    end;
    if Work.EntryPoints.Count = 0 then
    begin
      Log(Format('Skipped %s: no project module imports through its prefix ' +
        'entries yet', [Work.Key]));
      FWorks.Delete(I);
    end;
  end;
end;

{ Step 5: denies win over everything, a typed spec included. }
procedure TGocciaPackageInstaller.RefuseDenied;
var
  DenyScope: string;
  Work: TGocciaPackageWork;
begin
  for Work in FWorks do
  begin
    if not Work.Affected then
      Continue;
    if FCapabilities.ProviderPackageDenyScope(IMPORT_PROVIDER_GITHUB,
       Work.Address.Owner, Work.Address.Repository, DenyScope) then
    begin
      if DenyScope = '' then
        DenyScope := 'an unscoped import deny'
      else
        DenyScope := 'the import deny ' + DenyScope;
      InstallAudit(False, Work.Key, 'refused by ' + DenyScope);
      Fail(Format('import: %s (refused by %s)', [Work.Key, DenyScope]));
    end;
    if Work.Typed and Assigned(FOnAudit) then
      FOnAudit(True, Work.Key, 'the --add spec grants ' +
        Work.Address.ScopeText + ' for this invocation');
  end;
end;

{ Lists the package's refs and checks its pin against them, as ACheck
  says (see TGocciaRefCheck). }
procedure TGocciaPackageInstaller.CheckRefs(const AIndex: Integer;
  const ACheck: TGocciaRefCheck);
var
  Commit: string;
  Refs: TGitRefAdvertisement;
  Work: TGocciaPackageWork;
begin
  Work := FWorks[AIndex];
  if not Work.Typed and not FCapabilities.AllowsProviderPackage(
     IMPORT_PROVIDER_GITHUB, Work.Address.Owner, Work.Address.Repository) then
  begin
    InstallAudit(False, Work.Key, 'the import capability does not cover ' +
      Work.Address.ScopeText);
    Fail(Format('import: %s (listing its refs and fetching its files needs ' +
      '--allow-import=%s, or --allow-import=github:%s or github)',
      [Work.Key, Work.Address.ScopeText, LowerCase(Work.Address.Owner)]));
  end;
  if not Work.Typed and Assigned(FOnAudit) then
    FOnAudit(True, Work.Key, 'the import capability covers ' +
      Work.Address.ScopeText);
  try
    Refs := FetchRefAdvertisement(GitHubInfoRefsURL(Work.Address.Owner,
      Work.Address.Repository), RefsGet);
  except
    on E: EGitRefsError do
      Fail(Format('%s: %s', [Work.Key, E.Message]));
  end;
  try
    if IsCommitHash(Work.Address.Ref) then
    begin
      Commit := Work.Address.Ref;
      if not Refs.IsAdvertisedTip(Commit) then
      begin
        if ACheck = grcReport then
          Log(Format('Warning: %s: commit %s is no longer the tip of any ' +
            'tag or branch of %s/%s', [Work.Key, ShortCommit(Commit),
            Work.Address.Owner, Work.Address.Repository]))
        else
        begin
          InstallAudit(False, Work.Key,
            'the commit is not the tip of an advertised tag or branch');
          Fail(Format('%s: commit %s is not the tip of any tag or branch ' +
            'of %s/%s; pin a tag instead', [Work.Key, Commit,
            Work.Address.Owner, Work.Address.Repository]));
        end;
      end;
      Work.Commit := Commit;
      Work.RefKind := lrkCommit;
      Exit;
    end;

    if not Refs.FindTag(Work.Address.Ref, Commit) then
    begin
      InstallAudit(False, Work.Key, 'no such tag');
      if LooksLikeCommit(Work.Address.Ref) then
        Fail(Format('%s: %s/%s has no tag %s; a commit ref must be the full ' +
          '40-character lowercase commit', [Work.Key, Work.Address.Owner,
          Work.Address.Repository, Work.Address.Ref]));
      Fail(Format('%s: %s/%s has no tag %s (refs are tags or commits; ' +
        'branches are not accepted)', [Work.Key, Work.Address.Owner,
        Work.Address.Repository, Work.Address.Ref]));
    end;
    Work.RefKind := lrkTag;
    if Assigned(Work.Old) and (Commit <> Work.Old.Commit) and
       not ((ACheck = grcResolve) and FRequest.AcceptMovedTags) then
    begin
      InstallAudit(False, Work.Key, Format('tag moved %s -> %s',
        [ShortCommit(Work.Old.Commit), ShortCommit(Commit)]));
      Fail(Format('tag %s of %s/%s moved: locked %s, now %s. Tags are ' +
        'expected not to move; review the change, then re-pin it with ' +
        '--update --accept-moved-tags', [Work.Address.Ref,
        Work.Address.Owner, Work.Address.Repository,
        ShortCommit(Work.Old.Commit), ShortCommit(Commit)]));
    end;
    if ACheck = grcResolve then
    begin
      Work.Commit := Commit;
      Log(Format('Resolve %s -> tag %s at %s (%s)', [Work.Key,
        Work.Address.Ref, ShortCommit(Commit), GITHUB_HOST]));
    end
    else
      Log(Format('Checked %s: tag %s still names %s', [Work.Key,
        Work.Address.Ref, ShortCommit(Commit)]));
  finally
    Refs.Free;
  end;
end;

{ Step 6: each touched package's file set at its commit. A package the
  lockfile pins is crawled from the verified cache first, answering
  candidates from its pinned files as a run resolves them; only a file the
  cache cannot answer sends the crawl to the network, and then the refs are
  checked first. }
procedure TGocciaPackageInstaller.CrawlPackages;
var
  I, J: Integer;
  Work: TGocciaPackageWork;
  Fetcher: TGocciaPackageFetcher;
  Known: array of string;
  NeedsNetwork, Resolve: Boolean;

  procedure StartCrawl(const ANetwork: Boolean);
  var
    K: Integer;
  begin
    FreeAndNil(Work.Crawl);
    Work.Cached.Clear;
    Fetcher.Network := ANetwork;
    Work.Crawl := TGocciaPackageCrawl.Create(Work.Key, Fetcher.Fetch);
    if not ANetwork then
      Work.Crawl.SetKnownFiles(Known);
    for K := 0 to Work.EntryPoints.Count - 1 do
      Work.Crawl.AddModuleEntry(Work.EntryPoints[K]);
    try
      Work.Crawl.Run;
    except
      on E: EGocciaCrawlNeedsFetch do
        raise;
      on E: EGocciaCrawlError do
        Fail(E.Message);
      on E: EGocciaProviderPackageError do
        Fail(E.Message);
    end;
  end;

begin
  for I := 0 to FWorks.Count - 1 do
  begin
    Work := FWorks[I];
    if not Work.Affected then
      Continue;
    Resolve := not Assigned(Work.Old) or
      (Work.UpdateSelected and not IsCommitHash(Work.Address.Ref));
    if FRequest.Frozen and not Assigned(Work.Old) then
    begin
      Work.Unlocked := True;
      Continue;
    end;
    if Resolve then
      CheckRefs(I, grcResolve)
    else
    begin
      Work.Commit := Work.Old.Commit;
      Work.RefKind := Work.Old.RefKind;
    end;

    Fetcher := TGocciaPackageFetcher.Create;
    try
      Fetcher.Work := Work;
      Fetcher.CacheDirectory := FCacheDirectory;
      Fetcher.Transport := FTransport;
      Fetcher.OnAudit := FOnAudit;
      NeedsNetwork := Resolve or not Work.OldPinsCommit;
      if not NeedsNetwork then
      begin
        SetLength(Known, Work.Old.ArtifactCount);
        for J := 0 to Work.Old.ArtifactCount - 1 do
          Known[J] := Work.Old.Artifact(J).Path;
        try
          StartCrawl(False);
        except
          on E: EGocciaCrawlNeedsFetch do
            NeedsNetwork := True;
        end;
      end;
      if NeedsNetwork then
      begin
        { A fetch trusts the pin only as far as the repository still
          advertises it: a lockfile edited to name a fork commit is caught
          here, before its bytes are. }
        if not Resolve then
          CheckRefs(I, grcVerify);
        try
          StartCrawl(True);
        except
          on E: EGocciaCrawlNeedsFetch do
            Fail(E.Message);
        end;
      end
      else if Work.Typed then
        CheckRefs(I, grcVerify)
      else if FRequest.CheckRefs then
        CheckRefs(I, grcReport);
    finally
      Fetcher.Free;
    end;
    Log(Format('Pinned  %s: %d files (%d fetched, %d verified in the cache)',
      [Work.Key, Work.Crawl.Files.Count, Work.Crawl.Files.Count -
      Work.Cached.Count, Work.Cached.Count]));
  end;
end;

{ Step 7: the new lockfile. A touched package is pinned to what its crawl
  reached, under the key the import map writes; any other keeps its pin.
  The result is checked as the reader would check it, before anything is
  written. }
procedure TGocciaPackageInstaller.BuildNewLock;
var
  Work: TGocciaPackageWork;
  Locked: TGocciaLockedPackage;
  I: Integer;
begin
  FNewLock := TGocciaLockfile.Create;
  for Work in FWorks do
  begin
    if Work.Unlocked then
      Continue;
    if not Work.Affected then
    begin
      FNewLock.Packages.Add(CopyLockedPackage(Work.Old, Work.Key,
        Work.Address));
      Continue;
    end;
    Locked := TGocciaLockedPackage.Create(Work.Key, Work.Address);
    Locked.Commit := Work.Commit;
    Locked.RefKind := Work.RefKind;
    for I := 0 to Work.Crawl.Files.Count - 1 do
      Locked.AddArtifact(Work.Crawl.Files[I].Path,
        SHA256Hex(Work.Crawl.Files[I].Bytes));
    FNewLock.Packages.Add(Locked);
  end;
  { Case-colliding paths, or a path that is both file and directory, are
    refused here rather than after some of them reached the cache. }
  try
    ParseLockfile(SerializeLockfile(FNewLock), FLockPath).Free;
  except
    on E: EGocciaLockfileError do
      Fail(E.Message);
  end;
end;

{ The packages the new lockfile would change, one line each: `+` added,
  `-` removed, `~` changed. An unchanged package is not listed. }
function TGocciaPackageInstaller.DescribeDifferences: string;
var
  Package, Other: TGocciaLockedPackage;
  Work: TGocciaPackageWork;
  Added, Removed, Changed: Integer;
begin
  Result := '';
  for Work in FWorks do
    if Work.Unlocked then
      Result := Result + sLineBreak + '  + ' + Work.Key +
        '   (in the import map, not locked)';
  for Package in FNewLock.Packages do
  begin
    Other := FOldLock.FindPackage(Package.Key);
    if not Assigned(Other) then
      Result := Result + sLineBreak + '  + ' + Package.Key +
        '   (in the import map, not locked)'
    else if Other.Commit <> Package.Commit then
      Result := Result + sLineBreak + '  ~ ' + Package.Key + '   (' +
        ShortCommit(Other.Commit) + ' -> ' + ShortCommit(Package.Commit) + ')'
    else if not SamePin(Other, Package) then
    begin
      CountFileChanges(Other, Package, Added, Removed, Changed);
      Result := Result + sLineBreak + '  ~ ' + Package.Key + Format(
        '   (%d files added, %d removed, %d changed)',
        [Added, Removed, Changed]);
    end;
  end;
  for Package in FOldLock.Packages do
    if not Assigned(FNewLock.FindPackage(Package.Key)) then
      Result := Result + sLineBreak + '  - ' + Package.Key +
        '   (locked, no longer imported)';
end;

procedure TGocciaPackageInstaller.WriteCache;
var
  I: Integer;
  RelativeDirectory: string;
  Work: TGocciaPackageWork;
begin
  for Work in FWorks do
  begin
    if not Work.Affected or not Assigned(Work.Crawl) then
      Continue;
    RelativeDirectory := PackageCacheRelativeDirectory(Work.Address.Owner,
      Work.Address.Repository, Work.Commit);
    for I := 0 to Work.Crawl.Files.Count - 1 do
      if Work.Cached.IndexOf(Work.Crawl.Files[I].Path) < 0 then
        try
          WriteCacheFile(FCacheDirectory, ToHostRelativePath(
            RelativeDirectory + '/' + Work.Crawl.Files[I].Path), Work.Key,
            Work.Crawl.Files[I].Path, Work.Crawl.Files[I].Bytes);
        except
          on E: EGocciaProviderPackageError do
            Fail(E.Message);
        end;
  end;
end;

procedure TGocciaPackageInstaller.WriteLock(const AChanged: Boolean);
begin
  if not AChanged then
  begin
    Log(Format('Verified %s; unchanged', [LOCKFILE_NAME]));
    Exit;
  end;
  try
    SaveLockfile(FLockPath, FNewLock);
  except
    on E: EGocciaLockfileError do
      Fail(E.Message);
  end;
  Log(SummaryLine);
end;

{ The edits, as single-member splices that keep every other byte, and the
  file's mode. }
procedure TGocciaPackageInstaller.WriteImportMap;
var
  Bytes: TBytes;
  EditedText, Error, NewText: string;
  ErrorOffset, I: Integer;
  Mode: Cardinal;
  Written: Boolean;
begin
  if (Length(FRequest.Adds) = 0) and (Length(FRequest.Removes) = 0) then
    Exit;
  NewText := FImportMap.Text;
  for I := 0 to High(FRequest.Adds) do
  begin
    if not FImportMap.Exists and (I = 0) then
      NewText := NewImportMapText(FRequest.Adds[I].Key, FRequest.Adds[I].Spec)
    else if TrySetImportMapEntry(NewText, FRequest.Adds[I].Key,
       FRequest.Adds[I].Spec, EditedText, Error) then
      NewText := EditedText
    else
      Fail(Format('cannot edit %s: %s', [FImportMapPath, Error]));
    Log(Format('Add     %s: %s to %s imports', [QuoteJSONString(
      FRequest.Adds[I].Key), QuoteJSONString(FRequest.Adds[I].Spec),
      ExtractFileName(FImportMapPath)]));
  end;
  for I := 0 to High(FRequest.Removes) do
  begin
    if TryRemoveImportMapEntry(NewText, FRequest.Removes[I], EditedText,
       Error) then
      NewText := EditedText
    else
      Fail(Format('cannot edit %s: %s', [FImportMapPath, Error]));
    Log(Format('Remove  %s from %s imports', [QuoteJSONString(
      FRequest.Removes[I]), ExtractFileName(FImportMapPath)]));
  end;
  if not TryEncodeUTF8(NewText, Bytes, ErrorOffset) then
    Fail(Format('cannot encode %s', [FImportMapPath]));
  if FImportMap.Exists and TryHostFileMode(FImportMapPath, Mode) then
    Written := ReplaceHostFile(FImportMapPath, FImportMapPath +
      WRITE_TEMPORARY_INFIX + IntToStr(GetProcessID), Bytes, Mode, Error)
  else
    Written := ReplaceHostFile(FImportMapPath, FImportMapPath +
      WRITE_TEMPORARY_INFIX + IntToStr(GetProcessID), Bytes, Error);
  if not Written then
    Fail(Format('cannot write %s: %s', [FImportMapPath, Error]));
end;

{ Step 9: the cache directories of pins the new lockfile dropped. }
procedure TGocciaPackageInstaller.PruneCache;
var
  Error, RelativeDirectory: string;
  Kept, Locked: TGocciaLockedPackage;
  Found: Boolean;
begin
  for Locked in FOldLock.Packages do
  begin
    Found := False;
    for Kept in FNewLock.Packages do
      if SameText(Kept.Address.Owner, Locked.Address.Owner) and
         SameText(Kept.Address.Repository, Locked.Address.Repository) and
         (Kept.Commit = Locked.Commit) then
        Found := True;
    if Found then
      Continue;
    RelativeDirectory := ToHostRelativePath(PackageCacheRelativeDirectory(
      Locked.Address.Owner, Locked.Address.Repository, Locked.Commit));
    if not HostDirectoryExists(IncludeTrailingPathDelimiter(FCacheDirectory) +
       RelativeDirectory) then
      Continue;
    if RemoveHostTreeBeneath(FCacheDirectory, RelativeDirectory, Error) then
      Log(Format('Pruned  %s', [PACKAGE_CACHE_DIRECTORY_NAME + PathDelim +
        RelativeDirectory]))
    else
      Log(Format('Kept    %s: %s', [PACKAGE_CACHE_DIRECTORY_NAME + PathDelim +
        RelativeDirectory, Error]));
  end;
end;

{ `Wrote goccia.lock.json (…)`: every package added, removed, or updated,
  and every file added, removed, or changed within a package, counted. }
function TGocciaPackageInstaller.SummaryLine: string;
var
  Package, Other: TGocciaLockedPackage;
  PackagesAdded, PackagesRemoved, PackagesUpdated: Integer;
  FilesAdded, FilesRemoved, FilesChanged: Integer;
  Added, Removed, Changed: Integer;
begin
  PackagesAdded := 0;
  PackagesRemoved := 0;
  PackagesUpdated := 0;
  FilesAdded := 0;
  FilesRemoved := 0;
  FilesChanged := 0;
  for Package in FNewLock.Packages do
  begin
    Other := FOldLock.FindPackage(Package.Key);
    CountFileChanges(Other, Package, Added, Removed, Changed);
    Inc(FilesAdded, Added);
    Inc(FilesRemoved, Removed);
    Inc(FilesChanged, Changed);
    if not Assigned(Other) then
      Inc(PackagesAdded)
    else if not SamePin(Other, Package) then
      Inc(PackagesUpdated);
  end;
  for Package in FOldLock.Packages do
    if not Assigned(FNewLock.FindPackage(Package.Key)) then
    begin
      Inc(PackagesRemoved);
      Inc(FilesRemoved, Package.ArtifactCount);
    end;
  Result := Format('Wrote   %s (%d packages added, %d removed, %d updated; ' +
    '%d files added, %d removed, %d changed)', [LOCKFILE_NAME, PackagesAdded,
    PackagesRemoved, PackagesUpdated, FilesAdded, FilesRemoved, FilesChanged]);
end;

{ One import.provider.install event per package whose pin changed: added,
  updated (its commit or any of its files), or removed. }
procedure TGocciaPackageInstaller.ReportChanges;
var
  Package, Other: TGocciaLockedPackage;
  Added, Removed, Changed: Integer;
begin
  for Package in FNewLock.Packages do
  begin
    Other := FOldLock.FindPackage(Package.Key);
    CountFileChanges(Other, Package, Added, Removed, Changed);
    if not Assigned(Other) then
      InstallAudit(True, Package.Key, Format('added: %s -> %s; %d files',
        [LockRefKindName(Package.RefKind), Package.Commit, Added]))
    else if not SamePin(Other, Package) then
      InstallAudit(True, Package.Key, Format('updated: %s -> %s; %d files ' +
        'added, %d removed, %d changed', [Other.Commit, Package.Commit, Added,
        Removed, Changed]));
  end;
  for Package in FOldLock.Packages do
    if not Assigned(FNewLock.FindPackage(Package.Key)) then
      InstallAudit(True, Package.Key, Format('removed; %d files',
        [Package.ArtifactCount]));
end;

procedure TGocciaPackageInstaller.Run(const ARequest: TGocciaInstallRequest);
var
  Changed, HasUnlocked: Boolean;
  InstallLock: THostFileLock;
  LockError: string;
  Waited: Integer;
  Work: TGocciaPackageWork;
begin
  FRequest := ARequest;
  { One install at a time: another would read the lockfile this one is
    about to replace. The system releases the lock if this process dies. }
  Waited := 0;
  repeat
    case TryAcquireHostFileLock(FLockPath + INSTALL_LOCK_SUFFIX,
      LOCK_FILE_MODE, InstallLock, LockError) of
      hflAcquired:
        Break;
      hflFailed:
        Fail(LockError);
    end;
    if Waited >= FLockTimeoutMilliseconds then
      Fail(Format('%s is locked by another GocciaScript install (%s); retry ' +
        'when it finishes', [FLockPath, FLockPath + INSTALL_LOCK_SUFFIX]));
    Sleep(LOCK_RETRY_MILLISECONDS);
    Inc(Waited, LOCK_RETRY_MILLISECONDS);
  until False;
  try
    ReadImportMap;
    ReadOldLock;
    if not ApplyEdits then
      Exit;
    CollectPackages;
    CollectEntryPoints;
    RefuseDenied;
    CrawlPackages;
    BuildNewLock;

    HasUnlocked := False;
    for Work in FWorks do
      if Work.Unlocked then
        HasUnlocked := True;
    Changed := HasUnlocked or
      (SerializeLockfile(FOldLock) <> SerializeLockfile(FNewLock));
    if FRequest.Frozen and Changed then
      Fail(Format('%s is out of date; --frozen does not write it.%s%sRun ' +
        'GocciaRunner --install without --frozen and commit %s.',
        [LOCKFILE_NAME, DescribeDifferences, sLineBreak, LOCKFILE_NAME]));

    WriteCache;
    { Removing an entry: the import map first, so a failure leaves a pin no
      entry uses. Adding one: the lockfile first, so it leaves no entry
      without a pin. }
    if Length(FRequest.Removes) > 0 then
    begin
      WriteImportMap;
      WriteLock(Changed);
    end
    else
    begin
      WriteLock(Changed);
      WriteImportMap;
    end;
    ReportChanges;
    PruneCache;
  finally
    ReleaseHostFileLock(InstallLock);
  end;
end;

end.
