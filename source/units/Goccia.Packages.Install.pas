unit Goccia.Packages.Install;

{ Install mode: creates and updates the pins of provider packages (ADR 0122).

  A run never resolves a ref or writes goccia.lock.json. This is the one
  place that does:

  - `--add <key>=github:<owner>/<repo>@<tag|commit>[/<path>]` adds an
    import-map entry and pins its package; the typed spec is the `import`
    grant for that package, for that invocation;
  - `--remove <key>` removes an entry and prunes what no entry needs;
  - `--install` pins every entry the lockfile lacks and materializes every
    pin, without re-resolving a locked ref; `--frozen` refuses to change the
    lockfile, and `--check-refs` proves each pin is still what its tag or a
    branch advertises;
  - `--update [key…]` re-resolves refs; a moved tag is an error unless
    `--accept-moved-tags`.

  A ref is a tag or a commit. A commit must be the tip of an advertised tag
  or branch, never only reachable through `refs/pull/*`: a raw URL serves a
  commit that exists only in a fork, so this is what ties a commit pin to
  the repository it names.

  The order is safe: everything is fetched and hashed in memory first, then
  the cache is written, then the lockfile, and the import map last, so a
  failure leaves the import map naming only packages the lockfile pins.
  Pruning removes the cache directories of pins the new lockfile dropped. }

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

type
  { Exit status 1: out of date, integrity, network, a moved tag, or a deny.
    Exit status 2: a usage error or a malformed lockfile or import map. }
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

  TGocciaPackageInstaller = class
  private
    FCacheDirectory: string;
    FCapabilities: TGocciaCapabilities;
    FEditable: Boolean;
    FImportMapPath: string;
    FLockPath: string;
    FOnAudit: TGocciaProviderAuditHandler;
    FOnInstallAudit: TGocciaProviderAuditHandler;
    FOnLog: TGocciaInstallLog;
    FTransport: TGocciaProviderTransport;
    procedure Log(const ALine: string);
    procedure InstallAudit(const AAllowed: Boolean;
      const ASubject, AReason: string);
    function RefsGet(const AURL: string; out AStatusCode: Integer;
      out ABody: TBytes; out AError: string): Boolean;
  public
    { AImportMapPath is the import map to read (and, when AEditable, to
      write); goccia.lock.json and .goccia sit beside it. ACapabilities are
      the grants from the command line and trusted config. }
    constructor Create(const AImportMapPath: string; const AEditable: Boolean;
      const ACapabilities: TGocciaCapabilities;
      const ATransport: TGocciaProviderTransport);
    procedure Run(const ARequest: TGocciaInstallRequest);

    property OnLog: TGocciaInstallLog read FOnLog write FOnLog;
    { import.provider: each fetch. }
    property OnAudit: TGocciaProviderAuditHandler read FOnAudit write FOnAudit;
    { import.provider.install: each change to a pin, or a refusal. }
    property OnInstallAudit: TGocciaProviderAuditHandler
      read FOnInstallAudit write FOnInstallAudit;
  end;

{ Splits `--add` AArgument, `<key>=github:…`. }
function TryParseAddArgument(const AArgument: string; out AKey, ASpec,
  AError: string): Boolean;

implementation

uses
  FileUtils,
  SHA256,
  TextEncoding,

  Goccia.FileExtensions,
  Goccia.Packages.ImportMapEdit;

const
  REFS_MAX_BYTES = 8 * 1024 * 1024;
  HTTP_STATUS_OK = 200;
  HTTP_STATUS_NOT_FOUND = 404;
  SHORT_COMMIT_LENGTH = 12;
  MAX_PROJECT_FILES = 20000;
  PROJECT_SKIPPED_DIRECTORIES: array[0..2] of string = ('.goccia',
    'node_modules', '.git');

type
  { One import-map entry that names a provider package. }
  TProviderEntry = record
    Key: string;
    Address: TGocciaProviderAddress;
  end;

  { A package this run pins: where its commit came from and what it has. }
  TPackageWork = class
  public
    Key: string;
    Address: TGocciaProviderAddress;
    Entries: array of TProviderEntry;
    Typed: Boolean;
    Resolve: Boolean;
    Commit: string;
    RefKind: TGocciaLockRefKind;
    Old: TGocciaLockedPackage;
    Crawl: TGocciaPackageCrawl;
    { Paths whose bytes came from the verified cache, not the network. }
    Cached: TStringList;
    destructor Destroy; override;
  end;

  TPackageWorkList = TObjectList<TPackageWork>;

  { The crawl's fetcher for one package: the verified cache first, then the
    network when the package is granted. }
  TPackageFetcher = class
  public
    Installer: TGocciaPackageInstaller;
    Work: TPackageWork;
    CacheDirectory: string;
    Granted: Boolean;
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
  if (AKey[Length(AKey)] = '/') and not Address.IsPrefix then
  begin
    AError := Format('--add %s: the key ends with "/", so the address must ' +
      'name a directory ending with "/"', [AArgument]);
    Exit(False);
  end;
  Result := True;
end;

{ TPackageWork }

destructor TPackageWork.Destroy;
begin
  Crawl.Free;
  Cached.Free;
  inherited;
end;

{ TPackageFetcher }

function TPackageFetcher.Fetch(const APath: string; out ABytes: TBytes): Boolean;
var
  Pin, URL: string;
  Response: TGocciaProviderResponse;
begin
  ABytes := nil;
  { A file the old lock pins at this commit, cached with the pinned bytes,
    needs no network. }
  if Assigned(Work.Old) and (Work.Old.Commit = Work.Commit) and
     Work.Old.FindArtifact(APath, Pin) and
     ReadCacheFile(CacheDirectory, ToHostRelativePath(
       PackageCacheRelativeDirectory(Work.Address.Owner,
       Work.Address.Repository, Work.Commit) + '/' + APath), Work.Key,
       ABytes) and (SHA256Hex(ABytes) = Pin) then
  begin
    Work.Cached.Add(APath);
    Exit(True);
  end;
  if not Granted then
    Fail(Format('%s: %s is not cached, and downloading it needs ' +
      '--allow-import=%s', [Work.Key, APath, Work.Address.ScopeText]));
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
  if Response.StatusCode = HTTP_STATUS_NOT_FOUND then
    Exit(False);
  if Response.StatusCode <> HTTP_STATUS_OK then
  begin
    if Assigned(OnAudit) then
      OnAudit(False, URL, Format('HTTP %d', [Response.StatusCode]));
    Fail(Format('%s: fetching %s failed with HTTP %d', [Work.Key, APath,
      Response.StatusCode]));
  end;
  if Assigned(OnAudit) then
    OnAudit(True, URL, 'fetched for the lockfile');
  ABytes := Response.Body;
  Result := True;
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

{ Every literal specifier in the project's own modules, for prefix entries:
  a prefix entry pins the files its crawl reaches from the specifiers the
  project actually imports through it. Links and the cache, node_modules,
  and .git directories are not walked. }
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
          if (References[I].Kind <> grkAsset) and
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

function DescribeLockDifferences(const AOld, ANew: TGocciaLockfile): string;
var
  Package, Other: TGocciaLockedPackage;
begin
  Result := '';
  for Package in ANew.Packages do
  begin
    Other := AOld.FindPackage(Package.Key);
    if not Assigned(Other) then
      Result := Result + sLineBreak + '  + ' + Package.Key +
        '   (in the import map, not locked)'
    else if Other.Commit <> Package.Commit then
      Result := Result + sLineBreak + '  ~ ' + Package.Key + '   (' +
        ShortCommit(Other.Commit) + ' -> ' + ShortCommit(Package.Commit) + ')'
    else
      Result := Result + sLineBreak + '  ~ ' + Package.Key +
        '   (its files differ from the pins)';
  end;
  for Package in AOld.Packages do
    if not Assigned(ANew.FindPackage(Package.Key)) then
      Result := Result + sLineBreak + '  - ' + Package.Key +
        '   (locked, no longer imported)';
end;

procedure TGocciaPackageInstaller.Run(const ARequest: TGocciaInstallRequest);
var
  Address: TGocciaProviderAddress;
  AddressError, CachePath, EditError, EditedText, NewText, Text, DenyScope,
    Commit, RelativeDirectory: string;
  Adds, Removes, Updates, RemovedPackages: TStringList;
  Entries: TGocciaImportMapEntries;
  I, J, EntryIndex: Integer;
  ImportMapExists, Found, Changed, Granted: Boolean;
  NewLock, OldLock: TGocciaLockfile;
  Locked, Kept: TGocciaLockedPackage;
  Work: TPackageWork;
  Works: TPackageWorkList;
  Fetcher: TPackageFetcher;
  Refs: TGitRefAdvertisement;
  ProjectSpecifiers: TStringList;
  Entry: TProviderEntry;
  Bytes: TBytes;
  ErrorOffset: Integer;
  Added, Updated, Removed: Integer;

  function FindWork(const APackageKey: string): TPackageWork;
  var
    Candidate: TPackageWork;
  begin
    for Candidate in Works do
      if Candidate.Key = APackageKey then
        Exit(Candidate);
    Result := nil;
  end;

  function EntryIndexOf(const AKey: string): Integer;
  var
    K: Integer;
  begin
    for K := 0 to High(Entries) do
      if Entries[K].Key = AKey then
        Exit(K);
    Result := -1;
  end;

  procedure RequireGrant(const AWork: TPackageWork; const AWhy: string);
  begin
    if AWork.Typed or FCapabilities.AllowsProviderPackage(
       IMPORT_PROVIDER_GITHUB, AWork.Address.Owner,
       AWork.Address.Repository) then
      Exit;
    InstallAudit(False, AWork.Key, 'the import capability does not cover ' +
      AWork.Address.ScopeText);
    Fail(Format('import: %s (%s needs --allow-import=%s, or ' +
      '--allow-import=github:%s or github)', [AWork.Key, AWhy,
      AWork.Address.ScopeText, LowerCase(AWork.Address.Owner)]));
  end;

begin
  Adds := TStringList.Create;
  Adds.CaseSensitive := True;
  Removes := TStringList.Create;
  Removes.CaseSensitive := True;
  Updates := TStringList.Create;
  Updates.CaseSensitive := True;
  RemovedPackages := TStringList.Create;
  RemovedPackages.CaseSensitive := True;
  Works := TPackageWorkList.Create(True);
  OldLock := nil;
  NewLock := nil;
  ProjectSpecifiers := nil;
  try
    { 1. The import map and the lockfile as they are. }
    ImportMapExists := HostFileExists(FImportMapPath);
    if ImportMapExists then
    begin
      try
        Text := ReadUTF8FileText(FImportMapPath);
      except
        on E: Exception do
          FailUsage(Format('cannot read %s: %s', [FImportMapPath, E.Message]));
      end;
      if not TryReadImportMapEntries(Text, Entries, EditError) then
        FailUsage(Format('%s is not an import map install mode can read: %s',
          [FImportMapPath, EditError]));
    end
    else
    begin
      Text := '';
      Entries := nil;
    end;
    if HostFileExists(FLockPath) then
    begin
      try
        OldLock := LoadLockfile(FLockPath);
      except
        on E: EGocciaLockfileError do
          FailUsage(E.Message);
      end;
    end
    else
      OldLock := TGocciaLockfile.Create;

    { 2. The import map as it will be. }
    for I := 0 to High(ARequest.Adds) do
    begin
      EntryIndex := EntryIndexOf(ARequest.Adds[I].Key);
      if EntryIndex < 0 then
      begin
        SetLength(Entries, Length(Entries) + 1);
        EntryIndex := High(Entries);
        Entries[EntryIndex].Key := ARequest.Adds[I].Key;
      end;
      Entries[EntryIndex].Value := ARequest.Adds[I].Spec;
      Adds.Add(ARequest.Adds[I].Key);
    end;
    for I := 0 to High(ARequest.Removes) do
    begin
      EntryIndex := EntryIndexOf(ARequest.Removes[I]);
      if EntryIndex < 0 then
        Fail(Format('the import map %s has no entry "%s"', [FImportMapPath,
          ARequest.Removes[I]]));
      if not IsProviderAddress(Entries[EntryIndex].Value) then
        Fail(Format('"%s" is not a provider entry; --remove removes only ' +
          'provider entries', [ARequest.Removes[I]]));
      Removes.Add(ARequest.Removes[I]);
      if TryParseProviderAddress(Entries[EntryIndex].Value, Address,
         AddressError) then
        RemovedPackages.Add(Address.PackageKey);
      for J := EntryIndex to High(Entries) - 1 do
        Entries[J] := Entries[J + 1];
      SetLength(Entries, Length(Entries) - 1);
    end;
    { An import map install mode cannot edit (an --import-map file) gets
      the lines to change, and nothing is written: a pin the import map does
      not name would only be pruned by the next --install. }
    if not FEditable and ((Adds.Count > 0) or (Removes.Count > 0)) then
    begin
      for I := 0 to High(ARequest.Adds) do
        Log(Format('Add this entry to the "imports" of %s: "%s": "%s"',
          [FImportMapPath, ARequest.Adds[I].Key, ARequest.Adds[I].Spec]));
      for I := 0 to Removes.Count - 1 do
        Log(Format('Remove the entry "%s" from the "imports" of %s',
          [Removes[I], FImportMapPath]));
      Log('Then run GocciaRunner --install. Nothing was written.');
      Exit;
    end;

    for I := 0 to High(ARequest.UpdateKeys) do
    begin
      EntryIndex := EntryIndexOf(ARequest.UpdateKeys[I]);
      if (EntryIndex < 0) or
         not IsProviderAddress(Entries[EntryIndex].Value) then
        Fail(Format('the import map has no provider entry "%s" to update',
          [ARequest.UpdateKeys[I]]));
      Updates.Add(ARequest.UpdateKeys[I]);
    end;

    { 3. The packages the provider entries name, and which this run works
      on: the ones an --add names or an entry --remove took away from, every
      one for --install and --update (or those --update names). }
    for I := 0 to High(Entries) do
    begin
      if not IsProviderAddress(Entries[I].Value) then
        Continue;
      if not TryParseProviderAddress(Entries[I].Value, Address,
         AddressError) then
        FailUsage(Format('import map entry "%s": %s', [Entries[I].Key,
          AddressError]));
      Work := FindWork(Address.PackageKey);
      if not Assigned(Work) then
      begin
        Work := TPackageWork.Create;
        Work.Key := Address.PackageKey;
        Work.Address := Address;
        Work.Cached := TStringList.Create;
        Work.Cached.CaseSensitive := True;
        Work.Old := OldLock.FindPackage(Work.Key);
        Works.Add(Work);
      end;
      Entry.Key := Entries[I].Key;
      Entry.Address := Address;
      SetLength(Work.Entries, Length(Work.Entries) + 1);
      Work.Entries[High(Work.Entries)] := Entry;
      if Adds.IndexOf(Entries[I].Key) >= 0 then
        Work.Typed := True;
    end;

    for I := Works.Count - 1 downto 0 do
    begin
      Work := Works[I];
      Found := ARequest.Install or Work.Typed or
        (ARequest.Update and ((Updates.Count = 0) or
         (Updates.IndexOf(Work.Entries[0].Key) >= 0)));
      if ARequest.Update and not Found then
        for J := 0 to High(Work.Entries) do
          if Updates.IndexOf(Work.Entries[J].Key) >= 0 then
            Found := True;
      { A package an entry --remove took away from is re-pinned from what
        the rest of its entries reach. }
      if not Found and (RemovedPackages.IndexOf(Work.Key) >= 0) then
        Found := True;
      if not Found then
        Works.Delete(I);
    end;

    { 4. Denies win over everything, a typed spec included. }
    for Work in Works do
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

    { 5. Refs: a package the lockfile lacks, or one --update re-resolves,
      is resolved; --check-refs proves the others. }
    for Work in Works do
    begin
      Work.Resolve := (not Assigned(Work.Old)) or ARequest.Update;
      if not Work.Resolve then
      begin
        Work.Commit := Work.Old.Commit;
        Work.RefKind := Work.Old.RefKind;
      end;
      if not Work.Resolve and not ARequest.CheckRefs then
        Continue;
      if ARequest.Frozen and Work.Resolve then
        Continue;
      RequireGrant(Work, 'resolving its ref');
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
          { A commit pin must be a tip this repository advertises. A commit
            reachable only from a fork (refs/pull/*) is served under this
            repository's name by the raw host, and is refused here. }
          if not Refs.IsAdvertisedTip(Work.Address.Ref) then
          begin
            InstallAudit(False, Work.Key,
              'the commit is not the tip of an advertised tag or branch');
            Fail(Format('%s: commit %s is not the tip of any tag or branch ' +
              'of %s/%s; pin a tag instead', [Work.Key, Work.Address.Ref,
              Work.Address.Owner, Work.Address.Repository]));
          end;
          Commit := Work.Address.Ref;
          Work.RefKind := lrkCommit;
        end
        else
        begin
          if not Refs.FindTag(Work.Address.Ref, Commit) then
          begin
            InstallAudit(False, Work.Key, 'no such tag');
            Fail(Format('%s: %s/%s has no tag %s (refs are tags or commits; ' +
              'branches are not accepted)', [Work.Key, Work.Address.Owner,
              Work.Address.Repository, Work.Address.Ref]));
          end;
          Work.RefKind := lrkTag;
        end;
        if Assigned(Work.Old) and (Commit <> Work.Old.Commit) then
        begin
          if (Work.RefKind = lrkTag) and not ARequest.AcceptMovedTags then
          begin
            InstallAudit(False, Work.Key, Format('tag moved %s -> %s',
              [ShortCommit(Work.Old.Commit), ShortCommit(Commit)]));
            Fail(Format('tag %s of %s/%s moved: locked %s, now %s. Tags are ' +
              'expected not to move; review the change, then re-pin it with ' +
              '--update --accept-moved-tags', [Work.Address.Ref,
              Work.Address.Owner, Work.Address.Repository,
              ShortCommit(Work.Old.Commit), ShortCommit(Commit)]));
          end;
        end;
        if not Work.Resolve then
        begin
          Log(Format('Checked %s -> %s at %s', [Work.Key,
            LockRefKindName(Work.RefKind), ShortCommit(Commit)]));
          Continue;
        end;
        Work.Commit := Commit;
        Log(Format('Resolve %s -> %s %s at %s (%s)', [Work.Key,
          LockRefKindName(Work.RefKind), Work.Address.Ref, ShortCommit(Commit),
          GITHUB_HOST]));
      finally
        Refs.Free;
      end;
    end;

    { 6. Crawl each package at its commit. }
    for Work in Works do
    begin
      if ARequest.Frozen and Work.Resolve then
        Continue;
      Granted := Work.Typed or FCapabilities.AllowsProviderPackage(
        IMPORT_PROVIDER_GITHUB, Work.Address.Owner, Work.Address.Repository);
      Fetcher := TPackageFetcher.Create;
      try
        Fetcher.Installer := Self;
        Fetcher.Work := Work;
        Fetcher.CacheDirectory := FCacheDirectory;
        Fetcher.Granted := Granted;
        Fetcher.Transport := FTransport;
        Fetcher.OnAudit := FOnAudit;
        Work.Crawl := TGocciaPackageCrawl.Create(Work.Key, Fetcher.Fetch);
        for Entry in Work.Entries do
        begin
          if Entry.Key[Length(Entry.Key)] <> '/' then
          begin
            Work.Crawl.AddModuleEntry(Entry.Address.Path);
            Continue;
          end;
          if not Assigned(ProjectSpecifiers) then
            ProjectSpecifiers := CollectProjectSpecifiers(
              ExtractFilePath(FImportMapPath), FOnLog);
          for J := 0 to ProjectSpecifiers.Count - 1 do
            if (Length(ProjectSpecifiers[J]) > Length(Entry.Key)) and
               (Copy(ProjectSpecifiers[J], 1, Length(Entry.Key)) =
                Entry.Key) and
               IsSafeArtifactPath(Entry.Address.Path +
                 Copy(ProjectSpecifiers[J], Length(Entry.Key) + 1, MaxInt)) then
              Work.Crawl.AddModuleEntry(Entry.Address.Path +
                Copy(ProjectSpecifiers[J], Length(Entry.Key) + 1, MaxInt));
        end;
        try
          Work.Crawl.Run;
        except
          on E: EGocciaCrawlError do
            Fail(E.Message);
          on E: EGocciaProviderPackageError do
            Fail(E.Message);
        end;
      finally
        Fetcher.Free;
      end;
      if Work.Crawl.Files.Count = 0 then
        Log(Format('Skipped %s: no project module imports through its ' +
          'prefix entries yet, so nothing is pinned', [Work.Key]))
      else
        Log(Format('Pinned  %s: %d files (%d fetched, %d verified in the ' +
          'cache)', [Work.Key, Work.Crawl.Files.Count,
          Work.Crawl.Files.Count - Work.Cached.Count, Work.Cached.Count]));
    end;

    { 7. The new lockfile: pins this run did not touch are kept as they are;
      pins no entry names any more are dropped. }
    NewLock := TGocciaLockfile.Create;
    for Locked in OldLock.Packages do
    begin
      Work := FindWork(Locked.Key);
      Found := False;
      for I := 0 to High(Entries) do
        if IsProviderAddress(Entries[I].Value) and
           TryParseProviderAddress(Entries[I].Value, Address, AddressError) and
           (Address.PackageKey = Locked.Key) then
          Found := True;
      if not Found then
        Continue;
      if Assigned(Work) and not (ARequest.Frozen and Work.Resolve) then
        Continue;
      Kept := TGocciaLockedPackage.Create(Locked.Key, Locked.Address);
      Kept.Commit := Locked.Commit;
      Kept.RefKind := Locked.RefKind;
      for I := 0 to Locked.ArtifactCount - 1 do
        Kept.AddArtifact(Locked.Artifact(I).Path, Locked.Artifact(I).SHA256);
      NewLock.Packages.Add(Kept);
    end;
    for Work in Works do
    begin
      if not Assigned(Work.Crawl) or (Work.Crawl.Files.Count = 0) then
        Continue;
      Kept := TGocciaLockedPackage.Create(Work.Key, Work.Address);
      Kept.Commit := Work.Commit;
      Kept.RefKind := Work.RefKind;
      for I := 0 to Work.Crawl.Files.Count - 1 do
        Kept.AddArtifact(Work.Crawl.Files[I].Path,
          SHA256Hex(Work.Crawl.Files[I].Bytes));
      NewLock.Packages.Add(Kept);
    end;
    { A frozen install also reports packages the lockfile lacks. }
    if ARequest.Frozen then
      for Work in Works do
        if Work.Resolve and not Assigned(NewLock.FindPackage(Work.Key)) then
        begin
          Kept := TGocciaLockedPackage.Create(Work.Key, Work.Address);
          NewLock.Packages.Add(Kept);
        end;

    Changed := SerializeLockfile(OldLock) <> SerializeLockfile(NewLock);
    if ARequest.Frozen and Changed then
      Fail(Format('%s is out of date; --frozen does not write it.%s%sRun %s ' +
        'without --frozen and commit %s.', [LOCKFILE_NAME,
        DescribeLockDifferences(OldLock, NewLock), sLineBreak,
        'GocciaRunner --install', LOCKFILE_NAME]));

    { 8. The cache: every fetched file of every pin this run made. }
    for Work in Works do
    begin
      if not Assigned(Work.Crawl) then
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

    { 9. The lockfile, before the import map. }
    Added := 0;
    Updated := 0;
    Removed := 0;
    for Locked in NewLock.Packages do
    begin
      Kept := OldLock.FindPackage(Locked.Key);
      if not Assigned(Kept) then
      begin
        Inc(Added);
        InstallAudit(True, Locked.Key, Format('added: %s -> %s',
          [LockRefKindName(Locked.RefKind), Locked.Commit]));
      end
      else if Kept.Commit <> Locked.Commit then
      begin
        Inc(Updated);
        InstallAudit(True, Locked.Key, Format('updated: %s -> %s',
          [Kept.Commit, Locked.Commit]));
      end;
    end;
    for Locked in OldLock.Packages do
      if not Assigned(NewLock.FindPackage(Locked.Key)) then
      begin
        Inc(Removed);
        InstallAudit(True, Locked.Key, 'removed');
      end;
    if Changed then
    begin
      try
        SaveLockfile(FLockPath, NewLock);
      except
        on E: EGocciaLockfileError do
          Fail(E.Message);
      end;
      Log(Format('Wrote   %s (%d added, %d removed, %d updated)',
        [LOCKFILE_NAME, Added, Removed, Updated]));
    end
    else
      Log(Format('Verified %s; unchanged', [LOCKFILE_NAME]));

    { 10. The import map, last. }
    if (Adds.Count > 0) or (Removes.Count > 0) then
    begin
      NewText := Text;
      for I := 0 to High(ARequest.Adds) do
      begin
        if not ImportMapExists and (I = 0) then
          NewText := NewImportMapText(ARequest.Adds[I].Key,
            ARequest.Adds[I].Spec)
        else if TrySetImportMapEntry(NewText, ARequest.Adds[I].Key,
           ARequest.Adds[I].Spec, EditedText, EditError) then
          NewText := EditedText
        else
          Fail(Format('cannot edit %s: %s', [FImportMapPath, EditError]));
        Log(Format('Add     "%s": "%s" to %s imports',
          [ARequest.Adds[I].Key, ARequest.Adds[I].Spec,
          ExtractFileName(FImportMapPath)]));
      end;
      for I := 0 to Removes.Count - 1 do
      begin
        if TryRemoveImportMapEntry(NewText, Removes[I], EditedText,
           EditError) then
          NewText := EditedText
        else
          Fail(Format('cannot edit %s: %s', [FImportMapPath, EditError]));
        Log(Format('Remove  "%s" from %s imports', [Removes[I],
          ExtractFileName(FImportMapPath)]));
      end;
      if not TryEncodeUTF8(NewText, Bytes, ErrorOffset) or
         not ReplaceHostFile(FImportMapPath, FImportMapPath +
           '.goccia-write-' + IntToStr(GetProcessID), Bytes, EditError) then
        Fail(Format('cannot write %s: %s', [FImportMapPath, EditError]));
    end;

    { 11. Prune the cache directories of pins the new lockfile dropped. }
    for Locked in OldLock.Packages do
    begin
      Found := False;
      for Kept in NewLock.Packages do
        if SameText(Kept.Address.Owner, Locked.Address.Owner) and
           SameText(Kept.Address.Repository, Locked.Address.Repository) and
           (Kept.Commit = Locked.Commit) then
          Found := True;
      if Found then
        Continue;
      RelativeDirectory := ToHostRelativePath(PackageCacheRelativeDirectory(
        Locked.Address.Owner, Locked.Address.Repository, Locked.Commit));
      try
        RefuseCacheLinks(FCacheDirectory, RelativeDirectory, Locked.Key);
      except
        on E: EGocciaProviderPackageError do
        begin
          Log('Kept    ' + RelativeDirectory + ': ' + E.Message);
          Continue;
        end;
      end;
      CachePath := IncludeTrailingPathDelimiter(FCacheDirectory) +
        RelativeDirectory;
      if HostDirectoryExists(CachePath) then
      begin
        RemoveCacheTree(CachePath);
        { The repository and owner directories go too when now empty. }
        RemoveDir(ExtractFileDir(CachePath));
        RemoveDir(ExtractFileDir(ExtractFileDir(CachePath)));
        Log(Format('Pruned  %s', [PACKAGE_CACHE_DIRECTORY_NAME + PathDelim +
          RelativeDirectory]));
      end;
    end;
  finally
    ProjectSpecifiers.Free;
    RemovedPackages.Free;
    NewLock.Free;
    OldLock.Free;
    Works.Free;
    Updates.Free;
    Removes.Free;
    Adds.Free;
  end;
end;

end.
