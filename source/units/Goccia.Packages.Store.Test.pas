program Goccia.Packages.Store.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  HTTPTypes,
  SHA256,
  TestingPascalLibrary,
  TextEncoding,

  Goccia.Packages.Address,
  Goccia.Packages.Store,
  Goccia.Packages.Transport,
  Goccia.TestSetup;

const
  COMMIT = '0123456789abcdef0123456789abcdef01234567';
  PACKAGE_KEY = 'github:frostney/GocciaScript-Raylib@v0.10.0';
  ENTRY_PATH = 'bindings/raylib.ts';
  DATA_PATH = 'vendor/raylib.json';
  LINUX_LIBRARY_PATH = 'native/linux-x86_64/libraylib.so';
  WINDOWS_LIBRARY_PATH = 'native/windows-x86_64/raylib.dll';
  ENTRY_TEXT = 'export const version = "6.0";';
  DATA_TEXT = '{"structs": []}';
  LINUX_LIBRARY_TEXT = 'linux native bytes';
  WINDOWS_LIBRARY_TEXT = 'windows native bytes';

type
  { Serves fixture bytes for URLs and records every request. }
  TFixtureTransport = class(TGocciaProviderTransport)
  private
    FFailOnFetch: Boolean;
    FRequests: TStringList;
    FHosts: TStringList;
    FResponses: TStringList;
    FStatusCode: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Serve(const APath, AText: string);
    function Get(const AURL, AHost: string;
      const AMaxBytes: Integer): TGocciaProviderResponse; override;
    property FailOnFetch: Boolean read FFailOnFetch write FFailOnFetch;
    property Hosts: TStringList read FHosts;
    property Requests: TStringList read FRequests;
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  { The real transport with the request itself captured. }
  TCapturingHTTPTransport = class(TGocciaHTTPProviderTransport)
  private
    FAllowedHosts: string;
    FPolicy: THTTPRequestPolicy;
    FSent: Boolean;
    FTimeout: Integer;
  protected
    function Send(const AURL: string; const AAllowedHosts: TStrings;
      const ATimeoutMilliseconds: Integer;
      const APolicy: THTTPRequestPolicy): THTTPResponse; override;
  end;

  TPackageStoreTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;
    FAudit: TStringList;
    procedure RecordAudit(const AAllowed: Boolean;
      const ASubject, AReason: string);
    function Bytes(const AText: string): TBytes;
    function CreateProject: string;
    function CachePath(const AProject, AArtifactPath: string): string;
    function NewStore(const ATransport: TFixtureTransport):
      TGocciaProviderPackageStore;
    function PackageAddress: TGocciaProviderAddress;
    function ServeAll: TFixtureTransport;
    function MaterializeFails(const AStore: TGocciaProviderPackageStore;
      const AProject: string; out AMessage: string): Boolean;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteLock(const AProject: string);
    procedure WriteText(const APath, AText: string);

    procedure TestMaterializesEveryPinnedFile;
    procedure TestReusesVerifiedCacheWithoutNetwork;
    procedure TestRefetchesATamperedCacheFile;
    procedure TestHashMismatchIsNotCommitted;
    procedure TestNonSuccessResponseIsRefused;
    procedure TestUnpinnedPackageAndMissingLockfile;
    procedure TestCachedOnlyRefusesTheNetwork;
    procedure TestVerifyOnLoad;
    procedure TestTransportPinsHostAndRefusesPrivateRanges;
    procedure TestTransportRefusesOtherURLs;
    {$IFDEF UNIX}
    procedure TestRefusesSymlinkedCacheDirectory;
    procedure TestRefusesPlantedSymlinkInCache;
    procedure TestRefusesPlantedFileSymlink;
    procedure TestVerifiesThroughASymlinkedSpelling;
    {$ENDIF}
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
  public
    procedure SetupTests; override;
  end;

{ TFixtureTransport }

constructor TFixtureTransport.Create;
begin
  inherited Create;
  FRequests := TStringList.Create;
  FHosts := TStringList.Create;
  FResponses := TStringList.Create;
  FStatusCode := 200;
end;

destructor TFixtureTransport.Destroy;
begin
  FResponses.Free;
  FHosts.Free;
  FRequests.Free;
  inherited;
end;

procedure TFixtureTransport.Serve(const APath, AText: string);
begin
  FResponses.Values[PackageArtifactURL('frostney', 'GocciaScript-Raylib',
    COMMIT, APath)] := AText;
end;

function TFixtureTransport.Get(const AURL, AHost: string;
  const AMaxBytes: Integer): TGocciaProviderResponse;
var
  ErrorOffset, Index: Integer;
begin
  FRequests.Add(AURL);
  FHosts.Add(AHost);
  if FFailOnFetch then
    raise EGocciaProviderTransportError.Create('unexpected provider GET');
  Index := FResponses.IndexOfName(AURL);
  if Index < 0 then
    raise EGocciaProviderTransportError.Create('no fixture for ' + AURL);
  Result.StatusCode := FStatusCode;
  if not TryEncodeUTF8(FResponses.ValueFromIndex[Index], Result.Body,
     ErrorOffset) then
    raise Exception.Create('fixture encoding');
end;

{ TCapturingHTTPTransport }

function TCapturingHTTPTransport.Send(const AURL: string;
  const AAllowedHosts: TStrings; const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse;
begin
  FSent := True;
  FAllowedHosts := AAllowedHosts.CommaText;
  FTimeout := ATimeoutMilliseconds;
  FPolicy := APolicy;
  Result := Default(THTTPResponse);
  Result.StatusCode := 200;
end;

{ TPackageStoreTests }

procedure TPackageStoreTests.SetupTests;
begin
  Test('Materializes every pinned file, every platform included',
    TestMaterializesEveryPinnedFile);
  Test('Reuses a verified cache without network access',
    TestReusesVerifiedCacheWithoutNetwork);
  Test('Refetches a cached file that does not match its pin',
    TestRefetchesATamperedCacheFile);
  Test('A fetched file with the wrong hash is never committed',
    TestHashMismatchIsNotCommitted);
  Test('A non-200 response is refused', TestNonSuccessResponseIsRefused);
  Test('An unpinned package or a missing lockfile is refused',
    TestUnpinnedPackageAndMissingLockfile);
  Test('Cached-only refuses the network', TestCachedOnlyRefusesTheNetwork);
  Test('Verify on load hashes the bytes about to be used', TestVerifyOnLoad);
  Test('The HTTP transport pins the host and refuses private ranges',
    TestTransportPinsHostAndRefusesPrivateRanges);
  Test('The HTTP transport refuses URLs on another host or scheme',
    TestTransportRefusesOtherURLs);
  {$IFDEF UNIX}
  Test('A symbolic-link .goccia directory is refused',
    TestRefusesSymlinkedCacheDirectory);
  Test('A symbolic link planted in the cache is refused',
    TestRefusesPlantedSymlinkInCache);
  Test('A symbolic link planted as a cache file is refused',
    TestRefusesPlantedFileSymlink);
  Test('Verify on load recognizes a package reached through a link',
    TestVerifiesThroughASymlinkedSpelling);
  {$ENDIF}
end;

procedure TPackageStoreTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FTempDirectories := TStringList.Create;
  FAudit := TStringList.Create;
end;

procedure TPackageStoreTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  FAudit.Free;
  inherited AfterAll;
end;

procedure TPackageStoreTests.BeforeEach;
begin
  inherited BeforeEach;
  FAudit.Clear;
end;

procedure TPackageStoreTests.RecordAudit(const AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if AAllowed then
    FAudit.Add('allow ' + ASubject + ' ' + AReason)
  else
    FAudit.Add('deny ' + ASubject + ' ' + AReason);
end;

procedure TPackageStoreTests.DeleteDirectoryTree(const APath: string);
var
  EntryPath: string;
  SearchRecord: TSearchRec;
begin
  if HostPathIsSymlink(APath) then
  begin
    DeleteFile(APath);
    Exit;
  end;
  if not DirectoryExists(APath) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile or
    faSymLink, SearchRecord) = 0 then
  begin
    repeat
      if (SearchRecord.Name = '.') or (SearchRecord.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRecord.Name;
      if HostPathIsSymlink(EntryPath) then
        DeleteFile(EntryPath)
      else if (SearchRecord.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRecord) <> 0;
    FindClose(SearchRecord);
  end;
  RemoveDir(APath);
end;

function TPackageStoreTests.Bytes(const AText: string): TBytes;
var
  ErrorOffset: Integer;
begin
  if not TryEncodeUTF8(AText, Result, ErrorOffset) then
    raise Exception.Create('fixture encoding');
end;

function TPackageStoreTests.CreateProject: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-package-store-' + IntToStr(Random(MaxInt));
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
  WriteText(IncludeTrailingPathDelimiter(Result) + 'goccia.json',
    '{"imports": {"raylib": "github:frostney/GocciaScript-Raylib@v0.10.0/' +
    ENTRY_PATH + '"}}');
  WriteLock(Result);
end;

procedure TPackageStoreTests.WriteText(const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  WriteUTF8FileText(APath, AText);
end;

procedure TPackageStoreTests.WriteLock(const AProject: string);

  function Pin(const APath, AText: string): string;
  begin
    Result := '"' + APath + '": {"sha256": "' + SHA256Hex(Bytes(AText)) + '"}';
  end;

begin
  WriteText(IncludeTrailingPathDelimiter(AProject) + 'goccia.lock.json',
    '{"version": 1, "packages": {"' + PACKAGE_KEY + '": {"ref": "tag", ' +
    '"commit": "' + COMMIT + '", "artifacts": {' +
    Pin(ENTRY_PATH, ENTRY_TEXT) + ', ' + Pin(DATA_PATH, DATA_TEXT) + ', ' +
    Pin(LINUX_LIBRARY_PATH, LINUX_LIBRARY_TEXT) + ', ' +
    Pin(WINDOWS_LIBRARY_PATH, WINDOWS_LIBRARY_TEXT) + '}}}}');
end;

function TPackageStoreTests.CachePath(const AProject,
  AArtifactPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(AProject) + '.goccia' + PathDelim +
    'packages' + PathDelim + 'github' + PathDelim + 'frostney' + PathDelim +
    'gocciascript-raylib' + PathDelim + COMMIT;
  if AArtifactPath <> '' then
    Result := Result + PathDelim + ToHostRelativePath(AArtifactPath);
end;

function TPackageStoreTests.ServeAll: TFixtureTransport;
begin
  Result := TFixtureTransport.Create;
  Result.Serve(ENTRY_PATH, ENTRY_TEXT);
  Result.Serve(DATA_PATH, DATA_TEXT);
  Result.Serve(LINUX_LIBRARY_PATH, LINUX_LIBRARY_TEXT);
  Result.Serve(WINDOWS_LIBRARY_PATH, WINDOWS_LIBRARY_TEXT);
end;

function TPackageStoreTests.NewStore(
  const ATransport: TFixtureTransport): TGocciaProviderPackageStore;
begin
  Result := TGocciaProviderPackageStore.Create(ATransport, False);
  Result.OnAudit := RecordAudit;
end;

function TPackageStoreTests.PackageAddress: TGocciaProviderAddress;
var
  Error: string;
begin
  if not TryParseProviderAddress(
     'github:frostney/GocciaScript-Raylib@v0.10.0/' + ENTRY_PATH, Result,
     Error) then
    raise Exception.Create(Error);
end;

function TPackageStoreTests.MaterializeFails(
  const AStore: TGocciaProviderPackageStore; const AProject: string;
  out AMessage: string): Boolean;
begin
  AMessage := '';
  try
    AStore.Materialize(PackageAddress,
      IncludeTrailingPathDelimiter(AProject) + 'goccia.json');
    Result := False;
  except
    on E: EGocciaProviderPackageError do
    begin
      AMessage := E.Message;
      Result := True;
    end;
  end;
end;

procedure TPackageStoreTests.TestMaterializesEveryPinnedFile;
var
  Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
  Package: TGocciaMaterializedPackage;
  I: Integer;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Package := Store.Materialize(PackageAddress,
      IncludeTrailingPathDelimiter(Project) + 'goccia.json');
    Expect<string>(Package.Key).ToBe(PACKAGE_KEY);
    Expect<string>(Package.Root).ToBe(CachePath(Project, ''));
    Expect<Integer>(Transport.Requests.Count).ToBe(4);
    for I := 0 to Transport.Hosts.Count - 1 do
      Expect<string>(Transport.Hosts[I]).ToBe(GITHUB_RAW_HOST);
    Expect<string>(ReadUTF8FileText(CachePath(Project, ENTRY_PATH)))
      .ToBe(ENTRY_TEXT);
    { Every platform's library is materialized, not only this host's. }
    Expect<string>(ReadUTF8FileText(CachePath(Project, WINDOWS_LIBRARY_PATH)))
      .ToBe(WINDOWS_LIBRARY_TEXT);
    Expect<string>(ReadUTF8FileText(CachePath(Project, LINUX_LIBRARY_PATH)))
      .ToBe(LINUX_LIBRARY_TEXT);
    Expect<Boolean>(FAudit.IndexOf('allow ' + PackageArtifactURL('frostney',
      'GocciaScript-Raylib', COMMIT, ENTRY_PATH) + ' sha256 ok') >= 0)
      .ToBe(True);
    { A second resolution reuses the materialized package. }
    Expect<Boolean>(Store.Materialize(PackageAddress,
      IncludeTrailingPathDelimiter(Project) + 'goccia.json') = Package)
      .ToBe(True);
    Expect<Integer>(Transport.Requests.Count).ToBe(4);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestReusesVerifiedCacheWithoutNetwork;
var
  Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
  finally
    Store.Free;
  end;
  Transport.FailOnFetch := True;
  Transport.Requests.Clear;
  Store := NewStore(Transport);
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
    Expect<Integer>(Transport.Requests.Count).ToBe(0);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestRefetchesATamperedCacheFile;
var
  Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  WriteText(CachePath(Project, ENTRY_PATH), 'export const version = "evil";');
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
    Expect<string>(ReadUTF8FileText(CachePath(Project, ENTRY_PATH)))
      .ToBe(ENTRY_TEXT);
    Expect<Boolean>(FAudit.IndexOf('deny ' + PACKAGE_KEY + '/' + ENTRY_PATH +
      ' the cached file does not match its pin') >= 0).ToBe(True);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestHashMismatchIsNotCommitted;
var
  Message, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Transport.Serve(DATA_PATH, '{"structs": ["tampered"]}');
  Store := NewStore(Transport);
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Boolean>(Pos('does not match its SHA-256', Message) > 0).ToBe(True);
    Expect<Boolean>(FileExists(CachePath(Project, DATA_PATH))).ToBe(False);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestNonSuccessResponseIsRefused;
var
  Message, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Transport.StatusCode := 404;
  Store := NewStore(Transport);
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Boolean>(Pos('HTTP 404', Message) > 0).ToBe(True);
    Expect<Boolean>(FileExists(CachePath(Project, ENTRY_PATH))).ToBe(False);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestUnpinnedPackageAndMissingLockfile;
var
  Address: TGocciaProviderAddress;
  Error, Message, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Transport.FailOnFetch := True;
  Store := NewStore(Transport);
  try
    TryParseProviderAddress('github:frostney/GocciaScript-Raylib@v0.11.0',
      Address, Error);
    try
      Store.Materialize(Address, Project + PathDelim + 'goccia.json');
      Message := '';
    except
      on E: EGocciaProviderPackageError do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe(
      'github:frostney/GocciaScript-Raylib@v0.11.0 is not pinned in ' +
      'goccia.lock.json');
  finally
    Store.Free;
  end;
  DeleteFile(Project + PathDelim + 'goccia.lock.json');
  Store := NewStore(Transport);
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    { The guest-visible message names no host path. }
    Expect<Boolean>(Pos(Project, Message) = 0).ToBe(True);
    Expect<Integer>(Transport.Requests.Count).ToBe(0);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestCachedOnlyRefusesTheNetwork;
var
  Message, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Store := NewStore(Transport);
  Store.CachedOnly := True;
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Boolean>(Pos('--cached-only', Message) > 0).ToBe(True);
    Expect<Integer>(Transport.Requests.Count).ToBe(0);
    Expect<Boolean>(DirectoryExists(Project + PathDelim + '.goccia'))
      .ToBe(False);
  finally
    Store.Free;
  end;

  { A complete cache runs; a tampered one fails without a fetch. }
  Store := NewStore(Transport);
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
  finally
    Store.Free;
  end;
  Transport.Requests.Clear;
  Store := NewStore(Transport);
  Store.CachedOnly := True;
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
  finally
    Store.Free;
  end;
  WriteText(CachePath(Project, LINUX_LIBRARY_PATH), 'tampered');
  Store := NewStore(Transport);
  Store.CachedOnly := True;
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Integer>(Transport.Requests.Count).ToBe(0);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestVerifyOnLoad;
var
  Message, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
  Refused: Boolean;
begin
  Project := CreateProject;
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
    Expect<Boolean>(Store.OwnsPath(CachePath(Project, ENTRY_PATH))).ToBe(True);
    Expect<Boolean>(Store.OwnsPath(Project + PathDelim + 'app.js'))
      .ToBe(False);

    Store.VerifyContent(CachePath(Project, ENTRY_PATH), Bytes(ENTRY_TEXT));

    Refused := False;
    try
      Store.VerifyContent(CachePath(Project, ENTRY_PATH),
        Bytes('export const version = "swapped";'));
    except
      on E: EGocciaProviderVerificationError do
      begin
        Refused := True;
        Message := E.Message;
      end;
    end;
    Expect<Boolean>(Refused).ToBe(True);
    Expect<string>(Message).ToBe('Provider package file ' + PACKAGE_KEY + '/' +
      ENTRY_PATH + ' changed after it was verified');

    { A file in the package that the lockfile does not pin is refused. }
    Refused := False;
    try
      Store.VerifyContent(CachePath(Project, 'bindings/extra.ts'),
        Bytes('export {};'));
    except
      on E: EGocciaProviderVerificationError do
        Refused := True;
    end;
    Expect<Boolean>(Refused).ToBe(True);

    { Files outside every package are not the store's concern. }
    Store.VerifyContent(Project + PathDelim + 'app.js', Bytes('anything'));
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestTransportPinsHostAndRefusesPrivateRanges;
var
  Transport: TCapturingHTTPTransport;
  Reason: string;
begin
  Transport := TCapturingHTTPTransport.Create;
  try
    Transport.Get('https://raw.githubusercontent.com/o/r/' + COMMIT + '/x.ts',
      GITHUB_RAW_HOST, PROVIDER_MAX_ARTIFACT_BYTES);
    Expect<Boolean>(Transport.FSent).ToBe(True);
    Expect<string>(Transport.FAllowedHosts).ToBe(GITHUB_RAW_HOST);
    Expect<Integer>(Transport.FTimeout)
      .ToBe(PROVIDER_REQUEST_TIMEOUT_MILLISECONDS);
    Expect<Boolean>(Transport.FPolicy.DenyPrivateRanges).ToBe(True);
    Expect<Integer>(Transport.FPolicy.MaxResponseBytes)
      .ToBe(PROVIDER_MAX_ARTIFACT_BYTES);
    { Every hop is judged: another host, or the pinned host on another
      port (a downgrade to http), is refused. }
    Expect<Boolean>(Assigned(Transport.FPolicy.HostCheck)).ToBe(True);
    Expect<Boolean>(Transport.FPolicy.HostCheck(GITHUB_RAW_HOST, 443, '',
      Reason)).ToBe(True);
    Expect<Boolean>(Transport.FPolicy.HostCheck('evil.example', 443, '',
      Reason)).ToBe(False);
    Expect<Boolean>(Transport.FPolicy.HostCheck(GITHUB_RAW_HOST, 80, '',
      Reason)).ToBe(False);
  finally
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestTransportRefusesOtherURLs;

  function Refused(const AURL: string): Boolean;
  var
    Transport: TCapturingHTTPTransport;
  begin
    Transport := TCapturingHTTPTransport.Create;
    try
      try
        Transport.Get(AURL, GITHUB_RAW_HOST, PROVIDER_MAX_ARTIFACT_BYTES);
        Result := False;
      except
        on EGocciaProviderTransportError do
          Result := not Transport.FSent;
      end;
    finally
      Transport.Free;
    end;
  end;

begin
  Expect<Boolean>(Refused('https://evil.example/o/r/x.ts')).ToBe(True);
  Expect<Boolean>(Refused('http://raw.githubusercontent.com/o/r/x.ts'))
    .ToBe(True);
  Expect<Boolean>(Refused('https://raw.githubusercontent.com:8443/x.ts'))
    .ToBe(True);
  Expect<Boolean>(Refused('not a url')).ToBe(True);
end;

{$IFDEF UNIX}
procedure TPackageStoreTests.TestRefusesSymlinkedCacheDirectory;
var
  Message, Outside, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  Project := CreateProject;
  Outside := CreateProject;
  fpSymlink(PAnsiChar(AnsiString(Outside)),
    PAnsiChar(AnsiString(Project + PathDelim + '.goccia')));
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Boolean>(Pos('symbolic link', Message) > 0).ToBe(True);
    Expect<Integer>(Transport.Requests.Count).ToBe(0);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestRefusesPlantedSymlinkInCache;
var
  Message, Outside, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  { A link planted where a package directory belongs would let the cache
    write, or hand a run bytes, outside .goccia. }
  Project := CreateProject;
  Outside := CreateProject;
  ForceDirectories(CachePath(Project, ''));
  fpSymlink(PAnsiChar(AnsiString(Outside)),
    PAnsiChar(AnsiString(CachePath(Project, 'bindings'))));
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Boolean>(Pos('symbolic link', Message) > 0).ToBe(True);
    Expect<Boolean>(FileExists(Outside + PathDelim + 'raylib.ts')).ToBe(False);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestRefusesPlantedFileSymlink;
var
  Message, Outside, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
begin
  { Even a link to a file holding exactly the pinned bytes is refused. }
  Project := CreateProject;
  Outside := CreateProject + PathDelim + 'entry.ts';
  WriteText(Outside, ENTRY_TEXT);
  ForceDirectories(CachePath(Project, 'bindings'));
  fpSymlink(PAnsiChar(AnsiString(Outside)),
    PAnsiChar(AnsiString(CachePath(Project, ENTRY_PATH))));
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Expect<Boolean>(MaterializeFails(Store, Project, Message)).ToBe(True);
    Expect<Boolean>(Pos('symbolic link', Message) > 0).ToBe(True);
  finally
    Store.Free;
    Transport.Free;
  end;
end;

procedure TPackageStoreTests.TestVerifiesThroughASymlinkedSpelling;
var
  Link, Project: string;
  Transport: TFixtureTransport;
  Store: TGocciaProviderPackageStore;
  Refused: Boolean;
begin
  { The loader may reach a package file through a spelling that is not the
    store's; the canonical path still finds the package. }
  Project := CreateProject;
  Link := CreateProject + PathDelim + 'alias';
  fpSymlink(PAnsiChar(AnsiString(CachePath(Project, ''))),
    PAnsiChar(AnsiString(Link)));
  Transport := ServeAll;
  Store := NewStore(Transport);
  try
    Store.Materialize(PackageAddress, Project + PathDelim + 'goccia.json');
    Refused := False;
    try
      Store.VerifyContent(Link + PathDelim + 'bindings' + PathDelim +
        'raylib.ts', Bytes('tampered'));
    except
      on EGocciaProviderVerificationError do
        Refused := True;
    end;
    Expect<Boolean>(Refused).ToBe(True);
  finally
    Store.Free;
    Transport.Free;
  end;
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(TPackageStoreTests.Create(
    'Provider package store'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
