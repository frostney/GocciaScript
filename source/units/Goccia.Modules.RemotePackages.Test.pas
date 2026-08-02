program Goccia.Modules.RemotePackages.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  FileUtils,
  SHA256,
  TestingPascalLibrary,
  TextEncoding,

  Goccia.Modules.RemotePackages,
  Goccia.Platform,
  Goccia.TestSetup;

const
  PACKAGE_REFERENCE = 'github:frostney/GocciaScript-Raylib@v0.10.0';
  PACKAGE_REPOSITORY = 'frostney/GocciaScript-Raylib';
  RESOLVED_REFERENCE = '0123456789abcdef0123456789abcdef01234567';
  ENTRY_PATH = 'bindings/raylib.ts';
  NATIVE_PATH = 'native/libraylib.test';
  OTHER_PLATFORM_PATH = 'native/other/libraylib.test';
  ENTRY_TEXT = 'export const raylibVersion = "6.0";';
  NATIVE_TEXT = 'platform-native-raylib';
  OTHER_PLATFORM_TEXT = 'other-platform-raylib';

type
  TFixtureRemotePackageResolver = class(
    TGocciaProviderRemotePackageResolver)
  private
    FFailOnFetch: Boolean;
    FFetchCount: Integer;
    FResponses: TStringList;
  protected
    function FetchArtifact(const AURL: string): TBytes; override;
  public
    constructor Create(const ACacheDirectory: string);
    destructor Destroy; override;
    procedure AddResponse(const AURL, AText: string);
    property FailOnFetch: Boolean read FFailOnFetch write FFailOnFetch;
    property FetchCount: Integer read FFetchCount;
  end;

  TRemotePackageTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;
    function ArtifactURL(const APath: string): string;
    function Bytes(const AText: string): TBytes;
    function CachePath(const AProjectDirectory,
      AArtifactPath: string): string;
    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteLockfile(const AProjectDirectory: string;
      const AEntryHash, ANativeHash, AOtherHash: string);
    procedure WriteTextFile(const APath, AText: string);
    procedure TestMaterializesEntryAndCurrentPlatformArtifacts;
    procedure TestReusesVerifiedCacheWithoutFetching;
    procedure TestRejectsHashMismatchWithoutCommittingArtifact;
    procedure TestRejectsUnpinnedPackage;
    procedure TestRejectsRawHTTPSReference;
    procedure TestRejectsUnsafeProviderRepository;
    procedure TestRejectsUnsafeArtifactPath;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

constructor TFixtureRemotePackageResolver.Create(
  const ACacheDirectory: string);
begin
  inherited Create(ACacheDirectory);
  FResponses := TStringList.Create;
  FResponses.NameValueSeparator := '=';
end;

destructor TFixtureRemotePackageResolver.Destroy;
begin
  FResponses.Free;
  inherited;
end;

procedure TFixtureRemotePackageResolver.AddResponse(
  const AURL, AText: string);
begin
  FResponses.Values[AURL] := AText;
end;

function TFixtureRemotePackageResolver.FetchArtifact(
  const AURL: string): TBytes;
var
  ErrorOffset, ResponseIndex: Integer;
  ResponseText: string;
begin
  Inc(FFetchCount);
  if FFailOnFetch then
    raise Exception.Create('unexpected provider GET');
  ResponseIndex := FResponses.IndexOfName(AURL);
  if ResponseIndex < 0 then
    raise Exception.Create('missing fixture response for ' + AURL);
  ResponseText := FResponses.ValueFromIndex[ResponseIndex];
  if not TryEncodeUTF8(ResponseText, Result, ErrorOffset) then
    raise Exception.CreateFmt(
      'Fixture response encoding failed at %d', [ErrorOffset]);
end;

procedure TRemotePackageTests.SetupTests;
begin
  Test('Materializes the entry and current-platform artifacts',
    TestMaterializesEntryAndCurrentPlatformArtifacts);
  Test('Reuses a verified cache without fetching',
    TestReusesVerifiedCacheWithoutFetching);
  Test('Rejects a hash mismatch without committing the artifact',
    TestRejectsHashMismatchWithoutCommittingArtifact);
  Test('Rejects a package absent from the lockfile',
    TestRejectsUnpinnedPackage);
  Test('Rejects raw HTTPS references',
    TestRejectsRawHTTPSReference);
  Test('Rejects provider repository traversal',
    TestRejectsUnsafeProviderRepository);
  Test('Rejects lockfile artifact traversal',
    TestRejectsUnsafeArtifactPath);
end;

procedure TRemotePackageTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FTempDirectories := TStringList.Create;
end;

procedure TRemotePackageTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  inherited AfterAll;
end;

function TRemotePackageTests.CreateTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-remote-package-' + IntToStr(Random(MaxInt));
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
end;

procedure TRemotePackageTests.DeleteDirectoryTree(const APath: string);
var
  EntryPath: string;
  SearchRecord: TSearchRec;
begin
  if not DirectoryExists(APath) then
    Exit;

  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    SearchRecord) = 0 then
  begin
    repeat
      if (SearchRecord.Name = '.') or (SearchRecord.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRecord.Name;
      if (SearchRecord.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRecord) <> 0;
    FindClose(SearchRecord);
  end;
  RemoveDir(APath);
end;

function TRemotePackageTests.Bytes(const AText: string): TBytes;
var
  ErrorOffset: Integer;
begin
  if not TryEncodeUTF8(AText, Result, ErrorOffset) then
    raise Exception.CreateFmt('Fixture encoding failed at %d', [ErrorOffset]);
end;

function TRemotePackageTests.ArtifactURL(const APath: string): string;
begin
  Result := 'https://raw.githubusercontent.com/' +
    PACKAGE_REPOSITORY + '/' + RESOLVED_REFERENCE + '/' + APath;
end;

function TRemotePackageTests.CachePath(const AProjectDirectory,
  AArtifactPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(AProjectDirectory) +
    '.goccia' + PathDelim + 'packages' + PathDelim + 'github' +
    PathDelim + 'frostney' + PathDelim + 'GocciaScript-Raylib' +
    PathDelim + RESOLVED_REFERENCE + PathDelim +
    StringReplace(AArtifactPath, '/', PathDelim, [rfReplaceAll]);
end;

procedure TRemotePackageTests.WriteTextFile(
  const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  WriteUTF8FileText(APath, AText);
end;

procedure TRemotePackageTests.WriteLockfile(
  const AProjectDirectory: string;
  const AEntryHash, ANativeHash, AOtherHash: string);
begin
  WriteTextFile(IncludeTrailingPathDelimiter(AProjectDirectory) +
    'goccia.lock.json',
    '{' + sLineBreak +
    '  "version": 1,' + sLineBreak +
    '  "packages": {' + sLineBreak +
    '    "' + PACKAGE_REFERENCE + '": {' + sLineBreak +
    '      "resolvedRef": "' + RESOLVED_REFERENCE + '",' + sLineBreak +
    '      "entry": "' + ENTRY_PATH + '",' + sLineBreak +
    '      "artifacts": {' + sLineBreak +
    '        "' + ENTRY_PATH + '": {"sha256": "' +
      AEntryHash + '"},' + sLineBreak +
    '        "' + NATIVE_PATH + '": {"sha256": "' +
      ANativeHash + '", "platform": "' + GetBuildOS + '-' +
      GetBuildArch + '"},' + sLineBreak +
    '        "' + OTHER_PLATFORM_PATH + '": {"sha256": "' +
      AOtherHash + '", "platform": "unsupported-other"}' + sLineBreak +
    '      }' + sLineBreak +
    '    }' + sLineBreak +
    '  }' + sLineBreak +
    '}');
end;

procedure TRemotePackageTests.TestMaterializesEntryAndCurrentPlatformArtifacts;
var
  EntryResult, ImportMapPath, ProjectDirectory: string;
  Resolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(ImportMapPath, '{"imports":{}}');
  WriteLockfile(ProjectDirectory,
    SHA256Hex(Bytes(ENTRY_TEXT)),
    SHA256Hex(Bytes(NATIVE_TEXT)),
    SHA256Hex(Bytes(OTHER_PLATFORM_TEXT)));

  Resolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    Resolver.AddResponse(ArtifactURL(ENTRY_PATH), ENTRY_TEXT);
    Resolver.AddResponse(ArtifactURL(NATIVE_PATH), NATIVE_TEXT);
    Resolver.AddResponse(ArtifactURL(OTHER_PLATFORM_PATH),
      OTHER_PLATFORM_TEXT);
    EntryResult := Resolver.ResolvePackage(
      PACKAGE_REFERENCE, ImportMapPath);

    Expect<string>(EntryResult).ToBe(
      CachePath(ProjectDirectory, ENTRY_PATH));
    Expect<Integer>(Resolver.FetchCount).ToBe(2);
    Expect<Boolean>(HostFileExists(
      CachePath(ProjectDirectory, ENTRY_PATH))).ToBe(True);
    Expect<Boolean>(HostFileExists(
      CachePath(ProjectDirectory, NATIVE_PATH))).ToBe(True);
    Expect<Boolean>(HostFileExists(
      CachePath(ProjectDirectory, OTHER_PLATFORM_PATH))).ToBe(False);
  finally
    Resolver.Free;
  end;
end;

procedure TRemotePackageTests.TestReusesVerifiedCacheWithoutFetching;
var
  ImportMapPath, ProjectDirectory: string;
  OfflineResolver, PopulatingResolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(ImportMapPath, '{"imports":{}}');
  WriteLockfile(ProjectDirectory,
    SHA256Hex(Bytes(ENTRY_TEXT)),
    SHA256Hex(Bytes(NATIVE_TEXT)),
    SHA256Hex(Bytes(OTHER_PLATFORM_TEXT)));

  PopulatingResolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    PopulatingResolver.AddResponse(ArtifactURL(ENTRY_PATH), ENTRY_TEXT);
    PopulatingResolver.AddResponse(ArtifactURL(NATIVE_PATH), NATIVE_TEXT);
    PopulatingResolver.ResolvePackage(PACKAGE_REFERENCE, ImportMapPath);
  finally
    PopulatingResolver.Free;
  end;

  OfflineResolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    OfflineResolver.FailOnFetch := True;
    Expect<string>(OfflineResolver.ResolvePackage(
      PACKAGE_REFERENCE, ImportMapPath)).ToBe(
        CachePath(ProjectDirectory, ENTRY_PATH));
    Expect<Integer>(OfflineResolver.FetchCount).ToBe(0);
  finally
    OfflineResolver.Free;
  end;
end;

procedure TRemotePackageTests.TestRejectsHashMismatchWithoutCommittingArtifact;
var
  ErrorRaised: Boolean;
  ImportMapPath, ProjectDirectory: string;
  Resolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(ImportMapPath, '{"imports":{}}');
  WriteLockfile(ProjectDirectory,
    SHA256Hex(Bytes(ENTRY_TEXT)),
    SHA256Hex(Bytes(NATIVE_TEXT)),
    SHA256Hex(Bytes(OTHER_PLATFORM_TEXT)));
  Resolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    Resolver.AddResponse(ArtifactURL(ENTRY_PATH), 'tampered');
    ErrorRaised := False;
    try
      Resolver.ResolvePackage(PACKAGE_REFERENCE, ImportMapPath);
    except
      on EGocciaRemotePackageError do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Boolean>(HostFileExists(
      CachePath(ProjectDirectory, ENTRY_PATH))).ToBe(False);
  finally
    Resolver.Free;
  end;
end;

procedure TRemotePackageTests.TestRejectsUnpinnedPackage;
var
  ErrorRaised: Boolean;
  ImportMapPath, ProjectDirectory: string;
  Resolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(ImportMapPath, '{"imports":{}}');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.lock.json', '{"version":1,"packages":{}}');
  Resolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    ErrorRaised := False;
    try
      Resolver.ResolvePackage(PACKAGE_REFERENCE, ImportMapPath);
    except
      on EGocciaRemotePackageError do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(Resolver.FetchCount).ToBe(0);
  finally
    Resolver.Free;
  end;
end;

procedure TRemotePackageTests.TestRejectsRawHTTPSReference;
var
  ErrorRaised: Boolean;
  ProjectDirectory: string;
  Resolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  Resolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    ErrorRaised := False;
    try
      Resolver.ResolvePackage('https://example.com/package.js',
        IncludeTrailingPathDelimiter(ProjectDirectory) + 'goccia.json');
    except
      on EGocciaRemotePackageError do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(Resolver.FetchCount).ToBe(0);
  finally
    Resolver.Free;
  end;
end;

procedure TRemotePackageTests.TestRejectsUnsafeProviderRepository;
var
  ErrorRaised: Boolean;
  ProjectDirectory: string;
  Resolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  Resolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    ErrorRaised := False;
    try
      Resolver.ResolvePackage('github:../package@v1',
        IncludeTrailingPathDelimiter(ProjectDirectory) + 'goccia.json');
    except
      on EGocciaRemotePackageError do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(Resolver.FetchCount).ToBe(0);
  finally
    Resolver.Free;
  end;
end;

procedure TRemotePackageTests.TestRejectsUnsafeArtifactPath;
var
  ErrorRaised: Boolean;
  ImportMapPath, ProjectDirectory: string;
  Resolver: TFixtureRemotePackageResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(ImportMapPath, '{"imports":{}}');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.lock.json',
    '{"version":1,"packages":{"' + PACKAGE_REFERENCE + '":{' +
    '"resolvedRef":"' + RESOLVED_REFERENCE + '",' +
    '"entry":"../escape.js","artifacts":{"../escape.js":{' +
    '"sha256":"' + SHA256Hex(Bytes(ENTRY_TEXT)) + '"}}}}}');
  Resolver := TFixtureRemotePackageResolver.Create(
    IncludeTrailingPathDelimiter(ProjectDirectory) + '.goccia');
  try
    ErrorRaised := False;
    try
      Resolver.ResolvePackage(PACKAGE_REFERENCE, ImportMapPath);
    except
      on EGocciaRemotePackageError do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(Resolver.FetchCount).ToBe(0);
  finally
    Resolver.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TRemotePackageTests.Create('Remote Packages'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
