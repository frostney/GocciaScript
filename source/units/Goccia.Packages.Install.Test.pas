program Goccia.Packages.Install.Test;

{ Install mode against a fixture transport: no test touches the network. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  FileUtils,
  HostFileLock,
  SHA256,
  TestingPascalLibrary,
  TextEncoding,

  Goccia.Capabilities,
  Goccia.Packages.Address,
  Goccia.Packages.Install,
  Goccia.Packages.Lockfile,
  Goccia.Packages.Transport,
  Goccia.TestSetup;

const
  TAG_COMMIT = '1111111111111111111111111111111111111111';
  MOVED_COMMIT = '2222222222222222222222222222222222222222';
  MAIN_COMMIT = '3333333333333333333333333333333333333333';
  FORK_COMMIT = '4444444444444444444444444444444444444444';
  REFS_URL =
    'https://github.com/frostney/raylib.git/info/refs?service=git-upload-pack';
  KEY_V1 = 'github:frostney/raylib@v1.0.0';

type
  TFixtureTransport = class(TGocciaProviderTransport)
  public
    Responses: TDictionary<string, string>;
    Requests: TStringList;
    FailOnFetch: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Get(const AURL, AHost: string;
      const AMaxBytes: Integer): TGocciaProviderResponse; override;
  end;

  TInstallTests = class(TTestSuite)
  private
    FProject: string;
    FTransport: TFixtureTransport;
    FLog: TStringList;
    FAudit: TStringList;
    procedure RecordLog(const ALine: string);
    procedure RecordAudit(const AAllowed: Boolean;
      const ASubject, AReason: string);
    function Path(const AName: string): string;
    procedure ServeRefs(const ATagCommit: string);
    procedure ServeFiles(const ACommit: string);
    procedure WriteFile(const AName, AText: string);
    function ReadFile(const AName: string): string;
    function Run(const ARequest: TGocciaInstallRequest;
      const ACapabilities: TGocciaCapabilities;
      const AEditable: Boolean = True): string;
    function AddRequest(const AKey, ASpec: string): TGocciaInstallRequest;
    function InstallRequest: TGocciaInstallRequest;
    function CacheFile(const ACommit, AName: string): string;
    procedure TestAddPinsATagAndEditsTheImportMapLast;
    procedure TestInstallReusesTheCacheOffline;
    procedure TestCommitPinMustBeAnAdvertisedTip;
    procedure TestBranchesAreRefused;
    procedure TestDenyWinsOverATypedSpec;
    procedure TestInstallNeedsAGrantForUnlockedPackages;
    procedure TestFrozenRefusesToChangeTheLock;
    procedure TestMovedTagNeedsAcceptMovedTags;
    procedure TestCheckRefsFindsMovedTagsAndImposters;
    procedure TestRemovePrunesTheLockAndCache;
    procedure TestFailedCrawlLeavesTheImportMapAlone;
    procedure TestPrefixEntriesPinWhatTheProjectImports;
    procedure TestExplicitImportMapIsNotEdited;
    procedure TestInstallRefusesBytesThatDifferFromThePin;
    procedure TestAddOfALockedForkCommitIsCaught;
    procedure TestInstallOfALockedForkCommitIsCaughtOnFetch;
    procedure TestUnusedPrefixEntryIsSatisfied;
    procedure TestFrozenListsOnlyRealChanges;
    procedure TestReplacingAnAddKeyRepinsAndPrunes;
    procedure TestFileSetChangesAreCountedAndAudited;
    procedure TestSymlinkedImportMapIsNotEdited;
    procedure TestImportMapKeepsItsMode;
    procedure TestRemoveEditsTheImportMapFirst;
    procedure TestCaseCollisionsWriteNothing;
    procedure TestHeldInstallLockFails;
    procedure TestUsageErrorsExitTwo;
    procedure TestPrintedLinesAreJSON;
    procedure TestCommitPinsAndBranchMovement;
    procedure TestTypedSpecGrantIsAudited;
    procedure TestPruneDoesNotFollowLinks;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

var
  ProjectCounter: Integer = 0;

function Pkt(const ALine: string): string;
begin
  Result := LowerCase(IntToHex(Length(ALine) + 4, 4)) + ALine;
end;

procedure DeleteTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if not DirectoryExists(APath) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
     SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

{ TFixtureTransport }

constructor TFixtureTransport.Create;
begin
  inherited Create;
  Responses := TDictionary<string, string>.Create;
  Requests := TStringList.Create;
end;

destructor TFixtureTransport.Destroy;
begin
  Requests.Free;
  Responses.Free;
  inherited;
end;

function TFixtureTransport.Get(const AURL, AHost: string;
  const AMaxBytes: Integer): TGocciaProviderResponse;
var
  ErrorOffset: Integer;
  Text: string;
begin
  Requests.Add(AHost + ' ' + AURL);
  if FailOnFetch then
    raise EGocciaProviderTransportError.Create('unexpected request');
  if not Responses.TryGetValue(AURL, Text) then
  begin
    Result.StatusCode := 404;
    Result.Body := nil;
    Exit;
  end;
  Result.StatusCode := 200;
  TryEncodeUTF8(Text, Result.Body, ErrorOffset);
end;

{ TInstallTests }

procedure TInstallTests.SetupTests;
begin
  Test('--add pins a tag, and edits the import map last',
    TestAddPinsATagAndEditsTheImportMapLast);
  Test('--install reuses a verified cache without the network',
    TestInstallReusesTheCacheOffline);
  Test('A commit pin must be an advertised tag or branch tip',
    TestCommitPinMustBeAnAdvertisedTip);
  Test('Branches are not refs', TestBranchesAreRefused);
  Test('A deny wins over a typed spec', TestDenyWinsOverATypedSpec);
  Test('--install needs a grant for an unlocked package',
    TestInstallNeedsAGrantForUnlockedPackages);
  Test('--frozen refuses to change the lockfile',
    TestFrozenRefusesToChangeTheLock);
  Test('A moved tag needs --accept-moved-tags',
    TestMovedTagNeedsAcceptMovedTags);
  Test('--check-refs finds moved tags and unadvertised commits',
    TestCheckRefsFindsMovedTagsAndImposters);
  Test('--remove prunes the lockfile and the cache',
    TestRemovePrunesTheLockAndCache);
  Test('A failed crawl leaves the import map and lockfile alone',
    TestFailedCrawlLeavesTheImportMapAlone);
  Test('A prefix entry pins what the project imports through it',
    TestPrefixEntriesPinWhatTheProjectImports);
  Test('An explicit import map is not edited',
    TestExplicitImportMapIsNotEdited);
  Test('--install refuses fetched bytes that differ from the pin',
    TestInstallRefusesBytesThatDifferFromThePin);
  Test('--add of a key the lockfile pins to a fork commit is caught',
    TestAddOfALockedForkCommitIsCaught);
  Test('--install of a locked fork commit is caught when it must fetch',
    TestInstallOfALockedForkCommitIsCaughtOnFetch);
  Test('An unused prefix entry needs nothing and satisfies --frozen',
    TestUnusedPrefixEntryIsSatisfied);
  Test('--frozen lists only the packages that change',
    TestFrozenListsOnlyRealChanges);
  Test('Re-using an --add key re-pins the package and prunes the old one',
    TestReplacingAnAddKeyRepinsAndPrunes);
  Test('File-set changes are counted and audited',
    TestFileSetChangesAreCountedAndAudited);
  {$IFDEF UNIX}
  Test('An import map that is a symbolic link is not edited',
    TestSymlinkedImportMapIsNotEdited);
  Test('Editing the import map keeps its mode', TestImportMapKeepsItsMode);
  Test('Pruning does not follow a link planted in the cache',
    TestPruneDoesNotFollowLinks);
  {$ENDIF}
  Test('--remove edits the import map before the lockfile',
    TestRemoveEditsTheImportMapFirst);
  Test('Case-colliding files are refused before the cache is written',
    TestCaseCollisionsWriteNothing);
  Test('An install lock another install holds fails the run',
    TestHeldInstallLockFails);
  Test('Malformed input and unknown keys exit with status 2',
    TestUsageErrorsExitTwo);
  Test('Printed import-map lines are JSON', TestPrintedLinesAreJSON);
  Test('Commit pins are not re-resolved, and --check-refs only warns',
    TestCommitPinsAndBranchMovement);
  Test('A typed spec''s grant is audited', TestTypedSpecGrantIsAudited);
end;

procedure TInstallTests.BeforeEach;
begin
  inherited BeforeEach;
  Inc(ProjectCounter);
  FProject := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-install-' + IntToStr(GetProcessID) + '-' +
    IntToStr(ProjectCounter);
  ForceDirectories(FProject);
  FTransport := TFixtureTransport.Create;
  FLog := TStringList.Create;
  FAudit := TStringList.Create;
  ServeRefs(TAG_COMMIT);
  ServeFiles(TAG_COMMIT);
  ServeFiles(MOVED_COMMIT);
  ServeFiles(MAIN_COMMIT);
  ServeFiles(FORK_COMMIT);
end;

procedure TInstallTests.AfterEach;
begin
  FAudit.Free;
  FLog.Free;
  FTransport.Free;
  DeleteTree(FProject);
  inherited AfterEach;
end;

procedure TInstallTests.RecordLog(const ALine: string);
begin
  FLog.Add(ALine);
end;

procedure TInstallTests.RecordAudit(const AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if AAllowed then
    FAudit.Add('allow ' + ASubject + ' ' + AReason)
  else
    FAudit.Add('deny ' + ASubject + ' ' + AReason);
end;

function TInstallTests.Path(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FProject) +
    StringReplace(AName, '/', PathDelim, [rfReplaceAll]);
end;

procedure TInstallTests.WriteFile(const AName, AText: string);
begin
  ForceDirectories(ExtractFileDir(Path(AName)));
  WriteUTF8FileText(Path(AName), AText);
end;

function TInstallTests.ReadFile(const AName: string): string;
begin
  if FileExists(Path(AName)) then
    Result := ReadUTF8FileText(Path(AName))
  else
    Result := '<missing>';
end;

function TInstallTests.CacheFile(const ACommit, AName: string): string;
begin
  Result := '.goccia/packages/github/frostney/raylib/' + ACommit + '/' + AName;
end;

procedure TInstallTests.ServeRefs(const ATagCommit: string);
begin
  FTransport.Responses.AddOrSetValue(REFS_URL,
    Pkt('# service=git-upload-pack'#10) + '0000' +
    Pkt(MAIN_COMMIT + ' HEAD'#0'side-band-64k'#10) +
    Pkt(MAIN_COMMIT + ' refs/heads/main'#10) +
    Pkt(FORK_COMMIT + ' refs/pull/9/head'#10) +
    Pkt(ATagCommit + ' refs/tags/v1.0.0'#10) + '0000');
end;

procedure TInstallTests.ServeFiles(const ACommit: string);

  procedure Serve(const AName, AText: string);
  begin
    FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney',
      'raylib', ACommit, AName), AText);
  end;

begin
  Serve('bindings/raylib.ts', 'import { s } from "./lib/structs.ts";' +
    ' export const lib = new URL("../native/lib.so", import.meta.url);' +
    ' export const v = "' + ACommit + '";');
  Serve('bindings/lib/structs.ts', 'export const s = 1;');
  Serve('bindings/extra.ts', 'export const extra = 1;');
  Serve('bindings/bare.ts', 'import "lodash";');
  Serve('native/lib.so', 'native');
end;

function TInstallTests.Run(const ARequest: TGocciaInstallRequest;
  const ACapabilities: TGocciaCapabilities;
  const AEditable: Boolean): string;
var
  Installer: TGocciaPackageInstaller;
begin
  Result := '';
  Installer := TGocciaPackageInstaller.Create(Path('goccia.json'), AEditable,
    ACapabilities, FTransport);
  try
    Installer.OnLog := RecordLog;
    Installer.OnInstallAudit := RecordAudit;
    try
      Installer.Run(ARequest);
    except
      on E: EGocciaInstallError do
        Result := IntToStr(E.ExitCode) + ': ' + E.Message;
    end;
  finally
    Installer.Free;
  end;
end;

function TInstallTests.AddRequest(const AKey,
  ASpec: string): TGocciaInstallRequest;
begin
  Result := Default(TGocciaInstallRequest);
  SetLength(Result.Adds, 1);
  Result.Adds[0].Key := AKey;
  Result.Adds[0].Spec := ASpec;
end;

function TInstallTests.InstallRequest: TGocciaInstallRequest;
begin
  Result := Default(TGocciaInstallRequest);
  Result.Install := True;
end;

procedure TInstallTests.TestAddPinsATagAndEditsTheImportMapLast;
var
  Lock: TGocciaLockfile;
  Pin: string;
begin
  WriteFile('goccia.json', '{' + #10 + '  "mode": "bytecode"' + #10 + '}' +
    #10);
  Expect<string>(Run(AddRequest('raylib',
    KEY_V1 + '/bindings/raylib.ts'), TGocciaCapabilities.None)).ToBe('');
  Expect<string>(ReadFile('goccia.json')).ToBe('{' + #10 +
    '  "mode": "bytecode",' + #10 + '  "imports": {' + #10 +
    '    "raylib": "' + KEY_V1 + '/bindings/raylib.ts"' + #10 + '  }' + #10 +
    '}' + #10);
  Lock := LoadLockfile(Path('goccia.lock.json'));
  try
    Expect<Integer>(Lock.Packages.Count).ToBe(1);
    Expect<string>(Lock.Packages[0].Commit).ToBe(TAG_COMMIT);
    Expect<Boolean>(Lock.Packages[0].RefKind = lrkTag).ToBe(True);
    Expect<Integer>(Lock.Packages[0].ArtifactCount).ToBe(3);
    Expect<Boolean>(Lock.Packages[0].FindArtifact('native/lib.so', Pin))
      .ToBe(True);
  finally
    Lock.Free;
  end;
  Expect<string>(ReadFile(CacheFile(TAG_COMMIT, 'native/lib.so')))
    .ToBe('native');
  { refs from github.com, files from raw.githubusercontent.com only. }
  Expect<Boolean>(Pos('github.com ' + REFS_URL, FTransport.Requests.Text) > 0)
    .ToBe(True);
  Expect<Boolean>(FAudit.IndexOf('allow ' + KEY_V1 + ' added: tag -> ' +
    TAG_COMMIT + '; 3 files') >= 0).ToBe(True);
end;

procedure TInstallTests.TestInstallReusesTheCacheOffline;
begin
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  FTransport.FailOnFetch := True;
  FTransport.Requests.Clear;
  Expect<string>(Run(InstallRequest, TGocciaCapabilities.None)).ToBe('');
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);
  Expect<Boolean>(FLog.IndexOf('Verified goccia.lock.json; unchanged') >= 0)
    .ToBe(True);
end;

procedure TInstallTests.TestCommitPinMustBeAnAdvertisedTip;
var
  Outcome: string;
begin
  Expect<string>(Run(AddRequest('main', 'github:frostney/raylib@' +
    MAIN_COMMIT + '/bindings/raylib.ts'), TGocciaCapabilities.None)).ToBe('');
  DeleteFile(Path('goccia.json'));
  DeleteFile(Path('goccia.lock.json'));
  { The fork's commit is served by the raw host under this repository's
    name, but only refs/pull advertises it. }
  Outcome := Run(AddRequest('fork', 'github:frostney/raylib@' +
    FORK_COMMIT + '/bindings/raylib.ts'), TGocciaCapabilities.None);
  Expect<Boolean>(Pos('1: ', Outcome) = 1).ToBe(True);
  Expect<Boolean>(Pos('is not the tip of any tag or branch', Outcome) > 0)
    .ToBe(True);
  Expect<string>(ReadFile('goccia.json')).ToBe('<missing>');
  Expect<string>(ReadFile('goccia.lock.json')).ToBe('<missing>');
  Expect<Boolean>(DirectoryExists(Path(CacheFile(FORK_COMMIT, ''))))
    .ToBe(False);
end;

procedure TInstallTests.TestBranchesAreRefused;
begin
  Expect<Boolean>(Pos('branches are not accepted', Run(AddRequest('main',
    'github:frostney/raylib@main/bindings/raylib.ts'),
    TGocciaCapabilities.None)) > 0).ToBe(True);
end;

procedure TInstallTests.TestDenyWinsOverATypedSpec;
var
  Outcome: string;
begin
  Outcome := Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None.Deny(gcImport, 'github:frostney'));
  Expect<string>(Outcome).ToBe('1: import: ' + KEY_V1 +
    ' (refused by the import deny github:frostney)');
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);
end;

procedure TInstallTests.TestInstallNeedsAGrantForUnlockedPackages;
begin
  WriteFile('goccia.json', '{"imports": {"raylib": "' + KEY_V1 +
    '/bindings/raylib.ts"}}');
  Expect<Boolean>(Pos('1: import: ' + KEY_V1,
    Run(InstallRequest, TGocciaCapabilities.None)) = 1).ToBe(True);
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);
  Expect<string>(Run(InstallRequest,
    TGocciaCapabilities.None.Allow(gcImport, 'github:frostney/raylib')))
    .ToBe('');
  Expect<Boolean>(FileExists(Path('goccia.lock.json'))).ToBe(True);
end;

procedure TInstallTests.TestFrozenRefusesToChangeTheLock;
var
  Request: TGocciaInstallRequest;
  Outcome: string;
begin
  WriteFile('goccia.json', '{"imports": {"raylib": "' + KEY_V1 +
    '/bindings/raylib.ts"}}');
  Request := InstallRequest;
  Request.Frozen := True;
  Outcome := Run(Request, TGocciaCapabilities.None.Allow(gcImport, 'github'));
  Expect<Boolean>(Pos('goccia.lock.json is out of date; --frozen does not ' +
    'write it.', Outcome) > 0).ToBe(True);
  Expect<Boolean>(Pos('+ ' + KEY_V1, Outcome) > 0).ToBe(True);
  Expect<string>(ReadFile('goccia.lock.json')).ToBe('<missing>');
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);

  Run(InstallRequest, TGocciaCapabilities.None.Allow(gcImport, 'github'));
  FTransport.FailOnFetch := True;
  Expect<string>(Run(Request, TGocciaCapabilities.None)).ToBe('');

  { An entry the import map dropped is a difference too. }
  WriteFile('goccia.json', '{"imports": {}}');
  Outcome := Run(Request, TGocciaCapabilities.None);
  Expect<Boolean>(Pos('- ' + KEY_V1, Outcome) > 0).ToBe(True);
end;

procedure TInstallTests.TestMovedTagNeedsAcceptMovedTags;
var
  Request: TGocciaInstallRequest;
  Lock: TGocciaLockfile;
begin
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  ServeRefs(MOVED_COMMIT);
  Request := Default(TGocciaInstallRequest);
  Request.Update := True;
  Expect<Boolean>(Pos('tag v1.0.0 of frostney/raylib moved: locked ' +
    Copy(TAG_COMMIT, 1, 12) + ', now ' + Copy(MOVED_COMMIT, 1, 12),
    Run(Request, TGocciaCapabilities.None.Allow(gcImport, 'github'))) > 0)
    .ToBe(True);
  { A run never re-resolves: --install keeps the locked commit. }
  Expect<string>(Run(InstallRequest, TGocciaCapabilities.None)).ToBe('');
  Request.AcceptMovedTags := True;
  Expect<string>(Run(Request,
    TGocciaCapabilities.None.Allow(gcImport, 'github'))).ToBe('');
  Lock := LoadLockfile(Path('goccia.lock.json'));
  try
    Expect<string>(Lock.Packages[0].Commit).ToBe(MOVED_COMMIT);
  finally
    Lock.Free;
  end;
  { The old commit's files are pruned. }
  Expect<Boolean>(DirectoryExists(Path(CacheFile(TAG_COMMIT, ''))))
    .ToBe(False);
  Expect<Boolean>(FileExists(Path(CacheFile(MOVED_COMMIT,
    'bindings/raylib.ts')))).ToBe(True);
end;

procedure TInstallTests.TestCheckRefsFindsMovedTagsAndImposters;
var
  Request: TGocciaInstallRequest;
  Lock: string;
begin
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  Request := InstallRequest;
  Request.Frozen := True;
  Request.CheckRefs := True;
  Expect<string>(Run(Request,
    TGocciaCapabilities.None.Allow(gcImport, 'github'))).ToBe('');
  ServeRefs(MOVED_COMMIT);
  Expect<Boolean>(Pos('moved', Run(Request,
    TGocciaCapabilities.None.Allow(gcImport, 'github'))) > 0).ToBe(True);

  { A lock edited in a pull request to pin a fork's commit under the
    upstream name is caught. }
  DeleteTree(Path('.goccia'));
  DeleteFile(Path('goccia.lock.json'));
  WriteFile('goccia.json', '{}');
  Run(AddRequest('main', 'github:frostney/raylib@' + MAIN_COMMIT +
    '/bindings/raylib.ts'), TGocciaCapabilities.None);
  Lock := ReadFile('goccia.lock.json');
  WriteFile('goccia.lock.json', StringReplace(Lock, MAIN_COMMIT, FORK_COMMIT,
    [rfReplaceAll]));
  WriteFile('goccia.json', StringReplace(ReadFile('goccia.json'), MAIN_COMMIT,
    FORK_COMMIT, [rfReplaceAll]));
  Expect<Boolean>(Pos('is not the tip of any tag or branch', Run(Request,
    TGocciaCapabilities.None.Allow(gcImport, 'github'))) > 0).ToBe(True);
end;

procedure TInstallTests.TestRemovePrunesTheLockAndCache;
var
  Request: TGocciaInstallRequest;
begin
  WriteFile('goccia.json', '{' + #10 + '  "imports": {' + #10 +
    '    "@/": "./src/"' + #10 + '  }' + #10 + '}' + #10);
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  FTransport.FailOnFetch := True;
  Request := Default(TGocciaInstallRequest);
  SetLength(Request.Removes, 1);
  Request.Removes[0] := 'raylib';
  Expect<string>(Run(Request, TGocciaCapabilities.None)).ToBe('');
  Expect<string>(ReadFile('goccia.json')).ToBe('{' + #10 + '  "imports": {' +
    #10 + '    "@/": "./src/"' + #10 + '  }' + #10 + '}' + #10);
  Expect<Boolean>(Pos(KEY_V1, ReadFile('goccia.lock.json')) = 0).ToBe(True);
  Expect<Boolean>(DirectoryExists(Path(CacheFile(TAG_COMMIT, ''))))
    .ToBe(False);
  Expect<Boolean>(FAudit.IndexOf('allow ' + KEY_V1 + ' removed; 3 files') >=
    0).ToBe(True);
  { An entry the import map does not have is a usage error. }
  Expect<Boolean>(Pos('2: ', Run(Request, TGocciaCapabilities.None)) = 1)
    .ToBe(True);
end;

procedure TInstallTests.TestFailedCrawlLeavesTheImportMapAlone;
var
  Outcome: string;
begin
  WriteFile('goccia.json', '{"imports": {}}');
  Outcome := Run(AddRequest('bare', KEY_V1 + '/bindings/bare.ts'),
    TGocciaCapabilities.None);
  Expect<Boolean>(Pos('a package may import only its own files', Outcome) > 0)
    .ToBe(True);
  Expect<string>(ReadFile('goccia.json')).ToBe('{"imports": {}}');
  Expect<string>(ReadFile('goccia.lock.json')).ToBe('<missing>');
end;

procedure TInstallTests.TestPrefixEntriesPinWhatTheProjectImports;
var
  Lock: TGocciaLockfile;
  Pin: string;
begin
  WriteFile('src/app.js', 'import { extra } from "ray/extra.ts";' + #10 +
    'import { s } from "ray/lib/structs";');
  Expect<string>(Run(AddRequest('ray/', KEY_V1 + '/bindings/'),
    TGocciaCapabilities.None)).ToBe('');
  Lock := LoadLockfile(Path('goccia.lock.json'));
  try
    Expect<Boolean>(Lock.Packages[0].FindArtifact('bindings/extra.ts', Pin))
      .ToBe(True);
    Expect<Boolean>(Lock.Packages[0].FindArtifact('bindings/lib/structs.ts',
      Pin)).ToBe(True);
    Expect<Boolean>(Lock.Packages[0].FindArtifact('bindings/raylib.ts', Pin))
      .ToBe(False);
  finally
    Lock.Free;
  end;
end;

procedure TInstallTests.TestExplicitImportMapIsNotEdited;
begin
  WriteFile('goccia.json', '{"imports": {}}');
  Expect<string>(Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None, False)).ToBe('');
  Expect<string>(ReadFile('goccia.json')).ToBe('{"imports": {}}');
  Expect<string>(ReadFile('goccia.lock.json')).ToBe('<missing>');
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);
  Expect<Boolean>(Pos('Nothing was written', FLog.Text) > 0).ToBe(True);
end;

function HashText(const AText: string): string;
var
  Bytes: TBytes;
  ErrorOffset: Integer;
begin
  TryEncodeUTF8(AText, Bytes, ErrorOffset);
  Result := SHA256Hex(Bytes);
end;

{ A lockfile pinning KEY_V1 at ACommit with bindings/raylib.ts only. }
function LockText(const AKey, ACommit, AText: string): string;
begin
  Result := '{"version": 1, "packages": {"' + AKey + '": {"ref": "tag", ' +
    '"commit": "' + ACommit + '", "artifacts": {"index.js": {"sha256": "' +
    HashText(AText) + '"}}}}}';
end;

procedure TInstallTests.TestInstallRefusesBytesThatDifferFromThePin;
var
  Lock, Outcome: string;
begin
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    TAG_COMMIT, 'index.js'), 'export const v = "served";');
  WriteFile('goccia.json', '{"imports": {"raylib": "' + KEY_V1 +
    '/index.js"}}');
  Lock := LockText(KEY_V1, TAG_COMMIT, 'export const v = "reviewed";');
  WriteFile('goccia.lock.json', Lock);
  Outcome := Run(InstallRequest,
    TGocciaCapabilities.None.Allow(gcImport, 'github'));
  Expect<string>(Outcome).ToBe('1: ' + KEY_V1 + ': index.js does not match ' +
    'its pin in goccia.lock.json; the lockfile and the cache are unchanged');
  Expect<string>(ReadFile('goccia.lock.json')).ToBe(Lock);
  Expect<Boolean>(FileExists(Path(CacheFile(TAG_COMMIT, 'index.js'))))
    .ToBe(False);
end;

procedure TInstallTests.TestAddOfALockedForkCommitIsCaught;
var
  Outcome: string;
begin
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    FORK_COMMIT, 'index.js'), 'export const v = "fork";');
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    TAG_COMMIT, 'index.js'), 'export const v = "genuine";');
  WriteFile('goccia.json', '{}');
  WriteFile('goccia.lock.json', LockText(KEY_V1, FORK_COMMIT,
    'export const v = "fork";'));
  { The cache holds the fork's bytes too, so nothing needs fetching. }
  WriteFile(CacheFile(FORK_COMMIT, 'index.js'), 'export const v = "fork";');
  Outcome := Run(AddRequest('raylib', KEY_V1 + '/index.js'),
    TGocciaCapabilities.None);
  Expect<Boolean>(Pos('tag v1.0.0 of frostney/raylib moved: locked ' +
    Copy(FORK_COMMIT, 1, 12), Outcome) > 0).ToBe(True);
  Expect<Boolean>(Pos('github.com ' + REFS_URL, FTransport.Requests.Text) > 0)
    .ToBe(True);
  Expect<string>(ReadFile('goccia.json')).ToBe('{}');
end;

procedure TInstallTests.TestInstallOfALockedForkCommitIsCaughtOnFetch;
var
  Outcome: string;
begin
  { A commit pin of a fork's commit: fine while cached, refused as soon as
    its files must be fetched. }
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    FORK_COMMIT, 'index.js'), 'export const v = "fork";');
  WriteFile('goccia.json', '{"imports": {"f": "github:frostney/raylib@' +
    FORK_COMMIT + '/index.js"}}');
  WriteFile('goccia.lock.json', '{"version": 1, "packages": {' +
    '"github:frostney/raylib@' + FORK_COMMIT + '": {"ref": "commit", ' +
    '"commit": "' + FORK_COMMIT + '", "artifacts": {"index.js": {' +
    '"sha256": "' + HashText('export const v = "fork";') + '"}}}}}');
  Outcome := Run(InstallRequest,
    TGocciaCapabilities.None.Allow(gcImport, 'github'));
  Expect<Boolean>(Pos('is not the tip of any tag or branch', Outcome) > 0)
    .ToBe(True);
  Expect<Boolean>(FileExists(Path(CacheFile(FORK_COMMIT, 'index.js'))))
    .ToBe(False);
end;

procedure TInstallTests.TestUnusedPrefixEntryIsSatisfied;
var
  Request: TGocciaInstallRequest;
begin
  WriteFile('goccia.json', '{"imports": {"ray/": "' + KEY_V1 +
    '/bindings/"}}');
  FTransport.FailOnFetch := True;
  Expect<string>(Run(InstallRequest, TGocciaCapabilities.None)).ToBe('');
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);
  Request := InstallRequest;
  Request.Frozen := True;
  Expect<string>(Run(Request, TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(Pos(KEY_V1, ReadFile('goccia.lock.json')) = 0).ToBe(True);
end;

procedure TInstallTests.TestFrozenListsOnlyRealChanges;
var
  Outcome: string;
  Request: TGocciaInstallRequest;
begin
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  WriteFile('goccia.json', '{"imports": {"raylib": "' + KEY_V1 +
    '/bindings/raylib.ts", "other": "github:frostney/other@v2/x.ts"}}');
  Request := InstallRequest;
  Request.Frozen := True;
  Outcome := Run(Request, TGocciaCapabilities.None);
  Expect<Boolean>(Pos('+ github:frostney/other@v2', Outcome) > 0).ToBe(True);
  Expect<Boolean>(Pos('~ ' + KEY_V1, Outcome) = 0).ToBe(True);
end;

procedure TInstallTests.TestReplacingAnAddKeyRepinsAndPrunes;
begin
  Run(AddRequest('a', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  Run(AddRequest('b', KEY_V1 + '/bindings/extra.ts'), TGocciaCapabilities.None);
  Expect<Boolean>(Pos('bindings/extra.ts', ReadFile('goccia.lock.json')) > 0)
    .ToBe(True);
  { b now names a commit pin; v1.0.0 keeps only what a still reaches. }
  Expect<string>(Run(AddRequest('b', 'github:frostney/raylib@' + MAIN_COMMIT +
    '/bindings/extra.ts'), TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(FLog.IndexOf('Replace "b": ' + KEY_V1 +
    '/bindings/extra.ts -> github:frostney/raylib@' + MAIN_COMMIT +
    '/bindings/extra.ts') >= 0).ToBe(True);
  Expect<Boolean>(Pos('"' + KEY_V1 + '"', ReadFile('goccia.lock.json')) > 0)
    .ToBe(True);
  Expect<Boolean>(FileExists(Path(CacheFile(TAG_COMMIT, 'bindings/extra.ts'))))
    .ToBe(True);
  { a replaced too: the tag package is no longer named, and is pruned. }
  Expect<string>(Run(AddRequest('a', 'github:frostney/raylib@' + MAIN_COMMIT +
    '/bindings/raylib.ts'), TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(Pos('"' + KEY_V1 + '"', ReadFile('goccia.lock.json')) = 0)
    .ToBe(True);
  Expect<Boolean>(DirectoryExists(Path(CacheFile(TAG_COMMIT, ''))))
    .ToBe(False);
end;

procedure TInstallTests.TestFileSetChangesAreCountedAndAudited;
begin
  Run(AddRequest('a', KEY_V1 + '/bindings/extra.ts'), TGocciaCapabilities.None);
  FAudit.Clear;
  FLog.Clear;
  { A second entry of the same package adds files to its pin. }
  Expect<string>(Run(AddRequest('b', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(FLog.IndexOf('Wrote   goccia.lock.json (0 packages added, ' +
    '0 removed, 1 updated; 3 files added, 0 removed, 0 changed)') >= 0)
    .ToBe(True);
  Expect<Boolean>(FAudit.IndexOf('allow ' + KEY_V1 + ' updated: ' +
    TAG_COMMIT + ' -> ' + TAG_COMMIT + '; 3 files added, 0 removed, ' +
    '0 changed') >= 0).ToBe(True);
end;

{$IFDEF UNIX}
procedure TInstallTests.TestSymlinkedImportMapIsNotEdited;
begin
  WriteFile('real.json', '{"imports": {}}');
  fpSymlink(PAnsiChar(AnsiString(Path('real.json'))),
    PAnsiChar(AnsiString(Path('goccia.json'))));
  Expect<string>(Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(HostPathIsSymlink(Path('goccia.json'))).ToBe(True);
  Expect<string>(ReadFile('real.json')).ToBe('{"imports": {}}');
  Expect<Boolean>(Pos('Nothing was written', FLog.Text) > 0).ToBe(True);
end;

procedure TInstallTests.TestImportMapKeepsItsMode;
var
  Mode: Cardinal;
begin
  WriteFile('goccia.json', '{"imports": {}}');
  fpChmod(PAnsiChar(AnsiString(Path('goccia.json'))), &600);
  Expect<string>(Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(TryHostFileMode(Path('goccia.json'), Mode)).ToBe(True);
  Expect<Integer>(Mode).ToBe(&600);
end;

procedure TInstallTests.TestPruneDoesNotFollowLinks;
var
  Outside: string;
  Request: TGocciaInstallRequest;
begin
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  Outside := FProject + '-outside';
  ForceDirectories(Outside);
  WriteUTF8FileText(Outside + PathDelim + 'keep.txt', 'keep');
  fpSymlink(PAnsiChar(AnsiString(Outside)),
    PAnsiChar(AnsiString(Path(CacheFile(TAG_COMMIT, 'bindings/planted')))));
  Request := Default(TGocciaInstallRequest);
  SetLength(Request.Removes, 1);
  Request.Removes[0] := 'raylib';
  Expect<string>(Run(Request, TGocciaCapabilities.None)).ToBe('');
  Expect<Boolean>(FileExists(Outside + PathDelim + 'keep.txt')).ToBe(True);
  Expect<Boolean>(DirectoryExists(Path(CacheFile(TAG_COMMIT, ''))))
    .ToBe(False);
  DeleteFile(Outside + PathDelim + 'keep.txt');
  RemoveDir(Outside);
end;
{$ENDIF}

procedure TInstallTests.TestRemoveEditsTheImportMapFirst;
var
  Request: TGocciaInstallRequest;
  MapLine, LockLine: Integer;
begin
  Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None);
  Expect<Boolean>(Pos('Wrote', FLog[FLog.Count - 2]) = 1).ToBe(True);
  FLog.Clear;
  Request := Default(TGocciaInstallRequest);
  SetLength(Request.Removes, 1);
  Request.Removes[0] := 'raylib';
  Run(Request, TGocciaCapabilities.None);
  MapLine := -1;
  LockLine := -1;
  for MapLine := 0 to FLog.Count - 1 do
    if Pos('Remove  "raylib"', FLog[MapLine]) = 1 then
      Break;
  for LockLine := 0 to FLog.Count - 1 do
    if Pos('Wrote', FLog[LockLine]) = 1 then
      Break;
  Expect<Boolean>(MapLine < LockLine).ToBe(True);
end;

procedure TInstallTests.TestCaseCollisionsWriteNothing;
var
  Outcome: string;
begin
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    TAG_COMMIT, 'bindings/cc.js'), 'import "./A.js"; import "./a.js";');
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    TAG_COMMIT, 'bindings/A.js'), 'export const A = 1;');
  FTransport.Responses.AddOrSetValue(PackageArtifactURL('frostney', 'raylib',
    TAG_COMMIT, 'bindings/a.js'), 'export const a = 2;');
  Outcome := Run(AddRequest('cc', KEY_V1 + '/bindings/cc.js'),
    TGocciaCapabilities.None);
  Expect<Boolean>(Pos('differ only in case', Outcome) > 0).ToBe(True);
  Expect<Boolean>(DirectoryExists(Path('.goccia'))).ToBe(False);
  Expect<string>(ReadFile('goccia.lock.json')).ToBe('<missing>');
end;

procedure TInstallTests.TestHeldInstallLockFails;
var
  Error: string;
  Held: THostFileLock;
  Installer: TGocciaPackageInstaller;
  Outcome: string;
begin
  WriteFile('goccia.json', '{}');
  Expect<Boolean>(TryAcquireHostFileLock(Path('goccia.lock.json.lock'), &644,
    Held, Error) = hflAcquired).ToBe(True);
  try
    Installer := TGocciaPackageInstaller.Create(Path('goccia.json'), True,
      TGocciaCapabilities.None, FTransport);
    try
      Installer.LockTimeoutMilliseconds := 100;
      Outcome := '';
      try
        Installer.Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'));
      except
        on E: EGocciaInstallError do
          Outcome := IntToStr(E.ExitCode) + ': ' + E.Message;
      end;
    finally
      Installer.Free;
    end;
  finally
    ReleaseHostFileLock(Held);
  end;
  Expect<Boolean>(Pos('1: ', Outcome) = 1).ToBe(True);
  Expect<Boolean>(Pos('is locked by another GocciaScript install', Outcome) >
    0).ToBe(True);
  Expect<Integer>(FTransport.Requests.Count).ToBe(0);
end;

procedure TInstallTests.TestUsageErrorsExitTwo;
var
  Request: TGocciaInstallRequest;
begin
  WriteFile('goccia.json', '{"imports": ');
  Expect<Boolean>(Pos('2: ', Run(InstallRequest, TGocciaCapabilities.None)) =
    1).ToBe(True);
  WriteFile('goccia.json', '{"imports": {"": "' + KEY_V1 + '/x.ts"}}');
  Expect<Boolean>(Pos('2: ', Run(InstallRequest, TGocciaCapabilities.None)) =
    1).ToBe(True);
  WriteFile('goccia.json', '{"imports": {"x": "github:frostney/raylib"}}');
  Expect<Boolean>(Pos('2: ', Run(InstallRequest, TGocciaCapabilities.None)) =
    1).ToBe(True);
  WriteFile('goccia.json', '{"imports": {}}');
  Request := Default(TGocciaInstallRequest);
  SetLength(Request.Removes, 1);
  Request.Removes[0] := 'nosuch';
  Expect<Boolean>(Pos('2: ', Run(Request, TGocciaCapabilities.None)) = 1)
    .ToBe(True);
  Request := Default(TGocciaInstallRequest);
  Request.Update := True;
  SetLength(Request.UpdateKeys, 1);
  Request.UpdateKeys[0] := 'nosuch';
  Expect<Boolean>(Pos('2: ', Run(Request, TGocciaCapabilities.None)) = 1)
    .ToBe(True);
end;

procedure TInstallTests.TestPrintedLinesAreJSON;
begin
  WriteFile('goccia.json', '{}');
  Run(AddRequest('we"ird\key', KEY_V1 + '/bindings/raylib.ts'),
    TGocciaCapabilities.None, False);
  Expect<Boolean>(Pos('"we\"ird\\key": "' + KEY_V1 + '/bindings/raylib.ts"',
    FLog.Text) > 0).ToBe(True);
end;

procedure TInstallTests.TestCommitPinsAndBranchMovement;
var
  Request: TGocciaInstallRequest;
begin
  Expect<string>(Run(AddRequest('m', 'github:frostney/raylib@' + MAIN_COMMIT +
    '/bindings/raylib.ts'), TGocciaCapabilities.None)).ToBe('');
  { The branch advances. }
  FTransport.Responses.AddOrSetValue(REFS_URL,
    Pkt(MOVED_COMMIT + ' refs/heads/main'#10) +
    Pkt(TAG_COMMIT + ' refs/tags/v1.0.0'#10));
  Request := Default(TGocciaInstallRequest);
  Request.Update := True;
  Expect<string>(Run(Request, TGocciaCapabilities.None.Allow(gcImport,
    'github'))).ToBe('');
  FLog.Clear;
  Request := InstallRequest;
  Request.CheckRefs := True;
  Expect<string>(Run(Request, TGocciaCapabilities.None.Allow(gcImport,
    'github'))).ToBe('');
  Expect<Boolean>(Pos('Warning: github:frostney/raylib@' + MAIN_COMMIT +
    ': commit ' + Copy(MAIN_COMMIT, 1, 12) + ' is no longer the tip',
    FLog.Text) > 0).ToBe(True);
end;

procedure TInstallTests.TestTypedSpecGrantIsAudited;
var
  Installer: TGocciaPackageInstaller;
begin
  Installer := TGocciaPackageInstaller.Create(Path('goccia.json'), True,
    TGocciaCapabilities.None, FTransport);
  try
    Installer.OnLog := RecordLog;
    Installer.OnAudit := RecordAudit;
    Installer.Run(AddRequest('raylib', KEY_V1 + '/bindings/raylib.ts'));
  finally
    Installer.Free;
  end;
  Expect<Boolean>(FAudit.IndexOf('allow ' + KEY_V1 + ' the --add spec ' +
    'grants github:frostney/raylib for this invocation') >= 0).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TInstallTests.Create('Install mode'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
