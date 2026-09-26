program Goccia.CLI.Trust.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes,
  DateUtils,
  SysUtils,

  CLI.ConfigFile,
  FileUtils,
  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.CLI.Application,
  Goccia.CLI.Permissions,
  Goccia.CLI.Trust;

type
  TTrustTests = class(TTestSuite)
  private
    FRoot: string;
    FLoadCount: Integer;
    function WriteFile(const ARelativePath, AText: string): string;
    function LoadConfig(const APath: string): TConfigEntryArray;
    function EntryFor(const AConfigPath: string): TGocciaTrustEntry;
    function StoreError(const APath: string): string;
    procedure TestRoundTripLeavesNoTemporaryFile;
    procedure TestKeysCaseSensitivity;
    procedure TestSymlinkedConfigKeysToTarget;
    procedure TestSymlinkedConfigFileHasItsOwnKey;
    procedure TestSymlinkedDirectoryHashesAtItsTarget;
    procedure TestRetargetedScopesNeedTrust;
    procedure TestAbsentScopeParentRepointed;
    procedure TestNewerAndCorruptStoresRefused;
    procedure TestMalformedStoresRefused;
    procedure TestLockContention;
    function SaveWithLock(const AName, ALockContent: string): string;
    procedure TestTimestampIgnoresLocale;
    procedure TestStaleLocksAreReplaced;
    procedure TestPrivateDirectoryIsTightened;
    procedure TestSaveMergesConcurrentChanges;
    procedure TestRemoveAtOrUnderRespectsBoundaries;
    procedure TestDefaultPathPerPlatform;
    procedure TestGateStatesPerMode;
    procedure TestGateMemoizesAcrossThreads;
    procedure TestUntrustedReport;
    procedure TestFindTrustableConfigs;
    procedure TestAuditReasons;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

  TVerifyThread = class(TThread)
  private
    FGate: TGocciaConfigTrustGate;
    FPath: string;
    FStates: array of TGocciaConfigTrustState;
  protected
    procedure Execute; override;
  public
    constructor Create(const AGate: TGocciaConfigTrustGate;
      const APath: string);
  end;

var
  GEnvironment: TStringList;

function FakeEnvironment(const AName: string): string;
begin
  Result := GEnvironment.Values[AName];
end;

constructor TVerifyThread.Create(const AGate: TGocciaConfigTrustGate;
  const APath: string);
begin
  FGate := AGate;
  FPath := APath;
  inherited Create(False);
end;

procedure TVerifyThread.Execute;
var
  I: Integer;
begin
  SetLength(FStates, 50);
  for I := 0 to High(FStates) do
    FStates[I] := FGate.Verify(FPath).State;
end;

procedure TTrustTests.SetupTests;
begin
  Test('Save and Load round-trip without leaving a temporary file',
    TestRoundTripLeavesNoTemporaryFile);
  Test('Keys compare case-sensitively unless the platform folds case',
    TestKeysCaseSensitivity);
  Test('A config in a symlinked directory keys to its target',
    TestSymlinkedConfigKeysToTarget);
  Test('A symlinked config file has its own key and location',
    TestSymlinkedConfigFileHasItsOwnKey);
  Test('A config in a symlinked directory hashes at its target',
    TestSymlinkedDirectoryHashesAtItsTarget);
  Test('A re-pointed path scope needs trusting again',
    TestRetargetedScopesNeedTrust);
  Test('Re-pointing an absent scope''s parent needs trusting again',
    TestAbsentScopeParentRepointed);
  Test('Newer and corrupt stores are refused',
    TestNewerAndCorruptStoresRefused);
  Test('Stores of the wrong shape are refused', TestMalformedStoresRefused);
  Test('A held lock fails with a message naming the lock file',
    TestLockContention);
  Test('Timestamps ignore the locale''s separators',
    TestTimestampIgnoresLocale);
  Test('A stale lock is replaced; a live one is not', TestStaleLocksAreReplaced);
  Test('The default store''s directory is made private',
    TestPrivateDirectoryIsTightened);
  Test('Save applies its changes to the store as it is on disk',
    TestSaveMergesConcurrentChanges);
  Test('RemoveAtOrUnder stops at directory boundaries',
    TestRemoveAtOrUnderRespectsBoundaries);
  Test('DefaultPath per platform', TestDefaultPathPerPlatform);
  Test('Gate states per mode', TestGateStatesPerMode);
  Test('Gate verdicts are decided once across threads',
    TestGateMemoizesAcrossThreads);
  Test('The untrusted report', TestUntrustedReport);
  Test('--trust scanning picks each directory''s effective config',
    TestFindTrustableConfigs);
  Test('config.permissions audit decisions and reasons', TestAuditReasons);
end;

procedure TTrustTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FRoot := ExcludeTrailingPathDelimiter(ExpandFileName(
    IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-trust-' +
    IntToStr(GetProcessID) + '-' + IntToStr(Random(MaxInt))));
  ForceDirectories(FRoot);
  { Canonical, so keys built from it match the store's. }
  if CanonicalHostPath(FRoot) <> '' then
    FRoot := CanonicalHostPath(FRoot);
  EnsureConfigParsersRegistered;
end;

procedure DeleteDirectoryTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile or
    faSymLink, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if HostPathIsSymlink(EntryPath) then
        DeleteFile(EntryPath)
      else if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

procedure TTrustTests.AfterAll;
begin
  DeleteDirectoryTree(FRoot);
  inherited AfterAll;
end;

function TTrustTests.WriteFile(const ARelativePath, AText: string): string;
begin
  Result := FRoot + PathDelim + ARelativePath;
  ForceDirectories(ExtractFileDir(Result));
  WriteUTF8FileText(Result, AText);
end;

function TTrustTests.LoadConfig(const APath: string): TConfigEntryArray;
begin
  InterlockedIncrement(FLoadCount);
  Result := ParseConfigFile(APath);
end;

function TTrustTests.EntryFor(const AConfigPath: string): TGocciaTrustEntry;
var
  Request: TGocciaConfigPermissionRequest;
  Location: string;
begin
  { As --trust does: the request is read at the config's key location. }
  Location := TrustKeyForPath(AConfigPath);
  Request := ReadConfigPermissionRequest(ParseConfigFile(Location), Location);
  Result.ConfigPath := AConfigPath;
  Result.Hash := PermissionBlockHash(Request);
  Result.BlockJSON := NormalizedPermissionBlock(Request);
  Result.TrustedAt := '2026-09-20T10:12:03Z';
  Result.TrustedBy := 'GocciaTestRunner 0.14.0';
  Result.Targets := PathScopeTargets(Request);
end;

function TTrustTests.StoreError(const APath: string): string;
var
  Store: TGocciaTrustStore;
begin
  Result := '';
  try
    Store := TGocciaTrustStore.Load(APath);
    Store.Free;
  except
    on E: EGocciaTrustStoreError do
      Result := E.Message;
  end;
end;

procedure TTrustTests.TestRoundTripLeavesNoTemporaryFile;
var
  StorePath, ConfigPath: string;
  Store: TGocciaTrustStore;
  Entry, Found: TGocciaTrustEntry;
  SearchRec: TSearchRec;
  Names: string;
  {$IFDEF UNIX}
  Info: Stat;
  {$ENDIF}
begin
  StorePath := FRoot + PathDelim + 'roundtrip' + PathDelim + 'store' +
    PathDelim + 'trust.json';
  ConfigPath := WriteFile('roundtrip/project/goccia.json',
    '{"permissions": {"allow-net": ["a.test"], "allow-read": ["./data"]},' +
    ' "unsafe-shadowrealm": true}');
  Entry := EntryFor(ConfigPath);

  Store := TGocciaTrustStore.Load(StorePath);
  try
    Expect<Integer>(Store.Count).ToBe(0);
    Store.Put(Entry);
    Store.Save;
  finally
    Store.Free;
  end;

  Names := '';
  if FindFirst(ExtractFilePath(StorePath) + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        Names := Names + SearchRec.Name + ';';
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  Expect<string>(Names).ToBe('trust.json;');
  {$IFDEF UNIX}
  Expect<Integer>(FpStat(StorePath, Info)).ToBe(0);
  Expect<Integer>(Info.st_mode and &777).ToBe(&600);
  Expect<Integer>(FpStat(ExtractFileDir(StorePath), Info)).ToBe(0);
  Expect<Integer>(Info.st_mode and &777).ToBe(&700);
  {$ENDIF}

  Store := TGocciaTrustStore.Load(StorePath);
  try
    Expect<Integer>(Store.Count).ToBe(1);
    Expect<Boolean>(Store.TryFind(ConfigPath, Found)).ToBe(True);
    Expect<string>(Found.Hash).ToBe(Entry.Hash);
    Expect<string>(Found.BlockJSON).ToBe(Entry.BlockJSON);
    Expect<string>(Found.TrustedAt).ToBe(Entry.TrustedAt);
    Expect<string>(Found.TrustedBy).ToBe(Entry.TrustedBy);
  finally
    Store.Free;
  end;
end;

procedure TTrustTests.TestKeysCaseSensitivity;
var
  Sensitive, Insensitive: TGocciaTrustStore;
  Entry, Found: TGocciaTrustEntry;
  Lower: string;
begin
  { Paths that do not exist key to their expanded spelling. }
  Entry := Default(TGocciaTrustEntry);
  Entry.ConfigPath := FRoot + PathDelim + 'Case' + PathDelim + 'goccia.json';
  Entry.Hash := 'abc';
  Lower := FRoot + PathDelim + 'case' + PathDelim + 'goccia.json';

  Sensitive := TGocciaTrustStore.Create(FRoot + PathDelim + 's.json', False);
  Insensitive := TGocciaTrustStore.Create(FRoot + PathDelim + 'i.json', True);
  try
    Sensitive.Put(Entry);
    Insensitive.Put(Entry);
    Expect<Boolean>(Sensitive.TryFind(Entry.ConfigPath, Found)).ToBe(True);
    Expect<Boolean>(Sensitive.TryFind(Lower, Found)).ToBe(False);
    Expect<Boolean>(Insensitive.TryFind(Lower, Found)).ToBe(True);
  finally
    Sensitive.Free;
    Insensitive.Free;
  end;
  Expect<Boolean>(TRUST_KEYS_CASE_INSENSITIVE).ToBe(
    {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}True{$ELSE}False{$IFEND});
end;

procedure TTrustTests.TestSymlinkedConfigKeysToTarget;
{$IFDEF UNIX}
var
  Target, Link: string;
  Store: TGocciaTrustStore;
  Entry, Found: TGocciaTrustEntry;
begin
  Target := WriteFile('symlink/real/goccia.json',
    '{"permissions": {"allow-net": ["a.test"]}}');
  Link := FRoot + PathDelim + 'symlink' + PathDelim + 'linked';
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(FRoot + PathDelim + 'symlink' +
    PathDelim + 'real')), PAnsiChar(AnsiString(Link)))).ToBe(0);
  Entry := EntryFor(Link + PathDelim + 'goccia.json');
  Store := TGocciaTrustStore.Create(FRoot + PathDelim + 'sym.json', False);
  try
    Store.Put(Entry);
    Expect<Boolean>(Store.TryFind(Target, Found)).ToBe(True);
    Expect<string>(Found.ConfigPath).ToBe(Target);
    Expect<string>(TrustKeyForPath(Link + PathDelim + 'goccia.json'))
      .ToBe(Target);
  finally
    Store.Free;
  end;
end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
end;
{$ENDIF}

procedure TTrustTests.TestSymlinkedConfigFileHasItsOwnKey;
{$IFDEF UNIX}
var
  Trusted, Evil, StorePath: string;
  Store: TGocciaTrustStore;
  Gate: TGocciaConfigTrustGate;
begin
  { evil/goccia.json -> ../trusted/goccia.json: trusting trusted/ must not
    trust evil/, whose files the same text would govern. }
  Trusted := WriteFile('symfile/trusted/goccia.json',
    '{"unsafe-function-constructor": true, ' +
    '"permissions": {"allow-read": ["./data"]}}');
  ForceDirectories(FRoot + PathDelim + 'symfile' + PathDelim + 'evil');
  Evil := FRoot + PathDelim + 'symfile' + PathDelim + 'evil' + PathDelim +
    'goccia.json';
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString('../trusted/goccia.json')),
    PAnsiChar(AnsiString(Evil)))).ToBe(0);
  Expect<string>(TrustKeyForPath(Evil)).ToBe(Evil);
  Expect<string>(TrustKeyForPath(Trusted)).ToBe(Trusted);

  StorePath := FRoot + PathDelim + 'symfile' + PathDelim + 'trust.json';
  Store := TGocciaTrustStore.Load(StorePath);
  try
    Store.Put(EntryFor(Trusted));
    Store.Save;
  finally
    Store.Free;
  end;
  Gate := TGocciaConfigTrustGate.Create(StorePath, '', ctmStore,
    ALL_CAPABILITIES, True, LoadConfig);
  try
    Expect<Boolean>(Gate.Verify(Trusted).State = ctsTrusted).ToBe(True);
    Expect<Boolean>(Gate.Verify(Evil).State = ctsNotTrusted).ToBe(True);
    { The request is read at the config's own location: ./data is under
      evil/, not trusted/. }
    Expect<string>(Gate.Verify(Evil).Request.Allow[gcRead].Scopes[0]).ToBe(
      FRoot + PathDelim + 'symfile' + PathDelim + 'evil' + PathDelim +
      'data');
  finally
    Gate.Free;
  end;
end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
end;
{$ENDIF}

procedure TTrustTests.TestSymlinkedDirectoryHashesAtItsTarget;
{$IFDEF UNIX}
var
  Real, Link, StorePath: string;
  Store: TGocciaTrustStore;
  Gate: TGocciaConfigTrustGate;
begin
  { A config reached through a symlinked directory governs the same files,
    so it keys, and resolves its relative scopes, at the target: one key,
    one hash, whichever spelling is used. }
  Real := WriteFile('symdir/real/goccia.json',
    '{"permissions": {"allow-read": ["./data"]}}');
  Link := FRoot + PathDelim + 'symdir' + PathDelim + 'link';
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(ExtractFileDir(Real))),
    PAnsiChar(AnsiString(Link)))).ToBe(0);
  StorePath := FRoot + PathDelim + 'symdir' + PathDelim + 'trust.json';
  Gate := TGocciaConfigTrustGate.Create(StorePath, '', ctmStore,
    ALL_CAPABILITIES, True, LoadConfig);
  try
    Expect<string>(Gate.Verify(Link + PathDelim + 'goccia.json').Hash)
      .ToBe(Gate.Verify(Real).Hash);
    Expect<string>(Gate.Verify(Link + PathDelim + 'goccia.json').ConfigPath)
      .ToBe(Real);
  finally
    Gate.Free;
  end;
  Store := TGocciaTrustStore.Load(StorePath);
  try
    Store.Put(EntryFor(Link + PathDelim + 'goccia.json'));
    Store.Save;
  finally
    Store.Free;
  end;
  Gate := TGocciaConfigTrustGate.Create(StorePath, '', ctmStore,
    ALL_CAPABILITIES, True, LoadConfig);
  try
    Expect<Boolean>(Gate.Verify(Real).State = ctsTrusted).ToBe(True);
  finally
    Gate.Free;
  end;
end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
end;
{$ENDIF}

procedure TTrustTests.TestRetargetedScopesNeedTrust;
{$IFDEF UNIX}
var
  Base, ConfigPath, StorePath, Elsewhere: string;
  Store: TGocciaTrustStore;
  Verdict: TGocciaConfigTrustVerdict;
  Reloaded: TGocciaTrustStore;
  Found: TGocciaTrustEntry;

  function VerifyNow: TGocciaConfigTrustVerdict;
  var
    Gate: TGocciaConfigTrustGate;
  begin
    Gate := TGocciaConfigTrustGate.Create(StorePath, '', ctmStore,
      ALL_CAPABILITIES, True, LoadConfig);
    try
      Result := Gate.Verify(ConfigPath);
    finally
      Gate.Free;
    end;
  end;

begin
  Base := FRoot + PathDelim + 'retarget';
  ConfigPath := WriteFile('retarget/project/goccia.json',
    '{"permissions": {"allow-read": ["./data", "./build"]}}');
  WriteFile('retarget/project/data/x.txt', 'x');
  Elsewhere := Base + PathDelim + 'elsewhere';
  ForceDirectories(Elsewhere);
  StorePath := Base + PathDelim + 'trust.json';
  Store := TGocciaTrustStore.Load(StorePath);
  try
    Store.Put(EntryFor(ConfigPath));
    Store.Save;
  finally
    Store.Free;
  end;
  { The targets round-trip through the store, outside the hash. }
  Reloaded := TGocciaTrustStore.Load(StorePath);
  try
    Expect<Boolean>(Reloaded.TryFind(ConfigPath, Found)).ToBe(True);
    Expect<Integer>(Length(Found.Targets)).ToBe(2);
    { A scope that does not exist records where it would be. }
    Expect<string>(Found.Targets[0].Target).ToBe(Base + PathDelim +
      'project' + PathDelim + 'build');
    Expect<string>(Found.Targets[1].Target).ToBe(Base + PathDelim +
      'project' + PathDelim + 'data');
  finally
    Reloaded.Free;
  end;
  Expect<Boolean>(VerifyNow.State = ctsTrusted).ToBe(True);

  { A build output appearing in place of an absent scope is expected. }
  ForceDirectories(Base + PathDelim + 'project' + PathDelim + 'build');
  Expect<Boolean>(VerifyNow.State = ctsTrusted).ToBe(True);
  RemoveDir(Base + PathDelim + 'project' + PathDelim + 'build');

  { data/ replaced by a link elsewhere: same block, different place. }
  DeleteFile(Base + PathDelim + 'project' + PathDelim + 'data' + PathDelim +
    'x.txt');
  RemoveDir(Base + PathDelim + 'project' + PathDelim + 'data');
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Elsewhere)),
    PAnsiChar(AnsiString(Base + PathDelim + 'project' + PathDelim + 'data'))))
    .ToBe(0);
  Verdict := VerifyNow;
  Expect<Boolean>(Verdict.State = ctsChanged).ToBe(True);
  Expect<Integer>(Length(Verdict.TargetChanges)).ToBe(1);
  Expect<string>(Verdict.TargetChanges[0]).ToBe('target of ' + Base +
    PathDelim + 'project' + PathDelim + 'data: ' + Base + PathDelim +
    'project' + PathDelim + 'data -> ' + Elsewhere);

  { An absent scope appearing as a link out of its own place is a change. }
  DeleteFile(Base + PathDelim + 'project' + PathDelim + 'data');
  ForceDirectories(Base + PathDelim + 'project' + PathDelim + 'data');
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Elsewhere)),
    PAnsiChar(AnsiString(Base + PathDelim + 'project' + PathDelim +
    'build')))).ToBe(0);
  Verdict := VerifyNow;
  Expect<Boolean>(Verdict.State = ctsChanged).ToBe(True);
  Expect<string>(Verdict.TargetChanges[0]).ToBe('target of ' + Base +
    PathDelim + 'project' + PathDelim + 'build: ' + Base + PathDelim +
    'project' + PathDelim + 'build -> ' + Elsewhere);
end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
end;
{$ENDIF}

procedure TTrustTests.TestAbsentScopeParentRepointed;
{$IFDEF UNIX}
var
  Base, ConfigPath, StorePath, Elsewhere: string;

  function StateNow: TGocciaConfigTrustState;
  var
    Gate: TGocciaConfigTrustGate;
  begin
    Gate := TGocciaConfigTrustGate.Create(StorePath, '', ctmStore,
      ALL_CAPABILITIES, True, LoadConfig);
    try
      Result := Gate.Verify(ConfigPath).State;
    finally
      Gate.Free;
    end;
  end;

  procedure TrustNow;
  var
    Store: TGocciaTrustStore;
  begin
    DeleteFile(StorePath);
    Store := TGocciaTrustStore.Load(StorePath);
    try
      Store.Put(EntryFor(ConfigPath));
      Store.Save;
    finally
      Store.Free;
    end;
  end;

begin
  { allow-read ./cfg/ssh with no cfg/: re-pointing the parent re-points the
    scope, though the scope itself never existed. }
  Base := FRoot + PathDelim + 'absent-parent';
  ConfigPath := WriteFile('absent-parent/dd/goccia.json',
    '{"permissions": {"allow-read": ["./cfg/ssh"]}}');
  Elsewhere := Base + PathDelim + 'etc';
  ForceDirectories(Elsewhere + PathDelim + 'ssh');
  StorePath := Base + PathDelim + 'trust.json';
  TrustNow;
  Expect<Boolean>(StateNow = ctsTrusted).ToBe(True);
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Elsewhere)),
    PAnsiChar(AnsiString(Base + PathDelim + 'dd' + PathDelim + 'cfg'))))
    .ToBe(0);
  Expect<Boolean>(StateNow = ctsChanged).ToBe(True);

  { cfg/ a real directory when trusted, swapped for the link later. }
  DeleteFile(Base + PathDelim + 'dd' + PathDelim + 'cfg');
  ForceDirectories(Base + PathDelim + 'dd' + PathDelim + 'cfg');
  TrustNow;
  Expect<Boolean>(StateNow = ctsTrusted).ToBe(True);
  { The scope appearing in place, as a build would make it, is no change. }
  ForceDirectories(Base + PathDelim + 'dd' + PathDelim + 'cfg' + PathDelim +
    'ssh');
  Expect<Boolean>(StateNow = ctsTrusted).ToBe(True);
  RemoveDir(Base + PathDelim + 'dd' + PathDelim + 'cfg' + PathDelim + 'ssh');
  RemoveDir(Base + PathDelim + 'dd' + PathDelim + 'cfg');
  Expect<Integer>(fpSymlink(PAnsiChar(AnsiString(Elsewhere)),
    PAnsiChar(AnsiString(Base + PathDelim + 'dd' + PathDelim + 'cfg'))))
    .ToBe(0);
  Expect<Boolean>(StateNow = ctsChanged).ToBe(True);
end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
end;
{$ENDIF}

procedure TTrustTests.TestNewerAndCorruptStoresRefused;
var
  Newer, Corrupt, Unversioned: string;
begin
  Newer := WriteFile('refused/newer.json',
    '{"version": 2, "trusted": {}, "future": [1, 2]}');
  Expect<string>(StoreError(Newer)).ToBe('trust store ' + Newer +
    ' was written by a newer GocciaScript (version 2); upgrade ' +
    'GocciaScript or remove the file');
  Corrupt := WriteFile('refused/corrupt.json', '{"version": 1, "trusted": ');
  Expect<string>(StoreError(Corrupt)).ToBe('trust store ' + Corrupt +
    ' is not valid JSON; fix or delete it');
  Unversioned := WriteFile('refused/unversioned.json', '{"trusted": {}}');
  Expect<string>(StoreError(Unversioned)).ToBe('trust store ' + Unversioned +
    ' is not a valid trust store (no "version"); fix or delete it');
end;

procedure TTrustTests.TestMalformedStoresRefused;
const
  VALID_HASH = '"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"';
  VALID_ENTRY = '{"sha256": ' + VALID_HASH + ', "block": {"version": 1}, ' +
    '"trustedAt": "2026-09-20T10:12:03Z", "trustedBy": "GocciaTestRunner"}';

  function Detail(const AName, AText: string): string;
  var
    Path, Message: string;
  begin
    Path := WriteFile('malformed/' + AName + '.json', AText);
    Message := StoreError(Path);
    Result := Copy(Message, Pos('(', Message) + 1, MaxInt);
    Result := Copy(Result, 1, Pos('); fix or delete it', Result) - 1);
    if Pos('is not a valid trust store', Message) = 0 then
      Result := 'accepted: ' + Message;
  end;

begin
  Expect<string>(Detail('array', '[]')).ToBe('the top level is not an object');
  Expect<string>(Detail('no-trusted', '{"version": 1}')).ToBe('no "trusted"');
  Expect<string>(Detail('trusted-array', '{"version": 1, "trusted": []}'))
    .ToBe('"trusted" is not an object');
  Expect<string>(Detail('version-string', '{"version": "1", "trusted": {}}'))
    .ToBe('"version" is not an integer');
  Expect<string>(Detail('unknown-key',
    '{"version": 1, "trusted": {}, "extra": true}'))
    .ToBe('unknown key "extra"');
  Expect<string>(Detail('hash-number',
    '{"version": 1, "trusted": {"/a/goccia.json": {"sha256": 5, ' +
    '"block": {"version": 1}, "trustedAt": "t", "trustedBy": "b"}}}'))
    .ToBe('"sha256" of /a/goccia.json is not a string');
  Expect<string>(Detail('hash-short',
    '{"version": 1, "trusted": {"/a/goccia.json": {"sha256": "abc", ' +
    '"block": {"version": 1}, "trustedAt": "t", "trustedBy": "b"}}}'))
    .ToBe('"sha256" of /a/goccia.json is not a SHA-256');
  Expect<string>(Detail('missing-block',
    '{"version": 1, "trusted": {"/a/goccia.json": {"sha256": ' + VALID_HASH +
    ', "trustedAt": "t", "trustedBy": "b"}}}'))
    .ToBe('/a/goccia.json has no "block"');
  Expect<string>(Detail('entry-array',
    '{"version": 1, "trusted": {"/a/goccia.json": []}}'))
    .ToBe('the entry for /a/goccia.json is not an object');
  Expect<string>(Detail('valid',
    '{"version": 1, "trusted": {"/a/goccia.json": ' + VALID_ENTRY + '}}'))
    .ToBe('accepted: ');
end;

procedure TTrustTests.TestLockContention;
var
  StorePath, Message: string;
  Store: TGocciaTrustStore;
  Entry: TGocciaTrustEntry;
begin
  StorePath := WriteFile('locked/trust.json', '{"version": 1, "trusted": {}}');
  WriteFile('locked/trust.json.lock', '');
  Entry := Default(TGocciaTrustEntry);
  Entry.ConfigPath := FRoot + PathDelim + 'locked' + PathDelim + 'goccia.json';
  Entry.Hash := 'abc';
  Message := '';
  Store := TGocciaTrustStore.Load(StorePath);
  try
    Store.Put(Entry);
    try
      Store.Save;
    except
      on E: EGocciaTrustStoreError do
        Message := E.Message;
    end;
  finally
    Store.Free;
  end;
  Expect<string>(Message).ToBe('trust store ' + StorePath + ' is locked by ' +
    'another process (' + StorePath + '.lock exists); retry, or delete ' +
    StorePath + '.lock if no GocciaScript process is updating the store');
  { The store is untouched and the other writer's lock is left alone. }
  Expect<string>(ReadUTF8FileText(StorePath))
    .ToBe('{"version": 1, "trusted": {}}');
  Expect<Boolean>(FileExists(StorePath + '.lock')).ToBe(True);
end;

function TTrustTests.SaveWithLock(const AName, ALockContent: string): string;
var
  StorePath: string;
  Store: TGocciaTrustStore;
  Entry: TGocciaTrustEntry;
begin
  Result := '';
  StorePath := FRoot + PathDelim + 'stale-' + AName + PathDelim +
    'trust.json';
  ForceDirectories(ExtractFileDir(StorePath));
  WriteUTF8FileText(StorePath + '.lock', ALockContent);
  Entry := Default(TGocciaTrustEntry);
  Entry.ConfigPath := ExtractFileDir(StorePath) + PathDelim + 'goccia.json';
  Entry.Hash := 'abc';
  Store := TGocciaTrustStore.Load(StorePath);
  try
    Store.Put(Entry);
    try
      Store.Save;
      Result := 'saved';
    except
      on E: EGocciaTrustStoreError do
        Result := E.Message;
    end;
  finally
    Store.Free;
  end;
end;

function UnixSecondsAgo(const ASeconds: Integer): string;
begin
  Result := IntToStr(DateTimeToUnix(LocalTimeToUniversal(Now)) - ASeconds);
end;

procedure TTrustTests.TestTimestampIgnoresLocale;
var
  Previous: TFormatSettings;
  Stamp: string;
begin
  Previous := DefaultFormatSettings;
  try
    DefaultFormatSettings.TimeSeparator := '.';
    DefaultFormatSettings.DateSeparator := '/';
    Stamp := TrustTimestamp;
  finally
    DefaultFormatSettings := Previous;
  end;
  { yyyy-mm-ddThh:nn:ssZ whatever the locale's separators. }
  Expect<Integer>(Length(Stamp)).ToBe(20);
  Expect<string>(Stamp[5] + Stamp[8] + Stamp[11] + Stamp[14] + Stamp[17] +
    Stamp[20]).ToBe('--T::Z');
end;

procedure TTrustTests.TestStaleLocksAreReplaced;
const
  VANISHED_PROCESS = '999999999';
begin
  { A crashed writer's lock: its process is gone. }
  Expect<string>(SaveWithLock('gone', VANISHED_PROCESS + ' ' +
    UnixSecondsAgo(0))).ToBe('saved');
  Expect<Boolean>(FileExists(FRoot + PathDelim + 'stale-gone' + PathDelim +
    'trust.json.lock')).ToBe(False);
  { A live process, but held for far longer than any write takes. }
  Expect<string>(SaveWithLock('old', IntToStr(GetProcessID) + ' ' +
    UnixSecondsAgo(3600))).ToBe('saved');
  { A live, recent owner still excludes the writer. }
  Expect<Boolean>(Pos('is locked by another process', SaveWithLock('live',
    IntToStr(GetProcessID) + ' ' + UnixSecondsAgo(0))) > 0).ToBe(True);
end;

procedure TTrustTests.TestPrivateDirectoryIsTightened;
{$IFDEF UNIX}
const
  OPEN_DIRECTORY = &755;
  PRIVATE_DIRECTORY = &700;
  PERMISSION_BITS = &777;
var
  Directory: string;
  Store: TGocciaTrustStore;
  Info: Stat;
begin
  Directory := FRoot + PathDelim + 'tighten';
  ForceDirectories(Directory);
  fpChmod(Directory, OPEN_DIRECTORY);
  Store := TGocciaTrustStore.Load(Directory + PathDelim + 'trust.json');
  try
    Expect<Boolean>(Store.PrivateDirectory).ToBe(False);
    Store.Save;
    Expect<Integer>(FpStat(Directory, Info)).ToBe(0);
    Expect<Integer>(Info.st_mode and PERMISSION_BITS).ToBe(OPEN_DIRECTORY);
    Store.PrivateDirectory := True;
    Store.Save;
    Expect<Integer>(FpStat(Directory, Info)).ToBe(0);
    Expect<Integer>(Info.st_mode and PERMISSION_BITS).ToBe(PRIVATE_DIRECTORY);
  finally
    Store.Free;
  end;
end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
end;
{$ENDIF}

procedure TTrustTests.TestSaveMergesConcurrentChanges;
var
  StorePath: string;
  First, Second, Check: TGocciaTrustStore;
  A, B: TGocciaTrustEntry;
begin
  StorePath := FRoot + PathDelim + 'merge' + PathDelim + 'trust.json';
  A := Default(TGocciaTrustEntry);
  A.ConfigPath := FRoot + PathDelim + 'merge' + PathDelim + 'a.json';
  A.Hash := StringOfChar('a', 64);
  B := A;
  B.ConfigPath := FRoot + PathDelim + 'merge' + PathDelim + 'b.json';
  B.Hash := StringOfChar('b', 64);
  First := TGocciaTrustStore.Load(StorePath);
  Second := TGocciaTrustStore.Load(StorePath);
  try
    First.Put(A);
    Second.Put(B);
    First.Save;
    Second.Save;
  finally
    First.Free;
    Second.Free;
  end;
  Check := TGocciaTrustStore.Load(StorePath);
  try
    Expect<Integer>(Check.Count).ToBe(2);
  finally
    Check.Free;
  end;
end;

procedure TTrustTests.TestRemoveAtOrUnderRespectsBoundaries;
var
  Store: TGocciaTrustStore;
  Entry: TGocciaTrustEntry;
  Removed: TStringList;
  Base: string;

  procedure PutAt(const ARelative: string);
  begin
    Entry.ConfigPath := Base + PathDelim + ARelative;
    Store.Put(Entry);
  end;

begin
  Base := FRoot + PathDelim + 'remove';
  Entry := Default(TGocciaTrustEntry);
  Entry.Hash := 'abc';
  Store := TGocciaTrustStore.Create(FRoot + PathDelim + 'remove.json', False);
  Removed := TStringList.Create;
  try
    PutAt('a' + PathDelim + 'b' + PathDelim + 'goccia.json');
    PutAt('a' + PathDelim + 'bc' + PathDelim + 'goccia.json');
    PutAt('a' + PathDelim + 'goccia.json');
    Expect<Integer>(Store.RemoveAtOrUnder(Base + PathDelim + 'a' + PathDelim +
      'b', Removed)).ToBe(1);
    Expect<string>(Removed[0]).ToBe(Base + PathDelim + 'a' + PathDelim + 'b' +
      PathDelim + 'goccia.json');
    Expect<Integer>(Store.RemoveAtOrUnder(Base + PathDelim + 'a' + PathDelim +
      'goccia.json', nil)).ToBe(1);
    Expect<Integer>(Store.RemoveAtOrUnder(Base + PathDelim + 'a' + PathDelim +
      'b', nil)).ToBe(0);
    Expect<Integer>(Store.RemoveAtOrUnder(Base, nil)).ToBe(1);
    Expect<Integer>(Store.Count).ToBe(0);
  finally
    Removed.Free;
    Store.Free;
  end;
end;

procedure TTrustTests.TestDefaultPathPerPlatform;
begin
  GEnvironment.Clear;
  GEnvironment.Values['HOME'] := '/home/u';
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspUnix, @FakeEnvironment))
    .ToBe('/home/u/.config/goccia/trust.json');
  GEnvironment.Values['XDG_CONFIG_HOME'] := '/xdg/';
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspUnix, @FakeEnvironment))
    .ToBe('/xdg/goccia/trust.json');
  GEnvironment.Values['XDG_CONFIG_HOME'] := 'relative';
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspUnix, @FakeEnvironment))
    .ToBe('/home/u/.config/goccia/trust.json');
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspDarwin,
    @FakeEnvironment)).ToBe(
    '/home/u/Library/Application Support/Goccia/trust.json');
  GEnvironment.Values['APPDATA'] := 'C:\Users\u\AppData\Roaming\';
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspWindows,
    @FakeEnvironment)).ToBe('C:\Users\u\AppData\Roaming\Goccia\trust.json');
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspNone,
    @FakeEnvironment)).ToBe('');

  GEnvironment.Clear;
  Expect<string>(TGocciaTrustStore.DefaultPathFor(gtspUnix, @FakeEnvironment))
    .ToBe('');
  Expect<string>(TGocciaTrustStore.DefaultPathProblem(gtspUnix,
    @FakeEnvironment)).ToBe('HOME is not set');
  Expect<string>(TGocciaTrustStore.DefaultPathProblem(gtspWindows,
    @FakeEnvironment)).ToBe('APPDATA is not set');
  Expect<string>(TGocciaTrustStore.DefaultPathProblem(gtspNone,
    @FakeEnvironment)).ToBe('this build has no per-user trust store');
end;

procedure TTrustTests.TestGateStatesPerMode;
var
  ConfigPath, DenyOnly, StorePath: string;
  Store: TGocciaTrustStore;
  Entry: TGocciaTrustEntry;

  function StateIn(const AMode: TGocciaConfigTrustMode;
    const AHonored: TGocciaHonoredCapabilities;
    const APath: string): TGocciaConfigTrustState;
  var
    Gate: TGocciaConfigTrustGate;
  begin
    Gate := TGocciaConfigTrustGate.Create(StorePath, '', AMode, AHonored,
      True, LoadConfig);
    try
      Result := Gate.Verify(APath).State;
    finally
      Gate.Free;
    end;
  end;

begin
  ConfigPath := WriteFile('gate/goccia.json',
    '{"permissions": {"allow-net": ["a.test"], "deny-read": true}}');
  DenyOnly := WriteFile('gate/deny/goccia.json',
    '{"permissions": {"deny-net": true}}');
  StorePath := FRoot + PathDelim + 'gate' + PathDelim + 'trust.json';

  Expect<Boolean>(StateIn(ctmStore, ALL_CAPABILITIES, ConfigPath) =
    ctsNotTrusted).ToBe(True);
  Expect<Boolean>(StateIn(ctmStore, ALL_CAPABILITIES, DenyOnly) =
    ctsNoRequest).ToBe(True);
  Expect<Boolean>(StateIn(ctmStore, [gcRead], ConfigPath) =
    ctsNotHonored).ToBe(True);
  Expect<Boolean>(StateIn(ctmAcceptForRun, ALL_CAPABILITIES, ConfigPath) =
    ctsAcceptedForRun).ToBe(True);
  Expect<Boolean>(StateIn(ctmIgnoreConfig, ALL_CAPABILITIES, ConfigPath) =
    ctsIgnored).ToBe(True);
  { -P never reads the store: a corrupt one does not matter. }
  WriteFile('gate/trust.json', 'not json');
  Expect<Boolean>(StateIn(ctmAcceptForRun, ALL_CAPABILITIES, ConfigPath) =
    ctsAcceptedForRun).ToBe(True);
  Expect<Boolean>(StateIn(ctmStore, ALL_CAPABILITIES, ConfigPath) =
    ctsNotTrusted).ToBe(True);
  DeleteFile(StorePath);

  Entry := EntryFor(ConfigPath);
  Store := TGocciaTrustStore.Load(StorePath);
  try
    Store.Put(Entry);
    Store.Save;
  finally
    Store.Free;
  end;
  Expect<Boolean>(StateIn(ctmStore, ALL_CAPABILITIES, ConfigPath) =
    ctsTrusted).ToBe(True);

  WriteFile('gate/goccia.json',
    '{"permissions": {"allow-net": ["a.test", "b.test"], "deny-read": true}}');
  Expect<Boolean>(StateIn(ctmStore, ALL_CAPABILITIES, ConfigPath) =
    ctsChanged).ToBe(True);

  { The same block at another path is not trusted. }
  Expect<Boolean>(StateIn(ctmStore, ALL_CAPABILITIES, WriteFile(
    'gate/copy/goccia.json', '{"permissions": {"allow-net": ["a.test"], ' +
    '"deny-read": true}}')) = ctsNotTrusted).ToBe(True);
end;

procedure TTrustTests.TestGateMemoizesAcrossThreads;
var
  Gate: TGocciaConfigTrustGate;
  First, Second: TVerifyThread;
  ConfigPath: string;
  I: Integer;
begin
  ConfigPath := WriteFile('threads/goccia.json',
    '{"permissions": {"allow-net": ["a.test"]}}');
  FLoadCount := 0;
  Gate := TGocciaConfigTrustGate.Create('', '', ctmAcceptForRun,
    ALL_CAPABILITIES, True, LoadConfig);
  try
    First := TVerifyThread.Create(Gate, ConfigPath);
    Second := TVerifyThread.Create(Gate, ConfigPath);
    First.WaitFor;
    Second.WaitFor;
    for I := 0 to High(First.FStates) do
    begin
      Expect<Boolean>(First.FStates[I] = ctsAcceptedForRun).ToBe(True);
      Expect<Boolean>(Second.FStates[I] = ctsAcceptedForRun).ToBe(True);
    end;
    First.Free;
    Second.Free;
    Expect<Integer>(FLoadCount).ToBe(1);
  finally
    Gate.Free;
  end;
end;

procedure TTrustTests.TestUntrustedReport;
var
  Fetch, FFI: TGocciaConfigTrustVerdict;
  Report: string;
  Many: array of TGocciaConfigTrustVerdict;
  I: Integer;
begin
  Fetch := Default(TGocciaConfigTrustVerdict);
  Fetch.ConfigPath := FRoot + PathDelim + 'report' + PathDelim + 'fetch' +
    PathDelim + 'goccia.json';
  Fetch.State := ctsNotTrusted;
  Fetch.Request.Allow[gcNet].Declared := True;
  Fetch.Request.Allow[gcNet].Scopes := ['127.0.0.1', 'example.com'];

  FFI := Default(TGocciaConfigTrustVerdict);
  FFI.ConfigPath := FRoot + PathDelim + 'report' + PathDelim + 'ffi' +
    PathDelim + 'goccia.json';
  FFI.State := ctsChanged;
  FFI.Previous.TrustedAt := '2026-09-20T10:12:03Z';
  FFI.Previous.BlockJSON :=
    '{"permissions":{"allow-ffi":["/lib/ffi"]},"version":1}';
  FFI.Request.Allow[gcFFI].Declared := True;
  FFI.Request.Allow[gcFFI].Scopes := ['/lib/ffi'];
  FFI.Request.Allow[gcRead].Declared := True;
  FFI.Request.Allow[gcRead].Scopes := ['/lib/modules'];

  Report := FormatUntrustedReport([Fetch, FFI], 'GocciaTestRunner',
    '/home/u/.config/goccia/trust.json', '', '', ['report', '--mode=bytecode'],
    FRoot);
  Expect<string>(Report).ToBe(
    '2 config files request permissions that have not been trusted:' +
    sLineBreak + sLineBreak +
    '  report' + PathDelim + 'fetch' + PathDelim + 'goccia.json ' +
    '(never trusted)' + sLineBreak +
    '    allow-net: 127.0.0.1, example.com' + sLineBreak + sLineBreak +
    '  report' + PathDelim + 'ffi' + PathDelim + 'goccia.json ' +
    '(changed since trusted 2026-09-20T10:12:03Z)' + sLineBreak +
    '    allow-ffi: /lib/ffi' + sLineBreak +
    '  + allow-read: /lib/modules' + sLineBreak + sLineBreak +
    'Nothing was run. To trust these requests (stored in ' +
    '/home/u/.config/goccia/trust.json):' + sLineBreak +
    '  GocciaTestRunner --trust report' + PathDelim + 'fetch' + PathDelim +
    'goccia.json --trust report' + PathDelim + 'ffi' + PathDelim +
    'goccia.json' + sLineBreak +
    'To accept them for this run only:' + sLineBreak +
    '  GocciaTestRunner -P report --mode=bytecode' + sLineBreak +
    'To run with command-line grants only:' + sLineBreak +
    '  GocciaTestRunner --ignore-config-permissions report --mode=bytecode');

  { More than three configs are trusted through their common directory. }
  SetLength(Many, 4);
  for I := 0 to High(Many) do
  begin
    Many[I] := Fetch;
    Many[I].ConfigPath := FRoot + PathDelim + 'report' + PathDelim +
      IntToStr(I) + PathDelim + 'goccia.json';
  end;
  Report := FormatUntrustedReport(Many, 'GocciaTestRunner', '', 'HOME is ' +
    'not set', '', ['report'], FRoot);
  Expect<Boolean>(Pos('4 config files request', Report) = 1).ToBe(True);
  Expect<Boolean>(Pos('The per-user trust store cannot be located (HOME is ' +
    'not set); to trust these requests, name a store:' + sLineBreak +
    '  GocciaTestRunner --trust-store=<path> --trust report' + PathDelim,
    Report) > 0).ToBe(True);

  { An unreadable store is named, and --trust-store is echoed. }
  Report := FormatUntrustedReport([Fetch], 'GocciaTestRunner', '/s.json',
    'trust store /s.json is not valid JSON; fix or delete it', '/s.json',
    ['a b'], FRoot);
  Expect<Boolean>(Pos('1 config file requests', Report) = 1).ToBe(True);
  Expect<Boolean>(Pos('Nothing was run. Trust store /s.json is not valid ' +
    'JSON; fix or delete it. Then trust these requests:' + sLineBreak +
    '  GocciaTestRunner --trust-store=/s.json --trust', Report) > 0)
    .ToBe(True);
  {$IFNDEF MSWINDOWS}
  Expect<Boolean>(Pos('GocciaTestRunner -P ''a b''', Report) > 0).ToBe(True);
  {$ENDIF}
end;

procedure TTrustTests.TestFindTrustableConfigs;
var
  Configs: TStringList;
  Base: string;
begin
  Base := FRoot + PathDelim + 'scan';
  WriteFile('scan/goccia.json', '{}');
  WriteFile('scan/goccia.toml', '');
  WriteFile('scan/a/goccia.json5', '{}');
  WriteFile('scan/a/goccia.json', '{}');
  WriteFile('scan/Z/goccia.json', '{}');
  WriteFile('scan/b/other.json', '{}');
  WriteFile('scan/node_modules/p/goccia.json', '{}');
  WriteFile('scan/.git/goccia.json', '{}');
  Configs := TStringList.Create;
  try
    FindTrustableConfigs(Base, Configs);
    { Byte order: Z (0x5A) before a (0x61), whatever the locale. }
    Expect<Integer>(Configs.Count).ToBe(3);
    Expect<string>(Configs[0]).ToBe(Base + PathDelim + 'goccia.toml');
    Expect<string>(Configs[1]).ToBe(Base + PathDelim + 'Z' + PathDelim +
      'goccia.json');
    Expect<string>(Configs[2]).ToBe(Base + PathDelim + 'a' + PathDelim +
      'goccia.json5');
    Configs.Clear;
    FindTrustableConfigs(Base + PathDelim + 'b' + PathDelim + 'other.json',
      Configs);
    Expect<Integer>(Configs.Count).ToBe(1);
  finally
    Configs.Free;
  end;
end;

procedure TTrustTests.TestAuditReasons;
var
  Verdict: TGocciaConfigTrustVerdict;
begin
  Verdict := Default(TGocciaConfigTrustVerdict);
  Verdict.Hash := 'abc';
  Verdict.State := ctsTrusted;
  Expect<string>(ConfigTrustAuditReason(Verdict, 'P')).ToBe(
    'trusted sha256:abc');
  Expect<Boolean>(ConfigTrustAuditAllows(Verdict)).ToBe(True);
  Verdict.State := ctsAcceptedForRun;
  Expect<string>(ConfigTrustAuditReason(Verdict, 'P')).ToBe(
    'accepted for this run (-P)');
  Verdict.State := ctsNotHonored;
  Expect<string>(ConfigTrustAuditReason(Verdict, 'GocciaBundler')).ToBe(
    'not needed: GocciaBundler honors none of these requests');
  Expect<Boolean>(ConfigTrustAuditAllows(Verdict)).ToBe(True);
  Verdict.State := ctsNotTrusted;
  Expect<string>(ConfigTrustAuditReason(Verdict, 'P')).ToBe('not trusted');
  Expect<Boolean>(ConfigTrustAuditAllows(Verdict)).ToBe(False);
  Verdict.State := ctsChanged;
  Expect<string>(ConfigTrustAuditReason(Verdict, 'P')).ToBe(
    'changed since trusted');
  Verdict.State := ctsIgnored;
  Expect<string>(ConfigTrustAuditReason(Verdict, 'P')).ToBe(
    'ignored (--ignore-config-permissions)');
  Expect<Boolean>(ConfigTrustAuditAllows(Verdict)).ToBe(False);
  Expect<Boolean>(Verdict.GrantsAccepted).ToBe(False);
end;

begin
  GEnvironment := TStringList.Create;
  try
    TestRunnerProgram.AddSuite(TTrustTests.Create('CLI Trust'));
    TestRunnerProgram.Run;
  finally
    GEnvironment.Free;
  end;
  ExitCode := TestResultToExitCode;
end.
