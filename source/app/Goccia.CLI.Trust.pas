unit Goccia.CLI.Trust;

{ Config trust (ADR 0122): a config's permission requests take effect only
  once the user has trusted them, or accepted them for one run with -P.

  Trust is recorded in a per-user store outside any repository, keyed by the
  config's canonical path and the SHA-256 of its normalized permission block
  (Goccia.CLI.Permissions), so a block copied to another path is not trusted
  and any change to a trusted block needs trusting again.

  Readers never lock: a run reads the store once, and writers replace the
  file in one rename. Writers serialize on an exclusive lock file and merge
  their changes into the store as it is on disk at the time. }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  CriticalSections,

  Goccia.CLI.Permissions,
  Goccia.FileExtensions;

type
  TGocciaTrustEntry = record
    { The canonical path of the config file. }
    ConfigPath: string;
    Hash: string;
    { The normalized block that was trusted; only used to show what changed
      since. }
    BlockJSON: string;
    TrustedAt: string;
    TrustedBy: string;
  end;

  { An unreadable store: not JSON, or written by a newer GocciaScript. }
  EGocciaTrustStoreError = class(Exception);

  TGocciaEnvironmentLookup = function(const AName: string): string;

  { Where the per-user store lives. }
  TGocciaTrustStorePlatform = (gtspUnix, gtspDarwin, gtspWindows, gtspNone);

  TGocciaTrustStore = class
  private type
    TChangeKind = (tckPut, tckRemove);
    TChange = record
      Kind: TChangeKind;
      Entry: TGocciaTrustEntry;
    end;
  private
    FPath: string;
    FCaseInsensitive: Boolean;
    FPrivateDirectory: Boolean;
    FEntries: array of TGocciaTrustEntry;
    FChanges: array of TChange;
    function IndexOf(const AKey: string): Integer;
    function SameKey(const ALeft, ARight: string): Boolean;
    function IsAtOrUnder(const AKey, APath: string): Boolean;
    procedure ApplyPut(const AEntry: TGocciaTrustEntry);
    procedure ApplyRemove(const AKey: string);
    procedure ReadFile;
    function Serialize: string;
    procedure WriteFile;
  public
    constructor Create(const APath: string;
      const ACaseInsensitive: Boolean);
    { The per-user store path, or '' when it cannot be located. }
    class function DefaultPath(const ALookup: TGocciaEnvironmentLookup): string;
    class function DefaultPathFor(const APlatform: TGocciaTrustStorePlatform;
      const ALookup: TGocciaEnvironmentLookup): string;
    { Why DefaultPathFor is '', such as `HOME is not set`. }
    class function DefaultPathProblem(
      const APlatform: TGocciaTrustStorePlatform;
      const ALookup: TGocciaEnvironmentLookup): string;
    { A missing file is an empty store. Raises EGocciaTrustStoreError for a
      file that is not JSON or has a newer version. Keys compare
      case-insensitively on Darwin and Windows unless ACaseInsensitive says
      otherwise. }
    class function Load(const APath: string): TGocciaTrustStore; overload;
    class function Load(const APath: string;
      const ACaseInsensitive: Boolean): TGocciaTrustStore; overload;
    function TryFind(const AConfigPath: string;
      out AEntry: TGocciaTrustEntry): Boolean;
    procedure Put(const AEntry: TGocciaTrustEntry);
    { Removes the entries for APath and every config under it, a directory
      boundary at a time: /a/b covers /a/b/goccia.json, not /a/bc. Adds the
      removed paths to ARemoved when it is assigned. }
    function RemoveAtOrUnder(const APath: string;
      const ARemoved: TStrings): Integer;
    { Takes the store's lock, applies this object's Put and RemoveAtOrUnder
      calls to the file as it is now, and replaces it in one rename. }
    procedure Save;
    function Count: Integer;
    function EntryAt(const AIndex: Integer): TGocciaTrustEntry;
    property Path: string read FPath;
    { Save makes the store's directory private (0700) even when it already
      exists. True for the per-user default store, whose directory is its
      own; a --trust-store directory is the user's and is left alone. }
    property PrivateDirectory: Boolean read FPrivateDirectory
      write FPrivateDirectory;
  end;

  { How a run treats config permission requests. }
  TGocciaConfigTrustMode = (
    ctmStore,         // apply what the trust store has trusted
    ctmAcceptForRun,  // -P: apply every request for this run
    ctmIgnoreConfig   // --ignore-config-permissions: apply none
  );

  TGocciaConfigTrustState = (
    ctsNoRequest,       // the config requests no grant
    ctsNotHonored,      // it does, but none this binary can grant
    ctsTrusted,
    ctsAcceptedForRun,
    ctsIgnored,
    ctsNotTrusted,
    ctsChanged          // trusted once, with a different block
  );

  TGocciaConfigTrustVerdict = record
    ConfigPath: string;
    Hash: string;
    Request: TGocciaConfigPermissionRequest;
    State: TGocciaConfigTrustState;
    { The stored entry, for ctsChanged. }
    Previous: TGocciaTrustEntry;
    { The config's allows and unsafe-* keys may be applied. Its denies apply
      in every state. }
    function GrantsAccepted: Boolean;
    { The run must not start: ctsNotTrusted or ctsChanged. }
    function BlocksRun: Boolean;
    { The unsafe-* requests this verdict lets take effect. }
    function AcceptedUnsafe: TGocciaUnsafeRequests;
  end;
  TGocciaConfigTrustVerdicts = array of TGocciaConfigTrustVerdict;

  { A config requests permissions nobody trusted. Nothing ran. }
  EGocciaConfigTrustError = class(TCLIUsageError);

  TGocciaConfigLoader = function(const APath: string): TConfigEntryArray
    of object;

  { Decides each config's verdict once per run. Thread-safe: workers creating
    engines query the memoized verdicts. The store is read on first use, and
    only in ctmStore mode, so -P never touches the user's home directory. }
  TGocciaConfigTrustGate = class
  private
    FLock: TGocciaCriticalSection;
    FMode: TGocciaConfigTrustMode;
    FHonored: TGocciaHonoredCapabilities;
    FHonorsUnsafe: Boolean;
    FLoadConfig: TGocciaConfigLoader;
    FStorePath: string;
    FStoreProblem: string;
    FStore: TGocciaTrustStore;
    FStoreRead: Boolean;
    FPaths: TStringList;
    FVerdicts: TGocciaConfigTrustVerdicts;
    function Store: TGocciaTrustStore;
    function Decide(const AConfigPath: string): TGocciaConfigTrustVerdict;
  public
    { AStorePath is '' when there is no store; AStoreProblem then says why.
      ALoadConfig parses and validates one config file. }
    constructor Create(const AStorePath, AStoreProblem: string;
      const AMode: TGocciaConfigTrustMode;
      const AHonored: TGocciaHonoredCapabilities;
      const AHonorsUnsafe: Boolean; const ALoadConfig: TGocciaConfigLoader);
    destructor Destroy; override;
    { The verdict for the config at AConfigPath; '' has no request. Raises
      what ALoadConfig raises, without remembering it. }
    function Verify(const AConfigPath: string): TGocciaConfigTrustVerdict;
    { Raises EGocciaConfigTrustError, with FormatUntrustedReport, when any of
      AConfigPaths blocks the run. }
    procedure RequireAccepted(const AConfigPaths: TStrings;
      const AProgramName: string; const AArguments: array of string;
      const ATrustStoreArgument: string);
    property Mode: TGocciaConfigTrustMode read FMode;
    property StorePath: string read FStorePath;
    { Why the store could not be used: it cannot be located, or it is
      unreadable. Set once a verdict needed the store. }
    property StoreProblem: string read FStoreProblem;
  end;

const
  CONFIG_FILE_BASE_NAME = 'goccia';
  { In discovery priority order. }
  CONFIG_FILE_EXTENSIONS: array[0..2] of string = (EXT_TOML, EXT_JSON5,
    EXT_JSON);
  TRUST_STORE_VERSION = 1;
  IGNORE_CONFIG_PERMISSIONS_FLAG = 'ignore-config-permissions';
  TRUST_STORE_FLAG = 'trust-store';
  {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
  TRUST_KEYS_CASE_INSENSITIVE = True;
  {$ELSE}
  TRUST_KEYS_CASE_INSENSITIVE = False;
  {$IFEND}

{ The store key, and the location a request is read at, of a config path:
  its directory with symbolic links resolved, plus its own file name, which
  is not resolved. A config reached through a symlinked directory keys to the
  target; a symlinked config file keys to where the link is, because it
  governs the files beside the link. }
function TrustKeyForPath(const APath: string): string;
{ TrustKeyForPath, except that an existing directory is itself resolved (for
  --untrust <dir>). }
function TrustKeyForDirectory(const APath: string): string;

{ The platform this build keeps its store for. }
function CurrentTrustStorePlatform: TGocciaTrustStorePlatform;

{ APath relative to AWorkingDirectory when it is under it, else APath. }
function DisplayPath(const APath, AWorkingDirectory: string): string;

{ AArgument quoted for a POSIX shell (or cmd on Windows) when it needs it. }
function QuoteShellArgument(const AArgument: string): string;

{ The report for configs that block a run:

    2 config files request permissions that have not been trusted:

      tests/fetch/goccia.json (never trusted)
        allow-net: 127.0.0.1

    Nothing was run. To trust these requests (stored in <store>):
      <Program> --trust tests/fetch/goccia.json
    To accept them for this run only:
      <Program> -P <arguments>
    To run with command-line grants only:
      <Program> --ignore-config-permissions <arguments>

  Paths under AWorkingDirectory are shown relative to it. More than three
  configs are trusted through their nearest common directory. }
function FormatUntrustedReport(const AVerdicts: array of TGocciaConfigTrustVerdict;
  const AProgramName, AStorePath, AStoreProblem, ATrustStoreArgument: string;
  const AArguments: array of string; const AWorkingDirectory: string): string;

{ The goccia.* config files at or under APath: APath itself when it is a
  file, else the effective config of each directory under it (toml, then
  json5, then json), skipping node_modules and .git. Sorted. }
procedure FindTrustableConfigs(const APath: string; const AConfigs: TStrings);

{ The config.permissions audit decision for a verdict: True for allow. }
function ConfigTrustAuditAllows(const AVerdict: TGocciaConfigTrustVerdict):
  Boolean;
{ Its reason: `trusted sha256:<hex>`, `accepted for this run (-P)`,
  `not needed: <Program> honors none of these requests`, `not trusted`,
  `changed since trusted`, or `ignored (--ignore-config-permissions)`. }
function ConfigTrustAuditReason(const AVerdict: TGocciaConfigTrustVerdict;
  const AProgramName: string): string;

{ The current time as an ISO 8601 UTC timestamp, such as
  2026-09-25T10:12:03Z. }
function TrustTimestamp: string;

{ --trust: reports the permission requests of each config at or under
  ATargets and what changed since they were trusted, asks for confirmation
  on a terminal unless AConfirmed, and records them in the store at
  AStorePath. Raises TCLIUsageError when confirmation is needed and stdin is
  not a terminal; the store is then unchanged. }
procedure RunTrustCommand(const AStorePath: string; const ATargets: TStrings;
  const AConfirmed: Boolean; const ALoadConfig: TGocciaConfigLoader;
  const AProgramName: string);
{ --untrust: removes the entries at or under each of ATargets. }
procedure RunUntrustCommand(const AStorePath: string; const ATargets: TStrings);
{ --list-trusted: one line per entry, marked (changed) or (missing). }
procedure RunListTrustedCommand(const AStorePath: string;
  const ALoadConfig: TGocciaConfigLoader);

implementation

uses
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}BaseUnix,{$IFEND}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  DateUtils,

  FileUtils,
  JSONParser,
  NumericText,
  StringBuffer,
  TextEncoding,

  Goccia.Capabilities,
  Goccia.CLI.Stdin,
  Goccia.JSON.Utils,
  Goccia.Version;

const
  TRUSTED_KEY = 'trusted';
  VERSION_KEY = 'version';
  SHA256_KEY = 'sha256';
  BLOCK_KEY = 'block';
  TRUSTED_AT_KEY = 'trustedAt';
  TRUSTED_BY_KEY = 'trustedBy';
  LOCK_SUFFIX = '.lock';
  TEMPORARY_SUFFIX = '.tmp';
  LOCK_TIMEOUT_MILLISECONDS = 2000;
  LOCK_RETRY_MILLISECONDS = 50;
  { A lock older than this, or whose process is gone, was left by a writer
    that crashed: writers hold it for milliseconds. }
  LOCK_STALE_SECONDS = 60;
  { Initial capacities: a normalized block, a whole store, a report. They
    only size the first allocation; the buffers grow as needed. }
  BLOCK_BUFFER_CAPACITY = 128;
  STORE_BUFFER_CAPACITY = 256;
  REPORT_BUFFER_CAPACITY = 512;
  REPORT_INDENT = '  ';
  REPORT_DETAIL_INDENT = '    ';
  MAX_LISTED_TRUST_TARGETS = 3;
  EXCLUDED_SCAN_DIRECTORIES: array[0..1] of string = ('node_modules', '.git');
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  STORE_DIRECTORY_MODE = &700;
  STORE_FILE_MODE = &600;
  {$IFEND}

{ ── Paths ─────────────────────────────────────────────────────── }

function CanonicalDirectory(const APath: string): string;
begin
  Result := ExpandHostFileName(APath);
  if CanonicalHostPath(Result) <> '' then
    Result := CanonicalHostPath(Result);
  if (Length(Result) > 1) and IsPathDelimiter(Result, Length(Result)) then
    Result := ExcludeTrailingPathDelimiter(Result);
end;

function TrustKeyForPath(const APath: string): string;
var
  Expanded, Name: string;
begin
  Expanded := ExpandHostFileName(APath);
  if (Length(Expanded) > 1) and IsPathDelimiter(Expanded, Length(Expanded)) then
    Expanded := ExcludeTrailingPathDelimiter(Expanded);
  Name := ExtractFileName(Expanded);
  if Name = '' then
    Exit(CanonicalDirectory(Expanded));
  { The directory is resolved, the file name is not: a symlinked directory
    holds the same files as its target, while a symlinked config file
    governs different files than the one it points at. }
  Result := IncludeTrailingPathDelimiter(
    CanonicalDirectory(ExtractFileDir(Expanded))) + Name;
end;

function TrustKeyForDirectory(const APath: string): string;
begin
  if DirectoryExists(APath) then
    Result := CanonicalDirectory(APath)
  else
    Result := TrustKeyForPath(APath);
end;

function CurrentTrustStorePlatform: TGocciaTrustStorePlatform;
begin
  {$IF DEFINED(LAKON)}
  Result := gtspNone;
  {$ELSEIF DEFINED(MSWINDOWS)}
  Result := gtspWindows;
  {$ELSEIF DEFINED(DARWIN)}
  Result := gtspDarwin;
  {$ELSE}
  Result := gtspUnix;
  {$IFEND}
end;

function PathStartsWith(const APath, APrefix: string): Boolean;
begin
  {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
  Result := SameText(Copy(APath, 1, Length(APrefix)), APrefix);
  {$ELSE}
  Result := Copy(APath, 1, Length(APrefix)) = APrefix;
  {$IFEND}
end;

function DisplayPath(const APath, AWorkingDirectory: string): string;
var
  Prefix: string;
begin
  Prefix := IncludeTrailingPathDelimiter(AWorkingDirectory);
  if (AWorkingDirectory <> '') and (Length(APath) > Length(Prefix)) and
     PathStartsWith(APath, Prefix) then
    Result := Copy(APath, Length(Prefix) + 1, MaxInt)
  else
    Result := APath;
end;

function QuoteShellArgument(const AArgument: string): string;
const
  {$IFDEF MSWINDOWS}
  { cmd and PowerShell take path separators and 8.3 names as they are. }
  PLAIN_CHARACTERS = ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.', '/', '=',
    ':', ',', '@', '%', '+', '\', '~'];
  {$ELSE}
  PLAIN_CHARACTERS = ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.', '/', '=',
    ':', ',', '@', '%', '+'];
  {$ENDIF}
var
  I: Integer;
  NeedsQuotes: Boolean;
begin
  NeedsQuotes := AArgument = '';
  for I := 1 to Length(AArgument) do
    if not (AArgument[I] in PLAIN_CHARACTERS) then
    begin
      NeedsQuotes := True;
      Break;
    end;
  if not NeedsQuotes then
    Exit(AArgument);
  {$IFDEF MSWINDOWS}
  Result := '"' + StringReplace(AArgument, '"', '\"', [rfReplaceAll]) + '"';
  {$ELSE}
  Result := '''' + StringReplace(AArgument, '''', '''\''''', [rfReplaceAll]) +
    '''';
  {$ENDIF}
end;

function TrustTimestamp: string;
begin
  { Separators are quoted literals and the settings invariant, so no locale
    changes the stored text. }
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
    LocalTimeToUniversal(Now), CreateInvariantFormatSettings);
end;

{ ── Store reader ──────────────────────────────────────────────── }

type
  { Reads a trust store without building JSON values. Each entry's `block`
    is re-serialized compactly, which reproduces the normalized block that
    was written. Unknown keys are skipped, so a newer store still parses far
    enough to report its version. }
  TTrustStoreReader = class(TAbstractJSONParser)
  private
    FDepth: Integer;
    FTopKey: string;
    FField: string;
    FVersion: Int64;
    FEntry: TGocciaTrustEntry;
    FEntries: array of TGocciaTrustEntry;
    FInBlock: Boolean;
    FBlockDepth: Integer;
    FBlock: TStringBuffer;
    { One character per open block container: o/O for an object without or
      with members, a/A for an array without or with elements. }
    FBlockContainers: string;
    procedure BlockValuePrefix;
    procedure BlockOpen(const AContainer: Char; const AText: string);
    procedure BlockClose(const AText: string);
    function InEntry: Boolean;
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
  public
    procedure Read(const AText: string);
    property Version: Int64 read FVersion;
  end;

function TTrustStoreReader.InEntry: Boolean;
begin
  Result := (FTopKey = TRUSTED_KEY) and (FDepth = 3);
end;

procedure TTrustStoreReader.BlockValuePrefix;
var
  Last: Integer;
begin
  Last := Length(FBlockContainers);
  if Last = 0 then
    Exit;
  if FBlockContainers[Last] = 'A' then
    FBlock.Append(',')
  else if FBlockContainers[Last] = 'a' then
    FBlockContainers[Last] := 'A';
end;

procedure TTrustStoreReader.BlockOpen(const AContainer: Char;
  const AText: string);
begin
  BlockValuePrefix;
  FBlock.Append(AText);
  FBlockContainers := FBlockContainers + AContainer;
end;

procedure TTrustStoreReader.BlockClose(const AText: string);
begin
  FBlock.Append(AText);
  SetLength(FBlockContainers, Length(FBlockContainers) - 1);
end;

procedure TTrustStoreReader.OnNull;
begin
  if FInBlock then
  begin
    BlockValuePrefix;
    FBlock.Append('null');
  end;
end;

procedure TTrustStoreReader.OnBoolean(const AValue: Boolean);
begin
  if not FInBlock then
    Exit;
  BlockValuePrefix;
  if AValue then
    FBlock.Append('true')
  else
    FBlock.Append('false');
end;

procedure TTrustStoreReader.OnString(const AValue: string);
begin
  if FInBlock then
  begin
    BlockValuePrefix;
    FBlock.Append(QuoteJSONString(AValue));
  end
  else if InEntry then
  begin
    if FField = SHA256_KEY then
      FEntry.Hash := AValue
    else if FField = TRUSTED_AT_KEY then
      FEntry.TrustedAt := AValue
    else if FField = TRUSTED_BY_KEY then
      FEntry.TrustedBy := AValue;
  end;
end;

procedure TTrustStoreReader.OnInteger(const AValue: Int64);
begin
  if FInBlock then
  begin
    BlockValuePrefix;
    FBlock.Append(IntToStr(AValue));
  end
  else if (FDepth = 1) and (FTopKey = VERSION_KEY) then
    FVersion := AValue;
end;

procedure TTrustStoreReader.OnFloat(const AValue: Double);
begin
  if FInBlock then
  begin
    BlockValuePrefix;
    FBlock.Append(FloatToStr(AValue, CreateInvariantFormatSettings));
  end
  else if (FDepth = 1) and (FTopKey = VERSION_KEY) then
    { A fractional version is no version this reader knows. }
    FVersion := High(Int64);
end;

procedure TTrustStoreReader.OnBeginObject;
begin
  Inc(FDepth);
  if FInBlock then
    BlockOpen('o', '{')
  else if (FDepth = 4) and (FTopKey = TRUSTED_KEY) and
    (FField = BLOCK_KEY) then
  begin
    FInBlock := True;
    FBlockDepth := FDepth;
    FBlock := TStringBuffer.Create(BLOCK_BUFFER_CAPACITY);
    FBlockContainers := '';
    BlockOpen('o', '{');
  end
  else if InEntry then
  begin
    FEntry.Hash := '';
    FEntry.BlockJSON := '';
    FEntry.TrustedAt := '';
    FEntry.TrustedBy := '';
    FField := '';
  end;
end;

procedure TTrustStoreReader.OnObjectKey(const AKey: string);
var
  Last: Integer;
begin
  if FInBlock then
  begin
    Last := Length(FBlockContainers);
    if FBlockContainers[Last] = 'O' then
      FBlock.Append(',')
    else
      FBlockContainers[Last] := 'O';
    FBlock.Append(QuoteJSONString(AKey) + ':');
  end
  else if FDepth = 1 then
    FTopKey := AKey
  else if (FDepth = 2) and (FTopKey = TRUSTED_KEY) then
    FEntry.ConfigPath := AKey
  else if InEntry then
    FField := AKey;
end;

procedure TTrustStoreReader.OnEndObject;
begin
  if FInBlock then
  begin
    BlockClose('}');
    if FDepth = FBlockDepth then
    begin
      FInBlock := False;
      FEntry.BlockJSON := FBlock.ToString;
    end;
  end
  else if InEntry and (FEntry.ConfigPath <> '') then
  begin
    SetLength(FEntries, Length(FEntries) + 1);
    FEntries[High(FEntries)] := FEntry;
  end;
  Dec(FDepth);
end;

procedure TTrustStoreReader.OnBeginArray;
begin
  Inc(FDepth);
  if FInBlock then
    BlockOpen('a', '[');
end;

procedure TTrustStoreReader.OnEndArray;
begin
  if FInBlock then
    BlockClose(']');
  Dec(FDepth);
end;

procedure TTrustStoreReader.Read(const AText: string);
begin
  FDepth := 0;
  FVersion := 0;
  FInBlock := False;
  FEntries := nil;
  DoParse(AText);
end;

{ ── TGocciaTrustStore ─────────────────────────────────────────── }

constructor TGocciaTrustStore.Create(const APath: string;
  const ACaseInsensitive: Boolean);
begin
  inherited Create;
  FPath := APath;
  FCaseInsensitive := ACaseInsensitive;
  FPrivateDirectory := (APath <> '') and
    SameFileName(APath, DefaultPath(nil));
end;

class function TGocciaTrustStore.DefaultPathFor(
  const APlatform: TGocciaTrustStorePlatform;
  const ALookup: TGocciaEnvironmentLookup): string;

  function Directory(const AValue, ASeparator: string): string;
  begin
    Result := AValue;
    while (Length(Result) > 1) and
          (Copy(Result, Length(Result), 1) = ASeparator) do
      SetLength(Result, Length(Result) - 1);
  end;

var
  Home, ConfigHome, AppData: string;
begin
  Result := '';
  case APlatform of
    gtspWindows:
      begin
        AppData := ALookup('APPDATA');
        if AppData <> '' then
          Result := Directory(AppData, '\') + '\Goccia\trust.json';
      end;
    gtspDarwin:
      begin
        Home := ALookup('HOME');
        if Home <> '' then
          Result := Directory(Home, '/') +
            '/Library/Application Support/Goccia/trust.json';
      end;
    gtspUnix:
      begin
        { XDG: a relative XDG_CONFIG_HOME is invalid and ignored. }
        ConfigHome := ALookup('XDG_CONFIG_HOME');
        if (ConfigHome <> '') and (ConfigHome[1] = '/') then
          Result := Directory(ConfigHome, '/') + '/goccia/trust.json'
        else
        begin
          Home := ALookup('HOME');
          if Home <> '' then
            Result := Directory(Home, '/') + '/.config/goccia/trust.json';
        end;
      end;
  end;
end;

class function TGocciaTrustStore.DefaultPathProblem(
  const APlatform: TGocciaTrustStorePlatform;
  const ALookup: TGocciaEnvironmentLookup): string;
begin
  if DefaultPathFor(APlatform, ALookup) <> '' then
    Exit('');
  case APlatform of
    gtspWindows:
      Result := 'APPDATA is not set';
    gtspNone:
      Result := 'this build has no per-user trust store';
  else
    Result := 'HOME is not set';
  end;
end;

function EnvironmentValue(const AName: string): string;
begin
  Result := GetEnvironmentVariable(AName);
end;

class function TGocciaTrustStore.DefaultPath(
  const ALookup: TGocciaEnvironmentLookup): string;
begin
  if Assigned(ALookup) then
    Result := DefaultPathFor(CurrentTrustStorePlatform, ALookup)
  else
    Result := DefaultPathFor(CurrentTrustStorePlatform, @EnvironmentValue);
end;

class function TGocciaTrustStore.Load(const APath: string): TGocciaTrustStore;
begin
  Result := Load(APath, TRUST_KEYS_CASE_INSENSITIVE);
end;

class function TGocciaTrustStore.Load(const APath: string;
  const ACaseInsensitive: Boolean): TGocciaTrustStore;
begin
  Result := TGocciaTrustStore.Create(APath, ACaseInsensitive);
  try
    Result.ReadFile;
  except
    Result.Free;
    raise;
  end;
end;

procedure TGocciaTrustStore.ReadFile;
var
  Reader: TTrustStoreReader;
  Text: string;
  I: Integer;
begin
  FEntries := nil;
  if not FileExists(FPath) then
    Exit;
  try
    Text := ReadUTF8FileText(FPath);
  except
    on E: EConvertError do
      raise EGocciaTrustStoreError.CreateFmt(
        'trust store %s is not valid JSON; fix or delete it', [FPath]);
  end;
  Reader := TTrustStoreReader.Create;
  try
    try
      Reader.Read(Text);
    except
      on E: EJSONParseError do
        raise EGocciaTrustStoreError.CreateFmt(
          'trust store %s is not valid JSON; fix or delete it', [FPath]);
    end;
    if Reader.Version > TRUST_STORE_VERSION then
      raise EGocciaTrustStoreError.CreateFmt(
        'trust store %s was written by a newer GocciaScript (version %d); ' +
        'upgrade GocciaScript or remove the file', [FPath, Reader.Version]);
    if Reader.Version < TRUST_STORE_VERSION then
      raise EGocciaTrustStoreError.CreateFmt(
        'trust store %s has no valid "version"; fix or delete it', [FPath]);
    for I := 0 to High(Reader.FEntries) do
      ApplyPut(Reader.FEntries[I]);
  finally
    Reader.Free;
  end;
end;

function TGocciaTrustStore.SameKey(const ALeft, ARight: string): Boolean;
begin
  if FCaseInsensitive then
    Result := SameText(ALeft, ARight)
  else
    Result := ALeft = ARight;
end;

function TGocciaTrustStore.IsAtOrUnder(const AKey, APath: string): Boolean;
var
  Prefix: string;
begin
  if SameKey(AKey, APath) then
    Exit(True);
  Prefix := IncludeTrailingPathDelimiter(APath);
  Result := (Length(AKey) > Length(Prefix)) and
    SameKey(Copy(AKey, 1, Length(Prefix)), Prefix);
end;

function TGocciaTrustStore.IndexOf(const AKey: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FEntries) do
    if SameKey(FEntries[I].ConfigPath, AKey) then
      Exit(I);
  Result := -1;
end;

procedure TGocciaTrustStore.ApplyPut(const AEntry: TGocciaTrustEntry);
var
  Index: Integer;
begin
  Index := IndexOf(AEntry.ConfigPath);
  if Index < 0 then
  begin
    Index := Length(FEntries);
    SetLength(FEntries, Index + 1);
  end;
  FEntries[Index] := AEntry;
end;

procedure TGocciaTrustStore.ApplyRemove(const AKey: string);
var
  I, Kept: Integer;
begin
  Kept := 0;
  for I := 0 to High(FEntries) do
    if not IsAtOrUnder(FEntries[I].ConfigPath, AKey) then
    begin
      FEntries[Kept] := FEntries[I];
      Inc(Kept);
    end;
  SetLength(FEntries, Kept);
end;

function TGocciaTrustStore.TryFind(const AConfigPath: string;
  out AEntry: TGocciaTrustEntry): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(TrustKeyForPath(AConfigPath));
  Result := Index >= 0;
  if Result then
    AEntry := FEntries[Index]
  else
    AEntry := Default(TGocciaTrustEntry);
end;

procedure TGocciaTrustStore.Put(const AEntry: TGocciaTrustEntry);
var
  Change: TChange;
begin
  Change.Kind := tckPut;
  Change.Entry := AEntry;
  Change.Entry.ConfigPath := TrustKeyForPath(AEntry.ConfigPath);
  SetLength(FChanges, Length(FChanges) + 1);
  FChanges[High(FChanges)] := Change;
  ApplyPut(Change.Entry);
end;

function TGocciaTrustStore.RemoveAtOrUnder(const APath: string;
  const ARemoved: TStrings): Integer;
var
  Key: string;
  I: Integer;
  Change: TChange;
begin
  Key := TrustKeyForDirectory(APath);
  Result := 0;
  for I := 0 to High(FEntries) do
    if IsAtOrUnder(FEntries[I].ConfigPath, Key) then
    begin
      Inc(Result);
      if Assigned(ARemoved) then
        ARemoved.Add(FEntries[I].ConfigPath);
    end;
  if Result = 0 then
    Exit;
  Change := Default(TChange);
  Change.Kind := tckRemove;
  Change.Entry.ConfigPath := Key;
  SetLength(FChanges, Length(FChanges) + 1);
  FChanges[High(FChanges)] := Change;
  ApplyRemove(Key);
end;

function TGocciaTrustStore.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TGocciaTrustStore.EntryAt(const AIndex: Integer): TGocciaTrustEntry;
begin
  Result := FEntries[AIndex];
end;

function TGocciaTrustStore.Serialize: string;
var
  Buffer: TStringBuffer;
  Sorted: TStringList;
  I, Index: Integer;
  Entry: TGocciaTrustEntry;
  Block: string;
begin
  Sorted := TStringList.Create;
  try
    Sorted.UseLocale := False;
    Sorted.CaseSensitive := True;
    for I := 0 to High(FEntries) do
      Sorted.AddObject(FEntries[I].ConfigPath, TObject(NativeInt(I)));
    Sorted.Sort;

    Buffer := TStringBuffer.Create(STORE_BUFFER_CAPACITY);
    Buffer.Append('{' + sLineBreak);
    Buffer.Append('  "' + VERSION_KEY + '": ' + IntToStr(TRUST_STORE_VERSION) +
      ',' + sLineBreak);
    Buffer.Append('  "' + TRUSTED_KEY + '": {');
    for I := 0 to Sorted.Count - 1 do
    begin
      Index := NativeInt(Sorted.Objects[I]);
      Entry := FEntries[Index];
      Block := Entry.BlockJSON;
      if Block = '' then
        Block := '{}';
      if I > 0 then
        Buffer.Append(',');
      Buffer.Append(sLineBreak + '    ' + QuoteJSONString(Entry.ConfigPath) +
        ': {' + sLineBreak);
      Buffer.Append('      "' + SHA256_KEY + '": ' +
        QuoteJSONString(Entry.Hash) + ',' + sLineBreak);
      Buffer.Append('      "' + BLOCK_KEY + '": ' + Block + ',' + sLineBreak);
      Buffer.Append('      "' + TRUSTED_AT_KEY + '": ' +
        QuoteJSONString(Entry.TrustedAt) + ',' + sLineBreak);
      Buffer.Append('      "' + TRUSTED_BY_KEY + '": ' +
        QuoteJSONString(Entry.TrustedBy) + sLineBreak);
      Buffer.Append('    }');
    end;
    if Sorted.Count > 0 then
      Buffer.Append(sLineBreak + '  ');
    Buffer.Append('}' + sLineBreak + '}' + sLineBreak);
    Result := Buffer.ToString;
  finally
    Sorted.Free;
  end;
end;

{ Creates each missing directory of APath, private to the user. }
procedure CreateStoreDirectory(const APath: string);
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  Parent: string;
begin
  if (APath = '') or DirectoryExists(APath) then
    Exit;
  Parent := ExtractFileDir(ExcludeTrailingPathDelimiter(APath));
  if (Parent <> '') and (Parent <> APath) then
    CreateStoreDirectory(Parent);
  if (fpMkdir(APath, STORE_DIRECTORY_MODE) <> 0) and
     not DirectoryExists(APath) then
    raise EGocciaTrustStoreError.CreateFmt('cannot create %s: %s',
      [APath, SysErrorMessage(fpgeterrno)]);
end;
{$ELSE}
begin
  if (APath <> '') and not ForceDirectories(APath) then
    raise EGocciaTrustStoreError.CreateFmt('cannot create %s', [APath]);
end;
{$IFEND}

{ Creates ALockPath exclusively. False when it already exists. }
function TryCreateLockFile(const ALockPath: string): Boolean;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  PathBytes: TBytes;
  ErrorOffset: Integer;
  Handle: cint;
begin
  if not TryEncodeUTF8NullTerminated(ALockPath, PathBytes, ErrorOffset) then
    raise EGocciaTrustStoreError.CreateFmt(
      'cannot encode the lock file path %s', [ALockPath]);
  Handle := fpOpen(PAnsiChar(@PathBytes[0]), O_WRONLY or O_CREAT or O_EXCL,
    STORE_FILE_MODE);
  if Handle < 0 then
  begin
    if fpgeterrno = ESysEEXIST then
      Exit(False);
    raise EGocciaTrustStoreError.CreateFmt('cannot create %s: %s',
      [ALockPath, SysErrorMessage(fpgeterrno)]);
  end;
  fpClose(Handle);
  Result := True;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  Handle: THandle;
begin
  Handle := CreateFileW(PWideChar(UnicodeString(ALockPath)), GENERIC_WRITE, 0,
    nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    if (GetLastError = ERROR_FILE_EXISTS) or
       (GetLastError = ERROR_ALREADY_EXISTS) then
      Exit(False);
    raise EGocciaTrustStoreError.CreateFmt('cannot create %s: %s',
      [ALockPath, SysErrorMessage(GetLastError)]);
  end;
  CloseHandle(Handle);
  Result := True;
end;
{$ELSE}
begin
  raise EGocciaTrustStoreError.Create('this build cannot write a trust store');
end;
{$IFEND}

function UnixNow: Int64;
begin
  Result := DateTimeToUnix(LocalTimeToUniversal(Now));
end;

{ `<pid> <unix seconds>`: who holds the lock and since when, so a lock a
  crashed writer left behind can be recognized. }
procedure RecordLockOwner(const ALockPath: string);
var
  Stream: TFileStream;
  Owner: TBytes;
begin
  Owner := EncodeUTF8WithReplacement(IntToStr(GetProcessID) + ' ' +
    IntToStr(UnixNow) + sLineBreak);
  try
    Stream := TFileStream.Create(ALockPath, fmOpenWrite or fmShareDenyNone);
    try
      Stream.WriteBuffer(Owner[0], Length(Owner));
    finally
      Stream.Free;
    end;
  except
    { The lock still excludes other writers; only staleness detection is
      weaker without an owner. }
    on E: EStreamError do;
  end;
end;

function ProcessIsGone(const AProcessID: Int64): Boolean;
begin
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  Result := (AProcessID > 0) and (AProcessID <= High(TPid)) and
    (fpKill(TPid(AProcessID), 0) <> 0) and (fpgeterrno = ESysESRCH);
  {$ELSE}
  { Without a portable liveness probe the age alone decides. }
  Result := False;
  {$IFEND}
end;

{ Whether the lock at ALockPath was left by a writer that no longer holds
  it: its process is gone, or it is older than LOCK_STALE_SECONDS. AOwner is
  the lock's content that was judged; AReason says why it is stale. }
function LockIsStale(const ALockPath: string; out AOwner,
  AReason: string): Boolean;
var
  Owner: string;
  Separator: Integer;
  ProcessID, Since: Int64;
  Modified: TDateTime;
begin
  Result := False;
  AReason := '';
  try
    AOwner := ReadUTF8FileText(ALockPath);
  except
    { Gone, or unreadable: let the next attempt decide. }
    on E: Exception do
      Exit(False);
  end;
  Owner := Trim(AOwner);
  Separator := Pos(' ', Owner);
  if (Separator > 1) and
     TryStrToInt64(Copy(Owner, 1, Separator - 1), ProcessID) and
     TryStrToInt64(Copy(Owner, Separator + 1, MaxInt), Since) then
  begin
    if ProcessIsGone(ProcessID) then
    begin
      AReason := Format('process %d that held it is gone', [ProcessID]);
      Exit(True);
    end;
    if UnixNow - Since > LOCK_STALE_SECONDS then
    begin
      AReason := Format('held for more than %d seconds',
        [LOCK_STALE_SECONDS]);
      Exit(True);
    end;
    Exit(False);
  end;
  { No owner recorded: judge by the file's age. }
  if FileAge(ALockPath, Modified) and
     (SecondsBetween(Now, Modified) > LOCK_STALE_SECONDS) then
  begin
    AReason := Format('no owner recorded and older than %d seconds',
      [LOCK_STALE_SECONDS]);
    Result := True;
  end;
end;

{ Removes a stale lock, unless another writer replaced it meanwhile. }
function RemoveStaleLock(const ALockPath: string): Boolean;
var
  Owner, Reason, Current: string;
begin
  Result := False;
  if not LockIsStale(ALockPath, Owner, Reason) then
    Exit;
  try
    Current := ReadUTF8FileText(ALockPath);
  except
    on E: Exception do
      Exit;
  end;
  if (Current = Owner) and DeleteFile(ALockPath) then
  begin
    WriteLn(ErrOutput, Format('Warning: removed stale trust store lock %s ' +
      '(%s)', [ALockPath, Reason]));
    Result := True;
  end;
end;

procedure TGocciaTrustStore.WriteFile;
var
  Error: string;
begin
  { Private from creation: the temporary is 0600 before it is renamed. }
  if not ReplaceHostFile(FPath, FPath + '.' + IntToStr(GetProcessID) +
     TEMPORARY_SUFFIX, EncodeUTF8WithReplacement(Serialize), STORE_FILE_MODE,
     Error) then
    raise EGocciaTrustStoreError.CreateFmt('cannot write trust store %s: %s',
      [FPath, Error]);
end;

procedure TGocciaTrustStore.Save;
var
  LockPath, Directory: string;
  Waited: Integer;
  I: Integer;
begin
  Directory := ExtractFileDir(FPath);
  CreateStoreDirectory(Directory);
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  if FPrivateDirectory then
    fpChmod(Directory, STORE_DIRECTORY_MODE);
  {$IFEND}
  LockPath := FPath + LOCK_SUFFIX;
  Waited := 0;
  while not TryCreateLockFile(LockPath) do
  begin
    if RemoveStaleLock(LockPath) then
      Continue;
    if Waited >= LOCK_TIMEOUT_MILLISECONDS then
      raise EGocciaTrustStoreError.CreateFmt(
        'trust store %s is locked by another process (%s exists); retry, or ' +
        'delete %s if no GocciaScript process is updating the store',
        [FPath, LockPath, LockPath]);
    Sleep(LOCK_RETRY_MILLISECONDS);
    Inc(Waited, LOCK_RETRY_MILLISECONDS);
  end;
  RecordLockOwner(LockPath);
  try
    { Another writer may have changed the file since it was read: apply
      this store's changes to the file as it is now. }
    ReadFile;
    for I := 0 to High(FChanges) do
      if FChanges[I].Kind = tckPut then
        ApplyPut(FChanges[I].Entry)
      else
        ApplyRemove(FChanges[I].Entry.ConfigPath);
    WriteFile;
    FChanges := nil;
  finally
    DeleteFile(LockPath);
  end;
end;

{ ── Verdicts ──────────────────────────────────────────────────── }

function TGocciaConfigTrustVerdict.GrantsAccepted: Boolean;
begin
  Result := State in [ctsNoRequest, ctsTrusted, ctsAcceptedForRun];
end;

function TGocciaConfigTrustVerdict.BlocksRun: Boolean;
begin
  Result := State in [ctsNotTrusted, ctsChanged];
end;

function TGocciaConfigTrustVerdict.AcceptedUnsafe: TGocciaUnsafeRequests;
begin
  if GrantsAccepted then
    Result := Request.Unsafe
  else
    Result := [];
end;

function ConfigTrustAuditAllows(const AVerdict: TGocciaConfigTrustVerdict):
  Boolean;
begin
  Result := AVerdict.State in [ctsNoRequest, ctsNotHonored, ctsTrusted,
    ctsAcceptedForRun];
end;

function ConfigTrustAuditReason(const AVerdict: TGocciaConfigTrustVerdict;
  const AProgramName: string): string;
begin
  case AVerdict.State of
    ctsTrusted:
      Result := 'trusted sha256:' + AVerdict.Hash;
    ctsAcceptedForRun:
      Result := 'accepted for this run (-P)';
    ctsNotHonored:
      Result := 'not needed: ' + AProgramName +
        ' honors none of these requests';
    ctsNotTrusted:
      Result := 'not trusted';
    ctsChanged:
      Result := 'changed since trusted';
    ctsIgnored:
      Result := 'ignored (--' + IGNORE_CONFIG_PERMISSIONS_FLAG + ')';
  else
    Result := 'no permission requests';
  end;
end;

{ ── TGocciaConfigTrustGate ────────────────────────────────────── }

constructor TGocciaConfigTrustGate.Create(const AStorePath,
  AStoreProblem: string; const AMode: TGocciaConfigTrustMode;
  const AHonored: TGocciaHonoredCapabilities; const AHonorsUnsafe: Boolean;
  const ALoadConfig: TGocciaConfigLoader);
begin
  inherited Create;
  CriticalSectionInit(FLock);
  FStorePath := AStorePath;
  FStoreProblem := AStoreProblem;
  FMode := AMode;
  FHonored := AHonored;
  FHonorsUnsafe := AHonorsUnsafe;
  FLoadConfig := ALoadConfig;
  FPaths := TStringList.Create;
  FPaths.Sorted := True;
  FPaths.CaseSensitive := True;
end;

destructor TGocciaConfigTrustGate.Destroy;
begin
  FPaths.Free;
  FStore.Free;
  CriticalSectionDone(FLock);
  inherited Destroy;
end;

function TGocciaConfigTrustGate.Store: TGocciaTrustStore;
begin
  if not FStoreRead then
  begin
    FStoreRead := True;
    if FStorePath <> '' then
      try
        FStore := TGocciaTrustStore.Load(FStorePath);
      except
        on E: EGocciaTrustStoreError do
          FStoreProblem := E.Message;
      end;
  end;
  Result := FStore;
end;

function TGocciaConfigTrustGate.Decide(
  const AConfigPath: string): TGocciaConfigTrustVerdict;
var
  Entry: TGocciaTrustEntry;
  Location: string;
begin
  Result := Default(TGocciaConfigTrustVerdict);
  { Read at the key's location, so every spelling of the config yields one
    key and one hash. }
  Location := TrustKeyForPath(AConfigPath);
  Result.ConfigPath := Location;
  Result.Request := ReadConfigPermissionRequest(FLoadConfig(Location),
    Location);
  if not Result.Request.RequestsGrants then
  begin
    Result.State := ctsNoRequest;
    Exit;
  end;
  Result.Hash := PermissionBlockHash(Result.Request);
  if not Result.Request.RequestsHonoredGrants(FHonored, FHonorsUnsafe) then
  begin
    Result.State := ctsNotHonored;
    Exit;
  end;
  case FMode of
    ctmAcceptForRun:
      Result.State := ctsAcceptedForRun;
    ctmIgnoreConfig:
      Result.State := ctsIgnored;
  else
    if Assigned(Store) and Store.TryFind(AConfigPath, Entry) then
    begin
      if Entry.Hash = Result.Hash then
        Result.State := ctsTrusted
      else
      begin
        Result.State := ctsChanged;
        Result.Previous := Entry;
      end;
    end
    else
      Result.State := ctsNotTrusted;
  end;
end;

function TGocciaConfigTrustGate.Verify(
  const AConfigPath: string): TGocciaConfigTrustVerdict;
var
  Key: string;
  Index: Integer;
begin
  if AConfigPath = '' then
  begin
    Result := Default(TGocciaConfigTrustVerdict);
    Result.State := ctsNoRequest;
    Exit;
  end;
  Key := ExpandFileName(AConfigPath);
  CriticalSectionEnter(FLock);
  try
    if FPaths.Find(Key, Index) then
      Exit(FVerdicts[NativeInt(FPaths.Objects[Index])]);
    Result := Decide(Key);
    SetLength(FVerdicts, Length(FVerdicts) + 1);
    FVerdicts[High(FVerdicts)] := Result;
    FPaths.AddObject(Key, TObject(NativeInt(High(FVerdicts))));
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaConfigTrustGate.RequireAccepted(const AConfigPaths: TStrings;
  const AProgramName: string; const AArguments: array of string;
  const ATrustStoreArgument: string);
var
  Blocking: TGocciaConfigTrustVerdicts;
  Verdict: TGocciaConfigTrustVerdict;
  I: Integer;
begin
  Blocking := nil;
  for I := 0 to AConfigPaths.Count - 1 do
  begin
    Verdict := Verify(AConfigPaths[I]);
    if Verdict.BlocksRun then
    begin
      SetLength(Blocking, Length(Blocking) + 1);
      Blocking[High(Blocking)] := Verdict;
    end;
  end;
  if Length(Blocking) > 0 then
    raise EGocciaConfigTrustError.Create(FormatUntrustedReport(Blocking,
      AProgramName, FStorePath, FStoreProblem, ATrustStoreArgument,
      AArguments, GetCurrentDir));
end;

{ ── Report ────────────────────────────────────────────────────── }

function CommonDirectory(const APaths: array of string): string;
var
  I: Integer;
  Candidate: string;
begin
  if Length(APaths) = 0 then
    Exit('');
  Result := ExtractFileDir(APaths[0]);
  for I := 1 to High(APaths) do
  begin
    Candidate := IncludeTrailingPathDelimiter(Result);
    while (Result <> '') and
          not PathStartsWith(APaths[I], Candidate) do
    begin
      if ExtractFileDir(Result) = Result then
        Exit(Result);
      Result := ExtractFileDir(Result);
      Candidate := IncludeTrailingPathDelimiter(Result);
    end;
  end;
end;

function JoinArguments(const AArguments: array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AArguments) do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + QuoteShellArgument(AArguments[I]);
  end;
end;

function FormatUntrustedReport(const AVerdicts: array of TGocciaConfigTrustVerdict;
  const AProgramName, AStorePath, AStoreProblem, ATrustStoreArgument: string;
  const AArguments: array of string; const AWorkingDirectory: string): string;
var
  Buffer: TStringBuffer;
  I: Integer;
  Verdict: TGocciaConfigTrustVerdict;
  Paths: array of string;
  TrustCommand, Arguments, Target: string;
begin
  Buffer := TStringBuffer.Create(REPORT_BUFFER_CAPACITY);
  if Length(AVerdicts) = 1 then
    Buffer.Append('1 config file requests permissions that have not been ' +
      'trusted:' + sLineBreak)
  else
    Buffer.Append(IntToStr(Length(AVerdicts)) + ' config files request ' +
      'permissions that have not been trusted:' + sLineBreak);

  SetLength(Paths, Length(AVerdicts));
  for I := 0 to High(AVerdicts) do
  begin
    Verdict := AVerdicts[I];
    Paths[I] := Verdict.ConfigPath;
    Buffer.Append(sLineBreak + REPORT_INDENT +
      DisplayPath(Verdict.ConfigPath, AWorkingDirectory));
    if Verdict.State = ctsChanged then
    begin
      Buffer.Append(' (changed since trusted ' + Verdict.Previous.TrustedAt +
        ')' + sLineBreak);
      Buffer.Append(DescribePermissionChange(Verdict.Previous.BlockJSON,
        Verdict.Request, REPORT_DETAIL_INDENT));
    end
    else
    begin
      Buffer.Append(' (never trusted)' + sLineBreak);
      Buffer.Append(DescribePermissionRequest(Verdict.Request,
        REPORT_DETAIL_INDENT));
    end;
  end;

  TrustCommand := AProgramName;
  if ATrustStoreArgument <> '' then
    TrustCommand := TrustCommand + ' ' + QuoteShellArgument(
      '--' + TRUST_STORE_FLAG + '=' + ATrustStoreArgument)
  else if AStorePath = '' then
    TrustCommand := TrustCommand + ' --' + TRUST_STORE_FLAG + '=<path>';
  if Length(Paths) > MAX_LISTED_TRUST_TARGETS then
  begin
    Target := DisplayPath(CommonDirectory(Paths), AWorkingDirectory);
    TrustCommand := TrustCommand + ' --trust ' +
      QuoteShellArgument(IncludeTrailingPathDelimiter(Target));
  end
  else
    for I := 0 to High(Paths) do
      TrustCommand := TrustCommand + ' --trust ' +
        QuoteShellArgument(DisplayPath(Paths[I], AWorkingDirectory));

  Buffer.Append(sLineBreak + 'Nothing was run. ');
  if (AStorePath = '') and (ATrustStoreArgument = '') then
    Buffer.Append('The per-user trust store cannot be located (' +
      AStoreProblem + '); to trust these requests, name a store:' +
      sLineBreak)
  else if AStoreProblem <> '' then
    Buffer.Append(AStoreProblem + '. Then, to trust these requests:' +
      sLineBreak)
  else
    Buffer.Append('To trust these requests (stored in ' + AStorePath +
      '):' + sLineBreak);
  Buffer.Append(REPORT_INDENT + TrustCommand + sLineBreak);

  Arguments := JoinArguments(AArguments);
  if Arguments <> '' then
    Arguments := ' ' + Arguments;
  Buffer.Append('To accept them for this run only:' + sLineBreak);
  Buffer.Append(REPORT_INDENT + AProgramName + ' -' +
    ACCEPT_CONFIG_PERMISSIONS_SHORT_FLAG + Arguments + sLineBreak);
  Buffer.Append('To run with command-line grants only:' + sLineBreak);
  Buffer.Append(REPORT_INDENT + AProgramName + ' --' +
    IGNORE_CONFIG_PERMISSIONS_FLAG + Arguments);
  Result := Buffer.ToString;
end;

{ ── Scanning ──────────────────────────────────────────────────── }

function ConfigExtensionPriority(const AFileName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(CONFIG_FILE_EXTENSIONS) do
    if ExtractFileName(AFileName) =
       CONFIG_FILE_BASE_NAME + CONFIG_FILE_EXTENSIONS[I] then
      Exit(I);
  Result := -1;
end;

procedure FindTrustableConfigs(const APath: string; const AConfigs: TStrings);
var
  Found: TStringList;
  Effective: TStringList;
  I, Index, Priority: Integer;
  Directory: string;
begin
  if not DirectoryExists(APath) then
  begin
    if FileExists(APath) then
      AConfigs.Add(ExpandFileName(APath));
    Exit;
  end;

  Effective := TStringList.Create;
  try
    Effective.Sorted := True;
    Effective.CaseSensitive := True;
    Found := FindAllFilesExcludingDirectories(APath, CONFIG_FILE_EXTENSIONS,
      EXCLUDED_SCAN_DIRECTORIES);
    try
      for I := 0 to Found.Count - 1 do
      begin
        Priority := ConfigExtensionPriority(Found[I]);
        if Priority < 0 then
          Continue;
        Directory := ExtractFileDir(ExpandFileName(Found[I]));
        if not Effective.Find(Directory, Index) then
          Effective.AddObject(Directory, TObject(NativeInt(I)))
        else if Priority < ConfigExtensionPriority(
          Found[NativeInt(Effective.Objects[Index])]) then
          Effective.Objects[Index] := TObject(NativeInt(I));
      end;
      for I := 0 to Effective.Count - 1 do
        AConfigs.Add(ExpandFileName(Found[NativeInt(Effective.Objects[I])]));
    finally
      Found.Free;
    end;
  finally
    Effective.Free;
  end;
end;

{ ── Commands ──────────────────────────────────────────────────── }

function CountedConfigs(const ACount: Integer): string;
begin
  if ACount = 1 then
    Result := '1 config file'
  else
    Result := IntToStr(ACount) + ' config files';
end;

{ `tests/` for a directory, `tests/a/goccia.json` for a file. }
function DescribeTarget(const ATarget, AWorkingDirectory: string): string;
var
  Expanded: string;
begin
  Expanded := ExcludeTrailingPathDelimiter(ExpandFileName(ATarget));
  { The working directory itself is shown as ./, like any path under it. }
  if (AWorkingDirectory <> '') and
     SameFileName(Expanded, ExcludeTrailingPathDelimiter(AWorkingDirectory))
  then
    Result := '.'
  else
    Result := DisplayPath(Expanded, AWorkingDirectory);
  if DirectoryExists(ATarget) then
    Result := IncludeTrailingPathDelimiter(Result);
end;

{ An empty --trust or --untrust path names nothing, and would otherwise
  expand to the working directory. }
procedure RequireTargetPaths(const AOptionName: string;
  const ATargets: TStrings);
var
  I: Integer;
begin
  for I := 0 to ATargets.Count - 1 do
    if Trim(ATargets[I]) = '' then
      raise TCLIUsageError.CreateFmt('%s needs a path', [AOptionName]);
end;

function DescribeTargets(const ATargets: TStrings;
  const AWorkingDirectory: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ATargets.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + DescribeTarget(ATargets[I], AWorkingDirectory);
  end;
end;

procedure RunTrustCommand(const AStorePath: string; const ATargets: TStrings;
  const AConfirmed: Boolean; const ALoadConfig: TGocciaConfigLoader;
  const AProgramName: string);
var
  Store: TGocciaTrustStore;
  Configs, Found: TStringList;
  Pending: TGocciaConfigTrustVerdicts;
  Verdict: TGocciaConfigTrustVerdict;
  Entry: TGocciaTrustEntry;
  WorkingDirectory, Targets, Answer: string;
  I, Declaring, Unchanged: Integer;
begin
  WorkingDirectory := GetCurrentDir;
  RequireTargetPaths('--trust', ATargets);
  Targets := DescribeTargets(ATargets, WorkingDirectory);
  for I := 0 to ATargets.Count - 1 do
    if not FileExists(ATargets[I]) and not DirectoryExists(ATargets[I]) then
      raise Exception.CreateFmt('--trust path not found: %s', [ATargets[I]]);

  { A corrupt or newer store is refused before anything else. }
  Store := TGocciaTrustStore.Load(AStorePath);
  Configs := TStringList.Create;
  try
    Configs.UseLocale := False;
    Configs.CaseSensitive := True;
    Configs.Sorted := True;
    Configs.Duplicates := dupIgnore;
    Found := TStringList.Create;
    try
      for I := 0 to ATargets.Count - 1 do
        FindTrustableConfigs(ATargets[I], Found);
      { Two spellings of one config are one entry. }
      for I := 0 to Found.Count - 1 do
        Configs.Add(TrustKeyForPath(Found[I]));
    finally
      Found.Free;
    end;

    Pending := nil;
    Declaring := 0;
    Unchanged := 0;
    for I := 0 to Configs.Count - 1 do
    begin
      Verdict := Default(TGocciaConfigTrustVerdict);
      Verdict.ConfigPath := TrustKeyForPath(Configs[I]);
      Verdict.Request := ReadConfigPermissionRequest(
        ALoadConfig(Verdict.ConfigPath), Verdict.ConfigPath);
      if not Verdict.Request.RequestsGrants then
        Continue;
      Inc(Declaring);
      Verdict.Hash := PermissionBlockHash(Verdict.Request);
      if Store.TryFind(Configs[I], Entry) then
      begin
        if Entry.Hash = Verdict.Hash then
        begin
          Inc(Unchanged);
          Continue;
        end;
        Verdict.State := ctsChanged;
        Verdict.Previous := Entry;
      end
      else
        Verdict.State := ctsNotTrusted;
      SetLength(Pending, Length(Pending) + 1);
      Pending[High(Pending)] := Verdict;
    end;

    if Declaring = 0 then
    begin
      WriteLn('No config under ', Targets, ' requests permissions');
      Exit;
    end;
    if Length(Pending) = 0 then
    begin
      if Unchanged = 1 then
        WriteLn('The config file under ', Targets, ' is already trusted')
      else
        WriteLn('All ', CountedConfigs(Unchanged), ' under ', Targets,
          ' are already trusted');
      Exit;
    end;

    WriteLn('Permission requests under ', Targets, ':');
    for I := 0 to High(Pending) do
    begin
      WriteLn;
      if Pending[I].State = ctsChanged then
      begin
        WriteLn(REPORT_INDENT, DisplayPath(Pending[I].ConfigPath,
          WorkingDirectory), ' (changed)');
        Write(DescribePermissionChange(Pending[I].Previous.BlockJSON,
          Pending[I].Request, REPORT_DETAIL_INDENT));
      end
      else
      begin
        WriteLn(REPORT_INDENT, DisplayPath(Pending[I].ConfigPath,
          WorkingDirectory), ' (new)');
        Write(DescribePermissionRequest(Pending[I].Request,
          REPORT_DETAIL_INDENT));
      end;
    end;
    if Unchanged > 0 then
    begin
      WriteLn;
      WriteLn(REPORT_INDENT, Unchanged, ' more already trusted and unchanged.');
    end;
    WriteLn;

    if not AConfirmed then
    begin
      if not IsInputTerminal then
        raise TCLIUsageError.Create('--trust needs confirmation; re-run ' +
          'with --yes to trust without a prompt');
      Write('Trust ', CountedConfigs(Length(Pending)), '? [y/N] ');
      Answer := '';
      if not EOF(Input) then
        ReadLn(Input, Answer);
      Answer := LowerCase(Trim(Answer));
      if (Answer <> 'y') and (Answer <> 'yes') then
      begin
        WriteLn('Nothing was trusted.');
        ExitCode := 1;
        Exit;
      end;
    end;

    for I := 0 to High(Pending) do
    begin
      Entry.ConfigPath := Pending[I].ConfigPath;
      Entry.Hash := Pending[I].Hash;
      Entry.BlockJSON := NormalizedPermissionBlock(Pending[I].Request);
      Entry.TrustedAt := TrustTimestamp;
      Entry.TrustedBy := AProgramName + ' ' + GetVersion;
      Store.Put(Entry);
    end;
    Store.Save;
    WriteLn('Trusted ', CountedConfigs(Length(Pending)), ' in ', AStorePath);
  finally
    Configs.Free;
    Store.Free;
  end;
end;

procedure RunUntrustCommand(const AStorePath: string; const ATargets: TStrings);
var
  Store: TGocciaTrustStore;
  Report: TStringList;
  I, Removed: Integer;
  Changed: Boolean;
  WorkingDirectory: string;
begin
  RequireTargetPaths('--untrust', ATargets);
  WorkingDirectory := GetCurrentDir;
  Store := TGocciaTrustStore.Load(AStorePath);
  Report := TStringList.Create;
  try
    Changed := False;
    for I := 0 to ATargets.Count - 1 do
    begin
      Removed := Store.RemoveAtOrUnder(ATargets[I], nil);
      if Removed = 0 then
        Report.Add('No trusted config at or under ' +
          DescribeTarget(ATargets[I], WorkingDirectory))
      else
      begin
        Changed := True;
        Report.Add('Removed trust for ' + CountedConfigs(Removed) + ' under ' +
          DescribeTarget(ATargets[I], WorkingDirectory));
      end;
    end;
    { Report only what the saved store holds. }
    if Changed then
      Store.Save;
    for I := 0 to Report.Count - 1 do
      WriteLn(Report[I]);
  finally
    Report.Free;
    Store.Free;
  end;
end;

{ The keys a stored block declares: `allow-net, unsafe-shadowrealm`. }
function DescribeStoredKeys(const ABlockJSON: string): string;
var
  Lines: TGocciaCapabilityScopes;
  I: Integer;
begin
  Result := '';
  Lines := PermissionBlockLines(ABlockJSON);
  for I := 0 to High(Lines) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Copy(Lines[I], 1, Pos(':', Lines[I]) - 1);
  end;
end;

procedure RunListTrustedCommand(const AStorePath: string;
  const ALoadConfig: TGocciaConfigLoader);
var
  Store: TGocciaTrustStore;
  Entry: TGocciaTrustEntry;
  Status: string;
  I: Integer;
begin
  Store := TGocciaTrustStore.Load(AStorePath);
  try
    WriteLn('Trust store: ', AStorePath);
    if Store.Count = 0 then
    begin
      WriteLn('No trusted configs');
      Exit;
    end;
    for I := 0 to Store.Count - 1 do
    begin
      Entry := Store.EntryAt(I);
      Status := '';
      if not FileExists(Entry.ConfigPath) then
        Status := '  (missing)'
      else
        try
          if PermissionBlockHash(ReadConfigPermissionRequest(
             ALoadConfig(Entry.ConfigPath), Entry.ConfigPath)) <>
             Entry.Hash then
            Status := '  (changed)';
        except
          on E: Exception do
            Status := '  (unreadable: ' + E.Message + ')';
        end;
      WriteLn(Entry.ConfigPath, '  trusted ', Entry.TrustedAt, '  ',
        DescribeStoredKeys(Entry.BlockJSON), Status);
    end;
  finally
    Store.Free;
  end;
end;

end.
