unit Goccia.CLI.Application;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  CriticalSections,

  Goccia.Application,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.Trust,
  Goccia.Engine,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.ScriptLoader.SourceRegistry;

type
  TGocciaCLIApplication = class(TGocciaApplication)
  private
    FHelp: TFlagOption;
    FJobs: TIntegerOption;
    FLog: TStringOption;
    FAuditLog: TStringOption;
    FMultifile: TFlagOption;
    FConfig: TStringOption;
    FLogFileHandle: TextFile;
    FLogLock: TGocciaCriticalSection;
    FLogFileOpen: Boolean;
    FAuditLogStream: TFileStream;
    FAuditLogLock: TGocciaCriticalSection;
    FAuditLogOpen: Boolean;
    FEngineOptions: TGocciaEngineOptions;
    FCoverageOptions: TGocciaCoverageOptions;
    FProfilerOptions: TGocciaProfilerOptions;
    FSandboxOptions: TGocciaSandboxOptions;
    FOwnedOptions: TOptionBaseList;
    FAllOptions: TOptionArray;
    FSourceRegistry: TGocciaSourceRegistry;
    FRootConfigPath: string;
    FRootConfigExplicit: Boolean;
    FWarnLock: TGocciaCriticalSection;
    FWarned: TStringList;
    FTrust: TRepeatableOption;
    FUntrust: TRepeatableOption;
    FListTrusted: TFlagOption;
    FYes: TFlagOption;
    FAcceptConfigPermissions: TFlagOption;
    FIgnoreConfigPermissions: TFlagOption;
    FTrustStore: TStringOption;
    FTrustGate: TGocciaConfigTrustGate;
    FAuditedConfigs: TStringList;
    procedure BuildAllOptions;
    procedure CreateTrustOptions;
    function TrustOptions: TOptionArray;
    function TrustModeCount: Integer;
    procedure ValidateTrustOptions(const APaths: TStringList);
    function ResolveTrustStorePath(out AProblem: string): string;
    procedure RunTrustMode;
    procedure CreateTrustGate;
    function TrustStoreArgument: string;
    procedure AuditConfigVerdict(const AVerdict: TGocciaConfigTrustVerdict);
    function CommandLineGrantDescription: string;
    procedure InitializeSingletons;
    procedure ShutdownSingletons;
    procedure OpenLogFile;
    procedure CloseLogFile;
    procedure OpenAuditLog;
    procedure CloseAuditLog;
    procedure ValidateOutputPaths;
    procedure HandleCapabilityAudit(
      const AEvent: TGocciaCapabilityAuditEvent);
  protected
    procedure Configure; virtual; abstract;
    function UsageLine: string; virtual; abstract;
    { How this command relates to standard input.  suNone (the default)
      opts out of the no-argument rule entirely — for commands like
      GocciaREPL that never source a program from stdin.
      Stdin-defaulting commands override this so they get the "Input:"
      help section and the clig.dev no-argument behaviour from the shared
      base instead of restating it per binary. }
    function StdinUsage: TGocciaStdinUsage; virtual;
    { True when the command line names the program some other way than a
      path, so no path is not "no input" and the no-argument rule does not
      apply (GocciaRunner's --entry and sandbox options). Default: False. }
    function HasNonPathInput: Boolean; virtual;
    { Help text appended after the options and the Input: section. }
    function ExtraHelpText: string; virtual;
    { Checks the parsed command line before the shared capability and limit
      checks, so a binary can name a more specific problem than they would.
      Called once, after --help and the trust modes are handled. }
    procedure ValidateCommandLine(const APaths: TStringList); virtual;
    procedure Execute; override;
    procedure ExecuteWithPaths(const APaths: TStringList); virtual; abstract;
    procedure Validate; virtual;
    procedure AfterExecute; virtual;
    function AddEngineOptions: TGocciaEngineOptions;
    function AddCoverageOptions: TGocciaCoverageOptions;
    function AddProfilerOptions: TGocciaProfilerOptions;
    function AddSandboxOptions: TGocciaSandboxOptions;
    function AddFlag(const AName, AHelp: string): TFlagOption;
    function AddString(const AName, AHelp: string): TStringOption;
    function AddInteger(const AName, AHelp: string): TIntegerOption;
    function AddRepeatable(const AName, AHelp: string): TRepeatableOption;
    function Add(const AOption: TOptionBase): TOptionBase;
    procedure ConfigureCreatedEngine(const AEngine: TGocciaEngine;
      const AFileConfig: TConfigEntryArray); virtual;
    procedure ConfigureCapabilityAudit(const AEngine: TGocciaEngine);
    { The capabilities this binary can grant (ADR 0122). An --allow-* flag
      for any other capability is a usage error; a config request for one is
      a warning. Default: none. }
    function HonoredCapabilities: TGocciaHonoredCapabilities; virtual;
    { The limits this binary applies. Any other limit on the command line is
      a usage error; in config it is ignored. Default: all. }
    function HonoredSettings: TGocciaHonoredSettings; virtual;
    { Whether this binary applies the unsafe-* keys (it runs code). A config
      requesting them from a binary that does not is warned about instead of
      needing trust. Default: True. }
    function HonorsUnsafeRequests: Boolean; virtual;
    { Whether this binary reads the root config's `sandbox` section
      (GocciaRunner). Where it does, that section needs trust; a section it
      does not read is warned about. Default: False. }
    function HonorsSandboxSection: Boolean; virtual;
    { Who cannot grant a config's unsupported request, in its warning.
      Default: the program name. }
    function CapabilityPolicyName: string; virtual;
    { The trust verdict of the root config, without the unsupported-request
      warnings FileConfigVerdict prints. No request when there is no root
      config. }
    function RootConfigVerdict: TGocciaConfigTrustVerdict;
    { The engine capability set for a file: command-line grants, plus the
      permission request of the config that governs AFileName (see
      FileConfigVerdict) once the request is trusted or accepted, minus every
      deny. }
    function ResolveEngineCapabilities(const AFileConfigPath: string;
      const AFileName: string = ''): TGocciaCapabilities;
    { The trust verdict of the config that governs a file: its own config
      (AFileConfigPath), else the root config when RootConfigGoverns
      AFileName, else none. Requests this binary cannot honor are reported
      once on stderr. }
    function FileConfigVerdict(const AFileConfigPath: string;
      const AFileName: string = ''): TGocciaConfigTrustVerdict;
    { The permission request of FileConfigVerdict. }
    function FilePermissionRequest(const AFileConfigPath: string;
      const AFileName: string = ''): TGocciaConfigPermissionRequest;
    { Whether the root config's permissions and unsafe-* keys apply to
      AFileName: always for an explicit --config (and for AFileName = ''),
      otherwise only when the file is inside the root config's directory
      tree. A discovered config never grants to files outside its tree. }
    function RootConfigGoverns(const AFileName: string): Boolean;
    { The config whose permissions govern AFileName: its nearest config, or
      the root config when it governs the file. '' when there is neither. }
    function GoverningConfigPath(const AFileName: string): string;
    { The trust half of ValidateFileConfigs, for config paths rather than
      files: loads and validates each config, emits one config.permissions
      audit event per config that requests a grant, and raises
      EGocciaConfigTrustError with one report naming every config whose
      requests are neither trusted nor accepted for the run. }
    procedure VerifyGoverningConfigs(const AConfigPaths: TStrings);
    { ValidateFileConfigs for one input, such as STDIN_FILE_NAME or a
      session governed by the working directory's config. }
    procedure ValidateFileConfig(const AFileName: string);
    { Each WritesHostFile option whose value came from the root config (not
      the command line): a relative path is resolved against the directory of
      the config file that declared it, and the result must stay inside that
      directory, canonically. Raises TParseError naming the key and config
      otherwise. }
    procedure ConfineConfigOutputPaths(const AEntries: TConfigEntryArray);
    { Sends an event the application itself decides through the capability
      audit log, when one is open. Main thread only. }
    procedure EmitApplicationAudit(const AKind: TGocciaCapabilityKind;
      const ADecision: TGocciaCapabilityDecision;
      const ASubject, AReason: string);
    { Where an engine's capability set came from, for its
      capabilities.effective event: `cli --allow-net=example.com; config
      /repo/goccia.json trusted sha256:...`. }
    function CapabilityProvenance(
      const AVerdict: TGocciaConfigTrustVerdict): string;
    { The set for a main-thread warm-up engine: it grants ffi when any of
      AFiles would, so the FFI prototypes are warmed before workers start. }
    function WarmUpCapabilities(const AFiles: TStrings): TGocciaCapabilities;
    { Writes AMessage to stderr once per AKey for the whole run. Safe to call
      from worker threads. }
    procedure WarnOnce(const AKey, AMessage: string);
    { Parses the config at APath and rejects removed and command-line-only
      keys and malformed flag values. }
    function LoadFileConfig(const APath: string): TConfigEntryArray;
    { Loads and validates, on the calling thread, the config and permissions
      block of every distinct config governing AFiles (each file's nearest
      config, else the root config), so a config error stops the run before
      any file executes (a usage error exits 2 through Goccia.Application.Run)
      instead of failing one file among many. In the same pass, checks that
      each config's permission requests are trusted or accepted for the run
      (VerifyGoverningConfigs). ExpandMultifileFiles calls it. }
    procedure ValidateFileConfigs(const AFiles: TStrings);
    function ShouldApplyRootConfig(const APaths: TStringList;
      const AConfigPath: string; const AExplicitConfig: Boolean): Boolean; virtual;
    procedure HandleConsoleLog(const AMethod, ALine: string);
    { Discover the nearest goccia.json/json5/toml for a file and
      return its parsed entries.  Returns an empty array when no
      config is found.  Thread-safe: does not mutate shared state. }
    function DiscoverFileConfig(
      const AFileName: string): TConfigEntryArray;
    function DiscoverFileConfigPath(const AFileName: string): string;
    procedure ApplyVirtualModulesToEngine(const AEngine: TGocciaEngine;
      const AFileConfigPath: string);
    function CreateEngine(const AFileName: string;
      const ASource: TStringList;
      const AExecutor: TGocciaExecutor): TGocciaEngine;
    { Returns the effective job count: --jobs value, or ProcessorCount,
      capped to AFileCount. Returns 1 when parallelism is not desired. }
    function GetJobCount(const AFileCount: Integer): Integer;
    { True iff --multifile was passed (or set in goccia.json). }
    function MultifileEnabled: Boolean;
    { Single canonical source text loader.  All runners and workers should
      load source text through this method instead of calling
      CreateFileTextLines(ReadUTF8FileText(...)) directly.
      Returns a caller-owned TStringList — registered virtual sections
      return a fresh clone, unregistered names read from disk. }
    function SourceRegistry: TGocciaSourceRegistry;
    { When --multifile is set, walks AFiles, reads each input file,
      splits on --- separators, and replaces the original entry with
      its section names.  Sections are registered with SourceRegistry
      under "<original>[partN].<ext>".  Files without separators and
      .gbc files pass through unchanged.  Returns a NEW TStringList
      that the caller owns; AFiles is not mutated. }
    function ExpandMultifileFiles(
      const AFiles: TStringList): TStringList;
    { When --multifile is set and AStdinSource contains separators,
      splits, registers each section under "<stdin>[partN]", and
      returns the section name list.  Otherwise registers
      AStdinSource under STDIN_FILE_NAME and returns a single-entry
      list.  Either way the registry takes ownership of AStdinSource
      (or a fresh derivative); the caller must not free it.
      Caller owns the returned name list. }
    function SplitStdinMultifile(
      const AStdinSource: TStringList): TStringList;
    property EngineOptions: TGocciaEngineOptions read FEngineOptions;
    { The applied root config, or ''. }
    property RootConfigPath: string read FRootConfigPath;
    property CoverageOptions: TGocciaCoverageOptions read FCoverageOptions;
    property ProfilerOptions: TGocciaProfilerOptions read FProfilerOptions;
    property SandboxOptions: TGocciaSandboxOptions read FSandboxOptions;
    property LogFileOpen: Boolean read FLogFileOpen;
    property MultifileOption: TFlagOption read FMultifile;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;
  end;

{ Registers the JSON5 and TOML config parsers. Idempotent. }
procedure EnsureConfigParsersRegistered;

{ Why a config at AConfigPath may not have a host file written at APath, or
  '' when it may: the path is a symbolic link, or it resolves (through any
  existing directories) outside the config's directory. }
function ConfigOutputPathProblem(const APath, AConfigPath: string): string;

function ResolveSourceTypeOption(
  const AOption: TEnumOption<Goccia.CLI.Options.TGocciaSourceType>;
  const AFileConfig: TConfigEntryArray;
  const AFileName: string): Goccia.Engine.TGocciaSourceType;
procedure ApplyCompatibilityAndWarningFlags(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray);
{ The single place engine-affecting options reach an engine.  CreateEngine
  calls it for every binary that builds its engine through the base class;
  engines that must be constructed elsewhere — GocciaRunner's sandbox mode
  needs its own module resolver — call it directly rather than restating
  the option set, which is how the sandbox runner once lost --max-memory
  and the fetch policy.  Pass an empty AFileConfig when there is no host
  file to discover a per-file config for. AAcceptedUnsafe are the
  unsafe-* requests of the file's config that are trusted or accepted for
  the run (TGocciaConfigTrustVerdict.AcceptedUnsafe); the command-line flags
  apply regardless. }
procedure ApplyFileConfigToEngine(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AFileName: string;
  const AAcceptedUnsafe: TGocciaUnsafeRequests);

implementation

uses
  Generics.Collections,
  Math,

  CLI.Parser,
  CLI.Units,
  FileUtils,
  ProcessorDetection,
  TextEncoding,
  TextSemantics,

  Goccia.CLI.Help,
  Goccia.Coverage,
  Goccia.Error.Suggestions,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.JSON.Utils,
  Goccia.JSON5,
  Goccia.Keywords.Reserved,
  Goccia.Modules,
  Goccia.Modules.Configuration,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Profiler,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.ScriptLoader.Input,
  Goccia.StackLimit,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.TOML,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Formatting,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.YAML;

const
  TRUST_GROUP = 'Config trust';

type
  { `--trust <path>`: a repeatable path. }
  TPathListOption = class(TRepeatableOption)
  public
    function FormatForHelp: string; override;
  end;

  { `--trust-store=<path>`. The path attaches only with `=`, so the option
    never takes the next argument, an input file, as its value. }
  TPathOption = class(TStringOption)
  public
    procedure ApplyExplicit(const AValue: string;
      const AHasEquals: Boolean); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;
  end;

function TPathListOption.FormatForHelp: string;
begin
  Result := '--' + LongName + ' <path>';
end;

procedure TPathOption.ApplyExplicit(const AValue: string;
  const AHasEquals: Boolean);
begin
  if (not AHasEquals) or (AValue = '') then
    raise TCLIUsageError.CreateFmt('--%s needs a path: --%s=<path>',
      [LongName, LongName]);
  Apply(AValue);
end;

function TPathOption.ConsumesSeparateValue: Boolean;
begin
  Result := False;
end;

function TPathOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<path>';
end;

function IsAbsoluteFilePath(const APath: string): Boolean;
begin
  Result := (APath <> '') and ((APath[1] = PathDelim) or
    ((Length(APath) >= 2) and (APath[2] = ':')));
end;

{ ── Config file bridge parsers ─────────────────────────────── }

{ Extract top-level key-value pairs from a TGocciaObjectValue, flattening
  one level of nested objects to `parent.child` keys (the ParseJSONConfig
  contract in CLI.ConfigFile). }
procedure AppendObjectEntries(const AObject: TGocciaObjectValue;
  const APrefix: string; const ANested: Boolean;
  var AEntries: TConfigEntryArray; var ACount: Integer);

  procedure AddEntry(const AKey, AValue: string;
    const AKind: TConfigValueKind; const AInArray: Boolean);
  begin
    if ACount >= Length(AEntries) then
      SetLength(AEntries, Length(AEntries) * 2 + 4);
    AEntries[ACount].Key := AKey;
    AEntries[ACount].Value := AValue;
    AEntries[ACount].SourcePath := '';
    AEntries[ACount].Kind := AKind;
    AEntries[ACount].InArray := AInArray;
    Inc(ACount);
  end;

  function TryScalarText(const AValue: TGocciaValue; out AText: string;
    out AKind: TConfigValueKind): Boolean;
  begin
    Result := True;
    if AValue is TGocciaStringLiteralValue then
    begin
      AText := TGocciaStringLiteralValue(AValue).Value;
      AKind := cvkString;
    end
    else if AValue is TGocciaNumberLiteralValue then
    begin
      AText := ConfigNumberText(TGocciaNumberLiteralValue(AValue).Value);
      AKind := cvkNumber;
    end
    else if AValue is TGocciaBooleanLiteralValue then
    begin
      if TGocciaBooleanLiteralValue(AValue).Value then
        AText := 'true'
      else
        AText := 'false';
      AKind := cvkBoolean;
    end
    else
      Result := False;
  end;

var
  Keys: TArray<string>;
  I, J: Integer;
  Key, Text: string;
  Kind: TConfigValueKind;
  Value: TGocciaValue;
  Arr: TGocciaArrayValue;
begin
  Keys := AObject.GetOwnPropertyKeys;
  for I := 0 to High(Keys) do
  begin
    Key := APrefix + Keys[I];
    Value := AObject.GetProperty(Keys[I]);
    if not Assigned(Value) then
      Continue;

    if TryScalarText(Value, Text, Kind) then
      AddEntry(Key, Text, Kind, False)
    else if Value is TGocciaArrayValue then
    begin
      Arr := TGocciaArrayValue(Value);
      if Arr.GetLength = 0 then
        AddEntry(Key, '', cvkEmptyArray, True)
      else
        for J := 0 to Arr.GetLength - 1 do
          if TryScalarText(Arr.GetElement(J), Text, Kind) then
            AddEntry(Key, Text, Kind, True)
          else
            AddEntry(Key, '', cvkUnsupported, True);
    end
    else if (Value is TGocciaObjectValue) and not ANested then
    begin
      AddEntry(Key, '', cvkObject, False);
      AppendObjectEntries(TGocciaObjectValue(Value), Key + '.', True,
        AEntries, ACount);
    end
    else
      { null, deeper objects, and anything else without a flat form. }
      AddEntry(Key, '', cvkUnsupported, False);
  end;
end;

function ExtractObjectEntries(
  const AObject: TGocciaObjectValue): TConfigEntryArray;
var
  Count: Integer;
begin
  Result := nil;
  SetLength(Result, 8);
  Count := 0;
  AppendObjectEntries(AObject, '', False, Result, Count);
  SetLength(Result, Count);
end;

function ParseJSON5Config(const AContent: string): TConfigEntryArray;
var
  Parser: TGocciaJSON5Parser;
  Parsed: TGocciaValue;
begin
  SetLength(Result, 0);
  Parser := TGocciaJSON5Parser.Create;
  try
    Parsed := Parser.Parse(AContent);
    if (TGarbageCollector.Instance <> nil) and Assigned(Parsed) then
      TGarbageCollector.Instance.AddTempRoot(Parsed);
    try
      if Parsed is TGocciaObjectValue then
        Result := ExtractObjectEntries(TGocciaObjectValue(Parsed));
    finally
      if (TGarbageCollector.Instance <> nil) and Assigned(Parsed) then
        TGarbageCollector.Instance.RemoveTempRoot(Parsed);
    end;
  finally
    Parser.Free;
  end;
end;

function ParseTOMLConfig(const AContent: string): TConfigEntryArray;
var
  Parser: TGocciaTOMLParser;
  Parsed: TGocciaObjectValue;
begin
  SetLength(Result, 0);
  Parser := TGocciaTOMLParser.Create;
  try
    Parsed := Parser.Parse(AContent);
    if (TGarbageCollector.Instance <> nil) and Assigned(Parsed) then
      TGarbageCollector.Instance.AddTempRoot(Parsed);
    try
      if Assigned(Parsed) then
        Result := ExtractObjectEntries(Parsed);
    finally
      if (TGarbageCollector.Instance <> nil) and Assigned(Parsed) then
        TGarbageCollector.Instance.RemoveTempRoot(Parsed);
    end;
  finally
    Parser.Free;
  end;
end;

var
  GConfigParsersRegistered: Boolean = False;

procedure EnsureConfigParsersRegistered;
begin
  if GConfigParsersRegistered then
    Exit;
  RegisterConfigParser(EXT_JSON5, @ParseJSON5Config);
  RegisterConfigParser(EXT_TOML, @ParseTOMLConfig);
  GConfigParsersRegistered := True;
end;

{ TGocciaCLIApplication }

constructor TGocciaCLIApplication.Create(const AName: string);
begin
  inherited Create(AName);
  FOwnedOptions := TOptionBaseList.Create(True);
  FEngineOptions := nil;
  FCoverageOptions := nil;
  FProfilerOptions := nil;
  FSandboxOptions := nil;
  FHelp := nil;
  FJobs := nil;
  FLog := nil;
  FAuditLog := nil;
  FMultifile := nil;
  FConfig := nil;
  FRootConfigPath := '';
  // Created eagerly on the main thread so worker threads that read
  // through SourceRegistry.Load never race on first-access creation.
  FSourceRegistry := TGocciaSourceRegistry.Create;
  FLogFileOpen := False;
  FAuditLogStream := nil;
  FAuditLogOpen := False;
  CriticalSectionInit(FWarnLock);
  FWarned := TStringList.Create;
  FWarned.Sorted := True;
  FTrustGate := nil;
  FAuditedConfigs := TStringList.Create;
  FAuditedConfigs.Sorted := True;
end;

destructor TGocciaCLIApplication.Destroy;
begin
  CloseAuditLog;
  CloseLogFile;
  FOwnedOptions.Free;
  FEngineOptions.Free;
  FCoverageOptions.Free;
  FProfilerOptions.Free;
  FSandboxOptions.Free;
  FHelp.Free;
  FJobs.Free;
  FLog.Free;
  FAuditLog.Free;
  FMultifile.Free;
  FConfig.Free;
  FTrust.Free;
  FUntrust.Free;
  FListTrusted.Free;
  FYes.Free;
  FAcceptConfigPermissions.Free;
  FIgnoreConfigPermissions.Free;
  FTrustStore.Free;
  FTrustGate.Free;
  FAuditedConfigs.Free;
  FSourceRegistry.Free;
  FWarned.Free;
  CriticalSectionDone(FWarnLock);
  inherited Destroy;
end;

procedure TGocciaCLIApplication.CreateTrustOptions;
begin
  FTrust := TPathListOption.Create('trust',
    'Trust the permission requests of each goccia config at or under ' +
    '<path> (asks first; see --yes)', TRUST_GROUP);
  FUntrust := TPathListOption.Create('untrust',
    'Remove stored trust for configs at or under <path>', TRUST_GROUP);
  FListTrusted := TFlagOption.Create('list-trusted',
    'List trusted configs and whether they changed since', TRUST_GROUP);
  FYes := TFlagOption.Create('yes', 'Confirm --trust without prompting',
    TRUST_GROUP);
  FAcceptConfigPermissions := TFlagOption.Create(
    ACCEPT_CONFIG_PERMISSIONS_FLAG,
    'Apply config permission requests for this run without trusting them ' +
    '(short: -P)', TRUST_GROUP);
  FAcceptConfigPermissions.ShortName := ACCEPT_CONFIG_PERMISSIONS_SHORT_FLAG;
  FIgnoreConfigPermissions := TFlagOption.Create(
    IGNORE_CONFIG_PERMISSIONS_FLAG,
    'Ignore config permission requests and unsafe-* keys; use command-line ' +
    'grants only', TRUST_GROUP);
  FTrustStore := TPathOption.Create(TRUST_STORE_FLAG,
    'Use this trust store file instead of the per-user default', TRUST_GROUP);
  FTrust.CommandLineOnly := True;
  FUntrust.CommandLineOnly := True;
  FListTrusted.CommandLineOnly := True;
  FYes.CommandLineOnly := True;
  FAcceptConfigPermissions.CommandLineOnly := True;
  FIgnoreConfigPermissions.CommandLineOnly := True;
  FTrustStore.CommandLineOnly := True;
end;

function TGocciaCLIApplication.TrustOptions: TOptionArray;
begin
  SetLength(Result, 7);
  Result[0] := FTrust;
  Result[1] := FUntrust;
  Result[2] := FListTrusted;
  Result[3] := FYes;
  Result[4] := FAcceptConfigPermissions;
  Result[5] := FIgnoreConfigPermissions;
  Result[6] := FTrustStore;
end;

function TGocciaCLIApplication.TrustModeCount: Integer;
begin
  Result := Ord(FTrust.Present) + Ord(FUntrust.Present) +
    Ord(FListTrusted.Present);
end;

procedure TGocciaCLIApplication.ValidateTrustOptions(const APaths: TStringList);
var
  ModeName: string;
begin
  if FAcceptConfigPermissions.Present and FIgnoreConfigPermissions.Present then
    raise TCLIUsageError.Create('-P and --ignore-config-permissions cannot ' +
      'be combined');
  if TrustModeCount > 1 then
    raise TCLIUsageError.Create('--trust, --untrust, and --list-trusted ' +
      'cannot be combined; run each on its own');
  if FYes.Present and not FTrust.Present then
    raise TCLIUsageError.Create('--yes only confirms --trust');
  if TrustModeCount = 0 then
    Exit;
  if FTrust.Present then
    ModeName := '--trust'
  else if FUntrust.Present then
    ModeName := '--untrust'
  else
    ModeName := '--list-trusted';
  if APaths.Count > 0 then
    raise TCLIUsageError.CreateFmt('%s cannot be combined with input files; ' +
      'run it on its own', [ModeName]);
  if FAcceptConfigPermissions.Present or FIgnoreConfigPermissions.Present then
    raise TCLIUsageError.CreateFmt('%s cannot be combined with -P or ' +
      '--ignore-config-permissions', [ModeName]);
end;

function GetEnvironmentValue(const AName: string): string;
begin
  Result := GetEnvironmentVariable(AName);
end;

function TGocciaCLIApplication.ResolveTrustStorePath(
  out AProblem: string): string;
begin
  AProblem := '';
  if FTrustStore.Present then
  begin
    Exit(ExpandFileName(FTrustStore.Value));
  end;
  Result := TGocciaTrustStore.DefaultPath(@GetEnvironmentValue);
  if Result = '' then
    AProblem := TGocciaTrustStore.DefaultPathProblem(
      CurrentTrustStorePlatform, @GetEnvironmentValue);
end;

function TGocciaCLIApplication.TrustStoreArgument: string;
begin
  if FTrustStore.Present then
    Result := FTrustStore.Value
  else
    Result := '';
end;

procedure TGocciaCLIApplication.RunTrustMode;
var
  StorePath, Problem: string;
begin
  EnsureConfigParsersRegistered;
  StorePath := ResolveTrustStorePath(Problem);
  if StorePath = '' then
    raise Exception.CreateFmt('cannot locate the per-user trust store (%s); ' +
      'pass --%s=<path>', [Problem, TRUST_STORE_FLAG]);
  if FTrust.Present then
    RunTrustCommand(StorePath, FTrust.Values, FYes.Present, LoadFileConfig,
      Name)
  else if FUntrust.Present then
    RunUntrustCommand(StorePath, FUntrust.Values)
  else
    RunListTrustedCommand(StorePath, LoadFileConfig);
end;

procedure TGocciaCLIApplication.CreateTrustGate;
var
  Mode: TGocciaConfigTrustMode;
  StorePath, Problem, SandboxConfigPath: string;
begin
  StorePath := '';
  Problem := '';
  if FAcceptConfigPermissions.Present then
    Mode := ctmAcceptForRun
  else if FIgnoreConfigPermissions.Present then
    Mode := ctmIgnoreConfig
  else
  begin
    Mode := ctmStore;
    StorePath := ResolveTrustStorePath(Problem);
  end;
  if HonorsSandboxSection then
    SandboxConfigPath := FRootConfigPath
  else
    SandboxConfigPath := '';
  FTrustGate := TGocciaConfigTrustGate.Create(StorePath, Problem, Mode,
    HonoredCapabilities, HonorsUnsafeRequests, LoadFileConfig,
    SandboxConfigPath);
end;

procedure TGocciaCLIApplication.BuildAllOptions;
var
  Combined: array of TOptionArray;
  Count, I: Integer;
begin
  Count := 0;
  SetLength(Combined, 5);

  if Assigned(FEngineOptions) then
  begin
    Combined[Count] := FEngineOptions.Options;
    Inc(Count);
  end;

  if Assigned(FCoverageOptions) then
  begin
    Combined[Count] := FCoverageOptions.Options;
    Inc(Count);
  end;

  if Assigned(FProfilerOptions) then
  begin
    Combined[Count] := FProfilerOptions.Options;
    Inc(Count);
  end;

  if Assigned(FSandboxOptions) then
  begin
    Combined[Count] := FSandboxOptions.Options;
    Inc(Count);
  end;

  if FOwnedOptions.Count > 0 then
  begin
    SetLength(Combined[Count], FOwnedOptions.Count);
    for I := 0 to FOwnedOptions.Count - 1 do
      Combined[Count][I] := FOwnedOptions[I];
    Inc(Count);
  end;

  SetLength(Combined, Count);
  FAllOptions := ConcatOptions(Combined);

  SetLength(FAllOptions, Length(FAllOptions) + 1);
  FAllOptions[High(FAllOptions)] := FHelp;
end;

function TGocciaCLIApplication.AddEngineOptions: TGocciaEngineOptions;
begin
  FEngineOptions := TGocciaEngineOptions.Create;
  Result := FEngineOptions;
end;

function TGocciaCLIApplication.AddCoverageOptions: TGocciaCoverageOptions;
begin
  FCoverageOptions := TGocciaCoverageOptions.Create;
  Result := FCoverageOptions;
end;

function TGocciaCLIApplication.AddProfilerOptions: TGocciaProfilerOptions;
begin
  FProfilerOptions := TGocciaProfilerOptions.Create;
  Result := FProfilerOptions;
end;

function TGocciaCLIApplication.AddSandboxOptions: TGocciaSandboxOptions;
begin
  FSandboxOptions := TGocciaSandboxOptions.Create;
  Result := FSandboxOptions;
end;

function TGocciaCLIApplication.AddFlag(const AName, AHelp: string): TFlagOption;
begin
  Result := TFlagOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.AddString(const AName, AHelp: string): TStringOption;
begin
  Result := TStringOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.AddInteger(const AName, AHelp: string): TIntegerOption;
begin
  Result := TIntegerOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.AddRepeatable(const AName, AHelp: string): TRepeatableOption;
begin
  Result := TRepeatableOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.Add(const AOption: TOptionBase): TOptionBase;
begin
  FOwnedOptions.Add(AOption);
  Result := AOption;
end;

function TGocciaCLIApplication.DiscoverFileConfig(
  const AFileName: string): TConfigEntryArray;
var
  ConfigPath: string;
begin
  SetLength(Result, 0);
  ConfigPath := DiscoverFileConfigPath(AFileName);
  if ConfigPath <> '' then
    Result := LoadFileConfig(ConfigPath);
end;

function TGocciaCLIApplication.LoadFileConfig(
  const APath: string): TConfigEntryArray;
begin
  Result := ParseConfigFile(APath);
  ValidateConfigEntries(Result, FAllOptions);
end;

procedure TGocciaCLIApplication.ValidateFileConfigs(const AFiles: TStrings);
var
  ConfigPaths: TStringList;
  I: Integer;
begin
  ConfigPaths := TStringList.Create;
  try
    { Byte order, so reports list configs the same way in every locale. }
    ConfigPaths.UseLocale := False;
    ConfigPaths.CaseSensitive := True;
    ConfigPaths.Sorted := True;
    ConfigPaths.Duplicates := dupIgnore;
    for I := 0 to AFiles.Count - 1 do
      ConfigPaths.Add(GoverningConfigPath(AFiles[I]));
    VerifyGoverningConfigs(ConfigPaths);
  finally
    ConfigPaths.Free;
  end;
end;

procedure TGocciaCLIApplication.ValidateFileConfig(const AFileName: string);
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Add(AFileName);
    ValidateFileConfigs(Files);
  finally
    Files.Free;
  end;
end;

procedure TGocciaCLIApplication.WarnOnce(const AKey, AMessage: string);
var
  Index: Integer;
begin
  CriticalSectionEnter(FWarnLock);
  try
    if FWarned.Find(AKey, Index) then
      Exit;
    FWarned.Add(AKey);
    WriteLn(ErrOutput, AMessage);
  finally
    CriticalSectionLeave(FWarnLock);
  end;
end;

function TGocciaCLIApplication.DiscoverFileConfigPath(
  const AFileName: string): string;
var
  StartDir: string;
begin
  Result := '';
  if AFileName = '' then
    Exit;
  EnsureConfigParsersRegistered;
  StartDir := ExtractFilePath(ExpandFileName(AFileName));
  if StartDir = '' then
    StartDir := GetCurrentDir;
  Result := DiscoverConfigFile(StartDir,
    [CONFIG_FILE_BASE_NAME], CONFIG_FILE_EXTENSIONS);
end;

{ Resolve --source-type / config "source-type" into the engine's
  TGocciaSourceType enum.  Priority: CLI option > per-file config > root
  config > file-extension default (.mjs is module) > default (script).

  CLI option and root config values are validated by TEnumOption.Apply
  before they reach this function (invalid values raise TParseError
  at parse/apply time).  Per-file config values come in as raw strings via
  FindConfigEntry, so we validate here: 'module' and 'script' are accepted
  case-insensitively, anything else emits a stderr warning and falls back
  to the file-extension default. }
function DefaultSourceTypeForFileName(
  const AFileName: string): Goccia.Engine.TGocciaSourceType;
begin
  if IsModuleSourceFileName(AFileName) then
    Exit(Goccia.Engine.stModule);
  Result := Goccia.Engine.stScript;
end;

function ResolveSourceTypeOption(
  const AOption: TEnumOption<Goccia.CLI.Options.TGocciaSourceType>;
  const AFileConfig: TConfigEntryArray;
  const AFileName: string): Goccia.Engine.TGocciaSourceType;
var
  ValueStr, NormalizedValue: string;
begin
  if AOption.FromCommandLine then
  begin
    if AOption.Matches(Goccia.CLI.Options.stModule) then
      Exit(Goccia.Engine.stModule);
    Exit(Goccia.Engine.stScript);
  end;

  if FindConfigEntry(AFileConfig, 'source-type', ValueStr) then
  begin
    NormalizedValue := LowerCase(Trim(ValueStr));
    if NormalizedValue = 'module' then
      Exit(Goccia.Engine.stModule);
    if NormalizedValue = 'script' then
      Exit(Goccia.Engine.stScript);
    WriteLn(ErrOutput, Format(
      'Warning: invalid per-file config value for "source-type": %s '
      + '(valid: script, module). Falling back to file extension default.',
      [ValueStr]));
    Exit(DefaultSourceTypeForFileName(AFileName));
  end;

  if AOption.Present then
  begin
    if AOption.Matches(Goccia.CLI.Options.stModule) then
      Exit(Goccia.Engine.stModule);
    Exit(Goccia.Engine.stScript);
  end;

  Result := DefaultSourceTypeForFileName(AFileName);
end;

procedure ApplyCompatibilityAndWarningFlags(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray);
var
  Compatibility: TGocciaCompatibilityFlags;
begin
  ResolveCompatibilityFlags(AEngineOptions, AFileConfig, Compatibility);
  AEngine.Compatibility := Compatibility;
  AEngine.LabelStatementsEnabled := ResolveFlagOption(
    AEngineOptions.CompatibilityFlagOption(cfLabel), AFileConfig);
  AEngine.ForInLoopsEnabled := ResolveFlagOption(
    AEngineOptions.CompatibilityFlagOption(cfForIn), AFileConfig);
  AEngine.ExperimentalJSModuleSourceEnabled := ResolveFlagOption(
    AEngineOptions.CompatibilityFlagOption(cfExperimentalJSModuleSource),
    AFileConfig);
  AEngine.WarningUnsupportedFeatures := ResolveFlagOption(
    AEngineOptions.WarningUnsupportedFeatures, AFileConfig);
end;

{ A per-file config value of a limit. Per-file values are read from the
  entries rather than applied to the options, so they are parsed here by the
  option itself, with its units and bounds. }
function ParseConfigValue(const AOption: TInt64Option;
  const AEntry: TConfigEntry): Int64;
begin
  try
    Result := AOption.Parse(AEntry.Value);
  except
    on E: EOptionValueError do
      raise TParseError.CreateFmt('Invalid value for "%s" in %s: %s (%s)',
        [AEntry.Key, AEntry.SourcePath, ConfigEntryText(AEntry), E.Reason]);
    on E: TParseError do
      raise TParseError.CreateFmt('%s: %s', [AEntry.SourcePath, E.Message]);
  end;
end;

{ Apply per-file config entries to the engine.
  Priority: CLI option > per-file config > root config > default.
  FromCommandLine distinguishes CLI-set options from root config values
  so that a per-file config can override a root-level config value. }
procedure ApplyFileConfigToEngine(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AFileName: string;
  const AAcceptedUnsafe: TGocciaUnsafeRequests);
var
  Entry: TConfigEntry;
  MemoryLimit, ResponseLimit: Int64;
  GC: TGarbageCollector;
begin
  if not Assigned(AEngineOptions) then
    Exit;

  { source-type: CLI option > per-file config > root config > .mjs > script }
  AEngine.SourceType := ResolveSourceTypeOption(
    AEngineOptions.SourceType, AFileConfig, AFileName);

  { compatibility flags: CLI flag > per-file config > root config > default (empty) }
  ApplyCompatibilityAndWarningFlags(AEngine, AEngineOptions, AFileConfig);

  { strict-types: CLI flag > per-file config > root config > default (false) }
  AEngine.StrictTypes := ResolveFlagOption(
    AEngineOptions.StrictTypes, AFileConfig);

  { unsafe-*: the command-line flag, else the governing config's request once
    it is trusted or accepted (ADR 0122). The options are RequiresTrust, so
    a root config never sets them. }
  AEngine.FunctionConstructor.Enabled :=
    AEngineOptions.UnsafeFunctionConstructor.Present or
    (gurFunctionConstructor in AAcceptedUnsafe);
  if AEngineOptions.UnsafeShadowRealm.Present or
     (gurShadowRealm in AAcceptedUnsafe) then
    EnableShadowRealm(AEngine);

  { max-memory: CLI option > per-file config > root config > system default.
    Always set explicitly so a previous file's per-file override does
    not leak into subsequent files (GC.MaxBytes is process-global). }
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    if AEngineOptions.MaxMemory.FromCommandLine then
      GC.MaxBytes := AEngineOptions.MaxMemory.Value
    else if (not AEngineOptions.MaxMemory.ConfigIgnored) and
      TryFindConfigEntry(AFileConfig, AEngineOptions.MaxMemory.LongName,
      Entry) then
    begin
      MemoryLimit := ParseConfigValue(AEngineOptions.MaxMemory, Entry);
      GC.MaxBytes := MemoryLimit;
    end
    else if AEngineOptions.MaxMemory.Present then
      GC.MaxBytes := AEngineOptions.MaxMemory.Value
    else
      GC.MaxBytes := GC.SuggestedMaxBytes;
  end;

  { max-fetch-bytes: CLI option > per-file config > root config > default.
    An engine setting, not a capability; each request carries its engine's
    value. }
  ResponseLimit := 0;
  if AEngineOptions.MaxFetchBytes.FromCommandLine then
    ResponseLimit := AEngineOptions.MaxFetchBytes.Value
  else if (not AEngineOptions.MaxFetchBytes.ConfigIgnored) and
    TryFindConfigEntry(AFileConfig, AEngineOptions.MaxFetchBytes.LongName,
    Entry) then
    ResponseLimit := ParseConfigValue(AEngineOptions.MaxFetchBytes, Entry)
  else if AEngineOptions.MaxFetchBytes.Present then
    ResponseLimit := AEngineOptions.MaxFetchBytes.Value;

  { The option's Maximum keeps the value within Integer. }
  AEngine.FetchMaxResponseBytes := Integer(ResponseLimit);
end;

procedure TGocciaCLIApplication.ConfigureCreatedEngine(
  const AEngine: TGocciaEngine; const AFileConfig: TConfigEntryArray);
begin
end;

function TGocciaCLIApplication.HonoredCapabilities:
  TGocciaHonoredCapabilities;
begin
  Result := [];
end;

function TGocciaCLIApplication.HonoredSettings: TGocciaHonoredSettings;
begin
  Result := ALL_RUNTIME_SETTINGS;
end;

function TGocciaCLIApplication.HonorsUnsafeRequests: Boolean;
begin
  Result := True;
end;

function TGocciaCLIApplication.HonorsSandboxSection: Boolean;
begin
  Result := False;
end;

function TGocciaCLIApplication.CapabilityPolicyName: string;
begin
  Result := Name;
end;

function TGocciaCLIApplication.RootConfigVerdict: TGocciaConfigTrustVerdict;
begin
  if not Assigned(FTrustGate) then
    CreateTrustGate;
  Result := FTrustGate.Verify(FRootConfigPath);
end;

function TGocciaCLIApplication.WarmUpCapabilities(
  const AFiles: TStrings): TGocciaCapabilities;
var
  I: Integer;
begin
  Result := TGocciaCapabilities.None;
  if not (gcFFI in HonoredCapabilities) then
    Exit;
  for I := 0 to AFiles.Count - 1 do
    try
      if ResolveEngineCapabilities(DiscoverFileConfigPath(AFiles[I]),
         AFiles[I]).Grants(gcFFI) then
        Exit(Result.Allow(gcFFI));
    except
      { A config error belongs to that file's own run, which reports it. }
      on E: Exception do
        Continue;
    end;
end;

function TGocciaCLIApplication.GoverningConfigPath(
  const AFileName: string): string;
begin
  { One config per file: the file's nearest config, else the root config
    when it governs the file. extends is the only way configs compose. }
  Result := DiscoverFileConfigPath(AFileName);
  if (Result = '') and RootConfigGoverns(AFileName) then
    Result := FRootConfigPath;
end;

function TGocciaCLIApplication.RootConfigGoverns(
  const AFileName: string): Boolean;
var
  FileDirectory: string;
begin
  if FRootConfigPath = '' then
    Exit(False);
  if FRootConfigExplicit or (AFileName = '') then
    Exit(True);
  FileDirectory := ExtractFileDir(ExpandFileName(AFileName));
  Result := IsPathWithinScope(CanonicalCapabilityPath(FileDirectory),
    CanonicalCapabilityPath(ExtractFileDir(FRootConfigPath)));
end;

function TGocciaCLIApplication.FileConfigVerdict(const AFileConfigPath: string;
  const AFileName: string): TGocciaConfigTrustVerdict;
var
  ConfigPath: string;
  Warnings: TGocciaCapabilityScopes;
  I: Integer;
begin
  ConfigPath := AFileConfigPath;
  if (ConfigPath = '') and RootConfigGoverns(AFileName) then
    ConfigPath := FRootConfigPath;
  if not Assigned(FTrustGate) then
    CreateTrustGate;
  Result := FTrustGate.Verify(ConfigPath);

  { Only the root config's sandbox section is read. }
  Warnings := UnsupportedRequestWarnings(Result.Request, HonoredCapabilities,
    CapabilityPolicyName, HonorsUnsafeRequests, HonorsSandboxSection and
    (FRootConfigPath <> '') and
    (Result.ConfigPath = TrustKeyForPath(FRootConfigPath)));
  for I := 0 to High(Warnings) do
    WarnOnce(Result.ConfigPath + #0 + Warnings[I],
      'Warning: ' + Result.ConfigPath + ' ' + Warnings[I]);
end;

function TGocciaCLIApplication.FilePermissionRequest(
  const AFileConfigPath: string;
  const AFileName: string): TGocciaConfigPermissionRequest;
begin
  Result := FileConfigVerdict(AFileConfigPath, AFileName).Request;
end;

function ResolveVerdictCapabilities(const AEngineOptions: TGocciaEngineOptions;
  const AVerdict: TGocciaConfigTrustVerdict;
  const AHonored: TGocciaHonoredCapabilities): TGocciaCapabilities;
var
  CapabilityOptions: TGocciaCapabilityOptions;
begin
  if Assigned(AEngineOptions) then
    CapabilityOptions := AEngineOptions.Capabilities
  else
    CapabilityOptions := nil;
  { A config's denies apply in every verdict; its allows only once trusted
    or accepted for the run. }
  Result := ResolveCapabilities(CapabilityOptions, AVerdict.Request,
    AVerdict.GrantsAccepted, AHonored, GetCurrentDir);
end;

function TGocciaCLIApplication.ResolveEngineCapabilities(
  const AFileConfigPath: string;
  const AFileName: string): TGocciaCapabilities;
begin
  Result := ResolveVerdictCapabilities(FEngineOptions,
    FileConfigVerdict(AFileConfigPath, AFileName), HonoredCapabilities);
end;

procedure TGocciaCLIApplication.VerifyGoverningConfigs(
  const AConfigPaths: TStrings);
var
  Checked: TStringList;
  Verdict: TGocciaConfigTrustVerdict;
  I: Integer;
begin
  Checked := TStringList.Create;
  try
    for I := 0 to AConfigPaths.Count - 1 do
    begin
      if AConfigPaths[I] = '' then
        Continue;
      { A config that does not load stops the run here, before any file. }
      Verdict := FileConfigVerdict(AConfigPaths[I]);
      AuditConfigVerdict(Verdict);
      Checked.Add(AConfigPaths[I]);
    end;
    FTrustGate.RequireAccepted(Checked, Name, GetCommandLineArguments,
      TrustStoreArgument);
  finally
    Checked.Free;
  end;
end;

function ConfigOutputPathProblem(const APath, AConfigPath: string): string;
var
  Directory: string;
begin
  Result := '';
  Directory := ExtractFileDir(AConfigPath);
  { A link at the name itself would redirect the write wherever it points. }
  if HostPathIsSymlink(APath) then
    Exit('is a symbolic link');
  { Existing directories along the path are resolved, so a symlinked parent
    cannot carry the write out of the config's tree. }
  if not IsPathWithinScope(CanonicalCapabilityPath(APath),
     CanonicalCapabilityPath(Directory)) then
    Result := 'is outside ' + Directory;
end;

procedure TGocciaCLIApplication.ConfineConfigOutputPaths(
  const AEntries: TConfigEntryArray);
const
  OUTPUT_MODE_JSON = 'json';
  OUTPUT_MODE_COMPACT_JSON = 'compact-json';
var
  I: Integer;
  Option: TOptionBase;
  Entry: TConfigEntry;
  Key, Value, Path, Problem: string;
begin
  for I := 0 to High(FAllOptions) do
  begin
    Option := FAllOptions[I];
    if (not Option.WritesHostFile) or (not Option.Present) or
       Option.FromCommandLine or not (Option is TStringOption) then
      Continue;
    Value := TStringOption(Option).Value;
    { Not a path: an output mode, or "derive it from the input". }
    if (Value = '') or (Value = OUTPUT_MODE_JSON) or
       (Value = OUTPUT_MODE_COMPACT_JSON) then
      Continue;
    Key := Option.LongName;
    if (Option.ConfigName <> '') and
       TryFindConfigEntry(AEntries, Option.ConfigName, Entry) then
      Key := Option.ConfigName
    else if not TryFindConfigEntry(AEntries, Option.LongName, Entry) then
      Continue;
    { Relative to the file that wrote it, like a permission scope. }
    if IsAbsoluteFilePath(Value) then
      Path := ExpandFileName(Value)
    else
      Path := ExpandFileName(IncludeTrailingPathDelimiter(
        ExtractFileDir(Entry.SourcePath)) + Value);
    Problem := ConfigOutputPathProblem(Path, Entry.SourcePath);
    if Problem <> '' then
      raise TParseError.CreateFmt('%s: "%s" writes to %s, which %s; a ' +
        'config may only write inside its own directory (pass --%s on the ' +
        'command line to write elsewhere)',
        [Entry.SourcePath, Key, Path, Problem, Option.LongName]);
    TStringOption(Option).Apply(Path);
  end;
end;

procedure TGocciaCLIApplication.EmitApplicationAudit(
  const AKind: TGocciaCapabilityKind;
  const ADecision: TGocciaCapabilityDecision;
  const ASubject, AReason: string);
var
  Event: TGocciaCapabilityAuditEvent;
begin
  if not FAuditLogOpen then
    Exit;
  Event := Default(TGocciaCapabilityAuditEvent);
  Event.Kind := AKind;
  Event.Decision := ADecision;
  Event.Subject := ASubject;
  Event.Reason := AReason;
  HandleCapabilityAudit(Event);
end;

procedure TGocciaCLIApplication.AuditConfigVerdict(
  const AVerdict: TGocciaConfigTrustVerdict);
var
  Index: Integer;
begin
  if AVerdict.State = ctsNoRequest then
    Exit;
  if FAuditedConfigs.Find(AVerdict.ConfigPath, Index) then
    Exit;
  FAuditedConfigs.Add(AVerdict.ConfigPath);
  if ConfigTrustAuditAllows(AVerdict) then
    EmitApplicationAudit(gckConfigPermissions, gcdAllow, AVerdict.ConfigPath,
      ConfigTrustAuditReason(AVerdict, Name))
  else
    EmitApplicationAudit(gckConfigPermissions, gcdDeny, AVerdict.ConfigPath,
      ConfigTrustAuditReason(AVerdict, Name));
end;

function TGocciaCLIApplication.CommandLineGrantDescription: string;

  procedure AddFlag(const AText: string);
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + AText;
  end;

  procedure AddScopeOption(const AOption: TScopeListOption);
  var
    Scopes: string;
    I: Integer;
  begin
    if not AOption.Present then
      Exit;
    if AOption.Unscoped then
      AddFlag('--' + AOption.LongName);
    if AOption.Scopes.Count = 0 then
      Exit;
    Scopes := '';
    for I := 0 to AOption.Scopes.Count - 1 do
    begin
      if I > 0 then
        Scopes := Scopes + ',';
      Scopes := Scopes + AOption.Scopes[I];
    end;
    AddFlag('--' + AOption.LongName + '=' + Scopes);
  end;

var
  Capability: TGocciaCapability;
begin
  Result := '';
  if not Assigned(FEngineOptions) then
    Exit;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    AddScopeOption(FEngineOptions.Capabilities.AllowOption(Capability));
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    AddScopeOption(FEngineOptions.Capabilities.DenyOption(Capability));
  if FEngineOptions.UnsafeFunctionConstructor.Present then
    AddFlag('--' + FEngineOptions.UnsafeFunctionConstructor.LongName);
  if FEngineOptions.UnsafeShadowRealm.Present then
    AddFlag('--' + FEngineOptions.UnsafeShadowRealm.LongName);
end;

function TGocciaCLIApplication.CapabilityProvenance(
  const AVerdict: TGocciaConfigTrustVerdict): string;
var
  CommandLine: string;
begin
  Result := '';
  CommandLine := CommandLineGrantDescription;
  if CommandLine <> '' then
    Result := 'cli ' + CommandLine;
  if AVerdict.State <> ctsNoRequest then
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + 'config ' + AVerdict.ConfigPath + ' ' +
      ConfigTrustAuditReason(AVerdict, Name);
  end
  else if AVerdict.Request.DeclaresDenies then
  begin
    { A deny-only config needs no trust, but its denies shape the set. }
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + 'config ' + AVerdict.ConfigPath + ' denies only';
  end;
  if Result = '' then
    Result := 'defaults';
end;

procedure TGocciaCLIApplication.ConfigureCapabilityAudit(
  const AEngine: TGocciaEngine);
begin
  if FAuditLogOpen then
  begin
    AEngine.CapabilityAuditSink := HandleCapabilityAudit;
    AEngine.AuditEffectiveCapabilities;
  end;
end;

procedure InjectModulesFromFileSystemModule(const AEngine: TGocciaEngine;
  const APath: string);
var
  DefaultValue: TGocciaValue;
  ManifestJSON: string;
  ManifestLoader: TGocciaModuleLoader;
  ManifestModule: TGocciaModule;
  Stringifier: TGocciaJSONStringifier;
begin
  ManifestLoader := TGocciaModuleLoader.Create(APath);
  try
    ManifestLoader.SetContentProvider(
      TGocciaFileSystemModuleContentProvider.Create, True);
    ManifestLoader.Preprocessors := AEngine.ModuleLoader.Preprocessors;
    ManifestLoader.Compatibility := AEngine.ModuleLoader.Compatibility;
    ManifestLoader.LabelStatementsEnabled :=
      AEngine.ModuleLoader.LabelStatementsEnabled;
    ManifestLoader.ForInLoopsEnabled := AEngine.ModuleLoader.ForInLoopsEnabled;
    ManifestLoader.ExperimentalJSModuleSourceEnabled :=
      AEngine.ModuleLoader.ExperimentalJSModuleSourceEnabled;
    ManifestLoader.WarningUnsupportedFeatures :=
      AEngine.ModuleLoader.WarningUnsupportedFeatures;
    ManifestLoader.StrictTypesEnabled :=
      AEngine.ModuleLoader.StrictTypesEnabled;
    ManifestLoader.EvaluateModuleBody :=
      AEngine.ModuleLoader.EvaluateModuleBody;
    ManifestLoader.BindRuntime(AEngine.Interpreter.GlobalScope,
      AEngine.ThrowError);

    ManifestModule := ManifestLoader.LoadHostModule(APath, APath);
    if not ManifestModule.TryGetExportValue(KEYWORD_DEFAULT, DefaultValue) then
      raise EArgumentException.Create(
        'Virtual modules manifest module must have a default export.');
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.AddTempRoot(DefaultValue);
    try
      Stringifier := TGocciaJSONStringifier.Create;
      try
        ManifestJSON := Stringifier.Stringify(DefaultValue);
      finally
        Stringifier.Free;
      end;
      AEngine.InjectModulesFromJSON(ManifestJSON, ManifestModule.Path);
    finally
      if (TGarbageCollector.Instance <> nil) then
        TGarbageCollector.Instance.RemoveTempRoot(DefaultValue);
    end;
  finally
    ManifestLoader.Free;
  end;
end;

procedure InjectModulesFromManifestFile(const AEngine: TGocciaEngine;
  const APath: string);
var
  Content, Extension: string;
  JSON5Parser: TGocciaJSON5Parser;
  ParsedValue: TGocciaValue;
  Stringifier: TGocciaJSONStringifier;
  TOMLParser: TGocciaTOMLParser;
  YAMLParser: TGocciaYAMLParser;
begin
  Extension := LowerCase(ExtractFileExt(APath));
  if (Extension = '.js') or (Extension = '.mjs') or
     (Extension = '.ts') then
  begin
    { The manifest is a host file named by the host, so it always loads from
      the host filesystem. An engine whose own provider does not read the
      host (GocciaRunner's sandbox mode) cannot evaluate it in place.
      Evaluated through the engine's own loader, the manifest and its
      imports become host-owned there, so anything it leaves behind (a global function that calls
      import(), say) would import as the host; under an outright read deny
      (--deny-read) it is therefore evaluated in an isolated loader
      too, and a later import made from its code is a guest read the deny
      refuses. }
    if AEngine.Capabilities.DeniesAll(gcRead) or
       not AEngine.ContentProvider.ReadsHostFileSystem then
      InjectModulesFromFileSystemModule(AEngine, APath)
    else
      AEngine.InjectModulesFromModule(APath);
    Exit;
  end;

  Content := ReadUTF8FileText(APath);
  if Extension = EXT_JSON then
  begin
    AEngine.InjectModulesFromJSON(Content, APath);
    Exit;
  end;

  ParsedValue := nil;
  if Extension = EXT_JSON5 then
  begin
    JSON5Parser := TGocciaJSON5Parser.Create;
    try
      ParsedValue := JSON5Parser.Parse(Content);
    finally
      JSON5Parser.Free;
    end;
  end
  else if Extension = EXT_TOML then
  begin
    TOMLParser := TGocciaTOMLParser.Create;
    try
      ParsedValue := TOMLParser.Parse(Content);
    finally
      TOMLParser.Free;
    end;
  end
  else if (Extension = '.yaml') or (Extension = '.yml') then
  begin
    YAMLParser := TGocciaYAMLParser.Create;
    try
      ParsedValue := YAMLParser.Parse(string(Content));
    finally
      YAMLParser.Free;
    end;
  end
  else
    raise Exception.CreateFmt(
      'Unsupported virtual modules manifest format: %s', [APath]);

  if (TGarbageCollector.Instance <> nil) and Assigned(ParsedValue) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Stringifier := TGocciaJSONStringifier.Create;
    try
      AEngine.InjectModulesFromJSON(Stringifier.Stringify(ParsedValue), APath);
    finally
      Stringifier.Free;
    end;
  finally
    if (TGarbageCollector.Instance <> nil) and Assigned(ParsedValue) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure InjectInlineModuleDefinition(const AEngine: TGocciaEngine;
  const ADefinition, ABaseAddress: string); forward;

{ A manifest a config file names is the repository's choice, not the user's,
  so it is read under the capability set of the script the config governs
  (ADR 0122): a file inside the project is part of the module graph, anything
  else needs a read grant, and a read deny refuses it. ASpecifier is the path
  as the config wrote it, which is all the refusal names. }
procedure CheckConfiguredManifestRead(const AEngine: TGocciaEngine;
  const APath, ASpecifier: string);
var
  CanonicalPath: string;
  InProject: Boolean;
begin
  CanonicalPath := CanonicalCapabilityPath(APath);
  InProject := (AEngine.ProjectRoot <> '') and
    IsPathWithinScope(CanonicalPath, AEngine.ProjectRoot);
  if AEngine.Capabilities.DeniesPath(gcRead, CanonicalPath) then
  begin
    AEngine.EmitCapabilityAudit(gckReadFile, gcdDeny, CanonicalPath,
      'read is denied for this path');
    ThrowPermissionDenied(CapabilityName(gcRead), ASpecifier,
      Format(SSuggestReadDenied, [CanonicalPath]));
  end;
  if InProject then
    Exit;
  if not AEngine.Capabilities.AllowsPath(gcRead, CanonicalPath) then
  begin
    AEngine.EmitCapabilityAudit(gckReadFile, gcdDeny, CanonicalPath,
      'the path is outside the project and no read grant covers it');
    ThrowPermissionDenied(CapabilityName(gcRead), ASpecifier,
      Format(SSuggestReadNotGranted, [CanonicalPath,
        ExtractFileDir(CanonicalPath)]));
  end;
  AEngine.EmitCapabilityAudit(gckReadFile, gcdAllow, CanonicalPath,
    'a read grant covers the path');
end;

{ A JavaScript or TypeScript manifest a config file names runs in an engine
  of its own, with the capability set and project of the script it governs:
  its imports are guest reads, judged like the script's, and whatever it
  leaves on its global object stays there. Only its default export, as data,
  reaches the script's engine. }
procedure InjectModulesFromIsolatedManifest(const AEngine: TGocciaEngine;
  const APath: string);
var
  Isolated: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
  Module: TGocciaModule;
  DefaultValue: TGocciaValue;
  Stringifier: TGocciaJSONStringifier;
  ManifestJSON, ModulePath: string;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Isolated := TGocciaEngine.Create(APath, Source, Executor,
      AEngine.Capabilities);
    try
      Isolated.ProjectRoot := AEngine.ProjectRoot;
      Isolated.ConfigureCapabilityAuditAsChildOf(AEngine);
      Isolated.Preprocessors := AEngine.Preprocessors;
      Isolated.Compatibility := AEngine.Compatibility;
      Isolated.LabelStatementsEnabled := AEngine.LabelStatementsEnabled;
      Isolated.ForInLoopsEnabled := AEngine.ForInLoopsEnabled;
      Isolated.StrictTypes := AEngine.StrictTypes;
      { The filesystem content provider, with every read it makes checked. }
      AttachRuntime(Isolated);
      Module := Isolated.ModuleLoader.LoadModule(APath, APath);
      if not Module.TryGetExportValue(KEYWORD_DEFAULT, DefaultValue) then
        raise EArgumentException.Create(
          'Virtual modules manifest module must have a default export.');
      ModulePath := Module.Path;
      if TGarbageCollector.Instance <> nil then
        TGarbageCollector.Instance.AddTempRoot(DefaultValue);
      try
        Stringifier := TGocciaJSONStringifier.Create;
        try
          ManifestJSON := Stringifier.Stringify(DefaultValue);
        finally
          Stringifier.Free;
        end;
      finally
        if TGarbageCollector.Instance <> nil then
          TGarbageCollector.Instance.RemoveTempRoot(DefaultValue);
      end;
    finally
      Isolated.Free;
    end;
  finally
    Executor.Free;
    Source.Free;
  end;
  AEngine.InjectModulesFromJSON(ManifestJSON, ModulePath);
end;

{ A manifest path from a config file's "modules" key. }
procedure InjectConfiguredManifest(const AEngine: TGocciaEngine;
  const APath, ASpecifier: string);
var
  Extension: string;
begin
  CheckConfiguredManifestRead(AEngine, APath, ASpecifier);
  Extension := LowerCase(ExtractFileExt(APath));
  if (Extension = '.js') or (Extension = '.mjs') or (Extension = '.ts') then
    InjectModulesFromIsolatedManifest(AEngine, APath)
  else
    InjectModulesFromManifestFile(AEngine, APath);
end;

procedure InjectModulesFromConfigFile(const AEngine: TGocciaEngine;
  const APath: string; const ADepth: Integer = 0);
var
  Content, Extension, ExtendsPath, ItemPath, Written: string;
  I: Integer;
  ArrayValue: TGocciaArrayValue;
  ModulesValue, ParsedValue: TGocciaValue;
  ObjectValue: TGocciaObjectValue;
  JSONParser: TGocciaJSONParser;
  JSON5Parser: TGocciaJSON5Parser;
  TOMLParser: TGocciaTOMLParser;
  Stringifier: TGocciaJSONStringifier;
begin
  if (APath = '') or not FileExists(APath) then
    Exit;
  if ADepth > 32 then
    raise Exception.Create(
      'Circular or too-deeply-nested extends in virtual module config: ' +
      APath);
  Content := ReadUTF8FileText(APath);
  Extension := LowerCase(ExtractFileExt(APath));
  ParsedValue := nil;
  if Extension = EXT_JSON then
  begin
    JSONParser := TGocciaJSONParser.Create;
    try
      ParsedValue := JSONParser.Parse(Content);
    finally
      JSONParser.Free;
    end;
  end
  else if Extension = EXT_JSON5 then
  begin
    JSON5Parser := TGocciaJSON5Parser.Create;
    try
      ParsedValue := JSON5Parser.Parse(Content);
    finally
      JSON5Parser.Free;
    end;
  end
  else if Extension = EXT_TOML then
  begin
    TOMLParser := TGocciaTOMLParser.Create;
    try
      ParsedValue := TOMLParser.Parse(Content);
    finally
      TOMLParser.Free;
    end;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    Exit;
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    ObjectValue := TGocciaObjectValue(ParsedValue);
    if ObjectValue.HasOwnProperty('extends') and
       (ObjectValue.GetProperty('extends') is TGocciaStringLiteralValue) then
    begin
      ExtendsPath := TGocciaStringLiteralValue(
        ObjectValue.GetProperty('extends')).Value;
      if not IsAbsoluteFilePath(ExtendsPath) then
        ExtendsPath := ExpandFileName(
          IncludeTrailingPathDelimiter(ExtractFilePath(APath)) + ExtendsPath);
      InjectModulesFromConfigFile(AEngine, ExtendsPath, ADepth + 1);
    end;

    if ObjectValue.HasOwnProperty('modules') then
    begin
      ModulesValue := ObjectValue.GetProperty('modules');
      if ModulesValue is TGocciaStringLiteralValue then
      begin
        ItemPath := TGocciaStringLiteralValue(ModulesValue).Value;
        Written := ItemPath;
        if not IsAbsoluteFilePath(ItemPath) then
          ItemPath := ExpandFileName(
            IncludeTrailingPathDelimiter(ExtractFilePath(APath)) + ItemPath);
        InjectConfiguredManifest(AEngine, ItemPath, Written);
      end
      else if ModulesValue is TGocciaArrayValue then
      begin
        ArrayValue := TGocciaArrayValue(ModulesValue);
        for I := 0 to ArrayValue.GetLength - 1 do
        begin
          ModulesValue := ArrayValue.GetElement(I);
          if not (ModulesValue is TGocciaStringLiteralValue) then
            raise EArgumentException.Create(
              'Config modules manifest paths must be strings.');
          ItemPath := TGocciaStringLiteralValue(ModulesValue).Value;
          Written := ItemPath;
          if not IsAbsoluteFilePath(ItemPath) then
            ItemPath := ExpandFileName(
              IncludeTrailingPathDelimiter(ExtractFilePath(APath)) +
              ItemPath);
          InjectConfiguredManifest(AEngine, ItemPath, Written);
        end;
      end
      else if ModulesValue is TGocciaObjectValue then
      begin
        Stringifier := TGocciaJSONStringifier.Create;
        try
          AEngine.InjectModulesFromJSON(Stringifier.Stringify(ModulesValue),
            APath);
        finally
          Stringifier.Free;
        end;
      end
      else
        raise EArgumentException.Create(
          'Config modules must be a descriptor object, path, or path array.');
    end;

    if ObjectValue.HasOwnProperty('module') then
    begin
      ModulesValue := ObjectValue.GetProperty('module');
      if ModulesValue is TGocciaStringLiteralValue then
        InjectInlineModuleDefinition(AEngine,
          TGocciaStringLiteralValue(ModulesValue).Value, APath)
      else if ModulesValue is TGocciaArrayValue then
      begin
        ArrayValue := TGocciaArrayValue(ModulesValue);
        for I := 0 to ArrayValue.GetLength - 1 do
        begin
          ModulesValue := ArrayValue.GetElement(I);
          if not (ModulesValue is TGocciaStringLiteralValue) then
            raise EArgumentException.Create(
              'Config module definitions must be strings.');
          InjectInlineModuleDefinition(AEngine,
            TGocciaStringLiteralValue(ModulesValue).Value, APath);
        end;
      end
      else
        raise EArgumentException.Create(
          'Config module must be a definition string or string array.');
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure InjectInlineModuleDefinition(const AEngine: TGocciaEngine;
  const ADefinition, ABaseAddress: string);
var
  Address, Content: string;
  Separator: NativeInt;
begin
  Separator := Pos('=', ADefinition);
  if Separator <= 1 then
    raise Exception.CreateFmt(
      'Invalid --module value "%s"; expected name=source or name={descriptor}.',
      [ADefinition]);
  Address := Copy(ADefinition, 1, Separator - 1);
  Content := Copy(ADefinition, Separator + 1, MaxInt);
  if (Content <> '') and (Content[1] = '{') then
    AEngine.InjectModulesFromJSON('{"' + EscapeJSONString(Address) + '":' +
      Content + '}', ABaseAddress)
  else
    AEngine.InjectModule(Address, Content, 'javascript', ABaseAddress);
end;

procedure ApplyConfiguredVirtualModules(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions; const ARootConfigPath,
  AFileConfigPath: string);
var
  ManifestPath: string;

  procedure ApplyManifests;
  var
    ManifestIndex: Integer;
  begin
    for ManifestIndex := 0 to
      AEngineOptions.ModuleManifests.Values.Count - 1 do
    begin
      ManifestPath :=
        AEngineOptions.ModuleManifests.Values[ManifestIndex];
      if not IsAbsoluteFilePath(ManifestPath) then
        ManifestPath := ExpandFileName(ManifestPath);
      InjectModulesFromManifestFile(AEngine, ManifestPath);
    end;
  end;

  procedure ApplyDefinitions;
  var
    DefinitionIndex: Integer;
  begin
    for DefinitionIndex := 0 to
      AEngineOptions.ModuleDefinitions.Values.Count - 1 do
      InjectInlineModuleDefinition(AEngine,
        AEngineOptions.ModuleDefinitions.Values[DefinitionIndex],
        GetCurrentDir);
  end;
begin
  InjectModulesFromConfigFile(AEngine, ARootConfigPath);

  if (AFileConfigPath <> '') and
     (ExpandFileName(AFileConfigPath) <> ExpandFileName(ARootConfigPath)) then
    InjectModulesFromConfigFile(AEngine, AFileConfigPath);

  if not Assigned(AEngineOptions) then
    Exit;
  if AEngineOptions.ModuleManifests.FromCommandLine then
    ApplyManifests;
  if AEngineOptions.ModuleDefinitions.FromCommandLine then
    ApplyDefinitions;
end;

procedure TGocciaCLIApplication.ApplyVirtualModulesToEngine(
  const AEngine: TGocciaEngine; const AFileConfigPath: string);
begin
  ApplyConfiguredVirtualModules(AEngine, FEngineOptions, FRootConfigPath,
    AFileConfigPath);
end;

function TGocciaCLIApplication.ShouldApplyRootConfig(
  const APaths: TStringList; const AConfigPath: string;
  const AExplicitConfig: Boolean): Boolean;
begin
  Result := True;
end;

function TGocciaCLIApplication.CreateEngine(const AFileName: string;
  const ASource: TStringList; const AExecutor: TGocciaExecutor): TGocciaEngine;
var
  AliasBaseDirectory: string;
  FileConfig: TConfigEntryArray;
  FileConfigPath: string;
  Verdict: TGocciaConfigTrustVerdict;
begin
  FileConfigPath := DiscoverFileConfigPath(AFileName);
  if FileConfigPath <> '' then
    FileConfig := LoadFileConfig(FileConfigPath)
  else
    SetLength(FileConfig, 0);
  Verdict := FileConfigVerdict(FileConfigPath, AFileName);
  { The capability set is fixed when the engine is created (ADR 0122). }
  Result := TGocciaEngine.Create(AFileName, ASource, AExecutor,
    ResolveVerdictCapabilities(FEngineOptions, Verdict, HonoredCapabilities));
  try
    Result.CapabilityProvenance := CapabilityProvenance(Verdict);
    ConfigureCapabilityAudit(Result);
    if Assigned(FEngineOptions) then
    begin
      if FEngineOptions.Aliases.FromCommandLine or
         (FRootConfigPath = '') then
        AliasBaseDirectory := GetCurrentDir
      else
        AliasBaseDirectory := ExtractFilePath(FRootConfigPath);
      ConfigureModuleResolver(Result.Resolver, AFileName,
        FEngineOptions.ImportMap.ValueOr(''), FEngineOptions.Aliases.Values,
        AliasBaseDirectory);
      if ResolveFlagOption(FEngineOptions.Deterministic, FileConfig) then
        Result.HostEnvironment.UseDeterministicProfile;
    end;
    ConfigureCreatedEngine(Result, FileConfig);
    if Assigned(FEngineOptions) then
      { One config per file: a file with its own config takes its unsafe-*
        keys from that config (and its extends chain) alone; the root config
        fills in only for a file without one, inside its tree. }
      ApplyFileConfigToEngine(Result, FEngineOptions, FileConfig, AFileName,
        Verdict.AcceptedUnsafe);
    ApplyVirtualModulesToEngine(Result, FileConfigPath);
    if AExecutor is TGocciaBytecodeExecutor then
      TGocciaBytecodeExecutor(AExecutor).GlobalBackedTopLevel :=
        Result.SourceType = Goccia.Engine.stScript;
  except
    Result.Free;
    raise;
  end;
end;

procedure TGocciaCLIApplication.HandleConsoleLog(const AMethod, ALine: string);
begin
  CriticalSectionEnter(FLogLock);
  try
    WriteLn(FLogFileHandle, '[' + AMethod + '] ' + ALine);
  finally
    CriticalSectionLeave(FLogLock);
  end;
end;

procedure TGocciaCLIApplication.OpenLogFile;
begin
  if FLogFileOpen then
    Exit;
  CriticalSectionInit(FLogLock);
  try
    AssignFile(FLogFileHandle, FLog.Value);
    Rewrite(FLogFileHandle);
    FLogFileOpen := True;
  except
    CriticalSectionDone(FLogLock);
    raise;
  end;
end;

procedure TGocciaCLIApplication.CloseLogFile;
begin
  if not FLogFileOpen then
    Exit;
  try
    CloseFile(FLogFileHandle);
  finally
    FLogFileOpen := False;
    CriticalSectionDone(FLogLock);
  end;
end;

procedure TGocciaCLIApplication.HandleCapabilityAudit(
  const AEvent: TGocciaCapabilityAuditEvent);
var
  Line: TBytes;
begin
  Line := EncodeUTF8WithReplacement(AEvent.ToJSON + sLineBreak);
  CriticalSectionEnter(FAuditLogLock);
  try
    if Length(Line) > 0 then
      FAuditLogStream.WriteBuffer(Line[0], Length(Line));
  finally
    CriticalSectionLeave(FAuditLogLock);
  end;
end;

procedure TGocciaCLIApplication.OpenAuditLog;
begin
  if FAuditLogOpen then
    Exit;
  CriticalSectionInit(FAuditLogLock);
  try
    FAuditLogStream := TFileStream.Create(FAuditLog.Value, fmCreate);
    FAuditLogOpen := True;
  except
    FAuditLogStream.Free;
    FAuditLogStream := nil;
    CriticalSectionDone(FAuditLogLock);
    raise;
  end;
end;

procedure TGocciaCLIApplication.CloseAuditLog;
begin
  if not FAuditLogOpen then
    Exit;
  try
    FreeAndNil(FAuditLogStream);
  finally
    FAuditLogOpen := False;
    CriticalSectionDone(FAuditLogLock);
  end;
end;

procedure TGocciaCLIApplication.ValidateOutputPaths;
var
  LogPath: string;
  AuditPath: string;
begin
  if not FLog.Present or not FAuditLog.Present then
    Exit;

  LogPath := ExpandFileName(FLog.Value);
  AuditPath := ExpandFileName(FAuditLog.Value);
  {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
  if SameText(LogPath, AuditPath) then
  {$ELSE}
  if LogPath = AuditPath then
  {$ENDIF}
    raise Exception.Create(
      '--log and --audit-log must write to different files');
end;

procedure TGocciaCLIApplication.Validate;
begin
  // Override point for subclasses
end;

function TGocciaCLIApplication.StdinUsage: TGocciaStdinUsage;
begin
  Result := suNone;
end;

function TGocciaCLIApplication.HasNonPathInput: Boolean;
begin
  Result := False;
end;

function TGocciaCLIApplication.ExtraHelpText: string;
begin
  Result := '';
end;

procedure TGocciaCLIApplication.ValidateCommandLine(const APaths: TStringList);
begin
  // Override point for subclasses
end;

procedure TGocciaCLIApplication.AfterExecute;
begin
  // Override point for subclasses
end;

procedure InitializeCoverageIfEnabled(const AOptions: TGocciaCoverageOptions);
begin
  if AOptions.Enabled.Present or AOptions.Format.Present or
     AOptions.OutputPath.Present then
  begin
    TGocciaCoverageTracker.Initialize;
    TGocciaCoverageTracker.Instance.Enabled := True;
  end;
end;

procedure ShutdownCoverageIfEnabled(const AOptions: TGocciaCoverageOptions);
begin
  if (TGocciaCoverageTracker.Instance <> nil) then
    TGocciaCoverageTracker.Shutdown;
end;

procedure InitializeProfilerIfEnabled(const AOptions: TGocciaProfilerOptions);
begin
  if AOptions.Mode.Present then
  begin
    TGocciaProfiler.Initialize;
    TGocciaProfiler.Instance.Enabled := True;

    case AOptions.Mode.Value of
      Goccia.CLI.Options.pmOpcodes:
        TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes];
      Goccia.CLI.Options.pmFunctions:
        TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmFunctions];
      Goccia.CLI.Options.pmAll:
        TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes,
          Goccia.Profiler.pmFunctions];
    end;
  end;
end;

procedure ShutdownProfilerIfEnabled(const AOptions: TGocciaProfilerOptions);
begin
  if (TGocciaProfiler.Instance <> nil) then
    TGocciaProfiler.Shutdown;
end;

function TGocciaCLIApplication.GetJobCount(const AFileCount: Integer): Integer;
begin
  if AFileCount <= 1 then
    Exit(1);
  if Assigned(FJobs) and FJobs.Present then
    Result := Max(1, FJobs.Value)
  else
    Result := GetProcessorCount;
  Result := Min(Result, AFileCount);
end;

procedure TGocciaCLIApplication.InitializeSingletons;
begin
  SetMaxStackDepth(DEFAULT_MAX_STACK_DEPTH);
  SetInspectDepth(DEFAULT_INSPECT_DEPTH);
  if Assigned(FEngineOptions) then
  begin
    { MaxStack.Maximum keeps the value within Integer. }
    SetMaxStackDepth(Integer(FEngineOptions.MaxStack.ValueOr(
      DEFAULT_MAX_STACK_DEPTH)));
    SetInspectDepth(FEngineOptions.InspectDepth.ValueOr(DEFAULT_INSPECT_DEPTH));
  end;
  if Assigned(FCoverageOptions) then
    InitializeCoverageIfEnabled(FCoverageOptions);
  if Assigned(FProfilerOptions) then
    InitializeProfilerIfEnabled(FProfilerOptions);
end;

procedure TGocciaCLIApplication.ShutdownSingletons;
begin
  if Assigned(FProfilerOptions) then
    ShutdownProfilerIfEnabled(FProfilerOptions);
  if Assigned(FCoverageOptions) then
    ShutdownCoverageIfEnabled(FCoverageOptions);
end;

function ResolveConfigStartDirectory(const APaths: TStringList): string;
var
  FirstPath: string;
begin
  if APaths.Count > 0 then
  begin
    FirstPath := ExpandFileName(APaths[0]);
    if DirectoryExists(FirstPath) then
      Exit(FirstPath);
    if FileExists(FirstPath) or (ExtractFilePath(FirstPath) <> '') then
      Exit(ExtractFilePath(FirstPath));
  end;
  Result := GetCurrentDir;
end;

function TGocciaCLIApplication.MultifileEnabled: Boolean;
begin
  Result := Assigned(FMultifile) and FMultifile.Present;
end;

function TGocciaCLIApplication.SourceRegistry: TGocciaSourceRegistry;
begin
  // Eagerly constructed in Create — guaranteed non-nil, no race.
  Result := FSourceRegistry;
end;

function TGocciaCLIApplication.ExpandMultifileFiles(
  const AFiles: TStringList): TStringList;
var
  I, PartIndex: Integer;
  FileName, Extension, SectionName: string;
  RawSource: string;
  FullSource: TStringList;
  Sections: TObjectList<TStringList>;
  Section: TStringList;
begin
  Result := TStringList.Create;
  try
    if not MultifileEnabled then
    begin
      Result.AddStrings(AFiles);
      ValidateFileConfigs(Result);
      Exit;
    end;

    for I := 0 to AFiles.Count - 1 do
    begin
      FileName := AFiles[I];
      Extension := LowerCase(ExtractFileExt(FileName));

      // .gbc files are bytecode; multifile splitting does not apply.
      if Extension = EXT_GBC then
      begin
        Result.Add(FileName);
        Continue;
      end;

      try
        RawSource := ReadUTF8FileText(FileName);
      except
        // Fall through to the runner's own load-error handling.
        Result.Add(FileName);
        Continue;
      end;

      FullSource := CreateFileTextLines(RawSource);
      Sections := nil;
      try
        // Only treat as multifile when the input actually contains the
        // separator.  Sections.Count <= 1 alone is wrong: a file with
        // separators that trims down to a single non-empty section would
        // incorrectly fall through to the pass-through branch.
        if not ContainsMultifileSeparator(FullSource) then
        begin
          Result.Add(FileName);
          Continue;
        end;

        Sections := SplitMultifileSource(FullSource);

        // Pure-separator inputs (no surviving sections) pass the original
        // name through so the runner reports the source consistently —
        // matches pre-fix behaviour for that edge case.
        if Sections.Count = 0 then
        begin
          Result.Add(FileName);
          Continue;
        end;

        // Always extract index 0 — extraction shifts remaining items down.
        PartIndex := 0;
        while Sections.Count > 0 do
        begin
          Section := Sections.Extract(Sections[0]);
          Inc(PartIndex);
          SectionName := BuildMultifileSectionName(FileName, PartIndex);
          SourceRegistry.Register(SectionName, Section);
          Result.Add(SectionName);
        end;
      finally
        Sections.Free;
        FullSource.Free;
      end;
    end;
    ValidateFileConfigs(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TGocciaCLIApplication.SplitStdinMultifile(
  const AStdinSource: TStringList): TStringList;
var
  Sections: TObjectList<TStringList>;
  Section: TStringList;
  SectionName: string;
  PartIndex: Integer;
begin
  Result := TStringList.Create;
  try
    if not MultifileEnabled then
    begin
      SourceRegistry.Register(STDIN_FILE_NAME, AStdinSource);
      Result.Add(STDIN_FILE_NAME);
      Exit;
    end;

    // Only treat as multifile when the input actually contains the
    // separator.  See ExpandMultifileFiles for why "section count" is
    // not a reliable proxy.
    if not ContainsMultifileSeparator(AStdinSource) then
    begin
      SourceRegistry.Register(STDIN_FILE_NAME, AStdinSource);
      Result.Add(STDIN_FILE_NAME);
      Exit;
    end;

    Sections := SplitMultifileSource(AStdinSource);
    try
      if Sections.Count = 0 then
      begin
        // Pure-separator stdin: register the (now empty-ish) source
        // under the canonical stdin name to keep ownership clear.
        SourceRegistry.Register(STDIN_FILE_NAME, AStdinSource);
        Result.Add(STDIN_FILE_NAME);
        Exit;
      end;

      // Register each section, then free the original wrapper since
      // its contents have been redistributed into sections.
      // Always extract index 0 — extraction shifts remaining items down.
      PartIndex := 0;
      while Sections.Count > 0 do
      begin
        Section := Sections.Extract(Sections[0]);
        Inc(PartIndex);
        SectionName := BuildMultifileSectionName(STDIN_FILE_NAME,
          PartIndex);
        SourceRegistry.Register(SectionName, Section);
        Result.Add(SectionName);
      end;
      AStdinSource.Free;
    finally
      Sections.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ Resolve an explicit --config value to a concrete file path.
  Accepts either a path to a config file or to a directory containing
  goccia.toml / goccia.json5 / goccia.json (priority order).  The
  directory form checks only that directory and does not walk upward,
  so the user gets exactly what they asked for and a typo errors out
  rather than silently picking up a parent's config.  Raises when the
  path does not exist or when a directory contains no recognised
  config file. }
function ResolveExplicitConfigPath(const AValue: string): string;
var
  Expanded, Candidate: string;
  E: Integer;
begin
  Expanded := ExpandFileName(AValue);

  if DirectoryExists(Expanded) then
  begin
    for E := 0 to High(CONFIG_FILE_EXTENSIONS) do
    begin
      Candidate := IncludeTrailingPathDelimiter(Expanded) +
        CONFIG_FILE_BASE_NAME + CONFIG_FILE_EXTENSIONS[E];
      if FileExists(Candidate) then
        Exit(Candidate);
    end;
    raise Exception.CreateFmt(
      'No %s.{toml,json5,json} found in config directory: %s',
      [CONFIG_FILE_BASE_NAME, AValue]);
  end;

  if FileExists(Expanded) then
    Exit(Expanded);

  raise Exception.CreateFmt('Config path not found: %s', [AValue]);
end;

procedure TGocciaCLIApplication.Execute;
var
  Paths: TStringList;
  ConfigPath, ConfigStartDir: string;
  RootConfigEntries: TConfigEntryArray;
  I: Integer;

  { Built on demand — the common case never renders help at all. }
  function BuildHelpText: string;
  begin
    Result := GenerateHelpText(Name, UsageLine, FAllOptions);
    if StdinUsage <> suNone then
      Result := Result + sLineBreak + StdinUsageNote(Name, StdinUsage);
    if ExtraHelpText <> '' then
      Result := Result + sLineBreak + ExtraHelpText;
  end;

begin
  Configure;

  FHelp := TFlagOption.Create('help', 'Show this help message');
  FHelp.ShortName := 'h';

  FJobs := TIntegerOption.Create('jobs', 'Number of parallel worker threads');
  FJobs.ShortName := 'j';

  FLog := TStringOption.Create('log', 'Write console output to a log file');
  FAuditLog := TStringOption.Create('audit-log',
    'Write capability audit events as JSON Lines');
  FLog.WritesHostFile := True;
  FAuditLog.WritesHostFile := True;

  FMultifile := TFlagOption.Create('multifile',
    'Split each input (file or stdin) on "---" lines and run each ' +
    'section as an independent file');

  FConfig := TStringOption.Create('config',
    'Path to a config file or a directory containing one (skips auto-discovery)');

  CreateTrustOptions;

  if Assigned(FEngineOptions) then
  begin
    FEngineOptions.Capabilities.HideUnsupported(HonoredCapabilities);
    FEngineOptions.HideUnsupportedSettings(HonoredSettings);
  end;

  BuildAllOptions;
  // Append common application options after BuildAllOptions so
  // they appear in help
  SetLength(FAllOptions, Length(FAllOptions) + 5);
  FAllOptions[High(FAllOptions) - 4] := FJobs;
  FAllOptions[High(FAllOptions) - 3] := FLog;
  FAllOptions[High(FAllOptions) - 2] := FAuditLog;
  FAllOptions[High(FAllOptions) - 1] := FMultifile;
  FAllOptions[High(FAllOptions)] := FConfig;
  FAllOptions := ConcatOptions([FAllOptions, TrustOptions]);

  { Parse CLI first so we know the entry file. }
  Paths := ParseCommandLine(FAllOptions);
  try
    if FHelp.Present then
    begin
      Write(BuildHelpText);
      Exit;
    end;

    { --trust, --untrust, and --list-trusted manage the trust store and run
      nothing, so they come before the no-argument rule. }
    ValidateTrustOptions(Paths);
    if TrustModeCount > 0 then
    begin
      RunTrustMode;
      Exit;
    end;

    { clig.dev: "If your command is expecting to have something piped
      to it and stdin is an interactive terminal, display help
      immediately and quit."  Without this, a bare invocation at a
      terminal blocks on ReadLn until the platform's end-of-input keys
      (EndOfInputKeys) are pressed and looks stuck.  Help
      goes to stderr because this is an error, not a request for help,
      which also keeps stdout clean for --output=json callers. }
    if (StdinUsage <> suNone) and
       (DecideStdinInput((Paths.Count > 0) or HasNonPathInput,
          (Paths.Count = 1) and IsStdinPath(Paths[0]),
          IsInputTerminal) = sdShowUsage) then
    begin
      Write(ErrOutput, BuildHelpText);
      WriteLn(ErrOutput);
      Write(ErrOutput, NoInputAtTerminalMessage(Name, StdinUsage));
      ExitCode := EXIT_CODE_USAGE;
      Exit;
    end;

    ValidateCommandLine(Paths);

    { Capabilities and limits this binary cannot honor are usage errors on
      the command line (ADR 0122); a malformed scope is an invalid value. }
    if Assigned(FEngineOptions) then
    begin
      FEngineOptions.Capabilities.ValidateHonored(Name, HonoredCapabilities);
      ValidateHonoredSettings(FEngineOptions, Name, HonoredSettings);
      FEngineOptions.Capabilities.ValidateScopes(GetCurrentDir);
    end;

    { Snapshot CLI origin: any option Present at this point was set
      by the command line.  ApplyConfigFile below may set additional
      options, but those will not be marked FromCommandLine. }
    for I := 0 to High(FAllOptions) do
      if FAllOptions[I].Present then
        FAllOptions[I].MarkFromCommandLine;

    { Resolve the root config path.  When --config is given it takes
      precedence over auto-discovery and a missing path is a hard
      error so a typo is not silently ignored.  The value may point
      to a config file directly or to a directory containing
      goccia.toml / goccia.json5 / goccia.json (priority order); the
      directory form does NOT walk upward.  Otherwise walk up from
      the entry file's directory.  Either way, CLI options are skipped
      during application. }
    EnsureConfigParsersRegistered;
    if FConfig.Present then
    begin
      ConfigPath := ResolveExplicitConfigPath(FConfig.Value);
    end
    else
    begin
      ConfigStartDir := ResolveConfigStartDirectory(Paths);
      ConfigPath := DiscoverConfigFile(ConfigStartDir,
        [CONFIG_FILE_BASE_NAME], CONFIG_FILE_EXTENSIONS);
    end;
    FRootConfigExplicit := FConfig.Present;
    if (ConfigPath <> '') and
       ShouldApplyRootConfig(Paths, ConfigPath, FConfig.Present) then
    begin
      FRootConfigPath := ConfigPath;
      RootConfigEntries := ParseConfigFile(ConfigPath);
      { unsafe-* keys are RequiresTrust and skipped here: they reach an
        engine through the governing config's trust verdict. }
      ApplyConfigEntries(RootConfigEntries, FAllOptions);
      ConfineConfigOutputPaths(RootConfigEntries);
      { A malformed permissions block fails the run before anything else. }
      ReadConfigPermissionRequest(RootConfigEntries, ConfigPath);
    end
    else
      FRootConfigPath := '';

    { Every config's permission requests are checked against the trust store
      (or -P / --ignore-config-permissions) before any file runs, in the
      ValidateFileConfigs pass. }
    CreateTrustGate;

    Validate;
    ValidateOutputPaths;

    if FLog.Present then
      OpenLogFile;
    if FAuditLog.Present then
      OpenAuditLog;

    InitializeSingletons;
    try
      ExecuteWithPaths(Paths);
      AfterExecute;
    finally
      ShutdownSingletons;
      CloseAuditLog;
      CloseLogFile;
    end;
  finally
    Paths.Free;
  end;
end;

end.
