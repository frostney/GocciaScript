unit Goccia.CLI.Options;

{$I Goccia.inc}

interface

uses
  CLI.ConfigFile,
  CLI.Options,

  Goccia.Capabilities,
  Goccia.SourcePipeline;

type
  TGocciaExecutionMode = (emInterpreted, emBytecode);
  TGocciaSourceType = (stScript, stModule);

  TGocciaCompatibilityFlagDescriptor = record
    OptionName: string;
    HelpText: string;
  end;

  TGocciaEngineOptions = class
  private
    FMode: TEnumOption<TGocciaExecutionMode>;
    FSourceType: TEnumOption<TGocciaSourceType>;
    FCompatibilityFlags: array[TGocciaCompatibility] of TFlagOption;
    FImportMap: TStringOption;
    FAliases: TRepeatableOption;
    FAllowNodeModules: TOptionalStringOption;
    FTimeout: TIntegerOption;
    FMaxMemory: TInt64Option;
    FMaxInstructions: TInt64Option;
    FUnsafeFFI: TFlagOption;
    FUnsafeFunctionConstructor: TFlagOption;
    FUnsafeShadowRealm: TFlagOption;
    FDeterministic: TFlagOption;
    FWarningUnsupportedFeatures: TFlagOption;
    FStackSize: TIntegerOption;
    FStrictTypes: TFlagOption;
    FAllowedHosts: TRepeatableOption;
    FFetchDenyPrivateRanges: TFlagOption;
    FFetchMaxResponseBytes: TIntegerOption;
    FNoHostFilesystem: TFlagOption;
    FExperimentalAST: TFlagOption;
    FInspectDepth: TIntegerOption;
    FModule: TRepeatableOption;
    FModules: TRepeatableOption;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TOptionArray;
    function CompatibilityFlagOption(
      const AFlag: TGocciaCompatibility): TFlagOption;

    property Mode: TEnumOption<TGocciaExecutionMode> read FMode;
    property SourceType: TEnumOption<TGocciaSourceType> read FSourceType;
    property ImportMap: TStringOption read FImportMap;
    property Aliases: TRepeatableOption read FAliases;
    property AllowNodeModules: TOptionalStringOption read FAllowNodeModules;
    property Timeout: TIntegerOption read FTimeout;
    property MaxMemory: TInt64Option read FMaxMemory;
    property MaxInstructions: TInt64Option read FMaxInstructions;
    property UnsafeFFI: TFlagOption read FUnsafeFFI;
    property UnsafeFunctionConstructor: TFlagOption read FUnsafeFunctionConstructor;
    property UnsafeShadowRealm: TFlagOption read FUnsafeShadowRealm;
    property Deterministic: TFlagOption read FDeterministic;
    property WarningUnsupportedFeatures: TFlagOption read FWarningUnsupportedFeatures;
    property StackSize: TIntegerOption read FStackSize;
    property StrictTypes: TFlagOption read FStrictTypes;
    property AllowedHosts: TRepeatableOption read FAllowedHosts;
    property FetchDenyPrivateRanges: TFlagOption read FFetchDenyPrivateRanges;
    property FetchMaxResponseBytes: TIntegerOption read FFetchMaxResponseBytes;
    property NoHostFilesystem: TFlagOption read FNoHostFilesystem;
    property ExperimentalAST: TFlagOption read FExperimentalAST;
    property InspectDepth: TIntegerOption read FInspectDepth;
    property ModuleDefinitions: TRepeatableOption read FModule;
    property ModuleManifests: TRepeatableOption read FModules;
  end;

  TGocciaCoverageFormat = (cfLcov, cfJson);

  TGocciaCoverageOptions = class
  private
    FEnabled: TFlagOption;
    FFormat: TEnumOption<TGocciaCoverageFormat>;
    FOutputPath: TStringOption;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TOptionArray;

    property Enabled: TFlagOption read FEnabled;
    property Format: TEnumOption<TGocciaCoverageFormat> read FFormat;
    property OutputPath: TStringOption read FOutputPath;
  end;

  TGocciaProfileMode = (pmOpcodes, pmFunctions, pmAll);
  TGocciaProfileFormat = (pfFlamegraph);

  TGocciaProfilerOptions = class
  private
    FMode: TEnumOption<TGocciaProfileMode>;
    FOutputPath: TStringOption;
    FFormat: TEnumOption<TGocciaProfileFormat>;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TOptionArray;

    property Mode: TEnumOption<TGocciaProfileMode> read FMode;
    property OutputPath: TStringOption read FOutputPath;
    property Format: TEnumOption<TGocciaProfileFormat> read FFormat;
  end;

  { The capability-bearing options a binary honors today. Layer 1 of ADR 0122
    maps each binary's existing flags onto an engine capability set without
    changing what each binary does with them; a binary that ignores an option
    today leaves it out of its set. }
  TGocciaCapabilityOption = (
    { The binary loads modules from the host filesystem at all. }
    gcoHostFileLoading,
    { The binary honors --no-host-filesystem. }
    gcoNoHostFilesystem,
    gcoAllowedHosts,
    gcoFetchDenyPrivateRanges,
    gcoUnsafeFFI,
    gcoNodeModules
  );
  TGocciaCapabilityOptions = set of TGocciaCapabilityOption;

const
  AllCapabilityOptions: TGocciaCapabilityOptions = [gcoHostFileLoading,
    gcoNoHostFilesystem, gcoAllowedHosts, gcoFetchDenyPrivateRanges,
    gcoUnsafeFFI, gcoNodeModules];

{ Builds the engine capability set from today's options, with the precedence
  every option already has: command line, then per-file config, then root
  config.

  - read: every path for a binary that loads host files, unless
    --no-host-filesystem (or its config key) is honored and in force, which
    denies read outright. A binary that never loads host files gets none.
  - net: each allowed host; private ranges stay reachable for those hosts
    unless --fetch-deny-private-ranges is in force, which denies them.
  - ffi: every library, with --unsafe-ffi.
  - import: node_modules with --allow-node-modules, bounded by its ceiling.

  AFileConfigPath and ARootConfigPath anchor relative node_modules ceilings to
  the config file that supplied them. Invalid scopes raise
  EGocciaCapabilityScopeError. }
function ResolveCapabilities(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray;
  const AFileConfigPath, ARootConfigPath: string;
  const AHonoredOptions: TGocciaCapabilityOptions): TGocciaCapabilities;

function CompatibilityFlagDescriptor(
  const AFlag: TGocciaCompatibility): TGocciaCompatibilityFlagDescriptor;
procedure ResolveCompatibilityFlags(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray;
  out AFlags: TGocciaCompatibilityFlags);
function TryApplyCompatibilityFlagArg(const AArg: string;
  var AFlags: TGocciaCompatibilityFlags): Boolean;

implementation

uses
  Classes,
  SysUtils,

  Goccia.Modules.Configuration;

const
  ENGINE_FIXED_OPTION_COUNT = 23;
  ALLOWED_HOSTS_CONFIG_KEY = 'allowed-hosts';

{ allowed-host: command line wins outright; otherwise per-file config
  overrides root config. An empty-value config entry marks an explicit empty
  array, and in a merged extends chain child entries come first, so it stops
  accumulation of base values. }
procedure CollectAllowedHosts(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AHosts: TStrings);
var
  I: Integer;
  HasFileHosts: Boolean;
begin
  AHosts.Clear;
  if AEngineOptions.AllowedHosts.FromCommandLine then
  begin
    AHosts.AddStrings(AEngineOptions.AllowedHosts.Values);
    Exit;
  end;
  HasFileHosts := False;
  for I := 0 to High(AFileConfig) do
    if AFileConfig[I].Key = ALLOWED_HOSTS_CONFIG_KEY then
    begin
      HasFileHosts := True;
      Break;
    end;
  if HasFileHosts then
  begin
    for I := 0 to High(AFileConfig) do
      if AFileConfig[I].Key = ALLOWED_HOSTS_CONFIG_KEY then
      begin
        if AFileConfig[I].Value = '' then
          Break;
        AHosts.Add(AFileConfig[I].Value);
      end;
  end
  else if AEngineOptions.AllowedHosts.Present then
    AHosts.AddStrings(AEngineOptions.AllowedHosts.Values);
end;

{ allow-node-modules: a relative ceiling is anchored to whichever source
  supplied it — the invocation directory for the flag, the configuration
  file's own directory for a config key. }
function TryResolveNodeModulesScope(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray;
  const AFileConfigPath, ARootConfigPath: string; out AScope: string): Boolean;
var
  BaseDirectory, Setting: string;
  Option: TOptionalStringOption;
begin
  AScope := '';
  Option := AEngineOptions.AllowNodeModules;
  if Option.FromCommandLine then
  begin
    Setting := Option.Value;
    BaseDirectory := GetCurrentDir;
  end
  else if FindConfigEntry(AFileConfig, Option.LongName, Setting) then
    BaseDirectory := ExtractFilePath(AFileConfigPath)
  else
  begin
    if not Option.Present then
      Exit(False);
    Setting := Option.Value;
    if ARootConfigPath <> '' then
      BaseDirectory := ExtractFilePath(ARootConfigPath)
    else
      BaseDirectory := GetCurrentDir;
  end;
  if BaseDirectory = '' then
    BaseDirectory := GetCurrentDir;
  Result := TryNodeModulesImportScope(Setting, BaseDirectory, AScope);
end;

function ResolveCapabilities(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray;
  const AFileConfigPath, ARootConfigPath: string;
  const AHonoredOptions: TGocciaCapabilityOptions): TGocciaCapabilities;
var
  Hosts: TStringList;
  I: Integer;
  NodeModulesScope: string;
begin
  Result := TGocciaCapabilities.None;
  if not Assigned(AEngineOptions) then
    Exit;

  { Host-filesystem module loading is on by default today, so the default is a
    read grant covering everything; the deny-by-default flip is the next
    layer's CLI change. }
  if not (gcoHostFileLoading in AHonoredOptions) then
    { No host files are loaded, so there is nothing to grant. }
  else if (gcoNoHostFilesystem in AHonoredOptions) and
     ResolveFlagOption(AEngineOptions.NoHostFilesystem, AFileConfig) then
    Result := Result.Deny(gcRead)
  else
    Result := Result.Allow(gcRead);

  if gcoAllowedHosts in AHonoredOptions then
  begin
    Hosts := TStringList.Create;
    try
      CollectAllowedHosts(AEngineOptions, AFileConfig, Hosts);
      for I := 0 to Hosts.Count - 1 do
        Result := Result.Allow(gcNet, Hosts[I]);
      if (gcoFetchDenyPrivateRanges in AHonoredOptions) and
         ResolveFlagOption(AEngineOptions.FetchDenyPrivateRanges,
           AFileConfig) then
        Result := Result.Deny(gcNet, NET_PRIVATE_SCOPE)
      else if Hosts.Count > 0 then
        { Today an allowed host may resolve anywhere unless private ranges are
          denied explicitly; `private` lifts the engine's default refusal
          for exactly the hosts allowed above. }
        Result := Result.Allow(gcNet, NET_PRIVATE_SCOPE);
    finally
      Hosts.Free;
    end;
  end;

  if (gcoUnsafeFFI in AHonoredOptions) and
     ResolveFlagOption(AEngineOptions.UnsafeFFI, AFileConfig) then
    Result := Result.Allow(gcFFI);

  if (gcoNodeModules in AHonoredOptions) and
     TryResolveNodeModulesScope(AEngineOptions, AFileConfig, AFileConfigPath,
       ARootConfigPath, NodeModulesScope) then
    Result := Result.Allow(gcImport, NodeModulesScope);
end;

const
  SOURCE_COMPATIBILITY_FLAGS: array[TGocciaCompatibility]
    of TGocciaCompatibilityFlagDescriptor = (
    (OptionName: 'compat-asi';
     HelpText: 'Enable automatic semicolon insertion (compatibility)'),
    (OptionName: 'compat-var';
     HelpText: 'Enable var declarations (compatibility)'),
    (OptionName: 'compat-function';
     HelpText: 'Enable function declarations and expressions (compatibility)'),
    (OptionName: 'compat-traditional-for-loop';
     HelpText: 'Enable traditional C-style for(init; test; update) loops (compatibility)'),
    (OptionName: 'compat-while-loops';
     HelpText: 'Enable while and do...while loops (compatibility)'),
    (OptionName: 'compat-loose-equality';
     HelpText: 'Enable loose equality and inequality (== and !=) (compatibility)'),
    (OptionName: 'compat-non-strict-mode';
     HelpText: 'Enable non-strict-mode compatibility semantics'),
    (OptionName: 'compat-arguments-object';
     HelpText: 'Enable implicit arguments objects (compatibility)'),
    (OptionName: 'compat-label';
     HelpText: 'Enable labeled break and continue targets (compatibility)'),
    (OptionName: 'compat-for-in-loop';
     HelpText: 'Enable for...in property enumeration loops (compatibility)'),
    (OptionName: 'experimental-js-module-source';
     HelpText: 'Enable experimental JavaScript ModuleSource objects')
  );

function CompatibilityFlagCount: Integer;
begin
  Result := Ord(High(TGocciaCompatibility)) - Ord(Low(TGocciaCompatibility))
    + 1;
end;

function CompatibilityFlagDescriptor(
  const AFlag: TGocciaCompatibility): TGocciaCompatibilityFlagDescriptor;
begin
  Result := SOURCE_COMPATIBILITY_FLAGS[AFlag];
end;

procedure ResolveCompatibilityFlags(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray;
  out AFlags: TGocciaCompatibilityFlags);
var
  Flag: TGocciaCompatibility;
begin
  AFlags := [];
  if not Assigned(AEngineOptions) then
    Exit;

  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
    if ResolveFlagOption(AEngineOptions.CompatibilityFlagOption(Flag),
       AFileConfig) then
      Include(AFlags, Flag);
end;

function TryApplyCompatibilityFlagArg(const AArg: string;
  var AFlags: TGocciaCompatibilityFlags): Boolean;
var
  Flag: TGocciaCompatibility;
begin
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
    if AArg = '--' + SOURCE_COMPATIBILITY_FLAGS[Flag].OptionName then
    begin
      Include(AFlags, Flag);
      Exit(True);
    end;

  Result := False;
end;

{ TGocciaEngineOptions }

constructor TGocciaEngineOptions.Create;
var
  Flag: TGocciaCompatibility;
begin
  inherited Create;
  FMode := TEnumOption<TGocciaExecutionMode>.Create('mode',
    'Execution mode', 'Engine');
  FSourceType := TEnumOption<TGocciaSourceType>.Create('source-type',
    'Source loading kind (default: script; .mjs infers module)', 'Engine');
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
    FCompatibilityFlags[Flag] := TFlagOption.Create(
      SOURCE_COMPATIBILITY_FLAGS[Flag].OptionName,
      SOURCE_COMPATIBILITY_FLAGS[Flag].HelpText, 'Engine');
  FImportMap := TStringOption.Create('import-map',
    'Path to import map JSON file', 'Engine');
  FAliases := TRepeatableOption.Create('alias',
    'Import alias (e.g. @/=./src/)', 'Engine');
  FAllowNodeModules := TOptionalStringOption.Create('allow-node-modules',
    'Resolve bare specifiers against node_modules, optionally confined to <dir>',
    'Engine');
  FTimeout := TIntegerOption.Create('timeout',
    'Per-file timeout in milliseconds', 'Engine');
  FMaxMemory := TInt64Option.Create('max-memory',
    'GC heap byte limit (RangeError on exceed)', 'Engine');
  FMaxInstructions := TInt64Option.Create('max-instructions',
    'Maximum execution steps before aborting', 'Engine');
  FUnsafeFFI := TFlagOption.Create('unsafe-ffi',
    'Enable the FFI global (foreign function interface)', 'Runtime');
  FUnsafeFunctionConstructor := TFlagOption.Create('unsafe-function-constructor',
    'Enable the Function constructor (dynamic code generation)', 'Engine');
  FUnsafeShadowRealm := TFlagOption.Create('unsafe-shadowrealm',
    'Enable the ShadowRealm constructor (dynamic source evaluation)', 'Engine');
  FDeterministic := TFlagOption.Create('deterministic',
    'Use fixed script-visible time, UTC, and seeded randomness', 'Engine');
  FWarningUnsupportedFeatures := TFlagOption.Create(
    'warning-unsupported-features',
    'Warn and recover for unsupported/default-disabled syntax instead of failing parsing',
    'Engine');
  FStackSize := TIntegerOption.Create('stack-size',
    'Maximum call stack depth (0 = no limit)', 'Engine');
  FStrictTypes := TFlagOption.Create('strict-types',
    'Enforce type annotations at runtime (interpreter and bytecode)', 'Engine');
  FAllowedHosts := TRepeatableOption.Create('allowed-host',
    'Hostname allowed for fetch requests (repeatable)', 'Engine');
  FAllowedHosts.ConfigName := 'allowed-hosts';
  FFetchDenyPrivateRanges := TFlagOption.Create('fetch-deny-private-ranges',
    'Reject fetch targets resolving to private, loopback, or link-local addresses',
    'Runtime');
  FFetchMaxResponseBytes := TIntegerOption.Create('fetch-max-response-bytes',
    'Maximum fetch response body size in bytes (TypeError on exceed)',
    'Runtime');
  FNoHostFilesystem := TFlagOption.Create('no-host-filesystem',
    'Disable ambient host-filesystem module loading', 'Runtime');
  FExperimentalAST := TFlagOption.Create('experimental-ast',
    'Enable the experimental goccia:ast parse module', 'Runtime');
  FInspectDepth := TIntegerOption.Create('inspect-depth',
    'Maximum object inspection depth for console output (default: 5)', 'Engine');
  FModule := TRepeatableOption.Create('module',
    'Virtual module definition (name=source or name={descriptor})', 'Engine');
  FModules := TRepeatableOption.Create('modules',
    'Path to a virtual modules manifest (repeatable)', 'Engine');
end;

destructor TGocciaEngineOptions.Destroy;
var
  Flag: TGocciaCompatibility;
begin
  FMode.Free;
  FSourceType.Free;
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
    FCompatibilityFlags[Flag].Free;
  FImportMap.Free;
  FAliases.Free;
  FAllowNodeModules.Free;
  FTimeout.Free;
  FMaxMemory.Free;
  FMaxInstructions.Free;
  FUnsafeFFI.Free;
  FUnsafeFunctionConstructor.Free;
  FUnsafeShadowRealm.Free;
  FDeterministic.Free;
  FWarningUnsupportedFeatures.Free;
  FStackSize.Free;
  FStrictTypes.Free;
  FAllowedHosts.Free;
  FFetchDenyPrivateRanges.Free;
  FFetchMaxResponseBytes.Free;
  FNoHostFilesystem.Free;
  FExperimentalAST.Free;
  FInspectDepth.Free;
  FModule.Free;
  FModules.Free;
  inherited Destroy;
end;

function TGocciaEngineOptions.Options: TOptionArray;
var
  Flag: TGocciaCompatibility;
  Index: Integer;
begin
  SetLength(Result, ENGINE_FIXED_OPTION_COUNT + CompatibilityFlagCount);
  Index := 0;
  Result[Index] := FMode;
  Inc(Index);
  Result[Index] := FSourceType;
  Inc(Index);
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
  begin
    Result[Index] := FCompatibilityFlags[Flag];
    Inc(Index);
  end;
  Result[Index] := FImportMap;
  Inc(Index);
  Result[Index] := FAliases;
  Inc(Index);
  Result[Index] := FAllowNodeModules;
  Inc(Index);
  Result[Index] := FTimeout;
  Inc(Index);
  Result[Index] := FMaxMemory;
  Inc(Index);
  Result[Index] := FMaxInstructions;
  Inc(Index);
  Result[Index] := FUnsafeFFI;
  Inc(Index);
  Result[Index] := FUnsafeFunctionConstructor;
  Inc(Index);
  Result[Index] := FUnsafeShadowRealm;
  Inc(Index);
  Result[Index] := FDeterministic;
  Inc(Index);
  Result[Index] := FWarningUnsupportedFeatures;
  Inc(Index);
  Result[Index] := FStackSize;
  Inc(Index);
  Result[Index] := FStrictTypes;
  Inc(Index);
  Result[Index] := FAllowedHosts;
  Inc(Index);
  Result[Index] := FFetchDenyPrivateRanges;
  Inc(Index);
  Result[Index] := FFetchMaxResponseBytes;
  Inc(Index);
  Result[Index] := FNoHostFilesystem;
  Inc(Index);
  Result[Index] := FExperimentalAST;
  Inc(Index);
  Result[Index] := FInspectDepth;
  Inc(Index);
  Result[Index] := FModule;
  Inc(Index);
  Result[Index] := FModules;
end;

function TGocciaEngineOptions.CompatibilityFlagOption(
  const AFlag: TGocciaCompatibility): TFlagOption;
begin
  Result := FCompatibilityFlags[AFlag];
end;

{ TGocciaCoverageOptions }

constructor TGocciaCoverageOptions.Create;
begin
  inherited Create;
  FEnabled := TFlagOption.Create('coverage',
    'Enable line and branch coverage', 'Coverage');
  FFormat := TEnumOption<TGocciaCoverageFormat>.Create('coverage-format',
    'Coverage output format', 'Coverage');
  FOutputPath := TStringOption.Create('coverage-output',
    'Coverage output file path', 'Coverage');
end;

destructor TGocciaCoverageOptions.Destroy;
begin
  FEnabled.Free;
  FFormat.Free;
  FOutputPath.Free;
  inherited Destroy;
end;

function TGocciaCoverageOptions.Options: TOptionArray;
begin
  SetLength(Result, 3);
  Result[0] := FEnabled;
  Result[1] := FFormat;
  Result[2] := FOutputPath;
end;

{ TGocciaProfilerOptions }

constructor TGocciaProfilerOptions.Create;
begin
  inherited Create;
  FMode := TEnumOption<TGocciaProfileMode>.Create('profile',
    'Profiling mode', 'Profiler');
  FOutputPath := TStringOption.Create('profile-output',
    'Profile output file path', 'Profiler');
  FFormat := TEnumOption<TGocciaProfileFormat>.Create('profile-format',
    'Profile output format', 'Profiler');
end;

destructor TGocciaProfilerOptions.Destroy;
begin
  FMode.Free;
  FOutputPath.Free;
  FFormat.Free;
  inherited Destroy;
end;

function TGocciaProfilerOptions.Options: TOptionArray;
begin
  SetLength(Result, 3);
  Result[0] := FMode;
  Result[1] := FOutputPath;
  Result[2] := FFormat;
end;

end.
