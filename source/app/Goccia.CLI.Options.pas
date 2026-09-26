unit Goccia.CLI.Options;

{$I Goccia.inc}

interface

uses
  CLI.ConfigFile,
  CLI.Options,

  Goccia.Capabilities,
  Goccia.CLI.Permissions,
  Goccia.SourcePipeline;

type
  TGocciaExecutionMode = (emInterpreted, emBytecode);
  TGocciaSourceType = (stScript, stModule);

  TGocciaCompatibilityFlagDescriptor = record
    OptionName: string;
    HelpText: string;
  end;

  { The limits a binary may honor. A binary that does not honor one rejects
    it on the command line and ignores it in config. }
  TGocciaRuntimeSetting = (grsTimeout, grsMaxMemory, grsMaxInstructions,
    grsMaxStack, grsMaxFetchBytes);
  TGocciaHonoredSettings = set of TGocciaRuntimeSetting;

  { The `--allow-<cap>[=scope,...]` / `--deny-<cap>[=scope,...]` grammar
    (ADR 0122). Every option is command-line-only: config files declare
    permissions in their `permissions` block instead. }
  TGocciaCapabilityOptions = class
  private
    FAllow: array[TGocciaCapability] of TScopeListOption;
    FDeny: array[TGocciaCapability] of TScopeListOption;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TOptionArray;
    function AllowOption(const ACapability: TGocciaCapability):
      TScopeListOption;
    function DenyOption(const ACapability: TGocciaCapability):
      TScopeListOption;
    { Hides the options of capabilities the binary cannot grant. They stay
      parseable, so ValidateHonored can name the problem. }
    procedure HideUnsupported(const AHonored: TGocciaHonoredCapabilities);
    { Raises TCLIUsageError for an --allow-* the binary cannot grant. A
      --deny-* is always accepted. }
    procedure ValidateHonored(const AProgramName: string;
      const AHonored: TGocciaHonoredCapabilities);
    { Raises TParseError for a command-line scope the capability does not
      accept. }
    procedure ValidateScopes(const AWorkingDirectory: string);
  end;

  TGocciaEngineOptions = class
  private
    FMode: TEnumOption<TGocciaExecutionMode>;
    FSourceType: TEnumOption<TGocciaSourceType>;
    FCompatibilityFlags: array[TGocciaCompatibility] of TFlagOption;
    FImportMap: TStringOption;
    FAliases: TRepeatableOption;
    FCapabilities: TGocciaCapabilityOptions;
    FTimeout: TDurationOption;
    FMaxMemory: TByteSizeOption;
    FMaxInstructions: TCountOption;
    FMaxStack: TCountOption;
    FMaxFetchBytes: TByteSizeOption;
    FUnsafeFunctionConstructor: TFlagOption;
    FUnsafeShadowRealm: TFlagOption;
    FDeterministic: TFlagOption;
    FWarningUnsupportedFeatures: TFlagOption;
    FStrictTypes: TFlagOption;
    FExperimentalAST: TFlagOption;
    FInspectDepth: TIntegerOption;
    FModule: TRepeatableOption;
    FModules: TRepeatableOption;
    FRemoved: TOptionList;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TOptionArray;
    function CompatibilityFlagOption(
      const AFlag: TGocciaCompatibility): TFlagOption;
    function SettingOption(const ASetting: TGocciaRuntimeSetting): TOptionBase;
    { Hides the limits a binary does not apply from --help and makes config
      files ignore them without validation. }
    procedure HideUnsupportedSettings(const AHonored: TGocciaHonoredSettings);

    property Mode: TEnumOption<TGocciaExecutionMode> read FMode;
    property SourceType: TEnumOption<TGocciaSourceType> read FSourceType;
    property ImportMap: TStringOption read FImportMap;
    property Aliases: TRepeatableOption read FAliases;
    property Capabilities: TGocciaCapabilityOptions read FCapabilities;
    property Timeout: TDurationOption read FTimeout;
    property MaxMemory: TByteSizeOption read FMaxMemory;
    property MaxInstructions: TCountOption read FMaxInstructions;
    property MaxStack: TCountOption read FMaxStack;
    property MaxFetchBytes: TByteSizeOption read FMaxFetchBytes;
    property UnsafeFunctionConstructor: TFlagOption read FUnsafeFunctionConstructor;
    property UnsafeShadowRealm: TFlagOption read FUnsafeShadowRealm;
    property Deterministic: TFlagOption read FDeterministic;
    property WarningUnsupportedFeatures: TFlagOption read FWarningUnsupportedFeatures;
    property StrictTypes: TFlagOption read FStrictTypes;
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


const
  ALL_RUNTIME_SETTINGS: TGocciaHonoredSettings = [grsTimeout, grsMaxMemory,
    grsMaxInstructions, grsMaxStack, grsMaxFetchBytes];

{ Raises TCLIUsageError for a limit given on the command line that the binary
  does not honor. Call after parsing and before config is applied. }
procedure ValidateHonoredSettings(const AEngineOptions: TGocciaEngineOptions;
  const AProgramName: string; const AHonored: TGocciaHonoredSettings);

{ The engine capability set for one file (ADR 0122): command-line allows and,
  when AConfigGrantsAccepted, the config request's allows, restricted to the
  capabilities the binary honors; then every command-line and config deny,
  which always win. Command-line path scopes resolve against
  AWorkingDirectory; the request's scopes are already absolute. AOptions may
  be nil for a binary without capability flags. }
function ResolveCapabilities(const AOptions: TGocciaCapabilityOptions;
  const ARequest: TGocciaConfigPermissionRequest;
  const AConfigGrantsAccepted: Boolean;
  const AHonored: TGocciaHonoredCapabilities;
  const AWorkingDirectory: string): TGocciaCapabilities;

{ For binaries with their own argument parser: True when AArgument is an
  --allow-<cap> or --deny-<cap> flag, validated exactly as the shared parser
  validates it (scope syntax and scope values raise TParseError). An
  --allow-* for a capability outside AHonored raises TCLIUsageError; a
  well-formed --deny-* is accepted. }
function TryHandleCapabilityArgument(const AArgument, AProgramName: string;
  const AHonored: TGocciaHonoredCapabilities): Boolean;

{ For binaries with their own argument parser: raises TCLIUsageError when
  AArgument is a limit (--timeout, --max-*) outside AHonored, with the same
  message the shared application gives. }
procedure RejectUnsupportedSettingArgument(const AArgument,
  AProgramName: string; const AHonored: TGocciaHonoredSettings);

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

  CLI.Parser,

  Goccia.StackLimit;

const
  ENGINE_FIXED_OPTION_COUNT = 18;
  PERMISSIONS_GROUP = 'Permissions';
  LIMITS_GROUP = 'Limits';
  PERMISSIONS_CONFIG_HINT =
    '; declare it in the config''s "permissions" object instead';

type
  TGocciaCapabilityOptionText = record
    Placeholder: string;
    AllowHelp: string;
    AllowUnscopedMeaning: string;
    DenyHelp: string;
    DenyUnscopedMeaning: string;
    RequiresScope: Boolean;
  end;

const
  CAPABILITY_OPTION_TEXT: array[TGocciaCapability] of
    TGocciaCapabilityOptionText = (
    (Placeholder: '<path>';
     AllowHelp: 'Allow host reads beyond the project''s module graph, ' +
       'optionally only under <path> (relative to the working directory)';
     AllowUnscopedMeaning: 'allow reading every path';
     DenyHelp: 'Deny host reads under <path>; with no scope, also refuse ' +
       'imports from the project directory';
     DenyUnscopedMeaning: 'deny every read, including project imports';
     RequiresScope: False),
    (Placeholder: '<host>';
     AllowHelp: 'Allow fetch to host, host:port, *.domain, IP, or CIDR (no ' +
       'scope: every public host); a listed IP or CIDR also reaches that ' +
       'private address, and private reaches every private and loopback ' +
       'address';
     AllowUnscopedMeaning: 'allow every public host';
     DenyHelp: 'Deny fetch to these hosts even when allowed elsewhere ' +
       '(deny always wins)';
     DenyUnscopedMeaning: 'deny every host';
     RequiresScope: False),
    (Placeholder: '<library>';
     AllowHelp: 'Allow opening native libraries, optionally only these ' +
       'paths (enables the FFI global)';
     AllowUnscopedMeaning: 'allow every library';
     DenyHelp: 'Deny opening these native libraries (deny always wins)';
     DenyUnscopedMeaning: 'deny every library';
     RequiresScope: False),
    (Placeholder: '<source>';
     AllowHelp: 'Allow module sources outside the project: ' +
       'node_modules[=<dir>] or a provider such as github';
     AllowUnscopedMeaning: '';
     DenyHelp: 'Deny these module sources (deny always wins)';
     DenyUnscopedMeaning: '';
     RequiresScope: True)
  );

  SETTING_NAMES: array[TGocciaRuntimeSetting] of string = ('timeout',
    'max-memory', 'max-instructions', 'max-stack', 'max-fetch-bytes');

{ TGocciaCapabilityOptions }

constructor TGocciaCapabilityOptions.Create;
var
  Capability: TGocciaCapability;
  Text: TGocciaCapabilityOptionText;
begin
  inherited Create;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    Text := CAPABILITY_OPTION_TEXT[Capability];
    FAllow[Capability] := TScopeListOption.Create(
      PermissionKeyName(True, Capability), Text.AllowHelp, Text.Placeholder,
      PERMISSIONS_GROUP, Text.RequiresScope, Text.AllowUnscopedMeaning,
      IMPORT_SCOPE_REQUIREMENT);
    FDeny[Capability] := TScopeListOption.Create(
      PermissionKeyName(False, Capability), Text.DenyHelp, Text.Placeholder,
      PERMISSIONS_GROUP, Text.RequiresScope, Text.DenyUnscopedMeaning,
      IMPORT_SCOPE_REQUIREMENT);
    FAllow[Capability].CommandLineOnly := True;
    FAllow[Capability].ConfigHint := PERMISSIONS_CONFIG_HINT;
    FDeny[Capability].CommandLineOnly := True;
    FDeny[Capability].ConfigHint := PERMISSIONS_CONFIG_HINT;
  end;
end;

destructor TGocciaCapabilityOptions.Destroy;
var
  Capability: TGocciaCapability;
begin
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    FAllow[Capability].Free;
    FDeny[Capability].Free;
  end;
  inherited Destroy;
end;

function TGocciaCapabilityOptions.Options: TOptionArray;
var
  Capability: TGocciaCapability;
  Index: Integer;
begin
  SetLength(Result, 2 * (Ord(High(TGocciaCapability)) + 1));
  Index := 0;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    Result[Index] := FAllow[Capability];
    Inc(Index);
  end;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    Result[Index] := FDeny[Capability];
    Inc(Index);
  end;
end;

function TGocciaCapabilityOptions.AllowOption(
  const ACapability: TGocciaCapability): TScopeListOption;
begin
  Result := FAllow[ACapability];
end;

function TGocciaCapabilityOptions.DenyOption(
  const ACapability: TGocciaCapability): TScopeListOption;
begin
  Result := FDeny[ACapability];
end;

procedure TGocciaCapabilityOptions.HideUnsupported(
  const AHonored: TGocciaHonoredCapabilities);
var
  Capability: TGocciaCapability;
begin
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    FAllow[Capability].Hidden := not (Capability in AHonored);
    FDeny[Capability].Hidden := not (Capability in AHonored);
  end;
end;

procedure TGocciaCapabilityOptions.ValidateHonored(const AProgramName: string;
  const AHonored: TGocciaHonoredCapabilities);
var
  Capability: TGocciaCapability;
begin
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if FAllow[Capability].Present and not (Capability in AHonored) then
      raise TCLIUsageError.CreateFmt(
        '%s cannot grant %s; it supports %s. Remove --%s.',
        [AProgramName, CapabilityName(Capability),
         DescribeCapabilities(AHonored), FAllow[Capability].LongName]);
end;

procedure ValidateOptionScopes(const AOption: TScopeListOption;
  const ACapability: TGocciaCapability; const AWorkingDirectory: string);
var
  I: Integer;
begin
  for I := 0 to AOption.Scopes.Count - 1 do
    try
      TGocciaCapabilities.None.Allow(ACapability, ResolvePermissionScope(
        ACapability, AOption.Scopes[I], AWorkingDirectory));
    except
      on E: EGocciaCapabilityScopeError do
        raise TParseError.CreateFmt('Invalid scope for --%s: "%s" (%s)',
          [AOption.LongName, AOption.Scopes[I],
           PermissionScopeHint(ACapability)]);
    end;
end;

procedure TGocciaCapabilityOptions.ValidateScopes(
  const AWorkingDirectory: string);
var
  Capability: TGocciaCapability;
begin
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    ValidateOptionScopes(FAllow[Capability], Capability, AWorkingDirectory);
    ValidateOptionScopes(FDeny[Capability], Capability, AWorkingDirectory);
  end;
end;

{ ── capability resolution ─────────────────────────────────────── }

function AddOptionScopes(const ACapabilities: TGocciaCapabilities;
  const AOption: TScopeListOption; const ACapability: TGocciaCapability;
  const AAllow: Boolean; const AWorkingDirectory: string):
  TGocciaCapabilities;
var
  I: Integer;
  Scope: string;
begin
  Result := ACapabilities;
  if not Assigned(AOption) then
    Exit;
  if AOption.Unscoped then
  begin
    if AAllow then
      Result := Result.Allow(ACapability)
    else
      Result := Result.Deny(ACapability);
  end;
  for I := 0 to AOption.Scopes.Count - 1 do
  begin
    Scope := ResolvePermissionScope(ACapability, AOption.Scopes[I],
      AWorkingDirectory);
    if AAllow then
      Result := Result.Allow(ACapability, Scope)
    else
      Result := Result.Deny(ACapability, Scope);
  end;
end;

function AddRequestScopes(const ACapabilities: TGocciaCapabilities;
  const AScopes: TGocciaPermissionScopes;
  const ACapability: TGocciaCapability; const AAllow: Boolean):
  TGocciaCapabilities;
var
  I: Integer;
begin
  Result := ACapabilities;
  if AScopes.Unscoped then
  begin
    if AAllow then
      Result := Result.Allow(ACapability)
    else
      Result := Result.Deny(ACapability);
  end;
  for I := 0 to High(AScopes.Scopes) do
    if AAllow then
      Result := Result.Allow(ACapability, AScopes.Scopes[I])
    else
      Result := Result.Deny(ACapability, AScopes.Scopes[I]);
end;

function ResolveCapabilities(const AOptions: TGocciaCapabilityOptions;
  const ARequest: TGocciaConfigPermissionRequest;
  const AConfigGrantsAccepted: Boolean;
  const AHonored: TGocciaHonoredCapabilities;
  const AWorkingDirectory: string): TGocciaCapabilities;
var
  Capability: TGocciaCapability;
begin
  Result := TGocciaCapabilities.None;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    if not (Capability in AHonored) then
      Continue;
    if Assigned(AOptions) then
      Result := AddOptionScopes(Result, AOptions.AllowOption(Capability),
        Capability, True, AWorkingDirectory);
    if AConfigGrantsAccepted then
      Result := AddRequestScopes(Result, ARequest.Allow[Capability],
        Capability, True);
  end;
  { Denies apply whether or not the capability is honored: they can only
    remove authority. }
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    if Assigned(AOptions) then
      Result := AddOptionScopes(Result, AOptions.DenyOption(Capability),
        Capability, False, AWorkingDirectory);
    Result := AddRequestScopes(Result, ARequest.Deny[Capability], Capability,
      False);
  end;
end;

procedure ValidateHonoredSettings(const AEngineOptions: TGocciaEngineOptions;
  const AProgramName: string; const AHonored: TGocciaHonoredSettings);
var
  Setting: TGocciaRuntimeSetting;
begin
  if not Assigned(AEngineOptions) then
    Exit;
  for Setting := Low(TGocciaRuntimeSetting) to High(TGocciaRuntimeSetting) do
    if (not (Setting in AHonored)) and
       AEngineOptions.SettingOption(Setting).Present then
      raise TCLIUsageError.CreateFmt('%s does not support --%s. Remove it.',
        [AProgramName, SETTING_NAMES[Setting]]);
end;

function TryHandleCapabilityArgument(const AArgument, AProgramName: string;
  const AHonored: TGocciaHonoredCapabilities): Boolean;
const
  ALLOW_FLAG_PREFIX = '--allow-';
  DENY_FLAG_PREFIX = '--deny-';
var
  Name: string;
  Allow: Boolean;
  Capability: TGocciaCapability;
  EqualPos: Integer;
  Options: TGocciaCapabilityOptions;
begin
  if Copy(AArgument, 1, Length(ALLOW_FLAG_PREFIX)) = ALLOW_FLAG_PREFIX then
  begin
    Allow := True;
    Name := Copy(AArgument, Length(ALLOW_FLAG_PREFIX) + 1, MaxInt);
  end
  else if Copy(AArgument, 1, Length(DENY_FLAG_PREFIX)) = DENY_FLAG_PREFIX then
  begin
    Allow := False;
    Name := Copy(AArgument, Length(DENY_FLAG_PREFIX) + 1, MaxInt);
  end
  else
    Exit(False);
  EqualPos := Pos('=', Name);
  if EqualPos > 0 then
    Name := Copy(Name, 1, EqualPos - 1);
  if (not TryParseCapabilityName(Name, Capability)) or
     (Name <> CapabilityName(Capability)) then
    Exit(False);
  { The shared parser's order: grammar (exit 1), then support (exit 2),
    then scope values (exit 1). }
  Options := TGocciaCapabilityOptions.Create;
  try
    ParseArguments([AArgument], Options.Options).Free;
    if Allow and not (Capability in AHonored) then
      raise TCLIUsageError.CreateFmt(
        '%s cannot grant %s; it supports %s. Remove --%s.',
        [AProgramName, CapabilityName(Capability),
         DescribeCapabilities(AHonored),
         PermissionKeyName(True, Capability)]);
    Options.ValidateScopes(GetCurrentDir);
  finally
    Options.Free;
  end;
  Result := True;
end;

procedure RejectUnsupportedSettingArgument(const AArgument,
  AProgramName: string; const AHonored: TGocciaHonoredSettings);
var
  Setting: TGocciaRuntimeSetting;
  Name: string;
begin
  if Copy(AArgument, 1, 2) <> '--' then
    Exit;
  Name := Copy(AArgument, 3, MaxInt);
  if Pos('=', Name) > 0 then
    Name := Copy(Name, 1, Pos('=', Name) - 1);
  for Setting := Low(TGocciaRuntimeSetting) to High(TGocciaRuntimeSetting) do
    if (Name = SETTING_NAMES[Setting]) and not (Setting in AHonored) then
      raise TCLIUsageError.CreateFmt('%s does not support --%s. Remove it.',
        [AProgramName, Name]);
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

procedure AddRemovedOptions(const AList: TOptionList);
begin
  AList.Add(TRemovedOption.Create('allowed-host', 'allowed-hosts',
    'use --allow-net=<host>[,<host>...] instead',
    'use "permissions": { "allow-net": [...] } instead'));
  AList.Add(TRemovedOption.Create('fetch-deny-private-ranges',
    'fetch-deny-private-ranges',
    'private ranges are denied by default; allow them with ' +
    '--allow-net=private, or refuse them outright with --deny-net=private',
    'private ranges are denied by default; allow them with ' +
    '"permissions": { "allow-net": ["private"] }, or refuse them outright ' +
    'with "permissions": { "deny-net": ["private"] }'));
  AList.Add(TRemovedOption.Create('fetch-max-response-bytes',
    'fetch-max-response-bytes',
    'use --max-fetch-bytes instead (units: 1MiB)',
    'use "max-fetch-bytes" instead (units: "1MiB")'));
  AList.Add(TRemovedOption.Create('unsafe-ffi', 'unsafe-ffi',
    'use --allow-ffi[=<library>,...] instead',
    'use "permissions": { "allow-ffi": true } instead'));
  AList.Add(TRemovedOption.Create('allow-node-modules', 'allow-node-modules',
    'use --allow-import=node_modules[=<dir>] instead',
    'use "permissions": { "allow-import": ["node_modules"] } instead'));
  AList.Add(TRemovedOption.Create('no-host-filesystem', 'no-host-filesystem',
    'host reads are denied by default; use --deny-read to also refuse ' +
    'imports from the project',
    'use "permissions": { "deny-read": true } instead'));
  AList.Add(TRemovedOption.Create('stack-size', 'stack-size',
    'use --max-stack instead', 'use "max-stack" instead'));
end;

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
  FCapabilities := TGocciaCapabilityOptions.Create;
  FTimeout := TDurationOption.Create('timeout',
    'Per-file timeout: 500ms, 5s, 2m, or plain milliseconds (0 = none)',
    LIMITS_GROUP);
  FMaxMemory := TByteSizeOption.Create('max-memory',
    'GC heap limit: 64MiB, 1GiB, or plain bytes (RangeError on exceed)',
    LIMITS_GROUP);
  FMaxInstructions := TCountOption.Create('max-instructions',
    'Maximum execution steps before aborting (0 = no limit)', LIMITS_GROUP);
  FMaxStack := TCountOption.Create('max-stack',
    Format('Maximum call stack depth (default: %d; 0 = no limit)',
      [DEFAULT_MAX_STACK_DEPTH]), LIMITS_GROUP);
  FMaxStack.Maximum := High(Integer);
  FMaxFetchBytes := TByteSizeOption.Create('max-fetch-bytes',
    'Maximum fetch response body: 1MiB or plain bytes (default: 8MiB; ' +
    'TypeError on exceed)', LIMITS_GROUP);
  FMaxFetchBytes.Maximum := High(Integer);
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
  FStrictTypes := TFlagOption.Create('strict-types',
    'Enforce type annotations at runtime (interpreter and bytecode)', 'Engine');
  FExperimentalAST := TFlagOption.Create('experimental-ast',
    'Enable the experimental goccia:ast parse module', 'Runtime');
  FInspectDepth := TIntegerOption.Create('inspect-depth',
    'Maximum object inspection depth for console output (default: 5)', 'Engine');
  FModule := TRepeatableOption.Create('module',
    'Virtual module definition (name=source or name={descriptor})', 'Engine');
  FModules := TRepeatableOption.Create('modules',
    'Path to a virtual modules manifest (repeatable)', 'Engine');
  FModules.AcceptsObject := True;
  FRemoved := TOptionList.Create;
  AddRemovedOptions(FRemoved);
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
  FCapabilities.Free;
  FTimeout.Free;
  FMaxMemory.Free;
  FMaxInstructions.Free;
  FMaxStack.Free;
  FMaxFetchBytes.Free;
  FUnsafeFunctionConstructor.Free;
  FUnsafeShadowRealm.Free;
  FDeterministic.Free;
  FWarningUnsupportedFeatures.Free;
  FStrictTypes.Free;
  FExperimentalAST.Free;
  FInspectDepth.Free;
  FModule.Free;
  FModules.Free;
  FRemoved.Free;
  inherited Destroy;
end;

function TGocciaEngineOptions.Options: TOptionArray;
var
  Flag: TGocciaCompatibility;
  Leading, Trailing: TOptionArray;
  Index: Integer;
begin
  { Help groups appear in the order their first option does: Engine, then
    Permissions, then Limits. }
  SetLength(Leading, ENGINE_FIXED_OPTION_COUNT + CompatibilityFlagCount);
  Index := 0;
  Leading[Index] := FMode;
  Inc(Index);
  Leading[Index] := FSourceType;
  Inc(Index);
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
  begin
    Leading[Index] := FCompatibilityFlags[Flag];
    Inc(Index);
  end;
  Leading[Index] := FImportMap;
  Inc(Index);
  Leading[Index] := FAliases;
  Inc(Index);
  SetLength(Leading, Index);

  SetLength(Trailing, ENGINE_FIXED_OPTION_COUNT);
  Index := 0;
  Trailing[Index] := FTimeout;
  Inc(Index);
  Trailing[Index] := FMaxMemory;
  Inc(Index);
  Trailing[Index] := FMaxInstructions;
  Inc(Index);
  Trailing[Index] := FMaxStack;
  Inc(Index);
  Trailing[Index] := FMaxFetchBytes;
  Inc(Index);
  Trailing[Index] := FUnsafeFunctionConstructor;
  Inc(Index);
  Trailing[Index] := FUnsafeShadowRealm;
  Inc(Index);
  Trailing[Index] := FDeterministic;
  Inc(Index);
  Trailing[Index] := FWarningUnsupportedFeatures;
  Inc(Index);
  Trailing[Index] := FStrictTypes;
  Inc(Index);
  Trailing[Index] := FExperimentalAST;
  Inc(Index);
  Trailing[Index] := FInspectDepth;
  Inc(Index);
  Trailing[Index] := FModule;
  Inc(Index);
  Trailing[Index] := FModules;
  Inc(Index);
  SetLength(Trailing, Index);

  Result := ConcatOptions([Leading, FCapabilities.Options, Trailing,
    FRemoved.Options]);
end;

function TGocciaEngineOptions.CompatibilityFlagOption(
  const AFlag: TGocciaCompatibility): TFlagOption;
begin
  Result := FCompatibilityFlags[AFlag];
end;

function TGocciaEngineOptions.SettingOption(
  const ASetting: TGocciaRuntimeSetting): TOptionBase;
begin
  case ASetting of
    grsTimeout:
      Result := FTimeout;
    grsMaxMemory:
      Result := FMaxMemory;
    grsMaxInstructions:
      Result := FMaxInstructions;
    grsMaxStack:
      Result := FMaxStack;
  else
    Result := FMaxFetchBytes;
  end;
end;

procedure TGocciaEngineOptions.HideUnsupportedSettings(
  const AHonored: TGocciaHonoredSettings);
var
  Setting: TGocciaRuntimeSetting;
begin
  for Setting := Low(TGocciaRuntimeSetting) to High(TGocciaRuntimeSetting) do
  begin
    SettingOption(Setting).Hidden := not (Setting in AHonored);
    SettingOption(Setting).ConfigIgnored := not (Setting in AHonored);
  end;
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
