unit Goccia.CLI.Options;

{$I Goccia.inc}

interface

uses
  CLI.ConfigFile,
  CLI.Options,

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
    FTimeout: TIntegerOption;
    FMaxMemory: TInt64Option;
    FMaxInstructions: TInt64Option;
    FUnsafeFFI: TFlagOption;
    FUnsafeFunctionConstructor: TFlagOption;
    FUnsafeShadowRealm: TFlagOption;
    FWarningUnsupportedFeatures: TFlagOption;
    FStackSize: TIntegerOption;
    FStrictTypes: TFlagOption;
    FAllowedHosts: TRepeatableOption;
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
    property Timeout: TIntegerOption read FTimeout;
    property MaxMemory: TInt64Option read FMaxMemory;
    property MaxInstructions: TInt64Option read FMaxInstructions;
    property UnsafeFFI: TFlagOption read FUnsafeFFI;
    property UnsafeFunctionConstructor: TFlagOption read FUnsafeFunctionConstructor;
    property UnsafeShadowRealm: TFlagOption read FUnsafeShadowRealm;
    property WarningUnsupportedFeatures: TFlagOption read FWarningUnsupportedFeatures;
    property StackSize: TIntegerOption read FStackSize;
    property StrictTypes: TFlagOption read FStrictTypes;
    property AllowedHosts: TRepeatableOption read FAllowedHosts;
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

function CompatibilityFlagDescriptor(
  const AFlag: TGocciaCompatibility): TGocciaCompatibilityFlagDescriptor;
procedure ResolveCompatibilityFlags(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray;
  out AFlags: TGocciaCompatibilityFlags);
function TryApplyCompatibilityFlagArg(const AArg: string;
  var AFlags: TGocciaCompatibilityFlags): Boolean;

implementation

const
  ENGINE_FIXED_OPTION_COUNT = 17;

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
  FTimeout.Free;
  FMaxMemory.Free;
  FMaxInstructions.Free;
  FUnsafeFFI.Free;
  FUnsafeFunctionConstructor.Free;
  FUnsafeShadowRealm.Free;
  FWarningUnsupportedFeatures.Free;
  FStackSize.Free;
  FStrictTypes.Free;
  FAllowedHosts.Free;
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
  Result[Index] := FWarningUnsupportedFeatures;
  Inc(Index);
  Result[Index] := FStackSize;
  Inc(Index);
  Result[Index] := FStrictTypes;
  Inc(Index);
  Result[Index] := FAllowedHosts;
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
