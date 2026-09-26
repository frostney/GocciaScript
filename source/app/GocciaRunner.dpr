program GocciaRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TextEncoding,
  TimingUtils,
  TextSemantics,

  Goccia.Application,
  Goccia.Builtins.Console,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  Goccia.CLI.Stdin,
  Goccia.CLI.SourceMaps,
  Goccia.CLI.SourcePipelineResult,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.SandboxHost,
  Goccia.CLI.SandboxMode,
  Goccia.CLI.Trust,
  CLI.ConfigFile,
  CLI.Parser,
  CLI.Options,
  Goccia.Capabilities,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Engine,
  Goccia.Executor.Interpreter,
  Goccia.Executor.Bytecode,
  Goccia.Executor,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.HostEnvironment,
  Goccia.HostEnvironment.JavaScript,
  Goccia.InstructionLimit,
  Goccia.Modules.Resolver,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.AST,
  Goccia.RuntimeExtensions.FFI,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.RuntimeProfiles.Loader,
  Goccia.Sandbox.Context,
  Goccia.Scope,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.CLI.JSON.Reporter,
  Goccia.SourcePipeline,
  Goccia.SourceMap,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Threading.Flags,
  Goccia.Threading.Init,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception,

  FileUtils,
  SandboxHostInputs;

type
  TScriptLoaderConsoleCapture = class
  private
    FOutputLines: TStringList;
    FStdoutLines: TStringList;
    FStderrLines: TStringList;
    function MethodWritesToStderr(const AMethod: string): Boolean;
    function UnformatLine(const AMethod, ALine: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CaptureOutput(const AMethod, ALine: string);
    function OutputText: string;
    function StdoutText: string;
    function StderrText: string;
  end;

  TScriptExecutionReport = record
    ResultValue: TGocciaValue;
    Timing: TCLIJSONTiming;
    MemoryStats: TCLIJSONMemoryStats;
  end;

  TScriptLoaderJSONFileResult = record
    FileName: string;
    JSON: string;
    StdoutText: string;
    StderrText: string;
    OutputText: string;
    ErrorJSON: string;
    Timing: TCLIJSONTiming;
    MemoryStats: TCLIJSONMemoryStats;
    Ok: Boolean;
  end;

  TScriptLoaderJSONFileResultArray = array[0..MaxInt div SizeOf(TScriptLoaderJSONFileResult) - 1] of TScriptLoaderJSONFileResult;
  PScriptLoaderJSONFileResultArray = ^TScriptLoaderJSONFileResultArray;

  TRunnerApp = class(TGocciaCLIApplication)
  private
    FOutputPath: TStringOption;
    FSilent: TFlagOption;
    FPrint: TFlagOption;
    FSourceMap: TStringOption;
    FHostEnvironmentModule: TStringOption;
    FGlobalFiles: TRepeatableOption;
    FInlineGlobals: TRepeatableOption;
    FLastPaths: TStringList;
    FLastDiagnosticPrincipal: Int64;
    { Sandbox mode is on: set from the command line in ValidateCommandLine,
      or from a trusted root-config sandbox section in ExecuteWithPaths. }
    FSandboxActive: Boolean;
    { The raw arguments switch sandbox mode on. Known before any option is
      parsed, so even an invalid value is reported on stderr, keeping stdout
      for the guest's output and the diff. }
    FSandboxRequestedByArguments: Boolean;
    FSandboxHost: TGocciaSandboxHost;

    procedure InitializeRuntime(const AEngine: TGocciaEngine);
    function HostCapabilityAllowFlags: TGocciaCapabilityScopes;
    function HostOnlyCommandLineOptions(
      const ABeforeConfig: Boolean): TGocciaCapabilityScopes;
    procedure ConfigureSandboxEngine(const AEngine: TGocciaEngine;
      const AContext: TGocciaSandboxContext; const AEntryPath: string;
      const AParentEngine: TGocciaEngine;
      const AParentHostEnvironment: TGocciaHostEnvironment);
    function ResolveSandboxEntry(const AHost: TGocciaSandboxHost;
      const ARequest: TGocciaSandboxModeRequest): string;
    procedure WriteSandboxDiff(const AHost: TGocciaSandboxHost;
      const ARequest: TGocciaSandboxModeRequest;
      const ADiffFile: TSandboxHostOutputFile);
    procedure VerifyRootConfig;
    procedure ApplyConfigSandboxLimits;
    function ConfigSandboxReason(
      const AVerdict: TGocciaConfigTrustVerdict): string;
    procedure WarnIgnoredConfigHostKeys;
    procedure RunSandbox(const ARequest: TGocciaSandboxModeRequest);
    function IsJsonOutput: Boolean;
    function IsCompactJsonOutput: Boolean;
    procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
      const AFileName: string);
    procedure ConfigureConsole(const AConsole: TGocciaConsole;
      const ACapture: TScriptLoaderConsoleCapture);
    procedure ApplyDataGlobalsToEngine(const AEngine: TGocciaEngine);
    procedure ApplyModuleGlobalsToEngine(const AEngine: TGocciaEngine);
    function ExecuteInterpreted(const ASource: TStringList; const AFileName: string;
      const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
    function RunBytecodeModule(const AEngine: TGocciaEngine;
      const AModule: TGocciaCompiledModule;
      const AFileName: string): TGocciaValue;
    function ExecuteBytecodeFromSource(const ASource: TStringList; const AFileName: string;
      const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
    function ExecuteBytecodeFromFile(const AFileName: string;
      const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
    procedure PrintHumanReadableResult(const AFileName: string;
      const AReport: TScriptExecutionReport; const AExtension: string);
    function RunSourceForJSON(const ASource: TStringList;
      const AFileName: string;
      const AMeasureMemory: Boolean = True): TScriptLoaderJSONFileResult;
    procedure RunSource(const ASource: TStringList; const AFileName: string);
    function RunScriptFromFileForJSON(const AFileName: string;
      const AMeasureMemory: Boolean = True): TScriptLoaderJSONFileResult;
    procedure RunJSONFiles(const AFiles: TStringList);
    procedure RunScriptFromFile(const AFileName: string);
    procedure RunScriptFromStdin;
    procedure ScriptWorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
    procedure RunScriptsParallel(const AFiles: TStringList;
      const AJobCount: Integer);
    procedure RunScripts(const APath: string);
  protected
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    function HonorsSandboxSection: Boolean; override;
    function CapabilityPolicyName: string; override;
    procedure Configure; override;
    procedure ConfigureCreatedEngine(const AEngine: TGocciaEngine;
      const AFileConfig: TConfigEntryArray); override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    function HasNonPathInput: Boolean; override;
    function ExtraHelpText: string; override;
    procedure ValidateCommandLine(const APaths: TStringList); override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    procedure HandleError(const AException: Exception); override;
    procedure AfterExecute; override;
  end;

{ TScriptLoaderConsoleCapture }

constructor TScriptLoaderConsoleCapture.Create;
begin
  inherited Create;
  FOutputLines := TStringList.Create;
  FStdoutLines := TStringList.Create;
  FStderrLines := TStringList.Create;
end;

destructor TScriptLoaderConsoleCapture.Destroy;
begin
  FStderrLines.Free;
  FStdoutLines.Free;
  FOutputLines.Free;
  inherited Destroy;
end;

function TScriptLoaderConsoleCapture.MethodWritesToStderr(
  const AMethod: string): Boolean;
begin
  Result := (AMethod = 'warn') or (AMethod = 'error') or
    (AMethod = 'assert') or (AMethod = 'trace');
end;

function TScriptLoaderConsoleCapture.UnformatLine(const AMethod,
  ALine: string): string;
var
  Prefix: string;
  PrefixStart: Integer;
begin
  Prefix := '';
  if AMethod = 'warn' then
    Prefix := 'Warning: '
  else if AMethod = 'error' then
    Prefix := 'Error: '
  else if AMethod = 'info' then
    Prefix := 'Info: '
  else if AMethod = 'debug' then
    Prefix := 'Debug: '
  else if AMethod = 'trace' then
    Prefix := 'Trace: ';

  if Prefix = '' then
    Exit(ALine);

  PrefixStart := 1;
  while (PrefixStart <= Length(ALine)) and (ALine[PrefixStart] = ' ') do
    Inc(PrefixStart);
  if Copy(ALine, PrefixStart, Length(Prefix)) = Prefix then
    Result := Copy(ALine, 1, PrefixStart - 1) +
      Copy(ALine, PrefixStart + Length(Prefix), MaxInt)
  else
    Result := ALine;
end;

procedure TScriptLoaderConsoleCapture.CaptureOutput(const AMethod,
  ALine: string);
begin
  FOutputLines.Add(ALine);
  if MethodWritesToStderr(AMethod) then
    FStderrLines.Add(UnformatLine(AMethod, ALine))
  else
    FStdoutLines.Add(UnformatLine(AMethod, ALine));
end;

function TScriptLoaderConsoleCapture.OutputText: string;
begin
  Result := FOutputLines.Text;
end;

function TScriptLoaderConsoleCapture.StdoutText: string;
begin
  Result := FStdoutLines.Text;
end;

function TScriptLoaderConsoleCapture.StderrText: string;
begin
  Result := FStderrLines.Text;
end;

{ TRunnerApp - Configure }

function RuntimeConsole(const AEngine: TGocciaEngine): TGocciaConsole;
var
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := GetRuntime(AEngine);
  if Assigned(Runtime) then
  begin
    ConsoleExtension := TGocciaConsoleRuntimeExtension(
      Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
    if Assigned(ConsoleExtension) then
      Exit(ConsoleExtension.BuiltinConsole);
  end;
  Result := nil;
end;

procedure TRunnerApp.InitializeRuntime(const AEngine: TGocciaEngine);
var
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := AttachRuntime(AEngine);
  ApplyLoaderRuntimeProfile(Runtime);
  InstallFFIIfGranted(Runtime);
end;

function TRunnerApp.UsageLine: string;
begin
  Result := '[file|directory|-] [options]' + sLineBreak +
    '       ' + Name + ' <file> [--copy <host>[=<sandbox>]]... [options]';
end;

function TRunnerApp.StdinUsage: TGocciaStdinUsage;
begin
  Result := suStdinDefaultWithREPL;
end;

{ --entry names the program, and the sandbox options switch to a mode that
  never reads stdin, so neither leaves the run without input. Checked
  before config is applied, so Present means the command line. }
function TRunnerApp.HasNonPathInput: Boolean;
begin
  Result := (SandboxOptions.CommandLineActivation <> '') or
    SandboxOptions.Entry.Present;
end;

function TRunnerApp.ExtraHelpText: string;
begin
  Result := SandboxModeHelpNote;
end;

function ArgumentsRequestSandbox: Boolean;
var
  Arguments: TCommandLineArguments;
  I: Integer;
begin
  Arguments := GetCommandLineArguments;
  for I := 0 to High(Arguments) do
  begin
    if Arguments[I] = '--' then
      Break;
    if (Arguments[I] = '--sandbox') or (Arguments[I] = '--copy') or
       (Arguments[I] = '--copy-rw') or
       (Copy(Arguments[I], 1, Length('--copy=')) = '--copy=') or
       (Copy(Arguments[I], 1, Length('--copy-rw=')) = '--copy-rw=') then
      Exit(True);
  end;
  Result := False;
end;

procedure TRunnerApp.Configure;
begin
  FSandboxRequestedByArguments := ArgumentsRequestSandbox;
  AddEngineOptions;
  AddCoverageOptions;
  AddProfilerOptions;
  AddSandboxOptions;

  FOutputPath := AddString('output',
    '"json" for structured JSON output, "compact-json" omits build, memory, stdout, stderr');
  FSilent := AddFlag('silent', 'Suppress console output from the script');
  FPrint := AddFlag('print',
    'Print the script''s last value to stdout (mirrors node -p / bun --print / deno eval -p)');
  FSourceMap := TStringOption(Add(TOptionalStringOption.Create('source-map',
    'Write a .map source map file (optional: explicit path)')));
  FSourceMap.WritesHostFile := True;
  FHostEnvironmentModule := AddString('host-environment',
    'Configure script-visible time and randomness from a module with named exports');
  FGlobalFiles := AddRepeatable('globals',
    'Inject globals from a JSON/JSON5/TOML/YAML file or a module with named exports');
  FInlineGlobals := AddRepeatable('global',
    'Inject a single global; value is parsed as JSON or kept as a string');
end;

procedure TRunnerApp.ConfigureCreatedEngine(const AEngine: TGocciaEngine;
  const AFileConfig: TConfigEntryArray);
var
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  HostEnvironmentEntry: TConfigEntry;
  HostEnvironmentEntries: TConfigEntryArray;
  HostEnvironmentModulePath: string;
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := AttachRuntime(AEngine);

  { The command line names a host module; a config names a guest one. }
  HostEnvironmentModulePath := '';
  HostEnvironmentEntry := Default(TConfigEntry);
  if FHostEnvironmentModule.FromCommandLine then
    HostEnvironmentModulePath := FHostEnvironmentModule.Value
  else if not TryFindConfigEntry(AFileConfig, 'host-environment',
    HostEnvironmentEntry) then
  begin
    HostEnvironmentEntries := ConfigNamedEntries(FHostEnvironmentModule);
    if Length(HostEnvironmentEntries) > 0 then
      HostEnvironmentEntry := HostEnvironmentEntries[0];
  end;

  if (HostEnvironmentModulePath <> '') or
     (HostEnvironmentEntry.Value <> '') then
  begin
    if Assigned(EngineOptions) and
       ResolveFlagOption(EngineOptions.Deterministic, AFileConfig) then
      raise TParseError.Create(
        '--host-environment cannot be combined with --deterministic.');
    if HostEnvironmentModulePath <> '' then
      ConfigureHostEnvironmentFromModule(AEngine, HostEnvironmentModulePath)
    else
      ConfigureConfiguredHostEnvironment(AEngine, HostEnvironmentEntry.Value,
        HostEnvironmentEntry.SourcePath);
  end;

  ApplyLoaderRuntimeProfile(Runtime);
  InstallFFIIfGranted(Runtime);
  if Assigned(EngineOptions) and
     ResolveFlagOption(EngineOptions.ExperimentalAST, AFileConfig) then
    Runtime.Install(TGocciaASTRuntimeExtension.Create);
  ConsoleExtension := TGocciaConsoleRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
  if LogFileOpen and Assigned(ConsoleExtension) and
     Assigned(ConsoleExtension.BuiltinConsole) then
    ConsoleExtension.BuiltinConsole.LogCallback := HandleConsoleLog;
end;

{ A config's `output` is ignored in sandbox mode, whose stdout carries the
  guest's output and the diff. }
function TRunnerApp.IsJsonOutput: Boolean;
begin
  Result := (not FSandboxActive) and FOutputPath.Present and
    ((FOutputPath.Value = 'json') or (FOutputPath.Value = 'compact-json'));
end;

function TRunnerApp.IsCompactJsonOutput: Boolean;
begin
  Result := (not FSandboxActive) and FOutputPath.Present and
    (FOutputPath.Value = 'compact-json');
end;

{ Host mode grants everything the command line and trusted config ask for.
  Sandbox mode loads no host files and has no node_modules lookup, so net is
  the only capability it grants (ADR 0122). }
function TRunnerApp.HonoredCapabilities: TGocciaHonoredCapabilities;
begin
  if FSandboxActive then
    Result := [gcNet]
  else
    Result := ALL_CAPABILITIES;
end;

function TRunnerApp.HonorsSandboxSection: Boolean;
begin
  Result := True;
end;

function TRunnerApp.CapabilityPolicyName: string;
begin
  if FSandboxActive then
    Result := Name + ' sandbox mode'
  else
    Result := Name;
end;

{ The --allow-* flags of capabilities sandbox mode cannot grant. They are
  command-line-only, so Present means the command line. }
function TRunnerApp.HostCapabilityAllowFlags: TGocciaCapabilityScopes;
var
  Capability: TGocciaCapability;
begin
  Result := nil;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if (Capability <> gcNet) and
       EngineOptions.Capabilities.AllowOption(Capability).Present then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := '--' +
        EngineOptions.Capabilities.AllowOption(Capability).LongName;
    end;
end;

{ The host-mode options given on the command line. The same keys from a
  config are ignored in sandbox mode, so one project config serves both
  modes. ABeforeConfig: no config has been applied yet, so Present means the
  command line. }
function TRunnerApp.HostOnlyCommandLineOptions(
  const ABeforeConfig: Boolean): TGocciaCapabilityScopes;

  procedure Check(const AOption: TOptionBase);
  begin
    if Assigned(AOption) and (AOption.FromCommandLine or
       (ABeforeConfig and AOption.Present)) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := '--' + AOption.LongName;
    end;
  end;

begin
  Result := nil;
  Check(MultifileOption);
  Check(FOutputPath);
  Check(CoverageOptions.Enabled);
  Check(CoverageOptions.Format);
  Check(CoverageOptions.OutputPath);
  Check(ProfilerOptions.Mode);
  Check(ProfilerOptions.OutputPath);
  Check(ProfilerOptions.Format);
  Check(FSourceMap);
  Check(FHostEnvironmentModule);
  Check(FGlobalFiles);
  Check(FInlineGlobals);
end;

{ Sandbox mode switched on from the command line is known before any config
  is read, so the trust gate and the capability checks see the right set,
  and a host-filesystem grant gets an error that says why. }
procedure TRunnerApp.ValidateCommandLine(const APaths: TStringList);
begin
  if SandboxOptions.CommandLineActivation = '' then
    Exit;
  RejectHostCapabilityFlags(HostCapabilityAllowFlags,
    SandboxOptions.CommandLineActivation);
  { Before Validate, whose host-mode checks (--profile-output needs
    --profile, ...) would otherwise report the wrong problem. }
  RejectHostModeOptions(HostOnlyCommandLineOptions(True),
    SandboxOptions.CommandLineActivation);
  FSandboxActive := True;
end;

{ TRunnerApp - Validate }

function TRunnerApp.ConfigSandboxReason(
  const AVerdict: TGocciaConfigTrustVerdict): string;
begin
  Result := Format('the "%s" section of %s', [SANDBOX_CONFIG_KEY,
    AVerdict.Request.Sandbox.SourcePath]);
end;

procedure TRunnerApp.Validate;
var
  Verdict: TGocciaConfigTrustVerdict;
begin
  { A root-config sandbox section switches sandbox mode on too; decided here
    so a host-mode option gets the sandbox-mode error rather than a host-mode
    check's. Its trust is checked in ExecuteWithPaths, once the audit log is
    open. --ignore-config-permissions ignores the section. }
  if not FSandboxActive then
  begin
    Verdict := RootConfigVerdict;
    if Verdict.Request.Sandbox.Declared and
       (Verdict.State <> ctsIgnored) then
    begin
      FSandboxActive := True;
      RejectHostCapabilityFlags(HostCapabilityAllowFlags,
        ConfigSandboxReason(Verdict));
      RejectHostModeOptions(HostOnlyCommandLineOptions(False),
        ConfigSandboxReason(Verdict));
    end;
  end;

  inherited Validate;
  { The host-mode implications below (profiling and coverage force bytecode)
    do not apply: those options are refused on the command line, and ignored
    from config, in sandbox mode. }
  if FSandboxActive then
    Exit;

  // --profile-format implies --profile=functions when no explicit --profile given
  if ProfilerOptions.Format.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('functions');

  // Profiling requires bytecode mode regardless of the --mode option.
  if ProfilerOptions.Mode.Present then
    EngineOptions.Mode.Apply('bytecode');

  // --coverage-format / --coverage-output imply --coverage, so every later
  // reader can test Enabled alone instead of repeating the three-way check.
  // Mirrors TTestRunnerApp.Validate.
  if CoverageOptions.Format.Present or CoverageOptions.OutputPath.Present then
    CoverageOptions.Enabled.Apply('');

  // Coverage requires bytecode mode regardless of the --mode option: the
  // interpreter only instruments the entry file and counts statements per
  // AST node instead of per executed line.
  if CoverageOptions.Enabled.Present then
    EngineOptions.Mode.Apply('bytecode');

  if ProfilerOptions.OutputPath.Present and not ProfilerOptions.Mode.Present then
    raise TParseError.Create(
      '--profile-output requires --profile=opcodes|functions|all.');

  if ProfilerOptions.Format.Matches(pfFlamegraph) and
     not ProfilerOptions.OutputPath.Present then
    raise TParseError.Create(
      '--profile-format=flamegraph requires --profile-output=<path>.');
end;

{ TRunnerApp - Core logic }

procedure TRunnerApp.WriteSourceMapIfEnabled(
  const ASourceMap: TGocciaSourceMap; const AFileName: string);
var
  MapOutputPath: string;
begin
  if not FSourceMap.Present then
    Exit;
  MapOutputPath := FSourceMap.ValueOr('');
  if MapOutputPath = '' then
    MapOutputPath := AFileName + EXT_MAP;
  WriteSourceMapIfAvailable(ASourceMap, MapOutputPath, AFileName, AFileName,
    not IsJsonOutput);
end;

procedure TRunnerApp.ConfigureConsole(const AConsole: TGocciaConsole;
  const ACapture: TScriptLoaderConsoleCapture);
begin
  if not Assigned(AConsole) then
    Exit;

  AConsole.Enabled := not FSilent.Present;
  if IsJsonOutput and Assigned(ACapture) then
    AConsole.OutputCallback := ACapture.CaptureOutput
  else
    AConsole.OutputCallback := nil;
  AConsole.OutputLines := nil;
end;

function CapturedOutputText(const ACapture: TScriptLoaderConsoleCapture): string;
begin
  if Assigned(ACapture) then
    Result := ACapture.OutputText
  else
    Result := '';
end;

function CapturedStdoutText(const ACapture: TScriptLoaderConsoleCapture): string;
begin
  if Assigned(ACapture) then
    Result := ACapture.StdoutText
  else
    Result := '';
end;

function CapturedStderrText(const ACapture: TScriptLoaderConsoleCapture): string;
begin
  if Assigned(ACapture) then
    Result := ACapture.StderrText
  else
    Result := '';
end;

procedure PrintJSONSuccess(const AReport: TScriptExecutionReport;
  const ACapture: TScriptLoaderConsoleCapture; const AFileName: string;
  const ACompact: Boolean);
begin
  WriteLn(BuildCLIScriptSuccessJSON(AFileName, AReport.ResultValue,
    CapturedOutputText(ACapture), CapturedStdoutText(ACapture),
    CapturedStderrText(ACapture), AReport.Timing, AReport.MemoryStats, 1, 1,
    ACompact));
end;

procedure PrintJSONError(const E: Exception; const AReport: TScriptExecutionReport;
  const ACapture: TScriptLoaderConsoleCapture; const ADefaultFileName: string;
  const ACompact: Boolean);
var
  ErrorInfo: TCLIJSONErrorInfo;
begin
  ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
  if ErrorInfo.FileName = '' then
    ErrorInfo.FileName := ADefaultFileName;
  WriteLn(BuildCLIScriptErrorJSON(ADefaultFileName, CapturedOutputText(ACapture),
    CapturedStdoutText(ACapture), CapturedStderrText(ACapture), ErrorInfo,
    AReport.Timing, AReport.MemoryStats, 1, 1, ACompact));
end;

procedure AddTiming(var ATarget: TCLIJSONTiming;
  const ASource: TCLIJSONTiming);
begin
  ATarget.LexTimeNanoseconds := ATarget.LexTimeNanoseconds +
    ASource.LexTimeNanoseconds;
  ATarget.ParseTimeNanoseconds := ATarget.ParseTimeNanoseconds +
    ASource.ParseTimeNanoseconds;
  ATarget.CompileTimeNanoseconds := ATarget.CompileTimeNanoseconds +
    ASource.CompileTimeNanoseconds;
  ATarget.ExecuteTimeNanoseconds := ATarget.ExecuteTimeNanoseconds +
    ASource.ExecuteTimeNanoseconds;
  ATarget.TotalTimeNanoseconds := ATarget.TotalTimeNanoseconds +
    ASource.TotalTimeNanoseconds;
end;

function BuildAggregateScriptLoaderJSON(const AResults: array of TScriptLoaderJSONFileResult;
  const AMemoryStats: TCLIJSONMemoryStats; const AWorkerCount,
  AAvailableWorkerCount: Integer; const ACompact: Boolean): string;
var
  I: Integer;
  Ok: Boolean;
  StdoutText, StderrText, OutputText, FilesJSON: string;
  ErrorJSON: string;
  Timing: TCLIJSONTiming;
begin
  FillChar(Timing, SizeOf(Timing), 0);
  Ok := True;
  StdoutText := '';
  StderrText := '';
  OutputText := '';
  ErrorJSON := 'null';
  FilesJSON := '';

  for I := 0 to High(AResults) do
  begin
    Ok := Ok and AResults[I].Ok;
    if (not AResults[I].Ok) and (ErrorJSON = 'null') then
      ErrorJSON := AResults[I].ErrorJSON;
    StdoutText := StdoutText + AResults[I].StdoutText;
    StderrText := StderrText + AResults[I].StderrText;
    OutputText := OutputText + AResults[I].OutputText;
    AddTiming(Timing, AResults[I].Timing);
    if FilesJSON <> '' then
      FilesJSON := FilesJSON + ',';
    FilesJSON := FilesJSON + AResults[I].JSON;
  end;

  Result := BuildCLIReportJSON(Ok, OutputText, StdoutText, StderrText,
    ErrorJSON, Timing, AMemoryStats, AWorkerCount, AAvailableWorkerCount,
    FilesJSON, '', ACompact);
end;

{ Globals files the command line names are the user's own, read as host
  files; those a config names are read under the script's capability set
  (InjectConfiguredGlobals). }
procedure TRunnerApp.ApplyDataGlobalsToEngine(const AEngine: TGocciaEngine);
var
  I: Integer;
  Entries: TConfigEntryArray;
  Pair: TScriptLoaderGlobalPair;
begin
  Entries := ConfigNamedEntries(FGlobalFiles);
  for I := 0 to High(Entries) do
    if IsStructuredGlobalsFile(Entries[I].Value) then
      InjectConfiguredGlobals(AEngine, Entries[I].Value,
        Entries[I].SourcePath);
  if FGlobalFiles.FromCommandLine then
    for I := 0 to FGlobalFiles.Values.Count - 1 do
      if IsStructuredGlobalsFile(FGlobalFiles.Values[I]) then
      begin
        if IsYAMLGlobalsFile(FGlobalFiles.Values[I]) then
          AEngine.InjectGlobalsFromYAML(ReadFileText(FGlobalFiles.Values[I]))
        else if IsJSON5GlobalsFile(FGlobalFiles.Values[I]) then
          AEngine.InjectGlobalsFromJSON5(ReadFileText(FGlobalFiles.Values[I]))
        else if IsTOMLGlobalsFile(FGlobalFiles.Values[I]) then
          AEngine.InjectGlobalsFromTOML(ReadFileText(FGlobalFiles.Values[I]))
        else
          AEngine.InjectGlobalsFromJSON(ReadFileText(FGlobalFiles.Values[I]));
      end;

  for I := 0 to FInlineGlobals.Values.Count - 1 do
  begin
    Pair := ParseGlobalPair(FInlineGlobals.Values[I]);
    AEngine.InjectGlobal(Pair.Key, ParseInlineGlobalValue(Pair.ValueText));
  end;
end;

procedure TRunnerApp.ApplyModuleGlobalsToEngine(const AEngine: TGocciaEngine);
var
  I: Integer;
  Entries: TConfigEntryArray;
begin
  Entries := ConfigNamedEntries(FGlobalFiles);
  for I := 0 to High(Entries) do
    if not IsStructuredGlobalsFile(Entries[I].Value) then
      InjectConfiguredGlobals(AEngine, Entries[I].Value,
        Entries[I].SourcePath);
  if FGlobalFiles.FromCommandLine then
    for I := 0 to FGlobalFiles.Values.Count - 1 do
      if not IsStructuredGlobalsFile(FGlobalFiles.Values[I]) then
        AEngine.InjectGlobalsFromModule(FGlobalFiles.Values[I]);
end;

function TRunnerApp.ExecuteInterpreted(const ASource: TStringList;
  const AFileName: string; const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  ScriptResult: TGocciaScriptResult;
  SourceMap: TGocciaSourceMap;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := CreateEngine(AFileName, ASource, Executor);
    try
      FLastDiagnosticPrincipal := Engine.ModuleLoader.DiagnosticScope.Principal;
      Engine.SuppressWarnings := GIsWorkerThread or
        IsJsonOutput;
      ConfigureConsole(RuntimeConsole(Engine), ACapture);
      ApplyDataGlobalsToEngine(Engine);
      StartExecutionTimeout(EngineOptions.Timeout.Milliseconds(0));
      StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
      try
        ApplyModuleGlobalsToEngine(Engine);
        ScriptResult := Engine.Execute;
      finally
        ClearExecutionTimeout;
        ClearInstructionLimit;
        SourceMap := Engine.TakeLastSourceMap;
        try
          if Assigned(SourceMap) and Assigned(ASource) then
            SourceMap.SetSourceContent(0, StringListToLFText(ASource));
          WriteSourceMapIfEnabled(SourceMap, AFileName);
        finally
          SourceMap.Free;
        end;
      end;
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
  end;

  Result.ResultValue := ScriptResult.Result;
  Result.Timing.LexTimeNanoseconds := ScriptResult.LexTimeNanoseconds;
  Result.Timing.ParseTimeNanoseconds := ScriptResult.ParseTimeNanoseconds;
  Result.Timing.CompileTimeNanoseconds := 0;
  Result.Timing.ExecuteTimeNanoseconds := ScriptResult.ExecuteTimeNanoseconds;
  Result.Timing.TotalTimeNanoseconds := ScriptResult.TotalTimeNanoseconds;
end;

function TRunnerApp.RunBytecodeModule(const AEngine: TGocciaEngine;
  const AModule: TGocciaCompiledModule;
  const AFileName: string): TGocciaValue;
begin
  Result := AEngine.RunModuleForSourceType(AModule, AFileName);
end;

function TRunnerApp.ExecuteBytecodeFromSource(const ASource: TStringList;
  const AFileName: string; const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
var
  SourcePipelineResult: TGocciaCLISourcePipelineResult;
  Module: TGocciaCompiledModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  PipelineOptions: TGocciaSourcePipelineOptions;
  StartTime, CompileStart, CompileEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Executor := TGocciaBytecodeExecutor.Create;
  try
    Engine := CreateEngine(AFileName, ASource, Executor);
    try
      FLastDiagnosticPrincipal := Engine.ModuleLoader.DiagnosticScope.Principal;
      ConfigureConsole(RuntimeConsole(Engine), ACapture);
      ApplyDataGlobalsToEngine(Engine);

      PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
      PipelineOptions.Preprocessors := Engine.Preprocessors;
      PipelineOptions.Compatibility := Engine.Compatibility;
      PipelineOptions.LabelStatementsEnabled := Engine.LabelStatementsEnabled;
      PipelineOptions.ForInLoopsEnabled := Engine.ForInLoopsEnabled;
      PipelineOptions.ExperimentalJSModuleSourceEnabled :=
        Engine.ExperimentalJSModuleSourceEnabled;
      PipelineOptions.WarningUnsupportedFeatures :=
        Engine.WarningUnsupportedFeatures;
      PipelineOptions.SourceType := Engine.SourceType;
      ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(
        PipelineOptions);
      try
        SourcePipelineResult := TGocciaCLISourcePipelineResult.Parse(ASource, AFileName,
          PipelineOptions, IsJsonOutput);
        try
          Result.Timing.LexTimeNanoseconds :=
            SourcePipelineResult.LexTimeNanoseconds;
          Result.Timing.ParseTimeNanoseconds :=
            SourcePipelineResult.ParseTimeNanoseconds;
          WriteSourceMapIfEnabled(SourcePipelineResult.SourceMap, AFileName);
          SourcePipelineResult.RegisterCoverageSource(AFileName);

          CompileStart := GetNanoseconds;
          Module := Engine.CompileModule(SourcePipelineResult.ProgramNode);
          CompileEnd := GetNanoseconds;
          Result.Timing.CompileTimeNanoseconds := CompileEnd - CompileStart;
        finally
          SourcePipelineResult.Free;
        end;

        StartExecutionTimeout(EngineOptions.Timeout.Milliseconds(0));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          ApplyModuleGlobalsToEngine(Engine);
          Result.ResultValue := RunBytecodeModule(Engine, Module, AFileName);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;
      finally
        ActiveOptionsScope.Free;
      end;
      ExecEnd := GetNanoseconds;
      Result.Timing.ExecuteTimeNanoseconds := ExecEnd - CompileEnd;
      Result.Timing.TotalTimeNanoseconds := ExecEnd - StartTime;
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
  end;
end;

function TRunnerApp.ExecuteBytecodeFromFile(const AFileName: string;
  const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
var
  Module: TGocciaCompiledModule;
  RetainedModule: TGocciaCompiledModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  StartTime, LoadEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Module := Goccia.Bytecode.Binary.LoadModuleFromFile(AFileName);
  LoadEnd := GetNanoseconds;
  try
    Executor := TGocciaBytecodeExecutor.Create;
    try
      Engine := CreateEngine(AFileName, nil, Executor);
      try
        FLastDiagnosticPrincipal := Engine.ModuleLoader.DiagnosticScope.Principal;
        Engine.RetainModule(Module);
        RetainedModule := Module;
        Module := nil;
        ConfigureConsole(RuntimeConsole(Engine), ACapture);
        ApplyDataGlobalsToEngine(Engine);
        StartExecutionTimeout(EngineOptions.Timeout.Milliseconds(0));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          ApplyModuleGlobalsToEngine(Engine);
          Result.ResultValue := RunBytecodeModule(Engine,
            RetainedModule, AFileName);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;
        ExecEnd := GetNanoseconds;
        Result.Timing.LexTimeNanoseconds := 0;
        Result.Timing.ParseTimeNanoseconds := 0;
        Result.Timing.CompileTimeNanoseconds := 0;
        Result.Timing.ExecuteTimeNanoseconds := ExecEnd - LoadEnd;
        Result.Timing.TotalTimeNanoseconds := ExecEnd - StartTime;
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
  finally
    Module.Free;
  end;
end;

procedure TRunnerApp.PrintHumanReadableResult(const AFileName: string;
  const AReport: TScriptExecutionReport; const AExtension: string);
var
  LoadTimeNanoseconds: Int64;
begin
  if GIsWorkerThread then Exit;

  if AExtension = EXT_GBC then
  begin
    LoadTimeNanoseconds := AReport.Timing.TotalTimeNanoseconds -
      AReport.Timing.ExecuteTimeNanoseconds;
    WriteLn('Running bytecode: ', AFileName);
    WriteLn(SysUtils.Format('  Load: %s | Execute: %s | Total: %s',
      [FormatDuration(LoadTimeNanoseconds),
       FormatDuration(AReport.Timing.ExecuteTimeNanoseconds),
       FormatDuration(AReport.Timing.TotalTimeNanoseconds)]));
  end
  else if EngineOptions.Mode.Matches(emBytecode) then
  begin
    WriteLn('Running script (bytecode): ', AFileName);
    WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
      [FormatDuration(AReport.Timing.LexTimeNanoseconds),
       FormatDuration(AReport.Timing.ParseTimeNanoseconds),
       FormatDuration(AReport.Timing.CompileTimeNanoseconds),
       FormatDuration(AReport.Timing.ExecuteTimeNanoseconds),
       FormatDuration(AReport.Timing.TotalTimeNanoseconds)]));
  end
  else
  begin
    WriteLn('Running script (interpreted): ', AFileName);
    WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
      [FormatDuration(AReport.Timing.LexTimeNanoseconds),
       FormatDuration(AReport.Timing.ParseTimeNanoseconds),
       FormatDuration(AReport.Timing.ExecuteTimeNanoseconds),
       FormatDuration(AReport.Timing.TotalTimeNanoseconds)]));
  end;

  { Mirrors `node -p` / `bun --print` / `deno eval -p`: silent by default,
    prints the bare value (incl. `undefined`) only when --print is set. }
  if not FPrint.Present then
    Exit;
  if not Assigned(AReport.ResultValue) then
    Exit;
  WriteLn(AReport.ResultValue.ToStringLiteral.Value);
end;

procedure TRunnerApp.RunSource(const ASource: TStringList;
  const AFileName: string);
var
  Extension: string;
  Report: TScriptExecutionReport;
  Capture: TScriptLoaderConsoleCapture;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  StartTime: Int64;
begin
  FLastDiagnosticPrincipal := 0;
  FillChar(Report, SizeOf(Report), 0);
  Report.ResultValue := nil;
  Report.MemoryStats := DefaultCLIJSONMemoryStats;

  Capture := nil;
  if IsJsonOutput then
    Capture := TScriptLoaderConsoleCapture.Create;
  try
    StartTime := GetNanoseconds;
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    try
      Extension := LowerCase(ExtractFileExt(AFileName));

      if Extension = EXT_GBC then
        Report := ExecuteBytecodeFromFile(AFileName, Capture)
      else
        case EngineOptions.Mode.ValueOr(emInterpreted) of
          emInterpreted: Report := ExecuteInterpreted(ASource, AFileName, Capture);
          emBytecode:    Report := ExecuteBytecodeFromSource(ASource, AFileName, Capture);
        end;

      Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
      if IsJsonOutput then
        PrintJSONSuccess(Report, Capture, AFileName, IsCompactJsonOutput)
      else
        PrintHumanReadableResult(AFileName, Report, Extension);
    except
      { A config usage error is the invocation's, not this file's. }
      on E: TCLIUsageError do
        raise;
      on E: Exception do
      begin
        Report.Timing.TotalTimeNanoseconds := GetNanoseconds - StartTime;
        if Report.Timing.TotalTimeNanoseconds >
           Report.Timing.LexTimeNanoseconds + Report.Timing.ParseTimeNanoseconds +
           Report.Timing.CompileTimeNanoseconds then
          Report.Timing.ExecuteTimeNanoseconds :=
            Report.Timing.TotalTimeNanoseconds -
            Report.Timing.LexTimeNanoseconds -
            Report.Timing.ParseTimeNanoseconds -
            Report.Timing.CompileTimeNanoseconds;
        Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
        if not GIsWorkerThread then
        begin
          if IsJsonOutput then
            PrintJSONError(E, Report, Capture, AFileName, IsCompactJsonOutput)
          else if E is TGocciaError then
            WriteLn(FormatHostErrorDiagnostic(TGocciaError(E), IsColorTerminal))
          else if E is TGocciaThrowValue then
            WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName,
              ASource, IsColorTerminal, FLastDiagnosticPrincipal,
              TGocciaThrowValue(E).Suggestion))
          else if E is EGocciaBytecodeThrow then
            WriteLn(FormatThrowDetail(EGocciaBytecodeThrow(E).ThrownValue,
              AFileName, ASource, IsColorTerminal, FLastDiagnosticPrincipal,
              EGocciaBytecodeThrow(E).Suggestion))
          else
            WriteLn('Fatal error: ', E.Message);
        end;
        ExitCode := 1;
      end;
    end;
  finally
    Capture.Free;
  end;
end;

function TRunnerApp.RunSourceForJSON(const ASource: TStringList;
  const AFileName: string;
  const AMeasureMemory: Boolean): TScriptLoaderJSONFileResult;
var
  Extension: string;
  Report: TScriptExecutionReport;
  Capture: TScriptLoaderConsoleCapture;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  StartTime: Int64;
  ErrorInfo: TCLIJSONErrorInfo;
begin
  FillChar(Report, SizeOf(Report), 0);
  Report.ResultValue := nil;
  Report.MemoryStats := DefaultCLIJSONMemoryStats;

  Result.FileName := AFileName;
  Result.JSON := '';
  Result.StdoutText := '';
  Result.StderrText := '';
  Result.OutputText := '';
  Result.ErrorJSON := 'null';
  Result.Ok := False;
  FillChar(Result.Timing, SizeOf(Result.Timing), 0);
  Result.MemoryStats := DefaultCLIJSONMemoryStats;

  Capture := TScriptLoaderConsoleCapture.Create;
  try
    StartTime := GetNanoseconds;
    if AMeasureMemory then
      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    try
      Extension := LowerCase(ExtractFileExt(AFileName));
      if Extension = EXT_GBC then
        Report := ExecuteBytecodeFromFile(AFileName, Capture)
      else
        case EngineOptions.Mode.ValueOr(emInterpreted) of
          emInterpreted: Report := ExecuteInterpreted(ASource, AFileName, Capture);
          emBytecode:    Report := ExecuteBytecodeFromSource(ASource, AFileName, Capture);
        end;

      if AMeasureMemory then
        Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement)
      else
        Report.MemoryStats := DefaultCLIJSONMemoryStats;
      Result.StdoutText := CapturedStdoutText(Capture);
      Result.StderrText := CapturedStderrText(Capture);
      Result.OutputText := CapturedOutputText(Capture);
      Result.ErrorJSON := 'null';
      Result.Timing := Report.Timing;
      Result.MemoryStats := Report.MemoryStats;
      Result.Ok := True;
      Result.JSON := BuildCLIScriptFileSuccessJSON(AFileName,
        Report.ResultValue, Result.OutputText, Result.StdoutText,
        Result.StderrText, Report.Timing, Report.MemoryStats,
        IsCompactJsonOutput);
    except
      on E: Exception do
      begin
        Report.Timing.TotalTimeNanoseconds := GetNanoseconds - StartTime;
        if Report.Timing.TotalTimeNanoseconds >
           Report.Timing.LexTimeNanoseconds + Report.Timing.ParseTimeNanoseconds +
           Report.Timing.CompileTimeNanoseconds then
          Report.Timing.ExecuteTimeNanoseconds :=
            Report.Timing.TotalTimeNanoseconds -
            Report.Timing.LexTimeNanoseconds -
            Report.Timing.ParseTimeNanoseconds -
            Report.Timing.CompileTimeNanoseconds;
        if AMeasureMemory then
          Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement)
        else
          Report.MemoryStats := DefaultCLIJSONMemoryStats;
        ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
        if ErrorInfo.FileName = '' then
          ErrorInfo.FileName := AFileName;
        Result.StdoutText := CapturedStdoutText(Capture);
        Result.StderrText := CapturedStderrText(Capture);
        Result.OutputText := CapturedOutputText(Capture);
        Result.ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
        Result.Timing := Report.Timing;
        Result.MemoryStats := Report.MemoryStats;
        Result.Ok := False;
        Result.JSON := BuildCLIScriptFileErrorJSON(AFileName,
          Result.OutputText, Result.StdoutText, Result.StderrText,
          ErrorInfo, Report.Timing, Report.MemoryStats,
          IsCompactJsonOutput);
        ExitCode := 1;
      end;
    end;
  finally
    Capture.Free;
  end;
end;

procedure TRunnerApp.RunScriptFromFile(const AFileName: string);
var
  Source: TStringList;
begin
  if LowerCase(ExtractFileExt(AFileName)) = EXT_GBC then
  begin
    RunSource(nil, AFileName);
    Exit;
  end;

  Source := SourceRegistry.Load(AFileName);
  try
    RunSource(Source, AFileName);
  finally
    Source.Free;
  end;
end;

function TRunnerApp.RunScriptFromFileForJSON(const AFileName: string;
  const AMeasureMemory: Boolean): TScriptLoaderJSONFileResult;
var
  Source: TStringList;
  ErrorInfo: TCLIJSONErrorInfo;
  Timing: TCLIJSONTiming;
  StartTime: Int64;
begin
  if LowerCase(ExtractFileExt(AFileName)) = EXT_GBC then
    Exit(RunSourceForJSON(nil, AFileName, AMeasureMemory));

  StartTime := GetNanoseconds;
  try
    Source := SourceRegistry.Load(AFileName);
  except
    on E: Exception do
    begin
      FillChar(Timing, SizeOf(Timing), 0);
      Timing.TotalTimeNanoseconds := GetNanoseconds - StartTime;
      Timing.ExecuteTimeNanoseconds := Timing.TotalTimeNanoseconds;
      ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
      if ErrorInfo.FileName = '' then
        ErrorInfo.FileName := AFileName;
      Result.FileName := AFileName;
      Result.JSON := BuildCLIScriptFileErrorJSON(AFileName, '', '', '',
        ErrorInfo, Timing, DefaultCLIJSONMemoryStats, IsCompactJsonOutput);
      Result.StdoutText := '';
      Result.StderrText := '';
      Result.OutputText := '';
      Result.ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
      Result.Timing := Timing;
      Result.MemoryStats := DefaultCLIJSONMemoryStats;
      Result.Ok := False;
      ExitCode := 1;
      Exit;
    end;
  end;
  try
    Result := RunSourceForJSON(Source, AFileName, AMeasureMemory);
  finally
    Source.Free;
  end;
end;

procedure TRunnerApp.RunJSONFiles(const AFiles: TStringList);
var
  Results: array of TScriptLoaderJSONFileResult;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  MemoryStats: TCLIJSONMemoryStats;
  MainMemoryStats: TCLIJSONMemoryStats;
  WorkerMemoryStats: TCLIJSONMemoryStats;
  Pool: TGocciaThreadPool;
  CoverageTracker: TGocciaCoverageTracker;
  CoverageWasEnabled: Boolean;
  I, JobCount: Integer;
begin
  WorkerMemoryStats := DefaultCLIJSONMemoryStats;
  SetLength(Results, AFiles.Count);
  JobCount := GetJobCount(AFiles.Count);

  if JobCount > 1 then
  begin
    // Force all shared prototypes to be initialised on the main thread before
    // any worker starts. That throwaway engine is loader infrastructure, so do
    // not register its <thread-init> source in the user's coverage report.
    // Preserve the tracker state instead of filtering by file name so a user
    // source can never be hidden. Mirrors TTestRunnerApp.RunScriptsFromFilesParallel.
    CoverageTracker := TGocciaCoverageTracker.Instance;
    CoverageWasEnabled := False;
    if Assigned(CoverageTracker) then
    begin
      CoverageWasEnabled := CoverageTracker.Enabled;
      CoverageTracker.Enabled := False;
    end;
    try
      EnsureSharedPrototypesInitialized(WarmUpCapabilities(AFiles),
        InitializeRuntime);
    finally
      if Assigned(CoverageTracker) then
        CoverageTracker.Enabled := CoverageWasEnabled;
    end;
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    Pool := TGocciaThreadPool.Create(JobCount);
    try
      Pool.EnableCoverage := CoverageOptions.Enabled.Present;
      if (TGarbageCollector.Instance <> nil) then
        Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
      Pool.RunAll(AFiles, ScriptWorkerProc, @Results[0]);
      WorkerMemoryStats := Pool.MemoryStats;
      // Worker hits live in per-thread trackers; without this merge every
      // --coverage run under --jobs=N reports only the main thread's hits.
      if Pool.EnableCoverage and (TGocciaCoverageTracker.Instance <> nil) then
        Pool.MergeCoverageInto(TGocciaCoverageTracker.Instance);
    finally
      Pool.Free;
    end;
    MainMemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);

    for I := 0 to AFiles.Count - 1 do
      if not Results[I].Ok then
        ExitCode := 1;
    MemoryStats := CombineCLIJSONMemoryStats(
      MainMemoryStats, WorkerMemoryStats, True);
  end
  else
  begin
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    for I := 0 to AFiles.Count - 1 do
    begin
      Results[I] := RunScriptFromFileForJSON(AFiles[I], False);
      if not Results[I].Ok then
        ExitCode := 1;
    end;
    MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
  end;

  WriteLn(BuildAggregateScriptLoaderJSON(Results,
    MemoryStats, JobCount, GetJobCount(AFiles.Count),
    IsCompactJsonOutput));
end;

procedure TRunnerApp.RunScriptFromStdin;
var
  Source, SectionSource, Names: TStringList;
  I: Integer;
begin
  { ExecuteWithPaths verified the working directory's config first. }
  Source := ReadSourceFromText(Input);
  if not MultifileEnabled then
  begin
    try
      RunSource(Source, STDIN_FILE_NAME);
    finally
      Source.Free;
    end;
    Exit;
  end;

  // Multifile stdin: ownership of Source transfers to SplitStdinMultifile
  // (which either registers it under <stdin> as a single section or
  // splits it into multiple sections and frees the wrapper).
  Names := SplitStdinMultifile(Source);
  try
    for I := 0 to Names.Count - 1 do
    begin
      if I > 0 then
        WriteLn;
      SectionSource := SourceRegistry.Load(Names[I]);
      try
        RunSource(SectionSource, Names[I]);
      finally
        SectionSource.Free;
      end;
    end;
  finally
    Names.Free;
  end;
end;

procedure TRunnerApp.ScriptWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
var
  JSONResults: PScriptLoaderJSONFileResultArray;
  ErrorInfo: TCLIJSONErrorInfo;
  Timing: TCLIJSONTiming;
  MemoryStats: TCLIJSONMemoryStats;
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    if IsJsonOutput then
    begin
      JSONResults := PScriptLoaderJSONFileResultArray(AData);
      JSONResults^[AIndex] := RunScriptFromFileForJSON(AFileName, True);
      if not JSONResults^[AIndex].Ok then
        AErrorMessage := 'failed';
    end
    else
      RunScriptFromFile(AFileName);
  except
    on E: Exception do
    begin
      AErrorMessage := E.Message;
      if IsJsonOutput then
      begin
        JSONResults := PScriptLoaderJSONFileResultArray(AData);
        ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
        if ErrorInfo.FileName = '' then
          ErrorInfo.FileName := AFileName;
        FillChar(Timing, SizeOf(Timing), 0);
        MemoryStats := DefaultCLIJSONMemoryStats;
        JSONResults^[AIndex].FileName := AFileName;
        JSONResults^[AIndex].StdoutText := '';
        JSONResults^[AIndex].StderrText := '';
        JSONResults^[AIndex].OutputText := '';
        JSONResults^[AIndex].ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
        JSONResults^[AIndex].Timing := Timing;
        JSONResults^[AIndex].MemoryStats := MemoryStats;
        JSONResults^[AIndex].Ok := False;
        JSONResults^[AIndex].JSON := BuildCLIScriptFileErrorJSON(
          AFileName, '', '', '', ErrorInfo, Timing, MemoryStats,
          IsCompactJsonOutput);
      end;
      ExitCode := 1;
    end;
  end;
end;

procedure TRunnerApp.RunScriptsParallel(const AFiles: TStringList;
  const AJobCount: Integer);
var
  Pool: TGocciaThreadPool;
  CoverageTracker: TGocciaCoverageTracker;
  CoverageWasEnabled: Boolean;
  I: Integer;
begin
  // Force all shared prototypes to be initialised on the main thread before any
  // worker starts. That throwaway engine is loader infrastructure, so do not
  // register its <thread-init> source in the user's coverage report. Preserve
  // the tracker state instead of filtering by file name so a user source can
  // never be hidden. Mirrors TTestRunnerApp.RunScriptsFromFilesParallel.
  CoverageTracker := TGocciaCoverageTracker.Instance;
  CoverageWasEnabled := False;
  if Assigned(CoverageTracker) then
  begin
    CoverageWasEnabled := CoverageTracker.Enabled;
    CoverageTracker.Enabled := False;
  end;
  try
    EnsureSharedPrototypesInitialized(WarmUpCapabilities(AFiles),
      InitializeRuntime);
  finally
    if Assigned(CoverageTracker) then
      CoverageTracker.Enabled := CoverageWasEnabled;
  end;

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
    Pool.EnableCoverage := CoverageOptions.Enabled.Present;
    if (TGarbageCollector.Instance <> nil) then
      Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
    Pool.RunAll(AFiles, ScriptWorkerProc);
    // Worker hits live in per-thread trackers; without this merge every
    // --coverage run under --jobs=N reports only the main thread's hits.
    if Pool.EnableCoverage and (TGocciaCoverageTracker.Instance <> nil) then
      Pool.MergeCoverageInto(TGocciaCoverageTracker.Instance);

    for I := 0 to AFiles.Count - 1 do
      if Pool.Results[I].ErrorMessage <> '' then
      begin
        WriteLn('Error in ', AFiles[I], ': ', Pool.Results[I].ErrorMessage);
        ExitCode := 1;
      end;
  finally
    Pool.Free;
  end;
end;

procedure TRunnerApp.RunScripts(const APath: string);
var
  Files, RawFiles, SinglePath: TStringList;
  I: Integer;
begin
  if IsStdinPath(APath) then
  begin
    RunScriptFromStdin;
    Exit;
  end;

  if DirectoryExists(APath) then
  begin
    RawFiles := FindAllFiles(APath, ScriptExtensions);
    try
      Files := ExpandMultifileFiles(RawFiles);
    finally
      RawFiles.Free;
    end;
    try
      if GetJobCount(Files.Count) > 1 then
      begin
        WriteLn(SysUtils.Format('Running %d files with %d workers',
          [Files.Count, GetJobCount(Files.Count)]));
        RunScriptsParallel(Files, GetJobCount(Files.Count));
      end
      else
        for I := 0 to Files.Count - 1 do
        begin
          if I > 0 then
            WriteLn;
          RunScriptFromFile(Files[I]);
        end;
    finally
      Files.Free;
    end;
  end
  else if FileExists(APath) then
  begin
    if MultifileEnabled then
    begin
      SinglePath := TStringList.Create;
      try
        SinglePath.Add(APath);
        Files := ExpandMultifileFiles(SinglePath);
      finally
        SinglePath.Free;
      end;
      try
        if GetJobCount(Files.Count) > 1 then
        begin
          WriteLn(SysUtils.Format('Running %d files with %d workers',
            [Files.Count, GetJobCount(Files.Count)]));
          RunScriptsParallel(Files, GetJobCount(Files.Count));
        end
        else
          for I := 0 to Files.Count - 1 do
          begin
            if I > 0 then
              WriteLn;
            RunScriptFromFile(Files[I]);
          end;
      finally
        Files.Free;
      end;
    end
    else
      RunScriptFromFile(APath);
  end
  else
    raise Exception.Create('Path not found: ' + APath);
end;

{ TRunnerApp - Sandbox mode }

procedure TRunnerApp.ConfigureSandboxEngine(const AEngine: TGocciaEngine;
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AParentEngine: TGocciaEngine;
  const AParentHostEnvironment: TGocciaHostEnvironment);
var
  Runtime: TGocciaRuntimeCore;
  Console: TGocciaConsole;
  EmptyConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
begin
  { A sandbox path is not a host path: a per-file config walk from it would
    climb the host filesystem from its root and find a config unrelated to
    the run. The root config, already merged into the options, governs. }
  EmptyConfig := nil;
  Verdict := FileConfigVerdict('');
  if Assigned(AParentEngine) then
    AEngine.ConfigureCapabilityAuditAsChildOf(AParentEngine)
  else
  begin
    AEngine.CapabilityProvenance := CapabilityProvenance(Verdict);
    ConfigureCapabilityAudit(AEngine);
  end;
  if Assigned(AParentHostEnvironment) then
    AEngine.HostEnvironment.ConfigureAsChildOf(AParentHostEnvironment)
  else if ResolveFlagOption(EngineOptions.Deterministic, EmptyConfig) then
    AEngine.HostEnvironment.UseDeterministicProfile;

  Runtime := AttachRuntime(AEngine);
  ApplyLoaderRuntimeProfile(Runtime);
  Runtime.Install(TGocciaSandboxRuntimeExtension.Create(AContext));
  if ResolveFlagOption(EngineOptions.ExperimentalAST, EmptyConfig) then
    Runtime.Install(TGocciaASTRuntimeExtension.Create);

  ApplyFileConfigToEngine(AEngine, EngineOptions, EmptyConfig, AEntryPath,
    Verdict.AcceptedUnsafe);
  { Sandbox mode sees only the virtual filesystem: a config's modules,
    globals, and host-environment, which read host files, are not applied
    (WarnIgnoredConfigHostKeys says so). The command line's --module and
    --modules are the user's own. }
  ApplyCommandLineVirtualModulesToEngine(AEngine);

  Console := RuntimeConsole(AEngine);
  if Assigned(Console) then
  begin
    Console.Enabled := not FSilent.Present;
    Console.OutputCallback := FSandboxHost.CaptureConsoleLine;
    if LogFileOpen then
      Console.LogCallback := HandleConsoleLog;
  end;
end;

{ The sandbox path to run. A host entry inside a copied input runs from
  there; any other host entry is copied read-only to /<basename>. }
function TRunnerApp.ResolveSandboxEntry(const AHost: TGocciaSandboxHost;
  const ARequest: TGocciaSandboxModeRequest): string;
var
  Target: string;
begin
  if ARequest.EntrySandbox <> '' then
    Exit(AHost.Context.Fs.Normalize(ARequest.EntrySandbox));
  if AHost.Inputs.SandboxPathOfHostFile(ARequest.EntryHost, Result) then
    Exit;
  Target := DefaultSandboxPathFor(ARequest.EntryHost, ARequest.EntryHost,
    'the entry');
  if AHost.Context.Fs.Exists(Target) then
    raise TCLIUsageError.CreateFmt(
      'the entry %s would be copied to %s, which a copied input already ' +
      'fills; name the entry with --entry=%s, or give that input an ' +
      'explicit =<sandbox> path', [ExtractFileName(ARequest.EntryHost),
      Target, Target]);
  Result := AHost.CopyIn(ARequest.EntryHost, Target, False);
end;

procedure TRunnerApp.WriteSandboxDiff(const AHost: TGocciaSandboxHost;
  const ARequest: TGocciaSandboxModeRequest;
  const ADiffFile: TSandboxHostOutputFile);
var
  DiffText, Problem: string;
  Bytes: TBytes;
  ErrorOffset: Integer;
begin
  if not ARequest.DiffRequested then
    Exit;
  DiffText := AHost.DiffText(ARequest.DiffFormat = sdfUnified);
  if ARequest.DiffFile = '' then
  begin
    Write(DiffText);
    Exit;
  end;
  if not TryEncodeUTF8(DiffText, Bytes, ErrorOffset) then
    raise EConvertError.Create('the diff cannot be encoded as UTF-8');
  if not ADiffFile.Write(Bytes, Problem) then
    raise Exception.CreateFmt('diff file %s: %s', [ARequest.DiffFile,
      Problem]);
end;

procedure TRunnerApp.VerifyRootConfig;
var
  ConfigPaths: TStringList;
begin
  ConfigPaths := TStringList.Create;
  try
    ConfigPaths.Add(RootConfigPath);
    VerifyGoverningConfigs(ConfigPaths);
  finally
    ConfigPaths.Free;
  end;
end;

{ --max-fs-bytes and --max-fs-nodes are ConfigIgnored, so host mode never
  reads (or validates) them from a config; sandbox mode takes them from the
  root config here, unless the command line set them. }
procedure TRunnerApp.ApplyConfigSandboxLimits;
var
  Entries: TConfigEntryArray;
  Entry: TConfigEntry;
  Limits: array[0..1] of TInt64Option;
  I: Integer;
begin
  if RootConfigPath = '' then
    Exit;
  Entries := LoadFileConfig(RootConfigPath);
  Limits[0] := SandboxOptions.MaxFsBytes;
  Limits[1] := SandboxOptions.MaxFsNodes;
  for I := 0 to High(Limits) do
  begin
    if Limits[I].FromCommandLine or
       not TryFindConfigEntry(Entries, Limits[I].LongName, Entry) then
      Continue;
    if Entry.InArray or (Entry.Kind in [cvkObject, cvkUnsupported,
       cvkEmptyArray]) then
      raise TParseError.CreateFmt('%s: "%s" must be a single value',
        [Entry.SourcePath, Entry.Key]);
    try
      Limits[I].Apply(Entry.Value);
    except
      on E: EOptionValueError do
        raise TParseError.CreateFmt('Invalid value for "%s" in %s: %s (%s)',
          [Entry.Key, Entry.SourcePath, E.Value, E.Reason]);
    end;
  end;
end;

{ The root config's keys that read host files, which sandbox mode does not
  apply: one warning each, in the style of an unsupported capability. }
procedure TRunnerApp.WarnIgnoredConfigHostKeys;
const
  HOST_FILE_KEYS: array[0..4] of string = ('modules', 'module', 'globals',
    'global', 'host-environment');
var
  Entries: TConfigEntryArray;
  I, J: Integer;
  Key: string;
begin
  if RootConfigPath = '' then
    Exit;
  Entries := LoadFileConfig(RootConfigPath);
  for I := 0 to High(HOST_FILE_KEYS) do
    for J := 0 to High(Entries) do
    begin
      Key := Entries[J].Key;
      if (Key = HOST_FILE_KEYS[I]) or
         (Copy(Key, 1, Length(HOST_FILE_KEYS[I]) + 1) =
          HOST_FILE_KEYS[I] + '.') then
      begin
        WarnOnce(RootConfigPath + #0 + HOST_FILE_KEYS[I], Format(
          'Warning: %s sets "%s", which %s does not apply; ignoring it',
          [RootConfigPath, HOST_FILE_KEYS[I], CapabilityPolicyName]));
        Break;
      end;
    end;
end;

procedure TRunnerApp.RunSandbox(const ARequest: TGocciaSandboxModeRequest);
var
  Host: TGocciaSandboxHost;
  RunResult: TGocciaSandboxRunResult;
  Report: TStringList;
  EntryPath: string;
  DiffFile: TSandboxHostOutputFile;
  I: Integer;
begin
  { The root config is the only one that governs a sandbox run; its
    requests are checked before anything is copied in. }
  VerifyRootConfig;
  WarnIgnoredConfigHostKeys;
  { The entry's own config, when it is not the root config, has no say in a
    sandbox run; a sandbox section there is not read. }
  if ARequest.EntryHost <> '' then
    WarnIfSandboxSectionUnread(DiscoverFileConfigPath(ARequest.EntryHost));
  for I := 0 to High(ARequest.Notes) do
    WriteLn(ErrOutput, ARequest.Notes[I]);

  Host := TGocciaSandboxHost.Create(ARequest.MaxFsBytes, ARequest.MaxFsNodes);
  FSandboxHost := Host;
  try
    Host.Bytecode := EngineOptions.Mode.Matches(emBytecode);
    Host.TimeoutMilliseconds := EngineOptions.Timeout.Milliseconds(0);
    Host.MaxInstructions := EngineOptions.MaxInstructions.ValueOr(0);
    Host.ImportMapPath := EngineOptions.ImportMap.ValueOr('');
    Host.Aliases.Assign(EngineOptions.Aliases.Values);
    { Filtered to net by HonoredCapabilities. }
    Host.RootCapabilities := ResolveEngineCapabilities('');
    Host.OnConfigureEngine := ConfigureSandboxEngine;

    for I := 0 to High(ARequest.Inputs) do
      Host.CopyIn(ARequest.Inputs[I].HostPath, ARequest.Inputs[I].SandboxPath,
        ARequest.Inputs[I].ReadWrite);
    EntryPath := ResolveSandboxEntry(Host, ARequest);
    { Pinned before the run, so a directory swapped for a link while it runs
      cannot carry the diff elsewhere. }
    if ARequest.DiffFile <> '' then
      DiffFile := TSandboxHostOutputFile.Pin(ARequest.DiffFile);
    Host.CaptureBaseline;

    RunResult := Host.Run(EntryPath);
    if RunResult.Output <> '' then
      Write(RunResult.Output);
    if RunResult.ErrorOutput <> '' then
      Write(ErrOutput, RunResult.ErrorOutput)
    else if (not RunResult.Ok) and (RunResult.ErrorMessage <> '') then
      WriteLn(ErrOutput, RunResult.ErrorMessage);
    { Mirrors host mode: the bare value, `undefined` included. }
    if FPrint.Present and Assigned(RunResult.ResultValue) then
      WriteLn(RunResult.ResultValue.ToStringLiteral.Value);
    if not RunResult.Ok then
      ExitCode := RunResult.ExitCode;

    { ADR 0119: the host, not the guest, writes back, and only after a run
      that succeeded. The report is diagnostics, so stdout keeps only the
      guest's output and the diff. }
    if Host.Inputs.HasReadWriteInput then
    begin
      Report := TStringList.Create;
      try
        if RunResult.Ok then
        begin
          { A write that failed, or an input replaced during the run, fails
            the invocation: the host asked for files it did not get. }
          if not Host.Inputs.ApplyWriteBack(Host.Inputs.PlanWriteBack(
             Host.Context.Baseline), Report) then
            ExitCode := 1;
        end
        else
          Report.Add(WRITE_BACK_REPORT_PREFIX +
            'skipped, the run did not succeed.');
        for I := 0 to Report.Count - 1 do
          WriteLn(ErrOutput, Report[I]);
      finally
        Report.Free;
      end;
    end;

    WriteSandboxDiff(Host, ARequest, DiffFile);
  finally
    FSandboxHost := nil;
    Host.Free;
  end;
end;

{ TRunnerApp - ExecuteWithPaths }

procedure TRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I, SectionIndex: Integer;
  Files, RawFiles, StdinNames: TStringList;
  Source, SectionSource: TStringList;
  JSONResult: TScriptLoaderJSONFileResult;
  JSONResults: array of TScriptLoaderJSONFileResult;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  Verdict: TGocciaConfigTrustVerdict;
  SandboxCommandLine: TGocciaSandboxCommandLine;
  SandboxRequest: TGocciaSandboxModeRequest;
begin
  FLastPaths := APaths;

  { A trusted sandbox section in the root config switches sandbox mode on
    too; an untrusted one stops the run here, before anything is read from
    it. The mode is set before the config's requests are checked, so the
    ones sandbox mode cannot grant are warned about rather than needing
    trust. --ignore-config-permissions ignores the section. }
  Verdict := RootConfigVerdict;
  if Verdict.Request.Sandbox.Declared and (Verdict.State <> ctsIgnored) then
  begin
    FSandboxActive := True;
    VerifyRootConfig;
    Verdict := RootConfigVerdict;
  end;
  if FSandboxActive then
    ApplyConfigSandboxLimits;
  SandboxCommandLine.Paths := APaths;
  SandboxCommandLine.DeniedAllowFlags := HostCapabilityAllowFlags;
  SandboxCommandLine.HostOnlyOptions := HostOnlyCommandLineOptions(False);
  SandboxCommandLine.WorkingDirectory := GetCurrentDir;
  SandboxRequest := ResolveSandboxMode(SandboxOptions, Verdict.Request.Sandbox,
    Verdict.GrantsAccepted, SandboxCommandLine);
  if SandboxRequest.Active then
  begin
    FSandboxActive := True;
    RunSandbox(SandboxRequest);
    Exit;
  end;
  FSandboxActive := False;

  if FSourceMap.Present and (FSourceMap.ValueOr('') = '') and
     ((APaths.Count = 0) or
      ((APaths.Count = 1) and IsStdinPath(APaths[0]))) then
    raise TParseError.Create(
      '--source-map=<file> is required when reading source from stdin.');

  if (FSourceMap.ValueOr('') <> '') and
     ((APaths.Count > 1) or
      ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TParseError.Create(
      '--source-map=<file> supports a single input file or stdin.');

  // Use Present rather than the value to catch the bare --source-map
  // form too: even if every section gets its own derived .map file, a
  // user passing --source-map alongside --multifile is almost certainly
  // signalling intent that does not match the multi-output reality.
  if FSourceMap.Present and MultifileEnabled then
    raise TParseError.Create(
      '--source-map cannot be combined with --multifile (an input '
      + 'may expand to multiple sections).');

  { File inputs are checked in the ValidateFileConfigs pass; source from
    stdin is governed by the working directory's config. }
  if (APaths.Count = 0) or ((APaths.Count = 1) and IsStdinPath(APaths[0])) then
    ValidateFileConfig(STDIN_FILE_NAME);

  if IsJsonOutput then
  begin
    if (APaths.Count = 0) or
       ((APaths.Count = 1) and IsStdinPath(APaths[0])) then
    begin
      Source := ReadSourceFromText(Input);
      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      if MultifileEnabled then
      begin
        StdinNames := SplitStdinMultifile(Source);
        try
          SetLength(JSONResults, StdinNames.Count);
          for SectionIndex := 0 to StdinNames.Count - 1 do
          begin
            SectionSource := SourceRegistry.Load(StdinNames[SectionIndex]);
            try
              JSONResults[SectionIndex] :=
                RunSourceForJSON(SectionSource, StdinNames[SectionIndex], False);
              if not JSONResults[SectionIndex].Ok then
                ExitCode := 1;
            finally
              SectionSource.Free;
            end;
          end;
          WriteLn(BuildAggregateScriptLoaderJSON(JSONResults,
            FinishCLIJSONMemoryMeasurement(MemoryMeasurement),
            1, StdinNames.Count, IsCompactJsonOutput));
        finally
          StdinNames.Free;
        end;
      end
      else
      begin
        try
          JSONResult := RunSourceForJSON(Source, STDIN_FILE_NAME, False);
          SetLength(JSONResults, 1);
          JSONResults[0] := JSONResult;
          if not JSONResult.Ok then
            ExitCode := 1;
          WriteLn(BuildAggregateScriptLoaderJSON(JSONResults,
            FinishCLIJSONMemoryMeasurement(MemoryMeasurement), 1, 1,
            IsCompactJsonOutput));
        finally
          Source.Free;
        end;
      end;
      Exit;
    end;

    RawFiles := TStringList.Create;
    try
      for I := 0 to APaths.Count - 1 do
      begin
        if IsStdinPath(APaths[I]) then
          raise TParseError.Create(
            'stdin is supported only as the sole input.');
        if DirectoryExists(APaths[I]) then
          RawFiles.AddStrings(FindAllFiles(APaths[I], ScriptExtensions))
        else if FileExists(APaths[I]) then
          RawFiles.Add(APaths[I])
        else
          raise Exception.Create('Path not found: ' + APaths[I]);
      end;
      Files := ExpandMultifileFiles(RawFiles);
      try
        RunJSONFiles(Files);
      finally
        Files.Free;
      end;
    finally
      RawFiles.Free;
    end;
    Exit;
  end;

  if APaths.Count = 0 then
    RunScriptFromStdin
  else if (APaths.Count = 1) and IsStdinPath(APaths[0]) then
    RunScriptFromStdin
  else
  begin
    { Reject mixing "-" with file paths so stdin cannot silently be
      interleaved with on-disk files. Matches the rule enforced by
      --output=json mode and by GocciaTestRunner / GocciaBenchmarkRunner. }
    for I := 0 to APaths.Count - 1 do
      if IsStdinPath(APaths[I]) then
        raise TParseError.Create(
          'stdin is supported only as the sole input.');

    { Every config governing the inputs is checked before any of them runs. }
    RawFiles := TStringList.Create;
    try
      for I := 0 to APaths.Count - 1 do
        if DirectoryExists(APaths[I]) then
        begin
          Files := FindAllFiles(APaths[I], ScriptExtensions);
          try
            RawFiles.AddStrings(Files);
          finally
            Files.Free;
          end;
        end
        else if FileExists(APaths[I]) then
          RawFiles.Add(APaths[I]);
      { A missing path is reported by RunScripts itself. }
      ValidateFileConfigs(RawFiles);
    finally
      RawFiles.Free;
    end;

    for I := 0 to APaths.Count - 1 do
    begin
      if I > 0 then
        WriteLn;
      RunScripts(APaths[I]);
    end;
  end;
end;

{ TRunnerApp - HandleError }

procedure TRunnerApp.HandleError(const AException: Exception);
begin
  { In sandbox mode stdout carries only the guest's output and the diff; the
    guest's own failures are reported by RunSandbox, so what reaches here is
    the host's (a copy that failed, an invalid value) and goes to stderr. }
  if FSandboxActive or FSandboxRequestedByArguments then
    WriteLn(ErrOutput, 'Error: ', AException.Message)
  else if IsJsonOutput then
    WriteLn(BuildCLIScriptErrorJSON('', '', '', '', ExceptionToCLIJSONErrorInfo(AException),
      Default(TCLIJSONTiming), DefaultCLIJSONMemoryStats, 1, 1,
      IsCompactJsonOutput))
  else
    inherited HandleError(AException);
end;

{ TRunnerApp - AfterExecute }

procedure TRunnerApp.AfterExecute;
var
  ProfileOpcodes, ProfileFunctions: Boolean;
  ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
begin
  { Coverage and profiling are host-mode reports; a config that asks for
    them is ignored in sandbox mode, which keeps stdout to the guest's
    output and the diff. }
  if FSandboxActive then
    Exit;
  if (CoverageOptions.Enabled.Present or CoverageOptions.Format.Present or
      CoverageOptions.OutputPath.Present) and
     (TGocciaCoverageTracker.Instance <> nil) then
  begin
    if not IsJsonOutput then
    begin
      PrintCoverageSummary(TGocciaCoverageTracker.Instance);
      if Assigned(FLastPaths) and (FLastPaths.Count = 1) and
         FileExists(FLastPaths[0]) then
        PrintCoverageDetail(TGocciaCoverageTracker.Instance, FLastPaths[0]);
    end;
    if CoverageOptions.Format.Matches(cfLcov) and
       (CoverageOptions.OutputPath.ValueOr('') <> '') then
      WriteCoverageLcov(TGocciaCoverageTracker.Instance,
        CoverageOptions.OutputPath.Value);
    if CoverageOptions.Format.Matches(cfJson) and
       (CoverageOptions.OutputPath.ValueOr('') <> '') then
      WriteCoverageJSON(TGocciaCoverageTracker.Instance,
        CoverageOptions.OutputPath.Value);
  end;

  ProfileOpcodes := False;
  ProfileFunctions := False;
  if ProfilerOptions.Mode.Present then
  begin
    ProfileMode := ProfilerOptions.Mode.Value;
    ProfileOpcodes := (ProfileMode = Goccia.CLI.Options.pmOpcodes) or
                      (ProfileMode = Goccia.CLI.Options.pmAll);
    ProfileFunctions := (ProfileMode = Goccia.CLI.Options.pmFunctions) or
                        (ProfileMode = Goccia.CLI.Options.pmAll);
  end;

  if (ProfileOpcodes or ProfileFunctions) and
     (TGocciaProfiler.Instance <> nil) then
  begin
    if not IsJsonOutput then
    begin
      if ProfileOpcodes then
      begin
        PrintOpcodeProfile(TGocciaProfiler.Instance);
        PrintOpcodePairProfile(TGocciaProfiler.Instance);
        PrintScalarHitRate(TGocciaProfiler.Instance);
        PrintShapeSaturation(TGocciaProfiler.Instance);
      end;
      if ProfileFunctions then
        PrintFunctionProfile(TGocciaProfiler.Instance);
    end;
    if ProfilerOptions.OutputPath.Present then
    begin
      if ProfilerOptions.Format.Matches(pfFlamegraph) then
        WriteCollapsedStacks(TGocciaProfiler.Instance,
          ProfilerOptions.OutputPath.Value)
      else
        WriteProfileJSON(TGocciaProfiler.Instance,
          ProfilerOptions.OutputPath.Value);
    end;
  end;
end;

{ Entry point }

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TRunnerApp, 'GocciaRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
