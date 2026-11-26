program GocciaRunner;

{ GocciaRunner: the command line, configs, trust and capability policy (the
  base class), each entry file run by a TGocciaEngine (Goccia.Embed).
  `./build.pas runner` builds it as build/GocciaRunner.

  Sandbox mode runs each program on an engine over the virtual filesystem
  (Goccia.CLI.SandboxHost).

  Install mode (--add, --remove, --install, --update) is the old runner's
  (Goccia.Packages.Install); provider imports resolve through the shared
  resolver, granted, audited and checked against their pins here.

  Bytecode files are the engine's own (Goccia.Artifact, from GocciaBundler). }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  CLI.Parser,
  FileUtils,
  SandboxHostInputs,
  SandboxVirtualFileSystem,
  TextEncoding,
  TextSemantics,
  TimingUtils,

  Goccia.Application,
  Goccia.CLI.Application,
  Goccia.CLI.JSON.Reporter,
  Goccia.CLI.FileSettings,
  Goccia.CLI.FileEngine,
  Goccia.CLI.SandboxHost,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.SandboxMode,
  Goccia.CLI.Stdin,
  Goccia.CLI.SourceMaps,
  Goccia.CLI.Trust,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Error,
  Goccia.Error.Suggestions,
  Goccia.FileExtensions,
  Goccia.HostEnvironment,
  Goccia.JSX.Transformer,
  Goccia.Modules.Configuration,
  Goccia.Modules.Resolver,
  Goccia.ScriptLoader.Globals,
  Goccia.Packages.Address,
  Goccia.Packages.Install,
  Goccia.Packages.Store,
  Goccia.Packages.Transport,
  Goccia.Profiler,
  Goccia.Profiler.Compile,
  Goccia.Profiler.Report,
  Goccia.Sandbox.Context,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,

  Goccia.Artifact,
  Goccia.Core,
  Goccia.Embed,
  Goccia.HostAPI,
  Goccia.Sandbox,
  Goccia.VM,
  Goccia.ThreadCleanupRegistry;

const
  { the import map --add creates when the project has none }
  PROJECT_IMPORT_MAP_NAME = 'goccia.json';

type
  { an install mode run that failed (not a usage error): its message on stderr, exit 1 }
  EGocciaInstallModeFailure = class(Exception);

  { one file's part of a JSON report }
  TGocciaFileReport = record
    FileName: string;
    Ok: Boolean;
    OutputText, StdoutText, StderrText: string;
    ResultJSON: string;
    ErrorInfo: TCLIJSONErrorInfo;
    Timing: TCLIJSONTiming;
    { the file's engine heap and its thread's memory manager, from the engine's creation to the end of its run }
    Memory: TCLIJSONMemoryStats;
  end;

  TGocciaRunnerApp = class(TGocciaCLIApplication)
  private
    FReports: array of TGocciaFileReport;
    { a directory's files run at once without a JSON report (GocciaRunner's RunScriptsParallel): each prints its
      console output and its failure, not its banner or value }
    FQuietWorkers: Boolean;
    { the main thread's memory manager when the run began (a multi-file report's memory.heap) }
    FHeapStart: THeapStatus;
    { the JSON report's workers: the files run at once (1 when one after another) }
    FWorkers: Integer;
    { --coverage: the main thread's tracker (the base class makes it), which each file's thread merges its own into
      under FCoverageLock; FCoveragePaths the run's paths (one file shows its detail) }
    FCoverageMain: TGocciaCoverageTracker;
    FCoverageLock: TRTLCriticalSection;
    FCoveragePaths: TStringList;
    { GocciaRunner's options, all registered so that its command lines and configs parse the same; the ones the engine
      does not act on yet are listed in the header }
    FOutputPath: TStringOption;
    FSilent: TFlagOption;
    FPrint: TFlagOption;
    FPreferBytecode: TFlagOption;
    FSourceMap: TStringOption;
    FProfileDeterministic: TFlagOption;
    FHostEnvironmentModule: TStringOption;
    FGlobalFiles: TRepeatableOption;
    FInlineGlobals: TRepeatableOption;
    FAddPackages: TRepeatableOption;
    FRemovePackages: TRepeatableOption;
    FInstallPackages: TFlagOption;
    FUpdatePackages: TOptionalStringOption;
    FFrozen: TFlagOption;
    FCheckRefs: TFlagOption;
    FAcceptMovedTags: TFlagOption;
    { sandbox mode is on: set from the command line in ValidateCommandLine, or from a trusted root-config sandbox
      section; FSandboxRequestedByArguments: the raw arguments switch it on, known before any option is parsed }
    FSandboxActive: Boolean;
    FSandboxRequestedByArguments: Boolean;
    FSandboxHost: TGocciaSandboxHost;
    { --profile: the main thread's profilers, which each file's thread records into (profiled files run one at a
      time, as GocciaRunner runs them on its one reporting thread) }
    FProfiler: TGocciaProfiler;
    FCompileProfiler: TGocciaCompileProfiler;
    function JobCount(const AFileCount: Integer): Integer;
    function SandboxDeterministic: Boolean;
    procedure ConfigureSandboxEngine(const ARun: TGocciaSandboxRun);
    function FormatSandboxFailure(const ARun: TGocciaSandboxRun; const AResult: TGocciaRunResult): string;
    function ResolveSandboxEntry(const AHost: TGocciaSandboxHost; const ARequest: TGocciaSandboxModeRequest): string;
    procedure WriteSandboxDiff(const AHost: TGocciaSandboxHost; const ARequest: TGocciaSandboxModeRequest);
    procedure VerifyRootConfig;
    function ConfigRequestsSandbox: Boolean;
    procedure ApplyConfigSandboxLimits;
    function ConfigSandboxReason(const AVerdict: TGocciaConfigTrustVerdict): string;
    procedure WarnIgnoredConfigHostKeys;
    procedure RunSandbox(const ARequest: TGocciaSandboxModeRequest);
    function HostCapabilityAllowFlags: TGocciaCapabilityScopes;
    function HostOnlyCommandLineOptions(const ABeforeConfig: Boolean): TGocciaCapabilityScopes;
    function DefineCommandLineVirtualModules(const AEngine: TGocciaEngine; out AError: string): Boolean;
    function FailureText(const AFileName: string; const AResult: TGocciaRunResult; const ASource: TStringList;
      AEngine: TGocciaEngine; const AWithheld: string; const AFrameSource: TSandboxVirtualFileSystem;
      AFrames, ASuggestions, AColor: Boolean): string;
    procedure RunSource(const AFileName: string; const ASource: TStringList; AReportIndex: Integer = -1);
    procedure RunFilesParallel(const AFiles: TStringList);
    { a parallel file whose source does not load: its report, the class and message of the failure (as
      GocciaRunner's worker reports it) }
    procedure ReportLoadFailure(AReportIndex: Integer; const AFileName: string; const E: Exception;
      AStart: Int64);
    procedure RunOnWorker(const AFileName: string; const ASource: TStringList);
    procedure RunFile(const AFileName: string);
    function InjectGlobals(const AEngine: TGocciaEngine; out AError: string): Boolean;
    function DefineVirtualModules(const AEngine: TGocciaEngine; const AFileConfigPath: string;
      out AError: string): Boolean;
    procedure ReportFailure(const AFileName: string;
      const AResult: TGocciaRunResult; const ASource: TStringList; AEngine: TGocciaEngine; const AWithheld: string);
    function HostEnvironmentPath(const AFileConfig: TConfigEntryArray; ADeterministic: Boolean): string;
    function IsJsonOutput: Boolean;
    function IsCompactJsonOutput: Boolean;
    procedure PrintJsonReport;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    function HonorsSandboxSection: Boolean; override;
    function CapabilityPolicyName: string; override;
    function HasNonPathInput: Boolean; override;
    function ExtraHelpText: string; override;
    procedure ValidateCommandLine(const APaths: TStringList); override;
    procedure Validate; override;
    function InstallModeActive: Boolean;
    function ExclusiveModeName: string; override;
    procedure ValidateInstallMode(const APaths: TStringList);
    procedure RunInstallMode;
    procedure WriteInstallLine(const ALine: string);
    procedure AuditProviderFetch(const AAllowed: Boolean; const ASubject, AReason: string);
    procedure AuditProviderInstall(const AAllowed: Boolean; const ASubject, AReason: string);
    procedure HandleError(const AException: Exception); override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    procedure AfterExecute; override;
    procedure WriteRunSourceMap(const AFileName: string; const ASource: TStringList);
    { an entry's source lines; none for a bytecode file, whose bytes the run reads (ReadBytecodeFile) }
    function LoadEntrySource(const AFileName: string): TStringList;
    procedure RunFileList(const AFiles: TStringList);
  end;

type
  { One entry file on a thread of its own: an engine's state is its thread's (Goccia.Engine), so each file gets a
    fresh engine there and nothing of the previous one survives. The thread compiles with the JIT. }
  TGocciaFileThread = class(TThread)
  private
    FApp: TGocciaRunnerApp;
    FFileName: string;
    FSource: TStringList;
    FOwnsSource: Boolean;
    { the source is the thread's to load (a parallel JSON file): a failure is that file's report }
    FLoadSource: Boolean;
    FReportIndex: Integer;
    FErrorClass: ExceptClass;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  end;

procedure TGocciaFileThread.Execute;
var
  OwnsJit: Boolean;
  Start: Int64;
begin
  try
    if FLoadSource then
    begin
      Start := GetNanoseconds;
      try
        FSource := FApp.LoadEntrySource(FFileName);
        FOwnsSource := True;
      except
        on E: Exception do
        begin
          if FApp.IsJsonOutput then
            FApp.ReportLoadFailure(FReportIndex, FFileName, E, Start)
          else
          begin
            { GocciaRunner's RunScriptsParallel line for a file its worker could not run }
            WriteLn('Error in ', FFileName, ': ', E.Message);
            ExitCode := 1;
          end;
          Exit;
        end;
      end;
    end;
    OwnsJit := InterlockedCompareExchange(JitOwned, 1, 0) = 0;
    try
      if OwnsJit then
        JitTakeThread;
      try
        FApp.RunSource(FFileName, FSource, FReportIndex);
      finally
        if FOwnsSource then
          FSource.Free;
        if OwnsJit then
          InterLockedExchange(JitOwned, 0);
      end;
    except
      on E: Exception do
      begin
        FErrorClass := ExceptClass(E.ClassType);
        FErrorMessage := E.Message;
      end;
    end;
  finally
    { this thread's engine state: FPC does not finalize managed threadvars }
    RunThreadvarCleanups;
  end;
end;

const
  PACKAGES_GROUP = 'Packages';

{ GocciaRunner's option set (GocciaRunner.dpr TRunnerApp.Configure) }
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

procedure TGocciaRunnerApp.Configure;
begin
  FSandboxRequestedByArguments := ArgumentsRequestSandbox;
  AddEngineOptions;
  AddCoverageOptions;
  AddProfilerOptions;
  FProfileDeterministic := AddFlag('profile-deterministic',
    'Capture repeatable profile counts with zero timing fields');
  AddSandboxOptions;

  FOutputPath := AddString('output',
    '"json" for structured JSON output, "compact-json" omits build, memory, stdout, stderr');
  FSilent := AddFlag('silent', 'Suppress console and goccia:test reporter output from the script');
  FPrint := AddFlag('print',
    'Print the script''s last value to stdout (mirrors node -p / bun --print / deno eval -p)');
  FPreferBytecode := AddFlag('prefer-bytecode',
    'Load imported modules from companion .gbc files when present');
  FSourceMap := TStringOption(Add(TOptionalStringOption.Create('source-map',
    'Write a .map source map file (optional: explicit path)')));
  FSourceMap.WritesHostFile := True;
  FHostEnvironmentModule := AddString('host-environment',
    'Configure script-visible time and randomness from a module with named exports');
  FGlobalFiles := AddRepeatable('globals',
    'Inject globals from a JSON/JSON5/TOML/YAML file or a module with named exports');
  FInlineGlobals := AddRepeatable('global',
    'Inject a single global; value is parsed as JSON or kept as a string');

  FAddPackages := TRepeatableOption(Add(TRepeatableOption.Create('add',
    'Add an import-map entry <key>=github:<owner>/<repo>@<tag|commit>[/<path>] ' +
    'and pin its package in goccia.lock.json', PACKAGES_GROUP)));
  FRemovePackages := TRepeatableOption(Add(TRepeatableOption.Create('remove',
    'Remove the provider import-map entry <key> and prune its pins',
    PACKAGES_GROUP)));
  FInstallPackages := TFlagOption(Add(TFlagOption.Create('install',
    'Pin every provider entry the lockfile lacks and fetch every pinned file',
    PACKAGES_GROUP)));
  FUpdatePackages := TOptionalStringOption(Add(TOptionalStringOption.Create(
    'update', 'Re-resolve the refs of every provider entry, or of the ' +
    'comma-separated keys given', PACKAGES_GROUP)));
  FFrozen := TFlagOption(Add(TFlagOption.Create('frozen',
    'With --install: fail instead of changing goccia.lock.json',
    PACKAGES_GROUP)));
  FCheckRefs := TFlagOption(Add(TFlagOption.Create('check-refs',
    'With --install: check each pin is still its tag''s commit, or an ' +
    'advertised tip', PACKAGES_GROUP)));
  FAcceptMovedTags := TFlagOption(Add(TFlagOption.Create('accept-moved-tags',
    'With --update: re-pin a tag that now names another commit',
    PACKAGES_GROUP)));
  { A config cannot put a run into install mode. }
  FAddPackages.CommandLineOnly := True;
  FRemovePackages.CommandLineOnly := True;
  FInstallPackages.CommandLineOnly := True;
  FUpdatePackages.CommandLineOnly := True;
  FFrozen.CommandLineOnly := True;
  FCheckRefs.CommandLineOnly := True;
  FAcceptMovedTags.CommandLineOnly := True;
end;

{ Host mode grants everything the command line and trusted config ask for. Sandbox mode loads no host files and has
  no node_modules lookup, so net is the only capability it grants (ADR 0122). }
function TGocciaRunnerApp.HonoredCapabilities: TGocciaHonoredCapabilities;
begin
  if FSandboxActive then
    Result := [gcNet]
  else
    Result := ALL_CAPABILITIES;
end;

function TGocciaRunnerApp.HonorsSandboxSection: Boolean;
begin
  Result := True;
end;

function TGocciaRunnerApp.CapabilityPolicyName: string;
begin
  if FSandboxActive then
    Result := Name + ' sandbox mode'
  else
    Result := Name;
end;

{ --entry names the program, and the sandbox options switch to a mode that never reads stdin, so neither leaves the
  run without input. Checked before config is applied, so Present means the command line. }
function TGocciaRunnerApp.HasNonPathInput: Boolean;
begin
  Result := (SandboxOptions.CommandLineActivation <> '') or SandboxOptions.Entry.Present or InstallModeActive;
end;

function TGocciaRunnerApp.InstallModeActive: Boolean;
begin
  Result := FAddPackages.Present or FRemovePackages.Present or FInstallPackages.Present or FUpdatePackages.Present;
end;

function TGocciaRunnerApp.ExclusiveModeName: string;
begin
  if FAddPackages.Present then
    Result := '--add'
  else if FRemovePackages.Present then
    Result := '--remove'
  else if FInstallPackages.Present then
    Result := '--install'
  else if FUpdatePackages.Present then
    Result := '--update'
  else
    Result := '';
end;

{ Install mode runs on its own, like the trust modes: no input files, no sandbox, and one of --add/--remove,
  --install, or --update. Its modifiers belong to one mode each. }
procedure TGocciaRunnerApp.ValidateInstallMode(const APaths: TStringList);
var
  Modes: Integer;
begin
  if FFrozen.Present and not FInstallPackages.Present then
    raise TCLIUsageError.Create('--frozen goes with --install');
  if FCheckRefs.Present and not FInstallPackages.Present then
    raise TCLIUsageError.Create('--check-refs goes with --install');
  if FAcceptMovedTags.Present and not FUpdatePackages.Present then
    raise TCLIUsageError.Create('--accept-moved-tags goes with --update');
  if not InstallModeActive then
    Exit;
  Modes := Ord(FAddPackages.Present or FRemovePackages.Present) + Ord(FInstallPackages.Present) +
    Ord(FUpdatePackages.Present);
  if Modes > 1 then
    raise TCLIUsageError.Create('--add/--remove, --install, and --update cannot be combined; run each on its own');
  if APaths.Count > 0 then
    raise TCLIUsageError.CreateFmt('%s cannot be combined with input files; run it on its own',
      [ExclusiveModeName]);
  if SandboxOptions.CommandLineActivation <> '' then
    raise TCLIUsageError.CreateFmt('%s cannot be combined with sandbox mode (enabled by %s)',
      [ExclusiveModeName, SandboxOptions.CommandLineActivation]);
  if EngineOptions.CachedOnly.Present then
    raise TCLIUsageError.CreateFmt('%s cannot be combined with --cached-only, which is for runs',
      [ExclusiveModeName]);
end;

procedure TGocciaRunnerApp.WriteInstallLine(const ALine: string);
begin
  WriteLn(ALine);
end;

procedure TGocciaRunnerApp.AuditProviderFetch(const AAllowed: Boolean; const ASubject, AReason: string);
begin
  if AAllowed then
    EmitApplicationAudit(gckImportProvider, gcdAllow, ASubject, AReason)
  else
    EmitApplicationAudit(gckImportProvider, gcdDeny, ASubject, AReason);
end;

procedure TGocciaRunnerApp.AuditProviderInstall(const AAllowed: Boolean; const ASubject, AReason: string);
begin
  if AAllowed then
    EmitApplicationAudit(gckImportProviderInstall, gcdAllow, ASubject, AReason)
  else
    EmitApplicationAudit(gckImportProviderInstall, gcdDeny, ASubject, AReason);
end;

{ The import map is --import-map when given, which install mode never edits, else the goccia.json found walking up
  from the working directory (created by --add when there is none). A root config in another format keeps the
  import map from being edited too. The capability set is the command line's plus the trusted requests of the
  config governing the import map, as for a run. }
procedure TGocciaRunnerApp.RunInstallMode;
var
  Capabilities: TGocciaCapabilities;
  Editable: Boolean;
  ImportMapPath, Key, Spec, Error: string;
  Installer: TGocciaPackageInstaller;
  Request: TGocciaInstallRequest;
  Transport: TGocciaHTTPProviderTransport;
  Keys: TStringList;
  I: Integer;
begin
  if EngineOptions.ImportMap.Present then
  begin
    ImportMapPath := ExpandFileName(EngineOptions.ImportMap.Value);
    Editable := False;
  end
  else
  begin
    ImportMapPath := TGocciaModuleResolver.DiscoverProjectConfig(GetCurrentDir);
    { runs read imports only from goccia.json or --import-map, so a root config in another format holds no import
      map install mode could maintain }
    if (RootConfigPath <> '') and ((ImportMapPath = '') or
       not SameFileName(ExpandFileName(RootConfigPath), ImportMapPath)) then
      raise TCLIUsageError.CreateFmt('%s is the root config, but imports are read only from goccia.json or ' +
        '--import-map; put them in a goccia.json, or pass --import-map', [RootConfigPath]);
    if ImportMapPath = '' then
      ImportMapPath := IncludeTrailingPathDelimiter(GetCurrentDir) + PROJECT_IMPORT_MAP_NAME;
    Editable := True;
  end;

  Request := Default(TGocciaInstallRequest);
  for I := 0 to FAddPackages.Values.Count - 1 do
  begin
    if not TryParseAddArgument(FAddPackages.Values[I], Key, Spec, Error) then
      raise TCLIUsageError.Create(Error);
    SetLength(Request.Adds, Length(Request.Adds) + 1);
    Request.Adds[High(Request.Adds)].Key := Key;
    Request.Adds[High(Request.Adds)].Spec := Spec;
  end;
  for I := 0 to FRemovePackages.Values.Count - 1 do
  begin
    SetLength(Request.Removes, Length(Request.Removes) + 1);
    Request.Removes[High(Request.Removes)] := FRemovePackages.Values[I];
  end;
  Request.Install := FInstallPackages.Present;
  Request.Update := FUpdatePackages.Present;
  if FUpdatePackages.ValueOr('') <> '' then
  begin
    Keys := TStringList.Create;
    try
      Keys.StrictDelimiter := True;
      Keys.Delimiter := ',';
      Keys.DelimitedText := FUpdatePackages.Value;
      for I := 0 to Keys.Count - 1 do
        if Trim(Keys[I]) <> '' then
        begin
          SetLength(Request.UpdateKeys, Length(Request.UpdateKeys) + 1);
          Request.UpdateKeys[High(Request.UpdateKeys)] := Trim(Keys[I]);
        end;
    finally
      Keys.Free;
    end;
  end;
  Request.Frozen := FFrozen.Present;
  Request.CheckRefs := FCheckRefs.Present;
  Request.AcceptMovedTags := FAcceptMovedTags.Present;

  ValidateFileConfig(ImportMapPath);
  Capabilities := ResolveEngineCapabilities(DiscoverFileConfigPath(ImportMapPath), ImportMapPath);

  Transport := TGocciaHTTPProviderTransport.Create;
  try
    Installer := TGocciaPackageInstaller.Create(ImportMapPath, Editable, Capabilities, Transport);
    try
      Installer.OnLog := WriteInstallLine;
      Installer.OnAudit := AuditProviderFetch;
      Installer.OnInstallAudit := AuditProviderInstall;
      try
        Installer.Run(Request);
      except
        on E: EGocciaInstallError do
          if E.ExitCode = INSTALL_EXIT_USAGE then
            raise TCLIUsageError.Create(E.Message)
          else
            raise EGocciaInstallModeFailure.Create(E.Message);
      end;
    finally
      Installer.Free;
    end;
  finally
    Transport.Free;
  end;
end;

function TGocciaRunnerApp.ExtraHelpText: string;
begin
  Result := SandboxModeHelpNote;
end;

{ The --allow-* flags of capabilities sandbox mode cannot grant. They are command-line-only, so Present means the
  command line. }
function TGocciaRunnerApp.HostCapabilityAllowFlags: TGocciaCapabilityScopes;
var
  Capability: TGocciaCapability;
begin
  Result := nil;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if (Capability <> gcNet) and EngineOptions.Capabilities.AllowOption(Capability).Present then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := '--' + EngineOptions.Capabilities.AllowOption(Capability).LongName;
    end;
end;

{ The host-mode options given on the command line. The same keys from a config are ignored in sandbox mode, so one
  project config serves both modes. ABeforeConfig: no config has been applied yet, so Present means the command
  line. }
function TGocciaRunnerApp.HostOnlyCommandLineOptions(const ABeforeConfig: Boolean): TGocciaCapabilityScopes;

  procedure Check(const AOption: TOptionBase);
  begin
    if Assigned(AOption) and (AOption.FromCommandLine or (ABeforeConfig and AOption.Present)) then
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
  Check(FProfileDeterministic);
  Check(FSourceMap);
  Check(FHostEnvironmentModule);
  Check(FGlobalFiles);
  Check(FInlineGlobals);
end;

{ Sandbox mode switched on from the command line is known before any config is read, so the trust gate and the
  capability checks see the right set, and a host-filesystem grant gets an error that says why. }
procedure TGocciaRunnerApp.ValidateCommandLine(const APaths: TStringList);
begin
  inherited ValidateCommandLine(APaths);
  ValidateInstallMode(APaths);
  if SandboxOptions.CommandLineActivation = '' then
    Exit;
  RejectHostCapabilityFlags(HostCapabilityAllowFlags, SandboxOptions.CommandLineActivation);
  { before Validate, whose host-mode checks would otherwise report the wrong problem }
  RejectHostModeOptions(HostOnlyCommandLineOptions(True), SandboxOptions.CommandLineActivation);
  FSandboxActive := True;
end;

function TGocciaRunnerApp.ConfigSandboxReason(const AVerdict: TGocciaConfigTrustVerdict): string;
begin
  Result := Format('the "%s" section of %s', [SANDBOX_CONFIG_KEY, AVerdict.Request.Sandbox.SourcePath]);
end;

{ A root-config sandbox section switches sandbox mode on too; decided here so a host-mode option gets the
  sandbox-mode error rather than a host-mode check's. Its trust is checked in ExecuteWithPaths, once the audit log
  is open. --ignore-config-permissions ignores the section. }
procedure TGocciaRunnerApp.Validate;
var
  Verdict: TGocciaConfigTrustVerdict;
begin
  if not FSandboxActive and not InstallModeActive then
  begin
    Verdict := RootConfigVerdict;
    if Verdict.Request.Sandbox.Declared and (Verdict.State <> ctsIgnored) then
    begin
      FSandboxActive := True;
      RejectHostCapabilityFlags(HostCapabilityAllowFlags, ConfigSandboxReason(Verdict));
      RejectHostModeOptions(HostOnlyCommandLineOptions(False), ConfigSandboxReason(Verdict));
    end;
  end;
  inherited Validate;
  { the profiler's implications are host mode's: those options are refused on the command line, and ignored from
    config, in sandbox mode }
  if FSandboxActive then
    Exit;
  { --profile-format implies --profile=functions, --profile-deterministic --profile=all }
  if ProfilerOptions.Format.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('functions');
  if FProfileDeterministic.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('all');
  if ProfilerOptions.OutputPath.Present and not ProfilerOptions.Mode.Present then
    raise TParseError.Create('--profile-output requires --profile=opcodes|functions|all|compile.');
  if ProfilerOptions.Format.Matches(pfFlamegraph) and not ProfilerOptions.OutputPath.Present then
    raise TParseError.Create('--profile-format=flamegraph requires --profile-output=<path>.');
end;

{ one reporting thread accumulates every profiled file, as in GocciaRunner }
function TGocciaRunnerApp.JobCount(const AFileCount: Integer): Integer;
begin
  if ProfilerOptions.Mode.Present then
    Result := 1
  else
    Result := GetJobCount(AFileCount);
end;

{ In sandbox mode stdout carries only the guest's output and the diff; the guest's own failures are reported by
  RunSandbox, so what reaches here is the host's (a copy that failed, an invalid value) and goes to stderr. }
procedure TGocciaRunnerApp.HandleError(const AException: Exception);
begin
  if AException is EGocciaInstallModeFailure then
  begin
    WriteLn(ErrOutput, 'Error: ', AException.Message);
    Exit;
  end;
  if FSandboxActive or FSandboxRequestedByArguments or ConfigRequestsSandbox then
    WriteLn(ErrOutput, 'Error: ', AException.Message)
  else if IsJsonOutput then
    WriteLn(BuildCLIScriptErrorJSON('', '', '', '', ExceptionToCLIJSONErrorInfo(AException),
      Default(TCLIJSONTiming), DefaultCLIJSONMemoryStats, 1, 1, IsCompactJsonOutput))
  else
    inherited HandleError(AException);
end;

function TGocciaRunnerApp.StdinUsage: TGocciaStdinUsage;
begin
  Result := suStdinDefaultWithREPL;
end;

function TGocciaRunnerApp.UsageLine: string;
begin
  Result := '[file|directory|-] [options]' + sLineBreak +
    '       ' + Name + ' <file> [--copy <host>[=<sandbox>]]... [options]';
end;




{ A config's `output` is ignored in sandbox mode, whose stdout carries the guest's output and the diff. }
function TGocciaRunnerApp.IsJsonOutput: Boolean;
begin
  Result := (not FSandboxActive) and FOutputPath.Present and
    ((FOutputPath.Value = 'json') or (FOutputPath.Value = 'compact-json'));
end;

function TGocciaRunnerApp.IsCompactJsonOutput: Boolean;
begin
  Result := (not FSandboxActive) and FOutputPath.Present and (FOutputPath.Value = 'compact-json');
end;

{ The run's error as the JSON report's error object, as GocciaRunner's ExceptionToCLIJSONErrorInfo makes it }
function ErrorInfoOf(const AFileName: string;
  const AResult: TGocciaRunResult; const AEngineOptions: TGocciaEngineOptions): TCLIJSONErrorInfo;
begin
  Result := DefaultCLIJSONErrorInfo;
  Result.FileName := AFileName;
  case AResult.Aborted of
    gaTimeout:
      begin
        Result.ErrorType := 'TimeoutError';
        Result.Message := Format('file timed out after %dms', [AEngineOptions.Timeout.Value]);
        Exit;
      end;
    gaInstructionLimit:
      begin
        Result.ErrorType := 'InstructionLimitError';
        Result.Message := Format('Execution exceeded instruction limit of %d',
          [AEngineOptions.MaxInstructions.Value]);
        Exit;
      end;
    gaMemoryLimit:
      begin
        { GocciaRunner's name for the refusal no script can catch (TGocciaMemoryLimitError) }
        Result.ErrorType := 'MemoryLimitError';
        Result.Message := UTF8Encode(AResult.ErrorText);
        Exit;
      end;
    gaHostAbort:
      begin
        { the only host abort: the audit log could not be written (GocciaRunner's delivery error) }
        Result.ErrorType := 'EGocciaCapabilityAuditDeliveryError';
        Result.Message := UTF8Encode(AResult.ErrorText);
        Exit;
      end;
  end;
  if not AResult.ErrorIsError then
  begin
    { the old VM's text for a thrown value that is not an error (EGocciaBytecodeThrow) }
    Result.ErrorType := 'Error';
    Result.Message := 'Goccia VM throw';
    Exit;
  end;
  Result.ErrorType := UTF8Encode(AResult.ErrorName);
  if Result.ErrorType = '' then
    Result.ErrorType := 'Error';
  Result.Message := UTF8Encode(AResult.ErrorText);
  if AResult.ErrorFile <> '' then
    Result.FileName := UTF8Encode(AResult.ErrorFile);
  Result.Line := AResult.ErrorLine;
  Result.Column := AResult.ErrorColumn;
end;

procedure AddTiming(var ATotal: TCLIJSONTiming; const ATiming: TCLIJSONTiming);
begin
  Inc(ATotal.LexTimeNanoseconds, ATiming.LexTimeNanoseconds);
  Inc(ATotal.ParseTimeNanoseconds, ATiming.ParseTimeNanoseconds);
  Inc(ATotal.CompileTimeNanoseconds, ATiming.CompileTimeNanoseconds);
  Inc(ATotal.ExecuteTimeNanoseconds, ATiming.ExecuteTimeNanoseconds);
  Inc(ATotal.TotalTimeNanoseconds, ATiming.TotalTimeNanoseconds);
end;

{ one engine's memory.gc, from its heap when made (AStart) and after its run (AEnd), with its thread's memory
  manager over the same span (memory.heap); the engine does not count objects, so their counts stay 0 }
function AggregateMemoryStats(const AReports: array of TGocciaFileReport; AParallel: Boolean;
  const AHeapStart, AHeapEnd: THeapStatus; AWorkers: Integer): TCLIJSONMemoryStats;
var
  Stats: array of TCLIJSONMemoryStats;
  I: Integer;
begin
  SetLength(Stats, Length(AReports));
  for I := 0 to High(AReports) do
    Stats[I] := AReports[I].Memory;
  Result := Goccia.CLI.FileEngine.AggregateMemoryStats(Stats, AParallel, AHeapStart, AHeapEnd, AWorkers);
end;

{ the files' report: one file's own form, or the aggregate of GocciaRunner's BuildAggregateScriptLoaderJSON. As
  GocciaRunner's: one file reports its memory at the top level; files at once report each file's and their sum,
  files one after another (one worker) only the sum }
procedure TGocciaRunnerApp.PrintJsonReport;
var
  I: Integer;
  Ok: Boolean;
  StdoutText, StderrText, OutputText, FilesJSON, ErrorJSON, FileJSON: string;
  Timing: TCLIJSONTiming;
  FileMemory: TCLIJSONMemoryStats;
begin
  if Length(FReports) = 1 then
  begin
    { the file's memory at the top level only }
    with FReports[0] do
      if Ok then
        WriteLn(BuildCLIReportJSON(True, OutputText, StdoutText, StderrText, 'null', Timing, Memory,
          FWorkers, FWorkers, BuildCLIScriptFileSuccessJSON(FileName, ResultJSON, OutputText, StdoutText,
          StderrText, Timing, DefaultCLIJSONMemoryStats, IsCompactJsonOutput), '', IsCompactJsonOutput))
      else
        WriteLn(BuildCLIReportJSON(False, OutputText, StdoutText, StderrText,
          BuildCLIErrorObjectJSON(ErrorInfo), Timing, Memory, FWorkers, FWorkers,
          BuildCLIScriptFileErrorJSON(FileName, OutputText, StdoutText, StderrText, ErrorInfo, Timing,
          DefaultCLIJSONMemoryStats, IsCompactJsonOutput), '', IsCompactJsonOutput));
    Exit;
  end;
  FillChar(Timing, SizeOf(Timing), 0);
  Ok := True;
  StdoutText := '';
  StderrText := '';
  OutputText := '';
  ErrorJSON := 'null';
  FilesJSON := '';
  for I := 0 to High(FReports) do
  begin
    Ok := Ok and FReports[I].Ok;
    if FWorkers > 1 then
      FileMemory := FReports[I].Memory
    else
      FileMemory := DefaultCLIJSONMemoryStats;
    if FReports[I].Ok then
      FileJSON := BuildCLIScriptFileSuccessJSON(FReports[I].FileName,
        FReports[I].ResultJSON, FReports[I].OutputText,
        FReports[I].StdoutText, FReports[I].StderrText, FReports[I].Timing,
        FileMemory, IsCompactJsonOutput)
    else
    begin
      FileJSON := BuildCLIScriptFileErrorJSON(FReports[I].FileName,
        FReports[I].OutputText, FReports[I].StdoutText,
        FReports[I].StderrText, FReports[I].ErrorInfo, FReports[I].Timing,
        FileMemory, IsCompactJsonOutput);
      if ErrorJSON = 'null' then
        ErrorJSON := BuildCLIErrorObjectJSON(FReports[I].ErrorInfo);
    end;
    StdoutText := StdoutText + FReports[I].StdoutText;
    StderrText := StderrText + FReports[I].StderrText;
    OutputText := OutputText + FReports[I].OutputText;
    AddTiming(Timing, FReports[I].Timing);
    if FilesJSON <> '' then
      FilesJSON := FilesJSON + ',';
    FilesJSON := FilesJSON + FileJSON;
  end;
  WriteLn(BuildCLIReportJSON(Ok, OutputText, StdoutText, StderrText,
    ErrorJSON, Timing, AggregateMemoryStats(FReports, FWorkers > 1, FHeapStart, GetHeapStatus, FWorkers),
    FWorkers, FWorkers, FilesJSON, '',
    IsCompactJsonOutput));
end;

function TGocciaRunnerApp.InjectGlobals(const AEngine: TGocciaEngine; out AError: string): Boolean;
begin
  Result := Goccia.CLI.FileEngine.InjectGlobals(AEngine, ConfigNamedEntries(FGlobalFiles), FGlobalFiles,
    FInlineGlobals, AError);
end;

function TGocciaRunnerApp.DefineVirtualModules(const AEngine: TGocciaEngine; const AFileConfigPath: string;
  out AError: string): Boolean;
begin
  Result := Goccia.CLI.FileEngine.DefineVirtualModules(AEngine, RootConfigPath, AFileConfigPath, EngineOptions,
    AError);
end;

function TGocciaRunnerApp.DefineCommandLineVirtualModules(const AEngine: TGocciaEngine; out AError: string): Boolean;
begin
  Result := Goccia.CLI.FileEngine.DefineCommandLineVirtualModules(AEngine, EngineOptions, AError);
end;

function TGocciaRunnerApp.FailureText(const AFileName: string; const AResult: TGocciaRunResult; const ASource: TStringList;
  AEngine: TGocciaEngine; const AWithheld: string; const AFrameSource: TSandboxVirtualFileSystem;
  AFrames, ASuggestions, AColor: Boolean): string;
begin
  Result := Goccia.CLI.FileEngine.FailureText(AFileName, AResult, ASource, AEngine, AWithheld, AFrameSource, AFrames,
    ASuggestions, AColor, EngineOptions.Timeout.ValueOr(0), EngineOptions.MaxInstructions.ValueOr(0));
end;

procedure TGocciaRunnerApp.ReportFailure(const AFileName: string;
  const AResult: TGocciaRunResult; const ASource: TStringList; AEngine: TGocciaEngine; const AWithheld: string);
begin
  WriteLn(FailureText(AFileName, AResult, ASource, AEngine, AWithheld, nil, True, True, IsColorTerminal));
end;

{ --host-environment's module, else a config's "host-environment" (against that config's directory): the path,
  or '' for none. The command line's names a file from the working directory. }
{ a bytecode file's bytes, at most the format's limit (Goccia.Artifact checks them) }
function ReadBytecodeFile(const AFileName: string): RawByteString;
var
  Stream: TFileStream;
begin
  Result := '';
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > GOCCIA_GBC_MAX_FILE_BYTES then
      raise Exception.CreateFmt('Bytecode file %s exceeds the %d byte limit', [AFileName, GOCCIA_GBC_MAX_FILE_BYTES]);
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

{ --source-map: the JSX transform's map of the file (GocciaRunner's WriteSourceMapIfEnabled), beside it as
  <file>.map or at the path given; nothing for a file the transform leaves alone }
procedure TGocciaRunnerApp.WriteRunSourceMap(const AFileName: string; const ASource: TStringList);
var
  Transformed: TGocciaJSXTransformResult;
  MapPath, Text: string;
begin
  if IsJSXExcludedExtension(ExtractFileExt(AFileName)) then
    Exit;
  { the text as the source pipeline joins it, and the original as the map's source content, as GocciaScript's }
  Text := StringListToSourceText(ASource);
  Transformed := TGocciaJSXTransformer.Transform(Text, AFileName);
  try
    if Assigned(Transformed.SourceMap) then
      Transformed.SourceMap.SetSourceContent(0, Text);
    MapPath := FSourceMap.ValueOr('');
    if MapPath = '' then
      MapPath := AFileName + EXT_MAP;
    WriteSourceMapIfAvailable(Transformed.SourceMap, MapPath, AFileName, AFileName, not IsJsonOutput);
  finally
    Transformed.SourceMap.Free;
    Transformed.OriginMap.Free;
  end;
end;

function TGocciaRunnerApp.HostEnvironmentPath(const AFileConfig: TConfigEntryArray;
  ADeterministic: Boolean): string;
var
  Entry: TConfigEntry;
  Entries: TConfigEntryArray;
  Written: string;
begin
  Result := '';
  Written := '';
  Entry := Default(TConfigEntry);
  if FHostEnvironmentModule.FromCommandLine then
  begin
    Written := FHostEnvironmentModule.Value;
    Result := ExpandFileName(Written);
  end
  else
  begin
    if not TryFindConfigEntry(AFileConfig, 'host-environment', Entry) then
    begin
      Entries := ConfigNamedEntries(FHostEnvironmentModule);
      if Length(Entries) > 0 then
        Entry := Entries[0];
    end;
    if Entry.Value <> '' then
    begin
      Written := Entry.Value;
      Result := ConfigInputPath(Entry.Value, Entry.SourcePath);
    end;
  end;
  if Result = '' then
    Exit;
  if FProfileDeterministic.Present then
    raise TParseError.Create('--host-environment cannot be combined with --profile-deterministic.');
  if ADeterministic then
    raise TParseError.Create('--host-environment cannot be combined with --deterministic.');
  if not FileExists(Result) then
    raise EFOpenError.Create('Module not found: "' + Written + '"');
end;

procedure TGocciaRunnerApp.RunSource(const AFileName: string;
  const ASource: TStringList; AReportIndex: Integer);
var
  FileConfigPath: string;
  FileConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Start: Int64;
  Capture: TGocciaConsoleCapture;
  Report: TGocciaFileReport;
  Resolver: TGocciaModuleResolver;
  Grant: TGocciaImportGrant;
  AliasBaseDirectory: string;
  GlobalsError: string;
  HeapStart: THeapStatus;
  MemoryStart: TGocciaMemoryStats;
  Capabilities: TGocciaCapabilities;
  Effective: TGocciaCapabilityAuditEvent;
  HostError: TGocciaRunResult;
begin
  Start := GetNanoseconds;
  { the console capture, which also prints the parse warnings (not in a JSON report, as GocciaRunner) }
  Capture := TGocciaConsoleCapture.Create;
  Capture.FileName := AFileName;
  try
    FileConfigPath := DiscoverFileConfigPath(AFileName);
    if FileConfigPath <> '' then
      FileConfig := LoadFileConfig(FileConfigPath)
    else
      SetLength(FileConfig, 0);
    Verdict := FileConfigVerdict(FileConfigPath, AFileName);
    { the file's import resolution, as GocciaRunner's CreateEngine configures it }
    Capabilities := ResolveEngineCapabilities(FileConfigPath, AFileName);
    Grant := TGocciaImportGrant.Create(Capabilities);
    Resolver := TGocciaModuleResolver.Create(ExtractFilePath(ExpandFileName(AFileName)));
    Grant.Resolver := Resolver;
    Resolver.SetExtensions(ModuleImportExtensions);
    Resolver.NodeModulesGrant := Grant.Grant;
    Resolver.ProviderGrant := Grant.GrantProvider;
    Resolver.ProviderAudit := Grant.AuditProvider;
    Resolver.ProviderCachedOnly := ResolveFlagOption(EngineOptions.CachedOnly, FileConfig);
    { --prefer-bytecode (or the config key): an import of a source file takes its .gbc companion when there is
      one (docs/bytecode-loading.md) }
    Resolver.PreferBytecodeCompanions := ResolveFlagOption(FPreferBytecode, FileConfig);
    if EngineOptions.Aliases.FromCommandLine or (RootConfigPath = '') then
      AliasBaseDirectory := GetCurrentDir
    else
      AliasBaseDirectory := ExtractFilePath(RootConfigPath);
    ConfigureModuleResolver(Resolver, AFileName, EngineOptions.ImportMap.ValueOr(''),
      EngineOptions.Aliases.Values, AliasBaseDirectory);
    HeapStart := GetHeapStatus;
    { --coverage: this thread's tracker, the file registered with its executable lines in the run's }
    if Assigned(FCoverageMain) then
    begin
      TGocciaCoverageTracker.Initialize;
      TGocciaCoverageTracker.Instance.Enabled := True;
      EnterCriticalSection(FCoverageLock);
      try
        FCoverageMain.RegisterSourceFile(AFileName, CountExecutableLines(ASource));
      finally
        LeaveCriticalSection(FCoverageLock);
      end;
    end;
    Engine := TGocciaEngine.Create;
    try
      MemoryStart := Engine.Memory;
      Engine.Capabilities := Capabilities;
      Engine.ProjectRoot := DiscoverProjectRoot(AFileName);
      Engine.ConfigJson := HostConfigJson(EngineOptions, FileConfig);
      Engine.Coverage := Assigned(FCoverageMain);
      { --profile: this thread records into the main thread's profilers }
      if Assigned(FProfiler) then
      begin
        TGocciaProfiler.Adopt(FProfiler);
        TGocciaCompileProfiler.Adopt(FCompileProfiler);
        FProfiler.BeginEntry;
        Engine.ProfileOpcodes := Goccia.Profiler.pmOpcodes in FProfiler.Mode;
        Engine.ProfileFunctions := Goccia.Profiler.pmFunctions in FProfiler.Mode;
      end;
      Engine.Resolver := Resolver;
      Engine.ModuleContentCheck := Grant.CheckContent;
      Engine.ProviderGraph := Grant.ProviderGraph;
      { GocciaRunner has no timer globals (the test runner does) }
      Engine.ExposeTimers := False;
      Capture.Capturing := IsJsonOutput;
      Capture.Silent := FSilent.Present;
      { an imported goccia:test's reporter (its per-test lines and runTests' results block): muted in a JSON run and
        by --silent }
      TestReporterOn := not IsJsonOutput and not FSilent.Present;
      if LogFileOpen then
        Capture.LogLine := HandleConsoleLog;
      if IsJsonOutput then
        Engine.ResultForm := grfJSON
      else if FPrint.Present and not FQuietWorkers then
        Engine.ResultForm := grfString;
      if Capture.Capturing or Capture.Silent or LogFileOpen then
        Engine.OnConsole := Capture.Emit;
      Engine.FailOnUnhandledRejection := True;
      Engine.Deterministic := ResolveFlagOption(EngineOptions.Deterministic, FileConfig) or
        FProfileDeterministic.Present;
      { a file's set-up the options refuse fails that file, as GocciaRunner's per-file handler reports it }
      try
        { the audit log opens with the engine's capability set, before anything of the file runs }
        if AuditLogOpen then
        begin
          Engine.AuditSink := HandleCapabilityAudit;
          Grant.Engine := Engine;
          Effective := Default(TGocciaCapabilityAuditEvent);
          Effective.Kind := gckCapabilitiesEffective;
          Effective.Decision := gcdAllow;
          Effective.Subject := Capabilities.ToJSON;
          Effective.Reason := CapabilityProvenance(Verdict);
          Effective.Source.FilePath := AFileName;
          try
            HandleCapabilityAudit(Effective);
          except
            on E: Exception do
              raise EGocciaCapabilityAuditDeliveryError.Create('capability audit delivery failed: ' + E.Message);
          end;
        end;
        Engine.HostEnvironment := UnicodeString(HostEnvironmentPath(FileConfig, Engine.Deterministic));
        { a config's host-environment module is read as the script's }
        Engine.HostEnvironmentGuest := not FHostEnvironmentModule.FromCommandLine;
      except
        on E: TCLIUsageError do
          raise;
        on E: Exception do
        begin
          if IsJsonOutput then
            ReportLoadFailure(AReportIndex, AFileName, E, Start)
          else
          begin
            WriteLn('Fatal error: ', E.Message);
            ExitCode := 1;
          end;
          Exit;
        end;
      end;
      { parse warnings print in a text run on the main thread, as GocciaRunner's (#2038): never ahead of a JSON
        envelope, and never from a parallel worker, whose lines would interleave }
      if not IsJsonOutput and not FQuietWorkers then
      begin
        Engine.OnWarning := Capture.PrintWarning;
        Engine.OnModuleWarning := Capture.PrintModuleWarning;
      end;
      ApplySettings(Engine, ResolveFileSettings(EngineOptions, FileConfig,
        AFileName, Verdict.AcceptedUnsafe));
      if EngineOptions.Timeout.Present then
        Engine.TimeoutMs := EngineOptions.Timeout.Value;
      if EngineOptions.MaxInstructions.Present then
        Engine.MaxInstructions := EngineOptions.MaxInstructions.Value;
      { The engine's own frame bound unless one is given: a bound keeps compiled code
        out (Goccia.VM SetMaxFrames) }
      if EngineOptions.MaxStack.Present then
        Engine.MaxFrames := Integer(EngineOptions.MaxStack.Value);
      { a refused or unreadable global fails the file before it runs, as the old engine's RuntimeError }
      { a module definition that cannot apply is a configuration error, as GocciaRunner's }
      if not DefineVirtualModules(Engine, FileConfigPath, GlobalsError) then
      begin
        { an error a manifest threw, by its name (a refused read's PermissionDenied) }
        if Engine.DescribeHostError(HostError) and HostError.ErrorIsError and (HostError.ErrorName <> '') then
          WriteLn('Fatal error: ', UTF8Encode(HostError.ErrorName), ': ', UTF8Encode(HostError.ErrorText))
        else
          WriteLn('Fatal error: ', GlobalsError);
        ExitCode := 1;
        Exit;
      end;
      if not InjectGlobals(Engine, GlobalsError) then
      begin
        { a globals module that threw is reported as the run's error would be, located where it threw }
        if Engine.DescribeHostError(HostError) and HostError.ErrorIsError and (HostError.ErrorLine > 0) then
          WriteLn(FailureText(UTF8Encode(HostError.ErrorFile), HostError, nil, Engine, '', nil, True, True,
            IsColorTerminal))
        { an error without a location of its own (a refused import's PermissionDenied) keeps its name }
        else if HostError.ErrorIsError and (HostError.ErrorName <> '') then
          WriteLn(FormatErrorWithSourceContext(UTF8Encode(HostError.ErrorName), UTF8Encode(HostError.ErrorText),
            AFileName, 0, 0, ASource, IsColorTerminal, ''))
        else
          WriteLn(FormatErrorWithSourceContext('RuntimeError', GlobalsError, AFileName, 0, 0,
            ASource, IsColorTerminal, ''));
        ExitCode := 1;
        Exit;
      end;
      if FSourceMap.Present then
        WriteRunSourceMap(AFileName, ASource);
      { a bytecode file (GocciaBundler's output) runs from its bytes }
      if SameText(ExtractFileExt(AFileName), EXT_GBC) then
        RunResult := Engine.RunArtifact(AFileName, ReadBytecodeFile(AFileName))
      else
        RunResult := Engine.Run(AFileName, UTF8Decode(ASource.Text));
      if IsJsonOutput then
      begin
        Report := Default(TGocciaFileReport);
        Report.FileName := AFileName;
        Report.Ok := RunResult.Ok;
        Report.OutputText := Capture.OutputText;
        Report.StdoutText := Capture.StdoutText;
        Report.StderrText := Capture.StderrText;
        Report.Timing.TotalTimeNanoseconds := GetNanoseconds - Start;
        Report.Memory := FileMemoryStats(MemoryStart, Engine.Memory, HeapStart, GetHeapStatus);
        if RunResult.Ok then
        begin
          Report.ResultJSON := UTF8Encode(RunResult.ResultText);
          Report.Timing.CompileTimeNanoseconds := RunResult.CompileNanoseconds;
          Report.Timing.ExecuteTimeNanoseconds := RunResult.ExecuteNanoseconds;
        end
        else
        begin
          { as GocciaRunner's: a failed run's time is all execution }
          Report.Timing.ExecuteTimeNanoseconds := Report.Timing.TotalTimeNanoseconds;
          Report.ErrorInfo := ErrorInfoOf(AFileName, RunResult, EngineOptions);
          ExitCode := 1;
        end;
        { a parallel run's slot was made for it; one after another, the reports are appended }
        if AReportIndex >= 0 then
          FReports[AReportIndex] := Report
        else
        begin
          SetLength(FReports, Length(FReports) + 1);
          FReports[High(FReports)] := Report;
        end;
        Exit;
      end;
      if not RunResult.Ok then
      begin
        ReportFailure(AFileName, RunResult, ASource, Engine, UTF8Encode(Engine.HostEnvironment));
        ExitCode := 1;
        Exit;
      end;
      { a parallel worker prints no banner and no value, so it does not convert one either }
      if FQuietWorkers then
        Exit;
      WriteLn('Running script (bytecode): ', AFileName);
      WriteLn('  Lex: (in Compile) | Parse: (in Compile) | Compile: ',
        FormatDuration(RunResult.CompileNanoseconds), ' | Execute: ',
        FormatDuration(RunResult.ExecuteNanoseconds), ' | Total: ',
        FormatDuration(GetNanoseconds - Start));
      { --print: the bare value, after the banner (node -p) }
      if RunResult.HasResult then
        WriteLn(UTF8Encode(RunResult.ResultText));
    finally
      Engine.Free;
      Resolver.Free;
      Grant.Free;
      { a file's thread gives the main thread's profilers back (they are not its to free) }
      if Assigned(FProfiler) and (GetCurrentThreadId <> MainThreadID) then
      begin
        TGocciaProfiler.Adopt(nil);
        TGocciaCompileProfiler.Adopt(nil);
      end;
      { the file's coverage into the run's }
      if Assigned(FCoverageMain) and (TGocciaCoverageTracker.Instance <> FCoverageMain) then
      begin
        EnterCriticalSection(FCoverageLock);
        try
          FCoverageMain.MergeFrom(TGocciaCoverageTracker.Instance);
        finally
          LeaveCriticalSection(FCoverageLock);
        end;
        TGocciaCoverageTracker.Shutdown;
      end;
    end;
  finally
    Capture.Free;
  end;
end;

procedure TGocciaRunnerApp.RunOnWorker(const AFileName: string;
  const ASource: TStringList);
var
  Worker: TGocciaFileThread;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
begin
  Worker := TGocciaFileThread.Create(True, FILE_THREAD_STACK);
  try
    Worker.FApp := Self;
    Worker.FFileName := AFileName;
    Worker.FSource := ASource;
    Worker.FReportIndex := -1;
    Worker.Start;
    Worker.WaitFor;
    ErrorClass := Worker.FErrorClass;
    ErrorMessage := Worker.FErrorMessage;
  finally
    Worker.Free;
  end;
  { the file's error as the main thread's (a usage error ends the run with its exit code) }
  if ErrorClass <> nil then
    raise ErrorClass.Create(ErrorMessage);
end;

function TGocciaRunnerApp.LoadEntrySource(const AFileName: string): TStringList;
begin
  if SameText(ExtractFileExt(AFileName), EXT_GBC) then
  begin
    if not FileExists(AFileName) then
      raise Exception.Create('File not found: ' + AFileName);
    Result := TStringList.Create;
  end
  else
    Result := SourceRegistry.Load(AFileName);
end;

procedure TGocciaRunnerApp.RunFile(const AFileName: string);
var
  Source: TStringList;
begin
  Source := LoadEntrySource(AFileName);
  try
    RunOnWorker(AFileName, Source);
  finally
    Source.Free;
  end;
end;

{ a JSON report's files run at once, as GocciaRunner's RunJSONFiles does (GetJobCount: --jobs, else the processors,
  at most one per file): each on a thread of its own, at most that many running, their reports in file order }
procedure TGocciaRunnerApp.RunFilesParallel(const AFiles: TStringList);
var
  I, Next, Oldest, Started: Integer;
  Workers: array of TGocciaFileThread;
  Running: Integer;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
begin
  FWorkers := JobCount(AFiles.Count);
  if IsJsonOutput then
    SetLength(FReports, AFiles.Count);
  SetLength(Workers, AFiles.Count);
  ErrorClass := nil;
  ErrorMessage := '';
  Running := 0;
  Oldest := 0;
  Started := 0;
  try
    for Next := 0 to AFiles.Count - 1 do
    begin
      if Running = FWorkers then
      begin
        Workers[Oldest].WaitFor;
        Dec(Running);
        Inc(Oldest);
      end;
      Workers[Next] := TGocciaFileThread.Create(True, FILE_THREAD_STACK);
      Workers[Next].FApp := Self;
      Workers[Next].FFileName := AFiles[Next];
      Workers[Next].FReportIndex := Next;
      Workers[Next].FLoadSource := True;
      Workers[Next].Start;
      Inc(Started);
      Inc(Running);
    end;
  finally
    { every started thread is joined once: the window joined the first Oldest already (a second join is undefined) }
    for I := 0 to Started - 1 do
    begin
      if I >= Oldest then
        Workers[I].WaitFor;
      if (ErrorClass = nil) and (Workers[I].FErrorClass <> nil) then
      begin
        ErrorClass := Workers[I].FErrorClass;
        ErrorMessage := Workers[I].FErrorMessage;
      end;
      Workers[I].Free;
    end;
  end;
  if ErrorClass <> nil then
    raise ErrorClass.Create(ErrorMessage);
end;

procedure TGocciaRunnerApp.ReportLoadFailure(AReportIndex: Integer; const AFileName: string;
  const E: Exception; AStart: Int64);
var
  Report: TGocciaFileReport;
begin
  Report := Default(TGocciaFileReport);
  Report.FileName := AFileName;
  Report.Ok := False;
  Report.ErrorInfo := DefaultCLIJSONErrorInfo;
  Report.ErrorInfo.ErrorType := E.ClassName;
  Report.ErrorInfo.Message := E.Message;
  Report.ErrorInfo.FileName := AFileName;
  Report.Timing.TotalTimeNanoseconds := GetNanoseconds - AStart;
  Report.Timing.ExecuteTimeNanoseconds := Report.Timing.TotalTimeNanoseconds;
  { one after another, the reports are appended }
  if AReportIndex >= 0 then
    FReports[AReportIndex] := Report
  else
  begin
    SetLength(FReports, Length(FReports) + 1);
    FReports[High(FReports)] := Report;
  end;
  ExitCode := 1;
end;

{ a list of files or --multifile sections in text output, as GocciaRunner's RunScripts runs a directory: at once
  when more than one worker is allowed (RunScriptsParallel), else one after another with a blank line between
  them (RunScriptsSequentially) }
procedure TGocciaRunnerApp.RunFileList(const AFiles: TStringList);
var
  J: Integer;
begin
  if JobCount(AFiles.Count) > 1 then
  begin
    WriteLn(Format('Running %d files with %d workers', [AFiles.Count, JobCount(AFiles.Count)]));
    { before the workers', whose output goes through their own threads' buffers }
    Flush(Output);
    FQuietWorkers := True;
    try
      RunFilesParallel(AFiles);
    finally
      FQuietWorkers := False;
      FWorkers := 1;
    end;
  end
  else
    for J := 0 to AFiles.Count - 1 do
    begin
      if J > 0 then
        WriteLn;
      RunFile(AFiles[J]);
    end;
end;

procedure TGocciaRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I: Integer;
  Source, Files, Found, Names, Single: TStringList;
  Verdict: TGocciaConfigTrustVerdict;
  SandboxCommandLine: TGocciaSandboxCommandLine;
  SandboxRequest: TGocciaSandboxModeRequest;
begin
  { A trusted sandbox section in the root config switches sandbox mode on too; an untrusted one stops the run here,
    before anything is read from it. The mode is set before the config's requests are checked, so the ones sandbox
    mode cannot grant are warned about rather than needing trust. --ignore-config-permissions ignores the
    section. }
  if InstallModeActive then
  begin
    RunInstallMode;
    Exit;
  end;
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
  SandboxRequest := ResolveSandboxMode(SandboxOptions, Verdict.Request.Sandbox, Verdict.GrantsAccepted,
    SandboxCommandLine);
  if SandboxRequest.Active then
  begin
    FSandboxActive := True;
    RunSandbox(SandboxRequest);
    Exit;
  end;
  FSandboxActive := False;

  FProfiler := TGocciaProfiler.Instance;
  FCompileProfiler := TGocciaCompileProfiler.Instance;
  if FProfileDeterministic.Present then
  begin
    if Assigned(FProfiler) then
      FProfiler.Deterministic := True;
    if Assigned(FCompileProfiler) then
      FCompileProfiler.Deterministic := True;
  end;
  FWorkers := 1;
  FHeapStart := GetHeapStatus;
  FCoverageMain := TGocciaCoverageTracker.Instance;
  if Assigned(FCoverageMain) and not FCoverageMain.Enabled then FCoverageMain := nil;
  if Assigned(FCoverageMain) then
  begin
    InitCriticalSection(FCoverageLock);
    FCoveragePaths := TStringList.Create;
    FCoveragePaths.Assign(APaths);
  end;
  { GocciaRunner's --source-map rules: stdin has no file to put the map beside, one map file names one input, and a
    --multifile input may be several }
  if FSourceMap.Present and (FSourceMap.ValueOr('') = '') and
     ((APaths.Count = 0) or ((APaths.Count = 1) and IsStdinPath(APaths[0]))) then
    raise TParseError.Create('--source-map=<file> is required when reading source from stdin.');
  if (FSourceMap.ValueOr('') <> '') and
     ((APaths.Count > 1) or ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TParseError.Create('--source-map=<file> supports a single input file or stdin.');
  if FSourceMap.Present and MultifileEnabled then
    raise TParseError.Create('--source-map cannot be combined with --multifile (an input '
      + 'may expand to multiple sections).');
  { no path, or "-": the source from stdin (the base class applied the no-argument rule); with --multifile its
    sections, one after another }
  if (APaths.Count = 0) or ((APaths.Count = 1) and IsStdinPath(APaths[0])) then
  begin
    Source := ReadSourceFromText(Input);
    if MultifileEnabled then
    begin
      { the source goes to SplitStdinMultifile, which registers each section }
      Names := SplitStdinMultifile(Source);
      try
        for I := 0 to Names.Count - 1 do
        begin
          if (I > 0) and not IsJsonOutput then
            WriteLn;
          Source := SourceRegistry.Load(Names[I]);
          try
            RunOnWorker(Names[I], Source);
          finally
            Source.Free;
          end;
        end;
      finally
        Names.Free;
      end;
    end
    else
      try
        RunOnWorker(STDIN_FILE_NAME, Source);
      finally
        Source.Free;
      end;
    if IsJsonOutput then
      PrintJsonReport;
    Exit;
  end;
  { "-" with paths: stdin cannot be interleaved with files }
  for I := 0 to APaths.Count - 1 do
    if IsStdinPath(APaths[I]) then
      raise TParseError.Create('stdin is supported only as the sole input.');
  { a directory is its script files; every config governing the inputs is checked before any of them runs }
  Files := TStringList.Create;
  try
    for I := 0 to APaths.Count - 1 do
      if DirectoryExists(APaths[I]) then
      begin
        Found := FindAllFiles(APaths[I], ScriptExtensions);
        try
          Files.AddStrings(Found);
        finally
          Found.Free;
        end;
      end
      else if FileExists(APaths[I]) then
        Files.Add(APaths[I])
      else if IsJsonOutput then
        raise Exception.Create('Path not found: ' + APaths[I]);
    if IsJsonOutput then
    begin
      { the files, or with --multifile their sections, as one report }
      Found := ExpandMultifileFiles(Files);
      try
        if Found.Count > 1 then
          RunFilesParallel(Found)
        else if Found.Count = 1 then
          RunFile(Found[0]);
      finally
        Found.Free;
      end;
      PrintJsonReport;
      Exit;
    end;
    ValidateFileConfigs(Files);
  finally
    Files.Free;
  end;
  { in order; a missing path stops the run where it stands, as GocciaRunner's RunScripts }
  for I := 0 to APaths.Count - 1 do
    if DirectoryExists(APaths[I]) then
    begin
      Found := FindAllFiles(APaths[I], ScriptExtensions);
      try
        Files := ExpandMultifileFiles(Found);
        try
          RunFileList(Files);
        finally
          Files.Free;
        end;
      finally
        Found.Free;
      end;
    end
    else if FileExists(APaths[I]) then
    begin
      if MultifileEnabled then
      begin
        Single := TStringList.Create;
        try
          Single.Add(APaths[I]);
          Files := ExpandMultifileFiles(Single);
          try
            RunFileList(Files);
          finally
            Files.Free;
          end;
        finally
          Single.Free;
        end;
      end
      else
        RunFile(APaths[I]);
    end
    else
      raise Exception.Create('Path not found: ' + APaths[I]);
end;

{ --coverage's reports, as GocciaRunner writes them: the summary (and one file's detail), then the LCOV or JSON
  file }
{ ---- sandbox mode ---- }

{ --deterministic in sandbox mode: only the root config, already merged into the options, governs a sandbox run }
function TGocciaRunnerApp.SandboxDeterministic: Boolean;
var
  EmptyConfig: TConfigEntryArray;
begin
  EmptyConfig := nil;
  Result := ResolveFlagOption(EngineOptions.Deterministic, EmptyConfig);
end;

{ the engine of a sandbox run (the root or a nested child), as GocciaRunner's ConfigureSandboxEngine: the root
  config's settings (a per-file config walk from a sandbox path would climb the host filesystem), the audit log
  (the root's effective capabilities, a child's decisions to the same sink), the command line's virtual modules
  (a config's read host files and are not applied), and the console }
procedure TGocciaRunnerApp.ConfigureSandboxEngine(const ARun: TGocciaSandboxRun);
var
  EmptyConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
  Effective: TGocciaCapabilityAuditEvent;
  Error: string;
begin
  EmptyConfig := nil;
  Verdict := FileConfigVerdict('');
  ApplySettings(ARun.Engine, ResolveFileSettings(EngineOptions, EmptyConfig, ARun.EntryPath,
    Verdict.AcceptedUnsafe));
  ARun.Engine.ConfigJson := HostConfigJson(EngineOptions, EmptyConfig);
  { --prefer-bytecode: the sandbox's imports take .gbc companions in its virtual filesystem too }
  if Assigned(ARun.Engine.Resolver) then
    ARun.Engine.Resolver.PreferBytecodeCompanions := ResolveFlagOption(FPreferBytecode, EmptyConfig);
  if EngineOptions.MaxStack.Present then
    ARun.Engine.MaxFrames := Integer(EngineOptions.MaxStack.Value);
  if FSilent.Present then
    ARun.Silent := True;
  if LogFileOpen then
    ARun.LogLine := HandleConsoleLog;
  if AuditLogOpen then
  begin
    ARun.Engine.AuditSink := HandleCapabilityAudit;
    if ARun.Parent = nil then
    begin
      Effective := Default(TGocciaCapabilityAuditEvent);
      Effective.Kind := gckCapabilitiesEffective;
      Effective.Decision := gcdAllow;
      Effective.Subject := ARun.Capabilities.ToJSON;
      Effective.Reason := CapabilityProvenance(Verdict);
      Effective.Source.FilePath := ARun.EntryPath;
      try
        HandleCapabilityAudit(Effective);
      except
        on E: Exception do
          raise EGocciaCapabilityAuditDeliveryError.Create('capability audit delivery failed: ' + E.Message);
      end;
    end;
  end;
  { the command line's virtual modules are the user's own: a --modules manifest is read from the host }
  SandboxHostReads(True);
  try
    if not DefineCommandLineVirtualModules(ARun.Engine, Error) then
      raise Exception.Create(Error);
  finally
    SandboxHostReads(False);
  end;
end;

{ a sandbox run's script error: the root's with the code frame its own modules give, read from the virtual
  filesystem; a child's (which its caller reads) with the location only and none of the host's suggestions }
function TGocciaRunnerApp.FormatSandboxFailure(const ARun: TGocciaSandboxRun; const AResult: TGocciaRunResult): string;
begin
  Result := FailureText(ARun.EntryPath, AResult, ARun.Source, ARun.Engine, '', ARun.Context.Fs,
    ARun.Parent = nil, ARun.Parent = nil, False);
end;

{ The sandbox path to run. A host entry inside a copied input runs from there; any other host entry is copied,
  without write-back, to /<basename>. }
function TGocciaRunnerApp.ResolveSandboxEntry(const AHost: TGocciaSandboxHost;
  const ARequest: TGocciaSandboxModeRequest): string;
var
  Target: string;
begin
  if ARequest.EntrySandbox <> '' then
    Exit(AHost.Context.Fs.Normalize(ARequest.EntrySandbox));
  if AHost.Inputs.SandboxPathOfHostFile(ARequest.EntryHost, Result) then
    Exit;
  Target := DefaultSandboxPathFor(ARequest.EntryHost, ARequest.EntryHost, 'the entry');
  if AHost.Context.Fs.Exists(Target) then
    raise TCLIUsageError.CreateFmt(
      'the entry %s would be copied to %s, which a copied input already ' +
      'fills; name the entry with --entry=%s, or give that input an ' +
      'explicit =<sandbox> path', [ExtractFileName(ARequest.EntryHost),
      Target, Target]);
  Result := AHost.CopyIn(ARequest.EntryHost, Target, False);
end;

procedure TGocciaRunnerApp.WriteSandboxDiff(const AHost: TGocciaSandboxHost;
  const ARequest: TGocciaSandboxModeRequest);
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
  { a config's diff file goes through the route pinned when the config was checked; the command line's is the
    user's own choice, written as named }
  if ARequest.DiffFilePin.IsSet then
  begin
    if not TSandboxHostOutputFile.Create(ARequest.DiffFile, ARequest.DiffFilePin).Write(Bytes, Problem) then
      raise Exception.CreateFmt('diff file %s: %s', [ARequest.DiffFile, Problem]);
  end
  else
    WriteCommandLineOutputFile(ARequest.DiffFile, Bytes);
end;

procedure TGocciaRunnerApp.VerifyRootConfig;
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

{ Whether the config a run would use has a sandbox section, for an error raised before sandbox mode is decided (an
  invalid value on the command line, or in that config), so it reports on stderr. The config may not be applied
  yet, so it is found here as Execute would find it: --config, else from the first existing path argument, else the
  working directory. }
function TGocciaRunnerApp.ConfigRequestsSandbox: Boolean;
var
  Arguments: TCommandLineArguments;
  ConfigPath, StartDirectory, Candidate: string;
  Entries: TConfigEntryArray;
  I, J: Integer;
begin
  Result := False;
  try
    ConfigPath := RootConfigPath;
    if ConfigPath = '' then
    begin
      Arguments := GetCommandLineArguments;
      StartDirectory := '';
      for I := 0 to High(Arguments) do
      begin
        Candidate := '';
        if Copy(Arguments[I], 1, Length('--config=')) = '--config=' then
          Candidate := Copy(Arguments[I], Length('--config=') + 1, MaxInt)
        else if (Arguments[I] = '--config') and (I < High(Arguments)) then
          Candidate := Arguments[I + 1];
        if Candidate <> '' then
        begin
          Candidate := ExpandFileName(Candidate);
          if DirectoryExists(Candidate) then
          begin
            for J := 0 to High(CONFIG_FILE_EXTENSIONS) do
              if FileExists(IncludeTrailingPathDelimiter(Candidate) +
                 CONFIG_FILE_BASE_NAME + CONFIG_FILE_EXTENSIONS[J]) then
              begin
                ConfigPath := IncludeTrailingPathDelimiter(Candidate) +
                  CONFIG_FILE_BASE_NAME + CONFIG_FILE_EXTENSIONS[J];
                Break;
              end;
          end
          else
            ConfigPath := Candidate;
          Break;
        end;
        if (StartDirectory = '') and (Arguments[I] <> '') and (Arguments[I][1] <> '-') then
          if DirectoryExists(Arguments[I]) then
            StartDirectory := ExpandFileName(Arguments[I])
          else if FileExists(Arguments[I]) then
            StartDirectory := ExtractFilePath(ExpandFileName(Arguments[I]));
      end;
      if ConfigPath = '' then
      begin
        if StartDirectory = '' then
          StartDirectory := GetCurrentDir;
        EnsureConfigParsersRegistered;
        ConfigPath := DiscoverConfigFile(StartDirectory, [CONFIG_FILE_BASE_NAME], CONFIG_FILE_EXTENSIONS);
      end;
    end;
    if (ConfigPath = '') or not FileExists(ConfigPath) then
      Exit;
    EnsureConfigParsersRegistered;
    Entries := ParseConfigFile(ConfigPath);
    for I := 0 to High(Entries) do
      if (Entries[I].Key = SANDBOX_CONFIG_KEY) or
         (Copy(Entries[I].Key, 1, Length(SANDBOX_CONFIG_KEY) + 1) = SANDBOX_CONFIG_KEY + '.') then
        Exit(True);
  except
    { a config that does not parse cannot say; the error reports as usual }
    on E: Exception do
      Result := False;
  end;
end;

{ --max-fs-bytes and --max-fs-nodes are ConfigIgnored, so host mode never reads (or validates) them from a config;
  sandbox mode takes them from the root config here, unless the command line set them. }
procedure TGocciaRunnerApp.ApplyConfigSandboxLimits;
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
    if Limits[I].FromCommandLine or not TryFindConfigEntry(Entries, Limits[I].LongName, Entry) then
      Continue;
    if Entry.InArray or (Entry.Kind in [cvkObject, cvkUnsupported, cvkEmptyArray]) then
      raise TParseError.CreateFmt('%s: "%s" must be a single value', [Entry.SourcePath, Entry.Key]);
    try
      Limits[I].Apply(Entry.Value);
    except
      on E: EOptionValueError do
        raise TParseError.CreateFmt('Invalid value for "%s" in %s: %s (%s)',
          [Entry.Key, Entry.SourcePath, E.Value, E.Reason]);
    end;
  end;
end;

{ The root config's keys that read host files, which sandbox mode does not apply: one warning each, in the style
  of an unsupported capability. }
procedure TGocciaRunnerApp.WarnIgnoredConfigHostKeys;
const
  HOST_FILE_KEYS: array[0..4] of string = ('modules', 'module', 'globals', 'global', 'host-environment');
  IMPORTS_CONFIG_PREFIX = 'imports.';
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
         (Copy(Key, 1, Length(HOST_FILE_KEYS[I]) + 1) = HOST_FILE_KEYS[I] + '.') then
      begin
        WarnOnce(RootConfigPath + #0 + HOST_FILE_KEYS[I], Format(
          'Warning: %s sets "%s", which %s does not apply; ignoring it',
          [RootConfigPath, HOST_FILE_KEYS[I], CapabilityPolicyName]));
        Break;
      end;
    end;
  { sandbox mode resolves only the command line's import map, so the root config's provider entries do nothing
    there }
  for J := 0 to High(Entries) do
    if (Copy(Entries[J].Key, 1, Length(IMPORTS_CONFIG_PREFIX)) = IMPORTS_CONFIG_PREFIX) and
       IsProviderAddress(Entries[J].Value) then
    begin
      WarnOnce(RootConfigPath + #0 + IMPORTS_CONFIG_PREFIX, Format(
        'Warning: %s sets provider imports, which %s does not apply; ignoring them',
        [RootConfigPath, CapabilityPolicyName]));
      Break;
    end;
end;

procedure TGocciaRunnerApp.RunSandbox(const ARequest: TGocciaSandboxModeRequest);
var
  Host: TGocciaSandboxHost;
  FsClock: TSandboxFsClock;
  RunResult: TGocciaSandboxRunResult;
  Report: TStringList;
  EntryPath: string;
  I: Integer;
begin
  { the root config is the only one that governs a sandbox run; its requests are checked before anything is copied
    in }
  VerifyRootConfig;
  WarnIgnoredConfigHostKeys;
  { the entry's own config, when it is not the root config, has no say in a sandbox run }
  if ARequest.EntryHost <> '' then
    WarnIfSandboxSectionUnread(DiscoverFileConfigPath(ARequest.EntryHost));
  for I := 0 to High(ARequest.Notes) do
    WriteLn(ErrOutput, ARequest.Notes[I]);

  { under --deterministic the filesystem's timestamps read the clock the engines' deterministic profile reads, set
    before the inputs are copied in, so they carry it too }
  if SandboxDeterministic then
    FsClock := TGocciaSandboxFsHostClock.Create(TGocciaHostEnvironment.DeterministicClock)
  else
    FsClock := nil;
  Host := nil;
  try
    Host := TGocciaSandboxHost.Create(ARequest.MaxFsBytes, ARequest.MaxFsNodes, FsClock);
    FSandboxHost := Host;
    Host.TimeoutMilliseconds := EngineOptions.Timeout.Milliseconds(0);
    Host.MaxInstructions := EngineOptions.MaxInstructions.ValueOr(0);
    Host.InspectDepth := InspectDepthOf(EngineOptions);
    Host.ImportMapPath := EngineOptions.ImportMap.ValueOr('');
    Host.Aliases.Assign(EngineOptions.Aliases.Values);
    { filtered to net by HonoredCapabilities }
    Host.RootCapabilities := ResolveEngineCapabilities('');
    Host.Deterministic := SandboxDeterministic;
    Host.OnConfigureEngine := ConfigureSandboxEngine;
    Host.OnFormatFailure := FormatSandboxFailure;
    { only --print reads the root run's value }
    Host.PrintRootResult := FPrint.Present;

    { a config-named read-write input carries the pin taken when the config was checked, so write-back reaches it
      only along that route }
    for I := 0 to High(ARequest.Inputs) do
      Host.CopyIn(ARequest.Inputs[I].HostPath, ARequest.Inputs[I].SandboxPath,
        ARequest.Inputs[I].ReadWrite, ARequest.Inputs[I].Pin);
    EntryPath := ResolveSandboxEntry(Host, ARequest);
    Host.CaptureBaseline;

    RunResult := Host.Run(EntryPath);
    if RunResult.Output <> '' then
      Write(RunResult.Output);
    if RunResult.ErrorOutput <> '' then
      Write(ErrOutput, RunResult.ErrorOutput)
    else if (not RunResult.Ok) and (RunResult.ErrorMessage <> '') then
      WriteLn(ErrOutput, RunResult.ErrorMessage);
    { as host mode: the bare value, undefined included }
    if RunResult.HasResultText then
      WriteLn(RunResult.ResultText);
    if not RunResult.Ok then
      ExitCode := RunResult.ExitCode;

    { ADR 0119: the host, not the guest, writes back, and only after a run that succeeded. The report is
      diagnostics, so stdout keeps only the guest's output and the diff. }
    if Host.Inputs.HasReadWriteInput then
    begin
      Report := TStringList.Create;
      try
        if RunResult.Ok then
        begin
          if not Host.Inputs.ApplyWriteBack(Host.Inputs.PlanWriteBack(Host.Context.Baseline), Report) then
            ExitCode := 1;
        end
        else
          Report.Add(WRITE_BACK_REPORT_PREFIX + 'skipped, the run did not succeed.');
        for I := 0 to Report.Count - 1 do
          WriteLn(ErrOutput, Report[I]);
      finally
        Report.Free;
      end;
    end;

    WriteSandboxDiff(Host, ARequest);
  finally
    FSandboxHost := nil;
    Host.Free;
    FsClock.Free;
  end;
end;

procedure TGocciaRunnerApp.AfterExecute;
var
  Mode: Goccia.CLI.Options.TGocciaProfileMode;
  Opcodes, Functions, Compile: Boolean;
begin
  { the profile reports, as GocciaRunner prints and writes them (host mode only) }
  if (not FSandboxActive) and ProfilerOptions.Mode.Present and Assigned(FProfiler) then
  begin
    Mode := ProfilerOptions.Mode.Value;
    Opcodes := (Mode = Goccia.CLI.Options.pmOpcodes) or (Mode = Goccia.CLI.Options.pmAll);
    Functions := (Mode = Goccia.CLI.Options.pmFunctions) or (Mode = Goccia.CLI.Options.pmAll);
    Compile := Mode = Goccia.CLI.Options.pmCompile;
    if not IsJsonOutput then
    begin
      if Opcodes then
      begin
        PrintOpcodeProfile(FProfiler);
        PrintOpcodePairProfile(FProfiler);
        PrintScalarHitRate(FProfiler);
        PrintShapeSaturation(FProfiler);
      end;
      if Functions then
        PrintFunctionProfile(FProfiler);
      if Compile and Assigned(FCompileProfiler) then
        PrintCompileProfile(FCompileProfiler);
    end;
    if ProfilerOptions.OutputPath.Present then
    begin
      if ProfilerOptions.Format.Matches(pfFlamegraph) then
        WriteCollapsedStacks(FProfiler, ProfilerOptions.OutputPath.Value)
      else
        WriteProfileJSON(FProfiler, ProfilerOptions.OutputPath.Value);
    end;
  end;
  { coverage is a host-mode report; sandbox mode keeps stdout to the guest's output and the diff }
  if Assigned(FCoverageMain) and not FSandboxActive then
  begin
    if not IsJsonOutput then
    begin
      PrintCoverageSummary(FCoverageMain);
      if (FCoveragePaths.Count = 1) and FileExists(FCoveragePaths[0]) then
        PrintCoverageDetail(FCoverageMain, FCoveragePaths[0]);
    end;
    if CoverageOptions.Format.Matches(cfLcov) and (CoverageOptions.OutputPath.ValueOr('') <> '') then
      WriteCoverageLcov(FCoverageMain, CoverageOptions.OutputPath.Value);
    if CoverageOptions.Format.Matches(cfJson) and (CoverageOptions.OutputPath.ValueOr('') <> '') then
      WriteCoverageJSON(FCoverageMain, CoverageOptions.OutputPath.Value);
    FreeAndNil(FCoveragePaths);
    DoneCriticalSection(FCoverageLock);
    FCoverageMain := nil;
  end;
  inherited AfterExecute;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TGocciaRunnerApp, 'GocciaRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
