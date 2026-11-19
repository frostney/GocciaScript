program GocciaRunnerG2;

{ GocciaRunner on the G2 engine, while the apps move from the old engine to
  G2: the same command line, configs, trust and capability policy (the base
  class), each entry file run by a TG2Engine (G2.Embed). `./build.pas --g2
  runner` builds it as build/GocciaRunner, so the CLI tests run against it
  unchanged; once it covers GocciaRunner it replaces GocciaRunner.dpr.

  Not yet on G2: JSON output, --print, --silent, sandbox and install modes,
  source maps, coverage, the profiler, bytecode files, injected globals and
  modules, directories, stdin, parallel jobs, and the capability checks the
  engine makes. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  TimingUtils,

  Goccia.Application,
  Goccia.CLI.Application,
  Goccia.CLI.FileSettings,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.Trust,
  Goccia.Error,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,

  G2.Core,
  G2.Embed,
  G2.VM;

type
  TG2RunnerApp = class(TGocciaCLIApplication)
  private
    { GocciaRunner's options, all registered so that its command lines and configs parse the same; the ones G2
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
    procedure RunSource(const AFileName: string; const ASource: TStringList);
    procedure RunOnWorker(const AFileName: string; const ASource: TStringList);
    procedure ReportFailure(const AFileName: string;
      const AResult: TG2RunResult; const ASource: TStringList);
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

type
  { One entry file on a thread of its own: a G2 engine's state is its thread's (G2.Engine), so each file gets a
    fresh engine there and nothing of the previous one survives. The thread compiles with the JIT. }
  TG2FileThread = class(TThread)
  private
    FApp: TG2RunnerApp;
    FFileName: string;
    FSource: TStringList;
    FErrorClass: ExceptClass;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  end;

const
  { reserved, not committed: room for native recursion as deep as the main thread's }
  FILE_THREAD_STACK = 64 * 1024 * 1024;

procedure TG2FileThread.Execute;
begin
  try
    JitTakeThread;
    FApp.RunSource(FFileName, FSource);
  except
    on E: Exception do
    begin
      FErrorClass := ExceptClass(E.ClassType);
      FErrorMessage := E.Message;
    end;
  end;
end;

const
  PACKAGES_GROUP = 'Packages';

{ GocciaRunner's option set (GocciaRunner.dpr TRunnerApp.Configure) }
procedure TG2RunnerApp.Configure;
begin
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

{ host mode's: every capability (sandbox mode, which honours net alone, is not on G2 yet) }
function TG2RunnerApp.HonoredCapabilities: TGocciaHonoredCapabilities;
begin
  Result := ALL_CAPABILITIES;
end;

function TG2RunnerApp.StdinUsage: TGocciaStdinUsage;
begin
  Result := suStdinDefaultWithREPL;
end;

function TG2RunnerApp.UsageLine: string;
begin
  Result := '[file|directory|-] [options]' + sLineBreak +
    '       ' + Name + ' <file> [--copy <host>[=<sandbox>]]... [options]';
end;

{ The file's settings as G2's language profile and run settings. }
procedure ApplySettings(const AEngine: TG2Engine;
  const ASettings: TGocciaFileSettings);
var
  Profile: TLangProfile;
begin
  Profile := RecommendedProfile;
  Profile.Asi := cfASI in ASettings.Compatibility;
  Profile.VarDecl := cfVar in ASettings.Compatibility;
  Profile.FuncDecl := cfFunction in ASettings.Compatibility;
  Profile.ForLoop := cfTraditionalFor in ASettings.Compatibility;
  Profile.WhileLoop := cfWhileLoops in ASettings.Compatibility;
  Profile.LooseEq := cfLooseEquality in ASettings.Compatibility;
  Profile.NonStrict := cfNonStrictMode in ASettings.Compatibility;
  Profile.ArgumentsObject := cfArgumentsObject in ASettings.Compatibility;
  Profile.Labels := ASettings.LabelStatements;
  Profile.ForIn := ASettings.ForInLoops;
  Profile.WarningUnsupported := ASettings.WarningUnsupportedFeatures;
  Profile.StrictTypes := ASettings.StrictTypes;
  Profile.FunctionCtor := ASettings.FunctionConstructor;
  Profile.ShadowRealm := ASettings.ShadowRealm;
  AEngine.Profile := Profile;
  case ASettings.SourceType of
    fstScript: AEngine.SourceKind := g2skScript;
    fstModule: AEngine.SourceKind := g2skModule;
  else
    AEngine.SourceKind := g2skDetect;
  end;
  if ASettings.MaxMemorySet then
    AEngine.MaxMemory := ASettings.MaxMemory
  else
    AEngine.MaxMemory := 0;
end;

procedure TG2RunnerApp.ReportFailure(const AFileName: string;
  const AResult: TG2RunResult; const ASource: TStringList);
var
  Name: string;
begin
  case AResult.Aborted of
    g2aTimeout:
      begin
        WriteLn(Format('Fatal error: file timed out after %dms',
          [EngineOptions.Timeout.Value]));
        Exit;
      end;
    g2aInstructionLimit:
      begin
        WriteLn(Format('Fatal error: Execution exceeded instruction limit of %d',
          [EngineOptions.MaxInstructions.Value]));
        Exit;
      end;
  end;
  if not AResult.ErrorIsError then
  begin
    WriteLn(UTF8Encode(AResult.ErrorMessage));
    Exit;
  end;
  Name := UTF8Encode(AResult.ErrorName);
  if Name = '' then
    Name := 'Error';
  if AResult.ErrorLine > 0 then
    WriteLn(FormatErrorWithSourceContext(Name, UTF8Encode(AResult.ErrorText),
      UTF8Encode(AResult.ErrorFile), AResult.ErrorLine, AResult.ErrorColumn,
      ASource, IsColorTerminal, UTF8Encode(AResult.ErrorSuggestion)))
  else if AResult.ErrorText <> '' then
    WriteLn(Name, ': ', UTF8Encode(AResult.ErrorText))
  else
    WriteLn(Name);
end;

procedure TG2RunnerApp.RunSource(const AFileName: string;
  const ASource: TStringList);
var
  FileConfigPath: string;
  FileConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
  Engine: TG2Engine;
  RunResult: TG2RunResult;
  Start: Int64;
begin
  Start := GetNanoseconds;
  begin
    FileConfigPath := DiscoverFileConfigPath(AFileName);
    if FileConfigPath <> '' then
      FileConfig := LoadFileConfig(FileConfigPath)
    else
      SetLength(FileConfig, 0);
    Verdict := FileConfigVerdict(FileConfigPath, AFileName);
    Engine := TG2Engine.Create;
    try
      { GocciaRunner has no timer globals (the test runner does) }
      Engine.ExposeTimers := False;
      Engine.CaptureResult := FPrint.Present;
      Engine.FailOnUnhandledRejection := True;
      ApplySettings(Engine, ResolveFileSettings(EngineOptions, FileConfig,
        AFileName, Verdict.AcceptedUnsafe));
      if EngineOptions.Timeout.Present then
        Engine.TimeoutMs := EngineOptions.Timeout.Value;
      if EngineOptions.MaxInstructions.Present then
        Engine.MaxInstructions := EngineOptions.MaxInstructions.Value;
      { G2's own frame bound unless one is given: a bound keeps compiled code
        out (G2.VM SetMaxFrames) }
      if EngineOptions.MaxStack.Present then
        Engine.MaxFrames := Integer(EngineOptions.MaxStack.Value);
      RunResult := Engine.Run(AFileName, UTF8Decode(ASource.Text));
      if not RunResult.Ok then
      begin
        ReportFailure(AFileName, RunResult, ASource);
        ExitCode := 1;
        Exit;
      end;
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
    end;
  end;
end;

procedure TG2RunnerApp.RunOnWorker(const AFileName: string;
  const ASource: TStringList);
var
  Worker: TG2FileThread;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
begin
  Worker := TG2FileThread.Create(True, FILE_THREAD_STACK);
  try
    Worker.FApp := Self;
    Worker.FFileName := AFileName;
    Worker.FSource := ASource;
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

procedure TG2RunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I: Integer;
  Source: TStringList;
begin
  { no path, or "-": the source from stdin (the base class applied the no-argument rule) }
  if (APaths.Count = 0) or ((APaths.Count = 1) and IsStdinPath(APaths[0])) then
  begin
    Source := ReadSourceFromText(Input);
    try
      RunOnWorker(STDIN_FILE_NAME, Source);
    finally
      Source.Free;
    end;
    Exit;
  end;
  for I := 0 to APaths.Count - 1 do
  begin
    Source := SourceRegistry.Load(APaths[I]);
    try
      RunOnWorker(APaths[I], Source);
    finally
      Source.Free;
    end;
  end;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TG2RunnerApp, 'GocciaRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
