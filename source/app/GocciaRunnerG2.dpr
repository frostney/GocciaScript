program GocciaRunnerG2;

{ GocciaRunner on the G2 engine, while the apps move from the old engine to
  G2: the same command line, configs, trust and capability policy (the base
  class), each entry file run by a TG2Engine (G2.Embed). `./build.pas --g2
  runner` builds it as build/GocciaRunner, so the CLI tests run against it
  unchanged; once it covers GocciaRunner it replaces GocciaRunner.dpr.

  Not yet on G2: sandbox and install modes, source maps, coverage, the
  profiler, bytecode files (--prefer-bytecode), injected globals and modules
  (--globals, --global, --modules), --host-environment, directories, parallel
  jobs, memory statistics in the JSON report, and the capability checks the
  engine makes. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  FileUtils,
  TimingUtils,

  Goccia.Application,
  Goccia.CLI.Application,
  Goccia.CLI.JSON.Reporter,
  Goccia.CLI.FileSettings,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.Trust,
  Goccia.Capabilities,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.Modules.Configuration,
  Goccia.Modules.Resolver,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,

  G2.Core,
  G2.Embed,
  G2.VM;

type
  { The console of a JSON run, as GocciaRunner's TScriptLoaderConsoleCapture: every line in output, and each
    method's line, without the prefix the console gave it, on its own stream }
  TG2ConsoleCapture = class
  private
    FOutputLines: TStringList;
    FStdoutLines: TStringList;
    FStderrLines: TStringList;
  public
    FileName: string;
    { the console's lines go to the capture (a JSON report), are dropped (--silent), or go to stdout; and to the
      --log file through LogLine when it is open }
    Capturing, Silent: Boolean;
    LogLine: procedure(const AMethod, ALine: string) of object;
    procedure Emit(const AMethod, ALine: UnicodeString);
    constructor Create;
    destructor Destroy; override;
    procedure CaptureOutput(const AMethod, ALine: UnicodeString);
    procedure Discard(const AMethod, ALine: UnicodeString);
    { a parse warning as GocciaRunner prints it (PrintPipelineWarnings); FileName set by the runner }
    procedure PrintWarning(const AMessage, ASuggestion: UnicodeString; ALine, AColumn: Integer);
    function OutputText: string;
    function StdoutText: string;
    function StderrText: string;
  end;

  { node_modules resolution as the file's capabilities allow it (TGocciaEngine.GrantNodeModules): the walk's
    ceiling when import grants it, a refusal when it is denied, sealed otherwise }
  TG2NodeModulesGrant = class
  private
    FCapabilities: TGocciaCapabilities;
  public
    constructor Create(const ACapabilities: TGocciaCapabilities);
    function Grant(const ASpecifier, AImportingDirectory: string; out ACeiling: string): Boolean;
  end;

  { one file's part of a JSON report }
  TG2FileReport = record
    FileName: string;
    Ok: Boolean;
    OutputText, StdoutText, StderrText: string;
    ResultJSON: string;
    ErrorInfo: TCLIJSONErrorInfo;
    Timing: TCLIJSONTiming;
  end;

  TG2RunnerApp = class(TGocciaCLIApplication)
  private
    FReports: array of TG2FileReport;
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
    procedure RunFile(const AFileName: string);
    function InjectGlobals(const AEngine: TG2Engine; out AError: string): Boolean;
    function DefineVirtualModules(const AEngine: TG2Engine; const AFileConfigPath: string;
      out AError: string): Boolean;
    procedure ReportFailure(const AFileName: string;
      const AResult: TG2RunResult; const ASource: TStringList);
    function IsJsonOutput: Boolean;
    function IsCompactJsonOutput: Boolean;
    procedure PrintJsonReport;
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

constructor TG2NodeModulesGrant.Create(const ACapabilities: TGocciaCapabilities);
begin
  inherited Create;
  FCapabilities := ACapabilities;
end;

function TG2NodeModulesGrant.Grant(const ASpecifier, AImportingDirectory: string;
  out ACeiling: string): Boolean;
begin
  Result := FCapabilities.NodeModulesCeiling(AImportingDirectory, ACeiling);
  if Result then
    Exit;
  { an outright deny refuses; a capability never granted keeps the sealed resolution's message }
  if FCapabilities.DeniesNodeModules(AImportingDirectory) then
    raise Exception.Create('PermissionDenied: import is not allowed for ' + ASpecifier +
      ' (node_modules resolution is denied for ' + AImportingDirectory + ')');
end;

constructor TG2ConsoleCapture.Create;
begin
  inherited Create;
  FOutputLines := TStringList.Create;
  FStdoutLines := TStringList.Create;
  FStderrLines := TStringList.Create;
end;

destructor TG2ConsoleCapture.Destroy;
begin
  FStderrLines.Free;
  FStdoutLines.Free;
  FOutputLines.Free;
  inherited Destroy;
end;

function MethodWritesToStderr(const AMethod: string): Boolean;
begin
  Result := (AMethod = 'warn') or (AMethod = 'error') or
    (AMethod = 'assert') or (AMethod = 'trace');
end;

function UnformatLine(const AMethod, ALine: string): string;
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

procedure TG2ConsoleCapture.CaptureOutput(const AMethod, ALine: UnicodeString);
var
  Method, Line: string;
begin
  Method := UTF8Encode(AMethod);
  Line := UTF8Encode(ALine);
  FOutputLines.Add(Line);
  if MethodWritesToStderr(Method) then
    FStderrLines.Add(UnformatLine(Method, Line))
  else
    FStdoutLines.Add(UnformatLine(Method, Line));
end;

{ --silent }
procedure TG2ConsoleCapture.Discard(const AMethod, ALine: UnicodeString);
begin
end;

procedure TG2ConsoleCapture.Emit(const AMethod, ALine: UnicodeString);
begin
  if Assigned(LogLine) then
    LogLine(UTF8Encode(AMethod), UTF8Encode(ALine));
  if Capturing then
    CaptureOutput(AMethod, ALine)
  else if not Silent then
    WriteLn(UTF8Encode(ALine));
end;

procedure TG2ConsoleCapture.PrintWarning(const AMessage, ASuggestion: UnicodeString;
  ALine, AColumn: Integer);
begin
  WriteLn('Warning: ', UTF8Encode(AMessage));
  if ASuggestion <> '' then
    WriteLn('  Suggestion: ', UTF8Encode(ASuggestion));
  WriteLn(Format('  --> %s:%d:%d', [FileName, ALine, AColumn]));
end;

function TG2ConsoleCapture.OutputText: string;
begin
  Result := FOutputLines.Text;
end;

function TG2ConsoleCapture.StdoutText: string;
begin
  Result := FStdoutLines.Text;
end;

function TG2ConsoleCapture.StderrText: string;
begin
  Result := FStderrLines.Text;
end;

function TG2RunnerApp.IsJsonOutput: Boolean;
begin
  Result := FOutputPath.Present and
    ((FOutputPath.Value = 'json') or (FOutputPath.Value = 'compact-json'));
end;

function TG2RunnerApp.IsCompactJsonOutput: Boolean;
begin
  Result := FOutputPath.Present and (FOutputPath.Value = 'compact-json');
end;

{ The run's error as the JSON report's error object, as GocciaRunner's ExceptionToCLIJSONErrorInfo makes it }
function ErrorInfoOf(const AFileName: string;
  const AResult: TG2RunResult; const AEngineOptions: TGocciaEngineOptions): TCLIJSONErrorInfo;
begin
  Result := DefaultCLIJSONErrorInfo;
  Result.FileName := AFileName;
  case AResult.Aborted of
    g2aTimeout:
      begin
        Result.ErrorType := 'TimeoutError';
        Result.Message := Format('file timed out after %dms', [AEngineOptions.Timeout.Value]);
        Exit;
      end;
    g2aInstructionLimit:
      begin
        Result.ErrorType := 'InstructionLimitError';
        Result.Message := Format('Execution exceeded instruction limit of %d',
          [AEngineOptions.MaxInstructions.Value]);
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

{ the files' report: one file's own form, or the aggregate of GocciaRunner's BuildAggregateScriptLoaderJSON }
procedure TG2RunnerApp.PrintJsonReport;
var
  I: Integer;
  Ok: Boolean;
  StdoutText, StderrText, OutputText, FilesJSON, ErrorJSON, FileJSON: string;
  Timing: TCLIJSONTiming;
begin
  if Length(FReports) = 1 then
  begin
    with FReports[0] do
      if Ok then
        WriteLn(BuildCLIScriptSuccessJSON(FileName, ResultJSON, OutputText,
          StdoutText, StderrText, Timing, DefaultCLIJSONMemoryStats, 1, 1,
          IsCompactJsonOutput))
      else
        WriteLn(BuildCLIScriptErrorJSON(FileName, OutputText, StdoutText,
          StderrText, ErrorInfo, Timing, DefaultCLIJSONMemoryStats, 1, 1,
          IsCompactJsonOutput));
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
    if FReports[I].Ok then
      FileJSON := BuildCLIScriptFileSuccessJSON(FReports[I].FileName,
        FReports[I].ResultJSON, FReports[I].OutputText,
        FReports[I].StdoutText, FReports[I].StderrText, FReports[I].Timing,
        DefaultCLIJSONMemoryStats, IsCompactJsonOutput)
    else
    begin
      FileJSON := BuildCLIScriptFileErrorJSON(FReports[I].FileName,
        FReports[I].OutputText, FReports[I].StdoutText,
        FReports[I].StderrText, FReports[I].ErrorInfo, FReports[I].Timing,
        DefaultCLIJSONMemoryStats, IsCompactJsonOutput);
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
    ErrorJSON, Timing, DefaultCLIJSONMemoryStats, 1, 1, FilesJSON, '',
    IsCompactJsonOutput));
end;

function IsAbsoluteFilePath(const APath: string): Boolean;
begin
  Result := (APath <> '') and ((APath[1] = PathDelim) or
    ((Length(APath) >= 2) and (APath[2] = ':')));
end;

{ a path a config names, against the config's directory (Goccia.CLI.Application's ConfigInputPath) }
function ConfigInputPath(const AWritten, AConfigPath: string): string;
begin
  if IsAbsoluteFilePath(AWritten) then
    Result := ExpandFileName(AWritten)
  else
    Result := ExpandFileName(IncludeTrailingPathDelimiter(
      ExtractFileDir(AConfigPath)) + AWritten);
end;

function ReadFileUnicode(const APath: string): UnicodeString;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(APath);
    Result := UTF8Decode(Lines.Text);
  finally
    Lines.Free;
  end;
end;

{ GocciaRunner's globals (ApplyDataGlobalsToEngine): the root config's globals files, then the command line's, then
  each --global name=value. Globals modules (a .js file's exports) are not on G2 yet. }
function TG2RunnerApp.InjectGlobals(const AEngine: TG2Engine; out AError: string): Boolean;
var
  Entries: TConfigEntryArray;
  I: Integer;
  Path: string;
  Pair: TScriptLoaderGlobalPair;
  Err: UnicodeString;
begin
  Result := False;
  AError := '';
  Err := '';
  Entries := ConfigNamedEntries(FGlobalFiles);
  for I := 0 to High(Entries) do
  begin
    Path := ConfigInputPath(Entries[I].Value, Entries[I].SourcePath);
    if IsStructuredGlobalsFile(Path) and not AEngine.InjectDataGlobals(ReadFileUnicode(Path),
       UnicodeString(ExtractFileExt(Path)), UnicodeString(Path), Err) then
    begin
      AError := UTF8Encode(Err);
      Exit;
    end;
  end;
  if FGlobalFiles.FromCommandLine then
    for I := 0 to FGlobalFiles.Values.Count - 1 do
    begin
      Path := FGlobalFiles.Values[I];
      if IsStructuredGlobalsFile(Path) then
      begin
        if not AEngine.InjectDataGlobals(ReadFileUnicode(Path), UnicodeString(ExtractFileExt(Path)),
           UnicodeString(Path), Err) then
        begin
          AError := UTF8Encode(Err);
          Exit;
        end;
      end
      { a globals module: its named exports, evaluated in the script's engine }
      else if not AEngine.InjectModuleGlobals(UTF8Decode(Path), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
    end;
  for I := 0 to FInlineGlobals.Values.Count - 1 do
  begin
    Pair := ParseGlobalPair(FInlineGlobals.Values[I]);
    if not AEngine.InjectInlineGlobal(UTF8Decode(Pair.Key), UTF8Decode(Pair.ValueText), Err) then
    begin
      AError := UTF8Encode(Err);
      Exit;
    end;
  end;
  Result := True;
end;

{ GocciaRunner's virtual modules (ApplyConfiguredVirtualModules): the root config's, the file's own config's when
  it has another, then the command line's --modules manifests and --module definitions. JavaScript and TypeScript
  manifests are not on G2 yet. }
function TG2RunnerApp.DefineVirtualModules(const AEngine: TG2Engine; const AFileConfigPath: string;
  out AError: string): Boolean;
var
  I: Integer;
  Path, Ext: string;
  Err: UnicodeString;
begin
  Result := False;
  AError := '';
  Err := '';
  if (RootConfigPath <> '') and not AEngine.DefineConfigVirtualModules(UTF8Decode(RootConfigPath), Err) then
  begin
    AError := UTF8Encode(Err);
    Exit;
  end;
  if (AFileConfigPath <> '') and (ExpandFileName(AFileConfigPath) <> ExpandFileName(RootConfigPath)) and
     not AEngine.DefineConfigVirtualModules(UTF8Decode(AFileConfigPath), Err) then
  begin
    AError := UTF8Encode(Err);
    Exit;
  end;
  if EngineOptions.ModuleManifests.FromCommandLine then
    for I := 0 to EngineOptions.ModuleManifests.Values.Count - 1 do
    begin
      Path := ExpandFileName(EngineOptions.ModuleManifests.Values[I]);
      Ext := LowerCase(ExtractFileExt(Path));
      if (Ext = '.js') or (Ext = '.mjs') or (Ext = '.ts') or (Ext = '.mts') then
      begin
        if not AEngine.DefineModuleManifestVirtualModules(UTF8Decode(Path), Err) then
        begin
          AError := UTF8Encode(Err);
          Exit;
        end;
      end
      else if not AEngine.DefineManifestVirtualModules(UTF8Decode(Path), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
    end;
  if EngineOptions.ModuleDefinitions.FromCommandLine then
    for I := 0 to EngineOptions.ModuleDefinitions.Values.Count - 1 do
      if not AEngine.DefineInlineVirtualModule(UTF8Decode(EngineOptions.ModuleDefinitions.Values[I]),
         UTF8Decode(GetCurrentDir), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
  Result := True;
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
  { a static import that did not load, located at its declaration (FormatHostErrorDiagnostic: no code frame) }
  if AResult.ErrorResolvedPath <> '' then
  begin
    Write(FormatErrorWithSourceContext(Name, UTF8Encode(AResult.ErrorText), UTF8Encode(AResult.ErrorFile),
      AResult.ErrorLine, AResult.ErrorColumn, nil, IsColorTerminal, ''));
    WriteLn('  Resolved to: ', UTF8Encode(AResult.ErrorResolvedPath));
    WriteLn;
    Exit;
  end;
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
  Capture: TG2ConsoleCapture;
  Report: TG2FileReport;
  Resolver: TGocciaModuleResolver;
  Grant: TG2NodeModulesGrant;
  AliasBaseDirectory: string;
  GlobalsError: string;
begin
  Start := GetNanoseconds;
  { the console capture, which also prints the parse warnings (not in a JSON report, as GocciaRunner) }
  Capture := TG2ConsoleCapture.Create;
  Capture.FileName := AFileName;
  try
    FileConfigPath := DiscoverFileConfigPath(AFileName);
    if FileConfigPath <> '' then
      FileConfig := LoadFileConfig(FileConfigPath)
    else
      SetLength(FileConfig, 0);
    Verdict := FileConfigVerdict(FileConfigPath, AFileName);
    { the file's import resolution, as GocciaRunner's CreateEngine configures it }
    Grant := TG2NodeModulesGrant.Create(ResolveEngineCapabilities(FileConfigPath, AFileName));
    Resolver := TGocciaModuleResolver.Create(ExtractFilePath(ExpandFileName(AFileName)));
    Resolver.SetExtensions(ModuleImportExtensions);
    Resolver.NodeModulesGrant := Grant.Grant;
    Resolver.ProviderCachedOnly := ResolveFlagOption(EngineOptions.CachedOnly, FileConfig);
    if EngineOptions.Aliases.FromCommandLine or (RootConfigPath = '') then
      AliasBaseDirectory := GetCurrentDir
    else
      AliasBaseDirectory := ExtractFilePath(RootConfigPath);
    ConfigureModuleResolver(Resolver, AFileName, EngineOptions.ImportMap.ValueOr(''),
      EngineOptions.Aliases.Values, AliasBaseDirectory);
    Engine := TG2Engine.Create;
    try
      Engine.Resolver := Resolver;
      { GocciaRunner has no timer globals (the test runner does) }
      Engine.ExposeTimers := False;
      Capture.Capturing := IsJsonOutput;
      Capture.Silent := FSilent.Present;
      if LogFileOpen then
        Capture.LogLine := HandleConsoleLog;
      if IsJsonOutput then
        Engine.ResultForm := g2rfJSON
      else if FPrint.Present then
        Engine.ResultForm := g2rfString;
      if Capture.Capturing or Capture.Silent or LogFileOpen then
        Engine.OnConsole := Capture.Emit;
      Engine.FailOnUnhandledRejection := True;
      Engine.Deterministic := ResolveFlagOption(EngineOptions.Deterministic, FileConfig);
      if not IsJsonOutput then
        Engine.OnWarning := Capture.PrintWarning;
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
      { a refused or unreadable global fails the file before it runs, as the old engine's RuntimeError }
      { a module definition that cannot apply is a configuration error, as GocciaRunner's }
      if not DefineVirtualModules(Engine, FileConfigPath, GlobalsError) then
      begin
        WriteLn('Fatal error: ', GlobalsError);
        ExitCode := 1;
        Exit;
      end;
      if not InjectGlobals(Engine, GlobalsError) then
      begin
        WriteLn(FormatErrorWithSourceContext('RuntimeError', GlobalsError, AFileName, 0, 0,
          ASource, IsColorTerminal, ''));
        ExitCode := 1;
        Exit;
      end;
      RunResult := Engine.Run(AFileName, UTF8Decode(ASource.Text));
      if IsJsonOutput then
      begin
        Report := Default(TG2FileReport);
        Report.FileName := AFileName;
        Report.Ok := RunResult.Ok;
        Report.OutputText := Capture.OutputText;
        Report.StdoutText := Capture.StdoutText;
        Report.StderrText := Capture.StderrText;
        Report.Timing.TotalTimeNanoseconds := GetNanoseconds - Start;
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
        SetLength(FReports, Length(FReports) + 1);
        FReports[High(FReports)] := Report;
        Exit;
      end;
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
      Resolver.Free;
      Grant.Free;
    end;
  finally
    Capture.Free;
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

procedure TG2RunnerApp.RunFile(const AFileName: string);
var
  Source: TStringList;
begin
  Source := SourceRegistry.Load(AFileName);
  try
    RunOnWorker(AFileName, Source);
  finally
    Source.Free;
  end;
end;

procedure TG2RunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I, J: Integer;
  Source, Files, Found: TStringList;
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
        for J := 0 to Found.Count - 1 do
          RunFile(Found[J]);
      finally
        Found.Free;
      end;
    end
    else if FileExists(APaths[I]) then
      RunFile(APaths[I])
    else
      raise Exception.Create('Path not found: ' + APaths[I]);
  if IsJsonOutput then
    PrintJsonReport;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TG2RunnerApp, 'GocciaRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
