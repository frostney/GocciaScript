program GocciaRunnerG2;

{ GocciaRunner on the G2 engine, while the apps move from the old engine to
  G2: the same command line, configs, trust and capability policy (the base
  class), each entry file run by a TG2Engine (G2.Embed). `./build.pas --g2
  runner` builds it as build/GocciaRunner, so the CLI tests run against it
  unchanged; once it covers GocciaRunner it replaces GocciaRunner.dpr.

  Not yet on G2: sandbox and install modes, the profiler, bytecode files
  (--prefer-bytecode), and the read capability's audit events. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  FileUtils,
  TextSemantics,
  TimingUtils,

  Goccia.Application,
  Goccia.CLI.Application,
  Goccia.CLI.JSON.Reporter,
  Goccia.CLI.FileSettings,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.SourceMaps,
  Goccia.CLI.Trust,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.JSX.Transformer,
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
    { the file's engine, which audits each decision (nil: none audited) }
    Engine: TG2Engine;
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
    { the file's engine heap and its thread's memory manager, from the engine's creation to the end of its run }
    Memory: TCLIJSONMemoryStats;
  end;

  TG2RunnerApp = class(TGocciaCLIApplication)
  private
    FReports: array of TG2FileReport;
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
    procedure RunSource(const AFileName: string; const ASource: TStringList; AReportIndex: Integer = -1);
    procedure RunFilesParallel(const AFiles: TStringList);
    { a parallel file whose source does not load: its report, the class and message of the failure (as
      GocciaRunner's worker reports it) }
    procedure ReportLoadFailure(AReportIndex: Integer; const AFileName: string; const E: Exception;
      AStart: Int64);
    procedure RunOnWorker(const AFileName: string; const ASource: TStringList);
    procedure RunFile(const AFileName: string);
    function InjectGlobals(const AEngine: TG2Engine; out AError: string): Boolean;
    function DefineVirtualModules(const AEngine: TG2Engine; const AFileConfigPath: string;
      out AError: string): Boolean;
    procedure ReportFailure(const AFileName: string;
      const AResult: TG2RunResult; const ASource: TStringList; AEngine: TG2Engine; const AWithheld: string);
    function HostEnvironmentPath(const AFileConfig: TConfigEntryArray; ADeterministic: Boolean): string;
    function IsJsonOutput: Boolean;
    function IsCompactJsonOutput: Boolean;
    procedure PrintJsonReport;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    procedure AfterExecute; override;
    procedure WriteRunSourceMap(const AFileName: string; const ASource: TStringList);
    procedure RunFileList(const AFiles: TStringList);
  end;

type
  { One entry file on a thread of its own: a G2 engine's state is its thread's (G2.Engine), so each file gets a
    fresh engine there and nothing of the previous one survives. The thread compiles with the JIT. }
  TG2FileThread = class(TThread)
  private
    FApp: TG2RunnerApp;
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

const
  { reserved, not committed: room for native recursion as deep as the main thread's }
  FILE_THREAD_STACK = 64 * 1024 * 1024;

var
  { 1 while a running file thread compiles with the JIT: one compiles at a time (its emitter is shared) }
  JitOwned: LongInt = 0;

procedure TG2FileThread.Execute;
var
  OwnsJit: Boolean;
  Start: Int64;
begin
  if FLoadSource then
  begin
    Start := GetNanoseconds;
    try
      FSource := FApp.SourceRegistry.Load(FFileName);
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
  AEngine.FetchMaxResponseBytes := ASettings.FetchMaxResponseBytes;
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
var
  Reason: string;
begin
  Result := FCapabilities.NodeModulesCeiling(AImportingDirectory, ACeiling);
  { every decision is audited, as GocciaRunner's GrantNodeModules does; a log that cannot be written ends the run }
  if Result then
  begin
    if ACeiling = '' then
      Reason := 'node_modules walk is unbounded'
    else
      Reason := 'node_modules walk is bounded by ' + ACeiling;
    if Assigned(Engine) and not Engine.Audit(gckImportNodeModules, True, ASpecifier, Reason) then
      raise EGocciaCapabilityAuditDeliveryError.Create('capability audit delivery failed');
    Exit;
  end;
  if Assigned(Engine) and not Engine.Audit(gckImportNodeModules, False, ASpecifier,
     'the import capability does not grant node_modules here') then
    raise EGocciaCapabilityAuditDeliveryError.Create('capability audit delivery failed');
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
    g2aMemoryLimit:
      begin
        { GocciaRunner's name for the refusal no script can catch (TGocciaMemoryLimitError) }
        Result.ErrorType := 'MemoryLimitError';
        Result.Message := UTF8Encode(AResult.ErrorText);
        Exit;
      end;
    g2aHostAbort:
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
  manager over the same span (memory.heap); G2 does not count objects, so their counts stay 0 }
function FileMemoryStats(const AStart, AEnd: TG2MemoryStats;
  const AHeapStart, AHeapEnd: THeapStatus): TCLIJSONMemoryStats;
begin
  Result := DefaultCLIJSONMemoryStats;
  Result.Enabled := True;
  Result.GCStartBytes := AStart.LiveBytes;
  Result.GCEndBytes := AEnd.LiveBytes;
  Result.GCLiveBytes := AEnd.LiveBytes;
  Result.GCPeakBytes := AEnd.PeakLiveBytes;
  Result.GCDeltaBytes := AEnd.LiveBytes - AStart.LiveBytes;
  Result.GCAllocatedDuringRunBytes := AEnd.AllocatedBytes - AStart.AllocatedBytes;
  Result.GCMaxBytes := AEnd.LimitBytes;
  Result.GCCollections := AEnd.Collections - AStart.Collections;
  Result.HeapStartAllocatedBytes := Int64(AHeapStart.TotalAllocated);
  Result.HeapEndAllocatedBytes := Int64(AHeapEnd.TotalAllocated);
  Result.HeapDeltaAllocatedBytes := Result.HeapEndAllocatedBytes - Result.HeapStartAllocatedBytes;
  Result.HeapStartFreeBytes := Int64(AHeapStart.TotalFree);
  Result.HeapEndFreeBytes := Int64(AHeapEnd.TotalFree);
  Result.HeapDeltaFreeBytes := Result.HeapEndFreeBytes - Result.HeapStartFreeBytes;
end;

{ a multi-file report's top-level memory: the files' collections and allocations summed. At once (AParallel) the
  engines' heaps coexist, so their live and peak bytes add up, as GocciaRunner combines its workers'; one after
  another each heap is freed before the next, so the peak is the largest and the live bytes are the last file's.
  memory.heap is the main thread's memory manager over the run. }
function AggregateMemoryStats(const AReports: array of TG2FileReport; AParallel: Boolean;
  const AHeapStart, AHeapEnd: THeapStatus): TCLIJSONMemoryStats;
var
  I: Integer;
  First: Boolean;
begin
  Result := DefaultCLIJSONMemoryStats;
  First := True;
  for I := 0 to High(AReports) do
  begin
    if not AReports[I].Memory.Enabled then
      Continue;
    if First or AParallel then
    begin
      if First then
        Result := AReports[I].Memory
      else
        Result := CombineCLIJSONMemoryStats(Result, AReports[I].Memory, True);
    end
    else
    begin
      Inc(Result.GCAllocatedDuringRunBytes, AReports[I].Memory.GCAllocatedDuringRunBytes);
      Inc(Result.GCCollections, AReports[I].Memory.GCCollections);
      if AReports[I].Memory.GCPeakBytes > Result.GCPeakBytes then
        Result.GCPeakBytes := AReports[I].Memory.GCPeakBytes;
      if AReports[I].Memory.GCMaxBytes > Result.GCMaxBytes then
        Result.GCMaxBytes := AReports[I].Memory.GCMaxBytes;
      Result.GCEndBytes := AReports[I].Memory.GCEndBytes;
      Result.GCLiveBytes := AReports[I].Memory.GCLiveBytes;
      Result.GCDeltaBytes := Result.GCEndBytes - Result.GCStartBytes;
    end;
    First := False;
  end;
  Result.Enabled := True;
  Result.HeapStartAllocatedBytes := Int64(AHeapStart.TotalAllocated);
  Result.HeapEndAllocatedBytes := Int64(AHeapEnd.TotalAllocated);
  Result.HeapDeltaAllocatedBytes := Result.HeapEndAllocatedBytes - Result.HeapStartAllocatedBytes;
  Result.HeapStartFreeBytes := Int64(AHeapStart.TotalFree);
  Result.HeapEndFreeBytes := Int64(AHeapEnd.TotalFree);
  Result.HeapDeltaFreeBytes := Result.HeapEndFreeBytes - Result.HeapStartFreeBytes;
end;

{ the files' report: one file's own form, or the aggregate of GocciaRunner's BuildAggregateScriptLoaderJSON. As
  GocciaRunner's: one file reports its memory at the top level; files at once report each file's and their sum,
  files one after another (one worker) only the sum }
procedure TG2RunnerApp.PrintJsonReport;
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
    ErrorJSON, Timing, AggregateMemoryStats(FReports, FWorkers > 1, FHeapStart, GetHeapStatus),
    FWorkers, FWorkers, FilesJSON, '',
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

{$I GocciaRunnerG2.Suggestions.inc}

{ Msg is Template with each %s, %d or %x standing for any text }
function TemplateMatches(const ATemplate, AMsg: string): Boolean;
var
  T, M, StarT, StarM: Integer;
begin
  T := 1;
  M := 1;
  StarT := 0;
  StarM := 0;
  while M <= Length(AMsg) do
  begin
    if (T < Length(ATemplate)) and (ATemplate[T] = '%') and (ATemplate[T + 1] in ['s', 'd', 'x']) then
    begin
      Inc(T, 2);
      StarT := T;
      StarM := M;
    end
    else if (T <= Length(ATemplate)) and (ATemplate[T] = AMsg[M]) then
    begin
      Inc(T);
      Inc(M);
    end
    else if StarT > 0 then
    begin
      Inc(StarM);
      M := StarM;
      T := StarT;
    end
    else
      Exit(False);
  end;
  while (T < Length(ATemplate)) and (ATemplate[T] = '%') and (ATemplate[T + 1] in ['s', 'd', 'x']) do
    Inc(T, 2);
  Result := T > Length(ATemplate);
end;

{ GocciaScript's suggestion for a runtime error's message, '' for none }
function RuntimeSuggestion(const AMessage: string): string;
var
  I: Integer;
begin
  for I := 0 to RUNTIME_SUGGESTION_COUNT - 1 do
    if TemplateMatches(RUNTIME_SUGGESTIONS[I, 0], AMessage) then
      Exit(RUNTIME_SUGGESTIONS[I, 1]);
  Result := '';
end;

{ AEngine: the run's engine, which says which files it loaded; AWithheld: a file whose source no report shows
  (the host environment's provider) }
procedure TG2RunnerApp.ReportFailure(const AFileName: string;
  const AResult: TG2RunResult; const ASource: TStringList; AEngine: TG2Engine; const AWithheld: string);
var
  Name, Suggestion, ErrorPath: string;
  Source, Loaded: TStringList;
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
    g2aMemoryLimit, g2aHostAbort:
      begin
        WriteLn('Fatal error: ' + UTF8Encode(AResult.ErrorText));
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
  Suggestion := UTF8Encode(AResult.ErrorSuggestion);
  if Suggestion = '' then
    Suggestion := RuntimeSuggestion(UTF8Encode(AResult.ErrorText));
  if AResult.ErrorLine > 0 then
  begin
    { the code frame from the file the error is in: an imported module's own source, never a file the entry's run
      did not load as a module (a thrown value's stack can name any path; a module the host evaluated first, such
      as a globals module or the host environment's provider, keeps its source) }
    Source := ASource;
    Loaded := nil;
    ErrorPath := UTF8Encode(AResult.ErrorFile);
    if (ErrorPath <> '') and (ErrorPath <> AFileName) and
       (ExpandFileName(ErrorPath) <> ExpandFileName(AFileName)) then
    begin
      Source := nil;
      if ((AWithheld = '') or (ExpandFileName(ErrorPath) <> AWithheld)) and
         AEngine.ModuleLoaded(UnicodeString(ExpandFileName(ErrorPath))) then
      begin
        Loaded := TStringList.Create;
        try
          Loaded.LoadFromFile(ErrorPath);
          Source := Loaded;
        except
          on EStreamError do
            Source := nil;
        end;
      end;
    end;
    try
      WriteLn(FormatErrorWithSourceContext(Name, UTF8Encode(AResult.ErrorText),
        ErrorPath, AResult.ErrorLine, AResult.ErrorColumn,
        Source, IsColorTerminal, Suggestion));
    finally
      Loaded.Free;
    end;
  end
  else if AResult.ErrorText <> '' then
    WriteLn(Name, ': ', UTF8Encode(AResult.ErrorText))
  else
    WriteLn(Name);
end;

{ --host-environment's module, else a config's "host-environment" (against that config's directory): the path,
  or '' for none. The command line's names a file from the working directory. }
{ --source-map: the JSX transform's map of the file (GocciaRunner's WriteSourceMapIfEnabled), beside it as
  <file>.map or at the path given; nothing for a file the transform leaves alone }
procedure TG2RunnerApp.WriteRunSourceMap(const AFileName: string; const ASource: TStringList);
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

function TG2RunnerApp.HostEnvironmentPath(const AFileConfig: TConfigEntryArray;
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

procedure TG2RunnerApp.RunSource(const AFileName: string;
  const ASource: TStringList; AReportIndex: Integer);
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
  HeapStart: THeapStatus;
  MemoryStart: TG2MemoryStats;
  Capabilities: TGocciaCapabilities;
  Effective: TGocciaCapabilityAuditEvent;
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
    Capabilities := ResolveEngineCapabilities(FileConfigPath, AFileName);
    Grant := TG2NodeModulesGrant.Create(Capabilities);
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
    Engine := TG2Engine.Create;
    try
      MemoryStart := Engine.Memory;
      Engine.Capabilities := Capabilities;
      Engine.Coverage := Assigned(FCoverageMain);
      Engine.Resolver := Resolver;
      { GocciaRunner has no timer globals (the test runner does) }
      Engine.ExposeTimers := False;
      Capture.Capturing := IsJsonOutput;
      Capture.Silent := FSilent.Present;
      if LogFileOpen then
        Capture.LogLine := HandleConsoleLog;
      if IsJsonOutput then
        Engine.ResultForm := g2rfJSON
      else if FPrint.Present and not FQuietWorkers then
        Engine.ResultForm := g2rfString;
      if Capture.Capturing or Capture.Silent or LogFileOpen then
        Engine.OnConsole := Capture.Emit;
      Engine.FailOnUnhandledRejection := True;
      Engine.Deterministic := ResolveFlagOption(EngineOptions.Deterministic, FileConfig);
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
      if FSourceMap.Present then
        WriteRunSourceMap(AFileName, ASource);
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

{ a JSON report's files run at once, as GocciaRunner's RunJSONFiles does (GetJobCount: --jobs, else the processors,
  at most one per file): each on a thread of its own, at most that many running, their reports in file order }
procedure TG2RunnerApp.RunFilesParallel(const AFiles: TStringList);
var
  I, Next, Oldest, Started: Integer;
  Workers: array of TG2FileThread;
  Running: Integer;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
begin
  FWorkers := GetJobCount(AFiles.Count);
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
      Workers[Next] := TG2FileThread.Create(True, FILE_THREAD_STACK);
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

procedure TG2RunnerApp.ReportLoadFailure(AReportIndex: Integer; const AFileName: string;
  const E: Exception; AStart: Int64);
var
  Report: TG2FileReport;
begin
  Report := Default(TG2FileReport);
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
procedure TG2RunnerApp.RunFileList(const AFiles: TStringList);
var
  J: Integer;
begin
  if GetJobCount(AFiles.Count) > 1 then
  begin
    WriteLn(Format('Running %d files with %d workers', [AFiles.Count, GetJobCount(AFiles.Count)]));
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

procedure TG2RunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I: Integer;
  Source, Files, Found, Names, Single: TStringList;
begin
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
procedure TG2RunnerApp.AfterExecute;
begin
  if Assigned(FCoverageMain) then
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
  RunResult := TGocciaApplication.RunApplication(TG2RunnerApp, 'GocciaRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
