program GocciaTestRunner;

{ GocciaTestRunner on the G2 engine, while the apps move from the old engine
  to G2: the same command line, configs, trust and capability policy (the base
  class), each test file run by a TG2Engine in test mode (G2.Embed TestMode,
  the testing API of prelude/testing.js) on a thread of its own, its suites
  reported by G2.HostAPI RunTestSuites. `./build.pas testrunner` builds it
  as build/GocciaTestRunner.

  Not yet on G2: snapshots, heap snapshots, coverage, --globals and --global,
  --test-timeout and --describe-timeout, and bytecode files. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  StrUtils,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  FileUtils,
  HostOutputFiles,
  TimingUtils,

  Goccia.Application,
  Goccia.CapabilityAudit,
  Goccia.Capabilities,
  Goccia.CLI.Application,
  Goccia.CLI.FileSettings,
  Goccia.CLI.G2FileEngine,
  Goccia.CLI.JSON.Reporter,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.Trust,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.FileExtensions,
  Goccia.JSON.Utils,
  Goccia.Modules.Configuration,
  Goccia.Modules.NodeResolution,
  Goccia.Modules.Resolver,
  Goccia.ScriptLoader.Input,
  Goccia.TestRunner.SnapshotHost,
  Goccia.TextFiles,
  Goccia.Builtins.Testing.Snapshots,

  G2.Core,
  G2.Embed,
  G2.HostAPI,
  G2.VM,
  Goccia.ThreadCleanupRegistry;

const
  { the old runner's default per-file timeout (GocciaTestRunner.dpr DEFAULT_TIMEOUT_MS): an endless loop does not
    hang the run; --timeout=N overrides it, --timeout=0 disables it }
  DEFAULT_TIMEOUT_MS = 30000;

  { a node_modules tree in a test folder is a module-resolution fixture, not a suite }
  TEST_DISCOVERY_EXCLUDED_DIRECTORIES: array[0..0] of string = (NODE_MODULES_DIRECTORY_NAME);

type
  { one file's results, plain data so they cross from its thread }
  TG2TestFileResult = record
    FileName: string;
    Ran: Boolean;
    Passed, Failed, Skipped, SuiteErrors, TotalTests, TotalRunTests, Assertions: Integer;
    DurationNanoseconds: Int64;
    FailedTests: array of string;
    { a file that did not load or run its suites: why (also printed on stderr) }
    ErrorMessage: string;
    { its diagnostic was printed already (the code frame of a file that threw) }
    Reported: Boolean;
    CompileNanoseconds, ExecuteNanoseconds: Int64;
    { its engine's and the memory manager's counters over the file }
    Memory: TCLIJSONMemoryStats;
    { --heap-snapshots: the objects expect() was given (repeats included, and distinct), how their round trips
      went, why each refused one refused and what each failed one differed in }
    HeapObservations, HeapObjects, HeapPassed, HeapRefused, HeapFailed: Integer;
    HeapRefusals: array of string;
    HeapFailures: array of string;
  end;

  { what a file's engine is set up from (TG2TestRunnerApp.SetUpTestEngine) }
  TG2FileContext = record
    FileName, FileConfigPath: string;
    FileConfig: TConfigEntryArray;
    Verdict: TGocciaConfigTrustVerdict;
    Capabilities: TGocciaCapabilities;
    Grant: TG2ImportGrant;
    Resolver: TGocciaModuleResolver;
    Capture: TG2ConsoleCapture;
  end;

  TG2TestRunnerApp = class(TGocciaCLIApplication)
  private
    FNoProgress: TFlagOption;
    FNoResults: TFlagOption;
    FExitOnFirst: TFlagOption;
    FSilent: TFlagOption;
    FOutputFile: TStringOption;
    FTestTimeout: TIntegerOption;
    FDescribeTimeout: TIntegerOption;
    FUpdateSnapshots: TFlagOption;
    FUpdateSnapshotsAlias: TFlagOption;
    FGlobalFiles: TRepeatableOption;
    FInlineGlobals: TRepeatableOption;
    FHeapSnapshots: TFlagOption;
    FNoVitestCompat: TFlagOption;
    FResults: array of TG2TestFileResult;
    FJobCount: Integer;
    { more than one file runs at once: their console output is not printed, as the old runner's workers }
    FParallel: Boolean;
    { --coverage: the run's tracker (the main thread's), which each file's thread merges its own into under
      FCoverageLock }
    FCoverageMain: TGocciaCoverageTracker;
    FCoverageLock: TRTLCriticalSection;
    { a snapshot write-back that failed after the files ran (the envelope's error) }
    FFinalizationError: string;
    { it names none of the files, so only the run's totals count it }
    FinalizationUnattributed: Boolean;
    procedure ReportCoverage(const AFiles: TStringList);
    procedure PrintHeapSnapshotReport;
    function SnapshotUpdateMode: TGocciaSnapshotUpdateMode;
    procedure RecordSnapshotFinalizationFailure(const AMessage: string);
    function IsJsonOutput: Boolean;
    function IsCompactJsonOutput: Boolean;
    procedure RunTestFile(AIndex: Integer; ASource: TStringList);
    { APrintWarnings: the file's own engine, whose parse warnings print (a heap check's engine loads again) }
    procedure SetUpTestEngine(AEngine: TG2Engine; const C: TG2FileContext; APrintWarnings: Boolean);
    function VerifyHeapRoot(const C: TG2FileContext; const ARoot: TG2HeapRoot; const ASource: UnicodeString;
      out ADetail: UnicodeString): TG2HeapVerdict;
    procedure RunFiles(const AFiles: TStringList; const AStdinSource: TStringList);
    procedure PrintResults(const ATiming: TCLIJSONTiming; const AMemory: TCLIJSONMemoryStats);
    procedure WriteResultsJSON(const AFileName: string; ACompact: Boolean; const ATiming: TCLIJSONTiming;
      const AMemory: TCLIJSONMemoryStats);
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    function ShouldApplyRootConfig(const APaths: TStringList; const AConfigPath: string;
      const AExplicitConfig: Boolean): Boolean; override;
    procedure Validate; override;
  end;

  { one test file on a thread of its own: a G2 engine's state is its thread's, so each file gets a fresh engine }
  TG2TestThread = class(TThread)
  private
    FApp: TG2TestRunnerApp;
    FIndex: Integer;
    FSource: TStringList;
    FOwnsSource: Boolean;
    FErrorClass: ExceptClass;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  end;

procedure TG2TestThread.Execute;
var
  OwnsJit: Boolean;
begin
  try
    OwnsJit := InterlockedCompareExchange(JitOwned, 1, 0) = 0;
    try
      if OwnsJit then
        JitTakeThread;
      try
        FApp.RunTestFile(FIndex, FSource);
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

{ ---- options ---- }

function TG2TestRunnerApp.HonoredCapabilities: TGocciaHonoredCapabilities;
begin
  Result := ALL_CAPABILITIES;
end;

{ GocciaTestRunner.dpr TTestRunnerApp.Configure }
procedure TG2TestRunnerApp.Configure;
begin
  AddEngineOptions;
  AddCoverageOptions;
  FNoProgress := AddFlag('no-progress', 'Suppress per-file progress output');
  FNoResults := AddFlag('no-results', 'Suppress test results summary');
  FExitOnFirst := AddFlag('exit-on-first-failure',
    'Stop on first test failure or suite error. Sequential runs stop at the failing file; under --jobs the ' +
    'queue is cancelled but files already in flight run to completion, so some files after the failure still ' +
    'execute.');
  FSilent := AddFlag('silent', 'Suppress console output from test scripts');
  FOutputFile := AddString('output',
    '"json" emits a structured JSON envelope to stdout, "compact-json" omits build, memory, stdout, stderr; ' +
    'any other value is treated as a file path that receives the full JSON envelope.');
  FTestTimeout := AddInteger('test-timeout',
    'Per-test timeout in ms (0 disables). Marks the test TIMEOUT and continues.');
  FDescribeTimeout := AddInteger('describe-timeout',
    'Per-describe timeout in ms (0 disables). Aborts the suite and continues.');
  FOutputFile.WritesHostFile := True;
  FOutputFile.AcceptsOutputModes := True;
  FUpdateSnapshots := AddFlag('update-snapshots', 'Create, update, and prune snapshots');
  FUpdateSnapshots.ShortName := 'u';
  FUpdateSnapshotsAlias := AddFlag('update', 'Alias for --update-snapshots');
  FGlobalFiles := AddRepeatable('globals',
    'Inject globals from a JSON/JSON5/TOML/YAML file or a module with named exports');
  FInlineGlobals := AddRepeatable('global', 'Inject a single global; value is parsed as JSON or kept as a string');
  FHeapSnapshots := AddFlag('heap-snapshots',
    'Round-trip objects observed by expect() at file idle; runs files serially');
  FNoVitestCompat := AddFlag('no-vitest-compat',
    'Do not resolve the bare "vitest" specifier to the bundled compatibility shim');
end;

{ several inputs each take their own config: a discovered root config does not apply to all of them }
function TG2TestRunnerApp.ShouldApplyRootConfig(const APaths: TStringList; const AConfigPath: string;
  const AExplicitConfig: Boolean): Boolean;
begin
  Result := inherited ShouldApplyRootConfig(APaths, AConfigPath, AExplicitConfig);
  if Result and not AExplicitConfig and (APaths.Count > 1) then
    Result := False;
end;

{ a coverage format or output asks for coverage }
procedure TG2TestRunnerApp.Validate;
begin
  inherited Validate;
  if CoverageOptions.Format.Present or CoverageOptions.OutputPath.Present then
    CoverageOptions.Enabled.Apply('');
end;

function TG2TestRunnerApp.IsJsonOutput: Boolean;
begin
  Result := Assigned(FOutputFile) and FOutputFile.Present and
    ((FOutputFile.Value = 'json') or (FOutputFile.Value = 'compact-json'));
end;

function TG2TestRunnerApp.IsCompactJsonOutput: Boolean;
begin
  Result := Assigned(FOutputFile) and FOutputFile.Present and (FOutputFile.Value = 'compact-json');
end;

function TG2TestRunnerApp.UsageLine: string;
begin
  Result := '<filename.js|directory> [options]';
end;

function TG2TestRunnerApp.StdinUsage: TGocciaStdinUsage;
begin
  Result := suStdinDefault;
end;

{ ---- snapshots ---- }

{ GocciaTestRunner.dpr IsContinuousIntegration: std-env's providers, as Vitest detects CI (a set variable is on,
  "0" and "false" included) }
function IsContinuousIntegration: Boolean;
const
  PROVIDERS: array[0..52] of string = ('CI', 'CONTINUOUS_INTEGRATION',
    'APPVEYOR', 'AWS_APP_ID', 'SYSTEM_TEAMFOUNDATIONCOLLECTIONURI', 'INPUT_AZURE_STATIC_WEB_APPS_API_TOKEN',
    'AC_APPCIRCLE', 'bamboo_planKey', 'BITBUCKET_COMMIT', 'BITRISE_IO', 'BUDDY_WORKSPACE_ID', 'BUILDKITE', 'CIRCLECI',
    'CIRRUS_CI', 'CF_PAGES', 'WORKERS_CI', 'K_SERVICE', 'CLOUD_RUN_JOB', 'CODEBUILD_BUILD_ARN', 'CF_BUILD_ID', 'DRONE',
    'DRONE_BUILD_EVENT', 'DSARI', 'GITHUB_ACTIONS', 'GITLAB_CI', 'CI_MERGE_REQUEST_ID', 'GO_PIPELINE_LABEL',
    'LAYERCI', 'JENKINS_URL', 'HUDSON_URL', 'MAGNUM', 'NETLIFY', 'NEVERCODE', 'RENDER', 'SAILCI', 'SEMAPHORE',
    'SCREWDRIVER', 'SHIPPABLE', 'TDDIUM', 'STRIDER', 'TEAMCITY_VERSION', 'TRAVIS', 'NOW_BUILDER',
    'APPCENTER_BUILD_ID', 'STACKBLITZ', 'STORMKIT', 'CLEAVR', 'ZEABUR', 'CODESPHERE_APP_ID', 'RAILWAY_PROJECT_ID',
    'RAILWAY_SERVICE_ID', 'DENO_DEPLOY', 'DENO_DEPLOYMENT_ID');
var
  Name: string;
begin
  for Name in PROVIDERS do
    if GetEnvironmentVariable(Name) <> '' then
      Exit(True);
  Result := GetEnvironmentVariable('FIREBASE_APP_HOSTING') <> '';
end;

{ -u writes every snapshot; in CI none is written, so a missing one fails; otherwise new ones are written }
function TG2TestRunnerApp.SnapshotUpdateMode: TGocciaSnapshotUpdateMode;
begin
  if FUpdateSnapshots.Present or FUpdateSnapshotsAlias.Present then
    Result := sumAll
  else if IsContinuousIntegration then
    Result := sumNone
  else
    Result := sumNew;
end;

{ GocciaTestRunner.dpr RecordSnapshotFinalizationFailure: the inline write-back's failure fails the file its message
  names (the longest match, paths compared with forward slashes), and is the envelope's error }
procedure TG2TestRunnerApp.RecordSnapshotFinalizationFailure(const AMessage: string);
var
  I, FileIndex: Integer;
  Message_: string;
begin
  { the run's totals count it whichever file it names }
  FFinalizationError := AMessage;
  Message_ := StringReplace(AMessage, PathDelim, '/', [rfReplaceAll]);
  FileIndex := -1;
  for I := 0 to High(FResults) do
    if Pos(StringReplace(FResults[I].FileName, PathDelim, '/', [rfReplaceAll]), Message_) > 0 then
      if (FileIndex < 0) or (Length(FResults[I].FileName) > Length(FResults[FileIndex].FileName)) then
        FileIndex := I;
  if FileIndex < 0 then
  begin
    FinalizationUnattributed := True;
    Exit;
  end;
  Inc(FResults[FileIndex].Failed);
  Inc(FResults[FileIndex].TotalRunTests);
  FResults[FileIndex].ErrorMessage := AMessage;
  SetLength(FResults[FileIndex].FailedTests, Length(FResults[FileIndex].FailedTests) + 1);
  FResults[FileIndex].FailedTests[High(FResults[FileIndex].FailedTests)] := AMessage;
end;

{ ---- one file ---- }

{ a failed test as the old runner lists it: its name and the error }
function FailedTestText(const AName, AError: string): string;
begin
  if AError = '' then
    Result := AName
  else
    Result := AName + ': ' + AError;
end;

{ RunTestSuites' report (prelude/testing.js report): an object of numbers and a failedTests list of
  name-and-error records, read into AResult; False when it is not one }
function ReadSuiteReport(const AJson: string; var AResult: TG2TestFileResult): Boolean;
var
  P: Integer;

  procedure SkipSpace;
  begin
    while (P <= Length(AJson)) and (AJson[P] in [' ', #9, #10, #13]) do
      Inc(P);
  end;

  function Expect(C: Char): Boolean;
  begin
    SkipSpace;
    Result := (P <= Length(AJson)) and (AJson[P] = C);
    if Result then
      Inc(P);
  end;

  function Peek: Char;
  begin
    SkipSpace;
    if P <= Length(AJson) then
      Result := AJson[P]
    else
      Result := #0;
  end;

  { a JSON string, as UTF-8 }
  function ReadString(out AValue: string): Boolean;
  var
    U: UnicodeString;
    Code, Low_: Integer;
    C: Char;
  begin
    AValue := '';
    if not Expect('"') then
      Exit(False);
    while P <= Length(AJson) do
    begin
      C := AJson[P];
      Inc(P);
      if C = '"' then
        Exit(True);
      if C <> '\' then
      begin
        AValue := AValue + C;
        Continue;
      end;
      if P > Length(AJson) then
        Exit(False);
      C := AJson[P];
      Inc(P);
      case C of
        'n': AValue := AValue + #10;
        't': AValue := AValue + #9;
        'r': AValue := AValue + #13;
        'b': AValue := AValue + #8;
        'f': AValue := AValue + #12;
        'u':
          begin
            if not TryStrToInt('$' + Copy(AJson, P, 4), Code) then
              Exit(False);
            Inc(P, 4);
            U := WideChar(Code);
            { a surrogate pair's second half }
            if (Code >= $D800) and (Code <= $DBFF) and (Copy(AJson, P, 2) = '\u') and
               TryStrToInt('$' + Copy(AJson, P + 2, 4), Low_) then
            begin
              U := U + WideChar(Low_);
              Inc(P, 6);
            end;
            AValue := AValue + UTF8Encode(U);
          end;
      else
        AValue := AValue + C;
      end;
    end;
    Result := False;
  end;

  function ReadNumber(out AValue: Double): Boolean;
  var
    Start: Integer;
    Settings: TFormatSettings;
  begin
    SkipSpace;
    Start := P;
    while (P <= Length(AJson)) and (AJson[P] in ['-', '+', '.', 'e', 'E', '0'..'9']) do
      Inc(P);
    Settings := DefaultFormatSettings;
    Settings.DecimalSeparator := '.';
    Result := TryStrToFloat(Copy(AJson, Start, P - Start), AValue, Settings);
  end;

  { one name-and-error record of failedTests, as the old runner lists a failed test }
  function ReadFailed: Boolean;
  var
    Key, Name, Error, Text: string;
  begin
    Name := '';
    Error := '';
    if Peek = '"' then
    begin
      { a plain string entry }
      if not ReadString(Text) then
        Exit(False);
    end
    else
    begin
      if not Expect('{') then
        Exit(False);
      if Peek <> '}' then
        repeat
          if not ReadString(Key) or not Expect(':') then
            Exit(False);
          if Peek = '"' then
          begin
            if not ReadString(Text) then
              Exit(False);
            if Key = 'name' then
              Name := Text
            else if Key = 'error' then
              Error := Text;
          end
          else if not ReadString(Text) then
          begin
            { a non-string value: skipped to the next comma or brace }
            while (P <= Length(AJson)) and not (AJson[P] in [',', '}']) do
              Inc(P);
          end;
        until not Expect(',');
      if not Expect('}') then
        Exit(False);
      Text := FailedTestText(Name, Error);
    end;
    SetLength(AResult.FailedTests, Length(AResult.FailedTests) + 1);
    AResult.FailedTests[High(AResult.FailedTests)] := Text;
    Result := True;
  end;

var
  Key: string;
  Num: Double;
begin
  P := 1;
  SetLength(AResult.FailedTests, 0);
  if not Expect('{') then
    Exit(False);
  if Peek <> '}' then
    repeat
      if not ReadString(Key) or not Expect(':') then
        Exit(False);
      if Key = 'failedTests' then
      begin
        if not Expect('[') then
          Exit(False);
        if Peek <> ']' then
          repeat
            if not ReadFailed then
              Exit(False);
          until not Expect(',');
        if not Expect(']') then
          Exit(False);
      end
      else
      begin
        if not ReadNumber(Num) then
          Exit(False);
        if Key = 'totalTests' then
          AResult.TotalTests := Round(Num)
        else if Key = 'passed' then
          AResult.Passed := Round(Num)
        else if Key = 'failed' then
          AResult.Failed := Round(Num)
        else if Key = 'skipped' then
          AResult.Skipped := Round(Num)
        else if Key = 'totalRunTests' then
          AResult.TotalRunTests := Round(Num)
        else if Key = 'assertions' then
          AResult.Assertions := Round(Num)
        else if Key = 'suiteErrors' then
          AResult.SuiteErrors := Round(Num)
        else if Key = 'duration' then
          AResult.DurationNanoseconds := Round(Num);
      end;
    until not Expect(',');
  Result := Expect('}');
end;

{ a file's engine before its code runs: its settings, globals and testing API. A heap snapshot's restoring
  engine is set up the same way }
procedure TG2TestRunnerApp.SetUpTestEngine(AEngine: TG2Engine; const C: TG2FileContext;
  APrintWarnings: Boolean);
var
  Err: UnicodeString;
  SetupError: string;
  HostError: TG2RunResult;
begin
  AEngine.Capabilities := C.Capabilities;
  AEngine.ProjectRoot := DiscoverProjectRoot(C.FileName);
  AEngine.Coverage := Assigned(FCoverageMain);
  AEngine.ConfigJson := HostConfigJson(EngineOptions, C.FileConfig);
  { --no-vitest-compat: the bare "vitest" specifier resolves to no shim }
  if FNoVitestCompat.Present then
    if AEngine.ConfigJson = '{}' then
      AEngine.ConfigJson := '{"no-vitest-compat":"true"}'
    else
      AEngine.ConfigJson := Copy(AEngine.ConfigJson, 1, Length(AEngine.ConfigJson) - 1) +
        ',"no-vitest-compat":"true"}';
  AEngine.Resolver := C.Resolver;
  AEngine.ModuleContentCheck := C.Grant.CheckContent;
  AEngine.ProviderGraph := C.Grant.ProviderGraph;
  AEngine.TestMode := True;
  { a rejection the file's top level leaves fails the file before its tests run }
  AEngine.FailOnUnhandledRejection := True;
  { the console of a parallel run, or under --silent or a JSON report, is not printed }
  C.Capture.Capturing := IsJsonOutput;
  C.Capture.Silent := FSilent.Present or FParallel;
  if LogFileOpen then
    C.Capture.LogLine := HandleConsoleLog;
  AEngine.OnConsole := C.Capture.Emit;
  { parse warnings, the file's and its imports', print in a text run one file at a time, as GocciaTestRunner's
    (#2038): never ahead of a JSON report, and never from parallel files, whose lines would interleave }
  if APrintWarnings and not IsJsonOutput and not FParallel then
  begin
    AEngine.OnWarning := C.Capture.PrintWarning;
    AEngine.OnModuleWarning := C.Capture.PrintModuleWarning;
  end;
  AEngine.Deterministic := ResolveFlagOption(EngineOptions.Deterministic, C.FileConfig);
  if AuditLogOpen then
  begin
    AEngine.AuditSink := HandleCapabilityAudit;
    C.Grant.Engine := AEngine;
  end;
  ApplySettings(AEngine, ResolveFileSettings(EngineOptions, C.FileConfig, C.FileName, C.Verdict.AcceptedUnsafe));
  if EngineOptions.Timeout.Present then
    AEngine.TimeoutMs := EngineOptions.Timeout.Value
  else
    AEngine.TimeoutMs := DEFAULT_TIMEOUT_MS;
  if EngineOptions.MaxInstructions.Present then
    AEngine.MaxInstructions := EngineOptions.MaxInstructions.Value;
  if EngineOptions.MaxStack.Present then
    AEngine.MaxFrames := Integer(EngineOptions.MaxStack.Value);
  Err := '';
  TestTimeoutMs := FTestTimeout.ValueOr(0);
  DescribeTimeoutMs := FDescribeTimeout.ValueOr(0);
  TestReporterOn := not IsJsonOutput;
  { the file's snapshots: its .snap file and its inline edits, flushed once every file has run }
  SetTestSnapshots(TGocciaTestRunnerSnapshotHost.Create(C.FileName), SnapshotUpdateMode);
  if not AEngine.PrepareHost(Err) then
    raise Exception.Create('test host setup failed: ' + UTF8Encode(Err));
  { the file's virtual modules and globals, before its code runs (a failure fails the file) }
  if not DefineVirtualModules(AEngine, RootConfigPath, C.FileConfigPath, EngineOptions, SetupError) or
     not InjectGlobals(AEngine, ConfigNamedEntries(FGlobalFiles), FGlobalFiles, FInlineGlobals, SetupError) then
  begin
    { an error the host threw keeps its name (a refused read's PermissionDenied) }
    if AEngine.DescribeHostError(HostError) and HostError.ErrorIsError and (HostError.ErrorName <> '') then
      SetupError := UTF8Encode(HostError.ErrorName) + ': ' + UTF8Encode(HostError.ErrorText);
    raise Exception.Create(SetupError);
  end;
  { the testing API after the globals, which load before it (a globals module sees no test global) }
  if not InstallTestingAPI(Err) then
    raise Exception.Create('test host setup failed: ' + UTF8Encode(Err));
end;

{ --heap-snapshots: one object the file's engine wrote, restored in a fresh engine set up as the file's was }
function TG2TestRunnerApp.VerifyHeapRoot(const C: TG2FileContext; const ARoot: TG2HeapRoot;
  const ASource: UnicodeString; out ADetail: UnicodeString): TG2HeapVerdict;
var Dest: TG2Engine;
begin
  Dest := TG2Engine.Create;
  try
    { the same testing API as the file's engine, its observe native included }
    Dest.HeapSnapshots := True;
    SetUpTestEngine(Dest, C, False);
    Dest.Coverage := False;
    Result := Dest.HeapSnapshotVerify(ARoot, C.FileName, ASource, ADetail);
  finally
    TestSnapshotsDone;
    Dest.Free;
  end;
end;

procedure TG2TestRunnerApp.RunTestFile(AIndex: Integer; ASource: TStringList);
var
  FileName, FileConfigPath, AliasBaseDirectory: string;
  FileConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
  Capabilities: TGocciaCapabilities;
  Grant: TG2ImportGrant;
  Resolver: TGocciaModuleResolver;
  Engine: TG2Engine;
  Capture: TG2ConsoleCapture;
  RunResult: TG2RunResult;
  Err, Json, SrcText: UnicodeString;
  Res: TG2TestFileResult;
  Start, SuitesStart: Int64;
  Aborted: TG2Abort;
  Late, SetupError: string;
  MemoryStart: TG2MemoryStats;
  HeapStart: THeapStatus;
  HostError: TG2RunResult;
  HeapRoots: TArray<TG2HeapRoot>;

  { the file's engine before its code runs: its settings, globals and testing API. A heap snapshot's restoring
    engine is set up the same way }
  function Context: TG2FileContext;
  begin
    Result.FileName := FileName;
    Result.FileConfigPath := FileConfigPath;
    Result.FileConfig := FileConfig;
    Result.Verdict := Verdict;
    Result.Capabilities := Capabilities;
    Result.Grant := Grant;
    Result.Resolver := Resolver;
    Result.Capture := Capture;
  end;

  procedure SetUpEngine(AEngine: TG2Engine);
  begin
    SetUpTestEngine(AEngine, Context, True);
  end;

  { --heap-snapshots: each object the source engine wrote, restored in a fresh engine of its own }
  procedure VerifyHeapRoots;
  var I: Integer; Ctx: TG2FileContext; Verdict_: TG2HeapVerdict;
    Detail: UnicodeString;
  begin
    Ctx := Context;
    for I := 0 to High(HeapRoots) do
    begin
      if HeapRoots[I].Refusal <> '' then
      begin
        Inc(Res.HeapRefused);
        SetLength(Res.HeapRefusals, Length(Res.HeapRefusals) + 1);
        Res.HeapRefusals[High(Res.HeapRefusals)] := UTF8Encode(HeapRoots[I].Refusal);
        Continue;
      end;
      { a fresh engine of its own, on this thread now that the file's engine is gone }
      try
        Verdict_ := VerifyHeapRoot(Ctx, HeapRoots[I], SrcText, Detail);
      except
        on E: Exception do
        begin
          Verdict_ := g2hvFailed;
          Detail := 'the restoring engine raised ' + UnicodeString(E.ClassName) + ': ' + UnicodeString(E.Message);
        end;
      end;
      case Verdict_ of
        g2hvPassed: Inc(Res.HeapPassed);
        g2hvRefused:
          begin
            Inc(Res.HeapRefused);
            SetLength(Res.HeapRefusals, Length(Res.HeapRefusals) + 1);
            Res.HeapRefusals[High(Res.HeapRefusals)] := UTF8Encode(Detail);
          end;
      else
        Inc(Res.HeapFailed);
        SetLength(Res.HeapFailures, Length(Res.HeapFailures) + 1);
        Res.HeapFailures[High(Res.HeapFailures)] := UTF8Encode(Detail);
      end;
    end;
  end;

begin
  FileName := FResults[AIndex].FileName;
  Res := FResults[AIndex];
  Res.Ran := True;
  Start := GetNanoseconds;
  Capture := TG2ConsoleCapture.Create;
  Capture.FileName := FileName;
  Grant := nil;
  Resolver := nil;
  try
    FileConfigPath := DiscoverFileConfigPath(FileName);
    if FileConfigPath <> '' then
      FileConfig := LoadFileConfig(FileConfigPath)
    else
      SetLength(FileConfig, 0);
    Verdict := FileConfigVerdict(FileConfigPath, FileName);
    Capabilities := ResolveEngineCapabilities(FileConfigPath, FileName);
    Grant := TG2ImportGrant.Create(Capabilities);
    Resolver := TGocciaModuleResolver.Create(ExtractFilePath(ExpandFileName(FileName)));
    Grant.Resolver := Resolver;
    Resolver.SetExtensions(ModuleImportExtensions);
    Resolver.NodeModulesGrant := Grant.Grant;
    Resolver.ProviderGrant := Grant.GrantProvider;
    Resolver.ProviderAudit := Grant.AuditProvider;
    Resolver.ProviderCachedOnly := ResolveFlagOption(EngineOptions.CachedOnly, FileConfig);
    if EngineOptions.Aliases.FromCommandLine or (RootConfigPath = '') then
      AliasBaseDirectory := GetCurrentDir
    else
      AliasBaseDirectory := ExtractFilePath(RootConfigPath);
    ConfigureModuleResolver(Resolver, FileName, EngineOptions.ImportMap.ValueOr(''),
      EngineOptions.Aliases.Values, AliasBaseDirectory);
    { --coverage: this thread's tracker, the file registered with its executable lines in the run's }
    if Assigned(FCoverageMain) then
    begin
      TGocciaCoverageTracker.Initialize;
      TGocciaCoverageTracker.Instance.Enabled := True;
      EnterCriticalSection(FCoverageLock);
      try
        FCoverageMain.RegisterSourceFile(FileName, CountExecutableLines(ASource));
      finally
        LeaveCriticalSection(FCoverageLock);
      end;
    end;
    HeapStart := GetHeapStatus;
    Engine := TG2Engine.Create;
    try
      MemoryStart := Engine.Memory;
      Engine.HeapSnapshots := FHeapSnapshots.Present;
      SetUpEngine(Engine);
      SrcText := UTF8Decode(ASource.Text);
      HoistTestMocks(FileName, SrcText);
      RunResult := Engine.Run(FileName, SrcText);
      Res.CompileNanoseconds := RunResult.CompileNanoseconds;
      if RunResult.Ok then
        Err := ''
      else
      begin
        Err := RunResult.ErrorMessage;
        { a file that threw is reported as GocciaRunner reports it: the error, its suggestion and a code frame }
        if not IsJsonOutput and (RunResult.Aborted = g2aNone) then
        begin
          WriteLn(ErrOutput, FailureText(FileName, RunResult, ASource, Engine, '', nil, True, True, False,
            Engine.TimeoutMs, EngineOptions.MaxInstructions.ValueOr(0)));
          Res.Reported := True;
        end;
      end;
      Json := '';
      SuitesStart := GetNanoseconds;
      { the file's limits (its --timeout, 30 s by default) cover its suites too }
      if RunResult.Aborted = g2aTimeout then
        Err := Format('file timed out after %dms', [Engine.TimeoutMs]);
      if Err = '' then
      begin
        if not Engine.RunTests(Json, Err, Aborted) and (Err = '') then
          Err := 'the test suites did not finish';
        if Aborted = g2aTimeout then
          Err := Format('file timed out after %dms', [Engine.TimeoutMs]);
      end
      else
        Engine.EndTests;
      Res.DurationNanoseconds := GetNanoseconds - SuitesStart;
      { the objects expect() was given, written and described while the engine is idle after the tests }
      if FHeapSnapshots.Present and (Err = '') then
      begin
        HeapRoots := Engine.HeapSnapshotObserved(Res.HeapObservations);
        Res.HeapObjects := Length(HeapRoots);
      end;
      Res.ExecuteNanoseconds := GetNanoseconds - Start - Res.CompileNanoseconds;
      if Err <> '' then
      begin
        { a file that threw, or whose suites did not finish, is one failed test (GocciaTestRunner MarkLoadError) }
        Res.ErrorMessage := UTF8Encode(Err);
        Res.Failed := 1;
        Res.TotalRunTests := 1;
        SetLength(Res.FailedTests, 1);
        Res.FailedTests[0] := FileName + ': ' + Res.ErrorMessage;
      end
      else if not ReadSuiteReport(UTF8Encode(Json), Res) then
      begin
        Res.ErrorMessage := 'the test report could not be read';
        Res.SuiteErrors := 1;
      end
      else
      begin
        { a rejection the work after the last test left fails the file }
        Late := UTF8Encode(Engine.LateRejection);
        if Late <> '' then
        begin
          Res.ErrorMessage := Late;
          Inc(Res.Failed);
          Inc(Res.TotalRunTests);
          SetLength(Res.FailedTests, Length(Res.FailedTests) + 1);
          Res.FailedTests[High(Res.FailedTests)] := FileName + ': ' + Late;
        end;
      end;
    finally
      Res.Memory := FileMemoryStats(MemoryStart, Engine.Memory, HeapStart, GetHeapStatus);
      TestSnapshotsDone;
      Engine.Free;
      Engine := nil;
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
    if Length(HeapRoots) > 0 then
    begin
      VerifyHeapRoots;
      { after the tests: their counts stand, and the file gets one error (GocciaTestRunner's) }
      if Res.HeapFailed > 0 then
      begin
        Inc(Res.SuiteErrors);
        SetLength(Res.FailedTests, Length(Res.FailedTests) + 1);
        Res.FailedTests[High(Res.FailedTests)] := FileName + ': Heap snapshot round-trip failed';
        if Res.ErrorMessage = '' then Res.ErrorMessage := 'Heap snapshot round-trip failed';
      end;
    end;
  except
    on E: TCLIUsageError do
      raise;
    on E: Exception do
    begin
      { a file the host could not set up is one failed file (GocciaTestRunner RunScriptFromFile) }
      Res.ErrorMessage := E.Message;
      Res.Failed := 1;
      Res.TotalRunTests := 1;
      SetLength(Res.FailedTests, 1);
      Res.FailedTests[0] := FileName + ': ' + E.Message;
    end;
  end;
  Resolver.Free;
  Grant.Free;
  Capture.Free;
  FResults[AIndex] := Res;
end;

{ ---- the run ---- }

{ each file on a thread of its own, at most the job count at once (GetJobCount: --jobs, else the processors, at
  most one per file); results in file order. --exit-on-first-failure stops starting files after a failing one. }
procedure TG2TestRunnerApp.RunFiles(const AFiles: TStringList; const AStdinSource: TStringList);
var
  I, Next, Oldest, Started, Running: Integer;
  Workers: array of TG2TestThread;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
  Stop: Boolean;

  function Failing(AIndex: Integer): Boolean;
  begin
    Result := (FResults[AIndex].Failed > 0) or (FResults[AIndex].SuiteErrors > 0);
  end;

  procedure Progress(AIndex: Integer);
  begin
    if not FNoProgress.Present and not IsJsonOutput then
      WriteLn(Format('[%d/%d] %s', [AIndex + 1, AFiles.Count, AFiles[AIndex]]));
  end;

begin
  SetLength(FResults, AFiles.Count);
  for I := 0 to AFiles.Count - 1 do
  begin
    FResults[I] := Default(TG2TestFileResult);
    FResults[I].FileName := AFiles[I];
  end;
  SetLength(Workers, AFiles.Count);
  ErrorClass := nil;
  ErrorMessage := '';
  Running := 0;
  Oldest := 0;
  Started := 0;
  Stop := False;
  try
    for Next := 0 to AFiles.Count - 1 do
    begin
      if Running = FJobCount then
      begin
        Workers[Oldest].WaitFor;
        if FExitOnFirst.Present and Failing(Oldest) then
          Stop := True;
        Dec(Running);
        Inc(Oldest);
      end;
      if Stop then
        Break;
      { a sequential run prints each file's progress line before it runs, so its console output follows it }
      if not FParallel then
        Progress(Next);
      Workers[Next] := TG2TestThread.Create(True, FILE_THREAD_STACK);
      Workers[Next].FApp := Self;
      Workers[Next].FIndex := Next;
      if (Next = 0) and Assigned(AStdinSource) then
        Workers[Next].FSource := AStdinSource
      else
      begin
        Workers[Next].FSource := SourceRegistry.Load(AFiles[Next]);
        Workers[Next].FOwnsSource := True;
      end;
      Workers[Next].Start;
      Inc(Started);
      Inc(Running);
    end;
  finally
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
  { a parallel run prints the files' progress and errors in file order once they are done }
  for I := 0 to Started - 1 do
  begin
    if FParallel then
      Progress(I);
    if (FResults[I].ErrorMessage <> '') and not FResults[I].Reported then
      WriteLn(ErrOutput, FResults[I].ErrorMessage);
  end;
  { the files after the one that stopped the run never ran: no results row }
  SetLength(FResults, Started);
end;

procedure TG2TestRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  Files, RawFiles, Discovered, StdinNames, Expanded: TStringList;
  StdinSource: TStringList;
  UseStdin: Boolean;
  I: Integer;
  Start: Int64;
  Timing: TCLIJSONTiming;
  MemoryStats: TCLIJSONMemoryStats;
  FileMemory: array of TCLIJSONMemoryStats;
  HeapStart: THeapStatus;
begin
  { stdin when there are no paths or the only one is "-"; "-" among others is refused }
  UseStdin := (APaths.Count = 0) or ((APaths.Count = 1) and IsStdinPath(APaths[0]));
  if not UseStdin and (APaths.Count > 1) then
    for I := 0 to APaths.Count - 1 do
      if IsStdinPath(APaths[I]) then
      begin
        WriteLn(ErrOutput, 'Error: stdin is supported only as the sole input.');
        ExitCode := 1;
        Exit;
      end;
  Files := TStringList.Create;
  StdinSource := nil;
  try
    if UseStdin then
    begin
      ValidateFileConfig(STDIN_FILE_NAME);
      StdinSource := ReadSourceFromText(Input);
      if MultifileEnabled then
      begin
        StdinNames := SplitStdinMultifile(StdinSource);
        try
          Files.AddStrings(StdinNames);
        finally
          StdinNames.Free;
        end;
        StdinSource := nil;
      end
      else
        Files.Add(STDIN_FILE_NAME);
    end
    else
    begin
      RawFiles := TStringList.Create;
      try
        for I := 0 to APaths.Count - 1 do
          if DirectoryExists(APaths[I]) then
          begin
            Discovered := FindAllFilesExcludingDirectories(APaths[I], ScriptExtensions,
              TEST_DISCOVERY_EXCLUDED_DIRECTORIES);
            try
              RawFiles.AddStrings(Discovered);
            finally
              Discovered.Free;
            end;
          end
          else if FileExists(APaths[I]) then
            RawFiles.Add(APaths[I])
          else
          begin
            WriteLn(ErrOutput, 'Error: Path not found: ', APaths[I]);
            ExitCode := 1;
            Exit;
          end;
        Expanded := ExpandMultifileFiles(RawFiles);
        Files.Free;
        Files := Expanded;
      finally
        RawFiles.Free;
      end;
    end;

    FJobCount := GetJobCount(Files.Count);
    if FHeapSnapshots.Present then
      FJobCount := 1;
    if FJobCount < 1 then
      FJobCount := 1;
    FParallel := (Files.Count > 1) and (FJobCount > 1);
    if not FNoProgress.Present and not IsJsonOutput then
    begin
      if FJobCount > 1 then
        WriteLn(Format('Running %d files with %d workers', [Files.Count, FJobCount]))
      else
        WriteLn(Format('Running %d files', [Files.Count]));
    end;
    FCoverageMain := TGocciaCoverageTracker.Instance;
    if Assigned(FCoverageMain) and not FCoverageMain.Enabled then
      FCoverageMain := nil;
    if Assigned(FCoverageMain) then
      InitCriticalSection(FCoverageLock);
    HeapStart := GetHeapStatus;
    Start := GetNanoseconds;
    ResetPendingInlineSnapshots;
    try
      RunFiles(Files, StdinSource);
    except
      DiscardPendingInlineSnapshots;
      raise;
    end;
    StdinSource := nil;
    { the inline snapshots every file queued, written into their sources once none of them runs any more }
    try
      FlushPendingInlineSnapshots;
    except
      on E: Exception do
        RecordSnapshotFinalizationFailure('Snapshot finalization failed: ' + E.Message);
    end;
    { the files' engines ran on threads of their own: the report sums their memory }
    SetLength(FileMemory, Length(FResults));
    for I := 0 to High(FResults) do
      FileMemory[I] := FResults[I].Memory;
    MemoryStats := AggregateMemoryStats(FileMemory, FParallel, HeapStart, GetHeapStatus, FJobCount);
    Timing := Default(TCLIJSONTiming);
    for I := 0 to High(FResults) do
    begin
      Inc(Timing.CompileTimeNanoseconds, FResults[I].CompileNanoseconds);
      Inc(Timing.ExecuteTimeNanoseconds, FResults[I].ExecuteNanoseconds);
    end;
    Timing.TotalTimeNanoseconds := Timing.CompileTimeNanoseconds + Timing.ExecuteTimeNanoseconds;
    if Timing.TotalTimeNanoseconds = 0 then
      Timing.TotalTimeNanoseconds := GetNanoseconds - Start;
    PrintResults(Timing, MemoryStats);
    if FHeapSnapshots.Present then
      PrintHeapSnapshotReport;
    if Assigned(FCoverageMain) then
    begin
      ReportCoverage(Files);
      DoneCriticalSection(FCoverageLock);
      FCoverageMain := nil;
    end;
  finally
    Files.Free;
    StdinSource.Free;
  end;
end;

{ ---- reports ---- }

{ --heap-snapshots on stderr, as GocciaTestRunner reports it: each failed round trip, the counts, then each
  refusal reason with how often it came, in sorted order }
procedure TG2TestRunnerApp.PrintHeapSnapshotReport;
var
  I, J, Observations, Objects, Passed, Refused, Failed: Integer;
  Reasons: TStringList;
  Count: Integer;
begin
  Observations := 0;
  Objects := 0;
  Passed := 0;
  Refused := 0;
  Failed := 0;
  Reasons := TStringList.Create;
  try
    Reasons.Sorted := True;
    Reasons.Duplicates := dupIgnore;
    Reasons.CaseSensitive := True;
    for I := 0 to High(FResults) do
    begin
      Inc(Observations, FResults[I].HeapObservations);
      Inc(Objects, FResults[I].HeapObjects);
      Inc(Passed, FResults[I].HeapPassed);
      Inc(Refused, FResults[I].HeapRefused);
      Inc(Failed, FResults[I].HeapFailed);
      for J := 0 to High(FResults[I].HeapFailures) do
        WriteLn(ErrOutput, 'Heap snapshot failure: ', FResults[I].FileName, ': ', FResults[I].HeapFailures[J]);
      for J := 0 to High(FResults[I].HeapRefusals) do
      begin
        if not Reasons.Find(FResults[I].HeapRefusals[J], Count) then
          Count := Reasons.AddObject(FResults[I].HeapRefusals[J], TObject(PtrInt(0)));
        Reasons.Objects[Count] := TObject(PtrInt(Reasons.Objects[Count]) + 1);
      end;
    end;
    WriteLn(ErrOutput, Format('Heap snapshots: observations=%d objects=%d passed=%d refused=%d failed=%d',
      [Observations, Objects, Passed, Refused, Failed]));
    for I := 0 to Reasons.Count - 1 do
      WriteLn(ErrOutput, 'Heap snapshot refusal: ', PtrInt(Reasons.Objects[I]), ' ', Reasons[I]);
  finally
    Reasons.Free;
  end;
end;

{ GocciaTestRunner.dpr ExecuteWithPaths' coverage: the summary, and one file's detail (a multifile section's or
  stdin's from the source registry), on stdout unless it carries a JSON envelope; the lcov or JSON file }
procedure TG2TestRunnerApp.ReportCoverage(const AFiles: TStringList);
var
  Source: TStringList;
begin
  if not IsJsonOutput then
  begin
    PrintCoverageSummary(FCoverageMain);
    if AFiles.Count = 1 then
    begin
      if SourceRegistry.IsRegistered(AFiles[0]) then
      begin
        Source := SourceRegistry.Load(AFiles[0]);
        try
          PrintCoverageDetail(FCoverageMain, AFiles[0], Source);
        finally
          Source.Free;
        end;
      end
      else
        PrintCoverageDetail(FCoverageMain, AFiles[0]);
    end;
  end;
  if CoverageOptions.Format.Matches(cfLcov) and (CoverageOptions.OutputPath.ValueOr('') <> '') then
    WriteCoverageLcov(FCoverageMain, CoverageOptions.OutputPath.Value);
  if CoverageOptions.Format.Matches(cfJson) and (CoverageOptions.OutputPath.ValueOr('') <> '') then
    WriteCoverageJSON(FCoverageMain, CoverageOptions.OutputPath.Value);
end;

function JoinFailedTests(const AResults: array of TG2TestFileResult): string;
var
  I, J: Integer;
begin
  Result := '';
  for I := 0 to High(AResults) do
    for J := 0 to High(AResults[I].FailedTests) do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + AResults[I].FailedTests[J];
    end;
end;

{ GocciaTestRunner.dpr PrintTestResults }
procedure TG2TestRunnerApp.PrintResults(const ATiming: TCLIJSONTiming; const AMemory: TCLIJSONMemoryStats);
var
  I, Passed, Failed, Skipped, SuiteErrors, RunTests, Assertions: Integer;
  Duration, PerTest: Int64;
begin
  Passed := 0;
  Failed := 0;
  Skipped := 0;
  SuiteErrors := 0;
  RunTests := 0;
  Assertions := 0;
  Duration := 0;
  for I := 0 to High(FResults) do
  begin
    Inc(Passed, FResults[I].Passed);
    Inc(Failed, FResults[I].Failed);
    Inc(Skipped, FResults[I].Skipped);
    Inc(SuiteErrors, FResults[I].SuiteErrors);
    Inc(RunTests, FResults[I].TotalRunTests);
    Inc(Assertions, FResults[I].Assertions);
    Inc(Duration, FResults[I].DurationNanoseconds);
  end;
  if FinalizationUnattributed then
  begin
    Inc(Failed);
    Inc(RunTests);
  end;
  if not FNoResults.Present and not IsJsonOutput then
  begin
    WriteLn('Test Results Test Files: ', Length(FResults));
    WriteLn(Format('Test Results Run Tests: %d', [RunTests]));
    if RunTests > 0 then
    begin
      PerTest := Round(Duration / RunTests);
      WriteLn(Format('Test Results Passed: %d (%2.2f%%)', [Passed, Passed / RunTests * 100]));
      WriteLn(Format('Test Results Failed: %d (%2.2f%%)', [Failed, Failed / RunTests * 100]));
      WriteLn(Format('Test Results Skipped: %d (%2.2f%%)', [Skipped, Skipped / RunTests * 100]));
      WriteLn(Format('Test Results Assertions: %d', [Assertions]));
      WriteLn(Format('Test Results Test Duration: %s (%s/test)', [FormatDuration(Duration),
        FormatDuration(PerTest)]));
      if FJobCount > 1 then
      begin
        WriteLn(Format('Test Results Engine Timing (cumulative): Lex: (in Compile) | Parse: (in Compile) | ' +
          'Compile: %s | Execute: %s | Total: %s', [FormatDuration(ATiming.CompileTimeNanoseconds),
          FormatDuration(ATiming.ExecuteTimeNanoseconds), FormatDuration(ATiming.TotalTimeNanoseconds)]));
        WriteLn(Format('Test Results Engine Timing (avg/worker): Lex: (in Compile) | Parse: (in Compile) | ' +
          'Compile: %s | Execute: %s | Total: %s', [FormatDuration(ATiming.CompileTimeNanoseconds div FJobCount),
          FormatDuration(ATiming.ExecuteTimeNanoseconds div FJobCount),
          FormatDuration(ATiming.TotalTimeNanoseconds div FJobCount)]));
      end
      else
        WriteLn(Format('Test Results Engine Timing: Lex: (in Compile) | Parse: (in Compile) | Compile: %s | ' +
          'Execute: %s | Total: %s', [FormatDuration(ATiming.CompileTimeNanoseconds),
          FormatDuration(ATiming.ExecuteTimeNanoseconds), FormatDuration(ATiming.TotalTimeNanoseconds)]));
      if FinalizationUnattributed then
        if JoinFailedTests(FResults) = '' then
          WriteLn('Test Results Failed Tests: ', FFinalizationError)
        else
          WriteLn('Test Results Failed Tests: ', JoinFailedTests(FResults), ',', FFinalizationError)
      else
        WriteLn('Test Results Failed Tests: ', JoinFailedTests(FResults));
    end;
  end;
  if IsJsonOutput then
    WriteResultsJSON('', IsCompactJsonOutput, ATiming, AMemory)
  else if FOutputFile.ValueOr('') <> '' then
    WriteResultsJSON(FOutputFile.Value, False, ATiming, AMemory);
  { suite errors are not failed tests, but they fail the run as they fail the envelope's ok }
  if (Failed > 0) or (SuiteErrors > 0) then
    ExitCode := 1
  else
    ExitCode := 0;
end;

{ GocciaTestRunner.dpr WriteResultsJSON and WriteFileResults }
procedure TG2TestRunnerApp.WriteResultsJSON(const AFileName: string; ACompact: Boolean;
  const ATiming: TCLIJSONTiming; const AMemory: TCLIJSONMemoryStats);
var
  Lines: TStringList;
  I, Passed, Failed, Skipped, SuiteErrors, RunTests, Assertions: Integer;
  Duration: Int64;
  ErrorInfo: TCLIJSONErrorInfo;

  procedure WriteFiles(const AName: string; ATrailingComma: Boolean);
  var
    J, K: Integer;
    Entry, FailedList, ErrorJSON: string;
    Timing: TCLIJSONTiming;
    ErrorInfo: TCLIJSONErrorInfo;
  begin
    Lines.Add(Format('  "%s": [', [AName]));
    for K := 0 to High(FResults) do
    begin
      FailedList := '';
      for J := 0 to High(FResults[K].FailedTests) do
      begin
        if J > 0 then
          FailedList := FailedList + ',';
        FailedList := FailedList + '"' + EscapeJSONString(FResults[K].FailedTests[J]) + '"';
      end;
      Timing := Default(TCLIJSONTiming);
      Timing.CompileTimeNanoseconds := FResults[K].CompileNanoseconds;
      Timing.ExecuteTimeNanoseconds := FResults[K].ExecuteNanoseconds;
      Timing.TotalTimeNanoseconds := Timing.CompileTimeNanoseconds + Timing.ExecuteTimeNanoseconds;
      if FResults[K].ErrorMessage = '' then
        ErrorJSON := 'null'
      else
      begin
        ErrorInfo := DefaultCLIJSONErrorInfo;
        ErrorInfo.Message := FResults[K].ErrorMessage;
        ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
      end;
      Entry := '    {' + BuildCLIFileBaseJSON(FResults[K].FileName,
        (FResults[K].Failed = 0) and (FResults[K].ErrorMessage = '') and (FResults[K].SuiteErrors = 0),
        '', '', '', ErrorJSON, Timing, '"memory":null', ACompact) +
        Format(', "passed": %d, "failed": %d, "skipped": %d, "suiteErrors": %d, "totalTests": %d, ',
        [FResults[K].Passed, FResults[K].Failed, FResults[K].Skipped, FResults[K].SuiteErrors,
         FResults[K].TotalRunTests]) +
        '"errorMessage": "' + EscapeJSONString(FResults[K].ErrorMessage) + '", "failedTests": [' + FailedList +
        ']}';
      if K < High(FResults) then
        Entry := Entry + ',';
      Lines.Add(Entry);
    end;
    if ATrailingComma then
      Lines.Add('  ],')
    else
      Lines.Add('  ]');
  end;

begin
  Passed := 0;
  Failed := 0;
  Skipped := 0;
  SuiteErrors := 0;
  RunTests := 0;
  Assertions := 0;
  Duration := 0;
  for I := 0 to High(FResults) do
  begin
    Inc(Passed, FResults[I].Passed);
    Inc(Failed, FResults[I].Failed);
    Inc(Skipped, FResults[I].Skipped);
    Inc(SuiteErrors, FResults[I].SuiteErrors);
    Inc(RunTests, FResults[I].TotalRunTests);
    Inc(Assertions, FResults[I].Assertions);
    Inc(Duration, FResults[I].DurationNanoseconds);
  end;
  if FinalizationUnattributed then
  begin
    Inc(Failed);
    Inc(RunTests);
  end;
  Lines := TStringList.Create;
  try
    Lines.Add('{');
    if not ACompact then
      Lines.Add('  ' + BuildCLIBuildJSON + ',');
    Lines.Add(Format('  "ok": %s,', [IfThen((Failed = 0) and (SuiteErrors = 0), 'true', 'false')]));
    if not ACompact then
    begin
      Lines.Add('  "stdout": "",');
      Lines.Add('  "stderr": "",');
    end;
    Lines.Add('  ' + BuildCLIOutputJSON('') + ',');
    if FFinalizationError = '' then
      Lines.Add('  "error": null,')
    else
    begin
      ErrorInfo := DefaultCLIJSONErrorInfo;
      ErrorInfo.Message := FFinalizationError;
      Lines.Add('  "error": ' + BuildCLIErrorObjectJSON(ErrorInfo) + ',');
    end;
    Lines.Add(Format('  "jobCount": %d,', [FJobCount]));
    Lines.Add(Format('  "totalFiles": %d,', [Length(FResults)]));
    Lines.Add(Format('  "totalTests": %d,', [RunTests]));
    Lines.Add(Format('  "passed": %d,', [Passed]));
    Lines.Add(Format('  "failed": %d,', [Failed]));
    Lines.Add(Format('  "skipped": %d,', [Skipped]));
    Lines.Add(Format('  "suiteErrors": %d,', [SuiteErrors]));
    Lines.Add(Format('  "assertions": %d,', [Assertions]));
    Lines.Add(Format('  "durationNanoseconds": %d,', [Duration]));
    Lines.Add('  "lexTimeNanoseconds": 0,');
    Lines.Add('  "parseTimeNanoseconds": 0,');
    Lines.Add(Format('  "compileTimeNanoseconds": %d,', [ATiming.CompileTimeNanoseconds]));
    Lines.Add(Format('  "executeTimeNanoseconds": %d,', [ATiming.ExecuteTimeNanoseconds]));
    Lines.Add(Format('  "totalEngineNanoseconds": %d,', [ATiming.TotalTimeNanoseconds]));
    Lines.Add('  ' + BuildCLITimingJSON(ATiming) + ',');
    if not ACompact then
      Lines.Add('  ' + BuildCLIMemoryJSON(AMemory) + ',');
    Lines.Add('  ' + BuildCLIWorkersJSON(FJobCount, FJobCount) + ',');
    WriteFiles('files', True);
    WriteFiles('results', False);
    Lines.Add('}');
    if AFileName = '' then
    begin
      for I := 0 to Lines.Count - 1 do
        WriteLn(Lines[I]);
    end
    else
      SaveStringsToHostFile(Lines, AFileName);
  finally
    Lines.Free;
  end;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TG2TestRunnerApp, 'GocciaTestRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
