program GocciaBenchmarkRunner;

{ GocciaBenchmarkRunner: the command line, configs, trust and capability policy
  (the base class), each benchmark file run by a TGocciaEngine in benchmark mode
  (Goccia.Embed BenchMode, goccia:microbench from prelude/microbench.js) on a
  thread of its own, its benchmarks measured by Goccia.HostAPI
  RunBenchmarkSuites and reported by Goccia.Benchmark.Reporter.
  `./build.pas benchmarkrunner` builds it as build/GocciaBenchmarkRunner.

  Not yet supported: bytecode files. }

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
  Goccia.Benchmark.Reporter,
  Goccia.CapabilityAudit,
  Goccia.Capabilities,
  Goccia.CLI.Application,
  Goccia.CLI.FileSettings,
  Goccia.CLI.FileEngine,
  Goccia.CLI.JSON.Reporter,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.Trust,
  Goccia.FileExtensions,
  Goccia.Modules.Configuration,
  Goccia.Modules.Resolver,
  Goccia.Profiler,
  Goccia.Profiler.Compile,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Input,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,

  Goccia.Embed,
  Goccia.EngineFault,
  Goccia.HostAPI,
  Goccia.VM,
  Goccia.ThreadCleanupRegistry;

type
  TReportSpec = record
    Format: TBenchmarkReportFormat;
    OutputFile: string;
  end;

  { one file's results, plain data so they cross from its thread }
  TGocciaBenchFileResult = record
    Result: TBenchmarkFileResult;
    { its engine's and the memory manager's counters over the file }
    Memory: TCLIJSONMemoryStats;
  end;

  TGocciaBenchmarkRunnerApp = class(TGocciaCLIApplication)
  private
    FNoProgress: TFlagOption;
    FProfileDeterministic: TFlagOption;
    FFormats: TRepeatableOption;
    FOutputFile: TStringOption;
    FCanPrintProfile: Boolean;
    { the progress lines: before each file when they run one at a time, and before each benchmark }
    FShowProgress: Boolean;
    FJobCount: Integer;
    { more than one file runs at once: their diagnostics are not printed as they happen }
    FParallel: Boolean;
    { the input is stdin: GocciaBenchmarkRunner prints no file progress lines for it }
    FFromStdin: Boolean;
    FResults: array of TGocciaBenchFileResult;
    { --profile: the main thread's profilers, which each file's thread records into (profiled files run one at a
      time) }
    FProfiler: TGocciaProfiler;
    FCompileProfiler: TGocciaCompileProfiler;
    function ProfilingEnabled: Boolean;
    procedure RunBenchmarkFile(AIndex: Integer; ASource: TStringList);
    procedure RunFiles(const AFiles: TStringList; const AStdinSource: TStringList);
    procedure RunAndReport(const AFiles: TStringList; const AStdinSource: TStringList;
      const AReports: array of TReportSpec);
  protected
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    procedure Configure; override;
    procedure Validate; override;
    procedure AfterExecute; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

  { one benchmark file on a thread of its own: an engine's state is its thread's, so each file gets a fresh
    engine }
  TGocciaBenchThread = class(TThread)
  private
    FApp: TGocciaBenchmarkRunnerApp;
    FIndex: Integer;
    FSource: TStringList;
    FOwnsSource: Boolean;
    FErrorClass: ExceptClass;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  end;

var
  ProgressLock: TRTLCriticalSection;

{ a progress line, whole, from any file's thread }
procedure WriteProgressLine(const AMessage: string);
begin
  EnterCriticalSection(ProgressLock);
  try
    WriteLn(AMessage);
    Flush(Output);
  finally
    LeaveCriticalSection(ProgressLock);
  end;
end;

{ Goccia.HostAPI BenchProgress: the benchmark about to be measured, as GocciaBenchmarkRunner prints it }
procedure BenchmarkProgress(const ASuite, AName: UnicodeString; AIndex, ATotal: Integer);
begin
  if ASuite <> '' then
    WriteProgressLine(Format('  [%d/%d] %s > %s', [AIndex, ATotal, UTF8Encode(ASuite), UTF8Encode(AName)]))
  else
    WriteProgressLine(Format('  [%d/%d] %s', [AIndex, ATotal, UTF8Encode(AName)]));
end;

{ a file that did not load or run: one entry, "(fatal)", with why (GocciaBenchmarkRunner AssignErrorFileResult) }
procedure AssignErrorFileResult(var AFileResult: TBenchmarkFileResult; const AFileName, AMessage: string);
begin
  AFileResult := Default(TBenchmarkFileResult);
  AFileResult.FileName := AFileName;
  SetLength(AFileResult.Entries, 1);
  AFileResult.Entries[0].Suite := '';
  AFileResult.Entries[0].Name := '(fatal)';
  AFileResult.Entries[0].Error := AMessage;
end;

{ a benchmark directory's helpers/ folder holds modules the benchmarks import, not benchmarks }
function IsBenchmarkHelperFile(const AFileName: string): Boolean;
begin
  Result := Pos('/helpers/', StringReplace(AFileName, PathDelim, '/', [rfReplaceAll])) > 0;
end;

procedure TGocciaBenchThread.Execute;
var
  OwnsJit: Boolean;
begin
  { a file that would start while another thread reports a fault does not (ADR 0196) }
  StopIfEngineFaultReported;
  try
    OwnsJit := InterlockedCompareExchange(JitOwned, 1, 0) = 0;
    try
      if OwnsJit then
        JitTakeThread;
      try
        FApp.RunBenchmarkFile(FIndex, FSource);
      finally
        if FOwnsSource then
          FSource.Free;
        if OwnsJit then
          InterLockedExchange(JitOwned, 0);
      end;
    except
      on E: Exception do
      begin
        { the run stops here, on this file's thread (ADR 0196) }
        if IsEngineFault(E) then
          AbortOnEngineFault(FApp.FResults[FIndex].Result.FileName, E);
        FErrorClass := ExceptClass(E.ClassType);
        FErrorMessage := E.Message;
      end;
    end;
  finally
    { this thread's engine state: FPC does not finalize managed threadvars }
    RunThreadvarCleanupsOrAbort(FApp.FResults[FIndex].Result.FileName);
  end;
end;

{ ---- options ---- }

function TGocciaBenchmarkRunnerApp.HonoredCapabilities: TGocciaHonoredCapabilities;
begin
  Result := ALL_CAPABILITIES;
end;

{ GocciaBenchmarkRunner.dpr TBenchmarkRunnerApp.Configure }
procedure TGocciaBenchmarkRunnerApp.Configure;
begin
  AddEngineOptions;
  AddProfilerOptions;
  FNoProgress := AddFlag('no-progress', 'Suppress progress output');
  FProfileDeterministic := AddFlag('profile-deterministic',
    'Run each registered benchmark once for deterministic profile capture');
  FFormats := AddRepeatable('format',
    'Output format (console, text, csv, json, compact-json). ' +
    '"compact-json" emits the json envelope without build, memory, stdout, stderr.');
  FOutputFile := AddString('output', 'Output file path (attaches to last --format)');
  FOutputFile.WritesHostFile := True;
end;

{ --profile-format implies --profile=functions, --profile-deterministic --profile=all }
procedure TGocciaBenchmarkRunnerApp.Validate;
begin
  inherited Validate;
  if ProfilerOptions.Format.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('functions');
  if FProfileDeterministic.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('all');
  if ProfilerOptions.OutputPath.Present and not ProfilerOptions.Mode.Present then
    raise TParseError.Create('--profile-output requires --profile=opcodes|functions|all|compile.');
  if ProfilerOptions.Format.Matches(pfFlamegraph) and not ProfilerOptions.OutputPath.Present then
    raise TParseError.Create('--profile-format=flamegraph requires --profile-output=<path>.');
end;

function TGocciaBenchmarkRunnerApp.UsageLine: string;
begin
  Result := '[path...|-] [options]';
end;

function TGocciaBenchmarkRunnerApp.StdinUsage: TGocciaStdinUsage;
begin
  Result := suStdinDefault;
end;

function TGocciaBenchmarkRunnerApp.ProfilingEnabled: Boolean;
begin
  Result := ProfilerOptions.Mode.Present;
end;

{ ---- one file ---- }

procedure TGocciaBenchmarkRunnerApp.RunBenchmarkFile(AIndex: Integer; ASource: TStringList);
var
  FileName, FileConfigPath, AliasBaseDirectory, SetupError: string;
  FileConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
  Capabilities: TGocciaCapabilities;
  Grant: TGocciaImportGrant;
  Resolver: TGocciaModuleResolver;
  Engine: TGocciaEngine;
  Capture: TGocciaConsoleCapture;
  RunResult, HostError, Failure: TGocciaRunResult;
  Err: UnicodeString;
  Entries: TGocciaBenchEntries;
  Total, I: Integer;
  Duration, ExecStart, BenchStart: Int64;
  Aborted: TGocciaAbort;
  Res: TBenchmarkFileResult;
  MemoryStart: TGocciaMemoryStats;
  HeapStart: THeapStatus;
  Deterministic: Boolean;

  { a failure's report: on stderr as GocciaRunner prints it (a run one file at a time), and as the file's one entry,
    code frame included (GocciaBenchmarkRunner's FormatThrowDetail) }
  procedure Fail(const AResult: TGocciaRunResult);
  begin
    if not FParallel then
      WriteLn(ErrOutput, FailureText(FileName, AResult, ASource, Engine, '', nil, True, True, IsColorTerminal,
        Engine.TimeoutMs, EngineOptions.MaxInstructions.ValueOr(0)));
    AssignErrorFileResult(Res, FileName, FailureText(FileName, AResult, ASource, Engine, '', nil, True, True, False,
      Engine.TimeoutMs, EngineOptions.MaxInstructions.ValueOr(0)));
  end;

begin
  FileName := FResults[AIndex].Result.FileName;
  Res := Default(TBenchmarkFileResult);
  Res.FileName := FileName;
  Deterministic := FProfileDeterministic.Present;
  Capture := TGocciaConsoleCapture.Create;
  Capture.FileName := FileName;
  Grant := nil;
  Resolver := nil;
  Engine := nil;
  HeapStart := GetHeapStatus;
  MemoryStart := Default(TGocciaMemoryStats);
  try
    FileConfigPath := DiscoverFileConfigPath(FileName);
    if FileConfigPath <> '' then
      FileConfig := LoadFileConfig(FileConfigPath)
    else
      SetLength(FileConfig, 0);
    Verdict := FileConfigVerdict(FileConfigPath, FileName);
    Capabilities := ResolveEngineCapabilities(FileConfigPath, FileName);
    Grant := TGocciaImportGrant.Create(Capabilities);
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
    Engine := TGocciaEngine.Create;
    try
      MemoryStart := Engine.Memory;
      Engine.Capabilities := Capabilities;
      Engine.ProjectRoot := DiscoverProjectRoot(FileName);
      Engine.ConfigJson := HostConfigJson(EngineOptions, FileConfig);
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
      { GocciaBenchmarkRunner's runtime is the loader's: no timer globals }
      Engine.ExposeTimers := False;
      Engine.BenchMode := True;
      Engine.FailOnUnhandledRejection := True;
      if LogFileOpen then
      begin
        Capture.LogLine := HandleConsoleLog;
        Engine.OnConsole := Capture.Emit;
      end;
      Engine.Deterministic := ResolveFlagOption(EngineOptions.Deterministic, FileConfig);
      if AuditLogOpen then
      begin
        Engine.AuditSink := HandleCapabilityAudit;
        Grant.Engine := Engine;
      end;
      ApplySettings(Engine, ResolveFileSettings(EngineOptions, FileConfig, FileName, Verdict.AcceptedUnsafe));
      if EngineOptions.Timeout.Present then
        Engine.TimeoutMs := EngineOptions.Timeout.Value;
      if EngineOptions.MaxInstructions.Present then
        Engine.MaxInstructions := EngineOptions.MaxInstructions.Value;
      if EngineOptions.MaxStack.Present then
        Engine.MaxFrames := Integer(EngineOptions.MaxStack.Value);
      Err := '';
      if FShowProgress then
        BenchProgress := @BenchmarkProgress
      else
        BenchProgress := nil;
      if not Engine.PrepareHost(Err) then
        raise Exception.Create('benchmark host setup failed: ' + UTF8Encode(Err));
      { the file's virtual modules, before its code runs (a failure fails the file) }
      if not DefineVirtualModules(Engine, RootConfigPath, FileConfigPath, EngineOptions, SetupError) then
      begin
        { an error the host threw keeps its name (a refused read's PermissionDenied) }
        if Engine.DescribeHostError(HostError) and HostError.ErrorIsError and (HostError.ErrorName <> '') then
          SetupError := UTF8Encode(HostError.ErrorName) + ': ' + UTF8Encode(HostError.ErrorText);
        raise Exception.Create(SetupError);
      end;
      if not InstallBenchmarkAPI(Err) then
        raise Exception.Create('benchmark host setup failed: ' + UTF8Encode(Err));
      ExecStart := GetNanoseconds;
      RunResult := Engine.Run(FileName, StringListToEngineSourceText(ASource));
      Res.CompileTimeNanoseconds := RunResult.CompileNanoseconds;
      Res.DeterministicProfile := Deterministic;
      if not RunResult.Ok then
      begin
        Engine.EndTests;
        Fail(RunResult);
      end
      else
      begin
        { a deterministic profile counts the benchmarks' one run each, not the file's set-up }
        if Deterministic and Assigned(FProfiler) then
          FProfiler.ResetCounts;
        BenchStart := GetNanoseconds;
        if Engine.RunBenchmarks(Deterministic, Entries, Total, Duration, Err, Aborted) then
        begin
          Res.TotalBenchmarks := Total;
          Res.DurationNanoseconds := Duration;
          SetLength(Res.Entries, Length(Entries));
          for I := 0 to High(Entries) do
          begin
            Res.Entries[I] := Default(TBenchmarkEntry);
            Res.Entries[I].Suite := UTF8Encode(Entries[I].Suite);
            Res.Entries[I].Name := UTF8Encode(Entries[I].Name);
            Res.Entries[I].SummaryScope := Entries[I].SummaryScope;
            Res.Entries[I].BoxplotScope := Entries[I].BoxplotScope;
            if Entries[I].Failed then
            begin
              { what it threw, as GocciaBenchmarkRunner's BenchmarkExceptionMessage gives it: the error, where it
                was made and the suggestion, without a code frame }
              Failure := Default(TGocciaRunResult);
              Failure.ErrorMessage := Entries[I].Error;
              Failure.ErrorIsError := Entries[I].ErrorIsError;
              Failure.ErrorName := Entries[I].ErrorName;
              Failure.ErrorText := Entries[I].ErrorText;
              Failure.ErrorFile := Entries[I].ErrorFile;
              Failure.ErrorSuggestion := Entries[I].ErrorSuggestion;
              Failure.ErrorLine := Entries[I].ErrorLine;
              Failure.ErrorColumn := Entries[I].ErrorColumn;
              Res.Entries[I].Error := FailureText(FileName, Failure, ASource, Engine, '', nil, False, True, False,
                Engine.TimeoutMs, EngineOptions.MaxInstructions.ValueOr(0));
            end
            else
            begin
              Res.Entries[I].OpsPerSec := Entries[I].OpsPerSec;
              Res.Entries[I].MeanMs := Entries[I].MeanMs;
              Res.Entries[I].Iterations := Entries[I].Iterations;
              Res.Entries[I].VariancePercentage := Entries[I].VariancePercentage;
              Res.Entries[I].SetupMs := Entries[I].SetupMs;
              Res.Entries[I].TeardownMs := Entries[I].TeardownMs;
              Res.Entries[I].MinOpsPerSec := Entries[I].MinOpsPerSec;
              Res.Entries[I].MaxOpsPerSec := Entries[I].MaxOpsPerSec;
              Res.Entries[I].SampleCount := Entries[I].SampleCount;
              Res.Entries[I].MinSampleMs := Entries[I].MinSampleMs;
              Res.Entries[I].P25Ms := Entries[I].P25Ms;
              Res.Entries[I].MedianMs := Entries[I].MedianMs;
              Res.Entries[I].P75Ms := Entries[I].P75Ms;
              Res.Entries[I].P99Ms := Entries[I].P99Ms;
              Res.Entries[I].P999Ms := Entries[I].P999Ms;
              Res.Entries[I].MaxSampleMs := Entries[I].MaxSampleMs;
            end;
          end;
        end
        else
        begin
          { a limit spent, or the run itself failed: the file's one error, as a failed file run reports it }
          Failure := Default(TGocciaRunResult);
          Failure.Aborted := Aborted;
          if Err = '' then
            Err := 'the benchmarks did not finish';
          Failure.ErrorMessage := Err;
          Fail(Failure);
        end;
        Res.ExecuteTimeNanoseconds := BenchStart - ExecStart - Res.CompileTimeNanoseconds +
          (GetNanoseconds - BenchStart);
      end;
    finally
      if Assigned(Engine) then
        FResults[AIndex].Memory := FileMemoryStats(MemoryStart, Engine.Memory, HeapStart, GetHeapStatus);
      BenchProgress := nil;
      Engine.Free;
      Engine := nil;
      { a file's thread gives the main thread's profilers back (they are not its to free) }
      if Assigned(FProfiler) and (GetCurrentThreadId <> MainThreadID) then
      begin
        TGocciaProfiler.Adopt(nil);
        TGocciaCompileProfiler.Adopt(nil);
      end;
    end;
  except
    on E: TCLIUsageError do
      raise;
    on E: Exception do
    begin
      { an engine fault is no verdict on the file: it stops the run (ADR 0196) }
      if IsEngineFault(E) then
        AbortOnEngineFault(FileName, E);
      { a file the host could not set up is one failed file (GocciaBenchmarkRunner's "Error loading") }
      if not FParallel then
        WriteLn(ErrOutput, 'Error: ', E.Message);
      AssignErrorFileResult(Res, FileName, E.Message);
    end;
  end;
  Resolver.Free;
  Grant.Free;
  Capture.Free;
  FResults[AIndex].Result := Res;
end;

{ ---- files ---- }

{ each file on a thread of its own, at most the job count at once; results in file order }
procedure TGocciaBenchmarkRunnerApp.RunFiles(const AFiles: TStringList; const AStdinSource: TStringList);
var
  I, Next, Oldest, Started, Running: Integer;
  Workers: array of TGocciaBenchThread;
  ErrorClass: ExceptClass;
  ErrorMessage: string;

  procedure Progress(AIndex: Integer);
  begin
    if FShowProgress and not FFromStdin then
      WriteProgressLine(Format('[%d/%d] %s', [AIndex + 1, AFiles.Count, ExtractFileName(AFiles[AIndex])]));
  end;

begin
  SetLength(FResults, AFiles.Count);
  for I := 0 to AFiles.Count - 1 do
  begin
    FResults[I] := Default(TGocciaBenchFileResult);
    FResults[I].Result.FileName := AFiles[I];
  end;
  SetLength(Workers, AFiles.Count);
  ErrorClass := nil;
  ErrorMessage := '';
  Running := 0;
  Oldest := 0;
  Started := 0;
  try
    for Next := 0 to AFiles.Count - 1 do
    begin
      if Running = FJobCount then
      begin
        Workers[Oldest].WaitFor;
        StopIfEngineFaultReported;
        Dec(Running);
        Inc(Oldest);
      end;
      { a sequential run prints each file's progress line before it runs, so its benchmarks' lines follow it }
      if not FParallel then
        Progress(Next);
      Workers[Next] := TGocciaBenchThread.Create(True, FILE_THREAD_STACK);
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
      StopIfEngineFaultReported;
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
  { a parallel run prints the files' progress once they are done, in file order }
  if FParallel then
    for I := 0 to Started - 1 do
      Progress(I);
  if FShowProgress and not FFromStdin and (AFiles.Count > 0) then
    WriteProgressLine('');
end;

procedure TGocciaBenchmarkRunnerApp.RunAndReport(const AFiles: TStringList; const AStdinSource: TStringList;
  const AReports: array of TReportSpec);
var
  Reporter: TBenchmarkReporter;
  FileMemory: array of TCLIJSONMemoryStats;
  HeapStart: THeapStatus;
  WallClockStart: Int64;
  I: Integer;
begin
  FJobCount := GetJobCount(AFiles.Count);
  if ProfilingEnabled or (FJobCount < 1) then
    FJobCount := 1;
  FParallel := (AFiles.Count > 1) and (FJobCount > 1);
  if FShowProgress and not FFromStdin then
  begin
    if FJobCount > 1 then
      WriteProgressLine(Format('Running %d files with %d workers', [AFiles.Count, FJobCount]))
    else
      WriteProgressLine(Format('Running %d files', [AFiles.Count]));
  end;
  Reporter := TBenchmarkReporter.Create;
  try
    HeapStart := GetHeapStatus;
    WallClockStart := GetNanoseconds;
    RunFiles(AFiles, AStdinSource);
    if FParallel then
    begin
      Reporter.WallClockDurationNanoseconds := GetNanoseconds - WallClockStart;
      Reporter.JobCount := FJobCount;
    end;
    { the files' engines ran on threads of their own: the report sums their memory }
    SetLength(FileMemory, Length(FResults));
    for I := 0 to High(FResults) do
      FileMemory[I] := FResults[I].Memory;
    Reporter.MemoryStats := AggregateMemoryStats(FileMemory, FParallel, HeapStart, GetHeapStatus, FJobCount);
    for I := 0 to High(FResults) do
      Reporter.AddFileResult(FResults[I].Result);
    for I := 0 to Length(AReports) - 1 do
    begin
      Reporter.Render(AReports[I].Format);
      if AReports[I].OutputFile <> '' then
        Reporter.WriteToFile(AReports[I].OutputFile)
      else
        Reporter.WriteToStdOut;
    end;
    if Reporter.HasFailures then
      ExitCode := 1;
  finally
    Reporter.Free;
  end;
end;

procedure TGocciaBenchmarkRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  Reports: array of TReportSpec;
  ReportCount, I, P: Integer;
  HasStructuredStdout, HasReadableStdout, UseStdin: Boolean;
  Files, RawFiles, Discovered, StdinNames, Expanded: TStringList;
  StdinSource: TStringList;
begin
  FShowProgress := not FNoProgress.Present;

  ReportCount := FFormats.Values.Count;
  if ReportCount = 0 then
  begin
    SetLength(Reports, 1);
    Reports[0].Format := brfConsole;
    Reports[0].OutputFile := FOutputFile.ValueOr('');
  end
  else
  begin
    SetLength(Reports, ReportCount);
    for I := 0 to ReportCount - 1 do
    begin
      Reports[I].Format := ParseReportFormat(FFormats.Values[I]);
      Reports[I].OutputFile := '';
    end;
    if FOutputFile.Present then
      Reports[ReportCount - 1].OutputFile := FOutputFile.Value;
  end;

  { structured (JSON/CSV) and human-readable (console/text) formats both on stdout would corrupt the structured one }
  HasStructuredStdout := False;
  HasReadableStdout := False;
  FCanPrintProfile := True;
  for I := 0 to Length(Reports) - 1 do
    if Reports[I].OutputFile = '' then
    begin
      if Reports[I].Format in [brfJSON, brfCompactJSON, brfCSV] then
        HasStructuredStdout := True
      else
        HasReadableStdout := True;
    end;
  if HasStructuredStdout then
    FCanPrintProfile := False;
  if HasStructuredStdout and HasReadableStdout then
  begin
    WriteLn(ErrOutput, 'Error: Cannot write structured and human-readable formats both to stdout. Provide an --output file for one of them.');
    ExitCode := 1;
    Exit;
  end;
  { the progress lines stay out of a structured report on stdout }
  if HasStructuredStdout then
    FShowProgress := False;

  FProfiler := TGocciaProfiler.Instance;
  FCompileProfiler := TGocciaCompileProfiler.Instance;
  if FProfileDeterministic.Present then
  begin
    if Assigned(FProfiler) then
      FProfiler.Deterministic := True;
    if Assigned(FCompileProfiler) then
      FCompileProfiler.Deterministic := True;
  end;

  { stdin when there are no paths or the only one is "-"; "-" among others is refused }
  UseStdin := (APaths.Count = 0) or ((APaths.Count = 1) and IsStdinPath(APaths[0]));
  FFromStdin := UseStdin;
  if not UseStdin then
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
      { source from stdin is governed by the working directory's config }
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
        for P := 0 to APaths.Count - 1 do
          if DirectoryExists(APaths[P]) then
          begin
            Discovered := FindAllFiles(APaths[P], ScriptExtensions);
            try
              for I := 0 to Discovered.Count - 1 do
                if not IsBenchmarkHelperFile(Discovered[I]) then
                  RawFiles.Add(Discovered[I]);
            finally
              Discovered.Free;
            end;
          end
          else if FileExists(APaths[P]) then
            RawFiles.Add(APaths[P])
          else
          begin
            WriteLn(ErrOutput, 'Error: Path not found: ', APaths[P]);
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
    RunAndReport(Files, StdinSource, Reports);
    StdinSource := nil;
  finally
    Files.Free;
    StdinSource.Free;
  end;
end;

{ the profile reports, as GocciaBenchmarkRunner prints and writes them }
procedure TGocciaBenchmarkRunnerApp.AfterExecute;
var
  Mode: Goccia.CLI.Options.TGocciaProfileMode;
  Opcodes, Functions, Compile: Boolean;
begin
  if not ProfilerOptions.Mode.Present or not Assigned(FProfiler) then
    Exit;
  Mode := ProfilerOptions.Mode.Value;
  Opcodes := (Mode = Goccia.CLI.Options.pmOpcodes) or (Mode = Goccia.CLI.Options.pmAll);
  Functions := (Mode = Goccia.CLI.Options.pmFunctions) or (Mode = Goccia.CLI.Options.pmAll);
  Compile := Mode = Goccia.CLI.Options.pmCompile;
  if FCanPrintProfile then
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

var
  RunResult: Integer;
begin
  InitCriticalSection(ProgressLock);
  try
    RunResult := TGocciaApplication.RunApplication(TGocciaBenchmarkRunnerApp, 'GocciaBenchmarkRunner');
  finally
    DoneCriticalSection(ProgressLock);
  end;
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
