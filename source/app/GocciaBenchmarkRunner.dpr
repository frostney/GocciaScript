program GocciaBenchmarkRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  CriticalSections,
  TimingUtils,

  Goccia.Application,
  Goccia.Benchmark.Reporter,
  Goccia.Builtins.Benchmark,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  Goccia.CLI.SourcePipelineResult,
  Goccia.CLI.Options,
  CLI.ConfigFile,
  CLI.Options,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Executor.Interpreter,
  Goccia.Executor.Bytecode,
  Goccia.Executor,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Benchmark,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.FFI,
  Goccia.RuntimeProfiles.BenchmarkRunner,
  Goccia.Scope,
  Goccia.ScriptLoader.Input,
  Goccia.CLI.JSON.Reporter,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception,

  Goccia.Threading,
  Goccia.Threading.Flags,
  Goccia.Threading.Init,

  FileUtils;

type
  TReportSpec = record
    Format: TBenchmarkReportFormat;
    OutputFile: string;
  end;

  TBenchmarkProgress = class
    class procedure WriteLine(const AMessage: string);
    class procedure OnProgress(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer);
  end;

var
  BenchmarkProgressLock: TGocciaCriticalSection;

class procedure TBenchmarkProgress.WriteLine(const AMessage: string);
begin
  CriticalSectionEnter(BenchmarkProgressLock);
  try
    WriteLn(AMessage);
    Flush(Output);
  finally
    CriticalSectionLeave(BenchmarkProgressLock);
  end;
end;

class procedure TBenchmarkProgress.OnProgress(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer);
begin
  if ASuiteName <> '' then
    WriteLine(SysUtils.Format('  [%d/%d] %s > %s', [AIndex, ATotal, ASuiteName, ABenchName]))
  else
    WriteLine(SysUtils.Format('  [%d/%d] %s', [AIndex, ATotal, ABenchName]));
end;

type
  PBenchmarkFileResult = ^TBenchmarkFileResult;
  TBenchmarkFileResultPtrArray = array[0..MaxInt div SizeOf(PBenchmarkFileResult) - 1] of PBenchmarkFileResult;
  PBenchmarkFileResultPtrArray = ^TBenchmarkFileResultPtrArray;

procedure PopulateFileResult(const AFileResult: TBenchmarkFileResult;
  const AScriptResult: TGocciaObjectValue; const AReporter: TBenchmarkReporter);
var
  ResultsArray: TGocciaArrayValue;
  SingleResult: TGocciaObjectValue;
  MutableFileResult: TBenchmarkFileResult;
  Entry: TBenchmarkEntry;
  ErrorMsg: string;
  I, EntryCount: Integer;
begin
  MutableFileResult := AFileResult;

  if AScriptResult <> nil then
  begin
    MutableFileResult.TotalBenchmarks := Round(AScriptResult.GetProperty('totalBenchmarks').ToNumberLiteral.Value);
    MutableFileResult.DurationNanoseconds := Round(AScriptResult.GetProperty('durationNanoseconds').ToNumberLiteral.Value);

    if AScriptResult.GetProperty('results') is TGocciaArrayValue then
    begin
      ResultsArray := TGocciaArrayValue(AScriptResult.GetProperty('results'));
      EntryCount := ResultsArray.GetLength;
      SetLength(MutableFileResult.Entries, EntryCount);

      for I := 0 to EntryCount - 1 do
      begin
        Entry := Default(TBenchmarkEntry);
        SingleResult := TGocciaObjectValue(ResultsArray.GetElement(I));

        Entry.Suite := SingleResult.GetProperty('suite').ToStringLiteral.Value;
        Entry.Name := SingleResult.GetProperty(PROP_NAME).ToStringLiteral.Value;
        Entry.SummaryScope := Round(SingleResult.GetProperty('summaryScope').ToNumberLiteral.Value);
        Entry.BoxplotScope := Round(SingleResult.GetProperty('boxplotScope').ToNumberLiteral.Value);

        ErrorMsg := SingleResult.GetProperty('error').ToStringLiteral.Value;
        if ErrorMsg <> 'undefined' then
        begin
          Entry.Error := ErrorMsg;
          Entry.OpsPerSec := 0;
          Entry.MeanMs := 0;
          Entry.Iterations := 0;
          Entry.VariancePercentage := 0;
          Entry.SetupMs := 0;
          Entry.TeardownMs := 0;
        end
        else
        begin
          Entry.Error := '';
          Entry.OpsPerSec := SingleResult.GetProperty('opsPerSec').ToNumberLiteral.Value;
          Entry.MeanMs := SingleResult.GetProperty('meanMs').ToNumberLiteral.Value;
          Entry.Iterations := Round(SingleResult.GetProperty('iterations').ToNumberLiteral.Value);
          Entry.VariancePercentage := SingleResult.GetProperty('variancePercentage').ToNumberLiteral.Value;
          Entry.SetupMs := SingleResult.GetProperty('setupMs').ToNumberLiteral.Value;
          Entry.TeardownMs := SingleResult.GetProperty('teardownMs').ToNumberLiteral.Value;
          Entry.MinOpsPerSec := SingleResult.GetProperty('minOpsPerSec').ToNumberLiteral.Value;
          Entry.MaxOpsPerSec := SingleResult.GetProperty('maxOpsPerSec').ToNumberLiteral.Value;
          Entry.SampleCount := Round(SingleResult.GetProperty('sampleCount').ToNumberLiteral.Value);
          Entry.MinSampleMs := SingleResult.GetProperty('minSampleMs').ToNumberLiteral.Value;
          Entry.P25Ms := SingleResult.GetProperty('p25Ms').ToNumberLiteral.Value;
          Entry.MedianMs := SingleResult.GetProperty('medianMs').ToNumberLiteral.Value;
          Entry.P75Ms := SingleResult.GetProperty('p75Ms').ToNumberLiteral.Value;
          Entry.P99Ms := SingleResult.GetProperty('p99Ms').ToNumberLiteral.Value;
          Entry.P999Ms := SingleResult.GetProperty('p999Ms').ToNumberLiteral.Value;
          Entry.MaxSampleMs := SingleResult.GetProperty('maxSampleMs').ToNumberLiteral.Value;
        end;

        MutableFileResult.Entries[I] := Entry;
      end;
    end;

    AReporter.AddFileResult(MutableFileResult);
  end
  else
  begin
    MutableFileResult.TotalBenchmarks := 0;
    MutableFileResult.DurationNanoseconds := 0;
    SetLength(MutableFileResult.Entries, 0);
    AReporter.AddFileResult(MutableFileResult);
  end;
end;

procedure AssignErrorFileResult(var AFileResult: TBenchmarkFileResult;
  const AFileName, AMessage: string);
begin
  AFileResult := Default(TBenchmarkFileResult);
  AFileResult.FileName := AFileName;
  AFileResult.LexTimeNanoseconds := 0;
  AFileResult.ParseTimeNanoseconds := 0;
  AFileResult.CompileTimeNanoseconds := 0;
  AFileResult.ExecuteTimeNanoseconds := 0;
  AFileResult.TotalBenchmarks := 0;
  AFileResult.DurationNanoseconds := 0;
  SetLength(AFileResult.Entries, 1);
  AFileResult.Entries[0].Suite := '';
  AFileResult.Entries[0].Name := '(fatal)';
  AFileResult.Entries[0].Error := AMessage;
end;

procedure MakeErrorFileResult(const AFileName, AMessage: string;
  const AReporter: TBenchmarkReporter);
var
  FileResult: TBenchmarkFileResult;
begin
  AssignErrorFileResult(FileResult, AFileName, AMessage);
  AReporter.AddFileResult(FileResult);
end;

type
  TBenchmarkRunnerApp = class(TGocciaCLIApplication)
  private
    FNoProgress: TFlagOption;
    FProfileDeterministic: TFlagOption;
    FFormats: TRepeatableOption;
    FOutputFile: TStringOption;
    FCanPrintProfile: Boolean;
    FWorkerProgressEnabled: Boolean;
    function ProfilingEnabled: Boolean;
    procedure RunBytecodeBenchmarkModule(const AEngine: TGocciaEngine;
      const AModule: TGocciaCompiledModule; const AFileName: string);
    procedure CollectBenchmarkFileInterpreted(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
    procedure CollectBenchmarkFileBytecode(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
    procedure CollectBenchmarkFile(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AMode: TGocciaExecutionMode;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSourceInterpreted(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSourceBytecode(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSource(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
    procedure RunBenchmarksFromStdin(const AReports: array of TReportSpec;
      const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
    procedure BenchmarkWorkerProc(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    procedure RunBenchmarks(const APaths: TStringList;
      const AReports: array of TReportSpec;
      const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
    procedure InitializeRuntime(const AEngine: TGocciaEngine);
    procedure InitializeRuntimeWithUnsafeFFI(const AEngine: TGocciaEngine);
    procedure WarmUpRuntime(const AEngine: TGocciaEngine);
    procedure WarmUpRuntimeWithUnsafeFFI(const AEngine: TGocciaEngine);
  protected
    procedure Configure; override;
    procedure Validate; override;
    procedure AfterExecute; override;
    procedure ConfigureCreatedEngine(const AEngine: TGocciaEngine;
      const AFileConfig: TConfigEntryArray); override;
    function UsageLine: string; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

function IsBenchmarkHelperFile(const AFileName: string): Boolean;
var
  NormalizedPath: string;
begin
  NormalizedPath := StringReplace(AFileName, PathDelim, '/', [rfReplaceAll]);
  Result := Pos('/helpers/', NormalizedPath) > 0;
end;

function TBenchmarkRunnerApp.ProfilingEnabled: Boolean;
begin
  Result := Assigned(ProfilerOptions) and ProfilerOptions.Mode.Present;
end;

function RuntimeBenchmark(const AEngine: TGocciaEngine): TGocciaBenchmark;
var
  BenchmarkExtension: TGocciaBenchmarkRuntimeExtension;
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := GetRuntime(AEngine);
  if Assigned(Runtime) then
  begin
    BenchmarkExtension := TGocciaBenchmarkRuntimeExtension(
      Runtime.FindRuntimeExtension(TGocciaBenchmarkRuntimeExtension));
    if Assigned(BenchmarkExtension) then
      Exit(BenchmarkExtension.BuiltinBenchmark);
  end;
  Result := nil;
end;

procedure ConfigureBenchmarkRuntime(const AEngine: TGocciaEngine;
  const AShowProgress, AClearBeforeMeasurement: Boolean);
var
  Benchmark: TGocciaBenchmark;
begin
  Benchmark := RuntimeBenchmark(AEngine);
  if not Assigned(Benchmark) then
    Exit;
  if AShowProgress then
    Benchmark.OnProgress := TBenchmarkProgress.OnProgress;
  if AClearBeforeMeasurement then
    Benchmark.OnBeforeMeasurement := AEngine.ClearTransientCaches;
end;

function RunRegisteredBenchmarks(const AEngine: TGocciaEngine;
  const ADeterministicProfile: Boolean): TGocciaObjectValue;
var
  Benchmark: TGocciaBenchmark;
  Value: TGocciaValue;
begin
  Benchmark := RuntimeBenchmark(AEngine);
  if not Assigned(Benchmark) then
    Exit(nil);

  Value := Benchmark.RunForHost(ADeterministicProfile);

  if Value is TGocciaObjectValue then
    Result := TGocciaObjectValue(Value)
  else
    Result := nil;
end;

procedure ClearRegisteredBenchmarks(const AEngine: TGocciaEngine);
var
  Benchmark: TGocciaBenchmark;
begin
  Benchmark := RuntimeBenchmark(AEngine);
  if Assigned(Benchmark) then
    Benchmark.ClearRegisteredBenchmarks;
end;

procedure TBenchmarkRunnerApp.RunBytecodeBenchmarkModule(
  const AEngine: TGocciaEngine;
  const AModule: TGocciaCompiledModule; const AFileName: string);
begin
  AEngine.RunModuleForSourceType(AModule, AFileName);
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkFileInterpreted(
  const AFileName: string; const AReporter: TBenchmarkReporter;
  const AShowProgress: Boolean);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  GC: TGarbageCollector;
  EngineResult: TGocciaScriptResult;
  ScriptResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
  BenchStart: Int64;
begin
  Source := nil;
  try
    try
      Source := SourceRegistry.Load(AFileName);
    except
      on E: Exception do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, 'Error loading benchmark file: ', E.Message);
        MakeErrorFileResult(AFileName, E.Message, AReporter);
        Exit;
      end;
    end;
    try
      Executor := TGocciaInterpreterExecutor.Create;
      try
        Engine := CreateEngine(AFileName, Source, Executor);
        try
          ConfigureBenchmarkRuntime(Engine, AShowProgress, False);

          StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
          StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
          try
            EngineResult := Engine.Execute;
            FileResult.FileName := AFileName;
            FileResult.DeterministicProfile := FProfileDeterministic.Present;
            FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
            FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
            FileResult.CompileTimeNanoseconds := 0;
            BenchStart := GetNanoseconds;
            ScriptResult := RunRegisteredBenchmarks(Engine,
              FProfileDeterministic.Present);
            FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds +
              (GetNanoseconds - BenchStart);
          finally
            ClearExecutionTimeout;
            ClearInstructionLimit;
          end;

          GC := TGarbageCollector.Instance;

          if Assigned(ScriptResult) and Assigned(GC) then
            GC.AddTempRoot(ScriptResult);
          try
            PopulateFileResult(FileResult, ScriptResult, AReporter);
          finally
            ClearRegisteredBenchmarks(Engine);
            if Assigned(ScriptResult) and Assigned(GC) then
              GC.RemoveTempRoot(ScriptResult);
          end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    except
      on E: TGocciaError do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, E.GetDetailedMessage(IsColorTerminal));
        MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
      end;
      on E: TGocciaThrowValue do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, FormatThrowDetail(E.Value, AFileName, Source, IsColorTerminal, E.Suggestion));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.Value, AFileName, Source, False, E.Suggestion), AReporter);
      end;
      on E: Exception do
        MakeErrorFileResult(AFileName, E.Message, AReporter);
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.Collect;
    Source.Free;
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkFileBytecode(
  const AFileName: string; const AReporter: TBenchmarkReporter;
  const AShowProgress: Boolean);
var
  Source: TStringList;
  PipelineOptions: TGocciaSourcePipelineOptions;
  SourcePipelineResult: TGocciaCLISourcePipelineResult;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  Module: TGocciaCompiledModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  FileResult: TBenchmarkFileResult;
  ScriptResult: TGocciaObjectValue;
  LexStart, CompileStart, CompileEnd, ExecEnd, BenchStart: Int64;
  LexTimeNanoseconds, ParseTimeNanoseconds: Int64;
begin
  Source := nil;
  SourcePipelineResult := nil;
  try
    try
      Source := SourceRegistry.Load(AFileName);
    except
      on E: Exception do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, 'Error loading benchmark file: ', E.Message);
        MakeErrorFileResult(AFileName, E.Message, AReporter);
        Exit;
      end;
    end;

    try
      Executor := TGocciaBytecodeExecutor.Create;
      try
        Engine := CreateEngine(AFileName, Source, Executor);
        try
          LexStart := GetNanoseconds;
          PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
          PipelineOptions.Preprocessors := Engine.Preprocessors;
          PipelineOptions.Compatibility := Engine.Compatibility;
          PipelineOptions.LabelStatementsEnabled :=
            Engine.LabelStatementsEnabled;
          PipelineOptions.ForInLoopsEnabled := Engine.ForInLoopsEnabled;
          PipelineOptions.ExperimentalJSModuleSourceEnabled :=
            Engine.ExperimentalJSModuleSourceEnabled;
          PipelineOptions.WarningUnsupportedFeatures :=
            Engine.WarningUnsupportedFeatures;
          PipelineOptions.SourceType := Engine.SourceType;
          ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(
            PipelineOptions);
          try
            SourcePipelineResult := TGocciaCLISourcePipelineResult.Parse(Source, AFileName,
              PipelineOptions, True);
            LexTimeNanoseconds := SourcePipelineResult.LexTimeNanoseconds;
            ParseTimeNanoseconds := SourcePipelineResult.ParseTimeNanoseconds;

            CompileStart := GetNanoseconds;
            try
              Module := Engine.CompileModule(SourcePipelineResult.ProgramNode);
              CompileEnd := GetNanoseconds;
            finally
              SourcePipelineResult.Free;
              SourcePipelineResult := nil;
            end;

            ConfigureBenchmarkRuntime(Engine, AShowProgress, True);

            StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
            StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
            try
              RunBytecodeBenchmarkModule(Engine, Module, AFileName);
              ExecEnd := GetNanoseconds;
              if FProfileDeterministic.Present and
                 (TGocciaProfiler.Instance <> nil) then
                TGocciaProfiler.Instance.ResetCounts;

              FileResult.FileName := AFileName;
              FileResult.DeterministicProfile := FProfileDeterministic.Present;
              FileResult.LexTimeNanoseconds := LexTimeNanoseconds;
              FileResult.ParseTimeNanoseconds := ParseTimeNanoseconds;
              FileResult.CompileTimeNanoseconds := CompileEnd - CompileStart;
              BenchStart := GetNanoseconds;
              ScriptResult := RunRegisteredBenchmarks(Engine,
                FProfileDeterministic.Present);
              FileResult.ExecuteTimeNanoseconds := ExecEnd - CompileEnd +
                (GetNanoseconds - BenchStart);
            finally
              ClearExecutionTimeout;
              ClearInstructionLimit;
            end;
          finally
            ActiveOptionsScope.Free;
          end;

          GC := TGarbageCollector.Instance;
          if Assigned(ScriptResult) and Assigned(GC) then
            GC.AddTempRoot(ScriptResult);

          try
            PopulateFileResult(FileResult, ScriptResult, AReporter);
          finally
            ClearRegisteredBenchmarks(Engine);
            if Assigned(ScriptResult) and Assigned(GC) then
              GC.RemoveTempRoot(ScriptResult);
          end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    except
      on E: TGocciaError do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, E.GetDetailedMessage(IsColorTerminal));
        MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
      end;
      on E: TGocciaThrowValue do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, FormatThrowDetail(E.Value, AFileName, Source, IsColorTerminal, E.Suggestion));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.Value, AFileName, Source, False, E.Suggestion), AReporter);
      end;
      on E: EGocciaBytecodeThrow do
      begin
        if not GIsWorkerThread then
          WriteLn(ErrOutput, FormatThrowDetail(E.ThrownValue, AFileName, Source, IsColorTerminal));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.ThrownValue, AFileName, Source, False), AReporter);
      end;
      on E: Exception do
        MakeErrorFileResult(AFileName, E.Message, AReporter);
    end;
  finally
    SourcePipelineResult.Free;
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.Collect;
    Source.Free;
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkFile(const AFileName: string;
  const AReporter: TBenchmarkReporter; const AMode: TGocciaExecutionMode;
  const AShowProgress: Boolean);
begin
  case AMode of
    emInterpreted: CollectBenchmarkFileInterpreted(AFileName, AReporter,
      AShowProgress);
    emBytecode: CollectBenchmarkFileBytecode(AFileName, AReporter,
      AShowProgress);
  else
    raise Exception.CreateFmt('Unsupported execution mode: %d', [Ord(AMode)]);
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkSourceInterpreted(
  const ASource: TStringList; const AFileName: string;
  const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  GC: TGarbageCollector;
  EngineResult: TGocciaScriptResult;
  ScriptResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
  BenchStart: Int64;
begin
  try
    Executor := TGocciaInterpreterExecutor.Create;
    try
      Engine := CreateEngine(AFileName, ASource, Executor);
      try
        ConfigureBenchmarkRuntime(Engine, AShowProgress, False);

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          EngineResult := Engine.Execute;
          FileResult.FileName := AFileName;
          FileResult.DeterministicProfile := FProfileDeterministic.Present;
          FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
          FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
          FileResult.CompileTimeNanoseconds := 0;
          BenchStart := GetNanoseconds;
          ScriptResult := RunRegisteredBenchmarks(Engine,
            FProfileDeterministic.Present);
          FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds +
            (GetNanoseconds - BenchStart);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;

        GC := TGarbageCollector.Instance;

        if Assigned(ScriptResult) and Assigned(GC) then
          GC.AddTempRoot(ScriptResult);
        try
          PopulateFileResult(FileResult, ScriptResult, AReporter);
        finally
          ClearRegisteredBenchmarks(Engine);
          if Assigned(ScriptResult) and Assigned(GC) then
            GC.RemoveTempRoot(ScriptResult);
        end;
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
  except
    on E: TGocciaError do
    begin
      if not GIsWorkerThread then
        WriteLn(ErrOutput, E.GetDetailedMessage(IsColorTerminal));
      MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
    end;
    on E: TGocciaThrowValue do
    begin
      if not GIsWorkerThread then
        WriteLn(ErrOutput, FormatThrowDetail(E.Value, AFileName, ASource, IsColorTerminal, E.Suggestion));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.Value, AFileName, ASource, False, E.Suggestion), AReporter);
    end;
    on E: Exception do
      MakeErrorFileResult(AFileName, E.Message, AReporter);
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkSourceBytecode(
  const ASource: TStringList; const AFileName: string;
  const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
var
  PipelineOptions: TGocciaSourcePipelineOptions;
  SourcePipelineResult: TGocciaCLISourcePipelineResult;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  Module: TGocciaCompiledModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  FileResult: TBenchmarkFileResult;
  ScriptResult: TGocciaObjectValue;
  LexStart, CompileStart, CompileEnd, ExecEnd, BenchStart: Int64;
  LexTimeNanoseconds, ParseTimeNanoseconds: Int64;
begin
  SourcePipelineResult := nil;
  try
    Executor := TGocciaBytecodeExecutor.Create;
    try
      Engine := CreateEngine(AFileName, ASource, Executor);
      try
        LexStart := GetNanoseconds;
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
            PipelineOptions, True);
          LexTimeNanoseconds := SourcePipelineResult.LexTimeNanoseconds;
          ParseTimeNanoseconds := SourcePipelineResult.ParseTimeNanoseconds;

          CompileStart := GetNanoseconds;
          try
            Module := Engine.CompileModule(SourcePipelineResult.ProgramNode);
            CompileEnd := GetNanoseconds;
          finally
            SourcePipelineResult.Free;
            SourcePipelineResult := nil;
          end;

          ConfigureBenchmarkRuntime(Engine, AShowProgress, True);

          StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
          StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
          try
            RunBytecodeBenchmarkModule(Engine, Module, AFileName);
            ExecEnd := GetNanoseconds;
            if FProfileDeterministic.Present and
               (TGocciaProfiler.Instance <> nil) then
              TGocciaProfiler.Instance.ResetCounts;

            FileResult.FileName := AFileName;
            FileResult.DeterministicProfile := FProfileDeterministic.Present;
            FileResult.LexTimeNanoseconds := LexTimeNanoseconds;
            FileResult.ParseTimeNanoseconds := ParseTimeNanoseconds;
            FileResult.CompileTimeNanoseconds := CompileEnd - CompileStart;
            BenchStart := GetNanoseconds;
            ScriptResult := RunRegisteredBenchmarks(Engine,
              FProfileDeterministic.Present);
            FileResult.ExecuteTimeNanoseconds := ExecEnd - CompileEnd +
              (GetNanoseconds - BenchStart);
          finally
            ClearExecutionTimeout;
            ClearInstructionLimit;
          end;
        finally
          ActiveOptionsScope.Free;
        end;

        GC := TGarbageCollector.Instance;
        if Assigned(ScriptResult) and Assigned(GC) then
          GC.AddTempRoot(ScriptResult);

        try
          PopulateFileResult(FileResult, ScriptResult, AReporter);
        finally
          ClearRegisteredBenchmarks(Engine);
          if Assigned(ScriptResult) and Assigned(GC) then
            GC.RemoveTempRoot(ScriptResult);
        end;
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
  except
    on E: TGocciaError do
    begin
      if not GIsWorkerThread then
        WriteLn(ErrOutput, E.GetDetailedMessage(IsColorTerminal));
      MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
    end;
    on E: TGocciaThrowValue do
    begin
      if not GIsWorkerThread then
        WriteLn(ErrOutput, FormatThrowDetail(E.Value, AFileName, ASource, IsColorTerminal, E.Suggestion));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.Value, AFileName, ASource, False, E.Suggestion), AReporter);
    end;
    on E: EGocciaBytecodeThrow do
    begin
      if not GIsWorkerThread then
        WriteLn(ErrOutput, FormatThrowDetail(E.ThrownValue, AFileName, ASource, IsColorTerminal));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.ThrownValue, AFileName, ASource, False), AReporter);
    end;
    on E: Exception do
      MakeErrorFileResult(AFileName, E.Message, AReporter);
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkSource(const ASource: TStringList;
  const AFileName: string; const AReporter: TBenchmarkReporter;
  const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
begin
  case AMode of
    emInterpreted: CollectBenchmarkSourceInterpreted(ASource, AFileName,
      AReporter, AShowProgress);
    emBytecode: CollectBenchmarkSourceBytecode(ASource, AFileName,
      AReporter, AShowProgress);
  else
    raise Exception.CreateFmt('Unsupported execution mode: %d', [Ord(AMode)]);
  end;
end;

{ Worker procedure executed on each thread for a single benchmark file.
  Runs the benchmark, extracts the TBenchmarkFileResult into a pre-allocated
  array, and frees all GC-managed objects before returning. }
procedure TBenchmarkRunnerApp.BenchmarkWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
var
  WorkerReporter: TBenchmarkReporter;
  WorkerResults: PBenchmarkFileResultPtrArray;
  FileResult: PBenchmarkFileResult;
  Mode: TGocciaExecutionMode;
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  WorkerResults := PBenchmarkFileResultPtrArray(AData);

  if EngineOptions.Mode.Matches(emBytecode) then
    Mode := emBytecode
  else
    Mode := emInterpreted;

  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.Enabled := True;

  WorkerReporter := TBenchmarkReporter.Create;
  try
    try
      CollectBenchmarkFile(AFileName, WorkerReporter, Mode,
        FWorkerProgressEnabled);
      if WorkerReporter.FileCount > 0 then
      begin
        New(FileResult);
        FileResult^ := WorkerReporter.Files[0];
        WorkerResults^[AIndex] := FileResult;
      end;
    except
      on E: Exception do
      begin
        AErrorMessage := E.Message;
        New(FileResult);
        AssignErrorFileResult(FileResult^, AFileName, E.Message);
        WorkerResults^[AIndex] := FileResult;
      end;
    end;
  finally
    WorkerReporter.Free;
  end;
end;

procedure TBenchmarkRunnerApp.RunBenchmarksFromStdin(
  const AReports: array of TReportSpec;
  const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
var
  Source, SectionSource, Names: TStringList;
  Reporter: TBenchmarkReporter;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  I, J: Integer;
begin
  Source := ReadSourceFromText(Input);
  Reporter := TBenchmarkReporter.Create;
  try
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    if MultifileEnabled then
    begin
      // Ownership of Source transfers to SplitStdinMultifile.
      Names := SplitStdinMultifile(Source);
      try
        for I := 0 to Names.Count - 1 do
        begin
          SectionSource := SourceRegistry.Load(Names[I]);
          try
            CollectBenchmarkSource(SectionSource, Names[I], Reporter,
              AMode, AShowProgress);
          finally
            SectionSource.Free;
          end;
          // Match the per-file GC pass in CollectBenchmarkFile* so
          // GC-managed objects from one stdin section don't accumulate
          // across the next one.
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.Collect;
        end;
      finally
        Names.Free;
      end;
    end
    else
    begin
      try
        CollectBenchmarkSource(Source, STDIN_FILE_NAME, Reporter,
          AMode, AShowProgress);
      finally
        Source.Free;
      end;
    end;
    Reporter.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
    for J := 0 to Length(AReports) - 1 do
    begin
      Reporter.Render(AReports[J].Format);
      if AReports[J].OutputFile <> '' then
        Reporter.WriteToFile(AReports[J].OutputFile)
      else
        Reporter.WriteToStdOut;
    end;

    if Reporter.HasFailures then
      ExitCode := 1;
  finally
    Reporter.Free;
  end;
end;

procedure TBenchmarkRunnerApp.RunBenchmarks(const APaths: TStringList;
  const AReports: array of TReportSpec;
  const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
var
  Files, RawFiles: TStringList;
  I, J, P, JobCount: Integer;
  DiscoveredFiles: TStringList;
  Reporter: TBenchmarkReporter;
  Pool: TGocciaThreadPool;
  WorkerData: array of PBenchmarkFileResult;
  WallClockStart: Int64;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  MainMemoryStats: TCLIJSONMemoryStats;
  WorkerMemoryStats: TCLIJSONMemoryStats;
begin
  WorkerMemoryStats := DefaultCLIJSONMemoryStats;
  Files := nil;
  Reporter := TBenchmarkReporter.Create;
  try
    RawFiles := TStringList.Create;
    try
      for P := 0 to APaths.Count - 1 do
      begin
        if DirectoryExists(APaths[P]) then
        begin
          DiscoveredFiles := FindAllFiles(APaths[P], ScriptExtensions);
          try
            for I := 0 to DiscoveredFiles.Count - 1 do
              if not IsBenchmarkHelperFile(DiscoveredFiles[I]) then
                RawFiles.Add(DiscoveredFiles[I]);
          finally
            DiscoveredFiles.Free;
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
      end;
      Files := ExpandMultifileFiles(RawFiles);
    finally
      RawFiles.Free;
    end;

    JobCount := GetJobCount(Files.Count);
    if ProfilingEnabled and (JobCount > 1) then
      JobCount := 1;

    if AShowProgress then
    begin
      if JobCount > 1 then
        TBenchmarkProgress.WriteLine(SysUtils.Format('Running %d files with %d workers',
          [Files.Count, JobCount]))
      else
        TBenchmarkProgress.WriteLine(SysUtils.Format('Running %d files', [Files.Count]));
    end;

    if JobCount > 1 then
    begin
      { Parallel path: pre-allocate result array, run workers, then
        add results to the reporter on the main thread in file order. }
      SetLength(WorkerData, Files.Count);
      for I := 0 to Files.Count - 1 do
        WorkerData[I] := nil;

      if AnyFileConfigEnablesFlag(Files, EngineOptions.UnsafeFFI) then
        EnsureSharedPrototypesInitialized(WarmUpRuntimeWithUnsafeFFI)
      else
        EnsureSharedPrototypesInitialized(WarmUpRuntime);

      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      WallClockStart := GetNanoseconds;

      Pool := TGocciaThreadPool.Create(JobCount);
      try
        if (TGarbageCollector.Instance <> nil) then
          Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
        FWorkerProgressEnabled := AShowProgress;
        try
          Pool.RunAll(Files, BenchmarkWorkerProc, @WorkerData[0]);
        finally
          FWorkerProgressEnabled := False;
        end;
        WorkerMemoryStats := Pool.MemoryStats;
      finally
        Pool.Free;
      end;

      Reporter.WallClockDurationNanoseconds := GetNanoseconds - WallClockStart;
      Reporter.JobCount := JobCount;
      MainMemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
      Reporter.MemoryStats := CombineCLIJSONMemoryStats(
        MainMemoryStats, WorkerMemoryStats, True);

      { Collect results on the main thread in original file order. }
      try
        for I := 0 to Files.Count - 1 do
        begin
          if AShowProgress then
            TBenchmarkProgress.WriteLine(SysUtils.Format('[%d/%d] %s',
              [I + 1, Files.Count, ExtractFileName(Files[I])]));
          if Assigned(WorkerData[I]) then
            Reporter.AddFileResult(WorkerData[I]^)
          else
            MakeErrorFileResult(Files[I], 'Benchmark worker produced no result',
              Reporter);
        end;
      finally
        for I := 0 to Length(WorkerData) - 1 do
          if Assigned(WorkerData[I]) then
          begin
            Dispose(WorkerData[I]);
            WorkerData[I] := nil;
          end;
      end;
    end
    else
    begin
      { Sequential path: run each file in order on the main thread. }
      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      for I := 0 to Files.Count - 1 do
      begin
        if AShowProgress then
          TBenchmarkProgress.WriteLine(SysUtils.Format('[%d/%d] %s',
            [I + 1, Files.Count, ExtractFileName(Files[I])]));
        CollectBenchmarkFile(Files[I], Reporter, AMode, AShowProgress);
      end;
      Reporter.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
    end;

    if AShowProgress and (Files.Count > 0) then
      TBenchmarkProgress.WriteLine('');

    for J := 0 to Length(AReports) - 1 do
    begin
      Reporter.Render(AReports[J].Format);
      if AReports[J].OutputFile <> '' then
        Reporter.WriteToFile(AReports[J].OutputFile)
      else
        Reporter.WriteToStdOut;
    end;

    if Reporter.HasFailures then
      ExitCode := 1;
  finally
    Reporter.Free;
    Files.Free;
  end;
end;

function TBenchmarkRunnerApp.UsageLine: string;
begin
  Result := '[path...|-] [options]';
end;

procedure TBenchmarkRunnerApp.Configure;
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
end;

procedure TBenchmarkRunnerApp.Validate;
begin
  inherited Validate;

  if ProfilerOptions.Format.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('functions');

  if FProfileDeterministic.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('all');

  if ProfilerOptions.Mode.Present then
    EngineOptions.Mode.Apply('bytecode');

  if ProfilerOptions.OutputPath.Present and not ProfilerOptions.Mode.Present then
    raise TParseError.Create(
      '--profile-output requires --profile=opcodes|functions|all.');

  if ProfilerOptions.Format.Matches(pfFlamegraph) and
     not ProfilerOptions.OutputPath.Present then
    raise TParseError.Create(
      '--profile-format=flamegraph requires --profile-output=<path>.');
end;

procedure TBenchmarkRunnerApp.AfterExecute;
var
  ProfileOpcodes, ProfileFunctions: Boolean;
  ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
begin
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
    if FCanPrintProfile then
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

procedure TBenchmarkRunnerApp.ConfigureCreatedEngine(
  const AEngine: TGocciaEngine; const AFileConfig: TConfigEntryArray);
var
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  Runtime: TGocciaRuntimeCore;
begin
  InitializeRuntime(AEngine);
  Runtime := GetRuntime(AEngine);
  if Assigned(EngineOptions) and
     ResolveFlagOption(EngineOptions.UnsafeFFI, AFileConfig) then
    Runtime.Install(TGocciaFFIRuntimeExtension.Create);
  ConsoleExtension := TGocciaConsoleRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
  if LogFileOpen and Assigned(ConsoleExtension) and
     Assigned(ConsoleExtension.BuiltinConsole) then
    ConsoleExtension.BuiltinConsole.LogCallback := HandleConsoleLog;
end;

procedure TBenchmarkRunnerApp.InitializeRuntime(const AEngine: TGocciaEngine);
var
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := AttachRuntime(AEngine);
  ApplyBenchmarkRunnerRuntimeProfile(Runtime);
end;

procedure TBenchmarkRunnerApp.InitializeRuntimeWithUnsafeFFI(
  const AEngine: TGocciaEngine);
begin
  InitializeRuntime(AEngine);
  GetRuntime(AEngine).Install(TGocciaFFIRuntimeExtension.Create);
end;

procedure TBenchmarkRunnerApp.WarmUpRuntime(const AEngine: TGocciaEngine);
begin
  InitializeRuntime(AEngine);
  WarmUpSharedLazyGlobals(AEngine);
end;

procedure TBenchmarkRunnerApp.WarmUpRuntimeWithUnsafeFFI(
  const AEngine: TGocciaEngine);
begin
  InitializeRuntimeWithUnsafeFFI(AEngine);
  WarmUpSharedLazyGlobals(AEngine);
end;

procedure TBenchmarkRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  Reports: array of TReportSpec;
  ReportCount, I: Integer;
  Mode: TGocciaExecutionMode;
  ShowProgress: Boolean;
  HasStructuredStdout, HasReadableStdout: Boolean;
begin
  ShowProgress := not FNoProgress.Present;

  if EngineOptions.Mode.Matches(emBytecode) then
    Mode := emBytecode
  else
    Mode := emInterpreted;

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

  // Reject when both structured (JSON/CSV) and human-readable (console/text)
  // formats would write to stdout — the mixed output corrupts structured data.
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

  // Suppress progress when structured output (JSON/CSV) goes to stdout so
  // that human-readable status messages do not corrupt the output document.
  if ShowProgress then
    for I := 0 to Length(Reports) - 1 do
      if (Reports[I].Format in [brfJSON, brfCompactJSON, brfCSV]) and (Reports[I].OutputFile = '') then
      begin
        ShowProgress := False;
        Break;
      end;

  if APaths.Count = 0 then
    RunBenchmarksFromStdin(Reports, Mode, ShowProgress)
  else if (APaths.Count = 1) and IsStdinPath(APaths[0]) then
    RunBenchmarksFromStdin(Reports, Mode, ShowProgress)
  else
  begin
    { Reject mixing "-" with file paths so stdin cannot silently be
      interleaved with on-disk files. Matches the rule enforced by
      GocciaScriptLoader and GocciaTestRunner. }
    for I := 0 to APaths.Count - 1 do
      if IsStdinPath(APaths[I]) then
      begin
        WriteLn(ErrOutput,
          'Error: stdin is supported only as the sole input.');
        ExitCode := 1;
        Exit;
      end;
    RunBenchmarks(APaths, Reports, Mode, ShowProgress);
  end;
end;

var
  RunResult: Integer;
begin
  CriticalSectionInit(BenchmarkProgressLock);
  try
    RunResult := TGocciaApplication.RunApplication(TBenchmarkRunnerApp, 'GocciaBenchmarkRunner');
  finally
    CriticalSectionDone(BenchmarkProgressLock);
  end;
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
