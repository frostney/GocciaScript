program GocciaTestRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils,

  TimingUtils,
  TextSemantics,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  CLI.Options,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.JSON.Utils,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.ScriptLoader.Input,
  Goccia.SourceMap,
  Goccia.Builtins.TestingLibrary,
  Goccia.CLI.JSON.Reporter,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  Goccia.Threading,
  Goccia.Threading.Init,

  FileUtils in 'units/FileUtils.pas';

const
  { Default per-file execution timeout in milliseconds. Applied when the
    user does not pass --timeout explicitly. Prevents infinite loops from
    hanging the process. Override with --timeout=N or disable with
    --timeout=0. }
  DEFAULT_TIMEOUT_MS = 30000;  // 30 seconds

type
  { Plain-data record for extracting test results from GC-managed objects.
    Used by the parallel path to pass results across thread boundaries
    without referencing GC-managed TGocciaValue instances. }
  TTestWorkerData = record
    Passed: Double;
    Failed: Double;
    Skipped: Double;
    TotalRunTests: Double;
    Assertions: Double;
    Duration: Double;
    FailedTestNames: array of string;
    LexNs: Int64;
    ParseNs: Int64;
    CompileNs: Int64;
    ExecNs: Int64;
    ErrorMessage: string;
  end;

  TTestWorkerDataArray = array[0..MaxInt div SizeOf(TTestWorkerData) - 1] of TTestWorkerData;
  PTestWorkerDataArray = ^TTestWorkerDataArray;

  TTestFileResult = record
    TestResult: TGocciaObjectValue;
    Timing: TGocciaScriptResult;
    ErrorMessage: string;
  end;

  { Per-input-file outcome, serialised into the JSON output's "results"
    array so that external harnesses (e.g. the test262 driver) can map
    each input file to an explicit pass/fail result — rather than
    guessing "not in failedTests => passed", which silently
    mis-reports when a file registers zero tests or when the runner
    otherwise loses a count. }
  TTestFilePerResult = record
    FileName: string;
    Passed: Double;
    Failed: Double;
    Skipped: Double;
    TotalTests: Double;
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    CompileTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    FailedTests: array of string;
    ErrorMessage: string;
  end;

  TAggregatedTestResult = record
    TestResult: TGocciaObjectValue;
    TotalLexNanoseconds: Int64;
    TotalParseNanoseconds: Int64;
    TotalCompileNanoseconds: Int64;
    TotalExecNanoseconds: Int64;
    JobCount: Integer;
    MemoryStats: TCLIJSONMemoryStats;
    FileResults: array of TTestFilePerResult;
  end;

  TTestRunnerApp = class(TGocciaCLIApplication)
  private
    FNoProgress: TGocciaFlagOption;
    FNoResults: TGocciaFlagOption;
    FExitOnFirst: TGocciaFlagOption;
    FSilent: TGocciaFlagOption;
    FOutputFile: TGocciaStringOption;
    FTestTimeout: TGocciaIntegerOption;
    FDescribeTimeout: TGocciaIntegerOption;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    function GlobalBuiltins: TGocciaGlobalBuiltins; override;
  private
    function RunGocciaScriptInterpreted(const AFileName: string;
      APreloadedSource: TStringList = nil): TTestFileResult;
    function RunGocciaScriptBytecode(const AFileName: string;
      APreloadedSource: TStringList = nil): TTestFileResult;
    function RunGocciaScript(const AFileName: string;
      APreloadedSource: TStringList = nil): TTestFileResult;
    function RunScriptFromFile(const AFileName: string;
      APreloadedSource: TStringList = nil): TAggregatedTestResult;
    function RunScriptsFromFiles(const AFiles: TStringList): TAggregatedTestResult;
    function RunScriptsFromFilesParallel(const AFiles: TStringList; const AJobCount: Integer): TAggregatedTestResult;
    procedure TestWorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
    procedure WriteResultsJSON(const AResult: TAggregatedTestResult; const AFileName: string);
    procedure WriteFileResults(ALines: TStringList; const AResult: TAggregatedTestResult;
      const APropertyName: string; const ATrailingComma: Boolean);
    procedure PrintTestResults(const AResult: TAggregatedTestResult);
  end;

function MakeEmptyTestResult(const AScriptResult: TGocciaObjectValue;
  const AErrorMessage: string = ''): TTestFileResult;
begin
  Result.TestResult := AScriptResult;
  Result.Timing.Result := nil;
  Result.Timing.LexTimeNanoseconds := 0;
  Result.Timing.ParseTimeNanoseconds := 0;
  Result.Timing.CompileTimeNanoseconds := 0;
  Result.Timing.ExecuteTimeNanoseconds := 0;
  Result.Timing.TotalTimeNanoseconds := 0;
  Result.Timing.FileName := '';
  Result.ErrorMessage := AErrorMessage;
end;

function CreateDefaultScriptResult: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(1));
  Result.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.ZeroValue);
  Result.AssignProperty('passed', TGocciaNumberLiteralValue.ZeroValue);
  Result.AssignProperty('failed', TGocciaNumberLiteralValue.ZeroValue);
  Result.AssignProperty('skipped', TGocciaNumberLiteralValue.ZeroValue);
  Result.AssignProperty('assertions', TGocciaNumberLiteralValue.ZeroValue);
  Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
  Result.AssignProperty('failedTests', TGocciaArrayValue.Create);
end;

procedure MergeFileResult(const ATarget: TGocciaObjectValue; const AFileResult: TGocciaObjectValue);
begin
  if AFileResult = nil then Exit;
  if AFileResult.GetProperty('totalRunTests').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('totalRunTests', AFileResult.GetProperty('totalRunTests'));
  if AFileResult.GetProperty('passed').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('passed', AFileResult.GetProperty('passed'));
  if AFileResult.GetProperty('failed').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('failed', AFileResult.GetProperty('failed'));
  if AFileResult.GetProperty('skipped').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('skipped', AFileResult.GetProperty('skipped'));
  if AFileResult.GetProperty('assertions').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('assertions', AFileResult.GetProperty('assertions'));
  if AFileResult.GetProperty('duration').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('duration', AFileResult.GetProperty('duration'));
  if AFileResult.GetProperty('failedTests').ToStringLiteral.Value <> 'undefined' then
    ATarget.AssignProperty('failedTests', AFileResult.GetProperty('failedTests'));
end;

procedure MarkLoadError(const AResult: TGocciaObjectValue; const AFileName, AMessage: string);
begin
  AResult.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(1));
  AResult.AssignProperty('failed', TGocciaNumberLiteralValue.Create(1));
  TGocciaArrayValue(AResult.GetProperty('failedTests')).Elements.Add(
    TGocciaStringLiteralValue.Create(AFileName + ': ' + AMessage));
end;

{ TTestRunnerApp }

procedure TTestRunnerApp.Configure;
begin
  AddEngineOptions;
  AddCoverageOptions;
  FNoProgress := AddFlag('no-progress', 'Suppress per-file progress output');
  FNoResults := AddFlag('no-results', 'Suppress test results summary');
  FExitOnFirst := AddFlag('exit-on-first-failure', 'Stop on first test failure');
  FSilent := AddFlag('silent', 'Suppress console output from test scripts');
  FOutputFile := AddString('output', 'Write test results as JSON to file');
  FTestTimeout := AddInteger('test-timeout',
    'Per-test timeout in ms (0 disables). Marks the test TIMEOUT and continues.');
  FDescribeTimeout := AddInteger('describe-timeout',
    'Per-describe timeout in ms (0 disables). Aborts the suite and continues.');
end;

procedure TTestRunnerApp.Validate;
begin
  if CoverageOptions.Format.Present or CoverageOptions.OutputPath.Present then
    CoverageOptions.Enabled.Apply('');
end;

function TTestRunnerApp.UsageLine: string;
begin
  Result := '[path...|-] [options]';
end;

procedure TTestRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  Files: TStringList;
  I: Integer;
  AggregatedResult: TAggregatedTestResult;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  StdinSource: TStringList;
  UseStdin: Boolean;
begin
  { stdin support — match GocciaScriptLoader and GocciaBenchmarkRunner.
    Source is read from stdin and tagged as <stdin> when either:
      * no path arguments are supplied, OR
      * the sole path argument is "-".
    Mixing "-" with other paths is rejected so structured stdin input
    cannot silently be interleaved with on-disk files. }
  UseStdin := (APaths.Count = 0) or
              ((APaths.Count = 1) and IsStdinPath(APaths[0]));

  if (not UseStdin) and (APaths.Count > 1) then
    for I := 0 to APaths.Count - 1 do
      if IsStdinPath(APaths[I]) then
      begin
        WriteLn(StdErr,
          'Error: stdin supports only as the sole input path.');
        ExitCode := 1;
        Exit;
      end;

  { Publish per-test/per-describe deadlines to the testing library
    before any workers spawn. These globals are read-only after this
    point, so plain globals are safe across the worker pool. 0 means
    "no deadline at that scope" — the engine-level tsFile deadline
    still applies. }
  GTestRunnerTestTimeoutMs := FTestTimeout.ValueOr(0);
  GTestRunnerDescribeTimeoutMs := FDescribeTimeout.ValueOr(0);

  Files := TStringList.Create;
  StdinSource := nil;
  try
    if UseStdin then
    begin
      StdinSource := ReadSourceFromText(Input);
      Files.Add(STDIN_FILE_NAME);
    end
    else
      for I := 0 to APaths.Count - 1 do
      begin
        if DirectoryExists(APaths[I]) then
          Files.AddStrings(FindAllFiles(APaths[I], ScriptExtensions))
        else if FileExists(APaths[I]) then
          Files.Add(APaths[I])
        else
        begin
          WriteLn(StdErr, 'Error: Path not found: ', APaths[I]);
          ExitCode := 1;
          Exit;
        end;
      end;

    if not FNoProgress.Present then
    begin
      if GetJobCount(Files.Count) > 1 then
        WriteLn(SysUtils.Format('Running %d files with %d workers',
          [Files.Count, GetJobCount(Files.Count)]))
      else
        WriteLn(SysUtils.Format('Running %d files', [Files.Count]));
    end;

    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    if Files.Count = 1 then
    begin
      if not FNoProgress.Present then
        WriteLn('[1/1] ', Files[0]);
      { Stdin is always a single source — the callee takes ownership of
        StdinSource and frees it like a disk-loaded TStringList. }
      AggregatedResult := RunScriptFromFile(Files[0], StdinSource);
      StdinSource := nil;
    end
    else if GetJobCount(Files.Count) > 1 then
      AggregatedResult := RunScriptsFromFilesParallel(Files, GetJobCount(Files.Count))
    else
      AggregatedResult := RunScriptsFromFiles(Files);
    AggregatedResult.MemoryStats :=
      FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
    PrintTestResults(AggregatedResult);

    if (CoverageOptions.Enabled.Present or CoverageOptions.Format.Present or
        CoverageOptions.OutputPath.Present) and
       Assigned(TGocciaCoverageTracker.Instance) then
    begin
      PrintCoverageSummary(TGocciaCoverageTracker.Instance);
      if Files.Count = 1 then
        PrintCoverageDetail(TGocciaCoverageTracker.Instance, Files[0]);
      if CoverageOptions.Format.Matches(cfLcov) and
         (CoverageOptions.OutputPath.ValueOr('') <> '') then
        WriteCoverageLcov(TGocciaCoverageTracker.Instance,
          CoverageOptions.OutputPath.ValueOr(''));
      if CoverageOptions.Format.Matches(cfJson) and
         (CoverageOptions.OutputPath.ValueOr('') <> '') then
        WriteCoverageJSON(TGocciaCoverageTracker.Instance,
          CoverageOptions.OutputPath.ValueOr(''));
    end;
  finally
    Files.Free;
    { StdinSource is normally consumed by RunScriptFromFile (which frees
      it). Free here only if an early exit left it dangling. }
    StdinSource.Free;
  end;
end;

function TTestRunnerApp.GlobalBuiltins: TGocciaGlobalBuiltins;
begin
  Result := [ggTestAssertions];
end;

function TTestRunnerApp.RunGocciaScriptInterpreted(const AFileName: string;
  APreloadedSource: TStringList): TTestFileResult;
var
  Source: TStringList;
  ScriptResult, FileResult: TGocciaObjectValue;
  Engine: TGocciaEngine;
  EngineResult: TGocciaScriptResult;
begin
  ScriptResult := CreateDefaultScriptResult;

  Source := nil;
  try
    if Assigned(APreloadedSource) then
      Source := APreloadedSource
    else
    begin
      try
        Source := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
      except
        on E: EStreamError do
        begin
          if not GIsWorkerThread then
            WriteLn('Error loading test file: ', E.Message);
          MarkLoadError(ScriptResult, AFileName, E.Message);
          Result := MakeEmptyTestResult(ScriptResult, E.Message);
          Exit;
        end;
      end;
    end;

    Source.Add(Format('runTests({ exitOnFirstFailure: %s, showTestResults: false });',
      [BoolToStr(FExitOnFirst.Present, 'true', 'false')]));

    try
      Engine := CreateEngine(AFileName, Source);
      try
        if FSilent.Present or GIsWorkerThread then
        begin
          Engine.BuiltinConsole.Enabled := False;
          Engine.SuppressWarnings := True;
        end;

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(DEFAULT_TIMEOUT_MS));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          EngineResult := Engine.Execute;
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;
      finally
        Engine.Free;
      end;
      Result.Timing := EngineResult;
      Result.ErrorMessage := '';
      FileResult := EngineResult.Result as TGocciaObjectValue;
      MergeFileResult(ScriptResult, FileResult);
      Result.TestResult := ScriptResult;
    except
      on E: Exception do
      begin
        if E is TGocciaError then
        begin
          if not GIsWorkerThread then
            WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal));
          MarkLoadError(ScriptResult, AFileName, TGocciaError(E).GetDetailedMessage);
          Result := MakeEmptyTestResult(ScriptResult,
            TGocciaError(E).GetDetailedMessage);
        end
        else if E is TGocciaThrowValue then
        begin
          if not GIsWorkerThread then
            WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, IsColorTerminal, TGocciaThrowValue(E).Suggestion));
          MarkLoadError(ScriptResult, AFileName,
            FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, False, TGocciaThrowValue(E).Suggestion));
          Result := MakeEmptyTestResult(ScriptResult,
            FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source,
              False, TGocciaThrowValue(E).Suggestion));
        end
        else
        begin
          if not GIsWorkerThread then
            WriteLn('Fatal error: ', E.Message);
          MarkLoadError(ScriptResult, AFileName, E.Message);
          Result := MakeEmptyTestResult(ScriptResult, E.Message);
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function TTestRunnerApp.RunGocciaScriptBytecode(const AFileName: string;
  APreloadedSource: TStringList): TTestFileResult;
var
  Source: TStringList;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  SourceMap: TGocciaSourceMap;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  Warning: TGocciaParserWarning;
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  ScriptResult: TGocciaObjectValue;
  ResultValue: TGocciaValue;
  OrigLine, OrigCol, I: Integer;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  ScriptResult := CreateDefaultScriptResult;

  Source := nil;
  try
    if Assigned(APreloadedSource) then
      Source := APreloadedSource
    else
    begin
      try
        Source := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
      except
        on E: EStreamError do
        begin
          if not GIsWorkerThread then
            WriteLn('Error loading test file: ', E.Message);
          MarkLoadError(ScriptResult, AFileName, E.Message);
          Result := MakeEmptyTestResult(ScriptResult, E.Message);
          Exit;
        end;
      end;
    end;

    Source.Add(Format('runTests({ exitOnFirstFailure: %s, showTestResults: false });',
      [BoolToStr(FExitOnFirst.Present, 'true', 'false')]));

    SourceText := StringListToLFText(Source);
    SourceMap := nil;
    if ppJSX in TGocciaEngine.DefaultPreprocessors then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(SourceText);
      SourceText := JSXResult.Source;
      SourceMap := JSXResult.SourceMap;
    end;

    try try
      Executor := TGocciaBytecodeExecutor.Create;
      try
        Engine := CreateEngine(AFileName, Source, Executor);
        try
          if FSilent.Present or GIsWorkerThread then
          begin
            Engine.BuiltinConsole.Enabled := False;
            Engine.SuppressWarnings := True;
          end;

            LexStart := GetNanoseconds;
            Lexer := TGocciaLexer.Create(SourceText, AFileName);
            try
              Tokens := Lexer.ScanTokens;
              LexEnd := GetNanoseconds;

              Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
              Parser.AutomaticSemicolonInsertion := Engine.ASIEnabled;
              Parser.VarDeclarationsEnabled := Engine.VarEnabled;
              Parser.FunctionDeclarationsEnabled := Engine.FunctionEnabled;
              try
                ProgramNode := Parser.Parse;
                ParseEnd := GetNanoseconds;

                if Assigned(TGocciaCoverageTracker.Instance) and
                   TGocciaCoverageTracker.Instance.Enabled then
                begin
                  TGocciaCoverageTracker.Instance.RegisterSourceFile(
                    AFileName, CountExecutableLines(Lexer.SourceLines));
                  if Assigned(SourceMap) then
                    TGocciaCoverageTracker.Instance.RegisterSourceMap(
                      AFileName, SourceMap.Clone);
                end;

                if (not FSilent.Present) and (not GIsWorkerThread) then
                  for I := 0 to Parser.WarningCount - 1 do
                  begin
                    Warning := Parser.GetWarning(I);
                    WriteLn(Format('Warning: %s', [Warning.Message]));
                    if Warning.Suggestion <> '' then
                      WriteLn(Format('  Suggestion: %s', [Warning.Suggestion]));
                    if Assigned(SourceMap) and SourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
                      WriteLn(Format('  --> %s:%d:%d', [AFileName, OrigLine, OrigCol]))
                    else
                      WriteLn(Format('  --> %s:%d:%d', [AFileName, Warning.Line, Warning.Column]));
                  end;
                try
                  Module := TGocciaBytecodeExecutor(Engine.Executor).CompileToModule(ProgramNode);
                  CompileEnd := GetNanoseconds;
                finally
                  ProgramNode.Free;
                end;
              finally
                Parser.Free;
              end;
            finally
              Lexer.Free;
            end;

            StartExecutionTimeout(EngineOptions.Timeout.ValueOr(DEFAULT_TIMEOUT_MS));
            StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
            try
              try
                ResultValue := TGocciaBytecodeExecutor(Engine.Executor).RunModule(Module);
              finally
                ClearExecutionTimeout;
                ClearInstructionLimit;
              end;
              ExecEnd := GetNanoseconds;

              if ResultValue is TGocciaObjectValue then
                MergeFileResult(ScriptResult, TGocciaObjectValue(ResultValue));

              Result.TestResult := ScriptResult;
              Result.Timing.Result := ResultValue;
              Result.Timing.LexTimeNanoseconds := LexEnd - LexStart;
              Result.Timing.ParseTimeNanoseconds := ParseEnd - LexEnd;
              Result.Timing.CompileTimeNanoseconds := CompileEnd - ParseEnd;
              Result.Timing.ExecuteTimeNanoseconds := ExecEnd - CompileEnd;
              Result.Timing.TotalTimeNanoseconds := ExecEnd - LexStart;
              Result.Timing.FileName := AFileName;
              Result.ErrorMessage := '';
            finally
              Module.Free;
            end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    except
      on E: Exception do
      begin
        if E is TGocciaError then
        begin
          if not GIsWorkerThread then
            WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal));
          MarkLoadError(ScriptResult, AFileName, TGocciaError(E).GetDetailedMessage);
          Result := MakeEmptyTestResult(ScriptResult,
            TGocciaError(E).GetDetailedMessage);
        end
        else if E is TGocciaThrowValue then
        begin
          if not GIsWorkerThread then
            WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, IsColorTerminal, TGocciaThrowValue(E).Suggestion));
          MarkLoadError(ScriptResult, AFileName,
            FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, False, TGocciaThrowValue(E).Suggestion));
          Result := MakeEmptyTestResult(ScriptResult,
            FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source,
              False, TGocciaThrowValue(E).Suggestion));
        end
        else
        begin
          if not GIsWorkerThread then
            WriteLn('Fatal error: ', E.Message);
          MarkLoadError(ScriptResult, AFileName, E.Message);
          Result := MakeEmptyTestResult(ScriptResult, E.Message);
        end;
      end;
    end;
    finally
      SourceMap.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TTestRunnerApp.RunGocciaScript(const AFileName: string;
  APreloadedSource: TStringList): TTestFileResult;
begin
  case EngineOptions.Mode.ValueOr(emInterpreted) of
    emInterpreted: Result := RunGocciaScriptInterpreted(AFileName, APreloadedSource);
    emBytecode:    Result := RunGocciaScriptBytecode(AFileName, APreloadedSource);
  end;
end;

function TTestRunnerApp.RunScriptFromFile(const AFileName: string;
  APreloadedSource: TStringList): TAggregatedTestResult;
var
  FileResult: TTestFileResult;
  FileFailedTests: TGocciaValue;
  J: Integer;
begin
  Result.JobCount := 1;
  Result.TestResult := nil;
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalCompileNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;
  { A one-file invocation still needs a per-file record in the JSON
    output so downstream harnesses (e.g. the test262 driver, which
    funnels a lone filter match through here) can find the result
    they're expecting. }
  SetLength(Result.FileResults, 1);
  Result.FileResults[0].FileName := AFileName;
  try
    FileResult := RunGocciaScript(AFileName, APreloadedSource);
    Result.TestResult := FileResult.TestResult;
    Result.TotalLexNanoseconds := FileResult.Timing.LexTimeNanoseconds;
    Result.TotalParseNanoseconds := FileResult.Timing.ParseTimeNanoseconds;
    Result.TotalCompileNanoseconds := FileResult.Timing.CompileTimeNanoseconds;
    Result.TotalExecNanoseconds := FileResult.Timing.ExecuteTimeNanoseconds;
    Result.FileResults[0].LexTimeNanoseconds := FileResult.Timing.LexTimeNanoseconds;
    Result.FileResults[0].ParseTimeNanoseconds := FileResult.Timing.ParseTimeNanoseconds;
    Result.FileResults[0].CompileTimeNanoseconds := FileResult.Timing.CompileTimeNanoseconds;
    Result.FileResults[0].ExecuteTimeNanoseconds := FileResult.Timing.ExecuteTimeNanoseconds;
    Result.FileResults[0].ErrorMessage := FileResult.ErrorMessage;
    if Assigned(FileResult.TestResult) then
    begin
      Result.FileResults[0].Passed :=
        FileResult.TestResult.GetProperty('passed').ToNumberLiteral.Value;
      Result.FileResults[0].Failed :=
        FileResult.TestResult.GetProperty('failed').ToNumberLiteral.Value;
      Result.FileResults[0].Skipped :=
        FileResult.TestResult.GetProperty('skipped').ToNumberLiteral.Value;
      Result.FileResults[0].TotalTests :=
        FileResult.TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value;
      FileFailedTests := FileResult.TestResult.GetProperty('failedTests');
      if FileFailedTests is TGocciaArrayValue then
      begin
        SetLength(Result.FileResults[0].FailedTests,
          TGocciaArrayValue(FileFailedTests).Elements.Count);
        for J := 0 to TGocciaArrayValue(FileFailedTests).Elements.Count - 1 do
          Result.FileResults[0].FailedTests[J] :=
            TGocciaArrayValue(FileFailedTests).Elements[J].ToStringLiteral.Value;
      end;
    end;
  except
    on E: Exception do
    begin
      if E is TGocciaError then
        WriteLn(StdErr, TGocciaError(E).GetDetailedMessage(IsColorTerminal))
      else
        WriteLn(StdErr, 'Fatal error: ', E.Message);
      { Synthesize a one-failed-file TestResult so PrintTestResults still
        emits JSON for the file and the sequential aggregator does not
        drop the slot at its `if FileResult.TestResult = nil then
        Continue` filter. Without this, fatal host-side failures (e.g. a
        compile-time error before the test harness has a chance to
        register anything) silently disappear from the aggregated
        output. }
      Result.TestResult := CreateDefaultScriptResult;
      Result.TestResult.AssignProperty('failed',
        TGocciaNumberLiteralValue.Create(1));
      Result.FileResults[0].ErrorMessage := E.Message;
      Result.FileResults[0].Failed := 1;
      Result.FileResults[0].TotalTests := 1;
      SetLength(Result.FileResults[0].FailedTests, 1);
      Result.FileResults[0].FailedTests[0] := AFileName + ': ' + E.Message;
      ExitCode := 1;
    end;
  end;
end;

function TTestRunnerApp.RunScriptsFromFiles(const AFiles: TStringList): TAggregatedTestResult;
var
  GC: TGarbageCollector;
  I, J: Integer;
  ProcessedCount: Integer;
  AllTestResults: TGocciaObjectValue;
  AllFailedTests: TGocciaArrayValue;
  FileResult: TAggregatedTestResult;
  FileFailedTests: TGocciaValue;
  PassedCount, FailedCount, SkippedCount, TotalRunCount, TotalAssertions, TotalDuration: Double;
begin
  GC := TGarbageCollector.Instance;

  AllTestResults := TGocciaObjectValue.Create;
  AllFailedTests := TGocciaArrayValue.Create;

  if Assigned(GC) then
  begin
    GC.AddTempRoot(AllTestResults);
    GC.AddTempRoot(AllFailedTests);
  end;

  AllTestResults.AssignProperty('totalTests', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('failedTests', AllFailedTests);

  PassedCount := 0;
  FailedCount := 0;
  SkippedCount := 0;
  TotalRunCount := 0;
  TotalAssertions := 0;
  TotalDuration := 0;
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalCompileNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;
  // Preallocate to AFiles.Count and track the count actually populated;
  // FExitOnFirst Break and the FileResult.TestResult = nil Continue both
  // leave entries unwritten, and serialising those zero-filled slots
  // produced bogus "results" rows in the JSON output.  Resize at the end
  // to the true populated count so only real records are emitted.
  SetLength(Result.FileResults, AFiles.Count);
  ProcessedCount := 0;

  for I := 0 to AFiles.Count - 1 do
  begin
    if not FNoProgress.Present then
      WriteLn(SysUtils.Format('[%d/%d] %s', [I + 1, AFiles.Count, AFiles[I]]));
    FileResult := RunScriptFromFile(AFiles[I]);
    if FileResult.TestResult = nil then
      Continue;

    Result.TotalLexNanoseconds := Result.TotalLexNanoseconds + FileResult.TotalLexNanoseconds;
    Result.TotalParseNanoseconds := Result.TotalParseNanoseconds + FileResult.TotalParseNanoseconds;
    Result.TotalCompileNanoseconds := Result.TotalCompileNanoseconds + FileResult.TotalCompileNanoseconds;
    Result.TotalExecNanoseconds := Result.TotalExecNanoseconds + FileResult.TotalExecNanoseconds;

    PassedCount := PassedCount + FileResult.TestResult.GetProperty('passed').ToNumberLiteral.Value;
    FailedCount := FailedCount + FileResult.TestResult.GetProperty('failed').ToNumberLiteral.Value;
    SkippedCount := SkippedCount + FileResult.TestResult.GetProperty('skipped').ToNumberLiteral.Value;
    TotalRunCount := TotalRunCount + FileResult.TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value;
    TotalDuration := TotalDuration + FileResult.TestResult.GetProperty('duration').ToNumberLiteral.Value;
    TotalAssertions := TotalAssertions + FileResult.TestResult.GetProperty('assertions').ToNumberLiteral.Value;

    FileFailedTests := FileResult.TestResult.GetProperty('failedTests');
    if FileFailedTests is TGocciaArrayValue then
      for J := 0 to TGocciaArrayValue(FileFailedTests).Elements.Count - 1 do
        AllFailedTests.Elements.Add(TGocciaArrayValue(FileFailedTests).Elements[J]);

    { Per-file record for the JSON output — same schema as the parallel
      path so consumers do not have to branch on job count. }
    Result.FileResults[ProcessedCount].FileName := AFiles[I];
    Result.FileResults[ProcessedCount].LexTimeNanoseconds :=
      FileResult.TotalLexNanoseconds;
    Result.FileResults[ProcessedCount].ParseTimeNanoseconds :=
      FileResult.TotalParseNanoseconds;
    Result.FileResults[ProcessedCount].CompileTimeNanoseconds :=
      FileResult.TotalCompileNanoseconds;
    Result.FileResults[ProcessedCount].ExecuteTimeNanoseconds :=
      FileResult.TotalExecNanoseconds;
    Result.FileResults[ProcessedCount].Passed :=
      FileResult.TestResult.GetProperty('passed').ToNumberLiteral.Value;
    Result.FileResults[ProcessedCount].Failed :=
      FileResult.TestResult.GetProperty('failed').ToNumberLiteral.Value;
    Result.FileResults[ProcessedCount].Skipped :=
      FileResult.TestResult.GetProperty('skipped').ToNumberLiteral.Value;
    Result.FileResults[ProcessedCount].TotalTests :=
      FileResult.TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value;
    if Length(FileResult.FileResults) > 0 then
      Result.FileResults[ProcessedCount].ErrorMessage :=
        FileResult.FileResults[0].ErrorMessage;
    if FileFailedTests is TGocciaArrayValue then
    begin
      SetLength(Result.FileResults[ProcessedCount].FailedTests,
        TGocciaArrayValue(FileFailedTests).Elements.Count);
      for J := 0 to TGocciaArrayValue(FileFailedTests).Elements.Count - 1 do
        Result.FileResults[ProcessedCount].FailedTests[J] :=
          TGocciaArrayValue(FileFailedTests).Elements[J].ToStringLiteral.Value;
    end;
    Inc(ProcessedCount);

    if Assigned(GC) then
      GC.Collect;

    if FExitOnFirst.Present and (FailedCount > 0) then
      Break;
  end;
  SetLength(Result.FileResults, ProcessedCount);

  if Assigned(GC) then
  begin
    GC.RemoveTempRoot(AllTestResults);
    GC.RemoveTempRoot(AllFailedTests);
  end;

  AllTestResults.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(AFiles.Count * 1.0));
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.Create(PassedCount));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.Create(FailedCount));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(SkippedCount));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(TotalRunCount));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.Create(TotalDuration));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(TotalAssertions));

  Result.JobCount := 1;
  Result.TestResult := AllTestResults;
end;

{ Worker procedure executed on each thread for a single file.
  Runs the script, extracts numeric results into a thread-safe record,
  and frees all GC-managed objects before returning. }
procedure TTestRunnerApp.TestWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
var
  FileResult: TTestFileResult;
  TestResult: TGocciaObjectValue;
  FailedTests: TGocciaValue;
  FailedArr: TGocciaArrayValue;
  WorkerResults: PTestWorkerDataArray;
  I: Integer;
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  WorkerResults := PTestWorkerDataArray(AData);

  try
    FileResult := RunGocciaScript(AFileName);
    TestResult := FileResult.TestResult;

    if TestResult <> nil then
    begin
      WorkerResults^[AIndex].Passed := TestResult.GetProperty('passed').ToNumberLiteral.Value;
      WorkerResults^[AIndex].Failed := TestResult.GetProperty('failed').ToNumberLiteral.Value;
      WorkerResults^[AIndex].Skipped := TestResult.GetProperty('skipped').ToNumberLiteral.Value;
      WorkerResults^[AIndex].TotalRunTests := TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value;
      WorkerResults^[AIndex].Assertions := TestResult.GetProperty('assertions').ToNumberLiteral.Value;
      WorkerResults^[AIndex].Duration := TestResult.GetProperty('duration').ToNumberLiteral.Value;
      WorkerResults^[AIndex].LexNs := FileResult.Timing.LexTimeNanoseconds;
      WorkerResults^[AIndex].ParseNs := FileResult.Timing.ParseTimeNanoseconds;
      WorkerResults^[AIndex].CompileNs := FileResult.Timing.CompileTimeNanoseconds;
      WorkerResults^[AIndex].ExecNs := FileResult.Timing.ExecuteTimeNanoseconds;
      WorkerResults^[AIndex].ErrorMessage := FileResult.ErrorMessage;

      FailedTests := TestResult.GetProperty('failedTests');
      if FailedTests is TGocciaArrayValue then
      begin
        FailedArr := TGocciaArrayValue(FailedTests);
        SetLength(WorkerResults^[AIndex].FailedTestNames, FailedArr.Elements.Count);
        for I := 0 to FailedArr.Elements.Count - 1 do
          WorkerResults^[AIndex].FailedTestNames[I] := FailedArr.Elements[I].ToStringLiteral.Value;
      end;
    end
    else
      WorkerResults^[AIndex].ErrorMessage := 'Test result was nil';
  except
    on E: TGocciaError do
    begin
      WorkerResults^[AIndex].ErrorMessage := E.GetDetailedMessage;
      WorkerResults^[AIndex].Failed := 1;
      WorkerResults^[AIndex].TotalRunTests := 1;
      SetLength(WorkerResults^[AIndex].FailedTestNames, 1);
      WorkerResults^[AIndex].FailedTestNames[0] := AFileName + ': ' + E.GetDetailedMessage;
    end;
    on E: TGocciaThrowValue do
    begin
      WorkerResults^[AIndex].ErrorMessage := E.Message;
      WorkerResults^[AIndex].Failed := 1;
      WorkerResults^[AIndex].TotalRunTests := 1;
      SetLength(WorkerResults^[AIndex].FailedTestNames, 1);
      WorkerResults^[AIndex].FailedTestNames[0] := AFileName + ': ' + E.Message;
    end;
    on E: Exception do
    begin
      WorkerResults^[AIndex].ErrorMessage := E.Message;
      WorkerResults^[AIndex].Failed := 1;
      WorkerResults^[AIndex].TotalRunTests := 1;
      SetLength(WorkerResults^[AIndex].FailedTestNames, 1);
      WorkerResults^[AIndex].FailedTestNames[0] := AFileName + ': ' + E.Message;
    end;
  end;

  // No GC.Collect — worker GC is disabled to avoid FGCMark races on
  // shared objects. All thread-local objects are freed in bulk when
  // ShutdownThreadRuntime destroys the thread-local GC.
end;

function TTestRunnerApp.RunScriptsFromFilesParallel(
  const AFiles: TStringList; const AJobCount: Integer): TAggregatedTestResult;
var
  Pool: TGocciaThreadPool;
  WorkerData: array of TTestWorkerData;
  { Synthesised results for orphaned slots.  We deliberately keep these in a
    main-thread-only array instead of writing them into WorkerData, because
    an abandoned worker that unsticks later in native code will resume
    writing to its own WorkerData[AIndex] slot — assigning to managed types
    (string, dynamic array of string) concurrently with the main thread's
    aggregation reads is a refcount race we cannot tolerate. }
  OrphanResults: array of TTestWorkerData;
  IsOrphan: array of Boolean;
  Source: ^TTestWorkerData;
  GC: TGarbageCollector;
  I, J: Integer;
  AllTestResults: TGocciaObjectValue;
  AllFailedTests: TGocciaArrayValue;
  PassedCount, FailedCount, SkippedCount, TotalRunCount, TotalAssertions: Double;
  WallClockStart, WallClockDuration: Int64;
  EffectiveTimeoutMs: Integer;
  WatchdogMs: Integer;
begin
  SetLength(WorkerData, AFiles.Count);
  for I := 0 to AFiles.Count - 1 do
  begin
    WorkerData[I].Passed := 0;
    WorkerData[I].Failed := 0;
    WorkerData[I].Skipped := 0;
    WorkerData[I].TotalRunTests := 0;
    WorkerData[I].Assertions := 0;
    WorkerData[I].Duration := 0;
    WorkerData[I].LexNs := 0;
    WorkerData[I].ParseNs := 0;
    WorkerData[I].CompileNs := 0;
    WorkerData[I].ExecNs := 0;
    WorkerData[I].ErrorMessage := '';
    SetLength(WorkerData[I].FailedTestNames, 0);
  end;

  // Force all shared prototypes to be initialised on the main thread
  // before any worker thread starts, avoiding class-var race conditions.
  EnsureSharedPrototypesInitialized(EffectiveBuiltins);

  WallClockStart := GetNanoseconds;

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
    Pool.CancelOnError := FExitOnFirst.Present;
    Pool.EnableCoverage := CoverageOptions.Enabled.Present;
    if Assigned(TGarbageCollector.Instance) then
      Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
    // Per-worker-idle watchdog: the pool cancels if any single worker
    // sits on one file longer than this window.  2× the per-file timeout
    // gives the in-engine cooperative timeout slack to fire first; the
    // +10 s grace covers thread-runtime initialisation on the first
    // file.  Because the pool measures per-worker idleness, this bound
    // does not depend on the file count — a huge batch is no looser
    // than a small one.
    // When the effective timeout is 0 (user passed --timeout=0), disable
    // the watchdog entirely so the no-timeout contract is honoured.
    EffectiveTimeoutMs := EngineOptions.Timeout.ValueOr(DEFAULT_TIMEOUT_MS);
    if EffectiveTimeoutMs > 0 then
      WatchdogMs := 2 * EffectiveTimeoutMs + 10000
    else
      WatchdogMs := 0;
    Pool.RunAll(AFiles, TestWorkerProc, @WorkerData[0], WatchdogMs);
    if Pool.EnableCoverage and Assigned(TGocciaCoverageTracker.Instance) then
      Pool.MergeCoverageInto(TGocciaCoverageTracker.Instance);

    { Cross-reference Pool.Results with WorkerData. Two silent-drop
      failure modes have to be surfaced here:

      1. Watchdog cancels mid-batch: the stuck worker does not return,
         so other live workers keep draining the queue but mark each
         dequeued file as 'Cancelled' in Pool.Results — without ever
         invoking TestWorkerProc for it.  Pool.Results[I].Success is
         False and ErrorMessage is 'Cancelled'.

      2. Stuck worker's in-progress file: the thread pool skips live
         (unfinished) workers when collecting results, so the file the
         stuck worker was running on never makes it into Pool.Results
         at all — its slot stays at the default zero-initialised state
         (Success=False, ErrorMessage='').

      Helper / import-only files (e.g. tests/language/modules/helpers/*)
      legitimately run to completion without registering any tests, so
      their WorkerData is all zeros too.  Pool.Results[I].Success is
      True for those — the Success flag is the discriminator, and only
      Success=False slots get rewritten. }
    SetLength(OrphanResults, AFiles.Count);
    SetLength(IsOrphan, AFiles.Count);
    for I := 0 to AFiles.Count - 1 do
    begin
      if (I > High(Pool.Results)) or Pool.Results[I].Success then
        Continue;

      if Pool.Results[I].ErrorMessage <> '' then
      begin
        if WorkerData[I].ErrorMessage = '' then
        begin
          { Watchdog-cancelled slot: the worker for this file never ran (a
            peer stalled), so WorkerData[I] is still the zero-initialised
            default and no concurrent writer touches it any more.  Safe to
            keep the synthesis in WorkerData. }
          WorkerData[I].ErrorMessage := Pool.Results[I].ErrorMessage;
          WorkerData[I].Failed := 1;
          WorkerData[I].TotalRunTests := 1;
          SetLength(WorkerData[I].FailedTestNames, 1);
          WorkerData[I].FailedTestNames[0] := AFiles[I] + ': ' +
            Pool.Results[I].ErrorMessage;
        end;
      end
      else if (WorkerData[I].ErrorMessage = '') and
              (WorkerData[I].Passed = 0) and
              (WorkerData[I].Failed = 0) and
              (WorkerData[I].TotalRunTests = 0) then
      begin
        { Stuck-worker slot: the worker is still alive in native code and
          may eventually unstick and resume writing to WorkerData[I]
          itself.  Park the synthesis in OrphanResults[I] so we never
          touch the managed fields the orphan owns. }
        IsOrphan[I] := True;
        OrphanResults[I].ErrorMessage :=
          'Worker produced no result (likely stalled in native code and ' +
          'was orphaned by the watchdog)';
        OrphanResults[I].Failed := 1;
        OrphanResults[I].TotalRunTests := 1;
        SetLength(OrphanResults[I].FailedTestNames, 1);
        OrphanResults[I].FailedTestNames[0] := AFiles[I] + ': ' +
          OrphanResults[I].ErrorMessage;
      end;
    end;
  finally
    Pool.Free;
  end;

  WallClockDuration := GetNanoseconds - WallClockStart;

  // Aggregate results on the main thread using GC-managed objects
  GC := TGarbageCollector.Instance;

  AllTestResults := TGocciaObjectValue.Create;
  AllFailedTests := TGocciaArrayValue.Create;

  if Assigned(GC) then
  begin
    GC.AddTempRoot(AllTestResults);
    GC.AddTempRoot(AllFailedTests);
  end;

  PassedCount := 0;
  FailedCount := 0;
  SkippedCount := 0;
  TotalRunCount := 0;
  TotalAssertions := 0;
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalCompileNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;
  SetLength(Result.FileResults, AFiles.Count);

  for I := 0 to AFiles.Count - 1 do
  begin
    if not FNoProgress.Present then
      WriteLn(SysUtils.Format('[%d/%d] %s', [I + 1, AFiles.Count, AFiles[I]]));

    { Source orphaned slots from OrphanResults so we never read the
      WorkerData[I] managed fields that an unsticking worker might still
      be writing to.  Every other slot reads from WorkerData as before. }
    if (I < Length(IsOrphan)) and IsOrphan[I] then
      Source := @OrphanResults[I]
    else
      Source := @WorkerData[I];

    if Source^.ErrorMessage <> '' then
      WriteLn(StdErr, Source^.ErrorMessage);

    PassedCount := PassedCount + Source^.Passed;
    FailedCount := FailedCount + Source^.Failed;
    SkippedCount := SkippedCount + Source^.Skipped;
    TotalRunCount := TotalRunCount + Source^.TotalRunTests;
    TotalAssertions := TotalAssertions + Source^.Assertions;

    Result.TotalLexNanoseconds := Result.TotalLexNanoseconds + Source^.LexNs;
    Result.TotalParseNanoseconds := Result.TotalParseNanoseconds + Source^.ParseNs;
    Result.TotalCompileNanoseconds := Result.TotalCompileNanoseconds + Source^.CompileNs;
    Result.TotalExecNanoseconds := Result.TotalExecNanoseconds + Source^.ExecNs;

    for J := 0 to High(Source^.FailedTestNames) do
      AllFailedTests.Elements.Add(
        TGocciaStringLiteralValue.Create(Source^.FailedTestNames[J]));

    { Per-file record for the JSON output. Copying strings by value
      keeps the aggregated result self-contained once WorkerData goes
      out of scope. }
    Result.FileResults[I].FileName := AFiles[I];
    Result.FileResults[I].LexTimeNanoseconds := Source^.LexNs;
    Result.FileResults[I].ParseTimeNanoseconds := Source^.ParseNs;
    Result.FileResults[I].CompileTimeNanoseconds := Source^.CompileNs;
    Result.FileResults[I].ExecuteTimeNanoseconds := Source^.ExecNs;
    Result.FileResults[I].Passed := Source^.Passed;
    Result.FileResults[I].Failed := Source^.Failed;
    Result.FileResults[I].Skipped := Source^.Skipped;
    Result.FileResults[I].TotalTests := Source^.TotalRunTests;
    Result.FileResults[I].ErrorMessage := Source^.ErrorMessage;
    SetLength(Result.FileResults[I].FailedTests,
      Length(Source^.FailedTestNames));
    for J := 0 to High(Source^.FailedTestNames) do
      Result.FileResults[I].FailedTests[J] := Source^.FailedTestNames[J];
  end;

  AllTestResults.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(AFiles.Count * 1.0));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(TotalRunCount));
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.Create(PassedCount));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.Create(FailedCount));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(SkippedCount));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.Create(WallClockDuration));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(TotalAssertions));
  AllTestResults.AssignProperty('failedTests', AllFailedTests);

  if Assigned(GC) then
  begin
    GC.RemoveTempRoot(AllTestResults);
    GC.RemoveTempRoot(AllFailedTests);
  end;

  Result.JobCount := AJobCount;
  Result.TestResult := AllTestResults;
end;

procedure TTestRunnerApp.WriteResultsJSON(const AResult: TAggregatedTestResult; const AFileName: string);
var
  Lines: TStringList;
  TotalNanoseconds: Int64;
  IsBytecodeMode: Boolean;
  FailedCount: Integer;
  Timing: TCLIJSONTiming;
begin
  IsBytecodeMode := EngineOptions.Mode.Matches(emBytecode);
  FailedCount := Round(AResult.TestResult.GetProperty('failed').ToNumberLiteral.Value);
  if IsBytecodeMode then
    TotalNanoseconds := AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds
  else
    TotalNanoseconds := AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds;
  Timing.LexTimeNanoseconds := AResult.TotalLexNanoseconds;
  Timing.ParseTimeNanoseconds := AResult.TotalParseNanoseconds;
  Timing.CompileTimeNanoseconds := AResult.TotalCompileNanoseconds;
  Timing.ExecuteTimeNanoseconds := AResult.TotalExecNanoseconds;
  Timing.TotalTimeNanoseconds := TotalNanoseconds;

  Lines := TStringList.Create;
  try
    Lines.Add('{');
    Lines.Add('  ' + BuildCLIBuildJSON + ',');
    Lines.Add(Format('  "ok": %s,', [BoolToStr(FailedCount = 0, 'true', 'false')]));
    Lines.Add('  "stdout": "",');
    Lines.Add('  "stderr": "",');
    Lines.Add('  ' + BuildCLIOutputJSON('') + ',');
    Lines.Add('  "error": null,');
    Lines.Add(Format('  "mode": "%s",', [IfThen(IsBytecodeMode, 'bytecode', 'interpreted')]));
    Lines.Add(Format('  "jobCount": %d,', [AResult.JobCount]));
    Lines.Add(Format('  "totalFiles": %d,', [Round(AResult.TestResult.GetProperty('totalTests').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "totalTests": %d,', [Round(AResult.TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "passed": %d,', [Round(AResult.TestResult.GetProperty('passed').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "failed": %d,', [Round(AResult.TestResult.GetProperty('failed').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "skipped": %d,', [Round(AResult.TestResult.GetProperty('skipped').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "assertions": %d,', [Round(AResult.TestResult.GetProperty('assertions').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "durationNanoseconds": %d,', [Round(AResult.TestResult.GetProperty('duration').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "lexTimeNanoseconds": %d,', [AResult.TotalLexNanoseconds]));
    Lines.Add(Format('  "parseTimeNanoseconds": %d,', [AResult.TotalParseNanoseconds]));
    if IsBytecodeMode then
      Lines.Add(Format('  "compileTimeNanoseconds": %d,', [AResult.TotalCompileNanoseconds]));
    Lines.Add(Format('  "executeTimeNanoseconds": %d,', [AResult.TotalExecNanoseconds]));
    Lines.Add(Format('  "totalEngineNanoseconds": %d,', [TotalNanoseconds]));
    Lines.Add('  ' + BuildCLITimingJSON(Timing) + ',');
    Lines.Add('  ' + BuildCLIMemoryJSON(AResult.MemoryStats) + ',');
    Lines.Add('  ' + BuildCLIWorkersJSON(AResult.JobCount, AResult.JobCount) + ',');

    { Per-file results. External harnesses (the test262 driver) use
      this to map each input file to an explicit pass/fail outcome
      with its own failedTests list — there is no top-level aggregate.
      When no files were run this is an empty array.

      A previous version emitted a redundant top-level "failedTests"
      array reading from AResult.TestResult.GetProperty, but that read
      raced with abandoned-worker writes during test262 runs and
      occasionally produced torn AnsiString refcount reads, corrupting
      the serialised JSON. The per-file records below carry the same
      information and are written from main-thread-owned data only. }
    WriteFileResults(Lines, AResult, 'files', True);
    WriteFileResults(Lines, AResult, 'results', False);

    Lines.Add('}');
    Lines.SaveToFile(AFileName);
  finally
    Lines.Free;
  end;
end;

procedure TTestRunnerApp.WriteFileResults(ALines: TStringList;
  const AResult: TAggregatedTestResult; const APropertyName: string;
  const ATrailingComma: Boolean);
var
  I, J, Count: Integer;
  Fr: TTestFilePerResult;
  Entry, FailedList: TStringBuilder;
  Timing: TCLIJSONTiming;
  ErrorInfo: TCLIJSONErrorInfo;
  ErrorJSON: string;
begin
  ALines.Add(Format('  "%s": [', [EscapeJSONString(APropertyName)]));
  Count := Length(AResult.FileResults);
  if Count = 0 then
  begin
    ALines.Add('  ]');
    if ATrailingComma then
      ALines[ALines.Count - 1] := ALines[ALines.Count - 1] + ',';
    Exit;
  end;

  Entry := TStringBuilder.Create;
  FailedList := TStringBuilder.Create;
  try
    for I := 0 to Count - 1 do
    begin
      Fr := AResult.FileResults[I];
      FailedList.Clear;
      for J := 0 to High(Fr.FailedTests) do
      begin
        if J > 0 then FailedList.Append(',');
        FailedList.Append('"');
        FailedList.Append(EscapeJSONString(Fr.FailedTests[J]));
        FailedList.Append('"');
      end;

      Entry.Clear;
      Timing.LexTimeNanoseconds := Fr.LexTimeNanoseconds;
      Timing.ParseTimeNanoseconds := Fr.ParseTimeNanoseconds;
      Timing.CompileTimeNanoseconds := Fr.CompileTimeNanoseconds;
      Timing.ExecuteTimeNanoseconds := Fr.ExecuteTimeNanoseconds;
      Timing.TotalTimeNanoseconds := Fr.LexTimeNanoseconds +
        Fr.ParseTimeNanoseconds + Fr.CompileTimeNanoseconds +
        Fr.ExecuteTimeNanoseconds;
      if Fr.ErrorMessage = '' then
        ErrorJSON := 'null'
      else
      begin
        ErrorInfo := DefaultCLIJSONErrorInfo;
        ErrorInfo.Message := Fr.ErrorMessage;
        ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
      end;

      Entry.Append('    {');
      Entry.Append(BuildCLIFileBaseJSON(Fr.FileName,
        (Fr.Failed = 0) and (Fr.ErrorMessage = ''), '', '', '', ErrorJSON,
        Timing, '"memory":null'));
      Entry.Append(', ');
      Entry.Append('"passed": ');
      Entry.Append(Round(Fr.Passed));
      Entry.Append(', "failed": ');
      Entry.Append(Round(Fr.Failed));
      Entry.Append(', "skipped": ');
      Entry.Append(Round(Fr.Skipped));
      Entry.Append(', "totalTests": ');
      Entry.Append(Round(Fr.TotalTests));
      Entry.Append(', "errorMessage": "');
      Entry.Append(EscapeJSONString(Fr.ErrorMessage));
      Entry.Append('", "failedTests": [');
      Entry.Append(FailedList.ToString);
      Entry.Append(']}');
      if I < Count - 1 then
        Entry.Append(',');
      ALines.Add(Entry.ToString);
    end;
  finally
    Entry.Free;
    FailedList.Free;
  end;
  ALines.Add('  ]');
  if ATrailingComma then
    ALines[ALines.Count - 1] := ALines[ALines.Count - 1] + ',';
end;

procedure TTestRunnerApp.PrintTestResults(const AResult: TAggregatedTestResult);
var
  TestResult: TGocciaObjectValue;
  TotalRunTests: String;
  TotalPassed: String;
  TotalFailed: String;
  TotalSkipped: String;
  TotalAssertions: String;
  DurationNanoseconds: Int64;
  RunCount: Double;
  PerTestNanoseconds: Int64;
  IsBytecodeMode: Boolean;
  IsParallel: Boolean;
  CurrentOutputFile: string;
begin
  ExitCode := 0;
  TestResult := AResult.TestResult;
  if TestResult = nil then
    Exit;

  TotalRunTests := TestResult.GetProperty('totalRunTests').ToStringLiteral.Value;
  TotalPassed := TestResult.GetProperty('passed').ToStringLiteral.Value;
  TotalFailed := TestResult.GetProperty('failed').ToStringLiteral.Value;
  TotalSkipped := TestResult.GetProperty('skipped').ToStringLiteral.Value;
  TotalAssertions := TestResult.GetProperty('assertions').ToStringLiteral.Value;
  DurationNanoseconds := Round(TestResult.GetProperty('duration').ToNumberLiteral.Value);
  RunCount := StrToFloat(TotalRunTests);

  IsBytecodeMode := EngineOptions.Mode.Matches(emBytecode);
  IsParallel := AResult.JobCount > 1;

  if not FNoResults.Present then
  begin
    Writeln('Test Results Test Files: ', TestResult.GetProperty('totalTests').ToStringLiteral.Value);
    Writeln(Format('Test Results Run Tests: %s', [TotalRunTests]));

    if RunCount > 0 then
    begin
      PerTestNanoseconds := Round(DurationNanoseconds / RunCount);
      Writeln(Format('Test Results Passed: %s (%2.2f%%)', [TotalPassed, (StrToFloat(TotalPassed) / RunCount * 100)]));
      Writeln(Format('Test Results Failed: %s (%2.2f%%)', [TotalFailed, (StrToFloat(TotalFailed) / RunCount * 100)]));
      Writeln(Format('Test Results Skipped: %s (%2.2f%%)', [TotalSkipped, (StrToFloat(TotalSkipped) / RunCount * 100)]));
      Writeln(Format('Test Results Assertions: %s', [TotalAssertions]));
      Writeln(Format('Test Results Test Duration: %s (%s/test)', [FormatDuration(DurationNanoseconds), FormatDuration(PerTestNanoseconds)]));
      if IsParallel then
      begin
        if IsBytecodeMode then
        begin
          Writeln(Format('Test Results Engine Timing (cumulative): Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
            [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds),
             FormatDuration(AResult.TotalCompileNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
             FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds)]));
          Writeln(Format('Test Results Engine Timing (avg/worker): Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
            [FormatDuration(AResult.TotalLexNanoseconds div AResult.JobCount),
             FormatDuration(AResult.TotalParseNanoseconds div AResult.JobCount),
             FormatDuration(AResult.TotalCompileNanoseconds div AResult.JobCount),
             FormatDuration(AResult.TotalExecNanoseconds div AResult.JobCount),
             FormatDuration((AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds) div AResult.JobCount)]));
        end
        else
        begin
          Writeln(Format('Test Results Engine Timing (cumulative): Lex: %s | Parse: %s | Execute: %s | Total: %s',
            [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds),
             FormatDuration(AResult.TotalExecNanoseconds),
             FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds)]));
          Writeln(Format('Test Results Engine Timing (avg/worker): Lex: %s | Parse: %s | Execute: %s | Total: %s',
            [FormatDuration(AResult.TotalLexNanoseconds div AResult.JobCount),
             FormatDuration(AResult.TotalParseNanoseconds div AResult.JobCount),
             FormatDuration(AResult.TotalExecNanoseconds div AResult.JobCount),
             FormatDuration((AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds) div AResult.JobCount)]));
        end;
      end
      else
      begin
        if IsBytecodeMode then
          Writeln(Format('Test Results Engine Timing: Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
            [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds),
             FormatDuration(AResult.TotalCompileNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
             FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds)]))
        else
          Writeln(Format('Test Results Engine Timing: Lex: %s | Parse: %s | Execute: %s | Total: %s',
            [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
             FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds)]));
      end;
      Writeln(Format('Test Results Failed Tests: %s', [TestResult.GetProperty('failedTests').ToStringLiteral.Value]));
    end;
  end;

  CurrentOutputFile := FOutputFile.ValueOr('');
  if CurrentOutputFile <> '' then
    WriteResultsJSON(AResult, CurrentOutputFile);

  if StrToFloat(TotalFailed) > 0 then
    ExitCode := 1;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TTestRunnerApp, 'GocciaTestRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
