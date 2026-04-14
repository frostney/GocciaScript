program TestRunner;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils,

  TimingUtils,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Builtins.TestConsole,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  Goccia.CLI.Options,
  Goccia.Compiler,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Engine,
  Goccia.Engine.BytecodeBackend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON.Utils,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.SourceMap,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TTestFileResult = record
    TestResult: TGocciaObjectValue;
    Timing: TGocciaScriptResult;
  end;

  TAggregatedTestResult = record
    TestResult: TGocciaObjectValue;
    TotalLexNanoseconds: Int64;
    TotalParseNanoseconds: Int64;
    TotalCompileNanoseconds: Int64;
    TotalExecNanoseconds: Int64;
  end;

  TTestRunnerApp = class(TGocciaCLIApplication)
  private
    FNoProgress: TGocciaFlagOption;
    FNoResults: TGocciaFlagOption;
    FExitOnFirst: TGocciaFlagOption;
    FSilent: TGocciaFlagOption;
    FOutputFile: TGocciaStringOption;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    function GlobalBuiltins: TGocciaGlobalBuiltins; override;
  private
    function RunGocciaScriptInterpreted(const AFileName: string): TTestFileResult;
    function RunGocciaScriptBytecode(const AFileName: string): TTestFileResult;
    function RunGocciaScript(const AFileName: string): TTestFileResult;
    function RunScriptFromFile(const AFileName: string): TAggregatedTestResult;
    function RunScriptsFromFiles(const AFiles: TStringList): TAggregatedTestResult;
    procedure WriteResultsJSON(const AResult: TAggregatedTestResult; const AFileName: string);
    procedure PrintTestResults(const AResult: TAggregatedTestResult);
  end;

function MakeEmptyTestResult(const AScriptResult: TGocciaObjectValue): TTestFileResult;
begin
  Result.TestResult := AScriptResult;
  Result.Timing.Result := nil;
  Result.Timing.LexTimeNanoseconds := 0;
  Result.Timing.ParseTimeNanoseconds := 0;
  Result.Timing.CompileTimeNanoseconds := 0;
  Result.Timing.ExecuteTimeNanoseconds := 0;
  Result.Timing.TotalTimeNanoseconds := 0;
  Result.Timing.FileName := '';
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
end;

procedure TTestRunnerApp.Validate;
begin
  if CoverageOptions.Format.Present or CoverageOptions.OutputPath.Present then
    CoverageOptions.Enabled.Apply('');
end;

function TTestRunnerApp.UsageLine: string;
begin
  Result := '<path...> [options]';
end;

procedure TTestRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  Files: TStringList;
  I: Integer;
begin
  if APaths.Count = 0 then
  begin
    WriteLn('Error: No paths specified. Run with --help for usage.');
    ExitCode := 1;
    Exit;
  end;

  Files := TStringList.Create;
  try
    for I := 0 to APaths.Count - 1 do
    begin
      if DirectoryExists(APaths[I]) then
        Files.AddStrings(FindAllFiles(APaths[I], ScriptExtensions))
      else if FileExists(APaths[I]) then
        Files.Add(APaths[I])
      else
      begin
        WriteLn('Error: Path not found: ', APaths[I]);
        ExitCode := 1;
        Exit;
      end;
    end;

    if Files.Count = 1 then
    begin
      if not FNoProgress.Present then
        WriteLn('[1/1] ', Files[0]);
      PrintTestResults(RunScriptFromFile(Files[0]));
    end
    else
      PrintTestResults(RunScriptsFromFiles(Files));

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
  end;
end;

function TTestRunnerApp.GlobalBuiltins: TGocciaGlobalBuiltins;
begin
  Result := [ggTestAssertions, ggFFI];
end;

function TTestRunnerApp.RunGocciaScriptInterpreted(const AFileName: string): TTestFileResult;
var
  Source: TStringList;
  ScriptResult, FileResult: TGocciaObjectValue;
  Engine: TGocciaEngine;
  SilentCon: TGocciaTestConsole;
  EngineResult: TGocciaScriptResult;
begin
  ScriptResult := CreateDefaultScriptResult;

  Source := nil;
  try
    try
      Source := ReadUTF8FileLines(AFileName);
    except
      on E: EStreamError do
      begin
        WriteLn('Error loading test file: ', E.Message);
        MarkLoadError(ScriptResult, AFileName, E.Message);
        Result := MakeEmptyTestResult(ScriptResult);
        Exit;
      end;
    end;

    Source.Add(Format('runTests({ exitOnFirstFailure: %s, showTestResults: false });',
      [BoolToStr(FExitOnFirst.Present, 'true', 'false')]));

    try
      SilentCon := nil;
      Engine := CreateEngine(AFileName, Source);
      try
        if FSilent.Present then
        begin
          SilentCon := TGocciaTestConsole.Create;
          SilentCon.Silence(Engine.BuiltinConsole.BuiltinObject);
          Engine.SuppressWarnings := True;
        end;

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        try
          EngineResult := Engine.Execute;
        finally
          ClearExecutionTimeout;
        end;
      finally
        SilentCon.Free;
        Engine.Free;
      end;
      Result.Timing := EngineResult;
      FileResult := EngineResult.Result as TGocciaObjectValue;
      MergeFileResult(ScriptResult, FileResult);
      Result.TestResult := ScriptResult;
    except
      on E: Exception do
      begin
        if E is TGocciaError then
        begin
          WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal));
          MarkLoadError(ScriptResult, AFileName, TGocciaError(E).GetDetailedMessage);
        end
        else if E is TGocciaThrowValue then
        begin
          WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, IsColorTerminal));
          MarkLoadError(ScriptResult, AFileName,
            FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, False));
        end
        else
        begin
          WriteLn('Fatal error: ', E.Message);
          MarkLoadError(ScriptResult, AFileName, E.Message);
        end;
        Result := MakeEmptyTestResult(ScriptResult);
      end;
    end;
  finally
    Source.Free;
  end;
end;

function TTestRunnerApp.RunGocciaScriptBytecode(const AFileName: string): TTestFileResult;
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
  Backend: TGocciaBytecodeBackend;
  ScriptResult: TGocciaObjectValue;
  ResultValue: TGocciaValue;
  OrigLine, OrigCol, I: Integer;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  ScriptResult := CreateDefaultScriptResult;

  Source := nil;
  try
    try
      Source := ReadUTF8FileLines(AFileName);
    except
      on E: EStreamError do
      begin
        WriteLn('Error loading test file: ', E.Message);
        MarkLoadError(ScriptResult, AFileName, E.Message);
        Result := MakeEmptyTestResult(ScriptResult);
        Exit;
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
      Backend := CreateBytecodeBackend(AFileName);
      try
        LexStart := GetNanoseconds;
        Lexer := TGocciaLexer.Create(SourceText, AFileName);
        try
          Tokens := Lexer.ScanTokens;
          LexEnd := GetNanoseconds;

          Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
          Parser.AutomaticSemicolonInsertion := EngineOptions.ASI.Present;
          try
            ProgramNode := Parser.Parse;
            ParseEnd := GetNanoseconds;

            if Assigned(TGocciaCoverageTracker.Instance) and
               TGocciaCoverageTracker.Instance.Enabled then
              TGocciaCoverageTracker.Instance.RegisterSourceFile(
                AFileName, CountExecutableLines(Lexer.SourceLines));

            if not FSilent.Present then
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
              Module := Backend.CompileToModule(ProgramNode);
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

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        try
          try
            ResultValue := Backend.RunModule(Module);
          finally
            ClearExecutionTimeout;
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
        finally
          Module.Free;
        end;
      finally
        Backend.Free;
      end;
    except
      on E: Exception do
      begin
        if E is TGocciaError then
        begin
          WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal));
          MarkLoadError(ScriptResult, AFileName, TGocciaError(E).GetDetailedMessage);
        end
        else if E is TGocciaThrowValue then
        begin
          WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, IsColorTerminal));
          MarkLoadError(ScriptResult, AFileName,
            FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, Source, False));
        end
        else
        begin
          WriteLn('Fatal error: ', E.Message);
          MarkLoadError(ScriptResult, AFileName, E.Message);
        end;
        Result := MakeEmptyTestResult(ScriptResult);
      end;
    end;
    finally
      SourceMap.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TTestRunnerApp.RunGocciaScript(const AFileName: string): TTestFileResult;
begin
  case EngineOptions.Mode.ValueOr(emInterpreted) of
    emInterpreted: Result := RunGocciaScriptInterpreted(AFileName);
    emBytecode:    Result := RunGocciaScriptBytecode(AFileName);
  end;
end;

function TTestRunnerApp.RunScriptFromFile(const AFileName: string): TAggregatedTestResult;
var
  FileResult: TTestFileResult;
begin
  Result.TestResult := nil;
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalCompileNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;
  try
    FileResult := RunGocciaScript(AFileName);
    Result.TestResult := FileResult.TestResult;
    Result.TotalLexNanoseconds := FileResult.Timing.LexTimeNanoseconds;
    Result.TotalParseNanoseconds := FileResult.Timing.ParseTimeNanoseconds;
    Result.TotalCompileNanoseconds := FileResult.Timing.CompileTimeNanoseconds;
    Result.TotalExecNanoseconds := FileResult.Timing.ExecuteTimeNanoseconds;
  except
    on E: Exception do
    begin
      if E is TGocciaError then
        WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal))
      else
        WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

function TTestRunnerApp.RunScriptsFromFiles(const AFiles: TStringList): TAggregatedTestResult;
var
  GC: TGarbageCollector;
  I, J: Integer;
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

    if Assigned(GC) then
      GC.Collect;

    if FExitOnFirst.Present and (FailedCount > 0) then
      Break;
  end;

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

  Result.TestResult := AllTestResults;
end;

procedure TTestRunnerApp.WriteResultsJSON(const AResult: TAggregatedTestResult; const AFileName: string);
var
  Lines: TStringList;
  TotalNanoseconds: Int64;
  FailedTests: TGocciaValue;
  FailedArray: TGocciaArrayValue;
  I: Integer;
  IsBytecodeMode: Boolean;
begin
  IsBytecodeMode := EngineOptions.Mode.Matches(emBytecode);
  if IsBytecodeMode then
    TotalNanoseconds := AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds
  else
    TotalNanoseconds := AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds;

  Lines := TStringList.Create;
  try
    Lines.Add('{');
    Lines.Add(Format('  "mode": "%s",', [IfThen(IsBytecodeMode, 'bytecode', 'interpreted')]));
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

    FailedTests := AResult.TestResult.GetProperty('failedTests');
    Lines.Add('  "failedTests": [');
    if FailedTests is TGocciaArrayValue then
    begin
      FailedArray := TGocciaArrayValue(FailedTests);
      for I := 0 to FailedArray.Elements.Count - 1 do
      begin
        if I < FailedArray.Elements.Count - 1 then
          Lines.Add(Format('    "%s",', [EscapeJSONString(FailedArray.Elements[I].ToStringLiteral.Value)]))
        else
          Lines.Add(Format('    "%s"', [EscapeJSONString(FailedArray.Elements[I].ToStringLiteral.Value)]));
      end;
    end;
    Lines.Add('  ]');

    Lines.Add('}');
    Lines.SaveToFile(AFileName);
  finally
    Lines.Free;
  end;
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
      if IsBytecodeMode then
        Writeln(Format('Test Results Engine Timing: Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
          [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds),
           FormatDuration(AResult.TotalCompileNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
           FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds)]))
      else
        Writeln(Format('Test Results Engine Timing: Lex: %s | Parse: %s | Execute: %s | Total: %s',
          [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
           FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds)]));
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
  RunResult := TGocciaApplication.RunApplication(TTestRunnerApp, 'TestRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
