program TestRunner;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TimingUtils,

  Goccia.Engine,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TTestFileResult = record
    TestResult: TGocciaObjectValue;
    Timing: TGocciaScriptResult;
  end;

function RunGocciaScript(const AFileName: string): TTestFileResult;
var
  Source: TStringList;
  ScriptResult, FileResult: TGocciaObjectValue;
  EngineResult: TGocciaScriptResult;
  TestGlobals: TGocciaGlobalBuiltins;
begin
  TestGlobals := TGocciaEngine.DefaultGlobals + [ggTestAssertions];

  ScriptResult := TGocciaObjectValue.Create;
  ScriptResult.AssignProperty('totalTests', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('passed', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('failed', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('skipped', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('assertions', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
  ScriptResult.AssignProperty('failedTests', TGocciaArrayValue.Create);

  Source := TStringList.Create;
  try
    try
      Source.LoadFromFile(AFileName);
    except
      on E: EStreamError do
      begin
        WriteLn('Error loading test file: ', E.Message);
        Result.TestResult := ScriptResult;
        Result.Timing.Result := nil;
        Result.Timing.LexTimeNanoseconds := 0;
        Result.Timing.ParseTimeNanoseconds := 0;
        Result.Timing.ExecuteTimeNanoseconds := 0;
        Result.Timing.TotalTimeNanoseconds := 0;
        Result.Timing.FileName := '';
        Exit;
      end;
    end;

    Source.Add('runTests({ exitOnFirstFailure: false, showTestResults: false, silent: true });');

    try
      EngineResult := TGocciaEngine.RunScriptFromStringList(Source, AFileName, TestGlobals);
      Result.Timing := EngineResult;
      FileResult := EngineResult.Result as TGocciaObjectValue;
      
      if FileResult <> nil then
      begin
        if FileResult.GetProperty('totalTests').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('totalTests', FileResult.GetProperty('totalTests'));
        if FileResult.GetProperty('totalRunTests').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('totalRunTests', FileResult.GetProperty('totalRunTests'));
        if FileResult.GetProperty('passed').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('passed', FileResult.GetProperty('passed'));
        if FileResult.GetProperty('failed').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('failed', FileResult.GetProperty('failed'));
        if FileResult.GetProperty('skipped').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('skipped', FileResult.GetProperty('skipped'));
        if FileResult.GetProperty('assertions').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('assertions', FileResult.GetProperty('assertions'));
        if FileResult.GetProperty('duration').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('duration', FileResult.GetProperty('duration'));
        if FileResult.GetProperty('failedTests').ToStringLiteral.Value <> 'undefined' then
          ScriptResult.AssignProperty('failedTests', FileResult.GetProperty('failedTests'));
      end else
      begin
        ScriptResult := FileResult;
      end;

      Result.TestResult := ScriptResult;
    except
      on E: Exception do
      begin
        WriteLn('Fatal error: ', E.Message);
        Result.TestResult := ScriptResult;
        Result.Timing.Result := nil;
        Result.Timing.LexTimeNanoseconds := 0;
        Result.Timing.ParseTimeNanoseconds := 0;
        Result.Timing.ExecuteTimeNanoseconds := 0;
        Result.Timing.TotalTimeNanoseconds := 0;
        Result.Timing.FileName := '';
      end;
    end;
  finally
    Source.Free;
  end;
end;

type
  TAggregatedTestResult = record
    TestResult: TGocciaObjectValue;
    TotalLexNanoseconds: Int64;
    TotalParseNanoseconds: Int64;
    TotalExecNanoseconds: Int64;
  end;

function RunScriptFromFile(const AFileName: string): TAggregatedTestResult;
var
  FileResult: TTestFileResult;
begin
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;
  try
    WriteLn('Running script: ', AFileName);
    FileResult := RunGocciaScript(AFileName);
    Result.TestResult := FileResult.TestResult;
    Result.TotalLexNanoseconds := FileResult.Timing.LexTimeNanoseconds;
    Result.TotalParseNanoseconds := FileResult.Timing.ParseTimeNanoseconds;
    Result.TotalExecNanoseconds := FileResult.Timing.ExecuteTimeNanoseconds;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

function RunScriptsFromFiles(const AFiles: TStringList): TAggregatedTestResult;
var
  I: Integer;
  AllTestResults: TGocciaObjectValue;
  FileResult: TAggregatedTestResult;
  PassedCount, FailedCount, SkippedCount, TotalRunCount, TotalAssertions, TotalDuration: Double;
begin
  AllTestResults := TGocciaObjectValue.Create;

  AllTestResults.AssignProperty('totalTests', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
  AllTestResults.AssignProperty('failedTests', TGocciaArrayValue.Create);

  PassedCount := 0;
  FailedCount := 0;
  SkippedCount := 0;
  TotalRunCount := 0;
  TotalAssertions := 0;
  TotalDuration := 0;
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;

  for I := 0 to AFiles.Count - 1 do
  begin
    FileResult := RunScriptFromFile(AFiles[I]);
    AllTestResults.AssignProperty(AFiles[I], FileResult.TestResult);

    Result.TotalLexNanoseconds := Result.TotalLexNanoseconds + FileResult.TotalLexNanoseconds;
    Result.TotalParseNanoseconds := Result.TotalParseNanoseconds + FileResult.TotalParseNanoseconds;
    Result.TotalExecNanoseconds := Result.TotalExecNanoseconds + FileResult.TotalExecNanoseconds;

    PassedCount := PassedCount + FileResult.TestResult.GetProperty('passed').ToNumberLiteral.Value;
    FailedCount := FailedCount + FileResult.TestResult.GetProperty('failed').ToNumberLiteral.Value;
    SkippedCount := SkippedCount + FileResult.TestResult.GetProperty('skipped').ToNumberLiteral.Value;
    TotalRunCount := TotalRunCount + FileResult.TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value;
    TotalDuration := TotalDuration + FileResult.TestResult.GetProperty('duration').ToNumberLiteral.Value;
    TotalAssertions := TotalAssertions + FileResult.TestResult.GetProperty('assertions').ToNumberLiteral.Value;
  end;
  
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.Create(PassedCount));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.Create(FailedCount));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(SkippedCount));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(TotalRunCount));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.Create(TotalDuration));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(TotalAssertions));
  
  Result.TestResult := AllTestResults;
end;

procedure PrintTestResults(const AResult: TAggregatedTestResult);
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
begin
  ExitCode := 0;
  TestResult := AResult.TestResult;

  TotalRunTests := TestResult.GetProperty('totalRunTests').ToStringLiteral.Value;
  TotalPassed := TestResult.GetProperty('passed').ToStringLiteral.Value;
  TotalFailed := TestResult.GetProperty('failed').ToStringLiteral.Value;
  TotalSkipped := TestResult.GetProperty('skipped').ToStringLiteral.Value;
  TotalAssertions := TestResult.GetProperty('assertions').ToStringLiteral.Value;
  DurationNanoseconds := Round(TestResult.GetProperty('duration').ToNumberLiteral.Value);
  RunCount := StrToFloat(TotalRunTests);

  Writeln('Test Results Total Tests: ', TestResult.GetProperty('totalTests').ToStringLiteral.Value);
  Writeln(Format('Test Results Run Tests: %s', [TotalRunTests]));

  if RunCount > 0 then
  begin
    PerTestNanoseconds := Round(DurationNanoseconds / RunCount);
    Writeln(Format('Test Results Passed: %s (%2.2f%%)', [TotalPassed, (StrToFloat(TotalPassed) / RunCount * 100)]));
    Writeln(Format('Test Results Failed: %s (%2.2f%%)', [TotalFailed, (StrToFloat(TotalFailed) / RunCount * 100)]));
    Writeln(Format('Test Results Skipped: %s (%2.2f%%)', [TotalSkipped, (StrToFloat(TotalSkipped) / RunCount * 100)]));
    Writeln(Format('Test Results Assertions: %s', [TotalAssertions]));
    Writeln(Format('Test Results Test Execution: %s (%s/test)', [FormatDuration(DurationNanoseconds), FormatDuration(PerTestNanoseconds)]));
    Writeln(Format('Test Results Engine Timing: Lex: %s | Parse: %s | Execute: %s | Total: %s',
      [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
       FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds)]));
    Writeln(Format('Test Results Failed Tests: %s', [TestResult.GetProperty('failedTests').ToStringLiteral.Value]));
  end;

  if StrToFloat(TotalFailed) > 0 then
    ExitCode := 1;
end;

var
  Files: TStringList;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: TestRunner <filename.{js|jsx|ts|tsx|mjs}>');
    WriteLn('or');
    WriteLn('Usage: TestRunner <directory> (searches for .js, .jsx, .ts, .tsx, .mjs files)');
    ExitCode := 1;
  end
  else
  begin
    if DirectoryExists(ParamStr(1)) then
    begin
      Files := FindAllFiles(ParamStr(1));
      try 
        PrintTestResults(RunScriptsFromFiles(Files));
      finally
        Files.Free;
      end;
    end
    else
    begin
      PrintTestResults(RunScriptFromFile(ParamStr(1)));
    end;
  end;
end.
