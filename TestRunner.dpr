program TestRunner;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Souffle.Bytecode.Module,
  TimingUtils,

  Goccia.AST.Node,
  Goccia.Builtins.TestConsole,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.FileExtensions,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

var
  GShowProgress: Boolean = True;
  GShowResults: Boolean = True;
  GExitOnFirstFailure: Boolean = False;
  GSilentConsole: Boolean = False;
  GMode: TGocciaEngineBackend = ebTreeWalk;

type
  TTestFileResult = record
    TestResult: TGocciaObjectValue;
    Timing: TGocciaScriptResult;
  end;

function MakeEmptyTestResult(const AScriptResult: TGocciaObjectValue): TTestFileResult;
begin
  Result.TestResult := AScriptResult;
  Result.Timing.Result := nil;
  Result.Timing.LexTimeNanoseconds := 0;
  Result.Timing.ParseTimeNanoseconds := 0;
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

function RunGocciaScriptInterpreted(const AFileName: string): TTestFileResult;
var
  Source: TStringList;
  ScriptResult, FileResult: TGocciaObjectValue;
  Engine: TGocciaEngine;
  SilentConsole: TGocciaTestConsole;
  EngineResult: TGocciaScriptResult;
  TestGlobals: TGocciaGlobalBuiltins;
begin
  TestGlobals := TGocciaEngine.DefaultGlobals + [ggTestAssertions];
  ScriptResult := CreateDefaultScriptResult;

  Source := TStringList.Create;
  try
    try
      Source.LoadFromFile(AFileName);
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
      [BoolToStr(GExitOnFirstFailure, 'true', 'false')]));

    try
      SilentConsole := nil;
      Engine := TGocciaEngine.Create(AFileName, Source, TestGlobals);
      try
        if GSilentConsole then
        begin
          SilentConsole := TGocciaTestConsole.Create;
          SilentConsole.Silence(Engine.BuiltinConsole.BuiltinObject);
          Engine.SuppressWarnings := True;
        end;

        EngineResult := Engine.Execute;
      finally
        SilentConsole.Free;
        Engine.Free;
      end;
      Result.Timing := EngineResult;
      FileResult := EngineResult.Result as TGocciaObjectValue;
      MergeFileResult(ScriptResult, FileResult);
      Result.TestResult := ScriptResult;
    except
      on E: Exception do
      begin
        WriteLn('Fatal error: ', E.Message);
        MarkLoadError(ScriptResult, AFileName, E.Message);
        Result := MakeEmptyTestResult(ScriptResult);
      end;
    end;
  finally
    Source.Free;
  end;
end;

function RunGocciaScriptBytecode(const AFileName: string): TTestFileResult;
var
  Source: TStringList;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TSouffleBytecodeModule;
  Backend: TGocciaSouffleBackend;
  ScriptResult: TGocciaObjectValue;
  ResultValue: TGocciaValue;
  TestGlobals: TGocciaGlobalBuiltins;
  StartTime, EndTime: Int64;
begin
  TestGlobals := TGocciaEngine.DefaultGlobals + [ggTestAssertions];
  ScriptResult := CreateDefaultScriptResult;

  Source := TStringList.Create;
  try
    try
      Source.LoadFromFile(AFileName);
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
      [BoolToStr(GExitOnFirstFailure, 'true', 'false')]));

    try
      StartTime := GetNanoseconds;

      Backend := TGocciaSouffleBackend.Create(AFileName);
      try
        Backend.RegisterBuiltIns(TestGlobals);

        Lexer := TGocciaLexer.Create(Source.Text, AFileName);
        try
          Tokens := Lexer.ScanTokens;
          Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
          try
            ProgramNode := Parser.Parse;
            try
              Module := Backend.CompileToModule(ProgramNode);
            finally
              ProgramNode.Free;
            end;
          finally
            Parser.Free;
          end;
        finally
          Lexer.Free;
        end;

        try
          ResultValue := Backend.RunModule(Module);
          EndTime := GetNanoseconds;

          if ResultValue is TGocciaObjectValue then
            MergeFileResult(ScriptResult, TGocciaObjectValue(ResultValue));

          Result.TestResult := ScriptResult;
          Result.Timing.Result := ResultValue;
          Result.Timing.LexTimeNanoseconds := 0;
          Result.Timing.ParseTimeNanoseconds := 0;
          Result.Timing.ExecuteTimeNanoseconds := EndTime - StartTime;
          Result.Timing.TotalTimeNanoseconds := EndTime - StartTime;
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
        WriteLn('Fatal error: ', E.Message);
        MarkLoadError(ScriptResult, AFileName, E.Message);
        Result := MakeEmptyTestResult(ScriptResult);
      end;
    end;
  finally
    Source.Free;
  end;
end;

function RunGocciaScript(const AFileName: string): TTestFileResult;
begin
  case GMode of
    ebTreeWalk:  Result := RunGocciaScriptInterpreted(AFileName);
    ebSouffleVM: Result := RunGocciaScriptBytecode(AFileName);
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
  Result.TestResult := nil;
  Result.TotalLexNanoseconds := 0;
  Result.TotalParseNanoseconds := 0;
  Result.TotalExecNanoseconds := 0;
  try
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
  I, J: Integer;
  AllTestResults: TGocciaObjectValue;
  AllFailedTests: TGocciaArrayValue;
  FileResult: TAggregatedTestResult;
  FileFailedTests: TGocciaValue;
  PassedCount, FailedCount, SkippedCount, TotalRunCount, TotalAssertions, TotalDuration: Double;
begin
  AllTestResults := TGocciaObjectValue.Create;
  AllFailedTests := TGocciaArrayValue.Create;

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
  Result.TotalExecNanoseconds := 0;

  for I := 0 to AFiles.Count - 1 do
  begin
    if GShowProgress then
      WriteLn(SysUtils.Format('[%d/%d] %s', [I + 1, AFiles.Count, AFiles[I]]));
    FileResult := RunScriptFromFile(AFiles[I]);
    if FileResult.TestResult = nil then
      Continue;

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

    FileFailedTests := FileResult.TestResult.GetProperty('failedTests');
    if FileFailedTests is TGocciaArrayValue then
      for J := 0 to TGocciaArrayValue(FileFailedTests).Elements.Count - 1 do
        AllFailedTests.Elements.Add(TGocciaArrayValue(FileFailedTests).Elements[J]);

    if GExitOnFirstFailure and (FailedCount > 0) then
      Break;
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
  if TestResult = nil then
    Exit;

  TotalRunTests := TestResult.GetProperty('totalRunTests').ToStringLiteral.Value;
  TotalPassed := TestResult.GetProperty('passed').ToStringLiteral.Value;
  TotalFailed := TestResult.GetProperty('failed').ToStringLiteral.Value;
  TotalSkipped := TestResult.GetProperty('skipped').ToStringLiteral.Value;
  TotalAssertions := TestResult.GetProperty('assertions').ToStringLiteral.Value;
  DurationNanoseconds := Round(TestResult.GetProperty('duration').ToNumberLiteral.Value);
  RunCount := StrToFloat(TotalRunTests);

  if GShowResults then
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
      Writeln(Format('Test Results Test Execution: %s (%s/test)', [FormatDuration(DurationNanoseconds), FormatDuration(PerTestNanoseconds)]));
      Writeln(Format('Test Results Engine Timing: Lex: %s | Parse: %s | Execute: %s | Total: %s',
        [FormatDuration(AResult.TotalLexNanoseconds), FormatDuration(AResult.TotalParseNanoseconds), FormatDuration(AResult.TotalExecNanoseconds),
         FormatDuration(AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds)]));
      Writeln(Format('Test Results Failed Tests: %s', [TestResult.GetProperty('failedTests').ToStringLiteral.Value]));
    end;
  end;

  if StrToFloat(TotalFailed) > 0 then
    ExitCode := 1;
end;

var
  Files: TStringList;
  Paths: TStringList;
  I: Integer;

begin
  GShowProgress := True;
  GShowResults := True;
  GExitOnFirstFailure := False;
  GSilentConsole := False;
  GMode := ebTreeWalk;

  Paths := TStringList.Create;
  try
    for I := 1 to ParamCount do
    begin
      if ParamStr(I) = '--no-progress' then
        GShowProgress := False
      else if ParamStr(I) = '--no-results' then
        GShowResults := False
      else if ParamStr(I) = '--exit-on-first-failure' then
        GExitOnFirstFailure := True
      else if ParamStr(I) = '--silent' then
        GSilentConsole := True
      else if ParamStr(I) = '--mode=interpreted' then
        GMode := ebTreeWalk
      else if ParamStr(I) = '--mode=bytecode' then
        GMode := ebSouffleVM
      else if Copy(ParamStr(I), 1, 7) = '--mode=' then
      begin
        WriteLn('Error: Unknown mode "', Copy(ParamStr(I), 8, MaxInt), '". Use "interpreted" or "bytecode".');
        ExitCode := 1;
        Exit;
      end
      else if Copy(ParamStr(I), 1, 2) <> '--' then
        Paths.Add(ParamStr(I));
    end;

    if Paths.Count = 0 then
    begin
      WriteLn('Usage: TestRunner <path...> [options]');
      WriteLn('  <path...>               Script files (.js, .jsx, .ts, .tsx, .mjs) or directories');
      WriteLn('  --no-progress           Suppress per-file progress output');
      WriteLn('  --no-results            Suppress test results summary');
      WriteLn('  --exit-on-first-failure Stop on first test failure');
      WriteLn('  --silent                Suppress console output from test scripts');
      WriteLn('  --mode=interpreted|bytecode  Execution backend (default: interpreted)');
      ExitCode := 1;
    end
    else
    begin
      Files := TStringList.Create;
      try
        for I := 0 to Paths.Count - 1 do
        begin
          if DirectoryExists(Paths[I]) then
            Files.AddStrings(FindAllFiles(Paths[I], ScriptExtensions))
          else if FileExists(Paths[I]) then
            Files.Add(Paths[I])
          else
          begin
            WriteLn('Error: Path not found: ', Paths[I]);
            ExitCode := 1;
            Exit;
          end;
        end;

        if Files.Count = 1 then
        begin
          if GShowProgress then
            WriteLn('[1/1] ', Files[0]);
          PrintTestResults(RunScriptFromFile(Files[0]));
        end
        else
          PrintTestResults(RunScriptsFromFiles(Files));
      finally
        Files.Free;
      end;
    end;
  finally
    Paths.Free;
  end;
end.
