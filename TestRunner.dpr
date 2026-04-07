program TestRunner;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils,

  GarbageCollector.Generic,
  TimingUtils,

  Goccia.AST.Node,
  Goccia.Builtins.TestConsole,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Engine,
  Goccia.Engine.BytecodeBackend,
  Goccia.FileExtensions,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Modules.Configuration,
  Goccia.Parser,
  Goccia.TextFiles,
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
  GOutputFile: string = '';
  GImportMapPath: string = '';
  GInlineAliases: TStringList = nil;
  GMode: TGocciaEngineBackend = ebTreeWalk;
  GCoverageEnabled: Boolean = False;
  GCoverageLcovEnabled: Boolean = False;
  GCoverageJsonEnabled: Boolean = False;
  GCoverageOutputPath: string = '';

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
      [BoolToStr(GExitOnFirstFailure, 'true', 'false')]));

    try
      SilentConsole := nil;
      Engine := TGocciaEngine.Create(AFileName, Source, TestGlobals);
      try
        ConfigureModuleResolver(Engine.Resolver, AFileName, GImportMapPath,
          GInlineAliases);
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
  TestGlobals: TGocciaGlobalBuiltins;
  OrigLine, OrigCol, I: Integer;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  TestGlobals := TGocciaEngine.DefaultGlobals + [ggTestAssertions];
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
      [BoolToStr(GExitOnFirstFailure, 'true', 'false')]));

    SourceText := StringListToLFText(Source);
    SourceMap := nil;
    if ggJSX in TestGlobals then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(SourceText);
      SourceText := JSXResult.Source;
      SourceMap := JSXResult.SourceMap;
    end;

    try try
      Backend := TGocciaBytecodeBackend.Create(AFileName);
      try
        Backend.RegisterBuiltIns(TestGlobals);
        ConfigureModuleResolver(Backend.ModuleResolver, AFileName,
          GImportMapPath, GInlineAliases);

        LexStart := GetNanoseconds;
        Lexer := TGocciaLexer.Create(SourceText, AFileName);
        try
          Tokens := Lexer.ScanTokens;
          LexEnd := GetNanoseconds;

          Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
          try
            ProgramNode := Parser.Parse;
            ParseEnd := GetNanoseconds;

            if Assigned(TGocciaCoverageTracker.Instance) and
               TGocciaCoverageTracker.Instance.Enabled then
              TGocciaCoverageTracker.Instance.RegisterSourceFile(
                AFileName, CountExecutableLines(Lexer.SourceLines));

            if not GSilentConsole then
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

        try
          ResultValue := Backend.RunModule(Module);
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
        WriteLn('Fatal error: ', E.Message);
        MarkLoadError(ScriptResult, AFileName, E.Message);
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

function RunGocciaScript(const AFileName: string): TTestFileResult;
begin
  case GMode of
    ebTreeWalk:  Result := RunGocciaScriptInterpreted(AFileName);
    ebBytecode: Result := RunGocciaScriptBytecode(AFileName);
  end;
end;

type
  TAggregatedTestResult = record
    TestResult: TGocciaObjectValue;
    TotalLexNanoseconds: Int64;
    TotalParseNanoseconds: Int64;
    TotalCompileNanoseconds: Int64;
    TotalExecNanoseconds: Int64;
  end;

function RunScriptFromFile(const AFileName: string): TAggregatedTestResult;
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
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

function RunScriptsFromFiles(const AFiles: TStringList): TAggregatedTestResult;
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
    if GShowProgress then
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

    if GExitOnFirstFailure and (FailedCount > 0) then
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

function EscapeJSONString(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

procedure WriteResultsJSON(const AResult: TAggregatedTestResult; const AFileName: string);
var
  Lines: TStringList;
  TotalNanoseconds: Int64;
  FailedTests: TGocciaValue;
  FailedArray: TGocciaArrayValue;
  I: Integer;
begin
  if GMode = ebBytecode then
    TotalNanoseconds := AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalCompileNanoseconds + AResult.TotalExecNanoseconds
  else
    TotalNanoseconds := AResult.TotalLexNanoseconds + AResult.TotalParseNanoseconds + AResult.TotalExecNanoseconds;

  Lines := TStringList.Create;
  try
    Lines.Add('{');
    Lines.Add(Format('  "mode": "%s",', [IfThen(GMode = ebBytecode, 'bytecode', 'interpreted')]));
    Lines.Add(Format('  "totalFiles": %d,', [Round(AResult.TestResult.GetProperty('totalTests').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "totalTests": %d,', [Round(AResult.TestResult.GetProperty('totalRunTests').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "passed": %d,', [Round(AResult.TestResult.GetProperty('passed').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "failed": %d,', [Round(AResult.TestResult.GetProperty('failed').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "skipped": %d,', [Round(AResult.TestResult.GetProperty('skipped').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "assertions": %d,', [Round(AResult.TestResult.GetProperty('assertions').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "durationNanoseconds": %d,', [Round(AResult.TestResult.GetProperty('duration').ToNumberLiteral.Value)]));
    Lines.Add(Format('  "lexTimeNanoseconds": %d,', [AResult.TotalLexNanoseconds]));
    Lines.Add(Format('  "parseTimeNanoseconds": %d,', [AResult.TotalParseNanoseconds]));
    if GMode = ebBytecode then
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
      Writeln(Format('Test Results Test Duration: %s (%s/test)', [FormatDuration(DurationNanoseconds), FormatDuration(PerTestNanoseconds)]));
      if GMode = ebBytecode then
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

  if GOutputFile <> '' then
    WriteResultsJSON(AResult, GOutputFile);

  if StrToFloat(TotalFailed) > 0 then
    ExitCode := 1;
end;

var
  Files: TStringList;
  Paths: TStringList;
  I: Integer;
  Arg: string;

begin
  GShowProgress := True;
  GShowResults := True;
  GExitOnFirstFailure := False;
  GSilentConsole := False;
  GImportMapPath := '';
  GMode := ebTreeWalk;
  GInlineAliases := TStringList.Create;
  Paths := TStringList.Create;
  try
    I := 1;
    while I <= ParamCount do
    begin
      Arg := ParamStr(I);
      if Arg = '--no-progress' then
        GShowProgress := False
      else if Arg = '--no-results' then
        GShowResults := False
      else if Arg = '--exit-on-first-failure' then
        GExitOnFirstFailure := True
      else if Arg = '--silent' then
        GSilentConsole := True
      else if Copy(Arg, 1, 13) = '--import-map=' then
      begin
        GImportMapPath := Copy(Arg, 14, MaxInt);
        if GImportMapPath = '' then
        begin
          WriteLn('Error: --import-map requires a non-empty path.');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Copy(Arg, 1, 8) = '--alias=' then
        GInlineAliases.Add(Copy(Arg, 9, MaxInt))
      else if Copy(Arg, 1, 9) = '--output=' then
        GOutputFile := Copy(Arg, 10, MaxInt)
      else if Arg = '--alias' then
      begin
        if I = ParamCount then
        begin
          WriteLn('Error: --alias requires a key=value argument.');
          ExitCode := 1;
          Exit;
        end;
        Inc(I);
        GInlineAliases.Add(ParamStr(I));
      end
      else if Arg = '--mode=interpreted' then
        GMode := ebTreeWalk
      else if Arg = '--mode=bytecode' then
        GMode := ebBytecode
      else if Copy(Arg, 1, 7) = '--mode=' then
      begin
        WriteLn('Error: Unknown mode "', Copy(Arg, 8, MaxInt), '". Use "interpreted" or "bytecode".');
        ExitCode := 1;
        Exit;
      end
      else if Arg = '--coverage' then
        GCoverageEnabled := True
      else if Arg = '--coverage-format=lcov' then
      begin
        GCoverageLcovEnabled := True;
        GCoverageEnabled := True;
      end
      else if Arg = '--coverage-format=json' then
      begin
        GCoverageJsonEnabled := True;
        GCoverageEnabled := True;
      end
      else if Copy(Arg, 1, 18) = '--coverage-format=' then
      begin
        WriteLn('Error: Unknown coverage format "', Copy(Arg, 19, MaxInt), '". Use "lcov" or "json".');
        ExitCode := 1;
        Exit;
      end
      else if Copy(Arg, 1, 18) = '--coverage-output=' then
      begin
        GCoverageOutputPath := Copy(Arg, 19, MaxInt);
        GCoverageEnabled := True;
      end
      else if Copy(Arg, 1, 2) = '--' then
      begin
        WriteLn('Error: Unknown option "', Arg, '"');
        ExitCode := 1;
        Exit;
      end
      else
        Paths.Add(Arg);
      Inc(I);
    end;

    if Paths.Count = 0 then
    begin
      WriteLn('Usage: TestRunner <path...> [options]');
      WriteLn('  <path...>               Script files (.js, .jsx, .ts, .tsx, .mjs) or directories');
      WriteLn('  --no-progress           Suppress per-file progress output');
      WriteLn('  --no-results            Suppress test results summary');
      WriteLn('  --exit-on-first-failure Stop on first test failure');
      WriteLn('  --silent                Suppress console output from test scripts');
      WriteLn('  --import-map=<file>     Load an explicit import map JSON for module resolution');
      WriteLn('  --alias key=value       Add an inline import-map-style alias');
      WriteLn('  --output=<file>         Write test results as JSON to file');
      WriteLn('  --mode=interpreted|bytecode  Execution backend (default: interpreted)');
      WriteLn('  --coverage              Enable line and branch coverage reporting');
      WriteLn('  --coverage-format=lcov|json  Coverage output format (implies --coverage)');
      WriteLn('  --coverage-output=<file>     Coverage output file (paired with --coverage-format)');
      ExitCode := 1;
    end
    else
    begin
      if GCoverageEnabled then
      begin
        TGocciaCoverageTracker.Initialize;
        TGocciaCoverageTracker.Instance.Enabled := True;
      end;
      try
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

          if GCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
          begin
            PrintCoverageSummary(TGocciaCoverageTracker.Instance);
            if Files.Count = 1 then
              PrintCoverageDetail(TGocciaCoverageTracker.Instance, Files[0]);
            if GCoverageLcovEnabled and (GCoverageOutputPath <> '') then
              WriteCoverageLcov(TGocciaCoverageTracker.Instance, GCoverageOutputPath);
            if GCoverageJsonEnabled and (GCoverageOutputPath <> '') then
              WriteCoverageJSON(TGocciaCoverageTracker.Instance, GCoverageOutputPath);
          end;
        finally
          Files.Free;
        end;
      finally
        if GCoverageEnabled then
          TGocciaCoverageTracker.Shutdown;
      end;
    end;
  finally
    GInlineAliases.Free;
    Paths.Free;
  end;
end.
