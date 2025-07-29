program TestRunner;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Primitives, Goccia.Values.ObjectValue, 
  Goccia.Values.ArrayValue, Goccia.Engine, Goccia.Error, 
  Goccia.Values.ObjectPropertyDescriptor, Goccia.Values.ClassHelper, FileUtils in 'units/FileUtils.pas';

function RunGocciaScript(const FileName: string): TGocciaObjectValue;
var
  Source: TStringList;
  ScriptResult, FileResult: TGocciaObjectValue;
  TestGlobals: TGocciaGlobalBuiltins;
begin
  TestGlobals := TGocciaEngine.DefaultGlobals + [ggTestAssertions];

  ScriptResult := TGocciaObjectValue.Create;
  ScriptResult.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('passed', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('failed', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('duration', TGocciaNumberLiteralValue.Create(0));
  ScriptResult.AssignProperty('failedTests', TGocciaArrayValue.Create);

  Source := TStringList.Create;
  try  
    Source.LoadFromFile(FileName);
    // It's easiest to inject the runTests call
    Source.Add('runTests({ exitOnFirstFailure: false, showTestResults: false, silent: true });');

    try
      FileResult := TGocciaEngine.RunScriptFromStringList(Source, FileName, TestGlobals) as TGocciaObjectValue;
      
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

      Result := ScriptResult;
    except
      on E: Exception do
      begin
        WriteLn('Fatal error: ', E.Message);
        
        Result := ScriptResult;
      end;
    end;
  finally
    Source.Free; 
  end;
end;

function RunScriptFromFile(const FileName: string): TGocciaObjectValue;
var
  ScriptResult: TGocciaObjectValue;
begin
  try
    WriteLn('Running script: ', FileName);
    ScriptResult := RunGocciaScript(FileName);
  
    Result := ScriptResult;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

function RunScriptsFromFiles(const Files: TStringList): TGocciaObjectValue;
var
  I: Integer;
  AllTestResults: TGocciaObjectValue;
  ScriptResult: TGocciaObjectValue;
  PassedCount, FailedCount, SkippedCount, TotalRunCount, TotalAssertions, TotalDuration: Double;
begin
  AllTestResults := TGocciaObjectValue.Create;

  AllTestResults.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.Create(0));
  AllTestResults.AssignProperty('failedTests', TGocciaArrayValue.Create);

  // Initialize counters to avoid repeated object creation
  PassedCount := 0;
  FailedCount := 0;
  SkippedCount := 0;
  TotalRunCount := 0;
  TotalAssertions := 0;
  TotalDuration := 0;

  for I := 0 to Files.Count - 1 do
  begin
    ScriptResult := RunScriptFromFile(Files[I]) as TGocciaObjectValue;
    AllTestResults.AssignProperty(Files[I], ScriptResult);
    
    PassedCount := PassedCount + ScriptResult.GetProperty('passed').ToNumberLiteral.Value;
    FailedCount := FailedCount + ScriptResult.GetProperty('failed').ToNumberLiteral.Value;
    SkippedCount := SkippedCount + ScriptResult.GetProperty('skipped').ToNumberLiteral.Value;
    TotalRunCount := TotalRunCount + ScriptResult.GetProperty('totalRunTests').ToNumberLiteral.Value;
    TotalDuration := TotalDuration + ScriptResult.GetProperty('duration').ToNumberLiteral.Value;
    TotalAssertions := TotalAssertions + ScriptResult.GetProperty('assertions').ToNumberLiteral.Value;
  end;
  
  // Set final totals - create objects only once at the end
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteralValue.Create(PassedCount));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteralValue.Create(FailedCount));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(SkippedCount));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(TotalRunCount));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteralValue.Create(TotalDuration));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(TotalAssertions));
  
  Result := AllTestResults;
end;

procedure PrintTestResults(const TestResult: TGocciaObjectValue);
var
  TotalRunTests: String;
  TotalPassed: String;
  TotalFailed: String;
  TotalSkipped: String;
  TotalAssertions: String;
  TotalDuration: String;
begin
  ExitCode := 0;

  TotalRunTests := TestResult.GetProperty('totalRunTests').ToStringLiteral.Value;
  TotalPassed := TestResult.GetProperty('passed').ToStringLiteral.Value;
  TotalFailed := TestResult.GetProperty('failed').ToStringLiteral.Value;
  TotalSkipped := TestResult.GetProperty('skipped').ToStringLiteral.Value;
  TotalAssertions := TestResult.GetProperty('assertions').ToStringLiteral.Value;
  TotalDuration := TestResult.GetProperty('duration').ToStringLiteral.Value;

  Writeln('Test Results Total Tests: ', TestResult.GetProperty('totalTests').ToStringLiteral.Value);
  Writeln(Format('Test Results Run Tests: %s', [TotalRunTests]));

  if StrToFloat(TotalRunTests) > 0 then
  begin
    Writeln(Format('Test Results Passed: %s (%2.2f%%)', [TotalPassed, (StrToFloat(TotalPassed) / StrToFloat(TotalRunTests) * 100)]));
    Writeln(Format('Test Results Failed: %s (%2.2f%%)', [TotalFailed, (StrToFloat(TotalFailed) / StrToFloat(TotalRunTests) * 100)]));
    Writeln(Format('Test Results Skipped: %s (%2.2f%%)', [TotalSkipped, (StrToFloat(TotalSkipped) / StrToFloat(TotalRunTests) * 100)]));
    Writeln(Format('Test Results Assertions: %s', [TotalAssertions]));
    Writeln(Format('Test Results Duration: %sms (%2.2fms/test)', [TotalDuration, (StrToFloat(TotalDuration) / StrToFloat(TotalRunTests))]));
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
    WriteLn('Usage: GocciaScript <filename.js>');
    WriteLn('or');
    WriteLn('Usage: GocciaScript <directory>');
    ExitCode := 1;
  end
  else
  begin
    if DirectoryExists(ParamStr(1)) then
    begin
      Files := FindAllFiles(ParamStr(1), '.js');
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