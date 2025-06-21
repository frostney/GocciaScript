program TestRunner;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Base, Goccia.Lexer, Goccia.Parser, Goccia.Values.ObjectValue, 
  Goccia.Values.NumberValue, Goccia.Values.ArrayValue, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node, 
  Goccia.Values.ObjectPropertyDescriptor, FileUtils in 'units/FileUtils.pas';

function RunGocciaScript(const FileName: string): TGocciaValue;
var
  Source: TStringList;
  DefaultScriptResult: TGocciaObjectValue;
  TestGlobals: TGocciaGlobalBuiltins;
begin
  TestGlobals := TGocciaInterpreter.DefaultGlobals + [ggTestAssertions];

  Source := TStringList.Create;
  try  
    Source.LoadFromFile(FileName);
    // It's easiest to inject the runTests call
    Source.Add('runTests({ exitOnFirstFailure: false, showTestResults: false, silent: true });');

    try
      Result := RunGocciaScriptFromStringList(Source, FileName, TestGlobals).Value;
    except
      on E: Exception do
      begin
        WriteLn('Fatal error: ', E.Message);
        DefaultScriptResult := TGocciaObjectValue.Create;
        DefaultScriptResult.AssignProperty('totalTests', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('totalRunTests', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('passed', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('failed', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('skipped', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('assertions', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('duration', TGocciaNumberValue.Create(0));
        DefaultScriptResult.AssignProperty('failedTests', TGocciaArrayValue.Create);
        Result := DefaultScriptResult;
      end;
    end;

    if Result = nil then
      Result := DefaultScriptResult;
  finally
    Source.Free; 
  end;
end;

function RunScriptFromFile(const FileName: string): TGocciaValue;
var
  ScriptResult: TGocciaValue;
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

function RunScriptsFromFiles(const Files: TStringList): TGocciaValue;
var
  I: Integer;
  AllTestResults: TGocciaObjectValue;
  ScriptResult: TGocciaObjectValue;
  PassedCount, FailedCount, SkippedCount, TotalRunCount, TotalAssertions, TotalDuration: Double;
begin
  AllTestResults := TGocciaObjectValue.Create;

  AllTestResults.AssignProperty('totalTests', TGocciaNumberValue.Create(0));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberValue.Create(0));
  AllTestResults.AssignProperty('passed', TGocciaNumberValue.Create(0));
  AllTestResults.AssignProperty('failed', TGocciaNumberValue.Create(0));
  AllTestResults.AssignProperty('skipped', TGocciaNumberValue.Create(0));
  AllTestResults.AssignProperty('assertions', TGocciaNumberValue.Create(0));
  AllTestResults.AssignProperty('duration', TGocciaNumberValue.Create(0));
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
    
    PassedCount := PassedCount + ScriptResult.GetProperty('passed').ToNumber;
    FailedCount := FailedCount + ScriptResult.GetProperty('failed').ToNumber;
    SkippedCount := SkippedCount + ScriptResult.GetProperty('skipped').ToNumber;
    TotalRunCount := TotalRunCount + ScriptResult.GetProperty('totalRunTests').ToNumber;
    TotalDuration := TotalDuration + ScriptResult.GetProperty('duration').ToNumber;
    TotalAssertions := TotalAssertions + ScriptResult.GetProperty('assertions').ToNumber;
  end;
  
  // Set final totals - create objects only once at the end
  AllTestResults.AssignProperty('passed', TGocciaNumberValue.Create(PassedCount));
  AllTestResults.AssignProperty('failed', TGocciaNumberValue.Create(FailedCount));
  AllTestResults.AssignProperty('skipped', TGocciaNumberValue.Create(SkippedCount));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberValue.Create(TotalRunCount));
  AllTestResults.AssignProperty('duration', TGocciaNumberValue.Create(TotalDuration));
  AllTestResults.AssignProperty('assertions', TGocciaNumberValue.Create(TotalAssertions));
  
  Result := AllTestResults;
end;

procedure PrintTestResults(const TestResult: TGocciaValue);
var
  TestResultObject: TGocciaObjectValue;
  TotalRunTests: String;
  TotalPassed: String;
  TotalFailed: String;
  TotalSkipped: String;
  TotalAssertions: String;
  TotalDuration: String;
begin
  ExitCode := 0;

  TestResultObject := TestResult as TGocciaObjectValue;

  TotalRunTests := TestResultObject.GetProperty('totalRunTests').ToString;
  TotalPassed := TestResultObject.GetProperty('passed').ToString;
  TotalFailed := TestResultObject.GetProperty('failed').ToString;
  TotalSkipped := TestResultObject.GetProperty('skipped').ToString;
  TotalAssertions := TestResultObject.GetProperty('assertions').ToString;
  TotalDuration := TestResultObject.GetProperty('duration').ToString;

  Writeln('Test Results Total Tests: ', TestResultObject.GetProperty('totalTests').ToString);
  Writeln(Format('Test Results Run Tests: %s', [TotalRunTests]));
  Writeln(Format('Test Results Passed: %s (%2.2f%%)', [TotalPassed, (StrToFloat(TotalPassed) / StrToFloat(TotalRunTests) * 100)]));
  Writeln(Format('Test Results Failed: %s (%2.2f%%)', [TotalFailed, (StrToFloat(TotalFailed) / StrToFloat(TotalRunTests) * 100)]));
  Writeln(Format('Test Results Skipped: %s (%2.2f%%)', [TotalSkipped, (StrToFloat(TotalSkipped) / StrToFloat(TotalRunTests) * 100)]));
  Writeln(Format('Test Results Assertions: %s', [TotalAssertions]));
  Writeln(Format('Test Results Duration: %sms (%2.2fms/test)', [TotalDuration, (StrToFloat(TotalDuration) / StrToFloat(TotalRunTests))]));
  Writeln(Format('Test Results Failed Tests: %s', [TestResultObject.GetProperty('failedTests').ToString]));

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