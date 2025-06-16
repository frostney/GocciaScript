program TestRunner;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Base, Goccia.Lexer, Goccia.Parser, Goccia.Values.ObjectValue, Goccia.Values.NumberValue, Goccia.Values.ArrayValue, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node, FileUtils in 'units/FileUtils.pas';

function RunGocciaScript(const FileName: string): TGocciaValue;
var
  Source: TStringList;
  DefaultScriptResult: TGocciaObjectValue;
begin
  Source := TStringList.Create;
  Source.LoadFromFile(FileName);
  // It's easiest to inject the runTests call
  Source.Add('runTests({ exitOnFirstFailure: false, showTestResults: false, silent: true });');

  try
    Result := RunGocciaScriptFromStringList(Source, FileName).Value;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      DefaultScriptResult := TGocciaObjectValue.Create;
      DefaultScriptResult.SetProperty('totalTests', TGocciaNumberValue.Create(0));
      DefaultScriptResult.SetProperty('totalRunTests', TGocciaNumberValue.Create(0));
      DefaultScriptResult.SetProperty('passed', TGocciaNumberValue.Create(0));
      DefaultScriptResult.SetProperty('failed', TGocciaNumberValue.Create(0));
      DefaultScriptResult.SetProperty('assertions', TGocciaNumberValue.Create(0));
      DefaultScriptResult.SetProperty('duration', TGocciaNumberValue.Create(0));
      DefaultScriptResult.SetProperty('failedTests', TGocciaArrayValue.Create);
      Result := DefaultScriptResult;
    end;
  end;

  if Result = nil then
    Result := DefaultScriptResult;
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
begin
  AllTestResults := TGocciaObjectValue.Create;

  AllTestResults.SetProperty('totalTests', TGocciaNumberValue.Create(0));
  AllTestResults.SetProperty('totalRunTests', TGocciaNumberValue.Create(0));
  AllTestResults.SetProperty('passed', TGocciaNumberValue.Create(0));
  AllTestResults.SetProperty('failed', TGocciaNumberValue.Create(0));
  AllTestResults.SetProperty('assertions', TGocciaNumberValue.Create(0));
  AllTestResults.SetProperty('duration', TGocciaNumberValue.Create(0));
  AllTestResults.SetProperty('failedTests', TGocciaArrayValue.Create);

  for I := 0 to Files.Count - 1 do
  begin
    ScriptResult := RunScriptFromFile(Files[I]) as TGocciaObjectValue;
    AllTestResults.SetProperty(Files[I], ScriptResult);
    AllTestResults.SetProperty('passed', TGocciaNumberValue.Create(AllTestResults.GetProperty('passed').ToNumber + ScriptResult.GetProperty('passed').ToNumber));
    AllTestResults.SetProperty('failed', TGocciaNumberValue.Create(AllTestResults.GetProperty('failed').ToNumber + ScriptResult.GetProperty('failed').ToNumber));
    AllTestResults.SetProperty('totalRunTests', TGocciaNumberValue.Create(AllTestResults.GetProperty('totalRunTests').ToNumber + ScriptResult.GetProperty('totalRunTests').ToNumber));
    AllTestResults.SetProperty('duration', TGocciaNumberValue.Create(AllTestResults.GetProperty('duration').ToNumber + ScriptResult.GetProperty('duration').ToNumber));
    AllTestResults.SetProperty('assertions', TGocciaNumberValue.Create(AllTestResults.GetProperty('assertions').ToNumber + ScriptResult.GetProperty('assertions').ToNumber));
    // AllTestResults.SetProperty('failedTests', TGocciaArrayValue.Create(AllTestResults.GetProperty('failedTests').ToObject.GetProperty('failedTests').ToObject.Add(ScriptResult.GetProperty('failedTests').ToObject)));
  end;
  
  Result := AllTestResults;
end;

procedure PrintTestResults(const TestResult: TGocciaValue);
var
  TestResultObject: TGocciaObjectValue;
  TotalRunTests: String;
  TotalPassed: String;
  TotalFailed: String;
  TotalAssertions: String;
  TotalDuration: String;
begin
  TestResultObject := TestResult as TGocciaObjectValue;

  TotalRunTests := TestResultObject.GetProperty('totalRunTests').ToString;
  TotalPassed := TestResultObject.GetProperty('passed').ToString;
  TotalFailed := TestResultObject.GetProperty('failed').ToString;
  TotalAssertions := TestResultObject.GetProperty('assertions').ToString;
  TotalDuration := TestResultObject.GetProperty('duration').ToString;

  Writeln('Test Results Total Tests: ', TestResultObject.GetProperty('totalTests').ToString);
  Writeln(Format('Test Results Run Tests: %s', [TotalRunTests]));
  Writeln(Format('Test Results Passed: %s (%2.2f%%)', [TotalPassed, (StrToFloat(TotalPassed) / StrToFloat(TotalRunTests) * 100)]));
  Writeln(Format('Test Results Failed: %s (%2.2f%%)', [TotalFailed, (StrToFloat(TotalFailed) / StrToFloat(TotalRunTests) * 100)]));
  Writeln(Format('Test Results Assertions: %s', [TotalAssertions]));
  Writeln(Format('Test Results Duration: %sms (%2.2fms/test)', [TotalDuration, (StrToFloat(TotalDuration) / StrToFloat(TotalRunTests))]));
  Writeln(Format('Test Results Failed Tests: %s', [TestResultObject.GetProperty('failedTests').ToString]));
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
      PrintTestResults(RunScriptsFromFiles(Files));
    end
    else
    begin
      PrintTestResults(RunScriptFromFile(ParamStr(1)));
    end;
  end;
end.