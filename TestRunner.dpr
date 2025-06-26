program TestRunner;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Core, Goccia.Lexer, Goccia.Parser, Goccia.Values.ObjectValue, 
  Goccia.Values.NumberValue, Goccia.Values.ArrayValue, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node, 
  Goccia.Values.ObjectPropertyDescriptor, FileUtils in 'units/FileUtils.pas';

function RunGocciaScript(const FileName: string): TGocciaObjectValue;
var
  Source: TStringList;
  ScriptResult, FileResult: TGocciaObjectValue;
  TestGlobals: TGocciaGlobalBuiltins;
begin
  TestGlobals := TGocciaInterpreter.DefaultGlobals + [ggTestAssertions];

  ScriptResult := TGocciaObjectValue.Create;
  ScriptResult.AssignProperty('totalTests', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('totalRunTests', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('passed', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('failed', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('skipped', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('assertions', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('duration', TGocciaNumberLiteral.Create(0));
  ScriptResult.AssignProperty('failedTests', TGocciaArrayValue.Create);

  Source := TStringList.Create;
  try  
    Source.LoadFromFile(FileName);
    // It's easiest to inject the runTests call
    Source.Add('runTests({ exitOnFirstFailure: false, showTestResults: false, silent: true });');

    try
      FileResult := RunGocciaScriptFromStringList(Source, FileName, TestGlobals).Value as TGocciaObjectValue;
      
      if FileResult <> nil then
      begin
        if FileResult.GetProperty('totalTests').ToString <> 'undefined' then
          ScriptResult.AssignProperty('totalTests', FileResult.GetProperty('totalTests'));
        if FileResult.GetProperty('totalRunTests').ToString <> 'undefined' then
          ScriptResult.AssignProperty('totalRunTests', FileResult.GetProperty('totalRunTests'));
        if FileResult.GetProperty('passed').ToString <> 'undefined' then
          ScriptResult.AssignProperty('passed', FileResult.GetProperty('passed'));
        if FileResult.GetProperty('failed').ToString <> 'undefined' then
          ScriptResult.AssignProperty('failed', FileResult.GetProperty('failed'));
        if FileResult.GetProperty('skipped').ToString <> 'undefined' then
          ScriptResult.AssignProperty('skipped', FileResult.GetProperty('skipped'));
        if FileResult.GetProperty('assertions').ToString <> 'undefined' then
          ScriptResult.AssignProperty('assertions', FileResult.GetProperty('assertions'));
        if FileResult.GetProperty('duration').ToString <> 'undefined' then
          ScriptResult.AssignProperty('duration', FileResult.GetProperty('duration'));
        if FileResult.GetProperty('failedTests').ToString <> 'undefined' then
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

  AllTestResults.AssignProperty('totalTests', TGocciaNumberLiteral.Create(0));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteral.Create(0));
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteral.Create(0));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteral.Create(0));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteral.Create(0));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteral.Create(0));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteral.Create(0));
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
  AllTestResults.AssignProperty('passed', TGocciaNumberLiteral.Create(PassedCount));
  AllTestResults.AssignProperty('failed', TGocciaNumberLiteral.Create(FailedCount));
  AllTestResults.AssignProperty('skipped', TGocciaNumberLiteral.Create(SkippedCount));
  AllTestResults.AssignProperty('totalRunTests', TGocciaNumberLiteral.Create(TotalRunCount));
  AllTestResults.AssignProperty('duration', TGocciaNumberLiteral.Create(TotalDuration));
  AllTestResults.AssignProperty('assertions', TGocciaNumberLiteral.Create(TotalAssertions));
  
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

  TotalRunTests := TestResult.GetProperty('totalRunTests').ToString;
  TotalPassed := TestResult.GetProperty('passed').ToString;
  TotalFailed := TestResult.GetProperty('failed').ToString;
  TotalSkipped := TestResult.GetProperty('skipped').ToString;
  TotalAssertions := TestResult.GetProperty('assertions').ToString;
  TotalDuration := TestResult.GetProperty('duration').ToString;

  Writeln('Test Results Total Tests: ', TestResult.GetProperty('totalTests').ToString);
  Writeln(Format('Test Results Run Tests: %s', [TotalRunTests]));

  if StrToFloat(TotalRunTests) > 0 then
  begin
    Writeln(Format('Test Results Passed: %s (%2.2f%%)', [TotalPassed, (StrToFloat(TotalPassed) / StrToFloat(TotalRunTests) * 100)]));
    Writeln(Format('Test Results Failed: %s (%2.2f%%)', [TotalFailed, (StrToFloat(TotalFailed) / StrToFloat(TotalRunTests) * 100)]));
    Writeln(Format('Test Results Skipped: %s (%2.2f%%)', [TotalSkipped, (StrToFloat(TotalSkipped) / StrToFloat(TotalRunTests) * 100)]));
    Writeln(Format('Test Results Assertions: %s', [TotalAssertions]));
    Writeln(Format('Test Results Duration: %sms (%2.2fms/test)', [TotalDuration, (StrToFloat(TotalDuration) / StrToFloat(TotalRunTests))]));
    Writeln(Format('Test Results Failed Tests: %s', [TestResult.GetProperty('failedTests').ToString]));
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