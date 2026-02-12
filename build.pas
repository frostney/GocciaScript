#!/usr/bin/env instantfpc

// Add support for build-and-run and run-only

{$mode delphi}
{$H+}

uses
  SysUtils, Classes, Process, FileUtils in 'units/FileUtils.pas';

var
  I: Integer;
  BuildTriggers: TStringList;

procedure BuildREPL;
var
  Output: string;
begin
  WriteLn('Building REPL...');
  if not RunCommand('fpc', ['@config.cfg', '-vw-n-h-i-l-d-u-t-p-c-x-', 'REPL.dpr'], Output) then
  begin
    WriteLn(Output);
    WriteLn('REPL build failed');
    Halt(1);
  end;
  WriteLn(Output);
  WriteLn('REPL built successfully');
end;

procedure BuildScriptLoader;
var
  Output: string;
begin
  WriteLn('');
  WriteLn('Building ScriptLoader...');
  if not RunCommand('fpc', ['@config.cfg', '-vw-n-h-i-l-d-u-t-p-c-x-', 'ScriptLoader.dpr'], Output) then
  begin
    WriteLn(Output);
    WriteLn('ScriptLoader build failed');
    Halt(1);
  end;
  WriteLn(Output);
  WriteLn('ScriptLoader built successfully');
  WriteLn('');
end;

procedure BuildTests;
var
  AllUnitFiles: TStringList;
  TestFiles: TStringList;
  Output: string;
begin
  WriteLn('Building Tests...');
  AllUnitFiles := TStringList.Create;
  TestFiles := TStringList.Create;

  AllUnitFiles.AddStrings(FindAllFiles('units', '.pas'));

  for I := 0 to AllUnitFiles.Count - 1 do
  begin
    if Pos('.Test.pas', AllUnitFiles[I]) > 0 then
      TestFiles.Add(AllUnitFiles[I]);
  end;

  for I := 0 to TestFiles.Count - 1 do
  begin
    if not RunCommand('fpc', ['@config.cfg', '-vw-n-h-i-l-d-u-t-p-c-x-', TestFiles[I]], Output) then
    begin
      WriteLn(Output);
      WriteLn('Test build failed: ', TestFiles[I]);
      Halt(1);
    end;
    WriteLn(Output);
  end;

  WriteLn('Tests built successfully');
end;

procedure BuildTestRunner;
var
  Output: string;
begin
  WriteLn('Building TestRunner...');
  if not RunCommand('fpc', ['@config.cfg', '-vw-n-h-i-l-d-u-t-p-c-x-', 'TestRunner.dpr'], Output) then
  begin
    WriteLn(Output);
    WriteLn('TestRunner build failed');
    Halt(1);
  end;
  WriteLn(Output);
  WriteLn('TestRunner built successfully');
end;

procedure Build(const Trigger: string);
begin
  if Trigger = 'repl' then
    BuildREPL
  else if Trigger = 'loader' then
    BuildScriptLoader
  else if Trigger = 'tests' then
    BuildTests
  else if Trigger = 'testrunner' then
    BuildTestRunner;
end;

begin
  // Create build directory
  if not DirectoryExists('build') then
    CreateDir('build');

  BuildTriggers := TStringList.Create;

  if ParamCount = 0 then
  begin
    BuildTriggers.Add('tests');
    BuildTriggers.Add('loader');
    BuildTriggers.Add('testrunner');
    BuildTriggers.Add('repl');
  end else
  begin
    for I := 1 to ParamCount do
    begin
      BuildTriggers.Add(LowerCase(ParamStr(I)));
    end;
  end;

  for I := 0 to BuildTriggers.Count - 1 do
  begin
    Build(BuildTriggers[I]);
  end;

  BuildTriggers.Free;
end.