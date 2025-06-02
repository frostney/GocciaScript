#!/usr/bin/env instantfpc

// Add support for build-and-run and run-only

{$mode delphi}
{$H+}

uses
  SysUtils, Classes, Process;

var
  I: Integer;
  BuildTriggers: TStringList;

procedure BuildREPL;
var
  Output: string;
begin
  WriteLn('Building REPL...');
  RunCommand('fpc', ['@config.cfg', 'REPL.dpr'], Output);
  WriteLn(Output);
  WriteLn('REPL built successfully');
end;

procedure BuildScriptLoader;
var
  Output: string;
begin
  WriteLn('');
  WriteLn('Building ScriptLoader...');
  RunCommand('fpc', ['@config.cfg', 'ScriptLoader.dpr'], Output);
  WriteLn(Output);
  WriteLn('ScriptLoader built successfully');
  WriteLn('');
end;

procedure BuildTests;
var
  TestFiles: TStringList;
  Output: string;
begin
  WriteLn('Building Tests...');
  TestFiles := TStringList.Create;
  TestFiles.Add('units/Goccia.Values.Primitives.Test.pas');
  TestFiles.Add('units/Goccia.Values.ObjectValue.Test.pas');
  TestFiles.Add('units/Goccia.Values.FunctionValue.Test.pas');

  for I := 0 to TestFiles.Count - 1 do
  begin
    RunCommand('fpc', ['@config.cfg', TestFiles[I]], Output);
    WriteLn(Output);
  end;

  WriteLn('Tests built successfully');
end;

procedure Build(const Trigger: string);
begin
  if Trigger = 'repl' then
    BuildREPL
  else if Trigger = 'loader' then
    BuildScriptLoader
  else if Trigger = 'test' then
    BuildTests;
end;

begin
  // Create build directory
  if not DirectoryExists('build') then
    CreateDir('build');

  BuildTriggers := TStringList.Create;

  if ParamCount = 0 then
  begin
    BuildTriggers.Add('test');
    BuildTriggers.Add('loader');
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