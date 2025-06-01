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
begin
  WriteLn('Building Tests...');
  // RunCommand('fpc @config.cfg GocciaTest.dpr', Output);
  // WriteLn(Output);
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
  BuildTriggers := TStringList.Create;

  if ParamCount = 0 then
  begin
    BuildTriggers.Add('repl');
    BuildTriggers.Add('loader');
    BuildTriggers.Add('test');
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