#!/usr/bin/env instantfpc

{$mode delphi}
{$H+}

uses
  SysUtils, Classes, Process, FileUtils in 'units/FileUtils.pas';

type
  TBuildMode = (bmDev, bmProd);

var
  I: Integer;
  BuildTriggers: TStringList;
  BuildMode: TBuildMode = bmDev;

function FPCArgs(const Source: string): TStringArray;
var
  Arch: string;
  Args: TStringList;
  J: Integer;
begin
  Args := TStringList.Create;
  try
    Arch := GetEnvironmentVariable('FPC_TARGET_CPU');
    if Arch <> '' then
      Args.Add('-P' + Arch);

    Args.Add('@config.cfg');

    if BuildMode = bmProd then
    begin
      Args.Add('-O4');
      Args.Add('-dPRODUCTION');
      Args.Add('-Xs');
      Args.Add('-CX');
      Args.Add('-XX');
    end else
    begin
      Args.Add('-O-');
      Args.Add('-gw');
      Args.Add('-godwarfsets');
      Args.Add('-gh');
      Args.Add('-gl');
      Args.Add('-Ct');
      Args.Add('-Cr');
      Args.Add('-Sa');
    end;

    Args.Add('-vw-n-h-i-l-d-u-t-p-c-x-');
    Args.Add(Source);

    SetLength(Result, Args.Count);
    for J := 0 to Args.Count - 1 do
      Result[J] := Args[J];
  finally
    Args.Free;
  end;
end;

procedure BuildREPL;
var
  Output: string;
begin
  WriteLn('Building REPL...');
  if not RunCommand('fpc', FPCArgs('REPL.dpr'), Output) then
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
  if not RunCommand('fpc', FPCArgs('ScriptLoader.dpr'), Output) then
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
    if not RunCommand('fpc', FPCArgs(TestFiles[I]), Output) then
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
  if not RunCommand('fpc', FPCArgs('TestRunner.dpr'), Output) then
  begin
    WriteLn(Output);
    WriteLn('TestRunner build failed');
    Halt(1);
  end;
  WriteLn(Output);
  WriteLn('TestRunner built successfully');
end;

procedure BuildBenchmarkRunner;
var
  Output: string;
begin
  WriteLn('Building BenchmarkRunner...');
  if not RunCommand('fpc', FPCArgs('BenchmarkRunner.dpr'), Output) then
  begin
    WriteLn(Output);
    WriteLn('BenchmarkRunner build failed');
    Halt(1);
  end;
  WriteLn(Output);
  WriteLn('BenchmarkRunner built successfully');
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
    BuildTestRunner
  else if Trigger = 'benchmarkrunner' then
    BuildBenchmarkRunner;
end;

begin
  if not DirectoryExists('build') then
    CreateDir('build');

  BuildTriggers := TStringList.Create;

  for I := 1 to ParamCount do
  begin
    if ParamStr(I) = '--dev' then
      BuildMode := bmDev
    else if ParamStr(I) = '--prod' then
      BuildMode := bmProd
    else
      BuildTriggers.Add(LowerCase(ParamStr(I)));
  end;

  if BuildMode = bmProd then
    WriteLn('Build mode: production')
  else
    WriteLn('Build mode: development');

  WriteLn('');

  if BuildTriggers.Count = 0 then
  begin
    BuildTriggers.Add('tests');
    BuildTriggers.Add('loader');
    BuildTriggers.Add('testrunner');
    BuildTriggers.Add('benchmarkrunner');
    BuildTriggers.Add('repl');
  end;

  for I := 0 to BuildTriggers.Count - 1 do
  begin
    Build(BuildTriggers[I]);
  end;

  BuildTriggers.Free;
end.
