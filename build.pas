#!/usr/bin/env instantfpc

{$mode delphi}
{$H+}

uses
  Classes,
  Process,
  SysUtils,

  FileUtils in 'units/FileUtils.pas';

type
  TBuildMode = (bmDev, bmProd);

var
  I: Integer;
  BuildTriggers: TStringList;
  BuildMode: TBuildMode = bmDev;

procedure PrintVersion;
var
  Describe, Commit, Version: string;
  DashPos: SizeInt;
begin
  if not RunCommand('git', ['describe', '--tags', '--always'], Describe) then
    Describe := '';
  Describe := Trim(Describe);

  if not RunCommand('git', ['rev-parse', '--short', 'HEAD'], Commit) then
    Commit := '';
  Commit := Trim(Commit);
  if Commit = '' then
    Commit := 'unknown';

  if Describe = '' then
    Version := '0.0.0-dev'
  else
  begin
    DashPos := Pos('-', Describe);
    if DashPos > 0 then
      Version := Copy(Describe, 1, DashPos - 1) + '-dev'
    else
      Version := Describe;
  end;

  WriteLn('Version: ', Version, ' (', Commit, ')');
  WriteLn('');
end;

procedure CountLines(const AFileName: string; out ALOC, ASLOC: Integer);
var
  F: TextFile;
  Line, Trimmed: string;
  InBlock: Boolean;
  ClosePos: SizeInt;
begin
  ALOC := 0;
  ASLOC := 0;
  InBlock := False;

  AssignFile(F, AFileName);
  Reset(F);
  try
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Inc(ALOC);
      Trimmed := Trim(Line);

      if InBlock then
      begin
        ClosePos := Pos('}', Trimmed);
        if ClosePos = 0 then
          ClosePos := Pos('*)', Trimmed);
        if ClosePos > 0 then
        begin
          InBlock := False;
          Delete(Trimmed, 1, ClosePos);
          Trimmed := Trim(Trimmed);
          if Trimmed <> '' then
            Inc(ASLOC);
        end;
        Continue;
      end;

      if Trimmed = '' then
        Continue;

      if Copy(Trimmed, 1, 2) = '//' then
        Continue;

      if (Copy(Trimmed, 1, 1) = '{') and (Pos('}', Trimmed) = 0) then
      begin
        InBlock := True;
        Continue;
      end;

      if (Copy(Trimmed, 1, 2) = '(*') and (Pos('*)', Trimmed) = 0) then
      begin
        InBlock := True;
        Continue;
      end;

      Inc(ASLOC);
    end;
  finally
    CloseFile(F);
  end;
end;

function FormatNumber(AValue: Integer): string;
var
  S: string;
  Len, InsertPos: Integer;
begin
  S := IntToStr(AValue);
  Len := Length(S);
  InsertPos := Len - 3;
  while InsertPos > 0 do
  begin
    Insert(',', S, InsertPos + 1);
    Dec(InsertPos, 3);
  end;
  Result := S;
end;

procedure PrintSourceStats;
var
  Files: TStringList;
  K, FileLOC, FileSLOC, TotalLOC, TotalSLOC: Integer;
begin
  TotalLOC := 0;
  TotalSLOC := 0;

  Files := TStringList.Create;
  try
    Files.AddStrings(FindAllFiles('units', '.pas'));
    Files.AddStrings(FindAllFiles('.', '.dpr'));
    Files.AddStrings(FindAllFiles('units', '.inc'));

    for K := 0 to Files.Count - 1 do
    begin
      CountLines(Files[K], FileLOC, FileSLOC);
      TotalLOC := TotalLOC + FileLOC;
      TotalSLOC := TotalSLOC + FileSLOC;
    end;

    WriteLn('Source: ', FormatNumber(TotalLOC), ' LOC | ', FormatNumber(TotalSLOC), ' SLOC (', Files.Count, ' files)');
    WriteLn('');
  finally
    Files.Free;
  end;
end;

function FPCArgs(const ASource: string): TStringArray;
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
      Args.Add('-gl');
      Args.Add('-Ct');
      Args.Add('-Cr');
      Args.Add('-Sa');
    end;

    Args.Add('-vw-n-h-i-l-d-u-t-p-c-x-');
    Args.Add(ASource);

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
  K: Integer;
begin
  WriteLn('Building Tests...');
  AllUnitFiles := TStringList.Create;
  TestFiles := TStringList.Create;

  AllUnitFiles.AddStrings(FindAllFiles('units', '.pas'));

  for K := 0 to AllUnitFiles.Count - 1 do
  begin
    if Pos('.Test.pas', AllUnitFiles[K]) > 0 then
      TestFiles.Add(AllUnitFiles[K]);
  end;

  for K := 0 to TestFiles.Count - 1 do
  begin
    if not RunCommand('fpc', FPCArgs(TestFiles[K]), Output) then
    begin
      WriteLn(Output);
      WriteLn('Test build failed: ', TestFiles[K]);
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

procedure Clean;
var
  Files: TStringList;
  K: Integer;
begin
  WriteLn('Cleaning build artifacts...');
  if not DirectoryExists('build') then
  begin
    WriteLn('Nothing to clean');
    Exit;
  end;

  Files := TStringList.Create;
  try
    Files.AddStrings(FindAllFiles('build', '.ppu'));
    Files.AddStrings(FindAllFiles('build', '.o'));
    Files.AddStrings(FindAllFiles('build', '.res'));

    for K := 0 to Files.Count - 1 do
      DeleteFile(Files[K]);

    WriteLn('Removed ', Files.Count, ' file(s)');
  finally
    Files.Free;
  end;
end;

procedure Build(const ATrigger: string);
begin
  if ATrigger = 'clean' then
    Clean
  else if ATrigger = 'repl' then
    BuildREPL
  else if ATrigger = 'loader' then
    BuildScriptLoader
  else if ATrigger = 'tests' then
    BuildTests
  else if ATrigger = 'testrunner' then
    BuildTestRunner
  else if ATrigger = 'benchmarkrunner' then
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
  PrintVersion;
  PrintSourceStats;

  if BuildTriggers.Count = 0 then
  begin
    BuildTriggers.Add('clean');
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
