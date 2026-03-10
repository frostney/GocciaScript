#!/usr/bin/env instantfpc

{$mode delphi}
{$H+}

uses
  Classes,
  Process,
  SysUtils,

  FileUtils in 'units/FileUtils.pas';

const
  COLOR_GREEN  = #27'[32m';
  COLOR_RED    = #27'[31m';
  COLOR_YELLOW = #27'[33m';
  COLOR_RESET  = #27'[0m';
  COLOR_BOLD   = #27'[1m';

var
  ScriptLoaderPath, HostScriptPath: string;
  Passed, Failed, Skipped: Integer;
  TempFiles: TStringList;

function HasNode: Boolean;
var
  Output: string;
begin
  Result := RunCommand('node', ['--version'], Output);
end;

function ParseExpectedLines(const AFileName: string): TStringList;
var
  Source: TStringList;
  I: Integer;
  Line, Prefix: string;
begin
  Result := TStringList.Create;
  Prefix := '// Expected: ';
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    for I := 0 to Source.Count - 1 do
    begin
      Line := Source[I];
      if Copy(Line, 1, Length(Prefix)) = Prefix then
        Result.Add(Copy(Line, Length(Prefix) + 1, MaxInt));
    end;
  finally
    Source.Free;
  end;
end;

function IsSkipped(const AFileName: string): Boolean;
var
  Source: TStringList;
  I: Integer;
begin
  Result := False;
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    for I := 0 to Source.Count - 1 do
    begin
      if Copy(Trim(Source[I]), 1, 8) = '// Skip:' then
        Exit(True);
      if (Trim(Source[I]) <> '') and (Copy(Trim(Source[I]), 1, 2) <> '//') then
        Break;
    end;
  finally
    Source.Free;
  end;
end;

function RunProcess(const AExe: string; const AArgs: array of string;
  out AStdout, AStderr: string; out AExitCode: Integer): Boolean;
var
  Proc: TProcess;
  OutStream, ErrStream: TStringStream;
  BytesRead: LongInt;
  Buf: array[0..4095] of Byte;
begin
  Result := True;
  AStdout := '';
  AStderr := '';
  AExitCode := 0;
  Proc := TProcess.Create(nil);
  OutStream := TStringStream.Create('');
  ErrStream := TStringStream.Create('');
  try
    Proc.Executable := AExe;
    for BytesRead := 0 to High(AArgs) do
      Proc.Parameters.Add(AArgs[BytesRead]);
    Proc.Options := [poUsePipes];
    try
      Proc.Execute;
      repeat
        BytesRead := Proc.Output.Read(Buf, SizeOf(Buf));
        if BytesRead > 0 then
          OutStream.Write(Buf, BytesRead);
        BytesRead := Proc.Stderr.Read(Buf, SizeOf(Buf));
        if BytesRead > 0 then
          ErrStream.Write(Buf, BytesRead);
      until not Proc.Running;
      repeat
        BytesRead := Proc.Output.Read(Buf, SizeOf(Buf));
        if BytesRead > 0 then
          OutStream.Write(Buf, BytesRead);
      until BytesRead = 0;
      repeat
        BytesRead := Proc.Stderr.Read(Buf, SizeOf(Buf));
        if BytesRead > 0 then
          ErrStream.Write(Buf, BytesRead);
      until BytesRead = 0;
      AExitCode := Proc.ExitStatus;
      AStdout := OutStream.DataString;
      AStderr := ErrStream.DataString;
    except
      on E: Exception do
      begin
        AStderr := E.Message;
        AExitCode := -1;
        Result := False;
      end;
    end;
  finally
    ErrStream.Free;
    OutStream.Free;
    Proc.Free;
  end;
end;

procedure RunFixture(const AFixtureFile: string);
var
  WasmFile, StdoutStr, StderrStr, BaseName: string;
  ExitCode, I: Integer;
  Expected, ActualLines: TStringList;
  Match: Boolean;
begin
  BaseName := ExtractFileName(AFixtureFile);

  if IsSkipped(AFixtureFile) then
  begin
    WriteLn(COLOR_YELLOW, '  SKIP ', COLOR_RESET, BaseName);
    Inc(Skipped);
    Exit;
  end;

  WasmFile := GetTempDir + 'wasm-test-' + ChangeFileExt(BaseName, '.wasm');
  TempFiles.Add(WasmFile);

  if not RunProcess(ScriptLoaderPath,
    [AFixtureFile, '--emit=wasm', '--output=' + WasmFile],
    StdoutStr, StderrStr, ExitCode) or (ExitCode <> 0) then
  begin
    WriteLn(COLOR_RED, '  FAIL ', COLOR_RESET, BaseName, ' (compile error)');
    if StderrStr <> '' then
      WriteLn('       ', Trim(StderrStr));
    Inc(Failed);
    Exit;
  end;

  if not RunProcess('node', [HostScriptPath, WasmFile],
    StdoutStr, StderrStr, ExitCode) then
  begin
    WriteLn(COLOR_RED, '  FAIL ', COLOR_RESET, BaseName, ' (node error)');
    if StderrStr <> '' then
      WriteLn('       ', Trim(StderrStr));
    Inc(Failed);
    Exit;
  end;

  if ExitCode <> 0 then
  begin
    WriteLn(COLOR_RED, '  FAIL ', COLOR_RESET, BaseName, ' (exit code ', ExitCode, ')');
    if StderrStr <> '' then
      WriteLn('       ', Trim(StderrStr));
    Inc(Failed);
    Exit;
  end;

  Expected := ParseExpectedLines(AFixtureFile);
  ActualLines := TStringList.Create;
  try
    ActualLines.Text := StdoutStr;
    while (ActualLines.Count > 0) and (ActualLines[ActualLines.Count - 1] = '') do
      ActualLines.Delete(ActualLines.Count - 1);

    Match := ActualLines.Count = Expected.Count;
    if Match then
      for I := 0 to Expected.Count - 1 do
        if ActualLines[I] <> Expected[I] then
        begin
          Match := False;
          Break;
        end;

    if Match then
    begin
      WriteLn(COLOR_GREEN, '  PASS ', COLOR_RESET, BaseName);
      Inc(Passed);
    end
    else
    begin
      WriteLn(COLOR_RED, '  FAIL ', COLOR_RESET, BaseName, ' (output mismatch)');
      for I := 0 to Expected.Count - 1 do
        WriteLn('       expected: ', Expected[I]);
      for I := 0 to ActualLines.Count - 1 do
        WriteLn('       actual:   ', ActualLines[I]);
      Inc(Failed);
    end;
  finally
    ActualLines.Free;
    Expected.Free;
  end;
end;

procedure CleanupTempFiles;
var
  I: Integer;
begin
  for I := 0 to TempFiles.Count - 1 do
    if FileExists(TempFiles[I]) then
      DeleteFile(TempFiles[I]);
end;

var
  FixtureDir: string;
  Fixtures: TStringList;
  I: Integer;

begin
  ScriptLoaderPath := 'build/ScriptLoader';
  if not FileExists(ScriptLoaderPath) then
    ScriptLoaderPath := './build/ScriptLoader';
  if not FileExists(ScriptLoaderPath) then
  begin
    WriteLn(COLOR_RED, 'Error: ScriptLoader not found. Run ./build.pas loader first.', COLOR_RESET);
    Halt(1);
  end;

  HostScriptPath := 'tests-wasm/souffle-host.mjs';
  if not FileExists(HostScriptPath) then
  begin
    WriteLn(COLOR_RED, 'Error: tests-wasm/souffle-host.mjs not found.', COLOR_RESET);
    Halt(1);
  end;

  if not HasNode then
  begin
    WriteLn(COLOR_YELLOW, 'node not found — skipping WASM integration tests', COLOR_RESET);
    Halt(0);
  end;

  Passed := 0;
  Failed := 0;
  Skipped := 0;
  TempFiles := TStringList.Create;
  try
    WriteLn(COLOR_BOLD, 'WASM Integration Tests', COLOR_RESET);
    WriteLn;

    if ParamCount > 0 then
    begin
      for I := 1 to ParamCount do
        RunFixture(ParamStr(I));
    end
    else
    begin
      FixtureDir := 'tests-wasm/';
      Fixtures := FindAllFiles(FixtureDir, ['.js']);
      try
        Fixtures.Sort;
        for I := 0 to Fixtures.Count - 1 do
          RunFixture(Fixtures[I]);
      finally
        Fixtures.Free;
      end;
    end;

    WriteLn;
    Write(COLOR_BOLD, 'Results: ', COLOR_RESET);
    Write(COLOR_GREEN, Passed, ' passed', COLOR_RESET);
    if Failed > 0 then
      Write(', ', COLOR_RED, Failed, ' failed', COLOR_RESET);
    if Skipped > 0 then
      Write(', ', COLOR_YELLOW, Skipped, ' skipped', COLOR_RESET);
    WriteLn;

    CleanupTempFiles;

    if Failed > 0 then
      Halt(1);
  finally
    TempFiles.Free;
  end;
end.
