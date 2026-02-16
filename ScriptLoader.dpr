program ScriptLoader;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Primitives, Goccia.Engine, Goccia.Error, FileUtils in 'units/FileUtils.pas';

function RunGocciaScript(const FileName: string): TGocciaValue;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    Result := TGocciaEngine.RunScriptFromStringList(Source, FileName, TGocciaEngine.DefaultGlobals);
  finally
    Source.Free;
  end;
end;

procedure RunScriptFromFile(const FileName: string);
var
  ScriptResult: TGocciaValue;
begin
  try
    WriteLn('Running script: ', FileName);
    ScriptResult := RunGocciaScript(FileName);
    Writeln('Result: ', ScriptResult.ToStringLiteral.Value);
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure RunScripts(const Path: string);
var
  Files: TStringList;
  I: Integer;
begin
  if DirectoryExists(Path) then
  begin
    Files := FindAllFiles(Path, '.js');
    try
      for I := 0 to Files.Count - 1 do
      begin
        if I > 0 then WriteLn;
        RunScriptFromFile(Files[I]);
      end;
    finally
      Files.Free;
    end;
  end
  else if FileExists(Path) then
    RunScriptFromFile(Path)
  else
  begin
    WriteLn('Error: Path not found: ', Path);
    ExitCode := 1;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ScriptLoader <filename.js>');
    WriteLn('or');
    WriteLn('Usage: ScriptLoader <directory>');
    ExitCode := 1;
  end
  else
    RunScripts(ParamStr(1));
end.