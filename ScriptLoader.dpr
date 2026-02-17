program ScriptLoader;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Primitives, Goccia.Engine, TimingUtils, Goccia.Error, FileUtils in 'units/FileUtils.pas';

procedure RunScriptFromFile(const FileName: string);
var
  ScriptResult: TGocciaScriptResult;
begin
  try
    WriteLn('Running script: ', FileName);
    ScriptResult := TGocciaEngine.RunScriptFromFile(FileName, TGocciaEngine.DefaultGlobals);
    WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
      [FormatDuration(ScriptResult.LexTimeMicroseconds), FormatDuration(ScriptResult.ParseTimeMicroseconds),
       FormatDuration(ScriptResult.ExecuteTimeMicroseconds), FormatDuration(ScriptResult.TotalTimeMicroseconds)]));
    Writeln('Result: ', ScriptResult.Result.ToStringLiteral.Value);
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