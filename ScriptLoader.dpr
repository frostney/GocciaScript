program ScriptLoader;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Engine,
  Goccia.Error,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

procedure RunScriptFromFile(const AFileName: string);
var
  ScriptResult: TGocciaScriptResult;
begin
  try
    WriteLn('Running script: ', AFileName);
    ScriptResult := TGocciaEngine.RunScriptFromFile(AFileName, TGocciaEngine.DefaultGlobals);
    WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
      [FormatDuration(ScriptResult.LexTimeNanoseconds), FormatDuration(ScriptResult.ParseTimeNanoseconds),
       FormatDuration(ScriptResult.ExecuteTimeNanoseconds), FormatDuration(ScriptResult.TotalTimeNanoseconds)]));
    Writeln('Result: ', ScriptResult.Result.ToStringLiteral.Value);
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure RunScripts(const APath: string);
var
  Files: TStringList;
  I: Integer;
begin
  if DirectoryExists(APath) then
  begin
    Files := FindAllFiles(APath);
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
  else if FileExists(APath) then
    RunScriptFromFile(APath)
  else
  begin
    WriteLn('Error: Path not found: ', APath);
    ExitCode := 1;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ScriptLoader <filename.js|.jsx|.ts|.tsx>');
    WriteLn('or');
    WriteLn('Usage: ScriptLoader <directory>');
    ExitCode := 1;
  end
  else
    RunScripts(ParamStr(1));
end.
