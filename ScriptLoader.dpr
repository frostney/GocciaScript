program ScriptLoader;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Core, Goccia.Lexer, Goccia.Parser, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node, FileUtils in 'units/FileUtils.pas';

function RunGocciaScript(const FileName: string): TGocciaValue;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    Result := RunGocciaScriptFromStringList(Source, FileName, TGocciaInterpreter.DefaultGlobals).Value;
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

var
  Files: TStringList;
  ScriptResult: TGocciaValue;
  I: Integer;

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
      for I := 0 to Files.Count - 1 do
      begin
        RunScriptFromFile(Files[I]);
      end;
    end
    else
    begin
      RunScriptFromFile(ParamStr(1));
    end;
  end;
end.