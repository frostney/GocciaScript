program ScriptLoader;

{$I units/Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Base, Goccia.Lexer, Goccia.Parser, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node;

function RunGocciaScript(const FileName: string): TGocciaValue;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.LoadFromFile(FileName);
  Result := RunGocciaScriptFromStringList(Source, FileName).Value;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: GocciaScript <filename.js>');
    ExitCode := 1;
  end
  else
  begin
    try
      RunGocciaScript(ParamStr(1));
    except
      on E: Exception do
      begin
        WriteLn('Fatal error: ', E.Message);
        ExitCode := 1;
      end;
    end;
  end;
end.