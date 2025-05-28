{$mode delphi}
{$H+}

program GocciaScriptMain;

uses
  Classes, SysUtils, Generics.Collections, Goccia.Lexer, Goccia.Parser, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node;

// Main program
procedure RunGocciaScript(const FileName: string);
var
  Source: TStringList;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  Interpreter: TGocciaInterpreter;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    
    Lexer := TGocciaLexer.Create(Source.Text, FileName);
    try
    try
        Tokens := Lexer.ScanTokens;
        
        Parser := TGocciaParser.Create(Tokens, FileName, Lexer.SourceLines);
        try
        ProgramNode := Parser.Parse;
        try
            Interpreter := TGocciaInterpreter.Create(FileName, Lexer.SourceLines);
            try
            Interpreter.Execute(ProgramNode);
            finally
            Interpreter.Free;
            end;
        finally
            ProgramNode.Free;
        end;
        finally
        Parser.Free;
        end;
    except
        on E: TGocciaError do
        begin
        WriteLn(E.GetDetailedMessage);
        ExitCode := 1;
        end;
        on E: Exception do
        begin
        WriteLn('Error: ', E.Message);
        ExitCode := 1;
        end;
    end;
    finally
      Lexer.Free;
    end;
  finally
    Source.Free;
  end;
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