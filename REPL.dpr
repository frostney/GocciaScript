program REPL;

{$I units/Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Lexer, Goccia.Parser, Goccia.Interpreter, Goccia.Error, Goccia.Token, Goccia.AST.Node;

var
  Line: string;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  Interpreter: TGocciaInterpreter;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;

begin
  WriteLn('Goccia REPL');

  while True do
  begin
    Write('> ');
    ReadLn(Line);
    if Line = 'exit' then
      Break;

    Lexer := TGocciaLexer.Create(Line, 'REPL');
    Tokens := Lexer.ScanTokens;
    Parser := TGocciaParser.Create(Tokens, 'REPL', Lexer.SourceLines);
    ProgramNode := Parser.Parse;
    Interpreter := TGocciaInterpreter.Create('REPL', Lexer.SourceLines);
    try
      Interpreter.Execute(ProgramNode);
    finally
      Interpreter.Free;
      ProgramNode.Free;
      Parser.Free;
      Lexer.Free;
      Tokens.Free;
    end;
  end;
end.