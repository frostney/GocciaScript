program GocciaScript;

{$I units/Goccia.inc}

uses
  Goccia.Interpreter, Goccia.Parser, Goccia.Lexer, Goccia.AST.Node, Classes, SysUtils;

var
  Source: TStringList;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Interpreter: TGocciaInterpreter;
  Result: TGocciaValue;
begin
  try
    Source := TStringList.Create;
    try
      // Simple test program
      Source.Text := 'console.log("Hello, World!");';

      Lexer := TGocciaLexer.Create(Source.Text, 'test.gs');
      try
        Parser := TGocciaParser.Create(Lexer.ScanTokens, 'test.gs', Source);
        try
          ProgramNode := Parser.Parse;
          try
            Interpreter := TGocciaInterpreter.Create('test.gs', Source);
            try
              Result := Interpreter.Execute(ProgramNode);
              WriteLn('Program result: ', Result.ToString);
            finally
              Interpreter.Free;
            end;
          finally
            ProgramNode.Free;
          end;
        finally
          Parser.Free;
        end;
      finally
        Lexer.Free;
      end;
    finally
      Source.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
