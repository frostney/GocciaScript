program REPL;

{$I units/Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Engine, Goccia.Error, Goccia.Values.Primitives;

var
  Line: string;
  Engine: TGocciaEngine;
  Source: TStringList;
  Result: TGocciaValue;

begin
  WriteLn('Goccia REPL');
  
  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('REPL', Source, TGocciaEngine.DefaultGlobals);
  try
    while True do
    begin
      Write('> ');
      ReadLn(Line);
      if Line = 'exit' then
        Break;

      Source.Clear;
      Source.Add(Line);
      
      try
        Result := Engine.Execute;
        if Result <> nil then
          WriteLn(Result.ToStringLiteral.Value);
      except
        on E: Exception do
          WriteLn('Error: ', E.Message);
      end;
    end;
  finally
    Engine.Free;
    Source.Free;
  end;
end.