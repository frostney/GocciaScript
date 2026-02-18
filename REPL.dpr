program REPL;

{$I units/Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Engine,
  Goccia.Error,
  Goccia.Values.Primitives;

var
  Line: string;
  Engine: TGocciaEngine;
  Source: TStringList;
  ScriptResult: TGocciaScriptResult;
  ShowTiming: Boolean;
  I: Integer;

begin
  ShowTiming := False;
  for I := 1 to ParamCount do
  begin
    if ParamStr(I) = '--timing' then
      ShowTiming := True;
  end;

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
        ScriptResult := Engine.Execute;
        if ScriptResult.Result <> nil then
          WriteLn(ScriptResult.Result.ToStringLiteral.Value);
        if ShowTiming then
          WriteLn(SysUtils.Format('  [%s lex | %s parse | %s exec | %s total]',
            [FormatDuration(ScriptResult.LexTimeNanoseconds), FormatDuration(ScriptResult.ParseTimeNanoseconds),
             FormatDuration(ScriptResult.ExecuteTimeNanoseconds), FormatDuration(ScriptResult.TotalTimeNanoseconds)]));
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
