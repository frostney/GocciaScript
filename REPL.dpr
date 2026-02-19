program REPL;

{$I units/Goccia.inc}

uses
  Classes,
  SysUtils,

  TimingUtils,

  Goccia.Engine,
  Goccia.REPL.Formatter,
  Goccia.REPL.LineEditor,
  Goccia.Values.Primitives,
  Goccia.Version;

var
  Line: string;
  Engine: TGocciaEngine;
  Source: TStringList;
  ScriptResult: TGocciaScriptResult;
  ShowTiming: Boolean;
  I: Integer;
  Editor: TLineEditor;
  ReadResult: TLineReadResult;

begin
  ShowTiming := False;
  for I := 1 to ParamCount do
  begin
    if ParamStr(I) = '--timing' then
      ShowTiming := True;
  end;

  WriteLn('Goccia REPL v' + GetVersion);

  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('REPL', Source, TGocciaEngine.DefaultGlobals);
  Editor := TLineEditor.Create;
  try
    while True do
    begin
      ReadResult := Editor.ReadLine('> ', Line);
      if ReadResult = lrExit then
        Break;
      if Line = 'exit' then
        Break;
      if Line = '' then
        Continue;

      Editor.AddToHistory(Line);
      Source.Clear;
      Source.Add(Line);

      try
        ScriptResult := Engine.Execute;
        if ScriptResult.Result <> nil then
          WriteLn(FormatREPLValue(ScriptResult.Result));
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
    Editor.Free;
    Engine.Free;
    Source.Free;
  end;
end.
