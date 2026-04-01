program REPL;

{$I units/Goccia.inc}

uses
  Classes,
  SysUtils,

  TimingUtils,

  Goccia.Engine,
  Goccia.Modules.Configuration,
  Goccia.REPL.Formatter,
  Goccia.REPL.LineEditor,
  Goccia.Values.Primitives,
  Goccia.Version;

var
  Arg: string;
  ImportMapPath: string;
  InlineAliases: TStringList;
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
  ImportMapPath := '';
  InlineAliases := TStringList.Create;
  I := 1;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);
    if Arg = '--timing' then
      ShowTiming := True;
    if Copy(Arg, 1, 13) = '--import-map=' then
    begin
      ImportMapPath := Copy(Arg, 14, MaxInt);
      if ImportMapPath = '' then
      begin
        WriteLn('Error: --import-map requires a non-empty path.');
        ExitCode := 1;
        Exit;
      end;
    end
    else if Copy(Arg, 1, 8) = '--alias=' then
      InlineAliases.Add(Copy(Arg, 9, MaxInt))
    else if Arg = '--alias' then
    begin
      if I = ParamCount then
      begin
        WriteLn('Error: --alias requires a key=value argument.');
        ExitCode := 1;
        Exit;
      end;
      Inc(I);
      InlineAliases.Add(ParamStr(I));
    end
    else if (Arg <> '--timing') and (Arg <> '--help') and (Arg <> '-h') then
    begin
      WriteLn('Error: Unknown option ', Arg);
      ExitCode := 1;
      Exit;
    end;
    Inc(I);
  end;

  if FindCmdLineSwitch('help', ['-'], True) or FindCmdLineSwitch('h', ['-'], False) then
  begin
    WriteLn('Usage: REPL [--timing] [--import-map=file] [--alias key=value]');
    Exit;
  end;

  WriteLn('Goccia REPL v' + GetVersion);

  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('REPL', Source, TGocciaEngine.DefaultGlobals);
  Editor := TLineEditor.Create;
  try
    ConfigureModuleResolver(Engine.Resolver, 'REPL', ImportMapPath,
      InlineAliases);
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
    InlineAliases.Free;
  end;
end.
