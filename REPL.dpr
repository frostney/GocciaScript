program REPL;

{$I units/Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Modules.Configuration,
  Goccia.Parser,
  Goccia.REPL.Formatter,
  Goccia.REPL.LineEditor,
  Goccia.TextFiles,
  Goccia.Token,
  Goccia.Values.Primitives,
  Goccia.Version;

const
  REPL_FILE_NAME = 'REPL';

var
  Arg, ModeStr: string;
  ImportMapPath: string;
  InlineAliases: TStringList;
  Line: string;
  Source: TStringList;
  ShowTiming: Boolean;
  IsBytecodeMode: Boolean;
  I: Integer;
  Editor: TLineEditor;
  ReadResult: TLineReadResult;

  { Interpreted mode }
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;

  { Bytecode mode }
  Backend: TGocciaBytecodeBackend;
  LiveModules: TObjectList<TGocciaBytecodeModule>;
  ResultValue: TGocciaValue;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  StartTime, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;

begin
  ShowTiming := False;
  IsBytecodeMode := False;
  ImportMapPath := '';
  InlineAliases := TStringList.Create;
  I := 1;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);
    if Arg = '--timing' then
      ShowTiming := True
    else if Copy(Arg, 1, 7) = '--mode=' then
    begin
      ModeStr := Copy(Arg, 8, MaxInt);
      if ModeStr = 'interpreted' then
        IsBytecodeMode := False
      else if ModeStr = 'bytecode' then
        IsBytecodeMode := True
      else
      begin
        WriteLn('Error: Unknown mode "', ModeStr, '". Use "interpreted" or "bytecode".');
        ExitCode := 1;
        Exit;
      end;
    end
    else if Copy(Arg, 1, 13) = '--import-map=' then
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
    WriteLn('Usage: REPL [--timing] [--mode=interpreted|bytecode] [--import-map=file] [--alias key=value]');
    Exit;
  end;

  if IsBytecodeMode then
    WriteLn('Goccia REPL v' + GetVersion + ' (bytecode)')
  else
    WriteLn('Goccia REPL v' + GetVersion + ' (interpreted)');

  Source := TStringList.Create;
  Editor := TLineEditor.Create;

  if IsBytecodeMode then
  begin
    Backend := TGocciaBytecodeBackend.Create(REPL_FILE_NAME);
    LiveModules := TObjectList<TGocciaBytecodeModule>.Create(True);
    try
      Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);
      Backend.GlobalBackedTopLevel := True;
      ConfigureModuleResolver(Backend.ModuleResolver, REPL_FILE_NAME,
        ImportMapPath, InlineAliases);
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
          StartTime := GetNanoseconds;
          SourceText := StringListToLFText(Source);

          if ggJSX in TGocciaEngine.DefaultGlobals then
          begin
            JSXResult := TGocciaJSXTransformer.Transform(SourceText);
            SourceText := JSXResult.Source;
            JSXResult.SourceMap.Free;
          end;

          Lexer := TGocciaLexer.Create(SourceText, REPL_FILE_NAME);
          try
            Tokens := Lexer.ScanTokens;
            LexEnd := GetNanoseconds;

            Parser := TGocciaParser.Create(Tokens, REPL_FILE_NAME,
              Lexer.SourceLines);
            try
              ProgramNode := Parser.Parse;
              ParseEnd := GetNanoseconds;

              try
                Module := Backend.CompileToModule(ProgramNode);
                CompileEnd := GetNanoseconds;

                try
                  ResultValue := Backend.RunModule(Module);
                  if Assigned(TGocciaMicrotaskQueue.Instance) then
                    TGocciaMicrotaskQueue.Instance.DrainQueue;
                  ExecEnd := GetNanoseconds;

                  if ResultValue <> nil then
                    WriteLn(FormatREPLValue(ResultValue));
                  if ShowTiming then
                    WriteLn(SysUtils.Format(
                      '  Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
                      [FormatDuration(LexEnd - StartTime),
                       FormatDuration(ParseEnd - LexEnd),
                       FormatDuration(CompileEnd - ParseEnd),
                       FormatDuration(ExecEnd - CompileEnd),
                       FormatDuration(ExecEnd - StartTime)]));
                finally
                  if Assigned(TGocciaMicrotaskQueue.Instance) then
                    TGocciaMicrotaskQueue.Instance.ClearQueue;
                  LiveModules.Add(Module);
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
        except
          on E: Exception do
            WriteLn('Error: ', E.Message);
        end;
      end;
    finally
      LiveModules.Free;
      Backend.Free;
      Editor.Free;
      Source.Free;
      InlineAliases.Free;
    end;
  end
  else
  begin
    Engine := TGocciaEngine.Create(REPL_FILE_NAME, Source,
      TGocciaEngine.DefaultGlobals);
    try
      ConfigureModuleResolver(Engine.Resolver, REPL_FILE_NAME, ImportMapPath,
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
            WriteLn(SysUtils.Format(
              '  Lex: %s | Parse: %s | Execute: %s | Total: %s',
              [FormatDuration(ScriptResult.LexTimeNanoseconds),
               FormatDuration(ScriptResult.ParseTimeNanoseconds),
               FormatDuration(ScriptResult.ExecuteTimeNanoseconds),
               FormatDuration(ScriptResult.TotalTimeNanoseconds)]));
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
  end;
end.
