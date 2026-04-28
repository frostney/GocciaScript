program GocciaREPL;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,
  TextSemantics,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  CLI.Options,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FetchManager,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Parser,
  Goccia.REPL.Formatter,
  Goccia.REPL.LineEditor,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Token,
  Goccia.Values.Error,
  Goccia.Values.Primitives,
  Goccia.Version;

const
  REPL_FILE_NAME = 'REPL';

type
  TREPLApp = class(TGocciaCLIApplication)
  private
    FTiming: TGocciaFlagOption;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

procedure TREPLApp.Configure;
begin
  AddEngineOptions;
  FTiming := AddFlag('timing', 'Show per-line timing');
end;

function TREPLApp.UsageLine: string;
begin
  Result := '[options]';
end;

procedure TREPLApp.ExecuteWithPaths(const APaths: TStringList);
var
  IsBytecodeMode: Boolean;

  Line: string;
  Source: TStringList;
  Editor: TLineEditor;
  ReadResult: TLineReadResult;

  { Interpreted mode }
  Eng: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;

  { Bytecode mode }
  BcExecutor: TGocciaBytecodeExecutor;
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
  if APaths.Count > 0 then
  begin
    WriteLn('Error: Unexpected argument "', APaths[0],
      '". The REPL does not accept file arguments.');
    ExitCode := 1;
    Exit;
  end;

  IsBytecodeMode := EngineOptions.Mode.Matches(emBytecode);

  if IsBytecodeMode then
    WriteLn('Goccia REPL v' + GetVersion + ' (bytecode)')
  else
    WriteLn('Goccia REPL v' + GetVersion + ' (interpreted)');

  Source := TStringList.Create;
  Editor := TLineEditor.Create;

  if IsBytecodeMode then
  begin
    BcExecutor := TGocciaBytecodeExecutor.Create;
    BcExecutor.GlobalBackedTopLevel := True;
    LiveModules := TObjectList<TGocciaBytecodeModule>.Create(True);
    try
      Eng := CreateEngine(REPL_FILE_NAME, Source, BcExecutor);
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

          StartTime := GetNanoseconds;
          LexEnd := StartTime;
          ParseEnd := StartTime;
          CompileEnd := StartTime;
          ExecEnd := StartTime;
          try
            SourceText := StringListToLFText(Source);

            if ppJSX in TGocciaEngine.DefaultPreprocessors then
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
              Parser.AutomaticSemicolonInsertion := Eng.ASIEnabled;
              Parser.VarDeclarationsEnabled := Eng.VarEnabled;
              Parser.FunctionDeclarationsEnabled := Eng.FunctionEnabled;
              try
                ProgramNode := Parser.Parse;
                ParseEnd := GetNanoseconds;

                try
                  Module := TGocciaBytecodeExecutor(Eng.Executor).CompileToModule(ProgramNode);
                  CompileEnd := GetNanoseconds;

                  try
                    ResultValue := TGocciaBytecodeExecutor(Eng.Executor).RunModule(Module);
                    if Assigned(ResultValue) then
                      TGarbageCollector.Instance.AddTempRoot(ResultValue);
                    try
                      WaitForFetchIdle;
                      ExecEnd := GetNanoseconds;

                      if ResultValue <> nil then
                        WriteLn(FormatREPLValue(ResultValue, IsColorTerminal));
                    finally
                      if Assigned(ResultValue) then
                        TGarbageCollector.Instance.RemoveTempRoot(ResultValue);
                    end;
                  finally
                    if Assigned(TGocciaMicrotaskQueue.Instance) then
                      TGocciaMicrotaskQueue.Instance.ClearQueue;
                    DiscardFetchCompletions;
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
            begin
              ExecEnd := GetNanoseconds;
              if E is TGocciaError then
                WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal))
              else if E is TGocciaThrowValue then
                WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value,
                  REPL_FILE_NAME, Source, IsColorTerminal,
                  TGocciaThrowValue(E).Suggestion))
              else
                WriteLn('Error: ', E.Message);
            end;
          end;
          if FTiming.Present then
            WriteLn(SysUtils.Format(
              '  Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
              [FormatDuration(LexEnd - StartTime),
               FormatDuration(ParseEnd - LexEnd),
               FormatDuration(CompileEnd - ParseEnd),
               FormatDuration(ExecEnd - CompileEnd),
               FormatDuration(ExecEnd - StartTime)]));
        end;
      finally
        Eng.Free;
      end;
    finally
      LiveModules.Free;
      BcExecutor.Free;
      Editor.Free;
      Source.Free;
    end;
  end
  else
  begin
    Eng := CreateEngine(REPL_FILE_NAME, Source);
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
          ScriptResult := Eng.Execute;
          if ScriptResult.Result <> nil then
            WriteLn(FormatREPLValue(ScriptResult.Result, IsColorTerminal));
        except
          on E: Exception do
          begin
            if E is TGocciaError then
              WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal))
            else if E is TGocciaThrowValue then
              WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value,
                REPL_FILE_NAME, Source, IsColorTerminal,
                TGocciaThrowValue(E).Suggestion))
            else
              WriteLn('Error: ', E.Message);
          end;
        end;
        if FTiming.Present then
          WriteLn(SysUtils.Format(
            '  Lex: %s | Parse: %s | Execute: %s | Total: %s',
            [FormatDuration(Eng.LastTiming.LexTimeNanoseconds),
             FormatDuration(Eng.LastTiming.ParseTimeNanoseconds),
             FormatDuration(Eng.LastTiming.ExecuteTimeNanoseconds),
             FormatDuration(Eng.LastTiming.TotalTimeNanoseconds)]));
      end;
    finally
      Editor.Free;
      Eng.Free;
      Source.Free;
    end;
  end;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TREPLApp, 'GocciaREPL');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
