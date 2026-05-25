program GocciaREPL;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TimingUtils,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  Goccia.CLI.Options,
  CLI.ConfigFile,
  CLI.Options,
  Goccia.Engine,
  Goccia.Executor.Interpreter,
  Goccia.Executor.Bytecode,
  Goccia.Executor,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.REPL.Formatter,
  Goccia.REPL.LineEditor,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.FFI,
  Goccia.RuntimeProfiles.Loader,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Values.Error,
  Goccia.Values.Primitives,
  Goccia.Version;

const
  REPL_FILE_NAME = 'REPL';

type
  TREPLApp = class(TGocciaCLIApplication)
  private
    FTiming: TFlagOption;
    procedure InitializeRuntime(const AEngine: TGocciaEngine);
  protected
    procedure Configure; override;
    procedure ConfigureCreatedEngine(const AEngine: TGocciaEngine;
      const AFileConfig: TConfigEntryArray); override;
    function UsageLine: string; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

procedure TREPLApp.Configure;
begin
  AddEngineOptions;
  FTiming := AddFlag('timing', 'Show per-line timing');
end;

procedure TREPLApp.InitializeRuntime(const AEngine: TGocciaEngine);
var
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := AttachRuntime(AEngine);
  TGocciaLoaderRuntimeProfile.Apply(Runtime);
end;

procedure TREPLApp.ConfigureCreatedEngine(const AEngine: TGocciaEngine;
  const AFileConfig: TConfigEntryArray);
var
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  Runtime: TGocciaRuntimeCore;
begin
  InitializeRuntime(AEngine);
  Runtime := GetRuntime(AEngine);
  if Assigned(EngineOptions) and
     ResolveFlagOption(EngineOptions.UnsafeFFI, AFileConfig) then
    Runtime.Install(TGocciaFFIRuntimeExtension.Create);
  ConsoleExtension := TGocciaConsoleRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
  if LogFileOpen and Assigned(ConsoleExtension) and
     Assigned(ConsoleExtension.BuiltinConsole) then
    ConsoleExtension.BuiltinConsole.LogCallback := HandleConsoleLog;
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

  { Interpreter mode }
  Eng: TGocciaEngine;
  InterpExecutor: TGocciaInterpreterExecutor;
  ScriptResult: TGocciaScriptResult;

  { Bytecode mode }
  BcExecutor: TGocciaBytecodeExecutor;
  ResultValue: TGocciaValue;
  PipelineOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  Module: TGocciaCompiledModule;
  StartTime, CompileStart, CompileEnd, ExecStart, ExecEnd: Int64;
  LexTimeNanoseconds, ParseTimeNanoseconds: Int64;
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
          CompileStart := StartTime;
          CompileEnd := StartTime;
          ExecStart := StartTime;
          ExecEnd := StartTime;
          LexTimeNanoseconds := 0;
          ParseTimeNanoseconds := 0;
          PipelineResult := nil;
          try
            PipelineOptions.Preprocessors := Eng.Preprocessors;
            PipelineOptions.Compatibility := Eng.Compatibility;
            PipelineOptions.SourceType := Eng.SourceType;
            PipelineResult := TGocciaSourcePipeline.Parse(Source,
              REPL_FILE_NAME, PipelineOptions);
            LexTimeNanoseconds := PipelineResult.LexTimeNanoseconds;
            ParseTimeNanoseconds := PipelineResult.ParseTimeNanoseconds;

            CompileStart := GetNanoseconds;
            try
              Module := Eng.CompileModule(PipelineResult.ProgramNode);
              CompileEnd := GetNanoseconds;
            finally
              PipelineResult.Free;
              PipelineResult := nil;
            end;

            ExecStart := GetNanoseconds;
            ResultValue := Eng.RunModuleForSourceType(Module,
              REPL_FILE_NAME);
            ExecEnd := GetNanoseconds;

            if ResultValue <> nil then
              WriteLn(FormatREPLValue(ResultValue, IsColorTerminal));
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
              [FormatDuration(LexTimeNanoseconds),
               FormatDuration(ParseTimeNanoseconds),
               FormatDuration(CompileEnd - CompileStart),
               FormatDuration(ExecEnd - ExecStart),
               FormatDuration(ExecEnd - StartTime)]));
        end;
      finally
        Eng.Free;
      end;
    finally
      BcExecutor.Free;
      Editor.Free;
      Source.Free;
    end;
  end
  else
  begin
    InterpExecutor := TGocciaInterpreterExecutor.Create;
    try
      Eng := CreateEngine(REPL_FILE_NAME, Source, InterpExecutor);
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
        Eng.Free;
      end;
    finally
      InterpExecutor.Free;
      Editor.Free;
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
