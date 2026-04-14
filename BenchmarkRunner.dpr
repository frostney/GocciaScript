program BenchmarkRunner;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Benchmark.Reporter,
  Goccia.Builtins.Benchmark,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  Goccia.CLI.Options,
  Goccia.Compiler,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Engine.BytecodeBackend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.ScriptLoader.Input,
  Goccia.SourceMap,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception,

  FileUtils in 'units/FileUtils.pas';

type
  TReportSpec = record
    Format: TBenchmarkReportFormat;
    OutputFile: string;
  end;

  TBenchmarkProgress = class
    class procedure OnProgress(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer);
  end;

class procedure TBenchmarkProgress.OnProgress(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer);
begin
  if ASuiteName <> '' then
    WriteLn(SysUtils.Format('  [%d/%d] %s > %s', [AIndex, ATotal, ASuiteName, ABenchName]))
  else
    WriteLn(SysUtils.Format('  [%d/%d] %s', [AIndex, ATotal, ABenchName]));
end;

procedure PopulateFileResult(const AFileResult: TBenchmarkFileResult;
  const AScriptResult: TGocciaObjectValue; const AReporter: TBenchmarkReporter);
var
  ResultsArray: TGocciaArrayValue;
  SingleResult: TGocciaObjectValue;
  MutableFileResult: TBenchmarkFileResult;
  Entry: TBenchmarkEntry;
  ErrorMsg: string;
  I, EntryCount: Integer;
begin
  MutableFileResult := AFileResult;

  if AScriptResult <> nil then
  begin
    MutableFileResult.TotalBenchmarks := Round(AScriptResult.GetProperty('totalBenchmarks').ToNumberLiteral.Value);
    MutableFileResult.DurationNanoseconds := Round(AScriptResult.GetProperty('durationNanoseconds').ToNumberLiteral.Value);

    if AScriptResult.GetProperty('results') is TGocciaArrayValue then
    begin
      ResultsArray := TGocciaArrayValue(AScriptResult.GetProperty('results'));
      EntryCount := ResultsArray.GetLength;
      SetLength(MutableFileResult.Entries, EntryCount);

      for I := 0 to EntryCount - 1 do
      begin
        SingleResult := TGocciaObjectValue(ResultsArray.GetElement(I));

        Entry.Suite := SingleResult.GetProperty('suite').ToStringLiteral.Value;
        Entry.Name := SingleResult.GetProperty(PROP_NAME).ToStringLiteral.Value;

        ErrorMsg := SingleResult.GetProperty('error').ToStringLiteral.Value;
        if ErrorMsg <> 'undefined' then
        begin
          Entry.Error := ErrorMsg;
          Entry.OpsPerSec := 0;
          Entry.MeanMs := 0;
          Entry.Iterations := 0;
          Entry.VariancePercentage := 0;
          Entry.SetupMs := 0;
          Entry.TeardownMs := 0;
        end
        else
        begin
          Entry.Error := '';
          Entry.OpsPerSec := SingleResult.GetProperty('opsPerSec').ToNumberLiteral.Value;
          Entry.MeanMs := SingleResult.GetProperty('meanMs').ToNumberLiteral.Value;
          Entry.Iterations := Round(SingleResult.GetProperty('iterations').ToNumberLiteral.Value);
          Entry.VariancePercentage := SingleResult.GetProperty('variancePercentage').ToNumberLiteral.Value;
          Entry.SetupMs := SingleResult.GetProperty('setupMs').ToNumberLiteral.Value;
          Entry.TeardownMs := SingleResult.GetProperty('teardownMs').ToNumberLiteral.Value;
          Entry.MinOpsPerSec := SingleResult.GetProperty('minOpsPerSec').ToNumberLiteral.Value;
          Entry.MaxOpsPerSec := SingleResult.GetProperty('maxOpsPerSec').ToNumberLiteral.Value;
        end;

        MutableFileResult.Entries[I] := Entry;
      end;
    end;

    AReporter.AddFileResult(MutableFileResult);
  end
  else
  begin
    MutableFileResult.TotalBenchmarks := 0;
    MutableFileResult.DurationNanoseconds := 0;
    SetLength(MutableFileResult.Entries, 0);
    AReporter.AddFileResult(MutableFileResult);
  end;
end;

procedure MakeErrorFileResult(const AFileName, AMessage: string;
  const AReporter: TBenchmarkReporter);
var
  FileResult: TBenchmarkFileResult;
begin
  FileResult.FileName := AFileName;
  FileResult.LexTimeNanoseconds := 0;
  FileResult.ParseTimeNanoseconds := 0;
  FileResult.CompileTimeNanoseconds := 0;
  FileResult.ExecuteTimeNanoseconds := 0;
  FileResult.TotalBenchmarks := 0;
  FileResult.DurationNanoseconds := 0;
  SetLength(FileResult.Entries, 1);
  FileResult.Entries[0].Suite := '';
  FileResult.Entries[0].Name := '(fatal)';
  FileResult.Entries[0].Error := AMessage;
  AReporter.AddFileResult(FileResult);
end;

type
  TBenchmarkRunnerApp = class(TGocciaCLIApplication)
  private
    FNoProgress: TGocciaFlagOption;
    FFormats: TGocciaRepeatableOption;
    FOutputFile: TGocciaStringOption;
    procedure CollectBenchmarkFileInterpreted(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
    procedure CollectBenchmarkFileBytecode(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
    procedure CollectBenchmarkFile(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AMode: TGocciaEngineBackend;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSourceInterpreted(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSourceBytecode(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSource(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AMode: TGocciaEngineBackend; const AShowProgress: Boolean);
    procedure RunBenchmarksFromStdin(const AReports: array of TReportSpec;
      const AMode: TGocciaEngineBackend; const AShowProgress: Boolean);
    procedure RunBenchmarks(const APaths: TStringList;
      const AReports: array of TReportSpec;
      const AMode: TGocciaEngineBackend; const AShowProgress: Boolean);
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    function GlobalBuiltins: TGocciaGlobalBuiltins; override;
  end;

procedure TBenchmarkRunnerApp.CollectBenchmarkFileInterpreted(
  const AFileName: string; const AReporter: TBenchmarkReporter;
  const AShowProgress: Boolean);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  EngineResult: TGocciaScriptResult;
  ScriptResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
begin
  Source := ReadUTF8FileLines(AFileName);
  try
    Source.Add('runBenchmarks();');

    try
      Engine := CreateEngine(AFileName, Source);
      try
        if AShowProgress and Assigned(Engine.BuiltinBenchmark) then
          Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        try
          EngineResult := Engine.Execute;
        finally
          ClearExecutionTimeout;
        end;
        FileResult.FileName := AFileName;
        FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
        FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
        FileResult.CompileTimeNanoseconds := 0;
        FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds;

        GC := TGarbageCollector.Instance;
        ScriptResult := nil;
        if EngineResult.Result is TGocciaObjectValue then
          ScriptResult := TGocciaObjectValue(EngineResult.Result);

        if Assigned(ScriptResult) and Assigned(GC) then
          GC.AddTempRoot(ScriptResult);
        try
          PopulateFileResult(FileResult, ScriptResult, AReporter);
        finally
          if Assigned(ScriptResult) and Assigned(GC) then
            GC.RemoveTempRoot(ScriptResult);
        end;
      finally
        Engine.Free;
      end;
    except
      on E: TGocciaError do
      begin
        WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
        MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
      end;
      on E: TGocciaThrowValue do
      begin
        WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, Source, IsColorTerminal));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.Value, AFileName, Source, False), AReporter);
      end;
      on E: Exception do
        MakeErrorFileResult(AFileName, E.Message, AReporter);
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.Collect;
    Source.Free;
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkFileBytecode(
  const AFileName: string; const AReporter: TBenchmarkReporter;
  const AShowProgress: Boolean);
var
  Source: TStringList;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  GC: TGarbageCollector;
  ResultValue: TGocciaValue;
  FileResult: TBenchmarkFileResult;
  ScriptResult: TGocciaObjectValue;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  Source := ReadUTF8FileLines(AFileName);
  try
    Source.Add('runBenchmarks();');

    SourceText := StringListToLFText(Source);
    if ppJSX in TGocciaEngine.DefaultPreprocessors then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(SourceText);
      SourceText := JSXResult.Source;
      JSXResult.SourceMap.Free;
    end;

    try
      Backend := CreateBytecodeBackend(AFileName);
      try
        LexStart := GetNanoseconds;
        Lexer := TGocciaLexer.Create(SourceText, AFileName);
        try
          Tokens := Lexer.ScanTokens;
          LexEnd := GetNanoseconds;

          Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
          Parser.AutomaticSemicolonInsertion := EngineOptions.ASI.Present;
          try
            ProgramNode := Parser.Parse;
            ParseEnd := GetNanoseconds;

            try
              Module := Backend.CompileToModule(ProgramNode);
              CompileEnd := GetNanoseconds;
            finally
              ProgramNode.Free;
            end;
          finally
            Parser.Free;
          end;
        finally
          Lexer.Free;
        end;

        if AShowProgress and Assigned(Backend.Bootstrap) and
           Assigned(Backend.Bootstrap.BuiltinBenchmark) then
          Backend.Bootstrap.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;
        if Assigned(Backend.Bootstrap) and Assigned(Backend.Bootstrap.BuiltinBenchmark) then
          Backend.Bootstrap.BuiltinBenchmark.OnBeforeMeasurement := Backend.ClearTransientCaches;

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        try
          try
            ResultValue := Backend.RunModule(Module);
          finally
            ClearExecutionTimeout;
          end;
          ExecEnd := GetNanoseconds;

          FileResult.FileName := AFileName;
          FileResult.LexTimeNanoseconds := LexEnd - LexStart;
          FileResult.ParseTimeNanoseconds := ParseEnd - LexEnd;
          FileResult.CompileTimeNanoseconds := CompileEnd - ParseEnd;
          FileResult.ExecuteTimeNanoseconds := ExecEnd - CompileEnd;

          GC := TGarbageCollector.Instance;
          if Assigned(ResultValue) and Assigned(GC) then
            GC.AddTempRoot(ResultValue);

          ScriptResult := nil;
          if ResultValue is TGocciaObjectValue then
            ScriptResult := TGocciaObjectValue(ResultValue);
          try
            PopulateFileResult(FileResult, ScriptResult, AReporter);
          finally
            if Assigned(ResultValue) and Assigned(GC) then
              GC.RemoveTempRoot(ResultValue);
          end;
        finally
          Module.Free;
        end;
      finally
        Backend.Free;
      end;
    except
      on E: TGocciaError do
      begin
        WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
        MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
      end;
      on E: TGocciaThrowValue do
      begin
        WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, Source, IsColorTerminal));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.Value, AFileName, Source, False), AReporter);
      end;
      on E: EGocciaBytecodeThrow do
      begin
        WriteLn(StdErr, FormatThrowDetail(E.ThrownValue, AFileName, Source, IsColorTerminal));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.ThrownValue, AFileName, Source, False), AReporter);
      end;
      on E: Exception do
        MakeErrorFileResult(AFileName, E.Message, AReporter);
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.Collect;
    Source.Free;
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkFile(const AFileName: string;
  const AReporter: TBenchmarkReporter; const AMode: TGocciaEngineBackend;
  const AShowProgress: Boolean);
begin
  case AMode of
    ebTreeWalk:  CollectBenchmarkFileInterpreted(AFileName, AReporter,
      AShowProgress);
    ebBytecode: CollectBenchmarkFileBytecode(AFileName, AReporter,
      AShowProgress);
  else
    raise Exception.CreateFmt('Unsupported execution backend: %d', [Ord(AMode)]);
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkSourceInterpreted(
  const ASource: TStringList; const AFileName: string;
  const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
var
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  EngineResult: TGocciaScriptResult;
  ScriptResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
begin
  ASource.Add('runBenchmarks();');

  try
    Engine := CreateEngine(AFileName, ASource);
    try
      if AShowProgress and Assigned(Engine.BuiltinBenchmark) then
        Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      try
        EngineResult := Engine.Execute;
      finally
        ClearExecutionTimeout;
      end;
      FileResult.FileName := AFileName;
      FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
      FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
      FileResult.CompileTimeNanoseconds := 0;
      FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds;

      GC := TGarbageCollector.Instance;
      ScriptResult := nil;
      if EngineResult.Result is TGocciaObjectValue then
        ScriptResult := TGocciaObjectValue(EngineResult.Result);

      if Assigned(ScriptResult) and Assigned(GC) then
        GC.AddTempRoot(ScriptResult);
      try
        PopulateFileResult(FileResult, ScriptResult, AReporter);
      finally
        if Assigned(ScriptResult) and Assigned(GC) then
          GC.RemoveTempRoot(ScriptResult);
      end;
    finally
      Engine.Free;
    end;
  except
    on E: TGocciaError do
    begin
      WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
      MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
    end;
    on E: TGocciaThrowValue do
    begin
      WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, ASource, IsColorTerminal));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.Value, AFileName, ASource, False), AReporter);
    end;
    on E: Exception do
      MakeErrorFileResult(AFileName, E.Message, AReporter);
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkSourceBytecode(
  const ASource: TStringList; const AFileName: string;
  const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
var
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  GC: TGarbageCollector;
  ResultValue: TGocciaValue;
  FileResult: TBenchmarkFileResult;
  ScriptResult: TGocciaObjectValue;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  ASource.Add('runBenchmarks();');

  SourceText := StringListToLFText(ASource);
  if ppJSX in TGocciaEngine.DefaultPreprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    JSXResult.SourceMap.Free;
  end;

  try
    Backend := CreateBytecodeBackend(AFileName);
    try
      LexStart := GetNanoseconds;
      Lexer := TGocciaLexer.Create(SourceText, AFileName);
      try
        Tokens := Lexer.ScanTokens;
        LexEnd := GetNanoseconds;

        Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
        Parser.AutomaticSemicolonInsertion := EngineOptions.ASI.Present;
        try
          ProgramNode := Parser.Parse;
          ParseEnd := GetNanoseconds;

          try
            Module := Backend.CompileToModule(ProgramNode);
            CompileEnd := GetNanoseconds;
          finally
            ProgramNode.Free;
          end;
        finally
          Parser.Free;
        end;
      finally
        Lexer.Free;
      end;

      if AShowProgress and Assigned(Backend.Bootstrap) and
         Assigned(Backend.Bootstrap.BuiltinBenchmark) then
        Backend.Bootstrap.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;
      if Assigned(Backend.Bootstrap) and Assigned(Backend.Bootstrap.BuiltinBenchmark) then
        Backend.Bootstrap.BuiltinBenchmark.OnBeforeMeasurement := Backend.ClearTransientCaches;

      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      try
        try
          ResultValue := Backend.RunModule(Module);
        finally
          ClearExecutionTimeout;
        end;
        ExecEnd := GetNanoseconds;

        FileResult.FileName := AFileName;
        FileResult.LexTimeNanoseconds := LexEnd - LexStart;
        FileResult.ParseTimeNanoseconds := ParseEnd - LexEnd;
        FileResult.CompileTimeNanoseconds := CompileEnd - ParseEnd;
        FileResult.ExecuteTimeNanoseconds := ExecEnd - CompileEnd;

        GC := TGarbageCollector.Instance;
        if Assigned(ResultValue) and Assigned(GC) then
          GC.AddTempRoot(ResultValue);

        ScriptResult := nil;
        if ResultValue is TGocciaObjectValue then
          ScriptResult := TGocciaObjectValue(ResultValue);
        try
          PopulateFileResult(FileResult, ScriptResult, AReporter);
        finally
          if Assigned(ResultValue) and Assigned(GC) then
            GC.RemoveTempRoot(ResultValue);
        end;
      finally
        Module.Free;
      end;
    finally
      Backend.Free;
    end;
  except
    on E: TGocciaError do
    begin
      WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
      MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
    end;
    on E: TGocciaThrowValue do
    begin
      WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, ASource, IsColorTerminal));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.Value, AFileName, ASource, False), AReporter);
    end;
    on E: EGocciaBytecodeThrow do
    begin
      WriteLn(StdErr, FormatThrowDetail(E.ThrownValue, AFileName, ASource, IsColorTerminal));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.ThrownValue, AFileName, ASource, False), AReporter);
    end;
    on E: Exception do
      MakeErrorFileResult(AFileName, E.Message, AReporter);
  end;
end;

procedure TBenchmarkRunnerApp.CollectBenchmarkSource(const ASource: TStringList;
  const AFileName: string; const AReporter: TBenchmarkReporter;
  const AMode: TGocciaEngineBackend; const AShowProgress: Boolean);
begin
  case AMode of
    ebTreeWalk: CollectBenchmarkSourceInterpreted(ASource, AFileName,
      AReporter, AShowProgress);
    ebBytecode: CollectBenchmarkSourceBytecode(ASource, AFileName,
      AReporter, AShowProgress);
  else
    raise Exception.CreateFmt('Unsupported execution backend: %d', [Ord(AMode)]);
  end;
end;

procedure TBenchmarkRunnerApp.RunBenchmarksFromStdin(
  const AReports: array of TReportSpec;
  const AMode: TGocciaEngineBackend; const AShowProgress: Boolean);
var
  Source: TStringList;
  Reporter: TBenchmarkReporter;
  J: Integer;
begin
  Source := ReadSourceFromText(Input);
  Reporter := TBenchmarkReporter.Create;
  try
    CollectBenchmarkSource(Source, STDIN_FILE_NAME, Reporter,
      AMode, AShowProgress);
    for J := 0 to Length(AReports) - 1 do
    begin
      Reporter.Render(AReports[J].Format);
      if AReports[J].OutputFile <> '' then
        Reporter.WriteToFile(AReports[J].OutputFile)
      else
        Reporter.WriteToStdOut;
    end;

    if Reporter.HasFailures then
      ExitCode := 1;
  finally
    Reporter.Free;
    Source.Free;
  end;
end;

procedure TBenchmarkRunnerApp.RunBenchmarks(const APaths: TStringList;
  const AReports: array of TReportSpec;
  const AMode: TGocciaEngineBackend; const AShowProgress: Boolean);
var
  Files: TStringList;
  I, J, P: Integer;
  Reporter: TBenchmarkReporter;
begin
  Files := TStringList.Create;
  Reporter := TBenchmarkReporter.Create;
  try
    for P := 0 to APaths.Count - 1 do
    begin
      if DirectoryExists(APaths[P]) then
        Files.AddStrings(FindAllFiles(APaths[P], ScriptExtensions))
      else if FileExists(APaths[P]) then
        Files.Add(APaths[P])
      else
      begin
        WriteLn('Error: Path not found: ', APaths[P]);
        ExitCode := 1;
        Exit;
      end;
    end;

    for I := 0 to Files.Count - 1 do
    begin
      if AShowProgress then
        WriteLn(SysUtils.Format('[%d/%d] %s', [I + 1, Files.Count, ExtractFileName(Files[I])]));
      CollectBenchmarkFile(Files[I], Reporter, AMode, AShowProgress);
    end;
    if AShowProgress and (Files.Count > 0) then
      WriteLn;

    for J := 0 to Length(AReports) - 1 do
    begin
      Reporter.Render(AReports[J].Format);
      if AReports[J].OutputFile <> '' then
        Reporter.WriteToFile(AReports[J].OutputFile)
      else
        Reporter.WriteToStdOut;
    end;

    if Reporter.HasFailures then
      ExitCode := 1;
  finally
    Reporter.Free;
    Files.Free;
  end;
end;

function TBenchmarkRunnerApp.UsageLine: string;
begin
  Result := '[path...|-] [options]';
end;

procedure TBenchmarkRunnerApp.Configure;
begin
  AddEngineOptions;
  FNoProgress := AddFlag('no-progress', 'Suppress progress output');
  FFormats := AddRepeatable('format', 'Output format (console, text, csv, json)');
  FOutputFile := AddString('output', 'Output file path (attaches to last --format)');
end;

function TBenchmarkRunnerApp.GlobalBuiltins: TGocciaGlobalBuiltins;
begin
  Result := [ggBenchmark];
end;

procedure TBenchmarkRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  Reports: array of TReportSpec;
  ReportCount, I: Integer;
  Mode: TGocciaEngineBackend;
  ShowProgress: Boolean;
begin
  ShowProgress := not FNoProgress.Present;

  if EngineOptions.Mode.Matches(emBytecode) then
    Mode := ebBytecode
  else
    Mode := ebTreeWalk;

  ReportCount := FFormats.Values.Count;
  if ReportCount = 0 then
  begin
    SetLength(Reports, 1);
    Reports[0].Format := brfConsole;
    Reports[0].OutputFile := FOutputFile.ValueOr('');
  end
  else
  begin
    SetLength(Reports, ReportCount);
    for I := 0 to ReportCount - 1 do
    begin
      Reports[I].Format := ParseReportFormat(FFormats.Values[I]);
      Reports[I].OutputFile := '';
    end;
    if FOutputFile.Present then
      Reports[ReportCount - 1].OutputFile := FOutputFile.Value;
  end;

  if APaths.Count = 0 then
    RunBenchmarksFromStdin(Reports, Mode, ShowProgress)
  else
  begin
    if (APaths.Count = 1) and IsStdinPath(APaths[0]) then
      RunBenchmarksFromStdin(Reports, Mode, ShowProgress)
    else
      RunBenchmarks(APaths, Reports, Mode, ShowProgress);
  end;
end;

begin
  ExitCode := TGocciaApplication.RunApplication(TBenchmarkRunnerApp, 'BenchmarkRunner');
end.
