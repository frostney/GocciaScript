program BenchmarkRunner;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.AST.Node,
  Goccia.Benchmark.Reporter,
  Goccia.Builtins.Benchmark,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Engine.BytecodeBackend,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Modules.Configuration,
  Goccia.Parser,
  Goccia.ScriptLoader.Input,
  Goccia.SourceMap,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

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

var
  GShowProgress: Boolean = True;
  GImportMapPath: string = '';
  GInlineAliases: TStringList = nil;
  GMode: TGocciaEngineBackend = ebTreeWalk;
  GASIEnabled: Boolean = False;

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

procedure CollectBenchmarkFileInterpreted(const AFileName: string;
  const AReporter: TBenchmarkReporter);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  BenchGlobals: TGocciaGlobalBuiltins;
  EngineResult: TGocciaScriptResult;
  ScriptResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
begin
  BenchGlobals := [ggBenchmark];

  Source := ReadUTF8FileLines(AFileName);
  try
    Source.Add('runBenchmarks();');

    try
      Engine := TGocciaEngine.Create(AFileName, Source, BenchGlobals);
      try
        Engine.ASIEnabled := GASIEnabled;
        ConfigureModuleResolver(Engine.Resolver, AFileName, GImportMapPath,
          GInlineAliases);
        if GShowProgress and Assigned(Engine.BuiltinBenchmark) then
          Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

        EngineResult := Engine.Execute;
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
        MakeErrorFileResult(AFileName, E.Message, AReporter);
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

procedure CollectBenchmarkFileBytecode(const AFileName: string;
  const AReporter: TBenchmarkReporter);
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
  BenchGlobals: TGocciaGlobalBuiltins;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  BenchGlobals := [ggBenchmark];

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
      Backend := TGocciaBytecodeBackend.Create(AFileName);
      try
        Backend.ASIEnabled := GASIEnabled;
        Backend.RegisterBuiltIns(BenchGlobals);
        ConfigureModuleResolver(Backend.ModuleResolver, AFileName,
          GImportMapPath, GInlineAliases);

        LexStart := GetNanoseconds;
        Lexer := TGocciaLexer.Create(SourceText, AFileName);
        try
          Tokens := Lexer.ScanTokens;
          LexEnd := GetNanoseconds;

          Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
          Parser.AutomaticSemicolonInsertion := GASIEnabled;
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

        if GShowProgress and Assigned(Backend.Bootstrap) and
           Assigned(Backend.Bootstrap.BuiltinBenchmark) then
          Backend.Bootstrap.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;
        if Assigned(Backend.Bootstrap) and Assigned(Backend.Bootstrap.BuiltinBenchmark) then
          Backend.Bootstrap.BuiltinBenchmark.OnBeforeMeasurement := Backend.ClearTransientCaches;

        try
          ResultValue := Backend.RunModule(Module);
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
        MakeErrorFileResult(AFileName, E.Message, AReporter);
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

procedure CollectBenchmarkFile(const AFileName: string;
  const AReporter: TBenchmarkReporter);
begin
  case GMode of
    ebTreeWalk:  CollectBenchmarkFileInterpreted(AFileName, AReporter);
    ebBytecode: CollectBenchmarkFileBytecode(AFileName, AReporter);
  else
    raise Exception.CreateFmt('Unsupported execution backend: %d', [Ord(GMode)]);
  end;
end;

procedure CollectBenchmarkSourceInterpreted(const ASource: TStringList;
  const AFileName: string; const AReporter: TBenchmarkReporter);
var
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  BenchGlobals: TGocciaGlobalBuiltins;
  EngineResult: TGocciaScriptResult;
  ScriptResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
begin
  BenchGlobals := [ggBenchmark];
  ASource.Add('runBenchmarks();');

  try
    Engine := TGocciaEngine.Create(AFileName, ASource, BenchGlobals);
    try
      Engine.ASIEnabled := GASIEnabled;
      ConfigureModuleResolver(Engine.Resolver, AFileName, GImportMapPath,
        GInlineAliases);
      if GShowProgress and Assigned(Engine.BuiltinBenchmark) then
        Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

      EngineResult := Engine.Execute;
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
      MakeErrorFileResult(AFileName, E.Message, AReporter);
    end;
    on E: Exception do
      MakeErrorFileResult(AFileName, E.Message, AReporter);
  end;
end;

procedure CollectBenchmarkSourceBytecode(const ASource: TStringList;
  const AFileName: string; const AReporter: TBenchmarkReporter);
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
  BenchGlobals: TGocciaGlobalBuiltins;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd: Int64;
begin
  BenchGlobals := [ggBenchmark];
  ASource.Add('runBenchmarks();');

  SourceText := StringListToLFText(ASource);
  if ppJSX in TGocciaEngine.DefaultPreprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    JSXResult.SourceMap.Free;
  end;

  try
    Backend := TGocciaBytecodeBackend.Create(AFileName);
    try
      Backend.ASIEnabled := GASIEnabled;
      Backend.RegisterBuiltIns(BenchGlobals);
      ConfigureModuleResolver(Backend.ModuleResolver, AFileName,
        GImportMapPath, GInlineAliases);

      LexStart := GetNanoseconds;
      Lexer := TGocciaLexer.Create(SourceText, AFileName);
      try
        Tokens := Lexer.ScanTokens;
        LexEnd := GetNanoseconds;

        Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
        Parser.AutomaticSemicolonInsertion := GASIEnabled;
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

      if GShowProgress and Assigned(Backend.Bootstrap) and
         Assigned(Backend.Bootstrap.BuiltinBenchmark) then
        Backend.Bootstrap.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;
      if Assigned(Backend.Bootstrap) and Assigned(Backend.Bootstrap.BuiltinBenchmark) then
        Backend.Bootstrap.BuiltinBenchmark.OnBeforeMeasurement := Backend.ClearTransientCaches;

      try
        ResultValue := Backend.RunModule(Module);
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
      MakeErrorFileResult(AFileName, E.Message, AReporter);
    end;
    on E: Exception do
      MakeErrorFileResult(AFileName, E.Message, AReporter);
  end;
end;

procedure CollectBenchmarkSource(const ASource: TStringList;
  const AFileName: string; const AReporter: TBenchmarkReporter);
begin
  case GMode of
    ebTreeWalk: CollectBenchmarkSourceInterpreted(ASource, AFileName, AReporter);
    ebBytecode: CollectBenchmarkSourceBytecode(ASource, AFileName, AReporter);
  else
    raise Exception.CreateFmt('Unsupported execution backend: %d', [Ord(GMode)]);
  end;
end;

procedure RunBenchmarksFromStdin(const AReports: array of TReportSpec);
var
  Source: TStringList;
  Reporter: TBenchmarkReporter;
  J: Integer;
begin
  Source := ReadSourceFromText(Input);
  Reporter := TBenchmarkReporter.Create;
  try
    CollectBenchmarkSource(Source, STDIN_FILE_NAME, Reporter);
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

procedure RunBenchmarks(const APaths: TStringList; const AReports: array of TReportSpec);
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
      if GShowProgress then
        WriteLn(SysUtils.Format('[%d/%d] %s', [I + 1, Files.Count, ExtractFileName(Files[I])]));
      CollectBenchmarkFile(Files[I], Reporter);
    end;
    if GShowProgress and (Files.Count > 0) then
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

var
  Paths: TStringList;
  Reports: array of TReportSpec;
  ReportCount: Integer;
  I: Integer;
  Arg, OutputStr: string;

begin
  ReportCount := 0;
  GShowProgress := True;
  GImportMapPath := '';
  GMode := ebTreeWalk;
  GInlineAliases := TStringList.Create;
  SetLength(Reports, 0);

  Paths := TStringList.Create;
  try
    I := 1;
    while I <= ParamCount do
    begin
      Arg := ParamStr(I);
      if Copy(Arg, 1, 9) = '--format=' then
      begin
        Inc(ReportCount);
        SetLength(Reports, ReportCount);
        Reports[ReportCount - 1].Format := ParseReportFormat(Copy(Arg, 10, MaxInt));
        Reports[ReportCount - 1].OutputFile := '';
      end
      else if Copy(Arg, 1, 9) = '--output=' then
      begin
        OutputStr := Copy(Arg, 10, MaxInt);
        if ReportCount > 0 then
          Reports[ReportCount - 1].OutputFile := OutputStr
        else
        begin
          Inc(ReportCount);
          SetLength(Reports, ReportCount);
          Reports[ReportCount - 1].Format := brfConsole;
          Reports[ReportCount - 1].OutputFile := OutputStr;
        end;
      end
      else if Arg = '--no-progress' then
        GShowProgress := False
      else if Copy(Arg, 1, 13) = '--import-map=' then
      begin
        GImportMapPath := Copy(Arg, 14, MaxInt);
        if GImportMapPath = '' then
        begin
          WriteLn('Error: --import-map requires a non-empty path.');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Copy(Arg, 1, 8) = '--alias=' then
        GInlineAliases.Add(Copy(Arg, 9, MaxInt))
      else if Arg = '--alias' then
      begin
        if I = ParamCount then
        begin
          WriteLn('Error: --alias requires a key=value argument.');
          ExitCode := 1;
          Exit;
        end;
        Inc(I);
        GInlineAliases.Add(ParamStr(I));
      end
      else if Arg = '--asi' then
        GASIEnabled := True
      else if Arg = '--mode=interpreted' then
        GMode := ebTreeWalk
      else if Arg = '--mode=bytecode' then
        GMode := ebBytecode
      else if (Arg = '--help') or (Arg = '-h') then
      begin
        WriteLn('Usage: BenchmarkRunner [path...|-] [--format=console|text|csv|json [--output=file]] ... [--no-progress] [--mode=interpreted|bytecode] [--import-map=file] [--alias key=value] [--asi]');
        WriteLn('  -                       Read benchmark source from stdin');
        WriteLn('  (omitted path)          Read benchmark source from stdin');
        WriteLn('  --asi                   Enable automatic semicolon insertion');
        Exit;
      end
      else if Copy(Arg, 1, 7) = '--mode=' then
      begin
        WriteLn('Error: Unknown mode "', Copy(Arg, 8, MaxInt), '". Use "interpreted" or "bytecode".');
        ExitCode := 1;
        Exit;
      end
      else if Copy(Arg, 1, 2) <> '--' then
        Paths.Add(Arg)
      else
      begin
        WriteLn('Error: Unknown option "', Arg, '".');
        ExitCode := 1;
        Exit;
      end;
      Inc(I);
    end;

    if ReportCount = 0 then
    begin
      SetLength(Reports, 1);
      Reports[0].Format := brfConsole;
      Reports[0].OutputFile := '';
    end;

    if Paths.Count = 0 then
      RunBenchmarksFromStdin(Reports)
    else
    begin
      if (Paths.Count = 1) and IsStdinPath(Paths[0]) then
        RunBenchmarksFromStdin(Reports)
      else
        RunBenchmarks(Paths, Reports);
    end;
  finally
    GInlineAliases.Free;
    Paths.Free;
  end;
end.
