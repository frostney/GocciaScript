program GocciaBenchmarkRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,
  TextSemantics,

  Goccia.Application,
  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Benchmark.Reporter,
  Goccia.Builtins.Benchmark,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  CLI.Options,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Scope,
  Goccia.ScriptLoader.Input,
  Goccia.CLI.JSON.Reporter,
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

  Goccia.Threading,
  Goccia.Threading.Init,

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

type
  TBenchmarkFileResultArray = array[0..MaxInt div SizeOf(TBenchmarkFileResult) - 1] of TBenchmarkFileResult;
  PBenchmarkFileResultArray = ^TBenchmarkFileResultArray;

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
    procedure RunBytecodeBenchmarkModule(const AEngine: TGocciaEngine;
      const AExecutor: TGocciaBytecodeExecutor;
      const AModule: TGocciaBytecodeModule; const AFileName: string);
    procedure CollectBenchmarkFileInterpreted(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
    procedure CollectBenchmarkFileBytecode(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AShowProgress: Boolean);
    procedure CollectBenchmarkFile(const AFileName: string;
      const AReporter: TBenchmarkReporter; const AMode: TGocciaExecutionMode;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSourceInterpreted(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSourceBytecode(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AShowProgress: Boolean);
    procedure CollectBenchmarkSource(const ASource: TStringList;
      const AFileName: string; const AReporter: TBenchmarkReporter;
      const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
    procedure RunBenchmarksFromStdin(const AReports: array of TReportSpec;
      const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
    procedure BenchmarkWorkerProc(const AFileName: string;
      const AIndex: Integer; out AConsoleOutput: string;
      out AErrorMessage: string; AData: Pointer);
    procedure RunBenchmarks(const APaths: TStringList;
      const AReports: array of TReportSpec;
      const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    function GlobalBuiltins: TGocciaGlobalBuiltins; override;
  end;

function IsBenchmarkHelperFile(const AFileName: string): Boolean;
var
  NormalizedPath: string;
begin
  NormalizedPath := StringReplace(AFileName, PathDelim, '/', [rfReplaceAll]);
  Result := Pos('/helpers/', NormalizedPath) > 0;
end;

function RunRegisteredBenchmarks(const AEngine: TGocciaEngine): TGocciaObjectValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  Value: TGocciaValue;
begin
  if not Assigned(AEngine.BuiltinBenchmark) then
    Exit(nil);

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    Value := AEngine.BuiltinBenchmark.RunBenchmarks(EmptyArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    EmptyArgs.Free;
  end;

  if Value is TGocciaObjectValue then
    Result := TGocciaObjectValue(Value)
  else
    Result := nil;
end;

procedure TBenchmarkRunnerApp.RunBytecodeBenchmarkModule(
  const AEngine: TGocciaEngine;
  const AExecutor: TGocciaBytecodeExecutor;
  const AModule: TGocciaBytecodeModule; const AFileName: string);
var
  ModuleScope: TGocciaScope;
begin
  if AEngine.SourceType = stModule then
  begin
    { Run with module semantics: fresh module scope, this = undefined.
      Mirrors TGocciaModuleLoader.LoadModule for nested module loads. }
    ModuleScope := AEngine.Interpreter.GlobalScope.CreateChild(skModule,
      'Module:' + AFileName);
    ModuleScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    AExecutor.RunModuleInScope(AModule, ModuleScope);
  end
  else
    AExecutor.RunModule(AModule);
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
  BenchStart: Int64;
begin
  Source := nil;
  try
    try
      Source := SourceRegistry.Load(AFileName);
    except
      on E: Exception do
      begin
        if not GIsWorkerThread then
          WriteLn(StdErr, 'Error loading benchmark file: ', E.Message);
        MakeErrorFileResult(AFileName, E.Message, AReporter);
        Exit;
      end;
    end;
    try
      Engine := CreateEngine(AFileName, Source);
      try
        if AShowProgress and Assigned(Engine.BuiltinBenchmark) then
          Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          EngineResult := Engine.Execute;
          FileResult.FileName := AFileName;
          FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
          FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
          FileResult.CompileTimeNanoseconds := 0;
          BenchStart := GetNanoseconds;
          ScriptResult := RunRegisteredBenchmarks(Engine);
          FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds +
            (GetNanoseconds - BenchStart);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;

        GC := TGarbageCollector.Instance;

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
        if not GIsWorkerThread then
          WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
        MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
      end;
      on E: TGocciaThrowValue do
      begin
        if not GIsWorkerThread then
          WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, Source, IsColorTerminal, E.Suggestion));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.Value, AFileName, Source, False, E.Suggestion), AReporter);
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
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  FileResult: TBenchmarkFileResult;
  ScriptResult: TGocciaObjectValue;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd, BenchStart: Int64;
begin
  Source := nil;
  try
    try
      Source := SourceRegistry.Load(AFileName);
    except
      on E: Exception do
      begin
        if not GIsWorkerThread then
          WriteLn(StdErr, 'Error loading benchmark file: ', E.Message);
        MakeErrorFileResult(AFileName, E.Message, AReporter);
        Exit;
      end;
    end;
    SourceText := StringListToLFText(Source);
    if ppJSX in TGocciaEngine.DefaultPreprocessors then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(SourceText);
      SourceText := JSXResult.Source;
      JSXResult.SourceMap.Free;
    end;

    try
      Executor := TGocciaBytecodeExecutor.Create;
      try
        Engine := CreateEngine(AFileName, Source, Executor);
        try
          LexStart := GetNanoseconds;
          Lexer := TGocciaLexer.Create(SourceText, AFileName);
          try
            Tokens := Lexer.ScanTokens;
            LexEnd := GetNanoseconds;

            Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
            Parser.AutomaticSemicolonInsertion := Engine.ASIEnabled;
            Parser.VarDeclarationsEnabled := Engine.VarEnabled;
            Parser.FunctionDeclarationsEnabled := Engine.FunctionEnabled;
            try
              ProgramNode := Parser.Parse;
              ParseEnd := GetNanoseconds;

              try
                Module := TGocciaBytecodeExecutor(Engine.Executor).CompileToModule(ProgramNode);
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

          if AShowProgress and Assigned(Engine.BuiltinBenchmark) then
            Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;
          if Assigned(Engine.BuiltinBenchmark) then
            Engine.BuiltinBenchmark.OnBeforeMeasurement := Engine.ClearTransientCaches;

          try
          StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
          StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
          try
            RunBytecodeBenchmarkModule(Engine,
              TGocciaBytecodeExecutor(Engine.Executor), Module, AFileName);
            ExecEnd := GetNanoseconds;

            FileResult.FileName := AFileName;
            FileResult.LexTimeNanoseconds := LexEnd - LexStart;
            FileResult.ParseTimeNanoseconds := ParseEnd - LexEnd;
            FileResult.CompileTimeNanoseconds := CompileEnd - ParseEnd;
            BenchStart := GetNanoseconds;
            ScriptResult := RunRegisteredBenchmarks(Engine);
            FileResult.ExecuteTimeNanoseconds := ExecEnd - CompileEnd +
              (GetNanoseconds - BenchStart);
          finally
            ClearExecutionTimeout;
            ClearInstructionLimit;
          end;

          GC := TGarbageCollector.Instance;
          if Assigned(ScriptResult) and Assigned(GC) then
            GC.AddTempRoot(ScriptResult);

          try
            PopulateFileResult(FileResult, ScriptResult, AReporter);
          finally
            if Assigned(ScriptResult) and Assigned(GC) then
              GC.RemoveTempRoot(ScriptResult);
          end;
        finally
          Module.Free;
        end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    except
      on E: TGocciaError do
      begin
        if not GIsWorkerThread then
          WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
        MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
      end;
      on E: TGocciaThrowValue do
      begin
        if not GIsWorkerThread then
          WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, Source, IsColorTerminal, E.Suggestion));
        MakeErrorFileResult(AFileName,
          FormatThrowDetail(E.Value, AFileName, Source, False, E.Suggestion), AReporter);
      end;
      on E: EGocciaBytecodeThrow do
      begin
        if not GIsWorkerThread then
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
  const AReporter: TBenchmarkReporter; const AMode: TGocciaExecutionMode;
  const AShowProgress: Boolean);
begin
  case AMode of
    emInterpreted: CollectBenchmarkFileInterpreted(AFileName, AReporter,
      AShowProgress);
    emBytecode: CollectBenchmarkFileBytecode(AFileName, AReporter,
      AShowProgress);
  else
    raise Exception.CreateFmt('Unsupported execution mode: %d', [Ord(AMode)]);
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
  BenchStart: Int64;
begin
  try
    Engine := CreateEngine(AFileName, ASource);
    try
      if AShowProgress and Assigned(Engine.BuiltinBenchmark) then
        Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
      try
        EngineResult := Engine.Execute;
        FileResult.FileName := AFileName;
        FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
        FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
        FileResult.CompileTimeNanoseconds := 0;
        BenchStart := GetNanoseconds;
        ScriptResult := RunRegisteredBenchmarks(Engine);
        FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds +
          (GetNanoseconds - BenchStart);
      finally
        ClearExecutionTimeout;
        ClearInstructionLimit;
      end;

      GC := TGarbageCollector.Instance;

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
      if not GIsWorkerThread then
        WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
      MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
    end;
    on E: TGocciaThrowValue do
    begin
      if not GIsWorkerThread then
        WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, ASource, IsColorTerminal, E.Suggestion));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.Value, AFileName, ASource, False, E.Suggestion), AReporter);
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
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  GC: TGarbageCollector;
  FileResult: TBenchmarkFileResult;
  ScriptResult: TGocciaObjectValue;
  LexStart, LexEnd, ParseEnd, CompileEnd, ExecEnd, BenchStart: Int64;
begin
  SourceText := StringListToLFText(ASource);
  if ppJSX in TGocciaEngine.DefaultPreprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    JSXResult.SourceMap.Free;
  end;

  try
    Executor := TGocciaBytecodeExecutor.Create;
    try
      Engine := CreateEngine(AFileName, ASource, Executor);
      try
        LexStart := GetNanoseconds;
        Lexer := TGocciaLexer.Create(SourceText, AFileName);
        try
          Tokens := Lexer.ScanTokens;
          LexEnd := GetNanoseconds;

          Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
          Parser.AutomaticSemicolonInsertion := Engine.ASIEnabled;
          Parser.VarDeclarationsEnabled := Engine.VarEnabled;
          Parser.FunctionDeclarationsEnabled := Engine.FunctionEnabled;
          try
            ProgramNode := Parser.Parse;
            ParseEnd := GetNanoseconds;

            try
              Module := TGocciaBytecodeExecutor(Engine.Executor).CompileToModule(ProgramNode);
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

        if AShowProgress and Assigned(Engine.BuiltinBenchmark) then
          Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;
        if Assigned(Engine.BuiltinBenchmark) then
          Engine.BuiltinBenchmark.OnBeforeMeasurement := Engine.ClearTransientCaches;

        try
        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          RunBytecodeBenchmarkModule(Engine,
            TGocciaBytecodeExecutor(Engine.Executor), Module, AFileName);
          ExecEnd := GetNanoseconds;

          FileResult.FileName := AFileName;
          FileResult.LexTimeNanoseconds := LexEnd - LexStart;
          FileResult.ParseTimeNanoseconds := ParseEnd - LexEnd;
          FileResult.CompileTimeNanoseconds := CompileEnd - ParseEnd;
          BenchStart := GetNanoseconds;
          ScriptResult := RunRegisteredBenchmarks(Engine);
          FileResult.ExecuteTimeNanoseconds := ExecEnd - CompileEnd +
            (GetNanoseconds - BenchStart);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;

        GC := TGarbageCollector.Instance;
        if Assigned(ScriptResult) and Assigned(GC) then
          GC.AddTempRoot(ScriptResult);

        try
          PopulateFileResult(FileResult, ScriptResult, AReporter);
        finally
          if Assigned(ScriptResult) and Assigned(GC) then
            GC.RemoveTempRoot(ScriptResult);
        end;
      finally
        Module.Free;
      end;
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
  except
    on E: TGocciaError do
    begin
      if not GIsWorkerThread then
        WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
      MakeErrorFileResult(AFileName, E.GetDetailedMessage, AReporter);
    end;
    on E: TGocciaThrowValue do
    begin
      if not GIsWorkerThread then
        WriteLn(StdErr, FormatThrowDetail(E.Value, AFileName, ASource, IsColorTerminal, E.Suggestion));
      MakeErrorFileResult(AFileName,
        FormatThrowDetail(E.Value, AFileName, ASource, False, E.Suggestion), AReporter);
    end;
    on E: EGocciaBytecodeThrow do
    begin
      if not GIsWorkerThread then
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
  const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
begin
  case AMode of
    emInterpreted: CollectBenchmarkSourceInterpreted(ASource, AFileName,
      AReporter, AShowProgress);
    emBytecode: CollectBenchmarkSourceBytecode(ASource, AFileName,
      AReporter, AShowProgress);
  else
    raise Exception.CreateFmt('Unsupported execution mode: %d', [Ord(AMode)]);
  end;
end;

{ Worker procedure executed on each thread for a single benchmark file.
  Runs the benchmark, extracts the TBenchmarkFileResult into a pre-allocated
  array, and frees all GC-managed objects before returning. }
procedure TBenchmarkRunnerApp.BenchmarkWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
var
  WorkerReporter: TBenchmarkReporter;
  WorkerResults: PBenchmarkFileResultArray;
  Mode: TGocciaExecutionMode;
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  WorkerResults := PBenchmarkFileResultArray(AData);

  if EngineOptions.Mode.Matches(emBytecode) then
    Mode := emBytecode
  else
    Mode := emInterpreted;

  WorkerReporter := TBenchmarkReporter.Create;
  try
    try
      CollectBenchmarkFile(AFileName, WorkerReporter, Mode, False);
      if WorkerReporter.FileCount > 0 then
        WorkerResults^[AIndex] := WorkerReporter.Files[0];
    except
      on E: Exception do
      begin
        AErrorMessage := E.Message;
        WorkerResults^[AIndex].FileName := AFileName;
        WorkerResults^[AIndex].LexTimeNanoseconds := 0;
        WorkerResults^[AIndex].ParseTimeNanoseconds := 0;
        WorkerResults^[AIndex].CompileTimeNanoseconds := 0;
        WorkerResults^[AIndex].ExecuteTimeNanoseconds := 0;
        WorkerResults^[AIndex].TotalBenchmarks := 0;
        WorkerResults^[AIndex].DurationNanoseconds := 0;
        SetLength(WorkerResults^[AIndex].Entries, 1);
        WorkerResults^[AIndex].Entries[0].Suite := '';
        WorkerResults^[AIndex].Entries[0].Name := '(fatal)';
        WorkerResults^[AIndex].Entries[0].Error := E.Message;
      end;
    end;
  finally
    WorkerReporter.Free;
  end;
end;

procedure TBenchmarkRunnerApp.RunBenchmarksFromStdin(
  const AReports: array of TReportSpec;
  const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
var
  Source, SectionSource, Names: TStringList;
  Reporter: TBenchmarkReporter;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  I, J: Integer;
begin
  Source := ReadSourceFromText(Input);
  Reporter := TBenchmarkReporter.Create;
  try
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    if MultifileEnabled then
    begin
      // Ownership of Source transfers to SplitStdinMultifile.
      Names := SplitStdinMultifile(Source);
      try
        for I := 0 to Names.Count - 1 do
        begin
          SectionSource := SourceRegistry.Load(Names[I]);
          try
            CollectBenchmarkSource(SectionSource, Names[I], Reporter,
              AMode, AShowProgress);
          finally
            SectionSource.Free;
          end;
          // Match the per-file GC pass in CollectBenchmarkFile* so
          // GC-managed objects from one stdin section don't accumulate
          // across the next one.
          if Assigned(TGarbageCollector.Instance) then
            TGarbageCollector.Instance.Collect;
        end;
      finally
        Names.Free;
      end;
    end
    else
    begin
      try
        CollectBenchmarkSource(Source, STDIN_FILE_NAME, Reporter,
          AMode, AShowProgress);
      finally
        Source.Free;
      end;
    end;
    Reporter.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
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
  end;
end;

procedure TBenchmarkRunnerApp.RunBenchmarks(const APaths: TStringList;
  const AReports: array of TReportSpec;
  const AMode: TGocciaExecutionMode; const AShowProgress: Boolean);
var
  Files, RawFiles: TStringList;
  I, J, P, JobCount: Integer;
  DiscoveredFiles: TStringList;
  Reporter: TBenchmarkReporter;
  Pool: TGocciaThreadPool;
  WorkerData: array of TBenchmarkFileResult;
  WallClockStart: Int64;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  MainMemoryStats: TCLIJSONMemoryStats;
  WorkerMemoryStats: TCLIJSONMemoryStats;
begin
  WorkerMemoryStats := DefaultCLIJSONMemoryStats;
  Files := nil;
  Reporter := TBenchmarkReporter.Create;
  try
    RawFiles := TStringList.Create;
    try
      for P := 0 to APaths.Count - 1 do
      begin
        if DirectoryExists(APaths[P]) then
        begin
          DiscoveredFiles := FindAllFiles(APaths[P], ScriptExtensions);
          try
            for I := 0 to DiscoveredFiles.Count - 1 do
              if not IsBenchmarkHelperFile(DiscoveredFiles[I]) then
                RawFiles.Add(DiscoveredFiles[I]);
          finally
            DiscoveredFiles.Free;
          end;
        end
        else if FileExists(APaths[P]) then
          RawFiles.Add(APaths[P])
        else
        begin
          WriteLn(StdErr, 'Error: Path not found: ', APaths[P]);
          ExitCode := 1;
          Exit;
        end;
      end;
      Files := ExpandMultifileFiles(RawFiles);
    finally
      RawFiles.Free;
    end;

    JobCount := GetJobCount(Files.Count);

    if AShowProgress then
    begin
      if JobCount > 1 then
        WriteLn(SysUtils.Format('Running %d files with %d workers',
          [Files.Count, JobCount]))
      else
        WriteLn(SysUtils.Format('Running %d files', [Files.Count]));
    end;

    if JobCount > 1 then
    begin
      { Parallel path: pre-allocate result array, run workers, then
        add results to the reporter on the main thread in file order. }
      SetLength(WorkerData, Files.Count);
      for I := 0 to Files.Count - 1 do
      begin
        WorkerData[I].FileName := '';
        WorkerData[I].LexTimeNanoseconds := 0;
        WorkerData[I].ParseTimeNanoseconds := 0;
        WorkerData[I].CompileTimeNanoseconds := 0;
        WorkerData[I].ExecuteTimeNanoseconds := 0;
        WorkerData[I].TotalBenchmarks := 0;
        WorkerData[I].DurationNanoseconds := 0;
        SetLength(WorkerData[I].Entries, 0);
      end;

      EnsureSharedPrototypesInitialized(EffectiveBuiltins);

      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      WallClockStart := GetNanoseconds;

      Pool := TGocciaThreadPool.Create(JobCount);
      try
        if Assigned(TGarbageCollector.Instance) then
          Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
        Pool.RunAll(Files, BenchmarkWorkerProc, @WorkerData[0]);
        WorkerMemoryStats := Pool.MemoryStats;
      finally
        Pool.Free;
      end;

      Reporter.WallClockDurationNanoseconds := GetNanoseconds - WallClockStart;
      Reporter.JobCount := JobCount;
      MainMemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
      Reporter.MemoryStats := CombineCLIJSONMemoryStats(
        MainMemoryStats, WorkerMemoryStats, True);

      { Collect results on the main thread in original file order. }
      for I := 0 to Files.Count - 1 do
      begin
        if AShowProgress then
          WriteLn(SysUtils.Format('[%d/%d] %s',
            [I + 1, Files.Count, ExtractFileName(Files[I])]));
        Reporter.AddFileResult(WorkerData[I]);
      end;
    end
    else
    begin
      { Sequential path: run each file in order on the main thread. }
      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      for I := 0 to Files.Count - 1 do
      begin
        if AShowProgress then
          WriteLn(SysUtils.Format('[%d/%d] %s',
            [I + 1, Files.Count, ExtractFileName(Files[I])]));
        CollectBenchmarkFile(Files[I], Reporter, AMode, AShowProgress);
      end;
      Reporter.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
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
  FFormats := AddRepeatable('format',
    'Output format (console, text, csv, json, compact-json). ' +
    '"compact-json" emits the json envelope without build, memory, stdout, stderr.');
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
  Mode: TGocciaExecutionMode;
  ShowProgress: Boolean;
  HasStructuredStdout, HasReadableStdout: Boolean;
begin
  ShowProgress := not FNoProgress.Present;

  if EngineOptions.Mode.Matches(emBytecode) then
    Mode := emBytecode
  else
    Mode := emInterpreted;

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

  // Reject when both structured (JSON/CSV) and human-readable (console/text)
  // formats would write to stdout — the mixed output corrupts structured data.
  HasStructuredStdout := False;
  HasReadableStdout := False;
  for I := 0 to Length(Reports) - 1 do
    if Reports[I].OutputFile = '' then
    begin
      if Reports[I].Format in [brfJSON, brfCompactJSON, brfCSV] then
        HasStructuredStdout := True
      else
        HasReadableStdout := True;
    end;
  if HasStructuredStdout and HasReadableStdout then
  begin
    WriteLn(StdErr, 'Error: Cannot write structured and human-readable formats both to stdout. Provide an --output file for one of them.');
    ExitCode := 1;
    Exit;
  end;

  // Suppress progress when structured output (JSON/CSV) goes to stdout so
  // that human-readable status messages do not corrupt the output document.
  if ShowProgress then
    for I := 0 to Length(Reports) - 1 do
      if (Reports[I].Format in [brfJSON, brfCompactJSON, brfCSV]) and (Reports[I].OutputFile = '') then
      begin
        ShowProgress := False;
        Break;
      end;

  if APaths.Count = 0 then
    RunBenchmarksFromStdin(Reports, Mode, ShowProgress)
  else if (APaths.Count = 1) and IsStdinPath(APaths[0]) then
    RunBenchmarksFromStdin(Reports, Mode, ShowProgress)
  else
  begin
    { Reject mixing "-" with file paths so stdin cannot silently be
      interleaved with on-disk files. Matches the rule enforced by
      GocciaScriptLoader and GocciaTestRunner. }
    for I := 0 to APaths.Count - 1 do
      if IsStdinPath(APaths[I]) then
      begin
        WriteLn(StdErr,
          'Error: stdin supports only as the sole input path.');
        ExitCode := 1;
        Exit;
      end;
    RunBenchmarks(APaths, Reports, Mode, ShowProgress);
  end;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TBenchmarkRunnerApp, 'GocciaBenchmarkRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
