program GocciaScriptLoader;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Builtins.Console,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  CLI.Options,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.Coverage.Report,
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
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.CLI.JSON.Reporter,
  Goccia.SourceMap,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Threading.Init,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception,

  FileUtils in 'units/FileUtils.pas';

type
  TScriptLoaderConsoleCapture = class
  private
    FOutputLines: TStringList;
    FStdoutLines: TStringList;
    FStderrLines: TStringList;
    function MethodWritesToStderr(const AMethod: string): Boolean;
    function UnformatLine(const AMethod, ALine: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CaptureOutput(const AMethod, ALine: string);
    function OutputText: string;
    function StdoutText: string;
    function StderrText: string;
  end;

  TScriptExecutionReport = record
    ResultValue: TGocciaValue;
    Timing: TCLIJSONTiming;
    MemoryStats: TCLIJSONMemoryStats;
  end;

  TScriptLoaderJSONFileResult = record
    FileName: string;
    JSON: string;
    StdoutText: string;
    StderrText: string;
    OutputText: string;
    ErrorJSON: string;
    Timing: TCLIJSONTiming;
    Ok: Boolean;
  end;

  TScriptLoaderJSONFileResultArray = array[0..MaxInt div SizeOf(TScriptLoaderJSONFileResult) - 1] of TScriptLoaderJSONFileResult;
  PScriptLoaderJSONFileResultArray = ^TScriptLoaderJSONFileResultArray;

  TScriptLoaderApp = class(TGocciaCLIApplication)
  private
    FOutputPath: TGocciaStringOption;
    FSilent: TGocciaFlagOption;
    FSourceMap: TGocciaStringOption;
    FGlobalFiles: TGocciaRepeatableOption;
    FInlineGlobals: TGocciaRepeatableOption;
    FLastPaths: TStringList;

    function IsJsonOutput: Boolean;
    function ParseSource(const ASource: TStringList; const AFileName: string;
      const APreprocessors: TGocciaPreprocessors; const ASuppressWarnings: Boolean;
      const AASIEnabled, AVarEnabled, AFunctionEnabled: Boolean;
      out ALexTimeNanoseconds, AParseTimeNanoseconds: Int64;
      out ASourceMap: TGocciaSourceMap): TGocciaProgram;
    procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
      const AFileName: string);
    procedure ConfigureConsole(const AConsole: TGocciaConsole;
      const ACapture: TScriptLoaderConsoleCapture);
    procedure ApplyDataGlobalsToEngine(const AEngine: TGocciaEngine);
    procedure ApplyModuleGlobalsToEngine(const AEngine: TGocciaEngine);
    function ExecuteInterpreted(const ASource: TStringList; const AFileName: string;
      const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
    function ExecuteBytecodeFromSource(const ASource: TStringList; const AFileName: string;
      const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
    function ExecuteBytecodeFromFile(const AFileName: string;
      const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
    procedure PrintHumanReadableResult(const AFileName: string;
      const AReport: TScriptExecutionReport; const AExtension: string);
    function RunSourceForJSON(const ASource: TStringList;
      const AFileName: string;
      const AMeasureMemory: Boolean = True): TScriptLoaderJSONFileResult;
    procedure RunSource(const ASource: TStringList; const AFileName: string);
    function RunScriptFromFileForJSON(const AFileName: string;
      const AMeasureMemory: Boolean = True): TScriptLoaderJSONFileResult;
    procedure RunJSONFiles(const AFiles: TStringList);
    procedure RunScriptFromFile(const AFileName: string);
    procedure RunScriptFromStdin;
    procedure ScriptWorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
    procedure RunScriptsParallel(const AFiles: TStringList;
      const AJobCount: Integer);
    procedure RunScripts(const APath: string);
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
    procedure HandleError(const AException: Exception); override;
    procedure AfterExecute; override;
  end;

{ TScriptLoaderConsoleCapture }

constructor TScriptLoaderConsoleCapture.Create;
begin
  inherited Create;
  FOutputLines := TStringList.Create;
  FStdoutLines := TStringList.Create;
  FStderrLines := TStringList.Create;
end;

destructor TScriptLoaderConsoleCapture.Destroy;
begin
  FStderrLines.Free;
  FStdoutLines.Free;
  FOutputLines.Free;
  inherited Destroy;
end;

function TScriptLoaderConsoleCapture.MethodWritesToStderr(
  const AMethod: string): Boolean;
begin
  Result := (AMethod = 'warn') or (AMethod = 'error') or
    (AMethod = 'assert') or (AMethod = 'trace');
end;

function TScriptLoaderConsoleCapture.UnformatLine(const AMethod,
  ALine: string): string;
var
  Prefix: string;
  PrefixStart: Integer;
begin
  Prefix := '';
  if AMethod = 'warn' then
    Prefix := 'Warning: '
  else if AMethod = 'error' then
    Prefix := 'Error: '
  else if AMethod = 'info' then
    Prefix := 'Info: '
  else if AMethod = 'debug' then
    Prefix := 'Debug: '
  else if AMethod = 'trace' then
    Prefix := 'Trace: ';

  if Prefix = '' then
    Exit(ALine);

  PrefixStart := 1;
  while (PrefixStart <= Length(ALine)) and (ALine[PrefixStart] = ' ') do
    Inc(PrefixStart);
  if Copy(ALine, PrefixStart, Length(Prefix)) = Prefix then
    Result := Copy(ALine, 1, PrefixStart - 1) +
      Copy(ALine, PrefixStart + Length(Prefix), MaxInt)
  else
    Result := ALine;
end;

procedure TScriptLoaderConsoleCapture.CaptureOutput(const AMethod,
  ALine: string);
begin
  FOutputLines.Add(ALine);
  if MethodWritesToStderr(AMethod) then
    FStderrLines.Add(UnformatLine(AMethod, ALine))
  else
    FStdoutLines.Add(UnformatLine(AMethod, ALine));
end;

function TScriptLoaderConsoleCapture.OutputText: string;
begin
  Result := FOutputLines.Text;
end;

function TScriptLoaderConsoleCapture.StdoutText: string;
begin
  Result := FStdoutLines.Text;
end;

function TScriptLoaderConsoleCapture.StderrText: string;
begin
  Result := FStderrLines.Text;
end;

{ TScriptLoaderApp - Configure }

function TScriptLoaderApp.UsageLine: string;
begin
  Result := '[file|directory|-] [options]';
end;

procedure TScriptLoaderApp.Configure;
begin
  AddEngineOptions;
  AddCoverageOptions;
  AddProfilerOptions;

  FOutputPath := AddString('output',
    '"json" for structured JSON output');
  FSilent := AddFlag('silent', 'Suppress console output from the script');
  FSourceMap := AddString('source-map',
    'Write a .map source map file (optional: explicit path)');
  FGlobalFiles := AddRepeatable('globals',
    'Inject globals from a JSON/JSON5/TOML/YAML file or a module with named exports');
  FInlineGlobals := AddRepeatable('global',
    'Inject a single global; value is parsed as JSON or kept as a string');
end;

function TScriptLoaderApp.IsJsonOutput: Boolean;
begin
  Result := FOutputPath.Present and (FOutputPath.Value = 'json');
end;

{ TScriptLoaderApp - Validate }

procedure TScriptLoaderApp.Validate;
begin
  inherited Validate;

  if EngineOptions.Timeout.Present and (EngineOptions.Timeout.Value < 0) then
    raise TGocciaParseError.Create('--timeout must be 0 or greater.');

  // --profile-format implies --profile=functions when no explicit --profile given
  if ProfilerOptions.Format.Present and not ProfilerOptions.Mode.Present then
    ProfilerOptions.Mode.Apply('functions');

  // Profiling requires bytecode mode regardless of --mode flag
  if ProfilerOptions.Mode.Present then
    EngineOptions.Mode.Apply('bytecode');

  if ProfilerOptions.OutputPath.Present and not ProfilerOptions.Mode.Present then
    raise TGocciaParseError.Create(
      '--profile-output requires --profile=opcodes|functions|all.');

  if ProfilerOptions.Format.Matches(pfFlamegraph) and
     not ProfilerOptions.OutputPath.Present then
    raise TGocciaParseError.Create(
      '--profile-format=flamegraph requires --profile-output=<path>.');
end;

{ TScriptLoaderApp - Core logic }

function TScriptLoaderApp.ParseSource(const ASource: TStringList;
  const AFileName: string; const APreprocessors: TGocciaPreprocessors;
  const ASuppressWarnings: Boolean; const AASIEnabled, AVarEnabled, AFunctionEnabled: Boolean;
  out ALexTimeNanoseconds, AParseTimeNanoseconds: Int64;
  out ASourceMap: TGocciaSourceMap): TGocciaProgram;
var
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  Warning: TGocciaParserWarning;
  StartTime, LexEnd, ParseEnd: Int64;
  OrigLine, OrigCol, I: Integer;
begin
  StartTime := GetNanoseconds;
  SourceText := StringListToLFText(ASource);

  ASourceMap := nil;
  if ppJSX in APreprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    ASourceMap := JSXResult.SourceMap;
    if Assigned(ASourceMap) then
      ASourceMap.SetSourceContent(0, StringListToLFText(ASource));
  end;

  try
    Lexer := TGocciaLexer.Create(SourceText, AFileName);
    try
      Tokens := Lexer.ScanTokens;
      LexEnd := GetNanoseconds;
      ALexTimeNanoseconds := LexEnd - StartTime;

      Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
      Parser.AutomaticSemicolonInsertion := AASIEnabled;
      Parser.VarDeclarationsEnabled := AVarEnabled;
      Parser.FunctionDeclarationsEnabled := AFunctionEnabled;
      try
        Result := Parser.Parse;
        ParseEnd := GetNanoseconds;
        AParseTimeNanoseconds := ParseEnd - LexEnd;

        if (not ASuppressWarnings) and (not GIsWorkerThread) then
          for I := 0 to Parser.WarningCount - 1 do
          begin
            Warning := Parser.GetWarning(I);
            WriteLn(SysUtils.Format('Warning: %s', [Warning.Message]));
            if Warning.Suggestion <> '' then
              WriteLn(SysUtils.Format('  Suggestion: %s', [Warning.Suggestion]));
            if Assigned(ASourceMap) and
               ASourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
              WriteLn(SysUtils.Format('  --> %s:%d:%d', [AFileName, OrigLine, OrigCol]))
            else
              WriteLn(SysUtils.Format('  --> %s:%d:%d', [AFileName, Warning.Line, Warning.Column]));
          end;
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
    end;
  except
    ASourceMap.Free;
    ASourceMap := nil;
    raise;
  end;
end;

procedure TScriptLoaderApp.WriteSourceMapIfEnabled(
  const ASourceMap: TGocciaSourceMap; const AFileName: string);
var
  MapOutputPath: string;
begin
  if not FSourceMap.Present then
    Exit;
  if not Assigned(ASourceMap) then
    Exit;
  MapOutputPath := FSourceMap.ValueOr('');
  if MapOutputPath = '' then
    MapOutputPath := AFileName + EXT_MAP;
  ASourceMap.FileName := ExtractFileName(AFileName);
  ASourceMap.SetSourcePath(0, ExtractFileName(AFileName));
  ASourceMap.SaveToFile(MapOutputPath);
  if not IsJsonOutput then
    WriteLn(SysUtils.Format('  Source map written to %s', [MapOutputPath]));
end;

procedure TScriptLoaderApp.ConfigureConsole(const AConsole: TGocciaConsole;
  const ACapture: TScriptLoaderConsoleCapture);
begin
  if not Assigned(AConsole) then
    Exit;

  AConsole.Enabled := (not FSilent.Present) and (not GIsWorkerThread);
  if IsJsonOutput and not FSilent.Present and Assigned(ACapture) then
    AConsole.OutputCallback := ACapture.CaptureOutput
  else
    AConsole.OutputCallback := nil;
  AConsole.OutputLines := nil;
end;

function CapturedOutputText(const ACapture: TScriptLoaderConsoleCapture): string;
begin
  if Assigned(ACapture) then
    Result := ACapture.OutputText
  else
    Result := '';
end;

function CapturedStdoutText(const ACapture: TScriptLoaderConsoleCapture): string;
begin
  if Assigned(ACapture) then
    Result := ACapture.StdoutText
  else
    Result := '';
end;

function CapturedStderrText(const ACapture: TScriptLoaderConsoleCapture): string;
begin
  if Assigned(ACapture) then
    Result := ACapture.StderrText
  else
    Result := '';
end;

procedure PrintJSONSuccess(const AReport: TScriptExecutionReport;
  const ACapture: TScriptLoaderConsoleCapture; const AFileName: string);
begin
  WriteLn(BuildCLIScriptSuccessJSON(AFileName, AReport.ResultValue,
    CapturedOutputText(ACapture), CapturedStdoutText(ACapture),
    CapturedStderrText(ACapture), AReport.Timing, AReport.MemoryStats, 1, 1));
end;

procedure PrintJSONError(const E: Exception; const AReport: TScriptExecutionReport;
  const ACapture: TScriptLoaderConsoleCapture; const ADefaultFileName: string);
var
  ErrorInfo: TCLIJSONErrorInfo;
begin
  ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
  if ErrorInfo.FileName = '' then
    ErrorInfo.FileName := ADefaultFileName;
  WriteLn(BuildCLIScriptErrorJSON(ADefaultFileName, CapturedOutputText(ACapture),
    CapturedStdoutText(ACapture), CapturedStderrText(ACapture), ErrorInfo,
    AReport.Timing, AReport.MemoryStats, 1, 1));
end;

procedure AddTiming(var ATarget: TCLIJSONTiming;
  const ASource: TCLIJSONTiming);
begin
  ATarget.LexTimeNanoseconds := ATarget.LexTimeNanoseconds +
    ASource.LexTimeNanoseconds;
  ATarget.ParseTimeNanoseconds := ATarget.ParseTimeNanoseconds +
    ASource.ParseTimeNanoseconds;
  ATarget.CompileTimeNanoseconds := ATarget.CompileTimeNanoseconds +
    ASource.CompileTimeNanoseconds;
  ATarget.ExecuteTimeNanoseconds := ATarget.ExecuteTimeNanoseconds +
    ASource.ExecuteTimeNanoseconds;
  ATarget.TotalTimeNanoseconds := ATarget.TotalTimeNanoseconds +
    ASource.TotalTimeNanoseconds;
end;

function BuildAggregateScriptLoaderJSON(const AResults: array of TScriptLoaderJSONFileResult;
  const AMemoryStats: TCLIJSONMemoryStats; const AWorkerCount,
  AAvailableWorkerCount: Integer): string;
var
  I: Integer;
  Ok: Boolean;
  StdoutText, StderrText, OutputText, FilesJSON: string;
  ErrorJSON: string;
  Timing: TCLIJSONTiming;
begin
  FillChar(Timing, SizeOf(Timing), 0);
  Ok := True;
  StdoutText := '';
  StderrText := '';
  OutputText := '';
  ErrorJSON := 'null';
  FilesJSON := '';

  for I := 0 to High(AResults) do
  begin
    Ok := Ok and AResults[I].Ok;
    if (not AResults[I].Ok) and (ErrorJSON = 'null') then
      ErrorJSON := AResults[I].ErrorJSON;
    StdoutText := StdoutText + AResults[I].StdoutText;
    StderrText := StderrText + AResults[I].StderrText;
    OutputText := OutputText + AResults[I].OutputText;
    AddTiming(Timing, AResults[I].Timing);
    if FilesJSON <> '' then
      FilesJSON := FilesJSON + ',';
    FilesJSON := FilesJSON + AResults[I].JSON;
  end;

  Result := BuildCLIReportJSON(Ok, OutputText, StdoutText, StderrText,
    ErrorJSON, Timing, AMemoryStats, AWorkerCount, AAvailableWorkerCount,
    FilesJSON);
end;

procedure TScriptLoaderApp.ApplyDataGlobalsToEngine(const AEngine: TGocciaEngine);
var
  I: Integer;
  Pair: TScriptLoaderGlobalPair;
begin
  for I := 0 to FGlobalFiles.Values.Count - 1 do
    if IsStructuredGlobalsFile(FGlobalFiles.Values[I]) then
    begin
      if IsYAMLGlobalsFile(FGlobalFiles.Values[I]) then
        AEngine.InjectGlobalsFromYAML(ReadFileText(FGlobalFiles.Values[I]))
      else if IsJSON5GlobalsFile(FGlobalFiles.Values[I]) then
        AEngine.InjectGlobalsFromJSON5(ReadFileText(FGlobalFiles.Values[I]))
      else if IsTOMLGlobalsFile(FGlobalFiles.Values[I]) then
        AEngine.InjectGlobalsFromTOML(ReadFileText(FGlobalFiles.Values[I]))
      else
        AEngine.InjectGlobalsFromJSON(ReadFileText(FGlobalFiles.Values[I]));
    end;

  for I := 0 to FInlineGlobals.Values.Count - 1 do
  begin
    Pair := ParseGlobalPair(FInlineGlobals.Values[I]);
    AEngine.InjectGlobal(Pair.Key, ParseInlineGlobalValue(Pair.ValueText));
  end;
end;

procedure TScriptLoaderApp.ApplyModuleGlobalsToEngine(const AEngine: TGocciaEngine);
var
  I: Integer;
begin
  for I := 0 to FGlobalFiles.Values.Count - 1 do
    if not IsStructuredGlobalsFile(FGlobalFiles.Values[I]) then
      AEngine.InjectGlobalsFromModule(FGlobalFiles.Values[I]);
end;

function TScriptLoaderApp.ExecuteInterpreted(const ASource: TStringList;
  const AFileName: string; const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
var
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;
  SourceMap: TGocciaSourceMap;
begin
  Engine := CreateEngine(AFileName, ASource);
  try
    Engine.SuppressWarnings := GIsWorkerThread or
      IsJsonOutput;
    ConfigureConsole(Engine.BuiltinConsole, ACapture);
    ApplyDataGlobalsToEngine(Engine);
    StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
    StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
    try
      ApplyModuleGlobalsToEngine(Engine);
      ScriptResult := Engine.Execute;
    finally
      ClearExecutionTimeout;
      ClearInstructionLimit;
      SourceMap := Engine.TakeLastSourceMap;
      try
        if Assigned(SourceMap) and Assigned(ASource) then
          SourceMap.SetSourceContent(0, StringListToLFText(ASource));
        WriteSourceMapIfEnabled(SourceMap, AFileName);
      finally
        SourceMap.Free;
      end;
    end;
  finally
    Engine.Free;
  end;

  Result.ResultValue := ScriptResult.Result;
  Result.Timing.LexTimeNanoseconds := ScriptResult.LexTimeNanoseconds;
  Result.Timing.ParseTimeNanoseconds := ScriptResult.ParseTimeNanoseconds;
  Result.Timing.CompileTimeNanoseconds := 0;
  Result.Timing.ExecuteTimeNanoseconds := ScriptResult.ExecuteTimeNanoseconds;
  Result.Timing.TotalTimeNanoseconds := ScriptResult.TotalTimeNanoseconds;
end;

function TScriptLoaderApp.ExecuteBytecodeFromSource(const ASource: TStringList;
  const AFileName: string; const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
var
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  SourceMap: TGocciaSourceMap;
  StartTime, CompileStart, CompileEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Executor := TGocciaBytecodeExecutor.Create;
  try
    Engine := CreateEngine(AFileName, ASource, Executor);
    try
      ConfigureConsole(Engine.BuiltinConsole, ACapture);
      ApplyDataGlobalsToEngine(Engine);

      ProgramNode := ParseSource(ASource, AFileName, TGocciaEngine.DefaultPreprocessors,
        IsJsonOutput, Engine.ASIEnabled, Engine.VarEnabled, Engine.FunctionEnabled,
        Result.Timing.LexTimeNanoseconds,
        Result.Timing.ParseTimeNanoseconds, SourceMap);
      try
        WriteSourceMapIfEnabled(SourceMap, AFileName);

        if Assigned(TGocciaCoverageTracker.Instance) and
           TGocciaCoverageTracker.Instance.Enabled and Assigned(ASource) then
        begin
          TGocciaCoverageTracker.Instance.RegisterSourceFile(
            AFileName, CountExecutableLines(ASource));
          if Assigned(SourceMap) then
            TGocciaCoverageTracker.Instance.RegisterSourceMap(
              AFileName, SourceMap.Clone);
        end;

        CompileStart := GetNanoseconds;
        Module := TGocciaBytecodeExecutor(Engine.Executor).CompileToModule(ProgramNode);
        CompileEnd := GetNanoseconds;
        Result.Timing.CompileTimeNanoseconds := CompileEnd - CompileStart;
      finally
        ProgramNode.Free;
        SourceMap.Free;
      end;

      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
      try
        try
          ApplyModuleGlobalsToEngine(Engine);
          Result.ResultValue := TGocciaBytecodeExecutor(Engine.Executor).RunModule(Module);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;
        ExecEnd := GetNanoseconds;
        Result.Timing.ExecuteTimeNanoseconds := ExecEnd - CompileEnd;
        Result.Timing.TotalTimeNanoseconds := ExecEnd - StartTime;
      finally
        Module.Free;
      end;
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
  end;
end;

function TScriptLoaderApp.ExecuteBytecodeFromFile(const AFileName: string;
  const ACapture: TScriptLoaderConsoleCapture): TScriptExecutionReport;
var
  Module: TGocciaBytecodeModule;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  StartTime, LoadEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Module := Goccia.Bytecode.Binary.LoadModuleFromFile(AFileName);
  LoadEnd := GetNanoseconds;
  try
    Executor := TGocciaBytecodeExecutor.Create;
    try
      Engine := CreateEngine(AFileName, nil, Executor);
      try
        ConfigureConsole(Engine.BuiltinConsole, ACapture);
        ApplyDataGlobalsToEngine(Engine);
        StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
        StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
        try
          ApplyModuleGlobalsToEngine(Engine);
          Result.ResultValue := TGocciaBytecodeExecutor(Engine.Executor).RunModule(Module);
        finally
          ClearExecutionTimeout;
          ClearInstructionLimit;
        end;
        ExecEnd := GetNanoseconds;
        Result.Timing.LexTimeNanoseconds := 0;
        Result.Timing.ParseTimeNanoseconds := 0;
        Result.Timing.CompileTimeNanoseconds := 0;
        Result.Timing.ExecuteTimeNanoseconds := ExecEnd - LoadEnd;
        Result.Timing.TotalTimeNanoseconds := ExecEnd - StartTime;
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
  finally
    Module.Free;
  end;
end;

procedure TScriptLoaderApp.PrintHumanReadableResult(const AFileName: string;
  const AReport: TScriptExecutionReport; const AExtension: string);
var
  LoadTimeNanoseconds: Int64;
begin
  if GIsWorkerThread then Exit;

  if AExtension = EXT_GBC then
  begin
    LoadTimeNanoseconds := AReport.Timing.TotalTimeNanoseconds -
      AReport.Timing.ExecuteTimeNanoseconds;
    WriteLn('Running bytecode: ', AFileName);
    WriteLn(SysUtils.Format('  Load: %s | Execute: %s | Total: %s',
      [FormatDuration(LoadTimeNanoseconds),
       FormatDuration(AReport.Timing.ExecuteTimeNanoseconds),
       FormatDuration(AReport.Timing.TotalTimeNanoseconds)]));
  end
  else if EngineOptions.Mode.Matches(emBytecode) then
  begin
    WriteLn('Running script (bytecode): ', AFileName);
    WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
      [FormatDuration(AReport.Timing.LexTimeNanoseconds),
       FormatDuration(AReport.Timing.ParseTimeNanoseconds),
       FormatDuration(AReport.Timing.CompileTimeNanoseconds),
       FormatDuration(AReport.Timing.ExecuteTimeNanoseconds),
       FormatDuration(AReport.Timing.TotalTimeNanoseconds)]));
  end
  else
  begin
    WriteLn('Running script (interpreted): ', AFileName);
    WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
      [FormatDuration(AReport.Timing.LexTimeNanoseconds),
       FormatDuration(AReport.Timing.ParseTimeNanoseconds),
       FormatDuration(AReport.Timing.ExecuteTimeNanoseconds),
       FormatDuration(AReport.Timing.TotalTimeNanoseconds)]));
  end;

  WriteLn('Result: ', AReport.ResultValue.ToStringLiteral.Value);
end;

procedure TScriptLoaderApp.RunSource(const ASource: TStringList;
  const AFileName: string);
var
  Extension: string;
  Report: TScriptExecutionReport;
  Capture: TScriptLoaderConsoleCapture;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  StartTime: Int64;
begin
  FillChar(Report, SizeOf(Report), 0);
  Report.ResultValue := nil;
  Report.MemoryStats := DefaultCLIJSONMemoryStats;

  Capture := nil;
  if IsJsonOutput then
    Capture := TScriptLoaderConsoleCapture.Create;
  try
    StartTime := GetNanoseconds;
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    try
      Extension := LowerCase(ExtractFileExt(AFileName));

      if Extension = EXT_GBC then
        Report := ExecuteBytecodeFromFile(AFileName, Capture)
      else
        case EngineOptions.Mode.ValueOr(emInterpreted) of
          emInterpreted: Report := ExecuteInterpreted(ASource, AFileName, Capture);
          emBytecode:    Report := ExecuteBytecodeFromSource(ASource, AFileName, Capture);
        end;

      Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
      if IsJsonOutput then
        PrintJSONSuccess(Report, Capture, AFileName)
      else
        PrintHumanReadableResult(AFileName, Report, Extension);
    except
      on E: Exception do
      begin
        Report.Timing.TotalTimeNanoseconds := GetNanoseconds - StartTime;
        if Report.Timing.TotalTimeNanoseconds >
           Report.Timing.LexTimeNanoseconds + Report.Timing.ParseTimeNanoseconds +
           Report.Timing.CompileTimeNanoseconds then
          Report.Timing.ExecuteTimeNanoseconds :=
            Report.Timing.TotalTimeNanoseconds -
            Report.Timing.LexTimeNanoseconds -
            Report.Timing.ParseTimeNanoseconds -
            Report.Timing.CompileTimeNanoseconds;
        Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
        if not GIsWorkerThread then
        begin
          if IsJsonOutput then
            PrintJSONError(E, Report, Capture, AFileName)
          else if E is TGocciaError then
            WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal))
          else if E is TGocciaThrowValue then
            WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, ASource, IsColorTerminal, TGocciaThrowValue(E).Suggestion))
          else if E is EGocciaBytecodeThrow then
            WriteLn(FormatThrowDetail(EGocciaBytecodeThrow(E).ThrownValue, AFileName, ASource, IsColorTerminal))
          else
            WriteLn('Fatal error: ', E.Message);
        end;
        ExitCode := 1;
      end;
    end;
  finally
    Capture.Free;
  end;
end;

function TScriptLoaderApp.RunSourceForJSON(const ASource: TStringList;
  const AFileName: string;
  const AMeasureMemory: Boolean): TScriptLoaderJSONFileResult;
var
  Extension: string;
  Report: TScriptExecutionReport;
  Capture: TScriptLoaderConsoleCapture;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  StartTime: Int64;
  ErrorInfo: TCLIJSONErrorInfo;
begin
  FillChar(Report, SizeOf(Report), 0);
  Report.ResultValue := nil;
  Report.MemoryStats := DefaultCLIJSONMemoryStats;

  Result.FileName := AFileName;
  Result.JSON := '';
  Result.StdoutText := '';
  Result.StderrText := '';
  Result.OutputText := '';
  Result.ErrorJSON := 'null';
  Result.Ok := False;
  FillChar(Result.Timing, SizeOf(Result.Timing), 0);

  Capture := TScriptLoaderConsoleCapture.Create;
  try
    StartTime := GetNanoseconds;
    if AMeasureMemory then
      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    try
      Extension := LowerCase(ExtractFileExt(AFileName));
      if Extension = EXT_GBC then
        Report := ExecuteBytecodeFromFile(AFileName, Capture)
      else
        case EngineOptions.Mode.ValueOr(emInterpreted) of
          emInterpreted: Report := ExecuteInterpreted(ASource, AFileName, Capture);
          emBytecode:    Report := ExecuteBytecodeFromSource(ASource, AFileName, Capture);
        end;

      if AMeasureMemory then
        Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement)
      else
        Report.MemoryStats := DefaultCLIJSONMemoryStats;
      Result.StdoutText := CapturedStdoutText(Capture);
      Result.StderrText := CapturedStderrText(Capture);
      Result.OutputText := CapturedOutputText(Capture);
      Result.ErrorJSON := 'null';
      Result.Timing := Report.Timing;
      Result.Ok := True;
      Result.JSON := BuildCLIScriptFileSuccessJSON(AFileName,
        Report.ResultValue, Result.OutputText, Result.StdoutText,
        Result.StderrText, Report.Timing, Report.MemoryStats);
    except
      on E: Exception do
      begin
        Report.Timing.TotalTimeNanoseconds := GetNanoseconds - StartTime;
        if Report.Timing.TotalTimeNanoseconds >
           Report.Timing.LexTimeNanoseconds + Report.Timing.ParseTimeNanoseconds +
           Report.Timing.CompileTimeNanoseconds then
          Report.Timing.ExecuteTimeNanoseconds :=
            Report.Timing.TotalTimeNanoseconds -
            Report.Timing.LexTimeNanoseconds -
            Report.Timing.ParseTimeNanoseconds -
            Report.Timing.CompileTimeNanoseconds;
        if AMeasureMemory then
          Report.MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement)
        else
          Report.MemoryStats := DefaultCLIJSONMemoryStats;
        ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
        if ErrorInfo.FileName = '' then
          ErrorInfo.FileName := AFileName;
        Result.StdoutText := CapturedStdoutText(Capture);
        Result.StderrText := CapturedStderrText(Capture);
        Result.OutputText := CapturedOutputText(Capture);
        Result.ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
        Result.Timing := Report.Timing;
        Result.Ok := False;
        Result.JSON := BuildCLIScriptFileErrorJSON(AFileName,
          Result.OutputText, Result.StdoutText, Result.StderrText,
          ErrorInfo, Report.Timing, Report.MemoryStats);
        ExitCode := 1;
      end;
    end;
  finally
    Capture.Free;
  end;
end;

procedure TScriptLoaderApp.RunScriptFromFile(const AFileName: string);
var
  Source: TStringList;
begin
  if LowerCase(ExtractFileExt(AFileName)) = EXT_GBC then
  begin
    RunSource(nil, AFileName);
    Exit;
  end;

  Source := ReadUTF8FileLines(AFileName);
  try
    RunSource(Source, AFileName);
  finally
    Source.Free;
  end;
end;

function TScriptLoaderApp.RunScriptFromFileForJSON(const AFileName: string;
  const AMeasureMemory: Boolean): TScriptLoaderJSONFileResult;
var
  Source: TStringList;
  ErrorInfo: TCLIJSONErrorInfo;
  Timing: TCLIJSONTiming;
  StartTime: Int64;
begin
  if LowerCase(ExtractFileExt(AFileName)) = EXT_GBC then
    Exit(RunSourceForJSON(nil, AFileName, AMeasureMemory));

  StartTime := GetNanoseconds;
  try
    Source := ReadUTF8FileLines(AFileName);
  except
    on E: Exception do
    begin
      FillChar(Timing, SizeOf(Timing), 0);
      Timing.TotalTimeNanoseconds := GetNanoseconds - StartTime;
      Timing.ExecuteTimeNanoseconds := Timing.TotalTimeNanoseconds;
      ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
      if ErrorInfo.FileName = '' then
        ErrorInfo.FileName := AFileName;
      Result.FileName := AFileName;
      Result.JSON := BuildCLIScriptFileErrorJSON(AFileName, '', '', '',
        ErrorInfo, Timing, DefaultCLIJSONMemoryStats);
      Result.StdoutText := '';
      Result.StderrText := '';
      Result.OutputText := '';
      Result.ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
      Result.Timing := Timing;
      Result.Ok := False;
      ExitCode := 1;
      Exit;
    end;
  end;
  try
    Result := RunSourceForJSON(Source, AFileName, AMeasureMemory);
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderApp.RunJSONFiles(const AFiles: TStringList);
var
  Results: array of TScriptLoaderJSONFileResult;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
  Pool: TGocciaThreadPool;
  I, JobCount: Integer;
begin
  SetLength(Results, AFiles.Count);
  BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
  JobCount := GetJobCount(AFiles.Count);

  if JobCount > 1 then
  begin
    EnsureSharedPrototypesInitialized(EffectiveBuiltins);
    Pool := TGocciaThreadPool.Create(JobCount);
    try
      if Assigned(TGarbageCollector.Instance) then
        Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
      Pool.RunAll(AFiles, ScriptWorkerProc, @Results[0]);
    finally
      Pool.Free;
    end;

    for I := 0 to AFiles.Count - 1 do
      if not Results[I].Ok then
        ExitCode := 1;
  end
  else
    for I := 0 to AFiles.Count - 1 do
    begin
      Results[I] := RunScriptFromFileForJSON(AFiles[I], False);
      if not Results[I].Ok then
        ExitCode := 1;
    end;

  WriteLn(BuildAggregateScriptLoaderJSON(Results,
    FinishCLIJSONMemoryMeasurement(MemoryMeasurement), JobCount,
    GetJobCount(AFiles.Count)));
end;

procedure TScriptLoaderApp.RunScriptFromStdin;
var
  Source: TStringList;
begin
  Source := ReadSourceFromText(Input);
  try
    RunSource(Source, STDIN_FILE_NAME);
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderApp.ScriptWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
var
  JSONResults: PScriptLoaderJSONFileResultArray;
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    if IsJsonOutput then
    begin
      JSONResults := PScriptLoaderJSONFileResultArray(AData);
      JSONResults^[AIndex] := RunScriptFromFileForJSON(AFileName, False);
      if not JSONResults^[AIndex].Ok then
        AErrorMessage := 'failed';
    end
    else
      RunScriptFromFile(AFileName);
  except
    on E: Exception do
    begin
      AErrorMessage := E.Message;
      if IsJsonOutput then
      begin
        JSONResults := PScriptLoaderJSONFileResultArray(AData);
        JSONResults^[AIndex].FileName := AFileName;
        JSONResults^[AIndex].Ok := False;
        JSONResults^[AIndex].JSON := BuildCLIScriptFileErrorJSON(
          AFileName, '', '', '', ExceptionToCLIJSONErrorInfo(E),
          Default(TCLIJSONTiming), DefaultCLIJSONMemoryStats);
      end;
      ExitCode := 1;
    end;
  end;
end;

procedure TScriptLoaderApp.RunScriptsParallel(const AFiles: TStringList;
  const AJobCount: Integer);
var
  Pool: TGocciaThreadPool;
  I: Integer;
begin
  EnsureSharedPrototypesInitialized(EffectiveBuiltins);

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
    if Assigned(TGarbageCollector.Instance) then
      Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
    Pool.RunAll(AFiles, ScriptWorkerProc);

    for I := 0 to AFiles.Count - 1 do
      if Pool.Results[I].ErrorMessage <> '' then
      begin
        WriteLn('Error in ', AFiles[I], ': ', Pool.Results[I].ErrorMessage);
        ExitCode := 1;
      end;
  finally
    Pool.Free;
  end;
end;

procedure TScriptLoaderApp.RunScripts(const APath: string);
var
  Files: TStringList;
  I: Integer;
begin
  if IsStdinPath(APath) then
  begin
    RunScriptFromStdin;
    Exit;
  end;

  if DirectoryExists(APath) then
  begin
    Files := FindAllFiles(APath, ScriptExtensions);
    try
      if GetJobCount(Files.Count) > 1 then
      begin
        WriteLn(SysUtils.Format('Running %d files with %d workers',
          [Files.Count, GetJobCount(Files.Count)]));
        RunScriptsParallel(Files, GetJobCount(Files.Count));
      end
      else
        for I := 0 to Files.Count - 1 do
        begin
          if I > 0 then
            WriteLn;
          RunScriptFromFile(Files[I]);
        end;
    finally
      Files.Free;
    end;
  end
  else if FileExists(APath) then
    RunScriptFromFile(APath)
  else
    raise Exception.Create('Path not found: ' + APath);
end;

{ TScriptLoaderApp - ExecuteWithPaths }

procedure TScriptLoaderApp.ExecuteWithPaths(const APaths: TStringList);
var
  I: Integer;
  Files: TStringList;
  TempFiles: TStringList;
  Source: TStringList;
  JSONResult: TScriptLoaderJSONFileResult;
  JSONResults: array of TScriptLoaderJSONFileResult;
  MemoryMeasurement: TCLIJSONMemoryMeasurement;
begin
  FLastPaths := APaths;

  if FSourceMap.Present and (FSourceMap.ValueOr('') = '') and
     ((APaths.Count = 0) or
      ((APaths.Count = 1) and IsStdinPath(APaths[0]))) then
    raise TGocciaParseError.Create(
      '--source-map=<file> is required when reading source from stdin.');

  if (FSourceMap.ValueOr('') <> '') and
     ((APaths.Count > 1) or
      ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TGocciaParseError.Create(
      '--source-map=<file> supports a single input file or stdin.');

  if IsJsonOutput then
  begin
    if (APaths.Count = 0) or
       ((APaths.Count = 1) and IsStdinPath(APaths[0])) then
    begin
      Source := ReadSourceFromText(Input);
      try
        BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
        JSONResult := RunSourceForJSON(Source, STDIN_FILE_NAME, False);
        SetLength(JSONResults, 1);
        JSONResults[0] := JSONResult;
        if not JSONResult.Ok then
          ExitCode := 1;
        WriteLn(BuildAggregateScriptLoaderJSON(JSONResults,
          FinishCLIJSONMemoryMeasurement(MemoryMeasurement), 1, 1));
      finally
        Source.Free;
      end;
      Exit;
    end;

    Files := TStringList.Create;
    try
      for I := 0 to APaths.Count - 1 do
      begin
        if IsStdinPath(APaths[I]) then
          raise TGocciaParseError.Create(
            '--output=json supports stdin only as the sole input path.');
        if DirectoryExists(APaths[I]) then
        begin
          TempFiles := FindAllFiles(APaths[I], ScriptExtensions);
          try
            Files.AddStrings(TempFiles);
          finally
            TempFiles.Free;
          end;
        end
        else if FileExists(APaths[I]) then
          Files.Add(APaths[I])
        else
          raise Exception.Create('Path not found: ' + APaths[I]);
      end;
      RunJSONFiles(Files);
    finally
      Files.Free;
    end;
    Exit;
  end;

  if APaths.Count = 0 then
    RunScriptFromStdin
  else
    for I := 0 to APaths.Count - 1 do
    begin
      if I > 0 then
        WriteLn;
      RunScripts(APaths[I]);
    end;
end;

{ TScriptLoaderApp - HandleError }

procedure TScriptLoaderApp.HandleError(const AException: Exception);
begin
  if IsJsonOutput then
    WriteLn(BuildCLIScriptErrorJSON('', '', '', '', ExceptionToCLIJSONErrorInfo(AException),
      Default(TCLIJSONTiming), DefaultCLIJSONMemoryStats, 1, 1))
  else
    inherited HandleError(AException);
end;

{ TScriptLoaderApp - AfterExecute }

procedure TScriptLoaderApp.AfterExecute;
var
  ProfileOpcodes, ProfileFunctions: Boolean;
  ProfileMode: CLI.Options.TGocciaProfileMode;
begin
  if (CoverageOptions.Enabled.Present or CoverageOptions.Format.Present or
      CoverageOptions.OutputPath.Present) and
     Assigned(TGocciaCoverageTracker.Instance) then
  begin
    if not IsJsonOutput then
    begin
      PrintCoverageSummary(TGocciaCoverageTracker.Instance);
      if Assigned(FLastPaths) and (FLastPaths.Count = 1) and
         FileExists(FLastPaths[0]) then
        PrintCoverageDetail(TGocciaCoverageTracker.Instance, FLastPaths[0]);
    end;
    if CoverageOptions.Format.Matches(cfLcov) and
       (CoverageOptions.OutputPath.ValueOr('') <> '') then
      WriteCoverageLcov(TGocciaCoverageTracker.Instance,
        CoverageOptions.OutputPath.Value);
    if CoverageOptions.Format.Matches(cfJson) and
       (CoverageOptions.OutputPath.ValueOr('') <> '') then
      WriteCoverageJSON(TGocciaCoverageTracker.Instance,
        CoverageOptions.OutputPath.Value);
  end;

  ProfileOpcodes := False;
  ProfileFunctions := False;
  if ProfilerOptions.Mode.Present then
  begin
    ProfileMode := ProfilerOptions.Mode.Value;
    ProfileOpcodes := (ProfileMode = CLI.Options.pmOpcodes) or
                      (ProfileMode = CLI.Options.pmAll);
    ProfileFunctions := (ProfileMode = CLI.Options.pmFunctions) or
                        (ProfileMode = CLI.Options.pmAll);
  end;

  if (ProfileOpcodes or ProfileFunctions) and
     Assigned(TGocciaProfiler.Instance) then
  begin
    if not IsJsonOutput then
    begin
      if ProfileOpcodes then
      begin
        PrintOpcodeProfile(TGocciaProfiler.Instance);
        PrintOpcodePairProfile(TGocciaProfiler.Instance);
        PrintScalarHitRate(TGocciaProfiler.Instance);
      end;
      if ProfileFunctions then
        PrintFunctionProfile(TGocciaProfiler.Instance);
    end;
    if ProfilerOptions.OutputPath.Present then
    begin
      if ProfilerOptions.Format.Matches(pfFlamegraph) then
        WriteCollapsedStacks(TGocciaProfiler.Instance,
          ProfilerOptions.OutputPath.Value)
      else
        WriteProfileJSON(TGocciaProfiler.Instance,
          ProfilerOptions.OutputPath.Value);
    end;
  end;
end;

{ Entry point }

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TScriptLoaderApp, 'GocciaScriptLoader');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
