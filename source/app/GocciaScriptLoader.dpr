program GocciaScriptLoader;

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
  Goccia.Scope,
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
    MemoryStats: TCLIJSONMemoryStats;
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
    function IsCompactJsonOutput: Boolean;
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
    function RunBytecodeModule(const AEngine: TGocciaEngine;
      const AExecutor: TGocciaBytecodeExecutor;
      const AModule: TGocciaBytecodeModule;
      const AFileName: string): TGocciaValue;
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
    '"json" for structured JSON output, "compact-json" omits build, memory, stdout, stderr');
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
  Result := FOutputPath.Present and
    ((FOutputPath.Value = 'json') or (FOutputPath.Value = 'compact-json'));
end;

function TScriptLoaderApp.IsCompactJsonOutput: Boolean;
begin
  Result := FOutputPath.Present and (FOutputPath.Value = 'compact-json');
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

  AConsole.Enabled := not FSilent.Present;
  if IsJsonOutput and Assigned(ACapture) then
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
  const ACapture: TScriptLoaderConsoleCapture; const AFileName: string;
  const ACompact: Boolean);
begin
  WriteLn(BuildCLIScriptSuccessJSON(AFileName, AReport.ResultValue,
    CapturedOutputText(ACapture), CapturedStdoutText(ACapture),
    CapturedStderrText(ACapture), AReport.Timing, AReport.MemoryStats, 1, 1,
    ACompact));
end;

procedure PrintJSONError(const E: Exception; const AReport: TScriptExecutionReport;
  const ACapture: TScriptLoaderConsoleCapture; const ADefaultFileName: string;
  const ACompact: Boolean);
var
  ErrorInfo: TCLIJSONErrorInfo;
begin
  ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
  if ErrorInfo.FileName = '' then
    ErrorInfo.FileName := ADefaultFileName;
  WriteLn(BuildCLIScriptErrorJSON(ADefaultFileName, CapturedOutputText(ACapture),
    CapturedStdoutText(ACapture), CapturedStderrText(ACapture), ErrorInfo,
    AReport.Timing, AReport.MemoryStats, 1, 1, ACompact));
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
  AAvailableWorkerCount: Integer; const ACompact: Boolean): string;
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
    FilesJSON, '', ACompact);
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

function TScriptLoaderApp.RunBytecodeModule(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaBytecodeExecutor;
  const AModule: TGocciaBytecodeModule;
  const AFileName: string): TGocciaValue;
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
    Result := AExecutor.RunModuleInScope(AModule, ModuleScope);
  end
  else
    Result := AExecutor.RunModule(AModule);
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
          Result.ResultValue := RunBytecodeModule(Engine,
            TGocciaBytecodeExecutor(Engine.Executor), Module, AFileName);
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
          Result.ResultValue := RunBytecodeModule(Engine,
            TGocciaBytecodeExecutor(Engine.Executor), Module, AFileName);
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
        PrintJSONSuccess(Report, Capture, AFileName, IsCompactJsonOutput)
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
            PrintJSONError(E, Report, Capture, AFileName, IsCompactJsonOutput)
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
  Result.MemoryStats := DefaultCLIJSONMemoryStats;

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
      Result.MemoryStats := Report.MemoryStats;
      Result.Ok := True;
      Result.JSON := BuildCLIScriptFileSuccessJSON(AFileName,
        Report.ResultValue, Result.OutputText, Result.StdoutText,
        Result.StderrText, Report.Timing, Report.MemoryStats,
        IsCompactJsonOutput);
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
        Result.MemoryStats := Report.MemoryStats;
        Result.Ok := False;
        Result.JSON := BuildCLIScriptFileErrorJSON(AFileName,
          Result.OutputText, Result.StdoutText, Result.StderrText,
          ErrorInfo, Report.Timing, Report.MemoryStats,
          IsCompactJsonOutput);
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

  Source := SourceRegistry.Load(AFileName);
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
    Source := SourceRegistry.Load(AFileName);
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
        ErrorInfo, Timing, DefaultCLIJSONMemoryStats, IsCompactJsonOutput);
      Result.StdoutText := '';
      Result.StderrText := '';
      Result.OutputText := '';
      Result.ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
      Result.Timing := Timing;
      Result.MemoryStats := DefaultCLIJSONMemoryStats;
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
  MemoryStats: TCLIJSONMemoryStats;
  MainMemoryStats: TCLIJSONMemoryStats;
  WorkerMemoryStats: TCLIJSONMemoryStats;
  Pool: TGocciaThreadPool;
  I, JobCount: Integer;
begin
  WorkerMemoryStats := DefaultCLIJSONMemoryStats;
  SetLength(Results, AFiles.Count);
  JobCount := GetJobCount(AFiles.Count);

  if JobCount > 1 then
  begin
    EnsureSharedPrototypesInitialized(EffectiveBuiltins);
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    Pool := TGocciaThreadPool.Create(JobCount);
    try
      if Assigned(TGarbageCollector.Instance) then
        Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
      Pool.RunAll(AFiles, ScriptWorkerProc, @Results[0]);
      WorkerMemoryStats := Pool.MemoryStats;
    finally
      Pool.Free;
    end;
    MainMemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);

    for I := 0 to AFiles.Count - 1 do
      if not Results[I].Ok then
        ExitCode := 1;
    MemoryStats := CombineCLIJSONMemoryStats(
      MainMemoryStats, WorkerMemoryStats, True);
  end
  else
  begin
    BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
    for I := 0 to AFiles.Count - 1 do
    begin
      Results[I] := RunScriptFromFileForJSON(AFiles[I], False);
      if not Results[I].Ok then
        ExitCode := 1;
    end;
    MemoryStats := FinishCLIJSONMemoryMeasurement(MemoryMeasurement);
  end;

  WriteLn(BuildAggregateScriptLoaderJSON(Results,
    MemoryStats, JobCount, GetJobCount(AFiles.Count),
    IsCompactJsonOutput));
end;

procedure TScriptLoaderApp.RunScriptFromStdin;
var
  Source, SectionSource, Names: TStringList;
  I: Integer;
begin
  Source := ReadSourceFromText(Input);
  if not MultifileEnabled then
  begin
    try
      RunSource(Source, STDIN_FILE_NAME);
    finally
      Source.Free;
    end;
    Exit;
  end;

  // Multifile stdin: ownership of Source transfers to SplitStdinMultifile
  // (which either registers it under <stdin> as a single section or
  // splits it into multiple sections and frees the wrapper).
  Names := SplitStdinMultifile(Source);
  try
    for I := 0 to Names.Count - 1 do
    begin
      if I > 0 then
        WriteLn;
      SectionSource := SourceRegistry.Load(Names[I]);
      try
        RunSource(SectionSource, Names[I]);
      finally
        SectionSource.Free;
      end;
    end;
  finally
    Names.Free;
  end;
end;

procedure TScriptLoaderApp.ScriptWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
var
  JSONResults: PScriptLoaderJSONFileResultArray;
  ErrorInfo: TCLIJSONErrorInfo;
  Timing: TCLIJSONTiming;
  MemoryStats: TCLIJSONMemoryStats;
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    if IsJsonOutput then
    begin
      JSONResults := PScriptLoaderJSONFileResultArray(AData);
      JSONResults^[AIndex] := RunScriptFromFileForJSON(AFileName, True);
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
        ErrorInfo := ExceptionToCLIJSONErrorInfo(E);
        if ErrorInfo.FileName = '' then
          ErrorInfo.FileName := AFileName;
        FillChar(Timing, SizeOf(Timing), 0);
        MemoryStats := DefaultCLIJSONMemoryStats;
        JSONResults^[AIndex].FileName := AFileName;
        JSONResults^[AIndex].StdoutText := '';
        JSONResults^[AIndex].StderrText := '';
        JSONResults^[AIndex].OutputText := '';
        JSONResults^[AIndex].ErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
        JSONResults^[AIndex].Timing := Timing;
        JSONResults^[AIndex].MemoryStats := MemoryStats;
        JSONResults^[AIndex].Ok := False;
        JSONResults^[AIndex].JSON := BuildCLIScriptFileErrorJSON(
          AFileName, '', '', '', ErrorInfo, Timing, MemoryStats,
          IsCompactJsonOutput);
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
  Files, RawFiles, SinglePath: TStringList;
  I: Integer;
begin
  if IsStdinPath(APath) then
  begin
    RunScriptFromStdin;
    Exit;
  end;

  if DirectoryExists(APath) then
  begin
    RawFiles := FindAllFiles(APath, ScriptExtensions);
    try
      Files := ExpandMultifileFiles(RawFiles);
    finally
      RawFiles.Free;
    end;
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
  begin
    if MultifileEnabled then
    begin
      SinglePath := TStringList.Create;
      try
        SinglePath.Add(APath);
        Files := ExpandMultifileFiles(SinglePath);
      finally
        SinglePath.Free;
      end;
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
    else
      RunScriptFromFile(APath);
  end
  else
    raise Exception.Create('Path not found: ' + APath);
end;

{ TScriptLoaderApp - ExecuteWithPaths }

procedure TScriptLoaderApp.ExecuteWithPaths(const APaths: TStringList);
var
  I, SectionIndex: Integer;
  Files, RawFiles, StdinNames: TStringList;
  Source, SectionSource: TStringList;
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

  if (FSourceMap.ValueOr('') <> '') and MultifileEnabled then
    raise TGocciaParseError.Create(
      '--source-map=<file> cannot be combined with --multifile (an input '
      + 'may expand to multiple sections).');

  if IsJsonOutput then
  begin
    if (APaths.Count = 0) or
       ((APaths.Count = 1) and IsStdinPath(APaths[0])) then
    begin
      Source := ReadSourceFromText(Input);
      BeginCLIJSONMemoryMeasurement(MemoryMeasurement);
      if MultifileEnabled then
      begin
        StdinNames := SplitStdinMultifile(Source);
        try
          SetLength(JSONResults, StdinNames.Count);
          for SectionIndex := 0 to StdinNames.Count - 1 do
          begin
            SectionSource := SourceRegistry.Load(StdinNames[SectionIndex]);
            try
              JSONResults[SectionIndex] :=
                RunSourceForJSON(SectionSource, StdinNames[SectionIndex], False);
              if not JSONResults[SectionIndex].Ok then
                ExitCode := 1;
            finally
              SectionSource.Free;
            end;
          end;
          WriteLn(BuildAggregateScriptLoaderJSON(JSONResults,
            FinishCLIJSONMemoryMeasurement(MemoryMeasurement),
            1, StdinNames.Count, IsCompactJsonOutput));
        finally
          StdinNames.Free;
        end;
      end
      else
      begin
        try
          JSONResult := RunSourceForJSON(Source, STDIN_FILE_NAME, False);
          SetLength(JSONResults, 1);
          JSONResults[0] := JSONResult;
          if not JSONResult.Ok then
            ExitCode := 1;
          WriteLn(BuildAggregateScriptLoaderJSON(JSONResults,
            FinishCLIJSONMemoryMeasurement(MemoryMeasurement), 1, 1,
            IsCompactJsonOutput));
        finally
          Source.Free;
        end;
      end;
      Exit;
    end;

    RawFiles := TStringList.Create;
    try
      for I := 0 to APaths.Count - 1 do
      begin
        if IsStdinPath(APaths[I]) then
          raise TGocciaParseError.Create(
            'stdin supports only as the sole input path.');
        if DirectoryExists(APaths[I]) then
          RawFiles.AddStrings(FindAllFiles(APaths[I], ScriptExtensions))
        else if FileExists(APaths[I]) then
          RawFiles.Add(APaths[I])
        else
          raise Exception.Create('Path not found: ' + APaths[I]);
      end;
      Files := ExpandMultifileFiles(RawFiles);
      try
        RunJSONFiles(Files);
      finally
        Files.Free;
      end;
    finally
      RawFiles.Free;
    end;
    Exit;
  end;

  if APaths.Count = 0 then
    RunScriptFromStdin
  else if (APaths.Count = 1) and IsStdinPath(APaths[0]) then
    RunScriptFromStdin
  else
  begin
    { Reject mixing "-" with file paths so stdin cannot silently be
      interleaved with on-disk files. Matches the rule enforced by
      --output=json mode and by GocciaTestRunner / GocciaBenchmarkRunner. }
    for I := 0 to APaths.Count - 1 do
      if IsStdinPath(APaths[I]) then
        raise TGocciaParseError.Create(
          'stdin supports only as the sole input path.');

    for I := 0 to APaths.Count - 1 do
    begin
      if I > 0 then
        WriteLn;
      RunScripts(APaths[I]);
    end;
  end;
end;

{ TScriptLoaderApp - HandleError }

procedure TScriptLoaderApp.HandleError(const AException: Exception);
begin
  if IsJsonOutput then
    WriteLn(BuildCLIScriptErrorJSON('', '', '', '', ExceptionToCLIJSONErrorInfo(AException),
      Default(TCLIJSONTiming), DefaultCLIJSONMemoryStats, 1, 1,
      IsCompactJsonOutput))
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
