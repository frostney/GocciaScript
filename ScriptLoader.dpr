program ScriptLoader;

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
  Goccia.CLI.Options,
  Goccia.Compiler,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.ScriptLoader.JSON,
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
  TScriptExecutionReport = record
    ResultValue: TGocciaValue;
    Timing: TScriptLoaderTiming;
  end;

  TScriptLoaderApp = class(TGocciaCLIApplication)
  private
    FEmit: TGocciaStringOption;
    FOutputPath: TGocciaStringOption;
    FSilent: TGocciaFlagOption;
    FSourceMap: TGocciaStringOption;
    FGlobalFiles: TGocciaRepeatableOption;
    FInlineGlobals: TGocciaRepeatableOption;
    FLastPaths: TStringList;

    function ParseSource(const ASource: TStringList; const AFileName: string;
      const APreprocessors: TGocciaPreprocessors; const ASuppressWarnings: Boolean;
      out ALexTimeNanoseconds, AParseTimeNanoseconds: Int64;
      out ASourceMap: TGocciaSourceMap): TGocciaProgram;
    function CompileSource(const ASource: TStringList; const AFileName: string;
      const APreprocessors: TGocciaPreprocessors;
      const ASuppressWarnings: Boolean = False): TGocciaBytecodeModule;
    procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
      const AFileName: string);
    procedure ConfigureConsole(const AConsole: TGocciaConsole;
      const AOutputLines: TStrings);
    procedure ApplyDataGlobalsToEngine(const AEngine: TGocciaEngine);
    procedure ApplyModuleGlobalsToEngine(const AEngine: TGocciaEngine);
    procedure ApplyDataGlobalsToBytecodeBackend(const ABackend: TGocciaBytecodeBackend);
    procedure ApplyModuleGlobalsToBytecodeBackend(const ABackend: TGocciaBytecodeBackend);
    function ExecuteInterpreted(const ASource: TStringList; const AFileName: string;
      const AOutputLines: TStrings): TScriptExecutionReport;
    function ExecuteBytecodeFromSource(const ASource: TStringList; const AFileName: string;
      const AOutputLines: TStrings): TScriptExecutionReport;
    function ExecuteBytecodeFromFile(const AFileName: string;
      const AOutputLines: TStrings): TScriptExecutionReport;
    procedure EmitBytecode(const ASource: TStringList; const AFileName,
      AOutputPath: string);
    procedure PrintHumanReadableResult(const AFileName: string;
      const AReport: TScriptExecutionReport; const AExtension: string);
    procedure RunSource(const ASource: TStringList; const AFileName: string);
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

  FEmit := AddString('emit',
    'Compile to .gbc file (no execution); optional value: bytecode');
  FOutputPath := AddString('output',
    'Output path (used with --emit) or "json" for structured JSON output');
  FSilent := AddFlag('silent', 'Suppress console output from the script');
  FSourceMap := AddString('source-map',
    'Write a .map source map file (optional: explicit path)');
  FGlobalFiles := AddRepeatable('globals',
    'Inject globals from a JSON/JSON5/TOML/YAML file or a module with named exports');
  FInlineGlobals := AddRepeatable('global',
    'Inject a single global; value is parsed as JSON or kept as a string');
end;

{ TScriptLoaderApp - Validate }

procedure TScriptLoaderApp.Validate;
begin
  inherited Validate;

  if FEmit.Present then
  begin
    if (FEmit.Value <> '') and (FEmit.Value <> 'bytecode') then
      raise TGocciaParseError.CreateFmt(
        'Unknown emit format "%s". Use "bytecode".', [FEmit.Value]);
  end;

  if (FOutputPath.Present and (FOutputPath.Value = 'json')) and FEmit.Present then
    raise TGocciaParseError.Create('--output=json cannot be used with --emit.');

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
  const ASuppressWarnings: Boolean;
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
      Parser.AutomaticSemicolonInsertion := EngineOptions.ASI.Present;
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
  if not (FOutputPath.Present and (FOutputPath.Value = 'json')) then
    WriteLn(SysUtils.Format('  Source map written to %s', [MapOutputPath]));
end;

function TScriptLoaderApp.CompileSource(const ASource: TStringList;
  const AFileName: string; const APreprocessors: TGocciaPreprocessors;
  const ASuppressWarnings: Boolean): TGocciaBytecodeModule;
var
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  CompiledModule: TGocciaBytecodeModule;
  LexTimeNanoseconds, ParseTimeNanoseconds: Int64;
  SourceMap: TGocciaSourceMap;
begin
  CompiledModule := nil;
  ProgramNode := ParseSource(ASource, AFileName, APreprocessors, ASuppressWarnings,
    LexTimeNanoseconds, ParseTimeNanoseconds, SourceMap);
  try
    Compiler := TGocciaCompiler.Create(AFileName);
    try
      CompiledModule := Compiler.Compile(ProgramNode);
      WriteSourceMapIfEnabled(SourceMap, AFileName);
      Result := CompiledModule;
      CompiledModule := nil;
    finally
      Compiler.Free;
    end;
  finally
    ProgramNode.Free;
    SourceMap.Free;
    CompiledModule.Free;
  end;
end;

procedure TScriptLoaderApp.ConfigureConsole(const AConsole: TGocciaConsole;
  const AOutputLines: TStrings);
begin
  if not Assigned(AConsole) then
    Exit;

  AConsole.Enabled := (not FSilent.Present) and (not GIsWorkerThread);
  if (FOutputPath.Present and (FOutputPath.Value = 'json')) and not FSilent.Present then
    AConsole.OutputLines := AOutputLines
  else
    AConsole.OutputLines := nil;
end;

function CapturedOutputText(const AOutputLines: TStrings): string;
begin
  if Assigned(AOutputLines) then
    Result := AOutputLines.Text
  else
    Result := '';
end;

procedure PrintJSONSuccess(const AReport: TScriptExecutionReport;
  const AOutputLines: TStrings);
begin
  WriteLn(BuildSuccessJSON(AReport.ResultValue, CapturedOutputText(AOutputLines),
    AReport.Timing));
end;

procedure PrintJSONError(const E: Exception; const AReport: TScriptExecutionReport;
  const AOutputLines: TStrings; const ADefaultFileName: string);
var
  ErrorInfo: TScriptLoaderErrorInfo;
begin
  ErrorInfo := ExceptionToScriptLoaderErrorInfo(E);
  if ErrorInfo.FileName = '' then
    ErrorInfo.FileName := ADefaultFileName;
  WriteLn(BuildErrorJSON(CapturedOutputText(AOutputLines), ErrorInfo,
    AReport.Timing));
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

procedure TScriptLoaderApp.ApplyDataGlobalsToBytecodeBackend(
  const ABackend: TGocciaBytecodeBackend);
var
  I: Integer;
  Pair: TScriptLoaderGlobalPair;
begin
  for I := 0 to FGlobalFiles.Values.Count - 1 do
    if IsStructuredGlobalsFile(FGlobalFiles.Values[I]) then
    begin
      if IsYAMLGlobalsFile(FGlobalFiles.Values[I]) then
        ABackend.InjectGlobalsFromYAML(ReadFileText(FGlobalFiles.Values[I]))
      else if IsJSON5GlobalsFile(FGlobalFiles.Values[I]) then
        ABackend.InjectGlobalsFromJSON5(ReadFileText(FGlobalFiles.Values[I]))
      else if IsTOMLGlobalsFile(FGlobalFiles.Values[I]) then
        ABackend.InjectGlobalsFromTOML(ReadFileText(FGlobalFiles.Values[I]))
      else
        ABackend.InjectGlobalsFromJSON(ReadFileText(FGlobalFiles.Values[I]));
    end;

  for I := 0 to FInlineGlobals.Values.Count - 1 do
  begin
    Pair := ParseGlobalPair(FInlineGlobals.Values[I]);
    ABackend.RegisterGlobal(Pair.Key, ParseInlineGlobalValue(Pair.ValueText));
  end;
end;

procedure TScriptLoaderApp.ApplyModuleGlobalsToBytecodeBackend(
  const ABackend: TGocciaBytecodeBackend);
var
  I: Integer;
begin
  for I := 0 to FGlobalFiles.Values.Count - 1 do
    if not IsStructuredGlobalsFile(FGlobalFiles.Values[I]) then
      ABackend.InjectGlobalsFromModule(FGlobalFiles.Values[I]);
end;

function TScriptLoaderApp.ExecuteInterpreted(const ASource: TStringList;
  const AFileName: string; const AOutputLines: TStrings): TScriptExecutionReport;
var
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;
  SourceMap: TGocciaSourceMap;
begin
  Engine := CreateEngine(AFileName, ASource);
  try
    Engine.SuppressWarnings := GIsWorkerThread or
      (FOutputPath.Present and (FOutputPath.Value = 'json'));
    ConfigureConsole(Engine.BuiltinConsole, AOutputLines);
    ApplyDataGlobalsToEngine(Engine);
    StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
    try
      ApplyModuleGlobalsToEngine(Engine);
      ScriptResult := Engine.Execute;
    finally
      ClearExecutionTimeout;
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
  const AFileName: string; const AOutputLines: TStrings): TScriptExecutionReport;
var
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  SourceMap: TGocciaSourceMap;
  StartTime, CompileStart, CompileEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Backend := CreateBytecodeBackend(AFileName);
  try
    ConfigureConsole(Backend.Bootstrap.BuiltinConsole, AOutputLines);
    ApplyDataGlobalsToBytecodeBackend(Backend);

    ProgramNode := ParseSource(ASource, AFileName, TGocciaEngine.DefaultPreprocessors,
      (FOutputPath.Present and (FOutputPath.Value = 'json')), Result.Timing.LexTimeNanoseconds,
      Result.Timing.ParseTimeNanoseconds, SourceMap);
    try
      WriteSourceMapIfEnabled(SourceMap, AFileName);

      if Assigned(TGocciaCoverageTracker.Instance) and
         TGocciaCoverageTracker.Instance.Enabled and Assigned(ASource) then
        TGocciaCoverageTracker.Instance.RegisterSourceFile(
          AFileName, CountExecutableLines(ASource));

      CompileStart := GetNanoseconds;
      Module := Backend.CompileToModule(ProgramNode);
      CompileEnd := GetNanoseconds;
      Result.Timing.CompileTimeNanoseconds := CompileEnd - CompileStart;
    finally
      ProgramNode.Free;
      SourceMap.Free;
    end;

    StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
    try
      try
        ApplyModuleGlobalsToBytecodeBackend(Backend);
        Result.ResultValue := Backend.RunModule(Module);
      finally
        ClearExecutionTimeout;
      end;
      ExecEnd := GetNanoseconds;
      Result.Timing.ExecuteTimeNanoseconds := ExecEnd - CompileEnd;
      Result.Timing.TotalTimeNanoseconds := ExecEnd - StartTime;
    finally
      Module.Free;
    end;
  finally
    Backend.Free;
  end;
end;

function TScriptLoaderApp.ExecuteBytecodeFromFile(const AFileName: string;
  const AOutputLines: TStrings): TScriptExecutionReport;
var
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  StartTime, LoadEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Module := Goccia.Bytecode.Binary.LoadModuleFromFile(AFileName);
  LoadEnd := GetNanoseconds;
  try
    Backend := CreateBytecodeBackend(AFileName);
    try
      ConfigureConsole(Backend.Bootstrap.BuiltinConsole, AOutputLines);
      ApplyDataGlobalsToBytecodeBackend(Backend);
      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      try
        ApplyModuleGlobalsToBytecodeBackend(Backend);
        Result.ResultValue := Backend.RunModule(Module);
      finally
        ClearExecutionTimeout;
      end;
      ExecEnd := GetNanoseconds;
      Result.Timing.LexTimeNanoseconds := 0;
      Result.Timing.ParseTimeNanoseconds := 0;
      Result.Timing.CompileTimeNanoseconds := 0;
      Result.Timing.ExecuteTimeNanoseconds := ExecEnd - LoadEnd;
      Result.Timing.TotalTimeNanoseconds := ExecEnd - StartTime;
    finally
      Backend.Free;
    end;
  finally
    Module.Free;
  end;
end;

procedure TScriptLoaderApp.EmitBytecode(const ASource: TStringList;
  const AFileName, AOutputPath: string);
var
  Module: TGocciaBytecodeModule;
  StartTime, EndTime: Int64;
begin
  WriteLn('Compiling: ', AFileName);
  StartTime := GetNanoseconds;

  Module := CompileSource(ASource, AFileName, TGocciaEngine.DefaultPreprocessors,
    (FOutputPath.Present and (FOutputPath.Value = 'json')));
  try
    Goccia.Bytecode.Binary.SaveModuleToFile(Module, AOutputPath);
    EndTime := GetNanoseconds;
    WriteLn(SysUtils.Format('  Compiled to %s (%s)',
      [AOutputPath, FormatDuration(EndTime - StartTime)]));
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
  Extension, EmitOutputPath: string;
  Report: TScriptExecutionReport;
  OutputLines: TStringList;
  StartTime: Int64;
begin
  FillChar(Report, SizeOf(Report), 0);
  Report.ResultValue := nil;

  OutputLines := nil;
  if (FOutputPath.Present and (FOutputPath.Value = 'json')) then
    OutputLines := TStringList.Create;
  try
    StartTime := GetNanoseconds;
    try
      Extension := LowerCase(ExtractFileExt(AFileName));

      if Extension = EXT_GBC then
        Report := ExecuteBytecodeFromFile(AFileName, OutputLines)
      else if FEmit.Present then
      begin
        if FOutputPath.Present and (FOutputPath.Value <> 'json') then
          EmitOutputPath := FOutputPath.Value
        else if AFileName = STDIN_FILE_NAME then
          raise Exception.Create('--output=<path> is required when emitting bytecode from stdin.')
        else
          EmitOutputPath := ChangeFileExt(AFileName, EXT_GBC);
        EmitBytecode(ASource, AFileName, EmitOutputPath);
        Exit;
      end
      else
        case EngineOptions.Mode.ValueOr(emInterpreted) of
          emInterpreted: Report := ExecuteInterpreted(ASource, AFileName, OutputLines);
          emBytecode:    Report := ExecuteBytecodeFromSource(ASource, AFileName, OutputLines);
        end;

      if (FOutputPath.Present and (FOutputPath.Value = 'json')) then
        PrintJSONSuccess(Report, OutputLines)
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
        if not GIsWorkerThread then
        begin
          if (FOutputPath.Present and (FOutputPath.Value = 'json')) then
            PrintJSONError(E, Report, OutputLines, AFileName)
          else if E is TGocciaError then
            WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal))
          else if E is TGocciaThrowValue then
            WriteLn(FormatThrowDetail(TGocciaThrowValue(E).Value, AFileName, ASource, IsColorTerminal))
          else if E is EGocciaBytecodeThrow then
            WriteLn(FormatThrowDetail(EGocciaBytecodeThrow(E).ThrownValue, AFileName, ASource, IsColorTerminal))
          else
            WriteLn('Fatal error: ', E.Message);
        end;
        ExitCode := 1;
      end;
    end;
  finally
    OutputLines.Free;
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
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    RunScriptFromFile(AFileName);
  except
    on E: Exception do
    begin
      AErrorMessage := E.Message;
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
  EnsureSharedPrototypesInitialized(GlobalBuiltins);

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
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
    if (FOutputPath.Present and (FOutputPath.Value = 'json')) then
      raise Exception.Create('--output=json does not support directory input.');

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
begin
  FLastPaths := APaths;

  if (FOutputPath.Present and (FOutputPath.Value = 'json')) and (APaths.Count > 1) then
    raise TGocciaParseError.Create('--output=json supports a single input path.');

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
  if (FOutputPath.Present and (FOutputPath.Value = 'json')) then
    WriteLn(BuildErrorJSON('', ExceptionToScriptLoaderErrorInfo(AException),
      Default(TScriptLoaderTiming)))
  else
    inherited HandleError(AException);
end;

{ TScriptLoaderApp - AfterExecute }

procedure TScriptLoaderApp.AfterExecute;
var
  ProfileOpcodes, ProfileFunctions: Boolean;
  ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
begin
  if (CoverageOptions.Enabled.Present or CoverageOptions.Format.Present or
      CoverageOptions.OutputPath.Present) and
     Assigned(TGocciaCoverageTracker.Instance) then
  begin
    if not (FOutputPath.Present and (FOutputPath.Value = 'json')) then
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
    ProfileOpcodes := (ProfileMode = Goccia.CLI.Options.pmOpcodes) or
                      (ProfileMode = Goccia.CLI.Options.pmAll);
    ProfileFunctions := (ProfileMode = Goccia.CLI.Options.pmFunctions) or
                        (ProfileMode = Goccia.CLI.Options.pmAll);
  end;

  if (ProfileOpcodes or ProfileFunctions) and
     Assigned(TGocciaProfiler.Instance) then
  begin
    if not (FOutputPath.Present and (FOutputPath.Value = 'json')) then
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
  RunResult := TGocciaApplication.RunApplication(TScriptLoaderApp, 'ScriptLoader');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
