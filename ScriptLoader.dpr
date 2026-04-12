program ScriptLoader;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.AST.Node,
  Goccia.Builtins.Console,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Coverage,
  Goccia.Coverage.Report,
  Goccia.Engine,
  Goccia.Engine.BytecodeBackend,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Modules.Configuration,
  Goccia.Parser,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.ScriptLoader.JSON,
  Goccia.SourceMap,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TExecutionMode = (emInterpreted, emBytecode);

  TScriptExecutionReport = record
    ResultValue: TGocciaValue;
    Timing: TScriptLoaderTiming;
  end;

var
  GMode: TExecutionMode = emInterpreted;
  GOutputPath: string = '';
  GEmitOnly: Boolean = False;
  GJsonOutput: Boolean = False;
  GSilentConsole: Boolean = False;
  GTimeoutMilliseconds: Integer = 0;
  GImportMapPath: string = '';
  GGlobalsFiles: TStringList = nil;
  GInlineGlobals: TStringList = nil;
  GInlineAliases: TStringList = nil;
  GCoverageEnabled: Boolean = False;
  GCoverageLcovEnabled: Boolean = False;
  GCoverageJsonEnabled: Boolean = False;
  GCoverageOutputPath: string = '';
  GASIEnabled: Boolean = False;
  GProfileOpcodes: Boolean = False;
  GProfileFunctions: Boolean = False;
  GProfileOutputPath: string = '';
  GProfileFormatFlamegraph: Boolean = False;
  GSourceMapEnabled: Boolean = False;
  GSourceMapOutputPath: string = '';

function ParseSource(const ASource: TStringList; const AFileName: string;
  const APreprocessors: TGocciaPreprocessors; const ASuppressWarnings: Boolean;
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
      Parser.AutomaticSemicolonInsertion := GASIEnabled;
      try
        Result := Parser.Parse;
        ParseEnd := GetNanoseconds;
        AParseTimeNanoseconds := ParseEnd - LexEnd;

        if not ASuppressWarnings then
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

procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
  const AFileName: string);
var
  OutputPath: string;
begin
  if not GSourceMapEnabled then
    Exit;
  if not Assigned(ASourceMap) then
    Exit;
  if GSourceMapOutputPath <> '' then
    OutputPath := GSourceMapOutputPath
  else
    OutputPath := AFileName + EXT_MAP;
  ASourceMap.FileName := ExtractFileName(AFileName);
  ASourceMap.SetSourcePath(0, ExtractFileName(AFileName));
  ASourceMap.SaveToFile(OutputPath);
  if not GJsonOutput then
    WriteLn(SysUtils.Format('  Source map written to %s', [OutputPath]));
end;

function CompileSource(const ASource: TStringList; const AFileName: string;
  const APreprocessors: TGocciaPreprocessors; const ASuppressWarnings: Boolean = False): TGocciaBytecodeModule;
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

procedure ConfigureConsole(const AConsole: TGocciaConsole;
  const AOutputLines: TStrings);
begin
  if not Assigned(AConsole) then
    Exit;

  AConsole.Enabled := not GSilentConsole;
  if GJsonOutput and not GSilentConsole then
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

procedure ApplyDataGlobalsToEngine(const AEngine: TGocciaEngine);
var
  I: Integer;
  Pair: TScriptLoaderGlobalPair;
begin
  for I := 0 to GGlobalsFiles.Count - 1 do
    if IsStructuredGlobalsFile(GGlobalsFiles[I]) then
    begin
      if IsYAMLGlobalsFile(GGlobalsFiles[I]) then
        AEngine.InjectGlobalsFromYAML(ReadFileText(GGlobalsFiles[I]))
      else if IsJSON5GlobalsFile(GGlobalsFiles[I]) then
        AEngine.InjectGlobalsFromJSON5(ReadFileText(GGlobalsFiles[I]))
      else if IsTOMLGlobalsFile(GGlobalsFiles[I]) then
        AEngine.InjectGlobalsFromTOML(ReadFileText(GGlobalsFiles[I]))
      else
        AEngine.InjectGlobalsFromJSON(ReadFileText(GGlobalsFiles[I]));
    end;

  for I := 0 to GInlineGlobals.Count - 1 do
  begin
    Pair := ParseGlobalPair(GInlineGlobals[I]);
    AEngine.InjectGlobal(Pair.Key, ParseInlineGlobalValue(Pair.ValueText));
  end;
end;

procedure ApplyModuleGlobalsToEngine(const AEngine: TGocciaEngine);
var
  I: Integer;
begin
  for I := 0 to GGlobalsFiles.Count - 1 do
    if not IsStructuredGlobalsFile(GGlobalsFiles[I]) then
      AEngine.InjectGlobalsFromModule(GGlobalsFiles[I]);
end;

procedure ApplyDataGlobalsToBytecodeBackend(const ABackend: TGocciaBytecodeBackend);
var
  I: Integer;
  Pair: TScriptLoaderGlobalPair;
begin
  for I := 0 to GGlobalsFiles.Count - 1 do
    if IsStructuredGlobalsFile(GGlobalsFiles[I]) then
    begin
      if IsYAMLGlobalsFile(GGlobalsFiles[I]) then
        ABackend.InjectGlobalsFromYAML(ReadFileText(GGlobalsFiles[I]))
      else if IsJSON5GlobalsFile(GGlobalsFiles[I]) then
        ABackend.InjectGlobalsFromJSON5(ReadFileText(GGlobalsFiles[I]))
      else if IsTOMLGlobalsFile(GGlobalsFiles[I]) then
        ABackend.InjectGlobalsFromTOML(ReadFileText(GGlobalsFiles[I]))
      else
        ABackend.InjectGlobalsFromJSON(ReadFileText(GGlobalsFiles[I]));
    end;

  for I := 0 to GInlineGlobals.Count - 1 do
  begin
    Pair := ParseGlobalPair(GInlineGlobals[I]);
    ABackend.RegisterGlobal(Pair.Key, ParseInlineGlobalValue(Pair.ValueText));
  end;
end;

procedure ApplyModuleGlobalsToBytecodeBackend(const ABackend: TGocciaBytecodeBackend);
var
  I: Integer;
begin
  for I := 0 to GGlobalsFiles.Count - 1 do
    if not IsStructuredGlobalsFile(GGlobalsFiles[I]) then
      ABackend.InjectGlobalsFromModule(GGlobalsFiles[I]);
end;

function ExecuteInterpreted(const ASource: TStringList; const AFileName: string;
  const AOutputLines: TStrings): TScriptExecutionReport;
var
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;
  SourceMap: TGocciaSourceMap;
begin
  Engine := TGocciaEngine.Create(AFileName, ASource, []);
  try
    Engine.ASIEnabled := GASIEnabled;
    Engine.SuppressWarnings := GJsonOutput;
    ConfigureConsole(Engine.BuiltinConsole, AOutputLines);
    ApplyDataGlobalsToEngine(Engine);
    ConfigureModuleResolver(Engine.Resolver, AFileName, GImportMapPath,
      GInlineAliases);
    StartExecutionTimeout(GTimeoutMilliseconds);
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

function ExecuteBytecodeFromSource(const ASource: TStringList; const AFileName: string;
  const AOutputLines: TStrings): TScriptExecutionReport;
var
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  SourceMap: TGocciaSourceMap;
  StartTime, CompileStart, CompileEnd, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
  Backend := TGocciaBytecodeBackend.Create(AFileName);
  try
    Backend.ASIEnabled := GASIEnabled;
    Backend.RegisterBuiltIns([]);
    ConfigureConsole(Backend.Bootstrap.BuiltinConsole, AOutputLines);
    ApplyDataGlobalsToBytecodeBackend(Backend);
    ConfigureModuleResolver(Backend.ModuleResolver, AFileName, GImportMapPath,
      GInlineAliases);

    ProgramNode := ParseSource(ASource, AFileName, TGocciaEngine.DefaultPreprocessors,
      GJsonOutput, Result.Timing.LexTimeNanoseconds,
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

    try
      StartExecutionTimeout(GTimeoutMilliseconds);
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

function ExecuteBytecodeFromFile(const AFileName: string;
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
    Backend := TGocciaBytecodeBackend.Create(AFileName);
    try
      Backend.ASIEnabled := GASIEnabled;
      Backend.RegisterBuiltIns([]);
      ConfigureConsole(Backend.Bootstrap.BuiltinConsole, AOutputLines);
      ApplyDataGlobalsToBytecodeBackend(Backend);
      ConfigureModuleResolver(Backend.ModuleResolver, AFileName,
        GImportMapPath, GInlineAliases);
      StartExecutionTimeout(GTimeoutMilliseconds);
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

procedure EmitBytecode(const ASource: TStringList; const AFileName,
  AOutputPath: string);
var
  Module: TGocciaBytecodeModule;
  StartTime, EndTime: Int64;
begin
  WriteLn('Compiling: ', AFileName);
  StartTime := GetNanoseconds;

  TGarbageCollector.Initialize;
  try
    Module := CompileSource(ASource, AFileName, TGocciaEngine.DefaultPreprocessors,
      GJsonOutput);
    try
      Goccia.Bytecode.Binary.SaveModuleToFile(Module, AOutputPath);
      EndTime := GetNanoseconds;
      WriteLn(SysUtils.Format('  Compiled to %s (%s)',
        [AOutputPath, FormatDuration(EndTime - StartTime)]));
    finally
      Module.Free;
    end;
  finally
    TGarbageCollector.Shutdown;
  end;
end;

procedure PrintHumanReadableResult(const AFileName: string;
  const AReport: TScriptExecutionReport; const AExtension: string);
var
  LoadTimeNanoseconds: Int64;
begin
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
  else if GMode = emBytecode then
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

procedure RunSource(const ASource: TStringList; const AFileName: string);
var
  Extension, OutputPath: string;
  Report: TScriptExecutionReport;
  OutputLines: TStringList;
  StartTime: Int64;
begin
  FillChar(Report, SizeOf(Report), 0);
  Report.ResultValue := nil;

  OutputLines := nil;
  if GJsonOutput then
    OutputLines := TStringList.Create;
  try
    StartTime := GetNanoseconds;
    try
      Extension := LowerCase(ExtractFileExt(AFileName));

      if Extension = EXT_GBC then
        Report := ExecuteBytecodeFromFile(AFileName, OutputLines)
      else if GEmitOnly then
      begin
        if GOutputPath <> '' then
          OutputPath := GOutputPath
        else if AFileName = STDIN_FILE_NAME then
          raise Exception.Create('--output=<path> is required when emitting bytecode from stdin.')
        else
          OutputPath := ChangeFileExt(AFileName, EXT_GBC);
        EmitBytecode(ASource, AFileName, OutputPath);
        Exit;
      end
      else
        case GMode of
          emInterpreted: Report := ExecuteInterpreted(ASource, AFileName, OutputLines);
          emBytecode:    Report := ExecuteBytecodeFromSource(ASource, AFileName, OutputLines);
        end;

      if GJsonOutput then
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
        if GJsonOutput then
          PrintJSONError(E, Report, OutputLines, AFileName)
        else if E is TGocciaError then
          WriteLn(TGocciaError(E).GetDetailedMessage(IsColorTerminal))
        else
          WriteLn('Fatal error: ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    OutputLines.Free;
  end;
end;

procedure RunScriptFromFile(const AFileName: string);
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

procedure RunScriptFromStdin;
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

procedure RunScripts(const APath: string);
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
    if GJsonOutput then
      raise Exception.Create('--output=json does not support directory input.');

    Files := FindAllFiles(APath, ScriptExtensions);
    try
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

procedure PrintUsage;
begin
  WriteLn('Usage: ScriptLoader [file|directory|-] [options]');
  WriteLn;
  WriteLn('  <file>                  Script (.js, .jsx, .ts, .tsx, .mjs) or bytecode (.gbc)');
  WriteLn('  <directory>             Run all scripts in directory');
  WriteLn('  -                       Read source from stdin');
  WriteLn('  (omitted path)          Read source from stdin');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --mode=interpreted      Tree-walk interpreter (default)');
  WriteLn('  --mode=bytecode         Compile to bytecode and execute on the Goccia VM');
  WriteLn('  --emit                  Compile to .gbc file (no execution)');
  WriteLn('  --emit=bytecode         Compile to .gbc file (explicit, same as --emit)');
  WriteLn('  --import-map=<path>     Load WHATWG-style import map JSON before execution');
  WriteLn('  --alias key=value       Add an inline import-map-style alias (exact match unless key ends with "/")');
  WriteLn('  --global name=value     Inject a single global; value is parsed as JSON or kept as a string');
  WriteLn('  --globals=<path>        Inject globals from a JSON/JSON5/TOML/YAML file or a module with named exports');
  WriteLn('  --timeout=<ms>          Abort execution if it runs longer than the given milliseconds');
  WriteLn('  --output=json           Write structured JSON result to stdout');
  WriteLn('  --output=<path>         Output file path (used with --emit)');
  WriteLn('  --silent                Suppress console output from the script');
  WriteLn('  --asi                   Enable automatic semicolon insertion');
  WriteLn('  --coverage              Enable line and branch coverage reporting');
  WriteLn('  --coverage-format=lcov|json  Coverage output format (implies --coverage)');
  WriteLn('  --coverage-output=<file>     Coverage output file (paired with --coverage-format)');
  WriteLn('  --profile=opcodes|functions|all  Enable bytecode profiling (implies --mode=bytecode)');
  WriteLn('  --profile-output=<file>          Write profile results as JSON');
  WriteLn('  --profile-format=flamegraph      Write collapsed stack format for flame graph visualization');
  WriteLn('  --source-map                     Write a .map source map file alongside the output');
  WriteLn('  --source-map=<file>              Write source map to a specific file path');
end;

var
  Paths: TStringList;
  I: Integer;
  Arg, ModeStr, EmitStr, TimeoutStr: string;

begin
  GMode := emInterpreted;
  GOutputPath := '';
  GEmitOnly := False;
  GJsonOutput := False;
  GSilentConsole := False;
  GTimeoutMilliseconds := 0;
  GImportMapPath := '';
  GGlobalsFiles := TStringList.Create;
  GInlineGlobals := TStringList.Create;
  GInlineAliases := TStringList.Create;

  Paths := TStringList.Create;
  try
    I := 1;
    while I <= ParamCount do
    begin
      Arg := ParamStr(I);
      if Copy(Arg, 1, 7) = '--mode=' then
      begin
        ModeStr := Copy(Arg, 8, MaxInt);
        if ModeStr = 'interpreted' then
          GMode := emInterpreted
        else if ModeStr = 'bytecode' then
          GMode := emBytecode
        else
        begin
          WriteLn('Error: Unknown mode "', ModeStr, '". Use "interpreted" or "bytecode".');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Copy(Arg, 1, 7) = '--emit=' then
      begin
        GEmitOnly := True;
        EmitStr := Copy(Arg, 8, MaxInt);
        if (EmitStr <> 'bytecode') and (EmitStr <> '') then
        begin
          WriteLn('Error: Unknown emit format "', EmitStr, '". Use "bytecode".');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Arg = '--emit' then
        GEmitOnly := True
      else if (Arg = '--help') or (Arg = '-h') then
      begin
        PrintUsage;
        Exit;
      end
      else if Copy(Arg, 1, 9) = '--output=' then
      begin
        GOutputPath := Copy(Arg, 10, MaxInt);
        if GOutputPath = 'json' then
          GJsonOutput := True;
      end
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
      else if Copy(Arg, 1, 10) = '--globals=' then
        GGlobalsFiles.Add(Copy(Arg, 11, MaxInt))
      else if Copy(Arg, 1, 10) = '--timeout=' then
      begin
        TimeoutStr := Copy(Arg, 11, MaxInt);
        if not TryStrToInt(TimeoutStr, GTimeoutMilliseconds) then
        begin
          WriteLn('Error: --timeout must be an integer number of milliseconds.');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Arg = '--global' then
      begin
        if I = ParamCount then
        begin
          WriteLn('Error: --global requires a name=value argument.');
          ExitCode := 1;
          Exit;
        end;
        Inc(I);
        GInlineGlobals.Add(ParamStr(I));
      end
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
      else if Arg = '--silent' then
        GSilentConsole := True
      else if Arg = '--asi' then
        GASIEnabled := True
      else if Arg = '--coverage' then
        GCoverageEnabled := True
      else if Arg = '--coverage-format=lcov' then
      begin
        GCoverageLcovEnabled := True;
        GCoverageEnabled := True;
      end
      else if Arg = '--coverage-format=json' then
      begin
        GCoverageJsonEnabled := True;
        GCoverageEnabled := True;
      end
      else if Copy(Arg, 1, 18) = '--coverage-format=' then
      begin
        WriteLn('Error: Unknown coverage format "', Copy(Arg, 19, MaxInt), '". Use "lcov" or "json".');
        ExitCode := 1;
        Exit;
      end
      else if Copy(Arg, 1, 18) = '--coverage-output=' then
      begin
        GCoverageOutputPath := Copy(Arg, 19, MaxInt);
        GCoverageEnabled := True;
      end
      else if Arg = '--profile=opcodes' then
        GProfileOpcodes := True
      else if Arg = '--profile=functions' then
        GProfileFunctions := True
      else if Arg = '--profile=all' then
      begin
        GProfileOpcodes := True;
        GProfileFunctions := True;
      end
      else if Copy(Arg, 1, 10) = '--profile=' then
      begin
        WriteLn('Error: Unknown profile mode "', Copy(Arg, 11, MaxInt),
          '". Use "opcodes", "functions", or "all".');
        ExitCode := 1;
        Exit;
      end
      else if Copy(Arg, 1, 17) = '--profile-output=' then
      begin
        GProfileOutputPath := Copy(Arg, 18, MaxInt);
        if GProfileOutputPath = '' then
        begin
          WriteLn('Error: --profile-output requires a non-empty path.');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Arg = '--profile-format=flamegraph' then
        GProfileFormatFlamegraph := True
      else if Copy(Arg, 1, 18) = '--profile-format=' then
      begin
        WriteLn('Error: Unknown profile format "', Copy(Arg, 19, MaxInt),
          '". Use "flamegraph".');
        ExitCode := 1;
        Exit;
      end
      else if Arg = '--source-map' then
        GSourceMapEnabled := True
      else if Copy(Arg, 1, 13) = '--source-map=' then
      begin
        GSourceMapOutputPath := Copy(Arg, 14, MaxInt);
        GSourceMapEnabled := True;
        if GSourceMapOutputPath = '' then
        begin
          WriteLn('Error: --source-map= requires a non-empty path.');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Copy(Arg, 1, 2) <> '--' then
        Paths.Add(Arg)
      else
      begin
        WriteLn('Error: Unknown option "', Arg, '".');
        PrintUsage;
        ExitCode := 1;
        Exit;
      end;
      Inc(I);
    end;

    if GJsonOutput and GEmitOnly then
    begin
      WriteLn('Error: --output=json cannot be used with --emit.');
      ExitCode := 1;
      Exit;
    end;

    if GJsonOutput and (Paths.Count > 1) then
    begin
      WriteLn('Error: --output=json supports a single input path.');
      ExitCode := 1;
      Exit;
    end;

    if GSourceMapEnabled and (GSourceMapOutputPath = '') and
       ((Paths.Count = 0) or
        ((Paths.Count = 1) and IsStdinPath(Paths[0]))) then
    begin
      WriteLn('Error: --source-map=<file> is required when reading source from stdin.');
      ExitCode := 1;
      Exit;
    end;

    if (GSourceMapOutputPath <> '') and
       ((Paths.Count > 1) or
        ((Paths.Count = 1) and DirectoryExists(Paths[0]))) then
    begin
      WriteLn('Error: --source-map=<file> supports a single input file or stdin.');
      ExitCode := 1;
      Exit;
    end;

    if GTimeoutMilliseconds < 0 then
    begin
      WriteLn('Error: --timeout must be 0 or greater.');
      ExitCode := 1;
      Exit;
    end;

    // --profile-format implies --profile=functions when no explicit --profile given
    if GProfileFormatFlamegraph and not (GProfileOpcodes or GProfileFunctions) then
      GProfileFunctions := True;

    // Profiling requires bytecode mode regardless of --mode flag
    if GProfileOpcodes or GProfileFunctions then
      GMode := emBytecode;

    if (GProfileOutputPath <> '') and not (GProfileOpcodes or GProfileFunctions) then
    begin
      WriteLn('Error: --profile-output requires --profile=opcodes|functions|all.');
      ExitCode := 1;
      Exit;
    end;

    if GProfileFormatFlamegraph and (GProfileOutputPath = '') then
    begin
      WriteLn('Error: --profile-format=flamegraph requires --profile-output=<path>.');
      ExitCode := 1;
      Exit;
    end;

    if GCoverageEnabled then
    begin
      TGocciaCoverageTracker.Initialize;
      TGocciaCoverageTracker.Instance.Enabled := True;
    end;
    if GProfileOpcodes or GProfileFunctions then
    begin
      TGocciaProfiler.Initialize;
      TGocciaProfiler.Instance.Enabled := True;
      TGocciaProfiler.Instance.Mode := [];
      if GProfileOpcodes then
        TGocciaProfiler.Instance.Mode :=
          TGocciaProfiler.Instance.Mode + [pmOpcodes];
      if GProfileFunctions then
        TGocciaProfiler.Instance.Mode :=
          TGocciaProfiler.Instance.Mode + [pmFunctions];
    end;
    try
      try
        if Paths.Count = 0 then
          RunScriptFromStdin
        else
          for I := 0 to Paths.Count - 1 do
          begin
            if I > 0 then
              WriteLn;
            RunScripts(Paths[I]);
          end;
      except
        on E: Exception do
        begin
          if GJsonOutput then
            WriteLn(BuildErrorJSON('', ExceptionToScriptLoaderErrorInfo(E),
              Default(TScriptLoaderTiming)))
          else
            WriteLn('Error: ', E.Message);
          ExitCode := 1;
        end;
      end;

      if GCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
      begin
        if not GJsonOutput then
        begin
          PrintCoverageSummary(TGocciaCoverageTracker.Instance);
          if (Paths.Count = 1) and FileExists(Paths[0]) then
            PrintCoverageDetail(TGocciaCoverageTracker.Instance, Paths[0]);
        end;
        if GCoverageLcovEnabled and (GCoverageOutputPath <> '') then
          WriteCoverageLcov(TGocciaCoverageTracker.Instance, GCoverageOutputPath);
        if GCoverageJsonEnabled and (GCoverageOutputPath <> '') then
          WriteCoverageJSON(TGocciaCoverageTracker.Instance, GCoverageOutputPath);
      end;

      if (GProfileOpcodes or GProfileFunctions) and
         Assigned(TGocciaProfiler.Instance) then
      begin
        if not GJsonOutput then
        begin
          if GProfileOpcodes then
          begin
            PrintOpcodeProfile(TGocciaProfiler.Instance);
            PrintOpcodePairProfile(TGocciaProfiler.Instance);
            PrintScalarHitRate(TGocciaProfiler.Instance);
          end;
          if GProfileFunctions then
            PrintFunctionProfile(TGocciaProfiler.Instance);
        end;
        if GProfileOutputPath <> '' then
        begin
          if GProfileFormatFlamegraph then
            WriteCollapsedStacks(TGocciaProfiler.Instance, GProfileOutputPath)
          else
            WriteProfileJSON(TGocciaProfiler.Instance, GProfileOutputPath);
        end;
      end;
    finally
      if GCoverageEnabled then
        TGocciaCoverageTracker.Shutdown;
      if GProfileOpcodes or GProfileFunctions then
        TGocciaProfiler.Shutdown;
    end;
  finally
    Paths.Free;
    GGlobalsFiles.Free;
    GInlineGlobals.Free;
    GInlineAliases.Free;
  end;
end.
