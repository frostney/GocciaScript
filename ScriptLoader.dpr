program ScriptLoader;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  GarbageCollector.Generic,
  TimingUtils,

  Goccia.AST.Node,
  Goccia.Builtins.Console,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Engine.BytecodeBackend,
  Goccia.FileExtensions,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Logger,
  Goccia.Modules.Configuration,
  Goccia.Parser,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.ScriptLoader.JSON,
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

function ParseSource(const ASource: TStringList; const AFileName: string;
  const AGlobals: TGocciaGlobalBuiltins; const ASuppressWarnings: Boolean;
  out ALexTimeNanoseconds, AParseTimeNanoseconds: Int64): TGocciaProgram;
var
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  SourceMap: TGocciaSourceMap;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  Warning: TGocciaParserWarning;
  StartTime, LexEnd, ParseEnd: Int64;
  OrigLine, OrigCol, I: Integer;
begin
  StartTime := GetNanoseconds;
  SourceText := ASource.Text;

  SourceMap := nil;
  if ggJSX in AGlobals then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    SourceMap := JSXResult.SourceMap;
  end;

  try
    Lexer := TGocciaLexer.Create(SourceText, AFileName);
    try
      Tokens := Lexer.ScanTokens;
      LexEnd := GetNanoseconds;
      ALexTimeNanoseconds := LexEnd - StartTime;

      Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
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
            if Assigned(SourceMap) and
               SourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
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
  finally
    SourceMap.Free;
  end;
end;

function CompileSource(const ASource: TStringList; const AFileName: string;
  const AGlobals: TGocciaGlobalBuiltins; const ASuppressWarnings: Boolean = False): TGocciaBytecodeModule;
var
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  LexTimeNanoseconds, ParseTimeNanoseconds: Int64;
begin
  ProgramNode := ParseSource(ASource, AFileName, AGlobals, ASuppressWarnings,
    LexTimeNanoseconds, ParseTimeNanoseconds);
  try
    Compiler := TGocciaCompiler.Create(AFileName);
    try
      Result := Compiler.Compile(ProgramNode);
    finally
      Compiler.Free;
    end;
  finally
    ProgramNode.Free;
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
    if IsJSONGlobalsFile(GGlobalsFiles[I]) then
      AEngine.InjectGlobalsFromJSON(ReadFileText(GGlobalsFiles[I]));

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
    if not IsJSONGlobalsFile(GGlobalsFiles[I]) then
      AEngine.InjectGlobalsFromModule(GGlobalsFiles[I]);
end;

procedure ApplyDataGlobalsToBytecodeBackend(const ABackend: TGocciaBytecodeBackend);
var
  I: Integer;
  Pair: TScriptLoaderGlobalPair;
begin
  for I := 0 to GGlobalsFiles.Count - 1 do
    if IsJSONGlobalsFile(GGlobalsFiles[I]) then
      ABackend.InjectGlobalsFromJSON(ReadFileText(GGlobalsFiles[I]));

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
    if not IsJSONGlobalsFile(GGlobalsFiles[I]) then
      ABackend.InjectGlobalsFromModule(GGlobalsFiles[I]);
end;

function ExecuteInterpreted(const ASource: TStringList; const AFileName: string;
  const AOutputLines: TStrings): TScriptExecutionReport;
var
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;
begin
  Engine := TGocciaEngine.Create(AFileName, ASource, TGocciaEngine.DefaultGlobals);
  try
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
    end;
  finally
    Engine.Free;
  end;

  Result.ResultValue := ScriptResult.Result;
  Result.Timing.LexTimeNanoseconds := ScriptResult.LexTimeNanoseconds;
  Result.Timing.ParseTimeNanoseconds := ScriptResult.ParseTimeNanoseconds;
  Result.Timing.ExecuteTimeNanoseconds := ScriptResult.ExecuteTimeNanoseconds;
  Result.Timing.TotalTimeNanoseconds := ScriptResult.TotalTimeNanoseconds;
end;

function ExecuteBytecodeFromSource(const ASource: TStringList; const AFileName: string;
  const AOutputLines: TStrings): TScriptExecutionReport;
var
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  StartTime, ExecEnd: Int64;
begin
  StartTime := GetNanoseconds;
    Backend := TGocciaBytecodeBackend.Create(AFileName);
  try
    Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);
    ConfigureConsole(Backend.Bootstrap.BuiltinConsole, AOutputLines);
    ApplyDataGlobalsToBytecodeBackend(Backend);
    ConfigureModuleResolver(Backend.ModuleResolver, AFileName, GImportMapPath,
      GInlineAliases);

    ProgramNode := ParseSource(ASource, AFileName, TGocciaEngine.DefaultGlobals,
      GJsonOutput, Result.Timing.LexTimeNanoseconds,
      Result.Timing.ParseTimeNanoseconds);
    try
      Module := Backend.CompileToModule(ProgramNode);
    finally
      ProgramNode.Free;
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
      Result.Timing.ExecuteTimeNanoseconds := ExecEnd - StartTime -
        Result.Timing.LexTimeNanoseconds - Result.Timing.ParseTimeNanoseconds;
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
      Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);
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
    Module := CompileSource(ASource, AFileName, TGocciaEngine.DefaultGlobals,
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
  LoadTimeNanoseconds, FrontEndTimeNanoseconds: Int64;
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
    FrontEndTimeNanoseconds := AReport.Timing.TotalTimeNanoseconds -
      AReport.Timing.ExecuteTimeNanoseconds;
    WriteLn('Running script (bytecode): ', AFileName);
    WriteLn(SysUtils.Format('  Compile: %s | Execute: %s | Total: %s',
      [FormatDuration(FrontEndTimeNanoseconds),
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
           Report.Timing.LexTimeNanoseconds + Report.Timing.ParseTimeNanoseconds then
          Report.Timing.ExecuteTimeNanoseconds :=
            Report.Timing.TotalTimeNanoseconds -
            Report.Timing.LexTimeNanoseconds -
            Report.Timing.ParseTimeNanoseconds;
        if GJsonOutput then
          PrintJSONError(E, Report, OutputLines, AFileName)
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

  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
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
  WriteLn('  --globals=<path>        Inject globals from a JSON file or a module with named exports');
  WriteLn('  --timeout=<ms>          Abort execution if it runs longer than the given milliseconds');
  WriteLn('  --output=json           Write structured JSON result to stdout');
  WriteLn('  --output=<path>         Output file path (used with --emit)');
  WriteLn('  --silent                Suppress console output from the script');
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
  Logger.Levels := [];
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

    if GTimeoutMilliseconds < 0 then
    begin
      WriteLn('Error: --timeout must be 0 or greater.');
      ExitCode := 1;
      Exit;
    end;

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
  finally
    Paths.Free;
    GGlobalsFiles.Free;
    GInlineGlobals.Free;
    GInlineAliases.Free;
  end;
end.
