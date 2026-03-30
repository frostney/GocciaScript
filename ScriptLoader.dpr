program ScriptLoader;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  GarbageCollector.Generic,
  TimingUtils,

  Goccia.AST.Node,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Engine.BytecodeBackend,
  Goccia.FileExtensions,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.ScriptLoader.Input,
  Goccia.Token,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TExecutionMode = (emInterpreted, emBytecode);

var
  GMode: TExecutionMode = emInterpreted;
  GOutputPath: string = '';
  GEmitOnly: Boolean = False;

function ParseSource(const ASource: TStringList; const AFileName: string;
  const AGlobals: TGocciaGlobalBuiltins): TGocciaProgram;
var
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  SourceMap: TGocciaSourceMap;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  Warning: TGocciaParserWarning;
  OrigLine, OrigCol, I: Integer;
begin
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
      Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
      try
        Result := Parser.Parse;
        for I := 0 to Parser.WarningCount - 1 do
        begin
          Warning := Parser.GetWarning(I);
          WriteLn(SysUtils.Format('Warning: %s', [Warning.Message]));
          if Warning.Suggestion <> '' then
            WriteLn(SysUtils.Format('  Suggestion: %s', [Warning.Suggestion]));
          if Assigned(SourceMap) and SourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
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

function ParseSourceFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaProgram;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    Result := ParseSource(Source, AFileName, AGlobals);
  finally
    Source.Free;
  end;
end;

function CompileSource(const ASource: TStringList; const AFileName: string;
  const AGlobals: TGocciaGlobalBuiltins): TGocciaBytecodeModule;
var
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
begin
  ProgramNode := ParseSource(ASource, AFileName, AGlobals);
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

function CompileSourceFile(const AFileName: string;
  const AGlobals: TGocciaGlobalBuiltins): TGocciaBytecodeModule;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    Result := CompileSource(Source, AFileName, AGlobals);
  finally
    Source.Free;
  end;
end;

procedure RunInterpreted(const ASource: TStringList; const AFileName: string);
var
  ScriptResult: TGocciaScriptResult;
begin
  WriteLn('Running script (interpreted): ', AFileName);
  ScriptResult := TGocciaEngine.RunScriptFromStringList(ASource, AFileName, TGocciaEngine.DefaultGlobals);
  WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
    [FormatDuration(ScriptResult.LexTimeNanoseconds), FormatDuration(ScriptResult.ParseTimeNanoseconds),
     FormatDuration(ScriptResult.ExecuteTimeNanoseconds), FormatDuration(ScriptResult.TotalTimeNanoseconds)]));
  Writeln('Result: ', ScriptResult.Result.ToStringLiteral.Value);
end;

procedure RunBytecodeFromSource(const ASource: TStringList; const AFileName: string);
var
  ProgramNode: TGocciaProgram;
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  StartTime, CompileEnd, ExecEnd: Int64;
  ResultValue: TGocciaValue;
begin
  WriteLn('Running script (bytecode): ', AFileName);
  StartTime := GetNanoseconds;

  Backend := TGocciaBytecodeBackend.Create(AFileName);
  try
    Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);

    ProgramNode := ParseSource(ASource, AFileName, TGocciaEngine.DefaultGlobals);
    try
      Module := Backend.CompileToModule(ProgramNode);
    finally
      ProgramNode.Free;
    end;
    CompileEnd := GetNanoseconds;
    try
      ResultValue := Backend.RunModule(Module);
      ExecEnd := GetNanoseconds;
      WriteLn(SysUtils.Format('  Compile: %s | Execute: %s | Total: %s',
        [FormatDuration(CompileEnd - StartTime),
         FormatDuration(ExecEnd - CompileEnd),
         FormatDuration(ExecEnd - StartTime)]));
      WriteLn('Result: ', ResultValue.ToStringLiteral.Value);
    finally
      Module.Free;
    end;
  finally
    Backend.Free;
  end;
end;

procedure RunBytecodeFromFile(const AFileName: string);
var
  Module: TGocciaBytecodeModule;
  Backend: TGocciaBytecodeBackend;
  StartTime, LoadEnd, ExecEnd: Int64;
  ResultValue: TGocciaValue;
begin
  WriteLn('Running bytecode: ', AFileName);
  StartTime := GetNanoseconds;

  Module := Goccia.Bytecode.Binary.LoadModuleFromFile(AFileName);
  LoadEnd := GetNanoseconds;
  try
  Backend := TGocciaBytecodeBackend.Create(AFileName);
  try
    Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);
      ResultValue := Backend.RunModule(Module);
      ExecEnd := GetNanoseconds;
      WriteLn(SysUtils.Format('  Load: %s | Execute: %s | Total: %s',
        [FormatDuration(LoadEnd - StartTime),
         FormatDuration(ExecEnd - LoadEnd),
         FormatDuration(ExecEnd - StartTime)]));
      WriteLn('Result: ', ResultValue.ToStringLiteral.Value);
    finally
      Backend.Free;
    end;
  finally
    Module.Free;
  end;
end;

procedure EmitBytecode(const ASource: TStringList; const AFileName, AOutputPath: string);
var
  Module: TGocciaBytecodeModule;
  StartTime, EndTime: Int64;
begin
  WriteLn('Compiling: ', AFileName);
  StartTime := GetNanoseconds;

  TGarbageCollector.Initialize;
  try
    Module := CompileSource(ASource, AFileName, TGocciaEngine.DefaultGlobals);
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

procedure RunSource(const ASource: TStringList; const AFileName: string);
var
  Ext, OutputPath: string;
begin
  try
    Ext := LowerCase(ExtractFileExt(AFileName));

    if Ext = EXT_GBC then
    begin
      RunBytecodeFromFile(AFileName);
      Exit;
    end;

    if GEmitOnly then
    begin
      if GOutputPath <> '' then
        OutputPath := GOutputPath
      else if AFileName = STDIN_FILE_NAME then
      begin
        WriteLn('Error: --output=<path> is required when emitting bytecode from stdin.');
        ExitCode := 1;
        Exit;
      end
      else
        OutputPath := ChangeFileExt(AFileName, EXT_GBC);
      EmitBytecode(ASource, AFileName, OutputPath);
      Exit;
    end;

    case GMode of
      emInterpreted: RunInterpreted(ASource, AFileName);
      emBytecode:    RunBytecodeFromSource(ASource, AFileName);
    end;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure RunScriptFromFile(const AFileName: string);
var
  Source: TStringList;
begin
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
    Files := FindAllFiles(APath, ScriptExtensions);
    try
      for I := 0 to Files.Count - 1 do
      begin
        if I > 0 then WriteLn;
        RunScriptFromFile(Files[I]);
      end;
    finally
      Files.Free;
    end;
  end
  else if FileExists(APath) then
    RunScriptFromFile(APath)
  else
  begin
    WriteLn('Error: Path not found: ', APath);
    ExitCode := 1;
  end;
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
  WriteLn('  --output=<path>         Output file path (used with --emit)');
end;

var
  Paths: TStringList;
  I: Integer;
  Arg, ModeStr, EmitStr: string;

begin
  GMode := emInterpreted;
  GOutputPath := '';
  GEmitOnly := False;

  Paths := TStringList.Create;
  try
    for I := 1 to ParamCount do
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
        if (EmitStr = 'bytecode') or (EmitStr = '') then
        begin
          // Valid.
        end
        else
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
        GOutputPath := Copy(Arg, 10, MaxInt)
      else if Copy(Arg, 1, 2) <> '--' then
        Paths.Add(Arg)
      else
      begin
        WriteLn('Error: Unknown option "', Arg, '".');
        PrintUsage;
        ExitCode := 1;
        Exit;
      end;
    end;

    if Paths.Count = 0 then
      RunScriptFromStdin
    else
      for I := 0 to Paths.Count - 1 do
      begin
        if I > 0 then WriteLn;
        RunScripts(Paths[I]);
      end;
  finally
    Paths.Free;
  end;
end.
