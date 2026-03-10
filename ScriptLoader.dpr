program ScriptLoader;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  GarbageCollector.Generic,
  Souffle.Bytecode.Binary,
  Souffle.Bytecode.Module,
  Souffle.Wasm.Translator,
  TimingUtils,

  Goccia.AST.Node,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.FileExtensions,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TExecutionMode = (emInterpreted, emBytecode);
  TEmitFormat = (efBytecode, efWasm);

var
  GMode: TExecutionMode = emInterpreted;
  GEmitFormat: TEmitFormat = efBytecode;
  GOutputPath: string = '';
  GEmitOnly: Boolean = False;

function ParseSourceFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaProgram;
var
  Source: TStringList;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  SourceMap: TGocciaSourceMap;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  Warning: TGocciaParserWarning;
  OrigLine, OrigCol, I: Integer;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    SourceText := Source.Text;

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
  finally
    Source.Free;
  end;
end;

function CompileSourceFile(const AFileName: string;
  const AGlobals: TGocciaGlobalBuiltins): TSouffleBytecodeModule;
var
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
begin
  ProgramNode := ParseSourceFile(AFileName, AGlobals);
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

procedure RunInterpreted(const AFileName: string);
var
  ScriptResult: TGocciaScriptResult;
begin
  WriteLn('Running script (interpreted): ', AFileName);
  ScriptResult := TGocciaEngine.RunScriptFromFile(AFileName, TGocciaEngine.DefaultGlobals);
  WriteLn(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
    [FormatDuration(ScriptResult.LexTimeNanoseconds), FormatDuration(ScriptResult.ParseTimeNanoseconds),
     FormatDuration(ScriptResult.ExecuteTimeNanoseconds), FormatDuration(ScriptResult.TotalTimeNanoseconds)]));
  Writeln('Result: ', ScriptResult.Result.ToStringLiteral.Value);
end;

procedure RunBytecodeFromSource(const AFileName: string);
var
  ProgramNode: TGocciaProgram;
  Module: TSouffleBytecodeModule;
  Backend: TGocciaSouffleBackend;
  StartTime, CompileEnd, ExecEnd: Int64;
  ResultValue: TGocciaValue;
begin
  WriteLn('Running script (bytecode): ', AFileName);
  StartTime := GetNanoseconds;

  Backend := TGocciaSouffleBackend.Create(AFileName);
  try
    Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);

    ProgramNode := ParseSourceFile(AFileName, TGocciaEngine.DefaultGlobals);
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
  Module: TSouffleBytecodeModule;
  Backend: TGocciaSouffleBackend;
  StartTime, LoadEnd, ExecEnd: Int64;
  ResultValue: TGocciaValue;
begin
  WriteLn('Running bytecode: ', AFileName);
  StartTime := GetNanoseconds;

  Module := LoadModuleFromFile(AFileName);
  LoadEnd := GetNanoseconds;
  try
    Backend := TGocciaSouffleBackend.Create(AFileName);
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

procedure EmitBytecode(const AFileName, AOutputPath: string);
var
  Module: TSouffleBytecodeModule;
  StartTime, EndTime: Int64;
begin
  WriteLn('Compiling: ', AFileName);
  StartTime := GetNanoseconds;

  TGarbageCollector.Initialize;
  try
    Module := CompileSourceFile(AFileName, TGocciaEngine.DefaultGlobals);
    try
      SaveModuleToFile(Module, AOutputPath);
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

procedure EmitWasm(const AFileName, AOutputPath: string);
var
  Module: TSouffleBytecodeModule;
  Translator: TSouffleWasmTranslator;
  StartTime, EndTime: Int64;
begin
  WriteLn('Compiling to WASM: ', AFileName);
  StartTime := GetNanoseconds;

  TGarbageCollector.Initialize;
  try
    Module := CompileSourceFile(AFileName, TGocciaEngine.DefaultGlobals);
    try
      Translator := TSouffleWasmTranslator.Create;
      try
        Translator.TranslateToFile(Module, AOutputPath);
        EndTime := GetNanoseconds;
        WriteLn(SysUtils.Format('  Compiled to %s (%s)',
          [AOutputPath, FormatDuration(EndTime - StartTime)]));
      finally
        Translator.Free;
      end;
    finally
      Module.Free;
    end;
  finally
    TGarbageCollector.Shutdown;
  end;
end;

procedure RunScriptFromFile(const AFileName: string);
var
  Ext, OutputPath: string;
begin
  try
    Ext := LowerCase(ExtractFileExt(AFileName));

    if Ext = EXT_SBC then
    begin
      RunBytecodeFromFile(AFileName);
      Exit;
    end;

    if GEmitOnly then
    begin
      case GEmitFormat of
        efBytecode:
        begin
          if GOutputPath <> '' then
            OutputPath := GOutputPath
          else
            OutputPath := ChangeFileExt(AFileName, EXT_SBC);
          EmitBytecode(AFileName, OutputPath);
        end;
        efWasm:
        begin
          if GOutputPath <> '' then
            OutputPath := GOutputPath
          else
            OutputPath := ChangeFileExt(AFileName, EXT_WASM);
          EmitWasm(AFileName, OutputPath);
        end;
      end;
      Exit;
    end;

    case GMode of
      emInterpreted: RunInterpreted(AFileName);
      emBytecode:    RunBytecodeFromSource(AFileName);
    end;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure RunScripts(const APath: string);
var
  Files: TStringList;
  I: Integer;
begin
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
  WriteLn('Usage: ScriptLoader <file|directory> [options]');
  WriteLn;
  WriteLn('  <file>                  Script (.js, .jsx, .ts, .tsx, .mjs) or bytecode (.sbc)');
  WriteLn('  <directory>             Run all scripts in directory');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --mode=interpreted      Tree-walk interpreter (default)');
  WriteLn('  --mode=bytecode         Compile to bytecode and execute on Souffle VM');
  WriteLn('  --emit                  Compile to .sbc file (no execution)');
  WriteLn('  --emit=bytecode         Compile to .sbc file (explicit, same as --emit)');
  WriteLn('  --emit=wasm             Compile to .wasm file');
  WriteLn('  --output=<path>         Output file path (used with --emit)');
end;

var
  Paths: TStringList;
  I: Integer;
  Arg, ModeStr, EmitStr: string;

begin
  GMode := emInterpreted;
  GEmitFormat := efBytecode;
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
          GEmitFormat := efBytecode
        else if EmitStr = 'wasm' then
          GEmitFormat := efWasm
        else
        begin
          WriteLn('Error: Unknown emit format "', EmitStr, '". Use "bytecode" or "wasm".');
          ExitCode := 1;
          Exit;
        end;
      end
      else if Arg = '--emit' then
      begin
        GEmitOnly := True;
        GEmitFormat := efBytecode;
      end
      else if Copy(Arg, 1, 9) = '--output=' then
        GOutputPath := Copy(Arg, 10, MaxInt)
      else if Copy(Arg, 1, 2) <> '--' then
        Paths.Add(Arg);
    end;

    if Paths.Count = 0 then
    begin
      PrintUsage;
      ExitCode := 1;
      Exit;
    end;

    for I := 0 to Paths.Count - 1 do
    begin
      if I > 0 then WriteLn;
      RunScripts(Paths[I]);
    end;
  finally
    Paths.Free;
  end;
end.
