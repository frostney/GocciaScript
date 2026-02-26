program ScriptLoader;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Souffle.Bytecode.Binary,
  Souffle.Bytecode.Module,
  TimingUtils,

  Goccia.AST.Node,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TExecutionMode = (emInterpreted, emBytecode);

var
  GMode: TExecutionMode = emInterpreted;
  GEmitPath: string = '';
  GEmitOnly: Boolean = False;

function ParseSourceFile(const AFileName: string): TGocciaProgram;
var
  Source: TStringList;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    Lexer := TGocciaLexer.Create(Source.Text, AFileName);
    try
      Tokens := Lexer.ScanTokens;
      Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
      try
        Result := Parser.Parse;
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
    end;
  finally
    Source.Free;
  end;
end;

function CompileSourceFile(const AFileName: string): TSouffleBytecodeModule;
var
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
begin
  ProgramNode := ParseSourceFile(AFileName);
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

    ProgramNode := ParseSourceFile(AFileName);
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

  TGocciaGarbageCollector.Initialize;
  try
    Module := CompileSourceFile(AFileName);
    try
      SaveModuleToFile(Module, AOutputPath);
      EndTime := GetNanoseconds;
      WriteLn(SysUtils.Format('  Compiled to %s (%s)',
        [AOutputPath, FormatDuration(EndTime - StartTime)]));
    finally
      Module.Free;
    end;
  finally
    TGocciaGarbageCollector.Shutdown;
  end;
end;

procedure RunScriptFromFile(const AFileName: string);
var
  Ext: string;
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
      if GEmitPath <> '' then
        EmitBytecode(AFileName, GEmitPath)
      else
        EmitBytecode(AFileName, ChangeFileExt(AFileName, EXT_SBC));
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
  WriteLn('  --emit[=output.sbc]     Compile to .sbc file (no execution)');
end;

var
  Paths: TStringList;
  I: Integer;
  Arg, ModeStr: string;

begin
  GMode := emInterpreted;
  GEmitPath := '';
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
        GEmitPath := Copy(Arg, 8, MaxInt);
      end
      else if Arg = '--emit' then
      begin
        GEmitOnly := True;
        GEmitPath := '';
      end
      else if Copy(Arg, 1, 2) <> '--' then
        Paths.Add(Arg);
    end;

    if Paths.Count = 0 then
    begin
      PrintUsage;
      ExitCode := 1;
      Exit;
    end;

    if GEmitOnly and (GEmitPath = '') then
      GEmitPath := ChangeFileExt(Paths[0], EXT_SBC);

    for I := 0 to Paths.Count - 1 do
    begin
      if I > 0 then WriteLn;
      RunScripts(Paths[I]);
    end;
  finally
    Paths.Free;
  end;
end.
