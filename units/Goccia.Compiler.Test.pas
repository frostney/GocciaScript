program Goccia.Compiler.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Souffle.Bytecode,
  Souffle.Bytecode.Binary,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,
  Souffle.Bytecode.Module,
  TestRunner,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Compiler,
  Goccia.Compiler.Scope,
  Goccia.GarbageCollector,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  TTestCompiler = class(TTestSuite)
  private
    function CompileSource(const ASource: string): TSouffleBytecodeModule;

    procedure TestCompileLiteral;
    procedure TestCompileArithmetic;
    procedure TestCompileVariable;
    procedure TestCompileFunction;
    procedure TestBinaryRoundTrip;
  public
    procedure SetupTests; override;
  end;

procedure TTestCompiler.SetupTests;
begin
  Test('Compile literal', TestCompileLiteral);
  Test('Compile arithmetic', TestCompileArithmetic);
  Test('Compile variable', TestCompileVariable);
  Test('Compile function', TestCompileFunction);
  Test('Binary round-trip', TestBinaryRoundTrip);
end;

function TTestCompiler.CompileSource(
  const ASource: string): TSouffleBytecodeModule;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  SourceLines: TStringList;
begin
  Lexer := TGocciaLexer.Create(ASource, '<test>');
  Tokens := Lexer.ScanTokens;
  SourceLines := TStringList.Create;
  SourceLines.Text := ASource;
  Parser := TGocciaParser.Create(Tokens, '<test>', SourceLines);
  ProgramNode := Parser.Parse;

  Compiler := TGocciaCompiler.Create('<test>');
  try
    Result := Compiler.Compile(ProgramNode);
  finally
    Compiler.Free;
    ProgramNode.Free;
    Parser.Free;
    SourceLines.Free;
    Lexer.Free;
  end;
end;

procedure TTestCompiler.TestCompileLiteral;
var
  Module: TSouffleBytecodeModule;
begin
  Module := CompileSource('const x = 42;');
  try
    Expect<Boolean>(Assigned(Module)).ToBe(True);
    Expect<Boolean>(Assigned(Module.TopLevel)).ToBe(True);
    Expect<Boolean>(Module.TopLevel.CodeCount > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCompileArithmetic;
var
  Module: TSouffleBytecodeModule;
begin
  Module := CompileSource('const result = 1 + 2 * 3;');
  try
    Expect<Boolean>(Assigned(Module)).ToBe(True);
    Expect<Boolean>(Module.TopLevel.CodeCount > 3).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCompileVariable;
var
  Module: TSouffleBytecodeModule;
begin
  Module := CompileSource('let a = 10; let b = a;');
  try
    Expect<Boolean>(Assigned(Module)).ToBe(True);
    Expect<Boolean>(Module.TopLevel.CodeCount > 2).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCompileFunction;
var
  Module: TSouffleBytecodeModule;
begin
  Module := CompileSource('const add = (a, b) => a + b;');
  try
    Expect<Boolean>(Assigned(Module)).ToBe(True);
    Expect<Boolean>(Module.TopLevel.FunctionCount > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestBinaryRoundTrip;
var
  Original, Loaded: TSouffleBytecodeModule;
  TempFile: string;
begin
  Original := CompileSource('const x = 42; const y = "hello";');
  TempFile := GetTempFileName + '.sbc';
  try
    SaveModuleToFile(Original, TempFile);
    Loaded := LoadModuleFromFile(TempFile);
    try
      Expect<string>(Loaded.RuntimeTag).ToBe(Original.RuntimeTag);
      Expect<string>(Loaded.SourcePath).ToBe(Original.SourcePath);
      Expect<Integer>(Loaded.TopLevel.CodeCount).ToBe(Original.TopLevel.CodeCount);
      Expect<Integer>(Loaded.TopLevel.ConstantCount).ToBe(Original.TopLevel.ConstantCount);
      Expect<Integer>(Loaded.TopLevel.FunctionCount).ToBe(Original.TopLevel.FunctionCount);
    finally
      Loaded.Free;
    end;
  finally
    Original.Free;
    DeleteFile(TempFile);
  end;
end;

begin
  TGocciaGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestCompiler.Create('GocciaScript Compiler'));
    TestRunnerProgram.Run;

    ExitCode := TestResultToExitCode;
  finally
    TGocciaGarbageCollector.Shutdown;
  end;
end.
