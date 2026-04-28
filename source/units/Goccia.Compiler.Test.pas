program Goccia.Compiler.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TestingPascalLibrary,
  TextSemantics,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Debug,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Compiler.Scope,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.TestSetup,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  TTestCompiler = class(TTestSuite)
  private
    function CompileSource(const ASource: string): TGocciaBytecodeModule;

    procedure TestCompileLiteral;
    procedure TestCompileRegExpLiteral;
    procedure TestCompileArithmetic;
    procedure TestCompileVariable;
    procedure TestCompileFunction;
    procedure TestBinaryRoundTrip;
    procedure TestBinaryLittleEndian;
    procedure TestBinaryRoundTripConstants;
    procedure TestUndeclaredPrivateNameRaisesSyntaxError;
  public
    procedure SetupTests; override;
  end;

procedure TTestCompiler.SetupTests;
begin
  Test('Compile literal', TestCompileLiteral);
  Test('Compile RegExp literal', TestCompileRegExpLiteral);
  Test('Compile arithmetic', TestCompileArithmetic);
  Test('Compile variable', TestCompileVariable);
  Test('Compile function', TestCompileFunction);
  Test('Binary round-trip', TestBinaryRoundTrip);
  Test('Binary little-endian format', TestBinaryLittleEndian);
  Test('Binary round-trip constants', TestBinaryRoundTripConstants);
  Test('Undeclared private name raises SyntaxError', TestUndeclaredPrivateNameRaisesSyntaxError);
end;

function TTestCompiler.CompileSource(
  const ASource: string): TGocciaBytecodeModule;
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
  SourceLines := CreateUTF8StringList(ASource);
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
  Module: TGocciaBytecodeModule;
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

procedure TTestCompiler.TestCompileRegExpLiteral;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const re = /ab+c/gi;');
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
  Module: TGocciaBytecodeModule;
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
  Module: TGocciaBytecodeModule;
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
  Module: TGocciaBytecodeModule;
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
  Original, Loaded: TGocciaBytecodeModule;
  TempFile: string;
begin
  Original := CompileSource('const x = 42; const y = "hello";');
  TempFile := GetTempFileName + '.gbc';
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

procedure TTestCompiler.TestBinaryLittleEndian;
var
  Module: TGocciaBytecodeModule;
  Stream: TMemoryStream;
  Writer: TGocciaBytecodeWriter;
  Bytes: PByte;
  VersionLE: UInt16;
begin
  Module := CompileSource('const x = 42;');
  Stream := TMemoryStream.Create;
  try
    Writer := TGocciaBytecodeWriter.Create(Stream);
    try
      Writer.WriteModule(Module);
    finally
      Writer.Free;
    end;

    Bytes := Stream.Memory;
    Expect<Byte>(Bytes[0]).ToBe(Ord('G'));
    Expect<Byte>(Bytes[1]).ToBe(Ord('B'));
    Expect<Byte>(Bytes[2]).ToBe(Ord('C'));
    Expect<Byte>(Bytes[3]).ToBe(0);

    // Format version at offset 4 must be little-endian
    Move(Bytes[4], VersionLE, 2);
    Expect<UInt16>(VersionLE).ToBe(NtoLE(GOCCIA_FORMAT_VERSION));
  finally
    Stream.Free;
    Module.Free;
  end;
end;

procedure TTestCompiler.TestBinaryRoundTripConstants;
var
  Original, Loaded: TGocciaBytecodeModule;
  TempFile: string;
  I: Integer;
  OrigConst, LoadConst: TGocciaBytecodeConstant;
  OrigBits, LoadBits: Int64;
begin
  Original := CompileSource(
    'const a = 42;' +
    'const b = 3.14159265358979;' +
    'const c = "hello world";' +
    'const d = true;' +
    'const e = null;' +
    'const f = 9007199254740992;'
  );
  TempFile := GetTempFileName + '.gbc';
  try
    SaveModuleToFile(Original, TempFile);
    Loaded := LoadModuleFromFile(TempFile);
    try
      Expect<Integer>(Loaded.TopLevel.ConstantCount).ToBe(
        Original.TopLevel.ConstantCount);

      for I := 0 to Original.TopLevel.ConstantCount - 1 do
      begin
        OrigConst := Original.TopLevel.GetConstant(I);
        LoadConst := Loaded.TopLevel.GetConstant(I);
        Expect<Integer>(Ord(LoadConst.Kind)).ToBe(Ord(OrigConst.Kind));

        case OrigConst.Kind of
          bckInteger:
            Expect<Int64>(LoadConst.IntValue).ToBe(OrigConst.IntValue);
          bckFloat:
          begin
            Move(OrigConst.FloatValue, OrigBits, SizeOf(Double));
            Move(LoadConst.FloatValue, LoadBits, SizeOf(Double));
            Expect<Int64>(LoadBits).ToBe(OrigBits);
          end;
          bckString:
            Expect<string>(LoadConst.StringValue).ToBe(OrigConst.StringValue);
        end;
      end;

      for I := 0 to Original.TopLevel.CodeCount - 1 do
        Expect<UInt32>(Loaded.TopLevel.GetInstruction(I)).ToBe(
          Original.TopLevel.GetInstruction(I));
    finally
      Loaded.Free;
    end;
  finally
    Original.Free;
    DeleteFile(TempFile);
  end;
end;

procedure TTestCompiler.TestUndeclaredPrivateNameRaisesSyntaxError;
var
  Raised: Boolean;
  ContainsMessage: Boolean;
begin
  Raised := False;
  ContainsMessage := False;
  try
    CompileSource(
      'class Box {' +
      '  getValue() {' +
      '    return this.#missing;' +
      '  }' +
      '}' +
      'new Box();'
    ).Free;
  except
    on E: TGocciaSyntaxError do
    begin
      Raised := True;
      ContainsMessage := Pos('must be declared in an enclosing class', E.Message) > 0;
    end;
  end;

  Expect<Boolean>(Raised).ToBe(True);
  Expect<Boolean>(ContainsMessage).ToBe(True);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestCompiler.Create('GocciaScript Compiler'));
    TestRunnerProgram.Run;

    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
