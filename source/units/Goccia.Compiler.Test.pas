program Goccia.Compiler.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  Math,
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
  Goccia.Compiler.ConstantValue,
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
    function CompileSource(const ASource: string;
      const AStrictTypes: Boolean = False;
      const APreserveCoverageShape: Boolean = False;
      const AGlobalBackedTopLevel: Boolean = False;
      const AEnableConstantFolding: Boolean = True;
      const AEnableConstPropagation: Boolean = True;
      const AEnableDeadBranchElimination: Boolean = True): TGocciaBytecodeModule;
    function CountOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): Integer;
    function FindFunctionWithOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): TGocciaFunctionTemplate;
    function CountArithmeticOps(
      const ATemplate: TGocciaFunctionTemplate): Integer;
    function HasLoadInt(const ATemplate: TGocciaFunctionTemplate;
      const AValue: Int16): Boolean;
    function HasNaNFloatConstant(
      const ATemplate: TGocciaFunctionTemplate): Boolean;
    function HasInfinityFloatConstant(
      const ATemplate: TGocciaFunctionTemplate;
      const APositive: Boolean): Boolean;
    function HasFloatConstantBits(
      const ATemplate: TGocciaFunctionTemplate;
      const AExpected: Double): Boolean;
    function HasBigIntConstant(const ATemplate: TGocciaFunctionTemplate;
      const AExpected: string): Boolean;
    function NegativeZero: Double;

    procedure TestCompileLiteral;
    procedure TestCompileRegExpLiteral;
    procedure TestCompileArithmetic;
    procedure TestCompileVariable;
    procedure TestCompileFunction;
    procedure TestBinaryRoundTrip;
    procedure TestBinaryLittleEndian;
    procedure TestBinaryRoundTripConstants;
    procedure TestUndeclaredPrivateNameRaisesSyntaxError;
    procedure TestConstantFoldsNestedArithmetic;
    procedure TestConstantFoldsBigInt;
    procedure TestConstantFoldsSpecialNumbers;
    procedure TestConstPropagation;
    procedure TestConstPropagationBigInt;
    procedure TestConstPropagationSkipsMutable;
    procedure TestConstPropagationSkipsGlobalBacked;
    procedure TestInferredNumericLocalsUseTypedArithmetic;
    procedure TestAnnotatedParametersUseTypedArithmetic;
    procedure TestAssignmentClearsStaleNumericHint;
    procedure TestGlobalBackedAssignmentClearsStaleNumericHint;
    procedure TestShortCircuitAssignmentClearsStaleNumericHint;
    procedure TestGlobalBackedShortCircuitClearsStaleNumericHint;
    procedure TestGlobalBackedCompoundClearsStaleNumericHint;
    procedure TestGlobalBackedAssignmentChecksStrictType;
    procedure TestGlobalBackedShortCircuitChecksStrictType;
    procedure TestGlobalBackedCompoundChecksStrictType;
    procedure TestCapturedNumericLocalAvoidsTypedArithmetic;
    procedure TestForOfSkipsHandlerWithoutAbruptClose;
    procedure TestForOfUsesHandlerForExpressionBody;
    procedure TestForOfUsesOneIteratorCloseHandler;
    procedure TestConstantIfEliminatesBranch;
    procedure TestConstantIfPrunesAbruptTail;
    procedure TestCoveragePreservesConstantBranch;
    procedure TestConstantEvaluationOptionsAreIndependent;
    procedure TestStrictTypeSimplificationRequiresStrictTypes;
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
  Test('Constant folds nested arithmetic', TestConstantFoldsNestedArithmetic);
  Test('Constant folds BigInt', TestConstantFoldsBigInt);
  Test('Constant folds special numbers', TestConstantFoldsSpecialNumbers);
  Test('Const propagation', TestConstPropagation);
  Test('Const propagation with BigInt', TestConstPropagationBigInt);
  Test('Const propagation skips mutable bindings', TestConstPropagationSkipsMutable);
  Test('Const propagation skips global-backed bindings', TestConstPropagationSkipsGlobalBacked);
  Test('Inferred numeric locals use typed arithmetic', TestInferredNumericLocalsUseTypedArithmetic);
  Test('Annotated parameters use typed arithmetic', TestAnnotatedParametersUseTypedArithmetic);
  Test('Assignment clears stale numeric hint', TestAssignmentClearsStaleNumericHint);
  Test('Global-backed assignment clears stale numeric hint', TestGlobalBackedAssignmentClearsStaleNumericHint);
  Test('Short-circuit assignment clears stale numeric hint', TestShortCircuitAssignmentClearsStaleNumericHint);
  Test('Global-backed short-circuit clears stale numeric hint', TestGlobalBackedShortCircuitClearsStaleNumericHint);
  Test('Global-backed compound clears stale numeric hint', TestGlobalBackedCompoundClearsStaleNumericHint);
  Test('Global-backed assignment checks strict type', TestGlobalBackedAssignmentChecksStrictType);
  Test('Global-backed short-circuit checks strict type', TestGlobalBackedShortCircuitChecksStrictType);
  Test('Global-backed compound checks strict type', TestGlobalBackedCompoundChecksStrictType);
  Test('Captured numeric local avoids typed arithmetic', TestCapturedNumericLocalAvoidsTypedArithmetic);
  Test('for-of skips handler without abrupt close', TestForOfSkipsHandlerWithoutAbruptClose);
  Test('for-of uses handler for expression body', TestForOfUsesHandlerForExpressionBody);
  Test('for-of uses one iterator-close handler', TestForOfUsesOneIteratorCloseHandler);
  Test('Constant if eliminates branch', TestConstantIfEliminatesBranch);
  Test('Constant if prunes abrupt tail', TestConstantIfPrunesAbruptTail);
  Test('Coverage preserves constant branch shape', TestCoveragePreservesConstantBranch);
  Test('Constant evaluation options are independent', TestConstantEvaluationOptionsAreIndependent);
  Test('Strict type simplification requires strict-types', TestStrictTypeSimplificationRequiresStrictTypes);
end;

function TTestCompiler.CompileSource(
  const ASource: string; const AStrictTypes: Boolean;
  const APreserveCoverageShape: Boolean;
  const AGlobalBackedTopLevel: Boolean;
  const AEnableConstantFolding: Boolean;
  const AEnableConstPropagation: Boolean;
  const AEnableDeadBranchElimination: Boolean): TGocciaBytecodeModule;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  SourceLines: TStringList;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Lexer := TGocciaLexer.Create(ASource, '<test>');
  Tokens := Lexer.ScanTokens;
  SourceLines := CreateUTF8StringList(ASource);
  Parser := TGocciaParser.Create(Tokens, '<test>', SourceLines);
  ProgramNode := Parser.Parse;

  Compiler := TGocciaCompiler.Create('<test>');
  try
    Compiler.StrictTypes := AStrictTypes;
    Compiler.GlobalBackedTopLevel := AGlobalBackedTopLevel;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape := APreserveCoverageShape;
    Options.EnableConstantFolding := AEnableConstantFolding;
    Options.EnableConstPropagation := AEnableConstPropagation;
    Options.EnableDeadBranchElimination := AEnableDeadBranchElimination;
    Compiler.OptimizationOptions := Options;
    Result := Compiler.Compile(ProgramNode);
  finally
    Compiler.Free;
    ProgramNode.Free;
    Parser.Free;
    SourceLines.Free;
    Lexer.Free;
  end;
end;

function TTestCompiler.CountOp(const ATemplate: TGocciaFunctionTemplate;
  const AOp: TGocciaOpCode): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ATemplate.CodeCount - 1 do
    if TGocciaOpCode(DecodeOp(ATemplate.GetInstruction(I))) = AOp then
      Inc(Result);
end;

function TTestCompiler.FindFunctionWithOp(
  const ATemplate: TGocciaFunctionTemplate;
  const AOp: TGocciaOpCode): TGocciaFunctionTemplate;
var
  I: Integer;
  Candidate: TGocciaFunctionTemplate;
begin
  Result := nil;
  if CountOp(ATemplate, AOp) > 0 then
    Exit(ATemplate);

  for I := 0 to ATemplate.FunctionCount - 1 do
  begin
    Candidate := ATemplate.GetFunction(I);
    Result := FindFunctionWithOp(Candidate, AOp);
    if Assigned(Result) then
      Exit(Result);
  end;
end;

function TTestCompiler.CountArithmeticOps(
  const ATemplate: TGocciaFunctionTemplate): Integer;
begin
  Result :=
    CountOp(ATemplate, OP_ADD) +
    CountOp(ATemplate, OP_SUB) +
    CountOp(ATemplate, OP_MUL) +
    CountOp(ATemplate, OP_DIV) +
    CountOp(ATemplate, OP_MOD) +
    CountOp(ATemplate, OP_POW) +
    CountOp(ATemplate, OP_ADD_INT) +
    CountOp(ATemplate, OP_SUB_INT) +
    CountOp(ATemplate, OP_MUL_INT) +
    CountOp(ATemplate, OP_DIV_INT) +
    CountOp(ATemplate, OP_MOD_INT) +
    CountOp(ATemplate, OP_ADD_FLOAT) +
    CountOp(ATemplate, OP_SUB_FLOAT) +
    CountOp(ATemplate, OP_MUL_FLOAT) +
    CountOp(ATemplate, OP_DIV_FLOAT) +
    CountOp(ATemplate, OP_MOD_FLOAT);
end;

function TTestCompiler.HasLoadInt(const ATemplate: TGocciaFunctionTemplate;
  const AValue: Int16): Boolean;
var
  I: Integer;
  Instruction: UInt32;
begin
  for I := 0 to ATemplate.CodeCount - 1 do
  begin
    Instruction := ATemplate.GetInstruction(I);
    if (TGocciaOpCode(DecodeOp(Instruction)) = OP_LOAD_INT) and
       (DecodesBx(Instruction) = AValue) then
      Exit(True);
  end;
  Result := False;
end;

function TTestCompiler.HasNaNFloatConstant(
  const ATemplate: TGocciaFunctionTemplate): Boolean;
var
  I: Integer;
  Constant: TGocciaBytecodeConstant;
begin
  for I := 0 to ATemplate.ConstantCount - 1 do
  begin
    Constant := ATemplate.GetConstant(I);
    if (Constant.Kind = bckFloat) and IsNaN(Constant.FloatValue) then
      Exit(True);
  end;
  Result := False;
end;

function TTestCompiler.HasInfinityFloatConstant(
  const ATemplate: TGocciaFunctionTemplate;
  const APositive: Boolean): Boolean;
var
  I: Integer;
  Constant: TGocciaBytecodeConstant;
begin
  for I := 0 to ATemplate.ConstantCount - 1 do
  begin
    Constant := ATemplate.GetConstant(I);
    if (Constant.Kind = bckFloat) and IsInfinite(Constant.FloatValue) and
       ((Constant.FloatValue > 0) = APositive) then
      Exit(True);
  end;
  Result := False;
end;

function TTestCompiler.HasFloatConstantBits(
  const ATemplate: TGocciaFunctionTemplate;
  const AExpected: Double): Boolean;
var
  I: Integer;
  Constant: TGocciaBytecodeConstant;
  ExpectedBits, ActualBits: Int64;
begin
  Move(AExpected, ExpectedBits, SizeOf(Double));
  for I := 0 to ATemplate.ConstantCount - 1 do
  begin
    Constant := ATemplate.GetConstant(I);
    if Constant.Kind = bckFloat then
    begin
      Move(Constant.FloatValue, ActualBits, SizeOf(Double));
      if ActualBits = ExpectedBits then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TTestCompiler.HasBigIntConstant(
  const ATemplate: TGocciaFunctionTemplate;
  const AExpected: string): Boolean;
var
  I: Integer;
  Constant: TGocciaBytecodeConstant;
begin
  for I := 0 to ATemplate.ConstantCount - 1 do
  begin
    Constant := ATemplate.GetConstant(I);
    if (Constant.Kind = bckBigInt) and (Constant.StringValue = AExpected) then
      Exit(True);
  end;
  Result := False;
end;

function TTestCompiler.NegativeZero: Double;
begin
  Result := 0.0;
  Result := Result * -1.0;
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

procedure TTestCompiler.TestConstantFoldsNestedArithmetic;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const result = (1 + 2) * (3 + 4); result;');
  try
    Expect<Integer>(CountArithmeticOps(Module.TopLevel)).ToBe(0);
    Expect<Boolean>(HasLoadInt(Module.TopLevel, 21)).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstantFoldsBigInt;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('1n + 2n;');
  try
    Expect<Integer>(CountArithmeticOps(Module.TopLevel)).ToBe(0);
    Expect<Boolean>(HasBigIntConstant(Module.TopLevel, '3')).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstantFoldsSpecialNumbers;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('1 / 0;');
  try
    Expect<Boolean>(HasInfinityFloatConstant(Module.TopLevel, True)).ToBe(True);
  finally
    Module.Free;
  end;

  Module := CompileSource('(-Infinity) ** 0.5;');
  try
    Expect<Boolean>(HasInfinityFloatConstant(Module.TopLevel, True)).ToBe(True);
  finally
    Module.Free;
  end;

  Module := CompileSource('0 / 0;');
  try
    Expect<Boolean>(HasNaNFloatConstant(Module.TopLevel)).ToBe(True);
  finally
    Module.Free;
  end;

  Module := CompileSource('(-4) % 2;');
  try
    Expect<Boolean>(HasFloatConstantBits(Module.TopLevel, NegativeZero)).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagation;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const a = 2 + 3; const b = a * 4; b;');
  try
    Expect<Integer>(CountArithmeticOps(Module.TopLevel)).ToBe(0);
    Expect<Boolean>(HasLoadInt(Module.TopLevel, 20)).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationBigInt;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const a = 1n; const b = a + 2n; b;');
  try
    Expect<Integer>(CountArithmeticOps(Module.TopLevel)).ToBe(0);
    Expect<Boolean>(HasBigIntConstant(Module.TopLevel, '3')).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationSkipsMutable;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('let a = 2; const b = a * 4; b;');
  try
    Expect<Boolean>(CountArithmeticOps(Module.TopLevel) > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationSkipsGlobalBacked;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const a = 2; const b = a * 4; b;',
    False, False, True);
  try
    Expect<Boolean>(CountArithmeticOps(Module.TopLevel) > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestInferredNumericLocalsUseTypedArithmetic;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; let b = 2; let c = a + b; c;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestAnnotatedParametersUseTypedArithmetic;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const add = (a: number, b: number): number => a + b; add(1, 2);',
    True, False, False, False, False, False);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_ADD_FLOAT);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_ADD_FLOAT)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_ADD)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestAssignmentClearsStaleNumericHint;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; a = "x"; const b = a + 1; b;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedAssignmentClearsStaleNumericHint;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; a = "x"; const b = a + 1; b;',
    False, False, True, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestShortCircuitAssignmentClearsStaleNumericHint;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; a ||= "x"; const b = a + 1; b;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedShortCircuitClearsStaleNumericHint;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; a ||= "x"; const b = a + 1; b;',
    False, False, True, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedCompoundClearsStaleNumericHint;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; a += "x"; const b = a + 1; b;',
    False, False, True, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(2);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedAssignmentChecksStrictType;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a: number = 1; a = "x"; a;',
    True, False, True, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedShortCircuitChecksStrictType;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a: number = 0; a ||= "x"; a;',
    True, False, True, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedCompoundChecksStrictType;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a: number = 1; a += "x"; a;',
    True, False, True, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCapturedNumericLocalAvoidsTypedArithmetic;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1;' + LineEnding +
    'const set = () => { a = "x"; };' + LineEnding +
    'set();' + LineEnding +
    'const b = a + 1;' + LineEnding +
    'b;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestForOfSkipsHandlerWithoutAbruptClose;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'for (const x of [1, 2, 3]) { }',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_PUSH_HANDLER)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_POP_HANDLER)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestForOfUsesHandlerForExpressionBody;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let sum = 0; for (const x of [1, 2, 3]) { sum = sum + x; } sum;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_PUSH_HANDLER)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_POP_HANDLER)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestForOfUsesOneIteratorCloseHandler;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'for (const x of [1, 2, 3]) { throw x; }',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_PUSH_HANDLER)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_POP_HANDLER)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstantIfEliminatesBranch;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'if (false) { const x = 1 + 2; } else { const y = 3; }');
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP_IF_FALSE)).ToBe(0);
    Expect<Integer>(CountArithmeticOps(Module.TopLevel)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstantIfPrunesAbruptTail;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'if (true) { throw 1; } const unreachable = 2 + 3; unreachable;');
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_THROW)).ToBe(1);
    Expect<Boolean>(HasLoadInt(Module.TopLevel, 5)).ToBe(False);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    '{ if (true) { throw 1; } const unreachable = 2 + 3; }');
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_THROW)).ToBe(1);
    Expect<Boolean>(HasLoadInt(Module.TopLevel, 5)).ToBe(False);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCoveragePreservesConstantBranch;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'if (false) { const x = 1 + 2; } else { const y = 3; }',
    False, True);
  try
    Expect<Boolean>(CountOp(Module.TopLevel, OP_JUMP_IF_FALSE) > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstantEvaluationOptionsAreIndependent;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const a = 2; a;',
    False, False, False, False, True, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_GET_LOCAL)).ToBe(0);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'if (false) { const x = 1; } else { const y = 2; }',
    False, False, False, False, False, True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP_IF_FALSE)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestStrictTypeSimplificationRequiresStrictTypes;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('let b: boolean = true; !!b;', False);
  try
    Expect<Boolean>(CountOp(Module.TopLevel, OP_NOT) > 0).ToBe(True);
  finally
    Module.Free;
  end;

  Module := CompileSource('let b: boolean = true; !!b;', True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_NOT)).ToBe(0);
  finally
    Module.Free;
  end;
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
