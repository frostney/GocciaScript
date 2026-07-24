program Goccia.Compiler.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  NumberBits,
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
  Goccia.SourceSpan,
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
    function CountOpRecursive(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): Integer;
    function FindFunctionWithOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): TGocciaFunctionTemplate;
    function CountArithmeticOps(
      const ATemplate: TGocciaFunctionTemplate): Integer;
    function HasLoadInt(const ATemplate: TGocciaFunctionTemplate;
      const AValue: Int16): Boolean;
    function HasLoadChar(const ATemplate: TGocciaFunctionTemplate;
      const ACodeUnit: UInt16): Boolean;
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
    procedure TestASTSpansUseUTF16CodeUnitOffsets;
    procedure TestSourceCoordinatesCoverECMAScriptLineTerminators;
    procedure TestSingleCodeUnitStringLiteralsUseImmediateOpcode;
    procedure TestCompileRegExpLiteral;
    procedure TestCompileArithmetic;
    procedure TestCompileVariable;
    procedure TestCompileFunction;
    procedure TestStaticImportLoadsScaleWithDeclarations;
    procedure TestBinaryRoundTrip;
    procedure TestBinaryRoundTripClosedNumericSelfCall;
    procedure TestBinaryRoundTripUpvalueNames;
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
    procedure TestClosedNumericFibonacciUsesSuperinstructions;
    procedure TestClosedNumericScalarSelfCallArityLimit;
    procedure TestMixedOrEscapedCallsCancelNumericProof;
    procedure TestKnownNumericLocalUsesSubtractImmediate;
    procedure TestGenericAdditionDefersToPrimitiveToOpcode;
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
    procedure TestSwitchExitJumpsCloseUpvalues;
  public
    procedure SetupTests; override;
  end;

procedure TTestCompiler.SetupTests;
begin
  Test('Compile literal', TestCompileLiteral);
  Test('AST spans use canonical UTF-16 code-unit offsets',
    TestASTSpansUseUTF16CodeUnitOffsets);
  Test('Source coordinates cover ECMAScript line terminators',
    TestSourceCoordinatesCoverECMAScriptLineTerminators);
  Test('Single-code-unit string literals use immediate opcode',
    TestSingleCodeUnitStringLiteralsUseImmediateOpcode);
  Test('Compile RegExp literal', TestCompileRegExpLiteral);
  Test('Compile arithmetic', TestCompileArithmetic);
  Test('Compile variable', TestCompileVariable);
  Test('Compile function', TestCompileFunction);
  Test('Static import loads scale with declarations',
    TestStaticImportLoadsScaleWithDeclarations);
  Test('Binary round-trip', TestBinaryRoundTrip);
  Test('Binary round-trip closed numeric self-call',
    TestBinaryRoundTripClosedNumericSelfCall);
  Test('Binary round-trip upvalue names', TestBinaryRoundTripUpvalueNames);
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
  Test('Closed numeric Fibonacci uses superinstructions',
    TestClosedNumericFibonacciUsesSuperinstructions);
  Test('Closed numeric scalar self-call supports only small arities',
    TestClosedNumericScalarSelfCallArityLimit);
  Test('Mixed or escaped calls cancel numeric proof',
    TestMixedOrEscapedCallsCancelNumericProof);
  Test('Known numeric local uses subtract immediate',
    TestKnownNumericLocalUsesSubtractImmediate);
  Test('Generic addition defers ToPrimitive to opcode',
    TestGenericAdditionDefersToPrimitiveToOpcode);
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
  Test('Switch exit jumps close upvalues', TestSwitchExitJumpsCloseUpvalues);
end;

procedure TTestCompiler.TestASTSpansUseUTF16CodeUnitOffsets;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Source, FirstStatement: string;
begin
  FirstStatement := 'const x = "' + #$D83D#$DE00 + '";';
  Source := FirstStatement + #10 + 'x;';
  Lexer := TGocciaLexer.Create(Source, '<test>');
  Parser := TGocciaParser.CreateFromLexer(Lexer, '<test>', Lexer.SourceLines);
  try
    ProgramNode := Parser.Parse;
    try
      Expect<Integer>(ProgramNode.Body[0].Span.StartOffset).ToBe(0);
      Expect<Integer>(ProgramNode.Body[1].Span.StartOffset).ToBe(
        Length(FirstStatement) + 1);
      Expect<Integer>(ProgramNode.Body[1].Line).ToBe(2);
      Expect<Integer>(ProgramNode.Body[1].Column).ToBe(1);
      Expect<Integer>(ProgramNode.Span.StartOffset).ToBe(0);
      Expect<Integer>(ProgramNode.Span.EndOffset).ToBe(Length(Source));
    finally
      ProgramNode.Free;
    end;
  finally
    Parser.Free;
    Lexer.Free;
  end;
end;

procedure TTestCompiler.TestSourceCoordinatesCoverECMAScriptLineTerminators;
var
  Column, Line: Integer;
  Coordinates: IGocciaSourceCoordinates;
  Span: TGocciaSourceSpan;
begin
  Coordinates := TGocciaSourceCoordinates.Create(
    'a'#13#10'b'#$2028'c'#$2029'd'#10'e'#13'f');

  Coordinates.PositionAtOffset(3, Line, Column);
  Expect<Integer>(Line).ToBe(2);
  Expect<Integer>(Column).ToBe(1);
  Coordinates.PositionAtOffset(5, Line, Column);
  Expect<Integer>(Line).ToBe(3);
  Expect<Integer>(Column).ToBe(1);
  Coordinates.PositionAtOffset(7, Line, Column);
  Expect<Integer>(Line).ToBe(4);
  Expect<Integer>(Column).ToBe(1);
  Coordinates.PositionAtOffset(9, Line, Column);
  Expect<Integer>(Line).ToBe(5);
  Expect<Integer>(Column).ToBe(1);
  Coordinates.PositionAtOffset(11, Line, Column);
  Expect<Integer>(Line).ToBe(6);
  Expect<Integer>(Column).ToBe(1);

  Expect<Integer>(Coordinates.OffsetAtPosition(3, 1)).ToBe(5);
  Span := TGocciaSourceSpan.InSource(Coordinates, 3, 6);
  Expect<Integer>(Span.StartLine).ToBe(2);
  Expect<Integer>(Span.StartColumn).ToBe(1);
  Expect<Integer>(Span.EndLine).ToBe(3);
  Expect<Integer>(Span.EndColumn).ToBe(1);
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
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  SourceLines: TStringList;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Lexer := TGocciaLexer.Create(ASource, '<test>');
  SourceLines := CreateTextLines(ASource);
  Parser := TGocciaParser.CreateFromLexer(Lexer, '<test>', SourceLines);
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

function TTestCompiler.CountOpRecursive(
  const ATemplate: TGocciaFunctionTemplate;
  const AOp: TGocciaOpCode): Integer;
var
  I: Integer;
begin
  Result := CountOp(ATemplate, AOp);
  for I := 0 to ATemplate.FunctionCount - 1 do
    Inc(Result, CountOpRecursive(ATemplate.GetFunction(I), AOp));
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

procedure TTestCompiler.TestStaticImportLoadsScaleWithDeclarations;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'import { value } from "./dependency.js";' +
    'import { other } from "./other-dependency.js";' +
    'value; value; value; other; other;' +
    'const read = () => value + value + value + value;' +
    'read();');
  try
    // The declaration retains one module namespace. Identifier reads only
    // dereference the live binding, including reads captured by a closure.
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_IMPORT)).ToBe(2);
    Expect<Integer>(CountOpRecursive(Module.TopLevel,
      OP_GET_IMPORT_BINDING)).ToBe(9);
  finally
    Module.Free;
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
  Result := Result + CountOp(ATemplate, OP_SUB_NUM_IMM);
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

function TTestCompiler.HasLoadChar(const ATemplate: TGocciaFunctionTemplate;
  const ACodeUnit: UInt16): Boolean;
var
  I: Integer;
  Instruction: UInt32;
begin
  for I := 0 to ATemplate.CodeCount - 1 do
  begin
    Instruction := ATemplate.GetInstruction(I);
    if (TGocciaOpCode(DecodeOp(Instruction)) = OP_LOAD_CHAR) and
       (DecodeBx(Instruction) = ACodeUnit) then
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
  ExpectedBits, ActualBits: UInt64;
begin
  ExpectedBits := DoubleToBits(AExpected);
  for I := 0 to ATemplate.ConstantCount - 1 do
  begin
    Constant := ATemplate.GetConstant(I);
    if Constant.Kind = bckFloat then
    begin
      ActualBits := DoubleToBits(Constant.FloatValue);
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

procedure TTestCompiler.TestSingleCodeUnitStringLiteralsUseImmediateOpcode;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const a = "A"; const b = "\u0800"; const c = "ab";');
  try
    Expect<Boolean>(HasLoadChar(Module.TopLevel, Ord('A'))).ToBe(True);
    Expect<Boolean>(HasLoadChar(Module.TopLevel, $0800)).ToBe(True);
    Expect<Integer>(CountOp(Module.TopLevel, OP_LOAD_CHAR)).ToBe(2);
    Expect<Integer>(Module.TopLevel.ConstantCount).ToBe(1);
    Expect<string>(Module.TopLevel.GetConstant(0).StringValue).ToBe('ab');
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCompileRegExpLiteral;
var
  Module: TGocciaBytecodeModule;
  Constant: TGocciaBytecodeConstant;
  I: Integer;
  FoundLiteralConstant: Boolean;
begin
  Module := CompileSource('const re = /ab+c/gi;');
  try
    Expect<Boolean>(Assigned(Module)).ToBe(True);
    Expect<Boolean>(Assigned(Module.TopLevel)).ToBe(True);
    Expect<Boolean>(Module.TopLevel.CodeCount > 0).ToBe(True);
    Expect<Integer>(CountOp(Module.TopLevel, OP_LOAD_REGEXP)).ToBe(1);

    FoundLiteralConstant := False;
    for I := 0 to Module.TopLevel.ConstantCount - 1 do
    begin
      Constant := Module.TopLevel.GetConstant(I);
      if (Constant.Kind = bckRegExpLiteral) and
         (Constant.StringValue = 'ab+c') and
         (Constant.RegExpFlags = 'gi') then
      begin
        FoundLiteralConstant := True;
        Break;
      end;
    end;
    Expect<Boolean>(FoundLiteralConstant).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCompileArithmetic;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const result = 1 + 2 * 3;',
    False, False, False, False, False, False);
  try
    Expect<Boolean>(Assigned(Module)).ToBe(True);
    Expect<Boolean>(CountArithmeticOps(Module.TopLevel) > 0).ToBe(True);
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

procedure TTestCompiler.TestBinaryRoundTripClosedNumericSelfCall;
var
  Original, Loaded: TGocciaBytecodeModule;
  LoadedFunction: TGocciaFunctionTemplate;
  TempFile: string;
begin
  Original := CompileSource(
    'const run = () => {' +
    '  const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);' +
    '  fib(10);' +
    '}; run();', False, False, False, False, False, False);
  TempFile := GetTempFileName + '.gbc';
  try
    SaveModuleToFile(Original, TempFile);
    Loaded := LoadModuleFromFile(TempFile);
    try
      LoadedFunction := FindFunctionWithOp(Loaded.TopLevel,
        OP_CALL_SELF_NUM);
      Expect<Boolean>(Assigned(LoadedFunction)).ToBe(True);
      if Assigned(LoadedFunction) then
        Expect<Integer>(CountOp(LoadedFunction, OP_CALL_SELF_NUM)).ToBe(2);
    finally
      Loaded.Free;
    end;
  finally
    Original.Free;
    DeleteFile(TempFile);
  end;
end;

procedure TTestCompiler.TestBinaryRoundTripUpvalueNames;
var
  Original, Loaded: TGocciaBytecodeModule;
  OriginalFunc, LoadedFunc: TGocciaFunctionTemplate;
  OriginalDesc, LoadedDesc: TGocciaUpvalueDescriptor;
  TempFile: string;
begin
  Original := CompileSource(
    'let x = 1; const f = () => x; x = 2;',
    False, False, False, False, False, False);
  TempFile := GetTempFileName + '.gbc';
  try
    OriginalFunc := FindFunctionWithOp(Original.TopLevel, OP_GET_UPVALUE);
    Expect<Boolean>(Assigned(OriginalFunc)).ToBe(True);
    Expect<Integer>(OriginalFunc.UpvalueCount).ToBe(1);
    OriginalDesc := OriginalFunc.GetUpvalueDescriptor(0);
    Expect<string>(OriginalDesc.Name).ToBe('x');

    SaveModuleToFile(Original, TempFile);
    Loaded := LoadModuleFromFile(TempFile);
    try
      LoadedFunc := FindFunctionWithOp(Loaded.TopLevel, OP_GET_UPVALUE);
      Expect<Boolean>(Assigned(LoadedFunc)).ToBe(True);
      Expect<Integer>(LoadedFunc.UpvalueCount).ToBe(1);
      LoadedDesc := LoadedFunc.GetUpvalueDescriptor(0);
      Expect<string>(LoadedDesc.Name).ToBe(OriginalDesc.Name);
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
    Expect<Byte>(Bytes[4]).ToBe(Byte(GOCCIA_FORMAT_VERSION));
    Expect<Byte>(Bytes[5]).ToBe(Byte(GOCCIA_FORMAT_VERSION shr 8));
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
  OrigBits, LoadBits: UInt64;
begin
  Original := CompileSource(
    'const a = 42;' +
    'const b = 3.14159265358979;' +
    'const c = "hello world";' +
    'const d = true;' +
    'const e = null;' +
    'const f = 9007199254740992;' +
    'const g = /roundtrip/im;'
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
            OrigBits := DoubleToBits(OrigConst.FloatValue);
            LoadBits := DoubleToBits(LoadConst.FloatValue);
            Expect<UInt64>(LoadBits).ToBe(OrigBits);
          end;
          bckString:
            Expect<string>(LoadConst.StringValue).ToBe(OrigConst.StringValue);
          bckRegExpLiteral:
          begin
            Expect<string>(LoadConst.StringValue).ToBe(OrigConst.StringValue);
            Expect<string>(LoadConst.RegExpFlags).ToBe(OrigConst.RegExpFlags);
          end;
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

procedure TTestCompiler.TestClosedNumericFibonacciUsesSuperinstructions;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const run = () => {' +
    '  const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);' +
    '  fib(20);' +
    '}; run();', False, False, False, False, False, False);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_SUB_NUM_IMM);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_SUB_NUM_IMM)).ToBe(2);
      Expect<Integer>(CountOp(Func, OP_JUMP_IF_NUM_NOT_LTE_IMM)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_SUB)).ToBe(0);
      Expect<Integer>(CountOp(Func, OP_LTE)).ToBe(0);
      Expect<Integer>(CountOp(Func, OP_ADD)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_CALL_SELF_NUM)).ToBe(2);
      Expect<Integer>(CountOp(Func, OP_GET_UPVALUE)).ToBe(0);
      Expect<Integer>(CountOp(Func, OP_CALL)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestClosedNumericScalarSelfCallArityLimit;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const run = () => {' +
    '  const sum = (n, acc) => n <= 0 ? acc : sum(n - 1, acc + 1) + 0;' +
    '  sum(5, 2);' +
    '}; run();', False, False, False, False, False, False);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_CALL_SELF_NUM);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_CALL_SELF_NUM)).ToBe(1);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'const run = () => {' +
    '  const sum = (n, a, b) => n <= 0 ? a + b :' +
    '    sum(n - 1, a + 1, b + 2) + 0;' +
    '  sum(5, 2, 3);' +
    '}; run();', False, False, False, False, False, False);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_CALL_SELF_NUM);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_CALL_SELF_NUM)).ToBe(1);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'const run = () => {' +
    '  const wide = (n, a, b, c) => n <= 0 ? a + b + c :' +
    '    wide(n - 1, a + 1, b + 2, c + 3) + 0;' +
    '  wide(5, 2, 3, 4);' +
    '}; run();', False, False, False, False, False, False);
  try
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_CALL_SELF_NUM) = nil).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestMixedOrEscapedCallsCancelNumericProof;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'const mixed = () => {' +
    '  const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);' +
    '  fib(20); fib("20");' +
    '}; mixed();', False, False, False, False, False, False);
  try
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_SUB_NUM_IMM) = nil).ToBe(True);
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_JUMP_IF_NUM_NOT_LTE_IMM) = nil).ToBe(True);
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_CALL_SELF_NUM) = nil).ToBe(True);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'const wrongArity = () => {' +
    '  const down = (a, b) => a <= 1 ? a : down(a - 1, b - 1);' +
    '  down(20);' +
    '}; wrongArity();', False, False, False, False, False, False);
  try
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_SUB_NUM_IMM) = nil).ToBe(True);
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_JUMP_IF_NUM_NOT_LTE_IMM) = nil).ToBe(True);
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_CALL_SELF_NUM) = nil).ToBe(True);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'const escaped = (sink) => {' +
    '  const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);' +
    '  sink(fib); fib(20);' +
    '}; escaped(() => {});', False, False, False, False, False, False);
  try
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_SUB_NUM_IMM) = nil).ToBe(True);
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_JUMP_IF_NUM_NOT_LTE_IMM) = nil).ToBe(True);
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_CALL_SELF_NUM) = nil).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestKnownNumericLocalUsesSubtractImmediate;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('let i = 2; i - 1;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB_FLOAT)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGenericAdditionDefersToPrimitiveToOpcode;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = {}; let b = {}; a + b;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_TO_PRIMITIVE)).ToBe(0);
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
    'let a = 1;' + sLineBreak +
    'const set = () => { a = "x"; };' + sLineBreak +
    'set();' + sLineBreak +
    'const b = a + 1;' + sLineBreak +
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
    Expect<Integer>(CountOp(Module.TopLevel, OP_PUSH_FINALLY_HANDLER)).ToBe(0);
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
    Expect<Integer>(CountOp(Module.TopLevel, OP_PUSH_FINALLY_HANDLER)).ToBe(1);
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
    Expect<Integer>(CountOp(Module.TopLevel, OP_PUSH_FINALLY_HANDLER)).ToBe(1);
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

procedure TTestCompiler.TestSwitchExitJumpsCloseUpvalues;
var
  Module: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  I, CloseIndex, CloseTargetJumps, TargetIndex: Integer;
  Instruction: UInt32;
begin
  Module := CompileSource(
    'let read; switch (1) { case 1: let value = "switch"; read = () => value; break; }',
    False, False, False, False, False, False);
  try
    Template := Module.TopLevel;
    CloseIndex := -1;
    for I := 0 to Template.CodeCount - 1 do
    begin
      Instruction := Template.GetInstruction(I);
      if TGocciaOpCode(DecodeOp(Instruction)) = OP_CLOSE_UPVALUE then
      begin
        CloseIndex := I;
        Break;
      end;
    end;

    Expect<Boolean>(CloseIndex >= 0).ToBe(True);

    CloseTargetJumps := 0;
    for I := 0 to Template.CodeCount - 1 do
    begin
      Instruction := Template.GetInstruction(I);
      if TGocciaOpCode(DecodeOp(Instruction)) <> OP_JUMP then
        Continue;

      TargetIndex := I + 1 + DecodeAx(Instruction);
      if TargetIndex = CloseIndex then
        Inc(CloseTargetJumps);
    end;

    Expect<Boolean>(CloseTargetJumps >= 2).ToBe(True);
  finally
    Module.Free;
  end;
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestCompiler.Create('GocciaScript Compiler'));
    RunGocciaTests;

    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
