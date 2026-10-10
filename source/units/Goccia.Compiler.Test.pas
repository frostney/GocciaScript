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
      const AEnableDeadBranchElimination: Boolean = True;
      const ATraditionalForLoops: Boolean = False;
      const ANonStrictMode: Boolean = False;
      const AVarDeclarations: Boolean = False;
      const ADirectEvalAvailable: Boolean = False): TGocciaBytecodeModule;
    function CountOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): Integer;
    function CountOpRecursive(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): Integer;
    function FindFunctionWithOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): TGocciaFunctionTemplate;
    // Counts a raw opcode byte, for the runtime-only opcodes that are not
    // TGocciaOpCode members.
    function CountRawOpRecursive(const ATemplate: TGocciaFunctionTemplate;
      const AOp: UInt8): Integer;
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
    procedure TestThisPropertyReadUsesLocalRegister;
    procedure TestThisPropertyReadRetainsDerivedGuard;
    procedure TestLocalPropertyReadUsesFusedOpcode;
    procedure TestOptionalLocalPropertyReadSkipsFusedOpcode;
    procedure TestInitializedConstOperandsSkipGetLocal;
    procedure TestConstOperandsBeforeDeclarationKeepGetLocal;
    procedure TestSwitchClauseForgetsInitializedConsts;
    procedure TestCoverageKeepsConstOperandCopies;
    procedure TestParameterOperandsSkipGetLocal;
    procedure TestDirectEvalHostKeepsOperandCopies;
    procedure TestDirectEvalInFunctionKeepsLaterOperandCopies;
    procedure TestMethodParameterOperandsSkipGetLocal;
    procedure TestLetOperandsSkipGetLocal;
    procedure TestLetOperandBeforeDeclarationKeepsGetLocal;
    procedure TestOperandRebindingKeepsGetLocal;
    procedure TestCapturedLetOperandKeepsGetLocal;
    procedure TestLoopThatCreatesClosureKeepsGetLocal;
    procedure TestSwitchClauseForgetsInitializedLets;
    procedure TestDefaultParameterValueKeepsGetLocal;
    procedure TestDefaultParameterValueReachingItsBindingUsesTemporary;
    procedure TestParameterExpressionBodyVarEnvironment;
    procedure TestOperandThatIsItsOwnDestinationKeepsGetLocal;
    procedure TestCountedForVariableOperandSkipsGetLocal;
    procedure TestCoverageKeepsLetAndParameterOperandCopies;
    procedure TestAssignmentToInitializedLetSkipsProbe;
    procedure TestCompoundAssignmentReadsInitializedLetDirectly;
    procedure TestNumericImmediateConditionReadsParameterDirectly;
    procedure TestDiscardedStoreSkipsResultMove;
    procedure TestStaticImportLoadsScaleWithDeclarations;
    procedure TestBinaryRoundTrip;
    procedure TestBinaryRoundTripClosedNumericSelfCall;
    procedure TestBinaryRoundTripUpvalueNames;
    procedure TestBinaryRoundTripFunctionDeclarationPosition;
    procedure TestBinaryLittleEndian;
    procedure TestBinaryRoundTripConstants;
    procedure TestBinaryRejectsMalformedArtifacts;
    procedure TestUndeclaredPrivateNameRaisesSyntaxError;
    procedure TestConstantFoldsNestedArithmetic;
    procedure TestConstantFoldsBigInt;
    procedure TestConstantFoldsSpecialNumbers;
    procedure TestConstPropagation;
    procedure TestConstPropagationBigInt;
    procedure TestConstPropagationSkipsMutable;
    procedure TestConstPropagationReachesGlobalBackedReads;
    procedure TestConstPropagationSkipsGlobalBackedReadsCompiledEarlier;
    procedure TestConstPropagationSkipsGlobalBackedInNonStrictMode;
    procedure TestConstPropagationSkipsGlobalBackedBigInt;
    procedure TestInferredNumericLocalsUseTypedArithmetic;
    procedure TestAnnotatedParametersUseTypedArithmetic;
    procedure TestClosedNumericFibonacciUsesSuperinstructions;
    procedure TestClosedNumericScalarSelfCallArityLimit;
    procedure TestMixedOrEscapedCallsCancelNumericProof;
    procedure TestKnownNumericLocalUsesSubtractImmediate;
    procedure TestKnownNumericLocalUsesAddImmediate;
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
    procedure TestReassignedLocalKeepsTypeOnlyWhenEveryValueIsNumber;
    procedure TestReturnAnnotationDoesNotTypeCallResult;
    procedure TestForOfSkipsHandlerWithoutAbruptClose;
    procedure TestForOfUsesHandlerForExpressionBody;
    procedure TestForOfUsesOneIteratorCloseHandler;
    procedure TestCountedForLessThanUsesJumpIfNotLt;
    procedure TestConstSelfIncrementUsesGenericAssignment;
    procedure TestGlobalBackedCountedForLimitIsNotSnapshotted;
    procedure TestIfAndConditionalLessThanUseJumpIfNotLt;
    procedure TestLessThanValueKeepsGenericCompare;
    procedure TestConstantIfEliminatesBranch;
    procedure TestConstantIfPrunesAbruptTail;
    procedure TestCoveragePreservesConstantBranch;
    procedure TestCoveragePreservesGlobalBackedConstantBranch;
    procedure TestConstantEvaluationOptionsAreIndependent;
    procedure TestStrictTypeSimplificationRequiresStrictTypes;
    procedure TestStrictTypesHintOnlyEnforcedLocals;
    procedure TestStrictVarRedeclarationInCatchKeepsTypeOnVar;
    procedure TestSwitchExitJumpsCloseUpvalues;
    procedure TestBodyVarsStartUndefined;
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
  Test('Initialized const operands skip OP_GET_LOCAL',
    TestInitializedConstOperandsSkipGetLocal);
  Test('Const operands before the declaration keep OP_GET_LOCAL',
    TestConstOperandsBeforeDeclarationKeepGetLocal);
  Test('Switch clause forgets initialized consts',
    TestSwitchClauseForgetsInitializedConsts);
  Test('Coverage keeps const operand copies',
    TestCoverageKeepsConstOperandCopies);
  Test('Parameter operands skip OP_GET_LOCAL',
    TestParameterOperandsSkipGetLocal);
  Test('A host with direct eval keeps the copy of let and parameter operands',
    TestDirectEvalHostKeepsOperandCopies);
  Test('A function keeps operand copies from its first direct eval onwards',
    TestDirectEvalInFunctionKeepsLaterOperandCopies);
  Test('Method parameter operands skip OP_GET_LOCAL',
    TestMethodParameterOperandsSkipGetLocal);
  Test('Initialized let operands skip OP_GET_LOCAL',
    TestLetOperandsSkipGetLocal);
  Test('Let operands before the declaration keep OP_GET_LOCAL',
    TestLetOperandBeforeDeclarationKeepsGetLocal);
  Test('An operand that a later operand rebinds keeps OP_GET_LOCAL',
    TestOperandRebindingKeepsGetLocal);
  Test('A captured let operand keeps OP_GET_LOCAL',
    TestCapturedLetOperandKeepsGetLocal);
  Test('A loop that creates a closure keeps OP_GET_LOCAL',
    TestLoopThatCreatesClosureKeepsGetLocal);
  Test('Switch clause forgets initialized lets',
    TestSwitchClauseForgetsInitializedLets);
  Test('A default parameter value keeps OP_GET_LOCAL',
    TestDefaultParameterValueKeepsGetLocal);
  Test('A default parameter value that can reach its binding uses a temporary',
    TestDefaultParameterValueReachingItsBindingUsesTemporary);
  Test('With parameter expressions the body vars get their own environment',
    TestParameterExpressionBodyVarEnvironment);
  Test('An operand that is its own destination keeps OP_GET_LOCAL',
    TestOperandThatIsItsOwnDestinationKeepsGetLocal);
  Test('A counted for variable operand skips OP_GET_LOCAL',
    TestCountedForVariableOperandSkipsGetLocal);
  Test('Coverage keeps let and parameter operand copies',
    TestCoverageKeepsLetAndParameterOperandCopies);
  Test('Assignment to an initialized let skips the TDZ probe',
    TestAssignmentToInitializedLetSkipsProbe);
  Test('Compound assignment reads an initialized let directly',
    TestCompoundAssignmentReadsInitializedLetDirectly);
  Test('A numeric immediate condition reads a parameter directly',
    TestNumericImmediateConditionReadsParameterDirectly);
  Test('Discarded store skips the result move',
    TestDiscardedStoreSkipsResultMove);
  Test('this property read uses local register',
    TestThisPropertyReadUsesLocalRegister);
  Test('this property read retains derived-constructor guard',
    TestThisPropertyReadRetainsDerivedGuard);
  Test('local property read uses fused opcode',
    TestLocalPropertyReadUsesFusedOpcode);
  Test('optional local property read skips fused opcode',
    TestOptionalLocalPropertyReadSkipsFusedOpcode);
  Test('Static import loads scale with declarations',
    TestStaticImportLoadsScaleWithDeclarations);
  Test('Binary round-trip', TestBinaryRoundTrip);
  Test('Binary round-trip closed numeric self-call',
    TestBinaryRoundTripClosedNumericSelfCall);
  Test('Binary round-trip upvalue names', TestBinaryRoundTripUpvalueNames);
  Test('Binary round-trip function declaration position',
    TestBinaryRoundTripFunctionDeclarationPosition);
  Test('Binary little-endian format', TestBinaryLittleEndian);
  Test('Binary round-trip constants', TestBinaryRoundTripConstants);
  Test('Binary loader rejects malformed artifacts',
    TestBinaryRejectsMalformedArtifacts);
  Test('Undeclared private name raises SyntaxError', TestUndeclaredPrivateNameRaisesSyntaxError);
  Test('Constant folds nested arithmetic', TestConstantFoldsNestedArithmetic);
  Test('Constant folds BigInt', TestConstantFoldsBigInt);
  Test('Constant folds special numbers', TestConstantFoldsSpecialNumbers);
  Test('Const propagation', TestConstPropagation);
  Test('Const propagation with BigInt', TestConstPropagationBigInt);
  Test('Const propagation skips mutable bindings', TestConstPropagationSkipsMutable);
  Test('Const propagation reaches later reads of a global-backed binding',
    TestConstPropagationReachesGlobalBackedReads);
  Test('Const propagation skips global-backed reads compiled before the declaration',
    TestConstPropagationSkipsGlobalBackedReadsCompiledEarlier);
  Test('Const propagation skips global-backed bindings in non-strict mode',
    TestConstPropagationSkipsGlobalBackedInNonStrictMode);
  Test('Const propagation skips global-backed BigInt bindings',
    TestConstPropagationSkipsGlobalBackedBigInt);
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
  Test('Known numeric local uses add immediate',
    TestKnownNumericLocalUsesAddImmediate);
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
  Test('Reassigned local keeps type only when every value is a Number',
    TestReassignedLocalKeepsTypeOnlyWhenEveryValueIsNumber);
  Test('Return annotation does not type call result',
    TestReturnAnnotationDoesNotTypeCallResult);
  Test('for-of skips handler without abrupt close', TestForOfSkipsHandlerWithoutAbruptClose);
  Test('for-of uses handler for expression body', TestForOfUsesHandlerForExpressionBody);
  Test('for-of uses one iterator-close handler', TestForOfUsesOneIteratorCloseHandler);
  Test('counted-for less-than uses jump-if-not-lt',
    TestCountedForLessThanUsesJumpIfNotLt);
  Test('const self-increment uses generic assignment',
    TestConstSelfIncrementUsesGenericAssignment);
  Test('global-backed counted-for limit is not snapshotted',
    TestGlobalBackedCountedForLimitIsNotSnapshotted);
  Test('if and conditional less-than use jump-if-not-lt',
    TestIfAndConditionalLessThanUseJumpIfNotLt);
  Test('less-than value keeps generic compare',
    TestLessThanValueKeepsGenericCompare);
  Test('Constant if eliminates branch', TestConstantIfEliminatesBranch);
  Test('Constant if prunes abrupt tail', TestConstantIfPrunesAbruptTail);
  Test('Coverage preserves constant branch shape', TestCoveragePreservesConstantBranch);
  Test('Coverage preserves a branch on a global-backed constant',
    TestCoveragePreservesGlobalBackedConstantBranch);
  Test('Constant evaluation options are independent', TestConstantEvaluationOptionsAreIndependent);
  Test('Strict type simplification requires strict-types', TestStrictTypeSimplificationRequiresStrictTypes);
  Test('Strict types hint only enforced locals', TestStrictTypesHintOnlyEnforcedLocals);
  Test('Strict var redeclaration in a catch block keeps its type on the var',
    TestStrictVarRedeclarationInCatchKeepsTypeOnVar);
  Test('Switch exit jumps close upvalues', TestSwitchExitJumpsCloseUpvalues);
  Test('Body vars start undefined; other bindings emit nothing',
    TestBodyVarsStartUndefined);
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
  const AEnableDeadBranchElimination: Boolean;
  const ATraditionalForLoops: Boolean;
  const ANonStrictMode: Boolean;
  const AVarDeclarations: Boolean;
  const ADirectEvalAvailable: Boolean): TGocciaBytecodeModule;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  SourceLines: TStringList;
  Options: TGocciaCompilerOptimizationOptions;
  ParserOptions: TGocciaParserOptions;
begin
  Lexer := TGocciaLexer.Create(ASource, '<test>');
  SourceLines := CreateTextLines(ASource);
  Parser := TGocciaParser.CreateFromLexer(Lexer, '<test>', SourceLines);
  if ATraditionalForLoops or AVarDeclarations then
  begin
    ParserOptions := Parser.Options;
    ParserOptions.TraditionalForLoopsEnabled := ATraditionalForLoops;
    ParserOptions.VarDeclarationsEnabled := AVarDeclarations;
    Parser.ApplyOptions(ParserOptions);
  end;
  ProgramNode := Parser.Parse;

  Compiler := TGocciaCompiler.Create('<test>');
  try
    Compiler.StrictTypes := AStrictTypes;
    Compiler.GlobalBackedTopLevel := AGlobalBackedTopLevel;
    Compiler.NonStrictMode := ANonStrictMode;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape := APreserveCoverageShape;
    Options.DirectEvalAvailable := ADirectEvalAvailable;
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

function TTestCompiler.CountRawOpRecursive(
  const ATemplate: TGocciaFunctionTemplate; const AOp: UInt8): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ATemplate.CodeCount - 1 do
    if DecodeOp(ATemplate.GetInstruction(I)) = AOp then
      Inc(Result);
  for I := 0 to ATemplate.FunctionCount - 1 do
    Inc(Result, CountRawOpRecursive(ATemplate.GetFunction(I), AOp));
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
  Result := Result + CountOp(ATemplate, OP_ADD_NUM_IMM);
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

procedure TTestCompiler.TestThisPropertyReadUsesLocalRegister;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const holder = {' +
    '  value: 42,' +
    '  read() { return this.value; }' +
    '};');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_GET_PROP_CONST);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_GET_PROP_CONST)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_MOVE)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestThisPropertyReadRetainsDerivedGuard;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'class Base {}' +
    'class Derived extends Base {' +
    '  constructor() {' +
    '    this.value;' +
    '    super();' +
    '  }' +
    '}');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_GET_PROP_CONST);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Boolean>(CountOp(Func, OP_JUMP_IF_TRUE) > 0).ToBe(True);
      Expect<Boolean>(CountOp(Func, OP_THROW) > 0).ToBe(True);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestLocalPropertyReadUsesFusedOpcode;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource('const read = (a) => a.x;');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_GET_LOCAL_PROP_CONST);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL_PROP_CONST)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_GET_PROP_CONST)).ToBe(0);
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestInitializedConstOperandsSkipGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const sum = (p, q) => { const a = p.x; const b = q.x; ' +
    'return a * b + a; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstOperandsBeforeDeclarationKeepGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const early = (p) => { const r = a * p.x; const a = p.y; return r; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // The early read of `a` keeps its TDZ check; the other is `return r`.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(2);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestSwitchClauseForgetsInitializedConsts;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const pick = (p) => { switch (p.k) { ' +
    'case 0: const a = p.x; return a * a; ' +
    'case 1: return a * p.y; } return 0; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // Only the read in the second clause still needs its TDZ check.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCoverageKeepsConstOperandCopies;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const sum = (p, q) => { const a = p.x; const b = q.x; ' +
    'return a * b + a; };', False, True);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(3);
  finally
    Module.Free;
  end;
end;

// The sources below read their inputs from properties (`p.x`) so that no
// operand is a compile-time constant; a literal initializer would be folded
// and the test would count nothing.

procedure TTestCompiler.TestParameterOperandsSkipGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const sum = (a, b) => { a.r = a * b + a; a[b] = b; return a[b] < b; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
      Expect<Integer>(CountOp(Func, OP_ARRAY_SET)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_ARRAY_GET)).ToBe(1);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestDirectEvalHostKeepsOperandCopies;
const
  SOURCE =
    'const sum = (a, b) => { let t = a * b; const k = a * 2; return t + a + k; };';

  function CopiesWith(const ADirectEvalAvailable: Boolean): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(SOURCE, False, False, False, True, True, True,
      False, False, False, ADirectEvalAvailable);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  Expect<Integer>(CopiesWith(False)).ToBe(0);
  // Each read of a parameter or of the let binding is copied again: a three
  // times, b and t once. The const k is not: nothing can write it.
  Expect<Integer>(CopiesWith(True)).ToBe(5);
end;

procedure TTestCompiler.TestDirectEvalInFunctionKeepsLaterOperandCopies;

  function CopiesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  // A call to any other global reads every operand in place.
  Expect<Integer>(CopiesIn(
    'const f = (a) => { const before = a * 2; evil("0"); return a * 3 + before; };')).ToBe(0);
  // After a direct eval the parameter is copied again, for `a * 3`; the read
  // before the eval call and the const stay in place.
  Expect<Integer>(CopiesIn(
    'const f = (a) => { const before = a * 2; eval("0"); return a * 3 + before; };')).ToBe(1);

  // In a loop the eval call comes back around, so the reads compiled ahead of
  // it are copied as well: t and a in `t + a * 2`. The loop is examined before
  // it is compiled. Both loops copy `items` to iterate over it.
  Expect<Integer>(
    CopiesIn(
      'const f = (a, items) => { let t = 0; for (const item of items) { t = t + a * 2; eval("0"); } return t; };') -
    CopiesIn(
      'const f = (a, items) => { let t = 0; for (const item of items) { t = t + a * 2; evil("0"); } return t; };')).ToBe(2);
end;

procedure TTestCompiler.TestMethodParameterOperandsSkipGetLocal;

  function CopiesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  Expect<Integer>(CopiesIn(
    'const o = { m(a, b) { a.r = a * b; } };')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'class C { m(a, b) { a.r = a * b; } }')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'class C { static m(a, b) { a.r = a * b; } }')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'const k = "m"; class C { [k](a, b) { a.r = a * b; } }')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'class C { constructor(a, b) { a.r = a * b; } }')).ToBe(0);
  // A rest parameter is marked like any other; a destructured one is not.
  Expect<Integer>(CopiesIn(
    'const f = (a, ...rest) => { a.r = rest * a; };')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'const f = ({ a }, b) => { b.r = a * b; };')).ToBe(1);
end;

procedure TTestCompiler.TestLetOperandsSkipGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const sum = (p) => { let x = p.x; let y; y = p.y; x = x * y + x; ' +
    'p[y] = x; p.r = x < y ? x - y : y; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // The one copy left moves `y` into the conditional's result register.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestLetOperandBeforeDeclarationKeepsGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const early = (p) => { p.r = x * p.y; let x = p.x; p.s = x * p.y; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // Only the read ahead of the declaration keeps its TDZ check.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestOperandRebindingKeepsGetLocal;

  function CopiesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  // No later operand writes a local: nothing is copied.
  Expect<Integer>(CopiesIn('const f = (a, p) => a * (p.x + 1);')).ToBe(0);
  // A call copies its receiver and its argument into the call window; the
  // left operand is still read in place.
  Expect<Integer>(CopiesIn('const f = (a, p) => a * p.f(a);')).ToBe(2);
  // The right operand rebinds the left one, which therefore keeps its copy.
  Expect<Integer>(CopiesIn('const f = (a, p) => a * (a = p.x);')).ToBe(1);
  Expect<Integer>(CopiesIn('const f = (a, p) => a * (a += p.x);')).ToBe(1);
  Expect<Integer>(CopiesIn('const f = (a, p) => a * a++;')).ToBe(1);
  // The second copy is the TDZ probe of the destructuring assignment.
  Expect<Integer>(CopiesIn('const f = (a, p) => a * ([a] = p.x);')).ToBe(2);
  // A write to any other local is refused as well; the check is by shape.
  Expect<Integer>(CopiesIn('const f = (a, p) => a * (p = p.x);')).ToBe(1);
  // The object and the key of a store are evaluated before the value, so
  // both keep their copies when the value writes a local.
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; a[p] = (a = p.x); };')).ToBe(2);
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; a[p] = (p = a.x); };')).ToBe(2);
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; a.r = (a = p.x); };')).ToBe(1);
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; a[p] = p.x; };')).ToBe(0);
  // An element read takes its object before the index rebinds it; the store
  // around it takes `p` before its value, which holds that assignment.
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; p.s = a[(a = p.x)]; };')).ToBe(2);
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; p.s = a[p.x]; };')).ToBe(0);
  // A less-than condition reads its left operand first.
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { p.r = a * 2; if (a < (a = p.x)) { p.s = 1; } };')).ToBe(1);
end;

procedure TTestCompiler.TestCapturedLetOperandKeepsGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const outer = (p) => { let x = p.x; let y = p.y; const w = () => { x = 1; }; ' +
    'p.r = x * y; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // `x` lives in a cell once the closure exists; `y` and `p` do not.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestLoopThatCreatesClosureKeepsGetLocal;

  function CopiesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  // A loop without a closure reads its operands in place.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { p.r = x * s; } };'))
    .ToBe(0);
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (let s of p.l) { p.r = x * s; } };'))
    .ToBe(0);
  // The closure is compiled after the read, so the loop is examined first.
  // `p` (twice) and `x` are copied; the const `s` is not.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { p.r = x * s; ' +
    'p.w = () => s; } };')).ToBe(3);
  // A loop nested in such a loop inherits the answer.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { ' +
    'for (const t of p.m) { p.r = x * t; } p.w = () => s; } };')).ToBe(3);
  // The closure may hide in an accessor, a method or a class.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { p.r = x * s; ' +
    'p.w = { get v() { return s; } }; } };')).ToBe(3);
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { p.r = x * s; ' +
    'p.w = { m() { return s; } }; } };')).ToBe(3);
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { p.r = x * s; ' +
    'p.w = class { m() { return s; } }; } };')).ToBe(3);
  // Code after the loop is unaffected.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; for (const s of p.l) { p.w = () => s; } ' +
    'p.r = x * x; };')).ToBe(1);
end;

procedure TTestCompiler.TestSwitchClauseForgetsInitializedLets;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const pick = (p) => { switch (p.k) { ' +
    'case 0: let a = p.x; p.r = a * a; a = p.y; break; ' +
    'case 1: p.r = a * p.y; a = p.x; } };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // The second clause keeps the TDZ check of its read and of its
      // assignment; the first clause needs neither.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(2);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestDefaultParameterValueKeepsGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource('const f = (a, b = a * a) => a * b;');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // A default value runs while later parameters are still uninitialized,
      // so its two reads keep the check; the body's reads do not.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(2);
  finally
    Module.Free;
  end;
end;

// ES2026 §8.6.3 SingleNameBinding initializes the parameter only after its
// Initializer completes. An initializer that can reach the binding is compiled
// into a temporary and moved into the parameter's register once it completes,
// so the TDZ hole stays in place for the read or write. One that cannot reach
// it keeps the register as its destination and costs no move.
procedure TTestCompiler.TestDefaultParameterValueReachingItsBindingUsesTemporary;

  function PreambleMoves(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
    I: Integer;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if not Assigned(Func) then
        Exit;
      Result := 0;
      for I := 0 to Func.ParameterPreambleSize - 1 do
        if TGocciaOpCode(DecodeOp(Func.GetInstruction(I))) = OP_MOVE then
          Inc(Result);
    finally
      Module.Free;
    end;
  end;

begin
  // Each initializer is paired with one that has the same shape but reaches
  // another binding instead of its own.
  Expect<Integer>(PreambleMoves('const f = (p, a = [a, 1]) => a * p;') -
    PreambleMoves('const f = (p, a = [p, 1]) => a * p;')).ToBe(1);
  Expect<Integer>(PreambleMoves('const f = (p, a = { v: a }) => a * p;') -
    PreambleMoves('const f = (p, a = { v: p }) => a * p;')).ToBe(1);
  Expect<Integer>(PreambleMoves('const f = (p, a = 0 || a) => a * p;') -
    PreambleMoves('const f = (p, a = 0 || p) => a * p;')).ToBe(1);
  Expect<Integer>(PreambleMoves('const f = (p, a = (a = 7)) => a * p;') -
    PreambleMoves('const f = (p, a = (p = 7)) => a * p;')).ToBe(1);
  Expect<Integer>(PreambleMoves('const f = (p, a = ([a] = [7])) => a * p;') -
    PreambleMoves('const f = (p, a = ([p] = [7])) => a * p;')).ToBe(1);
  Expect<Integer>(PreambleMoves('const f = (p, a = () => a) => a * p;') -
    PreambleMoves('const f = (p, a = () => p) => a * p;')).ToBe(1);
  Expect<Integer>(PreambleMoves('const f = (p = () => a, a = [1]) => a * p;') -
    PreambleMoves('const f = (p = () => q, a = [1]) => a * p;')).ToBe(1);
end;

// ES2026 §10.2.11 FunctionDeclarationInstantiation step 30: with an expression
// in the parameter list the body's vars get an Environment Record of their own.
procedure TTestCompiler.TestParameterExpressionBodyVarEnvironment;

  function PreambleOps(const ASource: string; const AOp: TGocciaOpCode;
    out AAllGetLocals: Integer): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
    I: Integer;
  begin
    Result := -1;
    AAllGetLocals := -1;
    Module := CompileSource(ASource, False, False, False, True, True, True,
      False, False, True);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      if not Assigned(Func) then
        Exit;
      Result := 0;
      for I := 0 to Func.ParameterPreambleSize - 1 do
        if TGocciaOpCode(DecodeOp(Func.GetInstruction(I))) = AOp then
          Inc(Result);
      AAllGetLocals := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

var
  AllGetLocals: Integer;
begin
  // No closure in the parameter list captured `a`, so its var keeps the
  // parameter's register, and reads it in place like a parameter.
  Expect<Integer>(PreambleOps(
    'const f = (a, b = 1) => { var a; return a * b; };', OP_GET_LOCAL,
    AllGetLocals)).ToBe(0);
  Expect<Integer>(AllGetLocals).ToBe(0);

  // `g` captured `a`: the var is a register of its own, which the preamble
  // fills from the parameter's cell (step 30.e.i.4).
  Expect<Integer>(PreambleOps(
    'const f = (a, g = () => a) => { var a; return a * g(); };', OP_GET_LOCAL,
    AllGetLocals)).ToBe(1);
  Expect<Integer>(PreambleOps(
    'const f = (a, g = () => a) => { return a * g(); };', OP_GET_LOCAL,
    AllGetLocals)).ToBe(0);

  // A var that names no parameter starts as undefined (step 30.e.i.3). Its
  // register can hold a preamble temporary or a surplus argument, so the
  // preamble clears it.
  Expect<Integer>(PreambleOps(
    'const f = (p, a = p.x * p.y) => { var t; return t * a; };',
    OP_LOAD_UNDEFINED, AllGetLocals) - PreambleOps(
    'const f = (p, a = p.x * p.y) => { return p * a; };', OP_LOAD_UNDEFINED,
    AllGetLocals)).ToBe(1);
  Expect<Integer>(PreambleOps(
    'const f = (p, a = 1) => { var t; return t * p; };', OP_LOAD_UNDEFINED,
    AllGetLocals) - PreambleOps(
    'const f = (p, a = 1) => { return a * p; };', OP_LOAD_UNDEFINED,
    AllGetLocals)).ToBe(1);
end;

procedure TTestCompiler.TestOperandThatIsItsOwnDestinationKeepsGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  // `var a` redeclares the parameter, so the initializer is compiled straight
  // into the register it also reads.
  Module := CompileSource('const f = (a, p) => { var a = a * p.x; p.r = a; };',
    False, False, False, True, True, True, False, False, True);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCountedForVariableOperandSkipsGetLocal;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const f = (p) => { for (let i = 0; i < 3; i++) { p[i] = i * p.x; } };',
    False, False, False, True, True, True, True);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCoverageKeepsLetAndParameterOperandCopies;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const sum = (a, p) => { let x = p.x; x = x * a + x; x += a; ' +
    'return a < x ? x : a; };', False, True);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // x, a, x and the assignment probe; x and a for `+=`; a and x for `<`;
      // one copy into the result register in each branch.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(10);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'const sum = (a, p) => { let x = p.x; x = x * a + x; x += a; ' +
    'return a < x ? x : a; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      // Without coverage only the moves into a result register remain: the
      // right-hand side of `+=` and the two branches of the conditional.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(3);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestAssignmentToInitializedLetSkipsProbe;

  function CopiesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_SET_LOCAL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; x = p.y; };')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { a = p.y; };')).ToBe(0);
  // An assignment compiled ahead of the declaration can run in the TDZ.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { x = p.y; let x = p.x; };')).ToBe(1);
  // So can one inside the binding's own initializer.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = (x = p.y); };')).ToBe(1);
end;

procedure TTestCompiler.TestCompoundAssignmentReadsInitializedLetDirectly;

  function CopiesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_SET_LOCAL);
      if Assigned(Func) then
        Result := CountOp(Func, OP_GET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; x += p.y; };')).ToBe(0);
  Expect<Integer>(CopiesIn(
    'const f = (a, p) => { a *= p.y; };')).ToBe(0);
  // The old value is taken before a right-hand side that rebinds the target.
  Expect<Integer>(CopiesIn(
    'const f = (p) => { let x = p.x; x += (x = p.y); };')).ToBe(1);
  Expect<Integer>(CopiesIn(
    'const f = (p) => { x += p.y; let x = p.x; };')).ToBe(1);
end;

procedure TTestCompiler.TestNumericImmediateConditionReadsParameterDirectly;
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
    Func := FindFunctionWithOp(Module.TopLevel, OP_JUMP_IF_NUM_NOT_LTE_IMM);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_SUB_NUM_IMM)).ToBe(2);
      // The remaining copy moves `n` into the result register.
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(1);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestDiscardedStoreSkipsResultMove;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const store = (p, q) => { const list = p.items; const value = q.x; ' +
    'list[0] = value; p.last = value; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_ARRAY_SET);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      // The two declarations move their initializer into the binding; the
      // two stores move nothing, and neither copies an operand.
      Expect<Integer>(CountOp(Func, OP_MOVE)).ToBe(2);
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestOptionalLocalPropertyReadSkipsFusedOpcode;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource('const read = (a) => a?.x;');
  try
    Expect<Boolean>(FindFunctionWithOp(Module.TopLevel,
      OP_GET_LOCAL_PROP_CONST) = nil).ToBe(True);
    Func := FindFunctionWithOp(Module.TopLevel, OP_GET_PROP_CONST);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
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
    // The compiled module emits the closed numeric self-call...
    LoadedFunction := FindFunctionWithOp(Original.TopLevel, OP_CALL_SELF_NUM);
    Expect<Boolean>(Assigned(LoadedFunction)).ToBe(True);
    if Assigned(LoadedFunction) then
      Expect<Integer>(CountOp(LoadedFunction, OP_CALL_SELF_NUM)).ToBe(2);
    SaveModuleToFile(Original, TempFile);
    Loaded := LoadModuleFromFile(TempFile);
    try
      // ...but the loader de-specializes it to the ordinary self-call, because
      // the proof that makes a closed numeric frame memory-safe is not
      // serialized (ADR 0101, ADR 0127).
      Expect<Boolean>(Assigned(FindFunctionWithOp(Loaded.TopLevel,
        OP_CALL_SELF_NUM))).ToBe(False);
      Expect<Integer>(CountRawOpRecursive(Loaded.TopLevel, OP_CALL_SELF))
        .ToBe(2);
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

{ Coverage reports a function at its declaration site (LCOV FN:), which is a
  different position from the first executed instruction of its body. The
  declaration position must therefore be carried on the template and survive a
  .gbc round-trip, so precompiled bytecode reports the same lines as source. }
function FindTemplateByDeclarationLine(
  const AParent: TGocciaFunctionTemplate;
  const ALine: UInt32): TGocciaFunctionTemplate;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AParent.FunctionCount - 1 do
    if Assigned(AParent.GetFunction(I).DebugInfo) and
       (AParent.GetFunction(I).DebugInfo.DeclarationLine = ALine) then
    begin
      Result := AParent.GetFunction(I);
      Exit;
    end;
end;

procedure TTestCompiler.TestBinaryRoundTripFunctionDeclarationPosition;
var
  Original, Loaded: TGocciaBytecodeModule;
  OriginalFunc, LoadedFunc: TGocciaFunctionTemplate;
  TempFile: string;
begin
  // `multi` is declared on line 2; its body's first statement is on line 3.
  Original := CompileSource(
    'const oneLine = () => 1;'#10 +
    'const multi = (a) => {'#10 +
    '  const b = a + 1;'#10 +
    '  return b;'#10 +
    '};'#10,
    False, False, False, False, False, False);
  TempFile := GetTempFileName + '.gbc';
  try
    Expect<Integer>(Original.TopLevel.FunctionCount).ToBe(2);
    Expect<Boolean>(Assigned(
      FindTemplateByDeclarationLine(Original.TopLevel, 1))).ToBe(True);

    OriginalFunc := FindTemplateByDeclarationLine(Original.TopLevel, 2);
    Expect<Boolean>(Assigned(OriginalFunc)).ToBe(True);
    // The body's first instruction is on a later line than the declaration,
    // so the two positions cannot be conflated.
    Expect<Integer>(
      Integer(OriginalFunc.DebugInfo.GetLineMapEntry(0).Line)).ToBe(3);
    Expect<Integer>(Integer(OriginalFunc.DebugInfo.CoverageLine)).ToBe(2);

    SaveModuleToFile(Original, TempFile);
    Loaded := LoadModuleFromFile(TempFile);
    try
      LoadedFunc := FindTemplateByDeclarationLine(Loaded.TopLevel, 2);
      Expect<Boolean>(Assigned(LoadedFunc)).ToBe(True);
      Expect<Integer>(Integer(LoadedFunc.DebugInfo.DeclarationColumn)).ToBe(
        Integer(OriginalFunc.DebugInfo.DeclarationColumn));
      Expect<Integer>(Integer(LoadedFunc.DebugInfo.CoverageLine)).ToBe(2);
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

procedure TTestCompiler.TestBinaryRejectsMalformedArtifacts;
var
  Loaded, Module: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  TempFile: string;

  procedure ExpectRejected(const ATemplate: TGocciaFunctionTemplate);
  var
    ErrorMessage: string;
    Raised: Boolean;
  begin
    Module := TGocciaBytecodeModule.Create('test', '<malformed>');
    Module.TopLevel := ATemplate;
    TempFile := GetTempFileName + '.gbc';
    try
      SaveModuleToFile(Module, TempFile);
      Loaded := nil;
      ErrorMessage := '';
      Raised := False;
      try
        Loaded := LoadModuleFromFile(TempFile);
      except
        on E: Exception do
        begin
          Raised := True;
          ErrorMessage := E.Message;
        end;
      end;
      Loaded.Free;
      Expect<Boolean>(Raised).ToBe(True);
      Expect<Boolean>(Pos('Invalid bytecode', ErrorMessage) > 0).ToBe(True);
    finally
      Module.Free;
      DeleteFile(TempFile);
    end;
  end;

begin
  Module := nil;
  Loaded := nil;

  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_THROW_TYPE_ERROR_CONST))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(99)).ToBe(False);
  Expect<Boolean>(IsValidGocciaOpCode(144)).ToBe(False);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CALL_SELF_NUM))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_GET_LOCAL_PROP_CONST))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_ADD_NUM_IMM))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_JUMP_IF_NOT_LT))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CREATE_GLOBAL_IMPORT_BINDING))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CHECK_BINDING_INITIALIZED))).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_GET_LOCAL_PROP_CONST)).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_JUMP_IF_NOT_LT)).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterC(OP_JUMP_IF_NOT_LT)).ToBe(False);
  Expect<Boolean>(GocciaOpCodeUsesRegisterA(OP_CLOSE_UPVALUE)).ToBe(False);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_DEFINE_DATA_PROP)).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_DEFINE_METHOD_PROP)).ToBe(True);

  Template := TGocciaFunctionTemplate.Create('invalid-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_LOAD_TRUE, 1, 0, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-wide-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_MOVE, 0, 256, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-compact-b-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_MOVE, 0, 1, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-data-property-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_DEFINE_DATA_PROP, 0, 1, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-method-property-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_DEFINE_METHOD_PROP, 0, 1, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-compact-c-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_ADD, 0, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-call-register-window');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_CALL, 0, 1, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-bx-register');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_TO_PRIMITIVE, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-local');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_GET_LOCAL, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-local-write');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_SET_LOCAL, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-close-upvalue');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_CLOSE_UPVALUE, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-upvalue');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_GET_UPVALUE, 0, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-upvalue-reference');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABC(OP_RESOLVE_UPVALUE_REF, 0, 0, 0));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-constant');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-function');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeABx(OP_CLOSURE, 0, 1));
  ExpectRejected(Template);

  Template := TGocciaFunctionTemplate.Create('invalid-jump');
  Template.MaxRegisters := 1;
  Template.EmitInstruction(EncodeAx(OP_JUMP, -2));
  ExpectRejected(Template);
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

procedure TTestCompiler.TestConstPropagationReachesGlobalBackedReads;
var
  Module: TGocciaBytecodeModule;
  Reader: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'const a = 2; const b = a * 4; const f = () => a + b; f();',
    False, False, True);
  try
    // The bindings themselves are still declared and defined.
    Expect<Integer>(
      CountOp(Module.TopLevel, OP_DEFINE_GLOBAL_CONST_LONG)).ToBe(3);
    Expect<Integer>(CountArithmeticOps(Module.TopLevel)).ToBe(0);

    Reader := Module.TopLevel.GetFunction(0);
    Expect<Integer>(CountOp(Reader, OP_GET_GLOBAL)).ToBe(0);
    Expect<Integer>(CountArithmeticOps(Reader)).ToBe(0);
    Expect<Boolean>(HasLoadInt(Reader, 10)).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationSkipsGlobalBackedReadsCompiledEarlier;
var
  Module: TGocciaBytecodeModule;
begin
  // f can run while a is still uninitialized, so it keeps the named read
  // that raises; g cannot exist before a is initialized.
  Module := CompileSource(
    'const f = () => a; const a = 2; const g = () => a; f() + g();',
    False, False, True);
  try
    Expect<Integer>(
      CountOp(Module.TopLevel.GetFunction(0), OP_GET_GLOBAL)).ToBe(1);
    Expect<Integer>(
      CountOp(Module.TopLevel.GetFunction(1), OP_GET_GLOBAL)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationSkipsGlobalBackedInNonStrictMode;
var
  Module: TGocciaBytecodeModule;
begin
  // A sloppy direct eval can shadow the name with a function-level var.
  Module := CompileSource('const a = 2; const f = () => a; f();',
    False, False, True, True, True, True, False, True);
  try
    Expect<Integer>(
      CountOp(Module.TopLevel.GetFunction(0), OP_GET_GLOBAL)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationSkipsGlobalBackedBigInt;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const a = 2n; const f = () => a; f();',
    False, False, True);
  try
    Expect<Integer>(
      CountOp(Module.TopLevel.GetFunction(0), OP_GET_GLOBAL)).ToBe(1);
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

procedure TTestCompiler.TestKnownNumericLocalUsesAddImmediate;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('let i = 2; i + 3;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_INT)).ToBe(0);
  finally
    Module.Free;
  end;

  Module := CompileSource('let i = 2; 3 + i;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_INT)).ToBe(0);
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

procedure TTestCompiler.TestReassignedLocalKeepsTypeOnlyWhenEveryValueIsNumber;
var
  Module: TGocciaBytecodeModule;
begin
  // The string assigned at the end of the body reaches the reads at its
  // start on the next iteration.
  Module := CompileSource(
    'let a = 1;' + sLineBreak +
    'for (const s of [0, 1]) { a + 1; a + a; a = "x"; }',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(2);
  finally
    Module.Free;
  end;

  // The string assigned in one branch reaches the read after the join.
  Module := CompileSource(
    'let a = 1;' + sLineBreak +
    'if (a > 0) { a = "x"; } else { a = 2; }' + sLineBreak +
    'a + 1;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;

  // Every value given to a and b is a Number, so every read stays typed,
  // including the reads after assignments the compiler cannot type itself.
  Module := CompileSource(
    'let a = 1; let b = 0.5;' + sLineBreak +
    'for (const s of [0, 1]) { b = b * 2 - a; a += 2; a = a ^ 3; }' + sLineBreak +
    'a + b;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_MUL_FLOAT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB_FLOAT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(2);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_MUL)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestReturnAnnotationDoesNotTypeCallResult;
var
  Module: TGocciaBytecodeModule;
begin
  // A return-type annotation is not checked, so neither a direct call nor a
  // call through a captured binding gets typed arithmetic from it.
  Module := CompileSource(
    'const f = (): number => 1; f() + 1; f() + f();' +
    'const g = () => f() - 1; g();',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(2);
  finally
    Module.Free;
  end;

  // Under strict types a binding initialized from the call is not enforced,
  // and an enforced binding checks a value computed from the call.
  Module := CompileSource(
    'const f = (): number => 1; const r = f(); r + 1;' +
    'let t: number = 0; t = t + f();',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(2);
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
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

procedure TTestCompiler.TestCountedForLessThanUsesJumpIfNotLt;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(
    'for (let i = 0; i < 5; i = i + 1) { i; }',
    False, False, False, False, False, False, True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP_IF_NOT_LT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_LT)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_GTE_INT)).ToBe(0);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'const run = (n) => { let c = 0; for (let i = 0; i < n; i = i + 1) c = c + 1; return c; }; run(3);',
    False, False, False, False, False, False, True);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_JUMP_IF_NOT_LT);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_JUMP_IF_NOT_LT)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_LT)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstSelfIncrementUsesGenericAssignment;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const x = (x = x + 1);');
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_INC_NUMERIC)).ToBe(0);
  finally
    Module.Free;
  end;

  Module := CompileSource('let x = 0; x = x + 1;');
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_INC_NUMERIC)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestGlobalBackedCountedForLimitIsNotSnapshotted;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let n = 5; for (let i = 0; i < n; i = i + 1) { i; }',
    False, False, True, True, True, True, True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_INT)).ToBe(0);
    Expect<Boolean>(CountOp(Module.TopLevel, OP_INC_NUMERIC) > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestIfAndConditionalLessThanUseJumpIfNotLt;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; let b = 2; if (a < b) { a; }',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP_IF_NOT_LT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_LT)).ToBe(0);
  finally
    Module.Free;
  end;

  Module := CompileSource(
    'let a = 1; let b = 2; a < b ? 1 : 0;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP_IF_NOT_LT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_LT)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestLessThanValueKeepsGenericCompare;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'let a = 1; let b = 2; a < b;',
    False, False, False, False, False, False);
  try
    Expect<Boolean>((CountOp(Module.TopLevel, OP_LT) +
      CountOp(Module.TopLevel, OP_LT_INT) +
      CountOp(Module.TopLevel, OP_LT_FLOAT)) > 0).ToBe(True);
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP_IF_NOT_LT)).ToBe(0);
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

procedure TTestCompiler.TestCoveragePreservesGlobalBackedConstantBranch;
const
  SOURCE = 'const on = false; ' +
    'const f = () => { if (on) { return 1; } return 2; }; f();';
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(SOURCE, False, False, True);
  try
    Expect<Integer>(
      CountOp(Module.TopLevel.GetFunction(0), OP_JUMP_IF_FALSE)).ToBe(0);
  finally
    Module.Free;
  end;

  Module := CompileSource(SOURCE, False, True, True);
  try
    Expect<Boolean>(
      CountOp(Module.TopLevel.GetFunction(0), OP_JUMP_IF_FALSE) > 0).ToBe(True);
  finally
    Module.Free;
  end;

  // A logical expression and a ternary decided by the constant keep their
  // branches under coverage as well: the constant is not propagated at all.
  Module := CompileSource(
    'const on = false; const f = (g) => on && g(); ' +
    'const h = (g) => (on ? g() : 2);', False, True, True);
  try
    Expect<Boolean>(
      CountOp(Module.TopLevel.GetFunction(0), OP_JUMP_IF_FALSE) > 0).ToBe(True);
    Expect<Boolean>(
      CountOp(Module.TopLevel.GetFunction(1), OP_JUMP_IF_FALSE) > 0).ToBe(True);
    Expect<Boolean>(CountOpRecursive(Module.TopLevel, OP_GET_GLOBAL) >= 2)
      .ToBe(True);
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

procedure TTestCompiler.TestStrictTypesHintOnlyEnforcedLocals;
var
  Module: TGocciaBytecodeModule;
begin
  // A literal-initialized or annotated let is enforced, so it keeps its typed
  // arithmetic and a guard on each incompatible assignment.
  Module := CompileSource(
    'let a = 1; let b: number = 2; a + b; a = "x"; b = "y";',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(2);
  finally
    Module.Free;
  end;

  // An expression-initialized let is not enforced, so neither its
  // initializer nor a later assignment gives it a hint.
  Module := CompileSource(
    'let a = 1; let c = a + 2.5; c + a; c = 3; c + a; c = "x";',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(2);
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(0);
  finally
    Module.Free;
  end;

  // A const cannot be reassigned, so it keeps the wider inference.
  Module := CompileSource(
    'let a = 1; const c = a + 2.5; c + a;',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(2);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
  finally
    Module.Free;
  end;

  // Without strict types nothing is enforced and inference is unchanged.
  Module := CompileSource(
    'let a = 1; let c = a + 2.5; c + a;',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(2);
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestStrictVarRedeclarationInCatchKeepsTypeOnVar;
var
  Module: TGocciaBytecodeModule;
begin
  // `var x;` inside `catch (x)` redeclares the function's var binding, but
  // `x` in the catch block is the catch parameter (ES2026 B.3.4). The var's
  // enforced Number type stays on the var: the read after the try statement
  // is typed, the catch parameter read is not.
  Module := CompileSource(
    'var x = 1; try { throw "s"; } catch (x) { var x; x - 1; } x - 2;',
    True, False, False, False, False, False, False, False, True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB)).ToBe(1);
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

procedure TTestCompiler.TestBodyVarsStartUndefined;

  function LoadUndefinedCount(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource, False, False, False, True, True, True,
      False, False, True);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
      Expect<Boolean>(Assigned(Func)).ToBe(True);
      if Assigned(Func) then
        Result := CountOp(Func, OP_LOAD_UNDEFINED);
    finally
      Module.Free;
    end;
  end;

begin
  // Every function body ends with an implicit `return undefined`: one
  // OP_LOAD_UNDEFINED. Each var the body declares adds one, since its register
  // may hold a surplus argument or a parameter-destructuring temporary.
  Expect<Integer>(LoadUndefinedCount(
    'const f = (a, p) => { var x, y; p.r = a * x; };')).ToBe(3);
  Expect<Integer>(LoadUndefinedCount(
    'const f = ({ a }, p) => { if (p) { var x; } p.r = a * x; };')).ToBe(2);
  // A body without a var, and a var naming a parameter, add nothing.
  Expect<Integer>(LoadUndefinedCount(
    'const f = (a, p) => { const x = a; p.r = a * x; };')).ToBe(1);
  Expect<Integer>(LoadUndefinedCount(
    'const f = (a, p) => { var a; p.r = a * a; };')).ToBe(1);
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
