program Goccia.Compiler.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  Math,
  StrUtils,
  SysUtils,

  NumberBits,
  StringBuffer,
  TestingPascalLibrary,
  TextSemantics,
  TimingUtils,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Debug,
  Goccia.Bytecode.Module,
  Goccia.Bytecode.Profile,
  Goccia.Compiler,
  Goccia.Compiler.ConstantValue,
  Goccia.Compiler.Expressions,
  Goccia.Compiler.Scope,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Profiler.Compile,
  Goccia.SourceSpan,
  Goccia.TestSetup,
  Goccia.Token,
  Goccia.Values.Primitives;

const
  { The captured-bindings tests read CAPTURE_READS bindings in all, as
    CAPTURE_FEW_BINDINGS bindings read in turn and as CAPTURE_MANY_BINDINGS
    read in turn. Finding the upvalue of each read by visiting the closure's
    upvalues made the many cost several times the few (about 5.5 times
    here); finding it by an index costs about the same for both, and the
    bound allows 2.5 times the upvalue-list probes. }
  CAPTURE_READS = 240000;
  CAPTURE_FEW_BINDINGS = 1875;
  CAPTURE_MANY_BINDINGS = 30000;
  { Top-level declarations per compile in the linear-time tests: enough that
    a step costing time quadratic in them (a scan of every earlier local per
    declaration, about 5 s here for script consts) is far past the bound,
    and few enough to stay under the 65,535-entry constant pool. }
  LINEAR_DECLARATION_COUNT = 15000;
  LINEAR_TRIALS = 3;
  { Each kind compiles its declarations with a few times the work of a
    module-level `const`; the slack absorbs a busy machine. }
  LINEAR_COMPILE_RATIO = 4;
  LINEAR_SLACK_NANOSECONDS = 20 * 1000 * 1000;
  { Names in the shorter list of the parameter-list and pattern tests; the
    longer has twice as many. A scan of the names before each name took
    about 2 s here to parse a parameter list this long (the duplicate
    check), 0.1 s to compile it (the record of the parameter names) and 8 s
    to compile a pattern (collecting its names), and four times as long for
    the longer list. }
  NAME_LIST_COUNT = 8000;
  { Linear name work approximately doubles; full scans quadruple it. }
  NAME_LIST_DOUBLING_PERCENT = 250;
  { The operator-chain test compiles CHAIN_OPERATORS binary operators as
    chains of CHAIN_SHORT_TERMS terms and as chains CHAIN_LENGTH_FACTOR
    times as long. Work quadratic in a chain's length makes the long chains
    cost about CHAIN_LENGTH_FACTOR times as much (over 3 s here); a linear
    compile costs about the same for both, and the bound allows 2.5 times
    the work. }
  CHAIN_SHORT_TERMS = 25;
  CHAIN_LENGTH_FACTOR = 16;
  CHAIN_OPERATORS = 16000;
  CHAIN_COMPILE_RATIO = 3;
  // The bindings each closure of the captured shape reads, in turn.
  CHAIN_CAPTURED_BINDINGS = 25;

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
      const AFunctionKeyword: Boolean = False;
      const ALabels: Boolean = False;
      const AWhileLoops: Boolean = False;
      const AFileName: string = '<test>'): TGocciaBytecodeModule;
    function CountOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): Integer;
    function CountOpRecursive(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): Integer;
    // Reads of a global binding by name: OP_GET_GLOBAL (unresolved names and
    // `typeof`) and OP_GET_GLOBAL_DECLARED (a script's top-level bindings).
    function CountNamedGlobalReads(
      const ATemplate: TGocciaFunctionTemplate): Integer;
    function FindFunctionWithOp(const ATemplate: TGocciaFunctionTemplate;
      const AOp: TGocciaOpCode): TGocciaFunctionTemplate;
    function FindTemplateWithDirectEval(
      const ATemplate: TGocciaFunctionTemplate): TGocciaFunctionTemplate;
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
    // The compiler's recorded type of every typed local of the first
    // function in ASource, in slot order: I integer, F float, B boolean,
    // S string, R reference. A local the compiler left untyped adds nothing.
    function RecordedLocalTypes(const ASource: string;
      const AStrictTypes: Boolean = False;
      const ANonStrictMode: Boolean = False): string;
    // The recorded types of `const r = AInitializer;` in a function whose
    // parameters a, b and o are untyped, after the declarations in APrelude.
    function InitializerTypes(const APrelude, AInitializer: string;
      const AStrictTypes: Boolean): string;
    // Checks InitializerTypes in the default and the strict-types profile.
    procedure ExpectInitializerType(const APrelude, AInitializer,
      AExpected: string);
    // True when `f()` in g is typed as a Number: g adds two results of the
    // call and the sum uses OP_ADD_FLOAT. ADeclaration declares f inside the
    // function that declares g, so f is a local, not a global.
    function CallResultIsTyped(const ADeclaration: string;
      const AStrictTypes: Boolean = False;
      const ANonStrictMode: Boolean = False;
      const ACaller: string =
        'const g = () => { const r = f(); return r + r; };'): Boolean;

    procedure TestCompileLiteral;
    procedure TestASTSpansUseUTF16CodeUnitOffsets;
    procedure TestSourceCoordinatesCoverECMAScriptLineTerminators;
    procedure TestSingleCodeUnitStringLiteralsUseImmediateOpcode;
    procedure TestCompileRegExpLiteral;
    procedure TestCompileArithmetic;
    procedure TestCompileVariable;
    procedure TestCompileFunction;
    procedure TestAnonymousDerivedClassInBlockKeepsLaterLocalsApart;
    function CompileNanoseconds(const ALinePattern: string;
      const AGlobalBackedTopLevel: Boolean;
      const ACount: Integer = LINEAR_DECLARATION_COUNT;
      const APrefix: string = ''; const ASuffix: string = ''): Int64;
    procedure ExpectTopLevelCompileLinear(const ABaseline: Int64;
      const ALinePattern: string; const AGlobalBackedTopLevel: Boolean);
    procedure TestScriptTopLevelDeclarationsCompileInLinearTime;
    procedure TestModuleTopLevelDeclarationsCompileInLinearTime;
    procedure TestTopLevelCallsCompileInLinearTime;
    procedure TestManyCapturedBindingsCompileInLinearTime;
    procedure TestDirectEvalAmongManyCapturedBindingsCompilesInLinearTime;
    procedure ExpectNameListLinear(const APattern, AItem: string;
      const ACount: Integer = NAME_LIST_COUNT);
    function SourceCompileCounters(const ASource: string; const AStrictTypes: Boolean = False;
      const AFileName: string = '<test>';
      const APreserveCoverageShape: Boolean = False;
      const AGlobalBackedTopLevel: Boolean = False): TGocciaCompileCounters;
    procedure ExpectWorkLinear(const AShort, ALong: TGocciaCompileCounters;
      const ACounter: TGocciaCompileCounter);
    procedure TestAlternatingExpressionWork;
    procedure TestLogicalInitializerWork;
    procedure TestDirectEvalRecordWork;
    procedure TestNewTypeArgumentWork;
    procedure TestParameterListsParseAndCompileInLinearTime;
    procedure TestDestructuringPatternsCompileInLinearTime;
    procedure TestMatchBindingNameChecksScale;
    procedure TestDefaultParameterListWorkScales;
    procedure TestOrdinaryParametersSkipInitializerSourceChecks;
    procedure TestDirectEvalNameChecksScale;
    procedure TestLargeSwitchLookupProbesScale;
    procedure TestOperatorChainsCompileInLinearTime;
    procedure TestOperatorChainsFoldTheirConstantStart;
    procedure TestNestedChainsCompileInLinearTime;
    procedure TestNestedChainsFoldTheirConstantOperands;
    procedure TestSwitchCaseDeclarationsCompileInLinearTime;
    procedure TestTypeScriptComparisonChainsParseInLinearTime;
    procedure TestLongBranchesCompileOnce;
    procedure TestSlotHoldsLocalFollowsTheLocals;
    procedure TestEndingABlockRestoresTheBindingItShadowed;
    procedure TestVarAndSlotLookupsFindTheOldestLocal;
    procedure TestFloatConstantsArePooledByBitPattern;
    procedure TestSkippedLocalsResolveInConstantTime;
    procedure TestDistinctFloatConstantsCompileInLinearTime;
    procedure TestThisPropertyReadUsesLocalRegister;
    procedure TestThisPropertyReadRetainsDerivedGuard;
    procedure TestLocalPropertyReadUsesFusedOpcode;
    procedure TestOptionalLocalPropertyReadSkipsFusedOpcode;
    procedure TestAsyncGeneratorReturnAwaitsBeforeTryCleanup;
    procedure TestOtherFunctionKindsReturnWithoutAwait;
    procedure TestForAwaitCloseAwaitsReturnResult;
    procedure TestInitializedConstOperandsSkipGetLocal;
    procedure TestConstOperandsBeforeDeclarationKeepGetLocal;
    procedure TestSwitchClauseForgetsInitializedConsts;
    procedure TestCoverageKeepsConstOperandCopies;
    procedure TestParameterOperandsSkipGetLocal;
    procedure TestFunctionWithoutDirectEvalReadsOperandRegisters;
    procedure TestDirectEvalInFunctionKeepsOperandCopies;
    procedure TestDirectEvalSiteRecordListsCallSiteBindings;
    procedure TestDirectEvalSiteRecordTakesCallStrictness;
    procedure TestDirectEvalSiteRecordCheckRejectsMismatches;
    procedure TestDirectEvalCapturesCallSiteLocals;
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
    procedure TestCompoundAssignmentWritesRegisterOnlyBindingDirectly;
    procedure TestInitializerReachingItsBindingKeepsCopy;
    procedure TestLexicalHolesSetOnlyWhereObservable;
    procedure TestCompoundSmallIntegerUsesImmediate;
    procedure TestStaticImportLoadsScaleWithDeclarations;
    procedure TestBinaryRoundTrip;
    procedure TestObjectRestBytecodeRoundTrip;
    procedure TestGeneratedNamesPerCompilation;
    procedure TestBinaryRoundTripClosedNumericSelfCall;
    procedure TestBinaryRoundTripUpvalueNames;
    procedure TestBinaryRoundTripFunctionDeclarationPosition;
    procedure TestBinaryLittleEndian;
    procedure TestBinaryRoundTripConstants;
    procedure TestBinaryRoundTripKeepsBothZeros;
    procedure TestBinaryRejectsMalformedArtifacts;
    procedure TestUndeclaredPrivateNameRaisesSyntaxError;
    procedure TestBytecodeLimitRaisesSyntaxErrorAtSource;
    procedure TestLongBranchTakesLongFormInItsFunctionOnly;
    procedure TestCompileAfterRaisedCompileStartsAtNestingDepthZero;
    procedure TestConstantFoldsNestedArithmetic;
    procedure TestConstantFoldsBigInt;
    procedure TestConstantFoldsSpecialNumbers;
    procedure TestConstPropagation;
    procedure TestConstPropagationBigInt;
    procedure TestConstPropagationSkipsMutable;
    procedure TestConstPropagationReachesGlobalBackedReads;
    procedure TestConstPropagationSkipsGlobalBackedReadsCompiledEarlier;
    procedure TestConstPropagationSkipsGlobalBackedUnderSloppyDirectEval;
    procedure TestGlobalBackedReadsThrowForAMissingBinding;
    procedure TestSloppyFunctionFoldsEnclosingConstWithoutDirectEval;
    procedure TestDirectEvalSiteRecordNamesVarEnvironment;
    procedure TestDirectEvalSiteRecordNamesPrivateEnvironment;
    procedure TestDirectEvalCompilesAsClosureOfCallSite;
    procedure TestConstPropagationSkipsGlobalBackedBigInt;
    procedure TestInferredNumericLocalsUseTypedArithmetic;
    procedure TestAnnotatedParametersUseTypedArithmetic;
    procedure TestClosedNumericFibonacciUsesSuperinstructions;
    procedure TestConciseArrowBodyMarksTailCalls;
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
    procedure TestUncheckedStrictBindingDoesNotTypeOperands;
    procedure TestBinaryOperatorResultTypes;
    procedure TestBitwiseOperatorResultTypes;
    procedure TestUnaryOperatorResultTypes;
    procedure TestIntegerTypeComesOnlyFromInt32Results;
    procedure TestMathCallsAreNotTrusted;
    procedure TestConstTypeComesFromTheNumberProof;
    procedure TestLetKeepsItsTypeOnlyWhenEveryValueIsANumber;
    procedure TestCapturedConstKeepsItsType;
    procedure TestConstFunctionReturnTypeTypesCalls;
    procedure TestCallResultStaysUntypedWhenReturnTypeIsNotProven;
    procedure TestFallThroughMakesTheReturnTypeUntyped;
    procedure TestForOfSkipsHandlerWithoutAbruptClose;
    procedure TestForOfUsesHandlerForExpressionBody;
    procedure TestForOfUsesOneIteratorCloseHandler;
    procedure TestCountedForLessThanUsesJumpIfNotLt;
    procedure TestConstSelfIncrementUsesGenericAssignment;
    procedure TestGlobalBackedCountedForLimitIsNotSnapshotted;
    procedure TestIfAndConditionalLessThanUseJumpIfNotLt;
    procedure TestConstantOperandComparisonsUseImmediateJumps;
    procedure TestConstantOperandComparisonsKeepGenericForms;
    procedure TestHugeConstantComparisonCompilesWithInvalidOpUnmasked;
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
  Test('Generated names are reproducible across compilations',
    TestGeneratedNamesPerCompilation);
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
  Test('Anonymous derived class in a block keeps later locals apart',
    TestAnonymousDerivedClassInBlockKeepsLaterLocalsApart);
  Test('Script top-level declarations compile in linear time',
    TestScriptTopLevelDeclarationsCompileInLinearTime);
  Test('Module top-level declarations compile in linear time',
    TestModuleTopLevelDeclarationsCompileInLinearTime);
  Test('Top-level calls among many bindings compile in linear time',
    TestTopLevelCallsCompileInLinearTime);
  Test('A closure that captures many bindings compiles in linear time',
    TestManyCapturedBindingsCompileInLinearTime);
  Test('Direct eval code among many captured bindings compiles in linear time',
    TestDirectEvalAmongManyCapturedBindingsCompilesInLinearTime);
  Test('Parameter lists parse and compile in linear time',
    TestParameterListsParseAndCompileInLinearTime);
  Test('Alternating unary and binary expressions use linear type work', TestAlternatingExpressionWork);
  Test('Right-nested logical initializers use linear folding work', TestLogicalInitializerWork);
  Test('Direct eval records use linear name lookup work', TestDirectEvalRecordWork);
  Test('Constructor type argument probes use linear token work', TestNewTypeArgumentWork);
  Test('Destructuring patterns compile in linear time',
    TestDestructuringPatternsCompileInLinearTime);
  Test('Match-pattern binding-name checks scale with the bindings',
    TestMatchBindingNameChecksScale);
  Test('Default parameter parsing and initializer work scale with the parameters',
    TestDefaultParameterListWorkScales);
  Test('Large switches keep local lookup probes proportional to clauses',
    TestLargeSwitchLookupProbesScale);
  Test('Ordinary parameters do not repeat closure-source destination proofs',
    TestOrdinaryParametersSkipInitializerSourceChecks);
  Test('Direct eval string-name checks scale with the bindings',
    TestDirectEvalNameChecksScale);
  Test('Chains of binary operators compile in time linear in their length',
    TestOperatorChainsCompileInLinearTime);
  Test('Chains of binary operators fold the constants they start with',
    TestOperatorChainsFoldTheirConstantStart);
  Test('Right-nested and unary chains compile in time linear in their length',
    TestNestedChainsCompileInLinearTime);
  Test('Right-nested and unary chains fold the constant operands inside them',
    TestNestedChainsFoldTheirConstantOperands);
  Test('A switch with a declaration in each case clause compiles in linear time',
    TestSwitchCaseDeclarationsCompileInLinearTime);
  Test('TypeScript comparison chains parse in time linear in the code after them',
    TestTypeScriptComparisonChainsParseInLinearTime);
  Test('A branch past a 16-bit jump offset compiles each node once',
    TestLongBranchesCompileOnce);
  Test('SlotHoldsLocal follows the declared and dropped locals',
    TestSlotHoldsLocalFollowsTheLocals);
  Test('Ending a block restores the binding it shadowed',
    TestEndingABlockRestoresTheBindingItShadowed);
  Test('Var and slot lookups find the oldest local of a name',
    TestVarAndSlotLookupsFindTheOldestLocal);
  Test('Float constants are pooled by bit pattern',
    TestFloatConstantsArePooledByBitPattern);
  Test('Locals a name skips resolve in constant time',
    TestSkippedLocalsResolveInConstantTime);
  Test('Distinct float constants compile in linear time',
    TestDistinctFloatConstantsCompileInLinearTime);
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
  Test('A function without direct eval reads let and parameter operands ' +
    'from their registers', TestFunctionWithoutDirectEvalReadsOperandRegisters);
  Test('A function that contains a direct eval keeps operand copies',
    TestDirectEvalInFunctionKeepsOperandCopies);
  Test('A direct eval site record lists every binding visible at the call',
    TestDirectEvalSiteRecordListsCallSiteBindings);
  Test('A direct eval site record takes the strictness of its call',
    TestDirectEvalSiteRecordTakesCallStrictness);
  Test('The direct eval site record check rejects a record that differs from the scope chain',
    TestDirectEvalSiteRecordCheckRejectsMismatches);
  Test('A direct eval captures the bindings of the calling function',
    TestDirectEvalCapturesCallSiteLocals);
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
  Test('Compound assignment to a register-only binding writes it directly',
    TestCompoundAssignmentWritesRegisterOnlyBindingDirectly);
  Test('Initializer that reaches its binding keeps the copy',
    TestInitializerReachingItsBindingKeepsCopy);
  Test('Lexical TDZ holes are set once, and only where observable',
    TestLexicalHolesSetOnlyWhereObservable);
  Test('compound += and -= of a small integer use immediates',
    TestCompoundSmallIntegerUsesImmediate);
  Test('this property read uses local register',
    TestThisPropertyReadUsesLocalRegister);
  Test('this property read retains derived-constructor guard',
    TestThisPropertyReadRetainsDerivedGuard);
  Test('local property read uses fused opcode',
    TestLocalPropertyReadUsesFusedOpcode);
  Test('optional local property read skips fused opcode',
    TestOptionalLocalPropertyReadSkipsFusedOpcode);
  Test('async generator return awaits before the try cleanup',
    TestAsyncGeneratorReturnAwaitsBeforeTryCleanup);
  Test('other function kinds return without an await',
    TestOtherFunctionKindsReturnWithoutAwait);
  Test('for await close awaits the return result',
    TestForAwaitCloseAwaitsReturnResult);
  Test('Static import loads scale with declarations',
    TestStaticImportLoadsScaleWithDeclarations);
  Test('Binary round-trip', TestBinaryRoundTrip);
  Test('Object rest reserves both collection operand registers',
    TestObjectRestBytecodeRoundTrip);
  Test('Binary round-trip closed numeric self-call',
    TestBinaryRoundTripClosedNumericSelfCall);
  Test('Binary round-trip upvalue names', TestBinaryRoundTripUpvalueNames);
  Test('Binary round-trip function declaration position',
    TestBinaryRoundTripFunctionDeclarationPosition);
  Test('Binary little-endian format', TestBinaryLittleEndian);
  Test('Binary round-trip constants', TestBinaryRoundTripConstants);
  Test('Binary round-trip keeps +0 and -0 apart',
    TestBinaryRoundTripKeepsBothZeros);
  Test('Binary loader rejects malformed artifacts',
    TestBinaryRejectsMalformedArtifacts);
  Test('Undeclared private name raises SyntaxError', TestUndeclaredPrivateNameRaisesSyntaxError);
  Test('A bytecode limit raises a SyntaxError at the source that reached it',
    TestBytecodeLimitRaisesSyntaxErrorAtSource);
  Test('A long branch takes the long jump form in its own function only',
    TestLongBranchTakesLongFormInItsFunctionOnly);
  Test('A compile after compiles that raised starts at nesting depth 0',
    TestCompileAfterRaisedCompileStartsAtNestingDepthZero);
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
  Test('Const propagation skips global-backed reads under a sloppy direct eval',
    TestConstPropagationSkipsGlobalBackedUnderSloppyDirectEval);
  Test('Global-backed reads throw for a missing binding; typeof does not',
    TestGlobalBackedReadsThrowForAMissingBinding);
  Test('A sloppy function folds an enclosing const unless it contains a sloppy direct eval',
    TestSloppyFunctionFoldsEnclosingConstWithoutDirectEval);
  Test('A direct eval site record names where the eval''s vars go',
    TestDirectEvalSiteRecordNamesVarEnvironment);
  Test('A direct eval site record names the private environment at the call',
    TestDirectEvalSiteRecordNamesPrivateEnvironment);
  Test('Direct eval code compiles as a closure of its strict call site',
    TestDirectEvalCompilesAsClosureOfCallSite);
  Test('Const propagation skips global-backed BigInt bindings',
    TestConstPropagationSkipsGlobalBackedBigInt);
  Test('Inferred numeric locals use typed arithmetic', TestInferredNumericLocalsUseTypedArithmetic);
  Test('Annotated parameters use typed arithmetic', TestAnnotatedParametersUseTypedArithmetic);
  Test('Closed numeric Fibonacci uses superinstructions',
    TestClosedNumericFibonacciUsesSuperinstructions);
  Test('Concise arrow body marks tail calls',
    TestConciseArrowBodyMarksTailCalls);
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
  Test('Unchecked strict binding does not type operands',
    TestUncheckedStrictBindingDoesNotTypeOperands);
  Test('Binary operator result types', TestBinaryOperatorResultTypes);
  Test('Bitwise operator result types', TestBitwiseOperatorResultTypes);
  Test('Unary operator result types', TestUnaryOperatorResultTypes);
  Test('Integer type comes only from int32 and uint32 results',
    TestIntegerTypeComesOnlyFromInt32Results);
  Test('Math calls are not trusted', TestMathCallsAreNotTrusted);
  Test('Const type comes from the number proof',
    TestConstTypeComesFromTheNumberProof);
  Test('Let keeps its type only when every value is a Number',
    TestLetKeepsItsTypeOnlyWhenEveryValueIsANumber);
  Test('Captured const keeps its type', TestCapturedConstKeepsItsType);
  Test('Const function return type types calls',
    TestConstFunctionReturnTypeTypesCalls);
  Test('Call result stays untyped when the return type is not proven',
    TestCallResultStaysUntypedWhenReturnTypeIsNotProven);
  Test('Fall-through makes the return type untyped',
    TestFallThroughMakesTheReturnTypeUntyped);
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
  Test('comparisons with an Int16 constant use immediate jumps',
    TestConstantOperandComparisonsUseImmediateJumps);
  Test('comparisons outside the immediate forms keep generic compares',
    TestConstantOperandComparisonsKeepGenericForms);
  Test('a comparison with a constant past Int64 compiles with invalid ' +
    'operations unmasked', TestHugeConstantComparisonCompilesWithInvalidOpUnmasked);
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
  const AFunctionKeyword: Boolean;
  const ALabels: Boolean;
  const AWhileLoops: Boolean; const AFileName: string): TGocciaBytecodeModule;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  SourceLines: TStringList;
  Options: TGocciaCompilerOptimizationOptions;
  ParserOptions: TGocciaParserOptions;
  Profile: TGocciaCompileProfile;
begin
  Lexer := TGocciaLexer.Create(ASource, AFileName);
  SourceLines := CreateTextLines(ASource);
  Parser := TGocciaParser.CreateFromLexer(Lexer, AFileName, SourceLines);
  if ATraditionalForLoops or AVarDeclarations or ANonStrictMode or
     AFunctionKeyword or ALabels or AWhileLoops then
  begin
    ParserOptions := Parser.Options;
    ParserOptions.FunctionDeclarationsEnabled := AFunctionKeyword;
    ParserOptions.LabelStatementsEnabled := ALabels;
    ParserOptions.WhileLoopsEnabled := AWhileLoops;
    ParserOptions.TraditionalForLoopsEnabled := ATraditionalForLoops;
    ParserOptions.VarDeclarationsEnabled := AVarDeclarations;
    // The parser marks a function that contains a sloppy direct eval; the
    // compiler relies on it when it compiles that function as sloppy code.
    ParserOptions.NonStrictModeEnabled := ANonStrictMode;
    Parser.ApplyOptions(ParserOptions);
  end;
  ProgramNode := Parser.Parse;

  Compiler := TGocciaCompiler.Create(AFileName);
  try
    Compiler.StrictTypes := AStrictTypes;
    Compiler.GlobalBackedTopLevel := AGlobalBackedTopLevel;
    Compiler.NonStrictMode := ANonStrictMode;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape := APreserveCoverageShape;
    Options.EnableConstantFolding := AEnableConstantFolding;
    Options.EnableConstPropagation := AEnableConstPropagation;
    Options.EnableDeadBranchElimination := AEnableDeadBranchElimination;
    Compiler.OptimizationOptions := Options;
    Result := Compiler.Compile(ProgramNode);
    Profile := Default(TGocciaCompileProfile);
    Profile.StrictTypes := AStrictTypes;
    if ATraditionalForLoops then
      Profile.CompatibilityMask := Profile.CompatibilityMask or BYTECODE_COMPAT_TRADITIONAL_FOR;
    if AVarDeclarations then
      Profile.CompatibilityMask := Profile.CompatibilityMask or BYTECODE_COMPAT_VAR;
    if ANonStrictMode then
      Profile.CompatibilityMask := Profile.CompatibilityMask or BYTECODE_COMPAT_NON_STRICT;
    Result.CompileProfile := Profile;
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

function TTestCompiler.CountNamedGlobalReads(
  const ATemplate: TGocciaFunctionTemplate): Integer;
begin
  Result := CountOp(ATemplate, OP_GET_GLOBAL) +
    CountOp(ATemplate, OP_GET_GLOBAL_DECLARED);
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

// ES2026 §14.10.1 step 3: the value is awaited while the try is still
// active, so the await precedes the handler pop and the inlined finally, and
// the return does not await it again.
procedure TTestCompiler.TestAsyncGeneratorReturnAwaitsBeforeTryCleanup;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
  I, AwaitIndex, PopIndex, ReturnCount: Integer;
  Instruction: UInt32;
begin
  Module := CompileSource(
    'const holder = {' +
    '  async *values() {' +
    '    try { return holder; } finally { holder.done = true; }' +
    '  }' +
    '};');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_AWAIT);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      AwaitIndex := -1;
      PopIndex := -1;
      ReturnCount := 0;
      for I := 0 to Func.CodeCount - 1 do
      begin
        Instruction := Func.GetInstruction(I);
        case TGocciaOpCode(DecodeOp(Instruction)) of
          OP_AWAIT:
            if AwaitIndex < 0 then
              AwaitIndex := I;
          OP_POP_HANDLER:
            if PopIndex < 0 then
              PopIndex := I;
          OP_RETURN:
            begin
              Inc(ReturnCount);
              Expect<Integer>(DecodeB(Instruction)).ToBe(0);
            end;
        end;
      end;
      Expect<Integer>(CountOp(Func, OP_AWAIT)).ToBe(1);
      Expect<Boolean>(PopIndex > AwaitIndex).ToBe(True);
      Expect<Boolean>(ReturnCount > 0).ToBe(True);
    end;
  finally
    Module.Free;
  end;
end;

// A sync generator, an async function and a bare return compile no await;
// an async generator's return that leaves no try keeps OP_RETURN's own await
// (B = 1), since nothing runs between that await and the return.
// ES2026 §7.4.14 AsyncIteratorClose: a for-await close calls return(), skips
// the Await when there is no return method (step 4.b), awaits the result as a
// suspension (step 4.d) and checks it only for a completion that is not a
// throw (steps 5 and 7). The break exit and the throw handler each close once.
procedure TTestCompiler.TestForAwaitCloseAwaitsReturnResult;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
  Ops: array of Integer;
  I, OpCount, CallCount, CheckCount, AwaitCount: Integer;
  Instruction: UInt32;
begin
  Module := CompileSource(
    'const holder = {' +
    '  async read(source) {' +
    '    for await (const x of source) { break; }' +
    '  }' +
    '};');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_ITER_CLOSE);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      // Opcodes in order, without OP_WIDE prefixes; the close modes are
      // recorded as 1000 + C.
      SetLength(Ops, Func.CodeCount);
      OpCount := 0;
      for I := 0 to Func.CodeCount - 1 do
      begin
        Instruction := Func.GetInstruction(I);
        if TGocciaOpCode(DecodeOp(Instruction)) = OP_WIDE then
          Continue;
        if TGocciaOpCode(DecodeOp(Instruction)) = OP_ITER_CLOSE then
          Ops[OpCount] := 1000 + DecodeC(Instruction)
        else
          Ops[OpCount] := DecodeOp(Instruction);
        Inc(OpCount);
      end;
      CallCount := 0;
      CheckCount := 0;
      AwaitCount := 0;
      for I := 0 to OpCount - 1 do
        if Ops[I] = Ord(OP_AWAIT) then
          Inc(AwaitCount)
        else if Ops[I] = 1000 + ITER_CLOSE_ASYNC_RETURN then
        begin
          Inc(CallCount);
          Expect<Boolean>(I + 2 < OpCount).ToBe(True);
          if I + 2 < OpCount then
          begin
            Expect<Integer>(Ops[I + 1]).ToBe(Ord(OP_JUMP_IF_NULLISH));
            Expect<Integer>(Ops[I + 2]).ToBe(Ord(OP_AWAIT));
          end;
        end
        else if Ops[I] = 1000 + ITER_CLOSE_ASYNC_RESULT then
        begin
          Inc(CheckCount);
          Expect<Integer>(Ops[I - 1]).ToBe(Ord(OP_AWAIT));
        end
        else if Ops[I] >= 1000 then
          // A for-await close uses only the async close modes.
          Expect<Integer>(Ops[I]).ToBe(1000 + ITER_CLOSE_ASYNC_RETURN);
      Expect<Integer>(CallCount).ToBe(2);
      Expect<Integer>(CheckCount).ToBe(1);
      // next()'s await and one per close.
      Expect<Integer>(AwaitCount).ToBe(3);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestOtherFunctionKindsReturnWithoutAwait;

  // OP_RETURNs with a nonzero B operand in ATemplate and the functions
  // nested in it.
  function CountAwaitingReturns(
    const ATemplate: TGocciaFunctionTemplate): Integer;
  var
    I: Integer;
    Instruction: UInt32;
  begin
    Result := 0;
    for I := 0 to ATemplate.CodeCount - 1 do
    begin
      Instruction := ATemplate.GetInstruction(I);
      if (TGocciaOpCode(DecodeOp(Instruction)) = OP_RETURN) and
         (DecodeB(Instruction) <> 0) then
        Inc(Result);
    end;
    for I := 0 to ATemplate.FunctionCount - 1 do
      Inc(Result, CountAwaitingReturns(ATemplate.GetFunction(I)));
  end;

var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource(
    'const holder = {' +
    '  *values() {' +
    '    try { return holder; } finally { holder.done = true; }' +
    '  },' +
    '  async read() {' +
    '    try { return holder; } finally { holder.done = true; }' +
    '  },' +
    '  async *empty() {' +
    '    try { return; } finally { holder.done = true; }' +
    '  },' +
    '  async *outside() { return holder; }' +
    '};');
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_AWAIT)).ToBe(0);
    Expect<Integer>(CountAwaitingReturns(Module.TopLevel)).ToBe(1);
  finally
    Module.Free;
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

{ The fastest of LINEAR_TRIALS compiles of ACount lines, each ALinePattern
  formatted with its index, between the lines APrefix and ASuffix; the parse
  is not timed. }
function TTestCompiler.CompileNanoseconds(const ALinePattern: string;
  const AGlobalBackedTopLevel: Boolean; const ACount: Integer;
  const APrefix, ASuffix: string): Int64;
var
  Source: TStringBuffer;
  SourceText: string;
  I: Integer;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ParserOptions: TGocciaParserOptions;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  Module: TGocciaBytecodeModule;
  SourceLines: TStringList;
  Start, Elapsed: Int64;
  Trial: Integer;
begin
  Source := TStringBuffer.Create;
  if APrefix <> '' then
    Source.Append(APrefix + LineEnding);
  for I := 0 to ACount - 1 do
    Source.Append(Format(ALinePattern, [I]) + LineEnding);
  if ASuffix <> '' then
    Source.Append(ASuffix + LineEnding);
  SourceText := Source.ToString;
  Result := High(Int64);
  for Trial := 1 to LINEAR_TRIALS do
  begin
    Lexer := TGocciaLexer.Create(SourceText, '<test>');
    SourceLines := CreateTextLines(SourceText);
    Parser := TGocciaParser.CreateFromLexer(Lexer, '<test>', SourceLines);
    Compiler := TGocciaCompiler.Create('<test>');
    ProgramNode := nil;
    Module := nil;
    try
      ParserOptions := Parser.Options;
      ParserOptions.VarDeclarationsEnabled := True;
      ParserOptions.FunctionDeclarationsEnabled := True;
      Parser.ApplyOptions(ParserOptions);
      ProgramNode := Parser.Parse;
      Compiler.GlobalBackedTopLevel := AGlobalBackedTopLevel;
      Start := GetNanoseconds;
      Module := Compiler.Compile(ProgramNode);
      Elapsed := GetNanoseconds - Start;
    finally
      Module.Free;
      Compiler.Free;
      ProgramNode.Free;
      Parser.Free;
      SourceLines.Free;
      Lexer.Free;
    end;
    if Elapsed < Result then
      Result := Elapsed;
  end;
end;

{ A kind of top-level declaration against ABaseline, the compile of as many
  module-level `const`s on the same machine: a wall-clock bound would depend
  on the machine, a ratio does not. }
procedure TTestCompiler.ExpectTopLevelCompileLinear(const ABaseline: Int64;
  const ALinePattern: string; const AGlobalBackedTopLevel: Boolean);
var
  Elapsed: Int64;
begin
  Elapsed := CompileNanoseconds(ALinePattern, AGlobalBackedTopLevel);
  if Elapsed > LINEAR_COMPILE_RATIO * ABaseline + LINEAR_SLACK_NANOSECONDS then
    Fail(Format('%d declarations "%s" took %d ms to compile against %d ms ' +
      'for module-level consts', [LINEAR_DECLARATION_COUNT, ALinePattern,
      Elapsed div 1000000, ABaseline div 1000000]));
  Expect<Boolean>(Elapsed <=
    LINEAR_COMPILE_RATIO * ABaseline + LINEAR_SLACK_NANOSECONDS).ToBe(True);
end;

procedure TTestCompiler.TestScriptTopLevelDeclarationsCompileInLinearTime;
var
  Baseline: Int64;
begin
  Baseline := CompileNanoseconds('const a%0:d = %0:d;', False);
  ExpectTopLevelCompileLinear(Baseline, 'const a%0:d = %0:d;', True);
  ExpectTopLevelCompileLinear(Baseline, 'let a%0:d = %0:d;', True);
  ExpectTopLevelCompileLinear(Baseline, 'var a%0:d = %0:d;', True);
  ExpectTopLevelCompileLinear(Baseline, 'var a%0:d;', True);
  ExpectTopLevelCompileLinear(Baseline, 'function a%0:d() {}', True);
  ExpectTopLevelCompileLinear(Baseline, 'class A%0:d {}', True);
end;

procedure TTestCompiler.TestModuleTopLevelDeclarationsCompileInLinearTime;
var
  Baseline: Int64;
begin
  Baseline := CompileNanoseconds('const a%0:d = %0:d;', False);
  ExpectTopLevelCompileLinear(Baseline, 'let a%0:d = %0:d;', False);
  ExpectTopLevelCompileLinear(Baseline, 'var a%0:d = %0:d;', False);
  ExpectTopLevelCompileLinear(Baseline, 'function a%0:d() {}', False);
  ExpectTopLevelCompileLinear(Baseline, 'class A%0:d {}', False);
end;

{ A call compiled at a function's top level asks whether its destination
  register holds a local. Answering by visiting every local made each call
  cost time linear in the bindings before it, so a top-level call per
  binding compiled in quadratic time (about 12 s here for 15,000). Each
  shape is timed against the same declarations followed by a property read
  of about the call's cost, which compiles no call. }
procedure TTestCompiler.TestTopLevelCallsCompileInLinearTime;

  procedure ExpectCallsCompileLinear(const ADeclarations, ARead,
    ACall: string; const AGlobalBackedTopLevel: Boolean);
  var
    Baseline, Elapsed: Int64;
  begin
    Baseline := CompileNanoseconds(ADeclarations + ' ' + ARead,
      AGlobalBackedTopLevel);
    Elapsed := CompileNanoseconds(ADeclarations + ' ' + ACall,
      AGlobalBackedTopLevel);
    if Elapsed > LINEAR_COMPILE_RATIO * Baseline +
       LINEAR_SLACK_NANOSECONDS then
      Fail(Format('%d lines "%s %s" took %d ms to compile against %d ms ' +
        'with "%s" in place of the call', [LINEAR_DECLARATION_COUNT,
        ADeclarations, ACall, Elapsed div 1000000, Baseline div 1000000,
        ARead]));
    Expect<Boolean>(Elapsed <=
      LINEAR_COMPILE_RATIO * Baseline + LINEAR_SLACK_NANOSECONDS).ToBe(True);
  end;

begin
  ExpectCallsCompileLinear('let a%0:d = %0:d;', 'f.a%0:d;', 'f(a%0:d);',
    False);
  ExpectCallsCompileLinear('let a%0:d = %0:d;', 'f.a%0:d;', 'f(a%0:d);',
    True);
  // Functions that write a module-scope binding, each called at top level.
  ExpectCallsCompileLinear(
    'let a%0:d = 0; const w%0:d = () => { a%0:d = 1; };', 'w%0:d.a;',
    'w%0:d();', False);
end;

// A function of p with ABindings `let` bindings x0, x1, ..., and a closure
// h in it that reads them in turn, AReads reads in all.
function CapturedReadsSource(const ABindings, AReads: Integer): string;
var
  Source: TStringBuffer;
  Index: Integer;
begin
  Source := TStringBuffer.Create;
  Source.Append('const f = (p) => {' + LineEnding);
  for Index := 0 to ABindings - 1 do
    Source.Append(Format('let x%d = p + %d;', [Index, Index]) + LineEnding);
  Source.Append('const h = () => {' + LineEnding);
  for Index := 0 to AReads - 1 do
    Source.Append(Format('x%d;', [Index mod ABindings]) + LineEnding);
  Source.Append('};' + LineEnding + 'return h; };' + LineEnding);
  Result := Source.ToString;
end;

{ A closure resolves a captured binding at each read, and finding its
  upvalue visited every upvalue before it: a closure reading thousands of
  captured bindings compiled in time quadratic in them. }
procedure TTestCompiler.TestManyCapturedBindingsCompileInLinearTime;
var
  Few, Many: TGocciaCompileCounters;
begin
  Few := SourceCompileCounters(CapturedReadsSource(CAPTURE_FEW_BINDINGS,
    CAPTURE_READS));
  Many := SourceCompileCounters(CapturedReadsSource(CAPTURE_MANY_BINDINGS,
    CAPTURE_READS));
  ExpectWorkLinear(Few, Many, ccUpvalueListProbes);
end;

{ Direct eval code is compiled against its call site's record, which seeds
  the calling closure's upvalues, and each name the eval code reads was
  looked up among them by visiting them in turn. }
procedure TTestCompiler.TestDirectEvalAmongManyCapturedBindingsCompilesInLinearTime;

  // The eval code reads CAPTURE_READS div 4 bindings in turn, from a call
  // site whose record lists ABindings bindings of enclosing functions, which
  // the eval's caller has as upvalues. The record is built here: compiling
  // a caller with that many bindings takes long for another reason.
  function EvalCompileNanoseconds(const ABindings: Integer): Int64;
  var
    Reads: TStringBuffer;
    EvalSource: string;
    Caller, EvalModule: TGocciaBytecodeModule;
    Site: TGocciaFunctionTemplate;
    Env: TGocciaDirectEvalEnvironment;
    Lexer: TGocciaLexer;
    Parser: TGocciaParser;
    ProgramNode: TGocciaProgram;
    Compiler: TGocciaCompiler;
    SourceLines: TStringList;
    Index, Trial: Integer;
    Start, Elapsed: Int64;
  begin
    Reads := TStringBuffer.Create;
    for Index := 0 to CAPTURE_READS div 4 - 1 do
      Reads.Append(Format('x%d;', [Index mod ABindings]));
    EvalSource := Reads.ToString;
    Result := High(Int64);
    Caller := CompileSource('const f = () => () => eval("x");');
    try
      Site := FindTemplateWithDirectEval(Caller.TopLevel);
      Expect<Boolean>(Assigned(Site)).ToBe(True);
      if not Assigned(Site) then
        Exit;
      Env := Site.GetDirectEvalEnvironment(0);
      SetLength(Env.Bindings, ABindings);
      for Index := 0 to ABindings - 1 do
      begin
        Env.Bindings[Index] := Default(TGocciaDirectEvalBindingInfo);
        Env.Bindings[Index].Name := Format('x%d', [Index]);
        Env.Bindings[Index].Kind := debUpvalue;
        Env.Bindings[Index].Index := Index;
      end;
      for Trial := 1 to LINEAR_TRIALS do
      begin
        Lexer := TGocciaLexer.Create(EvalSource, '<eval>');
        SourceLines := CreateTextLines(EvalSource);
        Parser := TGocciaParser.CreateFromLexer(Lexer, '<eval>', SourceLines);
        ProgramNode := Parser.Parse;
        Compiler := TGocciaCompiler.Create('<eval>');
        EvalModule := nil;
        try
          Start := GetNanoseconds;
          EvalModule := Compiler.CompileDirectEval(ProgramNode, Env, True,
            nil);
          Elapsed := GetNanoseconds - Start;
          Expect<Integer>(EvalModule.TopLevel.UpvalueCount).ToBe(ABindings);
        finally
          EvalModule.Free;
          Compiler.Free;
          ProgramNode.Free;
          Parser.Free;
          SourceLines.Free;
          Lexer.Free;
        end;
        if Elapsed < Result then
          Result := Elapsed;
      end;
    finally
      Caller.Free;
    end;
  end;

var
  Few, Many: Int64;
begin
  Few := EvalCompileNanoseconds(CAPTURE_FEW_BINDINGS);
  Many := EvalCompileNanoseconds(CAPTURE_MANY_BINDINGS);
  if Many > CHAIN_COMPILE_RATIO * Few + LINEAR_SLACK_NANOSECONDS then
    Fail(Format('Eval code reading %d of %d captured bindings took %d ms to ' +
      'compile against %d ms among %d', [CAPTURE_READS div 4,
      CAPTURE_MANY_BINDINGS, Many div 1000000, Few div 1000000,
      CAPTURE_FEW_BINDINGS]));
  Expect<Boolean>(Many <=
    CHAIN_COMPILE_RATIO * Few + LINEAR_SLACK_NANOSECONDS).ToBe(True);
end;

{ Read the same work counters exposed by --profile=compile. Summing all
  function records includes parser work as well as nested compiler work. }
function TTestCompiler.SourceCompileCounters(const ASource: string;
  const AStrictTypes: Boolean; const AFileName: string;
  const APreserveCoverageShape: Boolean;
  const AGlobalBackedTopLevel: Boolean): TGocciaCompileCounters;
var
  Profile: TGocciaCompileProfiler;
  Module: TGocciaBytecodeModule;
  I: Integer;
  Counter: TGocciaCompileCounter;
begin
  Result := Default(TGocciaCompileCounters);
  TGocciaCompileProfiler.Initialize;
  try
    Profile := TGocciaCompileProfiler.Instance;
    Module := CompileSource(ASource, AStrictTypes, APreserveCoverageShape,
      AGlobalBackedTopLevel, True, True, True, True, False, True, True, False, False, AFileName);
    Module.Free;
    for I := 0 to Profile.FunctionCount - 1 do
      for Counter := Low(Counter) to High(Counter) do
        Inc(Result[Counter], Profile.GetFunction(I).Counters[Counter]);
  finally
    TGocciaCompileProfiler.Shutdown;
  end;
end;

procedure TTestCompiler.ExpectWorkLinear(const AShort, ALong: TGocciaCompileCounters;
  const ACounter: TGocciaCompileCounter);
begin
  if ALong[ACounter] > AShort[ACounter] * 5 div 2 then
    Fail(Format('%s grew from %d to %d, exceeding the 2.5x work bound',
      [COMPILE_COUNTER_JSON_NAMES[ACounter], AShort[ACounter], ALong[ACounter]]));
  Expect<Boolean>(AShort[ccNodesCompiled] > 0).ToBe(True);
  Expect<Boolean>(ALong[ACounter] <= AShort[ACounter] * 5 div 2).ToBe(True);
end;

procedure TTestCompiler.TestAlternatingExpressionWork;
const
  PREFIXES: array[0..1] of string = ('p + -(', 'p + -(1 + -(');
var
  Small, Large: TGocciaCompileCounters;
  Strict, Coverage: Boolean;
  I, Depth: Integer;
begin
  for I := Low(PREFIXES) to High(PREFIXES) do
    for Strict := False to True do
      for Coverage := False to True do
      begin
        Depth := I + 1;
        Small := SourceCompileCounters('const f = (p) => ' + DupeString(PREFIXES[I], 100) +
          'p' + StringOfChar(')', 100 * Depth) + ';', Strict, '<test>', Coverage);
        Large := SourceCompileCounters('const f = (p) => ' + DupeString(PREFIXES[I], 200) +
          'p' + StringOfChar(')', 200 * Depth) + ';', Strict, '<test>', Coverage);
        ExpectWorkLinear(Small, Large, ccExpressionTypeVisits);
      end;
end;

procedure TTestCompiler.TestLogicalInitializerWork;
const
  PREFIXES: array[0..2] of string = ('true && (', 'false || (', 'null ?? (');
var
  Small, Large: TGocciaCompileCounters;
  Strict, Coverage: Boolean;
  Prefix: string;
begin
  for Prefix in PREFIXES do
    for Strict := False to True do
      for Coverage := False to True do
      begin
        Small := SourceCompileCounters('const f = (p) => { const x = ' +
          DupeString(Prefix, 100) + 'p' + StringOfChar(')', 100) +
          '; return x; };', Strict, '<test>', Coverage);
        Large := SourceCompileCounters('const f = (p) => { const x = ' +
          DupeString(Prefix, 200) + 'p' + StringOfChar(')', 200) +
          '; return x; };', Strict, '<test>', Coverage);
        ExpectWorkLinear(Small, Large, ccConstantEvaluationVisits);
      end;
end;

procedure TTestCompiler.TestDirectEvalRecordWork;
  function Source(const ACount: Integer): string;
  var
    Buffer: TStringBuffer;
    I: Integer;
  begin
    Buffer := TStringBuffer.Create;
    Buffer.Append('const f = () => {');
    for I := 0 to ACount - 1 do
      Buffer.Append(Format('let b%d;', [I]));
    Buffer.Append('eval(""); };');
    Result := Buffer.ToString;
  end;
var
  Small, Large: TGocciaCompileCounters;
begin
  Small := SourceCompileCounters(Source(256));
  Large := SourceCompileCounters(Source(512));
  ExpectWorkLinear(Small, Large, ccDirectEvalRecordProbes);
end;

procedure TTestCompiler.TestNewTypeArgumentWork;
const
  FILE_NAMES: array[0..1] of string = ('probe.js', 'probe.ts');
var
  Small, Large: TGocciaCompileCounters;
  FileName: string;
begin
  for FileName in FILE_NAMES do
  begin
    Small := SourceCompileCounters('const f = (X) => ' + DupeString('new X < ', 100) + '0;',
      False, FileName);
    Large := SourceCompileCounters('const f = (X) => ' + DupeString('new X < ', 200) + '0;',
      False, FileName);
    ExpectWorkLinear(Small, Large, ccTypeArgumentProbeTokens);
  end;
end;

{ Compare the work of the same name list at two sizes. Wall time on shared
  runners is not a work bound, even when each size takes its fastest trial.
  The aggregate includes parser membership and compiler name/type work;
  individual hash probe ratios fluctuate as table occupancy changes. }
procedure TTestCompiler.ExpectNameListLinear(const APattern, AItem: string;
  const ACount: Integer);

  function NameList(const ACount: Integer): string;
  var
    Buffer: TStringBuffer;
    I: Integer;
  begin
    Buffer := TStringBuffer.Create;
    for I := 0 to ACount - 1 do
    begin
      if I > 0 then
        Buffer.Append(', ');
      Buffer.Append(Format(AItem, [I]));
    end;
    Result := Buffer.ToString;
  end;

  function Work(const ACount: Integer): Int64;
  var
    Counters: TGocciaCompileCounters;
    Counter: TGocciaCompileCounter;
  begin
    Counters := SourceCompileCounters(Format(APattern, [NameList(ACount)]));
    Result := 0;
    for Counter := Low(Counter) to High(Counter) do
      Inc(Result, Counters[Counter]);
    Expect<Boolean>(Counters[ccNodesCompiled] > 0).ToBe(True);
    Expect<Boolean>(Counters[ccBindingNameLookups] > 0).ToBe(True);
  end;

var
  ShortWork, LongWork: Int64;
begin
  ShortWork := Work(ACount);
  LongWork := Work(ACount * 2);
  if LongWork > ShortWork * NAME_LIST_DOUBLING_PERCENT div 100 then
    Fail(Format('"%s" with %d items "%s" did %d operations versus %d with %d',
      [APattern, ACount * 2, AItem, LongWork, ShortWork, ACount]));
  Expect<Boolean>(LongWork <= ShortWork * NAME_LIST_DOUBLING_PERCENT div 100).ToBe(True);
end;

{ The parse checks a parameter list's names for duplicates (ES2026 §15.1.1,
  §15.3.1) and the compiler records them for the body (§10.2.11 step 5), each
  once per name. }
procedure TTestCompiler.TestParameterListsParseAndCompileInLinearTime;
begin
  ExpectNameListLinear('const g = (%s) => a0;', 'a%d');
  ExpectNameListLinear('const g = async (%s, ...rest) => rest;', 'a%d');
  ExpectNameListLinear('const g = function (%s) { return a0; };', 'a%d');
  ExpectNameListLinear('function g(%s) { return a0; }', 'a%d');
  ExpectNameListLinear('const o = { m(%s) { return a0; } };', 'a%d');
  ExpectNameListLinear('class C { m(%s) { return a0; } }', 'a%d');
  // A pattern parameter of an async arrow is searched for `await`; twice
  // NAME_LIST_COUNT names would pass the parameter initialization's
  // instruction limit.
  ExpectNameListLinear('const g = async ({ %s }) => a0;', 'a%d',
    NAME_LIST_COUNT div 2);
end;

procedure TTestCompiler.TestMatchBindingNameChecksScale;

  function Probes(const ACount: Integer): Int64;
  var
    Source: TStringBuffer;
    I: Integer;
    Work: TGocciaCompileCounters;
  begin
    Source := TStringBuffer.Create;
    Source.Append('const g = (o) => o is { ');
    for I := 0 to ACount - 1 do
    begin
      if I > 0 then
        Source.Append(', ');
      Source.Append(Format('k%0:d: const b%0:d', [I]));
    end;
    Source.Append(' };');
    Work := SourceCompileCounters(Source.ToString);
    Result := Work[ccBindingNameProbes];
  end;

const
  BINDING_COUNTS: array[0..2] of Integer = (4000, 8000, 16000);
var
  ShortProbes, LongProbes: Int64;
  BindingCount: Integer;
begin
  for BindingCount in BINDING_COUNTS do
  begin
    ShortProbes := Probes(BindingCount);
    LongProbes := Probes(2 * BindingCount);
    // Small maps missed the residual hash clustering at 16k/32k names.
    Expect<Boolean>(ShortProbes >= BindingCount).ToBe(True);
    Expect<Boolean>(LongProbes >= 2 * BindingCount).ToBe(True);
    if LongProbes > ShortProbes * 5 div 2 then
      Fail(Format('Match pattern with %d bindings took %d name probes versus %d ' +
        'with %d', [2 * BindingCount, LongProbes, ShortProbes, BindingCount]));
    Expect<Boolean>(LongProbes <= ShortProbes * 5 div 2).ToBe(True);
  end;
end;

procedure TTestCompiler.TestDefaultParameterListWorkScales;

  function Counters(const ACount: Integer;
    const AInitializer: string): TGocciaCompileCounters;
  var
    Source: TStringBuffer;
    I: Integer;
  begin
    Source := TStringBuffer.Create;
    Source.Append('const g = (');
    for I := 0 to ACount - 1 do
    begin
      if I > 0 then
        Source.Append(', ');
      Source.Append(Format('p%d = %s', [I, AInitializer]));
    end;
    Source.Append(') => 1;');
    Result := SourceCompileCounters(Source.ToString);
  end;

  procedure CheckInitializer(const AInitializer: string);
  const
    PARAMETER_COUNT = 2000;
  var
    ShortWork, LongWork: TGocciaCompileCounters;
    Counter: TGocciaCompileCounter;
  begin
    ShortWork := Counters(PARAMETER_COUNT, AInitializer);
    LongWork := Counters(2 * PARAMETER_COUNT, AInitializer);
    // Duplicate-name validation covers the parse; prefix visits cover the
    // initializer proof. The old proof quadrupled work on doubling input.
    for Counter in [ccBindingNameProbes, ccParameterInitializerVisits] do
    begin
      Expect<Boolean>(ShortWork[Counter] >= PARAMETER_COUNT).ToBe(True);
      Expect<Boolean>(LongWork[Counter] >= 2 * PARAMETER_COUNT).ToBe(True);
      if LongWork[Counter] > ShortWork[Counter] * 5 div 2 then
        Fail(Format('Default "%s": %s grew from %d to %d for %d and %d parameters',
          [AInitializer, COMPILE_COUNTER_JSON_NAMES[Counter], ShortWork[Counter],
          LongWork[Counter], PARAMETER_COUNT, 2 * PARAMETER_COUNT]));
      Expect<Boolean>(LongWork[Counter] <= ShortWork[Counter] * 5 div 2).ToBe(True);
    end;
  end;

begin
  CheckInitializer('1');
  CheckInitializer('seed');
end;

procedure TTestCompiler.TestOrdinaryParametersSkipInitializerSourceChecks;

  function SourceChecks(const ACount: Integer): Int64;
  var
    Source: TStringBuffer;
    I: Integer;
    Work: TGocciaCompileCounters;
  begin
    Source := TStringBuffer.Create;
    Source.Append('const f = (first = () => 1');
    for I := 0 to ACount - 1 do
      Source.Append(Format(', p%d', [I]));
    Source.Append(') => 1;');
    Work := SourceCompileCounters(Source.ToString);
    Result := Work[ccParameterInitializerSourceChecks];
  end;

var
  ShortChecks, LongChecks: Int64;
begin
  ShortChecks := SourceChecks(128);
  LongChecks := SourceChecks(256);
  // Only the first parameter has a default destination to prove safe.
  // Adding ordinary parameters must not add closure-source searches.
  Expect<Boolean>(ShortChecks > 0).ToBe(True);
  Expect<Int64>(LongChecks).ToBe(ShortChecks);
end;

procedure TTestCompiler.TestLargeSwitchLookupProbesScale;

  function Probes(const ACount: Integer): Int64;
  var
    Source: TStringBuffer;
    I: Integer;
    Work: TGocciaCompileCounters;
  begin
    Source := TStringBuffer.Create;
    Source.Append('const g = (d) => { switch (d) {');
    for I := 0 to ACount - 1 do
      Source.Append(Format('case %0:d: const s%0:d = %0:d; break;', [I]));
    Source.Append('} };');
    Work := SourceCompileCounters(Source.ToString);
    Result := Work[ccLocalLookupProbes];
  end;

var
  ShortProbes, LongProbes: Int64;
begin
  ShortProbes := Probes(8000);
  LongProbes := Probes(16000);
  if LongProbes > ShortProbes * 5 div 2 then
    Fail(Format('16000 switch clauses took %d local probes versus %d for 8000',
      [LongProbes, ShortProbes]));
  Expect<Boolean>(ShortProbes > 0).ToBe(True);
  Expect<Boolean>(LongProbes <= ShortProbes * 5 div 2).ToBe(True);
end;

procedure TTestCompiler.TestDirectEvalNameChecksScale;

  function EvalSource(const ACount: Integer): string;
  var
    Source: TStringBuffer;
    I: Integer;
  begin
    Source := TStringBuffer.Create;
    Source.Append('const f = () => {');
    for I := 0 to ACount - 1 do
      Source.Append(Format('let v%d = 1;', [I]));
    Source.Append('eval("0"); };');
    Result := Source.ToString;
  end;

var
  ShortWork, LongWork: TGocciaCompileCounters;
  ShortProbes, LongProbes: Int64;
begin
  ShortWork := SourceCompileCounters(EvalSource(256));
  LongWork := SourceCompileCounters(EvalSource(1024));
  ShortProbes := ShortWork[ccBindingNameProbes];
  LongProbes := LongWork[ccBindingNameProbes];
  // Count string-name membership only. Direct-eval record-array scans and
  // growth remain separate work, so total compile time is not this contract.
  Expect<Boolean>(ShortProbes >= 256).ToBe(True);
  Expect<Boolean>(LongProbes >= 1024).ToBe(True);
  if LongProbes > 5 * ShortProbes then
    Fail(Format('Direct eval with 1024 bindings took %d name probes versus %d ' +
      'with 256', [LongProbes, ShortProbes]));
  Expect<Boolean>(LongProbes <= 5 * ShortProbes).ToBe(True);
end;

{ The compiler collects a pattern's bound names, each once, wherever it
  declares them (ES2026 §8.2.1 BoundNames), and the parse checks them
  against each other in a static block and a for statement's let or const
  (§15.7.1, §14.7.4.1). }
procedure TTestCompiler.TestDestructuringPatternsCompileInLinearTime;
begin
  // Body/block lexical-name validation must remain linear as the list grows.
  ExpectNameListLinear('function g() { let %s; }', 'b%d');
  ExpectNameListLinear('const g = () => { let %s; };', 'b%d');
  ExpectNameListLinear('class C { m() { let %s; } }', 'b%d');
  ExpectNameListLinear('{ let %s; }', 'b%d');
  ExpectNameListLinear('switch (0) { case 0: let %s; }', 'b%d');
  ExpectNameListLinear('const g = (o) => { const { %s } = o; return b0; };',
    'b%d');
  ExpectNameListLinear('const g = (o) => { let [%s] = o; return b0; };',
    'b%d');
  ExpectNameListLinear('const g = (o) => { var { %s } = o; return b0; };',
    'b%d');
  ExpectNameListLinear('const { %s } = {};', 'b%d');
  ExpectNameListLinear(
    'const g = (o) => { for (const { %s } of o) { return b0; } };', 'b%d');
  ExpectNameListLinear(
    'const g = () => { try { g(); } catch ({ %s }) { return b0; } };', 'b%d');
  ExpectNameListLinear(
    'const g = (o) => { const { %s } = o; return b0; };', 'k%0:d: [b%0:d = %0:d]');
  ExpectNameListLinear('class C { static { const { %s } = {}; } }', 'b%d');
  ExpectNameListLinear('for (let { %s } = {}; false;) {}', 'b%d');
end;

{ An operator compiled as the operand of another used to work out its type
  and try its constant value again from its whole subtree, so a chain of k
  binary operators compiled in time quadratic in k: a function with 250
  closures each summing 120 captured bindings took 3 s, and twice the terms
  four times as long. Each shape compiles the same number of operators as
  short chains and as long ones; type visits and constant-evaluation visits
  must each stay close, so neither can hide the other's regression. }
procedure TTestCompiler.TestOperatorChainsCompileInLinearTime;

  // Chains of ATerms terms, CHAIN_OPERATORS operators in all: AFirst, then
  // AOperator and ATerm (formatted with the term's index modulo
  // CHAIN_CAPTURED_BINDINGS) for each further term. Each chain is the body
  // of an arrow function of p, or, with ACaptured, of a closure over that
  // many `let` bindings of the function around it.
  function ChainsSource(const AFirst, AOperator, ATerm: string;
    const ATerms: Integer; const ACaptured: Boolean): string;
  var
    Source: TStringBuffer;
    Chain, Index, Term: Integer;
  begin
    Source := TStringBuffer.Create;
    if ACaptured then
    begin
      Source.Append('const outer = (p) => {' + LineEnding);
      for Index := 0 to CHAIN_CAPTURED_BINDINGS - 1 do
        Source.Append(Format('let x%d = p + %d;', [Index, Index]) +
          LineEnding);
    end;
    for Chain := 0 to CHAIN_OPERATORS div (ATerms - 1) - 1 do
    begin
      if ACaptured then
        Source.Append(Format('const h%d = () => ', [Chain]))
      else
        Source.Append(Format('const f%d = (p) => ', [Chain]));
      Source.Append(AFirst);
      for Term := 1 to ATerms - 1 do
        Source.Append(' ' + AOperator + ' ' +
          Format(ATerm, [Term mod CHAIN_CAPTURED_BINDINGS]));
      Source.Append(';' + LineEnding);
    end;
    if ACaptured then
      Source.Append('return p; };' + LineEnding);
    Result := Source.ToString;
  end;

  procedure ExpectChainsCompileLinear(const AFirst, AOperator,
    ATerm: string; const ACaptured: Boolean;
    const AStrictTypes: Boolean = False);
  var
    Short, Long: TGocciaCompileCounters;
  begin
    Short := SourceCompileCounters(ChainsSource(AFirst, AOperator, ATerm,
      CHAIN_SHORT_TERMS, ACaptured), AStrictTypes);
    Long := SourceCompileCounters(ChainsSource(AFirst, AOperator, ATerm,
      CHAIN_SHORT_TERMS * CHAIN_LENGTH_FACTOR, ACaptured), AStrictTypes);
    ExpectWorkLinear(Short, Long, ccExpressionTypeVisits);
    ExpectWorkLinear(Short, Long, ccConstantEvaluationVisits);
  end;

begin
  // A parameter: the operand types and the constant tries.
  ExpectChainsCompileLinear('p', '+', 'p', False);
  // An Int16 immediate on the right of every `+`.
  ExpectChainsCompileLinear('p', '+', '1', False);
  // A comparison's type needs no walk; only the constant tries remain.
  ExpectChainsCompileLinear('p', '<', 'p', False);
  // Closures summing captured bindings, the shape the slowdown was seen in.
  ExpectChainsCompileLinear('x0', '+', 'x%d', True);
  // Strict types try to simplify every operator by its operand types, and
  // `x + ""` to x once x is a String.
  ExpectChainsCompileLinear('p', '+', 'p', False, True);
  ExpectChainsCompileLinear('p', '+', '""', False, True);
end;

{ CompileExpressionNode skips the constant tries of the left operands a
  failed try already found not constant. The constant part at the start of a
  chain is still a left operand of every operator above it, and must still
  be folded when its turn comes. }
procedure TTestCompiler.TestOperatorChainsFoldTheirConstantStart;

  procedure ExpectFolded(const ASource: string; const AFolded: Int16;
    const AOperators: Integer);
  var
    Module: TGocciaBytecodeModule;
    Chain: TGocciaFunctionTemplate;
  begin
    Module := CompileSource(ASource);
    try
      Chain := Module.TopLevel.GetFunction(0);
      Expect<Boolean>(HasLoadInt(Chain, AFolded)).ToBe(True);
      Expect<Integer>(CountArithmeticOps(Chain)).ToBe(AOperators);
    finally
      Module.Free;
    end;
  end;

begin
  // `k * 2 + k` is 12; the operators with p and the last k remain.
  ExpectFolded('const k = 4; const f = (p) => k * 2 + k + p + k;', 12, 2);
  ExpectFolded('const f = (p) => 1 + 2 + p + 3 + 4;', 3, 3);
  ExpectFolded('const f = (p) => 2 * 3 * 4 + p + p + p + p;', 24, 4);
end;

{ In TypeScript, a '<' after a callee may open call-site type arguments,
  and the parser scans ahead to see whether it does. In `p < p < p` the
  depth of '<' never comes back to zero, so each scan went on to the bracket
  around the chain, or to the end of the file, and lexed those tokens again:
  40 chains of 120 terms took 5 s to lex and parse. A failed scan now decides
  the '<' tokens after it as well. Four times the source parses in at most 8
  times the time, where linear growth gives 4 and quadratic 16. }
procedure TTestCompiler.TestTypeScriptComparisonChainsParseInLinearTime;
const
  GROWTH_RATIO = 8;

  // ALines statements, each `const yN = ` and ATerms terms joined by '<', in
  // the body of an arrow function or at the top level, and then ATail.
  function ChainsSource(const ALines, ATerms: Integer;
    const AInFunction: Boolean; const ATail: string): string;
  var
    Source: TStringBuffer;
    Line, Term: Integer;
  begin
    Source := TStringBuffer.Create;
    if AInFunction then
      Source.Append('const f = (p: number) => {' + LineEnding)
    else
      Source.Append('const p = 1;' + LineEnding);
    for Line := 0 to ALines - 1 do
    begin
      Source.Append(Format('const y%d = p', [Line]));
      for Term := 1 to ATerms - 1 do
        Source.Append(' < p');
      Source.Append(';' + LineEnding);
    end;
    Source.Append(ATail + LineEnding);
    if AInFunction then
      Source.Append('};' + LineEnding);
    Result := Source.ToString;
  end;

  function ParseNanoseconds(const ASource: string): Int64;
  var
    Lexer: TGocciaLexer;
    Parser: TGocciaParser;
    ProgramNode: TGocciaProgram;
    SourceLines: TStringList;
    Start, Elapsed: Int64;
    Trial: Integer;
  begin
    Result := High(Int64);
    for Trial := 1 to LINEAR_TRIALS do
    begin
      Lexer := TGocciaLexer.Create(ASource, '<test>.ts');
      SourceLines := CreateTextLines(ASource);
      Parser := TGocciaParser.CreateFromLexer(Lexer, '<test>.ts', SourceLines);
      ProgramNode := nil;
      try
        Start := GetNanoseconds;
        ProgramNode := Parser.Parse;
        Elapsed := GetNanoseconds - Start;
      finally
        ProgramNode.Free;
        Parser.Free;
        SourceLines.Free;
        Lexer.Free;
      end;
      if Elapsed < Result then
        Result := Elapsed;
    end;
  end;

  procedure ExpectChainsParseLinear(const ALines, ATerms: Integer;
    const AInFunction: Boolean; const ATail: string);
  var
    Small, Large: Int64;
  begin
    Small := ParseNanoseconds(ChainsSource(ALines, ATerms, AInFunction,
      ATail));
    Large := ParseNanoseconds(ChainsSource(4 * ALines, ATerms, AInFunction,
      ATail));
    if Large > GROWTH_RATIO * Small + LINEAR_SLACK_NANOSECONDS then
      Fail(Format('%d chains of %d terms took %d ms to parse against %d ms ' +
        'for %d', [4 * ALines, ATerms, Large div 1000000, Small div 1000000,
        ALines]));
    Expect<Boolean>(Large <=
      GROWTH_RATIO * Small + LINEAR_SLACK_NANOSECONDS).ToBe(True);
  end;

begin
  // The shape the slowdown was seen in: long chains in a function body.
  ExpectChainsParseLinear(8, 120, True, 'return p;');
  // A division after the chains, which the scans lex as the start of a
  // regular expression that does not end.
  ExpectChainsParseLinear(8, 120, True, 'return p / 2;');
  // Short chains at the top level, where each scan ran to the end of the
  // file.
  ExpectChainsParseLinear(250, 3, False, '');
end;

{ A chain that nests to the right, such as `1 + (2 + (3 + p))`, or a chain
  of unary operators, such as `- - - p`, is tried as a constant at every
  operator, and each try evaluated the operands below it again down to p;
  `1 + x` also asked for the type of its whole right operand at every
  operator. Both took time quadratic in the chain's length (3.8 s on `main`
  for 19,200 operators in chains of 480, against about 50 ms now). The
  shapes compile CHAIN_OPERATORS operators as chains of CHAIN_SHORT_TERMS
  terms and as chains CHAIN_LENGTH_FACTOR times as long, with the bound of
  TestOperatorChainsCompileInLinearTime for both type and constant visits. }
procedure TTestCompiler.TestNestedChainsCompileInLinearTime;

  // Chains of ATerms terms, CHAIN_OPERATORS operators in all, each the body
  // of an arrow function of p: AOpen (formatted with the term's index) for
  // each operator, then p, then AClose for each operator.
  function NestedChainsSource(const AOpen, AClose: string;
    const ATerms: Integer): string;
  var
    Source: TStringBuffer;
    Chain, Term: Integer;
  begin
    Source := TStringBuffer.Create;
    for Chain := 0 to CHAIN_OPERATORS div (ATerms - 1) - 1 do
    begin
      Source.Append(Format('const f%d = (p) => ', [Chain]));
      for Term := 1 to ATerms - 1 do
        Source.Append(Format(AOpen, [Term]));
      Source.Append('p');
      for Term := 1 to ATerms - 1 do
        Source.Append(AClose);
      Source.Append(';' + LineEnding);
    end;
    Result := Source.ToString;
  end;

  procedure ExpectNestedChainsCompileLinear(const AOpen, AClose: string;
    const AStrictTypes: Boolean = False);
  var
    Short, Long: TGocciaCompileCounters;
  begin
    Short := SourceCompileCounters(NestedChainsSource(AOpen, AClose,
      CHAIN_SHORT_TERMS), AStrictTypes);
    Long := SourceCompileCounters(NestedChainsSource(AOpen, AClose,
      CHAIN_SHORT_TERMS * CHAIN_LENGTH_FACTOR), AStrictTypes);
    ExpectWorkLinear(Short, Long, ccExpressionTypeVisits);
    ExpectWorkLinear(Short, Long, ccConstantEvaluationVisits);
  end;

begin
  // Int16 constants: the constant tries and the type of `1 + x`'s x.
  ExpectNestedChainsCompileLinear('%d + (', ')');
  // Constants outside the immediate path, and strings.
  ExpectNestedChainsCompileLinear('%d.5 * (', ')');
  ExpectNestedChainsCompileLinear('"s" + (', ')');
  ExpectNestedChainsCompileLinear('- ', '');
  ExpectNestedChainsCompileLinear('!', '');
  ExpectNestedChainsCompileLinear('typeof ', '');
  // Strict types also try to simplify `x + ""` and `!!b` at every operator.
  ExpectNestedChainsCompileLinear('%d + (', ')', True);
  ExpectNestedChainsCompileLinear('!', '', True);
end;

{ CompileExpressionNode skips the constant tries of the operands a failed
  try already found not constant, on either side of a binary operator and
  below a unary one. An operand beside them that is constant must still be
  folded when its turn comes. }
procedure TTestCompiler.TestNestedChainsFoldTheirConstantOperands;

  procedure ExpectFolded(const ASource: string; const AFolded: Int16;
    const AOperators: Integer);
  var
    Module: TGocciaBytecodeModule;
    Chain: TGocciaFunctionTemplate;
  begin
    Module := CompileSource(ASource);
    try
      Chain := Module.TopLevel.GetFunction(0);
      Expect<Boolean>(HasLoadInt(Chain, AFolded)).ToBe(True);
      Expect<Integer>(CountArithmeticOps(Chain)).ToBe(AOperators);
    finally
      Module.Free;
    end;
  end;

begin
  ExpectFolded('const f = (p) => 1 + (p + (2 + 3));', 5, 2);
  ExpectFolded('const k = 4; const f = (p) => k * (p - (k * 2));', 8, 2);
  ExpectFolded('const f = (p) => -(p + -(2 + 3));', -5, 1);
  ExpectFolded('const f = (p) => 1 + (2 + (p + (3 * 4)));', 12, 3);
end;

{ The case clauses of a switch share one scope, so the compiler forgets
  before each clause which of their bindings the clauses before it
  initialized or gave a constant. Doing that by visiting every local of the
  function made a switch with a `let` or `const` in each clause compile in
  time quadratic in its clauses (about 0.45 s here for 4,000, 31 ms for
  1,000). Each shape compiles 4,000 clauses in at most 8 times the time of
  1,000, about 2.8 times per doubling where linear growth gives 2 and
  quadratic 4, and within a few times the same clauses with their bodies in
  braces, whose bindings live in a block of each clause. Both sizes stay
  below about 5,400 clauses, past which the switch's first jumps need the
  long form (TestLongBranchesCompileOnce). }
procedure TTestCompiler.TestSwitchCaseDeclarationsCompileInLinearTime;
const
  SMALL_CLAUSES = 1000;
  LARGE_CLAUSES = 4000;
  GROWTH_RATIO = 8;
  FLAT_CLAUSE = 'case %0:d: const s%0:d = %0:d; break;';
  BRACED_CLAUSE = 'case %0:d: { const s%0:d = %0:d; break; }';

  procedure ExpectSwitchCompilesLinear(const APrefix, ASuffix: string;
    const AGlobalBackedTopLevel: Boolean);
  var
    Small, Large, Braced: Int64;
  begin
    Small := CompileNanoseconds(FLAT_CLAUSE, AGlobalBackedTopLevel,
      SMALL_CLAUSES, APrefix, ASuffix);
    Large := CompileNanoseconds(FLAT_CLAUSE, AGlobalBackedTopLevel,
      LARGE_CLAUSES, APrefix, ASuffix);
    Braced := CompileNanoseconds(BRACED_CLAUSE, AGlobalBackedTopLevel,
      LARGE_CLAUSES, APrefix, ASuffix);
    if Large > GROWTH_RATIO * Small + LINEAR_SLACK_NANOSECONDS then
      Fail(Format('%d clauses "%s" in "%s" took %d ms to compile against ' +
        '%d ms for %d', [LARGE_CLAUSES, FLAT_CLAUSE, APrefix,
        Large div 1000000, Small div 1000000, SMALL_CLAUSES]));
    Expect<Boolean>(Large <=
      GROWTH_RATIO * Small + LINEAR_SLACK_NANOSECONDS).ToBe(True);
    if Large > LINEAR_COMPILE_RATIO * Braced + LINEAR_SLACK_NANOSECONDS then
      Fail(Format('%d clauses "%s" in "%s" took %d ms to compile against ' +
        '%d ms with braces', [LARGE_CLAUSES, FLAT_CLAUSE, APrefix,
        Large div 1000000, Braced div 1000000]));
    Expect<Boolean>(Large <=
      LINEAR_COMPILE_RATIO * Braced + LINEAR_SLACK_NANOSECONDS).ToBe(True);
  end;

begin
  ExpectSwitchCompilesLinear('const d = 1; switch (d) {', '}', False);
  ExpectSwitchCompilesLinear('const d = 1; switch (d) {', '}', True);
  ExpectSwitchCompilesLinear(
    'const d = 1; const g = () => { switch (d) {', '} };', False);
end;

{ A conditional jump over a branch longer than its 16-bit offset holds takes
  a long form. The compiler used to give it one by compiling the whole
  program a second time, so a program cost twice as much to compile once one
  of its branches passed the limit. Each shape here puts the same clauses or
  statements in branches past the limit and in twice as many branches short
  enough for the 16-bit offset. The short source has more branch nodes, so
  the long source must compile no more nodes than the short one, including
  nested functions. Recompiling the program or a function exceeds this bound
  regardless of machine speed. }
procedure TTestCompiler.TestLongBranchesCompileOnce;
const
  // Three switches of 6,000 clauses each need the long form, which starts
  // at about 5,400; six of 3,000 do not.
  CLAUSES = 18000;
  LONG_SWITCHES = 3;
  CLAUSE = 'case %0:d: const s%0:d = %0:d; break;';
  // Two instructions each: 18,000 of them need the long form, which starts
  // past 32,767 instructions, and 9,000 do not.
  STATEMENTS = 36000;
  LONG_IFS = 2;
  STATEMENT = 'r = r + 1;';

  { The CLAUSES clauses or STATEMENTS statements in ABranches switches or
    ifs, between APrefix and ASuffix. }
  function BranchSource(const ASwitch: Boolean; const ABranches: Integer;
    const APrefix, ASuffix: string): string;
  var
    Source: TStringBuffer;
    Count, Branch, I: Integer;
  begin
    Source := TStringBuffer.Create;
    Source.Append(APrefix + LineEnding);
    if ASwitch then
      Count := CLAUSES
    else
      Count := STATEMENTS;
    for Branch := 0 to ABranches - 1 do
    begin
      if ASwitch then
        Source.Append('switch (d) {' + LineEnding)
      else
        Source.Append('if (d) {' + LineEnding);
      for I := Branch * (Count div ABranches) to
          (Branch + 1) * (Count div ABranches) - 1 do
        if ASwitch then
          Source.Append(Format(CLAUSE, [I]) + LineEnding)
        else
          Source.Append(STATEMENT + LineEnding);
      Source.Append('}' + LineEnding);
    end;
    Source.Append(ASuffix + LineEnding);
    Result := Source.ToString;
  end;

  { The long-form jumps in ATemplate and the functions below it: a
    conditional jump or handler push, an OP_JUMP over the next instruction,
    and the OP_JUMP that reaches the branch's target. }
  function CountLongFormJumps(const ATemplate: TGocciaFunctionTemplate): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to ATemplate.CodeCount - 2 do
      if not (JumpOperandOf(DecodeOp(ATemplate.GetInstruction(I - 1))) in
           [gjoNone, gjoAx]) and
         (DecodeOp(ATemplate.GetInstruction(I)) = Ord(OP_JUMP)) and
         (DecodeAx(ATemplate.GetInstruction(I)) = 1) and
         (DecodeOp(ATemplate.GetInstruction(I + 1)) = Ord(OP_JUMP)) then
        Inc(Result);
    for I := 0 to ATemplate.FunctionCount - 1 do
      Inc(Result, CountLongFormJumps(ATemplate.GetFunction(I)));
  end;

  { ACheckShapes: whether to check that only the long branches take the
    long form (CompileSource compiles one source the same for both kinds of
    top level). }
  procedure ExpectLongBranchesCompileOnce(const ASwitch: Boolean;
    const ALongBranches: Integer; const APrefix, ASuffix: string;
    const AGlobalBackedTopLevel, ACheckShapes: Boolean);
  var
    LongSource, ShortSource: string;
    Module: TGocciaBytecodeModule;
    Long, Short: TGocciaCompileCounters;
  begin
    LongSource := BranchSource(ASwitch, ALongBranches, APrefix, ASuffix);
    ShortSource := BranchSource(ASwitch, 2 * ALongBranches, APrefix,
      ASuffix);
    if ACheckShapes then
    begin
      Module := CompileSource(LongSource);
      try
        Expect<Boolean>(CountLongFormJumps(Module.TopLevel) > 0).ToBe(True);
      finally
        Module.Free;
      end;
      Module := CompileSource(ShortSource);
      try
        Expect<Integer>(CountLongFormJumps(Module.TopLevel)).ToBe(0);
      finally
        Module.Free;
      end;
    end;

    Long := SourceCompileCounters(LongSource, False, '<test>', False,
      AGlobalBackedTopLevel);
    Short := SourceCompileCounters(ShortSource, False, '<test>', False,
      AGlobalBackedTopLevel);
    Expect<Boolean>(Short[ccNodesCompiled] > 0).ToBe(True);
    Expect<Boolean>(Long[ccNodesCompiled] > 0).ToBe(True);
    if Long[ccNodesCompiled] > Short[ccNodesCompiled] then
      Fail(Format('%d long branches in "%s" (global-backed: %s) compiled %d nodes ' +
        'against %d in %d short ones', [ALongBranches, APrefix,
        BoolToStr(AGlobalBackedTopLevel, True), Long[ccNodesCompiled],
        Short[ccNodesCompiled], 2 * ALongBranches]));
    Expect<Boolean>(Long[ccNodesCompiled] <= Short[ccNodesCompiled]).ToBe(True);
  end;

begin
  ExpectLongBranchesCompileOnce(True, LONG_SWITCHES, 'const d = 1;', '',
    False, True);
  ExpectLongBranchesCompileOnce(True, LONG_SWITCHES, 'const d = 1;', '',
    True, False);
  ExpectLongBranchesCompileOnce(True, LONG_SWITCHES,
    'const d = 1; const g = () => {', '};', False, True);
  // A parameter, since an if on a constant compiles to no jump.
  ExpectLongBranchesCompileOnce(False, LONG_IFS,
    'const g = (d) => { let r = 0;', 'return r; };', False, True);
end;

procedure TTestCompiler.TestSlotHoldsLocalFollowsTheLocals;
var
  Scope: TGocciaCompilerScope;
  Closed: TArray<UInt16>;
  ClosedCount: Integer;
  FirstSlot, VarSlot, BlockSlot, TempSlot: UInt16;
begin
  Scope := TGocciaCompilerScope.Create(nil, 0);
  try
    FirstSlot := Scope.DeclareLocal('a', False);
    Expect<Boolean>(Scope.SlotHoldsLocal(FirstSlot)).ToBe(True);
    TempSlot := Scope.AllocateRegister;
    // A register allocated for a value is not a local's.
    Expect<Boolean>(Scope.SlotHoldsLocal(TempSlot)).ToBe(False);
    Scope.FreeRegister;
    Expect<Boolean>(Scope.SlotHoldsLocal(1000)).ToBe(False);

    Scope.BeginScope;
    BlockSlot := Scope.DeclareLocal('b', False);
    VarSlot := Scope.DeclareVarLocal('v');
    Expect<Boolean>(Scope.SlotHoldsLocal(BlockSlot)).ToBe(True);
    Expect<Boolean>(Scope.SlotHoldsLocal(VarSlot)).ToBe(True);
    // The var, declared at depth 0, keeps the block's local from being
    // dropped, and both keep their registers.
    Scope.EndScope(Closed, ClosedCount);
    Expect<Boolean>(Scope.SlotHoldsLocal(BlockSlot)).ToBe(True);
    Expect<Boolean>(Scope.SlotHoldsLocal(VarSlot)).ToBe(True);
  finally
    Scope.Free;
  end;

  Scope := TGocciaCompilerScope.Create(nil, 0);
  try
    Scope.DeclareLocal('a', False);
    Scope.BeginScope;
    BlockSlot := Scope.DeclareLocal('b', False);
    Expect<Boolean>(Scope.SlotHoldsLocal(BlockSlot)).ToBe(True);
    Scope.EndScope(Closed, ClosedCount);
    // The block's local is dropped with its block, and the register it had
    // is free for a value again.
    Expect<Boolean>(Scope.SlotHoldsLocal(BlockSlot)).ToBe(False);
    Expect<Integer>(Scope.AllocateRegister).ToBe(BlockSlot);
    Scope.FreeRegister;

    // A caller's local rebuilt for a direct eval names its register.
    Scope.DeclareCallerLocal('c', 40, False, False);
    Expect<Boolean>(Scope.SlotHoldsLocal(40)).ToBe(True);
    Expect<Boolean>(Scope.SlotHoldsLocal(39)).ToBe(False);
  finally
    Scope.Free;
  end;
end;

{ Each a distinct number outside the immediate range, so each takes its own
  constant pool entry, against as many consts of one small integer. Near the
  pool's 65,535 entries, so a scan of the pool per number (about 3 s here)
  is far past the bound. }
procedure TTestCompiler.TestDistinctFloatConstantsCompileInLinearTime;
const
  FLOAT_COUNT = 50000;
var
  Baseline, Elapsed: Int64;
begin
  Baseline := CompileNanoseconds('const a%0:d = 1;', False, FLOAT_COUNT);
  Elapsed := CompileNanoseconds('const a%0:d = %0:d.5;', False, FLOAT_COUNT);
  if Elapsed > LINEAR_COMPILE_RATIO * Baseline + LINEAR_SLACK_NANOSECONDS then
    Fail(Format('%d distinct float constants took %d ms to compile against ' +
      '%d ms for one repeated integer', [FLOAT_COUNT, Elapsed div 1000000,
      Baseline div 1000000]));
  Expect<Boolean>(Elapsed <=
    LINEAR_COMPILE_RATIO * Baseline + LINEAR_SLACK_NANOSECONDS).ToBe(True);
end;

{ More locals than COMPILER_LOCAL_INDEX_THRESHOLD, so names resolve through
  the scope's index, which a block's end must point back at the binding the
  block hid. }
procedure TTestCompiler.TestEndingABlockRestoresTheBindingItShadowed;
var
  Scope: TGocciaCompilerScope;
  Closed: TArray<UInt16>;
  ClosedCount, I: Integer;
begin
  Scope := TGocciaCompilerScope.Create(nil, 0);
  try
    for I := 0 to 99 do
      Scope.DeclareLocal('x' + IntToStr(I), False);

    Scope.BeginScope;
    Scope.DeclareLocal('x5', True);
    Scope.DeclareLocal('fresh', True);
    Scope.BeginScope;
    Scope.DeclareLocal('x5', True);
    Expect<Integer>(Scope.ResolveLocal('x5')).ToBe(102);
    Scope.EndScope(Closed, ClosedCount);
    Expect<Integer>(Scope.ResolveLocal('x5')).ToBe(100);
    Scope.EndScope(Closed, ClosedCount);
    Expect<Integer>(Scope.ResolveLocal('x5')).ToBe(5);
    Expect<Integer>(Scope.ResolveLocal('fresh')).ToBe(-1);

    // A renamed local keeps its place under its new name only.
    Scope.RenameLocal(7, 'renamed');
    Expect<Integer>(Scope.ResolveLocal('x7')).ToBe(-1);
    Expect<Integer>(Scope.ResolveLocal('renamed')).ToBe(7);
    Scope.BeginScope;
    Scope.DeclareLocal('renamed', True);
    Expect<Integer>(Scope.ResolveLocal('renamed')).ToBe(100);
    Scope.EndScope(Closed, ClosedCount);
    Expect<Integer>(Scope.ResolveLocal('renamed')).ToBe(7);
  finally
    Scope.Free;
  end;
end;

procedure TTestCompiler.TestVarAndSlotLookupsFindTheOldestLocal;
var
  Scope: TGocciaCompilerScope;
  Closed: TArray<UInt16>;
  ClosedCount, I: Integer;
  VarSlot, BlockSlot: UInt16;
begin
  Scope := TGocciaCompilerScope.Create(nil, 0);
  try
    for I := 0 to 99 do
      Scope.DeclareLocal('x' + IntToStr(I), False);

    VarSlot := Scope.DeclareVarLocal('v');
    Scope.BeginScope;
    BlockSlot := Scope.DeclareLocal('v', False);
    // A var redeclared inside the block is still the function's var.
    Expect<Integer>(Scope.DeclareVarLocal('v')).ToBe(VarSlot);
    Expect<Integer>(Scope.FindLocalBySlot('v', VarSlot)).ToBe(100);
    Expect<Integer>(Scope.FindLocalBySlot('v', BlockSlot)).ToBe(101);
    Expect<Integer>(Scope.FindLocalBySlot('v', 3)).ToBe(-1);
    Scope.EndScope(Closed, ClosedCount);
    Expect<Integer>(Scope.DeclareVarLocal('x3')).ToBe(
      Scope.GetLocal(3).Slot);
    Expect<Integer>(Scope.LocalCount).ToBe(101);
  finally
    Scope.Free;
  end;
end;

{ A non-strict global eval resolves its global-backed locals by name
  (GlobalBackedBindingsByName), so ResolveLocal passes over each one. Passing
  over a local must lead only to an older local of the same name, not to every
  older local: resolving each of many names costs about the same either way. }
procedure TTestCompiler.TestSkippedLocalsResolveInConstantTime;
const
  NAME_COUNT = 20000;

  function ResolveAllNanoseconds(const AByName: Boolean): Int64;
  var
    Scope: TGocciaCompilerScope;
    Start, Elapsed: Int64;
    I, Trial, Found: Integer;
  begin
    Result := High(Int64);
    for Trial := 1 to LINEAR_TRIALS do
    begin
      Scope := TGocciaCompilerScope.Create(nil, 0);
      try
        for I := 0 to NAME_COUNT - 1 do
        begin
          Scope.DeclareLocal('a' + IntToStr(I), False);
          Scope.MarkGlobalBacked(I);
        end;
        Scope.GlobalBackedBindingsByName := AByName;
        Found := 0;
        Start := GetNanoseconds;
        for I := 0 to NAME_COUNT - 1 do
          if Scope.ResolveLocal('a' + IntToStr(I)) >= 0 then
            Inc(Found);
        Elapsed := GetNanoseconds - Start;
        if AByName then
          Expect<Integer>(Found).ToBe(0)
        else
          Expect<Integer>(Found).ToBe(NAME_COUNT);
      finally
        Scope.Free;
      end;
      if Elapsed < Result then
        Result := Elapsed;
    end;
  end;

var
  Direct, ByName: Int64;
begin
  Direct := ResolveAllNanoseconds(False);
  ByName := ResolveAllNanoseconds(True);
  if ByName > LINEAR_COMPILE_RATIO * Direct + LINEAR_SLACK_NANOSECONDS then
    Fail(Format('%d names resolving by name took %d ms against %d ms',
      [NAME_COUNT, ByName div 1000000, Direct div 1000000]));
  Expect<Boolean>(ByName <=
    LINEAR_COMPILE_RATIO * Direct + LINEAR_SLACK_NANOSECONDS).ToBe(True);
end;

procedure TTestCompiler.TestFloatConstantsArePooledByBitPattern;
var
  Template: TGocciaFunctionTemplate;
  PositiveZero, NegativeZeroIndex, FirstNaN: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('<test>');
  try
    Expect<Integer>(Template.AddConstantFloat(1.5)).ToBe(0);
    Expect<Integer>(Template.AddConstantFloat(2.5)).ToBe(1);
    Expect<Integer>(Template.AddConstantFloat(1.5)).ToBe(0);
    PositiveZero := Template.AddConstantFloat(0.0);
    NegativeZeroIndex := Template.AddConstantFloat(NegativeZero);
    // SameValue tells the zeros apart, so the pool does too.
    Expect<Boolean>(PositiveZero <> NegativeZeroIndex).ToBe(True);
    Expect<Integer>(Template.AddConstantFloat(NegativeZero))
      .ToBe(NegativeZeroIndex);
    FirstNaN := Template.AddConstantFloat(NaN);
    Expect<Integer>(Template.AddConstantFloat(-NaN)).ToBe(FirstNaN);
    // A string constant between them takes its own entry.
    Expect<Integer>(Template.AddConstantString('1.5')).ToBe(FirstNaN + 1);
    Expect<Integer>(Template.AddConstantFloat(2.5)).ToBe(1);
  finally
    Template.Free;
  end;
end;

// Issue #1666: the hidden `__super__` local of an anonymous class with a
// heritage was declared in the enclosing block and its register released
// twice, so a register allocated after the block was one a live local still
// held: the returned arrow was created into `c`'s register.
procedure TTestCompiler.TestAnonymousDerivedClassInBlockKeepsLaterLocalsApart;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
  I, ClosureReg: Integer;

  function SlotOf(const AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to Func.DebugInfo.LocalCount - 1 do
      if Func.DebugInfo.GetLocalInfo(I).Name = AName then
        Exit(Func.DebugInfo.GetLocalInfo(I).Slot);
  end;

begin
  Module := CompileSource(
    'const f = () => {' + LineEnding +
    '  let a = 1;' + LineEnding +
    '  { const K = class extends Object {}; }' + LineEnding +
    '  let b = 2;' + LineEnding +
    '  let c = 3;' + LineEnding +
    '  return () => a + b + c;' + LineEnding +
    '};');
  try
    Func := Module.TopLevel.GetFunction(0);
    Expect<Boolean>(Assigned(Func.DebugInfo)).ToBe(True);
    Expect<Boolean>(SlotOf('a') >= 0).ToBe(True);
    Expect<Boolean>(SlotOf('b') >= 0).ToBe(True);
    Expect<Boolean>(SlotOf('c') >= 0).ToBe(True);
    Expect<Boolean>(SlotOf('b') <> SlotOf('a')).ToBe(True);
    Expect<Boolean>(SlotOf('c') <> SlotOf('a')).ToBe(True);
    Expect<Boolean>(SlotOf('c') <> SlotOf('b')).ToBe(True);
    ClosureReg := -1;
    for I := 0 to Func.CodeCount - 1 do
      if TGocciaOpCode(DecodeOp(Func.GetInstruction(I))) = OP_CLOSURE then
        ClosureReg := DecodeA(Func.GetInstruction(I));
    Expect<Boolean>(ClosureReg >= 0).ToBe(True);
    Expect<Boolean>(ClosureReg <> SlotOf('a')).ToBe(True);
    Expect<Boolean>(ClosureReg <> SlotOf('b')).ToBe(True);
    Expect<Boolean>(ClosureReg <> SlotOf('c')).ToBe(True);
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

// ADR 0134: code a direct eval creates writes its caller's bindings through
// their cells, like any closure, so a function without one reads its let
// bindings and parameters from their registers whatever host compiles it.
procedure TTestCompiler.TestFunctionWithoutDirectEvalReadsOperandRegisters;
const
  SOURCE =
    'const sum = (a, b) => { let t = a * b; const k = a * 2; return t + a + k; };';
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  Module := CompileSource(SOURCE);
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_MUL);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestDirectEvalInFunctionKeepsOperandCopies;

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
  // A function that contains a direct eval copies every read of the
  // parameter, before the eval call as after it: code the eval creates
  // writes it through its cell (ADR 0134). The const stays in place.
  Expect<Integer>(CopiesIn(
    'const f = (a) => { const before = a * 2; eval("0"); return a * 3 + before; };')).ToBe(2);

  // In a loop too, the reads of t and a in `t + a * 2` are copied. Both loops
  // copy `items` to iterate over it, and t after the loop.
  Expect<Integer>(
    CopiesIn(
      'const f = (a, items) => { let t = 0; for (const item of items) { t = t + a * 2; eval("0"); } return t; };') -
    CopiesIn(
      'const f = (a, items) => { let t = 0; for (const item of items) { t = t + a * 2; evil("0"); } return t; };')).ToBe(2);
end;

function TTestCompiler.FindTemplateWithDirectEval(
  const ATemplate: TGocciaFunctionTemplate): TGocciaFunctionTemplate;
var
  I: Integer;
begin
  if ATemplate.DirectEvalEnvironmentCount > 0 then
    Exit(ATemplate);
  for I := 0 to ATemplate.FunctionCount - 1 do
  begin
    Result := FindTemplateWithDirectEval(ATemplate.GetFunction(I));
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function FindDirectEvalRecordEntry(const AEnv: TGocciaDirectEvalEnvironment;
  const AName: string; out ABinding: TGocciaDirectEvalBindingInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AEnv.Bindings) do
    if AEnv.Bindings[I].Name = AName then
    begin
      ABinding := AEnv.Bindings[I];
      Exit(True);
    end;
  Result := False;
end;

procedure TTestCompiler.TestDirectEvalSiteRecordListsCallSiteBindings;
const
  SOURCE =
    'const f = (p) => { try { throw 1; } catch (c) {' +
    ' { eval("0"); let later = 1; const k = 2; class K {} } } };';
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
begin
  Module := CompileSource(SOURCE, False, False, False, True, True, True,
    False, True, True);
  try
    Func := FindTemplateWithDirectEval(Module.TopLevel);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    Env := Func.GetDirectEvalEnvironment(0);
    Expect<Boolean>(Env.StrictCaller).ToBe(False);

    // A parameter of a simple parameter list is in the variable environment.
    Expect<Boolean>(FindDirectEvalRecordEntry(Env, 'p', Binding)).ToBe(True);
    Expect<Integer>(Ord(Binding.Kind)).ToBe(Ord(debLocal));
    Expect<Boolean>(Binding.IsVarEnvironmentBinding).ToBe(True);
    Expect<Boolean>(Binding.IsCatchParameter).ToBe(False);

    Expect<Boolean>(FindDirectEvalRecordEntry(Env, 'c', Binding)).ToBe(True);
    Expect<Boolean>(Binding.IsCatchParameter).ToBe(True);
    Expect<Boolean>(Binding.IsVarEnvironmentBinding).ToBe(False);

    // Declarations after the call, still in their temporal dead zone there.
    Expect<Boolean>(FindDirectEvalRecordEntry(Env, 'later', Binding)).ToBe(True);
    Expect<Boolean>(Binding.IsConst).ToBe(False);
    Expect<Boolean>(Binding.IsVarEnvironmentBinding).ToBe(False);
    Expect<Boolean>(FindDirectEvalRecordEntry(Env, 'k', Binding)).ToBe(True);
    Expect<Boolean>(Binding.IsConst).ToBe(True);
    Expect<Boolean>(FindDirectEvalRecordEntry(Env, 'K', Binding)).ToBe(True);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestDirectEvalSiteRecordTakesCallStrictness;
const
  SOURCE =
    'const g = () => { return eval("1"); };' +
    'const h = () => { "use strict"; return eval("1"); };' +
    'class C { static f = eval("1"); static [eval("''k''")] = 1; }' +
    'class D extends (eval("Object")) {}';
var
  Module: TGocciaBytecodeModule;
  StrictSites, SloppySites: Integer;

  procedure CountSites(const ATemplate: TGocciaFunctionTemplate);
  var
    J: Integer;
  begin
    for J := 0 to ATemplate.DirectEvalEnvironmentCount - 1 do
      if ATemplate.GetDirectEvalEnvironment(J).StrictCaller then
        Inc(StrictSites)
      else
        Inc(SloppySites);
  end;

  procedure CountNestedSites(const ATemplate: TGocciaFunctionTemplate);
  var
    J: Integer;
  begin
    for J := 0 to ATemplate.FunctionCount - 1 do
    begin
      CountSites(ATemplate.GetFunction(J));
      CountNestedSites(ATemplate.GetFunction(J));
    end;
  end;

begin
  Module := CompileSource(SOURCE, False, False, False, True, True, True,
    False, True, True);
  try
    // The computed key and the heritage run inline in the sloppy script,
    // and their calls are strict; the script's StrictCode is not.
    StrictSites := 0;
    SloppySites := 0;
    CountSites(Module.TopLevel);
    Expect<Boolean>(Module.TopLevel.StrictCode).ToBe(False);
    Expect<Integer>(StrictSites).ToBe(2);
    Expect<Integer>(SloppySites).ToBe(0);

    // g is sloppy; h and the static field initializer, which compiles to
    // its own function, are strict. g's call has two sites: `eval` may
    // resolve to a var an eval declared in g's hidden variable object, or
    // to the global (ADR 0134).
    StrictSites := 0;
    SloppySites := 0;
    CountNestedSites(Module.TopLevel);
    Expect<Integer>(SloppySites).ToBe(2);
    Expect<Integer>(StrictSites).ToBe(2);
  finally
    Module.Free;
  end;
end;

function WithoutEntry(const ABindings: TGocciaDirectEvalBindingArray;
  const AIndex: Integer): TGocciaDirectEvalBindingArray;
var
  I, J: Integer;
begin
  SetLength(Result, Length(ABindings) - 1);
  J := 0;
  for I := 0 to High(ABindings) do
    if I <> AIndex then
    begin
      Result[J] := ABindings[I];
      Inc(J);
    end;
end;

procedure TTestCompiler.TestDirectEvalSiteRecordCheckRejectsMismatches;
var
  Outer, Inner: TGocciaCompilerScope;
  Bindings, Changed: TGocciaDirectEvalBindingArray;
  UpvalueIdx: Integer;
  Name, Problem: string;

  procedure SetEntry(var AEntry: TGocciaDirectEvalBindingInfo;
    const AName: string; const AKind: TGocciaDirectEvalBindingKind;
    const AIndex: Integer);
  begin
    AEntry := Default(TGocciaDirectEvalBindingInfo);
    AEntry.Name := AName;
    AEntry.Kind := AKind;
    AEntry.Index := UInt16(AIndex);
  end;

begin
  Outer := TGocciaCompilerScope.Create(nil, 0);
  Inner := TGocciaCompilerScope.Create(Outer, 0);
  try
    Outer.DeclareLocal('up', False);
    Outer.DeclareLocal('hidden', False);
    Inner.DeclareLocal('x', False);
    // Declared, but not reached yet: in its temporal dead zone at the call.
    Inner.DeclareLocal('later', False);
    // A local of an enclosing function the calling function shadows.
    Inner.DeclareLocal('hidden', True);
    UpvalueIdx := Inner.ResolveUpvalue('up');

    SetLength(Bindings, 4);
    SetEntry(Bindings[0], 'x', debLocal, Inner.GetLocal(0).Slot);
    SetEntry(Bindings[1], 'later', debLocal, Inner.GetLocal(1).Slot);
    SetEntry(Bindings[2], 'hidden', debLocal, Inner.GetLocal(2).Slot);
    SetEntry(Bindings[3], 'up', debUpvalue, UpvalueIdx);
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Bindings,
      False, Name, Problem)).ToBe(True);

    // A binding in its temporal dead zone left out.
    Changed := WithoutEntry(Bindings, 1);
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Changed,
      False, Name, Problem)).ToBe(False);
    Expect<string>(Name).ToBe('later');

    // An enclosing function's binding left out.
    Changed := WithoutEntry(Bindings, 3);
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Changed,
      False, Name, Problem)).ToBe(False);
    Expect<string>(Name).ToBe('up');

    // The wrong slot.
    Changed := Copy(Bindings, 0, Length(Bindings));
    Changed[0].Index := Changed[1].Index;
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Changed,
      False, Name, Problem)).ToBe(False);
    Expect<string>(Name).ToBe('x');

    // The shadowed enclosing binding instead of the calling function's.
    Changed := Copy(Bindings, 0, Length(Bindings));
    SetEntry(Changed[2], 'hidden', debUpvalue, UpvalueIdx);
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Changed,
      False, Name, Problem)).ToBe(False);
    Expect<string>(Name).ToBe('hidden');

    // A name nothing declares.
    Changed := Copy(Bindings, 0, Length(Bindings));
    SetLength(Changed, Length(Changed) + 1);
    SetEntry(Changed[High(Changed)], 'ghost', debLocal, 0);
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Changed,
      False, Name, Problem)).ToBe(False);
    Expect<string>(Name).ToBe('ghost');

    // The call is sloppy, the record says strict.
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Bindings,
      True, Name, Problem)).ToBe(False);

    // Where the eval's vars go. Inner has no hidden variable object until
    // the parser's mark makes DeclareEvalVarObjects declare one.
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Inner, True,
      devNone, -1, Problem)).ToBe(True);
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Outer, False,
      devGlobal, -1, Problem)).ToBe(True);
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Inner, False,
      devFunction, -1, Problem)).ToBe(False);
    Expect<Boolean>(Pos('parser did not mark', Problem) > 0).ToBe(True);
    Inner.DirectEvalUsage := [deuDirectEval, deuSloppyDirectEval];
    Inner.DeclareEvalVarObjects(True);
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Inner, False,
      devFunction, Inner.EvalVarObjectSlot, Problem)).ToBe(True);
    // The wrong environment, register or strictness.
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Inner, False,
      devGlobal, -1, Problem)).ToBe(False);
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Inner, False,
      devFunction, Inner.GetLocal(0).Slot, Problem)).ToBe(False);
    Expect<Boolean>(DirectEvalSiteVarEnvironmentMatchesScope(Inner, True,
      devFunction, Inner.EvalVarObjectSlot, Problem)).ToBe(False);
    // The record lists the hidden variable object as a with binding, which
    // eval code probes as the calling function's own code does.
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Bindings,
      False, Name, Problem)).ToBe(False);
    Expect<string>(Name).ToBe(Inner.EvalVarObjectName);
    Changed := Copy(Bindings, 0, Length(Bindings));
    SetLength(Changed, Length(Changed) + 1);
    SetEntry(Changed[High(Changed)], Inner.EvalVarObjectName, debWithLocal,
      Inner.EvalVarObjectSlot);
    Expect<Boolean>(DirectEvalSiteRecordMatchesScope(Inner, True, Changed,
      False, Name, Problem)).ToBe(True);
  finally
    Inner.Free;
    Outer.Free;
  end;
end;

procedure TTestCompiler.TestDirectEvalCapturesCallSiteLocals;

  function ClosesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
  begin
    Module := CompileSource(ASource);
    try
      Result := CountOpRecursive(Module.TopLevel, OP_CLOSE_UPVALUE);
    finally
      Module.Free;
    end;
  end;

begin
  // Eval code reaches a block's bindings through their cells, as a closure
  // created at the call would, so the block closes them when it ends.
  Expect<Integer>(ClosesIn(
    'const f = () => { { let x = 1; const y = 2; evil("0"); } };')).ToBe(0);
  Expect<Integer>(ClosesIn(
    'const f = () => { { let x = 1; const y = 2; eval("0"); } };')).ToBe(2);
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
      // Every case reads p.y; the store itself may be OP_SET_LOCAL or, for a
      // binding that only lives in its register, the operator's destination.
      Func := FindFunctionWithOp(Module.TopLevel, OP_GET_LOCAL_PROP_CONST);
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
      // The two declarations evaluate their initializer straight into the
      // binding, which neither initializer can reach; the two stores move
      // nothing, and neither copies an operand.
      Expect<Integer>(CountOp(Func, OP_MOVE)).ToBe(0);
      Expect<Integer>(CountOp(Func, OP_GET_LOCAL)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestLexicalHolesSetOnlyWhereObservable;

  function HolesIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_ADD);
      if Assigned(Func) then
        Result := CountOp(Func, OP_LOAD_HOLE);
    finally
      Module.Free;
    end;
  end;

begin
  // Nothing before either declaration completes can reach a or b, so the
  // function body sets no hole for them.
  Expect<Integer>(HolesIn(
    'const f = (p) => { const a = p + 1; const b = a * 2; return b; };'))
    .ToBe(0);
  // The arrow created by the first initializer names b, so b keeps its hole.
  Expect<Integer>(HolesIn(
    'const f = (p) => { const r = () => b; const b = p + 1; return r; };'))
    .ToBe(1);
  // A block's leading binding that nothing can reach before its declaration
  // completes gets no hole either, so a loop body sets none per iteration.
  Expect<Integer>(HolesIn(
    'const f = (p) => { if (p) { let c = p + 1; return c; } return 0; };'))
    .ToBe(0);
  // A block sets the hole of a binding an earlier initializer's arrow names,
  // or one declared after another statement, on entry, and the declaration
  // does not set it again.
  Expect<Integer>(HolesIn(
    'const f = (p) => { if (p) { const r = () => c; let c = p + 1; return r; } return 0; };'))
    .ToBe(1);
  Expect<Integer>(HolesIn(
    'const f = (p) => { if (p) { p.x = 1; let c = p + 1; return c; } return 0; };'))
    .ToBe(1);
end;

procedure TTestCompiler.TestCompoundSmallIntegerUsesImmediate;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  // x holds only Numbers and no closure can rebind it: += 1 and -= 300 add
  // immediates.
  Module := CompileSource(
    'const f = (p) => { let x = 0; if (p) { x = 5; } x += 1; x -= 300; return x; }; f(1);');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_ADD_NUM_IMM);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
    begin
      Expect<Integer>(CountOp(Func, OP_ADD_NUM_IMM)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_SUB_NUM_IMM)).ToBe(1);
      Expect<Integer>(CountOp(Func, OP_ADD) + CountOp(Func, OP_SUB) +
        CountOp(Func, OP_ADD_FLOAT) + CountOp(Func, OP_SUB_FLOAT)).ToBe(0);
    end;
  finally
    Module.Free;
  end;
  // A closure can store a String in x, so += keeps the generic operator.
  Module := CompileSource(
    'const f = (p) => { let x = 0; const g = () => { x = "s"; }; g(); x += 1; return x; }; f(1);');
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestInitializerReachingItsBindingKeepsCopy;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
begin
  // The initializer names its own binding, so it runs into a temporary and the
  // binding keeps its TDZ hole until the initializer completes.
  Module := CompileSource(
    'const read = (p) => { const value = p.x + value; return value; };');
  try
    Func := FindFunctionWithOp(Module.TopLevel, OP_ADD);
    Expect<Boolean>(Assigned(Func)).ToBe(True);
    if Assigned(Func) then
      Expect<Integer>(CountOp(Func, OP_MOVE)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCompoundAssignmentWritesRegisterOnlyBindingDirectly;

  function SetLocalsIn(const ASource: string): Integer;
  var
    Module: TGocciaBytecodeModule;
    Func: TGocciaFunctionTemplate;
  begin
    Result := -1;
    Module := CompileSource(ASource);
    try
      Func := FindFunctionWithOp(Module.TopLevel, OP_GET_LOCAL_PROP_CONST);
      if Assigned(Func) then
        Result := CountOp(Func, OP_SET_LOCAL);
    finally
      Module.Free;
    end;
  end;

begin
  Expect<Integer>(SetLocalsIn(
    'const f = (p) => { let x = p.x; x += p.y; return x; };')).ToBe(0);
  // A closure reaches the binding through its cell, which OP_SET_LOCAL keeps
  // in step with the register.
  Expect<Integer>(SetLocalsIn(
    'const f = (p) => { let x = p.x; const g = () => x; x += p.y; ' +
    'return g; };')).ToBe(1);
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

procedure TTestCompiler.TestGeneratedNamesPerCompilation;
const
  SOURCES: array[0..2] of string = (
    'const make = () => class { #x = 1; get() { return this.#x; } };',
    'const key = "x"; class C { [key] = 1; static [key] = 2; }',
    'const outer = () => { eval("var x = 1"); ' +
      'return (a = eval("var y = 2")) => { eval("var z = 3"); return x; }; };');
var
  Original, Repeated, Unrelated: TGocciaBytecodeModule;
  OriginalStream, RepeatedStream: TMemoryStream;
  I: Integer;

  procedure Serialize(const AModule: TGocciaBytecodeModule;
    const AStream: TMemoryStream);
  var
    Writer: TGocciaBytecodeWriter;
  begin
    Writer := TGocciaBytecodeWriter.Create(AStream);
    try
      Writer.WriteModule(AModule);
    finally
      Writer.Free;
    end;
  end;
begin
  for I := 0 to High(SOURCES) do
  begin
    Original := CompileSource(SOURCES[I], False, False, False, True, True,
      True, False, True);
    try
      Unrelated := CompileSource(SOURCES[(I + 1) mod Length(SOURCES)],
        False, False, False, True, True, True, False, True);
      Unrelated.Free;
      Repeated := CompileSource(SOURCES[I], False, False, False, True, True,
        True, False, True);
      OriginalStream := TMemoryStream.Create;
      RepeatedStream := TMemoryStream.Create;
      try
        Serialize(Original, OriginalStream);
        Serialize(Repeated, RepeatedStream);
        Expect<Int64>(RepeatedStream.Size).ToBe(OriginalStream.Size);
        if RepeatedStream.Size = OriginalStream.Size then
          Expect<Boolean>(CompareMem(OriginalStream.Memory,
            RepeatedStream.Memory, OriginalStream.Size)).ToBe(True);
      finally
        RepeatedStream.Free;
        OriginalStream.Free;
        Repeated.Free;
      end;
    finally
      Original.Free;
    end;
  end;
end;

procedure TTestCompiler.TestObjectRestBytecodeRoundTrip;
const
  SOURCES: array[0..2] of string = (
    'const f = ({ a, ...rest }, b = rest) => b;',
    'const f = ({ nested: { a, ...rest } }) => rest;',
    'const f = (value) => { let rest; ({ a: value, ...rest } = value); ' +
      'return rest; };');
var
  Original, Loaded: TGocciaBytecodeModule;
  Stream: TMemoryStream;
  Writer: TGocciaBytecodeWriter;
  Reader: TGocciaBytecodeReader;
  I: Integer;
begin
  for I := 0 to High(SOURCES) do
  begin
    Original := CompileSource(SOURCES[I]);
    Stream := TMemoryStream.Create;
    try
      Expect<Boolean>(Assigned(FindFunctionWithOp(Original.TopLevel,
        OP_COLLECTION_OP))).ToBe(True);
      Writer := TGocciaBytecodeWriter.Create(Stream);
      try
        Writer.WriteModule(Original);
      finally
        Writer.Free;
      end;
      Stream.Position := 0;
      Reader := TGocciaBytecodeReader.Create(Stream);
      try
        // Loading checks the OP_MOVE destination that supplies the exclusion
        // list at the rest destination's next slot against the frame size.
        Loaded := Reader.ReadModule;
        try
          Expect<Boolean>(Assigned(FindFunctionWithOp(Loaded.TopLevel,
            OP_COLLECTION_OP))).ToBe(True);
        finally
          Loaded.Free;
        end;
      finally
        Reader.Free;
      end;
    finally
      Stream.Free;
      Original.Free;
    end;
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

{ The loader adds each constant again in order, so a pool holding both zeros
  must come back with the same entries at the same indices, which the
  instructions name. Source compiles +0 to an immediate load, so it is added
  to the pool directly. }
procedure TTestCompiler.TestBinaryRoundTripKeepsBothZeros;
var
  Original, Loaded: TGocciaBytecodeModule;
  TempFile: string;
  I: Integer;
  OrigConst, LoadConst: TGocciaBytecodeConstant;
begin
  Original := CompileSource('const a = -0; const b = 1.5; const c = -1.5;');
  try
    Original.TopLevel.AddConstantString('between');
    Original.TopLevel.AddConstantFloat(0.0);
    Original.TopLevel.AddConstantFloat(NegativeZero);
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
          if OrigConst.Kind = bckFloat then
            Expect<UInt64>(DoubleToBits(LoadConst.FloatValue)).ToBe(
              DoubleToBits(OrigConst.FloatValue));
        end;
        for I := 0 to Original.TopLevel.CodeCount - 1 do
          Expect<UInt32>(Loaded.TopLevel.GetInstruction(I)).ToBe(
            Original.TopLevel.GetInstruction(I));
      finally
        Loaded.Free;
      end;
    finally
      DeleteFile(TempFile);
    end;
  finally
    Original.Free;
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
    Module.CompileProfile := Default(TGocciaCompileProfile);
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
  Expect<Boolean>(IsValidGocciaOpCode(146)).ToBe(False);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_SET_PRIVATE_CLASS))).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_SET_PRIVATE_CLASS)).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CLASS_ADD_METHOD_DYNAMIC))).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_CLASS_ADD_METHOD_DYNAMIC)).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterC(OP_CLASS_ADD_METHOD_DYNAMIC)).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CALL_SELF_NUM))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_GET_LOCAL_PROP_CONST))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_ADD_NUM_IMM))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_JUMP_IF_NOT_LT))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_JUMP_IF_NOT_LT_IMM))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_JUMP_IF_STRICT_EQ_IMM))).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterB(OP_JUMP_IF_NOT_GT_IMM)).ToBe(False);
  Expect<Boolean>(GocciaOpCodeUsesRegisterC(OP_JUMP_IF_NOT_GT_IMM)).ToBe(False);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CREATE_GLOBAL_IMPORT_BINDING))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_CHECK_BINDING_INITIALIZED))).ToBe(True);
  Expect<Boolean>(IsValidGocciaOpCode(Ord(OP_THROW_UNDEFINED_VARIABLE))).ToBe(True);
  Expect<Boolean>(GocciaOpCodeUsesRegisterA(OP_THROW_UNDEFINED_VARIABLE)).ToBe(False);
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

  Template := TGocciaFunctionTemplate.Create('invalid-class-method-key-register');
  Template.MaxRegisters := 2;
  Template.EmitInstruction(EncodeABC(OP_CLASS_ADD_METHOD_DYNAMIC, 0, 2, 1));
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

// A compile that raises leaves the levels it had entered open. A compiler
// reused after it must not start the next walk from them: repeated failures
// would otherwise push every later walk past NESTING_UNCHECKED_DEPTH, where
// each node pays a native-stack check.
procedure TTestCompiler.TestCompileAfterRaisedCompileStartsAtNestingDepthZero;
var
  Compiler: TGocciaCompiler;
  Module: TGocciaBytecodeModule;
  Attempt: Integer;
  Raised: Boolean;

  function ParseSource(const ASource: string;
    const ANonStrictMode: Boolean): TGocciaProgram;
  var
    Lexer: TGocciaLexer;
    Parser: TGocciaParser;
    SourceLines: TStringList;
    ParserOptions: TGocciaParserOptions;
  begin
    Lexer := TGocciaLexer.Create(ASource, '<test>');
    SourceLines := CreateTextLines(ASource);
    Parser := TGocciaParser.CreateFromLexer(Lexer, '<test>', SourceLines);
    try
      ParserOptions := Parser.Options;
      ParserOptions.NonStrictModeEnabled := ANonStrictMode;
      Parser.ApplyOptions(ParserOptions);
      Result := Parser.Parse;
    finally
      Parser.Free;
      SourceLines.Free;
      Lexer.Free;
    end;
  end;

  procedure CompileProgram(const ASource: string;
    const ANonStrictParse: Boolean);
  var
    ProgramNode: TGocciaProgram;
  begin
    ProgramNode := ParseSource(ASource, ANonStrictParse);
    try
      Module := Compiler.Compile(ProgramNode);
    finally
      ProgramNode.Free;
    end;
  end;

begin
  Compiler := TGocciaCompiler.Create('<test>');
  try
    // The parser accepts `with` in non-strict code; the compiler, strict,
    // rejects it five levels into the walk: the declaration, the arrow, the
    // if, its block and the `with` itself.
    Compiler.NonStrictMode := False;
    for Attempt := 1 to 3 do
    begin
      Raised := False;
      Module := nil;
      try
        CompileProgram(
          'const outer = () => { if (true) { with ({}) {} } };', True);
      except
        on E: TGocciaSyntaxError do
          Raised := True;
      end;
      Module.Free;
      Expect<Boolean>(Raised).ToBe(True);
      Expect<Boolean>(Compiler.NestingDepth > 0).ToBe(True);
    end;

    Module := nil;
    CompileProgram('const a = (1 + (2 + 3)); a;', False);
    try
      Expect<Boolean>(Assigned(Module)).ToBe(True);
      Expect<Integer>(Compiler.NestingDepth).ToBe(0);
    finally
      Module.Free;
    end;
  finally
    Compiler.Free;
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

procedure TTestCompiler.TestBytecodeLimitRaisesSyntaxErrorAtSource;

  // The SyntaxError compiling ASource raises: its message, line and column.
  procedure ExpectLimit(const ASource, AMessage: string;
    const ALine, AColumn: Integer);
  var
    Raised: Boolean;
    Message: string;
    Line, Column: Integer;
  begin
    Raised := False;
    Line := 0;
    Column := 0;
    try
      CompileSource(ASource).Free;
    except
      on E: TGocciaSyntaxError do
      begin
        Raised := True;
        Message := E.Message;
        Line := E.Line;
        Column := E.Column;
      end;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(Pos(AMessage, Message) > 0).ToBe(True);
    Expect<Integer>(Line).ToBe(ALine);
    Expect<Integer>(Column).ToBe(AColumn);
  end;

var
  Args, Keys: string;
  I: Integer;
begin
  Args := '';
  for I := 1 to 70000 do
    Args := Args + '0, ';
  // The call's argument count is checked once its arguments are parsed, so
  // the error names the call.
  ExpectLimit('const f = (...a) => a.length;' + #10 + '  ' + #10 +
    '  f(' + Args + '0);', 'Too many arguments in one call: more than 65535',
    3, 3);

  Keys := '';
  for I := 0 to 69999 do
    Keys := Keys + 'p' + IntToStr(I) + ': 0, ';
  // A key's constant is added after its value is compiled, so the error
  // names the object literal.
  ExpectLimit('const o = {' + #10 + Keys + '};',
    'Too many constants in one function: more than 65535', 1, 11);
end;

procedure TTestCompiler.TestLongBranchTakesLongFormInItsFunctionOnly;
var
  Body: string;
  I: Integer;
  Module: TGocciaBytecodeModule;
  Small, Long: TGocciaFunctionTemplate;
begin
  Body := '';
  for I := 1 to 40000 do
    Body := Body + 'r = r + 1; ';
  Module := CompileSource('const small = (k) => k ? 1 : 2;' + #10 +
    'const long = (k) => { let r = 0; if (k) { ' + Body + '} return r; };');
  try
    Small := Module.TopLevel.GetFunction(0);
    Long := Module.TopLevel.GetFunction(1);
    // The short function's conditional jump keeps its one instruction, and
    // the top level has no jump.
    Expect<Integer>(CountOp(Small, OP_JUMP)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_JUMP)).ToBe(0);
    // The if without an else has no OP_JUMP of its own: the two are its
    // conditional jump's long form.
    Expect<Integer>(CountOp(Long, OP_JUMP)).ToBe(2);
  finally
    Module.Free;
  end;
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
    Expect<Integer>(CountNamedGlobalReads(Reader)).ToBe(0);
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
      CountOp(Module.TopLevel.GetFunction(0), OP_GET_GLOBAL_DECLARED)).ToBe(1);
    Expect<Integer>(
      CountNamedGlobalReads(Module.TopLevel.GetFunction(1))).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstPropagationSkipsGlobalBackedUnderSloppyDirectEval;
var
  Module: TGocciaBytecodeModule;
begin
  // A sloppy direct eval in g can shadow the name with a var of g, and in the
  // function nested in h, a var of h. f contains none and is nested in none.
  Module := CompileSource('const a = 2; const f = () => a;' +
    ' const g = () => { eval("var a = 3"); return a; };' +
    ' const h = () => { eval("0"); return () => a; };' +
    ' f() + g() + h()();',
    False, False, True, True, True, True, False, True);
  try
    Expect<Integer>(
      CountNamedGlobalReads(Module.TopLevel.GetFunction(0))).ToBe(0);
    Expect<Boolean>(HasLoadInt(Module.TopLevel.GetFunction(0), 2)).ToBe(True);
    // g reads `eval` and `a` by name.
    Expect<Integer>(
      CountNamedGlobalReads(Module.TopLevel.GetFunction(1))).ToBe(2);
    Expect<Integer>(CountOp(Module.TopLevel.GetFunction(2).GetFunction(0),
      OP_GET_GLOBAL_DECLARED)).ToBe(1);
  finally
    Module.Free;
  end;
end;

// A script var can redeclare a configurable global property and leave it
// configurable (ES2026 §9.1.1.4.16 CreateGlobalVarBinding), so the binding
// can be deleted. Reads of it then throw (§6.2.5.5 GetValue step 2), which
// OP_GET_GLOBAL_DECLARED does on its miss path; `typeof` gives "undefined"
// (§13.5.3.1 step 2.a) and keeps OP_GET_GLOBAL.
procedure TTestCompiler.TestGlobalBackedReadsThrowForAMissingBinding;
var
  Module: TGocciaBytecodeModule;
  Reader, TypeofReader, Updater: TGocciaFunctionTemplate;
begin
  Module := CompileSource('var x = 1; const read = () => x;' +
    ' const kind = () => typeof x; const bump = () => { x += 1; };' +
    ' read() + kind() + x;',
    False, False, True, True, True, True, False, False, True);
  try
    Reader := Module.TopLevel.GetFunction(0);
    TypeofReader := Module.TopLevel.GetFunction(1);
    Updater := Module.TopLevel.GetFunction(2);
    Expect<Integer>(CountOp(Reader, OP_GET_GLOBAL_DECLARED)).ToBe(1);
    Expect<Integer>(CountOp(Reader, OP_GET_GLOBAL)).ToBe(0);
    Expect<Integer>(CountOp(TypeofReader, OP_GET_GLOBAL)).ToBe(1);
    Expect<Integer>(CountOp(TypeofReader, OP_GET_GLOBAL_DECLARED)).ToBe(0);
    Expect<Integer>(CountOp(Updater, OP_GET_GLOBAL_DECLARED)).ToBe(1);
    Expect<Integer>(CountOp(Updater, OP_GET_GLOBAL)).ToBe(0);
    Expect<Boolean>(
      CountOp(Module.TopLevel, OP_GET_GLOBAL_DECLARED) >= 1).ToBe(True);
  finally
    Module.Free;
  end;
end;

// ADR 0134: the parser marks the functions that contain a sloppy direct eval,
// and only a binding read across one of them loses what the compiler knows
// about it (DirectEvalMayShadow). Before, every non-strict function did.
procedure TTestCompiler.TestSloppyFunctionFoldsEnclosingConstWithoutDirectEval;
const
  SOURCE =
    'const outer = () => {' +
    '  const k = 3;' +
    '  const plain = () => k * 2;' +
    '  const evals = () => { eval("var k = 1"); return k * 2; };' +
    '  const nested = () => { eval("0"); return () => k * 2; };' +
    '  const strictEval = () => { "use strict"; eval("var k = 1");' +
    '    return k * 2; };' +
    '  return plain() + evals() + nested()() + strictEval();' +
    '};';
var
  Module: TGocciaBytecodeModule;
  Outer, Plain, Evals, NestedInner, StrictEval: TGocciaFunctionTemplate;
begin
  Module := CompileSource(SOURCE, False, False, False, True, True, True,
    False, True);
  try
    Outer := Module.TopLevel.GetFunction(0);
    Expect<Boolean>(Outer.StrictCode).ToBe(False);
    Plain := Outer.GetFunction(0);
    Evals := Outer.GetFunction(1);
    NestedInner := Outer.GetFunction(2).GetFunction(0);
    StrictEval := Outer.GetFunction(3);

    // No sloppy direct eval between the read and the const: k * 2 is 6.
    Expect<Integer>(CountOp(Plain, OP_GET_UPVALUE)).ToBe(0);
    Expect<Integer>(CountArithmeticOps(Plain)).ToBe(0);
    Expect<Boolean>(HasLoadInt(Plain, 6)).ToBe(True);

    // The eval can declare a var k of the reading function, or of the
    // function the read is nested in. The read probes that function's
    // hidden variable object, a local of Evals and an upvalue of
    // NestedInner, before it reads the upvalue k.
    Expect<Integer>(CountOp(Evals, OP_GET_UPVALUE)).ToBe(1);
    Expect<Boolean>(CountArithmeticOps(Evals) > 0).ToBe(True);
    Expect<Integer>(CountOp(NestedInner, OP_GET_UPVALUE)).ToBe(2);
    Expect<Integer>(CountOp(NestedInner, OP_HAS_WITH_BINDING)).ToBe(1);
    Expect<Boolean>(CountArithmeticOps(NestedInner) > 0).ToBe(True);

    // A strict eval's vars stay in the eval.
    Expect<Integer>(CountOp(StrictEval, OP_GET_UPVALUE)).ToBe(0);
    Expect<Boolean>(HasLoadInt(StrictEval, 6)).ToBe(True);
  finally
    Module.Free;
  end;
end;

// The record of each eval site names where the eval's vars go (ES2026
// §19.2.1.1 PerformEval) and, inside a function, the register of the hidden
// variable object that stands for that environment. OP_DIRECT_EVAL names the
// record and comes right before the call on the same callee register.
procedure TTestCompiler.TestDirectEvalSiteRecordNamesVarEnvironment;
const
  SOURCE =
    'eval("0");' +
    'const f = (a, b = eval("1")) => { eval("2"); };' +
    'const g = (a) => { eval("3"); };' +
    'const h = () => { "use strict"; eval("4"); };';
var
  Module: TGocciaBytecodeModule;
  F, G, H: TGocciaFunctionTemplate;
  ParameterEnv, BodyEnv: TGocciaDirectEvalEnvironment;

  procedure ExpectSitesPrecedeCalls(const ATemplate: TGocciaFunctionTemplate);
  var
    PC, Sites: Integer;
    Instruction, Next: UInt32;
  begin
    Sites := 0;
    for PC := 0 to ATemplate.CodeCount - 1 do
    begin
      Instruction := ATemplate.GetInstruction(PC);
      if DecodeOp(Instruction) <> Ord(OP_DIRECT_EVAL) then
        Continue;
      Expect<Integer>(DecodeBx(Instruction)).ToBe(Sites);
      Expect<Integer>(ATemplate.GetDirectEvalEnvironment(Sites).PC).ToBe(PC);
      Next := ATemplate.GetInstruction(PC + 1);
      Expect<Integer>(DecodeOp(Next)).ToBe(Ord(OP_CALL));
      Expect<Integer>(DecodeA(Next)).ToBe(DecodeA(Instruction));
      Inc(Sites);
    end;
    Expect<Integer>(Sites).ToBe(ATemplate.DirectEvalEnvironmentCount);
  end;

begin
  Module := CompileSource(SOURCE, False, False, False, True, True, True,
    False, True);
  try
    Expect<Integer>(Module.TopLevel.DirectEvalEnvironmentCount).ToBe(1);
    Expect<Integer>(Ord(Module.TopLevel.GetDirectEvalEnvironment(0)
      .VarEnvironment)).ToBe(Ord(devGlobal));
    Expect<Integer>(
      Module.TopLevel.GetDirectEvalEnvironment(0).VarObjectSlot).ToBe(-1);
    ExpectSitesPrecedeCalls(Module.TopLevel);

    // A parameter list with an expression has an environment of its own.
    // Each call has a site for every binding `eval` may resolve to: the
    // hidden variable objects a var of that name could be in, nearest
    // first, then the global: two in the parameter list, three in the body.
    F := Module.TopLevel.GetFunction(0);
    Expect<Integer>(F.DirectEvalEnvironmentCount).ToBe(5);
    ParameterEnv := F.GetDirectEvalEnvironment(0);
    BodyEnv := F.GetDirectEvalEnvironment(2);
    Expect<Integer>(Ord(F.GetDirectEvalEnvironment(1).VarEnvironment))
      .ToBe(Ord(devParameters));
    Expect<Integer>(Ord(F.GetDirectEvalEnvironment(4).VarEnvironment))
      .ToBe(Ord(devFunction));
    Expect<Integer>(Ord(ParameterEnv.VarEnvironment))
      .ToBe(Ord(devParameters));
    Expect<Integer>(Ord(BodyEnv.VarEnvironment)).ToBe(Ord(devFunction));
    Expect<Boolean>(ParameterEnv.VarObjectSlot >= 0).ToBe(True);
    Expect<Boolean>(BodyEnv.VarObjectSlot >= 0).ToBe(True);
    Expect<Boolean>(ParameterEnv.VarObjectSlot <> BodyEnv.VarObjectSlot)
      .ToBe(True);
    Expect<Boolean>(BodyEnv.VarObjectSlot < F.MaxRegisters).ToBe(True);
    Expect<Boolean>(ParameterEnv.VarObjectSlot < F.MaxRegisters).ToBe(True);
    // The hidden locals follow the parameters, so a call's arguments land
    // in the parameters' registers as before.
    Expect<Boolean>(BodyEnv.VarObjectSlot > 2).ToBe(True);
    ExpectSitesPrecedeCalls(F);

    // A simple parameter list shares the function's environment.
    G := Module.TopLevel.GetFunction(1);
    Expect<Integer>(G.DirectEvalEnvironmentCount).ToBe(2);
    Expect<Integer>(Ord(G.GetDirectEvalEnvironment(0).VarEnvironment))
      .ToBe(Ord(devFunction));
    Expect<Boolean>(G.GetDirectEvalEnvironment(0).VarObjectSlot > 1)
      .ToBe(True);

    H := Module.TopLevel.GetFunction(2);
    Expect<Integer>(Ord(H.GetDirectEvalEnvironment(0).VarEnvironment))
      .ToBe(Ord(devNone));
    Expect<Integer>(H.GetDirectEvalEnvironment(0).VarObjectSlot).ToBe(-1);

    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_DIRECT_EVAL))
      .ToBe(9);
  finally
    Module.Free;
  end;
end;

// ADR 0134 Phase 3: eval code resolves private names as the code at its call
// does (ES2026 §19.2.1.1 PerformEval: privateEnv is the running context's
// PrivateEnvironment). A method's eval sees its class body's names and gets
// the class from the running closure; a computed key runs inline in the
// enclosing function, so its record names the register of the class being
// defined; and an `extends` expression runs in the outer PrivateEnvironment
// (§15.7.14), so the class's own names are not visible there.
procedure TTestCompiler.TestDirectEvalSiteRecordNamesPrivateEnvironment;
var
  Module: TGocciaBytecodeModule;
  Env: TGocciaDirectEvalEnvironment;
  Method, Inner: TGocciaFunctionTemplate;
  I: Integer;

  function NameList(const AEnv: TGocciaDirectEvalEnvironment): string;
  var
    I: Integer;
  begin

    Result := '';
    for I := 0 to High(AEnv.PrivateNames) do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + AEnv.PrivateNames[I].Name;
      Expect<Boolean>((AEnv.PrivateNames[I].Prefix <> '') and
        (AEnv.PrivateNames[I].Prefix[Length(AEnv.PrivateNames[I].Prefix)] =
         '$')).ToBe(True);
    end;
  end;

begin
  Module := CompileSource('class A { #a = 1; m() { return eval("1"); } }');
  try
    Method := FindTemplateWithDirectEval(Module.TopLevel);
    Expect<Boolean>(Assigned(Method)).ToBe(True);
    Env := Method.GetDirectEvalEnvironment(0);
    Expect<string>(NameList(Env)).ToBe('a');
    Expect<Integer>(Env.PrivateClassRegister).ToBe(-1);
  finally
    Module.Free;
  end;

  Module := CompileSource('class A { #a = 1; static [eval("\"k\"")] = 1; }');
  try
    Expect<Integer>(Module.TopLevel.DirectEvalEnvironmentCount).ToBe(1);
    Env := Module.TopLevel.GetDirectEvalEnvironment(0);
    Expect<string>(NameList(Env)).ToBe('a');
    Expect<Boolean>(Env.PrivateClassRegister >= 0).ToBe(True);
    Expect<Boolean>(Env.PrivateClassRegister <
      Module.TopLevel.MaxRegisters).ToBe(True);
  finally
    Module.Free;
  end;

  // The inner class's #b shadows nothing and the outer #a stays visible; in
  // the inner class's heritage only #a is.
  Module := CompileSource('class A { #a = 1; static m() { ' +
    'class B extends (eval("0"), Object) { #b; n() { return eval("2"); } } ' +
    '} }');
  try
    Method := FindTemplateWithDirectEval(Module.TopLevel);
    Expect<Boolean>(Assigned(Method)).ToBe(True);
    Env := Method.GetDirectEvalEnvironment(0);
    Expect<string>(NameList(Env)).ToBe('a');
    Expect<Integer>(Env.PrivateClassRegister).ToBe(-1);
    Inner := nil;
    for I := 0 to Method.FunctionCount - 1 do
    begin
      Inner := FindTemplateWithDirectEval(Method.GetFunction(I));
      if Assigned(Inner) then
        Break;
    end;
    Expect<Boolean>(Assigned(Inner)).ToBe(True);
    if Assigned(Inner) then
      Expect<string>(NameList(Inner.GetDirectEvalEnvironment(0)))
        .ToBe('a,b');
  finally
    Module.Free;
  end;
end;

// ADR 0134 Phase 3: eval code is compiled as a function nested at its call
// site against the call's record. A binding of the calling function becomes
// an upvalue that names the caller's register, one of an enclosing function
// an upvalue that names the caller closure's upvalue index, and a name the
// record does not list stays a global reference. The eval's own declarations
// are locals of its template, which is an arrow-like strict function.
procedure TTestCompiler.TestDirectEvalCompilesAsClosureOfCallSite;
const
  CALLER_SOURCE = 'const f = (a) => { let b = 2; ' +
    'return () => { let c = 3; return eval("x"); }; };';
  EVAL_SOURCE = 'let own = 1; a + b + c + own + typeof unlisted';
var
  Caller, EvalModule: TGocciaBytecodeModule;
  Site: TGocciaFunctionTemplate;
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  SourceLines: TStringList;
  Template: TGocciaFunctionTemplate;
  Descriptor: TGocciaUpvalueDescriptor;
  I, Matched: Integer;
begin
  Caller := CompileSource(CALLER_SOURCE);
  EvalModule := nil;
  try
    Site := FindTemplateWithDirectEval(Caller.TopLevel);
    Expect<Boolean>(Assigned(Site)).ToBe(True);
    if not Assigned(Site) then
      Exit;
    Env := Site.GetDirectEvalEnvironment(0);
    Expect<Boolean>(Env.StrictCaller).ToBe(True);

    Lexer := TGocciaLexer.Create(EVAL_SOURCE, '<eval>');
    SourceLines := CreateTextLines(EVAL_SOURCE);
    Parser := TGocciaParser.CreateFromLexer(Lexer, '<eval>', SourceLines);
    ProgramNode := Parser.Parse;
    Compiler := TGocciaCompiler.Create('<eval>');
    try
      EvalModule := Compiler.CompileDirectEval(ProgramNode, Env, True, nil);
    finally
      Compiler.Free;
      ProgramNode.Free;
      Parser.Free;
      SourceLines.Free;
      Lexer.Free;
    end;

    Template := EvalModule.TopLevel;
    Expect<string>(Template.Name).ToBe(DIRECT_EVAL_TEMPLATE_NAME);
    Expect<Boolean>(Template.IsArrow).ToBe(True);
    Expect<Boolean>(Template.StrictCode).ToBe(True);
    Expect<Integer>(Template.UpvalueCount).ToBe(3);
    Matched := 0;
    for I := 0 to Template.UpvalueCount - 1 do
    begin
      Descriptor := Template.GetUpvalueDescriptor(I);
      Expect<Boolean>(FindDirectEvalRecordEntry(Env, Descriptor.Name,
        Binding)).ToBe(True);
      // `c` is the arrow's own local; `a` and `b` are its upvalues.
      if Descriptor.Name = 'c' then
      begin
        Expect<Integer>(Ord(Binding.Kind)).ToBe(Ord(debLocal));
        Expect<Boolean>(Descriptor.IsLocal).ToBe(True);
        Inc(Matched);
      end
      else if (Descriptor.Name = 'a') or (Descriptor.Name = 'b') then
      begin
        Expect<Integer>(Ord(Binding.Kind)).ToBe(Ord(debUpvalue));
        Expect<Boolean>(Descriptor.IsLocal).ToBe(False);
        Inc(Matched);
      end;
      Expect<Integer>(Descriptor.Index).ToBe(Binding.Index);
    end;
    Expect<Integer>(Matched).ToBe(3);
    Expect<Integer>(CountOp(Template, OP_GET_GLOBAL)).ToBe(1);
  finally
    EvalModule.Free;
    Caller.Free;
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
      CountOp(Module.TopLevel.GetFunction(0), OP_GET_GLOBAL_DECLARED)).ToBe(1);
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

procedure TTestCompiler.TestConciseArrowBodyMarksTailCalls;

  function TailCalls(const ATemplate: TGocciaFunctionTemplate): Integer;
  var
    I: Integer;
    Instruction: UInt32;
  begin
    Result := 0;
    for I := 0 to ATemplate.CodeCount - 1 do
    begin
      Instruction := ATemplate.GetInstruction(I);
      if (TGocciaOpCode(DecodeOp(Instruction)) in [OP_CALL, OP_CALL_METHOD]) and
         ((DecodeC(Instruction) and CALL_FLAG_TAIL) <> 0) then
        Inc(Result);
    end;
    for I := 0 to ATemplate.FunctionCount - 1 do
      Inc(Result, TailCalls(ATemplate.GetFunction(I)));
  end;

  procedure ExpectTailCalls(const ASource: string; const AExpected: Integer;
    const ANonStrictMode: Boolean = False);
  var
    Module: TGocciaBytecodeModule;
  begin
    Module := CompileSource(ASource, False, False, False, True, True, True,
      False, ANonStrictMode);
    try
      Expect<Integer>(TailCalls(Module.TopLevel)).ToBe(AExpected);
    finally
      Module.Free;
    end;
  end;

var
  Module: TGocciaBytecodeModule;
begin
  // ES2026 §15.10.2: the call, each branch of a conditional, the right
  // operand of &&, || and ??, and the last operand of a comma expression.
  ExpectTailCalls('const f = (n) => f(n - 1);', 1);
  ExpectTailCalls('const f = (n) => n ? f(n - 1) : f(n - 2);', 2);
  ExpectTailCalls('const f = (n) => n && f(n - 1);', 1);
  ExpectTailCalls('const f = (n) => n || f(n - 1);', 1);
  ExpectTailCalls('const f = (n) => n ?? f(n - 1);', 1);
  ExpectTailCalls('const f = (n) => (n, f(n - 1));', 1);
  ExpectTailCalls('const o = { f: (n) => o.f(n - 1) };', 1);
  // Not in tail position: an operand, an argument, the condition.
  ExpectTailCalls('const f = (n) => 1 + f(n - 1);', 0);
  ExpectTailCalls('const f = (n) => [f(n - 1)];', 0);
  ExpectTailCalls('const f = (n) => f(n - 1) ? 1 : 2;', 0);
  ExpectTailCalls('const f = (n) => (f(n - 1), 1);', 0);
  // IsInTailPosition (§15.10.1): never in sloppy code or an async body.
  ExpectTailCalls('const f = async (n) => f(n - 1);', 0);
  ExpectTailCalls('const f = (n) => f(n - 1);', 0, True);

  // A closed numeric arrow with a tail self-call keeps ordinary frames: no
  // frame may run above a closed numeric frame (ADR 0127).
  Module := CompileSource(
    'const run = () => {' +
    '  const h = (n) => n <= 0 ? 0 : (n < 5 ? h(n - 1) : h(n - 1) + 1);' +
    '  h(10);' +
    '}; run();', False, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_CALL_SELF_NUM)).ToBe(0);
    Expect<Integer>(TailCalls(Module.TopLevel)).ToBe(1);
    // Its parameter is still a proven Number.
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(2);
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
    // `a += 2` adds an immediate, `a + b` adds two registers.
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_FLOAT)).ToBe(1);
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_MUL)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestUncheckedStrictBindingDoesNotTypeOperands;
var
  Module: TGocciaBytecodeModule;
begin
  // An annotated let declared without an initializer holds undefined, which
  // no check has seen: its reads, a closure's reads, a const copied from it
  // and a call returning it stay generic (#1912).
  Module := CompileSource(
    'let y: number; y + 1; 1 + y; y - 1; y + y;' +
    'let z!: number; z + 1;' +
    'const h = () => y + 1; const c = y; c + 1;' +
    'const f = () => { let u: number; return u; }; f() + 1;',
    True);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
  finally
    Module.Free;
  end;

  // The annotation is still enforced on every assignment.
  Module := CompileSource('let y: number; y = 1; y = "x";', True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
  finally
    Module.Free;
  end;

  // A rest parameter holds an array whatever its annotation names, so `+=`
  // can concatenate and its result is checked.
  Module := CompileSource(
    'const r = (...xs: number) => xs - 1; const s = (...xs: number) => xs + 1;' +
    'const t = (...xs: number) => { xs += 1; };',
    True);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
  finally
    Module.Free;
  end;

  // A var holds undefined until its declaration runs, initializer or not.
  Module := CompileSource('var v: number; v + 1; var w: number = 2; w - 1;',
    True, False, False, True, True, True, False, False, True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(0);
    Expect<Integer>(CountOp(Module.TopLevel, OP_SUB_NUM_IMM)).ToBe(0);
  finally
    Module.Free;
  end;

  // A checked initializer keeps the typed operator.
  Module := CompileSource('let k: number = 1; k + 1;', True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_ADD_NUM_IMM)).ToBe(1);
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
    'const f = (x): number => x; f() + 1; f() + f();' +
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
    'const f = (x): number => x; const r = f(); r + 1;' +
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

function TTestCompiler.RecordedLocalTypes(const ASource: string;
  const AStrictTypes: Boolean; const ANonStrictMode: Boolean): string;
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
  I: Integer;
begin
  Result := '';
  Module := CompileSource(ASource, AStrictTypes, False, False, False, False,
    False, False, ANonStrictMode);
  try
    Func := Module.TopLevel.GetFunction(0);
    for I := 0 to Func.LocalTypeCount - 1 do
      case Func.GetLocalType(I) of
        sltInteger: Result := Result + 'I';
        sltFloat: Result := Result + 'F';
        sltBoolean: Result := Result + 'B';
        sltString: Result := Result + 'S';
        sltReference: Result := Result + 'R';
      end;
  finally
    Module.Free;
  end;
end;

function TTestCompiler.InitializerTypes(const APrelude, AInitializer: string;
  const AStrictTypes: Boolean): string;
begin
  Result := RecordedLocalTypes('const f = (a, b, o) => { ' + APrelude +
    ' const r = ' + AInitializer + '; return r; };', AStrictTypes);
end;

procedure TTestCompiler.ExpectInitializerType(const APrelude, AInitializer,
  AExpected: string);
begin
  Expect<string>(InitializerTypes(APrelude, AInitializer, False)).ToBe(AExpected);
  Expect<string>(InitializerTypes(APrelude, AInitializer, True)).ToBe(AExpected);
end;

function TTestCompiler.CallResultIsTyped(const ADeclaration: string;
  const AStrictTypes: Boolean; const ANonStrictMode: Boolean;
  const ACaller: string): Boolean;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileSource('const main = () => { ' + ADeclaration + ' ' +
    ACaller + ' return g; };', AStrictTypes, False, False, False, False,
    False, False, ANonStrictMode, False, True, True, True);
  try
    // g adds two results of f. Nothing else in the source adds.
    Result := (CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT) = 1) and
      (CountOpRecursive(Module.TopLevel, OP_ADD) = 0);
    if not Result then
      Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestBinaryOperatorResultTypes;
const
  Operators: array[0..4] of string = ('-', '*', '/', '%', '**');
var
  Op: string;
begin
  for Op in Operators do
  begin
    // One proven Number operand makes the result a Number: ToNumeric runs on
    // both operands and a Number with a BigInt throws (ES2026 §13.15.3).
    ExpectInitializerType('', 'a ' + Op + ' 2', 'F');
    ExpectInitializerType('', '2 ' + Op + ' a', 'F');
    ExpectInitializerType('', '"7" ' + Op + ' 2', 'F');
    ExpectInitializerType('', '({ valueOf: () => 5 }) ' + Op + ' 2', 'F');
    ExpectInitializerType('const big = 1n;', 'big ' + Op + ' 2', 'F');
    ExpectInitializerType('', '(a ' + Op + ' 2) ' + Op + ' b', 'F');
    // No operand is a proven Number: the operands may be BigInts, and then
    // so is the result.
    ExpectInitializerType('', 'a ' + Op + ' b', '');
    ExpectInitializerType('const big = 1n;', 'big ' + Op + ' big', '');
    ExpectInitializerType('', '"7" ' + Op + ' "2"', '');
  end;

  // + adds only when both operands are Numbers, and concatenates when either
  // is a String; with an unknown operand it can do either or add BigInts.
  ExpectInitializerType('', '1 + 2', 'F');
  ExpectInitializerType('', 'a + 1', '');
  ExpectInitializerType('', '1 + a', '');
  ExpectInitializerType('', 'a + b', '');
  ExpectInitializerType('', '(a * 2) + b', '');
  ExpectInitializerType('const big = 1n;', 'big + 1', '');
  ExpectInitializerType('', 'a + "s"', 'S');
  ExpectInitializerType('', '"s" + a', 'S');
  ExpectInitializerType('', '"4" + 1', 'S');
  ExpectInitializerType('', '1 + "4"', 'S');
  ExpectInitializerType('', '`t${a}` + 2', 'S');
  ExpectInitializerType('const big = 1n;', 'big + "s"', 'S');
  ExpectInitializerType('', '(a * 2) + (b * 3)', 'F');

  // Comparisons are Booleans.
  ExpectInitializerType('', 'a < b', 'B');
  ExpectInitializerType('', 'a === b', 'B');
end;

procedure TTestCompiler.TestBitwiseOperatorResultTypes;
const
  Operators: array[0..4] of string = ('&', '|', '^', '<<', '>>');
var
  Op: string;
begin
  for Op in Operators do
  begin
    // The result is an int32 unless both operands are BigInts, in which
    // case it is a BigInt (ES2026 §6.1.6.1.9 to §6.1.6.1.11); a Number with
    // a BigInt throws.
    ExpectInitializerType('', 'a ' + Op + ' 0', 'I');
    ExpectInitializerType('', '0 ' + Op + ' a', 'I');
    ExpectInitializerType('', '(a * 1) ' + Op + ' b', 'I');
    ExpectInitializerType('const big = 1n;', 'big ' + Op + ' 1', 'I');
    ExpectInitializerType('', 'a ' + Op + ' b', '');
    ExpectInitializerType('const big = 1n;', 'big ' + Op + ' big', '');
  end;

  // >>> is a uint32 for every pair of operands: two BigInts throw.
  ExpectInitializerType('', 'a >>> b', 'I');
  ExpectInitializerType('', 'a >>> 0', 'I');
  ExpectInitializerType('const big = 1n;', 'big >>> 0n', 'I');
end;

procedure TTestCompiler.TestUnaryOperatorResultTypes;
var
  Module: TGocciaBytecodeModule;
begin
  // Unary + is ToNumber, which throws for a BigInt.
  ExpectInitializerType('', '+a', 'F');
  ExpectInitializerType('const big = 1n;', '+big', 'F');
  ExpectInitializerType('', '+({ valueOf: () => "4" })', 'F');
  // Unary - and ~ keep a Number a Number and turn a BigInt into a BigInt.
  ExpectInitializerType('', '-(a * 1)', 'F');
  ExpectInitializerType('', '-(-(a * 1))', 'F');
  ExpectInitializerType('', '-(a | 0)', 'F');
  ExpectInitializerType('', '-a', '');
  ExpectInitializerType('const big = 1n;', '-big', '');
  ExpectInitializerType('', '~(a * 1)', 'I');
  ExpectInitializerType('', '~a', '');
  ExpectInitializerType('const big = 1n;', '~big', '');
  // A typed instruction takes those results as operands.
  Module := CompileSource(
    'const f = (a, b) => { return (+a) + (+b); };',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(1);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD)).ToBe(0);
  finally
    Module.Free;
  end;
  // ! is a Boolean, typeof a String, and void and delete stay untyped.
  ExpectInitializerType('', '!a', 'B');
  ExpectInitializerType('', '!0', 'B');
  ExpectInitializerType('', 'typeof a', 'S');
  ExpectInitializerType('', 'void a', '');
  ExpectInitializerType('', 'delete o.x', '');
end;

procedure TTestCompiler.TestIntegerTypeComesOnlyFromInt32Results;
const
  Operators: array[0..5] of string = ('+', '-', '*', '/', '%', '**');
var
  Op, Source: string;
  I: Integer;
begin
  // Arithmetic is not closed over the integers: 5 / 2 has a fraction, 5 % 0
  // is NaN, 2 ** -1 has a fraction, and a sum, a difference or a product can
  // pass 2^1024 and become Infinity. Every arithmetic result is a Number.
  for Op in Operators do
    ExpectInitializerType('const x = a | 0; const y = b | 0;',
      'x ' + Op + ' y', 'IIF');
  ExpectInitializerType('', '(2147483647 | 0) + (2147483647 | 0)', 'F');
  ExpectInitializerType('', '(-2147483648 | 0) - (2147483647 | 0)', 'F');
  ExpectInitializerType('', '(a >>> 0) + (b >>> 0)', 'F');

  // A conditional of integers is an integer, and of an integer and a Number
  // a Number.
  ExpectInitializerType('', 'a ? (a | 0) : (b | 0)', 'I');
  ExpectInitializerType('', 'a ? (a | 0) : 1', 'F');
  ExpectInitializerType('', 'a ? 1 : 2', 'F');
  ExpectInitializerType('', 'a ? 1 : "s"', '');

  // A chain of 1,100 constants that each double the one before reaches
  // Infinity at the 1,025th, and Infinity - Infinity is NaN. Only the first
  // one, a bitwise result, is an integer.
  Source := 'const f = (n) => { const v0 = n | 0;';
  for I := 1 to 1099 do
    Source := Source + ' const v' + IntToStr(I) + ' = v' + IntToStr(I - 1) +
      ' + v' + IntToStr(I - 1) + ';';
  Source := Source + ' const w = v1099 - v1099; return w; };';
  Expect<string>(RecordedLocalTypes(Source)).ToBe('I' + StringOfChar('F', 1100));
  Expect<string>(RecordedLocalTypes(Source, True)).ToBe(
    'I' + StringOfChar('F', 1100));
end;

procedure TTestCompiler.TestMathCallsAreNotTrusted;
begin
  // Math is a writable global binding, so no call on it has a known type.
  ExpectInitializerType('', 'Math.round(a)', '');
  ExpectInitializerType('', 'Math.min(a, b) + 1', '');
  ExpectInitializerType('', 'Math.round(a) + 0.5', '');
  ExpectInitializerType('', 'globalThis.Math.round(2) + 0.5', '');
  ExpectInitializerType('', 'Math.round(a) + Math.round(b)', '');
  // A member of Math is a read, not a call: 3 * Math.PI is a Number only
  // because * with a Number operand is.
  ExpectInitializerType('', '3 * Math.PI', 'F');
  // A shadowing binding or a parameter named Math changes nothing.
  ExpectInitializerType('const Math = { round: () => "r" };',
    'Math.round(2) + 0.5', 'R');
  Expect<string>(RecordedLocalTypes(
    'const f = (Math) => { const r = Math.max(1, 2) + 0.5; return r; };'
    )).ToBe('');
end;

procedure TTestCompiler.TestConstTypeComesFromTheNumberProof;
var
  Module: TGocciaBytecodeModule;
begin
  // The scan proves a const a Number where the expression rules cannot: the
  // old value of a Number binding, and an assignment to it.
  ExpectInitializerType('let n = 1;', 'n++', 'FF');
  ExpectInitializerType('let n = 1;', '(n -= 2)', 'FF');
  ExpectInitializerType('let n = 1;', '(n = 5)', 'FF');
  // Logical operators yield one of their operands: a Number only when both
  // are. Only the proof knows, and it runs for a body that has only consts.
  ExpectInitializerType('const k = 1;', 'k && 2', 'FF');
  ExpectInitializerType('const k = 1;', 'k || 2', 'FF');
  ExpectInitializerType('const k = 1;', 'k ?? 2', 'FF');
  ExpectInitializerType('const k = 1;', 'k && a', 'F');
  ExpectInitializerType('const k = 1;', 'a && k', 'F');
  // A chain of consts whose initializers each have a Number operand.
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { const x = a - 1; const y = x * 3; return y; };'
    )).ToBe('FF');
  // The binding the initializer reads is not a Number, so neither is it.
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { let n = 1; n = "s"; const r = n++; return r; };'
    )).ToBe('');
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { let n = 1; n = "s"; const r = n++; return r; };',
    True)).ToBe('F');
  // Other initializers, and a destructuring pattern, give no type.
  ExpectInitializerType('', 'a', '');
  ExpectInitializerType('', 'o.x', '');
  ExpectInitializerType('const { y } = o;', 'a', '');
  ExpectInitializerType('const [z] = o;', 'a', '');
  ExpectInitializerType('const { y } = o; const { w } = y;', 'a - 1', 'F');

  // An initializer the expression rules type is not checked either.
  Module := CompileSource(
    'const f = (a) => { const x = a - 1; return x; };',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_CHECK_TYPE)).ToBe(0);
  finally
    Module.Free;
  end;
  // The initializer is covered by the proof, so strict types do not check it.
  Module := CompileSource(
    'const f = (a) => { let n = 1; const r = n++; return r; };',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_CHECK_TYPE)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestLetKeepsItsTypeOnlyWhenEveryValueIsANumber;

  function Additions(const ASource: string; const AStrictTypes: Boolean;
    out AFloatAdditions: Integer): Integer;
  var
    Module: TGocciaBytecodeModule;
  begin
    Module := CompileSource(ASource, AStrictTypes, False, False, False, False,
      False);
    try
      AFloatAdditions := CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT);
      Result := CountOpRecursive(Module.TopLevel, OP_ADD);
    finally
      Module.Free;
    end;
  end;

var
  Module: TGocciaBytecodeModule;
  Floats, Generics: Integer;
  Strict: Boolean;
begin
  for Strict in [False, True] do
  begin
    // Every assignment gives a Number: the let is a Number.
    Expect<string>(RecordedLocalTypes(
      'const f = (a) => { let x = a * 2; x = x - 1; return x; };', Strict
      )).ToBe('F');
    // One assignment of another type, directly or from a closure (the R is
    // the closure's own binding), and it is not. Under strict types it is not enforced either: the initializer is
    // not a literal form, so x stays free to hold the string.
    Expect<string>(RecordedLocalTypes(
      'const f = (a) => { let x = a * 2; x = "t"; return x; };', Strict
      )).ToBe('');
    Expect<string>(RecordedLocalTypes(
      'const f = (a) => { let x = a * 2; const set = () => { x = "z"; };' +
      ' return x; };', Strict)).ToBe('R');
    Expect<string>(RecordedLocalTypes(
      'const f = (a) => { let x = a; return x; };', Strict)).ToBe('');
    Expect<string>(RecordedLocalTypes(
      'const f = (a) => { let x = a * 2; if (a) { x = a; } return x; };',
      Strict)).ToBe('');

    // A closure reads a captured let that holds only Numbers typed, and one
    // that a closure assigns untyped.
    Generics := Additions(
      'const f = (k) => { let n = k * 2; n = n - 1; return () => n + n; };',
      Strict, Floats);
    Expect<Integer>(Floats).ToBe(1);
    Expect<Integer>(Generics).ToBe(0);
    Generics := Additions(
      'const f = (k) => { let n = k * 2; const set = () => { n = "z"; };' +
      ' return () => n + n; };', Strict, Floats);
    Expect<Integer>(Floats).ToBe(0);
    Expect<Integer>(Generics).ToBe(1);
    Generics := Additions(
      'const f = (k) => { let n = k * 2; const bump = () => { n = n + 1; };' +
      ' return () => n + n; };', Strict, Floats);
    Expect<Integer>(Floats).ToBe(0);
  end;

  // The proof says Number, not integer: an integer initializer that is
  // assigned a quotient later gives a Number, and so does one never assigned.
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { let x = a | 0; x = x / 2; return x; };')).ToBe('F');
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { let x = a | 0; x = x / 2; return x; };', True)).ToBe('F');
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { let x = a >>> 0; return x; };')).ToBe('F');
  // A const keeps the exact type of its initializer.
  Expect<string>(RecordedLocalTypes(
    'const f = (a) => { const x = a | 0; return x; };')).ToBe('I');

  // A literal initializer is the one profile difference. By default the let
  // is a Number only if every assignment is; with strict types it is a
  // Number whatever is assigned, and the assignment is checked.
  Expect<string>(RecordedLocalTypes(
    'const f = () => { let x = 1; x = "s"; return x; };')).ToBe('');
  Expect<string>(RecordedLocalTypes(
    'const f = () => { let x = 1; x = "s"; return x; };', True)).ToBe('F');
  Expect<string>(RecordedLocalTypes(
    'const f = () => { let x = 1; x = x + 1; return x; };')).ToBe('F');
  Module := CompileSource('const f = () => { let x = 1; x = "s"; return x; };',
    True, False, False, False, False, False);
  try
    Expect<Boolean>(CountOpRecursive(Module.TopLevel, OP_CHECK_TYPE) > 0).ToBe(True);
  finally
    Module.Free;
  end;
  Module := CompileSource(
    'const f = (a) => { let x = a * 2; x = x - 1; return x; };',
    True, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_CHECK_TYPE)).ToBe(0);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestCapturedConstKeepsItsType;
var
  Module: TGocciaBytecodeModule;
begin
  // A closure reads a captured const typed.
  Module := CompileSource(
    'const f = (k) => { const c = k * 3; return () => c + c; };',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(1);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD)).ToBe(0);
  finally
    Module.Free;
  end;

  // A closure compiled before the const is initialized cannot rely on it:
  // the call throws a ReferenceError.
  Module := CompileSource(
    'const f = (k) => { const g = () => c + c; const c = k * 3; return g; };',
    False, False, False, False, False, False);
  try
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD_FLOAT)).ToBe(0);
    Expect<Integer>(CountOpRecursive(Module.TopLevel, OP_ADD)).ToBe(1);
  finally
    Module.Free;
  end;
end;

procedure TTestCompiler.TestConstFunctionReturnTypeTypesCalls;
var
  Strict: Boolean;
begin
  for Strict in [False, True] do
  begin
    // Every return is a Number.
    Expect<Boolean>(CallResultIsTyped('const f = (x) => x * 2;', Strict)).ToBe(True);
    Expect<Boolean>(CallResultIsTyped(
      'const f = (x) => { if (x) { return x * 2; } return 0; };', Strict
      )).ToBe(True);
    Expect<Boolean>(CallResultIsTyped(
      'const f = (x) => { if (x) { return 1; } else { return 2; } };', Strict
      )).ToBe(True);
    Expect<Boolean>(CallResultIsTyped(
      'const f = (x) => { for (const i of x) { return i * 2; } return 0; };',
      Strict)).ToBe(True);
    Expect<Boolean>(CallResultIsTyped(
      'const f = function (x) { return x * 2; };', Strict)).ToBe(True);
    // A function that returns a typed call, and a call from a closure.
    Expect<Boolean>(CallResultIsTyped(
      'const h = (x) => x * 2; const f = (x) => h(x) - 1;', Strict)).ToBe(True);
    Expect<Boolean>(CallResultIsTyped('const f = (x) => x * 2;', Strict, False,
      'const g = () => { const k = () => { const r = f(); return r + r; };' +
      ' return k; };')).ToBe(True);
    // A recursive call is untyped, but multiplying it by a Number is not.
    Expect<Boolean>(CallResultIsTyped(
      'const f = (x) => x <= 1 ? 1 : f(x - 1) * 2;', Strict)).ToBe(True);
    // The call is a Number whatever the arguments are.
    Expect<Boolean>(CallResultIsTyped('const f = (x) => x * 2;', Strict, False,
      'const g = () => { const r = f("3"); return r + r; };')).ToBe(True);
  end;
end;

procedure TTestCompiler.TestCallResultStaysUntypedWhenReturnTypeIsNotProven;

  procedure ExpectUntyped(const ADeclaration: string;
    const ANonStrictMode: Boolean = False;
    const ACaller: string =
      'const g = () => { const r = f(); return r + r; };');
  var
    Strict: Boolean;
  begin
    for Strict in [False, True] do
      Expect<Boolean>(CallResultIsTyped(ADeclaration, Strict, ANonStrictMode,
        ACaller)).ToBe(False);
  end;

begin
  // Two return types, an unknown one, and a return without a value.
  ExpectUntyped('const f = (x) => x ? 1 : "s";');
  ExpectUntyped('const f = (x) => { if (x) { return 1; } return "s"; };');
  ExpectUntyped('const f = (x) => { return x; };');
  ExpectUntyped('const f = (x) => { if (x) { return 1; } return; };');
  ExpectUntyped('const f = (x) => { return; };');
  ExpectUntyped('const f = (x) => { throw x; };');
  ExpectUntyped('const f = (x) => 5n;');
  ExpectUntyped('const f = (x) => ({});');
  ExpectUntyped('const f = (x) => { try { return 1; } finally { return "s"; } };');
  // A Number and a BigInt: the sum of the two results must stay generic.
  ExpectUntyped('const f = (x) => x ? 1 : 5n;');
  // A function that calls itself is untyped inside its own body.
  ExpectUntyped('const f = (x) => x <= 1 ? 1 : x * f(x - 1);');
  // An async arrow returns a promise, and so does an async function. A
  // generator function returns a generator.
  ExpectUntyped('const f = async () => 1;');
  ExpectUntyped('const f = async function () { return 1; };');
  ExpectUntyped('const f = function* () { return 1; };');
  // A let binding can be reassigned, and a parameter or an inner const of
  // the same name is another function.
  ExpectUntyped('let f = () => 1;');
  ExpectUntyped('let f = () => 1; f = () => "s";');
  ExpectUntyped('const f = () => 1;', False,
    'const g = (f) => { const r = f(); return r + r; };');
  ExpectUntyped('const f = () => 1;', False,
    'const g = () => { const f = () => "s"; const r = f(); return r + r; };');
  // An optional call is not a plain call, and neither is a call of a member.
  ExpectUntyped('const f = () => 1;', False,
    'const g = () => { const r = f?.(); return r + r; };');
  ExpectUntyped('const f = () => 1; const o = { f };', False,
    'const g = () => { const r = o.f(); return r + r; };');
  // A with object can supply the callee, and a sloppy direct eval can
  // declare a var of the same name.
  ExpectUntyped('const f = () => 1;', True,
    'const g = (o) => { with (o) { const r = f(); return r + r; } };');
  ExpectUntyped('const f = () => 1;', True,
    'const g = () => { eval("var f = () => 1"); const r = f(); return r + r; };');
end;

procedure TTestCompiler.TestFallThroughMakesTheReturnTypeUntyped;

  procedure ExpectUntypedReturn(const ADeclaration: string);
  var
    Strict: Boolean;
  begin
    for Strict in [False, True] do
      Expect<Boolean>(CallResultIsTyped(ADeclaration, Strict)).ToBe(False);
  end;

begin
  // A function that can reach the end of its body returns undefined there.
  ExpectUntypedReturn('const f = (x) => { if (x) { return 1; } };');
  ExpectUntypedReturn(
    'const f = (x) => { if (x) { return 1; } else { if (x === 2) { return 2; } } };');
  ExpectUntypedReturn(
    'const f = (x) => { switch (x) { case 1: return 1; case 2: break; } };');
  ExpectUntypedReturn(
    'const f = (x) => { try { if (x) { throw x; } return 1; } catch (e) { } };');
  ExpectUntypedReturn(
    'const f = (x) => { for (const i of x) { return i * 2; } };');
  ExpectUntypedReturn('const f = (x) => { x * 2; };');
  // A try statement is never known to end abruptly, so one that returns in
  // every path still counts as one that can fall through.
  ExpectUntypedReturn(
    'const f = (x) => { try { return x * 1; } finally { x = 0; } };');
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

procedure TTestCompiler.TestConstantOperandComparisonsUseImmediateJumps;
const
  Cases: array[0..9] of string = (
    'const f = (a) => { if (a < 50) { return "y"; } return "n"; }; f(1);',
    'const f = (a) => { if (a > 500) { return "y"; } return "n"; }; f(1);',
    'const f = (a) => { if (a <= -3) { return "y"; } return "n"; }; f(1);',
    'const f = (a) => { if (a >= 0) { return "y"; } return "n"; }; f(1);',
    'const f = (a) => { if (5 < a) { return "y"; } return "n"; }; f(1);',
    'const f = (a) => a === 1 ? "y" : "n"; f(1);',
    'const f = (a) => { if (a !== 32767) { return "y"; } return "n"; }; f(1);',
    'const K = 4; const f = (a) => { if (a > K) { return "y"; } return "n"; }; f(1);',
    'const f = (a, b) => { if (a === 1 || b === 2) { return "y"; } return "n"; }; f(1, 2);',
    'const f = (a, b) => { if (a < 3 && b >= 2) { return "y"; } return "n"; }; f(1, 2);');
  Expected: array[0..9] of TGocciaOpCode = (
    OP_JUMP_IF_NOT_LT_IMM, OP_JUMP_IF_NOT_GT_IMM, OP_JUMP_IF_NOT_LTE_IMM,
    OP_JUMP_IF_NOT_GTE_IMM, OP_JUMP_IF_NOT_GT_IMM,
    OP_JUMP_IF_NOT_STRICT_EQ_IMM, OP_JUMP_IF_STRICT_EQ_IMM,
    OP_JUMP_IF_NOT_GT_IMM, OP_JUMP_IF_STRICT_EQ_IMM, OP_JUMP_IF_NOT_LT_IMM);
var
  Module: TGocciaBytecodeModule;
  Func: TGocciaFunctionTemplate;
  I: Integer;
begin
  for I := Low(Cases) to High(Cases) do
  begin
    Module := CompileSource(Cases[I]);
    try
      Func := FindFunctionWithOp(Module.TopLevel, Expected[I]);
      Expect<Boolean>(Assigned(Func)).ToBe(True);
      if Assigned(Func) then
      begin
        // The comparison is one instruction: no Boolean is tested and no
        // instruction loads the constant.
        Expect<Integer>(CountOp(Func, OP_JUMP_IF_FALSE) +
          CountOp(Func, OP_JUMP_IF_TRUE)).ToBe(0);
        Expect<Integer>(CountOp(Func, OP_LOAD_INT)).ToBe(0);
      end;
    finally
      Module.Free;
    end;
  end;
end;

procedure TTestCompiler.TestConstantOperandComparisonsKeepGenericForms;
const
  // A constant outside Int16, a fraction, a relational test jumping on
  // true inside `||`, and a comparison whose value is kept.
  Cases: array[0..3] of string = (
    'const f = (a) => { if (a < 40000) { return 1; } return 0; }; f(1);',
    'const f = (a) => { if (a < 0.5) { return 1; } return 0; }; f(1);',
    'const f = (a, b) => { if (a < 3 || b) { return 1; } return 0; }; f(1, 2);',
    'const f = (a) => { const c = a < 3; return c; }; f(1);');
var
  Module: TGocciaBytecodeModule;
  I: Integer;
begin
  for I := Low(Cases) to High(Cases) do
  begin
    Module := CompileSource(Cases[I]);
    try
      Expect<Integer>(CountOpRecursive(Module.TopLevel,
        OP_JUMP_IF_NOT_LT_IMM)).ToBe(0);
    finally
      Module.Free;
    end;
  end;
end;

procedure TTestCompiler.TestHugeConstantComparisonCompilesWithInvalidOpUnmasked;
const
  // A literal and a const binding past the Int64 range: an embedder can call
  // the compiler outside the engine's masked floating-point scope, where
  // truncating such a value raises EInvalidOp.
  Cases: array[0..1] of string = (
    'const f = (a) => { if (a < 1e30) { return 1; } return 0; }; f(1);',
    'const K = -1e300; const f = (a) => { if (a < K) { return 1; } return 0; }; f(1);');
var
  Module: TGocciaBytecodeModule;
  SavedMask: TFPUExceptionMask;
  I: Integer;
begin
  SavedMask := GetExceptionMask;
  SetExceptionMask(SavedMask - [exInvalidOp]);
  try
    for I := Low(Cases) to High(Cases) do
    begin
      Module := CompileSource(Cases[I]);
      try
        Expect<Integer>(CountOpRecursive(Module.TopLevel,
          OP_JUMP_IF_NOT_LT_IMM)).ToBe(0);
      finally
        Module.Free;
      end;
    end;
  finally
    SetExceptionMask(SavedMask);
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
    Expect<Boolean>(
      CountOpRecursive(Module.TopLevel, OP_GET_GLOBAL_DECLARED) >= 2)
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
  // enforced Number type stays on the var: the assignment after the try
  // statement is checked, the catch parameter's is not. (Reads of a var are
  // never typed by its enforced type: it holds undefined until its
  // declaration runs.)
  Module := CompileSource(
    'var x = 1; try { throw "s"; } catch (x) { var x; x = "a"; } x = "b";',
    True, False, False, False, False, False, False, False, True);
  try
    Expect<Integer>(CountOp(Module.TopLevel, OP_CHECK_TYPE)).ToBe(1);
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
