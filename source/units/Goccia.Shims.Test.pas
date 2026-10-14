{ Pins the per-thread shim program cache in Goccia.Shims.

  Every engine used to lex and parse each shim it loaded. A parsed AST is never
  released — the functions a shim defines point into it, and
  TGocciaProgram.Free frees only the program node — so each engine leaked a
  full shim AST: about 1.1 MB per engine that uses Date on 64-bit, 0.93 MB of
  it the Date shim and 0.18 MB the seven Object.prototype shims that then ran
  at boot. The test runner builds one engine per file and the testing library
  loads the Date shim in every one, which helped run a 32-bit process out of
  address space.

  Also pins how shims load (#873, #911): no shim loads at boot, and with a
  bytecode executor a shim is compiled, so its functions are VM closures and
  the engine builds no interpreter. }

program Goccia.Shims.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Engine,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.Shims,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TTestShims = class(TTestSuite)
  private
    function RunNumber(const ASource: string): Double;
    function RunString(const ASource: string): string;
    function LiveHeapBytes: Int64;
    function TemplateObjectsInAFreshRealm(
      const AProgram: TGocciaProgram): Integer;
  public
    procedure SetupTests; override;

    procedure TestEnginesDoNotGrowTheHeapByAShimProgramEach;
    procedure TestEachEngineEvaluatesTheSharedProgramIntoItsOwnRealm;
    procedure TestEnginesStillLoadShimsAfterTheCacheIsReleased;
    procedure TestCacheReleaseIsRegistered;
    procedure TestASharedProgramKeepsItsTemplateObjectsInEachRealm;
    procedure TestABytecodeEngineBuildsNoInterpreter;
    procedure TestBootLoadsNoShim;
    procedure TestBytecodeShimFunctionsAreCompiled;
    procedure TestInterpretedShimFunctionsTreeWalk;
    procedure TestToStringOfANamelessFunctionLeavesNothingBehind;
  end;

procedure TTestShims.SetupTests;
begin
  Test('engines built one after another do not grow the heap by a shim program each',
    TestEnginesDoNotGrowTheHeapByAShimProgramEach);
  Test('each engine evaluates the shared Date shim program into its own realm',
    TestEachEngineEvaluatesTheSharedProgramIntoItsOwnRealm);
  Test('engines still load shims after the thread releases its cached programs',
    TestEnginesStillLoadShimsAfterTheCacheIsReleased);
  Test('the cached shim programs are released by the thread cleanup registry',
    TestCacheReleaseIsRegistered);
  Test('a program evaluated in one engine after another keeps its template objects in each realm',
    TestASharedProgramKeepsItsTemplateObjectsInEachRealm);
  Test('an engine with a bytecode executor builds no interpreter',
    TestABytecodeEngineBuildsNoInterpreter);
  Test('booting an engine loads no shim',
    TestBootLoadsNoShim);
  Test('with a bytecode executor the shims define VM closures',
    TestBytecodeShimFunctionsAreCompiled);
  Test('with an interpreter executor the shims define tree-walk functions',
    TestInterpretedShimFunctionsTreeWalk);
  Test('Function.prototype.toString of a nameless built-in leaves nothing behind',
    TestToStringOfANamelessFunctionLeavesNothingBehind);
end;

function TTestShims.RunNumber(const ASource: string): Double;
begin
  Result := (TGocciaEngine.RunScript(ASource, '<shims-test>').Result as
    TGocciaNumberLiteralValue).Value;
end;

function TTestShims.RunString(const ASource: string): string;
begin
  Result := (TGocciaEngine.RunScript(ASource, '<shims-test>').Result as
    TGocciaStringLiteralValue).Value;
end;

function TTestShims.LiveHeapBytes: Int64;
begin
  TGarbageCollector.Instance.Collect;
  Result := Int64(GetHeapStatus.TotalAllocated);
end;

procedure TTestShims.TestEnginesDoNotGrowTheHeapByAShimProgramEach;
const
  WARM_UP_ENGINES = 3;
  MEASURED_ENGINES = 10;
  { Without the cache each of these engines leaked about 1.1 MB on 64-bit:
    0.93 MB of Date shim AST and 0.18 MB of eager Object.prototype shim ASTs.
    What an engine still leaves behind is a few KB of allocator and cache
    bookkeeping. }
  MAX_GROWTH_PER_ENGINE_BYTES = 64 * 1024;
  SOURCE = 'new Date(Date.UTC(2020, 0, 2)).getUTCDate();';
var
  I: Integer;
  Before, After: Int64;
begin
  for I := 1 to WARM_UP_ENGINES do
    Expect<Double>(RunNumber(SOURCE)).ToBe(2);
  Before := LiveHeapBytes;
  for I := 1 to MEASURED_ENGINES do
    Expect<Double>(RunNumber(SOURCE)).ToBe(2);
  After := LiveHeapBytes;
  Expect<Boolean>((After - Before) div MEASURED_ENGINES <
    MAX_GROWTH_PER_ENGINE_BYTES).ToBe(True);
end;

procedure TTestShims.TestEachEngineEvaluatesTheSharedProgramIntoItsOwnRealm;
begin
  Expect<Double>(RunNumber(
    'Date.prototype.poisoned = 1;' +
    'Date.poisoned = 2;' +
    'Date.prototype.poisoned + Date.poisoned;')).ToBe(3);
  { The second engine evaluates the program the first one parsed. Its Date,
    and Date.prototype, must be new objects in its own realm. }
  Expect<string>(RunString(
    'typeof Date.prototype.poisoned + "," + typeof Date.poisoned + "," +' +
    'String(new Date(0).getTime());')).ToBe('undefined,undefined,0');
  { The eager shims install methods on each realm's Object.prototype. }
  Expect<string>(RunString(
    'typeof Object.prototype.__defineGetter__;')).ToBe('function');
end;

procedure TTestShims.TestEnginesStillLoadShimsAfterTheCacheIsReleased;
var
  Cached: Integer;
begin
  { Start from an empty cache: earlier tests on this thread loaded shims. }
  ReleaseShimProgramCache;
  Expect<Double>(RunNumber('new Date(86400000).getUTCDate();')).ToBe(2);
  { Only Date: no other shim provides anything the script touched. }
  Cached := CachedShimProgramCount;
  Expect<Integer>(Cached).ToBe(1);
  ReleaseShimProgramCache;
  Expect<Integer>(CachedShimProgramCount).ToBe(0);
  Expect<Double>(RunNumber('new Date(86400000).getUTCDate();')).ToBe(2);
  Expect<Integer>(CachedShimProgramCount).ToBe(Cached);
  { Releasing an already-empty cache is a no-op. }
  ReleaseShimProgramCache;
  ReleaseShimProgramCache;
  Expect<Integer>(CachedShimProgramCount).ToBe(0);
  Expect<Double>(RunNumber('new Date(0).getUTCFullYear();')).ToBe(1970);
end;

procedure TTestShims.TestCacheReleaseIsRegistered;
begin
  Expect<Boolean>(IsThreadvarCleanupRegistered(@ReleaseShimProgramCache))
    .ToBe(True);
end;

function TTestShims.TemplateObjectsInAFreshRealm(
  const AProgram: TGocciaProgram): Integer;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<shims-test>', Source, Executor);
    Executor.EvaluateShim(AProgram,
      CreateShimScope(Engine.GlobalScope, 'test'), '<shims-test-template>');
    Result := Engine.Realm.TemplateMapCount;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TTestShims.TestASharedProgramKeepsItsTemplateObjectsInEachRealm;
const
  SOURCE =
    'const tag = (strings) => strings;' + LineEnding +
    'tag`a${1}b`;' + LineEnding +
    '(() => tag`c`)();' + LineEnding;
var
  Options: TGocciaSourcePipelineOptions;
  ParseResult: TGocciaSourcePipelineModuleResult;
  ProgramNode: TGocciaProgram;
  Site: TGocciaTaggedTemplateExpression;
  I: Integer;
begin
  { Sharing a shim program across engines relies on evaluation writing
    nothing into the AST. A tagged template's template object is the one
    value the evaluator could cache on a node, and it must go into the
    realm's template map instead (ES2026 §13.2.8.3 GetTemplateObject), both
    at the top level and inside a function the program defines. }
  Options := TGocciaSourcePipeline.DefaultOptions;
  Options.Preprocessors := [];
  Options.Compatibility := [cfFunction];
  Options.SourceType := stModule;
  ParseResult := TGocciaSourcePipeline.ParseModuleSource(SOURCE,
    '<shims-test-template>', Options);
  try
    ProgramNode := ParseResult.TakeProgramNode;
  finally
    ParseResult.Free;
  end;
  try
    Site := (ProgramNode.Body[1] as TGocciaExpressionStatement).Expression as
      TGocciaTaggedTemplateExpression;
    for I := 1 to 2 do
    begin
      Expect<Integer>(TemplateObjectsInAFreshRealm(ProgramNode)).ToBe(2);
      Expect<Boolean>(Assigned(Site.TemplateObject)).ToBe(False);
    end;
  finally
    ProgramNode.Free;
  end;
end;

procedure TTestShims.TestABytecodeEngineBuildsNoInterpreter;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<shims-test>', Source, Executor);
    Expect<Boolean>(Assigned(Engine.Interpreter)).ToBe(False);
    Expect<Boolean>(Assigned(Engine.GlobalScope)).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TTestShims.TestBootLoadsNoShim;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  ReleaseShimProgramCache;
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<shims-test>', Source, Executor);
    Expect<Integer>(CachedShimProgramCount).ToBe(0);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TTestShims.TestBytecodeShimFunctionsAreCompiled;
var
  DateConstructor: TGocciaValue;
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<shims-test>', Source, Executor);
    DateConstructor := GetDateIntrinsic(Engine.Executor);
    { The VM's closure class is private to Goccia.VM. }
    Expect<string>(DateConstructor.ClassName)
      .ToBe('TGocciaBytecodeFunctionValue');
    Expect<string>(TGocciaObjectValue(DateConstructor)
      .GetProperty('parse').ClassName).ToBe('TGocciaBytecodeFunctionValue');
    Expect<string>(TGocciaObjectValue.SharedObjectPrototype
      .GetProperty('__lookupGetter__').ClassName)
      .ToBe('TGocciaBytecodeFunctionValue');
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TTestShims.TestInterpretedShimFunctionsTreeWalk;
var
  DateConstructor: TGocciaValue;
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<shims-test>', Source, Executor);
    Expect<Boolean>(Assigned(Engine.Interpreter)).ToBe(True);
    DateConstructor := GetDateIntrinsic(Engine.Executor);
    Expect<Boolean>(DateConstructor is TGocciaFunctionValue).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TTestShims.TestToStringOfANamelessFunctionLeavesNothingBehind;
const
  { 20,000 calls: a descriptor left behind per call would be about 1 MB. }
  MAX_GROWTH_BYTES = 256 * 1024;
  SOURCE =
    'const compare = new Intl.Collator().compare;' +
    'const toString = Function.prototype.toString;' +
    'for (const _ of Array.from({ length: 20000 })) toString.call(compare);' +
    'toString.call(compare);';
var
  Before, After: Int64;
begin
  { The native-code text of a function without a name of its own falls back
    to an own `name` data property (a shim's computed-key method). A
    synthesized name has no entry, and asking for its descriptor would
    allocate one per call that nothing frees. An Intl.Collator's bound
    compare function is such a nameless built-in. }
  Expect<string>(RunString(SOURCE)).ToBe('function () { [native code] }');
  Before := LiveHeapBytes;
  Expect<string>(RunString(SOURCE)).ToBe('function () { [native code] }');
  After := LiveHeapBytes;
  Expect<Boolean>(After - Before < MAX_GROWTH_BYTES).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TTestShims.Create('Shims'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
