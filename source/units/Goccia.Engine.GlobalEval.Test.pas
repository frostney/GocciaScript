program Goccia.Engine.GlobalEval.Test;

{$I Goccia.inc}

{ ADR 0134 Phase 2: eval code whose variable environment is the global one
  (indirect eval, a ShadowRealm child realm's eval and
  ShadowRealm.prototype.evaluate) runs through TGocciaEngine.EvaluateGlobalEval.
  The code is compiled and run on the VM, so a function or class it creates
  is a VM value. These tests call the entry directly and through the Test262
  host's eval; the ShadowRealm unit reaches the same entry.

  ADR 0134 Phase 3: a direct eval is compiled too, as a closure of the call
  site, so what it creates is a VM value as well, at a strict call site and
  at a sloppy one.

  ADR 0134 Phase 4: each direct eval site keeps the code it compiled, keyed on
  the source text, so evaluating the same source at the site again compiles
  nothing and the engine keeps no further module alive. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Math,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.Error,
  Goccia.SourcePipeline,
  Goccia.Test262.Host,
  Goccia.TestSetup,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Error,
  Goccia.Values.Primitives,
  Goccia.VM.DirectEvalCodeCache,
  Goccia.VM.Exception;

const
  // The VM's function and class values are private to Goccia.VM.
  BYTECODE_FUNCTION_CLASS = 'TGocciaBytecodeFunctionValue';
  VM_CLASS_CLASS = 'TGocciaVMClassValue';

type
  TGlobalEvalTests = class(TTestSuite)
  private
    function CreateEngine(const ASource: TStringList): TGocciaEngine;
    function EvalStage(const AEngine: TGocciaEngine;
      const ASource: string): TGocciaGlobalEvalStage;
    procedure TestBytecodeEvalCreatesVMFunctions;
    procedure TestBytecodeEvalCreatesVMClasses;
    procedure TestBytecodeTest262HostEvalCreatesVMFunctions;
    procedure TestBytecodeStrictDirectEvalCreatesVMValues;
    function DirectEvalRetainedModules(const ASource: string;
      const ACalls: Integer): Integer;
    procedure TestBytecodeDirectEvalReusesCodePerSite;
    procedure TestBytecodeEvalVarIsDeletableGlobal;
    procedure TestBytecodeEvalReturnsCompletionValue;
    procedure TestBytecodeEvalReportsCompileStage;
    procedure TestBytecodeEvalReportsRunStage;
    procedure AssertDirectiveFreeEvalIsNonStrict;
    procedure TestBytecodeEvalIsNonStrictInStrictEngine;
    procedure TestEvalEstablishesFloatingPointMode;
    procedure TestWaitForRuntimeIdleEstablishesFloatingPointMode;
  public
    procedure SetupTests; override;
  end;

procedure TGlobalEvalTests.SetupTests;
begin
  Test('Bytecode global eval creates VM functions',
    TestBytecodeEvalCreatesVMFunctions);
  Test('Bytecode global eval creates VM classes',
    TestBytecodeEvalCreatesVMClasses);
  Test('Bytecode Test262 host eval creates VM functions',
    TestBytecodeTest262HostEvalCreatesVMFunctions);
  Test('Bytecode direct eval creates VM values at strict and sloppy sites',
    TestBytecodeStrictDirectEvalCreatesVMValues);
  Test('Bytecode direct eval reuses the code a site compiled for a source',
    TestBytecodeDirectEvalReusesCodePerSite);
  Test('Bytecode global eval var is a deletable global property',
    TestBytecodeEvalVarIsDeletableGlobal);
  Test('Bytecode global eval returns the completion value',
    TestBytecodeEvalReturnsCompletionValue);
  Test('Bytecode global eval reports a compile-stage failure',
    TestBytecodeEvalReportsCompileStage);
  Test('Bytecode global eval reports a run-stage failure',
    TestBytecodeEvalReportsRunStage);
  Test('Bytecode directive-free global eval is non-strict in a strict engine',
    TestBytecodeEvalIsNonStrictInStrictEngine);
  Test('Global eval runs NaN arithmetic when called outside a run scope',
    TestEvalEstablishesFloatingPointMode);
  Test('WaitForRuntimeIdle runs NaN arithmetic when called outside a run scope',
    TestWaitForRuntimeIdleEstablishesFloatingPointMode);
end;

function TGlobalEvalTests.CreateEngine(const ASource: TStringList): TGocciaEngine;
begin
  Result := TGocciaEngine.Create('<global-eval-test>', ASource);
  Result.Compatibility := [cfASI, cfVar, cfFunction, cfNonStrictMode];
  Result.LabelStatementsEnabled := True;
end;

function TGlobalEvalTests.EvalStage(const AEngine: TGocciaEngine;
  const ASource: string): TGocciaGlobalEvalStage;
begin
  Result := gesCompile;
  try
    AEngine.EvaluateGlobalEval(ASource, '<global-eval-test>', Result);
    Fail('Expected the eval to throw');
  except
    on E: EGocciaBytecodeThrow do;
    on E: TGocciaThrowValue do;
    on E: TGocciaError do;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalCreatesVMFunctions;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Source);
    Value := Engine.EvaluateGlobalEval('function made() {} made',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(True);
    Value := Engine.EvaluateGlobalEval('(x) => x', '<global-eval-test>',
      Stage);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(True);
    Value := Engine.EvaluateGlobalEval('"use strict"; (function* g() {})',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalCreatesVMClasses;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Source);
    Value := Engine.EvaluateGlobalEval('class Made {} Made',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value.ClassName = VM_CLASS_CLASS).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeTest262HostEvalCreatesVMFunctions;
var
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  Host: TGocciaTest262Host;
  Source: TStringList;
  GlobalObject: TGocciaObjectValue;
begin
  Source := TStringList.Create;
  Source.Text :=
    'globalThis.madeFunction = (0, eval)("(function () {})");' + LineEnding +
    'globalThis.madeClass = (0, eval)("(class {})");' + LineEnding +
    'globalThis.childEval = (0, eval)("eval")("(function () {})");';
  EngineOptions := Default(TGocciaTest262EngineOptions);
  EngineOptions.Compatibility := [cfASI, cfVar, cfFunction, cfNonStrictMode];
  EngineOptions.SourceType := stScript;
  EngineOptions.Capabilities := TGocciaCapabilities.None;
  Engine := nil;
  Host := nil;
  try
    Engine := TGocciaEngine.Create('<global-eval-test>', Source,
      EngineOptions.Capabilities);
    ConfigureTest262Engine(Engine, EngineOptions);
    Host := TGocciaTest262Host.Create(EngineOptions);
    Host.ConfigureHostEnvironment(Engine.HostEnvironment);
    Host.Install(Engine);
    Engine.Execute;
    GlobalObject := TGocciaObjectValue(Engine.Realm.GlobalObject);
    Expect<Boolean>(GlobalObject.GetProperty('madeFunction').ClassName =
      BYTECODE_FUNCTION_CLASS).ToBe(True);
    Expect<Boolean>(GlobalObject.GetProperty('madeClass').ClassName =
      VM_CLASS_CLASS).ToBe(True);
    Expect<Boolean>(GlobalObject.GetProperty('childEval').ClassName =
      BYTECODE_FUNCTION_CLASS).ToBe(True);
  finally
    Host.Free;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeStrictDirectEvalCreatesVMValues;
var
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  Host: TGocciaTest262Host;
  Source: TStringList;
  GlobalObject: TGocciaObjectValue;
begin
  Source := TStringList.Create;
  Source.Text :=
    'function strict() { "use strict"; return eval("(function () {})"); }' +
      LineEnding +
    'globalThis.strictFunction = strict();' + LineEnding +
    'globalThis.strictClass = (function () { "use strict";' +
      ' return eval("(class {})"); })();' + LineEnding +
    'globalThis.classBodyFunction = class { static f = eval("() => 1"); }.f;' +
      LineEnding +
    'globalThis.sloppyFunction = eval("(function () {})");' + LineEnding +
    'globalThis.sloppyInFunction = (function () {' +
      ' return eval("var v = 1; (function () { return v; })"); })();';
  EngineOptions := Default(TGocciaTest262EngineOptions);
  EngineOptions.Compatibility := [cfASI, cfVar, cfFunction, cfNonStrictMode];
  EngineOptions.SourceType := stScript;
  EngineOptions.Capabilities := TGocciaCapabilities.None;
  Engine := nil;
  Host := nil;
  try
    Engine := TGocciaEngine.Create('<direct-eval-test>', Source,
      EngineOptions.Capabilities);
    ConfigureTest262Engine(Engine, EngineOptions);
    Host := TGocciaTest262Host.Create(EngineOptions);
    Host.ConfigureHostEnvironment(Engine.HostEnvironment);
    Host.Install(Engine);
    Engine.Execute;
    GlobalObject := TGocciaObjectValue(Engine.Realm.GlobalObject);
    Expect<Boolean>(GlobalObject.GetProperty('strictFunction').ClassName =
      BYTECODE_FUNCTION_CLASS).ToBe(True);
    Expect<Boolean>(GlobalObject.GetProperty('strictClass').ClassName =
      VM_CLASS_CLASS).ToBe(True);
    Expect<Boolean>(GlobalObject.GetProperty('classBodyFunction').ClassName =
      BYTECODE_FUNCTION_CLASS).ToBe(True);
    Expect<Boolean>(GlobalObject.GetProperty('sloppyFunction').ClassName =
      BYTECODE_FUNCTION_CLASS).ToBe(True);
    Expect<Boolean>(GlobalObject.GetProperty('sloppyInFunction').ClassName =
      BYTECODE_FUNCTION_CLASS).ToBe(True);
  finally
    Host.Free;
    Engine.Free;
    Source.Free;
  end;
end;

// The modules the engine keeps after a script defines its functions with
// ASource and calls `run(i)` ACalls times.
function TGlobalEvalTests.DirectEvalRetainedModules(const ASource: string;
  const ACalls: Integer): Integer;
var
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  Host: TGocciaTest262Host;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := ASource + LineEnding +
    'for (var i = 0; i < ' + IntToStr(ACalls) + '; i++) run(i);';
  EngineOptions := Default(TGocciaTest262EngineOptions);
  EngineOptions.Compatibility := [cfASI, cfVar, cfFunction, cfNonStrictMode,
    cfTraditionalFor];
  EngineOptions.SourceType := stScript;
  EngineOptions.Capabilities := TGocciaCapabilities.None;
  Engine := nil;
  Host := nil;
  try
    Engine := TGocciaEngine.Create('<direct-eval-cache-test>', Source,
      EngineOptions.Capabilities);
    ConfigureTest262Engine(Engine, EngineOptions);
    Host := TGocciaTest262Host.Create(EngineOptions);
    Host.ConfigureHostEnvironment(Engine.HostEnvironment);
    Host.Install(Engine);
    Engine.Execute;
    Result := Engine.RetainedModuleCount;
  finally
    Host.Free;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeDirectEvalReusesCodePerSite;
const
  CALLS = 40;
  SLOPPY_SITE = 'var x = 1; function run() { return eval("x + 1"); }';
  STRICT_SITE = '"use strict"; var x = 1;' +
    ' function run() { return eval("var y = x; y + 1"); }';
  SLOPPY_VAR_SITE = 'function run() { eval("var v = 1"); return v; }';
  CLOSURE_SITE = 'function run(i) {' +
    ' return eval("(function () { return i; })")(); }';
  GLOBAL_SITE = 'function run() {} for (var j = 0; j < 40; j++)' +
    ' eval("var g = j");';
  // Four sources at one site, and one source at two sites.
  DISTINCT_SITE = 'var srcs = ["1", "2", "3", "4"];' +
    ' function run(i) { return eval(srcs[i % 4]); }';
  TWO_SITES = 'function a() { return eval("1"); }' +
    ' function b() { return eval("1"); } function run() { a(); b(); }';
  // GetTemplateObject keys a template object on its Parse Node, and each
  // eval parses a new one: such code is compiled at every evaluation.
  TAGGED_TEMPLATE_SITE = 'function tag(t) { return t; }' +
    ' function run() { return eval("tag`x`"); }';
  // More sources at one site than its cache holds, each evaluated twice.
  CAPPED_SOURCES = 100;
  CAPPED_SITE = 'var srcs = []; for (var k = 0; k < 100; k++)' +
    ' srcs.push("x + " + k); var x = 1;' +
    ' function run(i) { return eval(srcs[i % 100]); }';
begin
  Expect<Integer>(DirectEvalRetainedModules(SLOPPY_SITE, CALLS)).ToBe(
    DirectEvalRetainedModules(SLOPPY_SITE, 1));
  Expect<Integer>(DirectEvalRetainedModules(STRICT_SITE, CALLS)).ToBe(
    DirectEvalRetainedModules(STRICT_SITE, 1));
  Expect<Integer>(DirectEvalRetainedModules(SLOPPY_VAR_SITE, CALLS)).ToBe(
    DirectEvalRetainedModules(SLOPPY_VAR_SITE, 1));
  Expect<Integer>(DirectEvalRetainedModules(CLOSURE_SITE, CALLS)).ToBe(
    DirectEvalRetainedModules(CLOSURE_SITE, 1));
  Expect<Integer>(DirectEvalRetainedModules(GLOBAL_SITE, 0)).ToBe(
    DirectEvalRetainedModules(SLOPPY_SITE, 0) + 1);
  Expect<Integer>(DirectEvalRetainedModules(DISTINCT_SITE, CALLS)).ToBe(
    DirectEvalRetainedModules(DISTINCT_SITE, 1) + 3);
  Expect<Integer>(DirectEvalRetainedModules(TWO_SITES, CALLS)).ToBe(
    DirectEvalRetainedModules(TWO_SITES, 0) + 2);
  Expect<Integer>(DirectEvalRetainedModules(TAGGED_TEMPLATE_SITE, CALLS)).ToBe(
    DirectEvalRetainedModules(TAGGED_TEMPLATE_SITE, 1) + CALLS - 1);
  // The site caches the first DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES sources:
  // the second round compiles only the others again.
  Expect<Integer>(DirectEvalRetainedModules(CAPPED_SITE,
    2 * CAPPED_SOURCES)).ToBe(DirectEvalRetainedModules(CAPPED_SITE, 0) +
    CAPPED_SOURCES + CAPPED_SOURCES - DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES);
end;

procedure TGlobalEvalTests.TestBytecodeEvalVarIsDeletableGlobal;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Descriptor: TGocciaPropertyDescriptor;
  GlobalObject: TGocciaObjectValue;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Source);
    Engine.EvaluateGlobalEval('var made = 1; function madeFn() {}',
      '<global-eval-test>', Stage);
    GlobalObject := TGocciaObjectValue(Engine.Realm.GlobalObject);
    Descriptor := GlobalObject.GetOwnPropertyDescriptor('made');
    Expect<Boolean>(Assigned(Descriptor)).ToBe(True);
    Expect<Boolean>(Descriptor.Configurable).ToBe(True);
    Descriptor := GlobalObject.GetOwnPropertyDescriptor('madeFn');
    Expect<Boolean>(Assigned(Descriptor)).ToBe(True);
    Expect<Boolean>(Descriptor.Configurable).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalReturnsCompletionValue;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Source);
    Value := Engine.EvaluateGlobalEval('1; if (true) { 2 } else { 3 }',
      '<global-eval-test>', Stage);
    Expect<Double>((Value as TGocciaNumberLiteralValue).Value).ToBe(2);
    Value := Engine.EvaluateGlobalEval('try { 4 } finally { 5 }',
      '<global-eval-test>', Stage);
    Expect<Double>((Value as TGocciaNumberLiteralValue).Value).ToBe(4);
    Value := Engine.EvaluateGlobalEval('6; if (false) 7;',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value is TGocciaUndefinedLiteralValue).ToBe(True);
    // ES2026 §14.15.3: an empty break out of a finally block makes the try
    // statement's value undefined.
    Value := Engine.EvaluateGlobalEval(
      '99; b: { -99; try { 39 } finally { break b; } }',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value is TGocciaUndefinedLiteralValue).ToBe(True);
    Value := Engine.EvaluateGlobalEval('l: try { 5 } finally { break l; }',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value is TGocciaUndefinedLiteralValue).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalReportsCompileStage;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Source);
    Expect<Boolean>(EvalStage(Engine, 'new.target') = gesCompile).ToBe(True);
    Expect<Boolean>(EvalStage(Engine, 'return 1') = gesCompile).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalReportsRunStage;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Source);
    Expect<Boolean>(EvalStage(Engine, 'throw 1') = gesRun).ToBe(True);
    TGocciaObjectValue(Engine.Realm.GlobalObject).PreventExtensions;
    // EvalDeclarationInstantiation's CanDeclareGlobalVar check runs after
    // compilation: a run-stage TypeError.
    Expect<Boolean>(EvalStage(Engine, 'var neverDeclared;') = gesRun).ToBe(
      True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

// ES2026 §19.2.1.1 step 13: eval code is strict only by its own directive,
// also in an engine without the non-strict compatibility profile.
procedure TGlobalEvalTests.AssertDirectiveFreeEvalIsNonStrict;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<global-eval-test>', Source);
    Engine.Compatibility := [cfASI, cfVar, cfFunction];
    Value := Engine.EvaluateGlobalEval(
      'implicitFromEval = 1; typeof implicitFromEval', '<global-eval-test>',
      Stage);
    Expect<string>((Value as TGocciaStringLiteralValue).Value).ToBe(
      'number');
    Expect<Boolean>(EvalStage(Engine,
      '"use strict"; strictImplicit = 1') = gesRun).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalIsNonStrictInStrictEngine;
begin
  AssertDirectiveFreeEvalIsNonStrict;
end;

// ES2026 §6.1.6.1.4 and §6.1.6.1.5 (Number::divide, Number::subtract) and
// §13.15.4: NaN and infinity arithmetic follows IEEE 754 and never traps. An
// embedder that calls EvaluateGlobalEval directly runs with whatever
// floating-point mode its own process has, FPC's default among them, which
// raises EInvalidOp on 0/0. The entry establishes the mode and restores the
// caller's.
procedure TGlobalEvalTests.TestEvalEstablishesFloatingPointMode;
const
  // FPC's startup mask: invalid operation, zero divide and overflow trap.
  EMBEDDER_MASK: TFPUExceptionMask = [exDenormalized, exUnderflow,
    exPrecision];
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  SavedMask: TFPUExceptionMask;
begin
  Source := TStringList.Create;
  Engine := nil;
  SavedMask := GetExceptionMask;
  try
    Engine := CreateEngine(Source);
    SetExceptionMask(EMBEDDER_MASK);
    try
      Value := Engine.EvaluateGlobalEval('0 / 0', '<global-eval-test>',
        Stage);
      Expect<Boolean>((Value as TGocciaNumberLiteralValue).IsNaN).ToBe(True);
      Value := Engine.EvaluateGlobalEval('Infinity - Infinity',
        '<global-eval-test>', Stage);
      Expect<Boolean>((Value as TGocciaNumberLiteralValue).IsNaN).ToBe(True);
      // Operands the compiler cannot fold.
      Value := Engine.EvaluateGlobalEval(
        'var zero = 0, inf = Infinity; [zero / zero, inf - inf, inf * zero]' +
        '.every((n) => n !== n)', '<global-eval-test>', Stage);
      Expect<Boolean>((Value as TGocciaBooleanLiteralValue).Value).ToBe(True);
      // The caller's mask is back once the entry returns.
      Expect<Boolean>(GetExceptionMask = EMBEDDER_MASK).ToBe(True);
    finally
      SetExceptionMask(SavedMask);
    end;
  finally
    Engine.Free;
    Source.Free;
  end;
end;

// Jobs a run left queued run script too: a promise reaction that divides 0
// by 0 runs inside WaitForRuntimeIdle, which an embedder can call on its own.
procedure TGlobalEvalTests.TestWaitForRuntimeIdleEstablishesFloatingPointMode;
const
  EMBEDDER_MASK: TFPUExceptionMask = [exDenormalized, exUnderflow,
    exPrecision];
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  SavedMask: TFPUExceptionMask;
begin
  Source := TStringList.Create;
  Engine := nil;
  SavedMask := GetExceptionMask;
  try
    Engine := CreateEngine(Source);
    Engine.EvaluateGlobalEval(
      'var zero = 0; var settled = "pending";' +
      'Promise.resolve().then(() => { settled = zero / zero; });',
      '<global-eval-test>', Stage);
    SetExceptionMask(EMBEDDER_MASK);
    try
      Engine.WaitForRuntimeIdle;
      Expect<Boolean>(GetExceptionMask = EMBEDDER_MASK).ToBe(True);
    finally
      SetExceptionMask(SavedMask);
    end;
    Value := Engine.EvaluateGlobalEval('settled !== settled',
      '<global-eval-test>', Stage);
    Expect<Boolean>((Value as TGocciaBooleanLiteralValue).Value).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TGlobalEvalTests.Create('Engine Global Eval'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
