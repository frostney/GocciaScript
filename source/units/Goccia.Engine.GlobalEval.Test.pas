program Goccia.Engine.GlobalEval.Test;

{$I Goccia.inc}

{ ADR 0134 Phase 2: eval code whose variable environment is the global one
  (indirect eval, a ShadowRealm child realm's eval and
  ShadowRealm.prototype.evaluate) runs through TGocciaEngine.EvaluateGlobalEval.
  In bytecode mode the code is compiled and run on the VM, so a function or
  class it creates is a VM value; only the tree-walk evaluator creates
  AST-backed ones, which interpreter mode still does. These tests call the
  entry directly and through the Test262 host's eval; the ShadowRealm unit
  reaches the same entry and no longer links the evaluator.

  ADR 0134 Phase 3: a direct eval is compiled too, as a closure of the call
  site, so what it creates is a VM value as well, at a strict call site and
  at a sloppy one. The VM no longer reaches the tree-walk evaluator for eval
  code. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.SourcePipeline,
  Goccia.Test262.Host,
  Goccia.TestSetup,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Error,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

const
  // The VM's function and class values are private to Goccia.VM.
  BYTECODE_FUNCTION_CLASS = 'TGocciaBytecodeFunctionValue';
  VM_CLASS_CLASS = 'TGocciaVMClassValue';

type
  TGlobalEvalTests = class(TTestSuite)
  private
    function CreateEngine(const AExecutor: TGocciaExecutor;
      const ASource: TStringList): TGocciaEngine;
    function EvalStage(const AEngine: TGocciaEngine;
      const ASource: string): TGocciaGlobalEvalStage;
    procedure TestBytecodeEvalCreatesVMFunctions;
    procedure TestBytecodeEvalCreatesVMClasses;
    procedure TestInterpreterEvalCreatesASTFunctions;
    procedure TestBytecodeTest262HostEvalCreatesVMFunctions;
    procedure TestBytecodeStrictDirectEvalCreatesVMValues;
    procedure TestBytecodeEvalVarIsDeletableGlobal;
    procedure TestBytecodeEvalReturnsCompletionValue;
    procedure TestBytecodeEvalReportsCompileStage;
    procedure TestBytecodeEvalReportsRunStage;
    procedure AssertDirectiveFreeEvalIsNonStrict(
      const AExecutor: TGocciaExecutor);
    procedure TestBytecodeEvalIsNonStrictInStrictEngine;
    procedure TestInterpreterEvalIsNonStrictInStrictEngine;
  public
    procedure SetupTests; override;
  end;

procedure TGlobalEvalTests.SetupTests;
begin
  Test('Bytecode global eval creates VM functions, not AST functions',
    TestBytecodeEvalCreatesVMFunctions);
  Test('Bytecode global eval creates VM classes',
    TestBytecodeEvalCreatesVMClasses);
  Test('Interpreter global eval still creates AST functions',
    TestInterpreterEvalCreatesASTFunctions);
  Test('Bytecode Test262 host eval creates VM functions',
    TestBytecodeTest262HostEvalCreatesVMFunctions);
  Test('Bytecode direct eval creates VM values at strict and sloppy sites',
    TestBytecodeStrictDirectEvalCreatesVMValues);
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
  Test('Interpreter directive-free global eval is non-strict in a strict ' +
    'engine', TestInterpreterEvalIsNonStrictInStrictEngine);
end;

function TGlobalEvalTests.CreateEngine(const AExecutor: TGocciaExecutor;
  const ASource: TStringList): TGocciaEngine;
begin
  Result := TGocciaEngine.Create('<global-eval-test>', ASource, AExecutor);
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
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
    Value := Engine.EvaluateGlobalEval('function made() {} made',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(True);
    Expect<Boolean>(Value is TGocciaFunctionValue).ToBe(False);
    Value := Engine.EvaluateGlobalEval('(x) => x', '<global-eval-test>',
      Stage);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(True);
    Value := Engine.EvaluateGlobalEval('"use strict"; (function* g() {})',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalCreatesVMClasses;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
    Value := Engine.EvaluateGlobalEval('class Made {} Made',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value.ClassName = VM_CLASS_CLASS).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestInterpreterEvalCreatesASTFunctions;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
    Value := Engine.EvaluateGlobalEval('function made() {} made',
      '<global-eval-test>', Stage);
    Expect<Boolean>(Value is TGocciaFunctionValue).ToBe(True);
    Expect<Boolean>(Value.ClassName = BYTECODE_FUNCTION_CLASS).ToBe(False);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeTest262HostEvalCreatesVMFunctions;
var
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  Executor: TGocciaExecutor;
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
  EngineOptions.Mode := t262emBytecode;
  EngineOptions.SourceType := stScript;
  EngineOptions.Capabilities := TGocciaCapabilities.None;
  Executor := CreateTest262Executor(t262emBytecode);
  Engine := nil;
  Host := nil;
  try
    Engine := TGocciaEngine.Create('<global-eval-test>', Source, Executor,
      EngineOptions.Capabilities);
    ConfigureTest262Engine(Engine, Executor, EngineOptions);
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
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeStrictDirectEvalCreatesVMValues;
var
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  Executor: TGocciaExecutor;
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
  EngineOptions.Mode := t262emBytecode;
  EngineOptions.SourceType := stScript;
  EngineOptions.Capabilities := TGocciaCapabilities.None;
  Executor := CreateTest262Executor(t262emBytecode);
  Engine := nil;
  Host := nil;
  try
    Engine := TGocciaEngine.Create('<direct-eval-test>', Source, Executor,
      EngineOptions.Capabilities);
    ConfigureTest262Engine(Engine, Executor, EngineOptions);
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
    Expect<Boolean>(GlobalObject.GetProperty('sloppyFunction') is
      TGocciaFunctionValue).ToBe(False);
  finally
    Host.Free;
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalVarIsDeletableGlobal;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Descriptor: TGocciaPropertyDescriptor;
  GlobalObject: TGocciaObjectValue;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
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
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalReturnsCompletionValue;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
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
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalReportsCompileStage;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
    Expect<Boolean>(EvalStage(Engine, 'new.target') = gesCompile).ToBe(True);
    Expect<Boolean>(EvalStage(Engine, 'return 1') = gesCompile).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TGlobalEvalTests.TestBytecodeEvalReportsRunStage;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := CreateEngine(Executor, Source);
    Expect<Boolean>(EvalStage(Engine, 'throw 1') = gesRun).ToBe(True);
    TGocciaObjectValue(Engine.Realm.GlobalObject).PreventExtensions;
    // EvalDeclarationInstantiation's CanDeclareGlobalVar check runs after
    // compilation: a run-stage TypeError.
    Expect<Boolean>(EvalStage(Engine, 'var neverDeclared;') = gesRun).ToBe(
      True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

// ES2026 §19.2.1.1 step 13: eval code is strict only by its own directive,
// also in an engine without the non-strict compatibility profile.
procedure TGlobalEvalTests.AssertDirectiveFreeEvalIsNonStrict(
  const AExecutor: TGocciaExecutor);
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<global-eval-test>', Source, AExecutor);
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
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertDirectiveFreeEvalIsNonStrict(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TGlobalEvalTests.TestInterpreterEvalIsNonStrictInStrictEngine;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertDirectiveFreeEvalIsNonStrict(Executor);
  finally
    Executor.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TGlobalEvalTests.Create('Engine Global Eval'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
