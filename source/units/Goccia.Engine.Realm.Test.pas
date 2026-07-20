program Goccia.Engine.Realm.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.ExecutionContext,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.Realm,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.URL,
  Goccia.Scope,
  Goccia.TestSetup,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TTestEngineRealm = class(TTestSuite)
  private
    FExpectedRealm: TGocciaRealm;
    function RunInline(const ASource: string): TGocciaScriptResult;
    function RunRuntimeInline(const ASource: string): TGocciaScriptResult;
    function RealmProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionContextProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure AssertRealmProbeWithExecutor(const AExecutor: TGocciaExecutor);
    procedure AssertFunctionContextProbeWithExecutor(
      const AExecutor: TGocciaExecutor);
    procedure AssertConstructorFunctionContextProbeWithExecutor(
      const AExecutor: TGocciaExecutor);
    procedure AssertRepeatedTaggedTemplateExecutionWithExecutor(
      const AExecutor: TGocciaExecutor);
  public
    procedure SetupTests; override;

    procedure TestCurrentRealmIsAssignedDuringLife;
    procedure TestCurrentRealmIsClearedAfterDestroy;
    procedure TestSequentialEnginesHaveIsolatedArrayPrototype;
    procedure TestSequentialEnginesHaveIsolatedObjectPrototype;
    procedure TestSequentialEnginesHaveIsolatedStringPrototype;
    procedure TestSequentialEnginesHaveFreshDataViewPrototypeMembers;
    procedure TestSequentialEnginesHaveFreshURLSearchParamsPrototype;
    procedure TestSequentialEnginesHaveFreshURLPrototype;
    procedure TestNestedEngineRestoresOuterRealmOnDestroy;
    procedure TestEachEngineGetsADistinctRealm;
    procedure TestInterpreterExecutionContextUsesEngineRealm;
    procedure TestBytecodeExecutionContextUsesEngineRealm;
    procedure TestInterpreterFunctionExecutionContextUsesFunctionValue;
    procedure TestBytecodeFunctionExecutionContextUsesFunctionValue;
    procedure TestInterpreterConstructorExecutionContextUsesFunctionValue;
    procedure TestBytecodeConstructorExecutionContextUsesFunctionValue;
    procedure TestInterpreterRepeatedEngineExecutionGetsFreshTemplateSites;
    procedure TestBytecodeRepeatedEngineExecutionGetsFreshTemplateSites;
    procedure TestBytecodeGlobalReadCacheRevalidatesLexicalShadow;
  end;

procedure TTestEngineRealm.SetupTests;
begin
  Test('CurrentRealm is the engine''s realm during its lifetime',
    TestCurrentRealmIsAssignedDuringLife);
  Test('CurrentRealm is nil after the only engine is destroyed',
    TestCurrentRealmIsClearedAfterDestroy);
  Test('Array.prototype mutations do not leak to the next engine',
    TestSequentialEnginesHaveIsolatedArrayPrototype);
  Test('Object.prototype mutations do not leak to the next engine',
    TestSequentialEnginesHaveIsolatedObjectPrototype);
  Test('String.prototype mutations do not leak to the next engine',
    TestSequentialEnginesHaveIsolatedStringPrototype);
  Test('DataView.prototype methods are fresh for each engine',
    TestSequentialEnginesHaveFreshDataViewPrototypeMembers);
  Test('URLSearchParams.prototype is fresh for each engine',
    TestSequentialEnginesHaveFreshURLSearchParamsPrototype);
  Test('URL.prototype is fresh for each engine',
    TestSequentialEnginesHaveFreshURLPrototype);
  Test('Destroying a nested engine restores the outer engine''s realm',
    TestNestedEngineRestoresOuterRealmOnDestroy);
  Test('Each engine owns a distinct realm instance',
    TestEachEngineGetsADistinctRealm);
  Test('Interpreter execution context uses the engine realm',
    TestInterpreterExecutionContextUsesEngineRealm);
  Test('Bytecode execution context uses the engine realm',
    TestBytecodeExecutionContextUsesEngineRealm);
  Test('Interpreter function execution context carries function value',
    TestInterpreterFunctionExecutionContextUsesFunctionValue);
  Test('Bytecode function execution context carries function value',
    TestBytecodeFunctionExecutionContextUsesFunctionValue);
  Test('Interpreter constructor execution context carries function value',
    TestInterpreterConstructorExecutionContextUsesFunctionValue);
  Test('Bytecode constructor execution context carries function value',
    TestBytecodeConstructorExecutionContextUsesFunctionValue);
  Test('Interpreter repeated engine execution gets fresh template sites',
    TestInterpreterRepeatedEngineExecutionGetsFreshTemplateSites);
  Test('Bytecode repeated engine execution gets fresh template sites',
    TestBytecodeRepeatedEngineExecutionGetsFreshTemplateSites);
  Test('Bytecode global read cache revalidates a later lexical shadow',
    TestBytecodeGlobalReadCacheRevalidatesLexicalShadow);
end;

function TTestEngineRealm.RunInline(const ASource: string): TGocciaScriptResult;
begin
  Result := TGocciaEngine.RunScript(ASource, '<engine-realm-test>');
end;

function TTestEngineRealm.RunRuntimeInline(
  const ASource: string): TGocciaScriptResult;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := nil;
  Runtime := nil;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<engine-realm-test>', Source, Executor);
    Runtime := TGocciaRuntime.Create(Engine);
    Runtime.Install(TGocciaURLRuntimeExtension.Create);
    Result := Runtime.Execute;
  finally
    Runtime.Free;
    Engine.Free;
    Source.Free;
    Executor.Free;
  end;
end;

function TTestEngineRealm.RealmProbe(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Running: TGocciaExecutionContext;
begin
  Running := RunningExecutionContext;
  Result := TGocciaBooleanLiteralValue.FromBoolean(
    Assigned(FExpectedRealm) and
    (CurrentRealm = FExpectedRealm) and
    (Running.Realm = FExpectedRealm) and
    Assigned(Running.Scope) and
    not Assigned(Running.FunctionValue));
end;

function TTestEngineRealm.FunctionContextProbe(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Running: TGocciaExecutionContext;
begin
  Running := RunningExecutionContext;
  Result := TGocciaBooleanLiteralValue.FromBoolean(
    Assigned(FExpectedRealm) and
    (CurrentRealm = FExpectedRealm) and
    (Running.Realm = FExpectedRealm) and
    Assigned(Running.Scope) and
    Assigned(Running.FunctionValue));
end;

procedure TTestEngineRealm.AssertRealmProbeWithExecutor(
  const AExecutor: TGocciaExecutor);
var
  Engine: TGocciaEngine;
  Source: TStringList;
  ResultValue: TGocciaScriptResult;
begin
  Source := TStringList.Create;
  Source.Text := 'realmProbe();';
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<realm-execution-context>', Source,
      AExecutor);
    FExpectedRealm := Engine.Realm;
    Engine.InjectGlobal('realmProbe',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RealmProbe,
        'realmProbe', 0));
    ResultValue := Engine.Execute;
    Expect<Boolean>(
      (ResultValue.Result as TGocciaBooleanLiteralValue).Value).ToBe(True);
  finally
    FExpectedRealm := nil;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.AssertFunctionContextProbeWithExecutor(
  const AExecutor: TGocciaExecutor);
var
  Engine: TGocciaEngine;
  Source: TStringList;
  ResultValue: TGocciaScriptResult;
begin
  Source := TStringList.Create;
  Source.Text := 'const checked = () => functionContextProbe(); checked();';
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<realm-function-context>', Source,
      AExecutor);
    FExpectedRealm := Engine.Realm;
    Engine.InjectGlobal('functionContextProbe',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FunctionContextProbe,
        'functionContextProbe', 0));
    ResultValue := Engine.Execute;
    Expect<Boolean>(
      (ResultValue.Result as TGocciaBooleanLiteralValue).Value).ToBe(True);
  finally
    FExpectedRealm := nil;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.AssertConstructorFunctionContextProbeWithExecutor(
  const AExecutor: TGocciaExecutor);
var
  Engine: TGocciaEngine;
  Source: TStringList;
  ResultValue: TGocciaScriptResult;
begin
  Source := TStringList.Create;
  Source.Text :=
    'class Checked {' +
    '  constructor() {' +
    '    this.ok = functionContextProbe();' +
    '  }' +
    '}' +
    'new Checked().ok;';
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<realm-constructor-context>', Source,
      AExecutor);
    FExpectedRealm := Engine.Realm;
    Engine.InjectGlobal('functionContextProbe',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(FunctionContextProbe,
        'functionContextProbe', 0));
    ResultValue := Engine.Execute;
    Expect<Boolean>(
      (ResultValue.Result as TGocciaBooleanLiteralValue).Value).ToBe(True);
  finally
    FExpectedRealm := nil;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.AssertRepeatedTaggedTemplateExecutionWithExecutor(
  const AExecutor: TGocciaExecutor);
var
  Engine: TGocciaEngine;
  Source: TStringList;
  FirstResult, SecondResult: TGocciaScriptResult;
begin
  Source := TStringList.Create;
  Source.Text :=
    'globalThis.tag = (strings) => {' +
    '  globalThis.firstTemplate = strings;' +
    '  return strings[0];' +
    '};' +
    'tag`first`;';
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<realm-repeated-template>', Source,
      AExecutor);
    FirstResult := Engine.Execute;
    Expect<string>((FirstResult.Result as TGocciaStringLiteralValue).Value).
      ToBe('first');
    Expect<Integer>(Engine.Realm.TemplateMapCount).ToBe(1);

    Source.Text :=
      'globalThis.tag = (strings) => ' +
      '  globalThis.firstTemplate === strings ? "stale" : strings[0];' +
      'tag`second`;';
    SecondResult := Engine.Execute;
    Expect<string>((SecondResult.Result as TGocciaStringLiteralValue).Value).
      ToBe('second');
    Expect<Integer>(Engine.Realm.TemplateMapCount).ToBe(2);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.TestCurrentRealmIsAssignedDuringLife;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := '';
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<realm-life>', Source, Executor);
    try
      Expect<Boolean>(CurrentRealm <> nil).ToBe(True);
    finally
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestCurrentRealmIsClearedAfterDestroy;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
  PreviousRealm: TGocciaRealm;
begin
  PreviousRealm := CurrentRealm;
  Source := TStringList.Create;
  Source.Text := '';
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<realm-clear>', Source, Executor);
    Engine.Free;
    Source.Free;
    // FPrevRealm defaults to whatever was current at construction; for a single
    // engine on a clean main thread that's PreviousRealm.
    Expect<Boolean>(CurrentRealm = PreviousRealm).ToBe(True);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestSequentialEnginesHaveIsolatedArrayPrototype;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  ResultA := RunInline(
    'Array.prototype.__poisonA = 42;' +
    'Array.prototype.__poisonA;');
  Expect<Double>((ResultA.Result as TGocciaNumberLiteralValue).Value).ToBe(42);

  // A fresh engine must not see __poisonA because Array.prototype was rebuilt
  // from the new realm's intrinsic graph.
  ResultB := RunInline('typeof Array.prototype.__poisonA;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestSequentialEnginesHaveIsolatedObjectPrototype;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  ResultA := RunInline(
    'Object.prototype.__poisonObj = "leaked";' +
    'Object.prototype.__poisonObj;');
  Expect<string>((ResultA.Result as TGocciaStringLiteralValue).Value).ToBe(
    'leaked');

  ResultB := RunInline('typeof Object.prototype.__poisonObj;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestSequentialEnginesHaveIsolatedStringPrototype;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  ResultA := RunInline(
    'String.prototype.__poisonStr = () => "x";' +
    'typeof String.prototype.__poisonStr;');
  Expect<string>((ResultA.Result as TGocciaStringLiteralValue).Value).ToBe(
    'function');

  ResultB := RunInline('typeof String.prototype.__poisonStr;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestSequentialEnginesHaveFreshDataViewPrototypeMembers;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  ResultA := RunInline(
    'DataView.prototype.getUint8.__poisonDV = 7;' +
    'DataView.prototype.getUint8.__poisonDV;');
  Expect<Double>((ResultA.Result as TGocciaNumberLiteralValue).Value).ToBe(7);

  ResultB := RunInline('typeof DataView.prototype.getUint8.__poisonDV;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestSequentialEnginesHaveFreshURLSearchParamsPrototype;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  // URLSearchParams.prototype is built lazily through TGocciaSharedPrototype
  // and rebuilt per realm; mutations on engine A must not leak.
  ResultA := RunRuntimeInline(
    'URLSearchParams.prototype.__poisonUSP = 7;' +
    'URLSearchParams.prototype.__poisonUSP;');
  Expect<Double>((ResultA.Result as TGocciaNumberLiteralValue).Value).ToBe(7);

  ResultB := RunRuntimeInline('typeof URLSearchParams.prototype.__poisonUSP;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestSequentialEnginesHaveFreshURLPrototype;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  // URL.prototype is built lazily through TGocciaSharedPrototype and
  // rebuilt per realm; mutations on engine A must not leak.
  ResultA := RunRuntimeInline(
    'URL.prototype.__poisonURL = 7;' +
    'URL.prototype.__poisonURL;');
  Expect<Double>((ResultA.Result as TGocciaNumberLiteralValue).Value).ToBe(7);

  ResultB := RunRuntimeInline('typeof URL.prototype.__poisonURL;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestNestedEngineRestoresOuterRealmOnDestroy;
var
  OuterEngine, InnerEngine: TGocciaEngine;
  OuterExecutor, InnerExecutor: TGocciaInterpreterExecutor;
  OuterSource, InnerSource: TStringList;
  OuterRealm, InnerRealm: TGocciaRealm;
begin
  OuterSource := TStringList.Create;
  OuterSource.Text := '';
  InnerSource := TStringList.Create;
  InnerSource.Text := '';

  OuterExecutor := TGocciaInterpreterExecutor.Create;
  InnerExecutor := TGocciaInterpreterExecutor.Create;
  try
    OuterEngine := TGocciaEngine.Create('<outer>', OuterSource, OuterExecutor);
    try
      OuterRealm := CurrentRealm;
      Expect<Boolean>(OuterRealm <> nil).ToBe(True);

      InnerEngine := TGocciaEngine.Create('<inner>', InnerSource, InnerExecutor);
      try
        InnerRealm := CurrentRealm;
        // Constructing a nested engine swaps in its own realm.
        Expect<Boolean>(InnerRealm <> OuterRealm).ToBe(True);
      finally
        InnerEngine.Free;
      end;

      // Destroying the inner engine must restore the outer engine's realm.
      Expect<Boolean>(CurrentRealm = OuterRealm).ToBe(True);
    finally
      OuterEngine.Free;
      InnerSource.Free;
      OuterSource.Free;
    end;
  finally
    InnerExecutor.Free;
    OuterExecutor.Free;
  end;
end;

procedure TTestEngineRealm.TestEachEngineGetsADistinctRealm;
var
  EngineA, EngineB: TGocciaEngine;
  ExecutorA, ExecutorB: TGocciaInterpreterExecutor;
  SourceA, SourceB: TStringList;
  RealmA, RealmB: TGocciaRealm;
begin
  // Keep both engines alive simultaneously while capturing their realms so the
  // distinctness check compares two live pointers; once EngineA is freed its
  // realm pointer becomes dangling and the FPC heap is free to reuse the
  // address for EngineB's realm, which would intermittently fail RealmA <>
  // RealmB.  The nested-engine path (TGocciaEngine stacks via FPrevRealm) lets
  // us hold both at once.
  SourceA := TStringList.Create;
  SourceA.Text := '';
  SourceB := TStringList.Create;
  SourceB.Text := '';

  ExecutorA := TGocciaInterpreterExecutor.Create;
  ExecutorB := TGocciaInterpreterExecutor.Create;
  try
    EngineA := TGocciaEngine.Create('<engine-a>', SourceA, ExecutorA);
    try
      RealmA := CurrentRealm;
      EngineB := TGocciaEngine.Create('<engine-b>', SourceB, ExecutorB);
      try
        RealmB := CurrentRealm;
        Expect<Boolean>(RealmA <> nil).ToBe(True);
        Expect<Boolean>(RealmB <> nil).ToBe(True);
        Expect<Boolean>(RealmB <> RealmA).ToBe(True);
      finally
        EngineB.Free;
      end;
    finally
      EngineA.Free;
      SourceA.Free;
      SourceB.Free;
    end;
  finally
    ExecutorB.Free;
    ExecutorA.Free;
  end;
end;

procedure TTestEngineRealm.TestInterpreterExecutionContextUsesEngineRealm;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertRealmProbeWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeExecutionContextUsesEngineRealm;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertRealmProbeWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestInterpreterFunctionExecutionContextUsesFunctionValue;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertFunctionContextProbeWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeFunctionExecutionContextUsesFunctionValue;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertFunctionContextProbeWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestInterpreterConstructorExecutionContextUsesFunctionValue;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertConstructorFunctionContextProbeWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeConstructorExecutionContextUsesFunctionValue;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertConstructorFunctionContextProbeWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestInterpreterRepeatedEngineExecutionGetsFreshTemplateSites;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertRepeatedTaggedTemplateExecutionWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeRepeatedEngineExecutionGetsFreshTemplateSites;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertRepeatedTaggedTemplateExecutionWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeGlobalReadCacheRevalidatesLexicalShadow;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  ResultValue: TGocciaScriptResult;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text :=
    'globalThis.readCachedArray = () => Array;' +
    'globalThis.readCachedArray();' +
    'globalThis.readCachedArray();';
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<global-read-cache-shadow>', Source,
      Executor);
    Engine.Execute;

    Engine.Interpreter.GlobalScope.PredeclareLexicalBinding('Array', dtLet);
    Engine.Interpreter.GlobalScope.DefineLexicalBinding('Array',
      TGocciaStringLiteralValue.Create('lexical Array'), dtLet);
    Source.Text :=
      'globalThis.readCachedArray() === "lexical Array";';
    ResultValue := Engine.Execute;

    Expect<Boolean>(
      (ResultValue.Result as TGocciaBooleanLiteralValue).Value).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestEngineRealm.Create('Engine Realm'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
