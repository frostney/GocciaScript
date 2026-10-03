program Goccia.Engine.Realm.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.AsyncContext,
  Goccia.CallStack,
  Goccia.Engine,
  Goccia.ExecutionContext,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Parser,
  Goccia.Realm,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.URL,
  Goccia.Scope,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

type
  TTestEngineRealm = class(TTestSuite)
  private
    FExpectedRealm: TGocciaRealm;
    FRecordedSourcePaths: TStringList;
    FNestedLog: TStringList;
    FNestedChildSource: string;
    FNestedChildIsBytecode: Boolean;
    FRejectionLog: TStringList;
    FChildRejectionLog: TStringList;
    FRejectionHookHandles: Boolean;
    FRejectionHookHandlesAll: Boolean;
    FRejectionHookHandlesFirst: Boolean;
    FRejectionHookRaises: Boolean;
    FRejectionHookEngine: TGocciaEngine;
    FRejectionHookProgram: TGocciaProgram;
    function CreateProgram(const ASource: string): TGocciaProgram;
    function RunInline(const ASource: string): TGocciaScriptResult;
    function RunRuntimeInline(const ASource: string): TGocciaScriptResult;
    function RealmProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionContextProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SourcePathProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RunSourcePathProbe(const AFileName, ASource: string): string;
    function RecordSourcePathProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure AssertRealmProbeWithExecutor(const AExecutor: TGocciaExecutor);
    procedure AssertFunctionContextProbeWithExecutor(
      const AExecutor: TGocciaExecutor);
    procedure AssertConstructorFunctionContextProbeWithExecutor(
      const AExecutor: TGocciaExecutor);
    procedure AssertRepeatedTaggedTemplateExecutionWithExecutor(
      const AExecutor: TGocciaExecutor);
    function NestedReportProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NestedRunChildProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure AssertNestedExecuteLeavesOuterJobsWithExecutor(
      const AExecutor: TGocciaExecutor; const AIsBytecode: Boolean);
    function UnhandledRejectionMessage(const AExecutor: TGocciaExecutor;
      const ASource: string;
      const AMode: TGocciaUnhandledRejectionMode): string;
    function TakeRejectionProbe(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure AssertUnhandledRejectionModesWithExecutor(
      const AExecutor: TGocciaExecutor);
    procedure RecordRejection(const APromise: TGocciaValue;
      const AReason: TGocciaValue);
    procedure RecordChildRejection(const APromise: TGocciaValue;
      const AReason: TGocciaValue);
    function HookedRejections(const AExecutor: TGocciaExecutor;
      const ASource: string; const AMode: TGocciaUnhandledRejectionMode;
      out ARaised: string): string;
    procedure AssertUnhandledRejectionHookWithExecutor(
      const AExecutor: TGocciaExecutor; const AIsBytecode: Boolean);
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
    procedure TestNestedEngineRestoresOuterAsyncContextOnDestroy;
    procedure TestInterpreterNestedExecuteLeavesOuterJobs;
    procedure TestBytecodeNestedExecuteLeavesOuterJobs;
    procedure TestInterpreterExecuteRaisesUnhandledRejection;
    procedure TestBytecodeExecuteRaisesUnhandledRejection;
    procedure TestInterpreterUnhandledRejectionHook;
    procedure TestBytecodeUnhandledRejectionHook;
    procedure TestEachEngineGetsADistinctRealm;
    procedure TestInterpreterExecutionContextUsesEngineRealm;
    procedure TestBytecodeExecutionContextUsesEngineRealm;
    procedure TestInterpreterFunctionExecutionContextUsesFunctionValue;
    procedure TestBytecodeFunctionExecutionContextUsesFunctionValue;
    procedure TestInterpreterConstructorExecutionContextUsesFunctionValue;
    procedure TestBytecodeConstructorExecutionContextUsesFunctionValue;
    procedure TestBytecodeFunctionExecutionContextCarriesSourcePath;
    procedure TestBytecodeFunctionExecutionContextFollowsCalleeModule;
    procedure TestBytecodeEntryRebindsTheThreadCallStack;
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
  Test('Destroying a nested engine restores the outer async context',
    TestNestedEngineRestoresOuterAsyncContextOnDestroy);
  Test('Interpreter nested Execute neither runs nor drops the outer jobs',
    TestInterpreterNestedExecuteLeavesOuterJobs);
  Test('Bytecode nested Execute neither runs nor drops the outer jobs',
    TestBytecodeNestedExecuteLeavesOuterJobs);
  Test('Interpreter Execute raises an unhandled rejection unless told to ' +
    'ignore it',
    TestInterpreterExecuteRaisesUnhandledRejection);
  Test('Bytecode Execute raises an unhandled rejection unless told to ' +
    'ignore it',
    TestBytecodeExecuteRaisesUnhandledRejection);
  Test('Interpreter reports each unhandled rejection to the hook',
    TestInterpreterUnhandledRejectionHook);
  Test('Bytecode reports each unhandled rejection to the hook',
    TestBytecodeUnhandledRejectionHook);
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
  Test('Bytecode function execution context carries its source path',
    TestBytecodeFunctionExecutionContextCarriesSourcePath);
  Test('Bytecode function execution context follows the callee''s module',
    TestBytecodeFunctionExecutionContextFollowsCalleeModule);
  Test('Bytecode entry rebinds the thread''s call stack',
    TestBytecodeEntryRebindsTheThreadCallStack);
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

function TTestEngineRealm.SourcePathProbe(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    RunningExecutionContext.SourcePath);
end;

function TTestEngineRealm.RunSourcePathProbe(const AFileName,
  ASource: string): string;
var
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create(AFileName, Source, Executor);
    Engine.InjectGlobal('sourcePathProbe',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(SourcePathProbe,
        'sourcePathProbe', 0));
    Result := (Engine.Execute.Result as TGocciaStringLiteralValue).Value;
  finally
    Engine.Free;
    Source.Free;
    Executor.Free;
  end;
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

{ An engine's teardown used to clear the thread's async-context state outright,
  which is correct for a worker thread reusing a slot but wrong for the nested
  lifetimes the engine supports: a ShadowRealm owns a child engine, and freeing
  it can happen inside the outer engine's run or a microtask callback. Clearing
  there stripped the outer engine's AsyncLocalStorage binding mid-run. }
procedure TTestEngineRealm.TestNestedEngineRestoresOuterAsyncContextOnDestroy;
var
  OuterEngine, InnerEngine: TGocciaEngine;
  OuterExecutor, InnerExecutor: TGocciaInterpreterExecutor;
  OuterSource, InnerSource: TStringList;
  OuterContext: TGocciaAsyncContextSnapshot;
  Key, Store: TGocciaValue;
begin
  OuterSource := TStringList.Create;
  OuterSource.Text := '';
  InnerSource := TStringList.Create;
  InnerSource.Text := '';

  OuterExecutor := TGocciaInterpreterExecutor.Create;
  InnerExecutor := TGocciaInterpreterExecutor.Create;
  try
    OuterEngine := TGocciaEngine.Create('<outer-async>', OuterSource,
      OuterExecutor);
    try
      Key := TGocciaStringLiteralValue.Create('storage-key');
      Store := TGocciaStringLiteralValue.Create('outer-store');
      // Key and Store live only in Pascal locals until the snapshot is
      // installed as the current context; the derive itself allocates, so
      // they need temp roots across it or a collection makes this test
      // nondeterministic.
      TGarbageCollector.Instance.AddTempRoot(Key);
      TGarbageCollector.Instance.AddTempRoot(Store);
      try
        OuterContext := DeriveAsyncContext(nil, Key, Store);
        SetCurrentAsyncContext(OuterContext);
      finally
        TGarbageCollector.Instance.RemoveTempRoot(Store);
        TGarbageCollector.Instance.RemoveTempRoot(Key);
      end;
      Expect<Boolean>(CurrentAsyncContext = OuterContext).ToBe(True);

      InnerEngine := TGocciaEngine.Create('<inner-async>', InnerSource,
        InnerExecutor);
      try
        // A nested engine starts on an empty context rather than inheriting
        // the outer engine's, whose stores belong to the outer realm.
        Expect<Boolean>(CurrentAsyncContext = nil).ToBe(True);
      finally
        InnerEngine.Free;
      end;

      Expect<Boolean>(CurrentAsyncContext = OuterContext).ToBe(True);
    finally
      OuterEngine.Free;
      InnerSource.Free;
      OuterSource.Free;
    end;
    // The outermost engine's own teardown still leaves the thread clean.
    Expect<Boolean>(CurrentAsyncContext = nil).ToBe(True);
  finally
    InnerExecutor.Free;
    OuterExecutor.Free;
  end;
end;

function TTestEngineRealm.NestedReportProbe(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FNestedLog.Add(AArgs.GetElement(0).ToStringLiteral.Value);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ What an embedder's native callback does when it runs another engine to
  completion: the outer engine is mid-statement for the whole call. }
function TTestEngineRealm.NestedRunChildProbe(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ChildEngine: TGocciaEngine;
  ChildExecutor: TGocciaExecutor;
  ChildSource: TStringList;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  ChildSource := TStringList.Create;
  ChildSource.Text := FNestedChildSource;
  if FNestedChildIsBytecode then
    ChildExecutor := TGocciaBytecodeExecutor.Create
  else
    ChildExecutor := TGocciaInterpreterExecutor.Create;
  ChildEngine := nil;
  try
    ChildEngine := TGocciaEngine.Create('<nested-child>', ChildSource,
      ChildExecutor);
    ChildEngine.InjectGlobal('report',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(NestedReportProbe,
        'report', 1));
    if Assigned(FChildRejectionLog) then
    begin
      ChildEngine.UnhandledRejections := urIgnore;
      ChildEngine.OnUnhandledRejection := RecordChildRejection;
    end;
    // Values the outer engine hands over, as an embedder sharing them would.
    if AArgs.Length >= 2 then
    begin
      ChildEngine.InjectGlobal('gate', AArgs.GetElement(0));
      ChildEngine.InjectGlobal('open', AArgs.GetElement(1));
    end;
    try
      ChildEngine.Execute;
    except
      on E: Exception do
        FNestedLog.Add('child failed');
    end;
  finally
    ChildEngine.Free;
    ChildExecutor.Free;
    ChildSource.Free;
  end;
end;

procedure TTestEngineRealm.AssertNestedExecuteLeavesOuterJobsWithExecutor(
  const AExecutor: TGocciaExecutor; const AIsBytecode: Boolean);
const
  OUTER_SOURCE =
    'Promise.resolve().then(() => report("outer job"));' +
    'runChild();' +
    'report("returned");';
  CHILD_JOB = 'Promise.resolve().then(() => report("child job"));';
  SHARING_OUTER_SOURCE =
    'let open;' +
    'const gate = new Promise((resolve) => { open = resolve; });' +
    'runChild(gate, open);' +
    'report("returned");';
  SHARING_CHILD_SOURCE =
    'gate.then((value) => report("child awaited " + value));' +
    'open(2);';
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := OUTER_SOURCE;
  FNestedLog := TStringList.Create;
  FNestedChildIsBytecode := AIsBytecode;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<nested-outer>', Source, AExecutor);
    Engine.InjectGlobal('report',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(NestedReportProbe,
        'report', 1));
    Engine.InjectGlobal('runChild',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(NestedRunChildProbe,
        'runChild', 0));

    FNestedChildSource := CHILD_JOB;
    Engine.Execute;
    Expect<string>(FNestedLog.CommaText).ToBe(
      '"child job",returned,"outer job"');

    // A child that fails discards its own leftover job, not the outer one.
    FNestedLog.Clear;
    FNestedChildSource := CHILD_JOB + 'throw new Error("child failed");';
    Engine.Execute;
    Expect<string>(FNestedLog.CommaText).ToBe(
      '"child failed",returned,"outer job"');

    // A reaction belongs to the engine that registered it, whoever created
    // the promise: the child's callback on a promise the outer engine handed
    // over runs in the child, not after the child is gone.
    FNestedLog.Clear;
    Source.Text := SHARING_OUTER_SOURCE;
    FNestedChildSource := SHARING_CHILD_SOURCE;
    Engine.Execute;
    Expect<string>(FNestedLog.CommaText).ToBe('"child awaited 2",returned');
  finally
    Engine.Free;
    FreeAndNil(FNestedLog);
    Source.Free;
  end;
end;

procedure TTestEngineRealm.TestInterpreterNestedExecuteLeavesOuterJobs;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertNestedExecuteLeavesOuterJobsWithExecutor(Executor, False);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeNestedExecuteLeavesOuterJobs;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertNestedExecuteLeavesOuterJobsWithExecutor(Executor, True);
  finally
    Executor.Free;
  end;
end;

{ What a host that attributes rejections itself does from inside the run:
  takes the oldest promise left rejected and reports its reason. }
function TTestEngineRealm.TakeRejectionProbe(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if TGocciaMicrotaskQueue.Instance.TakeUnhandledRejection(Promise) then
    Result := (TGocciaPromiseValue(Promise).PromiseResult as
      TGocciaObjectValue).GetProperty('message');
end;

{ Runs ASource and returns the message of the error the run raised for a
  promise left rejected, or '' when the run returned normally. }
function TTestEngineRealm.UnhandledRejectionMessage(
  const AExecutor: TGocciaExecutor; const ASource: string;
  const AMode: TGocciaUnhandledRejectionMode): string;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Result := '';
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<unhandled-rejection>', Source, AExecutor);
    Engine.UnhandledRejections := AMode;
    Engine.InjectGlobal('takeRejection',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TakeRejectionProbe,
        'takeRejection', 0));
    try
      Engine.Execute;
    except
      on E: TGocciaThrowValue do
        Result := (E.Value as TGocciaObjectValue).GetProperty('message')
          .ToStringLiteral.Value;
    end;
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.AssertUnhandledRejectionModesWithExecutor(
  const AExecutor: TGocciaExecutor);
const
  LEFT_UNHANDLED = 'Promise.reject(new Error("left unhandled"));';
  ASYNC_THROW =
    'const fail = async () => { throw new Error("async throw"); };' +
    'fail();';
  HANDLED_LATER =
    'const rejected = Promise.reject(new Error("handled later"));' +
    'Promise.resolve().then(() => rejected.catch(() => {}));';
  FIRST_OF_TWO =
    'Promise.reject(new Error("first"));' +
    'Promise.reject(new Error("second"));';
  TAKEN_BY_THE_HOST =
    'Promise.reject(new Error("taken"));' +
    'if (takeRejection() !== "taken") throw new Error("not handed over");';
begin
  Expect<string>(UnhandledRejectionMessage(AExecutor, LEFT_UNHANDLED,
    urThrow)).ToBe('left unhandled');
  Expect<string>(UnhandledRejectionMessage(AExecutor, ASYNC_THROW,
    urThrow)).ToBe('async throw');
  Expect<string>(UnhandledRejectionMessage(AExecutor, FIRST_OF_TWO,
    urThrow)).ToBe('first');
  // A handler that arrives before the run has nothing left to do counts.
  Expect<string>(UnhandledRejectionMessage(AExecutor, HANDLED_LATER,
    urThrow)).ToBe('');
  Expect<string>(UnhandledRejectionMessage(AExecutor, LEFT_UNHANDLED,
    urIgnore)).ToBe('');
  // A promise the host took during the run is the host's to report: the
  // engine raises nothing for it under either setting.
  Expect<string>(UnhandledRejectionMessage(AExecutor, TAKEN_BY_THE_HOST,
    urIgnore)).ToBe('');
  Expect<string>(UnhandledRejectionMessage(AExecutor, TAKEN_BY_THE_HOST,
    urThrow)).ToBe('');
  // One run's rejection is not the next run's.
  Expect<string>(UnhandledRejectionMessage(AExecutor, '1 + 1;',
    urThrow)).ToBe('');
end;

procedure TTestEngineRealm.TestInterpreterExecuteRaisesUnhandledRejection;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertUnhandledRejectionModesWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeExecuteRaisesUnhandledRejection;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertUnhandledRejectionModesWithExecutor(Executor);
  finally
    Executor.Free;
  end;
end;

function RejectionMessage(const AReason: TGocciaValue): string;
begin
  Result := (AReason as TGocciaObjectValue).GetProperty('message')
    .ToStringLiteral.Value;
end;

procedure TTestEngineRealm.RecordRejection(const APromise: TGocciaValue;
  const AReason: TGocciaValue);
var
  Tracked: TGocciaValue;
begin
  FRejectionLog.Add(RejectionMessage(AReason));
  if FRejectionHookRaises then
    raise Exception.Create('hook failed');
  if FRejectionHookHandles or
     (FRejectionHookHandlesFirst and (FRejectionLog.Count = 1)) then
    TGocciaPromiseValue(APromise).MarkHandled;
  if FRejectionHookHandlesAll then
    for Tracked in TGocciaMicrotaskQueue.Instance.UnhandledRejectionsInOrder do
      TGocciaPromiseValue(Tracked).MarkHandled;
  // A host that runs script on the engine from the hook, and collects.
  if Assigned(FRejectionHookProgram) then
  begin
    FRejectionHookEngine.ExecuteProgram(FRejectionHookProgram);
    TGarbageCollector.Instance.Collect;
  end;
end;

function TTestEngineRealm.CreateProgram(const ASource: string): TGocciaProgram;
var
  Lexer: TGocciaLexer;
  Lines: TStringList;
  Parser: TGocciaParser;
begin
  Lexer := TGocciaLexer.Create(ASource, '<rejection-hook-program>');
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    Parser := TGocciaParser.CreateFromLexer(Lexer, '<rejection-hook-program>',
      Lines);
    try
      Result := Parser.Parse;
    finally
      Parser.Free;
    end;
  finally
    Lines.Free;
    Lexer.Free;
  end;
end;

procedure TTestEngineRealm.RecordChildRejection(const APromise: TGocciaValue;
  const AReason: TGocciaValue);
begin
  FChildRejectionLog.Add(RejectionMessage(AReason));
end;

{ Runs ASource with the hook installed. Returns what the hook was handed, in
  order; ARaised is the message of the error the run raised, or ''. }
function TTestEngineRealm.HookedRejections(const AExecutor: TGocciaExecutor;
  const ASource: string; const AMode: TGocciaUnhandledRejectionMode;
  out ARaised: string): string;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  ARaised := '';
  FRejectionLog.Clear;
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create('<rejection-hook>', Source, AExecutor);
    Engine.UnhandledRejections := AMode;
    Engine.OnUnhandledRejection := RecordRejection;
    Engine.InjectGlobal('runChild',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(NestedRunChildProbe,
        'runChild', 0));
    try
      Engine.Execute;
    except
      on E: TGocciaThrowValue do
        ARaised := RejectionMessage(E.Value);
      // What a bytecode run raises for a script's own throw.
      on E: EGocciaBytecodeThrow do
        ARaised := RejectionMessage(E.ThrownValue);
      // What the hook itself raised.
      on E: Exception do
        ARaised := E.Message;
    end;
    Result := FRejectionLog.CommaText;
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.AssertUnhandledRejectionHookWithExecutor(
  const AExecutor: TGocciaExecutor; const AIsBytecode: Boolean);
const
  TWO_LEFT =
    'Promise.reject(new Error("first"));' +
    'Promise.reject(new Error("second"));';
  HANDLED_LATER =
    'const rejected = Promise.reject(new Error("handled later"));' +
    'Promise.resolve().then(() => rejected.catch(() => {}));';
  LEFT_THEN_THROWN =
    'Promise.reject(new Error("left"));' +
    'throw new Error("thrown");';
  OUTER_AND_CHILD =
    'Promise.reject(new Error("outer"));' +
    'runChild();';
var
  Engine: TGocciaEngine;
  ProgramNode: TGocciaProgram;
  Promise: TGocciaValue;
  Raised: string;
  Source: TStringList;
begin
  FRejectionLog := TStringList.Create;
  FChildRejectionLog := nil;
  FNestedLog := TStringList.Create;
  FNestedChildIsBytecode := AIsBytecode;
  FRejectionHookHandles := False;
  FRejectionHookHandlesAll := False;
  FRejectionHookHandlesFirst := False;
  FRejectionHookRaises := False;
  FRejectionHookEngine := nil;
  FRejectionHookProgram := nil;
  try
    // Every rejection is reported, oldest first, and the mode still decides.
    Expect<string>(HookedRejections(AExecutor, TWO_LEFT, urIgnore, Raised))
      .ToBe('first,second');
    Expect<string>(Raised).ToBe('');
    Expect<string>(HookedRejections(AExecutor, TWO_LEFT, urThrow, Raised))
      .ToBe('first,second');
    Expect<string>(Raised).ToBe('first');

    // A promise the hook gives a handler is handled.
    FRejectionHookHandles := True;
    Expect<string>(HookedRejections(AExecutor, TWO_LEFT, urThrow, Raised))
      .ToBe('first,second');
    Expect<string>(Raised).ToBe('');
    FRejectionHookHandles := False;

    // That includes one handled while an earlier promise was being reported:
    // it is not reported after all.
    FRejectionHookHandlesAll := True;
    Expect<string>(HookedRejections(AExecutor, TWO_LEFT, urThrow, Raised))
      .ToBe('first');
    Expect<string>(Raised).ToBe('');
    FRejectionHookHandlesAll := False;

    // The oldest one the hook left unhandled is the one that fails the run.
    FRejectionHookHandlesFirst := True;
    Expect<string>(HookedRejections(AExecutor, TWO_LEFT, urThrow, Raised))
      .ToBe('first,second');
    Expect<string>(Raised).ToBe('second');
    FRejectionHookHandlesFirst := False;

    // An exception the hook raises ends the run.
    FRejectionHookRaises := True;
    Expect<string>(HookedRejections(AExecutor, TWO_LEFT, urIgnore, Raised))
      .ToBe('first');
    Expect<string>(Raised).ToBe('hook failed');
    FRejectionHookRaises := False;

    // Nothing is reported for a promise that got a handler in time, or for a
    // run that ends by exception.
    Expect<string>(HookedRejections(AExecutor, HANDLED_LATER, urThrow, Raised))
      .ToBe('');
    Expect<string>(Raised).ToBe('');
    Expect<string>(HookedRejections(AExecutor, LEFT_THEN_THROWN, urThrow,
      Raised)).ToBe('');
    Expect<string>(Raised).ToBe('thrown');

    // A nested engine's rejections go to its own hook.
    FChildRejectionLog := TStringList.Create;
    FNestedChildSource := 'Promise.reject(new Error("inner"));';
    Expect<string>(HookedRejections(AExecutor, OUTER_AND_CHILD, urIgnore,
      Raised)).ToBe('outer');
    Expect<string>(FChildRejectionLog.CommaText).ToBe('inner');
    FreeAndNil(FChildRejectionLog);

    // ExecuteProgram clears nothing, so a reported rejection has to be
    // forgotten or the engine's next idle point would report it again.
    FRejectionLog.Clear;
    Source := TStringList.Create;
    Engine := TGocciaEngine.Create('<rejection-hook-program>', Source,
      AExecutor);
    try
      Engine.UnhandledRejections := urIgnore;
      Engine.OnUnhandledRejection := RecordRejection;
      ProgramNode := CreateProgram(TWO_LEFT);
      try
        Engine.ExecuteProgram(ProgramNode);
      finally
        ProgramNode.Free;
      end;
      Expect<string>(FRejectionLog.CommaText).ToBe('first,second');
      Expect<Boolean>(TGocciaMicrotaskQueue.Instance.TakeUnhandledRejection(
        Promise)).ToBe(False);

      // A hook that runs script on the engine and collects still gets each
      // rejection once: the run it starts reports nothing itself.
      FRejectionLog.Clear;
      FRejectionHookEngine := Engine;
      FRejectionHookProgram := CreateProgram('1 + 1;');
      ProgramNode := CreateProgram(TWO_LEFT);
      try
        Engine.ExecuteProgram(ProgramNode);
      finally
        ProgramNode.Free;
        FreeAndNil(FRejectionHookProgram);
        FRejectionHookEngine := nil;
      end;
      Expect<string>(FRejectionLog.CommaText).ToBe('first,second');
    finally
      Engine.Free;
      Source.Free;
    end;
  finally
    FreeAndNil(FChildRejectionLog);
    FreeAndNil(FNestedLog);
    FreeAndNil(FRejectionLog);
  end;
end;

procedure TTestEngineRealm.TestInterpreterUnhandledRejectionHook;
var
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    AssertUnhandledRejectionHookWithExecutor(Executor, False);
  finally
    Executor.Free;
  end;
end;

procedure TTestEngineRealm.TestBytecodeUnhandledRejectionHook;
var
  Executor: TGocciaBytecodeExecutor;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  try
    AssertUnhandledRejectionHookWithExecutor(Executor, True);
  finally
    Executor.Free;
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

{ The VM interns a function's source path once and reuses the reference for
  later calls. Each call, each kind of call and each engine must still see the
  path of the file its function was compiled from. }
procedure TTestEngineRealm.TestBytecodeFunctionExecutionContextCarriesSourcePath;
const
  PROBE_SOURCE =
    'const direct = () => sourcePathProbe();' +
    'const nested = (a, b, c, d) => [a, b, c, d].map(() => direct()).join(",");' +
    'class Holder { constructor() { this.path = sourcePathProbe(); } ' +
    '  method() { return direct(); } }' +
    '[direct(), direct(), nested(1, 2, 3, 4), new Holder().path, ' +
    ' new Holder().method(), direct.call(null), direct.apply(null, [])]' +
    '.join(",");';
  FIRST_FILE = '<source-path-first>';
  SECOND_FILE = '<source-path-second>';

  function Repeated(const APath: string): string;
  var
    I: Integer;
  begin
    Result := APath;
    for I := 2 to 10 do
      Result := Result + ',' + APath;
  end;

begin
  Expect<string>(RunSourcePathProbe(FIRST_FILE, PROBE_SOURCE))
    .ToBe(Repeated(FIRST_FILE));
  Expect<string>(RunSourcePathProbe(SECOND_FILE, PROBE_SOURCE))
    .ToBe(Repeated(SECOND_FILE));
  Expect<string>(RunSourcePathProbe(FIRST_FILE, PROBE_SOURCE))
    .ToBe(Repeated(FIRST_FILE));
end;

function TTestEngineRealm.RecordSourcePathProbe(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedSourcePaths.Add(RunningExecutionContext.SourcePath);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ One engine, three source files, calls alternating between them. The VM
  reuses the reference it interned for the previous call's path while the
  path is the same string; a reference that is not refreshed when the callee
  comes from another file would report the earlier file here. }
procedure TTestEngineRealm.TestBytecodeFunctionExecutionContextFollowsCalleeModule;
const
  MAIN_FILE = 'source-path-main.mjs';
  FIRST_MODULE = 'source-path-first';
  SECOND_MODULE = 'source-path-second';
  MAIN_SOURCE =
    'import { fromFirst, viaFirst } from "' + FIRST_MODULE + '";' +
    'import { fromSecond } from "' + SECOND_MODULE + '";' +
    'const own = () => recordSourcePath();' +
    'fromFirst(); fromSecond(); own();' +
    'fromFirst(); own(); fromSecond();' +
    'fromSecond(); fromFirst();' +
    'viaFirst(own); viaFirst(fromSecond);';
  // The module each recorded call's function was compiled from: main (M),
  // first (F) or second (S). viaFirst calls its argument from the first
  // module, so the probe runs in the argument's own module.
  EXPECTED_ORDER = 'FSMFMSSFMS';
var
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  Source: TStringList;
  MainPath, FirstPath, SecondPath, Expected: string;
  I: Integer;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  Source := TStringList.Create;
  Source.Text := MAIN_SOURCE;
  FRecordedSourcePaths := TStringList.Create;
  Engine := nil;
  try
    Engine := TGocciaEngine.Create(MAIN_FILE, Source, Executor);
    Engine.InjectGlobal('recordSourcePath',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RecordSourcePathProbe,
        'recordSourcePath', 0));
    Engine.InjectModule(FIRST_MODULE,
      'export const fromFirst = () => recordSourcePath();' +
      'export const viaFirst = (callback) => callback();');
    Engine.InjectModule(SECOND_MODULE,
      'export const fromSecond = () => recordSourcePath();');
    Engine.Execute;

    Expect<Integer>(FRecordedSourcePaths.Count).ToBe(Length(EXPECTED_ORDER));
    if FRecordedSourcePaths.Count = Length(EXPECTED_ORDER) then
    begin
      FirstPath := FRecordedSourcePaths[0];
      SecondPath := FRecordedSourcePaths[1];
      MainPath := FRecordedSourcePaths[2];
      Expect<Boolean>(Pos(FIRST_MODULE, FirstPath) > 0).ToBe(True);
      Expect<Boolean>(Pos(SECOND_MODULE, SecondPath) > 0).ToBe(True);
      Expect<Boolean>(Pos(MAIN_FILE, MainPath) > 0).ToBe(True);
      for I := 0 to FRecordedSourcePaths.Count - 1 do
      begin
        case EXPECTED_ORDER[I + 1] of
          'F': Expected := FirstPath;
          'S': Expected := SecondPath;
        else
          Expected := MainPath;
        end;
        Expect<string>(FRecordedSourcePaths[I]).ToBe(Expected);
      end;
    end;
  finally
    Engine.Free;
    FreeAndNil(FRecordedSourcePaths);
    Source.Free;
    Executor.Free;
  end;
end;

{ A VM resolves the thread's call stack when native code enters it while it
  is running nothing, and must do so again on every such entry: between two
  entries the thread's call stack can be a different object. The second run
  below happens after the thread's call stack has been replaced, with a decoy
  holding the old one's memory, and must record its frames on the new one. }
procedure TTestEngineRealm.TestBytecodeEntryRebindsTheThreadCallStack;
const
  STACK_SOURCE =
    'const inner = (tag) => new Error(tag).stack;' +
    'const middle = (tag) => { const trace = inner(tag); return trace; };' +
    'const outer = (tag) => { const trace = middle(tag); return trace; };' +
    'outer("probe");';
var
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  Source: TStringList;
  Decoy: TGocciaCallStack;
  FirstTrace, SecondTrace: string;
begin
  Executor := TGocciaBytecodeExecutor.Create;
  Source := TStringList.Create;
  Source.Text := STACK_SOURCE;
  Engine := nil;
  Decoy := nil;
  try
    Engine := TGocciaEngine.Create('<call-stack-rebind>', Source, Executor);
    FirstTrace := (Engine.Execute.Result as TGocciaStringLiteralValue).Value;
    Expect<Boolean>(Pos('at inner', FirstTrace) > 0).ToBe(True);
    Expect<Boolean>(Pos('at middle', FirstTrace) > 0).ToBe(True);
    Expect<Boolean>(Pos('at outer', FirstTrace) > 0).ToBe(True);

    TGocciaCallStack.Shutdown;
    Decoy := TGocciaCallStack.Create;
    TGocciaCallStack.Initialize;
    Expect<Boolean>(TGocciaCallStack.Instance <> Decoy).ToBe(True);

    SecondTrace := (Engine.Execute.Result as TGocciaStringLiteralValue).Value;
    Expect<string>(SecondTrace).ToBe(FirstTrace);
    Expect<Integer>(Decoy.Count).ToBe(0);
    Expect<Integer>(TGocciaCallStack.Instance.Count).ToBe(0);
  finally
    Engine.Free;
    Decoy.Free;
    Source.Free;
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
