program Goccia.Engine.StrictTypes.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Scope,
  Goccia.TestSetup,
  Goccia.Values.Primitives;

type
  TEngineStrictTypesTests = class(TTestSuite)
  private
    function CreateEmptySource: TStringList;
    procedure TestDefaultIsFalse;
    procedure TestBytecodeExecutorDefaultIsFalse;
    procedure TestSetterPropagatesToInterpreterScope;
    procedure TestSetterPropagatesToBytecodeExecutor;
    procedure TestEffectiveStrictTypesReadsLiveRoot;
  public
    procedure SetupTests; override;
  end;

procedure TEngineStrictTypesTests.SetupTests;
begin
  Test('Engine.StrictTypes defaults to False (interpreter)',
    TestDefaultIsFalse);
  Test('Engine.StrictTypes defaults to False (bytecode executor)',
    TestBytecodeExecutorDefaultIsFalse);
  Test('Setting Engine.StrictTypes propagates to interpreter global scope',
    TestSetterPropagatesToInterpreterScope);
  Test('Setting Engine.StrictTypes propagates to bytecode executor',
    TestSetterPropagatesToBytecodeExecutor);
  Test('EffectiveStrictTypes reads live root after setter (regression)',
    TestEffectiveStrictTypesReadsLiveRoot);
end;

function TEngineStrictTypesTests.CreateEmptySource: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := '';
end;

procedure TEngineStrictTypesTests.TestDefaultIsFalse;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<strict-test>', Source, []);
  try
    Expect<Boolean>(Engine.StrictTypes).ToBe(False);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TEngineStrictTypesTests.TestBytecodeExecutorDefaultIsFalse;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := TGocciaEngine.Create('<strict-test>', Source, [], Executor);
  try
    { Both execution modes default to non-strict.  Strict-types is now
      an opt-in flag controlled by the --strict-types CLI option or
      "strict-types" config key. }
    Expect<Boolean>(Engine.StrictTypes).ToBe(False);
    Expect<Boolean>(Executor.StrictTypes).ToBe(False);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineStrictTypesTests.TestSetterPropagatesToInterpreterScope;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<strict-test>', Source, []);
  try
    Engine.StrictTypes := True;
    Expect<Boolean>(Engine.StrictTypes).ToBe(True);
    Expect<Boolean>(Engine.Interpreter.StrictTypesEnabled).ToBe(True);
    Expect<Boolean>(Engine.Interpreter.GlobalScope.StrictTypes).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TEngineStrictTypesTests.TestSetterPropagatesToBytecodeExecutor;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := TGocciaEngine.Create('<strict-test>', Source, [], Executor);
  try
    Engine.StrictTypes := True;
    Expect<Boolean>(Engine.StrictTypes).ToBe(True);
    Expect<Boolean>(Executor.StrictTypes).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineStrictTypesTests.TestEffectiveStrictTypesReadsLiveRoot;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Child: TGocciaScope;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<strict-test>', Source, []);
  try
    { Simulate a closure whose lexical scope was created before the
      setter ran: a child of the global scope captures the
      then-current StrictTypes (False) into its own FStrictTypes. }
    Child := Engine.Interpreter.GlobalScope.CreateChild(skBlock,
      'embedder-block');
    Expect<Boolean>(Child.StrictTypes).ToBe(False);

    { Flipping Engine.StrictTypes only updates the global scope's own
      FStrictTypes; the child's snapshot stays stale. }
    Engine.StrictTypes := True;
    Expect<Boolean>(Child.StrictTypes).ToBe(False);

    { EffectiveStrictTypes walks to the root, so closures
      created before the setter still observe the live value -- this
      is what TGocciaFunctionValue.ExecuteBody and its generator
      counterparts read when seeding the call context. }
    Expect<Boolean>(Child.EffectiveStrictTypes).ToBe(True);

    { Flipping it back also flows through. }
    Engine.StrictTypes := False;
    Expect<Boolean>(Child.EffectiveStrictTypes).ToBe(False);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TEngineStrictTypesTests.Create(
    'Engine StrictTypes'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
