program Goccia.Engine.StrictTypes.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.TestSetup,
  Goccia.Values.Primitives;

type
  TEngineStrictTypesTests = class(TTestSuite)
  private
    function CreateEmptySource: TStringList;
    procedure TestDefaultExecutorStrictTypesFalse;
    procedure TestBytecodeExecutorStrictTypesTrue;
  public
    procedure SetupTests; override;
  end;

procedure TEngineStrictTypesTests.SetupTests;
begin
  Test('Default (interpreter) executor sets StrictTypes to False',
    TestDefaultExecutorStrictTypesFalse);
  Test('Bytecode executor sets StrictTypes to True',
    TestBytecodeExecutorStrictTypesTrue);
end;

function TEngineStrictTypesTests.CreateEmptySource: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := '';
end;

procedure TEngineStrictTypesTests.TestDefaultExecutorStrictTypesFalse;
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

procedure TEngineStrictTypesTests.TestBytecodeExecutorStrictTypesTrue;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := TGocciaEngine.Create('<strict-test>', Source, [], Executor);
  try
    Expect<Boolean>(Engine.StrictTypes).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TEngineStrictTypesTests.Create(
    'Engine StrictTypes'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
