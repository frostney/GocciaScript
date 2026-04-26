program Goccia.Engine.Realm.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.Realm,
  Goccia.Values.Primitives,
  Goccia.TestSetup;

type
  TTestEngineRealm = class(TTestSuite)
  private
    function RunInline(const ASource: string): TGocciaScriptResult;
  public
    procedure SetupTests; override;

    procedure TestCurrentRealmIsAssignedDuringLife;
    procedure TestCurrentRealmIsClearedAfterDestroy;
    procedure TestSequentialEnginesHaveIsolatedArrayPrototype;
    procedure TestSequentialEnginesHaveIsolatedObjectPrototype;
    procedure TestSequentialEnginesHaveIsolatedStringPrototype;
    procedure TestSequentialEnginesHaveFreshURLSearchParamsPrototype;
    procedure TestNestedEngineRestoresOuterRealmOnDestroy;
    procedure TestEachEngineGetsADistinctRealm;
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
  Test('URLSearchParams.prototype is fresh for each engine',
    TestSequentialEnginesHaveFreshURLSearchParamsPrototype);
  Test('Destroying a nested engine restores the outer engine''s realm',
    TestNestedEngineRestoresOuterRealmOnDestroy);
  Test('Each engine owns a distinct realm instance',
    TestEachEngineGetsADistinctRealm);
end;

function TTestEngineRealm.RunInline(const ASource: string): TGocciaScriptResult;
begin
  Result := TGocciaEngine.RunScript(ASource, '<engine-realm-test>');
end;

procedure TTestEngineRealm.TestCurrentRealmIsAssignedDuringLife;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := '';
  Engine := TGocciaEngine.Create('<realm-life>', Source, []);
  try
    Expect<Boolean>(CurrentRealm <> nil).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestEngineRealm.TestCurrentRealmIsClearedAfterDestroy;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  PreviousRealm: TGocciaRealm;
begin
  PreviousRealm := CurrentRealm;
  Source := TStringList.Create;
  Source.Text := '';
  Engine := TGocciaEngine.Create('<realm-clear>', Source, []);
  Engine.Free;
  Source.Free;
  // FPrevRealm defaults to whatever was current at construction; for a single
  // engine on a clean main thread that's PreviousRealm.
  Expect<Boolean>(CurrentRealm = PreviousRealm).ToBe(True);
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

procedure TTestEngineRealm.TestSequentialEnginesHaveFreshURLSearchParamsPrototype;
var
  ResultA, ResultB: TGocciaScriptResult;
begin
  // URLSearchParams.prototype is built lazily through TGocciaSharedPrototype
  // and rebuilt per realm; mutations on engine A must not leak.
  ResultA := RunInline(
    'URLSearchParams.prototype.__poisonUSP = 7;' +
    'URLSearchParams.prototype.__poisonUSP;');
  Expect<Double>((ResultA.Result as TGocciaNumberLiteralValue).Value).ToBe(7);

  ResultB := RunInline('typeof URLSearchParams.prototype.__poisonUSP;');
  Expect<string>((ResultB.Result as TGocciaStringLiteralValue).Value).ToBe(
    'undefined');
end;

procedure TTestEngineRealm.TestNestedEngineRestoresOuterRealmOnDestroy;
var
  OuterEngine, InnerEngine: TGocciaEngine;
  OuterSource, InnerSource: TStringList;
  OuterRealm, InnerRealm: TGocciaRealm;
begin
  OuterSource := TStringList.Create;
  OuterSource.Text := '';
  InnerSource := TStringList.Create;
  InnerSource.Text := '';

  OuterEngine := TGocciaEngine.Create('<outer>', OuterSource, []);
  try
    OuterRealm := CurrentRealm;
    Expect<Boolean>(OuterRealm <> nil).ToBe(True);

    InnerEngine := TGocciaEngine.Create('<inner>', InnerSource, []);
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
end;

procedure TTestEngineRealm.TestEachEngineGetsADistinctRealm;
var
  EngineA, EngineB: TGocciaEngine;
  SourceA, SourceB: TStringList;
  RealmA, RealmB: TGocciaRealm;
begin
  SourceA := TStringList.Create;
  SourceA.Text := '';
  EngineA := TGocciaEngine.Create('<engine-a>', SourceA, []);
  RealmA := CurrentRealm;
  EngineA.Free;
  SourceA.Free;

  SourceB := TStringList.Create;
  SourceB.Text := '';
  EngineB := TGocciaEngine.Create('<engine-b>', SourceB, []);
  try
    RealmB := CurrentRealm;
    // The two engines must own different realm instances; the dangling
    // RealmA pointer should not be reused.
    Expect<Boolean>(RealmB <> RealmA).ToBe(True);
    Expect<Boolean>(RealmB <> nil).ToBe(True);
  finally
    EngineB.Free;
    SourceB.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestEngineRealm.Create('Engine Realm'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
