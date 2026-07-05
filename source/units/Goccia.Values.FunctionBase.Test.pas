program Goccia.Values.FunctionBase.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase;

type
  TTestFunctionBase = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestGetFunctionRealmReturnsClassCreationRealm;
  end;

procedure TTestFunctionBase.SetupTests;
begin
  Test('GetFunctionRealm returns a class creation realm',
    TestGetFunctionRealmReturnsClassCreationRealm);
end;

procedure TTestFunctionBase.TestGetFunctionRealmReturnsClassCreationRealm;
var
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  ClassValue: TGocciaClassValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('function-base-test');
  SetCurrentRealm(Realm);
  try
    ClassValue := TGocciaClassValue.Create('RealmClass', nil);
    Expect<TGocciaRealm>(GetFunctionRealm(ClassValue)).ToBe(Realm);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestFunctionBase.Create('Function Base'));
  TestRunnerProgram.Run;
end.
