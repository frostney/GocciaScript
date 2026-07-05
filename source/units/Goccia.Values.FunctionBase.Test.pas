program Goccia.Values.FunctionBase.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TTestFunctionBase = class(TTestSuite)
  private
    function NativeNoOp(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    procedure SetupTests; override;

    procedure TestGetFunctionRealmReturnsClassCreationRealm;
    procedure TestGetFunctionRealmReturnsFunctionCreationRealm;
    procedure TestGetFunctionRealmUsesRegisteredProxyHook;
    procedure TestGetFunctionRealmReturnsNilForOtherValues;
  end;

var
  GProxyRealmTestValue: TGocciaValue;
  GProxyRealmTestRealm: TGocciaRealm;

function TestProxyPredicate(const AValue: TGocciaValue): Boolean;
begin
  Result := AValue = GProxyRealmTestValue;
end;

function TestProxyApply(const AProxy: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TestProxyConstruct(const AProxy: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TestProxyGetPrototype(
  const AProxy: TGocciaObjectValue): TGocciaValue;
begin
  Result := nil;
end;

function TestProxyGetFunctionRealm(const AProxy: TGocciaValue): TGocciaRealm;
begin
  Result := GProxyRealmTestRealm;
end;

procedure TTestFunctionBase.SetupTests;
begin
  Test('GetFunctionRealm returns a class creation realm',
    TestGetFunctionRealmReturnsClassCreationRealm);
  Test('GetFunctionRealm returns a function creation realm',
    TestGetFunctionRealmReturnsFunctionCreationRealm);
  Test('GetFunctionRealm uses the registered proxy hook',
    TestGetFunctionRealmUsesRegisteredProxyHook);
  Test('GetFunctionRealm returns nil for other values',
    TestGetFunctionRealmReturnsNilForOtherValues);
end;

function TTestFunctionBase.NativeNoOp(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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

procedure TTestFunctionBase.TestGetFunctionRealmReturnsFunctionCreationRealm;
var
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  FunctionValue: TGocciaNativeFunctionValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('function-base-function-test');
  SetCurrentRealm(Realm);
  try
    FunctionValue := TGocciaNativeFunctionValue.Create(NativeNoOp,
      'realmFunction', 0);
    Expect<TGocciaRealm>(GetFunctionRealm(FunctionValue)).ToBe(Realm);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

procedure TTestFunctionBase.TestGetFunctionRealmUsesRegisteredProxyHook;
var
  Realm: TGocciaRealm;
  ProxyValue: TGocciaObjectValue;
begin
  Realm := TGocciaRealm.Create('function-base-proxy-test');
  ProxyValue := TGocciaObjectValue.Create(nil);
  GProxyRealmTestValue := ProxyValue;
  GProxyRealmTestRealm := Realm;
  RegisterProxyDispatchHooks(TestProxyPredicate, TestProxyApply,
    TestProxyConstruct, TestProxyGetPrototype, TestProxyGetFunctionRealm);
  try
    Expect<TGocciaRealm>(GetFunctionRealm(ProxyValue)).ToBe(Realm);
  finally
    RegisterProxyDispatchHooks(nil, nil, nil, nil, nil);
    GProxyRealmTestValue := nil;
    GProxyRealmTestRealm := nil;
    Realm.Free;
  end;
end;

procedure TTestFunctionBase.TestGetFunctionRealmReturnsNilForOtherValues;
var
  Value: TGocciaObjectValue;
begin
  RegisterProxyDispatchHooks(nil, nil, nil, nil, nil);
  Value := TGocciaObjectValue.Create(nil);
  Expect<TGocciaRealm>(GetFunctionRealm(Value)).ToBe(nil);
end;

begin
  TestRunnerProgram.AddSuite(TTestFunctionBase.Create('Function Base'));
  TestRunnerProgram.Run;
end.
