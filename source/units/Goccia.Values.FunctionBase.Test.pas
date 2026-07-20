program Goccia.Values.FunctionBase.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
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
    procedure TestGetFunctionRealmPropagatesRevokedProxyTypeError;
    procedure TestGetFunctionRealmReturnsNilForOtherValues;
  end;

var
  GProxyRealmTestValue: TGocciaValue;
  GProxyRealmTestRealm: TGocciaRealm;
  GProxyRealmTestRevoked: Boolean;

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
  if GProxyRealmTestRevoked then
    ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);

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
  Test('GetFunctionRealm propagates revoked proxy TypeError',
    TestGetFunctionRealmPropagatesRevokedProxyTypeError);
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
    GProxyRealmTestRevoked := False;
    Realm.Free;
  end;
end;

procedure TTestFunctionBase.TestGetFunctionRealmPropagatesRevokedProxyTypeError;
var
  ProxyValue: TGocciaObjectValue;
  RaisedExpected: Boolean;
  ThrownMessage: string;
  ThrownName: string;
  ThrownNameValue: TGocciaValue;
  ThrownMessageValue: TGocciaValue;
begin
  ProxyValue := TGocciaObjectValue.Create(nil);
  GProxyRealmTestValue := ProxyValue;
  GProxyRealmTestRevoked := True;
  RegisterProxyDispatchHooks(TestProxyPredicate, TestProxyApply,
    TestProxyConstruct, TestProxyGetPrototype, TestProxyGetFunctionRealm);

  RaisedExpected := False;
  ThrownName := '';
  ThrownMessage := '';
  try
    try
      GetFunctionRealm(ProxyValue);
    except
      on E: TGocciaThrowValue do
      begin
        RaisedExpected := True;
        ThrownNameValue := nil;
        ThrownMessageValue := nil;
        if E.Value is TGocciaObjectValue then
        begin
          ThrownNameValue := TGocciaObjectValue(E.Value).GetProperty(PROP_NAME);
          ThrownMessageValue := TGocciaObjectValue(E.Value).GetProperty(PROP_MESSAGE);
        end;
        if ThrownNameValue is TGocciaStringLiteralValue then
          ThrownName := TGocciaStringLiteralValue(ThrownNameValue).Value;
        if ThrownMessageValue is TGocciaStringLiteralValue then
          ThrownMessage := TGocciaStringLiteralValue(ThrownMessageValue).Value;
      end;
    end;

    Expect<Boolean>(RaisedExpected).ToBe(True);
    Expect<string>(ThrownName).ToBe(TYPE_ERROR_NAME);
    Expect<string>(ThrownMessage).ToBe(SErrorProxyRevoked);
  finally
    RegisterProxyDispatchHooks(nil, nil, nil, nil, nil);
    GProxyRealmTestValue := nil;
    GProxyRealmTestRealm := nil;
    GProxyRealmTestRevoked := False;
  end;
end;

procedure TTestFunctionBase.TestGetFunctionRealmReturnsNilForOtherValues;
var
  Value: TGocciaObjectValue;
begin
  RegisterProxyDispatchHooks(nil, nil, nil, nil, nil);
  GProxyRealmTestRevoked := False;
  Value := TGocciaObjectValue.Create(nil);
  Expect<TGocciaRealm>(GetFunctionRealm(Value)).ToBe(nil);
end;

begin
  TestRunnerProgram.AddSuite(TTestFunctionBase.Create('Function Base'));
  RunGocciaTests;
end.
