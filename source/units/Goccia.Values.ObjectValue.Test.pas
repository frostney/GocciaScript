program Goccia.Values.ObjectValue.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.Formatting,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.ProxyValue;

type
  TTestObjectValue = class(TTestSuite)
  private
    FLazyFactoryCalls: Integer;
    FTrapSeenValue: string;

    function SimpleObject: TGocciaObjectValue;
    function LazyPropertyValue: TGocciaValue;
    function RecordDefinePropertyTrap(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    procedure SetupTests; override;

    procedure TestCasting;
    procedure TestGetProperties;
    procedure TestHasProperties;
    procedure TestModifyProperties;
    procedure TestDeleteProperties;
    procedure TestPrototype;
    procedure TestPrototypeChain;
    procedure TestProxyDefineTrapSeesMaterializedLazyValue;
  end;

function ContainsFragment(const AText, AFragment: string): Boolean;
begin
  Result := Pos(AFragment, AText) > 0;
end;

function TTestObjectValue.SimpleObject: TGocciaObjectValue;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := TGocciaObjectValue.Create;
  ObjectValue.AssignProperty('name', TGocciaStringLiteralValue.Create('John'));
  ObjectValue.AssignProperty('age', TGocciaNumberLiteralValue.Create(30));
  ObjectValue.AssignProperty('isStudent', TGocciaBooleanLiteralValue.Create(True));
  ObjectValue.AssignProperty('address', TGocciaStringLiteralValue.Create('123 Main St'));
  ObjectValue.AssignProperty('city', TGocciaStringLiteralValue.Create('Anytown'));
  ObjectValue.AssignProperty('state', TGocciaStringLiteralValue.Create('CA'));
  ObjectValue.AssignProperty('zip', TGocciaStringLiteralValue.Create('12345'));

  Result := ObjectValue;
end;

function TTestObjectValue.LazyPropertyValue: TGocciaValue;
begin
  Inc(FLazyFactoryCalls);
  Result := TGocciaStringLiteralValue.Create('materialized');
end;

function TTestObjectValue.RecordDefinePropertyTrap(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  DescriptorObject: TGocciaValue;
begin
  FTrapSeenValue := '<no descriptor>';
  if AArgs.Length > 2 then
  begin
    DescriptorObject := AArgs.GetElement(2);
    if DescriptorObject is TGocciaObjectValue then
      FTrapSeenValue := TGocciaObjectValue(DescriptorObject)
        .GetProperty(PROP_VALUE).ToStringLiteral.Value;
  end;
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

procedure TTestObjectValue.SetupTests;
begin
  Test('Casting', TestCasting);
  Test('Has Properties', TestHasProperties);
  Test('Get Properties', TestGetProperties);
  Test('Modify Properties', TestModifyProperties);
  Test('Delete Properties', TestDeleteProperties);
  Test('Prototype', TestPrototype);
  Test('Prototype Chain', TestPrototypeChain);
  Test('Proxy Define Trap Sees Materialized Lazy Value',
    TestProxyDefineTrapSeesMaterializedLazyValue);
end;

procedure TTestObjectValue.TestCasting;
var
  ObjectValue: TGocciaObjectValue;
  DebugString: string;
  ToStringThrew: Boolean;
  ThrownNameValue: TGocciaValue;
  ThrownTypeName: string;
begin
  ObjectValue := SimpleObject;
  DebugString := FormatForDisplay(ObjectValue);

  Expect<Boolean>(ContainsFragment(DebugString, 'name: ''John''')).ToBe(True);
  Expect<Boolean>(ContainsFragment(DebugString, 'age: 30')).ToBe(True);
  Expect<Boolean>(ContainsFragment(DebugString, 'isStudent: true')).ToBe(True);
  Expect<Boolean>(ContainsFragment(DebugString, 'address: ''123 Main St''')).ToBe(True);
  Expect<Boolean>(ContainsFragment(DebugString, 'city: ''Anytown''')).ToBe(True);
  Expect<Boolean>(ContainsFragment(DebugString, 'state: ''CA''')).ToBe(True);
  Expect<Boolean>(ContainsFragment(DebugString, 'zip: ''12345''')).ToBe(True);

  // ES2026 §7.1.17 ToString on an object without Object.prototype.toString
  // (no prototype assigned in this test fixture) throws TypeError because
  // neither toString() nor valueOf() can be located. This exercises the spec
  // path through ToPrimitive(string). Real engine objects always inherit
  // Object.prototype and therefore stringify successfully. Verify both that
  // the throw fires AND that the JS-level error name is "TypeError" — any
  // other thrown error class would be a real bug masking the spec one.
  ToStringThrew := False;
  ThrownTypeName := '';
  try
    ObjectValue.ToStringLiteral;
  except
    on E: TGocciaThrowValue do
    begin
      ToStringThrew := True;
      ThrownNameValue := nil;
      if E.Value is TGocciaObjectValue then
        ThrownNameValue := TGocciaObjectValue(E.Value).GetProperty(PROP_NAME);
      if ThrownNameValue is TGocciaStringLiteralValue then
        ThrownTypeName := TGocciaStringLiteralValue(ThrownNameValue).Value;
    end;
  end;
  Expect<Boolean>(ToStringThrew).ToBe(True);
  Expect<string>(ThrownTypeName).ToBe(TYPE_ERROR_NAME);

  Expect<Boolean>(ObjectValue.ToBooleanLiteral.Value).ToBe(True);

  // ES2026 §7.1.4 ToNumber on an object without Object.prototype also throws
  // TypeError — ToPrimitive finds no valueOf()/toString().
  ToStringThrew := False;
  ThrownTypeName := '';
  try
    ObjectValue.ToNumberLiteral;
  except
    on E: TGocciaThrowValue do
    begin
      ToStringThrew := True;
      ThrownNameValue := nil;
      if E.Value is TGocciaObjectValue then
        ThrownNameValue := TGocciaObjectValue(E.Value).GetProperty(PROP_NAME);
      if ThrownNameValue is TGocciaStringLiteralValue then
        ThrownTypeName := TGocciaStringLiteralValue(ThrownNameValue).Value;
    end;
  end;
  Expect<Boolean>(ToStringThrew).ToBe(True);
  Expect<string>(ThrownTypeName).ToBe(TYPE_ERROR_NAME);

  Expect<string>(ObjectValue.TypeName).ToBe('object');
end;

procedure TTestObjectValue.TestGetProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('John');
  Expect<Double>(ObjectValue.GetProperty('age').ToNumberLiteral.Value).ToBe(30);
  Expect<Boolean>(ObjectValue.GetProperty('isStudent').ToBooleanLiteral.Value).ToBe(True);
  Expect<string>(ObjectValue.GetProperty('address').ToStringLiteral.Value).ToBe('123 Main St');
  Expect<string>(ObjectValue.GetProperty('city').ToStringLiteral.Value).ToBe('Anytown');
  Expect<string>(ObjectValue.GetProperty('state').ToStringLiteral.Value).ToBe('CA');
  Expect<string>(ObjectValue.GetProperty('zip').ToStringLiteral.Value).ToBe('12345');
end;

procedure TTestObjectValue.TestHasProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  Expect<Boolean>(ObjectValue.HasProperty('name')).ToBe(True);
  Expect<Boolean>(ObjectValue.HasProperty('year')).ToBe(False);
end;

procedure TTestObjectValue.TestModifyProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  ObjectValue.AssignProperty('name', TGocciaStringLiteralValue.Create('Jane'));
  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('Jane');
end;

procedure TTestObjectValue.TestDeleteProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  ObjectValue.DeleteProperty('name');
  Expect<Boolean>(ObjectValue.HasProperty('name')).ToBe(False);
end;

procedure TTestObjectValue.TestPrototype;
var
  ObjectValue: TGocciaObjectValue;
  Prototype: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;
  Prototype := TGocciaObjectValue.Create;

  ObjectValue.Prototype := Prototype;
  Expect<Boolean>(ObjectValue.Prototype = Prototype).ToBe(True);

  Prototype.AssignProperty('name', TGocciaStringLiteralValue.Create('Jane'));

  // Instance property > Prototype property
  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('John');

  ObjectValue.DeleteProperty('name');

  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('Jane');
end;

procedure TTestObjectValue.TestPrototypeChain;
var
  ObjectValue: TGocciaObjectValue;
  Prototype: TGocciaObjectValue;
  OtherPrototype: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;
  Prototype := TGocciaObjectValue.Create;
  OtherPrototype := TGocciaObjectValue.Create;

  ObjectValue.Prototype := Prototype;
  ObjectValue.Prototype.Prototype := OtherPrototype;

  Expect<Boolean>(ObjectValue.Prototype = Prototype).ToBe(True);
  Expect<Boolean>(ObjectValue.Prototype.Prototype = OtherPrototype).ToBe(True);
  Expect<Boolean>(ObjectValue.Prototype.Prototype.Prototype = nil).ToBe(True);

  ObjectValue.AssignProperty('name', TGocciaStringLiteralValue.Create('Jane'));
  ObjectValue.Prototype.AssignProperty('name', TGocciaStringLiteralValue.Create('John'));
  ObjectValue.Prototype.Prototype.AssignProperty('name', TGocciaStringLiteralValue.Create('Joseph'));

  // Instance property > Prototype property > Other Prototype property
  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('Jane');

  ObjectValue.DeleteProperty('name');
  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('John');

  ObjectValue.Prototype.DeleteProperty('name');
  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('Joseph');

  ObjectValue.AssignProperty('name', TGocciaStringLiteralValue.Create('Jane'));
  Expect<string>(ObjectValue.GetProperty('name').ToStringLiteral.Value).ToBe('Jane');
end;

{ A lazy descriptor exists so a plain object can keep the factory and run it on
  first read. A proxy keeps nothing: its [[DefineOwnProperty]] hands the
  descriptor to the trap and frees it, so the value the trap sees is the only
  chance the factory's result ever gets. The define path therefore materializes
  before it roots — without that, the trap is handed `value: undefined` and the
  factory result is dropped on the floor. }
procedure TTestObjectValue.TestProxyDefineTrapSeesMaterializedLazyValue;
var
  Handler: TGocciaObjectValue;
  PreviousRealm: TGocciaRealm;
  Proxy: TGocciaProxyValue;
  Realm: TGocciaRealm;
  Target: TGocciaObjectValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('object-value-proxy-lazy-test');
  SetCurrentRealm(Realm);
  try
    FLazyFactoryCalls := 0;
    FTrapSeenValue := '<trap not run>';

    Target := TGocciaObjectValue.Create;
    Handler := TGocciaObjectValue.Create;
    Handler.AssignProperty(PROP_DEFINE_PROPERTY,
      TGocciaNativeFunctionValue.Create(RecordDefinePropertyTrap,
        PROP_DEFINE_PROPERTY, 3));
    Proxy := TGocciaProxyValue.Create(Target, Handler);

    Proxy.DefineProperty('lazy',
      TGocciaLazyPropertyDescriptorData.Create(LazyPropertyValue,
        [pfEnumerable, pfConfigurable, pfWritable]));

    Expect<string>(FTrapSeenValue).ToBe('materialized');
    Expect<Integer>(FLazyFactoryCalls).ToBe(1);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestObjectValue.Create('Object Value'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
