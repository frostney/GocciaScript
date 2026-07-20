program Goccia.Values.ObjectValue.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.Formatting,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TTestObjectValue = class(TTestSuite)
  private
    function SimpleObject: TGocciaObjectValue;
  public
    procedure SetupTests; override;

    procedure TestCasting;
    procedure TestGetProperties;
    procedure TestHasProperties;
    procedure TestModifyProperties;
    procedure TestDeleteProperties;
    procedure TestPrototype;
    procedure TestPrototypeChain;
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

procedure TTestObjectValue.SetupTests;
begin
  Test('Casting', TestCasting);
  Test('Has Properties', TestHasProperties);
  Test('Get Properties', TestGetProperties);
  Test('Modify Properties', TestModifyProperties);
  Test('Delete Properties', TestDeleteProperties);
  Test('Prototype', TestPrototype);
  Test('Prototype Chain', TestPrototypeChain);
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

begin
  TestRunnerProgram.AddSuite(TTestObjectValue.Create('Object Value'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
