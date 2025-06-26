program Goccia.Values.ObjectValue.Test;

{$I Goccia.inc}

uses
  Goccia.Values.Core, Goccia.Values.ObjectValue, Goccia.Values.StringValue, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, StrUtils, Math, TestRunner;

type
  TTestObjectValue = class(TTestSuite)
  private
    function SimpleObject: TGocciaObjectValue;
    function FullName(const AObject: TGocciaObjectValue): TGocciaValue;
  public
    procedure SetupTests; override;

    procedure TestCasting;
    procedure TestGetProperties;
    procedure TestHasProperties;
    procedure TestModifyProperties;
    procedure TestDeleteProperties;
    procedure TestPrototype;
    procedure TestPrototypeChain;
    procedure TestComputedProperties;
  end;


function TTestObjectValue.SimpleObject: TGocciaObjectValue;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := TGocciaObjectValue.Create;
  ObjectValue.SetProperty('name', TGocciaStringLiteralValue.Create('John'));
  ObjectValue.SetProperty('age', TGocciaNumberLiteralValue.Create(30));
  ObjectValue.SetProperty('isStudent', TGocciaBooleanLiteralValue.Create(True));
  ObjectValue.SetProperty('address', TGocciaStringLiteralValue.Create('123 Main St'));
  ObjectValue.SetProperty('city', TGocciaStringLiteralValue.Create('Anytown'));
  ObjectValue.SetProperty('state', TGocciaStringLiteralValue.Create('CA'));
  ObjectValue.SetProperty('zip', TGocciaStringLiteralValue.Create('12345'));

  Result := ObjectValue;
end;

function TTestObjectValue.FullName(const AObject: TGocciaObjectValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AObject.GetProperty('name').ToString + ' ' + AObject.GetProperty('lastName').ToString);
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
  Test('Computed Properties', TestComputedProperties);
end;

procedure TTestObjectValue.TestCasting;
var
  ObjectValue: TGocciaObjectValue;
  DebugString: string;
begin
  ObjectValue := SimpleObject;
  DebugString := ObjectValue.ToDebugString;

  Expect<Boolean>(ContainsText(DebugString, 'name: John')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, 'age: 30')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, 'isStudent: true')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, 'address: 123 Main St')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, 'city: Anytown')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, 'state: CA')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, 'zip: 12345')).ToBe(True);
  Expect<string>(ObjectValue.ToString).ToBe('[object Object]');
  Expect<Boolean>(ObjectValue.ToBoolean).ToBe(True);
  Expect<Boolean>(IsNaN(ObjectValue.ToNumber)).ToBe(True);
  Expect<string>(ObjectValue.TypeName).ToBe('object');
  ObjectValue.Free;
end;

procedure TTestObjectValue.TestGetProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('John');
  Expect<Double>(ObjectValue.GetProperty('age').ToNumber).ToBe(30);
  Expect<Boolean>(ObjectValue.GetProperty('isStudent').ToBoolean).ToBe(True);
  Expect<string>(ObjectValue.GetProperty('address').ToString).ToBe('123 Main St');
  Expect<string>(ObjectValue.GetProperty('city').ToString).ToBe('Anytown');
  Expect<string>(ObjectValue.GetProperty('state').ToString).ToBe('CA');
  Expect<string>(ObjectValue.GetProperty('zip').ToString).ToBe('12345');

  ObjectValue.Free;
end;

procedure TTestObjectValue.TestHasProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  Expect<Boolean>(ObjectValue.HasProperty('name')).ToBe(True);
  Expect<Boolean>(ObjectValue.HasProperty('year')).ToBe(False);

  ObjectValue.Free;
end;

procedure TTestObjectValue.TestModifyProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  ObjectValue.SetProperty('name', TGocciaStringLiteralValue.Create('Jane'));
  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('Jane');

  ObjectValue.Free;
end;

procedure TTestObjectValue.TestDeleteProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  ObjectValue.DeleteProperty('name');
  Expect<Boolean>(ObjectValue.HasProperty('name')).ToBe(False);

  ObjectValue.Free;
end;

procedure TTestObjectValue.TestPrototype;
var
  ObjectValue: TGocciaObjectValue;
  Prototype: TGocciaObjectValue;
  DebugString: string;
begin
  ObjectValue := SimpleObject;
  Prototype := TGocciaObjectValue.Create;

  ObjectValue.Prototype := Prototype;
  Expect<Boolean>(ObjectValue.Prototype = Prototype).ToBe(True);

  Prototype.SetProperty('name', TGocciaStringLiteralValue.Create('Jane'));

  // Instance property > Prototype property
  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('John');

  DebugString := ObjectValue.ToDebugString;
  Expect<Boolean>(ContainsText(DebugString, 'name: John')).ToBe(True);
  Expect<Boolean>(ContainsText(DebugString, '[[Prototype]]: {name: Jane}')).ToBe(True);

  ObjectValue.DeleteProperty('name');

  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('Jane');
  Expect<Boolean>(ObjectValue.GetProperty('name') = ObjectValue.Prototype.GetProperty('name')).ToBe(True);

  ObjectValue.Free;
  Prototype.Free;
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

  ObjectValue.SetProperty('name', TGocciaStringLiteralValue.Create('Jane'));
  ObjectValue.Prototype.SetProperty('name', TGocciaStringLiteralValue.Create('John'));
  ObjectValue.Prototype.Prototype.SetProperty('name', TGocciaStringLiteralValue.Create('Joseph'));

  // Instance property > Prototype property > Other Prototype property
  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('Jane');

  WriteLn(ObjectValue.ToDebugString);

  ObjectValue.DeleteProperty('name');

  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('John');

  ObjectValue.Prototype.DeleteProperty('name');

  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('Joseph');

  ObjectValue.SetProperty('name', TGocciaStringLiteralValue.Create('Jane'));
  Expect<string>(ObjectValue.GetProperty('name').ToString).ToBe('Jane');

  ObjectValue.Free;
  Prototype.Free;
  OtherPrototype.Free;
end;

procedure TTestObjectValue.TestComputedProperties;
var
  ObjectValue: TGocciaObjectValue;
begin
  ObjectValue := SimpleObject;

  ObjectValue.SetComputedProperty('fullName', FullName);
  Expect<string>(ObjectValue.GetProperty('fullName').ToString).ToBe('John undefined');

  ObjectValue.SetProperty('lastName', TGocciaStringLiteralValue.Create('Doe'));
  Expect<string>(ObjectValue.GetProperty('fullName').ToString).ToBe('John Doe');

  ObjectValue.DeleteProperty('lastName');
  Expect<string>(ObjectValue.GetProperty('fullName').ToString).ToBe('John undefined');

  ObjectValue.Free;
end;

begin
  TestRunnerProgram.AddSuite(TTestObjectValue.Create('Object Value'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
