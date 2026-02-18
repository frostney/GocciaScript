program Goccia.Values.ObjectValue.Test;

{$I Goccia.inc}

uses
  StrUtils,

  TestRunner,

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
  Expect<string>(ObjectValue.ToStringLiteral.Value).ToBe('[object Object]');
  Expect<Boolean>(ObjectValue.ToBooleanLiteral.Value).ToBe(True);
  Expect<Boolean>(ObjectValue.ToNumberLiteral.IsNaN).ToBe(True);
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
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
