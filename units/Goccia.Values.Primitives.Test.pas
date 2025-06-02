program Goccia.Values.Primitives.Test;

{$I Goccia.inc}

uses
  Classes, SysUtils, Goccia.Values.UndefinedValue, Goccia.Values.StringValue, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, Goccia.Values.NullValue,
  TestRunner, Math;

type
  TTestPrimitives = class(TTestSuite)
  private
    procedure TestUndefinedValue;
    procedure TestStringValueContent;
    procedure TestStringValueEmpty;
    procedure TestStringValueNumber;
    procedure TestNumberValue;
    procedure TestBooleanValueTrue;
    procedure TestBooleanValueFalse;
    procedure TestNullValue;
  public
    procedure SetupTests; override;
  end;

procedure TTestPrimitives.SetupTests;
begin
  Test('Undefined value', TestUndefinedValue);
  Test('String value with content', TestStringValueContent);
  Test('String value with empty content', TestStringValueEmpty);
  Test('String value with number content', TestStringValueNumber);
  Test('Number value', TestNumberValue);
  Test('Boolean value with true value', TestBooleanValueTrue);
  Test('Boolean value with false value', TestBooleanValueFalse);
  Test('Null value', TestNullValue);
end;

procedure TTestPrimitives.TestUndefinedValue;
var
  UndefinedValue: TGocciaUndefinedValue;
begin
  UndefinedValue := TGocciaUndefinedValue.Create;
  Expect<string>(UndefinedValue.ToString).ToBe('undefined');
  Expect<Boolean>(UndefinedValue.ToBoolean).ToBe(False);
  Expect<Boolean>(IsNaN(UndefinedValue.ToNumber)).ToBe(True);
  Expect<string>(UndefinedValue.TypeName).ToBe('undefined');
  UndefinedValue.Free;
end;

procedure TTestPrimitives.TestStringValueContent;
var
  StringValue: TGocciaStringValue;
begin
  StringValue := TGocciaStringValue.Create('Hello, world!');
  Expect<string>(StringValue.ToString).ToBe('Hello, world!');
  Expect<Boolean>(StringValue.ToBoolean).ToBe(True);
  Expect<Boolean>(IsNaN(StringValue.ToNumber)).ToBe(True);
  Expect<string>(StringValue.TypeName).ToBe('string');
  StringValue.Free;
end;

procedure TTestPrimitives.TestStringValueEmpty;
var
  StringValue: TGocciaStringValue;
begin
  StringValue := TGocciaStringValue.Create('');
  Expect<string>(StringValue.ToString).ToBe('');
  Expect<Boolean>(StringValue.ToBoolean).ToBe(False);
  Expect<Boolean>(IsNaN(StringValue.ToNumber)).ToBe(True);
  Expect<string>(StringValue.TypeName).ToBe('string');
  StringValue.Free;
end;

procedure TTestPrimitives.TestStringValueNumber;
var
  StringValue: TGocciaStringValue;
begin
  StringValue := TGocciaStringValue.Create('123.456');
  Expect<string>(StringValue.ToString).ToBe('123.456');
  Expect<Boolean>(StringValue.ToBoolean).ToBe(True);
  Expect<Double>(StringValue.ToNumber).ToBe(123.456);
  Expect<string>(StringValue.TypeName).ToBe('string');
  StringValue.Free;
end;

procedure TTestPrimitives.TestNumberValue;
var
  NumberValue: TGocciaNumberValue;
begin
  NumberValue := TGocciaNumberValue.Create(123.456);
  Expect<string>(NumberValue.ToString).ToBe('123.456');
  Expect<Boolean>(NumberValue.ToBoolean).ToBe(True);
  Expect<Double>(NumberValue.ToNumber).ToBe(123.456);
  Expect<string>(NumberValue.TypeName).ToBe('number');
  NumberValue.Free;
end;

procedure TTestPrimitives.TestBooleanValueTrue;
var
  BooleanValue: TGocciaBooleanValue;
begin
  BooleanValue := TGocciaBooleanValue.Create(True);
  Expect<string>(BooleanValue.ToString).ToBe('true');
  Expect<Boolean>(BooleanValue.ToBoolean).ToBe(True);
  Expect<Double>(BooleanValue.ToNumber).ToBe(1);
  Expect<string>(BooleanValue.TypeName).ToBe('boolean');
  BooleanValue.Free;
end;

procedure TTestPrimitives.TestBooleanValueFalse;
var
  BooleanValue: TGocciaBooleanValue;
begin
  BooleanValue := TGocciaBooleanValue.Create(False);
  Expect<string>(BooleanValue.ToString).ToBe('false');
  Expect<Boolean>(BooleanValue.ToBoolean).ToBe(False);
  Expect<Double>(BooleanValue.ToNumber).ToBe(0);
  Expect<string>(BooleanValue.TypeName).ToBe('boolean');
  BooleanValue.Free;
end;

procedure TTestPrimitives.TestNullValue;
var
  NullValue: TGocciaNullValue;
begin
  NullValue := TGocciaNullValue.Create;
  Expect<string>(NullValue.ToString).ToBe('null');
  Expect<Boolean>(NullValue.ToBoolean).ToBe(False);
  Expect<Boolean>(IsNaN(NullValue.ToNumber)).ToBe(True);
  Expect<string>(NullValue.TypeName).ToBe('object');
  NullValue.Free;
end;

var
  TestResult: TTestResult;

begin
  TestRunnerProgram.AddSuite(TTestPrimitives.Create('Primitives'));
  TestRunnerProgram.Run;

  // Exit with appropriate code for CI/CD
  ExitCode := 0;
  for TestResult in TestRunnerProgram.Results do
  begin
    if TestResult.Status = tsFail then
    begin
      ExitCode := 1;
      Break;
    end;
  end;

  if ExitCode = 0 then
    WriteLn(#10'Success! All tests passed.')
  else
    WriteLn(#10'Failure! Some tests failed.');
end.