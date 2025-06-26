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
  UndefinedValue: TGocciaUndefinedLiteralValue;
begin
  UndefinedValue := TGocciaUndefinedLiteralValue.Create;
  Expect<string>(UndefinedValue.ToString).ToBe('undefined');
  Expect<Boolean>(UndefinedValue.ToBoolean).ToBe(False);
  Expect<Boolean>(IsNaN(UndefinedValue.ToNumber)).ToBe(True);
  Expect<string>(UndefinedValue.TypeName).ToBe('undefined');
  UndefinedValue.Free;
end;

procedure TTestPrimitives.TestStringValueContent;
var
  StringValue: TGocciaStringLiteralValue;
begin
  StringValue := TGocciaStringLiteralValue.Create('Hello, world!');
  Expect<string>(StringValue.ToString).ToBe('Hello, world!');
  Expect<Boolean>(StringValue.ToBoolean).ToBe(True);
  Expect<Boolean>(IsNaN(StringValue.ToNumber)).ToBe(True);
  Expect<string>(StringValue.TypeName).ToBe('string');
  StringValue.Free;
end;

procedure TTestPrimitives.TestStringValueEmpty;
var
  StringValue: TGocciaStringLiteralValue;
begin
  StringValue := TGocciaStringLiteralValue.Create('');
  Expect<string>(StringValue.ToString).ToBe('');
  Expect<Boolean>(StringValue.ToBoolean).ToBe(False);
  Expect<Boolean>(IsNaN(StringValue.ToNumber)).ToBe(True);
  Expect<string>(StringValue.TypeName).ToBe('string');
  StringValue.Free;
end;

procedure TTestPrimitives.TestStringValueNumber;
var
  StringValue: TGocciaStringLiteralValue;
begin
  StringValue := TGocciaStringLiteralValue.Create('123.456');
  Expect<string>(StringValue.ToString).ToBe('123.456');
  Expect<Boolean>(StringValue.ToBoolean).ToBe(True);
  Expect<Double>(StringValue.ToNumber).ToBe(123.456);
  Expect<string>(StringValue.TypeName).ToBe('string');
  StringValue.Free;
end;

procedure TTestPrimitives.TestNumberValue;
var
  NumberValue: TGocciaNumberLiteralValue;
begin
  NumberValue := TGocciaNumberLiteralValue.Create(123.456);
  Expect<string>(NumberValue.ToString).ToBe('123.456');
  Expect<Boolean>(NumberValue.ToBoolean).ToBe(True);
  Expect<Double>(NumberValue.ToNumber).ToBe(123.456);
  Expect<string>(NumberValue.TypeName).ToBe('number');
  NumberValue.Free;
end;

procedure TTestPrimitives.TestBooleanValueTrue;
var
  BooleanValue: TGocciaBooleanLiteralValue;
begin
  BooleanValue := TGocciaBooleanLiteralValue.Create(True);
  Expect<string>(BooleanValue.ToString).ToBe('true');
  Expect<Boolean>(BooleanValue.ToBoolean).ToBe(True);
  Expect<Double>(BooleanValue.ToNumber).ToBe(1);
  Expect<string>(BooleanValue.TypeName).ToBe('boolean');
  BooleanValue.Free;
end;

procedure TTestPrimitives.TestBooleanValueFalse;
var
  BooleanValue: TGocciaBooleanLiteralValue;
begin
  BooleanValue := TGocciaBooleanLiteralValue.Create(False);
  Expect<string>(BooleanValue.ToString).ToBe('false');
  Expect<Boolean>(BooleanValue.ToBoolean).ToBe(False);
  Expect<Double>(BooleanValue.ToNumber).ToBe(0);
  Expect<string>(BooleanValue.TypeName).ToBe('boolean');
  BooleanValue.Free;
end;

procedure TTestPrimitives.TestNullValue;
var
  NullValue: TGocciaNullLiteralValue;
begin
  NullValue := TGocciaNullLiteralValue.Create;
  Expect<string>(NullValue.ToString).ToBe('null');
  Expect<Boolean>(NullValue.ToBoolean).ToBe(False);
  Expect<Double>(NullValue.ToNumber).ToBe(0);
  Expect<string>(NullValue.TypeName).ToBe('object');
  NullValue.Free;
end;

var
  TestResult: TTestResult;

begin
  TestRunnerProgram.AddSuite(TTestPrimitives.Create('Primitives'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
