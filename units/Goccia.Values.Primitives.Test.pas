program Goccia.Values.Primitives.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  Goccia.Values.Primitives,

  TestRunner;

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
  Value: TGocciaUndefinedLiteralValue;
begin
  Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  Expect<string>(Value.ToStringLiteral.Value).ToBe('undefined');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
  Expect<Boolean>(Value.ToNumberLiteral.IsNaN).ToBe(True);
  Expect<string>(Value.TypeName).ToBe('undefined');
end;

procedure TTestPrimitives.TestStringValueContent;
var
  Value: TGocciaStringLiteralValue;
begin
  Value := TGocciaStringLiteralValue.Create('Hello, world!');
  Expect<string>(Value.ToStringLiteral.Value).ToBe('Hello, world!');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<Boolean>(Value.ToNumberLiteral.IsNaN).ToBe(True);
  Expect<string>(Value.TypeName).ToBe('string');
end;

procedure TTestPrimitives.TestStringValueEmpty;
var
  Value: TGocciaStringLiteralValue;
begin
  Value := TGocciaStringLiteralValue.Create('');
  Expect<string>(Value.ToStringLiteral.Value).ToBe('');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(0);
  Expect<string>(Value.TypeName).ToBe('string');
end;

procedure TTestPrimitives.TestStringValueNumber;
var
  Value: TGocciaStringLiteralValue;
begin
  Value := TGocciaStringLiteralValue.Create('123.456');
  Expect<string>(Value.ToStringLiteral.Value).ToBe('123.456');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(123.456);
  Expect<string>(Value.TypeName).ToBe('string');
end;

procedure TTestPrimitives.TestNumberValue;
var
  Value: TGocciaNumberLiteralValue;
begin
  Value := TGocciaNumberLiteralValue.Create(123.456);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('123.456');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(123.456);
  Expect<string>(Value.TypeName).ToBe('number');
end;

procedure TTestPrimitives.TestBooleanValueTrue;
var
  Value: TGocciaBooleanLiteralValue;
begin
  Value := TGocciaBooleanLiteralValue.Create(True);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('true');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(1);
  Expect<string>(Value.TypeName).ToBe('boolean');
end;

procedure TTestPrimitives.TestBooleanValueFalse;
var
  Value: TGocciaBooleanLiteralValue;
begin
  Value := TGocciaBooleanLiteralValue.Create(False);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('false');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(0);
  Expect<string>(Value.TypeName).ToBe('boolean');
end;

procedure TTestPrimitives.TestNullValue;
var
  Value: TGocciaNullLiteralValue;
begin
  Value := TGocciaNullLiteralValue.Create;
  Expect<string>(Value.ToStringLiteral.Value).ToBe('null');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(0);
  Expect<string>(Value.TypeName).ToBe('null');
  Expect<string>(Value.TypeOf).ToBe('object'); // JavaScript: typeof null === 'object'
end;

begin
  TestRunnerProgram.AddSuite(TTestPrimitives.Create('Primitives'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
