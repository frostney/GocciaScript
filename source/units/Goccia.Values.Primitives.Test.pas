program Goccia.Values.Primitives.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.GarbageCollector,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.Primitives;

type
  TTestPrimitives = class(TTestSuite)
  private
    procedure TestUndefinedValue;
    procedure TestStringValueContent;
    procedure TestStringValueEmpty;
    procedure TestStringValueNumber;
    procedure TestStringValuePreservesUnicode;
    procedure TestStringValueAccountsForBackingStore;
    procedure TestStringValueMemoryLimitRaisesOnce;
    procedure TestNumberValue;
    procedure TestNumberValueNaN;
    procedure TestNumberValueInfinity;
    procedure TestNumberValueNegativeInfinity;
    procedure TestNumberValueNegativeZero;
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
  Test('String value preserves Unicode text', TestStringValuePreservesUnicode);
  Test('String value accounts for its backing store',
    TestStringValueAccountsForBackingStore);
  Test('String value memory limit raises without recursion',
    TestStringValueMemoryLimitRaisesOnce);
  Test('Number value', TestNumberValue);
  Test('NaN value', TestNumberValueNaN);
  Test('Infinity value', TestNumberValueInfinity);
  Test('Negative Infinity value', TestNumberValueNegativeInfinity);
  Test('Negative zero value', TestNumberValueNegativeZero);
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

procedure TTestPrimitives.TestStringValuePreservesUnicode;
var
  Value: TGocciaStringLiteralValue;
begin
  Value := TGocciaStringLiteralValue.Create(
    'Caf' + #$00E9 + ' d' + #$00E9 + 'j' + #$00E0 + ' vu');
  Expect<string>(Value.ToStringLiteral.Value).ToBe(
    'Caf' + #$00E9 + ' d' + #$00E9 + 'j' + #$00E0 + ' vu');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<string>(Value.TypeName).ToBe('string');
end;

procedure TTestPrimitives.TestStringValueAccountsForBackingStore;
const
  STRING_CODE_UNITS = 1024;
var
  AccountedBytes, BaselineBytes: Int64;
  GC: TGarbageCollector;
  OwnsGarbageCollector: Boolean;
  Value: TGocciaStringLiteralValue;
begin
  OwnsGarbageCollector := TGarbageCollector.Instance = nil;
  if OwnsGarbageCollector then
    TGarbageCollector.Initialize;
  GC := TGarbageCollector.Instance;
  try
    BaselineBytes := GC.BytesAllocated;
    Value := TGocciaStringLiteralValue.Create(
      StringOfChar('x', STRING_CODE_UNITS));
    GC.AddRootObject(Value);
    try
      AccountedBytes := GC.BytesAllocated - BaselineBytes;
      Expect<Boolean>(
        AccountedBytes >= STRING_CODE_UNITS * SizeOf(Char)).ToBe(True);
    finally
      GC.RemoveRootObject(Value);
      GC.Collect;
    end;
  finally
    if OwnsGarbageCollector then
      TGarbageCollector.Shutdown;
  end;
end;

procedure TTestPrimitives.TestStringValueMemoryLimitRaisesOnce;
var
  GC: TGarbageCollector;
  OldMaxBytes: Int64;
  OwnsGarbageCollector: Boolean;
  RaisedMemoryLimit: Boolean;
begin
  OwnsGarbageCollector := TGarbageCollector.Instance = nil;
  if OwnsGarbageCollector then
    TGarbageCollector.Initialize;
  GC := TGarbageCollector.Instance;
  OldMaxBytes := GC.MaxBytes;
  RaisedMemoryLimit := False;
  try
    GC.MaxBytes := GC.BytesAllocated + 1;
    try
      TGocciaStringLiteralValue.Create(StringOfChar('x', 64));
    except
      on E: TGocciaThrowValue do
        RaisedMemoryLimit := True;
    end;
    Expect<Boolean>(RaisedMemoryLimit).ToBe(True);
    Expect<Boolean>(GC.MemoryLimitFiring).ToBe(False);
  finally
    GC.MaxBytes := OldMaxBytes;
    GC.Collect;
    if OwnsGarbageCollector then
      TGarbageCollector.Shutdown;
  end;
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

procedure TTestPrimitives.TestNumberValueNaN;
var
  Value: TGocciaNumberLiteralValue;
begin
  Value := TGocciaNumberLiteralValue.NaNValue;
  Expect<Boolean>(Value.IsNaN).ToBe(True);
  Expect<Boolean>(Value.IsInfinite).ToBe(False);
  Expect<Boolean>(Value.IsNegativeZero).ToBe(False);
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('NaN');
  Expect<string>(Value.TypeName).ToBe('number');
end;

procedure TTestPrimitives.TestNumberValueInfinity;
var
  Value: TGocciaNumberLiteralValue;
begin
  Value := TGocciaNumberLiteralValue.InfinityValue;
  Expect<Boolean>(Value.IsInfinity).ToBe(True);
  Expect<Boolean>(Value.IsInfinite).ToBe(True);
  Expect<Boolean>(Value.IsNaN).ToBe(False);
  Expect<Boolean>(Value.IsNegativeZero).ToBe(False);
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('Infinity');
  Expect<string>(Value.TypeName).ToBe('number');
end;

procedure TTestPrimitives.TestNumberValueNegativeInfinity;
var
  Value: TGocciaNumberLiteralValue;
begin
  Value := TGocciaNumberLiteralValue.NegativeInfinityValue;
  Expect<Boolean>(Value.IsNegativeInfinity).ToBe(True);
  Expect<Boolean>(Value.IsInfinite).ToBe(True);
  Expect<Boolean>(Value.IsInfinity).ToBe(False);
  Expect<Boolean>(Value.IsNaN).ToBe(False);
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(True);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('-Infinity');
  Expect<string>(Value.TypeName).ToBe('number');
end;

procedure TTestPrimitives.TestNumberValueNegativeZero;
var
  Value: TGocciaNumberLiteralValue;
begin
  Value := TGocciaNumberLiteralValue.NegativeZeroValue;
  Expect<Boolean>(Value.IsNegativeZero).ToBe(True);
  Expect<Boolean>(Value.IsNaN).ToBe(False);
  Expect<Boolean>(Value.IsInfinite).ToBe(False);
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
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
  Value := TGocciaNullLiteralValue.NullValue;
  Expect<string>(Value.ToStringLiteral.Value).ToBe('null');
  Expect<Boolean>(Value.ToBooleanLiteral.Value).ToBe(False);
  Expect<Double>(Value.ToNumberLiteral.Value).ToBe(0);
  Expect<string>(Value.TypeName).ToBe('null');
  Expect<string>(Value.TypeOf).ToBe('object'); // JavaScript: typeof null === 'object'
  Expect<Boolean>(Value = TGocciaNullLiteralValue.NullValue).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TTestPrimitives.Create('Primitives'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
