program Goccia.NumberConversion.Test;

{$I Goccia.inc}

uses
  Math,

  NumberBits,
  TestingPascalLibrary,

  Goccia.NumberConversion;

type
  TNumberConversionTests = class(TTestSuite)
  private
    procedure TestUint32;
    procedure TestInt32;
    procedure TestUint16;
    procedure TestUint8Clamp;
    procedure TestFloat32;
  public
    procedure SetupTests; override;
  end;

procedure TNumberConversionTests.SetupTests;
begin
  Test('ToUint32 truncates and wraps exact binary64 values', TestUint32);
  Test('ToInt32 maps the upper half into the signed range', TestInt32);
  Test('ToUint16 keeps the low sixteen bits', TestUint16);
  Test('ToUint8Clamp rounds halfway values to even', TestUint8Clamp);
  Test('float32 conversion uses native nearest binary32 rounding', TestFloat32);
end;

procedure TNumberConversionTests.TestUint32;
begin
  Expect<UInt32>(NumberToUint32(NaN)).ToBe(UInt32(0));
  Expect<UInt32>(NumberToUint32(Infinity)).ToBe(UInt32(0));
  Expect<UInt32>(NumberToUint32(-1.0)).ToBe(UInt32($FFFFFFFF));
  Expect<UInt32>(NumberToUint32(4294967297.75)).ToBe(UInt32(1));
  Expect<UInt32>(NumberToUint32(1.0E308)).ToBe(UInt32(0));
end;

procedure TNumberConversionTests.TestInt32;
begin
  Expect<Int32>(NumberToInt32(2147483647.0)).ToBe(2147483647);
  Expect<Int32>(NumberToInt32(2147483648.0)).ToBe(Int32(-2147483647 - 1));
  Expect<Int32>(NumberToInt32(4294967295.0)).ToBe(-1);
end;

procedure TNumberConversionTests.TestUint16;
begin
  Expect<UInt16>(NumberToUint16(65537.0)).ToBe(UInt16(1));
  Expect<UInt16>(NumberToUint16(-1.0)).ToBe(UInt16($FFFF));
end;

procedure TNumberConversionTests.TestUint8Clamp;
begin
  Expect<UInt8>(NumberToUint8Clamp(NaN)).ToBe(UInt8(0));
  Expect<UInt8>(NumberToUint8Clamp(-1.0)).ToBe(UInt8(0));
  Expect<UInt8>(NumberToUint8Clamp(255.5)).ToBe(UInt8(255));
  Expect<UInt8>(NumberToUint8Clamp(0.5)).ToBe(UInt8(0));
  Expect<UInt8>(NumberToUint8Clamp(1.5)).ToBe(UInt8(2));
  Expect<UInt8>(NumberToUint8Clamp(2.5)).ToBe(UInt8(2));
  Expect<UInt8>(NumberToUint8Clamp(254.5)).ToBe(UInt8(254));
end;

procedure TNumberConversionTests.TestFloat32;
var
  FloatValue: Single;
  FloatBits: UInt32;
begin
  FloatValue := NumberToFloat32(1.337);
  Move(FloatValue, FloatBits, SizeOf(FloatBits));
  Expect<UInt32>(FloatBits).ToBe(UInt32($3FAB22D1));
end;

begin
  TestRunnerProgram.AddSuite(
    TNumberConversionTests.Create('Goccia.NumberConversion'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
