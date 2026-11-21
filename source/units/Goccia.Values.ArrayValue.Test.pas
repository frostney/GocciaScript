program Goccia.Values.ArrayValue.Test;

{$I Goccia.inc}

{ Turning a large array into a string costs work linear in the result. An
  array reaches a string through Array.prototype.join, by a direct call or
  through toString from a template literal, String(), `+` and toString(); a
  typed array through %TypedArray%.prototype.join. Each built its result by
  appending to a Pascal string, which reallocates the whole string on every
  append, so the conversion took time quadratic in its length. Since #2129
  removed the array's own ToStringLiteral, a template literal that substitutes
  an array takes that path too, and one of 600,000 short strings took about
  0.7 s on Linux where it had taken 40 ms.

  A time would depend on the machine and its load, so each conversion counts
  the bytes it asks the memory manager for at two lengths instead: linear work
  about doubles when the array doubles, the quadratic appends about quadruple.
  The collector is held off while it counts: a collection would add the bytes
  of its own marking, when its threshold falls inside one call and not the
  other. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  AllocationCounter,
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  SHORT_LENGTH = 10000;
  LONG_LENGTH = 2 * SHORT_LENGTH;
  { Doubling the array may at most multiply the bytes by this. Linear work
    gives 2, the appends gave 4. }
  LINEAR_GROWTH_RATIO = 2.5;
  { The characters of `s`, and the bytes a conversion of an array holding only
    `s` may ask for: far less than a copy of it. }
  LONG_STRING_LENGTH = 1000000;
  SHARED_STRING_BYTES = 64 * 1024;

type
  TConversionWork = record
    Bytes: Int64;
    ResultLength: Integer;
  end;

  TTestArrayValue = class(TTestSuite)
  private
    function ConversionWork(const AExpression: string;
      const ALength: Integer): TConversionWork;
    procedure ExpectConversionLinear(const AExpression: string;
      const AElementLength: Integer);
  public
    procedure SetupTests; override;

    procedure TestArrayConversionsTakeLinearWork;
    procedure TestTypedArrayConversionsTakeLinearWork;
    procedure TestOneLongStringIsNotCopied;
  end;

procedure TTestArrayValue.SetupTests;
begin
  Test('joining an array, and every conversion of one to a string, takes work linear in the result',
    TestArrayConversionsTakeLinearWork);
  Test('joining a typed array, and converting one to a string, takes work linear in the result',
    TestTypedArrayConversionsTakeLinearWork);
  Test('joining an array of one long string returns that string without a copy',
    TestOneLongStringIsNotCopied);
end;

{ Runs a script that builds `a`, ALength strings of ten digits, `ta`, a
  Float64Array of ALength eleven-digit integers, and `s`, a string of
  LONG_STRING_LENGTH characters, then counts the bytes asked for by one call
  of `() => AExpression`. }
function TTestArrayValue.ConversionWork(const AExpression: string;
  const ALength: Integer): TConversionWork;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Convert, Converted: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Collector: TGarbageCollector;
  WasEnabled: Boolean;
begin
  Source := TStringList.Create;
  Source.Text := Format(
    'const a = Array.from({ length: %0:d }, (_, i) => String(i).padStart(10, "0"));' +
    LineEnding +
    'const ta = new Float64Array(%0:d).map((_, i) => 10000000000 + i);' +
    LineEnding +
    'const s = "x".repeat(%2:d);' + LineEnding +
    'globalThis.convert = () => %1:s;',
    [ALength, AExpression, LONG_STRING_LENGTH]);
  Engine := TGocciaEngine.Create('array-conversion.js', Source);
  try
    Engine.Execute;
    Convert := TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('convert');
    Collector := TGarbageCollector.Instance;
    Collector.Collect;
    WasEnabled := Collector.Enabled;
    Collector.Enabled := False;
    Args := TGocciaArgumentsCollection.Create;
    try
      StartCountingAllocations;
      try
        Converted := TGocciaFunctionBase(Convert).Call(Args,
          TGocciaUndefinedLiteralValue.UndefinedValue);
      finally
        StopCountingAllocations;
      end;
      Result.Bytes := CountedAllocationBytes;
      Result.ResultLength := Length(Converted.ToStringLiteral.Value);
    finally
      Collector.Enabled := WasEnabled;
      Args.Free;
    end;
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TTestArrayValue.ExpectConversionLinear(const AExpression: string;
  const AElementLength: Integer);
var
  Short, Long: TConversionWork;
begin
  Short := ConversionWork(AExpression, SHORT_LENGTH);
  Long := ConversionWork(AExpression, LONG_LENGTH);
  // Each element and the comma after it, but the last.
  Expect<Integer>(Short.ResultLength)
    .ToBe(SHORT_LENGTH * (AElementLength + 1) - 1);
  Expect<Integer>(Long.ResultLength)
    .ToBe(LONG_LENGTH * (AElementLength + 1) - 1);
  if Long.Bytes > LINEAR_GROWTH_RATIO * Short.Bytes then
    Fail(Format('%s asked for %d bytes for %d elements against %d bytes ' +
      'for %d', [AExpression, Long.Bytes, LONG_LENGTH, Short.Bytes,
      SHORT_LENGTH]));
  Expect<Boolean>(Long.Bytes <= LINEAR_GROWTH_RATIO * Short.Bytes).ToBe(True);
end;

procedure TTestArrayValue.TestArrayConversionsTakeLinearWork;
begin
  ExpectConversionLinear('a.join(",")', 10);
  ExpectConversionLinear('a.join()', 10);
  ExpectConversionLinear('`${a}`', 10);
  ExpectConversionLinear('String(a)', 10);
  ExpectConversionLinear('a + ""', 10);
  ExpectConversionLinear('a.toString()', 10);
end;

procedure TTestArrayValue.TestTypedArrayConversionsTakeLinearWork;
begin
  ExpectConversionLinear('ta.join(",")', 11);
  ExpectConversionLinear('`${ta}`', 11);
end;

{ A join used to hand back the one element's string itself, and still does:
  copying it into a buffer that grows by doubling, and out again at the end,
  would need about three times the string. }
procedure TTestArrayValue.TestOneLongStringIsNotCopied;

  procedure ExpectShared(const AExpression: string);
  var
    Work: TConversionWork;
  begin
    Work := ConversionWork(AExpression, 1);
    Expect<Integer>(Work.ResultLength).ToBe(LONG_STRING_LENGTH);
    if Work.Bytes > SHARED_STRING_BYTES then
      Fail(Format('%s asked for %d bytes for a string of %d characters',
        [AExpression, Work.Bytes, LONG_STRING_LENGTH]));
    Expect<Boolean>(Work.Bytes <= SHARED_STRING_BYTES).ToBe(True);
  end;

begin
  ExpectShared('[s].join()');
  ExpectShared('[undefined, s, null].join("")');
  ExpectShared('`${[s]}`');
end;

begin
  TestRunnerProgram.AddSuite(TTestArrayValue.Create('Array Value'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
