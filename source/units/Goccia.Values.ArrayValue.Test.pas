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
    procedure TestALongStringIsCopiedOnce;
    procedure TestRepeatedSeparatorsTakeNoScratch;
    procedure TestAJoinPastTheBudgetThrowsARangeError;
    procedure TestAJoinSurvivesACollectionAtEveryAsk;
  end;

procedure TTestArrayValue.SetupTests;
begin
  Test('joining an array, and every conversion of one to a string, takes work linear in the result',
    TestArrayConversionsTakeLinearWork);
  Test('joining a typed array, and converting one to a string, takes work linear in the result',
    TestTypedArrayConversionsTakeLinearWork);
  Test('joining an array of one long string returns that string without a copy',
    TestOneLongStringIsNotCopied);
  Test('joining a long string with something else copies it once',
    TestALongStringIsCopiedOnce);
  Test('joining empty elements with a long separator asks for little more than the result',
    TestRepeatedSeparatorsTakeNoScratch);
  Test('a join whose result would pass the memory budget throws a catchable RangeError',
    TestAJoinPastTheBudgetThrowsARangeError);
  Test('a join that collects at every table growth and before its result keeps every string',
    TestAJoinSurvivesACollectionAtEveryAsk);
end;

{ Runs a script that builds `a`, ALength strings of ten digits, `ta`, a
  Float64Array of ALength eleven-digit integers, `s`, a string of
  LONG_STRING_LENGTH characters, `e`, ALength undefined elements, and
  `separator`, 255 characters, then counts the bytes asked for by one call of
  `() => AExpression`. }
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
    'const e = Array(%0:d).fill(undefined);' + LineEnding +
    'const separator = "x".repeat(255);' + LineEnding +
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

{ A join used to hand back the one element's string itself, and still does,
  and a join that adds to a long string copies it once, into a result of the
  exact length: a buffer that grew by doubling would need up to twice the
  result, and more again to copy it out. }
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

procedure TTestArrayValue.TestALongStringIsCopiedOnce;

  procedure ExpectOneCopy(const AExpression: string);
  var
    Work: TConversionWork;
  begin
    Work := ConversionWork(AExpression, 1);
    Expect<Integer>(Work.ResultLength).ToBe(LONG_STRING_LENGTH + 1);
    if Work.Bytes > (LONG_STRING_LENGTH + 1) * SizeOf(WideChar) +
       SHARED_STRING_BYTES then
      Fail(Format('%s asked for %d bytes for a result of %d characters',
        [AExpression, Work.Bytes, LONG_STRING_LENGTH + 1]));
    Expect<Boolean>(Work.Bytes <= (LONG_STRING_LENGTH + 1) * SizeOf(WideChar) +
      SHARED_STRING_BYTES).ToBe(True);
  end;

begin
  ExpectOneCopy('[s, ""].join()');
  ExpectOneCopy('["", s].join()');
  ExpectOneCopy('`${[s, "x"].join("")}`');
end;

{ The appends needed the result and nothing more. Separators and empty
  elements take no memory before the result is allocated, so a join that is
  almost all separators, Array(60000).join("x".repeat(255)) at its largest,
  still fits wherever the result does. }
procedure TTestArrayValue.TestRepeatedSeparatorsTakeNoScratch;
const
  BOOKKEEPING_BYTES = 64 * 1024;
var
  Work: TConversionWork;
  ResultBytes: Int64;
begin
  Work := ConversionWork('e.join(separator)', LONG_LENGTH);
  Expect<Integer>(Work.ResultLength).ToBe((LONG_LENGTH - 1) * 255);
  ResultBytes := Int64(Work.ResultLength) * SizeOf(WideChar);
  if Work.Bytes > ResultBytes + BOOKKEEPING_BYTES then
    Fail(Format('e.join(separator) asked for %d bytes for a result of %d ' +
      'bytes', [Work.Bytes, ResultBytes]));
  Expect<Boolean>(Work.Bytes <= ResultBytes + BOOKKEEPING_BYTES).ToBe(True);
end;

{ The join asks the budget about its result before allocating it, and refuses
  as the result's own charge would have: with a RangeError the script can
  catch, having asked for far less than the result. Joining only separators
  keeps the join's tables out of it, so the result alone is refused. }
procedure TTestArrayValue.TestAJoinPastTheBudgetThrowsARangeError;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Convert: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Collector: TGarbageCollector;
  PreviousMaxBytes, Bytes: Int64;
  Answer: string;

  function Call: string;
  begin
    Result := TGocciaFunctionBase(Convert).Call(Args,
      TGocciaUndefinedLiteralValue.UndefinedValue).ToStringLiteral.Value;
  end;

begin
  Source := TStringList.Create;
  Source.Text := Format(
    'const e = Array(%d).fill(undefined);' + LineEnding +
    'const separator = "x".repeat(255);' + LineEnding +
    'globalThis.convert = () => { try { return String(e.join(separator).length); } ' +
    'catch (error) { return error.name; } };', [LONG_LENGTH]);
  Engine := TGocciaEngine.Create('array-join-budget.js', Source);
  Args := TGocciaArgumentsCollection.Create;
  try
    Engine.Execute;
    Convert := TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('convert');
    Collector := TGarbageCollector.Instance;
    PreviousMaxBytes := Collector.MaxBytes;
    try
      Collector.Collect;
      Collector.MaxBytes := Collector.BytesAllocated + 64 * 1024;
      StartCountingAllocations;
      try
        Answer := Call;
      finally
        StopCountingAllocations;
        Bytes := CountedAllocationBytes;
      end;
      Expect<string>(Answer).ToBe('RangeError');
      // Far less than the result's (LONG_LENGTH - 1) * 255 * 2 bytes.
      Expect<Boolean>(Bytes < 256 * 1024).ToBe(True);
      Collector.MaxBytes := PreviousMaxBytes;
      Expect<string>(Call).ToBe(IntToStr((LONG_LENGTH - 1) * 255));
    finally
      Collector.MaxBytes := PreviousMaxBytes;
    end;
  finally
    Args.Free;
    Engine.Free;
    Source.Free;
  end;
end;

{ The join's gate is a collecting safe point: under the stress switch every
  table growth and the result's ask collects, while the buffer holds strings
  whose values nothing roots any more. The strings must survive them, and the
  quarantine must see no swept value reached again. }
procedure TTestArrayValue.TestAJoinSurvivesACollectionAtEveryAsk;
const
  ELEMENT_COUNT = 200;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Convert: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Collector: TGarbageCollector;
  Expected: string;
  I: Integer;
begin
  Source := TStringList.Create;
  Source.Text := Format(
    'const items = Array.from({ length: %0:d }, (_, i) => ' +
    '({ toString() { return "item" + i; } }));' + LineEnding +
    'const typed = Float64Array.from({ length: %0:d }, (_, i) => i);' +
    LineEnding +
    'globalThis.convert = () => items.join("-") + "|" + typed.join("-");',
    [ELEMENT_COUNT]);
  Expected := '';
  for I := 0 to ELEMENT_COUNT - 1 do
  begin
    if I > 0 then
      Expected := Expected + '-';
    Expected := Expected + 'item' + IntToStr(I);
  end;
  Expected := Expected + '|';
  for I := 0 to ELEMENT_COUNT - 1 do
  begin
    if I > 0 then
      Expected := Expected + '-';
    Expected := Expected + IntToStr(I);
  end;
  Engine := TGocciaEngine.Create('array-join-stress.js', Source);
  Args := TGocciaArgumentsCollection.Create;
  try
    Engine.Execute;
    Convert := TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('convert');
    Collector := TGarbageCollector.Instance;
    TGarbageCollector.ResetQuarantineViolations;
    Collector.QuarantineSwept := True;
    Collector.CollectBeforeEveryReservation := True;
    try
      Expect<string>(TGocciaFunctionBase(Convert).Call(Args,
        TGocciaUndefinedLiteralValue.UndefinedValue).ToStringLiteral.Value)
        .ToBe(Expected);
    finally
      Collector.CollectBeforeEveryReservation := False;
      Collector.QuarantineSwept := False;
    end;
    Expect<Integer>(TGarbageCollector.QuarantineViolations).ToBe(0);
  finally
    Args.Free;
    Engine.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestArrayValue.Create('Array Value'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
