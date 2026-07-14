program Goccia.Builtins.Testing.SnapshotFormatting.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Testing.SnapshotFormatting,
  Goccia.TestSetup,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsymmetricMatcher,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue;

const
  LF = #10;

type
  TReplacementSnapshotFormatter = class(TInterfacedObject,
    IGocciaSnapshotFormatter)
  public
    function Format(const AValue: TGocciaValue;
      const ASerializers: TGocciaSnapshotSerializerRegistry): string;
  end;

  TSnapshotFormattingTests = class(TTestSuite)
  private
    FFormatting: TGocciaSnapshotFormatting;
    FFirstSerializer: TGocciaObjectValue;
    FSecondSerializer: TGocciaObjectValue;

    function MatchesCustom(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FirstSerialize(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SecondSerialize(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RecursiveSerialize(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function MaxWidthSerialize(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function MakeSerializer(
      const ASerialize: TGocciaNativeFunctionValue): TGocciaObjectValue;

    procedure TestPrimitivesMatchVitest;
    procedure TestObjectOrderingAndCircularReference;
    procedure TestCollectionsMatchVitest;
    procedure TestBinaryValuesMatchVitest;
    procedure TestLastAddedSerializerRunsFirst;
    procedure TestSerializerReceivesPrettyFormatArguments;
    procedure TestSerializerCanLimitRecursiveCollectionWidth;
    procedure TestAsymmetricMatchersUseVitestDisplay;
    procedure TestFormatterCanBeReplaced;
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
  end;

function TReplacementSnapshotFormatter.Format(const AValue: TGocciaValue;
  const ASerializers: TGocciaSnapshotSerializerRegistry): string;
begin
  Result := 'replacement';
end;

procedure TSnapshotFormattingTests.SetupTests;
begin
  Test('Primitives match Vitest', TestPrimitivesMatchVitest);
  Test('Object properties are sorted and cycles are stable',
    TestObjectOrderingAndCircularReference);
  Test('Map and Set match Vitest', TestCollectionsMatchVitest);
  Test('Typed arrays and buffers match Vitest', TestBinaryValuesMatchVitest);
  Test('Last added serializer runs first', TestLastAddedSerializerRunsFirst);
  Test('Serializer receives pretty-format arguments',
    TestSerializerReceivesPrettyFormatArguments);
  Test('Serializer can limit recursive collection width',
    TestSerializerCanLimitRecursiveCollectionWidth);
  Test('Asymmetric matchers use Vitest display',
    TestAsymmetricMatchersUseVitestDisplay);
  Test('Pascal formatter can be replaced', TestFormatterCanBeReplaced);
end;

procedure TSnapshotFormattingTests.BeforeEach;
begin
  FFormatting := TGocciaSnapshotFormatting.Create;
  FFirstSerializer := nil;
  FSecondSerializer := nil;
end;

procedure TSnapshotFormattingTests.AfterEach;
begin
  FFormatting.Free;
end;

function TSnapshotFormattingTests.MatchesCustom(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.GetElement(0) is TGocciaObjectValue then
    Result := TGocciaBooleanLiteralValue.FromBoolean(
      TGocciaObjectValue(AArgs.GetElement(0)).GetProperty(
        'custom').ToBooleanLiteral.Value)
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TSnapshotFormattingTests.FirstSerialize(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('first');
end;

function TSnapshotFormattingTests.SecondSerialize(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('second');
end;

function TSnapshotFormattingTests.RecursiveSerialize(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Printer, Nested, Printed: TGocciaValue;
  PrinterArguments: TGocciaArgumentsCollection;
  Config: TGocciaObjectValue;
  References: TGocciaArrayValue;
begin
  Config := TGocciaObjectValue(AArgs.GetElement(1));
  References := TGocciaArrayValue(AArgs.GetElement(4));
  Printer := AArgs.GetElement(5);
  Nested := TGocciaObjectValue(AArgs.GetElement(0)).GetProperty('nested');
  PrinterArguments := TGocciaArgumentsCollection.Create([
    Nested,
    Config,
    AArgs.GetElement(2),
    AArgs.GetElement(3),
    References
  ]);
  try
    Printed := DispatchCall(Printer, PrinterArguments,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    PrinterArguments.Free;
  end;
  Result := TGocciaStringLiteralValue.Create(
    'Custom(depth=' + AArgs.GetElement(3).ToStringLiteral.Value +
    ', indentation=' +
    AArgs.GetElement(2).ToStringLiteral.Value +
    ', min=' + Config.GetProperty('min').ToStringLiteral.Value +
    ', compareKeys=' + Config.GetProperty('compareKeys').TypeOf +
    ', singleQuote=' +
    Config.GetProperty('singleQuote').ToStringLiteral.Value +
    ', quoteKeys=' + Config.GetProperty('quoteKeys').ToStringLiteral.Value +
    ', maxOutputLength=' +
    Config.GetProperty('maxOutputLength').ToStringLiteral.Value +
    ', outputDepths=' + IntToStr(TGocciaArrayValue(
      Config.GetProperty('_outputLengthPerDepth')).Elements.Count) +
    ', refs=' + IntToStr(References.Elements.Count) +
    ', nested=' + Printed.ToStringLiteral.Value + ')');
end;

function TSnapshotFormattingTests.MaxWidthSerialize(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Config: TGocciaObjectValue;
  Nested, Printed, Printer: TGocciaValue;
  PrinterArguments: TGocciaArgumentsCollection;
begin
  Config := TGocciaObjectValue(AArgs.GetElement(1));
  Config.AssignProperty('maxWidth', TGocciaNumberLiteralValue.OneValue);
  Nested := TGocciaObjectValue(AArgs.GetElement(0)).GetProperty('nested');
  Printer := AArgs.GetElement(5);
  PrinterArguments := TGocciaArgumentsCollection.Create([
    Nested,
    Config,
    AArgs.GetElement(2),
    AArgs.GetElement(3),
    AArgs.GetElement(4)
  ]);
  try
    Printed := DispatchCall(Printer, PrinterArguments,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    PrinterArguments.Free;
  end;
  Result := Printed;
end;

function TSnapshotFormattingTests.MakeSerializer(
  const ASerialize: TGocciaNativeFunctionValue): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.CreateDataPropertyOrThrow('test',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(MatchesCustom, 'test', 1));
  Result.CreateDataPropertyOrThrow('serialize', ASerialize);
end;

procedure TSnapshotFormattingTests.TestPrimitivesMatchVitest;
var
  Values: TGocciaArrayValue;
begin
  Values := TGocciaArrayValue.Create;
  Values.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
  Values.Elements.Add(TGocciaNullLiteralValue.NullValue);
  Values.Elements.Add(TGocciaBooleanLiteralValue.TrueValue);
  Values.Elements.Add(TGocciaBooleanLiteralValue.FalseValue);
  Values.Elements.Add(TGocciaNumberLiteralValue.ZeroValue);
  Values.Elements.Add(TGocciaNumberLiteralValue.NegativeZeroValue);
  Values.Elements.Add(TGocciaNumberLiteralValue.NaNValue);
  Values.Elements.Add(TGocciaNumberLiteralValue.InfinityValue);
  Values.Elements.Add(TGocciaNumberLiteralValue.NegativeInfinityValue);
  Values.Elements.Add(TGocciaStringLiteralValue.Create('line' + LF + '"quote"'));
  Values.Elements.Add(TGocciaSymbolValue.Create('description'));

  Expect<string>(FFormatting.Format(Values)).ToBe(
    '[' + LF +
    '  undefined,' + LF +
    '  null,' + LF +
    '  true,' + LF +
    '  false,' + LF +
    '  0,' + LF +
    '  -0,' + LF +
    '  NaN,' + LF +
    '  Infinity,' + LF +
    '  -Infinity,' + LF +
    '  "line' + LF + '"quote"",' + LF +
    '  Symbol(description),' + LF +
    ']');
end;

procedure TSnapshotFormattingTests.TestObjectOrderingAndCircularReference;
var
  Root, Circular: TGocciaObjectValue;
begin
  Root := TGocciaObjectValue.Create;
  Circular := TGocciaObjectValue.Create;
  Root.CreateDataPropertyOrThrow('z', TGocciaNumberLiteralValue.OneValue);
  Root.CreateDataPropertyOrThrow('a', Circular);
  Circular.CreateDataPropertyOrThrow('name',
    TGocciaStringLiteralValue.Create('root'));
  Circular.CreateDataPropertyOrThrow('self', Circular);

  Expect<string>(FFormatting.Format(Root)).ToBe(
    '{' + LF +
    '  "a": {' + LF +
    '    "name": "root",' + LF +
    '    "self": [Circular],' + LF +
    '  },' + LF +
    '  "z": 1,' + LF +
    '}');
end;

procedure TSnapshotFormattingTests.TestCollectionsMatchVitest;
var
  Root: TGocciaObjectValue;
  MapValue: TGocciaMapValue;
  SetValue: TGocciaSetValue;
begin
  MapValue := TGocciaMapValue.Create;
  MapValue.SetEntry(TGocciaStringLiteralValue.Create('a'),
    TGocciaNumberLiteralValue.OneValue);
  SetValue := TGocciaSetValue.Create;
  SetValue.AddItem(TGocciaStringLiteralValue.Create('x'));
  SetValue.AddItem(TGocciaStringLiteralValue.Create('y'));
  Root := TGocciaObjectValue.Create;
  Root.CreateDataPropertyOrThrow('set', SetValue);
  Root.CreateDataPropertyOrThrow('map', MapValue);

  Expect<string>(FFormatting.Format(Root)).ToBe(
    '{' + LF +
    '  "map": Map {' + LF +
    '    "a" => 1,' + LF +
    '  },' + LF +
    '  "set": Set {' + LF +
    '    "x",' + LF +
    '    "y",' + LF +
    '  },' + LF +
    '}');
end;

procedure TSnapshotFormattingTests.TestBinaryValuesMatchVitest;
var
  Root: TGocciaObjectValue;
  BufferValue: TGocciaArrayBufferValue;
  TypedArray: TGocciaTypedArrayValue;
begin
  BufferValue := TGocciaArrayBufferValue.Create(3);
  BufferValue.Data[0] := 1;
  BufferValue.Data[1] := 2;
  BufferValue.Data[2] := 255;
  TypedArray := TGocciaTypedArrayValue.Create(takUint8, 2);
  TypedArray.SetProperty('0', TGocciaNumberLiteralValue.Create(1));
  TypedArray.SetProperty('1', TGocciaNumberLiteralValue.Create(255));
  Root := TGocciaObjectValue.Create;
  Root.CreateDataPropertyOrThrow('typed', TypedArray);
  Root.CreateDataPropertyOrThrow('buffer', BufferValue);

  Expect<string>(FFormatting.Format(Root)).ToBe(
    '{' + LF +
    '  "buffer": ArrayBuffer [' + LF +
    '    1,' + LF +
    '    2,' + LF +
    '    -1,' + LF +
    '  ],' + LF +
    '  "typed": Uint8Array [' + LF +
    '    1,' + LF +
    '    255,' + LF +
    '  ],' + LF +
    '}');
end;

procedure TSnapshotFormattingTests.TestLastAddedSerializerRunsFirst;
var
  Value: TGocciaObjectValue;
begin
  FFirstSerializer := MakeSerializer(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(FirstSerialize,
      'serialize', 6));
  FSecondSerializer := MakeSerializer(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(SecondSerialize,
      'serialize', 6));
  Expect<Boolean>(FFormatting.Serializers.Add(FFirstSerializer)).ToBe(True);
  Expect<Boolean>(FFormatting.Serializers.Add(FSecondSerializer)).ToBe(True);
  Value := TGocciaObjectValue.Create;
  Value.CreateDataPropertyOrThrow('custom',
    TGocciaBooleanLiteralValue.TrueValue);
  Expect<string>(FFormatting.Format(Value)).ToBe('second');
end;

procedure TSnapshotFormattingTests.TestSerializerReceivesPrettyFormatArguments;
var
  Value, Nested: TGocciaObjectValue;
begin
  Value := TGocciaObjectValue.Create;
  Nested := TGocciaObjectValue.Create;
  Nested.CreateDataPropertyOrThrow('x', TGocciaNumberLiteralValue.OneValue);
  Value.CreateDataPropertyOrThrow('nested', Nested);
  Value.CreateDataPropertyOrThrow('custom',
    TGocciaBooleanLiteralValue.TrueValue);
  FFirstSerializer := MakeSerializer(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(RecursiveSerialize,
      'serialize', 6));
  Expect<Boolean>(FFormatting.Serializers.Add(FFirstSerializer)).ToBe(True);
  Expect<string>(FFormatting.Format(Value)).ToBe(
    'Custom(depth=0, indentation=, min=false, compareKeys=undefined, ' +
    'singleQuote=false, quoteKeys=true, maxOutputLength=134217728, ' +
    'outputDepths=0, refs=0, nested={' + LF +
    '  "x": 1,' + LF +
    '})');
end;

procedure TSnapshotFormattingTests.TestSerializerCanLimitRecursiveCollectionWidth;
var
  MapValue: TGocciaMapValue;
  Nested: TGocciaArrayValue;
  ObjectValue, Value: TGocciaObjectValue;
  SetValue: TGocciaSetValue;
begin
  Nested := TGocciaArrayValue.Create;
  Nested.Elements.Add(TGocciaNumberLiteralValue.OneValue);
  Nested.Elements.Add(TGocciaNumberLiteralValue.Create(2));
  Nested.Elements.Add(TGocciaNumberLiteralValue.Create(3));
  Value := TGocciaObjectValue.Create;
  Value.CreateDataPropertyOrThrow('nested', Nested);
  Value.CreateDataPropertyOrThrow('custom',
    TGocciaBooleanLiteralValue.TrueValue);
  FFirstSerializer := MakeSerializer(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(MaxWidthSerialize,
      'serialize', 6));
  Expect<Boolean>(FFormatting.Serializers.Add(FFirstSerializer)).ToBe(True);
  Expect<string>(FFormatting.Format(Value)).ToBe(
    '[' + LF +
    '  1,' + LF +
    '  …(2)' + LF +
    ']');

  ObjectValue := TGocciaObjectValue.Create;
  ObjectValue.CreateDataPropertyOrThrow('a',
    TGocciaNumberLiteralValue.OneValue);
  ObjectValue.CreateDataPropertyOrThrow('b',
    TGocciaNumberLiteralValue.Create(2));
  ObjectValue.CreateDataPropertyOrThrow('c',
    TGocciaNumberLiteralValue.Create(3));
  Value.AssignProperty('nested', ObjectValue);
  Expect<string>(FFormatting.Format(Value)).ToBe(
    '{' + LF +
    '  "a": 1,' + LF +
    '  …(2)' + LF +
    '}');

  MapValue := TGocciaMapValue.Create;
  MapValue.SetEntry(TGocciaStringLiteralValue.Create('a'),
    TGocciaNumberLiteralValue.OneValue);
  MapValue.SetEntry(TGocciaStringLiteralValue.Create('b'),
    TGocciaNumberLiteralValue.Create(2));
  MapValue.SetEntry(TGocciaStringLiteralValue.Create('c'),
    TGocciaNumberLiteralValue.Create(3));
  Value.AssignProperty('nested', MapValue);
  Expect<string>(FFormatting.Format(Value)).ToBe(
    'Map {' + LF +
    '  "a" => 1,' + LF +
    '  …(2)' + LF +
    '}');

  SetValue := TGocciaSetValue.Create;
  SetValue.AddItem(TGocciaNumberLiteralValue.OneValue);
  SetValue.AddItem(TGocciaNumberLiteralValue.Create(2));
  SetValue.AddItem(TGocciaNumberLiteralValue.Create(3));
  Value.AssignProperty('nested', SetValue);
  Expect<string>(FFormatting.Format(Value)).ToBe(
    'Set {' + LF +
    '  1,' + LF +
    '  …(2)' + LF +
    '}');
end;

procedure TSnapshotFormattingTests.TestFormatterCanBeReplaced;
begin
  FFormatting.Formatter := TReplacementSnapshotFormatter.Create;
  Expect<string>(FFormatting.Format(TGocciaNumberLiteralValue.OneValue)).ToBe(
    'replacement');
end;

procedure TSnapshotFormattingTests.TestAsymmetricMatchersUseVitestDisplay;
var
  ArraySample: TGocciaArrayValue;
  ObjectSample: TGocciaObjectValue;
begin
  ArraySample := TGocciaArrayValue.Create;
  ArraySample.Elements.Add(TGocciaNumberLiteralValue.OneValue);
  Expect<string>(FFormatting.Format(
    TGocciaArrayContainingMatcherValue.Create(ArraySample))).ToBe(
    'ArrayContaining [' + LF +
    '  1,' + LF +
    ']');

  ObjectSample := TGocciaObjectValue.Create;
  ObjectSample.CreateDataPropertyOrThrow('x',
    TGocciaNumberLiteralValue.OneValue);
  Expect<string>(FFormatting.Format(
    TGocciaObjectContainingMatcherValue.Create(ObjectSample))).ToBe(
    'ObjectContaining {' + LF +
    '  "x": 1,' + LF +
    '}');
  Expect<string>(FFormatting.Format(
    TGocciaStringContainingMatcherValue.Create(
      TGocciaStringLiteralValue.Create('math')))).ToBe(
    'StringContaining "math"');
  Expect<string>(FFormatting.Format(TGocciaCloseToMatcherValue.Create(
    TGocciaNumberLiteralValue.Create(1.23),
    TGocciaNumberLiteralValue.Create(2)))).ToBe(
    'NumberCloseTo 1.23 (2 digits)');
end;

begin
  TestRunnerProgram.AddSuite(TSnapshotFormattingTests.Create(
    'Snapshot Formatting'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
