program Goccia.Values.TypedArrayValue.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue;

type
  TTestTypedArrayValue = class(TTestSuite)
  private
    FGetterCalls: Integer;
    FGetterReceiver: TGocciaValue;

    function GetterKind(const APrototype: TGocciaObjectValue;
      const AName: string): TGocciaNativeIntrinsicKind;
    function RecordingGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    procedure SetupTests; override;

    procedure TestSlotGettersCarryTheirIntrinsicKind;
    procedure TestSlotNamesAreReadThroughThePrototype;
    procedure TestUnmarkedNativeGetterIsCalled;
  end;

procedure TTestTypedArrayValue.SetupTests;
begin
  Test('the %TypedArray%.prototype slot getters carry their intrinsic kind',
    TestSlotGettersCarryTheirIntrinsicKind);
  Test('length, byteLength, byteOffset and buffer are read through the prototype',
    TestSlotNamesAreReadThroughThePrototype);
  Test('a native getter without a slot-getter kind is called',
    TestUnmarkedNativeGetterIsCalled);
end;

function TTestTypedArrayValue.GetterKind(const APrototype: TGocciaObjectValue;
  const AName: string): TGocciaNativeIntrinsicKind;
var
  Descriptor: TGocciaPropertyDescriptor;
  Getter: TGocciaValue;
begin
  Result := nikNone;
  Descriptor := APrototype.GetOwnPropertyDescriptor(AName);
  if not (Descriptor is TGocciaPropertyDescriptorAccessor) then
    Exit;
  Getter := TGocciaPropertyDescriptorAccessor(Descriptor).Getter;
  if Getter is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(Getter).IntrinsicKind;
end;

function TTestTypedArrayValue.RecordingGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Inc(FGetterCalls);
  FGetterReceiver := AThisValue;
  Result := TGocciaNumberLiteralValue.Create(77);
end;

{ A typed array answers `length`, `byteLength`, `byteOffset` and `buffer` from
  its internal slots only when the accessor its prototype chain resolves to
  carries the matching kind. If the stamp went missing every read would call
  the getter instead: the same value, at several times the cost, which no
  behaviour test can observe. This is the test that can. }
procedure TTestTypedArrayValue.TestSlotGettersCarryTheirIntrinsicKind;
var
  PreviousRealm: TGocciaRealm;
  Prototype: TGocciaObjectValue;
  Realm: TGocciaRealm;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('typed-array-slot-getter-kinds');
  SetCurrentRealm(Realm);
  try
    TGocciaTypedArrayValue.Create(takUint8, 0);
    Prototype := TGocciaTypedArrayValue.GetSharedPrototypeObject;

    Expect<Boolean>(GetterKind(Prototype, PROP_BUFFER) =
      nikTypedArrayBuffer).ToBe(True);
    Expect<Boolean>(GetterKind(Prototype, PROP_BYTE_LENGTH) =
      nikTypedArrayByteLength).ToBe(True);
    Expect<Boolean>(GetterKind(Prototype, PROP_BYTE_OFFSET) =
      nikTypedArrayByteOffset).ToBe(True);
    Expect<Boolean>(GetterKind(Prototype, PROP_LENGTH) =
      nikTypedArrayLength).ToBe(True);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

procedure TTestTypedArrayValue.TestSlotNamesAreReadThroughThePrototype;
var
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  TypedArray: TGocciaTypedArrayValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('typed-array-slot-reads');
  SetCurrentRealm(Realm);
  try
    TypedArray := TGocciaTypedArrayValue.Create(takInt32, 3);

    Expect<Double>(TypedArray.GetProperty(PROP_LENGTH)
      .ToNumberLiteral.Value).ToBe(3);
    Expect<Double>(TypedArray.GetProperty(PROP_BYTE_LENGTH)
      .ToNumberLiteral.Value).ToBe(12);
    Expect<Double>(TypedArray.GetProperty(PROP_BYTE_OFFSET)
      .ToNumberLiteral.Value).ToBe(0);
    Expect<Boolean>(TypedArray.GetProperty(PROP_BUFFER) =
      TypedArray.BufferValue).ToBe(True);
    Expect<Boolean>(TypedArray.HasOwnProperty(PROP_LENGTH)).ToBe(False);

    TypedArray.Prototype := nil;

    Expect<Boolean>(TypedArray.GetProperty(PROP_LENGTH) is
      TGocciaUndefinedLiteralValue).ToBe(True);
    Expect<Boolean>(TypedArray.GetProperty(PROP_BUFFER) is
      TGocciaUndefinedLiteralValue).ToBe(True);
    Expect<Integer>(TypedArray.Length).ToBe(3);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

procedure TTestTypedArrayValue.TestUnmarkedNativeGetterIsCalled;
var
  Holder: TGocciaObjectValue;
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  TypedArray: TGocciaTypedArrayValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('typed-array-unmarked-getter');
  SetCurrentRealm(Realm);
  try
    FGetterCalls := 0;
    FGetterReceiver := nil;
    TypedArray := TGocciaTypedArrayValue.Create(takUint8, 4);
    Holder := TGocciaObjectValue.Create;
    Holder.DefineProperty(PROP_LENGTH, TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(RecordingGetter,
        'get length', 0),
      nil, [pfConfigurable]));
    TypedArray.Prototype := Holder;

    Expect<Double>(TypedArray.GetProperty(PROP_LENGTH)
      .ToNumberLiteral.Value).ToBe(77);
    Expect<Integer>(FGetterCalls).ToBe(1);
    Expect<Boolean>(FGetterReceiver = TypedArray).ToBe(True);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestTypedArrayValue.Create('Typed Array Value'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
