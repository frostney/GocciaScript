program Goccia.Values.TypedArrayValue.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SharedArrayBufferValue,
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
    procedure TestStampedGetterIsAnsweredWithoutACall;
    procedure TestBufferDataFollowsAResize;
    procedure TestAGrowKeepsTheBlock;
  end;

procedure TTestTypedArrayValue.SetupTests;
begin
  Test('the %TypedArray%.prototype slot getters carry their intrinsic kind',
    TestSlotGettersCarryTheirIntrinsicKind);
  Test('length, byteLength, byteOffset and buffer are read through the prototype',
    TestSlotNamesAreReadThroughThePrototype);
  Test('a native getter without a slot-getter kind is called',
    TestUnmarkedNativeGetterIsCalled);
  Test('a getter stamped as the length getter is answered from the slots',
    TestStampedGetterIsAnsweredWithoutACall);
  Test('BufferData is the current block after a resize or a detach, read before Length',
    TestBufferDataFollowsAResize);
  Test('a SharedArrayBuffer grows in place: its one block keeps every byte',
    TestAGrowKeepsTheBlock);
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

{ The shortcut itself: a getter that carries nikTypedArrayLength is not called,
  wherever on the chain it is found, and the read yields the element count. A
  regression that stopped taking the shortcut would only cost time, so a script
  could not tell. }
procedure TTestTypedArrayValue.TestStampedGetterIsAnsweredWithoutACall;
var
  Getter: TGocciaNativeFunctionValue;
  Holder, Middle: TGocciaObjectValue;
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  TypedArray: TGocciaTypedArrayValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('typed-array-stamped-getter');
  SetCurrentRealm(Realm);
  try
    FGetterCalls := 0;
    TypedArray := TGocciaTypedArrayValue.Create(takUint16, 5);
    Getter := TGocciaNativeFunctionValue.CreateWithoutPrototype(RecordingGetter,
      'get length', 0);
    Getter.IntrinsicKind := nikTypedArrayLength;
    Holder := TGocciaObjectValue.Create;
    Holder.DefineProperty(PROP_LENGTH, TGocciaPropertyDescriptorAccessor.Create(
      Getter, nil, [pfConfigurable]));
    Middle := TGocciaObjectValue.Create(Holder);
    TypedArray.Prototype := Middle;

    Expect<Double>(TypedArray.GetProperty(PROP_LENGTH)
      .ToNumberLiteral.Value).ToBe(5);
    Expect<Integer>(FGetterCalls).ToBe(0);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

procedure ResizeBuffer(const ABuffer: TGocciaArrayBufferValue;
  const ANewLength: Integer);
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create(
    [TGocciaNumberLiteralValue.Create(ANewLength)]);
  try
    ABuffer.ArrayBufferResize(Args, ABuffer);
  finally
    Args.Free;
  end;
end;

{ resize gives the buffer a new data block. A view that last synced before it
  must not hand a native reader the old one: the reader would copy Length's
  elements, measured on the new block, out of the old, smaller one. Nothing
  reads Length or an element between the resize and BufferData here, so only
  the accessor itself can refresh the block. }
procedure TTestTypedArrayValue.TestBufferDataFollowsAResize;
var
  Buffer: TGocciaArrayBufferValue;
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  Tracking, Fixed: TGocciaTypedArrayValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('typed-array-buffer-data-resize');
  SetCurrentRealm(Realm);
  try
    Buffer := TGocciaArrayBufferValue.Create(4, 4096);
    Tracking := TGocciaTypedArrayValue.Create(takUint8, Buffer);
    Fixed := TGocciaTypedArrayValue.Create(takUint16, Buffer, 0, 2);
    Expect<Integer>(System.Length(Tracking.BufferData)).ToBe(4);

    ResizeBuffer(Buffer, 4096);
    Expect<Integer>(System.Length(Tracking.BufferData)).ToBe(4096);
    Expect<Boolean>(Pointer(Tracking.BufferData) = Pointer(Buffer.Data))
      .ToBe(True);
    Expect<Integer>(Tracking.Length).ToBe(4096);

    ResizeBuffer(Buffer, 2);
    Expect<Integer>(System.Length(Fixed.BufferData)).ToBe(2);
    Expect<Integer>(Fixed.Length).ToBe(0);

    Buffer.Detach;
    Expect<Integer>(System.Length(Tracking.BufferData)).ToBe(0);
    Expect<Integer>(Tracking.Length).ToBe(0);
  finally
    SetCurrentRealm(PreviousRealm);
    Realm.Free;
  end;
end;

{ Every agent that holds a growable SharedArrayBuffer must see the same
  memory before and after a grow, so the buffer is allocated at its maximum
  length once and a grow only raises the length it exposes. A grow that
  moved the bytes to a new block would leave another agent writing into the
  old one. }
procedure TTestTypedArrayValue.TestAGrowKeepsTheBlock;
var
  Args: TGocciaArgumentsCollection;
  Block: TBytes;
  Buffer: TGocciaSharedArrayBufferValue;
  PreviousRealm: TGocciaRealm;
  Realm: TGocciaRealm;
  Tracking, Fixed: TGocciaTypedArrayValue;
begin
  PreviousRealm := CurrentRealm;
  Realm := TGocciaRealm.Create('typed-array-buffer-data-grow');
  SetCurrentRealm(Realm);
  try
    Buffer := TGocciaSharedArrayBufferValue.Create(4, 4096);
    Tracking := TGocciaTypedArrayValue.Create(takInt32, Buffer);
    Fixed := TGocciaTypedArrayValue.Create(takUint8, Buffer, 0, 4);
    Block := Tracking.BufferData;
    Expect<Integer>(System.Length(Block)).ToBe(4096);
    Expect<Integer>(Buffer.ByteLength).ToBe(4);
    Expect<Integer>(Tracking.BufferByteLength).ToBe(4);
    Expect<Integer>(Tracking.Length).ToBe(1);
    Block[0] := 42;

    Args := TGocciaArgumentsCollection.Create(
      [TGocciaNumberLiteralValue.Create(4096)]);
    try
      Buffer.SharedArrayBufferGrow(Args, Buffer);
    finally
      Args.Free;
    end;
    Expect<Boolean>(Pointer(Tracking.BufferData) = Pointer(Block)).ToBe(True);
    Expect<Boolean>(Pointer(Fixed.BufferData) = Pointer(Block)).ToBe(True);
    Expect<Boolean>(Pointer(Buffer.Data) = Pointer(Block)).ToBe(True);
    Expect<Integer>(Buffer.ByteLength).ToBe(4096);
    Expect<Integer>(Tracking.BufferByteLength).ToBe(4096);
    Expect<Integer>(Tracking.Length).ToBe(1024);
    Expect<Integer>(Fixed.Length).ToBe(4);
    Expect<Integer>(Tracking.BufferData[0]).ToBe(42);
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
