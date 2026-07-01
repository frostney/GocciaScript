unit Goccia.Values.DataViewValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.BinaryData,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SharedArrayBufferValue;

type
  TGocciaDataViewValue = class(TGocciaInstanceValue)
  private
    const AUTO_BYTE_LENGTH = -1;
  private
    FBufferValue: TGocciaValue;
    FBufferData: TBytes;
    FByteOffset: Integer;
    FByteLength: Integer;

    procedure InitializePrototype;
    procedure SyncBufferData;
    function IsDetachedBuffer: Boolean;
    function IsAutoLength: Boolean; inline;
    function IsViewOutOfBounds: Boolean;
    function GetViewByteLength: Integer;
    function GetBufferByteLength: Integer;
    function GetViewValue(const ARequestIndex, ALittleEndian: TGocciaValue;
      const AKind: TGocciaBinaryElementKind): TGocciaValue;
    function SetViewValue(const ARequestIndex, ALittleEndian: TGocciaValue;
      const AKind: TGocciaBinaryElementKind; const AValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AClass: TGocciaClassValue = nil); overload;
    constructor Create(const ABuffer: TGocciaArrayBufferValue;
      const AByteOffset: Integer = 0; const AByteLength: Integer = AUTO_BYTE_LENGTH); overload;
    constructor Create(const ABuffer: TGocciaSharedArrayBufferValue;
      const AByteOffset: Integer = 0; const AByteLength: Integer = AUTO_BYTE_LENGTH); overload;

    function ToStringTag: string; override;
    function UsesECMAScriptBuiltinTagFallback: Boolean; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property BufferValue: TGocciaValue read FBufferValue;
    property ByteOffset: Integer read FByteOffset;
  published
    function DataViewBufferGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewByteOffsetGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function DataViewGetBigInt64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetBigUint64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetFloat16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetFloat32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetFloat64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetInt8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetInt16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetInt32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetUint8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetUint16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewGetUint32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function DataViewSetBigInt64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetBigUint64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetFloat16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetFloat32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetFloat64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetInt8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetInt16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetInt32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetUint8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetUint16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DataViewSetUint32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

var
  GDataViewSharedSlot: TGocciaRealmOwnedSlotId;

function GetDataViewShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GDataViewSharedSlot))
  else
    Result := nil;
end;

function RequireDataView(const AThisValue: TGocciaValue; const AMethodName: string): TGocciaDataViewValue;
begin
  if not (AThisValue is TGocciaDataViewValue) then
    ThrowTypeError(Format(SErrorDataViewRequiresDataView, [AMethodName]),
      SSuggestDataViewThisType);
  Result := TGocciaDataViewValue(AThisValue);
end;

// ES2026 §6.2.4.2 ToIndex(value)
function DataViewToIndex(const AValue: TGocciaValue): Integer;
var
  Num: TGocciaNumberLiteralValue;
  IntegerIndex: Double;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(0);

  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    IntegerIndex := 0
  else if Num.IsInfinite then
  begin
    ThrowRangeError(SErrorInvalidDataViewOffset, SSuggestTypedArrayLength);
    Exit(0);
  end
  else
    IntegerIndex := Trunc(Num.Value);

  if (IntegerIndex < 0) or (IntegerIndex > MAX_SAFE_INTEGER_F) or
     (IntegerIndex > High(Integer)) then
    ThrowRangeError(SErrorInvalidDataViewOffset, SSuggestTypedArrayLength);

  Result := Trunc(IntegerIndex);
end;

function BigIntFromQWord(const AValue: QWord): TBigInteger;
var
  LowPart, HighPart: Int64;
begin
  LowPart := Int64(AValue and $FFFFFFFF);
  HighPart := Int64(AValue shr 32);
  if HighPart = 0 then
    Result := TBigInteger.FromInt64(LowPart)
  else
    Result := TBigInteger.FromInt64(HighPart).Multiply(TBigInteger.FromInt64($100000000)).Add(TBigInteger.FromInt64(LowPart));
end;

// ES2026 §7.1.13 ToBigInt(argument)
function ToBigIntForDataView(const AValue: TGocciaValue): TGocciaBigIntValue;
var
  PrimitiveValue: TGocciaValue;
  StringBigInt: TBigInteger;
begin
  if AValue is TGocciaObjectValue then
  begin
    PrimitiveValue := ToPrimitive(AValue, tphNumber);
    Exit(ToBigIntForDataView(PrimitiveValue));
  end;

  if AValue is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue(AValue));

  if AValue is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(AValue).Value then
      Exit(TGocciaBigIntValue.BigIntOne)
    else
      Exit(TGocciaBigIntValue.BigIntZero);
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    if not TryStringToBigInt(TGocciaStringLiteralValue(AValue).Value, StringBigInt) then
      ThrowSyntaxError(Format(SErrorBigIntInvalidConversion,
        [TGocciaStringLiteralValue(AValue).Value]), SSuggestBigIntNoImplicitConversion);
    Exit(TGocciaBigIntValue.Create(StringBigInt));
  end;

  ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt, SSuggestBigIntTypedArrayValue);
  Result := nil;
end;

function RawBigIntForDataView(const AKind: TGocciaBinaryElementKind;
  const AValue: TGocciaBigIntValue): Int64;
begin
  if AKind = bekBigUint64 then
    Result := AValue.Value.AsUintN(64).ToInt64
  else
    Result := AValue.Value.AsIntN(64).ToInt64;
end;

constructor TGocciaDataViewValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FBufferValue := nil;
  SetLength(FBufferData, 0);
  FByteOffset := 0;
  FByteLength := 0;
  InitializePrototype;
  Shared := GetDataViewShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

constructor TGocciaDataViewValue.Create(const ABuffer: TGocciaArrayBufferValue;
  const AByteOffset: Integer; const AByteLength: Integer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FBufferValue := ABuffer;
  FBufferData := ABuffer.Data;
  FByteOffset := AByteOffset;
  FByteLength := AByteLength;
  InitializePrototype;
  Shared := GetDataViewShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

constructor TGocciaDataViewValue.Create(const ABuffer: TGocciaSharedArrayBufferValue;
  const AByteOffset: Integer; const AByteLength: Integer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FBufferValue := ABuffer;
  FBufferData := ABuffer.Data;
  FByteOffset := AByteOffset;
  FByteLength := AByteLength;
  InitializePrototype;
  Shared := GetDataViewShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaDataViewValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetDataViewShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GDataViewSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor(PROP_BUFFER, DataViewBufferGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_BYTE_LENGTH, DataViewByteLengthGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_BYTE_OFFSET, DataViewByteOffsetGetter, nil, [pfConfigurable]);

    Members.AddMethod(DataViewGetBigInt64, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetBigUint64, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetFloat16, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetFloat32, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetFloat64, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetInt8, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetInt16, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetInt32, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetUint8, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetUint16, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewGetUint32, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);

    Members.AddMethod(DataViewSetBigInt64, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetBigUint64, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetFloat16, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetFloat32, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetFloat64, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetInt8, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetInt16, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetInt32, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetUint8, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetUint16, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(DataViewSetUint32, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);

    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_DATA_VIEW),
      [pfConfigurable]);
    RegisterMemberDefinitions(Shared.Prototype, Members.ToDefinitions);
  finally
    Members.Free;
  end;
end;

class procedure TGocciaDataViewValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetDataViewShared;
  if not Assigned(Shared) then
  begin
    TGocciaDataViewValue.Create;
    Shared := GetDataViewShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

procedure TGocciaDataViewValue.SyncBufferData;
begin
  if FBufferValue is TGocciaArrayBufferValue then
    FBufferData := TGocciaArrayBufferValue(FBufferValue).Data
  else if FBufferValue is TGocciaSharedArrayBufferValue then
    FBufferData := TGocciaSharedArrayBufferValue(FBufferValue).Data;
end;

function TGocciaDataViewValue.IsDetachedBuffer: Boolean;
begin
  Result := (FBufferValue is TGocciaArrayBufferValue) and
    TGocciaArrayBufferValue(FBufferValue).Detached;
end;

function TGocciaDataViewValue.IsAutoLength: Boolean;
begin
  Result := FByteLength = AUTO_BYTE_LENGTH;
end;

function TGocciaDataViewValue.GetBufferByteLength: Integer;
begin
  if IsDetachedBuffer then
    Exit(0);
  SyncBufferData;
  Result := Length(FBufferData);
end;

// ES2026 §25.3.1.4 IsViewOutOfBounds(viewRecord)
function TGocciaDataViewValue.IsViewOutOfBounds: Boolean;
var
  BufferByteLength, ByteOffsetEnd: Integer;
begin
  if IsDetachedBuffer then
    Exit(True);

  BufferByteLength := GetBufferByteLength;
  if IsAutoLength then
    ByteOffsetEnd := BufferByteLength
  else
    ByteOffsetEnd := FByteOffset + FByteLength;

  Result := (FByteOffset > BufferByteLength) or
    (ByteOffsetEnd > BufferByteLength);
end;

// ES2026 §25.3.1.3 GetViewByteLength(viewRecord)
function TGocciaDataViewValue.GetViewByteLength: Integer;
begin
  if IsAutoLength then
    Result := GetBufferByteLength - FByteOffset
  else
    Result := FByteLength;
end;

// ES2026 §25.3.2.1 DataView(buffer [, byteOffset [, byteLength]])
procedure TGocciaDataViewValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  BufferArg, ByteLengthArg: TGocciaValue;
  Offset, ViewByteLength, BufferByteLength: Integer;
  HasByteLength: Boolean;
  BufferIsResizable: Boolean;
begin
  if (AArguments = nil) or (AArguments.Length = 0) then
    ThrowTypeError(SErrorDataViewRequiresArrayBuffer, SSuggestArrayBufferThisType);

  BufferArg := AArguments.GetElement(0);
  if not (BufferArg is TGocciaArrayBufferValue) and
     not (BufferArg is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(SErrorDataViewRequiresArrayBuffer, SSuggestArrayBufferThisType);

  if AArguments.Length > 1 then
    Offset := DataViewToIndex(AArguments.GetElement(1))
  else
    Offset := 0;

  if (BufferArg is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(BufferArg).Detached then
    ThrowTypeError(SErrorCannotUseDetachedDataView, SSuggestArrayBufferDetached);

  FBufferValue := BufferArg;
  SyncBufferData;
  BufferByteLength := Length(FBufferData);
  if Offset > BufferByteLength then
    ThrowRangeError(SErrorInvalidDataViewOffset, SSuggestTypedArrayLength);

  HasByteLength := (AArguments.Length > 2) and
    not (AArguments.GetElement(2) is TGocciaUndefinedLiteralValue);
  BufferIsResizable := (BufferArg is TGocciaArrayBufferValue) and
    (TGocciaArrayBufferValue(BufferArg).MaxByteLength >= 0);

  if HasByteLength then
  begin
    ByteLengthArg := AArguments.GetElement(2);
    ViewByteLength := DataViewToIndex(ByteLengthArg);
    if Int64(Offset) + Int64(ViewByteLength) > Int64(BufferByteLength) then
      ThrowRangeError(SErrorInvalidDataViewLength, SSuggestTypedArrayLength);
  end
  else if BufferIsResizable then
    ViewByteLength := AUTO_BYTE_LENGTH
  else
    ViewByteLength := BufferByteLength - Offset;

  FByteOffset := Offset;
  FByteLength := ViewByteLength;

  if IsDetachedBuffer then
    ThrowTypeError(SErrorCannotUseDetachedDataView, SSuggestArrayBufferDetached);
  if IsViewOutOfBounds then
    ThrowRangeError(SErrorInvalidDataViewLength, SSuggestTypedArrayLength);
end;

function TGocciaDataViewValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_DATA_VIEW;
end;

function TGocciaDataViewValue.UsesECMAScriptBuiltinTagFallback: Boolean;
begin
  Result := True;
end;

procedure TGocciaDataViewValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FBufferValue) and not FBufferValue.GCMarked then
    FBufferValue.MarkReferences;
end;

// ES2026 §25.3.1.5 GetViewValue(view, requestIndex, isLittleEndian, type)
function TGocciaDataViewValue.GetViewValue(const ARequestIndex, ALittleEndian: TGocciaValue;
  const AKind: TGocciaBinaryElementKind): TGocciaValue;
var
  Index, ViewSize, ElementSize, BufferIndex: Integer;
  IsLittleEndian: Boolean;
  RawBigInt: Int64;
begin
  Index := DataViewToIndex(ARequestIndex);
  IsLittleEndian := Assigned(ALittleEndian) and ALittleEndian.ToBooleanLiteral.Value;

  if IsViewOutOfBounds then
    ThrowTypeError(SErrorCannotUseDetachedDataView, SSuggestArrayBufferDetached);

  ViewSize := GetViewByteLength;
  ElementSize := BinaryBytesPerElement(AKind);
  if Int64(Index) + Int64(ElementSize) > Int64(ViewSize) then
    ThrowRangeError(SErrorInvalidDataViewOffset, SSuggestTypedArrayLength);

  BufferIndex := Index + FByteOffset;
  SyncBufferData;
  if BinaryIsBigIntElement(AKind) then
  begin
    RawBigInt := ReadBinaryBigIntElement(FBufferData, BufferIndex, AKind,
      IsLittleEndian);
    if AKind = bekBigUint64 then
      Result := TGocciaBigIntValue.Create(BigIntFromQWord(QWord(RawBigInt)))
    else
      Result := TGocciaBigIntValue.Create(TBigInteger.FromInt64(RawBigInt));
  end
  else
    Result := TGocciaNumberLiteralValue.Create(
      ReadBinaryNumberElement(FBufferData, BufferIndex, AKind, IsLittleEndian));
end;

// ES2026 §25.3.1.6 SetViewValue(view, requestIndex, isLittleEndian, type, value)
function TGocciaDataViewValue.SetViewValue(const ARequestIndex, ALittleEndian: TGocciaValue;
  const AKind: TGocciaBinaryElementKind; const AValue: TGocciaValue): TGocciaValue;
var
  Index, ViewSize, ElementSize, BufferIndex: Integer;
  IsLittleEndian: Boolean;
  NumberValue: TGocciaNumberLiteralValue;
  BigIntRaw: Int64;
begin
  // Immutable ArrayBuffers proposal, SetViewValue step 3: reject writes to an
  // immutable buffer before ToIndex, the value coercion, IsViewOutOfBounds, and
  // the RangeError bounds check. So an out-of-range write to an immutable buffer
  // still throws TypeError (immutable wins over RangeError) and the value's
  // valueOf is not observed. (TypedArray's [[Set]] coerces first, by contrast.)
  if (FBufferValue is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(FBufferValue).Immutable then
    ThrowTypeError('DataView cannot write to an immutable ArrayBuffer');

  Index := DataViewToIndex(ARequestIndex);
  NumberValue := nil;
  BigIntRaw := 0;
  if BinaryIsBigIntElement(AKind) then
    BigIntRaw := RawBigIntForDataView(AKind, ToBigIntForDataView(AValue))
  else
    NumberValue := AValue.ToNumberLiteral;

  IsLittleEndian := Assigned(ALittleEndian) and ALittleEndian.ToBooleanLiteral.Value;

  if IsViewOutOfBounds then
    ThrowTypeError(SErrorCannotUseDetachedDataView, SSuggestArrayBufferDetached);

  ViewSize := GetViewByteLength;
  ElementSize := BinaryBytesPerElement(AKind);
  if Int64(Index) + Int64(ElementSize) > Int64(ViewSize) then
    ThrowRangeError(SErrorInvalidDataViewOffset, SSuggestTypedArrayLength);

  BufferIndex := Index + FByteOffset;
  SyncBufferData;
  if BinaryIsBigIntElement(AKind) then
    WriteBinaryBigIntElement(FBufferData, BufferIndex, BigIntRaw, IsLittleEndian)
  else
    WriteBinaryNumberElement(FBufferData, BufferIndex, AKind,
      NumberValue.Value, IsLittleEndian);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaDataViewValue.DataViewBufferGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.buffer').FBufferValue;
end;

function TGocciaDataViewValue.DataViewByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TGocciaDataViewValue;
begin
  View := RequireDataView(AThisValue, 'DataView.prototype.byteLength');
  if View.IsViewOutOfBounds then
    ThrowTypeError(SErrorCannotUseDetachedDataView, SSuggestArrayBufferDetached);
  Result := TGocciaNumberLiteralValue.Create(View.GetViewByteLength);
end;

function TGocciaDataViewValue.DataViewByteOffsetGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TGocciaDataViewValue;
begin
  View := RequireDataView(AThisValue, 'DataView.prototype.byteOffset');
  if View.IsViewOutOfBounds then
    ThrowTypeError(SErrorCannotUseDetachedDataView, SSuggestArrayBufferDetached);
  Result := TGocciaNumberLiteralValue.Create(View.FByteOffset);
end;

function TGocciaDataViewValue.DataViewGetBigInt64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getBigInt64').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekBigInt64);
end;

function TGocciaDataViewValue.DataViewGetBigUint64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getBigUint64').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekBigUint64);
end;

function TGocciaDataViewValue.DataViewGetFloat16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getFloat16').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekFloat16);
end;

function TGocciaDataViewValue.DataViewGetFloat32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getFloat32').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekFloat32);
end;

function TGocciaDataViewValue.DataViewGetFloat64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getFloat64').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekFloat64);
end;

function TGocciaDataViewValue.DataViewGetInt8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getInt8').GetViewValue(
    AArgs.GetElement(0), TGocciaBooleanLiteralValue.TrueValue, bekInt8);
end;

function TGocciaDataViewValue.DataViewGetInt16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getInt16').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekInt16);
end;

function TGocciaDataViewValue.DataViewGetInt32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getInt32').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekInt32);
end;

function TGocciaDataViewValue.DataViewGetUint8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getUint8').GetViewValue(
    AArgs.GetElement(0), TGocciaBooleanLiteralValue.TrueValue, bekUint8);
end;

function TGocciaDataViewValue.DataViewGetUint16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getUint16').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekUint16);
end;

function TGocciaDataViewValue.DataViewGetUint32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.getUint32').GetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(1), bekUint32);
end;

function TGocciaDataViewValue.DataViewSetBigInt64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setBigInt64').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekBigInt64, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetBigUint64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setBigUint64').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekBigUint64, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetFloat16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setFloat16').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekFloat16, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetFloat32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setFloat32').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekFloat32, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetFloat64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setFloat64').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekFloat64, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetInt8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setInt8').SetViewValue(
    AArgs.GetElement(0), TGocciaBooleanLiteralValue.TrueValue, bekInt8, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetInt16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setInt16').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekInt16, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetInt32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setInt32').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekInt32, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetUint8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setUint8').SetViewValue(
    AArgs.GetElement(0), TGocciaBooleanLiteralValue.TrueValue, bekUint8, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetUint16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setUint16').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekUint16, AArgs.GetElement(1));
end;

function TGocciaDataViewValue.DataViewSetUint32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireDataView(AThisValue, 'DataView.prototype.setUint32').SetViewValue(
    AArgs.GetElement(0), AArgs.GetElement(2), bekUint32, AArgs.GetElement(1));
end;

initialization
  GDataViewSharedSlot := RegisterRealmOwnedSlot('DataView.shared');

end.
