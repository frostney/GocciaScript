unit Goccia.Values.TypedArrayValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SharedArrayBufferValue;

type
  TGocciaTypedArrayKind = (
    takInt8, takUint8, takUint8Clamped,
    takInt16, takUint16,
    takInt32, takUint32,
    takFloat16, takFloat32, takFloat64,
    takBigInt64, takBigUint64
  );

  TGocciaTypedArrayValue = class(TGocciaInstanceValue)
  private
    FBufferValue: TGocciaValue;
    FBufferData: TBytes;
    FByteOffset: Integer;
    FLength: Integer;
    FKind: TGocciaTypedArrayKind;
    FAutoLength: Boolean;

    procedure InitializePrototype;
    procedure SyncBufferData;
    function GetLength: Integer;
    function HasValidBackingRange(const ALength: Integer): Boolean;
    function HasValidElementIndex(const AIndex: Integer): Boolean;

    function ReadElementUnchecked(const AIndex: Integer): Double;
    function ReadElement(const AIndex: Integer): Double;
    procedure WriteElementUnchecked(const AIndex: Integer; const AValue: Double);
    procedure WriteElement(const AIndex: Integer; const AValue: Double);
    procedure WriteNumberLiteral(const AIndex: Integer; const ANum: TGocciaNumberLiteralValue);

    function ReadBigIntElement(const AIndex: Integer): Int64;
    procedure WriteBigIntElement(const AIndex: Integer; const AValue: Int64);

    function GetElementAsValue(const AIndex: Integer): TGocciaValue;
    procedure WriteValueToElement(const AIndex: Integer; const AValue: TGocciaValue);
    function IsValidIntegerIndexedElement(const ANumericIndex: Double;
      const AIsNegativeZero: Boolean; out AIndex: Integer): Boolean;
    function OrdinarySetIntegerIndexedElementWithReceiver(const AName: string;
      const ANumericIndex: Double; const AIsNegativeZero: Boolean;
      const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean;
    procedure SetIntegerIndexedElement(const ANumericIndex: Double;
      const AIsNegativeZero: Boolean; const AValue: TGocciaValue);

  public
    constructor Create(const AKind: TGocciaTypedArrayKind; const ALength: Integer); overload;
    constructor Create(const AKind: TGocciaTypedArrayKind; const ABuffer: TGocciaArrayBufferValue;
      const AByteOffset: Integer = 0; const ALength: Integer = -1); overload;
    constructor Create(const AKind: TGocciaTypedArrayKind; const ASharedBuffer: TGocciaSharedArrayBufferValue;
      const AByteOffset: Integer = 0; const ALength: Integer = -1); overload;

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); override;
    function AssignPropertyWithReceiver(const AName: string; const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean; override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    function DeleteProperty(const AName: string): Boolean; override;
    function HasProperty(const AName: string): Boolean; override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    class function BytesPerElement(const AKind: TGocciaTypedArrayKind): Integer;
    class function IsFloatKind(const AKind: TGocciaTypedArrayKind): Boolean; inline;
    class function IsBigIntKind(const AKind: TGocciaTypedArrayKind): Boolean; inline;
    class function KindName(const AKind: TGocciaTypedArrayKind): string;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class procedure SetSharedPrototypeParent(const AParent: TGocciaObjectValue);
    class function GetSharedPrototypeObject: TGocciaObjectValue; static;
    class function GetSharedPrototypeObjectForRealm(const ARealm: TGocciaRealm): TGocciaObjectValue; static;
    class procedure SetUint8Prototype(const APrototype: TGocciaObjectValue);

    property BufferValue: TGocciaValue read FBufferValue;
    property BufferData: TBytes read FBufferData;
    property ByteOffset: Integer read FByteOffset;
    property Length: Integer read GetLength;
    property Kind: TGocciaTypedArrayKind read FKind;

    // Boxing-free element fast paths for the bytecode VM computed-access cores.
    // TryReadIndexedScalar yields the element as a raw Double for non-BigInt kinds
    // and a valid in-range index; it returns False (caller falls back to GetProperty)
    // for BigInt kinds and out-of-range indices. TryWriteIndexedScalar stores an
    // already-numeric scalar value (ToNumber on a Number is side-effect-free, so the
    // observable conversion the spec requires is preserved) with the same coercion as
    // WriteNumberLiteral; it returns False for BigInt kinds so the caller takes the
    // throwing slow path, and True (handled) for non-BigInt kinds whether or not the
    // index is in range or the backing buffer is immutable.
    function TryReadIndexedScalar(const AIndex: Integer; out AValue: Double): Boolean;
    function TryWriteIndexedScalar(const AIndex: Integer; const AValue: Double): Boolean;
  published
    function TypedArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArraySubarray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArraySet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayReduceRight(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayBufferGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayByteOffsetGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayToStringTagGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaTypedArrayClassValue = class(TGocciaClassValue)
  private
    FKind: TGocciaTypedArrayKind;
  public
    constructor Create(const AName: string; const ASuperClass: TGocciaClassValue; const AKind: TGocciaTypedArrayKind);
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function NativeInstanceDefaultPrototype: TGocciaObjectValue; override;
    function DefaultPrototypeForNewTarget(const ANewTarget: TGocciaValue;
      const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
    function GetClassLength: Integer; override;
    property Kind: TGocciaTypedArrayKind read FKind;
  end;

  TGocciaTypedArrayStaticFrom = class
  public
    constructor Create;
    function TypedArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

function IsTypedArrayIteratorValue(const AObject: TGocciaObjectValue): Boolean;

implementation

uses
  Math,

  Goccia.Arithmetic,
  Goccia.BinaryData,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Scope,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.Values.ToPrimitive;

var
  GTypedArraySharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FUint8Prototype: TGocciaObjectValue;

procedure ClearThreadvarMembers;
begin
  SetLength(FPrototypeMembers, 0);
end;

const
  TYPED_ARRAY_LITTLE_ENDIAN = True;

type
  TGocciaTypedArrayIteratorKind = (taikValues, taikKeys, taikEntries);

  TGocciaTypedArrayIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaTypedArrayValue;
    FIndex: Integer;
    FKind: TGocciaTypedArrayIteratorKind;
    function CurrentLengthOrThrow: Integer;
  public
    constructor Create(const ASource: TGocciaTypedArrayValue;
      const AKind: TGocciaTypedArrayIteratorKind);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

function IsTypedArrayIteratorValue(const AObject: TGocciaObjectValue): Boolean;
begin
  Result := AObject is TGocciaTypedArrayIteratorValue;
end;

function GetTypedArrayShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTypedArraySharedSlot))
  else
    Result := nil;
end;

function IsTypedArrayOutOfBounds(const AArray: TGocciaTypedArrayValue): Boolean; forward;

function IsTypedArrayBackedByImmutableArrayBuffer(
  const AArray: TGocciaTypedArrayValue): Boolean; forward;

function RequireTypedArrayConstructor(
  const AThisValue: TGocciaValue;
  const AMethod: string): TGocciaClassValue;
var
  ClassValue: TGocciaClassValue;
begin
  if AThisValue is TGocciaTypedArrayClassValue then
    Exit(TGocciaClassValue(AThisValue));

  if AThisValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(AThisValue);
    while Assigned(ClassValue) do
    begin
      if ClassValue is TGocciaTypedArrayClassValue then
        Exit(TGocciaClassValue(AThisValue));
      ClassValue := ClassValue.SuperClass;
    end;
  end;

  ThrowTypeError(Format(SErrorTypedArrayStaticReceiver, [AMethod]),
    SSuggestTypedArrayConstructorReceiver);
  Result := nil;
end;

function CreateTypedArrayFromConstructor(const AConstructor: TGocciaValue;
  const AMethod: string; const ALength: Integer): TGocciaTypedArrayValue;
var
  ConstructArgs: TGocciaArgumentsCollection;
  Constructed: TGocciaValue;
begin
  ConstructArgs := TGocciaArgumentsCollection.Create;
  try
    ConstructArgs.Add(TGocciaNumberLiteralValue.Create(ALength));
    Constructed := ConstructValue(AConstructor, ConstructArgs, AConstructor);
  finally
    ConstructArgs.Free;
  end;

  if not (Constructed is TGocciaTypedArrayValue) then
    ThrowTypeError(Format(SErrorTypedArrayRequiresTypedArray, [AMethod]),
      SSuggestTypedArrayConstructorReceiver);

  Result := TGocciaTypedArrayValue(Constructed);
  if IsTypedArrayOutOfBounds(Result) or (Result.Length < ALength) then
    ThrowTypeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
  if IsTypedArrayBackedByImmutableArrayBuffer(Result) then
    ThrowTypeError('TypedArray constructor returned an immutable ArrayBuffer-backed view',
      SSuggestTypedArrayLength);
end;

function ToBinaryElementKind(const AKind: TGocciaTypedArrayKind): TGocciaBinaryElementKind;
begin
  case AKind of
    takInt8:
      Result := bekInt8;
    takUint8:
      Result := bekUint8;
    takUint8Clamped:
      Result := bekUint8Clamped;
    takInt16:
      Result := bekInt16;
    takUint16:
      Result := bekUint16;
    takInt32:
      Result := bekInt32;
    takUint32:
      Result := bekUint32;
    takFloat16:
      Result := bekFloat16;
    takFloat32:
      Result := bekFloat32;
    takFloat64:
      Result := bekFloat64;
    takBigInt64:
      Result := bekBigInt64;
    takBigUint64:
      Result := bekBigUint64;
  else
    Result := bekUint8;
  end;
end;

class function TGocciaTypedArrayValue.BytesPerElement(const AKind: TGocciaTypedArrayKind): Integer;
begin
  Result := BinaryBytesPerElement(ToBinaryElementKind(AKind));
end;

class function TGocciaTypedArrayValue.IsFloatKind(const AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := BinaryIsFloatElement(ToBinaryElementKind(AKind));
end;

class function TGocciaTypedArrayValue.IsBigIntKind(const AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := BinaryIsBigIntElement(ToBinaryElementKind(AKind));
end;

class function TGocciaTypedArrayValue.KindName(const AKind: TGocciaTypedArrayKind): string;
begin
  case AKind of
    takInt8: Result := CONSTRUCTOR_INT8_ARRAY;
    takUint8: Result := CONSTRUCTOR_UINT8_ARRAY;
    takUint8Clamped: Result := CONSTRUCTOR_UINT8_CLAMPED_ARRAY;
    takInt16: Result := CONSTRUCTOR_INT16_ARRAY;
    takUint16: Result := CONSTRUCTOR_UINT16_ARRAY;
    takInt32: Result := CONSTRUCTOR_INT32_ARRAY;
    takUint32: Result := CONSTRUCTOR_UINT32_ARRAY;
    takFloat16: Result := CONSTRUCTOR_FLOAT16_ARRAY;
    takFloat32: Result := CONSTRUCTOR_FLOAT32_ARRAY;
    takFloat64: Result := CONSTRUCTOR_FLOAT64_ARRAY;
    takBigInt64: Result := CONSTRUCTOR_BIGINT64_ARRAY;
    takBigUint64: Result := CONSTRUCTOR_BIGUINT64_ARRAY;
  else
    Result := 'TypedArray';
  end;
end;

procedure TGocciaTypedArrayValue.SyncBufferData;
begin
  if FBufferValue is TGocciaArrayBufferValue then
    FBufferData := TGocciaArrayBufferValue(FBufferValue).Data
  else if FBufferValue is TGocciaSharedArrayBufferValue then
    FBufferData := TGocciaSharedArrayBufferValue(FBufferValue).Data;
end;

function TGocciaTypedArrayValue.GetLength: Integer;
var
  BPE: Integer;
  AvailableBytes: Integer;
begin
  if (FBufferValue is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(FBufferValue).Detached then
    Exit(0);

  SyncBufferData;
  if FAutoLength then
  begin
    BPE := BytesPerElement(FKind);
    AvailableBytes := System.Length(FBufferData) - FByteOffset;
    if AvailableBytes <= 0 then
      Exit(0);
    Exit(AvailableBytes div BPE);
  end;

  if not HasValidBackingRange(FLength) then
    Exit(0);
  Result := FLength;
end;

function TGocciaTypedArrayValue.HasValidBackingRange(const ALength: Integer): Boolean;
var
  RequiredBytes: Int64;
begin
  if (FBufferValue is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(FBufferValue).Detached then
    Exit(False);

  SyncBufferData;
  RequiredBytes := Int64(FByteOffset) + Int64(ALength) * Int64(BytesPerElement(FKind));
  Result := RequiredBytes <= System.Length(FBufferData);
end;

function TGocciaTypedArrayValue.HasValidElementIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < GetLength);
end;

{ Element read/write via buffer }

function TGocciaTypedArrayValue.ReadElementUnchecked(const AIndex: Integer): Double;
var
  Offset: Integer;
begin
  // Precondition: AIndex is in range (the caller validated HasValidElementIndex).
  // One sync + read, with no redundant bounds re-check on the hot element path.
  SyncBufferData;
  Offset := FByteOffset + AIndex * BytesPerElement(FKind);
  Result := ReadBinaryNumberElement(FBufferData, Offset,
    ToBinaryElementKind(FKind), TYPED_ARRAY_LITTLE_ENDIAN);
end;

function TGocciaTypedArrayValue.ReadElement(const AIndex: Integer): Double;
begin
  if not HasValidElementIndex(AIndex) then
    Exit(0);
  Result := ReadElementUnchecked(AIndex);
end;

procedure TGocciaTypedArrayValue.WriteElementUnchecked(const AIndex: Integer; const AValue: Double);
var
  Offset: Integer;
begin
  // Precondition: AIndex is in range (the caller validated HasValidElementIndex).
  // Integer coercion of the ToNumber result — non-finite -> 0 for integer kinds,
  // Uint8Clamped clamping +Infinity to 255, float kinds verbatim — is performed by
  // WriteBinaryNumberElement, so it is not repeated here. One sync + write.
  SyncBufferData;
  Offset := FByteOffset + AIndex * BytesPerElement(FKind);
  WriteBinaryNumberElement(FBufferData, Offset, ToBinaryElementKind(FKind),
    AValue, TYPED_ARRAY_LITTLE_ENDIAN);
end;

procedure TGocciaTypedArrayValue.WriteElement(const AIndex: Integer; const AValue: Double);
begin
  if not HasValidElementIndex(AIndex) then
    Exit;
  WriteElementUnchecked(AIndex, AValue);
end;

procedure TGocciaTypedArrayValue.WriteNumberLiteral(const AIndex: Integer; const ANum: TGocciaNumberLiteralValue);
begin
  if not HasValidElementIndex(AIndex) then
    Exit;
  WriteElementUnchecked(AIndex, ANum.Value);
end;

function TGocciaTypedArrayValue.TryReadIndexedScalar(const AIndex: Integer; out AValue: Double): Boolean;
begin
  // BigInt kinds yield TGocciaBigIntValue, never a Double, so they fall back to the
  // boxed path; an out-of-range index falls back so the caller yields `undefined`.
  if IsBigIntKind(FKind) or (not HasValidElementIndex(AIndex)) then
    Exit(False);
  AValue := ReadElementUnchecked(AIndex);
  Result := True;
end;

function TGocciaTypedArrayValue.TryWriteIndexedScalar(const AIndex: Integer; const AValue: Double): Boolean;
begin
  // A Number value into a BigInt typed array must throw (ToBigInt(Number) throws), so
  // signal not-handled and let the caller take the boxed, throwing slow path.
  if IsBigIntKind(FKind) then
    Exit(False);
  // Non-BigInt integer-indexed [[Set]] is always "handled": an out-of-range index is
  // ignored and an immutable backing buffer skips the store, both reporting success
  // per ES2026 10.4.5.9 / the Immutable ArrayBuffers proposal.
  Result := True;
  if not HasValidElementIndex(AIndex) then
    Exit;
  if IsTypedArrayBackedByImmutableArrayBuffer(Self) then
    Exit;
  WriteElementUnchecked(AIndex, AValue);
end;

function TGocciaTypedArrayValue.ReadBigIntElement(const AIndex: Integer): Int64;
var
  Offset: Integer;
begin
  if not HasValidElementIndex(AIndex) then
    Exit(0);

  SyncBufferData;
  Offset := FByteOffset + AIndex * 8;
  Result := ReadBinaryBigIntElement(FBufferData, Offset,
    ToBinaryElementKind(FKind), TYPED_ARRAY_LITTLE_ENDIAN);
end;

procedure TGocciaTypedArrayValue.WriteBigIntElement(const AIndex: Integer; const AValue: Int64);
var
  Offset: Integer;
begin
  if not HasValidElementIndex(AIndex) then
    Exit;

  SyncBufferData;
  Offset := FByteOffset + AIndex * 8;
  WriteBinaryBigIntElement(FBufferData, Offset, AValue,
    TYPED_ARRAY_LITTLE_ENDIAN);
end;

function BigIntFromQWord(const AValue: QWord): TBigInteger;
var
  Lo, Hi: Int64;
begin
  Lo := Int64(AValue and $FFFFFFFF);
  Hi := Int64(AValue shr 32);
  if Hi = 0 then
    Result := TBigInteger.FromInt64(Lo)
  else
    Result := TBigInteger.FromInt64(Hi).Multiply(TBigInteger.FromInt64($100000000)).Add(TBigInteger.FromInt64(Lo));
end;

function RawBigIntForKind(const AKind: TGocciaTypedArrayKind;
  const AValue: TGocciaBigIntValue): Int64;
begin
  if AKind = takBigUint64 then
    Result := AValue.Value.AsUintN(64).ToInt64
  else
    Result := AValue.Value.AsIntN(64).ToInt64;
end;

function ToBigIntForTypedArray(const AValue: TGocciaValue): TGocciaBigIntValue;
var
  Prim: TGocciaValue;
  StringBigInt: TBigInteger;
begin
  if AValue is TGocciaObjectValue then
  begin
    Prim := ToPrimitive(AValue, tphNumber);
    Exit(ToBigIntForTypedArray(Prim));
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

// Hot path for CanonicalNumericIndexString: a non-negative integer string with
// no leading zeros (e.g. "0", "1234") is canonical by construction, because
// ToString(ToNumber(s)) reproduces s exactly. Up to 15 digits stays below 2^53,
// so the Double is exact and Number::toString uses plain (non-exponential)
// notation, guaranteeing the round-trip. Anything else (signs, leading zeros,
// fractions, exponents, "-0", out-of-range) returns False and defers to the
// allocation-heavy String->Number->String validation in the caller.
function TryFastCanonicalIntegerIndex(const AName: string;
  out ANumericIndex: Double): Boolean;
var
  Len, I: Integer;
  Acc: Int64;
  C: Char;
begin
  Len := Length(AName);
  if (Len < 1) or (Len > 15) then
    Exit(False);

  C := AName[1];
  if (C < '1') or (C > '9') then
  begin
    if (Len = 1) and (C = '0') then
    begin
      ANumericIndex := 0;
      Exit(True);
    end;
    Exit(False);
  end;

  Acc := Ord(C) - Ord('0');
  for I := 2 to Len do
  begin
    C := AName[I];
    if (C < '0') or (C > '9') then
      Exit(False);
    Acc := Acc * 10 + (Ord(C) - Ord('0'));
  end;

  ANumericIndex := Acc;
  Result := True;
end;

function TryCanonicalNumericIndexString(const AName: string;
  out ANumericIndex: Double; out AIsNegativeZero: Boolean): Boolean;
var
  NumberValue: TGocciaNumberLiteralValue;
begin
  AIsNegativeZero := False;
  if TryFastCanonicalIntegerIndex(AName, ANumericIndex) then
    Exit(True);
  if AName = '-0' then
  begin
    ANumericIndex := 0;
    AIsNegativeZero := True;
    Exit(True);
  end;

  NumberValue := TGocciaStringLiteralValue.Create(AName).ToNumberLiteral;
  ANumericIndex := NumberValue.Value;
  Result := NumberValue.ToStringLiteral.Value = AName;
end;

function TGocciaTypedArrayValue.GetElementAsValue(const AIndex: Integer): TGocciaValue;
var
  Raw: Int64;
begin
  if not HasValidElementIndex(AIndex) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if IsBigIntKind(FKind) then
  begin
    Raw := ReadBigIntElement(AIndex);
    if FKind = takBigUint64 then
      Result := TGocciaBigIntValue.Create(BigIntFromQWord(QWord(Raw)))
    else
      Result := TGocciaBigIntValue.Create(TBigInteger.FromInt64(Raw));
  end
  else
    Result := TGocciaNumberLiteralValue.Create(ReadElement(AIndex));
end;

procedure TGocciaTypedArrayValue.WriteValueToElement(const AIndex: Integer; const AValue: TGocciaValue);
begin
  if IsBigIntKind(FKind) then
    WriteBigIntElement(AIndex, RawBigIntForKind(FKind, ToBigIntForTypedArray(AValue)))
  else
    WriteNumberLiteral(AIndex, AValue.ToNumberLiteral);
end;

function TGocciaTypedArrayValue.IsValidIntegerIndexedElement(
  const ANumericIndex: Double; const AIsNegativeZero: Boolean;
  out AIndex: Integer): Boolean;
begin
  AIndex := -1;
  if AIsNegativeZero or Math.IsNaN(ANumericIndex) or Math.IsInfinite(ANumericIndex) or
     (Frac(ANumericIndex) <> 0.0) or (ANumericIndex < 0) or
     (ANumericIndex > High(Integer)) then
    Exit(False);

  AIndex := Trunc(ANumericIndex);
  if (AIndex < 0) or (AIndex >= GetLength) then
    Exit(False);

  Result := True;
end;

// ES2026 §10.4.5.9 [[Set]](P, V, Receiver), integer-indexed branch.
procedure TGocciaTypedArrayValue.SetIntegerIndexedElement(const ANumericIndex: Double;
  const AIsNegativeZero: Boolean; const AValue: TGocciaValue);
var
  Index: Integer;
  NumberValue: TGocciaNumberLiteralValue;
  BigIntRaw: Int64;
begin
  // ES2026 §10.4.5.16 TypedArraySetElement step 3: conversion is observable
  // and happens before the valid-index check.
  if IsBigIntKind(FKind) then
    BigIntRaw := RawBigIntForKind(FKind, ToBigIntForTypedArray(AValue))
  else
    NumberValue := AValue.ToNumberLiteral;

  if not IsValidIntegerIndexedElement(ANumericIndex, AIsNegativeZero, Index) then
    Exit;

  // Immutable ArrayBuffers proposal: TypedArraySetElement performs the
  // observable numeric conversion above but skips the store when the backing
  // buffer is immutable; integer-indexed [[Set]] still reports success.
  if IsTypedArrayBackedByImmutableArrayBuffer(Self) then
    Exit;

  if IsBigIntKind(FKind) then
    WriteBigIntElement(Index, BigIntRaw)
  else
    WriteNumberLiteral(Index, NumberValue);
end;

function TGocciaTypedArrayValue.OrdinarySetIntegerIndexedElementWithReceiver(
  const AName: string; const ANumericIndex: Double;
  const AIsNegativeZero: Boolean; const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
var
  Index: Integer;
  ReceiverDesc: TGocciaPropertyDescriptor;
  ReceiverObj: TGocciaObjectValue;
  ReceiverTypedArray: TGocciaTypedArrayValue;
begin
  if not IsValidIntegerIndexedElement(ANumericIndex, AIsNegativeZero, Index) then
    Exit(True);

  if not (AReceiver is TGocciaObjectValue) then
    Exit(False);

  ReceiverObj := TGocciaObjectValue(AReceiver);
  if ReceiverObj is TGocciaTypedArrayValue then
  begin
    ReceiverTypedArray := TGocciaTypedArrayValue(ReceiverObj);
    if not ReceiverTypedArray.IsValidIntegerIndexedElement(ANumericIndex,
      AIsNegativeZero, Index) then
      Exit(False);
    ReceiverTypedArray.SetIntegerIndexedElement(ANumericIndex,
      AIsNegativeZero, AValue);
    Exit(True);
  end;

  ReceiverDesc := ReceiverObj.GetOwnPropertyDescriptor(AName);
  if Assigned(ReceiverDesc) then
  begin
    if (ReceiverDesc is TGocciaPropertyDescriptorAccessor) or
       (not ReceiverDesc.Writable) then
      Exit(False);
    Exit(ReceiverObj.TryDefineProperty(AName,
      TGocciaPropertyDescriptorData.Create(AValue, ReceiverDesc.Flags)));
  end;

  Result := ReceiverObj.TryDefineProperty(AName,
    TGocciaPropertyDescriptorData.Create(AValue,
      [pfEnumerable, pfConfigurable, pfWritable]));
end;

{ Constructors }

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind; const ALength: Integer);
var
  ByteLen: Integer;
  ByteLen64: Int64;
  Buf: TGocciaArrayBufferValue;
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FKind := AKind;
  FByteOffset := 0;
  FLength := ALength;
  FAutoLength := False;
  ByteLen64 := Int64(ALength) * Int64(BytesPerElement(AKind));
  if (ALength < 0) or (ByteLen64 > High(Integer)) then
    ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
  ByteLen := Integer(ByteLen64);
  Buf := TGocciaArrayBufferValue.Create(ByteLen);
  FBufferValue := Buf;
  FBufferData := Buf.Data;
  InitializePrototype;
  Shared := GetTypedArrayShared;
  if (AKind = takUint8) and Assigned(FUint8Prototype) then
    FPrototype := FUint8Prototype
  else if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind;
  const ABuffer: TGocciaArrayBufferValue; const AByteOffset: Integer; const ALength: Integer);
var
  BPE: Integer;
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FKind := AKind;
  FBufferValue := ABuffer;
  FBufferData := ABuffer.Data;
  FByteOffset := AByteOffset;
  BPE := BytesPerElement(AKind);
  FAutoLength := (ALength < 0) and (ABuffer.MaxByteLength >= 0);

  if ALength >= 0 then
    FLength := ALength
  else
    FLength := (System.Length(ABuffer.Data) - AByteOffset) div BPE;

  InitializePrototype;
  Shared := GetTypedArrayShared;
  if (AKind = takUint8) and Assigned(FUint8Prototype) then
    FPrototype := FUint8Prototype
  else if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind;
  const ASharedBuffer: TGocciaSharedArrayBufferValue; const AByteOffset: Integer; const ALength: Integer);
var
  BPE: Integer;
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FKind := AKind;
  FBufferValue := ASharedBuffer;
  FBufferData := ASharedBuffer.Data;
  FByteOffset := AByteOffset;
  BPE := BytesPerElement(AKind);
  FAutoLength := False;

  if ALength >= 0 then
    FLength := ALength
  else
    FLength := (System.Length(ASharedBuffer.Data) - AByteOffset) div BPE;

  InitializePrototype;
  Shared := GetTypedArrayShared;
  if (AKind = takUint8) and Assigned(FUint8Prototype) then
    FPrototype := FUint8Prototype
  else if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

{ Prototype initialization }

procedure TGocciaTypedArrayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  ValuesMethod: TGocciaValue;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTypedArrayShared) then Exit;

  // Rebuild member definitions per realm: callbacks bind to Self (the
  // bootstrap instance pinned by Shared), and TGocciaSharedPrototype.Destroy
  // unpins Self on realm tear-down.  Caching across realms would leave stale
  // method pointers referencing a freed instance.
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTypedArraySharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(TypedArrayAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayFill, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayCopyWithin, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArraySlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArraySubarray, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArraySet, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayReverse, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArraySort, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayLastIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayIncludes, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayFind, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayFindIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayFindLast, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayFindLastIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayEvery, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArraySome, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayFilter, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayReduce, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayReduceRight, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayJoin, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_TO_LOCALE_STRING, TypedArrayToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayToReversed, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayToSorted, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayWith, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayValues, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayKeys, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(TypedArrayEntries, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddAccessor(PROP_BUFFER, TypedArrayBufferGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_BYTE_LENGTH, TypedArrayByteLengthGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_BYTE_OFFSET, TypedArrayByteOffsetGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_LENGTH, TypedArrayLengthGetter, nil, [pfConfigurable]);
    Members.AddSymbolMethod(
      TGocciaSymbolValue.WellKnownIterator, '[Symbol.iterator]',
      TypedArrayValues, 0, [pfConfigurable, pfWritable]);
    Members.AddSymbolAccessor(
      TGocciaSymbolValue.WellKnownToStringTag, '[Symbol.toStringTag]',
      TypedArrayToStringTagGetter, nil, [pfConfigurable]);
    FPrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
  ValuesMethod := Shared.Prototype.GetProperty('values');
  Shared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(ValuesMethod, [pfConfigurable, pfWritable]));
end;

class procedure TGocciaTypedArrayValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTypedArrayShared;
  if not Assigned(Shared) then
  begin
    TGocciaTypedArrayValue.Create(takUint8, 0);
    Shared := GetTypedArrayShared;
  end;
  // InitializePrototype exits early when CurrentRealm is nil, so the lazy
  // Create() can leave Shared still nil — guard before deref.
  if Assigned(Shared) and (AConstructor is TGocciaClassValue) then
  begin
    TGocciaClassValue(AConstructor).Prototype.Prototype := Shared.Prototype;
    TGocciaClassValue(AConstructor).Prototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(AConstructor, [pfConfigurable, pfWritable]));
  end;
end;

class procedure TGocciaTypedArrayValue.SetSharedPrototypeParent(const AParent: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTypedArrayShared;
  if Assigned(Shared) and not Assigned(Shared.Prototype.Prototype) then
    Shared.Prototype.Prototype := AParent;
end;

class function TGocciaTypedArrayValue.GetSharedPrototypeObject: TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTypedArrayShared;
  if Assigned(Shared) then
    Result := Shared.Prototype
  else
    Result := nil;
end;

class function TGocciaTypedArrayValue.GetSharedPrototypeObjectForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Result := nil;
  if not Assigned(ARealm) then
    Exit;

  Shared := TGocciaSharedPrototype(ARealm.GetOwnedSlot(GTypedArraySharedSlot));
  if Assigned(Shared) then
    Result := Shared.Prototype;
end;

class procedure TGocciaTypedArrayValue.SetUint8Prototype(const APrototype: TGocciaObjectValue);
begin
  FUint8Prototype := APrototype;
end;

{ Property access — indexed elements }

function TGocciaTypedArrayValue.GetProperty(const AName: string): TGocciaValue;
var
  IsNegativeZero: Boolean;
  Index: Integer;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
  begin
    if IsValidIntegerIndexedElement(NumericIndex, IsNegativeZero, Index) then
      Result := GetElementAsValue(Index)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  if AName = PROP_LENGTH then
    Exit(TGocciaNumberLiteralValue.Create(GetLength));
  if AName = PROP_BYTE_LENGTH then
    Exit(TGocciaNumberLiteralValue.Create(GetLength * BytesPerElement(FKind)));
  if AName = PROP_BYTE_OFFSET then
  begin
    if IsTypedArrayOutOfBounds(Self) then
      Exit(TGocciaNumberLiteralValue.Create(0));
    Exit(TGocciaNumberLiteralValue.Create(FByteOffset));
  end;
  if AName = PROP_BUFFER then
    Exit(FBufferValue);
  if AName = PROP_BYTES_PER_ELEMENT then
    Exit(TGocciaNumberLiteralValue.Create(BytesPerElement(FKind)));
  Result := inherited GetProperty(AName);
end;

procedure TGocciaTypedArrayValue.AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean);
var
  IsNegativeZero: Boolean;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
  begin
    SetIntegerIndexedElement(NumericIndex, IsNegativeZero, AValue);
    Exit;
  end;
  inherited AssignProperty(AName, AValue, ACanCreate);
end;

function TGocciaTypedArrayValue.AssignPropertyWithReceiver(const AName: string;
  const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean;
var
  IsNegativeZero: Boolean;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
  begin
    if AReceiver = Self then
    begin
      SetIntegerIndexedElement(NumericIndex, IsNegativeZero, AValue);
      Exit(True);
    end;
    Exit(OrdinarySetIntegerIndexedElementWithReceiver(AName, NumericIndex,
      IsNegativeZero, AValue, AReceiver));
  end;
  Result := inherited AssignPropertyWithReceiver(AName, AValue, AReceiver);
end;

// ES2026 §10.4.5.4 [[DefineOwnProperty]](P, Desc), integer-indexed branch.
function TryDefineTypedArrayIntegerIndexedProperty(
  const AArray: TGocciaTypedArrayValue;
  const ANumericIndex: Double;
  const AIsNegativeZero: Boolean;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Index: Integer;
begin
  if not AArray.IsValidIntegerIndexedElement(ANumericIndex,
    AIsNegativeZero, Index) then
    Exit(False);
  // Immutable ArrayBuffers proposal: a valid integer index over an immutable
  // backing buffer cannot be redefined, so [[DefineOwnProperty]] returns false
  // (Object.defineProperty throws; Reflect.defineProperty returns false).
  if IsTypedArrayBackedByImmutableArrayBuffer(AArray) then
    Exit(False);
  if ADescriptor.HasConfigurableField and not ADescriptor.Configurable then
    Exit(False);
  if ADescriptor.HasEnumerableField and not ADescriptor.Enumerable then
    Exit(False);
  if IsAccessorDescriptor(ADescriptor) then
    Exit(False);
  if ADescriptor.HasWritableField and not ADescriptor.Writable then
    Exit(False);
  if ADescriptor.HasValue then
    AArray.SetIntegerIndexedElement(ANumericIndex, AIsNegativeZero,
      TGocciaPropertyDescriptorData(ADescriptor).Value);
  Result := True;
end;

// ES2026 §10.4.5.4 [[DefineOwnProperty]](P, Desc)
procedure TGocciaTypedArrayValue.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  IsNegativeZero: Boolean;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
  begin
    if TryDefineTypedArrayIntegerIndexedProperty(Self, NumericIndex,
      IsNegativeZero, ADescriptor) then
    begin
      ADescriptor.Free;
      Exit;
    end;
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
      SSuggestCannotDeleteNonConfigurable);
  end;

  inherited DefineProperty(AName, ADescriptor);
end;

// ES2026 §10.4.5.4 [[DefineOwnProperty]](P, Desc) — boolean variant
function TGocciaTypedArrayValue.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  IsNegativeZero: Boolean;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
  begin
    try
      Result := TryDefineTypedArrayIntegerIndexedProperty(Self, NumericIndex,
        IsNegativeZero, ADescriptor);
    finally
      ADescriptor.Free;
    end;
    Exit;
  end;

  Result := inherited TryDefineProperty(AName, ADescriptor);
end;

function TGocciaTypedArrayValue.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
var
  IsNegativeZero: Boolean;
  Index: Integer;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
  begin
    if IsValidIntegerIndexedElement(NumericIndex, IsNegativeZero, Index) then
      Exit(TGocciaPropertyDescriptorData.Create(GetElementAsValue(Index),
        [pfEnumerable, pfConfigurable, pfWritable]));
    Exit(nil);
  end;

  Result := inherited GetOwnPropertyDescriptor(AName);
end;

// ES2026 §10.4.5.7 [[Delete]](P)
function TGocciaTypedArrayValue.DeleteProperty(const AName: string): Boolean;
var
  IsNegativeZero: Boolean;
  Index: Integer;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
    Exit(not IsValidIntegerIndexedElement(NumericIndex, IsNegativeZero, Index));

  Result := inherited DeleteProperty(AName);
end;

function TGocciaTypedArrayValue.HasProperty(const AName: string): Boolean;
var
  IsNegativeZero: Boolean;
  Index: Integer;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) then
    Exit(IsValidIntegerIndexedElement(NumericIndex, IsNegativeZero, Index));

  Result := inherited HasProperty(AName);
end;

function TGocciaTypedArrayValue.HasOwnProperty(const AName: string): Boolean;
var
  IsNegativeZero: Boolean;
  Index: Integer;
  NumericIndex: Double;
begin
  if TryCanonicalNumericIndexString(AName, NumericIndex, IsNegativeZero) and
     IsValidIntegerIndexedElement(NumericIndex, IsNegativeZero, Index) then
    Result := True
  else if AName = PROP_LENGTH then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
end;

function TGocciaTypedArrayValue.GetOwnPropertyKeys: TArray<string>;
var
  Count, I, Len: Integer;
  IsNegativeZero: Boolean;
  Key: string;
  NumericIndex: Double;
  OwnKeys: TArray<string>;
begin
  Len := GetLength;
  OwnKeys := inherited GetOwnPropertyKeys;
  SetLength(Result, Len + System.Length(OwnKeys));
  Count := 0;

  for I := 0 to Len - 1 do
  begin
    Result[Count] := IntToStr(I);
    Inc(Count);
  end;

  for Key in OwnKeys do
  begin
    if TryCanonicalNumericIndexString(Key, NumericIndex, IsNegativeZero) and
       (not IsNegativeZero) and (NumericIndex >= 0) and
       (NumericIndex < Len) and (Frac(NumericIndex) = 0.0) then
      Continue;
    Result[Count] := Key;
    Inc(Count);
  end;

  SetLength(Result, Count);
end;

function TGocciaTypedArrayValue.GetAllPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaTypedArrayValue.ToStringTag: string;
begin
  Result := KindName(FKind);
end;

procedure TGocciaTypedArrayValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FBufferValue) and not FBufferValue.GCMarked then
    FBufferValue.MarkReferences;
end;

{ Helpers }

function RequireTypedArray(const AThisValue: TGocciaValue; const AMethod: string): TGocciaTypedArrayValue;
begin
  if not (AThisValue is TGocciaTypedArrayValue) then
    ThrowTypeError(Format(SErrorTypedArrayRequiresTypedArray, [AMethod]), SSuggestTypedArrayThisType);
  Result := TGocciaTypedArrayValue(AThisValue);
end;

function IsDetachedTypedArray(const AArray: TGocciaTypedArrayValue): Boolean;
begin
  Result := (AArray.FBufferValue is TGocciaArrayBufferValue) and
    TGocciaArrayBufferValue(AArray.FBufferValue).Detached;
end;

// ES2026 §23.2.4.4 ValidateTypedArray(obj, order)
function IsTypedArrayOutOfBounds(const AArray: TGocciaTypedArrayValue): Boolean;
var
  BufferByteLength, ByteOffsetEnd: Int64;
begin
  if IsDetachedTypedArray(AArray) then
    Exit(True);

  AArray.SyncBufferData;
  BufferByteLength := System.Length(AArray.FBufferData);
  if AArray.FAutoLength then
    Exit(AArray.FByteOffset > BufferByteLength);

  ByteOffsetEnd := Int64(AArray.FByteOffset) +
    Int64(AArray.FLength) * Int64(TGocciaTypedArrayValue.BytesPerElement(AArray.FKind));
  Result := (AArray.FByteOffset > BufferByteLength) or
    (ByteOffsetEnd > BufferByteLength);
end;

function IsTypedArrayBackedByImmutableArrayBuffer(
  const AArray: TGocciaTypedArrayValue): Boolean;
begin
  Result := (AArray.FBufferValue is TGocciaArrayBufferValue) and
    TGocciaArrayBufferValue(AArray.FBufferValue).Immutable;
end;

procedure EnsureTypedArrayAttached(const AArray: TGocciaTypedArrayValue; const AMethod: string);
begin
  if IsTypedArrayOutOfBounds(AArray) then
    ThrowTypeError(Format(SErrorCannotUseDetachedTypedArray, [AMethod]), SSuggestArrayBufferDetached);
end;

procedure EnsureTypedArrayWritable(const AArray: TGocciaTypedArrayValue; const AMethod: string);
begin
  EnsureTypedArrayAttached(AArray, AMethod);
  if IsTypedArrayBackedByImmutableArrayBuffer(AArray) then
    ThrowTypeError(AMethod + ' cannot write to an immutable ArrayBuffer');
end;

function RequireAttachedTypedArray(const AThisValue: TGocciaValue; const AMethod: string): TGocciaTypedArrayValue;
begin
  Result := RequireTypedArray(AThisValue, AMethod);
  EnsureTypedArrayAttached(Result, AMethod);
end;

function DefaultTypedArrayConstructor(const AKind: TGocciaTypedArrayKind): TGocciaValue;
var
  GlobalScope: TGocciaScope;
begin
  if Assigned(CurrentRealm) and (CurrentRealm.GlobalEnv is TGocciaScope) then
  begin
    GlobalScope := TGocciaScope(CurrentRealm.GlobalEnv);
    if GlobalScope.TryGetBindingValue(TGocciaTypedArrayValue.KindName(AKind), Result) and
       Assigned(Result) and Result.IsConstructable then
      Exit;
  end;

  ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);
  Result := nil;
end;

// ES2026 §7.3.22 SpeciesConstructor(O, defaultConstructor)
function TypedArraySpeciesConstructor(const AArray: TGocciaTypedArrayValue): TGocciaValue;
var
  ConstructorValue, SpeciesValue: TGocciaValue;
begin
  Result := DefaultTypedArrayConstructor(AArray.FKind);
  ConstructorValue := AArray.GetProperty(PROP_CONSTRUCTOR);
  if ConstructorValue is TGocciaUndefinedLiteralValue then
    Exit;

  if not (ConstructorValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);

  if (ConstructorValue is TGocciaClassValue) and
     (TGocciaClassValue(ConstructorValue).Name = 'TypedArray') then
    Exit;

  SpeciesValue := TGocciaObjectValue(ConstructorValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies);
  if (SpeciesValue is TGocciaUndefinedLiteralValue) or
     (SpeciesValue is TGocciaNullLiteralValue) then
    Exit;
  if not SpeciesValue.IsConstructable then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);
  Result := SpeciesValue;
end;

// ES2026 §23.2.4.3 TypedArraySpeciesCreate(exemplar, argumentList)
function TypedArraySpeciesCreate(const AExemplar: TGocciaTypedArrayValue;
  const AArguments: TGocciaArgumentsCollection): TGocciaTypedArrayValue;
var
  ConstructorValue, ConstructedValue: TGocciaValue;
  ExpectedLength: Integer;
begin
  ConstructorValue := TypedArraySpeciesConstructor(AExemplar);
  ConstructedValue := ConstructValue(ConstructorValue, AArguments, ConstructorValue);
  if not (ConstructedValue is TGocciaTypedArrayValue) then
    ThrowTypeError(Format(SErrorTypedArrayRequiresTypedArray,
      ['TypedArraySpeciesCreate']), SSuggestTypedArrayConstructorReceiver);

  Result := TGocciaTypedArrayValue(ConstructedValue);
  if TGocciaTypedArrayValue.IsBigIntKind(Result.FKind) <>
     TGocciaTypedArrayValue.IsBigIntKind(AExemplar.FKind) then
    ThrowTypeError(SErrorBigIntTypedArrayCannotMix, SSuggestBigIntTypedArrayValue);

  if (AArguments.Length = 1) and
     (AArguments.GetElement(0) is TGocciaNumberLiteralValue) then
  begin
    ExpectedLength := Trunc(TGocciaNumberLiteralValue(AArguments.GetElement(0)).Value);
    if IsTypedArrayOutOfBounds(Result) or (Result.Length < ExpectedLength) then
      ThrowTypeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
  end;
end;

function ToIntegerOrInfinityBounded(const AValue: TGocciaValue): Integer;
var
  Num: TGocciaNumberLiteralValue;
begin
  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    Result := 0
  else if Num.IsInfinity then
    Result := MaxInt
  else if Num.IsNegativeInfinity then
    Result := -MaxInt
  else if Num.Value >= MaxInt then
    Result := MaxInt
  else if Num.Value <= -MaxInt then
    Result := -MaxInt
  else
    Result := Trunc(Num.Value);
end;

{ TGocciaTypedArrayIteratorValue }

constructor TGocciaTypedArrayIteratorValue.Create(
  const ASource: TGocciaTypedArrayValue;
  const AKind: TGocciaTypedArrayIteratorKind);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create;
  SharedPrototype := EnsureArrayIteratorPrototype;
  if Assigned(SharedPrototype) then
    FPrototype := SharedPrototype;
  FSource := ASource;
  FIndex := 0;
  FKind := AKind;
end;

function TGocciaTypedArrayIteratorValue.CurrentLengthOrThrow: Integer;
begin
  if IsDetachedTypedArray(FSource) then
    ThrowTypeError(
      Format(SErrorCannotUseDetachedTypedArray, ['TypedArray Iterator']),
      SSuggestArrayBufferDetached);

  if FSource.FAutoLength then
  begin
    if not FSource.HasValidBackingRange(0) then
      ThrowTypeError(
        Format(SErrorCannotUseDetachedTypedArray, ['TypedArray Iterator']),
        SSuggestArrayBufferDetached);
    Exit(FSource.GetLength);
  end;

  if not FSource.HasValidBackingRange(FSource.FLength) then
    ThrowTypeError(
      Format(SErrorCannotUseDetachedTypedArray, ['TypedArray Iterator']),
      SSuggestArrayBufferDetached);
  Result := FSource.FLength;
end;

function TGocciaTypedArrayIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  EntryArray: TGocciaArrayValue;
  Len: Integer;
  Value: TGocciaValue;
begin
  if FDone then
    Exit(CreateIteratorResult(
      TGocciaUndefinedLiteralValue.UndefinedValue, True));

  Len := CurrentLengthOrThrow;
  if FIndex >= Len then
  begin
    FDone := True;
    Exit(CreateIteratorResult(
      TGocciaUndefinedLiteralValue.UndefinedValue, True));
  end;

  case FKind of
    taikKeys:
      Value := TGocciaNumberLiteralValue.Create(FIndex);
    taikEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(TGocciaNumberLiteralValue.Create(FIndex));
      EntryArray.Elements.Add(FSource.GetElementAsValue(FIndex));
      Value := EntryArray;
    end;
  else
    Value := FSource.GetElementAsValue(FIndex);
  end;

  Inc(FIndex);
  Result := CreateIteratorResult(Value, False);
end;

function TGocciaTypedArrayIteratorValue.DirectNext(
  out ADone: Boolean): TGocciaValue;
var
  EntryArray: TGocciaArrayValue;
  Len: Integer;
begin
  if FDone then
  begin
    ADone := True;
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Len := CurrentLengthOrThrow;
  if FIndex >= Len then
  begin
    FDone := True;
    ADone := True;
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  ADone := False;
  case FKind of
    taikKeys:
      Result := TGocciaNumberLiteralValue.Create(FIndex);
    taikEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(TGocciaNumberLiteralValue.Create(FIndex));
      EntryArray.Elements.Add(FSource.GetElementAsValue(FIndex));
      Result := EntryArray;
    end;
  else
    Result := FSource.GetElementAsValue(FIndex);
  end;
  Inc(FIndex);
end;

function TGocciaTypedArrayIteratorValue.ToStringTag: string;
begin
  Result := 'Array Iterator';
end;

procedure TGocciaTypedArrayIteratorValue.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FSource) and not FSource.GCMarked then
    FSource.MarkReferences;
end;

function InvokeCallback(const ACallback: TGocciaValue; const AElement, AIndex, AArray, AThisArg: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  CallbackRoot, ElementRoot, IndexRoot, ArrayRoot, ThisRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(CallbackRoot);
  InitializeTempRoot(ElementRoot);
  InitializeTempRoot(IndexRoot);
  InitializeTempRoot(ArrayRoot);
  InitializeTempRoot(ThisRoot);
  AddTempRootIfNeeded(CallbackRoot, ACallback);
  AddTempRootIfNeeded(ElementRoot, AElement);
  AddTempRootIfNeeded(IndexRoot, AIndex);
  AddTempRootIfNeeded(ArrayRoot, AArray);
  AddTempRootIfNeeded(ThisRoot, AThisArg);
  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(AElement);
    Args.Add(AIndex);
    Args.Add(AArray);
    Result := TGocciaFunctionBase(ACallback).Call(Args, AThisArg);
  finally
    Args.Free;
    RemoveTempRootIfNeeded(ThisRoot);
    RemoveTempRootIfNeeded(ArrayRoot);
    RemoveTempRootIfNeeded(IndexRoot);
    RemoveTempRootIfNeeded(ElementRoot);
    RemoveTempRootIfNeeded(CallbackRoot);
  end;
end;

function CreateSameKindArray(const ASource: TGocciaTypedArrayValue; const ALength: Integer): TGocciaTypedArrayValue;
begin
  Result := TGocciaTypedArrayValue.Create(ASource.FKind, ALength);
  Result.Prototype := ASource.Prototype;
end;

{ Prototype getters }

function TGocciaTypedArrayValue.TypedArrayBufferGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RequireTypedArray(AThisValue, 'TypedArray.prototype.buffer').FBufferValue;
end;

function TGocciaTypedArrayValue.TypedArrayByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.byteLength');
  Result := TGocciaNumberLiteralValue.Create(TA.GetLength * BytesPerElement(TA.FKind));
end;

function TGocciaTypedArrayValue.TypedArrayByteOffsetGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.byteOffset');
  if IsTypedArrayOutOfBounds(TA) then
    Exit(TGocciaNumberLiteralValue.Create(0));
  Result := TGocciaNumberLiteralValue.Create(TA.FByteOffset);
end;

function TGocciaTypedArrayValue.TypedArrayLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(RequireTypedArray(AThisValue, 'TypedArray.prototype.length').GetLength);
end;

// ES2026 §23.2.3.32 get %TypedArray%.prototype [ @@toStringTag ]
function TGocciaTypedArrayValue.TypedArrayToStringTagGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaTypedArrayValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaStringLiteralValue.Create(KindName(TGocciaTypedArrayValue(AThisValue).FKind));
end;

{ Prototype methods }

// ES2026 §23.2.3.1 %TypedArray%.prototype.at(index)
function TGocciaTypedArrayValue.TypedArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  RelIndex, ActualIndex, Len: Integer;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.at');
  Len := TA.Length;
  // ES2026 §23.2.3.1 step 4: a missing index coerces to 0, not undefined
  RelIndex := ToIntegerFromArgs(AArgs, 0);
  if RelIndex >= 0 then
    ActualIndex := RelIndex
  else
    ActualIndex := Len + RelIndex;
  if (ActualIndex < 0) or (ActualIndex >= Len) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TA.GetElementAsValue(ActualIndex);
end;

// ES2026 §23.2.3.8 %TypedArray%.prototype.fill(value [, start [, end]])
function TGocciaTypedArrayValue.TypedArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  FillNum: TGocciaNumberLiteralValue;
  FillBigInt: Int64;
  First, Final, I, RelStart, RelEnd, Len: Integer;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.fill');
  EnsureTypedArrayWritable(TA, 'TypedArray.prototype.fill');
  Len := TA.Length;
  FillBigInt := 0;
  FillNum := nil;
  if IsBigIntKind(TA.FKind) then
  begin
    if AArgs.Length = 0 then
      FillBigInt := RawBigIntForKind(TA.FKind,
        ToBigIntForTypedArray(TGocciaUndefinedLiteralValue.UndefinedValue))
    else
      FillBigInt := RawBigIntForKind(TA.FKind,
        ToBigIntForTypedArray(AArgs.GetElement(0)));
  end
  else
  begin
    if AArgs.Length = 0 then
      FillNum := TGocciaUndefinedLiteralValue.UndefinedValue.ToNumberLiteral
    else
      FillNum := AArgs.GetElement(0).ToNumberLiteral;
  end;

  // ES2026 §23.2.3.8 step 5-6: RelativeIndex → clamp
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    RelStart := ToIntegerFromArgs(AArgs, 1);
    if RelStart < 0 then
      First := Max(Len + RelStart, 0)
    else
      First := Min(RelStart, Len);
  end
  else
    First := 0;

  // ES2026 §23.2.3.8 step 7-8
  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
  begin
    RelEnd := ToIntegerFromArgs(AArgs, 2);
    if RelEnd < 0 then
      Final := Max(Len + RelEnd, 0)
    else
      Final := Min(RelEnd, Len);
  end
  else
    Final := Len;

  EnsureTypedArrayAttached(TA, 'TypedArray.prototype.fill');

  if IsBigIntKind(TA.FKind) then
  begin
    for I := First to Final - 1 do
      TA.WriteBigIntElement(I, FillBigInt);
  end
  else
  begin
    for I := First to Final - 1 do
      TA.WriteNumberLiteral(I, FillNum);
  end;
  Result := AThisValue;
end;

// ES2026 §23.2.3.3 %TypedArray%.prototype.copyWithin(target, start [, end])
function TGocciaTypedArrayValue.TypedArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Target, Start, Final, Count, Direction, From, To_, Len, CurrentLen: Integer;
  BPE: Integer;
  TempBuf: TBytes;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.copyWithin');
  EnsureTypedArrayWritable(TA, 'TypedArray.prototype.copyWithin');
  Len := TA.Length;

  Target := ToIntegerFromArgs(AArgs, 0);
  Start := ToIntegerFromArgs(AArgs, 1);

  if Target < 0 then Target := Max(Len + Target, 0) else Target := Min(Target, Len);
  if Start < 0 then Start := Max(Len + Start, 0) else Start := Min(Start, Len);

  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
  begin
    Final := ToIntegerFromArgs(AArgs, 2);
    if Final < 0 then Final := Max(Len + Final, 0) else Final := Min(Final, Len);
  end
  else
    Final := Len;

  Count := Min(Final - Start, Len - Target);
  if Count <= 0 then
    Exit(AThisValue);
  EnsureTypedArrayAttached(TA, 'TypedArray.prototype.copyWithin');
  CurrentLen := TA.Length;
  Count := Min(Count, Max(CurrentLen - Target, 0));
  Count := Min(Count, Max(CurrentLen - Start, 0));
  if Count <= 0 then
    Exit(AThisValue);

  BPE := BytesPerElement(TA.FKind);
  SetLength(TempBuf, Count * BPE);
  From := TA.FByteOffset + Start * BPE;
  To_ := TA.FByteOffset + Target * BPE;
  Move(TA.FBufferData[From], TempBuf[0], Count * BPE);
  Move(TempBuf[0], TA.FBufferData[To_], Count * BPE);

  Result := AThisValue;
end;

// ES2026 §23.2.3.27 %TypedArray%.prototype.slice(start, end)
function TGocciaTypedArrayValue.TypedArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  First, Final, NewLen, SourceLength, I: Integer;
  ConstructorArgs: TGocciaArgumentsCollection;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.slice');
  SourceLength := TA.Length;

  if AArgs.Length > 0 then
  begin
    First := ToIntegerFromArgs(AArgs, 0);
    if First < 0 then First := Max(SourceLength + First, 0) else First := Min(First, SourceLength);
  end
  else
    First := 0;

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    Final := ToIntegerFromArgs(AArgs, 1);
    if Final < 0 then Final := Max(SourceLength + Final, 0) else Final := Min(Final, SourceLength);
  end
  else
    Final := SourceLength;

  NewLen := Max(Final - First, 0);
  ConstructorArgs := TGocciaArgumentsCollection.Create(
    [TGocciaNumberLiteralValue.Create(NewLen)]);
  try
    NewTA := TypedArraySpeciesCreate(TA, ConstructorArgs);
  finally
    ConstructorArgs.Free;
  end;

  if NewLen > 0 then
    EnsureTypedArrayAttached(TA, 'TypedArray.prototype.slice');
  if NewTA.FBufferValue <> TA.FBufferValue then
    EnsureTypedArrayWritable(NewTA, 'TypedArray.prototype.slice');
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 0 to NewLen - 1 do
      if First + I < TA.Length then
        NewTA.WriteValueToElement(I, TA.GetElementAsValue(First + I));
  end
  else
  begin
    for I := 0 to NewLen - 1 do
      if First + I < TA.Length then
        NewTA.WriteValueToElement(I, TA.GetElementAsValue(First + I));
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.30 %TypedArray%.prototype.subarray(begin, end)
function TGocciaTypedArrayValue.TypedArraySubarray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  BeginIdx, EndIdx, NewLen, SourceLength, BPE: Integer;
  EndIsUndefined, UseAutoLengthResult: Boolean;
  ConstructorArgs: TGocciaArgumentsCollection;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.subarray');
  if IsTypedArrayOutOfBounds(TA) then
    SourceLength := 0
  else
    SourceLength := TA.Length;
  BPE := BytesPerElement(TA.FKind);

  if AArgs.Length > 0 then
  begin
    BeginIdx := ToIntegerFromArgs(AArgs, 0);
    if BeginIdx < 0 then BeginIdx := Max(SourceLength + BeginIdx, 0) else BeginIdx := Min(BeginIdx, SourceLength);
  end
  else
    BeginIdx := 0;

  EndIsUndefined := (AArgs.Length <= 1) or
    (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue);
  if not EndIsUndefined then
  begin
    EndIdx := ToIntegerFromArgs(AArgs, 1);
    if EndIdx < 0 then EndIdx := Max(SourceLength + EndIdx, 0) else EndIdx := Min(EndIdx, SourceLength);
  end
  else
    EndIdx := SourceLength;

  NewLen := Max(EndIdx - BeginIdx, 0);
  UseAutoLengthResult := TA.FAutoLength and EndIsUndefined;
  ConstructorArgs := TGocciaArgumentsCollection.Create;
  try
    ConstructorArgs.Add(TA.FBufferValue);
    ConstructorArgs.Add(TGocciaNumberLiteralValue.Create(
      TA.FByteOffset + BeginIdx * BPE));
    if not UseAutoLengthResult then
      ConstructorArgs.Add(TGocciaNumberLiteralValue.Create(NewLen));
    NewTA := TypedArraySpeciesCreate(TA, ConstructorArgs);
  finally
    ConstructorArgs.Free;
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.25 %TypedArray%.prototype.set(source [, offset])
function TGocciaTypedArrayValue.TypedArraySet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, SrcTA: TGocciaTypedArrayValue;
  TargetOffset, I, SrcLen, TargetLen, SourceLen: Integer;
  SrcArray: TGocciaArrayValue;
  SrcObj: TGocciaObjectValue;
  NumberSnapshot: array of Double;
  BigIntSnapshot: array of Int64;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.set');
  EnsureTypedArrayWritable(TA, 'TypedArray.prototype.set');
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorTypedArraySetRequiresArg, SSuggestTypedArraySetSource);

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    TargetOffset := ToIntegerOrInfinityBounded(AArgs.GetElement(1));
    if TargetOffset < 0 then
      ThrowRangeError(SErrorTypedArraySetOffsetNonNegative, SSuggestTypedArrayLength);
  end
  else
    TargetOffset := 0;

  EnsureTypedArrayAttached(TA, 'TypedArray.prototype.set');
  TargetLen := TA.Length;

  if AArgs.GetElement(0) is TGocciaTypedArrayValue then
  begin
    SrcTA := TGocciaTypedArrayValue(AArgs.GetElement(0));
    EnsureTypedArrayAttached(SrcTA, 'TypedArray.prototype.set');
    SourceLen := SrcTA.Length;
    if (TargetOffset > TargetLen) or
       (SourceLen > TargetLen - TargetOffset) then
      ThrowRangeError(SErrorTypedArraySourceTooLarge, SSuggestTypedArrayLength);
    // ES2026 §23.2.3.25.1 step 1.b: mixed BigInt/Number content types throw
    if IsBigIntKind(TA.FKind) <> IsBigIntKind(SrcTA.FKind) then
      ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt, SSuggestBigIntTypedArrayValue);
    if IsBigIntKind(TA.FKind) then
    begin
      SetLength(BigIntSnapshot, SourceLen);
      for I := 0 to SourceLen - 1 do
        BigIntSnapshot[I] := SrcTA.ReadBigIntElement(I);
      for I := 0 to SourceLen - 1 do
        TA.WriteBigIntElement(TargetOffset + I, BigIntSnapshot[I]);
    end
    else
    begin
      SetLength(NumberSnapshot, SourceLen);
      for I := 0 to SourceLen - 1 do
        NumberSnapshot[I] := SrcTA.ReadElement(I);
      for I := 0 to SourceLen - 1 do
        TA.WriteElement(TargetOffset + I, NumberSnapshot[I]);
    end;
  end
  else if AArgs.GetElement(0) is TGocciaArrayValue then
  begin
    SrcArray := TGocciaArrayValue(AArgs.GetElement(0));
    if (TargetOffset > TargetLen) or
       (SrcArray.Elements.Count > TargetLen - TargetOffset) then
      ThrowRangeError(SErrorTypedArraySourceTooLarge, SSuggestTypedArrayLength);
    for I := 0 to SrcArray.Elements.Count - 1 do
      TA.WriteValueToElement(TargetOffset + I, SrcArray.Elements[I]);
  end
  else
  begin
    // ES2026 §23.2.3.25.2 SetTypedArrayFromArrayLike
    SrcObj := ToObject(AArgs.GetElement(0));
    SrcLen := LengthOfArrayLike(SrcObj);
    if (TargetOffset > TargetLen) or
       (SrcLen > TargetLen - TargetOffset) then
      ThrowRangeError(SErrorTypedArraySourceTooLarge, SSuggestTypedArrayLength);
    for I := 0 to SrcLen - 1 do
      TA.WriteValueToElement(TargetOffset + I, SrcObj.GetProperty(IntToStr(I)));
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.24 %TypedArray%.prototype.reverse()
function TGocciaTypedArrayValue.TypedArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, Len: Integer;
  Tmp: Double;
  TmpBig: Int64;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.reverse');
  EnsureTypedArrayWritable(TA, 'TypedArray.prototype.reverse');
  Len := TA.Length;
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 0 to (Len div 2) - 1 do
    begin
      TmpBig := TA.ReadBigIntElement(I);
      TA.WriteBigIntElement(I, TA.ReadBigIntElement(Len - 1 - I));
      TA.WriteBigIntElement(Len - 1 - I, TmpBig);
    end;
  end
  else
  begin
    for I := 0 to (Len div 2) - 1 do
    begin
      Tmp := TA.ReadElement(I);
      TA.WriteElement(I, TA.ReadElement(Len - 1 - I));
      TA.WriteElement(Len - 1 - I, Tmp);
    end;
  end;
  Result := AThisValue;
end;

// ES2026 §23.2.3.28 %TypedArray%.prototype.sort([comparefn])
function TGocciaTypedArrayValue.TypedArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SortLen: Integer;
  NumberValues, NumberScratch: array of Double;
  BigIntValues, BigIntScratch: array of Int64;
  HasCompare: Boolean;
  AbortSortWriteback: Boolean;
  Comparator: TGocciaValue;

  function NumberBits(const AValue: Double): Int64;
  begin
    Move(AValue, Result, SizeOf(Result));
  end;

  function BigIntValueFromRaw(const ARaw: Int64): TGocciaBigIntValue;
  begin
    if TA.FKind = takBigUint64 then
      Result := TGocciaBigIntValue.Create(BigIntFromQWord(QWord(ARaw)))
    else
      Result := TGocciaBigIntValue.Create(TBigInteger.FromInt64(ARaw));
  end;

  function CallComparator(const ALeft, ARight: TGocciaValue): Double;
  var
    CompareArgs: TGocciaArgumentsCollection;
    CompareResult: TGocciaValue;
  begin
    CompareArgs := TGocciaArgumentsCollection.Create;
    try
      CompareArgs.Add(ALeft);
      CompareArgs.Add(ARight);
      CompareResult := InvokeCallable(Comparator, CompareArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
      Result := CompareResult.ToNumberLiteral.Value;
      if Math.IsNaN(Result) then
        Result := 0;
      AbortSortWriteback := IsTypedArrayOutOfBounds(TA);
    finally
      CompareArgs.Free;
    end;
  end;

  function CompareNumbers(const ALeft, ARight: Double): Double;
  var
    LeftBits, RightBits: Int64;
  begin
    if HasCompare then
      Exit(CallComparator(TGocciaNumberLiteralValue.Create(ALeft),
        TGocciaNumberLiteralValue.Create(ARight)));

    if Math.IsNaN(ALeft) then
    begin
      if Math.IsNaN(ARight) then
        Exit(0);
      Exit(1);
    end;
    if Math.IsNaN(ARight) then
      Exit(-1);

    if ALeft < ARight then
      Exit(-1);
    if ALeft > ARight then
      Exit(1);

    if (ALeft = 0) and (ARight = 0) then
    begin
      LeftBits := NumberBits(ALeft);
      RightBits := NumberBits(ARight);
      if (LeftBits < 0) and (RightBits >= 0) then
        Exit(-1);
      if (LeftBits >= 0) and (RightBits < 0) then
        Exit(1);
    end;

    Result := 0;
  end;

  function CompareBigInts(const ALeft, ARight: Int64): Double;
  begin
    if HasCompare then
      Exit(CallComparator(BigIntValueFromRaw(ALeft), BigIntValueFromRaw(ARight)));

    if TA.FKind = takBigUint64 then
    begin
      if QWord(ALeft) < QWord(ARight) then
        Exit(-1);
      if QWord(ALeft) > QWord(ARight) then
        Exit(1);
    end
    else
    begin
      if ALeft < ARight then
        Exit(-1);
      if ALeft > ARight then
        Exit(1);
    end;

    Result := 0;
  end;

  // Bottom-up merge sort over NumberValues using CompareNumbers. O(n log n);
  // stable, so equal keys retain order (needed for -0/+0 and NaN runs to stay
  // in CompareNumbers order). Bails out early when a comparator callback detaches
  // the buffer (AbortSortWriteback).
  procedure MergeSortNumbers;
  var
    // Width and Lo are Int64 so doubling Width and computing Lo + 2 * Width
    // cannot overflow Integer for very large (multi-GB) 1-byte typed arrays,
    // where SortLen can approach High(Integer). Range checks are off in
    // release builds, so an overflow would silently corrupt the merge bounds.
    Width, Lo: Int64;
    Mid, Hi, Left, Right, Dest: Integer;
  begin
    SetLength(NumberScratch, SortLen);
    Width := 1;
    while Width < SortLen do
    begin
      Lo := 0;
      while Lo < SortLen do
      begin
        // Clamp in Int64, then narrow: Mid and Hi are <= SortLen <= High(Integer).
        Mid := Integer(Min(Lo + Width, Int64(SortLen)));
        Hi := Integer(Min(Lo + 2 * Width, Int64(SortLen)));
        Left := Integer(Lo);
        Right := Mid;
        Dest := Integer(Lo);
        while (Left < Mid) and (Right < Hi) do
        begin
          if CompareNumbers(NumberValues[Right], NumberValues[Left]) < 0 then
          begin
            NumberScratch[Dest] := NumberValues[Right];
            Inc(Right);
          end
          else
          begin
            NumberScratch[Dest] := NumberValues[Left];
            Inc(Left);
          end;
          if AbortSortWriteback then
            Exit;
          Inc(Dest);
        end;
        while Left < Mid do
        begin
          NumberScratch[Dest] := NumberValues[Left];
          Inc(Left);
          Inc(Dest);
        end;
        while Right < Hi do
        begin
          NumberScratch[Dest] := NumberValues[Right];
          Inc(Right);
          Inc(Dest);
        end;
        Lo := Lo + 2 * Width;
      end;
      for Dest := 0 to SortLen - 1 do
        NumberValues[Dest] := NumberScratch[Dest];
      Width := Width * 2;
    end;
  end;

  // Bottom-up merge sort over BigIntValues using CompareBigInts. See
  // MergeSortNumbers for the structure; identical algorithm over Int64 keys.
  procedure MergeSortBigInts;
  var
    // Width and Lo are Int64 so doubling Width and computing Lo + 2 * Width
    // cannot overflow Integer for very large (multi-GB) 1-byte typed arrays,
    // where SortLen can approach High(Integer). Range checks are off in
    // release builds, so an overflow would silently corrupt the merge bounds.
    Width, Lo: Int64;
    Mid, Hi, Left, Right, Dest: Integer;
  begin
    SetLength(BigIntScratch, SortLen);
    Width := 1;
    while Width < SortLen do
    begin
      Lo := 0;
      while Lo < SortLen do
      begin
        // Clamp in Int64, then narrow: Mid and Hi are <= SortLen <= High(Integer).
        Mid := Integer(Min(Lo + Width, Int64(SortLen)));
        Hi := Integer(Min(Lo + 2 * Width, Int64(SortLen)));
        Left := Integer(Lo);
        Right := Mid;
        Dest := Integer(Lo);
        while (Left < Mid) and (Right < Hi) do
        begin
          if CompareBigInts(BigIntValues[Right], BigIntValues[Left]) < 0 then
          begin
            BigIntScratch[Dest] := BigIntValues[Right];
            Inc(Right);
          end
          else
          begin
            BigIntScratch[Dest] := BigIntValues[Left];
            Inc(Left);
          end;
          if AbortSortWriteback then
            Exit;
          Inc(Dest);
        end;
        while Left < Mid do
        begin
          BigIntScratch[Dest] := BigIntValues[Left];
          Inc(Left);
          Inc(Dest);
        end;
        while Right < Hi do
        begin
          BigIntScratch[Dest] := BigIntValues[Right];
          Inc(Right);
          Inc(Dest);
        end;
        Lo := Lo + 2 * Width;
      end;
      for Dest := 0 to SortLen - 1 do
        BigIntValues[Dest] := BigIntScratch[Dest];
      Width := Width * 2;
    end;
  end;

begin
  Comparator := TGocciaUndefinedLiteralValue.UndefinedValue;
  HasCompare := False;
  AbortSortWriteback := False;
  if AArgs.Length > 0 then
  begin
    if not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    begin
      if not AArgs.GetElement(0).IsCallable then
        ThrowTypeError(SErrorCustomSortMustBeFunction, SSuggestCallbackRequired);
      Comparator := AArgs.GetElement(0);
      HasCompare := True;
    end;
  end;

  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.sort');
  EnsureTypedArrayWritable(TA, 'TypedArray.prototype.sort');
  SortLen := TA.Length;

  // BigInt sort path
  if IsBigIntKind(TA.FKind) then
  begin
    SetLength(BigIntValues, SortLen);
    for I := 0 to SortLen - 1 do
      BigIntValues[I] := TA.ReadBigIntElement(I);

    MergeSortBigInts;
    if AbortSortWriteback then
      Exit(AThisValue);

    for I := 0 to SortLen - 1 do
      TA.WriteBigIntElement(I, BigIntValues[I]);
    Exit(AThisValue);
  end;

  SetLength(NumberValues, SortLen);
  for I := 0 to SortLen - 1 do
    NumberValues[I] := TA.ReadElement(I);

  MergeSortNumbers;
  if AbortSortWriteback then
    Exit(AThisValue);

  for I := 0 to SortLen - 1 do
    TA.WriteElement(I, NumberValues[I]);
  Result := AThisValue;
end;

// ES2026 §23.2.3.16 %TypedArray%.prototype.indexOf(searchElement [, fromIndex])
function TGocciaTypedArrayValue.TypedArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  SearchValue: TGocciaValue;
  I, StartIdx, Len: Integer;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.indexOf');
  Len := TA.Length;
  if Len = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if AArgs.Length = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));

  if AArgs.Length > 1 then
  begin
    StartIdx := ToIntegerFromArgs(AArgs, 1);
    if StartIdx < 0 then StartIdx := Max(Len + StartIdx, 0);
  end
  else
    StartIdx := 0;

  if IsTypedArrayOutOfBounds(TA) then
    Exit(TGocciaNumberLiteralValue.Create(-1));

  SearchValue := AArgs.GetElement(0);
  for I := StartIdx to Len - 1 do
    if IsStrictEqual(TA.GetElementAsValue(I), SearchValue) then
      Exit(TGocciaNumberLiteralValue.Create(I));
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.2.3.18 %TypedArray%.prototype.lastIndexOf(searchElement [, fromIndex])
function TGocciaTypedArrayValue.TypedArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  SearchValue: TGocciaValue;
  I, StartIdx, Len: Integer;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.lastIndexOf');
  Len := TA.Length;
  if Len = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if AArgs.Length = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if AArgs.Length > 1 then
  begin
    StartIdx := ToIntegerFromArgs(AArgs, 1);
    if StartIdx < 0 then StartIdx := Len + StartIdx;
  end
  else
    StartIdx := Len - 1;

  if IsTypedArrayOutOfBounds(TA) then
    Exit(TGocciaNumberLiteralValue.Create(-1));

  SearchValue := AArgs.GetElement(0);
  for I := Min(StartIdx, Len - 1) downto 0 do
    if IsStrictEqual(TA.GetElementAsValue(I), SearchValue) then
      Exit(TGocciaNumberLiteralValue.Create(I));
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.2.3.15 %TypedArray%.prototype.includes(searchElement [, fromIndex])
function TGocciaTypedArrayValue.TypedArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  SearchValue: TGocciaValue;
  I, StartIdx, Len: Integer;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.includes');
  Len := TA.Length;
  if Len = 0 then
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  if AArgs.Length = 0 then
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  if AArgs.Length > 1 then
  begin
    StartIdx := ToIntegerFromArgs(AArgs, 1);
    if StartIdx < 0 then StartIdx := Max(Len + StartIdx, 0);
  end
  else
    StartIdx := 0;

  SearchValue := AArgs.GetElement(0);
  for I := StartIdx to Len - 1 do
    if IsSameValueZero(TA.GetElementAsValue(I), SearchValue) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.2.3.11 %TypedArray%.prototype.find(predicate [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.find');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to SourceLength - 1 do
  begin
    Element := TA.GetElementAsValue(I);
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToBooleanLiteral.Value then
      Exit(Element);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.12 %TypedArray%.prototype.findIndex(predicate [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.findIndex');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindIndexCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to SourceLength - 1 do
  begin
    Element := TA.GetElementAsValue(I);
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToBooleanLiteral.Value then
      Exit(TGocciaNumberLiteralValue.Create(I));
  end;
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.2.3.13 %TypedArray%.prototype.findLast(predicate [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.findLast');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindLastCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := SourceLength - 1 downto 0 do
  begin
    Element := TA.GetElementAsValue(I);
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToBooleanLiteral.Value then
      Exit(Element);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.14 %TypedArray%.prototype.findLastIndex(predicate [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.findLastIndex');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindLastIndexCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := SourceLength - 1 downto 0 do
  begin
    Element := TA.GetElementAsValue(I);
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToBooleanLiteral.Value then
      Exit(TGocciaNumberLiteralValue.Create(I));
  end;
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.2.3.7 %TypedArray%.prototype.every(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.every');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayEveryCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to SourceLength - 1 do
  begin
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TA.GetElementAsValue(I),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if not CallResult.ToBooleanLiteral.Value then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §23.2.3.29 %TypedArray%.prototype.some(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.some');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArraySomeCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to SourceLength - 1 do
  begin
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TA.GetElementAsValue(I),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToBooleanLiteral.Value then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
  end;
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.2.3.14 %TypedArray%.prototype.forEach(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  ThisArg: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.forEach');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayForEachCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to SourceLength - 1 do
    InvokeCallback(AArgs.GetElement(0),
      TA.GetElementAsValue(I),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.20 %TypedArray%.prototype.map(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  CallResult, ThisArg: TGocciaValue;
  ResultRoot: TGocciaTempRoot;
  ConstructorArgs: TGocciaArgumentsCollection;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.map');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayMapCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  ConstructorArgs := TGocciaArgumentsCollection.Create(
    [TGocciaNumberLiteralValue.Create(SourceLength)]);
  try
    NewTA := TypedArraySpeciesCreate(TA, ConstructorArgs);
  finally
    ConstructorArgs.Free;
  end;
  EnsureTypedArrayWritable(NewTA, 'TypedArray.prototype.map');
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(ResultRoot, NewTA);
  try
    for I := 0 to SourceLength - 1 do
    begin
      CallResult := InvokeCallback(AArgs.GetElement(0),
        TA.GetElementAsValue(I),
        TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
      NewTA.WriteValueToElement(I, CallResult);
    end;
    Result := NewTA;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

// ES2026 §23.2.3.9 %TypedArray%.prototype.filter(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  CallResult, ThisArg, Element: TGocciaValue;
  Kept: TGocciaValueList;
  ConstructorArgs: TGocciaArgumentsCollection;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.filter');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFilterCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  Kept := TGocciaValueList.Create(False);
  try
    for I := 0 to SourceLength - 1 do
    begin
      Element := TA.GetElementAsValue(I);
      CallResult := InvokeCallback(AArgs.GetElement(0),
        Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Kept.Add(Element);
        if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(Element);
      end;
    end;
    ConstructorArgs := TGocciaArgumentsCollection.Create(
      [TGocciaNumberLiteralValue.Create(Kept.Count)]);
    try
      NewTA := TypedArraySpeciesCreate(TA, ConstructorArgs);
    finally
      ConstructorArgs.Free;
    end;
    EnsureTypedArrayWritable(NewTA, 'TypedArray.prototype.filter');
    for I := 0 to Kept.Count - 1 do
      NewTA.WriteValueToElement(I, Kept[I]);
    Result := NewTA;
  finally
    if Assigned(TGarbageCollector.Instance) then
      for I := 0 to Kept.Count - 1 do
        TGarbageCollector.Instance.RemoveTempRoot(Kept[I]);
    Kept.Free;
  end;
end;

// ES2026 §23.2.3.22 %TypedArray%.prototype.reduce(callbackfn [, initialValue])
function TGocciaTypedArrayValue.TypedArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.reduce');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayReduceCallable, SSuggestTypedArrayCallable);

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    I := 0;
  end
  else
  begin
    if SourceLength = 0 then
      ThrowTypeError(SErrorReduceEmptyTypedArray, SSuggestReduceInitialValue);
    Accumulator := TA.GetElementAsValue(0);
    I := 1;
  end;

  while I < SourceLength do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(TA.GetElementAsValue(I));
      CallArgs.Add(TGocciaNumberLiteralValue.Create(I));
      CallArgs.Add(AThisValue);
      Accumulator := InvokeCallable(AArgs.GetElement(0), CallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
    Inc(I);
  end;
  Result := Accumulator;
end;

// ES2026 §23.2.3.23 %TypedArray%.prototype.reduceRight(callbackfn [, initialValue])
function TGocciaTypedArrayValue.TypedArrayReduceRight(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, SourceLength: Integer;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.reduceRight');
  SourceLength := TA.Length;
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayReduceRightCallable, SSuggestTypedArrayCallable);

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    I := SourceLength - 1;
  end
  else
  begin
    if SourceLength = 0 then
      ThrowTypeError(SErrorReduceEmptyTypedArray, SSuggestReduceInitialValue);
    Accumulator := TA.GetElementAsValue(SourceLength - 1);
    I := SourceLength - 2;
  end;

  while I >= 0 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(TA.GetElementAsValue(I));
      CallArgs.Add(TGocciaNumberLiteralValue.Create(I));
      CallArgs.Add(AThisValue);
      Accumulator := InvokeCallable(AArgs.GetElement(0), CallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
    Dec(I);
  end;
  Result := Accumulator;
end;

// ES2026 §23.2.3.17 %TypedArray%.prototype.join(separator)
function TGocciaTypedArrayValue.TypedArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Sep: string;
  I, Len: Integer;
  S: string;
  Element: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.join');
  Len := TA.Length;
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Sep := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Sep := ',';
  S := '';
  for I := 0 to Len - 1 do
  begin
    if I > 0 then S := S + Sep;
    Element := TA.GetElementAsValue(I);
    if not ((Element is TGocciaUndefinedLiteralValue) or
            (Element is TGocciaNullLiteralValue)) then
      S := S + Element.ToStringLiteral.Value;
  end;
  Result := TGocciaStringLiteralValue.Create(S);
end;

// ECMA-402 §17.1.6 %TypedArray%.prototype.toLocaleString([locales [, options]])
function TGocciaTypedArrayValue.TypedArrayToLocaleString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, Len: Integer;
  S: string;
  Element, Method, Formatted: TGocciaValue;
  ElementObject: TGocciaObjectValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  TA := RequireAttachedTypedArray(AThisValue, '%TypedArray%.prototype.toLocaleString');
  Len := TA.Length;
  S := '';
  for I := 0 to Len - 1 do
  begin
    if I > 0 then S := S + ',';
    Element := TA.GetElementAsValue(I);
    if (Element is TGocciaUndefinedLiteralValue) or
       (Element is TGocciaNullLiteralValue) then
      Continue;
    ElementObject := ToObject(Element);
    Method := ElementObject.GetPropertyWithContext(PROP_TO_LOCALE_STRING, Element);
    if (Method = nil) or not Method.IsCallable then
      ThrowTypeError('TypedArray.prototype.toLocaleString element toLocaleString is not a function');

    CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(2);
    try
      if AArgs.Length > 0 then
        CallArgs.Add(AArgs.GetElement(0))
      else
        CallArgs.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
      if AArgs.Length > 1 then
        CallArgs.Add(AArgs.GetElement(1))
      else
        CallArgs.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
      Formatted := InvokeCallable(Method, CallArgs, Element);
    finally
      CallArgs.Free;
    end;
    S := S + Formatted.ToStringLiteral.Value;
  end;
  Result := TGocciaStringLiteralValue.Create(S);
end;

// ES2026 §23.2.3.33 %TypedArray%.prototype.toString()
function TGocciaTypedArrayValue.TypedArrayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TypedArrayJoin(AArgs, AThisValue);
end;

// ES2026 §23.2.3.32 %TypedArray%.prototype.toReversed()
function TGocciaTypedArrayValue.TypedArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I: Integer;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.toReversed');
  NewTA := CreateSameKindArray(TA, TA.FLength);
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 0 to TA.FLength - 1 do
      NewTA.WriteBigIntElement(I, TA.ReadBigIntElement(TA.FLength - 1 - I));
  end
  else
  begin
    for I := 0 to TA.FLength - 1 do
      NewTA.WriteElement(I, TA.ReadElement(TA.FLength - 1 - I));
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.33 %TypedArray%.prototype.toSorted([comparefn])
function TGocciaTypedArrayValue.TypedArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, Len: Integer;
  SortArgs: TGocciaArgumentsCollection;
  ResultRoot: TGocciaTempRoot;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.toSorted');
  if (AArgs.Length > 0) and
     not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) and
     not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorCustomSortMustBeFunction, SSuggestCallbackRequired);
  Len := TA.Length;
  NewTA := CreateSameKindArray(TA, Len);
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(ResultRoot, NewTA);
  try
    if IsBigIntKind(TA.FKind) then
    begin
      for I := 0 to Len - 1 do
        NewTA.WriteBigIntElement(I, TA.ReadBigIntElement(I));
    end
    else
    begin
      for I := 0 to Len - 1 do
        NewTA.WriteElement(I, TA.ReadElement(I));
    end;
    SortArgs := TGocciaArgumentsCollection.Create;
    try
      if (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable then
        SortArgs.Add(AArgs.GetElement(0));
      TypedArraySort(SortArgs, NewTA);
    finally
      SortArgs.Free;
    end;
    Result := NewTA;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

// ES2026 §23.2.3.36 %TypedArray%.prototype.with(index, value)
function TGocciaTypedArrayValue.TypedArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, ActualIndex, Len, ValidIndex: Integer;
  NewNum: TGocciaNumberLiteralValue;
  NewBigInt: Int64;
  NewVal: TGocciaValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.with');
  Len := TA.Length;
  // ES2026 §23.2.3.36 step 3: Let relativeIndex be ? ToIntegerOrInfinity(index)
  if AArgs.Length > 0 then
    ActualIndex := ToIntegerFromArgs(AArgs, 0)
  else
    ActualIndex := 0;
  if ActualIndex < 0 then
    ActualIndex := Len + ActualIndex;

  if IsBigIntKind(TA.FKind) then
  begin
    if AArgs.Length > 1 then
      NewVal := AArgs.GetElement(1)
    else
      NewVal := TGocciaUndefinedLiteralValue.UndefinedValue;
    NewBigInt := RawBigIntForKind(TA.FKind, ToBigIntForTypedArray(NewVal));
    if not TA.IsValidIntegerIndexedElement(ActualIndex, False, ValidIndex) then
      ThrowRangeError(SErrorInvalidTypedArrayIndex, SSuggestTypedArrayLength);
    NewTA := CreateSameKindArray(TA, Len);
    for I := 0 to Len - 1 do
    begin
      if I = ActualIndex then
        NewTA.WriteBigIntElement(I, NewBigInt)
      else
        NewTA.WriteBigIntElement(I, TA.ReadBigIntElement(I));
    end;
  end
  else
  begin
    // ES2026 §23.2.3.36 step 7: Let numericValue be ? ToNumber(value)
    if AArgs.Length > 1 then
      NewNum := AArgs.GetElement(1).ToNumberLiteral
    else
      NewNum := TGocciaNumberLiteralValue.NaNValue;
    if not TA.IsValidIntegerIndexedElement(ActualIndex, False, ValidIndex) then
      ThrowRangeError(SErrorInvalidTypedArrayIndex, SSuggestTypedArrayLength);
    NewTA := CreateSameKindArray(TA, Len);
    for I := 0 to Len - 1 do
    begin
      if I = ActualIndex then
        NewTA.WriteNumberLiteral(I, NewNum)
      else
        NewTA.WriteElement(I, TA.ReadElement(I));
    end;
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.34 %TypedArray%.prototype.values()
function TGocciaTypedArrayValue.TypedArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.values');
  Result := TGocciaTypedArrayIteratorValue.Create(TA, taikValues);
end;

// ES2026 §23.2.3.18 %TypedArray%.prototype.keys()
function TGocciaTypedArrayValue.TypedArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.keys');
  Result := TGocciaTypedArrayIteratorValue.Create(TA, taikKeys);
end;

// ES2026 §23.2.3.6 %TypedArray%.prototype.entries()
function TGocciaTypedArrayValue.TypedArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
begin
  TA := RequireAttachedTypedArray(AThisValue, 'TypedArray.prototype.entries');
  Result := TGocciaTypedArrayIteratorValue.Create(TA, taikEntries);
end;

{ TGocciaTypedArrayClassValue }

constructor TGocciaTypedArrayClassValue.Create(const AName: string; const ASuperClass: TGocciaClassValue; const AKind: TGocciaTypedArrayKind);
begin
  inherited Create(AName, ASuperClass);
  FKind := AKind;
end;

function TGocciaTypedArrayClassValue.NativeInstanceDefaultPrototype: TGocciaObjectValue;
begin
  Result := Prototype;
end;

function TGocciaTypedArrayClassValue.DefaultPrototypeForNewTarget(
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
var
  ConstructorPrototype, ConstructorValue, ProtoValue: TGocciaValue;
  FallbackRealm: TGocciaRealm;
  GlobalScope: TGocciaScope;
begin
  if ANewTarget is TGocciaObjectValue then
    ProtoValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE)
  else
    ProtoValue := nil;

  if ProtoValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ProtoValue));

  FallbackRealm := nil;
  if ANewTarget is TGocciaFunctionBase then
    FallbackRealm := TGocciaFunctionBase(ANewTarget).CreationRealm;

  if Assigned(FallbackRealm) and
     (FallbackRealm.GlobalEnv is TGocciaScope) then
  begin
    GlobalScope := TGocciaScope(FallbackRealm.GlobalEnv);
    if GlobalScope.TryGetBindingValue(TGocciaTypedArrayValue.KindName(FKind),
       ConstructorValue) and
       (ConstructorValue is TGocciaObjectValue) then
    begin
      ConstructorPrototype :=
        TGocciaObjectValue(ConstructorValue).GetProperty(PROP_PROTOTYPE);
      if ConstructorPrototype is TGocciaObjectValue then
        Exit(TGocciaObjectValue(ConstructorPrototype));
    end;
  end;

  Result := ACurrentRealmDefault;
end;

function TGocciaTypedArrayClassValue.GetClassLength: Integer;
begin
  Result := 3;
end;

function TGocciaTypedArrayClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  FirstArg: TGocciaValue;
  Num: TGocciaNumberLiteralValue;
  Len, BPE, ByteOff, ElemLen: Integer;
  Buf: TGocciaArrayBufferValue;
  SAB: TGocciaSharedArrayBufferValue;
  SrcTA: TGocciaTypedArrayValue;
  SrcArr: TGocciaArrayValue;
  I: Integer;
  NewTA: TGocciaTypedArrayValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  Values: TGocciaValueList;
  BufferByteLength: Integer;
  HasLength: Boolean;
  RawLen: Double;
  Val, NextMethod: TGocciaValue;
  ResultRoot, IteratorRoot, NextMethodRoot: TGocciaTempRoot;
  ValueRoots: array of TGocciaTempRoot;
  ValueRootCount: Integer;
begin
  BPE := TGocciaTypedArrayValue.BytesPerElement(FKind);

  if (AArguments = nil) or (AArguments.Length = 0) then
    Exit(TGocciaTypedArrayValue.Create(FKind, 0));

  FirstArg := AArguments.GetElement(0);

  // new TypedArray(typedArray)
  if FirstArg is TGocciaTypedArrayValue then
  begin
    SrcTA := TGocciaTypedArrayValue(FirstArg);
    EnsureTypedArrayAttached(SrcTA, TGocciaTypedArrayValue.KindName(SrcTA.Kind));
    if TGocciaTypedArrayValue.IsBigIntKind(FKind) <>
       TGocciaTypedArrayValue.IsBigIntKind(SrcTA.Kind) then
      ThrowTypeError(SErrorBigIntTypedArrayCannotMix, SSuggestBigIntTypedArrayValue);
    NewTA := TGocciaTypedArrayValue.Create(FKind, SrcTA.Length);
    if TGocciaTypedArrayValue.IsBigIntKind(FKind) then
    begin
      for I := 0 to SrcTA.Length - 1 do
        NewTA.WriteBigIntElement(I, SrcTA.ReadBigIntElement(I));
    end
    else
    begin
      for I := 0 to SrcTA.Length - 1 do
        NewTA.WriteElement(I, SrcTA.ReadElement(I));
    end;
    Exit(NewTA);
  end;

  // new TypedArray(buffer [, byteOffset [, length]])
  if FirstArg is TGocciaArrayBufferValue then
  begin
    Buf := TGocciaArrayBufferValue(FirstArg);
    if AArguments.Length > 1 then
      ByteOff := ToIntegerOrInfinityBounded(AArguments.GetElement(1))
    else
      ByteOff := 0;

    if ByteOff < 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetNonNegative, [TGocciaTypedArrayValue.KindName(FKind)]), SSuggestTypedArrayLength);
    if (ByteOff mod BPE) <> 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);

    HasLength := (AArguments.Length > 2) and
      not (AArguments.GetElement(2) is TGocciaUndefinedLiteralValue);
    if HasLength then
    begin
      ElemLen := ToIntegerOrInfinityBounded(AArguments.GetElement(2));
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end
    else
      ElemLen := -1;

    if Buf.Detached then
      ThrowTypeError(Format(SErrorCannotUseDetachedTypedArray,
        [TGocciaTypedArrayValue.KindName(FKind)]), SSuggestArrayBufferDetached);

    BufferByteLength := System.Length(Buf.Data);
    if ByteOff > BufferByteLength then
      ThrowRangeError(SErrorTypedArrayStartOffsetBounds, SSuggestTypedArrayLength);

    if HasLength then
    begin
      if Int64(ByteOff) + Int64(ElemLen) * Int64(BPE) > Int64(BufferByteLength) then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end
    else if Buf.MaxByteLength >= 0 then
      ElemLen := -1
    else
    begin
      if ((Int64(BufferByteLength) - Int64(ByteOff)) mod Int64(BPE)) <> 0 then
        ThrowRangeError(Format(SErrorTypedArrayByteLengthMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);
      if (BufferByteLength - ByteOff) < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
      ElemLen := (BufferByteLength - ByteOff) div BPE;
    end;

    Exit(TGocciaTypedArrayValue.Create(FKind, Buf, ByteOff, ElemLen));
  end;

  // new TypedArray(sharedBuffer [, byteOffset [, length]])
  if FirstArg is TGocciaSharedArrayBufferValue then
  begin
    SAB := TGocciaSharedArrayBufferValue(FirstArg);
    if AArguments.Length > 1 then
      ByteOff := ToIntegerOrInfinityBounded(AArguments.GetElement(1))
    else
      ByteOff := 0;

    if ByteOff < 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetNonNegative, [TGocciaTypedArrayValue.KindName(FKind)]), SSuggestTypedArrayLength);
    if (ByteOff mod BPE) <> 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);

    HasLength := (AArguments.Length > 2) and
      not (AArguments.GetElement(2) is TGocciaUndefinedLiteralValue);
    if HasLength then
    begin
      ElemLen := ToIntegerOrInfinityBounded(AArguments.GetElement(2));
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end
    else
      ElemLen := -1;

    BufferByteLength := System.Length(SAB.Data);
    if ByteOff > BufferByteLength then
      ThrowRangeError(SErrorTypedArrayStartOffsetBounds, SSuggestTypedArrayLength);

    if HasLength then
    begin
      if Int64(ByteOff) + Int64(ElemLen) * Int64(BPE) > Int64(BufferByteLength) then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end
    else
    begin
      if ((Int64(BufferByteLength) - Int64(ByteOff)) mod Int64(BPE)) <> 0 then
        ThrowRangeError(Format(SErrorTypedArrayByteLengthMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);
      ElemLen := Integer((Int64(BufferByteLength) - Int64(ByteOff)) div Int64(BPE));
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end;

    Exit(TGocciaTypedArrayValue.Create(FKind, SAB, ByteOff, ElemLen));
  end;

  // ES2026 §23.2.5.1 step 5c: @@iterator check before array fast path
  if FirstArg is TGocciaObjectValue then
  begin
    Iterator := GetIteratorFromValue(FirstArg);
    if Assigned(Iterator) then
    begin
      if not (Iterator is TGocciaGenericIteratorValue) then
      begin
        InitializeTempRoot(IteratorRoot);
        InitializeTempRoot(NextMethodRoot);
        AddTempRootIfNeeded(IteratorRoot, Iterator);
        try
          NextMethod := Iterator.GetProperty(PROP_NEXT);
          AddTempRootIfNeeded(NextMethodRoot, NextMethod);
          try
            Iterator := CreateRootedGenericIterator(Iterator, NextMethod);
          finally
            RemoveTempRootIfNeeded(NextMethodRoot);
          end;
        finally
          RemoveTempRootIfNeeded(IteratorRoot);
        end;
      end;
      Values := TGocciaValueList.Create(False);
      ValueRootCount := 0;
      InitializeTempRoot(ResultRoot);
      try
        TGarbageCollector.Instance.AddTempRoot(Iterator);
        try
          try
            IterResult := Iterator.AdvanceNext;
            while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
            begin
              Val := IterResult.GetProperty(PROP_VALUE);
              Values.Add(Val);
              if ValueRootCount >= Length(ValueRoots) then
                SetLength(ValueRoots, ValueRootCount * 2 + 4);
              InitializeTempRoot(ValueRoots[ValueRootCount]);
              AddTempRootIfNeeded(ValueRoots[ValueRootCount], Val);
              Inc(ValueRootCount);
              IterResult := Iterator.AdvanceNext;
            end;
          except
            AcquireExceptionObject;
            CloseIteratorPreservingError(Iterator);
            raise;
          end;
        finally
          TGarbageCollector.Instance.RemoveTempRoot(Iterator);
        end;
        NewTA := TGocciaTypedArrayValue.Create(FKind, Values.Count);
        AddTempRootIfNeeded(ResultRoot, NewTA);
        try
          for I := 0 to Values.Count - 1 do
            NewTA.WriteValueToElement(I, Values[I]);
        finally
          RemoveTempRootIfNeeded(ResultRoot);
        end;
      finally
        for I := ValueRootCount - 1 downto 0 do
          RemoveTempRootIfNeeded(ValueRoots[I]);
        SetLength(ValueRoots, 0);
        Values.Free;
      end;
      Exit(NewTA);
    end;

    if FirstArg is TGocciaArrayValue then
    begin
      SrcArr := TGocciaArrayValue(FirstArg);
      NewTA := TGocciaTypedArrayValue.Create(FKind, SrcArr.Elements.Count);
      for I := 0 to SrcArr.Elements.Count - 1 do
        NewTA.WriteValueToElement(I, SrcArr.Elements[I]);
      Exit(NewTA);
    end;

    Len := LengthOfArrayLikeEx(TGocciaObjectValue(FirstArg), RawLen);
    if RawLen > High(Integer) then
      ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    NewTA := TGocciaTypedArrayValue.Create(FKind, Len);
    for I := 0 to Len - 1 do
      NewTA.WriteValueToElement(I, TGocciaObjectValue(FirstArg).GetProperty(IntToStr(I)));
    Exit(NewTA);
  end;

  // new TypedArray(length) — ES2026 §23.2.1.2 step 5: ToIndex(length)
  // ToIndex calls ToNumber, which throws TypeError for BigInt (§7.1.4)
  Num := FirstArg.ToNumberLiteral;
  if Num.IsNaN then
    Len := 0
  else if Num.IsInfinity or Num.IsNegativeInfinity then
    ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength)
  else
  begin
    if Num.Value >= MaxInt then
      ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    Len := Trunc(Num.Value);
    if Len < 0 then
      ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
  end;
  Result := TGocciaTypedArrayValue.Create(FKind, Len);
end;

{ TGocciaTypedArrayStaticFrom }

constructor TGocciaTypedArrayStaticFrom.Create;
begin
  inherited Create;
end;

// ES2026 §23.2.2.1 %TypedArray%.from(source [, mapfn [, thisArg]])
function TGocciaTypedArrayStaticFrom.TypedArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Source: TGocciaValue;
  SrcArr: TGocciaArrayValue;
  SrcTA: TGocciaTypedArrayValue;
  NewTA: TGocciaTypedArrayValue;
  HasMapFn: Boolean;
  MapFnArg: TGocciaValue;
  ThisArg: TGocciaValue;
  I, Len: Integer;
  Val, NextMethod: TGocciaValue;
  MapArgs: TGocciaArgumentsCollection;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  ConstructorValue: TGocciaValue;
  Values: TGocciaValueList;
  SrcObj: TGocciaObjectValue;
  SourceRoot, MapFnRoot, ThisRoot, ResultRoot, IteratorRoot,
  NextMethodRoot: TGocciaTempRoot;
  ValueRoots: array of TGocciaTempRoot;
  ValueRootCount: Integer;
begin
  ConstructorValue := AThisValue;
  if (not Assigned(ConstructorValue)) or (not ConstructorValue.IsConstructable) then
    ThrowTypeError(Format(SErrorTypedArrayStaticReceiver, ['TypedArray.from']),
      SSuggestTypedArrayConstructorReceiver);
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorTypedArrayFromRequiresArg, SSuggestTypedArraySetSource);
  Source := AArgs.GetElement(0);
  HasMapFn := False;
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    MapFnArg := AArgs.GetElement(1);
    if not MapFnArg.IsCallable then
      ThrowTypeError(SErrorTypedArrayFromMapFn, SSuggestTypedArrayCallable);
    HasMapFn := True;
  end;
  if AArgs.Length > 2 then
    ThisArg := AArgs.GetElement(2)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  InitializeTempRoot(SourceRoot);
  InitializeTempRoot(MapFnRoot);
  InitializeTempRoot(ThisRoot);
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(SourceRoot, Source);
  if HasMapFn then
  begin
    AddTempRootIfNeeded(MapFnRoot, MapFnArg);
    AddTempRootIfNeeded(ThisRoot, ThisArg);
  end;
  try
    // ES2026 §23.2.2.1 step 5: GetMethod(source, @@iterator) before type fast paths
    Iterator := GetIteratorFromValue(Source);
    if Assigned(Iterator) then
    begin
      if not (Iterator is TGocciaGenericIteratorValue) then
      begin
        InitializeTempRoot(IteratorRoot);
        InitializeTempRoot(NextMethodRoot);
        AddTempRootIfNeeded(IteratorRoot, Iterator);
        try
          NextMethod := Iterator.GetProperty(PROP_NEXT);
          AddTempRootIfNeeded(NextMethodRoot, NextMethod);
          try
            Iterator := CreateRootedGenericIterator(Iterator, NextMethod);
          finally
            RemoveTempRootIfNeeded(NextMethodRoot);
          end;
        finally
          RemoveTempRootIfNeeded(IteratorRoot);
        end;
      end;
      Values := TGocciaValueList.Create(False);
      ValueRootCount := 0;
      try
        TGarbageCollector.Instance.AddTempRoot(Iterator);
        try
          try
            IterResult := Iterator.AdvanceNext;
            while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
            begin
              Val := IterResult.GetProperty(PROP_VALUE);
              Values.Add(Val);
              if ValueRootCount >= Length(ValueRoots) then
                SetLength(ValueRoots, ValueRootCount * 2 + 4);
              InitializeTempRoot(ValueRoots[ValueRootCount]);
              AddTempRootIfNeeded(ValueRoots[ValueRootCount], Val);
              Inc(ValueRootCount);
              IterResult := Iterator.AdvanceNext;
            end;
          except
            AcquireExceptionObject;
            CloseIteratorPreservingError(Iterator);
            raise;
          end;
        finally
          TGarbageCollector.Instance.RemoveTempRoot(Iterator);
        end;
        NewTA := CreateTypedArrayFromConstructor(ConstructorValue,
          'TypedArray.from', Values.Count);
        AddTempRootIfNeeded(ResultRoot, NewTA);
        try
          for I := 0 to Values.Count - 1 do
          begin
            Val := Values[I];
            if HasMapFn then
            begin
              MapArgs := TGocciaArgumentsCollection.Create;
              try
                MapArgs.Add(Val);
                MapArgs.Add(TGocciaNumberLiteralValue.Create(I));
                Val := InvokeCallable(MapFnArg, MapArgs, ThisArg);
              finally
                MapArgs.Free;
              end;
            end;
            NewTA.WriteValueToElement(I, Val);
          end;
        finally
          RemoveTempRootIfNeeded(ResultRoot);
        end;
      finally
        for I := ValueRootCount - 1 downto 0 do
          RemoveTempRootIfNeeded(ValueRoots[I]);
        SetLength(ValueRoots, 0);
        Values.Free;
      end;
      Exit(NewTA);
    end;

    if Source is TGocciaTypedArrayValue then
    begin
      SrcTA := TGocciaTypedArrayValue(Source);
      NewTA := CreateTypedArrayFromConstructor(ConstructorValue,
        'TypedArray.from', SrcTA.Length);
      AddTempRootIfNeeded(ResultRoot, NewTA);
      try
        for I := 0 to SrcTA.Length - 1 do
        begin
          Val := SrcTA.GetElementAsValue(I);
          if HasMapFn then
          begin
            MapArgs := TGocciaArgumentsCollection.Create;
            try
              MapArgs.Add(Val);
              MapArgs.Add(TGocciaNumberLiteralValue.Create(I));
              Val := InvokeCallable(MapFnArg, MapArgs, ThisArg);
            finally
              MapArgs.Free;
            end;
          end;
          NewTA.WriteValueToElement(I, Val);
        end;
      finally
        RemoveTempRootIfNeeded(ResultRoot);
      end;
      Exit(NewTA);
    end;

    if Source is TGocciaArrayValue then
    begin
      SrcArr := TGocciaArrayValue(Source);
      NewTA := CreateTypedArrayFromConstructor(ConstructorValue,
        'TypedArray.from', SrcArr.Elements.Count);
      AddTempRootIfNeeded(ResultRoot, NewTA);
      try
        for I := 0 to SrcArr.Elements.Count - 1 do
        begin
          Val := SrcArr.Elements[I];
          if HasMapFn then
          begin
            MapArgs := TGocciaArgumentsCollection.Create;
            try
              MapArgs.Add(Val);
              MapArgs.Add(TGocciaNumberLiteralValue.Create(I));
              Val := InvokeCallable(MapFnArg, MapArgs, ThisArg);
            finally
              MapArgs.Free;
            end;
          end;
          NewTA.WriteValueToElement(I, Val);
        end;
      finally
        RemoveTempRootIfNeeded(ResultRoot);
      end;
      Exit(NewTA);
    end;

    // Step 7: array-like path — ToObject(source), LengthOfArrayLike, indexed Get
    SrcObj := ToObject(Source);
    Len := LengthOfArrayLike(SrcObj);
    NewTA := CreateTypedArrayFromConstructor(ConstructorValue,
      'TypedArray.from', Len);
    AddTempRootIfNeeded(ResultRoot, NewTA);
    try
      for I := 0 to Len - 1 do
      begin
        Val := SrcObj.GetProperty(IntToStr(I));
        if HasMapFn then
        begin
          MapArgs := TGocciaArgumentsCollection.Create;
          try
            MapArgs.Add(Val);
            MapArgs.Add(TGocciaNumberLiteralValue.Create(I));
            Val := InvokeCallable(MapFnArg, MapArgs, ThisArg);
          finally
            MapArgs.Free;
          end;
        end;
        NewTA.WriteValueToElement(I, Val);
      end;
      Result := NewTA;
    finally
      RemoveTempRootIfNeeded(ResultRoot);
    end;
  finally
    RemoveTempRootIfNeeded(ThisRoot);
    RemoveTempRootIfNeeded(MapFnRoot);
    RemoveTempRootIfNeeded(SourceRoot);
  end;
end;

// ES2026 §23.2.2.2 %TypedArray%.of(...items)
function TGocciaTypedArrayStaticFrom.TypedArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NewTA: TGocciaTypedArrayValue;
  ConstructorValue: TGocciaValue;
  I: Integer;
  ResultRoot: TGocciaTempRoot;
begin
  ConstructorValue := AThisValue;
  if (not Assigned(ConstructorValue)) or (not ConstructorValue.IsConstructable) then
    ThrowTypeError(Format(SErrorTypedArrayStaticReceiver, ['TypedArray.of']),
      SSuggestTypedArrayConstructorReceiver);
  NewTA := CreateTypedArrayFromConstructor(ConstructorValue,
    'TypedArray.of', AArgs.Length);
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(ResultRoot, NewTA);
  try
    for I := 0 to AArgs.Length - 1 do
      NewTA.WriteValueToElement(I, AArgs.GetElement(I));
    Result := NewTA;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);
  GTypedArraySharedSlot := RegisterRealmOwnedSlot('TypedArray.shared');

end.
