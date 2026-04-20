unit Goccia.Values.TypedArrayValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ClassValue,
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

    procedure InitializePrototype;

    function ReadElement(const AIndex: Integer): Double;
    procedure WriteElement(const AIndex: Integer; const AValue: Double);
    procedure WriteNumberLiteral(const AIndex: Integer; const ANum: TGocciaNumberLiteralValue);

    function ReadBigIntElement(const AIndex: Integer): Int64;
    procedure WriteBigIntElement(const AIndex: Integer; const AValue: Int64);

    function GetElementAsValue(const AIndex: Integer): TGocciaValue;
    procedure WriteValueToElement(const AIndex: Integer; const AValue: TGocciaValue);

  public
    constructor Create(const AKind: TGocciaTypedArrayKind; const ALength: Integer); overload;
    constructor Create(const AKind: TGocciaTypedArrayKind; const ABuffer: TGocciaArrayBufferValue;
      const AByteOffset: Integer = 0; const ALength: Integer = -1); overload;
    constructor Create(const AKind: TGocciaTypedArrayKind; const ASharedBuffer: TGocciaSharedArrayBufferValue;
      const AByteOffset: Integer = 0; const ALength: Integer = -1); overload;

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    class function BytesPerElement(const AKind: TGocciaTypedArrayKind): Integer;
    class function IsFloatKind(const AKind: TGocciaTypedArrayKind): Boolean; inline;
    class function IsBigIntKind(const AKind: TGocciaTypedArrayKind): Boolean; inline;
    class function KindName(const AKind: TGocciaTypedArrayKind): string;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class procedure SetSharedPrototypeParent(const AParent: TGocciaObjectValue);
    class procedure SetUint8Prototype(const APrototype: TGocciaObjectValue);

    property BufferValue: TGocciaValue read FBufferValue;
    property BufferData: TBytes read FBufferData;
    property ByteOffset: Integer read FByteOffset;
    property Length: Integer read FLength;
    property Kind: TGocciaTypedArrayKind read FKind;
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
    property Kind: TGocciaTypedArrayKind read FKind;
  end;

  TGocciaTypedArrayStaticFrom = class
  private
    FKind: TGocciaTypedArrayKind;
  public
    constructor Create(const AKind: TGocciaTypedArrayKind);
    function TypedArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TypedArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Float16,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FUint8Prototype: TGocciaObjectValue;

class function TGocciaTypedArrayValue.BytesPerElement(const AKind: TGocciaTypedArrayKind): Integer;
begin
  case AKind of
    takInt8, takUint8, takUint8Clamped: Result := 1;
    takInt16, takUint16, takFloat16: Result := 2;
    takInt32, takUint32, takFloat32: Result := 4;
    takFloat64, takBigInt64, takBigUint64: Result := 8;
  else
    Result := 1;
  end;
end;

class function TGocciaTypedArrayValue.IsFloatKind(const AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := AKind in [takFloat16, takFloat32, takFloat64];
end;

class function TGocciaTypedArrayValue.IsBigIntKind(const AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := AKind in [takBigInt64, takBigUint64];
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

{ Element read/write via buffer }

function TGocciaTypedArrayValue.ReadElement(const AIndex: Integer): Double;
var
  Offset: Integer;
  I8: ShortInt;
  U8: Byte;
  I16: SmallInt;
  U16: Word;
  I32: LongInt;
  U32: LongWord;
  I64: Int64;
  F32: Single;
  F64: Double;
begin
  Offset := FByteOffset + AIndex * BytesPerElement(FKind);
  case FKind of
    takInt8:
    begin
      I8 := ShortInt(FBufferData[Offset]);
      Result := I8;
    end;
    takUint8, takUint8Clamped:
    begin
      U8 := FBufferData[Offset];
      Result := U8;
    end;
    takInt16:
    begin
      Move(FBufferData[Offset], I16, 2);
      Result := I16;
    end;
    takUint16:
    begin
      Move(FBufferData[Offset], U16, 2);
      Result := U16;
    end;
    takInt32:
    begin
      Move(FBufferData[Offset], I32, 4);
      I64 := I32;
      Result := I64;
    end;
    takUint32:
    begin
      Move(FBufferData[Offset], U32, 4);
      I64 := U32;
      Result := I64;
    end;
    takFloat16:
    begin
      Move(FBufferData[Offset], U16, 2);
      Result := Float16ToDouble(U16);
    end;
    takFloat32:
    begin
      Move(FBufferData[Offset], F32, 4);
      Result := F32;
    end;
    takFloat64:
    begin
      Move(FBufferData[Offset], F64, 8);
      Result := F64;
    end;
  else
    Result := 0;
  end;
end;

procedure TGocciaTypedArrayValue.WriteElement(const AIndex: Integer; const AValue: Double);
var
  Offset: Integer;
  I8: ShortInt;
  U8: Byte;
  I16: SmallInt;
  U16: Word;
  I32: LongInt;
  U32: LongWord;
  F32: Single;
  F64: Double;
  Clamped: Integer;
begin
  Offset := FByteOffset + AIndex * BytesPerElement(FKind);
  case FKind of
    takInt8:
    begin
      I8 := ShortInt(Trunc(AValue));
      FBufferData[Offset] := Byte(I8);
    end;
    takUint8:
    begin
      U8 := Byte(Trunc(AValue));
      FBufferData[Offset] := U8;
    end;
    takUint8Clamped:
    begin
      if IsNan(AValue) then
        Clamped := 0
      else if AValue <= 0 then
        Clamped := 0
      else if AValue >= 255 then
        Clamped := 255
      else
        Clamped := Round(AValue);
      FBufferData[Offset] := Byte(Clamped);
    end;
    takInt16:
    begin
      I16 := SmallInt(Trunc(AValue));
      Move(I16, FBufferData[Offset], 2);
    end;
    takUint16:
    begin
      U16 := Word(Trunc(AValue));
      Move(U16, FBufferData[Offset], 2);
    end;
    takInt32:
    begin
      I32 := LongInt(Trunc(AValue));
      Move(I32, FBufferData[Offset], 4);
    end;
    takUint32:
    begin
      U32 := LongWord(Trunc(AValue));
      Move(U32, FBufferData[Offset], 4);
    end;
    takFloat16:
    begin
      U16 := DoubleToFloat16(AValue);
      Move(U16, FBufferData[Offset], 2);
    end;
    takFloat32:
    begin
      F32 := AValue;
      Move(F32, FBufferData[Offset], 4);
    end;
    takFloat64:
    begin
      F64 := AValue;
      Move(F64, FBufferData[Offset], 8);
    end;
  end;
end;

procedure WriteFloatDirect(var AData: TBytes;
  const AOffset: Integer; const AKind: TGocciaTypedArrayKind;
  const AValue: Double);
var
  F16: Word;
  F32: Single;
begin
  case AKind of
    takFloat16:
    begin
      F16 := DoubleToFloat16(AValue);
      Move(F16, AData[AOffset], 2);
    end;
    takFloat32:
    begin
      F32 := AValue;
      Move(F32, AData[AOffset], 4);
    end;
    takFloat64:
      Move(AValue, AData[AOffset], 8);
  end;
end;

procedure TGocciaTypedArrayValue.WriteNumberLiteral(const AIndex: Integer; const ANum: TGocciaNumberLiteralValue);
var
  Offset: Integer;
begin
  if ANum.IsNaN then
  begin
    if IsFloatKind(FKind) then
    begin
      Offset := FByteOffset + AIndex * BytesPerElement(FKind);
      WriteFloatDirect(FBufferData, Offset, FKind, ANum.Value);
    end
    else
      WriteElement(AIndex, 0);
  end
  else if ANum.IsInfinity then
  begin
    case FKind of
      takUint8Clamped: WriteElement(AIndex, 255);
      takFloat16, takFloat32, takFloat64:
      begin
        Offset := FByteOffset + AIndex * BytesPerElement(FKind);
        WriteFloatDirect(FBufferData, Offset, FKind, ANum.Value);
      end;
    else
      WriteElement(AIndex, 0);
    end;
  end
  else if ANum.IsNegativeInfinity then
  begin
    case FKind of
      takFloat16, takFloat32, takFloat64:
      begin
        Offset := FByteOffset + AIndex * BytesPerElement(FKind);
        WriteFloatDirect(FBufferData, Offset, FKind, ANum.Value);
      end;
    else
      WriteElement(AIndex, 0);
    end;
  end
  else
    WriteElement(AIndex, ANum.Value);
end;

function TGocciaTypedArrayValue.ReadBigIntElement(const AIndex: Integer): Int64;
var
  Offset: Integer;
  I64: Int64;
  U64: QWord;
begin
  Offset := FByteOffset + AIndex * 8;
  case FKind of
    takBigInt64:
    begin
      Move(FBufferData[Offset], I64, 8);
      Result := I64;
    end;
    takBigUint64:
    begin
      Move(FBufferData[Offset], U64, 8);
      Result := Int64(U64);
    end;
  else
    Result := 0;
  end;
end;

procedure TGocciaTypedArrayValue.WriteBigIntElement(const AIndex: Integer; const AValue: Int64);
var
  Offset: Integer;
begin
  Offset := FByteOffset + AIndex * 8;
  Move(AValue, FBufferData[Offset], 8);
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

function TGocciaTypedArrayValue.GetElementAsValue(const AIndex: Integer): TGocciaValue;
var
  Raw: Int64;
begin
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
  begin
    if not (AValue is TGocciaBigIntValue) then
      ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt, SSuggestBigIntTypedArrayValue);
    WriteBigIntElement(AIndex, TGocciaBigIntValue(AValue).Value.ToInt64);
  end
  else
    WriteNumberLiteral(AIndex, AValue.ToNumberLiteral);
end;

{ Constructors }

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind; const ALength: Integer);
var
  ByteLen: Integer;
  Buf: TGocciaArrayBufferValue;
begin
  inherited Create(nil);
  FKind := AKind;
  FByteOffset := 0;
  FLength := ALength;
  ByteLen := ALength * BytesPerElement(AKind);
  Buf := TGocciaArrayBufferValue.Create(ByteLen);
  FBufferValue := Buf;
  FBufferData := Buf.Data;
  InitializePrototype;
  if (AKind = takUint8) and Assigned(FUint8Prototype) then
    FPrototype := FUint8Prototype
  else if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind;
  const ABuffer: TGocciaArrayBufferValue; const AByteOffset: Integer; const ALength: Integer);
var
  BPE: Integer;
begin
  inherited Create(nil);
  FKind := AKind;
  FBufferValue := ABuffer;
  FBufferData := ABuffer.Data;
  FByteOffset := AByteOffset;
  BPE := BytesPerElement(AKind);

  if ALength >= 0 then
    FLength := ALength
  else
    FLength := (System.Length(ABuffer.Data) - AByteOffset) div BPE;

  InitializePrototype;
  if (AKind = takUint8) and Assigned(FUint8Prototype) then
    FPrototype := FUint8Prototype
  else if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind;
  const ASharedBuffer: TGocciaSharedArrayBufferValue; const AByteOffset: Integer; const ALength: Integer);
var
  BPE: Integer;
begin
  inherited Create(nil);
  FKind := AKind;
  FBufferValue := ASharedBuffer;
  FBufferData := ASharedBuffer.Data;
  FByteOffset := AByteOffset;
  BPE := BytesPerElement(AKind);

  if ALength >= 0 then
    FLength := ALength
  else
    FLength := (System.Length(ASharedBuffer.Data) - AByteOffset) div BPE;

  InitializePrototype;
  if (AKind = takUint8) and Assigned(FUint8Prototype) then
    FPrototype := FUint8Prototype
  else if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

{ Prototype initialization }

procedure TGocciaTypedArrayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if System.Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(TypedArrayAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayFill, 3, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayCopyWithin, 3, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArraySlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArraySubarray, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArraySet, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayReverse, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArraySort, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayIndexOf, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayLastIndexOf, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayIncludes, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayFind, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayFindIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayFindLast, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayFindLastIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayEvery, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArraySome, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayFilter, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayReduce, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayReduceRight, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(TypedArrayJoin, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
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
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTypedArrayValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaTypedArrayValue.Create(takUint8, 0);
  if AConstructor is TGocciaClassValue then
  begin
    TGocciaClassValue(AConstructor).Prototype.Prototype := FShared.Prototype;
    TGocciaClassValue(AConstructor).Prototype.AssignProperty(PROP_CONSTRUCTOR, AConstructor);
  end;
end;

class procedure TGocciaTypedArrayValue.SetSharedPrototypeParent(const AParent: TGocciaObjectValue);
begin
  if Assigned(FShared) and not Assigned(FShared.Prototype.Prototype) then
    FShared.Prototype.Prototype := AParent;
end;

class procedure TGocciaTypedArrayValue.SetUint8Prototype(const APrototype: TGocciaObjectValue);
begin
  FUint8Prototype := APrototype;
end;

{ Property access — indexed elements }

function TGocciaTypedArrayValue.GetProperty(const AName: string): TGocciaValue;
var
  Index: Integer;
begin
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < FLength) then
      Result := GetElementAsValue(Index)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  if AName = PROP_LENGTH then
    Exit(TGocciaNumberLiteralValue.Create(FLength));
  if AName = PROP_BYTE_LENGTH then
    Exit(TGocciaNumberLiteralValue.Create(FLength * BytesPerElement(FKind)));
  if AName = PROP_BYTE_OFFSET then
    Exit(TGocciaNumberLiteralValue.Create(FByteOffset));
  if AName = PROP_BUFFER then
    Exit(FBufferValue);
  if AName = PROP_BYTES_PER_ELEMENT then
    Exit(TGocciaNumberLiteralValue.Create(BytesPerElement(FKind)));
  Result := inherited GetProperty(AName);
end;

procedure TGocciaTypedArrayValue.AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean);
var
  Index: Integer;
begin
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < FLength) then
      WriteValueToElement(Index, AValue);
    Exit;
  end;
  inherited AssignProperty(AName, AValue, ACanCreate);
end;

function TGocciaTypedArrayValue.HasOwnProperty(const AName: string): Boolean;
var
  Index: Integer;
begin
  if TryStrToInt(AName, Index) and (Index >= 0) and (Index < FLength) then
    Result := True
  else if AName = PROP_LENGTH then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
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

function InvokeCallback(const ACallback: TGocciaValue; const AElement, AIndex, AArray, AThisArg: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(AElement);
    Args.Add(AIndex);
    Args.Add(AArray);
    Result := TGocciaFunctionBase(ACallback).Call(Args, AThisArg);
  finally
    Args.Free;
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
  Result := TGocciaNumberLiteralValue.Create(TA.FLength * BytesPerElement(TA.FKind));
end;

function TGocciaTypedArrayValue.TypedArrayByteOffsetGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(RequireTypedArray(AThisValue, 'TypedArray.prototype.byteOffset').FByteOffset);
end;

function TGocciaTypedArrayValue.TypedArrayLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(RequireTypedArray(AThisValue, 'TypedArray.prototype.length').FLength);
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
  RelIndex, ActualIndex: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.at');
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  RelIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  if RelIndex >= 0 then
    ActualIndex := RelIndex
  else
    ActualIndex := TA.FLength + RelIndex;
  if (ActualIndex < 0) or (ActualIndex >= TA.FLength) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TA.GetElementAsValue(ActualIndex);
end;

// ES2026 §23.2.3.8 %TypedArray%.prototype.fill(value [, start [, end]])
function TGocciaTypedArrayValue.TypedArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  FillNum: TGocciaNumberLiteralValue;
  FillBigInt: Int64;
  First, Final, I, RelStart, RelEnd: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.fill');
  FillBigInt := 0;
  FillNum := nil;
  if IsBigIntKind(TA.FKind) then
  begin
    if (AArgs.Length = 0) or not (AArgs.GetElement(0) is TGocciaBigIntValue) then
      ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt, SSuggestBigIntTypedArrayValue);
    FillBigInt := TGocciaBigIntValue(AArgs.GetElement(0)).Value.ToInt64;
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
    RelStart := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if RelStart < 0 then
      First := Max(TA.FLength + RelStart, 0)
    else
      First := Min(RelStart, TA.FLength);
  end
  else
    First := 0;

  // ES2026 §23.2.3.8 step 7-8
  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
  begin
    RelEnd := Trunc(AArgs.GetElement(2).ToNumberLiteral.Value);
    if RelEnd < 0 then
      Final := Max(TA.FLength + RelEnd, 0)
    else
      Final := Min(RelEnd, TA.FLength);
  end
  else
    Final := TA.FLength;

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
  Target, Start, Final, Count, Direction, From, To_: Integer;
  BPE: Integer;
  TempBuf: TBytes;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.copyWithin');
  if AArgs.Length < 2 then
    Exit(AThisValue);

  Target := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Start := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);

  if Target < 0 then Target := Max(TA.FLength + Target, 0) else Target := Min(Target, TA.FLength);
  if Start < 0 then Start := Max(TA.FLength + Start, 0) else Start := Min(Start, TA.FLength);

  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
  begin
    Final := Trunc(AArgs.GetElement(2).ToNumberLiteral.Value);
    if Final < 0 then Final := Max(TA.FLength + Final, 0) else Final := Min(Final, TA.FLength);
  end
  else
    Final := TA.FLength;

  Count := Min(Final - Start, TA.FLength - Target);
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
  First, Final, NewLen, I: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.slice');

  if AArgs.Length > 0 then
  begin
    First := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
    if First < 0 then First := Max(TA.FLength + First, 0) else First := Min(First, TA.FLength);
  end
  else
    First := 0;

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    Final := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if Final < 0 then Final := Max(TA.FLength + Final, 0) else Final := Min(Final, TA.FLength);
  end
  else
    Final := TA.FLength;

  NewLen := Max(Final - First, 0);
  NewTA := CreateSameKindArray(TA, NewLen);
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 0 to NewLen - 1 do
      NewTA.WriteBigIntElement(I, TA.ReadBigIntElement(First + I));
  end
  else
  begin
    for I := 0 to NewLen - 1 do
      NewTA.WriteElement(I, TA.ReadElement(First + I));
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.30 %TypedArray%.prototype.subarray(begin, end)
function TGocciaTypedArrayValue.TypedArraySubarray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  BeginIdx, EndIdx, NewLen, BPE: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.subarray');
  BPE := BytesPerElement(TA.FKind);

  if AArgs.Length > 0 then
  begin
    BeginIdx := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
    if BeginIdx < 0 then BeginIdx := Max(TA.FLength + BeginIdx, 0) else BeginIdx := Min(BeginIdx, TA.FLength);
  end
  else
    BeginIdx := 0;

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    EndIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if EndIdx < 0 then EndIdx := Max(TA.FLength + EndIdx, 0) else EndIdx := Min(EndIdx, TA.FLength);
  end
  else
    EndIdx := TA.FLength;

  NewLen := Max(EndIdx - BeginIdx, 0);
  if TA.FBufferValue is TGocciaSharedArrayBufferValue then
    Result := TGocciaTypedArrayValue.Create(TA.FKind, TGocciaSharedArrayBufferValue(TA.FBufferValue), TA.FByteOffset + BeginIdx * BPE, NewLen)
  else
    Result := TGocciaTypedArrayValue.Create(TA.FKind, TGocciaArrayBufferValue(TA.FBufferValue), TA.FByteOffset + BeginIdx * BPE, NewLen);
  TGocciaTypedArrayValue(Result).Prototype := TA.Prototype;
end;

// ES2026 §23.2.3.25 %TypedArray%.prototype.set(source [, offset])
function TGocciaTypedArrayValue.TypedArraySet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, SrcTA: TGocciaTypedArrayValue;
  TargetOffset, I: Integer;
  SrcArray: TGocciaArrayValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.set');
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorTypedArraySetRequiresArg, SSuggestTypedArraySetSource);

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    TargetOffset := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if TargetOffset < 0 then
      ThrowRangeError(SErrorTypedArraySetOffsetNonNegative, SSuggestTypedArrayLength);
  end
  else
    TargetOffset := 0;

  if AArgs.GetElement(0) is TGocciaTypedArrayValue then
  begin
    SrcTA := TGocciaTypedArrayValue(AArgs.GetElement(0));
    if TargetOffset + SrcTA.FLength > TA.FLength then
      ThrowRangeError(SErrorTypedArraySourceTooLarge, SSuggestTypedArrayLength);
    // ES2026 §23.2.3.25.1 step 1.b: mixed BigInt/Number content types throw
    if IsBigIntKind(TA.FKind) <> IsBigIntKind(SrcTA.FKind) then
      ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt, SSuggestBigIntTypedArrayValue);
    if IsBigIntKind(TA.FKind) then
    begin
      for I := 0 to SrcTA.FLength - 1 do
        TA.WriteBigIntElement(TargetOffset + I, SrcTA.ReadBigIntElement(I));
    end
    else
    begin
      for I := 0 to SrcTA.FLength - 1 do
        TA.WriteElement(TargetOffset + I, SrcTA.ReadElement(I));
    end;
  end
  else if AArgs.GetElement(0) is TGocciaArrayValue then
  begin
    SrcArray := TGocciaArrayValue(AArgs.GetElement(0));
    if TargetOffset + SrcArray.Elements.Count > TA.FLength then
      ThrowRangeError(SErrorTypedArraySourceTooLarge, SSuggestTypedArrayLength);
    for I := 0 to SrcArray.Elements.Count - 1 do
      TA.WriteValueToElement(TargetOffset + I, SrcArray.Elements[I]);
  end
  else
    ThrowTypeError(SErrorTypedArraySetArgType, SSuggestTypedArraySetSource);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.24 %TypedArray%.prototype.reverse()
function TGocciaTypedArrayValue.TypedArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I: Integer;
  Tmp: Double;
  TmpBig: Int64;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.reverse');
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 0 to (TA.FLength div 2) - 1 do
    begin
      TmpBig := TA.ReadBigIntElement(I);
      TA.WriteBigIntElement(I, TA.ReadBigIntElement(TA.FLength - 1 - I));
      TA.WriteBigIntElement(TA.FLength - 1 - I, TmpBig);
    end;
  end
  else
  begin
    for I := 0 to (TA.FLength div 2) - 1 do
    begin
      Tmp := TA.ReadElement(I);
      TA.WriteElement(I, TA.ReadElement(TA.FLength - 1 - I));
      TA.WriteElement(TA.FLength - 1 - I, Tmp);
    end;
  end;
  Result := AThisValue;
end;

// ES2026 §23.2.3.28 %TypedArray%.prototype.sort([comparefn])
function TGocciaTypedArrayValue.TypedArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, J, SortLen, NaNCount: Integer;
  Tmp, CompResult: Double;
  TmpBig: Int64;
  HasCompare: Boolean;
  CompareArgs: TGocciaArgumentsCollection;
  CompareResult: TGocciaValue;
  IsFloat: Boolean;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.sort');
  HasCompare := (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable;
  SortLen := TA.FLength;

  // BigInt sort path
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 1 to SortLen - 1 do
    begin
      TmpBig := TA.ReadBigIntElement(I);
      J := I - 1;
      while J >= 0 do
      begin
        if HasCompare then
        begin
          CompareArgs := TGocciaArgumentsCollection.Create;
          try
            if TA.FKind = takBigUint64 then
            begin
              CompareArgs.Add(TGocciaBigIntValue.Create(BigIntFromQWord(QWord(TA.ReadBigIntElement(J)))));
              CompareArgs.Add(TGocciaBigIntValue.Create(BigIntFromQWord(QWord(TmpBig))));
            end
            else
            begin
              CompareArgs.Add(TGocciaBigIntValue.Create(TBigInteger.FromInt64(TA.ReadBigIntElement(J))));
              CompareArgs.Add(TGocciaBigIntValue.Create(TBigInteger.FromInt64(TmpBig)));
            end;
            CompareResult := TGocciaFunctionBase(AArgs.GetElement(0)).Call(CompareArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
            CompResult := CompareResult.ToNumberLiteral.Value;
          finally
            CompareArgs.Free;
          end;
        end
        else
        begin
          if TA.FKind = takBigUint64 then
          begin
            // Unsigned comparison to avoid sign confusion for values >= 2^63
            if QWord(TA.ReadBigIntElement(J)) > QWord(TmpBig) then
              CompResult := 1
            else if QWord(TA.ReadBigIntElement(J)) < QWord(TmpBig) then
              CompResult := -1
            else
              CompResult := 0;
          end
          else
          begin
            if TA.ReadBigIntElement(J) > TmpBig then
              CompResult := 1
            else if TA.ReadBigIntElement(J) < TmpBig then
              CompResult := -1
            else
              CompResult := 0;
          end;
        end;

        if CompResult > 0 then
        begin
          TA.WriteBigIntElement(J + 1, TA.ReadBigIntElement(J));
          Dec(J);
        end
        else
          Break;
      end;
      TA.WriteBigIntElement(J + 1, TmpBig);
    end;
    Exit(AThisValue);
  end;

  // For float kinds, partition NaN values to the end before sorting
  IsFloat := IsFloatKind(TA.FKind);
  if IsFloat and not HasCompare then
  begin
    NaNCount := 0;
    J := 0;
    for I := 0 to SortLen - 1 do
    begin
      Tmp := TA.ReadElement(I);
      if Math.IsNaN(Tmp) then
        Inc(NaNCount)
      else
      begin
        if J <> I then
          TA.WriteElement(J, Tmp);
        Inc(J);
      end;
    end;
    SortLen := SortLen - NaNCount;
    for I := SortLen to SortLen + NaNCount - 1 do
      TA.WriteElement(I, Math.NaN);
  end;

  for I := 1 to SortLen - 1 do
  begin
    Tmp := TA.ReadElement(I);
    J := I - 1;
    while J >= 0 do
    begin
      if HasCompare then
      begin
        CompareArgs := TGocciaArgumentsCollection.Create;
        try
          CompareArgs.Add(TGocciaNumberLiteralValue.Create(TA.ReadElement(J)));
          CompareArgs.Add(TGocciaNumberLiteralValue.Create(Tmp));
          CompareResult := TGocciaFunctionBase(AArgs.GetElement(0)).Call(CompareArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
          CompResult := CompareResult.ToNumberLiteral.Value;
        finally
          CompareArgs.Free;
        end;
      end
      else
      begin
        if TA.ReadElement(J) > Tmp then
          CompResult := 1
        else if TA.ReadElement(J) < Tmp then
          CompResult := -1
        else
          CompResult := 0;
      end;

      if CompResult > 0 then
      begin
        TA.WriteElement(J + 1, TA.ReadElement(J));
        Dec(J);
      end
      else
        Break;
    end;
    TA.WriteElement(J + 1, Tmp);
  end;
  Result := AThisValue;
end;

// ES2026 §23.2.3.16 %TypedArray%.prototype.indexOf(searchElement [, fromIndex])
function TGocciaTypedArrayValue.TypedArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  SearchNum: TGocciaNumberLiteralValue;
  SearchVal: Double;
  SearchBigInt: Int64;
  I, StartIdx: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.indexOf');
  if AArgs.Length = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));

  if AArgs.Length > 1 then
  begin
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if StartIdx < 0 then StartIdx := Max(TA.FLength + StartIdx, 0);
  end
  else
    StartIdx := 0;

  if IsBigIntKind(TA.FKind) then
  begin
    if not (AArgs.GetElement(0) is TGocciaBigIntValue) then
      Exit(TGocciaNumberLiteralValue.Create(-1));
    SearchBigInt := TGocciaBigIntValue(AArgs.GetElement(0)).Value.ToInt64;
    for I := StartIdx to TA.FLength - 1 do
      if TA.ReadBigIntElement(I) = SearchBigInt then
        Exit(TGocciaNumberLiteralValue.Create(I));
    Exit(TGocciaNumberLiteralValue.Create(-1));
  end;

  SearchNum := AArgs.GetElement(0).ToNumberLiteral;
  // ES2026: indexOf uses strict equality — NaN !== NaN
  if SearchNum.IsNaN then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if SearchNum.IsInfinite then
  begin
    if not (IsFloatKind(TA.FKind)) then
      Exit(TGocciaNumberLiteralValue.Create(-1));
    for I := StartIdx to TA.FLength - 1 do
      if Math.IsInfinite(TA.ReadElement(I)) and
         ((SearchNum.IsInfinity and (TA.ReadElement(I) > 0)) or
          (SearchNum.IsNegativeInfinity and (TA.ReadElement(I) < 0))) then
        Exit(TGocciaNumberLiteralValue.Create(I));
    Exit(TGocciaNumberLiteralValue.Create(-1));
  end;
  SearchVal := SearchNum.Value;
  for I := StartIdx to TA.FLength - 1 do
    if TA.ReadElement(I) = SearchVal then
      Exit(TGocciaNumberLiteralValue.Create(I));
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.2.3.18 %TypedArray%.prototype.lastIndexOf(searchElement [, fromIndex])
function TGocciaTypedArrayValue.TypedArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  SearchNum: TGocciaNumberLiteralValue;
  SearchVal: Double;
  SearchBigInt: Int64;
  I, StartIdx: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.lastIndexOf');
  if AArgs.Length = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if AArgs.Length > 1 then
  begin
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if StartIdx < 0 then StartIdx := TA.FLength + StartIdx;
  end
  else
    StartIdx := TA.FLength - 1;

  if IsBigIntKind(TA.FKind) then
  begin
    if not (AArgs.GetElement(0) is TGocciaBigIntValue) then
      Exit(TGocciaNumberLiteralValue.Create(-1));
    SearchBigInt := TGocciaBigIntValue(AArgs.GetElement(0)).Value.ToInt64;
    for I := Min(StartIdx, TA.FLength - 1) downto 0 do
      if TA.ReadBigIntElement(I) = SearchBigInt then
        Exit(TGocciaNumberLiteralValue.Create(I));
    Exit(TGocciaNumberLiteralValue.Create(-1));
  end;

  SearchNum := AArgs.GetElement(0).ToNumberLiteral;
  if SearchNum.IsNaN then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if SearchNum.IsInfinite then
  begin
    if not (IsFloatKind(TA.FKind)) then
      Exit(TGocciaNumberLiteralValue.Create(-1));
    for I := Min(StartIdx, TA.FLength - 1) downto 0 do
      if Math.IsInfinite(TA.ReadElement(I)) and
         ((SearchNum.IsInfinity and (TA.ReadElement(I) > 0)) or
          (SearchNum.IsNegativeInfinity and (TA.ReadElement(I) < 0))) then
        Exit(TGocciaNumberLiteralValue.Create(I));
    Exit(TGocciaNumberLiteralValue.Create(-1));
  end;
  SearchVal := SearchNum.Value;
  for I := Min(StartIdx, TA.FLength - 1) downto 0 do
    if TA.ReadElement(I) = SearchVal then
      Exit(TGocciaNumberLiteralValue.Create(I));
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.2.3.15 %TypedArray%.prototype.includes(searchElement [, fromIndex])
function TGocciaTypedArrayValue.TypedArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  SearchNum: TGocciaNumberLiteralValue;
  SearchVal: Double;
  SearchBigInt: Int64;
  I, StartIdx: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.includes');
  if AArgs.Length = 0 then
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  if AArgs.Length > 1 then
  begin
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if StartIdx < 0 then StartIdx := Max(TA.FLength + StartIdx, 0);
  end
  else
    StartIdx := 0;

  if IsBigIntKind(TA.FKind) then
  begin
    if not (AArgs.GetElement(0) is TGocciaBigIntValue) then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    SearchBigInt := TGocciaBigIntValue(AArgs.GetElement(0)).Value.ToInt64;
    for I := StartIdx to TA.FLength - 1 do
      if TA.ReadBigIntElement(I) = SearchBigInt then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  SearchNum := AArgs.GetElement(0).ToNumberLiteral;
  // SameValueZero: NaN === NaN for includes
  if SearchNum.IsNaN then
  begin
    if IsFloatKind(TA.FKind) then
      for I := StartIdx to TA.FLength - 1 do
        if Math.IsNaN(TA.ReadElement(I)) then
          Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;
  if SearchNum.IsInfinite then
  begin
    if not (IsFloatKind(TA.FKind)) then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    for I := StartIdx to TA.FLength - 1 do
      if Math.IsInfinite(TA.ReadElement(I)) and
         ((SearchNum.IsInfinity and (TA.ReadElement(I) > 0)) or
          (SearchNum.IsNegativeInfinity and (TA.ReadElement(I) < 0))) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;
  SearchVal := SearchNum.Value;
  for I := StartIdx to TA.FLength - 1 do
    if TA.ReadElement(I) = SearchVal then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.2.3.11 %TypedArray%.prototype.find(predicate [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.find');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
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
  I: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.findIndex');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindIndexCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
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
  I: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.findLast');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindLastCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := TA.FLength - 1 downto 0 do
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
  I: Integer;
  Element, CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.findLastIndex');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFindLastIndexCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := TA.FLength - 1 downto 0 do
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
  I: Integer;
  CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.every');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayEveryCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
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
  I: Integer;
  CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.some');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArraySomeCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
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
  I: Integer;
  ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.forEach');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayForEachCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
    InvokeCallback(AArgs.GetElement(0),
      TA.GetElementAsValue(I),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.20 %TypedArray%.prototype.map(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I: Integer;
  CallResult, ThisArg: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.map');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayMapCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  NewTA := CreateSameKindArray(TA, TA.FLength);
  for I := 0 to TA.FLength - 1 do
  begin
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TA.GetElementAsValue(I),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    NewTA.WriteValueToElement(I, CallResult);
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.9 %TypedArray%.prototype.filter(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, Count: Integer;
  ElemVal: Double;
  ElemBigVal: Int64;
  CallResult, ThisArg, Element: TGocciaValue;
  Kept: array of Double;
  KeptBig: array of Int64;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.filter');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayFilterCallable, SSuggestTypedArrayCallable);
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if IsBigIntKind(TA.FKind) then
  begin
    SetLength(KeptBig, 0);
    for I := 0 to TA.FLength - 1 do
    begin
      ElemBigVal := TA.ReadBigIntElement(I);
      Element := TA.GetElementAsValue(I);
      CallResult := InvokeCallback(AArgs.GetElement(0),
        Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Count := System.Length(KeptBig);
        SetLength(KeptBig, Count + 1);
        KeptBig[Count] := ElemBigVal;
      end;
    end;
    NewTA := CreateSameKindArray(TA, System.Length(KeptBig));
    for I := 0 to System.Length(KeptBig) - 1 do
      NewTA.WriteBigIntElement(I, KeptBig[I]);
  end
  else
  begin
    SetLength(Kept, 0);
    for I := 0 to TA.FLength - 1 do
    begin
      ElemVal := TA.ReadElement(I);
      CallResult := InvokeCallback(AArgs.GetElement(0),
        TGocciaNumberLiteralValue.Create(ElemVal),
        TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Count := System.Length(Kept);
        SetLength(Kept, Count + 1);
        Kept[Count] := ElemVal;
      end;
    end;
    NewTA := CreateSameKindArray(TA, System.Length(Kept));
    for I := 0 to System.Length(Kept) - 1 do
      NewTA.WriteElement(I, Kept[I]);
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.22 %TypedArray%.prototype.reduce(callbackfn [, initialValue])
function TGocciaTypedArrayValue.TypedArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I: Integer;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.reduce');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayReduceCallable, SSuggestTypedArrayCallable);

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    I := 0;
  end
  else
  begin
    if TA.FLength = 0 then
      ThrowTypeError(SErrorReduceEmptyTypedArray, SSuggestReduceInitialValue);
    Accumulator := TA.GetElementAsValue(0);
    I := 1;
  end;

  while I < TA.FLength do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(TA.GetElementAsValue(I));
      CallArgs.Add(TGocciaNumberLiteralValue.Create(I));
      CallArgs.Add(AThisValue);
      Accumulator := TGocciaFunctionBase(AArgs.GetElement(0)).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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
  I: Integer;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.reduceRight');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(SErrorTypedArrayReduceRightCallable, SSuggestTypedArrayCallable);

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    I := TA.FLength - 1;
  end
  else
  begin
    if TA.FLength = 0 then
      ThrowTypeError(SErrorReduceEmptyTypedArray, SSuggestReduceInitialValue);
    Accumulator := TA.GetElementAsValue(TA.FLength - 1);
    I := TA.FLength - 2;
  end;

  while I >= 0 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(TA.GetElementAsValue(I));
      CallArgs.Add(TGocciaNumberLiteralValue.Create(I));
      CallArgs.Add(AThisValue);
      Accumulator := TGocciaFunctionBase(AArgs.GetElement(0)).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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
  I: Integer;
  S: string;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.join');
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Sep := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Sep := ',';
  S := '';
  for I := 0 to TA.FLength - 1 do
  begin
    if I > 0 then S := S + Sep;
    S := S + TA.GetElementAsValue(I).ToStringLiteral.Value;
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
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.toReversed');
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
  I: Integer;
  SortArgs: TGocciaArgumentsCollection;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.toSorted');
  NewTA := CreateSameKindArray(TA, TA.FLength);
  if IsBigIntKind(TA.FKind) then
  begin
    for I := 0 to TA.FLength - 1 do
      NewTA.WriteBigIntElement(I, TA.ReadBigIntElement(I));
  end
  else
  begin
    for I := 0 to TA.FLength - 1 do
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
end;

// ES2026 §23.2.3.36 %TypedArray%.prototype.with(index, value)
function TGocciaTypedArrayValue.TypedArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, ActualIndex: Integer;
  NewNum: TGocciaNumberLiteralValue;
  NewBigInt: Int64;
  NewVal: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.with');
  // ES2026 §23.2.3.36 step 3: Let relativeIndex be ? ToIntegerOrInfinity(index)
  if AArgs.Length > 0 then
    ActualIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    ActualIndex := 0;
  if ActualIndex < 0 then
    ActualIndex := TA.FLength + ActualIndex;
  if (ActualIndex < 0) or (ActualIndex >= TA.FLength) then
    ThrowRangeError(SErrorInvalidTypedArrayIndex, SSuggestTypedArrayLength);

  if IsBigIntKind(TA.FKind) then
  begin
    if AArgs.Length > 1 then
      NewVal := AArgs.GetElement(1)
    else
      NewVal := TGocciaUndefinedLiteralValue.UndefinedValue;
    if not (NewVal is TGocciaBigIntValue) then
      ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt, SSuggestBigIntTypedArrayValue);
    NewBigInt := TGocciaBigIntValue(NewVal).Value.ToInt64;
    NewTA := CreateSameKindArray(TA, TA.FLength);
    for I := 0 to TA.FLength - 1 do
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
    NewTA := CreateSameKindArray(TA, TA.FLength);
    for I := 0 to TA.FLength - 1 do
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
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.values');
  Arr := TGocciaArrayValue.Create;
  for I := 0 to TA.FLength - 1 do
    Arr.Elements.Add(TA.GetElementAsValue(I));
  Result := TGocciaArrayIteratorValue.Create(Arr, akValues);
end;

// ES2026 §23.2.3.18 %TypedArray%.prototype.keys()
function TGocciaTypedArrayValue.TypedArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.keys');
  Arr := TGocciaArrayValue.Create;
  for I := 0 to TA.FLength - 1 do
    Arr.Elements.Add(TGocciaNumberLiteralValue.Create(I));
  Result := TGocciaArrayIteratorValue.Create(Arr, akValues);
end;

// ES2026 §23.2.3.6 %TypedArray%.prototype.entries()
function TGocciaTypedArrayValue.TypedArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Arr: TGocciaArrayValue;
  I: Integer;
  Entry: TGocciaArrayValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.entries');
  Arr := TGocciaArrayValue.Create;
  for I := 0 to TA.FLength - 1 do
  begin
    Entry := TGocciaArrayValue.Create;
    Entry.Elements.Add(TGocciaNumberLiteralValue.Create(I));
    Entry.Elements.Add(TA.GetElementAsValue(I));
    Arr.Elements.Add(Entry);
  end;
  Result := TGocciaArrayIteratorValue.Create(Arr, akValues);
end;

{ TGocciaTypedArrayClassValue }

constructor TGocciaTypedArrayClassValue.Create(const AName: string; const ASuperClass: TGocciaClassValue; const AKind: TGocciaTypedArrayKind);
begin
  inherited Create(AName, ASuperClass);
  FKind := AKind;
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
begin
  BPE := TGocciaTypedArrayValue.BytesPerElement(FKind);

  if (AArguments = nil) or (AArguments.Length = 0) then
    Exit(TGocciaTypedArrayValue.Create(FKind, 0));

  FirstArg := AArguments.GetElement(0);

  // new TypedArray(typedArray)
  if FirstArg is TGocciaTypedArrayValue then
  begin
    SrcTA := TGocciaTypedArrayValue(FirstArg);
    NewTA := TGocciaTypedArrayValue.Create(FKind, SrcTA.Length);
    if TGocciaTypedArrayValue.IsBigIntKind(FKind) and TGocciaTypedArrayValue.IsBigIntKind(SrcTA.Kind) then
    begin
      for I := 0 to SrcTA.Length - 1 do
        NewTA.WriteBigIntElement(I, SrcTA.ReadBigIntElement(I));
    end
    else if TGocciaTypedArrayValue.IsBigIntKind(FKind) then
    begin
      for I := 0 to SrcTA.Length - 1 do
        NewTA.WriteBigIntElement(I, Trunc(SrcTA.ReadElement(I)));
    end
    else if TGocciaTypedArrayValue.IsBigIntKind(SrcTA.Kind) then
    begin
      for I := 0 to SrcTA.Length - 1 do
        NewTA.WriteElement(I, SrcTA.ReadBigIntElement(I));
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
      ByteOff := Trunc(AArguments.GetElement(1).ToNumberLiteral.Value)
    else
      ByteOff := 0;

    if ByteOff < 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetNonNegative, [TGocciaTypedArrayValue.KindName(FKind)]), SSuggestTypedArrayLength);
    if (ByteOff mod BPE) <> 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);
    if ByteOff > System.Length(Buf.Data) then
      ThrowRangeError(SErrorTypedArrayStartOffsetBounds, SSuggestTypedArrayLength);

    if AArguments.Length > 2 then
    begin
      ElemLen := Trunc(AArguments.GetElement(2).ToNumberLiteral.Value);
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
      if Int64(ByteOff) + Int64(ElemLen) * Int64(BPE) > Int64(System.Length(Buf.Data)) then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end
    else
    begin
      if ((Int64(System.Length(Buf.Data)) - Int64(ByteOff)) mod Int64(BPE)) <> 0 then
        ThrowRangeError(Format(SErrorTypedArrayByteLengthMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);
      ElemLen := Integer((Int64(System.Length(Buf.Data)) - Int64(ByteOff)) div Int64(BPE));
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end;

    Exit(TGocciaTypedArrayValue.Create(FKind, Buf, ByteOff, ElemLen));
  end;

  // new TypedArray(sharedBuffer [, byteOffset [, length]])
  if FirstArg is TGocciaSharedArrayBufferValue then
  begin
    SAB := TGocciaSharedArrayBufferValue(FirstArg);
    if AArguments.Length > 1 then
      ByteOff := Trunc(AArguments.GetElement(1).ToNumberLiteral.Value)
    else
      ByteOff := 0;

    if ByteOff < 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetNonNegative, [TGocciaTypedArrayValue.KindName(FKind)]), SSuggestTypedArrayLength);
    if (ByteOff mod BPE) <> 0 then
      ThrowRangeError(Format(SErrorTypedArrayStartOffsetMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);
    if ByteOff > System.Length(SAB.Data) then
      ThrowRangeError(SErrorTypedArrayStartOffsetBounds, SSuggestTypedArrayLength);

    if AArguments.Length > 2 then
    begin
      ElemLen := Trunc(AArguments.GetElement(2).ToNumberLiteral.Value);
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
      if Int64(ByteOff) + Int64(ElemLen) * Int64(BPE) > Int64(System.Length(SAB.Data)) then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end
    else
    begin
      if ((Int64(System.Length(SAB.Data)) - Int64(ByteOff)) mod Int64(BPE)) <> 0 then
        ThrowRangeError(Format(SErrorTypedArrayByteLengthMultiple, [TGocciaTypedArrayValue.KindName(FKind), BPE]), SSuggestTypedArrayAlignment);
      ElemLen := Integer((Int64(System.Length(SAB.Data)) - Int64(ByteOff)) div Int64(BPE));
      if ElemLen < 0 then
        ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
    end;

    Exit(TGocciaTypedArrayValue.Create(FKind, SAB, ByteOff, ElemLen));
  end;

  // new TypedArray(array)
  if FirstArg is TGocciaArrayValue then
  begin
    SrcArr := TGocciaArrayValue(FirstArg);
    NewTA := TGocciaTypedArrayValue.Create(FKind, SrcArr.Elements.Count);
    for I := 0 to SrcArr.Elements.Count - 1 do
      NewTA.WriteValueToElement(I, SrcArr.Elements[I]);
    Exit(NewTA);
  end;

  // new TypedArray(length) — ES2026 §23.2.1.2 step 5: ToIndex(length)
  // ToIndex calls ToNumber, which throws TypeError for BigInt (§7.1.4)
  Num := FirstArg.ToNumberLiteral;
  if Num.IsNaN then
    Len := 0
  else
  begin
    Len := Trunc(Num.Value);
    if Len < 0 then
      ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
  end;
  Result := TGocciaTypedArrayValue.Create(FKind, Len);
end;

{ TGocciaTypedArrayStaticFrom }

constructor TGocciaTypedArrayStaticFrom.Create(const AKind: TGocciaTypedArrayKind);
begin
  inherited Create;
  FKind := AKind;
end;

// ES2026 §23.2.2.1 %TypedArray%.from(source [, mapfn [, thisArg]])
function TGocciaTypedArrayStaticFrom.TypedArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Source: TGocciaValue;
  SrcArr: TGocciaArrayValue;
  SrcTA: TGocciaTypedArrayValue;
  NewTA: TGocciaTypedArrayValue;
  HasMapFn: Boolean;
  MapFn: TGocciaFunctionBase;
  MapFnArg: TGocciaValue;
  ThisArg: TGocciaValue;
  I: Integer;
  Val: TGocciaValue;
  MapArgs: TGocciaArgumentsCollection;
begin
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorTypedArrayFromRequiresArg, SSuggestTypedArraySetSource);
  Source := AArgs.GetElement(0);
  HasMapFn := False;
  MapFn := nil;
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    MapFnArg := AArgs.GetElement(1);
    if not MapFnArg.IsCallable then
      ThrowTypeError(SErrorTypedArrayFromMapFn, SSuggestTypedArrayCallable);
    HasMapFn := True;
    MapFn := TGocciaFunctionBase(MapFnArg);
  end;
  if AArgs.Length > 2 then
    ThisArg := AArgs.GetElement(2)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if Source is TGocciaTypedArrayValue then
  begin
    SrcTA := TGocciaTypedArrayValue(Source);
    NewTA := TGocciaTypedArrayValue.Create(FKind, SrcTA.Length);
    for I := 0 to SrcTA.Length - 1 do
    begin
      Val := SrcTA.GetElementAsValue(I);
      if HasMapFn then
      begin
        MapArgs := TGocciaArgumentsCollection.Create;
        try
          MapArgs.Add(Val);
          MapArgs.Add(TGocciaNumberLiteralValue.Create(I));
          Val := MapFn.Call(MapArgs, ThisArg);
        finally
          MapArgs.Free;
        end;
      end;
      NewTA.WriteValueToElement(I, Val);
    end;
    Exit(NewTA);
  end;

  if Source is TGocciaArrayValue then
  begin
    SrcArr := TGocciaArrayValue(Source);
    NewTA := TGocciaTypedArrayValue.Create(FKind, SrcArr.Elements.Count);
    for I := 0 to SrcArr.Elements.Count - 1 do
    begin
      Val := SrcArr.Elements[I];
      if HasMapFn then
      begin
        MapArgs := TGocciaArgumentsCollection.Create;
        try
          MapArgs.Add(Val);
          MapArgs.Add(TGocciaNumberLiteralValue.Create(I));
          Val := MapFn.Call(MapArgs, ThisArg);
        finally
          MapArgs.Free;
        end;
      end;
      NewTA.WriteValueToElement(I, Val);
    end;
    Exit(NewTA);
  end;

  ThrowTypeError(SErrorTypedArrayFromSource, SSuggestTypedArraySetSource);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.2.2 %TypedArray%.of(...items)
function TGocciaTypedArrayStaticFrom.TypedArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NewTA: TGocciaTypedArrayValue;
  I: Integer;
begin
  NewTA := TGocciaTypedArrayValue.Create(FKind, AArgs.Length);
  for I := 0 to AArgs.Length - 1 do
    NewTA.WriteValueToElement(I, AArgs.GetElement(I));
  Result := NewTA;
end;

end.
