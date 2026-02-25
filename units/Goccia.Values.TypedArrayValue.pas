unit Goccia.Values.TypedArrayValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTypedArrayKind = (
    takInt8, takUint8, takUint8Clamped,
    takInt16, takUint16,
    takInt32, takUint32,
    takFloat32, takFloat64
  );

  TGocciaTypedArrayValue = class(TGocciaInstanceValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FBuffer: TGocciaArrayBufferValue;
    FByteOffset: Integer;
    FLength: Integer;
    FKind: TGocciaTypedArrayKind;

    procedure InitializePrototype;

    function ReadElement(const AIndex: Integer): Double;
    procedure WriteElement(const AIndex: Integer; const AValue: Double);
    procedure WriteNumberLiteral(const AIndex: Integer; const ANum: TGocciaNumberLiteralValue);

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
  public
    constructor Create(const AKind: TGocciaTypedArrayKind; const ALength: Integer); overload;
    constructor Create(const AKind: TGocciaTypedArrayKind; const ABuffer: TGocciaArrayBufferValue;
      const AByteOffset: Integer = 0; const ALength: Integer = -1); overload;

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    class function BytesPerElement(const AKind: TGocciaTypedArrayKind): Integer;
    class function KindName(const AKind: TGocciaTypedArrayKind): string;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class procedure SetSharedPrototypeParent(const AParent: TGocciaObjectValue);

    property Buffer: TGocciaArrayBufferValue read FBuffer;
    property ByteOffset: Integer read FByteOffset;
    property Length: Integer read FLength;
    property Kind: TGocciaTypedArrayKind read FKind;
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
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue;

class function TGocciaTypedArrayValue.BytesPerElement(const AKind: TGocciaTypedArrayKind): Integer;
begin
  case AKind of
    takInt8, takUint8, takUint8Clamped: Result := 1;
    takInt16, takUint16: Result := 2;
    takInt32, takUint32, takFloat32: Result := 4;
    takFloat64: Result := 8;
  else
    Result := 1;
  end;
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
    takFloat32: Result := CONSTRUCTOR_FLOAT32_ARRAY;
    takFloat64: Result := CONSTRUCTOR_FLOAT64_ARRAY;
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
  F32: Single;
  F64: Double;
begin
  Offset := FByteOffset + AIndex * BytesPerElement(FKind);
  case FKind of
    takInt8:
    begin
      I8 := ShortInt(FBuffer.Data[Offset]);
      Result := I8;
    end;
    takUint8, takUint8Clamped:
    begin
      U8 := FBuffer.Data[Offset];
      Result := U8;
    end;
    takInt16:
    begin
      Move(FBuffer.Data[Offset], I16, 2);
      Result := I16;
    end;
    takUint16:
    begin
      Move(FBuffer.Data[Offset], U16, 2);
      Result := U16;
    end;
    takInt32:
    begin
      Move(FBuffer.Data[Offset], I32, 4);
      Result := I32;
    end;
    takUint32:
    begin
      Move(FBuffer.Data[Offset], U32, 4);
      Result := U32;
    end;
    takFloat32:
    begin
      Move(FBuffer.Data[Offset], F32, 4);
      Result := F32;
    end;
    takFloat64:
    begin
      Move(FBuffer.Data[Offset], F64, 8);
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
      FBuffer.Data[Offset] := Byte(I8);
    end;
    takUint8:
    begin
      U8 := Byte(Trunc(AValue));
      FBuffer.Data[Offset] := U8;
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
      FBuffer.Data[Offset] := Byte(Clamped);
    end;
    takInt16:
    begin
      I16 := SmallInt(Trunc(AValue));
      Move(I16, FBuffer.Data[Offset], 2);
    end;
    takUint16:
    begin
      U16 := Word(Trunc(AValue));
      Move(U16, FBuffer.Data[Offset], 2);
    end;
    takInt32:
    begin
      I32 := LongInt(Trunc(AValue));
      Move(I32, FBuffer.Data[Offset], 4);
    end;
    takUint32:
    begin
      U32 := LongWord(Trunc(AValue));
      Move(U32, FBuffer.Data[Offset], 4);
    end;
    takFloat32:
    begin
      F32 := AValue;
      Move(F32, FBuffer.Data[Offset], 4);
    end;
    takFloat64:
    begin
      F64 := AValue;
      Move(F64, FBuffer.Data[Offset], 8);
    end;
  end;
end;

procedure WriteFloatSpecial(const ABuffer: TGocciaArrayBufferValue;
  const AOffset: Integer; const AKind: TGocciaTypedArrayKind;
  const ASpecial: TGocciaNumberSpecialValue);
const
  NAN_F32: LongWord = $7FC00000;
  INF_F32: LongWord = $7F800000;
  NINF_F32: LongWord = $FF800000;
  NAN_F64: Int64 = Int64($7FF8000000000000);
  INF_F64: Int64 = Int64($7FF0000000000000);
  NINF_F64: Int64 = Int64($FFF0000000000000);
begin
  case AKind of
    takFloat32:
      case ASpecial of
        nsvNaN: Move(NAN_F32, ABuffer.Data[AOffset], 4);
        nsvInfinity: Move(INF_F32, ABuffer.Data[AOffset], 4);
        nsvNegativeInfinity: Move(NINF_F32, ABuffer.Data[AOffset], 4);
      end;
    takFloat64:
      case ASpecial of
        nsvNaN: Move(NAN_F64, ABuffer.Data[AOffset], 8);
        nsvInfinity: Move(INF_F64, ABuffer.Data[AOffset], 8);
        nsvNegativeInfinity: Move(NINF_F64, ABuffer.Data[AOffset], 8);
      end;
  end;
end;

procedure TGocciaTypedArrayValue.WriteNumberLiteral(const AIndex: Integer; const ANum: TGocciaNumberLiteralValue);
var
  Offset: Integer;
begin
  if ANum.IsNaN then
  begin
    if FKind in [takFloat32, takFloat64] then
    begin
      Offset := FByteOffset + AIndex * BytesPerElement(FKind);
      WriteFloatSpecial(FBuffer, Offset, FKind, nsvNaN);
    end
    else
      WriteElement(AIndex, 0);
  end
  else if ANum.IsInfinity then
  begin
    case FKind of
      takUint8Clamped: WriteElement(AIndex, 255);
      takFloat32, takFloat64:
      begin
        Offset := FByteOffset + AIndex * BytesPerElement(FKind);
        WriteFloatSpecial(FBuffer, Offset, FKind, nsvInfinity);
      end;
    else
      WriteElement(AIndex, 0);
    end;
  end
  else if ANum.IsNegativeInfinity then
  begin
    case FKind of
      takFloat32, takFloat64:
      begin
        Offset := FByteOffset + AIndex * BytesPerElement(FKind);
        WriteFloatSpecial(FBuffer, Offset, FKind, nsvNegativeInfinity);
      end;
    else
      WriteElement(AIndex, 0);
    end;
  end
  else
    WriteElement(AIndex, ANum.Value);
end;

{ Constructors }

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind; const ALength: Integer);
var
  ByteLen: Integer;
begin
  inherited Create(nil);
  FKind := AKind;
  FByteOffset := 0;
  FLength := ALength;
  ByteLen := ALength * BytesPerElement(AKind);
  FBuffer := TGocciaArrayBufferValue.Create(ByteLen);
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

constructor TGocciaTypedArrayValue.Create(const AKind: TGocciaTypedArrayKind;
  const ABuffer: TGocciaArrayBufferValue; const AByteOffset: Integer; const ALength: Integer);
var
  BPE: Integer;
begin
  inherited Create(nil);
  FKind := AKind;
  FBuffer := ABuffer;
  FByteOffset := AByteOffset;
  BPE := BytesPerElement(AKind);

  if ALength >= 0 then
    FLength := ALength
  else
    FLength := (System.Length(ABuffer.Data) - AByteOffset) div BPE;

  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

{ Prototype initialization }

procedure TGocciaTypedArrayValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayAt, 'at', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayFill, 'fill', 3));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayCopyWithin, 'copyWithin', 3));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArraySlice, 'slice', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArraySubarray, 'subarray', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArraySet, 'set', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayReverse, 'reverse', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArraySort, 'sort', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayIndexOf, 'indexOf', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayLastIndexOf, 'lastIndexOf', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayIncludes, 'includes', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayFind, 'find', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayFindIndex, 'findIndex', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayFindLast, 'findLast', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayFindLastIndex, 'findLastIndex', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayEvery, 'every', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArraySome, 'some', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayForEach, 'forEach', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayMap, 'map', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayFilter, 'filter', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayReduce, 'reduce', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayReduceRight, 'reduceRight', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayJoin, 'join', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayToString, PROP_TO_STRING, 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayToReversed, 'toReversed', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayToSorted, 'toSorted', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayWith, 'with', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayValues, 'values', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayKeys, 'keys', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayEntries, 'entries', 0));

  FShared.Prototype.DefineProperty(PROP_BUFFER,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayBufferGetter, 'get buffer', 0),
      nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty(PROP_BYTE_LENGTH,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayByteLengthGetter, 'get byteLength', 0),
      nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty(PROP_BYTE_OFFSET,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayByteOffsetGetter, 'get byteOffset', 0),
      nil, [pfConfigurable]));
  FShared.Prototype.DefineProperty(PROP_LENGTH,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayLengthGetter, 'get length', 0),
      nil, [pfConfigurable]));

  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayValues, 'values', 0),
      [pfConfigurable, pfWritable]));
  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(TypedArrayToStringTagGetter, 'get [Symbol.toStringTag]', 0),
      nil, [pfConfigurable]));
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

{ Property access — indexed elements }

function TGocciaTypedArrayValue.GetProperty(const AName: string): TGocciaValue;
var
  Index: Integer;
begin
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < FLength) then
      Result := TGocciaNumberLiteralValue.Create(ReadElement(Index))
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
    Exit(FBuffer);
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
      WriteNumberLiteral(Index, AValue.ToNumberLiteral);
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
  if Assigned(FBuffer) and not FBuffer.GCMarked then
    FBuffer.MarkReferences;
end;

{ Helpers }

function RequireTypedArray(const AThisValue: TGocciaValue; const AMethod: string): TGocciaTypedArrayValue;
begin
  if not (AThisValue is TGocciaTypedArrayValue) then
    ThrowTypeError(AMethod + ' requires a TypedArray');
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
  Result := RequireTypedArray(AThisValue, 'TypedArray.prototype.buffer').FBuffer;
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
  Result := TGocciaNumberLiteralValue.Create(TA.ReadElement(ActualIndex));
end;

// ES2026 §23.2.3.8 %TypedArray%.prototype.fill(value [, start [, end]])
function TGocciaTypedArrayValue.TypedArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  FillNum: TGocciaNumberLiteralValue;
  First, Final, I, RelStart, RelEnd: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.fill');
  if AArgs.Length = 0 then
    FillNum := TGocciaNumberLiteralValue.ZeroValue
  else
    FillNum := AArgs.GetElement(0).ToNumberLiteral;

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

  for I := First to Final - 1 do
    TA.WriteNumberLiteral(I, FillNum);
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
  Move(TA.FBuffer.Data[From], TempBuf[0], Count * BPE);
  Move(TempBuf[0], TA.FBuffer.Data[To_], Count * BPE);

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
  for I := 0 to NewLen - 1 do
    NewTA.WriteElement(I, TA.ReadElement(First + I));
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
  Result := TGocciaTypedArrayValue.Create(TA.FKind, TA.FBuffer, TA.FByteOffset + BeginIdx * BPE, NewLen);
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
    ThrowTypeError('TypedArray.prototype.set requires at least one argument');

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    TargetOffset := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if TargetOffset < 0 then
      ThrowRangeError('TypedArray.prototype.set offset must be >= 0');
  end
  else
    TargetOffset := 0;

  if AArgs.GetElement(0) is TGocciaTypedArrayValue then
  begin
    SrcTA := TGocciaTypedArrayValue(AArgs.GetElement(0));
    if TargetOffset + SrcTA.FLength > TA.FLength then
      ThrowRangeError('Source is too large');
    for I := 0 to SrcTA.FLength - 1 do
      TA.WriteElement(TargetOffset + I, SrcTA.ReadElement(I));
  end
  else if AArgs.GetElement(0) is TGocciaArrayValue then
  begin
    SrcArray := TGocciaArrayValue(AArgs.GetElement(0));
    if TargetOffset + SrcArray.Elements.Count > TA.FLength then
      ThrowRangeError('Source is too large');
    for I := 0 to SrcArray.Elements.Count - 1 do
      TA.WriteNumberLiteral(TargetOffset + I, SrcArray.Elements[I].ToNumberLiteral);
  end
  else
    ThrowTypeError('TypedArray.prototype.set argument must be an array or typed array');

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.2.3.24 %TypedArray%.prototype.reverse()
function TGocciaTypedArrayValue.TypedArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I: Integer;
  Tmp: Double;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.reverse');
  for I := 0 to (TA.FLength div 2) - 1 do
  begin
    Tmp := TA.ReadElement(I);
    TA.WriteElement(I, TA.ReadElement(TA.FLength - 1 - I));
    TA.WriteElement(TA.FLength - 1 - I, Tmp);
  end;
  Result := AThisValue;
end;

// ES2026 §23.2.3.28 %TypedArray%.prototype.sort([comparefn])
function TGocciaTypedArrayValue.TypedArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, J: Integer;
  Tmp, CompResult: Double;
  HasCompare: Boolean;
  CompareArgs: TGocciaArgumentsCollection;
  CompareResult: TGocciaValue;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.sort');
  HasCompare := (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable;

  for I := 1 to TA.FLength - 1 do
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
  I, StartIdx: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.indexOf');
  if AArgs.Length = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  SearchNum := AArgs.GetElement(0).ToNumberLiteral;
  // ES2026: indexOf uses strict equality — NaN !== NaN
  if SearchNum.IsNaN then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if AArgs.Length > 1 then
  begin
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if StartIdx < 0 then StartIdx := Max(TA.FLength + StartIdx, 0);
  end
  else
    StartIdx := 0;
  if SearchNum.IsInfinite then
  begin
    if not (TA.FKind in [takFloat32, takFloat64]) then
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
  I, StartIdx: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.lastIndexOf');
  if AArgs.Length = 0 then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  SearchNum := AArgs.GetElement(0).ToNumberLiteral;
  if SearchNum.IsNaN then
    Exit(TGocciaNumberLiteralValue.Create(-1));
  if AArgs.Length > 1 then
  begin
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if StartIdx < 0 then StartIdx := TA.FLength + StartIdx;
  end
  else
    StartIdx := TA.FLength - 1;
  if SearchNum.IsInfinite then
  begin
    if not (TA.FKind in [takFloat32, takFloat64]) then
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
  I, StartIdx: Integer;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.includes');
  if AArgs.Length = 0 then
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  SearchNum := AArgs.GetElement(0).ToNumberLiteral;
  if AArgs.Length > 1 then
  begin
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if StartIdx < 0 then StartIdx := Max(TA.FLength + StartIdx, 0);
  end
  else
    StartIdx := 0;
  // SameValueZero: NaN === NaN for includes
  if SearchNum.IsNaN then
  begin
    if TA.FKind in [takFloat32, takFloat64] then
      for I := StartIdx to TA.FLength - 1 do
        if Math.IsNaN(TA.ReadElement(I)) then
          Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;
  if SearchNum.IsInfinite then
  begin
    if not (TA.FKind in [takFloat32, takFloat64]) then
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
    ThrowTypeError('TypedArray.prototype.find requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
  begin
    Element := TGocciaNumberLiteralValue.Create(TA.ReadElement(I));
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value <> 0 then
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
    ThrowTypeError('TypedArray.prototype.findIndex requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
  begin
    Element := TGocciaNumberLiteralValue.Create(TA.ReadElement(I));
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value <> 0 then
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
    ThrowTypeError('TypedArray.prototype.findLast requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := TA.FLength - 1 downto 0 do
  begin
    Element := TGocciaNumberLiteralValue.Create(TA.ReadElement(I));
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value <> 0 then
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
    ThrowTypeError('TypedArray.prototype.findLastIndex requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := TA.FLength - 1 downto 0 do
  begin
    Element := TGocciaNumberLiteralValue.Create(TA.ReadElement(I));
    CallResult := InvokeCallback(AArgs.GetElement(0), Element, TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value <> 0 then
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
    ThrowTypeError('TypedArray.prototype.every requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
  begin
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TGocciaNumberLiteralValue.Create(TA.ReadElement(I)),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value = 0 then
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
    ThrowTypeError('TypedArray.prototype.some requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
  begin
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TGocciaNumberLiteralValue.Create(TA.ReadElement(I)),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value <> 0 then
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
    ThrowTypeError('TypedArray.prototype.forEach requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to TA.FLength - 1 do
    InvokeCallback(AArgs.GetElement(0),
      TGocciaNumberLiteralValue.Create(TA.ReadElement(I)),
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
    ThrowTypeError('TypedArray.prototype.map requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  NewTA := CreateSameKindArray(TA, TA.FLength);
  for I := 0 to TA.FLength - 1 do
  begin
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TGocciaNumberLiteralValue.Create(TA.ReadElement(I)),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    NewTA.WriteNumberLiteral(I, CallResult.ToNumberLiteral);
  end;
  Result := NewTA;
end;

// ES2026 §23.2.3.9 %TypedArray%.prototype.filter(callbackfn [, thisArg])
function TGocciaTypedArrayValue.TypedArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TA, NewTA: TGocciaTypedArrayValue;
  I, Count: Integer;
  ElemVal: Double;
  CallResult, ThisArg: TGocciaValue;
  Kept: array of Double;
begin
  TA := RequireTypedArray(AThisValue, 'TypedArray.prototype.filter');
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError('TypedArray.prototype.filter requires a callable argument');
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  SetLength(Kept, 0);
  for I := 0 to TA.FLength - 1 do
  begin
    ElemVal := TA.ReadElement(I);
    CallResult := InvokeCallback(AArgs.GetElement(0),
      TGocciaNumberLiteralValue.Create(ElemVal),
      TGocciaNumberLiteralValue.Create(I), AThisValue, ThisArg);
    if CallResult.ToNumberLiteral.Value <> 0 then
    begin
      Count := System.Length(Kept);
      SetLength(Kept, Count + 1);
      Kept[Count] := ElemVal;
    end;
  end;

  NewTA := CreateSameKindArray(TA, System.Length(Kept));
  for I := 0 to System.Length(Kept) - 1 do
    NewTA.WriteElement(I, Kept[I]);
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
    ThrowTypeError('TypedArray.prototype.reduce requires a callable argument');

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    I := 0;
  end
  else
  begin
    if TA.FLength = 0 then
      ThrowTypeError('Reduce of empty typed array with no initial value');
    Accumulator := TGocciaNumberLiteralValue.Create(TA.ReadElement(0));
    I := 1;
  end;

  while I < TA.FLength do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(TGocciaNumberLiteralValue.Create(TA.ReadElement(I)));
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
    ThrowTypeError('TypedArray.prototype.reduceRight requires a callable argument');

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    I := TA.FLength - 1;
  end
  else
  begin
    if TA.FLength = 0 then
      ThrowTypeError('Reduce of empty typed array with no initial value');
    Accumulator := TGocciaNumberLiteralValue.Create(TA.ReadElement(TA.FLength - 1));
    I := TA.FLength - 2;
  end;

  while I >= 0 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(TGocciaNumberLiteralValue.Create(TA.ReadElement(I)));
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
    S := S + TGocciaNumberLiteralValue.Create(TA.ReadElement(I)).ToStringLiteral.Value;
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
  for I := 0 to TA.FLength - 1 do
    NewTA.WriteElement(I, TA.ReadElement(TA.FLength - 1 - I));
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
  for I := 0 to TA.FLength - 1 do
    NewTA.WriteElement(I, TA.ReadElement(I));
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
    ThrowRangeError('Invalid index');
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
    Arr.Elements.Add(TGocciaNumberLiteralValue.Create(TA.ReadElement(I)));
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
    Entry.Elements.Add(TGocciaNumberLiteralValue.Create(TA.ReadElement(I)));
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
    for I := 0 to SrcTA.Length - 1 do
      NewTA.WriteElement(I, SrcTA.ReadElement(I));
    Exit(NewTA);
  end;

  // new TypedArray(buffer [, byteOffset [, length]])
  if (FirstArg is TGocciaArrayBufferValue) and not (FirstArg is TGocciaSharedArrayBufferValue) then
  begin
    Buf := TGocciaArrayBufferValue(FirstArg);
    if AArguments.Length > 1 then
      ByteOff := Trunc(AArguments.GetElement(1).ToNumberLiteral.Value)
    else
      ByteOff := 0;

    if ByteOff < 0 then
      ThrowRangeError('Start offset of ' + TGocciaTypedArrayValue.KindName(FKind) + ' must be non-negative');
    if (ByteOff mod BPE) <> 0 then
      ThrowRangeError('Start offset of ' + TGocciaTypedArrayValue.KindName(FKind) + ' should be a multiple of ' + IntToStr(BPE));
    if ByteOff > System.Length(Buf.Data) then
      ThrowRangeError('Start offset is outside the bounds of the buffer');

    if AArguments.Length > 2 then
    begin
      ElemLen := Trunc(AArguments.GetElement(2).ToNumberLiteral.Value);
      if ElemLen < 0 then
        ThrowRangeError('Invalid typed array length');
      if ByteOff + ElemLen * BPE > System.Length(Buf.Data) then
        ThrowRangeError('Invalid typed array length');
    end
    else
    begin
      if ((System.Length(Buf.Data) - ByteOff) mod BPE) <> 0 then
        ThrowRangeError('Byte length of ' + TGocciaTypedArrayValue.KindName(FKind) + ' should be a multiple of ' + IntToStr(BPE));
      ElemLen := (System.Length(Buf.Data) - ByteOff) div BPE;
      if ElemLen < 0 then
        ThrowRangeError('Invalid typed array length');
    end;

    Exit(TGocciaTypedArrayValue.Create(FKind, Buf, ByteOff, ElemLen));
  end;

  // new TypedArray(array)
  if FirstArg is TGocciaArrayValue then
  begin
    SrcArr := TGocciaArrayValue(FirstArg);
    NewTA := TGocciaTypedArrayValue.Create(FKind, SrcArr.Elements.Count);
    for I := 0 to SrcArr.Elements.Count - 1 do
      NewTA.WriteNumberLiteral(I, SrcArr.Elements[I].ToNumberLiteral);
    Exit(NewTA);
  end;

  // new TypedArray(length)
  Num := FirstArg.ToNumberLiteral;
  if Num.IsNaN then
    Len := 0
  else
  begin
    Len := Trunc(Num.Value);
    if Len < 0 then
      ThrowRangeError('Invalid typed array length');
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
    ThrowTypeError('TypedArray.from requires at least one argument');
  Source := AArgs.GetElement(0);
  HasMapFn := False;
  MapFn := nil;
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    MapFnArg := AArgs.GetElement(1);
    if not MapFnArg.IsCallable then
      ThrowTypeError('TypedArray.from mapfn must be a function');
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
      Val := TGocciaNumberLiteralValue.Create(SrcTA.ReadElement(I));
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
      NewTA.WriteNumberLiteral(I, Val.ToNumberLiteral);
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
      NewTA.WriteNumberLiteral(I, Val.ToNumberLiteral);
    end;
    Exit(NewTA);
  end;

  ThrowTypeError('TypedArray.from requires an array-like or iterable source');
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
    NewTA.WriteNumberLiteral(I, AArgs.GetElement(I).ToNumberLiteral);
  Result := NewTA;
end;

end.
