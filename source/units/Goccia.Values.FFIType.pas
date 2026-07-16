unit Goccia.Values.FFIType;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.FFI.LibraryGuard,
  Goccia.FFI.Types,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFFITypeDescriptorValue = class(TGocciaObjectValue)
  private
    FDescriptor: TGocciaFFITypeDescriptor;
    FCreateFunction: TGocciaValue;
    function CreateValue(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ADescriptor: TGocciaFFITypeDescriptor;
      const AAdoptReference: Boolean = False);
    destructor Destroy; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
    property Descriptor: TGocciaFFITypeDescriptor read FDescriptor;
  end;

  TGocciaFFIAggregatePointerGuardEntry = record
    Offset: Integer;
    LifetimeGuard: TGocciaFFIDependentGuard;
  end;

  TGocciaFFIAggregatePointerGuards = class
  private
    FReferenceCount: Integer;
    FEntries: array of TGocciaFFIAggregatePointerGuardEntry;
    function FindEntry(const AOffset: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddReference;
    procedure ReleaseReference;
    function GuardAt(const AOffset: Integer): TGocciaFFIDependentGuard;
    procedure SetGuard(const AOffset: Integer;
      const ALifetimeGuard: TGocciaFFIDependentGuard);
    procedure ClearRange(const AOffset, ASize: Integer);
    procedure CopyRangeFrom(const ASource: TGocciaFFIAggregatePointerGuards;
      const ASourceOffset, ADestinationOffset, ASize: Integer);
  end;

  TGocciaFFIAggregateValue = class(TGocciaObjectValue)
  private
    FDescriptor: TGocciaFFITypeDescriptor;
    FBuffer: TGocciaArrayBufferValue;
    FByteOffset: Integer;
    FPointerGuards: TGocciaFFIAggregatePointerGuards;
    function GetElementOrField(const AName: string;
      out AType: TGocciaFFITypeDescriptor; out AOffset: Integer): Boolean;
    function ReadValue(const AType: TGocciaFFITypeDescriptor;
      const AOffset: Integer): TGocciaValue;
    procedure WriteValue(const AType: TGocciaFFITypeDescriptor;
      const AOffset: Integer; const AValue: TGocciaValue);
    procedure ApplyInitializer(const AInitializer: TGocciaValue);
  public
    constructor Create(const ADescriptor: TGocciaFFITypeDescriptor;
      const AInitializer: TGocciaValue = nil); overload;
    constructor CreateView(const ADescriptor: TGocciaFFITypeDescriptor;
      const ABuffer: TGocciaArrayBufferValue;
      const AByteOffset: Integer;
      const APointerGuards: TGocciaFFIAggregatePointerGuards = nil); overload;
    destructor Destroy; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure SetProperty(const AName: string;
      const AValue: TGocciaValue); override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue;
      const ACanCreate: Boolean = True); override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
    procedure EnsureBackingStore;
    function DataPointer: Pointer;
    procedure CopyTo(const ADestination: Pointer);
    procedure CopyFrom(const ASource: Pointer);
    procedure AttachLibraryPointerFields(
      const ALibraryGuard: TGocciaFFILibraryGuard);
    property Descriptor: TGocciaFFITypeDescriptor read FDescriptor;
    property Buffer: TGocciaArrayBufferValue read FBuffer;
    property ByteOffset: Integer read FByteOffset;
  end;

function ParseFFITypeDescriptorValue(const AValue: TGocciaValue;
  const AAllowVoid: Boolean): TGocciaFFITypeDescriptor;
function CreateFFIStructType(const ADefinition: TGocciaValue): TGocciaValue;
function CreateFFIUnionType(const ADefinition: TGocciaValue): TGocciaValue;
function CreateFFIArrayType(const AElementValue, ALengthValue: TGocciaValue): TGocciaValue;
function CreateFFICallbackType(const ADefinition: TGocciaValue): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.BinaryData,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFICallback,
  Goccia.Values.FFIPointer,
  Goccia.Values.NativeFunction;

const
  FFI_TYPE_DESCRIPTOR_TAG = 'FFIType';
  FFI_AGGREGATE_TAG = 'FFIAggregate';
  PROP_CREATE = 'create';
  PROP_KIND = 'kind';
  PROP_SIZE = 'size';
  PROP_ALIGNMENT = 'alignment';
  PROP_BUFFER = 'buffer';
  PROP_BYTE_OFFSET = 'byteOffset';
  PROP_LENGTH = 'length';

constructor TGocciaFFIAggregatePointerGuards.Create;
begin
  inherited;
  FReferenceCount := 1;
end;

destructor TGocciaFFIAggregatePointerGuards.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FEntries) do
    if Assigned(FEntries[I].LifetimeGuard) then
      FEntries[I].LifetimeGuard.ReleaseDependent;
  inherited;
end;

function TGocciaFFIAggregatePointerGuards.FindEntry(
  const AOffset: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FEntries) do
    if FEntries[I].Offset = AOffset then
      Exit(I);
  Result := -1;
end;

procedure TGocciaFFIAggregatePointerGuards.AddReference;
begin
  Inc(FReferenceCount);
end;

procedure TGocciaFFIAggregatePointerGuards.ReleaseReference;
begin
  Dec(FReferenceCount);
  if FReferenceCount <= 0 then
    Free;
end;

function TGocciaFFIAggregatePointerGuards.GuardAt(
  const AOffset: Integer): TGocciaFFIDependentGuard;
var
  Index: Integer;
begin
  Index := FindEntry(AOffset);
  if Index >= 0 then
    Result := FEntries[Index].LifetimeGuard
  else
    Result := nil;
end;

procedure TGocciaFFIAggregatePointerGuards.SetGuard(
  const AOffset: Integer; const ALifetimeGuard: TGocciaFFIDependentGuard);
var
  Index, I: Integer;
begin
  Index := FindEntry(AOffset);
  if not Assigned(ALifetimeGuard) then
  begin
    if Index < 0 then Exit;
    if Assigned(FEntries[Index].LifetimeGuard) then
      FEntries[Index].LifetimeGuard.ReleaseDependent;
    for I := Index to High(FEntries) - 1 do
      FEntries[I] := FEntries[I + 1];
    SetLength(FEntries, Length(FEntries) - 1);
    Exit;
  end;

  if Index >= 0 then
  begin
    if FEntries[Index].LifetimeGuard = ALifetimeGuard then Exit;
    if Assigned(FEntries[Index].LifetimeGuard) then
      FEntries[Index].LifetimeGuard.ReleaseDependent;
  end
  else
  begin
    Index := Length(FEntries);
    SetLength(FEntries, Index + 1);
    FEntries[Index].Offset := AOffset;
  end;

  ALifetimeGuard.RetainDependent;
  FEntries[Index].LifetimeGuard := ALifetimeGuard;
end;

procedure TGocciaFFIAggregatePointerGuards.ClearRange(
  const AOffset, ASize: Integer);
var
  I, EndOffset: Integer;
begin
  if ASize <= 0 then Exit;
  EndOffset := AOffset + ASize;
  I := 0;
  while I < Length(FEntries) do
  begin
    if (FEntries[I].Offset >= AOffset) and
       (FEntries[I].Offset < EndOffset) then
      SetGuard(FEntries[I].Offset, nil)
    else
      Inc(I);
  end;
end;

procedure TGocciaFFIAggregatePointerGuards.CopyRangeFrom(
  const ASource: TGocciaFFIAggregatePointerGuards;
  const ASourceOffset, ADestinationOffset, ASize: Integer);
var
  I, Count, EndOffset: Integer;
  Offsets: array of Integer;
  Guards: array of TGocciaFFIDependentGuard;
begin
  if ASize <= 0 then Exit;
  if not Assigned(ASource) then
  begin
    ClearRange(ADestinationOffset, ASize);
    Exit;
  end;
  if (ASource = Self) and (ASourceOffset = ADestinationOffset) then Exit;

  EndOffset := ASourceOffset + ASize;
  Count := 0;
  for I := 0 to High(ASource.FEntries) do
    if (ASource.FEntries[I].Offset >= ASourceOffset) and
       (ASource.FEntries[I].Offset < EndOffset) then
    begin
      SetLength(Offsets, Count + 1);
      SetLength(Guards, Count + 1);
      Offsets[Count] := ADestinationOffset +
        (ASource.FEntries[I].Offset - ASourceOffset);
      Guards[Count] := ASource.FEntries[I].LifetimeGuard;
      if Assigned(Guards[Count]) then
        Guards[Count].RetainDependent;
      Inc(Count);
    end;

  ClearRange(ADestinationOffset, ASize);
  for I := 0 to Count - 1 do
    SetGuard(Offsets[I], Guards[I]);
  for I := 0 to Count - 1 do
    if Assigned(Guards[I]) then
      Guards[I].ReleaseDependent;
end;

function TypeKindName(const AKind: TGocciaFFITypeKind): string;
begin
  case AKind of
    ftkScalar: Result := 'scalar';
    ftkStruct: Result := 'struct';
    ftkUnion: Result := 'union';
    ftkArray: Result := 'array';
    ftkCallback: Result := 'callback';
  else
    Result := 'unknown';
  end;
end;

function NumericElementKind(const AType: TGocciaFFIType): TGocciaBinaryElementKind;
begin
  case AType of
    fftBool, fftU8: Result := bekUint8;
    fftI8: Result := bekInt8;
    fftI16: Result := bekInt16;
    fftU16: Result := bekUint16;
    fftI32: Result := bekInt32;
    fftU32: Result := bekUint32;
    fftI64: Result := bekBigInt64;
    fftU64: Result := bekBigUint64;
    fftF32: Result := bekFloat32;
    fftF64: Result := bekFloat64;
  else
    Result := bekUint8;
  end;
end;

function ParseCanonicalIndex(const AName: string; out AIndex: Integer): Boolean;
begin
  Result := TryStrToInt(AName, AIndex) and (AIndex >= 0) and
    (IntToStr(AIndex) = AName);
end;

function ParseFFITypeDescriptorValue(const AValue: TGocciaValue;
  const AAllowVoid: Boolean): TGocciaFFITypeDescriptor;
var
  TypeName: string;
  ScalarType: TGocciaFFIType;
begin
  if AValue is TGocciaFFITypeDescriptorValue then
  begin
    Result := TGocciaFFITypeDescriptorValue(AValue).Descriptor;
    Result.AddReference;
    Exit;
  end;

  if not (AValue is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorFFITypeDescriptorExpected,
      SSuggestFFIUsage);

  TypeName := TGocciaStringLiteralValue(AValue).Value;
  ScalarType := ParseFFIType(TypeName);
  if (ScalarType = fftVoid) and (TypeName <> FFI_TYPE_VOID) then
    ThrowTypeError(Format(SErrorFFIUnknownType, [TypeName]), SSuggestFFIUsage);
  if (ScalarType = fftVoid) and not AAllowVoid then
    ThrowTypeError(SErrorFFIVoidReturnOnly,
      SSuggestFFIUsage);
  {$IFNDEF CPU64}
  if ScalarType in [fftI64, fftU64] then
    ThrowTypeError(SErrorFFI64BitType32Bit, SSuggestFFIUsage);
  {$ENDIF}
  Result := TGocciaFFITypeDescriptor.CreateScalar(ScalarType);
end;

function CreateAggregateType(const ADefinition: TGocciaValue;
  const AKind: TGocciaFFITypeKind): TGocciaValue;
var
  DefinitionObject: TGocciaObjectValue;
  FieldNames: TArray<string>;
  Fields: array of TGocciaFFIFieldDescriptor;
  Descriptor: TGocciaFFITypeDescriptor;
  I, J: Integer;
begin
  if not (ADefinition is TGocciaObjectValue) or
     (ADefinition is TGocciaArrayValue) then
    ThrowTypeError(SErrorFFIAggregateDefinitionObject,
      SSuggestFFIUsage);
  DefinitionObject := TGocciaObjectValue(ADefinition);
  FieldNames := DefinitionObject.GetEnumerablePropertyNames;
  if Length(FieldNames) = 0 then
    ThrowTypeError(SErrorFFIAggregateDefinitionEmpty,
      SSuggestFFIUsage);

  SetLength(Fields, Length(FieldNames));
  try
    for I := 0 to High(FieldNames) do
    begin
      if (FieldNames[I] = PROP_BUFFER) or
         (FieldNames[I] = PROP_BYTE_OFFSET) then
        ThrowTypeError(Format(SErrorFFIAggregateFieldReserved,
          [FieldNames[I]]), SSuggestFFIUsage);
      Fields[I].Name := FieldNames[I];
      Fields[I].TypeDescriptor := ParseFFITypeDescriptorValue(
        DefinitionObject.GetProperty(FieldNames[I]), False);
    end;
    try
      if AKind = ftkStruct then
        Descriptor := TGocciaFFITypeDescriptor.CreateStruct(Fields)
      else
        Descriptor := TGocciaFFITypeDescriptor.CreateUnion(Fields);
    except
      on E: Exception do ThrowTypeError(E.Message, SSuggestFFIUsage);
    end;
  finally
    for J := 0 to High(Fields) do
      if Assigned(Fields[J].TypeDescriptor) then
        Fields[J].TypeDescriptor.ReleaseReference;
  end;
  Result := TGocciaFFITypeDescriptorValue.Create(Descriptor, True);
end;

function CreateFFIStructType(const ADefinition: TGocciaValue): TGocciaValue;
begin
  Result := CreateAggregateType(ADefinition, ftkStruct);
end;

function CreateFFIUnionType(const ADefinition: TGocciaValue): TGocciaValue;
begin
  Result := CreateAggregateType(ADefinition, ftkUnion);
end;

function CreateFFIArrayType(const AElementValue,
  ALengthValue: TGocciaValue): TGocciaValue;
var
  ElementType, Descriptor: TGocciaFFITypeDescriptor;
  LengthLiteral: TGocciaNumberLiteralValue;
  LengthValue: Double;
  ElementCount: Integer;
begin
  LengthLiteral := ALengthValue.ToNumberLiteral;
  LengthValue := LengthLiteral.Value;
  if LengthLiteral.IsNaN or LengthLiteral.IsInfinity or
     LengthLiteral.IsNegativeInfinity or (LengthValue < 1) or
     (LengthValue > MAX_FFI_AGGREGATE_SIZE) or
     (LengthValue <> Trunc(LengthValue)) then
    ThrowTypeError(SErrorFFIArrayLengthPositive,
      SSuggestFFIUsage);
  ElementCount := Trunc(LengthValue);
  ElementType := ParseFFITypeDescriptorValue(AElementValue, False);
  try
    try
      Descriptor := TGocciaFFITypeDescriptor.CreateArray(ElementType,
        ElementCount);
    except
      on E: Exception do ThrowTypeError(E.Message, SSuggestFFIUsage);
    end;
  finally
    ElementType.ReleaseReference;
  end;
  Result := TGocciaFFITypeDescriptorValue.Create(Descriptor, True);
end;

function CreateFFICallbackType(const ADefinition: TGocciaValue): TGocciaValue;
var
  DefinitionObject: TGocciaObjectValue;
  ArgumentsValue, ReturnValue: TGocciaValue;
  ArgumentsArray: TGocciaArrayValue;
  ArgumentTypes: array of TGocciaFFITypeDescriptor;
  ReturnType, Descriptor: TGocciaFFITypeDescriptor;
  I, J: Integer;
begin
  if not (ADefinition is TGocciaObjectValue) or
     (ADefinition is TGocciaArrayValue) then
    ThrowTypeError(SErrorFFICallbackDefinitionObject,
      SSuggestFFIUsage);
  DefinitionObject := TGocciaObjectValue(ADefinition);

  ReturnType := nil;
  try
    ArgumentsValue := DefinitionObject.GetProperty('args');
    if ArgumentsValue is TGocciaArrayValue then
    begin
      ArgumentsArray := TGocciaArrayValue(ArgumentsValue);
      if ArgumentsArray.Elements.Count > MAX_FFI_ARGS then
        ThrowTypeError(Format(SErrorFFIMaxCallbackArguments,
          [MAX_FFI_ARGS]), SSuggestFFIUsage);
      SetLength(ArgumentTypes, ArgumentsArray.Elements.Count);
      for I := 0 to ArgumentsArray.Elements.Count - 1 do
        ArgumentTypes[I] := ParseFFITypeDescriptorValue(
          ArgumentsArray.Elements[I], False);
    end
    else if not (ArgumentsValue is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(SErrorFFICallbackArgsArray, SSuggestFFIUsage);

    ReturnValue := DefinitionObject.GetProperty('returns');
    if ReturnValue is TGocciaUndefinedLiteralValue then
      ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid)
    else
      ReturnType := ParseFFITypeDescriptorValue(ReturnValue, True);
    try
      Descriptor := TGocciaFFITypeDescriptor.CreateCallback(ArgumentTypes,
        ReturnType);
    except
      on E: Exception do ThrowTypeError(E.Message, SSuggestFFIUsage);
    end;
  finally
    if Assigned(ReturnType) then ReturnType.ReleaseReference;
    for J := 0 to High(ArgumentTypes) do
      if Assigned(ArgumentTypes[J]) then
        ArgumentTypes[J].ReleaseReference;
  end;
  Result := TGocciaFFITypeDescriptorValue.Create(Descriptor, True);
end;

constructor TGocciaFFITypeDescriptorValue.Create(
  const ADescriptor: TGocciaFFITypeDescriptor;
  const AAdoptReference: Boolean);
var
  CreateFunction: TGocciaNativeFunctionValue;
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype);
  FDescriptor := ADescriptor;
  if not AAdoptReference then
    FDescriptor.AddReference;
  CreateFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    CreateValue, PROP_CREATE, 1);
  CreateFunction.CapturedRoot := Self;
  FCreateFunction := CreateFunction;
end;

destructor TGocciaFFITypeDescriptorValue.Destroy;
begin
  FDescriptor.ReleaseReference;
  inherited;
end;

function TGocciaFFITypeDescriptorValue.CreateValue(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Initializer: TGocciaValue;
begin
  if FDescriptor.Kind = ftkCallback then
  begin
    if AArguments.Length < 1 then
      ThrowTypeError(SErrorFFICallbackCreateCallable,
        SSuggestFFIUsage);
    Exit(TGocciaFFICallbackValue.Create(FDescriptor,
      AArguments.GetElement(0)));
  end;
  if not FDescriptor.IsAggregate then
    ThrowTypeError(SErrorFFITypeCannotCreateValue,
      SSuggestFFIUsage);
  if AArguments.Length > 0 then
    Initializer := AArguments.GetElement(0)
  else
    Initializer := nil;
  Result := TGocciaFFIAggregateValue.Create(FDescriptor, Initializer);
end;

function TGocciaFFITypeDescriptorValue.GetProperty(
  const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFFITypeDescriptorValue.GetPropertyWithContext(
  const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_CREATE then
    Result := FCreateFunction
  else if AName = PROP_KIND then
    Result := TGocciaStringLiteralValue.Create(TypeKindName(FDescriptor.Kind))
  else if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FDescriptor.Size)
  else if AName = PROP_ALIGNMENT then
    Result := TGocciaNumberLiteralValue.Create(FDescriptor.Alignment)
  else if (AName = PROP_LENGTH) and (FDescriptor.Kind = ftkArray) then
    Result := TGocciaNumberLiteralValue.Create(FDescriptor.ElementCount)
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaFFITypeDescriptorValue.ToStringTag: string;
begin
  Result := FFI_TYPE_DESCRIPTOR_TAG;
end;

procedure TGocciaFFITypeDescriptorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCreateFunction) then
    FCreateFunction.MarkReferences;
end;

constructor TGocciaFFIAggregateValue.Create(
  const ADescriptor: TGocciaFFITypeDescriptor;
  const AInitializer: TGocciaValue);
var
  GarbageCollector: TGarbageCollector;
  BufferPinned: Boolean;
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype);
  FDescriptor := ADescriptor;
  FDescriptor.AddReference;
  FBuffer := TGocciaArrayBufferValue.Create(FDescriptor.Size);
  FPointerGuards := TGocciaFFIAggregatePointerGuards.Create;
  FByteOffset := 0;
  if Assigned(AInitializer) and
     not (AInitializer is TGocciaUndefinedLiteralValue) then
  begin
    GarbageCollector := TGarbageCollector.Instance;
    BufferPinned := Assigned(GarbageCollector);
    if BufferPinned then
      GarbageCollector.PinObject(FBuffer);
    try
      ApplyInitializer(AInitializer);
    finally
      if BufferPinned then
        GarbageCollector.UnpinObject(FBuffer);
    end;
  end;
end;

constructor TGocciaFFIAggregateValue.CreateView(
  const ADescriptor: TGocciaFFITypeDescriptor;
  const ABuffer: TGocciaArrayBufferValue; const AByteOffset: Integer;
  const APointerGuards: TGocciaFFIAggregatePointerGuards);
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype);
  FDescriptor := ADescriptor;
  FDescriptor.AddReference;
  FBuffer := ABuffer;
  FByteOffset := AByteOffset;
  if Assigned(APointerGuards) then
  begin
    FPointerGuards := APointerGuards;
    FPointerGuards.AddReference;
  end
  else
    FPointerGuards := TGocciaFFIAggregatePointerGuards.Create;
  EnsureBackingStore;
end;

destructor TGocciaFFIAggregateValue.Destroy;
begin
  FDescriptor.ReleaseReference;
  if Assigned(FPointerGuards) then
    FPointerGuards.ReleaseReference;
  inherited;
end;

function TGocciaFFIAggregateValue.GetElementOrField(const AName: string;
  out AType: TGocciaFFITypeDescriptor; out AOffset: Integer): Boolean;
var
  Index: Integer;
  Field: TGocciaFFIFieldDescriptor;
begin
  if FDescriptor.Kind in [ftkStruct, ftkUnion] then
  begin
    Index := FDescriptor.FieldIndex(AName);
    if Index >= 0 then
    begin
      Field := FDescriptor.FieldAt(Index);
      AType := Field.TypeDescriptor;
      AOffset := FByteOffset + Field.Offset;
      Exit(True);
    end;
  end
  else if (FDescriptor.Kind = ftkArray) and
          ParseCanonicalIndex(AName, Index) and
          (Index < FDescriptor.ElementCount) then
  begin
    AType := FDescriptor.ElementType;
    AOffset := FByteOffset + (Index * AType.Size);
    Exit(True);
  end;
  Result := False;
end;

function TGocciaFFIAggregateValue.ReadValue(
  const AType: TGocciaFFITypeDescriptor;
  const AOffset: Integer): TGocciaValue;
var
  Data: TBytes;
  RawPointer: PtrUInt;
begin
  EnsureBackingStore;
  if AType.IsAggregate then
    Exit(TGocciaFFIAggregateValue.CreateView(AType, FBuffer, AOffset,
      FPointerGuards));
  if AType.Kind = ftkCallback then
  begin
    Data := FBuffer.Data;
    RawPointer := 0;
    Move(Data[AOffset], RawPointer, SizeOf(Pointer));
    Exit(TGocciaFFIPointerValue.Create(Pointer(RawPointer),
      FPointerGuards.GuardAt(AOffset)));
  end;

  Data := FBuffer.Data;
  case AType.ScalarType of
    fftBool:
      if Data[AOffset] <> 0 then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    fftI64, fftU64:
      Result := TGocciaNumberLiteralValue.Create(ReadBinaryBigIntElement(Data,
        AOffset, NumericElementKind(AType.ScalarType), True));
    fftPointer, fftCString:
    begin
      RawPointer := 0;
      Move(Data[AOffset], RawPointer, SizeOf(Pointer));
      Result := TGocciaFFIPointerValue.Create(Pointer(RawPointer),
        FPointerGuards.GuardAt(AOffset));
    end;
    fftVoid:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  else
    Result := TGocciaNumberLiteralValue.Create(ReadBinaryNumberElement(Data,
      AOffset, NumericElementKind(AType.ScalarType), True));
  end;
end;

procedure TGocciaFFIAggregateValue.WriteValue(
  const AType: TGocciaFFITypeDescriptor; const AOffset: Integer;
  const AValue: TGocciaValue);
var
  Data, SourceData: TBytes;
  SourceAggregate: TGocciaFFIAggregateValue;
  CallbackValue: TGocciaFFICallbackValue;
  RawPointer: PtrUInt;
begin
  EnsureBackingStore;
  Data := FBuffer.Data;
  if AType.IsAggregate then
  begin
    if not (AValue is TGocciaFFIAggregateValue) then
      ThrowTypeError(SErrorFFIAggregateFieldValue,
        SSuggestFFIUsage);
    SourceAggregate := TGocciaFFIAggregateValue(AValue);
    if SourceAggregate.Descriptor <> AType then
      ThrowTypeError(SErrorFFIAggregateFieldType,
        SSuggestFFIUsage);
    SourceAggregate.EnsureBackingStore;
    SourceData := SourceAggregate.Buffer.Data;
    Move(SourceData[SourceAggregate.ByteOffset], Data[AOffset], AType.Size);
    FBuffer.Data := Data;
    FPointerGuards.CopyRangeFrom(SourceAggregate.FPointerGuards,
      SourceAggregate.ByteOffset, AOffset, AType.Size);
    Exit;
  end;

  if AType.Kind = ftkCallback then
  begin
    if AValue is TGocciaNullLiteralValue then
    begin
      RawPointer := 0;
      Move(RawPointer, Data[AOffset], SizeOf(Pointer));
      FBuffer.Data := Data;
      FPointerGuards.SetGuard(AOffset, nil);
      Exit;
    end;
    if not (AValue is TGocciaFFICallbackValue) then
      ThrowTypeError(SErrorFFICallbackFieldHandle,
        SSuggestFFIUsage);
    CallbackValue := TGocciaFFICallbackValue(AValue);
    if CallbackValue.Descriptor <> AType then
      ThrowTypeError(SErrorFFICallbackFieldType,
        SSuggestFFIUsage);
    CallbackValue.EnsureOpen;
    RawPointer := PtrUInt(CallbackValue.CodePointer);
    Move(RawPointer, Data[AOffset], SizeOf(Pointer));
    FBuffer.Data := Data;
    FPointerGuards.SetGuard(AOffset, CallbackValue.LifetimeGuard);
    Exit;
  end;

  case AType.ScalarType of
    fftBool:
      if AValue.ToBooleanLiteral.Value then
        Data[AOffset] := 1
      else
        Data[AOffset] := 0;
    fftI64, fftU64:
      WriteBinaryBigIntElement(Data, AOffset,
        ToInt64Value(AValue), True);
    fftPointer, fftCString:
    begin
      if AValue is TGocciaFFIPointerValue then
      begin
        RawPointer := PtrUInt(TGocciaFFIPointerValue(AValue).Address);
        FPointerGuards.SetGuard(AOffset,
          TGocciaFFIPointerValue(AValue).LifetimeGuard);
      end
      else if AValue is TGocciaNullLiteralValue then
      begin
        RawPointer := 0;
        FPointerGuards.SetGuard(AOffset, nil);
      end
      else
        ThrowTypeError(SErrorFFIPointerFieldValue,
          SSuggestFFIUsage);
      Move(RawPointer, Data[AOffset], SizeOf(Pointer));
    end;
    fftVoid:
      ThrowTypeError(SErrorFFIVoidAggregateStorage,
        SSuggestFFIUsage);
  else
    WriteBinaryNumberElement(Data, AOffset,
      NumericElementKind(AType.ScalarType),
      AValue.ToNumberLiteral.Value, True);
  end;
  if not (AType.ScalarType in [fftPointer, fftCString]) then
    FPointerGuards.ClearRange(AOffset, AType.Size);
  FBuffer.Data := Data;
end;

procedure TGocciaFFIAggregateValue.ApplyInitializer(
  const AInitializer: TGocciaValue);
var
  InitializerObject: TGocciaObjectValue;
  I: Integer;
  Field: TGocciaFFIFieldDescriptor;
  Value: TGocciaValue;
begin
  if not (AInitializer is TGocciaObjectValue) then
    ThrowTypeError(SErrorFFIAggregateInitializer,
      SSuggestFFIUsage);
  InitializerObject := TGocciaObjectValue(AInitializer);
  if FDescriptor.Kind in [ftkStruct, ftkUnion] then
    for I := 0 to FDescriptor.FieldCount - 1 do
    begin
      Field := FDescriptor.FieldAt(I);
      Value := InitializerObject.GetProperty(Field.Name);
      if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
        WriteValue(Field.TypeDescriptor, FByteOffset + Field.Offset, Value);
    end
  else if FDescriptor.Kind = ftkArray then
    for I := 0 to FDescriptor.ElementCount - 1 do
    begin
      Value := InitializerObject.GetProperty(IntToStr(I));
      if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
        WriteValue(FDescriptor.ElementType,
          FByteOffset + (I * FDescriptor.ElementType.Size), Value);
    end;
end;

function TGocciaFFIAggregateValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFFIAggregateValue.GetPropertyWithContext(const AName: string;
  const AThisContext: TGocciaValue): TGocciaValue;
var
  ElementType: TGocciaFFITypeDescriptor;
  Offset: Integer;
begin
  if GetElementOrField(AName, ElementType, Offset) then
    Result := ReadValue(ElementType, Offset)
  else if AName = PROP_BUFFER then
    Result := FBuffer
  else if AName = PROP_BYTE_OFFSET then
    Result := TGocciaNumberLiteralValue.Create(FByteOffset)
  else if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FDescriptor.Size)
  else if (AName = PROP_LENGTH) and (FDescriptor.Kind = ftkArray) then
    Result := TGocciaNumberLiteralValue.Create(FDescriptor.ElementCount)
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

procedure TGocciaFFIAggregateValue.SetProperty(const AName: string;
  const AValue: TGocciaValue);
var
  ElementType: TGocciaFFITypeDescriptor;
  Offset: Integer;
begin
  if GetElementOrField(AName, ElementType, Offset) then
  begin
    WriteValue(ElementType, Offset, AValue);
    Exit;
  end;
  inherited;
end;

procedure TGocciaFFIAggregateValue.AssignProperty(const AName: string;
  const AValue: TGocciaValue; const ACanCreate: Boolean);
var
  ElementType: TGocciaFFITypeDescriptor;
  Offset: Integer;
begin
  if GetElementOrField(AName, ElementType, Offset) then
  begin
    WriteValue(ElementType, Offset, AValue);
    Exit;
  end;
  inherited;
end;

function TGocciaFFIAggregateValue.ToStringTag: string;
begin
  Result := FFI_AGGREGATE_TAG;
end;

procedure TGocciaFFIAggregateValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FBuffer) then
    FBuffer.MarkReferences;
end;

procedure TGocciaFFIAggregateValue.EnsureBackingStore;
var
  DataLength: Integer;
begin
  if not Assigned(FBuffer) or FBuffer.Detached then
    ThrowTypeError(SErrorFFIAggregateBackingStore, SSuggestFFIUsage);
  DataLength := Length(FBuffer.Data);
  if (FByteOffset < 0) or (FByteOffset > DataLength) or
     (FDescriptor.Size > DataLength - FByteOffset) then
    ThrowTypeError(SErrorFFIAggregateBackingStore, SSuggestFFIUsage);
end;

function TGocciaFFIAggregateValue.DataPointer: Pointer;
var
  Data: TBytes;
begin
  EnsureBackingStore;
  Data := FBuffer.Data;
  if Length(Data) = 0 then
    Result := nil
  else
    Result := @Data[FByteOffset];
end;

procedure TGocciaFFIAggregateValue.CopyTo(const ADestination: Pointer);
var
  Data: TBytes;
begin
  EnsureBackingStore;
  if (FDescriptor.Size = 0) or not Assigned(ADestination) then
    Exit;
  Data := FBuffer.Data;
  Move(Data[FByteOffset], ADestination^, FDescriptor.Size);
end;

procedure TGocciaFFIAggregateValue.CopyFrom(const ASource: Pointer);
var
  Data: TBytes;
begin
  EnsureBackingStore;
  if (FDescriptor.Size = 0) or not Assigned(ASource) then
    Exit;
  Data := FBuffer.Data;
  Move(ASource^, Data[FByteOffset], FDescriptor.Size);
  FBuffer.Data := Data;
  FPointerGuards.ClearRange(FByteOffset, FDescriptor.Size);
end;

procedure TGocciaFFIAggregateValue.AttachLibraryPointerFields(
  const ALibraryGuard: TGocciaFFILibraryGuard);
var
  Data: TBytes;

  procedure AttachType(const AType: TGocciaFFITypeDescriptor;
    const AOffset: Integer);
  var
    I: Integer;
    Field: TGocciaFFIFieldDescriptor;
    RawPointer: PtrUInt;
  begin
    if AType.Kind = ftkCallback then
    begin
      RawPointer := 0;
      Move(Data[AOffset], RawPointer, SizeOf(Pointer));
      if RawPointer <> 0 then
        FPointerGuards.SetGuard(AOffset, ALibraryGuard)
      else
        FPointerGuards.SetGuard(AOffset, nil);
      Exit;
    end;

    if AType.IsAggregate then
    begin
      case AType.Kind of
        ftkStruct, ftkUnion:
          for I := 0 to AType.FieldCount - 1 do
          begin
            Field := AType.FieldAt(I);
            AttachType(Field.TypeDescriptor, AOffset + Field.Offset);
          end;
        ftkArray:
          for I := 0 to AType.ElementCount - 1 do
            AttachType(AType.ElementType,
              AOffset + (I * AType.ElementType.Size));
      end;
      Exit;
    end;

    if AType.ScalarType in [fftPointer, fftCString] then
    begin
      RawPointer := 0;
      Move(Data[AOffset], RawPointer, SizeOf(Pointer));
      if RawPointer <> 0 then
        FPointerGuards.SetGuard(AOffset, ALibraryGuard)
      else
        FPointerGuards.SetGuard(AOffset, nil);
    end;
  end;

begin
  if not Assigned(ALibraryGuard) then Exit;
  EnsureBackingStore;
  Data := FBuffer.Data;
  AttachType(FDescriptor, FByteOffset);
end;

end.
