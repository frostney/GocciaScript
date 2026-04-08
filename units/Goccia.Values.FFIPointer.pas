unit Goccia.Values.FFIPointer;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFFIPointerOwnership = (fpoOwned, fpoBorrowed);

  TGocciaFFIPointerValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
    class var FNullPointer: TGocciaFFIPointerValue;
  private
    FAddress: Pointer;
    FSize: Integer;
    FOwnership: TGocciaFFIPointerOwnership;
    FFreed: Boolean;

    procedure EnsureValid;
    procedure EnsureReadable(const AOffset, ALength: Integer);
    procedure EnsureWritable(const AOffset, ALength: Integer);

    procedure InitializePrototype;
  published
    function IsNullGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AddressGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SizeGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Offset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadU8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadI8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadU16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadI16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadU32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadI32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadF32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadF64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReadCString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteU8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteI8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteU16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteI16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteU32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteI32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteF32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WriteF64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AAddress: Pointer; const ASize: Integer; const AOwnership: TGocciaFFIPointerOwnership);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;

    procedure FreeAllocation;

    class function NullPointer: TGocciaFFIPointerValue;
    class procedure ExposePrototype(const ATarget: TGocciaObjectValue);

    property Address: Pointer read FAddress;
    property Size: Integer read FSize;
    property Ownership: TGocciaFFIPointerOwnership read FOwnership;
    property IsFreed: Boolean read FFreed;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,

  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

const
  FFI_POINTER_TAG = 'FFIPointer';

  PROP_IS_NULL = 'isNull';
  PROP_FFI_ADDRESS = 'address';
  PROP_FFI_SIZE = 'size';

constructor TGocciaFFIPointerValue.Create(const AAddress: Pointer; const ASize: Integer; const AOwnership: TGocciaFFIPointerOwnership);
begin
  inherited Create;
  FAddress := AAddress;
  FSize := ASize;
  FOwnership := AOwnership;
  FFreed := False;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaFFIPointerValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_IS_NULL, IsNullGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_FFI_ADDRESS, AddressGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_FFI_SIZE, SizeGetter, nil, [pfConfigurable]);
      Members.AddNamedMethod('offset', Offset, 1);
      Members.AddNamedMethod('readU8', ReadU8, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readI8', ReadI8, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readU16', ReadU16, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readI16', ReadI16, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readU32', ReadU32, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readI32', ReadI32, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readF32', ReadF32, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readF64', ReadF64, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('readCString', ReadCString, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeU8', WriteU8, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeI8', WriteI8, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeU16', WriteU16, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeI16', WriteI16, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeU32', WriteU32, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeI32', WriteI32, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeF32', WriteF32, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('writeF64', WriteF64, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(FFI_POINTER_TAG),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class function TGocciaFFIPointerValue.NullPointer: TGocciaFFIPointerValue;
begin
  if not Assigned(FNullPointer) then
  begin
    FNullPointer := TGocciaFFIPointerValue.Create(nil, 0, fpoBorrowed);
    TGarbageCollector.Instance.PinObject(FNullPointer);
  end;
  Result := FNullPointer;
end;

class procedure TGocciaFFIPointerValue.ExposePrototype(const ATarget: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaFFIPointerValue.Create(nil, 0, fpoBorrowed);
end;

procedure TGocciaFFIPointerValue.EnsureValid;
begin
  if FFreed then
    ThrowTypeError('Cannot use a freed FFI pointer');
end;

procedure TGocciaFFIPointerValue.EnsureReadable(const AOffset, ALength: Integer);
begin
  EnsureValid;
  if not Assigned(FAddress) then
    ThrowTypeError('Cannot read from a null pointer');
  if (FSize > 0) and ((AOffset < 0) or (AOffset + ALength > FSize)) then
    ThrowRangeError('Read at offset ' + IntToStr(AOffset) + ' with size ' +
      IntToStr(ALength) + ' exceeds allocation of ' + IntToStr(FSize) + ' bytes');
end;

procedure TGocciaFFIPointerValue.EnsureWritable(const AOffset, ALength: Integer);
begin
  EnsureValid;
  if not Assigned(FAddress) then
    ThrowTypeError('Cannot write to a null pointer');
  if (FSize > 0) and ((AOffset < 0) or (AOffset + ALength > FSize)) then
    ThrowRangeError('Write at offset ' + IntToStr(AOffset) + ' with size ' +
      IntToStr(ALength) + ' exceeds allocation of ' + IntToStr(FSize) + ' bytes');
end;

procedure TGocciaFFIPointerValue.FreeAllocation;
begin
  if FFreed then
    ThrowTypeError('FFI pointer already freed');
  if FOwnership <> fpoOwned then
    ThrowTypeError('Cannot free a borrowed FFI pointer');
  if Assigned(FAddress) then
    FreeMem(FAddress);
  FAddress := nil;
  FSize := 0;
  FFreed := True;
end;

function TGocciaFFIPointerValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_IS_NULL then
  begin
    if Assigned(FAddress) then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.TrueValue;
  end
  else if AName = PROP_FFI_ADDRESS then
    Result := TGocciaNumberLiteralValue.Create(PtrUInt(FAddress) * 1.0)
  else if AName = PROP_FFI_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FSize)
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaFFIPointerValue.ToStringTag: string;
begin
  Result := FFI_POINTER_TAG;
end;

procedure TGocciaFFIPointerValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

// -- Accessor getters -------------------------------------------------------

function TGocciaFFIPointerValue.IsNullGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('FFIPointer.isNull requires an FFIPointer');
  if Assigned(TGocciaFFIPointerValue(AThisValue).FAddress) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaFFIPointerValue.AddressGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('FFIPointer.address requires an FFIPointer');
  Result := TGocciaNumberLiteralValue.Create(
    PtrUInt(TGocciaFFIPointerValue(AThisValue).FAddress) * 1.0);
end;

function TGocciaFFIPointerValue.SizeGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('FFIPointer.size requires an FFIPointer');
  Result := TGocciaNumberLiteralValue.Create(
    TGocciaFFIPointerValue(AThisValue).FSize);
end;

// -- Offset -----------------------------------------------------------------

function TGocciaFFIPointerValue.Offset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  ByteOffset: Integer;
  NewAddr: Pointer;
  NewSize: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('FFIPointer.offset requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Ptr.EnsureValid;
  if AArgs.Length < 1 then
    ThrowTypeError('FFIPointer.offset requires 1 argument');
  ByteOffset := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  NewAddr := Pointer(PtrUInt(Ptr.FAddress) + PtrUInt(ByteOffset));
  if Ptr.FSize > 0 then
    NewSize := Ptr.FSize - ByteOffset
  else
    NewSize := 0;
  Result := TGocciaFFIPointerValue.Create(NewAddr, NewSize, fpoBorrowed);
end;

// -- Read methods -----------------------------------------------------------

function GetReadOffset(const AArgs: TGocciaArgumentsCollection): Integer;
begin
  if AArgs.Length > 0 then
    Result := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    Result := 0;
end;

function TGocciaFFIPointerValue.ReadU8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readU8 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 1);
  Result := TGocciaNumberLiteralValue.Create(PByte(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadI8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readI8 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 1);
  Result := TGocciaNumberLiteralValue.Create(PShortInt(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadU16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readU16 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 2);
  Result := TGocciaNumberLiteralValue.Create(PWord(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadI16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readI16 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 2);
  Result := TGocciaNumberLiteralValue.Create(PSmallInt(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadU32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readU32 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 4);
  Result := TGocciaNumberLiteralValue.Create(PLongWord(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadI32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readI32 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 4);
  Result := TGocciaNumberLiteralValue.Create(PLongInt(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadF32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readF32 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 4);
  Result := TGocciaNumberLiteralValue.Create(PSingle(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadF64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readF64 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureReadable(Off, 8);
  Result := TGocciaNumberLiteralValue.Create(PDouble(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^);
end;

function TGocciaFFIPointerValue.ReadCString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
  CStr: PAnsiChar;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('readCString requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  Off := GetReadOffset(AArgs);
  Ptr.EnsureValid;
  if not Assigned(Ptr.FAddress) then
    ThrowTypeError('Cannot read from a null pointer');
  CStr := PAnsiChar(PtrUInt(Ptr.FAddress) + PtrUInt(Off));
  Result := TGocciaStringLiteralValue.Create(string(CStr));
end;

// -- Write methods ----------------------------------------------------------

function TGocciaFFIPointerValue.WriteU8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeU8 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeU8 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 1);
  PByte(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := Byte(Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteI8(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeI8 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeI8 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 1);
  PShortInt(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := ShortInt(Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteU16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeU16 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeU16 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 2);
  PWord(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := Word(Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteI16(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeI16 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeI16 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 2);
  PSmallInt(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := SmallInt(Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteU32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeU32 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeU32 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 4);
  PLongWord(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := LongWord(Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteI32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeI32 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeI32 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 4);
  PLongInt(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := LongInt(Trunc(AArgs.GetElement(1).ToNumberLiteral.Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteF32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
  FloatVal: Single;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeF32 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeF32 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 4);
  FloatVal := AArgs.GetElement(1).ToNumberLiteral.Value;
  PSingle(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := FloatVal;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFIPointerValue.WriteF64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
  Off: Integer;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('writeF64 requires an FFIPointer');
  Ptr := TGocciaFFIPointerValue(AThisValue);
  if AArgs.Length < 2 then
    ThrowTypeError('writeF64 requires offset and value');
  Off := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
  Ptr.EnsureWritable(Off, 8);
  PDouble(PtrUInt(Ptr.FAddress) + PtrUInt(Off))^ := AArgs.GetElement(1).ToNumberLiteral.Value;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
