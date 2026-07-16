unit Goccia.FFI.Types;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaFFIType = (
    fftVoid,
    fftBool,
    fftI8, fftI16, fftI32, fftI64,
    fftU8, fftU16, fftU32, fftU64,
    fftF32, fftF64,
    fftPointer,
    fftCString
  );

  TGocciaFFITypeKind = (
    ftkScalar,
    ftkStruct,
    ftkUnion,
    ftkArray,
    ftkCallback
  );

  TGocciaFFITypeDescriptor = class;

  TGocciaFFIFieldDescriptor = record
    Name: string;
    TypeDescriptor: TGocciaFFITypeDescriptor;
    Offset: Integer;
  end;

  TGocciaFFITypeDescriptor = class
  private
    FReferenceCount: Integer;
    FKind: TGocciaFFITypeKind;
    FScalarType: TGocciaFFIType;
    FFields: array of TGocciaFFIFieldDescriptor;
    FElementType: TGocciaFFITypeDescriptor;
    FElementCount: Integer;
    FCallbackArguments: array of TGocciaFFITypeDescriptor;
    FCallbackReturn: TGocciaFFITypeDescriptor;
    FSize: Integer;
    FAlignment: Integer;
    FDepth: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateScalar(
      const AScalarType: TGocciaFFIType): TGocciaFFITypeDescriptor;
    class function CreateStruct(
      const AFields: array of TGocciaFFIFieldDescriptor): TGocciaFFITypeDescriptor;
    class function CreateUnion(
      const AFields: array of TGocciaFFIFieldDescriptor): TGocciaFFITypeDescriptor;
    class function CreateArray(const AElementType: TGocciaFFITypeDescriptor;
      const AElementCount: Integer): TGocciaFFITypeDescriptor;
    class function CreateCallback(
      const AArguments: array of TGocciaFFITypeDescriptor;
      const AReturnType: TGocciaFFITypeDescriptor): TGocciaFFITypeDescriptor;
    procedure AddReference;
    procedure ReleaseReference;
    function IsAggregate: Boolean; inline;
    function ContainsUnion: Boolean;
    function FieldIndex(const AName: string): Integer;
    function FieldCount: Integer; inline;
    function FieldAt(const AIndex: Integer): TGocciaFFIFieldDescriptor;
    function CallbackArgumentCount: Integer; inline;
    function CallbackArgumentAt(
      const AIndex: Integer): TGocciaFFITypeDescriptor;
    property Kind: TGocciaFFITypeKind read FKind;
    property ScalarType: TGocciaFFIType read FScalarType;
    property ElementType: TGocciaFFITypeDescriptor read FElementType;
    property ElementCount: Integer read FElementCount;
    property CallbackReturn: TGocciaFFITypeDescriptor read FCallbackReturn;
    property Size: Integer read FSize;
    property Alignment: Integer read FAlignment;
    property Depth: Integer read FDepth;
  end;

  TGocciaFFIArgClass = (facInteger, facSingle, facDouble, facMixed);
  TGocciaFFIReturnClass = (frcVoid, frcInteger, frcSingle, frcDouble);

  TGocciaFFISlot = record
    case Integer of
      0: (AsInt: PtrInt);
      1: (AsSingle: Single);
      2: (AsDouble: Double);
  end;

  TGocciaFFISignature = record
    ArgTypes: array of TGocciaFFIType;
    ReturnType: TGocciaFFIType;
    ArgCount: Integer;
    ArgClass: TGocciaFFIArgClass;
    ArgBitmask: Integer;
    ReturnClass: TGocciaFFIReturnClass;
  end;

  TGocciaFFIResult = record
    case Integer of
      0: (AsInt: PtrInt);
      1: (AsSingle: Single);
      2: (AsDouble: Double);
  end;

  // Call state record for the assembly trampolines (64-bit only).
  // Pascal fills the arrays; the asm stub loads them into registers and calls.
  // Layout must have deterministic offsets — see constants below.
  {$PUSH}{$PACKRECORDS 8}
  TGocciaFFICallState = record
    FuncPtr:  CodePointer;              // 64-bit offset   0 | 32-bit offset   0
    GprCount: Integer;                  // 64-bit offset   8 | 32-bit offset   4
    FprCount: Integer;                  // 64-bit offset  12 | 32-bit offset   8
    Gpr:      array[0..7] of PtrInt;    // 64-bit offset  16 | 32-bit offset  12
    Fpr:      array[0..7] of Double;    // 64-bit offset  80 | 32-bit offset  48
    RetInt:   PtrInt;                   // 64-bit offset 144 | 32-bit offset 112
    RetFloat: Double;                   // 64-bit offset 152 | 32-bit offset 120
    // i386 trampoline: pre-packed stack args (max 8 args x 8 bytes + padding)
    StackBuf:      array[0..79] of Byte; // 32-bit offset 128
    StackSize:     Integer;              // 32-bit offset 208
    ReturnIsFloat: Boolean;              // 32-bit offset 212
  end;
  {$POP}

const
  MAX_FFI_ARGS = 8;
  MAX_FFI_FIELDS = 256;
  MAX_FFI_DESCRIPTOR_DEPTH = 32;
  MAX_FFI_AGGREGATE_SIZE = 65536;

  FFI_TYPE_VOID    = 'void';
  FFI_TYPE_BOOL    = 'bool';
  FFI_TYPE_I8      = 'i8';
  FFI_TYPE_I16     = 'i16';
  FFI_TYPE_I32     = 'i32';
  FFI_TYPE_I64     = 'i64';
  FFI_TYPE_U8      = 'u8';
  FFI_TYPE_U16     = 'u16';
  FFI_TYPE_U32     = 'u32';
  FFI_TYPE_U64     = 'u64';
  FFI_TYPE_F32     = 'f32';
  FFI_TYPE_F64     = 'f64';
  FFI_TYPE_POINTER = 'pointer';
  FFI_TYPE_CSTRING = 'cstring';

function ParseFFIType(const AName: string): TGocciaFFIType;
function FFITypeName(const AType: TGocciaFFIType): string;
function AlignFFIOffset(const AOffset, AAlignment: Integer): Integer;
function FFITypeToArgClass(const AType: TGocciaFFIType): TGocciaFFIArgClass;
function FFITypeToReturnClass(const AType: TGocciaFFIType): TGocciaFFIReturnClass;
function ValidateSignature(var ASignature: TGocciaFFISignature): string;

implementation

function ScalarTypeSize(const AType: TGocciaFFIType): Integer;
begin
  case AType of
    fftVoid:
      Result := 0;
    fftBool, fftI8, fftU8:
      Result := 1;
    fftI16, fftU16:
      Result := 2;
    fftI32, fftU32, fftF32:
      Result := 4;
    fftI64, fftU64, fftF64:
      Result := 8;
    fftPointer, fftCString:
      Result := SizeOf(Pointer);
  else
    Result := 0;
  end;
end;

function ScalarTypeAlignment(const AType: TGocciaFFIType): Integer;
begin
  Result := ScalarTypeSize(AType);
  if Result = 0 then
    Result := 1;
end;

function AlignFFIOffset(const AOffset, AAlignment: Integer): Integer;
begin
  if AAlignment <= 1 then
    Exit(AOffset);
  if AOffset > High(Integer) - AAlignment + 1 then
    raise EArgumentOutOfRangeException.Create('FFI layout size overflow');
  Result := ((AOffset + AAlignment - 1) div AAlignment) * AAlignment;
end;

constructor TGocciaFFITypeDescriptor.Create;
begin
  inherited;
  FReferenceCount := 1;
  FAlignment := 1;
  FDepth := 1;
end;

destructor TGocciaFFITypeDescriptor.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FFields) do
    if Assigned(FFields[I].TypeDescriptor) then
      FFields[I].TypeDescriptor.ReleaseReference;
  if Assigned(FElementType) then
    FElementType.ReleaseReference;
  for I := 0 to High(FCallbackArguments) do
    if Assigned(FCallbackArguments[I]) then
      FCallbackArguments[I].ReleaseReference;
  if Assigned(FCallbackReturn) then
    FCallbackReturn.ReleaseReference;
  inherited;
end;

class function TGocciaFFITypeDescriptor.CreateScalar(
  const AScalarType: TGocciaFFIType): TGocciaFFITypeDescriptor;
begin
  Result := TGocciaFFITypeDescriptor.Create;
  Result.FKind := ftkScalar;
  Result.FScalarType := AScalarType;
  Result.FSize := ScalarTypeSize(AScalarType);
  Result.FAlignment := ScalarTypeAlignment(AScalarType);
end;

class function TGocciaFFITypeDescriptor.CreateStruct(
  const AFields: array of TGocciaFFIFieldDescriptor): TGocciaFFITypeDescriptor;
var
  I, Offset, ChildDepth: Integer;
begin
  if Length(AFields) = 0 then
    raise EArgumentException.Create('FFI struct must contain at least one field');
  if Length(AFields) > MAX_FFI_FIELDS then
    raise EArgumentException.Create('FFI struct has too many fields');
  Result := TGocciaFFITypeDescriptor.Create;
  try
    Result.FKind := ftkStruct;
    SetLength(Result.FFields, Length(AFields));
    Offset := 0;
    for I := 0 to High(AFields) do
    begin
      if not Assigned(AFields[I].TypeDescriptor) then
        raise EArgumentException.Create('FFI struct field type is missing');
      if (AFields[I].TypeDescriptor.Kind = ftkScalar) and
         (AFields[I].TypeDescriptor.ScalarType = fftVoid) then
        raise EArgumentException.Create('FFI struct field cannot be void');
      Result.FFields[I] := AFields[I];
      AFields[I].TypeDescriptor.AddReference;
      if AFields[I].TypeDescriptor.Alignment > Result.FAlignment then
        Result.FAlignment := AFields[I].TypeDescriptor.Alignment;
      Offset := AlignFFIOffset(Offset, AFields[I].TypeDescriptor.Alignment);
      if AFields[I].TypeDescriptor.Size > MAX_FFI_AGGREGATE_SIZE - Offset then
        raise EArgumentOutOfRangeException.Create('FFI struct size exceeds limit');
      Result.FFields[I].Offset := Offset;
      Inc(Offset, AFields[I].TypeDescriptor.Size);
      ChildDepth := AFields[I].TypeDescriptor.Depth + 1;
      if ChildDepth > Result.FDepth then
        Result.FDepth := ChildDepth;
    end;
    Result.FSize := AlignFFIOffset(Offset, Result.FAlignment);
    if Result.FSize > MAX_FFI_AGGREGATE_SIZE then
      raise EArgumentOutOfRangeException.Create('FFI struct size exceeds limit');
    if Result.FDepth > MAX_FFI_DESCRIPTOR_DEPTH then
      raise EArgumentOutOfRangeException.Create('FFI descriptor nesting exceeds limit');
  except
    Result.Free;
    raise;
  end;
end;

class function TGocciaFFITypeDescriptor.CreateUnion(
  const AFields: array of TGocciaFFIFieldDescriptor): TGocciaFFITypeDescriptor;
var
  I, ChildDepth: Integer;
begin
  if Length(AFields) = 0 then
    raise EArgumentException.Create('FFI union must contain at least one field');
  if Length(AFields) > MAX_FFI_FIELDS then
    raise EArgumentException.Create('FFI union has too many fields');
  Result := TGocciaFFITypeDescriptor.Create;
  try
    Result.FKind := ftkUnion;
    SetLength(Result.FFields, Length(AFields));
    for I := 0 to High(AFields) do
    begin
      if not Assigned(AFields[I].TypeDescriptor) then
        raise EArgumentException.Create('FFI union field type is missing');
      if (AFields[I].TypeDescriptor.Kind = ftkScalar) and
         (AFields[I].TypeDescriptor.ScalarType = fftVoid) then
        raise EArgumentException.Create('FFI union field cannot be void');
      Result.FFields[I] := AFields[I];
      Result.FFields[I].Offset := 0;
      AFields[I].TypeDescriptor.AddReference;
      if AFields[I].TypeDescriptor.Alignment > Result.FAlignment then
        Result.FAlignment := AFields[I].TypeDescriptor.Alignment;
      if AFields[I].TypeDescriptor.Size > Result.FSize then
        Result.FSize := AFields[I].TypeDescriptor.Size;
      ChildDepth := AFields[I].TypeDescriptor.Depth + 1;
      if ChildDepth > Result.FDepth then
        Result.FDepth := ChildDepth;
    end;
    Result.FSize := AlignFFIOffset(Result.FSize, Result.FAlignment);
    if Result.FSize > MAX_FFI_AGGREGATE_SIZE then
      raise EArgumentOutOfRangeException.Create('FFI union size exceeds limit');
    if Result.FDepth > MAX_FFI_DESCRIPTOR_DEPTH then
      raise EArgumentOutOfRangeException.Create('FFI descriptor nesting exceeds limit');
  except
    Result.Free;
    raise;
  end;
end;

class function TGocciaFFITypeDescriptor.CreateArray(
  const AElementType: TGocciaFFITypeDescriptor;
  const AElementCount: Integer): TGocciaFFITypeDescriptor;
begin
  if not Assigned(AElementType) then
    raise EArgumentException.Create('FFI array element type is missing');
  if (AElementType.Kind = ftkScalar) and
     (AElementType.ScalarType = fftVoid) then
    raise EArgumentException.Create('FFI array element cannot be void');
  if AElementCount <= 0 then
    raise EArgumentOutOfRangeException.Create('FFI array length must be positive');
  if (AElementType.Size <> 0) and
     (AElementCount > MAX_FFI_AGGREGATE_SIZE div AElementType.Size) then
    raise EArgumentOutOfRangeException.Create('FFI array size exceeds limit');
  Result := TGocciaFFITypeDescriptor.Create;
  try
    Result.FKind := ftkArray;
    Result.FElementType := AElementType;
    Result.FElementType.AddReference;
    Result.FElementCount := AElementCount;
    Result.FAlignment := AElementType.Alignment;
    Result.FSize := AElementType.Size * AElementCount;
    Result.FDepth := AElementType.Depth + 1;
    if Result.FDepth > MAX_FFI_DESCRIPTOR_DEPTH then
      raise EArgumentOutOfRangeException.Create('FFI descriptor nesting exceeds limit');
  except
    Result.Free;
    raise;
  end;
end;

class function TGocciaFFITypeDescriptor.CreateCallback(
  const AArguments: array of TGocciaFFITypeDescriptor;
  const AReturnType: TGocciaFFITypeDescriptor): TGocciaFFITypeDescriptor;
var
  I: Integer;
begin
  if not Assigned(AReturnType) then
    raise EArgumentException.Create('FFI callback return type is missing');
  if Length(AArguments) > MAX_FFI_ARGS then
    raise EArgumentException.Create('FFI callback has too many arguments');
  Result := TGocciaFFITypeDescriptor.Create;
  try
    Result.FKind := ftkCallback;
    Result.FSize := SizeOf(Pointer);
    Result.FAlignment := SizeOf(Pointer);
    SetLength(Result.FCallbackArguments, Length(AArguments));
    for I := 0 to High(AArguments) do
    begin
      if not Assigned(AArguments[I]) then
        raise EArgumentException.Create('FFI callback argument type is missing');
      if (AArguments[I].Kind = ftkScalar) and
         (AArguments[I].ScalarType = fftVoid) then
        raise EArgumentException.Create('FFI callback argument cannot be void');
      Result.FCallbackArguments[I] := AArguments[I];
      AArguments[I].AddReference;
      if AArguments[I].Depth + 1 > Result.FDepth then
        Result.FDepth := AArguments[I].Depth + 1;
    end;
    Result.FCallbackReturn := AReturnType;
    Result.FCallbackReturn.AddReference;
    if AReturnType.Depth + 1 > Result.FDepth then
      Result.FDepth := AReturnType.Depth + 1;
    if Result.FDepth > MAX_FFI_DESCRIPTOR_DEPTH then
      raise EArgumentOutOfRangeException.Create('FFI descriptor nesting exceeds limit');
  except
    Result.Free;
    raise;
  end;
end;

procedure TGocciaFFITypeDescriptor.AddReference;
begin
  Inc(FReferenceCount);
end;

procedure TGocciaFFITypeDescriptor.ReleaseReference;
begin
  Dec(FReferenceCount);
  if FReferenceCount = 0 then
    Free;
end;

function TGocciaFFITypeDescriptor.IsAggregate: Boolean;
begin
  Result := FKind in [ftkStruct, ftkUnion, ftkArray];
end;

function TGocciaFFITypeDescriptor.ContainsUnion: Boolean;
var
  I: Integer;
begin
  if FKind = ftkUnion then
    Exit(True);
  if FKind = ftkArray then
    Exit(FElementType.ContainsUnion);
  if FKind = ftkStruct then
    for I := 0 to High(FFields) do
      if FFields[I].TypeDescriptor.ContainsUnion then
        Exit(True);
  Result := False;
end;

function TGocciaFFITypeDescriptor.FieldIndex(const AName: string): Integer;
begin
  for Result := 0 to High(FFields) do
    if FFields[Result].Name = AName then
      Exit;
  Result := -1;
end;

function TGocciaFFITypeDescriptor.FieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TGocciaFFITypeDescriptor.FieldAt(
  const AIndex: Integer): TGocciaFFIFieldDescriptor;
begin
  Result := FFields[AIndex];
end;

function TGocciaFFITypeDescriptor.CallbackArgumentCount: Integer;
begin
  Result := Length(FCallbackArguments);
end;

function TGocciaFFITypeDescriptor.CallbackArgumentAt(
  const AIndex: Integer): TGocciaFFITypeDescriptor;
begin
  Result := FCallbackArguments[AIndex];
end;

function ParseFFIType(const AName: string): TGocciaFFIType;
begin
  if AName = FFI_TYPE_VOID then Result := fftVoid
  else if AName = FFI_TYPE_BOOL then Result := fftBool
  else if AName = FFI_TYPE_I8 then Result := fftI8
  else if AName = FFI_TYPE_I16 then Result := fftI16
  else if AName = FFI_TYPE_I32 then Result := fftI32
  else if AName = FFI_TYPE_I64 then Result := fftI64
  else if AName = FFI_TYPE_U8 then Result := fftU8
  else if AName = FFI_TYPE_U16 then Result := fftU16
  else if AName = FFI_TYPE_U32 then Result := fftU32
  else if AName = FFI_TYPE_U64 then Result := fftU64
  else if AName = FFI_TYPE_F32 then Result := fftF32
  else if AName = FFI_TYPE_F64 then Result := fftF64
  else if AName = FFI_TYPE_POINTER then Result := fftPointer
  else if AName = FFI_TYPE_CSTRING then Result := fftCString
  else
    Result := fftVoid;
end;

function FFITypeName(const AType: TGocciaFFIType): string;
begin
  case AType of
    fftVoid: Result := FFI_TYPE_VOID;
    fftBool: Result := FFI_TYPE_BOOL;
    fftI8: Result := FFI_TYPE_I8;
    fftI16: Result := FFI_TYPE_I16;
    fftI32: Result := FFI_TYPE_I32;
    fftI64: Result := FFI_TYPE_I64;
    fftU8: Result := FFI_TYPE_U8;
    fftU16: Result := FFI_TYPE_U16;
    fftU32: Result := FFI_TYPE_U32;
    fftU64: Result := FFI_TYPE_U64;
    fftF32: Result := FFI_TYPE_F32;
    fftF64: Result := FFI_TYPE_F64;
    fftPointer: Result := FFI_TYPE_POINTER;
    fftCString: Result := FFI_TYPE_CSTRING;
  else
    Result := FFI_TYPE_VOID;
  end;
end;

function FFITypeToArgClass(const AType: TGocciaFFIType): TGocciaFFIArgClass;
begin
  case AType of
    fftF32: Result := facSingle;
    fftF64: Result := facDouble;
  else
    Result := facInteger;
  end;
end;

function FFITypeToReturnClass(const AType: TGocciaFFIType): TGocciaFFIReturnClass;
begin
  case AType of
    fftVoid: Result := frcVoid;
    fftF32: Result := frcSingle;
    fftF64: Result := frcDouble;
  else
    Result := frcInteger;
  end;
end;

function ValidateSignature(var ASignature: TGocciaFFISignature): string;
var
  I: Integer;
  HasInteger, HasSingle, HasDouble: Boolean;
  Bitmask: Integer;
begin
  Result := '';
  ASignature.ArgBitmask := 0;

  if ASignature.ArgCount > MAX_FFI_ARGS then
  begin
    Result := 'FFI supports a maximum of ' + IntToStr(MAX_FFI_ARGS) + ' arguments';
    Exit;
  end;

  {$IFNDEF CPU64}
  for I := 0 to ASignature.ArgCount - 1 do
    if ASignature.ArgTypes[I] in [fftI64, fftU64] then
    begin
      Result := 'i64/u64 argument types are not supported on 32-bit platforms';
      Exit;
    end;
  if ASignature.ReturnType in [fftI64, fftU64] then
  begin
    Result := 'i64/u64 return types are not supported on 32-bit platforms';
    Exit;
  end;
  {$ENDIF}

  ASignature.ReturnClass := FFITypeToReturnClass(ASignature.ReturnType);

  if ASignature.ArgCount = 0 then
  begin
    ASignature.ArgClass := facInteger;
    Exit;
  end;

  HasInteger := False;
  HasSingle := False;
  HasDouble := False;
  for I := 0 to ASignature.ArgCount - 1 do
    case FFITypeToArgClass(ASignature.ArgTypes[I]) of
      facInteger: HasInteger := True;
      facSingle:  HasSingle := True;
      facDouble:  HasDouble := True;
    end;

  if HasInteger and not HasSingle and not HasDouble then
  begin
    ASignature.ArgClass := facInteger;
    Exit;
  end;
  if HasSingle and not HasInteger and not HasDouble then
  begin
    ASignature.ArgClass := facSingle;
    Exit;
  end;
  if HasDouble and not HasInteger and not HasSingle then
  begin
    ASignature.ArgClass := facDouble;
    Exit;
  end;

  if HasSingle then
  begin
    Result := 'f32 arguments cannot be mixed with other types. Use f64 instead, or keep all arguments f32';
    Exit;
  end;

  // Mixed integer + double — supported via assembly trampolines on all platforms
  Bitmask := 0;
  for I := 0 to ASignature.ArgCount - 1 do
    if FFITypeToArgClass(ASignature.ArgTypes[I]) = facDouble then
      Bitmask := Bitmask or (1 shl I);

  ASignature.ArgClass := facMixed;
  ASignature.ArgBitmask := Bitmask;
end;

end.
