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

  // Argument class for homogeneous signatures (all args same ABI class).
  // facMixed indicates a per-position int/double mix (bitmask dispatch).
  TGocciaFFIArgClass = (facInteger, facSingle, facDouble, facMixed);
  TGocciaFFIReturnClass = (frcVoid, frcInteger, frcSingle, frcDouble);

  // Per-argument slot — carries the native value in the matching union field.
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
    // Per-position bitmask for facMixed: bit I = 1 means arg I is Double,
    // bit I = 0 means arg I is PtrInt. Only valid when ArgClass = facMixed.
    ArgBitmask: Integer;
    ReturnClass: TGocciaFFIReturnClass;
  end;

  TGocciaFFIResult = record
    case Integer of
      0: (AsInt: PtrInt);
      1: (AsSingle: Single);
      2: (AsDouble: Double);
  end;

const
  MAX_FFI_ARGS = 8;
  MAX_FFI_MIXED_ARGS = 4;

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
function FFITypeToArgClass(const AType: TGocciaFFIType): TGocciaFFIArgClass;
function FFITypeToReturnClass(const AType: TGocciaFFIType): TGocciaFFIReturnClass;
function ValidateSignature(var ASignature: TGocciaFFISignature): string;

implementation

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
    Result := fftVoid; // Caller must check for unknown types
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

  // Classify what arg types are present
  HasInteger := False;
  HasSingle := False;
  HasDouble := False;
  for I := 0 to ASignature.ArgCount - 1 do
    case FFITypeToArgClass(ASignature.ArgTypes[I]) of
      facInteger: HasInteger := True;
      facSingle:  HasSingle := True;
      facDouble:  HasDouble := True;
    end;

  // All-homogeneous cases
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

  // Mixed: only integer + double is supported (f32 cannot mix)
  if HasSingle then
  begin
    Result := 'f32 arguments cannot be mixed with other types. Use f64 instead, or keep all arguments f32';
    Exit;
  end;

  // Mixed integer + double
  if ASignature.ArgCount > MAX_FFI_MIXED_ARGS then
  begin
    Result := 'Mixed integer/float signatures support a maximum of ' +
      IntToStr(MAX_FFI_MIXED_ARGS) + ' arguments';
    Exit;
  end;

  // Build per-position bitmask: bit I = 1 means Double
  Bitmask := 0;
  for I := 0 to ASignature.ArgCount - 1 do
    if FFITypeToArgClass(ASignature.ArgTypes[I]) = facDouble then
      Bitmask := Bitmask or (1 shl I);

  ASignature.ArgClass := facMixed;
  ASignature.ArgBitmask := Bitmask;
end;

end.
