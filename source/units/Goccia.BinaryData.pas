unit Goccia.BinaryData;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaBinaryElementKind = (
    bekInt8, bekUint8, bekUint8Clamped,
    bekInt16, bekUint16,
    bekInt32, bekUint32,
    bekFloat16, bekFloat32, bekFloat64,
    bekBigInt64, bekBigUint64
  );

function BinaryBytesPerElement(const AKind: TGocciaBinaryElementKind): Integer;
function BinaryIsFloatElement(const AKind: TGocciaBinaryElementKind): Boolean; {$IFDEF FPC}inline;{$ENDIF}
function BinaryIsBigIntElement(const AKind: TGocciaBinaryElementKind): Boolean; {$IFDEF FPC}inline;{$ENDIF}

function ReadBinaryNumberElement(const AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const ALittleEndian: Boolean): Double;
procedure WriteBinaryNumberElement(var AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const AValue: Double;
  const ALittleEndian: Boolean);

function ReadBinaryBigIntElement(const AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const ALittleEndian: Boolean): Int64;
procedure WriteBinaryBigIntElement(var AData: TBytes; const AOffset: Integer;
  const AValue: Int64; const ALittleEndian: Boolean);

implementation

uses
  Math,

  NumberBits,

  Goccia.Float16,
  Goccia.NumberConversion;

function UInt64BitsToInt64(const AValue: UInt64): Int64; {$IFDEF FPC}inline;{$ENDIF}
begin
  Move(AValue, Result, SizeOf(Result));
end;

function Int64ToUInt64Bits(const AValue: Int64): UInt64; {$IFDEF FPC}inline;{$ENDIF}
begin
  Move(AValue, Result, SizeOf(Result));
end;

function BinaryBytesPerElement(const AKind: TGocciaBinaryElementKind): Integer;
begin
  case AKind of
    bekInt8, bekUint8, bekUint8Clamped:
      Result := 1;
    bekInt16, bekUint16, bekFloat16:
      Result := 2;
    bekInt32, bekUint32, bekFloat32:
      Result := 4;
    bekFloat64, bekBigInt64, bekBigUint64:
      Result := 8;
  else
    Result := 1;
  end;
end;

function BinaryIsFloatElement(const AKind: TGocciaBinaryElementKind): Boolean;
begin
  Result := AKind in [bekFloat16, bekFloat32, bekFloat64];
end;

function BinaryIsBigIntElement(const AKind: TGocciaBinaryElementKind): Boolean;
begin
  Result := AKind in [bekBigInt64, bekBigUint64];
end;

function ReadUnsignedRaw(const AData: TBytes; const AOffset, ASize: Integer;
  const ALittleEndian: Boolean): UInt64;
var
  I: Integer;
begin
  Result := 0;
  if ALittleEndian then
  begin
    for I := ASize - 1 downto 0 do
      Result := (Result shl 8) or UInt64(AData[AOffset + I]);
  end
  else
  begin
    for I := 0 to ASize - 1 do
      Result := (Result shl 8) or UInt64(AData[AOffset + I]);
  end;
end;

procedure WriteUnsignedRaw(var AData: TBytes; const AOffset, ASize: Integer;
  const AValue: UInt64; const ALittleEndian: Boolean);
var
  I: Integer;
begin
  if ALittleEndian then
  begin
    for I := 0 to ASize - 1 do
      AData[AOffset + I] := Byte((AValue shr (I * 8)) and $FF);
  end
  else
  begin
    for I := 0 to ASize - 1 do
      AData[AOffset + ASize - 1 - I] := Byte((AValue shr (I * 8)) and $FF);
  end;
end;

function SignExtendRaw(const AValue: UInt64; const ABits: Integer): Int64;
var
  SignBit: UInt64;
begin
  if ABits = 64 then
    Exit(UInt64BitsToInt64(AValue));

  SignBit := UInt64(1) shl (ABits - 1);
  if (AValue and SignBit) <> 0 then
    Result := Int64(AValue) - Int64(UInt64(1) shl ABits)
  else
    Result := Int64(AValue);
end;

function IntegerBitsForKind(const AKind: TGocciaBinaryElementKind): Integer;
begin
  case AKind of
    bekInt8, bekUint8, bekUint8Clamped:
      Result := 8;
    bekInt16, bekUint16:
      Result := 16;
    bekInt32, bekUint32:
      Result := 32;
  else
    Result := 64;
  end;
end;

function ReadBinaryNumberElement(const AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const ALittleEndian: Boolean): Double;
var
  Raw: UInt64;
  RawWord: Word;
  RawLongWord: LongWord;
  RawUInt64: UInt64;
begin
  case AKind of
    bekInt8, bekInt16, bekInt32:
      Result := SignExtendRaw(
        ReadUnsignedRaw(AData, AOffset, BinaryBytesPerElement(AKind),
          ALittleEndian),
        IntegerBitsForKind(AKind));
    bekUint8, bekUint8Clamped, bekUint16, bekUint32:
    begin
      Raw := ReadUnsignedRaw(AData, AOffset, BinaryBytesPerElement(AKind),
        ALittleEndian);
      // Number element kinds are at most 32 bits. Narrow before converting to
      // Double because Delphi Win32 mis-converts UInt64 values even when their
      // high 32 bits are zero.
      Result := UInt32(Raw);
    end;
    bekFloat16:
    begin
      RawWord := Word(ReadUnsignedRaw(AData, AOffset, 2, ALittleEndian));
      Result := Float16ToDouble(RawWord);
    end;
    bekFloat32:
    begin
      RawLongWord := LongWord(ReadUnsignedRaw(AData, AOffset, 4, ALittleEndian));
      Result := BitsToSingle(RawLongWord);
    end;
    bekFloat64:
    begin
      RawUInt64 := ReadUnsignedRaw(AData, AOffset, 8, ALittleEndian);
      Result := BitsToDouble(RawUInt64);
    end;
  else
    Result := 0;
  end;
end;

procedure WriteBinaryNumberElement(var AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const AValue: Double;
  const ALittleEndian: Boolean);
var
  Float32Value: Single;
  RawWord: Word;
  RawLongWord: LongWord;
  RawUInt64: UInt64;
begin
  case AKind of
    bekUint8Clamped:
      WriteUnsignedRaw(AData, AOffset, 1, NumberToUint8Clamp(AValue),
        ALittleEndian);
    bekInt8, bekUint8, bekInt16, bekUint16, bekInt32, bekUint32:
      WriteUnsignedRaw(AData, AOffset, BinaryBytesPerElement(AKind),
        NumberToUint32(AValue), ALittleEndian);
    bekFloat16:
    begin
      RawWord := DoubleToFloat16(AValue);
      WriteUnsignedRaw(AData, AOffset, 2, RawWord, ALittleEndian);
    end;
    bekFloat32:
    begin
      Float32Value := NumberToFloat32(AValue);
      if Math.IsNan(Float32Value) then
        RawLongWord := CANONICAL_FLOAT32_NAN_BITS
      else
        RawLongWord := SingleToBits(Float32Value);
      WriteUnsignedRaw(AData, AOffset, 4, RawLongWord, ALittleEndian);
    end;
    bekFloat64:
    begin
      if Math.IsNan(AValue) then
        RawUInt64 := CANONICAL_FLOAT64_NAN_BITS
      else
        RawUInt64 := DoubleToBits(AValue);
      WriteUnsignedRaw(AData, AOffset, 8, RawUInt64, ALittleEndian);
    end;
  end;
end;

function ReadBinaryBigIntElement(const AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const ALittleEndian: Boolean): Int64;
begin
  Result := UInt64BitsToInt64(ReadUnsignedRaw(AData, AOffset,
    BinaryBytesPerElement(AKind), ALittleEndian));
end;

procedure WriteBinaryBigIntElement(var AData: TBytes; const AOffset: Integer;
  const AValue: Int64; const ALittleEndian: Boolean);
begin
  WriteUnsignedRaw(AData, AOffset, 8, Int64ToUInt64Bits(AValue), ALittleEndian);
end;

end.
