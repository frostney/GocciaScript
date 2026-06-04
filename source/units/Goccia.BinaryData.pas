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
function BinaryIsFloatElement(const AKind: TGocciaBinaryElementKind): Boolean; inline;
function BinaryIsBigIntElement(const AKind: TGocciaBinaryElementKind): Boolean; inline;

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

  BigInteger,

  Goccia.Float16,
  Goccia.Values.Primitives;

const
  MAX_SAFE_DOUBLE_INTEGER = 9007199254740992.0;

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
  const ALittleEndian: Boolean): QWord;
var
  I: Integer;
begin
  Result := 0;
  if ALittleEndian then
  begin
    for I := ASize - 1 downto 0 do
      Result := (Result shl 8) or QWord(AData[AOffset + I]);
  end
  else
  begin
    for I := 0 to ASize - 1 do
      Result := (Result shl 8) or QWord(AData[AOffset + I]);
  end;
end;

procedure WriteUnsignedRaw(var AData: TBytes; const AOffset, ASize: Integer;
  const AValue: QWord; const ALittleEndian: Boolean);
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

function SignExtendRaw(const AValue: QWord; const ABits: Integer): Int64;
var
  SignBit: QWord;
begin
  if ABits = 64 then
    Exit(Int64(AValue));

  SignBit := QWord(1) shl (ABits - 1);
  if (AValue and SignBit) <> 0 then
    Result := Int64(AValue) - Int64(QWord(1) shl ABits)
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

function IntegralNumberDecimalString(const AValue: Double): string;
var
  DecimalDigits, DotPos, EPos, Exponent, I: Integer;
  Digits, ExponentText, Mantissa, NumberText: string;
  IsNegative: Boolean;
begin
  NumberText := FormatDouble(AValue);
  IsNegative := (Length(NumberText) > 0) and (NumberText[1] = '-');
  if IsNegative then
    Delete(NumberText, 1, 1);

  EPos := Pos('e', LowerCase(NumberText));
  if EPos = 0 then
  begin
    DotPos := Pos('.', NumberText);
    if DotPos > 0 then
      Result := Copy(NumberText, 1, DotPos - 1)
    else
      Result := NumberText;
  end
  else
  begin
    Mantissa := Copy(NumberText, 1, EPos - 1);
    ExponentText := Copy(NumberText, EPos + 1, Length(NumberText) - EPos);
    if (Length(ExponentText) > 0) and (ExponentText[1] = '+') then
      Delete(ExponentText, 1, 1);
    Exponent := StrToInt(ExponentText);

    DotPos := Pos('.', Mantissa);
    if DotPos > 0 then
    begin
      DecimalDigits := Length(Mantissa) - DotPos;
      Delete(Mantissa, DotPos, 1);
    end
    else
      DecimalDigits := 0;

    Digits := Mantissa;
    if Exponent >= DecimalDigits then
      Result := Digits + StringOfChar('0', Exponent - DecimalDigits)
    else
    begin
      Result := Copy(Digits, 1, Length(Digits) - (DecimalDigits - Exponent));
      if Result = '' then
        Result := '0';
    end;
  end;

  I := 1;
  while (I < Length(Result)) and (Result[I] = '0') do
    Inc(I);
  if I > 1 then
    Delete(Result, 1, I - 1);

  if IsNegative and (Result <> '0') then
    Result := '-' + Result;
end;

function IntegralNumberToBigInteger(const AValue: Double): TBigInteger;
begin
  if (AValue >= -MAX_SAFE_DOUBLE_INTEGER) and
     (AValue <= MAX_SAFE_DOUBLE_INTEGER) then
    Exit(TBigInteger.FromInt64(Trunc(AValue)));

  Result := TBigInteger.FromDecimalString(IntegralNumberDecimalString(AValue));
end;

function TruncatedIntegerBits(const AValue: Double;
  const ABits: Integer): QWord;
var
  IntegerValue: TBigInteger;
begin
  if Math.IsNan(AValue) or Math.IsInfinite(AValue) or (AValue = 0.0) then
    Exit(0);

  if not (ABits in [8, 16, 32]) then
    Exit(0);

  IntegerValue := IntegralNumberToBigInteger(AValue);
  Result := QWord(IntegerValue.AsUintN(ABits).ToInt64);
end;

function ReadBinaryNumberElement(const AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const ALittleEndian: Boolean): Double;
var
  Raw: QWord;
  RawWord: Word;
  RawLongWord: LongWord;
  RawQWord: QWord;
  Float32Value: Single;
  Float64Value: Double;
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
      Result := Raw;
    end;
    bekFloat16:
    begin
      RawWord := Word(ReadUnsignedRaw(AData, AOffset, 2, ALittleEndian));
      Result := Float16ToDouble(RawWord);
    end;
    bekFloat32:
    begin
      RawLongWord := LongWord(ReadUnsignedRaw(AData, AOffset, 4, ALittleEndian));
      Move(RawLongWord, Float32Value, 4);
      Result := Float32Value;
    end;
    bekFloat64:
    begin
      RawQWord := ReadUnsignedRaw(AData, AOffset, 8, ALittleEndian);
      Move(RawQWord, Float64Value, 8);
      Result := Float64Value;
    end;
  else
    Result := 0;
  end;
end;

procedure WriteBinaryNumberElement(var AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const AValue: Double;
  const ALittleEndian: Boolean);
var
  Clamped: Integer;
  Float32Value: Single;
  RawWord: Word;
  RawLongWord: LongWord;
  RawQWord: QWord;
  Float64Value: Double;
begin
  case AKind of
    bekUint8Clamped:
    begin
      if Math.IsNan(AValue) or (AValue <= 0) then
        Clamped := 0
      else if AValue >= 255 then
        Clamped := 255
      else
        Clamped := Round(AValue);
      WriteUnsignedRaw(AData, AOffset, 1, QWord(Clamped), ALittleEndian);
    end;
    bekInt8, bekUint8, bekInt16, bekUint16, bekInt32, bekUint32:
      WriteUnsignedRaw(AData, AOffset, BinaryBytesPerElement(AKind),
        TruncatedIntegerBits(AValue, IntegerBitsForKind(AKind)), ALittleEndian);
    bekFloat16:
    begin
      RawWord := DoubleToFloat16(AValue);
      WriteUnsignedRaw(AData, AOffset, 2, RawWord, ALittleEndian);
    end;
    bekFloat32:
    begin
      Float32Value := AValue;
      Move(Float32Value, RawLongWord, 4);
      WriteUnsignedRaw(AData, AOffset, 4, RawLongWord, ALittleEndian);
    end;
    bekFloat64:
    begin
      Float64Value := AValue;
      Move(Float64Value, RawQWord, 8);
      WriteUnsignedRaw(AData, AOffset, 8, RawQWord, ALittleEndian);
    end;
  end;
end;

function ReadBinaryBigIntElement(const AData: TBytes; const AOffset: Integer;
  const AKind: TGocciaBinaryElementKind; const ALittleEndian: Boolean): Int64;
begin
  Result := Int64(ReadUnsignedRaw(AData, AOffset, BinaryBytesPerElement(AKind),
    ALittleEndian));
end;

procedure WriteBinaryBigIntElement(var AData: TBytes; const AOffset: Integer;
  const AValue: Int64; const ALittleEndian: Boolean);
begin
  WriteUnsignedRaw(AData, AOffset, 8, QWord(AValue), ALittleEndian);
end;

end.
