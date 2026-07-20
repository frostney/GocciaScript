unit Goccia.NumberConversion;

{$I Goccia.inc}

interface

function NumberToUint32(const AValue: Double): UInt32;
function NumberToInt32(const AValue: Double): Int32;
function NumberToUint16(const AValue: Double): UInt16;
function NumberToUint8Clamp(const AValue: Double): UInt8;
function NumberToFloat32(const AValue: Double): Single;
function SignedRightShiftInt32(const AValue: Int32;
  const ACount: UInt32): Int32;

implementation

uses
  Math,

  Goccia.NumberRemainder;

const
  UINT32_MODULUS = 4294967296.0;
  INT32_SIGN_BOUNDARY = 2147483648.0;
  MAXIMUM_UINT8 = 255;

// ES2026 §7.1.8 ToUint32(argument)
function NumberToUint32(const AValue: Double): UInt32;
var
  IntegerValue, WrappedValue: Double;
begin
  // ES2026 §7.1.8 step 2: non-finite values and either zero become +0.
  if IsNan(AValue) or IsInfinite(AValue) or (AValue = 0.0) then
    Exit(0);

  // ES2026 §7.1.8 steps 3-4: truncate, then reduce modulo 2^32.
  IntegerValue := Int(AValue);
  WrappedValue := NumberRemainder(IntegerValue, UINT32_MODULUS);
  if WrappedValue < 0.0 then
    WrappedValue := WrappedValue + UINT32_MODULUS;
  Result := UInt32(Trunc(WrappedValue));
end;

// ES2026 §7.1.7 ToInt32(argument)
function NumberToInt32(const AValue: Double): Int32;
var
  UnsignedValue: UInt32;
  SignedValue: Int64;
begin
  UnsignedValue := NumberToUint32(AValue);
  SignedValue := Int64(UnsignedValue);
  if SignedValue >= Trunc(INT32_SIGN_BOUNDARY) then
    SignedValue := SignedValue - Trunc(UINT32_MODULUS);
  Result := Int32(SignedValue);
end;

// ES2026 §7.1.10 ToUint16(argument)
function NumberToUint16(const AValue: Double): UInt16;
begin
  Result := UInt16(NumberToUint32(AValue) and UInt32($FFFF));
end;

// ES2026 §7.1.13 ToUint8Clamp(argument)
function NumberToUint8Clamp(const AValue: Double): UInt8;
var
  FloorValue, HalfwayValue: Double;
  IntegerFloor: Integer;
begin
  // ES2026 §7.1.13 steps 2-3: clamp NaN and the lower/upper domains.
  if IsNan(AValue) or (AValue <= 0.0) then
    Exit(0);
  if AValue >= MAXIMUM_UINT8 then
    Exit(MAXIMUM_UINT8);

  // ES2026 §7.1.13 steps 4-8: nearest integer, ties to even.
  FloorValue := Floor(AValue);
  IntegerFloor := Trunc(FloorValue);
  HalfwayValue := FloorValue + 0.5;
  if AValue < HalfwayValue then
    Exit(UInt8(IntegerFloor));
  if AValue > HalfwayValue then
    Exit(UInt8(IntegerFloor + 1));
  if (IntegerFloor and 1) <> 0 then
    Inc(IntegerFloor);
  Result := UInt8(IntegerFloor);
end;

// ES2026 §21.3.2.17 Math.fround(x), step 3
function NumberToFloat32(const AValue: Double): Single;
begin
  Result := AValue;
end;

function SignedRightShiftInt32(const AValue: Int32;
  const ACount: UInt32): Int32;
var
  Bits: UInt32;
  ShiftCount: UInt32;
begin
  ShiftCount := ACount and 31;
  if ShiftCount = 0 then
    Exit(AValue);

  Bits := UInt32(AValue);
  if AValue >= 0 then
    Result := Int32(Bits shr ShiftCount)
  else
    Result := Int32((Bits shr ShiftCount) or
      (High(UInt32) shl (32 - ShiftCount)));
end;

end.
