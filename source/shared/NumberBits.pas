unit NumberBits;

{$I Shared.inc}

interface

const
  CANONICAL_FLOAT16_NAN_BITS: UInt16 = $7E00;
  CANONICAL_FLOAT32_NAN_BITS: UInt32 = $7FC00000;
  CANONICAL_FLOAT64_NAN_BITS: UInt64 = $7FF8000000000000;

function DoubleToBits(const AValue: Double): UInt64; {$IFDEF FPC}inline;{$ENDIF}
function BitsToDouble(const ABits: UInt64): Double; {$IFDEF FPC}inline;{$ENDIF}
function SingleToBits(const AValue: Single): UInt32; {$IFDEF FPC}inline;{$ENDIF}
function BitsToSingle(const ABits: UInt32): Single; {$IFDEF FPC}inline;{$ENDIF}
function IsNegativeZero(const AValue: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
// Int64 -> Double widening that avoids FPC Delphi-mode bit-pattern casts and
// Single-precision expression promotion on some targets.
function Int64ToDouble(const AValue: Int64): Double; {$IFDEF FPC}inline;{$ENDIF}

implementation

function Int64ToDouble(const AValue: Int64): Double;
begin
  Result := AValue;
end;

function DoubleToBits(const AValue: Double): UInt64;
begin
  Result := PUInt64(@AValue)^;
end;

function BitsToDouble(const ABits: UInt64): Double;
begin
  Result := PDouble(@ABits)^;
end;

function SingleToBits(const AValue: Single): UInt32;
begin
  Result := PUInt32(@AValue)^;
end;

function BitsToSingle(const ABits: UInt32): Single;
begin
  Result := PSingle(@ABits)^;
end;

function IsNegativeZero(const AValue: Double): Boolean;
begin
  Result := DoubleToBits(AValue) = (UInt64(1) shl 63);
end;

end.
