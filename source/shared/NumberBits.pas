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

implementation

function DoubleToBits(const AValue: Double): UInt64;
begin
  Move(AValue, Result, SizeOf(Result));
end;

function BitsToDouble(const ABits: UInt64): Double;
begin
  Move(ABits, Result, SizeOf(Result));
end;

function SingleToBits(const AValue: Single): UInt32;
begin
  Move(AValue, Result, SizeOf(Result));
end;

function BitsToSingle(const ABits: UInt32): Single;
begin
  Move(ABits, Result, SizeOf(Result));
end;

function IsNegativeZero(const AValue: Double): Boolean;
begin
  Result := DoubleToBits(AValue) = (UInt64(1) shl 63);
end;

end.
