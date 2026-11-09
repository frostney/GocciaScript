unit G2.Typed;
{ ArrayBuffer, typed arrays and DataView storage.

  A buffer (okArrayBuffer): Aux1 = the bytes cell (its Len is the capacity: maxByteLength for a resizable one),
  Aux2 = the current byte length; OFlags bfShared (a SharedArrayBuffer: Native = its G2.Shared block),
  bfResizable (resizable / growable), bfDetached.
  A typed array (okTypedArray): Aux1 = its buffer, Aux2 = its fixed length or -1 (length-tracking), Pad = its byte
  offset, OFlags = kind shl 4. TALength is the spec's TypedArrayLength of a fresh buffer witness record: 0 when the
  view is out of bounds or the buffer detached; on a fixed-length, attached, unshared buffer it is Aux2 directly. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$INLINE ON}
{$Q-}{$R-}

interface

uses
  SysUtils, Math, BigInteger, G2.Core, G2.Shared, G2.BigIntV;

const
  ckBytes = 10;
  okTypedArray = 14;
  okArrayBuffer = 15;
  okDataView = 16;

  bfImmutable = $10;   { an immutable ArrayBuffer (the Immutable ArrayBuffer proposal): its bytes never change }
  bfShared = $20;
  bfResizable = $40;
  bfDetached = $80;

  taInt8 = 0; taUint8 = 1; taUint8C = 2; taInt16 = 3; taUint16 = 4; taInt32 = 5; taUint32 = 6;
  taFloat32 = 7; taFloat64 = 8; taFloat16 = 9; taBigInt64 = 10; taBigUint64 = 11;
  TAKindCount = 12;
  TAElemSize: array[0..TAKindCount - 1] of Integer = (1, 1, 1, 2, 2, 4, 4, 4, 8, 2, 8, 8);
  TANames: array[0..TAKindCount - 1] of UnicodeString = ('Int8Array', 'Uint8Array', 'Uint8ClampedArray',
    'Int16Array', 'Uint16Array', 'Int32Array', 'Uint32Array', 'Float32Array', 'Float64Array', 'Float16Array',
    'BigInt64Array', 'BigUint64Array');

type
  PBytes = ^TBytesCell;
  TBytesCell = packed record
    Hdr: TCellHeader;
    Len: Int64;
    Data: array[0..0] of Byte;
  end;

function NewArrayBuffer(Len: Int64; Proto: TValue): PObj;
{ a resizable buffer: MaxLen bytes reserved, Len in use }
function NewResizableBuffer(Len, MaxLen: Int64; Proto: TValue): PObj;
function BufferData(Buf: PObj): PByte; inline;
{ the current byte length (0 once detached) }
function BufferLen(Buf: PObj): Int64; inline;
function BufferMaxLen(Buf: PObj): Int64;
function BufferDetached(Buf: PObj): Boolean; inline;
{ 25.1.3.5 DetachArrayBuffer: the bytes go, every view sees length 0 }
procedure DetachBuffer(Buf: PObj);
{ IsImmutableBuffer of a typed array's or a DataView's buffer }
function ViewImmutable(V: PObj): Boolean; inline;
function TAKind(O: PObj): Integer; inline;
function TAIsBig(O: PObj): Boolean; inline;
function TALength(O: PObj): Integer; inline;
function TALengthSlow(O: PObj): Integer;
{ 10.4.5.12 IsTypedArrayOutOfBounds (a detached buffer counts as out of bounds) }
function TAOutOfBounds(O: PObj): Boolean;
function TAData(O: PObj): PByte; inline;
function TAGet(O: PObj; Idx: Integer): TValue; inline;
{ number kinds (a BigInt kind ignores these: its stores go through TASetBig) }
procedure TASet(O: PObj; Idx: Integer; D: Double); inline;
procedure TASetInt(O: PObj; Idx: Integer; I: Int32); inline;
{ BigInt64 / BigUint64: the value's low 64 bits }
procedure TASetBig(O: PObj; Idx: Integer; Q: QWord);
function NewTypedArray(Kind: Integer; Buf: TValue; ByteOffset, Len: Integer; Proto: TValue): PObj;

{ IEEE 754 binary16 (Float16Array, DataView, Math.f16round): round to nearest, ties to even }
function DoubleToHalf(D: Double): Word;
function HalfToDouble(H: Word): Double;
{ BigInt values of 64-bit integers, and a BigInt's low 64 bits (two's complement) }
function BigFromInt64(I: Int64): TValue;
function BigFromUInt64(Q: QWord): TValue;
function BigToU64(V: TValue): QWord;

implementation

function NewBuffer(Cap, Len: Int64; Proto: TValue; Flags: Byte): PObj;
var B: PBytes;
begin
  PushRoot(Proto);   { the bytes' allocation can move a young prototype }
  B := PBytes(AllocCell(SizeOf(TBytesCell) - 1 + Max(Cap, 1), ckBytes));
  B^.Len := Cap;
  FillChar(B^.Data[0], Max(Cap, 1), 0);
  PushRoot(MkPtr(B));
  Result := NewObject(RootShapeFor(RootAt(TempTop - 2)), 0, okArrayBuffer);
  Result^.Aux1 := RootAt(TempTop - 1);
  Result^.Aux2 := MkNumber(Len);
  Result^.OFlags := Flags;
  PopRoots(2);
end;

function NewArrayBuffer(Len: Int64; Proto: TValue): PObj;
begin
  Result := NewBuffer(Len, Len, Proto, 0);
end;

function NewResizableBuffer(Len, MaxLen: Int64; Proto: TValue): PObj;
begin
  Result := NewBuffer(MaxLen, Len, Proto, bfResizable);
end;

{ a SharedArrayBuffer (bfShared, G2.Atomics) keeps its bytes in a G2.Shared block outside the heap }
function BufferData(Buf: PObj): PByte; inline;
begin
  if (Buf^.OFlags and bfShared) <> 0 then Exit(PSharedBlock(Buf^.Native)^.Data);
  Result := @PBytes(PtrOf(Buf^.Aux1))^.Data[0];
end;

function BufferLen(Buf: PObj): Int64; inline;
begin
  if (Buf^.OFlags and (bfShared or bfDetached)) = 0 then Exit(Trunc(NumOf(Buf^.Aux2)));
  if (Buf^.OFlags and bfShared) <> 0 then Exit(BlockLength(PSharedBlock(Buf^.Native)));
  Result := 0;
end;

function BufferMaxLen(Buf: PObj): Int64;
begin
  if (Buf^.OFlags and bfShared) <> 0 then
  begin
    if PSharedBlock(Buf^.Native)^.MaxByteLength < 0 then Exit(BlockLength(PSharedBlock(Buf^.Native)));
    Exit(PSharedBlock(Buf^.Native)^.MaxByteLength);
  end;
  if (Buf^.OFlags and bfDetached) <> 0 then Exit(0);
  if (Buf^.OFlags and bfResizable) <> 0 then Exit(PBytes(PtrOf(Buf^.Aux1))^.Len);
  Result := BufferLen(Buf);
end;

function BufferDetached(Buf: PObj): Boolean; inline;
begin
  Result := (Buf^.OFlags and bfDetached) <> 0;
end;

function ViewImmutable(V: PObj): Boolean; inline;
begin
  Result := IsObject(V^.Aux1) and ((ObjOf(V^.Aux1)^.OFlags and bfImmutable) <> 0);
end;

procedure DetachBuffer(Buf: PObj);
begin
  Buf^.OFlags := Buf^.OFlags or bfDetached;
  Buf^.Aux1 := VUndefined;
  Buf^.Aux2 := MkInt(0);
end;

function TAKind(O: PObj): Integer; inline;
begin
  Result := O^.OFlags shr 4;
end;

function TAIsBig(O: PObj): Boolean; inline;
begin
  Result := (O^.OFlags shr 4) >= taBigInt64;
end;

function TALengthSlow(O: PObj): Integer;
var B: PObj; BL, Off, L: Int64; ES: Integer;
begin
  B := ObjOf(O^.Aux1);
  if BufferDetached(B) then Exit(0);
  BL := BufferLen(B);
  Off := O^.Pad;
  ES := TAElemSize[TAKind(O)];
  L := IntOf(O^.Aux2);
  if L < 0 then
  begin
    if Off > BL then Exit(0);
    Exit((BL - Off) div ES);
  end;
  if Off + L * ES > BL then Exit(0);
  Result := L;
end;

function TALength(O: PObj): Integer; inline;
begin
  if (ObjOf(O^.Aux1)^.OFlags and (bfShared or bfResizable or bfDetached)) = 0 then Exit(Int32(UInt32(O^.Aux2)));
  Result := TALengthSlow(O);
end;

function TAOutOfBounds(O: PObj): Boolean;
var B: PObj; BL, Off, L: Int64;
begin
  B := ObjOf(O^.Aux1);
  if BufferDetached(B) then Exit(True);
  BL := BufferLen(B);
  Off := O^.Pad;
  L := IntOf(O^.Aux2);
  if L < 0 then Exit(Off > BL);
  Result := Off + L * TAElemSize[TAKind(O)] > BL;
end;

function TAData(O: PObj): PByte; inline;
begin
  Result := BufferData(ObjOf(O^.Aux1)) + O^.Pad;
end;

{ ---- binary16 ---- }

function DoubleToHalf(D: Double): Word;
var Bits, M, Q, Rem, HalfW: QWord; Sign: Word; E, HE, Shift: Integer;
begin
  Bits := PQWord(@D)^;
  Sign := Word((Bits shr 48) and $8000);
  E := (Bits shr 52) and $7FF;
  M := Bits and $FFFFFFFFFFFFF;
  if E = $7FF then
  begin
    if M <> 0 then Exit($7E00);   { NaN }
    Exit(Sign or $7C00);
  end;
  if E = 0 then Exit(Sign);       { zero or a double subnormal: far below half's range }
  M := M or (QWord(1) shl 52);
  HE := E - 1023 + 15;
  if HE >= 31 then Exit(Sign or $7C00);
  if HE >= 1 then Shift := 42 else Shift := 42 + (1 - HE);
  if Shift >= 64 then Exit(Sign);
  Q := M shr Shift;
  Rem := M and ((QWord(1) shl Shift) - 1);
  HalfW := QWord(1) shl (Shift - 1);
  if (Rem > HalfW) or ((Rem = HalfW) and ((Q and 1) = 1)) then Inc(Q);
  if HE >= 1 then
  begin
    if Q = 2048 then begin Inc(HE); Q := 1024; end;
    if HE >= 31 then Exit(Sign or $7C00);
    Result := Sign or Word(HE shl 10) or Word(Q - 1024);
  end
  else Result := Sign or Word(Q);   { a subnormal; 1024 rounds up into the smallest normal }
end;

function HalfToDouble(H: Word): Double;
var E, M: Integer;
begin
  E := (H shr 10) and $1F;
  M := H and $3FF;
  if E = 0 then Result := M * 5.9604644775390625e-8   { 2^-24 }
  else if E = 31 then
  begin
    if M <> 0 then Result := NaN else Result := Infinity;
  end
  else Result := (1024 + M) * Power(2, E - 25);
  if (H and $8000) <> 0 then Result := -Result;
end;

{ ---- 64-bit BigInts ---- }

function BigFromUInt64Sign(Mag: QWord; Neg: Boolean): TValue;
var C: PBigCell; N: Integer;
begin
  if Mag = 0 then Exit(BigToValue(TBigInteger.Zero));
  if (Mag shr 32) <> 0 then N := 2 else N := 1;
  C := PBigCell(AllocCell(SizeOf(TBigCell) - SizeOf(Cardinal) + N * SizeOf(Cardinal), ckBigInt));
  C^.Count := N;
  C^.Negative := Ord(Neg);
  C^.Limbs[0] := Cardinal(Mag);
  if N = 2 then C^.Limbs[1] := Cardinal(Mag shr 32);
  Result := MkPtr(C);
end;

function BigFromInt64(I: Int64): TValue;
begin
  if I < 0 then Result := BigFromUInt64Sign(QWord(-(I + 1)) + 1, True)
  else Result := BigFromUInt64Sign(QWord(I), False);
end;

function BigFromUInt64(Q: QWord): TValue;
begin
  Result := BigFromUInt64Sign(Q, False);
end;

function BigToU64(V: TValue): QWord;
var C: PBigCell;
begin
  C := PBigCell(PtrOf(V));
  Result := 0;
  if C^.Count >= 1 then Result := C^.Limbs[0];
  if C^.Count >= 2 then Result := Result or (QWord(C^.Limbs[1]) shl 32);
  if C^.Negative <> 0 then Result := QWord(-Int64(Result));
end;

{ ---- elements ---- }

function TAGet(O: PObj; Idx: Integer): TValue; inline;
var P: PByte;
begin
  P := TAData(O);
  case TAKind(O) of
    taInt8: Result := MkInt(PShortInt(P)[Idx]);
    taUint8, taUint8C: Result := MkInt(P[Idx]);
    taInt16: Result := MkInt(PSmallInt(P)[Idx]);
    taUint16: Result := MkInt(PWord(P)[Idx]);
    taInt32: Result := MkInt(PLongInt(P)[Idx]);
    taUint32: Result := MkNumber(PLongWord(P)[Idx]);
    taFloat32: Result := MkNumber(PSingle(P)[Idx]);
    taFloat16: Result := MkNumber(HalfToDouble(PWord(P)[Idx]));
    taBigInt64: Result := BigFromInt64(PInt64(P)[Idx]);
    taBigUint64: Result := BigFromUInt64(PQWord(P)[Idx]);
  else Result := MkNumber(PDouble(P)[Idx]);
  end;
end;

function ToInt32Wrap(D: Double): Int32; inline;
begin
  if IsNaN(D) or IsInfinite(D) then Exit(0);
  if (D >= -2147483648.0) and (D <= 2147483647.0) then Exit(Trunc(D));
  D := Int(D);
  D := D - 4294967296.0 * DFloor(D / 4294967296.0);
  Result := Int32(UInt32(Trunc(D)));
end;

{ 7.1.12 ToUint8Clamp: round half to even }
function Uint8Clamp(D: Double): Byte; inline;
begin
  if IsNaN(D) or (D <= 0) then Exit(0);
  if D >= 255 then Exit(255);
  Result := Round(D);   { FPC's Round: half to even, as ToUint8Clamp }
end;

procedure TASet(O: PObj; Idx: Integer; D: Double); inline;
var P: PByte; I: Int32;
begin
  P := TAData(O);
  case TAKind(O) of
    taFloat32: PSingle(P)[Idx] := D;
    taFloat64: PDouble(P)[Idx] := D;
    taFloat16: PWord(P)[Idx] := DoubleToHalf(D);
    taUint8C: P[Idx] := Uint8Clamp(D);
    taBigInt64, taBigUint64: ;
  else
    begin
      I := ToInt32Wrap(D);
      case TAKind(O) of
        taInt8, taUint8: P[Idx] := Byte(I);
        taInt16, taUint16: PWord(P)[Idx] := Word(I);
      else PLongWord(P)[Idx] := LongWord(I);
      end;
    end;
  end;
end;

procedure TASetInt(O: PObj; Idx: Integer; I: Int32); inline;
var P: PByte;
begin
  P := TAData(O);
  case TAKind(O) of
    taInt8, taUint8: P[Idx] := Byte(I);
    taUint8C: if I < 0 then P[Idx] := 0 else if I > 255 then P[Idx] := 255 else P[Idx] := I;
    taInt16, taUint16: PWord(P)[Idx] := Word(I);
    taInt32, taUint32: PLongWord(P)[Idx] := LongWord(I);
    taFloat32: PSingle(P)[Idx] := I;
    taFloat16: PWord(P)[Idx] := DoubleToHalf(I);
    taBigInt64, taBigUint64: ;
  else PDouble(P)[Idx] := I;
  end;
end;

procedure TASetBig(O: PObj; Idx: Integer; Q: QWord);
begin
  if TAIsBig(O) then PQWord(TAData(O))[Idx] := Q;
end;

function NewTypedArray(Kind: Integer; Buf: TValue; ByteOffset, Len: Integer; Proto: TValue): PObj;
begin
  PushRoot(Buf);
  Result := NewObject(RootShapeFor(Proto), 0, okTypedArray);
  Result^.Aux1 := RootAt(TempTop - 1);
  PopRoots(1);
  Result^.Aux2 := MkInt(Len);
  Result^.Pad := ByteOffset;
  Result^.OFlags := Kind shl 4;
end;

end.
