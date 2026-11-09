unit G2.BigIntV;
{ BigInt values: GC cells holding sign and 32-bit limbs; arithmetic through Goccia's TBigInteger. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  SysUtils, Math, BigInteger, G2.Core;

const
  ckBigInt = 9;

type
  PBigCell = ^TBigCell;
  TBigCell = packed record
    Hdr: TCellHeader;
    Count: Int32;
    Negative: Int32;
    Limbs: array[0..0] of Cardinal;
  end;

function IsBigInt(V: TValue): Boolean; inline;
function BigToValue(const B: TBigInteger; Permanent: Boolean = False): TValue;
function ValueToBig(V: TValue): TBigInteger;
function BigToString(V: TValue; Radix: Integer = 10): UnicodeString;
function BigCompare(A, B: TValue): Integer;
function BigEquals(A, B: TValue): Boolean;
function BigIsZero(V: TValue): Boolean;

implementation

function IsBigInt(V: TValue): Boolean; inline;
begin
  Result := ((V and TAG_MASK) = TAG_PTR) and (PCell(PtrUInt(V and PTR_MASK))^.Kind = ckBigInt);
end;

function BigToValue(const B: TBigInteger; Permanent: Boolean): TValue;
var C: PBigCell; N, I: Integer; Size: SizeInt;
begin
  N := B.LimbCount;
  Size := SizeOf(TBigCell) - SizeOf(Cardinal) + Max(N, 1) * SizeOf(Cardinal);
  if Permanent then
  begin
    Size := (Size + 7) and not 7;
    C := GetMem(Size);
    C^.Hdr.Size := Size;
    C^.Hdr.Kind := ckBigInt;
    C^.Hdr.Flags := cfPermanent or cfOld;
    C^.Hdr.Extra := 0;
  end
  else
    C := PBigCell(AllocCell(Size, ckBigInt));
  C^.Count := N;
  C^.Negative := Ord(B.IsNegative);
  for I := 0 to N - 1 do C^.Limbs[I] := B.LimbAt(I);
  Result := MkPtr(C);
end;

function ValueToBig(V: TValue): TBigInteger;
var C: PBigCell; L: TLimbArray; I: Integer;
begin
  C := PBigCell(PtrOf(V));
  SetLength(L, C^.Count);
  for I := 0 to C^.Count - 1 do L[I] := C^.Limbs[I];
  Result := TBigInteger.FromLimbs(L, C^.Negative <> 0);
end;

function BigToString(V: TValue; Radix: Integer): UnicodeString;
begin
  if Radix = 10 then Result := ValueToBig(V).ToString else Result := ValueToBig(V).ToRadixString(Radix);
end;

function BigCompare(A, B: TValue): Integer;
begin
  Result := ValueToBig(A).Compare(ValueToBig(B));
end;

function BigEquals(A, B: TValue): Boolean;
begin
  Result := BigCompare(A, B) = 0;
end;

function BigIsZero(V: TValue): Boolean;
begin
  Result := PBigCell(PtrOf(V))^.Count = 0;
end;

end.
