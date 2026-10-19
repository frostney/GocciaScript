unit SHA256;

{$I Shared.inc}

interface

uses
  SysUtils;

type
  // An incremental SHA-256: Init, Update with the data in as many pieces as
  // it comes, then FinalHex. The same digest as hashing it in one piece.
  TSHA256Context = record
    State: array[0..7] of Cardinal;
    Buffer: array[0..63] of Byte;
    BufferLength: Integer;
    TotalLength: QWord;
  end;

procedure SHA256Init(out AContext: TSHA256Context);
procedure SHA256Update(var AContext: TSHA256Context; const AData: Pointer;
  const ALength: NativeInt);
function SHA256FinalHex(var AContext: TSHA256Context): string;

function SHA256Hex(const AData: TBytes): string;

implementation

const
  ROUND_CONSTANTS: array[0..63] of Cardinal = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
    $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3,
    $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC,
    $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7,
    $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13,
    $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3,
    $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5,
    $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208,
    $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );

function RotateRight(const AValue: Cardinal; const ACount: Byte): Cardinal; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AValue shr ACount) or (AValue shl (32 - ACount));
end;

{$PUSH}{$Q-}{$R-}
procedure Compress(var AState: array of Cardinal; const ABlock: PByte);
var
  A, B, C, D, E, F, G, H: Cardinal;
  I, RoundIndex: Integer;
  Choice, Majority, Sigma, Temporary, TemporaryTwo: Cardinal;
  Schedule: array[0..63] of Cardinal;
begin
  for I := 0 to 15 do
    Schedule[I] :=
      (Cardinal(ABlock[I * 4]) shl 24) or
      (Cardinal(ABlock[I * 4 + 1]) shl 16) or
      (Cardinal(ABlock[I * 4 + 2]) shl 8) or
      Cardinal(ABlock[I * 4 + 3]);

  for I := 16 to 63 do
  begin
    Sigma := RotateRight(Schedule[I - 15], 7) xor
      RotateRight(Schedule[I - 15], 18) xor
      (Schedule[I - 15] shr 3);
    Temporary := RotateRight(Schedule[I - 2], 17) xor
      RotateRight(Schedule[I - 2], 19) xor
      (Schedule[I - 2] shr 10);
    Schedule[I] := Schedule[I - 16] + Sigma +
      Schedule[I - 7] + Temporary;
  end;

  A := AState[0];
  B := AState[1];
  C := AState[2];
  D := AState[3];
  E := AState[4];
  F := AState[5];
  G := AState[6];
  H := AState[7];

  for RoundIndex := 0 to 63 do
  begin
    Sigma := RotateRight(E, 6) xor RotateRight(E, 11) xor
      RotateRight(E, 25);
    Choice := (E and F) xor ((not E) and G);
    Temporary := H + Sigma + Choice +
      ROUND_CONSTANTS[RoundIndex] + Schedule[RoundIndex];
    Sigma := RotateRight(A, 2) xor RotateRight(A, 13) xor
      RotateRight(A, 22);
    Majority := (A and B) xor (A and C) xor (B and C);
    TemporaryTwo := Sigma + Majority;

    H := G;
    G := F;
    F := E;
    E := D + Temporary;
    D := C;
    C := B;
    B := A;
    A := Temporary + TemporaryTwo;
  end;

  Inc(AState[0], A);
  Inc(AState[1], B);
  Inc(AState[2], C);
  Inc(AState[3], D);
  Inc(AState[4], E);
  Inc(AState[5], F);
  Inc(AState[6], G);
  Inc(AState[7], H);
end;
{$POP}

procedure SHA256Init(out AContext: TSHA256Context);
begin
  AContext.State[0] := $6A09E667;
  AContext.State[1] := $BB67AE85;
  AContext.State[2] := $3C6EF372;
  AContext.State[3] := $A54FF53A;
  AContext.State[4] := $510E527F;
  AContext.State[5] := $9B05688C;
  AContext.State[6] := $1F83D9AB;
  AContext.State[7] := $5BE0CD19;
  FillChar(AContext.Buffer, SizeOf(AContext.Buffer), 0);
  AContext.BufferLength := 0;
  AContext.TotalLength := 0;
end;

procedure SHA256Update(var AContext: TSHA256Context; const AData: Pointer;
  const ALength: NativeInt);
var
  Source: PByte;
  Remaining, Take: NativeInt;
begin
  if ALength <= 0 then
    Exit;
  Source := PByte(AData);
  Remaining := ALength;
  Inc(AContext.TotalLength, QWord(ALength));
  // Top up a partial block first.
  if AContext.BufferLength > 0 then
  begin
    Take := 64 - AContext.BufferLength;
    if Take > Remaining then
      Take := Remaining;
    Move(Source^, AContext.Buffer[AContext.BufferLength], Take);
    Inc(AContext.BufferLength, Take);
    Inc(Source, Take);
    Dec(Remaining, Take);
    if AContext.BufferLength < 64 then
      Exit;
    Compress(AContext.State, @AContext.Buffer[0]);
    AContext.BufferLength := 0;
  end;
  // Whole blocks straight from the input.
  while Remaining >= 64 do
  begin
    Compress(AContext.State, Source);
    Inc(Source, 64);
    Dec(Remaining, 64);
  end;
  if Remaining > 0 then
  begin
    Move(Source^, AContext.Buffer[0], Remaining);
    AContext.BufferLength := Remaining;
  end;
end;

function SHA256FinalHex(var AContext: TSHA256Context): string;
var
  BitLength: QWord;
  Padding: array[0..71] of Byte;
  PaddingLength, I: Integer;
begin
  BitLength := AContext.TotalLength * 8;
  // A one bit, zeros up to 56 bytes into a block, then the bit length.
  FillChar(Padding, SizeOf(Padding), 0);
  Padding[0] := $80;
  if AContext.BufferLength < 56 then
    PaddingLength := 56 - AContext.BufferLength
  else
    PaddingLength := 120 - AContext.BufferLength;
  for I := 0 to 7 do
    Padding[PaddingLength + I] := Byte((BitLength shr ((7 - I) * 8)) and $FF);
  SHA256Update(AContext, @Padding[0], PaddingLength + 8);
  Result := '';
  for I := 0 to 7 do
    Result := Result + LowerCase(IntToHex(AContext.State[I], 8));
end;

function SHA256Hex(const AData: TBytes): string;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  if Length(AData) > 0 then
    SHA256Update(Context, @AData[0], Length(AData));
  Result := SHA256FinalHex(Context);
end;

end.
