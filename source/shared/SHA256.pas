unit SHA256;

{$I Shared.inc}

interface

uses
  SysUtils;

function SHA256Hex(const AData: TBytes): string;

implementation

type
  TSHA256Digest = array[0..31] of Byte;

function RotateRight(const AValue: Cardinal; const ACount: Byte): Cardinal; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AValue shr ACount) or (AValue shl (32 - ACount));
end;

{$PUSH}{$Q-}{$R-}
function CalculateSHA256(const AData: TBytes): TSHA256Digest;
const
  RoundConstants: array[0..63] of Cardinal = (
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
var
  A, B, C, D, E, F, G, H: Cardinal;
  BitLength: QWord;
  BlockOffset, I, RoundIndex: Integer;
  Choice, Majority, Sigma, Temporary, TemporaryTwo: Cardinal;
  HashState: array[0..7] of Cardinal;
  Message: TBytes;
  Schedule: array[0..63] of Cardinal;
begin
  HashState[0] := $6A09E667;
  HashState[1] := $BB67AE85;
  HashState[2] := $3C6EF372;
  HashState[3] := $A54FF53A;
  HashState[4] := $510E527F;
  HashState[5] := $9B05688C;
  HashState[6] := $1F83D9AB;
  HashState[7] := $5BE0CD19;

  BitLength := QWord(Length(AData)) * 8;
  Message := Copy(AData, 0, Length(AData));
  SetLength(Message, Length(Message) + 1);
  Message[High(Message)] := $80;
  while (Length(Message) mod 64) <> 56 do
    SetLength(Message, Length(Message) + 1);
  SetLength(Message, Length(Message) + 8);
  for I := 0 to 7 do
    Message[High(Message) - I] := Byte(
      (BitLength shr (I * 8)) and $FF);

  BlockOffset := 0;
  while BlockOffset < Length(Message) do
  begin
    for I := 0 to 15 do
      Schedule[I] :=
        (Cardinal(Message[BlockOffset + I * 4]) shl 24) or
        (Cardinal(Message[BlockOffset + I * 4 + 1]) shl 16) or
        (Cardinal(Message[BlockOffset + I * 4 + 2]) shl 8) or
        Cardinal(Message[BlockOffset + I * 4 + 3]);

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

    A := HashState[0];
    B := HashState[1];
    C := HashState[2];
    D := HashState[3];
    E := HashState[4];
    F := HashState[5];
    G := HashState[6];
    H := HashState[7];

    for RoundIndex := 0 to 63 do
    begin
      Sigma := RotateRight(E, 6) xor RotateRight(E, 11) xor
        RotateRight(E, 25);
      Choice := (E and F) xor ((not E) and G);
      Temporary := H + Sigma + Choice +
        RoundConstants[RoundIndex] + Schedule[RoundIndex];
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

    Inc(HashState[0], A);
    Inc(HashState[1], B);
    Inc(HashState[2], C);
    Inc(HashState[3], D);
    Inc(HashState[4], E);
    Inc(HashState[5], F);
    Inc(HashState[6], G);
    Inc(HashState[7], H);
    Inc(BlockOffset, 64);
  end;

  for I := 0 to 7 do
  begin
    Result[I * 4] := Byte((HashState[I] shr 24) and $FF);
    Result[I * 4 + 1] := Byte((HashState[I] shr 16) and $FF);
    Result[I * 4 + 2] := Byte((HashState[I] shr 8) and $FF);
    Result[I * 4 + 3] := Byte(HashState[I] and $FF);
  end;
end;
{$POP}

function SHA256Hex(const AData: TBytes): string;
var
  Digest: TSHA256Digest;
  I: Integer;
begin
  Digest := CalculateSHA256(AData);
  Result := '';
  for I := Low(Digest) to High(Digest) do
    Result := Result + LowerCase(IntToHex(Digest[I], 2));
end;

end.
