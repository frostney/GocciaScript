unit Goccia.Base64;

{$I Goccia.inc}

// Shared Base64 encoding for standard (RFC 4648 §4) alphabet.
// Used by Uint8Array encoding and source map inline comments.

interface

uses
  SysUtils;

function EncodeBase64Standard(const AData: TBytes): string;
function TryDecodeBase64Standard(const AText: string;
  out AData: TBytes): Boolean;

implementation

const
  BASE64_TABLE: array[0..63] of Char = (
    'A','B','C','D','E','F','G','H','I','J','K','L','M',
    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    'a','b','c','d','e','f','g','h','i','j','k','l','m',
    'n','o','p','q','r','s','t','u','v','w','x','y','z',
    '0','1','2','3','4','5','6','7','8','9','+','/'
  );
  BASE64_PAD_CHAR = '=';

// RFC 4648 §4 — standard Base64 encoding with padding.
function EncodeBase64Standard(const AData: TBytes): string;
var
  Groups, Remainder, OutLen, Pos, I: Integer;
  B0, B1, B2: Byte;
begin
  Groups := Length(AData) div 3;
  Remainder := Length(AData) mod 3;
  if Remainder > 0 then
    OutLen := (Groups + 1) * 4
  else
    OutLen := Groups * 4;
  SetLength(Result, OutLen);
  Pos := 1;

  for I := 0 to Groups - 1 do
  begin
    B0 := AData[I * 3];
    B1 := AData[I * 3 + 1];
    B2 := AData[I * 3 + 2];
    Result[Pos]     := BASE64_TABLE[(B0 shr 2) and $3F];
    Result[Pos + 1] := BASE64_TABLE[((B0 and $03) shl 4) or ((B1 shr 4) and $0F)];
    Result[Pos + 2] := BASE64_TABLE[((B1 and $0F) shl 2) or ((B2 shr 6) and $03)];
    Result[Pos + 3] := BASE64_TABLE[B2 and $3F];
    Inc(Pos, 4);
  end;

  case Remainder of
    1:
    begin
      B0 := AData[Groups * 3];
      Result[Pos]     := BASE64_TABLE[(B0 shr 2) and $3F];
      Result[Pos + 1] := BASE64_TABLE[(B0 and $03) shl 4];
      Result[Pos + 2] := BASE64_PAD_CHAR;
      Result[Pos + 3] := BASE64_PAD_CHAR;
    end;
    2:
    begin
      B0 := AData[Groups * 3];
      B1 := AData[Groups * 3 + 1];
      Result[Pos]     := BASE64_TABLE[(B0 shr 2) and $3F];
      Result[Pos + 1] := BASE64_TABLE[((B0 and $03) shl 4) or ((B1 shr 4) and $0F)];
      Result[Pos + 2] := BASE64_TABLE[(B1 and $0F) shl 2];
      Result[Pos + 3] := BASE64_PAD_CHAR;
    end;
  end;
end;

function Base64Value(const AChar: Char): Integer;
begin
  case AChar of
    'A'..'Z': Result := Ord(AChar) - Ord('A');
    'a'..'z': Result := Ord(AChar) - Ord('a') + 26;
    '0'..'9': Result := Ord(AChar) - Ord('0') + 52;
    '+': Result := 62;
    '/': Result := 63;
  else
    Result := -1;
  end;
end;

// RFC 4648 section 4 standard Base64 decoding. The final padding is optional,
// matching the existing configuration-file behaviour, but padding may only
// appear at the end of a complete four-character group.
function TryDecodeBase64Standard(const AText: string;
  out AData: TBytes): Boolean;
var
  InputLength, OutputLength, Padding, Remainder: Integer;
  InputIndex, OutputIndex, I: Integer;
  V0, V1, V2, V3: Integer;
begin
  SetLength(AData, 0);
  InputLength := Length(AText);
  if InputLength = 0 then
    Exit(True);

  Remainder := InputLength mod 4;
  if Remainder = 1 then
    Exit(False);

  Padding := 0;
  if AText[InputLength] = BASE64_PAD_CHAR then
  begin
    Padding := 1;
    if (InputLength > 1) and
       (AText[InputLength - 1] = BASE64_PAD_CHAR) then
      Padding := 2;
  end;
  if (Padding > 0) and (Remainder <> 0) then
    Exit(False);

  for I := 1 to InputLength - Padding do
    if Base64Value(AText[I]) < 0 then
      Exit(False);
  for I := InputLength - Padding + 1 to InputLength do
    if AText[I] <> BASE64_PAD_CHAR then
      Exit(False);

  OutputLength := (InputLength div 4) * 3 - Padding;
  if (Padding = 0) and (Remainder > 0) then
    Inc(OutputLength, Remainder - 1);
  SetLength(AData, OutputLength);

  InputIndex := 1;
  OutputIndex := 0;
  while InputIndex <= InputLength do
  begin
    V0 := Base64Value(AText[InputIndex]);
    V1 := Base64Value(AText[InputIndex + 1]);
    if (InputIndex + 2 <= InputLength) and
       (AText[InputIndex + 2] <> BASE64_PAD_CHAR) then
      V2 := Base64Value(AText[InputIndex + 2])
    else
      V2 := 0;
    if (InputIndex + 3 <= InputLength) and
       (AText[InputIndex + 3] <> BASE64_PAD_CHAR) then
      V3 := Base64Value(AText[InputIndex + 3])
    else
      V3 := 0;

    if OutputIndex < OutputLength then
    begin
      AData[OutputIndex] := Byte((V0 shl 2) or (V1 shr 4));
      Inc(OutputIndex);
    end;
    if OutputIndex < OutputLength then
    begin
      AData[OutputIndex] := Byte((V1 shl 4) or (V2 shr 2));
      Inc(OutputIndex);
    end;
    if OutputIndex < OutputLength then
    begin
      AData[OutputIndex] := Byte((V2 shl 6) or V3);
      Inc(OutputIndex);
    end;
    Inc(InputIndex, 4);
  end;
  Result := True;
end;

end.
