unit Goccia.Base64;

{$I Goccia.inc}

// Shared Base64 encoding for standard (RFC 4648 §4) alphabet.
// Used by Uint8Array encoding and source map inline comments.

interface

uses
  SysUtils;

function EncodeBase64Standard(const AData: TBytes): string;

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

end.
