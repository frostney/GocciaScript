unit Goccia.SourceMap.VLQ;

{$I Goccia.inc}

// TC39 Source Map Specification — VLQ Base64 encoding
// https://tc39.es/source-map-spec/#mappings-structure

interface

function EncodeVLQ(const AValue: Integer): string;
function DecodeVLQ(const AEncoded: string; var APos: Integer): Integer;

implementation

uses
  SysUtils;

const
  VLQ_BASE_SHIFT = 5;
  VLQ_BASE = 1 shl VLQ_BASE_SHIFT;       // 32
  VLQ_BASE_MASK = VLQ_BASE - 1;           // 31
  VLQ_CONTINUATION_BIT = VLQ_BASE;        // 32

  // RFC 4648 §4 Base64 alphabet
  BASE64_CHARS: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  // Decode lookup: ASCII ordinal -> 6-bit value, -1 = invalid
  BASE64_DECODE: array[0..127] of ShortInt = (
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, // 0-15
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, // 16-31
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63, // 32-47 ('+' = 62, '/' = 63)
    52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1, // 48-63 ('0'-'9' = 52-61)
    -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14, // 64-79 ('A'-'O')
    15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1, // 80-95 ('P'-'Z')
    -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40, // 96-111 ('a'-'o')
    41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1  // 112-127 ('p'-'z')
  );

// TC39 Source Map Spec — VLQ encoding
// Bit 0 of the first sextet carries the sign (0 = positive, 1 = negative).
// Remaining bits carry the magnitude in groups of 5, LSB first.
// Bit 5 of each sextet is the continuation flag.
function EncodeVLQ(const AValue: Integer): string;
var
  VLQ: Cardinal;
  Digit: Integer;
begin
  Result := '';
  if AValue < 0 then
    VLQ := Cardinal(-AValue) shl 1 or 1
  else
    VLQ := Cardinal(AValue) shl 1;

  repeat
    Digit := VLQ and VLQ_BASE_MASK;
    VLQ := VLQ shr VLQ_BASE_SHIFT;
    if VLQ > 0 then
      Digit := Digit or VLQ_CONTINUATION_BIT;
    Result := Result + BASE64_CHARS[Digit + 1]; // 1-based string index
  until VLQ = 0;
end;

// TC39 Source Map Spec — VLQ decoding
// Inverse of EncodeVLQ. Reads one VLQ value starting at APos (1-based),
// advances APos past the consumed characters.
function DecodeVLQ(const AEncoded: string; var APos: Integer): Integer;
var
  Sextet: Integer;
  Shift: Integer;
  Value: Cardinal;
  CharOrd: Integer;
begin
  Value := 0;
  Shift := 0;

  repeat
    if APos > Length(AEncoded) then
      raise Exception.Create('VLQ: unexpected end of input');
    CharOrd := Ord(AEncoded[APos]);
    Inc(APos);
    if (CharOrd < 0) or (CharOrd > 127) then
      raise Exception.Create('VLQ: invalid Base64 character');
    Sextet := BASE64_DECODE[CharOrd];
    if Sextet < 0 then
      raise Exception.Create('VLQ: invalid Base64 character');
    Value := Value or Cardinal(Sextet and VLQ_BASE_MASK) shl Shift;
    Inc(Shift, VLQ_BASE_SHIFT);
  until (Sextet and VLQ_CONTINUATION_BIT) = 0;

  // Bit 0 is sign
  if (Value and 1) = 1 then
    Result := -Integer(Value shr 1)
  else
    Result := Integer(Value shr 1);
end;

end.
