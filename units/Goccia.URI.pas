unit Goccia.URI;

// ES2026 §19.2.6 URI Handling Functions
// Shared URI encoding and decoding used by encodeURI, encodeURIComponent,
// decodeURI, decodeURIComponent, and import.meta path encoding.

{$I Goccia.inc}

interface

{ ES2026 §19.2.6.2 encodeURI(uriString) — encode a complete URI }
function EncodeURI(const AString: string): string;

{ ES2026 §19.2.6.4 encodeURIComponent(uriComponent) — encode a URI component }
function EncodeURIComponent(const AString: string): string;

{ ES2026 §19.2.6.3 decodeURI(encodedURI) — decode a complete URI }
function DecodeURI(const AString: string): string;

{ ES2026 §19.2.6.5 decodeURIComponent(encodedURIComponent) — decode a URI component }
function DecodeURIComponent(const AString: string): string;

{ RFC 3986 §2.3 — percent-encode file-path characters for file:// URLs.
  Preserves unreserved characters plus sub-delims and '/', ':', '@'. }
function PercentEncodePath(const APath: string): string;

implementation

uses
  SysUtils,

  StringBuffer,

  Goccia.Values.ErrorHelper;

const
  HEX_DIGITS = '0123456789ABCDEF';

  // ES2026 §19.2.6.2 step 4: uriReserved + uriUnescaped + '#'
  // uriReserved = ; / ? : @ & = + $ ,
  // uriUnescaped = A-Z a-z 0-9 - _ . ! ~ * ' ( )
  // '#' is also unescaped for encodeURI
  ENCODE_URI_UNESCAPED = [
    'A'..'Z', 'a'..'z', '0'..'9',
    '-', '_', '.', '!', '~', '*', '''', '(', ')',
    ';', '/', '?', ':', '@', '&', '=', '+', '$', ',', '#'
  ];

  // ES2026 §19.2.6.4 step 4: uriUnescaped
  // uriUnescaped = A-Z a-z 0-9 - _ . ! ~ * ' ( )
  ENCODE_URI_COMPONENT_UNESCAPED = [
    'A'..'Z', 'a'..'z', '0'..'9',
    '-', '_', '.', '!', '~', '*', '''', '(', ')'
  ];

  // ES2026 §19.2.6.3 step 4: uriReserved + '#'
  // Characters that decodeURI does NOT decode even when percent-encoded
  DECODE_URI_RESERVED = [
    ';', '/', '?', ':', '@', '&', '=', '+', '$', ',', '#'
  ];

  // RFC 3986 §2.3 unreserved characters plus sub-delimiters and path characters
  PERCENT_ENCODE_PATH_UNESCAPED = [
    'A'..'Z', 'a'..'z', '0'..'9',
    '-', '.', '_', '~', '/',
    ':', '@', '!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='
  ];

  UNICODE_REPLACEMENT_CHARACTER = $FFFD;

{ Returns True if the byte at AIndex is a valid UTF-8 continuation byte (10xxxxxx). }
function IsContinuationByte(const AString: string; const AIndex, ALength: Integer): Boolean; inline;
begin
  Result := (AIndex <= ALength) and ((Ord(AString[AIndex]) and $C0) = $80);
end;

{ Decode a single UTF-8 code point starting at position AIndex.
  Advances AIndex past the decoded sequence. Returns the Unicode code point.
  Returns -1 for lone surrogates (U+D800..U+DFFF) which are invalid. }
function NextUTF8CodePoint(const AString: string; var AIndex: Integer): Integer;
var
  B: Byte;
  Len: Integer;
begin
  Len := Length(AString);
  B := Ord(AString[AIndex]);
  if B < $80 then
  begin
    Result := B;
    Inc(AIndex);
  end
  else if (B and $E0) = $C0 then
  begin
    Inc(AIndex);
    if not IsContinuationByte(AString, AIndex, Len) then
      Exit(UNICODE_REPLACEMENT_CHARACTER);
    Result := ((B and $1F) shl 6) or (Ord(AString[AIndex]) and $3F);
    Inc(AIndex);
  end
  else if (B and $F0) = $E0 then
  begin
    Inc(AIndex);
    if not IsContinuationByte(AString, AIndex, Len) then
      Exit(UNICODE_REPLACEMENT_CHARACTER);
    Result := (B and $0F) shl 12;
    Result := Result or ((Ord(AString[AIndex]) and $3F) shl 6);
    Inc(AIndex);
    if not IsContinuationByte(AString, AIndex, Len) then
      Exit(UNICODE_REPLACEMENT_CHARACTER);
    Result := Result or (Ord(AString[AIndex]) and $3F);
    Inc(AIndex);
  end
  else if (B and $F8) = $F0 then
  begin
    Inc(AIndex);
    if not IsContinuationByte(AString, AIndex, Len) then
      Exit(UNICODE_REPLACEMENT_CHARACTER);
    Result := (B and $07) shl 18;
    Result := Result or ((Ord(AString[AIndex]) and $3F) shl 12);
    Inc(AIndex);
    if not IsContinuationByte(AString, AIndex, Len) then
      Exit(UNICODE_REPLACEMENT_CHARACTER);
    Result := Result or ((Ord(AString[AIndex]) and $3F) shl 6);
    Inc(AIndex);
    if not IsContinuationByte(AString, AIndex, Len) then
      Exit(UNICODE_REPLACEMENT_CHARACTER);
    Result := Result or (Ord(AString[AIndex]) and $3F);
    Inc(AIndex);
  end
  else
  begin
    Result := UNICODE_REPLACEMENT_CHARACTER;
    Inc(AIndex);
  end;
end;

{ Percent-encode a single byte as %XX (uppercase hex). }
function PercentEncodeByte(const AByte: Byte): string; inline;
begin
  Result := '%' + HEX_DIGITS[(AByte shr 4) + 1] + HEX_DIGITS[(AByte and $0F) + 1];
end;

{ Append the UTF-8 bytes of a code point to ABuffer, percent-encoding each byte. }
procedure AppendPercentEncodedCodePoint(var ABuffer: TStringBuffer; const ACodePoint: Integer);
begin
  if ACodePoint < $80 then
    ABuffer.Append(PercentEncodeByte(ACodePoint))
  else if ACodePoint < $800 then
  begin
    ABuffer.Append(PercentEncodeByte($C0 or (ACodePoint shr 6)));
    ABuffer.Append(PercentEncodeByte($80 or (ACodePoint and $3F)));
  end
  else if ACodePoint < $10000 then
  begin
    ABuffer.Append(PercentEncodeByte($E0 or (ACodePoint shr 12)));
    ABuffer.Append(PercentEncodeByte($80 or ((ACodePoint shr 6) and $3F)));
    ABuffer.Append(PercentEncodeByte($80 or (ACodePoint and $3F)));
  end
  else
  begin
    ABuffer.Append(PercentEncodeByte($F0 or (ACodePoint shr 18)));
    ABuffer.Append(PercentEncodeByte($80 or ((ACodePoint shr 12) and $3F)));
    ABuffer.Append(PercentEncodeByte($80 or ((ACodePoint shr 6) and $3F)));
    ABuffer.Append(PercentEncodeByte($80 or (ACodePoint and $3F)));
  end;
end;

{ Returns True if ACodePoint is a UTF-16 surrogate (U+D800..U+DFFF). }
function IsSurrogate(const ACodePoint: Integer): Boolean; inline;
begin
  Result := (ACodePoint >= $D800) and (ACodePoint <= $DFFF);
end;

{ Returns True if ACodePoint is a high surrogate (U+D800..U+DBFF). }
function IsHighSurrogate(const ACodePoint: Integer): Boolean; inline;
begin
  Result := (ACodePoint >= $D800) and (ACodePoint <= $DBFF);
end;

{ Returns True if ACodePoint is a low surrogate (U+DC00..U+DFFF). }
function IsLowSurrogate(const ACodePoint: Integer): Boolean; inline;
begin
  Result := (ACodePoint >= $DC00) and (ACodePoint <= $DFFF);
end;

// ES2026 §19.2.6.1 Encode(string, unescapedSet)
function Encode(const AString: string; const AUnescapedSet: TSysCharSet): string;
var
  Buffer: TStringBuffer;
  I, Len, CodePoint, NextCP: Integer;
begin
  Len := Length(AString);
  if Len = 0 then
    Exit('');

  Buffer := TStringBuffer.Create(Len);
  I := 1;
  while I <= Len do
  begin
    CodePoint := NextUTF8CodePoint(AString, I);

    // ES2026 §19.2.6.1 step 4c: if the code point is in unescapedSet, pass it through
    if (CodePoint < 128) and (Chr(CodePoint) in AUnescapedSet) then
      Buffer.Append(Chr(CodePoint))
    else
    begin
      // ES2026 §19.2.6.1 step 4d: lone surrogates throw URIError
      if IsSurrogate(CodePoint) then
      begin
        // Check for surrogate pair — FPC stores UTF-8 internally, but the
        // JavaScript string model is UTF-16. A code point in the surrogate
        // range that appears in our internal UTF-8 string means a lone
        // surrogate in the JS source. Supplementary code points (>= U+10000)
        // are already decoded as full code points by NextUTF8CodePoint, so
        // they never appear here.
        if IsHighSurrogate(CodePoint) and (I <= Len) then
        begin
          NextCP := NextUTF8CodePoint(AString, I);
          if IsLowSurrogate(NextCP) then
          begin
            // Surrogate pair — combine to supplementary code point
            CodePoint := $10000 + ((CodePoint - $D800) shl 10) + (NextCP - $DC00);
            AppendPercentEncodedCodePoint(Buffer, CodePoint);
            Continue;
          end
          else
            ThrowURIError('URI malformed');
        end
        else
          ThrowURIError('URI malformed');
      end;

      // ES2026 §19.2.6.1 step 4d.vi: percent-encode UTF-8 octets
      AppendPercentEncodedCodePoint(Buffer, CodePoint);
    end;
  end;
  Result := Buffer.ToString;
end;

{ Returns True if C is an ASCII hex digit. }
function IsASCIIHexDigit(const C: Char): Boolean; inline;
begin
  Result := (C in ['0'..'9', 'A'..'F', 'a'..'f']);
end;

{ Parse a hex digit character to its numeric value. }
function HexVal(const C: Char): Byte; inline;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'A'..'F': Result := Ord(C) - Ord('A') + 10;
    'a'..'f': Result := Ord(C) - Ord('a') + 10;
  else
    Result := 0;
  end;
end;

{ Decode a single percent-encoded byte (%XX) at position AIndex.
  AIndex must point to the '%'. Advances AIndex by 3 on success.
  Throws URIError if the hex digits are invalid. }
function DecodePercentByte(const AString: string; var AIndex: Integer;
  const ALength: Integer): Byte;
begin
  if (AIndex + 2 > ALength) or
     not IsASCIIHexDigit(AString[AIndex + 1]) or
     not IsASCIIHexDigit(AString[AIndex + 2]) then
    ThrowURIError('URI malformed');
  Result := (HexVal(AString[AIndex + 1]) shl 4) or HexVal(AString[AIndex + 2]);
  Inc(AIndex, 3);
end;

// ES2026 §19.2.6.2 Decode(string, reservedSet)
function Decode(const AString: string; const AReservedSet: TSysCharSet): string;
var
  Buffer: TStringBuffer;
  I, Len, ByteCount, J: Integer;
  B, B2: Byte;
  CodePoint: Integer;
  UTF8Bytes: array[0..3] of Byte;
  DecodedChar: Char;
begin
  Len := Length(AString);
  if Len = 0 then
    Exit('');

  Buffer := TStringBuffer.Create(Len);
  I := 1;
  while I <= Len do
  begin
    if AString[I] <> '%' then
    begin
      Buffer.Append(AString[I]);
      Inc(I);
    end
    else
    begin
      // Decode the first percent-encoded byte
      B := DecodePercentByte(AString, I, Len);

      if B < $80 then
      begin
        // ES2026 §19.2.6.2 step 4b.ii: single-byte character
        DecodedChar := Chr(B);
        if DecodedChar in AReservedSet then
        begin
          // Reserved characters are re-emitted as percent-encoded (uppercase)
          Buffer.Append(PercentEncodeByte(B));
        end
        else
          Buffer.Append(DecodedChar);
      end
      else
      begin
        // ES2026 §19.2.6.2 step 4b.v: multi-byte UTF-8 sequence
        // Determine expected byte count from lead byte
        if (B and $E0) = $C0 then
          ByteCount := 2
        else if (B and $F0) = $E0 then
          ByteCount := 3
        else if (B and $F8) = $F0 then
          ByteCount := 4
        else
          ThrowURIError('URI malformed');

        UTF8Bytes[0] := B;

        // Decode continuation bytes
        for J := 1 to ByteCount - 1 do
        begin
          if (I > Len) or (AString[I] <> '%') then
            ThrowURIError('URI malformed');
          B2 := DecodePercentByte(AString, I, Len);
          if (B2 and $C0) <> $80 then
            ThrowURIError('URI malformed');
          UTF8Bytes[J] := B2;
        end;

        // Reconstruct the code point
        case ByteCount of
          2: CodePoint := ((UTF8Bytes[0] and $1F) shl 6) or
                           (UTF8Bytes[1] and $3F);
          3: CodePoint := ((UTF8Bytes[0] and $0F) shl 12) or
                          ((UTF8Bytes[1] and $3F) shl 6) or
                           (UTF8Bytes[2] and $3F);
          4: CodePoint := ((UTF8Bytes[0] and $07) shl 18) or
                          ((UTF8Bytes[1] and $3F) shl 12) or
                          ((UTF8Bytes[2] and $3F) shl 6) or
                           (UTF8Bytes[3] and $3F);
        else
          CodePoint := UNICODE_REPLACEMENT_CHARACTER;
        end;

        // Lone surrogates are invalid
        if IsSurrogate(CodePoint) then
          ThrowURIError('URI malformed');

        // Overlong encodings are invalid
        if ((ByteCount = 2) and (CodePoint < $80)) or
           ((ByteCount = 3) and (CodePoint < $800)) or
           ((ByteCount = 4) and (CodePoint < $10000)) then
          ThrowURIError('URI malformed');

        // Code points above U+10FFFF are invalid
        if CodePoint > $10FFFF then
          ThrowURIError('URI malformed');

        // Append the decoded UTF-8 bytes directly
        for J := 0 to ByteCount - 1 do
          Buffer.Append(Chr(UTF8Bytes[J]));
      end;
    end;
  end;
  Result := Buffer.ToString;
end;

// ES2026 §19.2.6.2 encodeURI(uriString)
function EncodeURI(const AString: string): string;
begin
  Result := Encode(AString, ENCODE_URI_UNESCAPED);
end;

// ES2026 §19.2.6.4 encodeURIComponent(uriComponent)
function EncodeURIComponent(const AString: string): string;
begin
  Result := Encode(AString, ENCODE_URI_COMPONENT_UNESCAPED);
end;

// ES2026 §19.2.6.3 decodeURI(encodedURI)
function DecodeURI(const AString: string): string;
begin
  Result := Decode(AString, DECODE_URI_RESERVED);
end;

// ES2026 §19.2.6.5 decodeURIComponent(encodedURIComponent)
function DecodeURIComponent(const AString: string): string;
begin
  Result := Decode(AString, []);
end;

// RFC 3986 §2.3 — percent-encode path characters that are not unreserved or '/'
function PercentEncodePath(const APath: string): string;
var
  Buffer: TStringBuffer;
  I: Integer;
  Ch: Char;
begin
  if Length(APath) = 0 then
    Exit('');

  Buffer := TStringBuffer.Create(Length(APath));
  for I := 1 to Length(APath) do
  begin
    Ch := APath[I];
    if Ch in PERCENT_ENCODE_PATH_UNESCAPED then
      Buffer.Append(Ch)
    else
      Buffer.Append(PercentEncodeByte(Ord(Ch)));
  end;
  Result := Buffer.ToString;
end;

end.
