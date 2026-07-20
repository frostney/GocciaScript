unit Goccia.JSON.Utils;

{$I Goccia.inc}

interface

/// Escape a string for embedding inside a JSON string literal.
///
/// All C0 control characters (U+0000-U+001F) are escaped:
///   - Known short escapes: \b (#8), \t (#9), \n (#10), \f (#12), \r (#13)
///   - Remaining control characters: \u00xx hex escapes
///   - Surrogate code points: \uXXXX hex escapes
///
/// Backslash and double-quote are also escaped.
function EscapeJSONString(const AValue: string): string;

/// Wrap a value in escaped double quotes: "…"
function QuoteJSONString(const AValue: string): string;

implementation

uses
  SysUtils,

  StringBuffer,
  TextSemantics;

const
  C0_CONTROL_LIMIT = $20;
  JSON_HEX_DIGITS = '0123456789abcdef';

procedure AppendJSONUnicodeEscape(var ABuffer: TStringBuffer;
  const ACodeUnit: Cardinal);
begin
  ABuffer.Append('\u');
  ABuffer.AppendChar(JSON_HEX_DIGITS[((ACodeUnit shr 12) and $F) + 1]);
  ABuffer.AppendChar(JSON_HEX_DIGITS[((ACodeUnit shr 8) and $F) + 1]);
  ABuffer.AppendChar(JSON_HEX_DIGITS[((ACodeUnit shr 4) and $F) + 1]);
  ABuffer.AppendChar(JSON_HEX_DIGITS[(ACodeUnit and $F) + 1]);
end;

procedure AppendJSONEscapedCodePoint(var ABuffer: TStringBuffer;
  const ACodePoint: Cardinal; const AText: string);
begin
  case ACodePoint of
    Ord('"'): ABuffer.Append('\"');
    Ord('\'): ABuffer.Append('\\');
    8: ABuffer.Append('\b');
    9: ABuffer.Append('\t');
    10: ABuffer.Append('\n');
    12: ABuffer.Append('\f');
    13: ABuffer.Append('\r');
  else
    if (ACodePoint < C0_CONTROL_LIMIT) or
       IsUnicodeSurrogateCodePoint(ACodePoint) then
      AppendJSONUnicodeEscape(ABuffer, ACodePoint)
    else
      ABuffer.Append(AText);
  end;
end;

// ES2026 §25.5.4.3 QuoteJSONString(value)
function EscapeJSONString(const AValue: string): string;
var
  Buffer: TStringBuffer;
  ByteLength: Integer;
  CodePoint: Cardinal;
  Index: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AValue));
  Index := 1;
  while Index <= Length(AValue) do
  begin
    if TryReadCodePointAtAllowSurrogates(AValue, Index, CodePoint,
      ByteLength) then
    begin
      AppendJSONEscapedCodePoint(Buffer, CodePoint,
        Copy(AValue, Index, ByteLength));
      Inc(Index, ByteLength);
    end
    else
    begin
      AppendJSONEscapedCodePoint(Buffer, Ord(AValue[Index]), AValue[Index]);
      Inc(Index);
    end;
  end;
  Result := Buffer.ToString;
end;

// ES2026 §25.5.4.3 QuoteJSONString(value)
function QuoteJSONString(const AValue: string): string;
begin
  Result := '"' + EscapeJSONString(AValue) + '"';
end;

end.
