unit Goccia.RegExp.Unicode;

{$I Goccia.inc}

interface

function ExpandUnicodePropertyEscape(const APropertyName: string;
  const ANegated: Boolean): string;
function PreprocessUnicodePattern(const APattern: string): string;

implementation

uses
  SysUtils;

const
  UNSUPPORTED_PROPERTY_PREFIX = 'Invalid Unicode property name: ';

  // ES2026 §22.2.2.9 Unicode property escape character classes.
  // These use ASCII-safe approximations for the most commonly used
  // General Category properties and Binary properties.
  CHAR_CLASS_LETTER = 'A-Za-z\xC0-\xD6\xD8-\xF6\xF8-\xFF';
  CHAR_CLASS_UPPERCASE_LETTER = 'A-Z\xC0-\xD6\xD8-\xDE';
  CHAR_CLASS_LOWERCASE_LETTER = 'a-z\xDF-\xF6\xF8-\xFF';
  CHAR_CLASS_DECIMAL_NUMBER = '0-9';
  CHAR_CLASS_NUMBER = '0-9';
  CHAR_CLASS_PUNCTUATION =
    '!\x22#%&\x27\x28\x29*,\x2D.\x2F:;\x3F@\x5B\\\x5D_\x7B\x7D';
  CHAR_CLASS_SYMBOL = '\x24+<=>^`|~';
  CHAR_CLASS_SEPARATOR = '\x20\xA0';
  CHAR_CLASS_CONTROL = '\x00-\x1F\x7F-\x9F';
  CHAR_CLASS_ASCII = '\x00-\x7F';
  CHAR_CLASS_ASCII_HEX_DIGIT = '0-9A-Fa-f';
  CHAR_CLASS_WHITE_SPACE = '\x09-\x0D\x20\xA0';

// ES2026 §22.2.2.9 CharacterClassEscape :: \p{UnicodePropertyValueExpression}
function ExpandUnicodePropertyEscape(const APropertyName: string;
  const ANegated: Boolean): string;
var
  CharClass: string;
  NegatePrefix: string;
begin
  CharClass := '';

  if (APropertyName = 'L') or (APropertyName = 'Letter') then
    CharClass := CHAR_CLASS_LETTER
  else if (APropertyName = 'Lu') or (APropertyName = 'Uppercase_Letter') then
    CharClass := CHAR_CLASS_UPPERCASE_LETTER
  else if (APropertyName = 'Ll') or (APropertyName = 'Lowercase_Letter') then
    CharClass := CHAR_CLASS_LOWERCASE_LETTER
  else if (APropertyName = 'N') or (APropertyName = 'Number') then
    CharClass := CHAR_CLASS_NUMBER
  else if (APropertyName = 'Nd') or (APropertyName = 'Decimal_Number') then
    CharClass := CHAR_CLASS_DECIMAL_NUMBER
  else if (APropertyName = 'P') or (APropertyName = 'Punctuation') then
    CharClass := CHAR_CLASS_PUNCTUATION
  else if (APropertyName = 'S') or (APropertyName = 'Symbol') then
    CharClass := CHAR_CLASS_SYMBOL
  else if (APropertyName = 'Z') or (APropertyName = 'Separator') then
    CharClass := CHAR_CLASS_SEPARATOR
  else if (APropertyName = 'Cc') or (APropertyName = 'Control') then
    CharClass := CHAR_CLASS_CONTROL
  else if APropertyName = 'ASCII' then
    CharClass := CHAR_CLASS_ASCII
  else if APropertyName = 'ASCII_Hex_Digit' then
    CharClass := CHAR_CLASS_ASCII_HEX_DIGIT
  else if APropertyName = 'White_Space' then
    CharClass := CHAR_CLASS_WHITE_SPACE
  else
    raise EConvertError.Create(UNSUPPORTED_PROPERTY_PREFIX + APropertyName);

  if ANegated then
    NegatePrefix := '^'
  else
    NegatePrefix := '';

  Result := '[' + NegatePrefix + CharClass + ']';
end;

// ES2026 §11.1.4 Static Semantics: UTF16EncodeCodePoint ( cp )
function CodePointToUtf8(const ACodePoint: Cardinal): string;
begin
  if ACodePoint <= $7F then
    Result := Chr(ACodePoint)
  else if ACodePoint <= $7FF then
    Result := Chr($C0 or (ACodePoint shr 6)) +
              Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $FFFF then
    Result := Chr($E0 or (ACodePoint shr 12)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $10FFFF then
    Result := Chr($F0 or (ACodePoint shr 18)) +
              Chr($80 or ((ACodePoint shr 12) and $3F)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else
    raise EConvertError.Create('Invalid Unicode code point: U+' +
      IntToHex(ACodePoint, 4));
end;

function IsHexDigit(const C: Char): Boolean; inline;
begin
  Result := CharInSet(C, ['0'..'9', 'a'..'f', 'A'..'F']);
end;

// ES2026 §22.2.1 Patterns — preprocess pattern for Unicode mode.
// Expands \p{...} / \P{...} property escapes into TRegExpr-compatible
// character classes and converts \u{XXXX} code point escapes into
// literal UTF-8 byte sequences.
function PreprocessUnicodePattern(const APattern: string): string;
var
  I, PatternLength: Integer;
  PropertyName: string;
  Negated: Boolean;
  InCharacterClass: Boolean;
  CodePoint: Cardinal;
  HexStart, HexLen: Integer;
  HexStr: string;
begin
  Result := '';
  I := 1;
  PatternLength := Length(APattern);
  InCharacterClass := False;

  while I <= PatternLength do
  begin
    if APattern[I] = '\' then
    begin
      if I + 1 > PatternLength then
      begin
        Result := Result + APattern[I];
        Inc(I);
        Continue;
      end;

      case APattern[I + 1] of
        'p', 'P':
          begin
            Negated := APattern[I + 1] = 'P';
            if (I + 2 <= PatternLength) and (APattern[I + 2] = '{') then
            begin
              PropertyName := '';
              Inc(I, 3);
              while (I <= PatternLength) and (APattern[I] <> '}') do
              begin
                PropertyName := PropertyName + APattern[I];
                Inc(I);
              end;
              if I > PatternLength then
                raise EConvertError.Create(
                  'Unterminated Unicode property escape');
              Inc(I); // skip closing brace
              Result := Result +
                ExpandUnicodePropertyEscape(PropertyName, Negated);
            end
            else
            begin
              Result := Result + APattern[I] + APattern[I + 1];
              Inc(I, 2);
            end;
          end;
        'u':
          begin
            // \u{XXXX} or \u{XXXXX} code point escape
            if (I + 2 <= PatternLength) and (APattern[I + 2] = '{') then
            begin
              HexStart := I + 3;
              HexLen := 0;
              while (HexStart + HexLen <= PatternLength) and
                    (APattern[HexStart + HexLen] <> '}') do
                Inc(HexLen);
              if HexStart + HexLen > PatternLength then
                raise EConvertError.Create(
                  'Unterminated Unicode escape sequence');
              HexStr := Copy(APattern, HexStart, HexLen);
              if HexStr = '' then
                raise EConvertError.Create(
                  'Empty Unicode escape sequence');
              CodePoint := StrToInt('$' + HexStr);
              if CodePoint > $10FFFF then
                raise EConvertError.Create(
                  'Unicode escape out of range: \u{' + HexStr + '}');
              if InCharacterClass then
                Result := Result + CodePointToUtf8(CodePoint)
              else
                Result := Result + '(?:' + CodePointToUtf8(CodePoint) + ')';
              I := HexStart + HexLen + 1;
            end
            // \uHHHH four-digit Unicode escape
            else if (I + 5 <= PatternLength) and
                    IsHexDigit(APattern[I + 2]) and
                    IsHexDigit(APattern[I + 3]) and
                    IsHexDigit(APattern[I + 4]) and
                    IsHexDigit(APattern[I + 5]) then
            begin
              HexStr := Copy(APattern, I + 2, 4);
              CodePoint := StrToInt('$' + HexStr);
              if InCharacterClass then
                Result := Result + CodePointToUtf8(CodePoint)
              else
                Result := Result + '(?:' + CodePointToUtf8(CodePoint) + ')';
              Inc(I, 6);
            end
            else
            begin
              Result := Result + APattern[I] + APattern[I + 1];
              Inc(I, 2);
            end;
          end;
      else
        begin
          Result := Result + APattern[I] + APattern[I + 1];
          Inc(I, 2);
        end;
      end;
    end
    else if APattern[I] = '[' then
    begin
      InCharacterClass := True;
      Result := Result + APattern[I];
      Inc(I);
    end
    else if (APattern[I] = ']') and InCharacterClass then
    begin
      InCharacterClass := False;
      Result := Result + APattern[I];
      Inc(I);
    end
    else
    begin
      Result := Result + APattern[I];
      Inc(I);
    end;
  end;
end;

end.
