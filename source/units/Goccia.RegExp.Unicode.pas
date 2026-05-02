unit Goccia.RegExp.Unicode;

{$I Goccia.inc}

interface

function ExpandUnicodePropertyEscape(const APropertyName: string;
  const ANegated: Boolean): string;
function PreprocessUnicodePattern(const APattern: string;
  const AIgnoreCase: Boolean = False): string;

implementation

uses
  SysUtils;

const
  UNSUPPORTED_PROPERTY_PREFIX = 'Invalid Unicode property name: ';
  UTF8_ANY_CODE_POINT =
    '(?:[\x00-\x7F]|[\xC2-\xDF][\x80-\xBF]|' +
    '[\xE0-\xEF][\x80-\xBF][\x80-\xBF]|' +
    '[\xF0-\xF4][\x80-\xBF][\x80-\xBF][\x80-\xBF])';
  UTF8_NON_SPACE_CODE_POINT =
    '(?:[\x00-\x08\x0E-\x1F\x21-\x7F]|' +
    '[\xC2-\xDF][\x80-\xBF]|' +
    '[\xE0-\xEF][\x80-\xBF][\x80-\xBF]|' +
    '[\xF0-\xF4][\x80-\xBF][\x80-\xBF][\x80-\xBF])';

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

function DecodeUtf8At(const APattern: string; const AIndex: Integer;
  out ACodePoint: Cardinal; out AByteLength: Integer): Boolean;
var
  B1, B2, B3, B4: Byte;
begin
  Result := False;
  ACodePoint := 0;
  AByteLength := 0;
  if AIndex > Length(APattern) then
    Exit;
  B1 := Ord(APattern[AIndex]);
  if B1 < $80 then
  begin
    ACodePoint := B1;
    AByteLength := 1;
    Exit(True);
  end;
  if (B1 >= $C2) and (B1 <= $DF) and (AIndex + 1 <= Length(APattern)) then
  begin
    B2 := Ord(APattern[AIndex + 1]);
    if (B2 and $C0) <> $80 then
      Exit;
    ACodePoint := ((B1 and $1F) shl 6) or (B2 and $3F);
    AByteLength := 2;
    Exit(True);
  end;
  if (B1 >= $E0) and (B1 <= $EF) and (AIndex + 2 <= Length(APattern)) then
  begin
    B2 := Ord(APattern[AIndex + 1]);
    B3 := Ord(APattern[AIndex + 2]);
    if ((B2 and $C0) <> $80) or ((B3 and $C0) <> $80) then
      Exit;
    ACodePoint := ((B1 and $0F) shl 12) or ((B2 and $3F) shl 6) or
      (B3 and $3F);
    AByteLength := 3;
    Exit(True);
  end;
  if (B1 >= $F0) and (B1 <= $F4) and (AIndex + 3 <= Length(APattern)) then
  begin
    B2 := Ord(APattern[AIndex + 1]);
    B3 := Ord(APattern[AIndex + 2]);
    B4 := Ord(APattern[AIndex + 3]);
    if ((B2 and $C0) <> $80) or ((B3 and $C0) <> $80) or
       ((B4 and $C0) <> $80) then
      Exit;
    ACodePoint := ((B1 and $07) shl 18) or ((B2 and $3F) shl 12) or
      ((B3 and $3F) shl 6) or (B4 and $3F);
    AByteLength := 4;
    Exit(True);
  end;
end;

function IsHexDigit(const C: Char): Boolean; inline;
begin
  Result := CharInSet(C, ['0'..'9', 'a'..'f', 'A'..'F']);
end;

function EscapeLiteralAtom(const AValue: string): string;
const
  REGEXP_SYNTAX_CHARS = ['\', '^', '$', '.', '|', '?', '*', '+', '(', ')',
    '[', ']', '{', '}'];
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AValue) do
  begin
    if CharInSet(AValue[I], REGEXP_SYNTAX_CHARS) then
      Result := Result + '\';
    Result := Result + AValue[I];
  end;
end;

function HexByte(const AValue: Byte): string; inline;
begin
  Result := '\x' + IntToHex(AValue, 2);
end;

function ByteRangeExcept(const AMin, AMax, AExcluded: Byte): string;
begin
  Result := '';
  if AExcluded > AMin then
    Result := Result + HexByte(AMin) + '-' + HexByte(AExcluded - 1);
  if AExcluded < AMax then
  begin
    if Result <> '' then
      Result := Result + HexByte(AExcluded + 1) + '-' + HexByte(AMax)
    else
      Result := HexByte(AExcluded + 1) + '-' + HexByte(AMax);
  end;
  if Result = '' then
    Result := '[^\s\S]'
  else
    Result := '[' + Result + ']';
end;

function Utf8AnyCodePointExcept(const ACodePoint: Cardinal): string;
var
  Bytes: string;
  Parts: array of string;

  procedure AddPart(const APart: string);
  begin
    SetLength(Parts, Length(Parts) + 1);
    Parts[High(Parts)] := APart;
  end;

var
  I: Integer;
begin
  Bytes := CodePointToUtf8(ACodePoint);
  SetLength(Parts, 0);
  if Length(Bytes) <> 1 then
    AddPart('[\x00-\x7F]');
  if Length(Bytes) <> 2 then
    AddPart('[\xC2-\xDF][\x80-\xBF]');
  if Length(Bytes) <> 3 then
    AddPart('[\xE0-\xEF][\x80-\xBF][\x80-\xBF]');
  if Length(Bytes) <> 4 then
    AddPart('[\xF0-\xF4][\x80-\xBF][\x80-\xBF][\x80-\xBF]');

  case Length(Bytes) of
    1:
      AddPart(ByteRangeExcept($00, $7F, Ord(Bytes[1])));
    2:
      begin
        AddPart(ByteRangeExcept($C2, $DF, Ord(Bytes[1])) + '[\x80-\xBF]');
        AddPart(HexByte(Ord(Bytes[1])) +
          ByteRangeExcept($80, $BF, Ord(Bytes[2])));
      end;
    3:
      begin
        AddPart(ByteRangeExcept($E0, $EF, Ord(Bytes[1])) +
          '[\x80-\xBF][\x80-\xBF]');
        AddPart(HexByte(Ord(Bytes[1])) +
          ByteRangeExcept($80, $BF, Ord(Bytes[2])) + '[\x80-\xBF]');
        AddPart(HexByte(Ord(Bytes[1])) + HexByte(Ord(Bytes[2])) +
          ByteRangeExcept($80, $BF, Ord(Bytes[3])));
      end;
    4:
      begin
        AddPart(ByteRangeExcept($F0, $F4, Ord(Bytes[1])) +
          '[\x80-\xBF][\x80-\xBF][\x80-\xBF]');
        AddPart(HexByte(Ord(Bytes[1])) +
          ByteRangeExcept($80, $BF, Ord(Bytes[2])) +
          '[\x80-\xBF][\x80-\xBF]');
        AddPart(HexByte(Ord(Bytes[1])) + HexByte(Ord(Bytes[2])) +
          ByteRangeExcept($80, $BF, Ord(Bytes[3])) + '[\x80-\xBF]');
        AddPart(HexByte(Ord(Bytes[1])) + HexByte(Ord(Bytes[2])) +
          HexByte(Ord(Bytes[3])) + ByteRangeExcept($80, $BF, Ord(Bytes[4])));
      end;
  end;

  Result := '(?:';
  for I := 0 to High(Parts) do
  begin
    if I > 0 then
      Result := Result + '|';
    Result := Result + Parts[I];
  end;
  Result := Result + ')';
end;

function EmitUnicodeAtom(const ACodePoint: Cardinal;
  const AIgnoreCase: Boolean): string;
begin
  if AIgnoreCase and (ACodePoint = $212A) then
    Result := '[Kk]'
  else
    Result := '(?:' + EscapeLiteralAtom(CodePointToUtf8(ACodePoint)) + ')';
end;

function TryParseUnicodeClassAtom(const APattern: string; var AIndex: Integer;
  const AStopIndex: Integer; out ACodePoint: Cardinal): Boolean;
var
  HexStr: string;
  HighSurrogate, LowSurrogate: Cardinal;
  ByteLength: Integer;
begin
  Result := False;
  ACodePoint := 0;
  if AIndex > AStopIndex then
    Exit;
  if (APattern[AIndex] = '\') and (AIndex + 5 <= AStopIndex) and
     (APattern[AIndex + 1] = 'u') then
  begin
    HexStr := Copy(APattern, AIndex + 2, 4);
    if not ((Length(HexStr) = 4) and IsHexDigit(HexStr[1]) and
            IsHexDigit(HexStr[2]) and IsHexDigit(HexStr[3]) and
            IsHexDigit(HexStr[4])) then
      Exit;
    HighSurrogate := StrToInt('$' + HexStr);
    Inc(AIndex, 6);
    if (HighSurrogate >= $D800) and (HighSurrogate <= $DBFF) and
       (AIndex + 5 <= AStopIndex) and (APattern[AIndex] = '\') and
       (APattern[AIndex + 1] = 'u') then
    begin
      HexStr := Copy(APattern, AIndex + 2, 4);
      if (Length(HexStr) = 4) and IsHexDigit(HexStr[1]) and
         IsHexDigit(HexStr[2]) and IsHexDigit(HexStr[3]) and
         IsHexDigit(HexStr[4]) then
      begin
        LowSurrogate := StrToInt('$' + HexStr);
        if (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
        begin
          ACodePoint := $10000 + ((HighSurrogate - $D800) shl 10) +
            (LowSurrogate - $DC00);
          Inc(AIndex, 6);
          Exit(True);
        end;
      end;
    end;
    ACodePoint := HighSurrogate;
    Exit(True);
  end;
  if DecodeUtf8At(APattern, AIndex, ACodePoint, ByteLength) and
     (ByteLength > 1) then
  begin
    Inc(AIndex, ByteLength);
    Exit(True);
  end;
end;

function TryConvertUnicodeCharacterClass(const APattern: string;
  const AStartIndex: Integer; const AIgnoreCase: Boolean;
  out AReplacement: string; out ANextIndex: Integer): Boolean;
var
  EndIndex, AtomStart, I: Integer;
  Negated: Boolean;
  FirstCodePoint, LastCodePoint, CurrentCodePoint: Cardinal;
  CodePoints: array of Cardinal;
begin
  Result := False;
  AReplacement := '';
  ANextIndex := AStartIndex;
  EndIndex := AStartIndex + 1;
  while (EndIndex <= Length(APattern)) and (APattern[EndIndex] <> ']') do
    Inc(EndIndex);
  if EndIndex > Length(APattern) then
    Exit;
  AtomStart := AStartIndex + 1;
  Negated := (AtomStart < EndIndex) and (APattern[AtomStart] = '^');
  if Negated then
    Inc(AtomStart);
  I := AtomStart;
  if not TryParseUnicodeClassAtom(APattern, I, EndIndex - 1, FirstCodePoint) then
    Exit;
  if I = EndIndex then
  begin
    if Negated then
      AReplacement := Utf8AnyCodePointExcept(FirstCodePoint)
    else
      AReplacement := EmitUnicodeAtom(FirstCodePoint, AIgnoreCase);
    ANextIndex := EndIndex + 1;
    Exit(True);
  end;
  if (not Negated) and (I < EndIndex) and (APattern[I] = '-') then
  begin
    Inc(I);
    if TryParseUnicodeClassAtom(APattern, I, EndIndex - 1, LastCodePoint) and
       (I = EndIndex) and (FirstCodePoint <= LastCodePoint) and
       (LastCodePoint - FirstCodePoint <= 32) then
    begin
      AReplacement := '(?:';
      for CurrentCodePoint := FirstCodePoint to LastCodePoint do
      begin
        if CurrentCodePoint > FirstCodePoint then
          AReplacement := AReplacement + '|';
        AReplacement := AReplacement +
          EscapeLiteralAtom(CodePointToUtf8(CurrentCodePoint));
      end;
      AReplacement := AReplacement + ')';
      ANextIndex := EndIndex + 1;
      Exit(True);
    end;
  end;

  SetLength(CodePoints, 1);
  CodePoints[0] := FirstCodePoint;
  while I < EndIndex do
  begin
    if not TryParseUnicodeClassAtom(APattern, I, EndIndex - 1, CurrentCodePoint) then
      Exit(False);
    SetLength(CodePoints, Length(CodePoints) + 1);
    CodePoints[High(CodePoints)] := CurrentCodePoint;
  end;

  if Negated then
  begin
    AReplacement := '[^\s\S]';
    ANextIndex := EndIndex + 1;
    Exit(True);
  end;
  AReplacement := '';
  AReplacement := AReplacement + '(?:';
  for I := 0 to High(CodePoints) do
  begin
    if I > 0 then
      AReplacement := AReplacement + '|';
    AReplacement := AReplacement +
      EscapeLiteralAtom(CodePointToUtf8(CodePoints[I]));
  end;
  AReplacement := AReplacement + ')';
  ANextIndex := EndIndex + 1;
  Result := True;
end;

// ES2026 §22.2.1 Patterns — preprocess pattern for Unicode mode.
// Expands \p{...} / \P{...} property escapes into TRegExpr-compatible
// character classes and converts \u{XXXX} code point escapes into
// literal UTF-8 byte sequences.
function PreprocessUnicodePattern(const APattern: string;
  const AIgnoreCase: Boolean): string;
var
  I, J, PatternLength: Integer;
  PropertyName: string;
  Negated: Boolean;
  InCharacterClass: Boolean;
  CodePoint: Cardinal;
  HexStart, HexLen: Integer;
  HexStr: string;
  LowSurrogate: Cardinal;
  ByteLength: Integer;
  ClassReplacement: string;
  NextIndex: Integer;
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
        '0':
          begin
            Result := Result + '(?:' + #0 + ')';
            Inc(I, 2);
          end;
        'S':
          begin
            Result := Result + UTF8_NON_SPACE_CODE_POINT;
            Inc(I, 2);
          end;
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
              for J := 1 to Length(HexStr) do
                if not IsHexDigit(HexStr[J]) then
                  raise EConvertError.Create(
                    'Invalid hex digit in Unicode escape: \u{' +
                    HexStr + '}');
              CodePoint := StrToInt('$' + HexStr);
              if CodePoint > $10FFFF then
                raise EConvertError.Create(
                  'Unicode escape out of range: \u{' + HexStr + '}');
              if InCharacterClass then
                Result := Result + EscapeLiteralAtom(CodePointToUtf8(CodePoint))
              else
                Result := Result + EmitUnicodeAtom(CodePoint, AIgnoreCase);
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
              if (CodePoint >= $D800) and (CodePoint <= $DBFF) and
                 (I + 11 <= PatternLength) and (APattern[I + 6] = '\') and
                 (APattern[I + 7] = 'u') and
                 IsHexDigit(APattern[I + 8]) and
                 IsHexDigit(APattern[I + 9]) and
                 IsHexDigit(APattern[I + 10]) and
                 IsHexDigit(APattern[I + 11]) then
              begin
                HexStr := Copy(APattern, I + 8, 4);
                LowSurrogate := StrToInt('$' + HexStr);
                if (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
                begin
                  CodePoint := $10000 + ((CodePoint - $D800) shl 10) +
                    (LowSurrogate - $DC00);
                  if InCharacterClass then
                    Result := Result + EscapeLiteralAtom(CodePointToUtf8(CodePoint))
                  else
                    Result := Result + EmitUnicodeAtom(CodePoint, AIgnoreCase);
                  Inc(I, 12);
                  Continue;
                end;
              end;
              if InCharacterClass then
                Result := Result + EscapeLiteralAtom(CodePointToUtf8(CodePoint))
              else
                Result := Result + EmitUnicodeAtom(CodePoint, AIgnoreCase);
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
      if TryConvertUnicodeCharacterClass(APattern, I, AIgnoreCase,
         ClassReplacement, NextIndex) then
      begin
        Result := Result + ClassReplacement;
        I := NextIndex;
        Continue;
      end;
      InCharacterClass := True;
      Result := Result + APattern[I];
      Inc(I);
    end
    else if (not InCharacterClass) and (APattern[I] = '.') then
    begin
      Result := Result + UTF8_ANY_CODE_POINT;
      Inc(I);
    end
    else if (not InCharacterClass) and
            DecodeUtf8At(APattern, I, CodePoint, ByteLength) and
            (ByteLength > 1) then
    begin
      Result := Result + EmitUnicodeAtom(CodePoint, AIgnoreCase);
      Inc(I, ByteLength);
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
