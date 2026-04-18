unit JSONParser;

{$I Shared.inc}

interface

uses
  Math,
  SysUtils;

type
  EJSONParseError = class(Exception);

  TJSONParserCapability = (
    jpcAllowComments,
    jpcAllowTrailingCommas,
    jpcAllowSingleQuotedStrings,
    jpcAllowIdentifierKeys,
    jpcAllowHexNumbers,
    jpcAllowLeadingPlusSign,
    jpcAllowLeadingDecimalPoint,
    jpcAllowTrailingDecimalPoint,
    jpcAllowInfinityAndNaN,
    jpcAllowLineContinuationsInStrings,
    jpcAllowExtendedStringEscapes,
    jpcAllowAdditionalWhitespace
  );

  TJSONParserCapabilities = set of TJSONParserCapability;

const
  JSONParserStrictCapabilities: TJSONParserCapabilities = [];
  JSONParserJSON5Capabilities: TJSONParserCapabilities = [
    jpcAllowComments,
    jpcAllowTrailingCommas,
    jpcAllowSingleQuotedStrings,
    jpcAllowIdentifierKeys,
    jpcAllowHexNumbers,
    jpcAllowLeadingPlusSign,
    jpcAllowLeadingDecimalPoint,
    jpcAllowTrailingDecimalPoint,
    jpcAllowInfinityAndNaN,
    jpcAllowLineContinuationsInStrings,
    jpcAllowExtendedStringEscapes,
    jpcAllowAdditionalWhitespace
  ];

type
  TAbstractJSONParser = class abstract
  private
    FCapabilities: TJSONParserCapabilities;
    FLength: Integer;
    FPosition: Integer;
    FText: UTF8String;

    class function HexDigitValue(const AChar: Char): Integer; static;
    class function IsASCIIDigit(const AChar: Char): Boolean; static;
    class function IsASCIIHexDigit(const AChar: Char): Boolean; static;
    class function IsASCIIIdentifierContinueByte(const AChar: Char): Boolean; static;
    class function IsASCIIIdentifierStartByte(const AChar: Char): Boolean; static;
    class function IsJSON5WhitespaceCodePoint(const ACodePoint: Cardinal): Boolean; static;
    class function IsIdentifierContinueText(const AText: string): Boolean; static;
    class function IsIdentifierStartText(const AText: string): Boolean; static;
    class function TryDecodeIdentifierCodePoint(const AText: string;
      out ACodePoint: Cardinal): Boolean; static;
    class function UTF8SequenceLengthFromLeadByte(const AChar: Char): Integer; static;
    function ConsumeComment: Boolean;
    function ConsumeSequence(const ASequence: string): Boolean;
    function ConsumeWhitespace: Boolean;
    function MatchSequence(const ASequence: string): Boolean;
    function ParseHexEscape(const ADigits: Integer): string;
    function ParseIdentifierEscape(const AIsStart: Boolean): string;
    function ParseIdentifierName: string;
    function ParseObjectKey: string;
    function PeekUTF8Sequence: UTF8String;
    function ReadUTF8Sequence: UTF8String;
    function Supports(const ACapability: TJSONParserCapability): Boolean; inline;
    function TryMatchAdditionalWhitespace(out AByteLength: Integer): Boolean;
    function TryMatchLineTerminator(out AByteLength: Integer): Boolean;
    function TryParseAdditionalLiteral(const AIsNegative: Boolean): Boolean;
    function TryParseHexNumber(const ASignText: string): Boolean;
    function TryParseStringContinuation: Boolean;

    procedure SkipWhitespace;

    function PeekChar: Char; inline;
    function ReadChar: Char; inline;
    function ExpectChar(const AExpected: Char): Boolean;
    function IsAtEnd: Boolean; inline;
    procedure RaiseParseError(const AMessage: string);
  protected
    function ParseString(const AQuote: Char): string; overload;
    function ParseString: string; overload;
    function ParseUnicodeEscape: string; virtual;

    procedure DoParse(const AText: UTF8String);
    procedure DoParseArray;
    procedure DoParseNumber;
    procedure DoParseObject;
    procedure DoParseValue;

    procedure OnNull; virtual; abstract;
    procedure OnBoolean(const AValue: Boolean); virtual; abstract;
    procedure OnString(const AValue: string); virtual; abstract;
    procedure OnInteger(const AValue: Int64); virtual; abstract;
    procedure OnFloat(const AValue: Double); virtual; abstract;
    procedure OnBeginObject; virtual; abstract;
    procedure OnObjectKey(const AKey: string); virtual; abstract;
    procedure OnEndObject; virtual; abstract;
    procedure OnBeginArray; virtual; abstract;
    procedure OnEndArray; virtual; abstract;
    procedure OnValueStart; virtual;

    property CurrentPosition: Integer read FPosition;
    property SourceTextData: UTF8String read FText;
  public
    constructor Create; overload; virtual;
    constructor Create(
      const ACapabilities: TJSONParserCapabilities); overload; virtual;
  end;

implementation

const
  JSON5_NO_BREAK_SPACE = #$C2#$A0;
  JSON5_OGHAM_SPACE_MARK = #$E1#$9A#$80;
  JSON5_EN_QUAD = #$E2#$80#$80;
  JSON5_EM_QUAD = #$E2#$80#$81;
  JSON5_EN_SPACE = #$E2#$80#$82;
  JSON5_EM_SPACE = #$E2#$80#$83;
  JSON5_THREE_PER_EM_SPACE = #$E2#$80#$84;
  JSON5_FOUR_PER_EM_SPACE = #$E2#$80#$85;
  JSON5_SIX_PER_EM_SPACE = #$E2#$80#$86;
  JSON5_FIGURE_SPACE = #$E2#$80#$87;
  JSON5_PUNCTUATION_SPACE = #$E2#$80#$88;
  JSON5_THIN_SPACE = #$E2#$80#$89;
  JSON5_HAIR_SPACE = #$E2#$80#$8A;
  JSON5_LINE_SEPARATOR = #$E2#$80#$A8;
  JSON5_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
  JSON5_NARROW_NO_BREAK_SPACE = #$E2#$80#$AF;
  JSON5_MEDIUM_MATHEMATICAL_SPACE = #$E2#$81#$9F;
  JSON5_IDEOGRAPHIC_SPACE = #$E3#$80#$80;
  JSON5_BYTE_ORDER_MARK = #$EF#$BB#$BF;
  JSON5_ZERO_WIDTH_NON_JOINER = #$E2#$80#$8C;
  JSON5_ZERO_WIDTH_JOINER = #$E2#$80#$8D;

constructor TAbstractJSONParser.Create;
begin
  Create(JSONParserStrictCapabilities);
end;

constructor TAbstractJSONParser.Create(
  const ACapabilities: TJSONParserCapabilities);
begin
  inherited Create;
  FCapabilities := ACapabilities;
end;

class function TAbstractJSONParser.HexDigitValue(const AChar: Char): Integer;
begin
  case AChar of
    '0'..'9':
      Result := Ord(AChar) - Ord('0');
    'a'..'f':
      Result := Ord(AChar) - Ord('a') + 10;
    'A'..'F':
      Result := Ord(AChar) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

class function TAbstractJSONParser.IsASCIIDigit(const AChar: Char): Boolean;
begin
  Result := AChar in ['0'..'9'];
end;

class function TAbstractJSONParser.IsASCIIHexDigit(const AChar: Char): Boolean;
begin
  Result := HexDigitValue(AChar) >= 0;
end;

class function TAbstractJSONParser.IsASCIIIdentifierStartByte(
  const AChar: Char): Boolean;
begin
  Result := (AChar in ['a'..'z', 'A'..'Z']) or (AChar = '_') or
    (AChar = '$');
end;

class function TAbstractJSONParser.IsASCIIIdentifierContinueByte(
  const AChar: Char): Boolean;
begin
  Result := IsASCIIIdentifierStartByte(AChar) or IsASCIIDigit(AChar);
end;

class function TAbstractJSONParser.IsJSON5WhitespaceCodePoint(
  const ACodePoint: Cardinal): Boolean;
begin
  case ACodePoint of
    $00A0,
    $1680,
    $2028,
    $2029,
    $202F,
    $205F,
    $3000,
    $FEFF:
      Exit(True);
    $2000..$200A:
      Exit(True);
  end;
  Result := False;
end;

class function TAbstractJSONParser.TryDecodeIdentifierCodePoint(
  const AText: string; out ACodePoint: Cardinal): Boolean;
var
  Byte1, Byte2, Byte3, Byte4: Byte;
begin
  Result := False;
  ACodePoint := 0;
  if AText = '' then
    Exit;

  case Length(AText) of
    1:
      begin
        ACodePoint := Ord(AText[1]);
        Exit(True);
      end;
    2:
      begin
        Byte1 := Ord(AText[1]);
        Byte2 := Ord(AText[2]);
        if ((Byte1 and $E0) <> $C0) or ((Byte2 and $C0) <> $80) then
          Exit;
        ACodePoint := Cardinal(Byte1 and $1F) shl 6 or Cardinal(Byte2 and $3F);
        Exit(True);
      end;
    3:
      begin
        Byte1 := Ord(AText[1]);
        Byte2 := Ord(AText[2]);
        Byte3 := Ord(AText[3]);
        if ((Byte1 and $F0) <> $E0) or ((Byte2 and $C0) <> $80) or
          ((Byte3 and $C0) <> $80) then
          Exit;
        ACodePoint := Cardinal(Byte1 and $0F) shl 12 or
          Cardinal(Byte2 and $3F) shl 6 or
          Cardinal(Byte3 and $3F);
        Exit(True);
      end;
    4:
      begin
        Byte1 := Ord(AText[1]);
        Byte2 := Ord(AText[2]);
        Byte3 := Ord(AText[3]);
        Byte4 := Ord(AText[4]);
        if ((Byte1 and $F8) <> $F0) or ((Byte2 and $C0) <> $80) or
          ((Byte3 and $C0) <> $80) or ((Byte4 and $C0) <> $80) then
          Exit;
        ACodePoint := Cardinal(Byte1 and $07) shl 18 or
          Cardinal(Byte2 and $3F) shl 12 or
          Cardinal(Byte3 and $3F) shl 6 or
          Cardinal(Byte4 and $3F);
        Exit(True);
      end;
  end;
end;

class function TAbstractJSONParser.IsIdentifierStartText(
  const AText: string): Boolean;
var
  CodePoint: Cardinal;
begin
  if AText = '' then
    Exit(False);
  if not TryDecodeIdentifierCodePoint(AText, CodePoint) then
    Exit(False);
  if CodePoint <= $7F then
    Exit(IsASCIIIdentifierStartByte(Chr(CodePoint)));
  if (CodePoint = $200C) or (CodePoint = $200D) then
    Exit(False);
  Result := not IsJSON5WhitespaceCodePoint(CodePoint);
end;

class function TAbstractJSONParser.IsIdentifierContinueText(
  const AText: string): Boolean;
var
  CodePoint: Cardinal;
begin
  if AText = '' then
    Exit(False);
  if not TryDecodeIdentifierCodePoint(AText, CodePoint) then
    Exit(False);
  if CodePoint <= $7F then
    Exit(IsASCIIIdentifierContinueByte(Chr(CodePoint)));
  if (CodePoint = $200C) or (CodePoint = $200D) then
    Exit(True);
  Result := not IsJSON5WhitespaceCodePoint(CodePoint);
end;

class function TAbstractJSONParser.UTF8SequenceLengthFromLeadByte(
  const AChar: Char): Integer;
var
  ByteValue: Byte;
begin
  ByteValue := Ord(AChar);
  if ByteValue < $80 then
    Exit(1);
  if (ByteValue and $E0) = $C0 then
    Exit(2);
  if (ByteValue and $F0) = $E0 then
    Exit(3);
  if (ByteValue and $F8) = $F0 then
    Exit(4);
  Result := 1;
end;

function TAbstractJSONParser.Supports(
  const ACapability: TJSONParserCapability): Boolean;
begin
  Result := ACapability in FCapabilities;
end;

procedure TAbstractJSONParser.DoParse(const AText: UTF8String);
begin
  FText := AText;
  FPosition := 1;
  FLength := Length(FText);

  SkipWhitespace;
  if IsAtEnd then
    RaiseParseError('Empty JSON input');
  DoParseValue;
  SkipWhitespace;
  if not IsAtEnd then
    RaiseParseError('Unexpected character after JSON value');
end;

function TAbstractJSONParser.PeekUTF8Sequence: UTF8String;
var
  I: Integer;
  SequenceLength: Integer;
begin
  Result := '';
  if IsAtEnd then
    Exit;

  SequenceLength := UTF8SequenceLengthFromLeadByte(PeekChar);
  if (SequenceLength = 0) or (FPosition + SequenceLength - 1 > FLength) then
    Exit;

  Result := Copy(FText, FPosition, SequenceLength);
  if SequenceLength = 1 then
    Exit;

  for I := 2 to SequenceLength do
    if (Ord(Result[I]) and $C0) <> $80 then
      Exit('');
end;

function TAbstractJSONParser.ReadUTF8Sequence: UTF8String;
begin
  Result := PeekUTF8Sequence;
  if Result <> '' then
    Inc(FPosition, Length(Result));
end;

procedure TAbstractJSONParser.OnValueStart;
begin
  // No-op by default. Subclasses override to track value source positions.
end;

procedure TAbstractJSONParser.DoParseValue;
var
  Ch: Char;
begin
  SkipWhitespace;
  if IsAtEnd then
    RaiseParseError('Unexpected end of JSON input');

  OnValueStart;

  Ch := PeekChar;
  case Ch of
    '{':
      DoParseObject;
    '[':
      DoParseArray;
    '"':
      OnString(ParseString('"'));
    '''':
      if Supports(jpcAllowSingleQuotedStrings) then
        OnString(ParseString(''''))
      else
        RaiseParseError('Unexpected character: ' + Ch);
    't':
    begin
      if MatchSequence('true') then
      begin
        ConsumeSequence('true');
        OnBoolean(True);
      end
      else
        RaiseParseError('Invalid literal');
    end;
    'f':
    begin
      if MatchSequence('false') then
      begin
        ConsumeSequence('false');
        OnBoolean(False);
      end
      else
        RaiseParseError('Invalid literal');
    end;
    'n':
    begin
      if MatchSequence('null') then
      begin
        ConsumeSequence('null');
        OnNull;
      end
      else
        RaiseParseError('Invalid literal');
    end;
    '-', '+', '.', '0'..'9', 'I', 'N':
      DoParseNumber;
  else
    RaiseParseError('Unexpected character: ' + Ch);
  end;
end;

procedure TAbstractJSONParser.DoParseObject;
var
  Key: string;
begin
  ReadChar;
  SkipWhitespace;
  OnBeginObject;

  if PeekChar = '}' then
  begin
    ReadChar;
    OnEndObject;
    Exit;
  end;

  while not IsAtEnd do
  begin
    Key := ParseObjectKey;
    OnObjectKey(Key);

    SkipWhitespace;
    if not ExpectChar(':') then
      RaiseParseError('Expected colon after object key');

    DoParseValue;
    SkipWhitespace;

    if PeekChar = '}' then
    begin
      ReadChar;
      OnEndObject;
      Exit;
    end;

    if not ExpectChar(',') then
      RaiseParseError('Expected comma between object properties');

    SkipWhitespace;
    if Supports(jpcAllowTrailingCommas) and (PeekChar = '}') then
    begin
      ReadChar;
      OnEndObject;
      Exit;
    end;
  end;

  RaiseParseError('Unterminated object');
end;

procedure TAbstractJSONParser.DoParseArray;
begin
  ReadChar;
  SkipWhitespace;
  OnBeginArray;

  if PeekChar = ']' then
  begin
    ReadChar;
    OnEndArray;
    Exit;
  end;

  while not IsAtEnd do
  begin
    DoParseValue;
    SkipWhitespace;

    if PeekChar = ']' then
    begin
      ReadChar;
      OnEndArray;
      Exit;
    end;

    if not ExpectChar(',') then
      RaiseParseError('Expected comma between array elements');

    SkipWhitespace;
    if Supports(jpcAllowTrailingCommas) and (PeekChar = ']') then
    begin
      ReadChar;
      OnEndArray;
      Exit;
    end;
  end;

  RaiseParseError('Unterminated array');
end;

function TAbstractJSONParser.TryParseAdditionalLiteral(
  const AIsNegative: Boolean): Boolean;
begin
  Result := False;

  if Supports(jpcAllowInfinityAndNaN) and MatchSequence('Infinity') then
  begin
    ConsumeSequence('Infinity');
    if AIsNegative then
      OnFloat(-Infinity)
    else
      OnFloat(Infinity);
    Exit(True);
  end;

  if Supports(jpcAllowInfinityAndNaN) and MatchSequence('NaN') then
  begin
    ConsumeSequence('NaN');
    OnFloat(NaN);
    Exit(True);
  end;
end;

function TAbstractJSONParser.TryParseHexNumber(const ASignText: string): Boolean;
var
  DigitCount: Integer;
  Value: Double;
begin
  Result := False;
  if not Supports(jpcAllowHexNumbers) then
    Exit;
  if not MatchSequence('0x') and not MatchSequence('0X') then
    Exit;

  ConsumeSequence(Copy(FText, FPosition, 2));
  if IsAtEnd or not IsASCIIHexDigit(PeekChar) then
    RaiseParseError('Invalid number format');

  Value := 0;
  DigitCount := 0;
  while not IsAtEnd and IsASCIIHexDigit(PeekChar) do
  begin
    Value := (Value * 16) + HexDigitValue(ReadChar);
    Inc(DigitCount);
  end;

  if DigitCount = 0 then
    RaiseParseError('Invalid number format');

  if ASignText = '-' then
    Value := -Value;
  OnFloat(Value);
  Result := True;
end;

procedure TAbstractJSONParser.DoParseNumber;
var
  Code: Integer;
  DecimalPointSeen: Boolean;
  ExponentSeen: Boolean;
  FloatFormat: TFormatSettings;
  FloatValue: Double;
  IntegerValue: Int64;
  NumStr: string;
  SignText: string;
begin
  NumStr := '';
  SignText := '';
  DecimalPointSeen := False;
  ExponentSeen := False;

  if PeekChar in ['-', '+'] then
  begin
    SignText := ReadChar;
    if (SignText = '+') and not Supports(jpcAllowLeadingPlusSign) then
      RaiseParseError('Invalid number format');
  end;

  if TryParseAdditionalLiteral(SignText = '-') then
    Exit;

  if TryParseHexNumber(SignText) then
    Exit;

  NumStr := SignText;

  if not IsAtEnd and (PeekChar = '.') then
  begin
    if not Supports(jpcAllowLeadingDecimalPoint) then
      RaiseParseError('Invalid number format');
    DecimalPointSeen := True;
    NumStr := NumStr + '0' + ReadChar;
    if IsAtEnd or not IsASCIIDigit(PeekChar) then
      RaiseParseError('Invalid number format after decimal point');
    while not IsAtEnd and IsASCIIDigit(PeekChar) do
      NumStr := NumStr + ReadChar;
  end
  else if not IsAtEnd and (PeekChar = '0') then
  begin
    NumStr := NumStr + ReadChar;
    if not IsAtEnd and IsASCIIDigit(PeekChar) then
      RaiseParseError('Invalid number format');
  end
  else if not IsAtEnd and (PeekChar in ['1'..'9']) then
  begin
    while not IsAtEnd and IsASCIIDigit(PeekChar) do
      NumStr := NumStr + ReadChar;
  end
  else
    RaiseParseError('Invalid number format');

  if not IsAtEnd and (PeekChar = '.') then
  begin
    DecimalPointSeen := True;
    NumStr := NumStr + ReadChar;
    if IsAtEnd or not IsASCIIDigit(PeekChar) then
    begin
      if not Supports(jpcAllowTrailingDecimalPoint) then
        RaiseParseError('Invalid number format after decimal point');
    end
    else
      while not IsAtEnd and IsASCIIDigit(PeekChar) do
        NumStr := NumStr + ReadChar;
  end;

  if not IsAtEnd and (PeekChar in ['e', 'E']) then
  begin
    ExponentSeen := True;
    NumStr := NumStr + ReadChar;
    if not IsAtEnd and (PeekChar in ['+', '-']) then
      NumStr := NumStr + ReadChar;
    if IsAtEnd or not IsASCIIDigit(PeekChar) then
      RaiseParseError('Invalid number format in exponent');
    while not IsAtEnd and IsASCIIDigit(PeekChar) do
      NumStr := NumStr + ReadChar;
  end;

  Val(NumStr, IntegerValue, Code);
  if (Code = 0) and not DecimalPointSeen and not ExponentSeen then
  begin
    if (IntegerValue = 0) and (Length(NumStr) > 0) and (NumStr[1] = '-') then
      OnFloat(-0.0)
    else
      OnInteger(IntegerValue);
    Exit;
  end;

  FloatFormat := DefaultFormatSettings;
  FloatFormat.DecimalSeparator := '.';
  FloatValue := StrToFloat(NumStr, FloatFormat);
  OnFloat(FloatValue);
end;

function TAbstractJSONParser.ParseObjectKey: string;
begin
  SkipWhitespace;

  case PeekChar of
    '"':
      Exit(ParseString('"'));
    '''':
      if Supports(jpcAllowSingleQuotedStrings) then
        Exit(ParseString(''''));
  end;

  if Supports(jpcAllowIdentifierKeys) then
    Exit(ParseIdentifierName);

  RaiseParseError('Expected string key in object');
end;

function TAbstractJSONParser.ParseIdentifierEscape(
  const AIsStart: Boolean): string;
begin
  if IsAtEnd or (PeekChar <> 'u') then
    RaiseParseError('Invalid escape character');
  ReadChar;
  Result := ParseUnicodeEscape;
  if AIsStart then
  begin
    if not IsIdentifierStartText(Result) then
      RaiseParseError('Invalid identifier escape');
  end
  else if not IsIdentifierContinueText(Result) then
    RaiseParseError('Invalid identifier escape');
end;

function TAbstractJSONParser.ParseIdentifierName: string;
var
  Sequence: UTF8String;
begin
  Result := '';

  if IsAtEnd then
    RaiseParseError('Unexpected end of JSON input');

  if PeekChar = '\' then
  begin
    ReadChar;
    Result := ParseIdentifierEscape(True);
  end
  else if IsASCIIIdentifierStartByte(PeekChar) then
    Result := Result + ReadChar
  else
  begin
    Sequence := PeekUTF8Sequence;
    if not IsIdentifierStartText(Sequence) then
      RaiseParseError('Expected string key in object');
    Result := Result + ReadUTF8Sequence;
  end;

  while not IsAtEnd do
  begin
    if PeekChar = '\' then
    begin
      ReadChar;
      Result := Result + ParseIdentifierEscape(False);
    end
    else if IsASCIIIdentifierContinueByte(PeekChar) then
      Result := Result + ReadChar
    else
    begin
      Sequence := PeekUTF8Sequence;
      if not IsIdentifierContinueText(Sequence) then
        Break;
      Result := Result + ReadUTF8Sequence;
    end;
  end;
end;

function TAbstractJSONParser.ParseString: string;
begin
  Result := ParseString('"');
end;

function TAbstractJSONParser.TryParseStringContinuation: Boolean;
var
  ByteLength: Integer;
begin
  Result := False;
  if not Supports(jpcAllowLineContinuationsInStrings) then
    Exit;
  if not TryMatchLineTerminator(ByteLength) then
    Exit;
  Inc(FPosition, ByteLength);
  Result := True;
end;

function TAbstractJSONParser.ParseString(const AQuote: Char): string;
var
  Ch: Char;
begin
  ExpectChar(AQuote);
  Result := '';

  while not IsAtEnd do
  begin
    Ch := ReadChar;
    if Ch = AQuote then
      Exit;

    if Ch = '\' then
    begin
      if IsAtEnd then
        RaiseParseError('Unexpected end in string escape');

      if TryParseStringContinuation then
        Continue;

      Ch := ReadChar;
      case Ch of
        '"':
          Result := Result + '"';
        '''':
          if Supports(jpcAllowExtendedStringEscapes) or (AQuote = '''') then
            Result := Result + ''''
          else
            RaiseParseError('Invalid escape character: \' + Ch);
        '\':
          Result := Result + '\';
        '/':
          Result := Result + '/';
        'b':
          Result := Result + #8;
        'f':
          Result := Result + #12;
        'n':
          Result := Result + #10;
        'r':
          Result := Result + #13;
        't':
          Result := Result + #9;
        'u':
          Result := Result + ParseUnicodeEscape;
        'v':
          if Supports(jpcAllowExtendedStringEscapes) then
            Result := Result + #11
          else
            RaiseParseError('Invalid escape character: \' + Ch);
        'x':
          if Supports(jpcAllowExtendedStringEscapes) then
            Result := Result + ParseHexEscape(2)
          else
            RaiseParseError('Invalid escape character: \' + Ch);
        '0':
          begin
            if Supports(jpcAllowExtendedStringEscapes) then
            begin
              if not IsAtEnd and IsASCIIDigit(PeekChar) then
                RaiseParseError('Invalid escape character: \' + PeekChar);
              Result := Result + #0;
            end
            else
              RaiseParseError('Invalid escape character: \' + Ch);
          end;
        '1'..'9':
          if Supports(jpcAllowExtendedStringEscapes) then
            RaiseParseError('Invalid escape character: \' + Ch)
          else
            RaiseParseError('Invalid escape character: \' + Ch);
      else
        if Supports(jpcAllowExtendedStringEscapes) then
          Result := Result + Ch
        else
          RaiseParseError('Invalid escape character: \' + Ch);
      end;
    end
    else if Ch in [#10, #13] then
      RaiseParseError('Unescaped control character in string')
    else if Ord(Ch) < 32 then
      RaiseParseError('Unescaped control character in string')
    else
      Result := Result + Ch;
  end;

  RaiseParseError('Unterminated string');
end;

function TAbstractJSONParser.ParseHexEscape(const ADigits: Integer): string;
var
  CodePoint: Cardinal;
  I: Integer;
  Value: Integer;
begin
  CodePoint := 0;
  for I := 1 to ADigits do
  begin
    if IsAtEnd then
      RaiseParseError('Incomplete unicode escape sequence');
    Value := HexDigitValue(ReadChar);
    if Value < 0 then
      RaiseParseError('Invalid hex digit in unicode escape');
    CodePoint := (CodePoint shl 4) or Cardinal(Value);
  end;

  if CodePoint <= $7F then
    Result := Chr(CodePoint)
  else if CodePoint <= $7FF then
    Result := Chr($C0 or (CodePoint shr 6)) +
      Chr($80 or (CodePoint and $3F))
  else if CodePoint <= $FFFF then
    Result := Chr($E0 or (CodePoint shr 12)) +
      Chr($80 or ((CodePoint shr 6) and $3F)) +
      Chr($80 or (CodePoint and $3F))
  else
    Result := Chr($F0 or (CodePoint shr 18)) +
      Chr($80 or ((CodePoint shr 12) and $3F)) +
      Chr($80 or ((CodePoint shr 6) and $3F)) +
      Chr($80 or (CodePoint and $3F));
end;

function TAbstractJSONParser.ParseUnicodeEscape: string;
var
  Ch: Char;
  CodePoint, High, Low: Cardinal;
  HexStr: string;
  I: Integer;
  SavedPos: Integer;

  function CodePointToUTF8(const ACP: Cardinal): string;
  begin
    if ACP <= $7F then
      Result := Chr(ACP)
    else if ACP <= $7FF then
      Result := Chr($C0 or (ACP shr 6)) + Chr($80 or (ACP and $3F))
    else if ACP <= $FFFF then
      Result := Chr($E0 or (ACP shr 12)) +
                Chr($80 or ((ACP shr 6) and $3F)) +
                Chr($80 or (ACP and $3F))
    else
      Result := Chr($F0 or (ACP shr 18)) +
                Chr($80 or ((ACP shr 12) and $3F)) +
                Chr($80 or ((ACP shr 6) and $3F)) +
                Chr($80 or (ACP and $3F));
  end;

begin
  HexStr := '';
  for I := 1 to 4 do
  begin
    if IsAtEnd then
      RaiseParseError('Incomplete unicode escape sequence');
    Ch := ReadChar;
    if not IsASCIIHexDigit(Ch) then
      RaiseParseError('Invalid hex digit in unicode escape: ' + Ch);
    HexStr := HexStr + Ch;
  end;

  CodePoint := StrToInt('$' + HexStr);

  if (CodePoint >= $D800) and (CodePoint <= $DBFF) then
  begin
    High := CodePoint;
    SavedPos := FPosition;
    if not IsAtEnd and (FPosition + 1 <= FLength) and
       (FText[FPosition] = '\') and (FText[FPosition + 1] = 'u') then
    begin
      Inc(FPosition, 2);
      HexStr := '';
      for I := 1 to 4 do
      begin
        if IsAtEnd then
        begin
          FPosition := SavedPos;
          Break;
        end;
        Ch := ReadChar;
        if not IsASCIIHexDigit(Ch) then
        begin
          FPosition := SavedPos;
          Break;
        end;
        HexStr := HexStr + Ch;
      end;
      if Length(HexStr) = 4 then
      begin
        Low := StrToInt('$' + HexStr);
        if (Low >= $DC00) and (Low <= $DFFF) then
          CodePoint := $10000 + ((High - $D800) shl 10) + (Low - $DC00)
        else
          FPosition := SavedPos;
      end;
    end;
  end;

  Result := CodePointToUTF8(CodePoint);
end;

function TAbstractJSONParser.MatchSequence(const ASequence: string): Boolean;
begin
  Result := (ASequence <> '') and (FPosition + Length(ASequence) - 1 <= FLength) and
    (Copy(FText, FPosition, Length(ASequence)) = ASequence);
end;

function TAbstractJSONParser.ConsumeSequence(const ASequence: string): Boolean;
begin
  Result := MatchSequence(ASequence);
  if Result then
    Inc(FPosition, Length(ASequence));
end;

function TAbstractJSONParser.TryMatchAdditionalWhitespace(
  out AByteLength: Integer): Boolean;
const
  JSON5_WHITESPACE_SEQUENCES: array[0..16] of string = (
    JSON5_NO_BREAK_SPACE,
    JSON5_OGHAM_SPACE_MARK,
    JSON5_EN_QUAD,
    JSON5_EM_QUAD,
    JSON5_EN_SPACE,
    JSON5_EM_SPACE,
    JSON5_THREE_PER_EM_SPACE,
    JSON5_FOUR_PER_EM_SPACE,
    JSON5_SIX_PER_EM_SPACE,
    JSON5_FIGURE_SPACE,
    JSON5_PUNCTUATION_SPACE,
    JSON5_THIN_SPACE,
    JSON5_HAIR_SPACE,
    JSON5_NARROW_NO_BREAK_SPACE,
    JSON5_MEDIUM_MATHEMATICAL_SPACE,
    JSON5_IDEOGRAPHIC_SPACE,
    JSON5_BYTE_ORDER_MARK
  );
var
  Sequence: string;
begin
  for Sequence in JSON5_WHITESPACE_SEQUENCES do
    if MatchSequence(Sequence) then
    begin
      AByteLength := Length(Sequence);
      Exit(True);
    end;
  AByteLength := 0;
  Result := False;
end;

function TAbstractJSONParser.TryMatchLineTerminator(
  out AByteLength: Integer): Boolean;
begin
  if MatchSequence(#13#10) then
  begin
    AByteLength := 2;
    Exit(True);
  end;

  if not IsAtEnd and (PeekChar in [#10, #13]) then
  begin
    AByteLength := 1;
    Exit(True);
  end;

  if MatchSequence(JSON5_LINE_SEPARATOR) then
  begin
    AByteLength := Length(JSON5_LINE_SEPARATOR);
    Exit(True);
  end;

  if MatchSequence(JSON5_PARAGRAPH_SEPARATOR) then
  begin
    AByteLength := Length(JSON5_PARAGRAPH_SEPARATOR);
    Exit(True);
  end;

  AByteLength := 0;
  Result := False;
end;

function TAbstractJSONParser.ConsumeWhitespace: Boolean;
var
  ByteLength: Integer;
begin
  Result := False;

  if not IsAtEnd and (PeekChar in [' ', #9, #10, #13]) then
  begin
    Inc(FPosition);
    Exit(True);
  end;

  if Supports(jpcAllowAdditionalWhitespace) and not IsAtEnd and
     (PeekChar in [#11, #12]) then
  begin
    Inc(FPosition);
    Exit(True);
  end;

  if Supports(jpcAllowAdditionalWhitespace) and
     TryMatchLineTerminator(ByteLength) then
  begin
    Inc(FPosition, ByteLength);
    Exit(True);
  end;

  if Supports(jpcAllowAdditionalWhitespace) and
     TryMatchAdditionalWhitespace(ByteLength) then
  begin
    Inc(FPosition, ByteLength);
    Exit(True);
  end;
end;

function TAbstractJSONParser.ConsumeComment: Boolean;
var
  ByteLength: Integer;
begin
  Result := False;
  if not Supports(jpcAllowComments) or IsAtEnd or (PeekChar <> '/') then
    Exit;

  if MatchSequence('//') then
  begin
    Inc(FPosition, 2);
    while not IsAtEnd do
    begin
      if TryMatchLineTerminator(ByteLength) then
      begin
        Inc(FPosition, ByteLength);
        Break;
      end;
      Inc(FPosition);
    end;
    Exit(True);
  end;

  if MatchSequence('/*') then
  begin
    Inc(FPosition, 2);
    while not IsAtEnd do
    begin
      if MatchSequence('*/') then
      begin
        Inc(FPosition, 2);
        Exit(True);
      end;
      Inc(FPosition);
    end;
    RaiseParseError('Unexpected end of JSON input');
  end;
end;

procedure TAbstractJSONParser.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    if ConsumeWhitespace then
      Continue;
    if ConsumeComment then
      Continue;
    Break;
  end;
end;

function TAbstractJSONParser.PeekChar: Char;
begin
  if FPosition > FLength then
    Result := #0
  else
    Result := FText[FPosition];
end;

function TAbstractJSONParser.ReadChar: Char;
begin
  Result := FText[FPosition];
  Inc(FPosition);
end;

function TAbstractJSONParser.ExpectChar(const AExpected: Char): Boolean;
begin
  SkipWhitespace;
  if IsAtEnd or (FText[FPosition] <> AExpected) then
    Exit(False);
  Inc(FPosition);
  Result := True;
end;

function TAbstractJSONParser.IsAtEnd: Boolean;
begin
  Result := FPosition > FLength;
end;

procedure TAbstractJSONParser.RaiseParseError(const AMessage: string);
begin
  raise EJSONParseError.Create(AMessage + ' at position ' + IntToStr(FPosition));
end;

end.
