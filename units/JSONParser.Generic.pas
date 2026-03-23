unit JSONParser.Generic;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  EJSONParseError = class(Exception);

  TAbstractJSONParser = class abstract
  private
    FText: string;
    FPosition: Integer;
    FLength: Integer;

    procedure SkipWhitespace; inline;
    function PeekChar: Char; inline;
    function ReadChar: Char; inline;
    function ExpectChar(const AExpected: Char): Boolean;
    function IsAtEnd: Boolean; inline;
    procedure RaiseParseError(const AMessage: string);
  protected
    function ParseString: string;
    function ParseUnicodeEscape: string; virtual;

    procedure DoParse(const AText: string);
    procedure DoParseValue;
    procedure DoParseObject;
    procedure DoParseArray;
    procedure DoParseNumber;

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
  end;

implementation

procedure TAbstractJSONParser.DoParse(const AText: string);
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

procedure TAbstractJSONParser.DoParseValue;
var
  Ch: Char;
begin
  SkipWhitespace;
  if IsAtEnd then
    RaiseParseError('Unexpected end of JSON input');

  Ch := PeekChar;
  case Ch of
    '{': DoParseObject;
    '[': DoParseArray;
    '"': OnString(ParseString);
    't':
    begin
      if (FPosition + 3 <= FLength) and (Copy(FText, FPosition, 4) = 'true') then
      begin
        Inc(FPosition, 4);
        OnBoolean(True);
      end
      else
        RaiseParseError('Invalid literal');
    end;
    'f':
    begin
      if (FPosition + 4 <= FLength) and (Copy(FText, FPosition, 5) = 'false') then
      begin
        Inc(FPosition, 5);
        OnBoolean(False);
      end
      else
        RaiseParseError('Invalid literal');
    end;
    'n':
    begin
      if (FPosition + 3 <= FLength) and (Copy(FText, FPosition, 4) = 'null') then
      begin
        Inc(FPosition, 4);
        OnNull;
      end
      else
        RaiseParseError('Invalid literal');
    end;
    '-', '0'..'9':
      DoParseNumber;
  else
    RaiseParseError('Unexpected character: ' + Ch);
  end;
end;

procedure TAbstractJSONParser.DoParseObject;
var
  Key: string;
  First: Boolean;
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

  First := True;
  while not IsAtEnd do
  begin
    if not First then
    begin
      if not ExpectChar(',') then
        RaiseParseError('Expected comma between object properties');
      SkipWhitespace;
    end;
    First := False;

    if PeekChar <> '"' then
      RaiseParseError('Expected string key in object');
    Key := ParseString;
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
  end;
end;

procedure TAbstractJSONParser.DoParseArray;
var
  First: Boolean;
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

  First := True;
  while not IsAtEnd do
  begin
    if not First then
    begin
      if not ExpectChar(',') then
        RaiseParseError('Expected comma between array elements');
      SkipWhitespace;
    end;
    First := False;

    DoParseValue;

    SkipWhitespace;
    if PeekChar = ']' then
    begin
      ReadChar;
      OnEndArray;
      Exit;
    end;
  end;
end;

procedure TAbstractJSONParser.DoParseNumber;
var
  NumStr: string;
  IntVal: Int64;
  FloatVal: Double;
  Code: Integer;
  FloatFormat: TFormatSettings;
begin
  NumStr := '';

  if PeekChar = '-' then
    NumStr := NumStr + ReadChar;

  if PeekChar = '0' then
    NumStr := NumStr + ReadChar
  else if PeekChar in ['1'..'9'] then
  begin
    while not IsAtEnd and (PeekChar in ['0'..'9']) do
      NumStr := NumStr + ReadChar;
  end
  else
    RaiseParseError('Invalid number format');

  if not IsAtEnd and (PeekChar = '.') then
  begin
    NumStr := NumStr + ReadChar;
    if IsAtEnd or not (PeekChar in ['0'..'9']) then
      RaiseParseError('Invalid number format after decimal point');
    while not IsAtEnd and (PeekChar in ['0'..'9']) do
      NumStr := NumStr + ReadChar;
  end;

  if not IsAtEnd and (PeekChar in ['e', 'E']) then
  begin
    NumStr := NumStr + ReadChar;
    if not IsAtEnd and (PeekChar in ['+', '-']) then
      NumStr := NumStr + ReadChar;
    if IsAtEnd or not (PeekChar in ['0'..'9']) then
      RaiseParseError('Invalid number format in exponent');
    while not IsAtEnd and (PeekChar in ['0'..'9']) do
      NumStr := NumStr + ReadChar;
  end;

  Val(NumStr, IntVal, Code);
  if (Code = 0) and (Pos('.', NumStr) = 0) and (Pos('e', LowerCase(NumStr)) = 0) then
    OnInteger(IntVal)
  else
  begin
    FloatFormat := DefaultFormatSettings;
    FloatFormat.DecimalSeparator := '.';
    FloatVal := StrToFloat(NumStr, FloatFormat);
    OnFloat(FloatVal);
  end;
end;

function TAbstractJSONParser.ParseString: string;
var
  Ch: Char;
begin
  ExpectChar('"');
  Result := '';

  while not IsAtEnd do
  begin
    Ch := ReadChar;
    if Ch = '"' then
      Exit;
    if Ch = '\' then
    begin
      if IsAtEnd then
        RaiseParseError('Unexpected end in string escape');
      Ch := ReadChar;
      case Ch of
        '"': Result := Result + '"';
        '\': Result := Result + '\';
        '/': Result := Result + '/';
        'b': Result := Result + #8;
        'f': Result := Result + #12;
        'n': Result := Result + #10;
        'r': Result := Result + #13;
        't': Result := Result + #9;
        'u': Result := Result + ParseUnicodeEscape;
      else
        RaiseParseError('Invalid escape character: \' + Ch);
      end;
    end
    else
      Result := Result + Ch;
  end;

  RaiseParseError('Unterminated string');
end;

function TAbstractJSONParser.ParseUnicodeEscape: string;
var
  HexStr: string;
  I: Integer;
  CodePoint, High, Low: Cardinal;
  Ch: Char;

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
    if not (Ch in ['0'..'9', 'a'..'f', 'A'..'F']) then
      RaiseParseError('Invalid hex digit in unicode escape: ' + Ch);
    HexStr := HexStr + Ch;
  end;

  CodePoint := StrToInt('$' + HexStr);

  if (CodePoint >= $D800) and (CodePoint <= $DBFF) then
  begin
    High := CodePoint;
    if not IsAtEnd and (FPosition + 1 <= FLength) and
       (FText[FPosition] = '\') and (FText[FPosition + 1] = 'u') then
    begin
      Inc(FPosition, 2);
      HexStr := '';
      for I := 1 to 4 do
      begin
        if IsAtEnd then
          RaiseParseError('Incomplete surrogate pair');
        Ch := ReadChar;
        if not (Ch in ['0'..'9', 'a'..'f', 'A'..'F']) then
          RaiseParseError('Invalid hex digit in surrogate pair: ' + Ch);
        HexStr := HexStr + Ch;
      end;
      Low := StrToInt('$' + HexStr);
      if (Low >= $DC00) and (Low <= $DFFF) then
        CodePoint := $10000 + ((High - $D800) shl 10) + (Low - $DC00);
    end;
  end;

  Result := CodePointToUTF8(CodePoint);
end;

procedure TAbstractJSONParser.SkipWhitespace;
begin
  while not IsAtEnd and (FText[FPosition] in [' ', #9, #10, #13]) do
    Inc(FPosition);
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
