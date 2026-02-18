unit Goccia.JSON;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  EGocciaJSONParseError = class(Exception);

  TGocciaJSONParser = class
  private
    FText: string;
    FPosition: Integer;
    FLength: Integer;

    function ParseValue: TGocciaValue;
    function ParseObject: TGocciaObjectValue;
    function ParseArray: TGocciaArrayValue;
    function ParseString: TGocciaStringLiteralValue;
    function ParseNumber: TGocciaNumberLiteralValue;
    function ParseLiteral: TGocciaValue;

    procedure SkipWhitespace;
    function PeekChar: Char; inline;
    function ReadChar: Char;
    function ExpectChar(const AExpected: Char): Boolean; inline;
    function IsAtEnd: Boolean; inline;
    procedure RaiseParseError(const AMessage: string);
  public
    function Parse(const AText: string): TGocciaValue;
  end;

  TGocciaJSONStringifier = class
  private
    function StringifyValue(const AValue: TGocciaValue; const AIndent: Integer = 0): string;
    function StringifyObject(const AObj: TGocciaObjectValue; const AIndent: Integer): string;
    function StringifyArray(const AArr: TGocciaArrayValue; const AIndent: Integer): string;
    function EscapeString(const AStr: string): string;
  public
    function Stringify(const AValue: TGocciaValue): string;
  end;

implementation

uses
  Goccia.Values.FunctionValue;

{ TGocciaJSONParser }

function TGocciaJSONParser.Parse(const AText: string): TGocciaValue;
begin
  FText := AText;
  FPosition := 1;
  FLength := Length(FText);

  SkipWhitespace;
  Result := ParseValue;
  SkipWhitespace;

  if not IsAtEnd then
    RaiseParseError('Unexpected character after JSON value');
end;

function TGocciaJSONParser.ParseValue: TGocciaValue;
var
  Ch: Char;
begin
  SkipWhitespace;

  if IsAtEnd then
    RaiseParseError('Unexpected end of JSON input');

  Ch := PeekChar;

  case Ch of
    '{': Result := ParseObject;
    '[': Result := ParseArray;
    '"': Result := ParseString;
    't', 'f', 'n': Result := ParseLiteral;
    '-', '0'..'9': Result := ParseNumber;
  else
    RaiseParseError('Unexpected character: ' + Ch);
  end;
end;

function TGocciaJSONParser.ParseObject: TGocciaObjectValue;
var
  Key: string;
  Value: TGocciaValue;
  FirstProperty: Boolean;
begin
  Result := TGocciaObjectValue.Create;

  ExpectChar('{');
  SkipWhitespace;

  if PeekChar = '}' then
  begin
    ReadChar;
    Exit;
  end;

  FirstProperty := True;
  while not IsAtEnd do
  begin
    if not FirstProperty then
    begin
      if not ExpectChar(',') then
        RaiseParseError('Expected comma between object properties');
      SkipWhitespace;
    end;
    FirstProperty := False;

    if PeekChar <> '"' then
      RaiseParseError('Expected string key in object');
    Key := ParseString.ToStringLiteral.Value;

    SkipWhitespace;
    if not ExpectChar(':') then
      RaiseParseError('Expected colon after object key');

    Value := ParseValue;
    Result.AssignProperty(Key, Value);

    SkipWhitespace;
    if PeekChar = '}' then
    begin
      ReadChar;
      Break;
    end;
  end;
end;

function TGocciaJSONParser.ParseArray: TGocciaArrayValue;
var
  Value: TGocciaValue;
  FirstElement: Boolean;
begin
  Result := TGocciaArrayValue.Create;

  ExpectChar('[');
  SkipWhitespace;

  if PeekChar = ']' then
  begin
    ReadChar;
    Exit;
  end;

  FirstElement := True;
  while not IsAtEnd do
  begin
    if not FirstElement then
    begin
      if not ExpectChar(',') then
        RaiseParseError('Expected comma between array elements');
      SkipWhitespace;
    end;
    FirstElement := False;

    Value := ParseValue;
    Result.Elements.Add(Value);

    SkipWhitespace;
    if PeekChar = ']' then
    begin
      ReadChar;
      Break;
    end;
  end;
end;

function TGocciaJSONParser.ParseString: TGocciaStringLiteralValue;
var
  Str: string;
  Ch: Char;
begin
  ExpectChar('"');
  Str := '';

  while not IsAtEnd do
  begin
    Ch := ReadChar;

    if Ch = '"' then
    begin
      Result := TGocciaStringLiteralValue.Create(Str);
      Exit;
    end;

    if Ch = '\' then
    begin
      if IsAtEnd then
        RaiseParseError('Unexpected end in string escape');

      Ch := ReadChar;
      case Ch of
        '"': Str := Str + '"';
        '\': Str := Str + '\';
        '/': Str := Str + '/';
        'b': Str := Str + #8;
        'f': Str := Str + #12;
        'n': Str := Str + #10;
        'r': Str := Str + #13;
        't': Str := Str + #9;
        'u': begin
          Str := Str + '\u';
        end;
      else
        RaiseParseError('Invalid escape character: \' + Ch);
      end;
    end
    else
      Str := Str + Ch;
  end;

  RaiseParseError('Unterminated string');
end;

function TGocciaJSONParser.ParseNumber: TGocciaNumberLiteralValue;
var
  NumStr: string;
  Value: Double;
begin
  NumStr := '';

  if PeekChar = '-' then
    NumStr := NumStr + ReadChar;

  if PeekChar = '0' then
  begin
    NumStr := NumStr + ReadChar;
  end
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

  try
    Value := StrToFloat(NumStr);
    Result := TGocciaNumberLiteralValue.Create(Value);
  except
    on E: Exception do
      RaiseParseError('Invalid number format: ' + NumStr);
  end;
end;

function TGocciaJSONParser.ParseLiteral: TGocciaValue;
begin
  case PeekChar of
    't': begin
      if (FPosition + 3 <= FLength) and
         (Copy(FText, FPosition, 4) = 'true') then
      begin
        Inc(FPosition, 4);
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end
      else
        RaiseParseError('Invalid literal starting with t');
    end;
    'f': begin
      if (FPosition + 4 <= FLength) and
         (Copy(FText, FPosition, 5) = 'false') then
      begin
        Inc(FPosition, 5);
        Result := TGocciaBooleanLiteralValue.FalseValue;
      end
      else
        RaiseParseError('Invalid literal starting with f');
    end;
    'n': begin
      if (FPosition + 3 <= FLength) and
         (Copy(FText, FPosition, 4) = 'null') then
      begin
        Inc(FPosition, 4);
        Result := TGocciaNullLiteralValue.Create;
      end
      else
        RaiseParseError('Invalid literal starting with n');
    end;
  else
    RaiseParseError('Unknown literal');
  end;
end;

procedure TGocciaJSONParser.SkipWhitespace;
begin
  while not IsAtEnd and (PeekChar in [' ', #9, #10, #13]) do
    Inc(FPosition);
end;

function TGocciaJSONParser.PeekChar: Char;
begin
  if IsAtEnd then
    Result := #0
  else
    Result := FText[FPosition];
end;

function TGocciaJSONParser.ReadChar: Char;
begin
  Result := PeekChar;
  Inc(FPosition);
end;

function TGocciaJSONParser.ExpectChar(const AExpected: Char): Boolean;
begin
  SkipWhitespace;
  if IsAtEnd or (PeekChar <> AExpected) then
  begin
    Result := False;
    Exit;
  end;
  ReadChar;
  Result := True;
end;

function TGocciaJSONParser.IsAtEnd: Boolean;
begin
  Result := FPosition > FLength;
end;

procedure TGocciaJSONParser.RaiseParseError(const AMessage: string);
begin
  raise EGocciaJSONParseError.Create(AMessage + ' at position ' + IntToStr(FPosition));
end;

{ TGocciaJSONStringifier }

function TGocciaJSONStringifier.Stringify(const AValue: TGocciaValue): string;
begin
  Result := StringifyValue(AValue);
end;

function TGocciaJSONStringifier.StringifyValue(const AValue: TGocciaValue; const AIndent: Integer): string;
begin
  if AValue is TGocciaNullLiteralValue then
    Result := 'null'
  else if AValue is TGocciaUndefinedLiteralValue then
    Result := 'null'
  else if AValue is TGocciaBooleanLiteralValue then
  begin
    if AValue.ToBooleanLiteral.Value then
      Result := 'true'
    else
      Result := 'false';
  end
  else if AValue is TGocciaNumberLiteralValue then
  begin
    if TGocciaNumberLiteralValue(AValue).IsInfinity or TGocciaNumberLiteralValue(AValue).IsNegativeInfinity then
      Result := 'null'
    else if TGocciaNumberLiteralValue(AValue).IsNaN then
      Result := 'null'
    else
      Result := FloatToStr(AValue.ToNumberLiteral.Value);
  end
  else if AValue is TGocciaStringLiteralValue then
    Result := '"' + EscapeString(AValue.ToStringLiteral.Value) + '"'
  else if AValue is TGocciaArrayValue then
    Result := StringifyArray(TGocciaArrayValue(AValue), AIndent)
  else if AValue is TGocciaObjectValue then
    Result := StringifyObject(TGocciaObjectValue(AValue), AIndent)
  else
    Result := 'null';
end;

function TGocciaJSONStringifier.StringifyObject(const AObj: TGocciaObjectValue; const AIndent: Integer): string;
var
  SB: TStringBuilder;
  Key: string;
  Value: TGocciaValue;
  HasProperties: Boolean;
begin
  SB := TStringBuilder.Create;
  try
    HasProperties := False;

    for Key in AObj.GetEnumerablePropertyNames do
    begin
      Value := AObj.GetProperty(Key);

      if not (Value is TGocciaUndefinedLiteralValue) and
         (not (Value is TGocciaFunctionValue) or (Pos('Function', Value.ClassName) = 0)) then
      begin
        if HasProperties then
          SB.Append(',');
        SB.Append('"').Append(EscapeString(Key)).Append('":').Append(StringifyValue(Value, AIndent + 1));
        HasProperties := True;
      end;
    end;

    if not HasProperties then
      Result := '{}'
    else
      Result := '{' + SB.ToString + '}';
  finally
    SB.Free;
  end;
end;

function TGocciaJSONStringifier.StringifyArray(const AArr: TGocciaArrayValue; const AIndent: Integer): string;
var
  SB: TStringBuilder;
  I: Integer;
begin
  if AArr.Elements.Count = 0 then
  begin
    Result := '[]';
    Exit;
  end;

  SB := TStringBuilder.Create;
  try
    for I := 0 to AArr.Elements.Count - 1 do
    begin
      if I > 0 then
        SB.Append(',');
      SB.Append(StringifyValue(AArr.Elements[I], AIndent + 1));
    end;
    Result := '[' + SB.ToString + ']';
  finally
    SB.Free;
  end;
end;

function TGocciaJSONStringifier.EscapeString(const AStr: string): string;
var
  SB: TStringBuilder;
  I: Integer;
  Ch: Char;
begin
  SB := TStringBuilder.Create;
  try
    for I := 1 to Length(AStr) do
    begin
      Ch := AStr[I];
      case Ch of
        '"': SB.Append('\"');
        '\': SB.Append('\\');
        '/': SB.Append('\/');
        #8: SB.Append('\b');
        #12: SB.Append('\f');
        #10: SB.Append('\n');
        #13: SB.Append('\r');
        #9: SB.Append('\t');
      else
        if Ord(Ch) < 32 then
          SB.Append('\u').Append(IntToHex(Ord(Ch), 4))
        else
          SB.Append(Ch);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.
