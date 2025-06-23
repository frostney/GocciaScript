unit Goccia.Builtins.JSON;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base,
  Goccia.Scope,
  Goccia.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.UndefinedValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.StringValue,
  Goccia.Values.NumberValue,
  Goccia.Values.ArrayValue,
  Generics.Collections,
  Goccia.Builtins.Base,
  SysUtils,
  Math;

type
  TGocciaJSON = class(TGocciaBuiltin)
  private
    // JSON Parser state
    FJsonText: string;
    FPosition: Integer;
    FLength: Integer;

    // Parser methods
    function ParseValue: TGocciaValue;
    function ParseObject: TGocciaObjectValue;
    function ParseArray: TGocciaArrayValue;
    function ParseString: TGocciaStringValue;
    function ParseNumber: TGocciaNumberValue;
    function ParseLiteral: TGocciaValue; // true, false, null

    // Parser utilities
    procedure SkipWhitespace;
    function PeekChar: Char;
    function ReadChar: Char;
    function ExpectChar(Expected: Char): Boolean; inline;
    function IsAtEnd: Boolean; inline;
    procedure RaiseParseError(const Message: string);

    // Stringifier methods
    function StringifyValue(Value: TGocciaValue; Indent: Integer = 0): string;
    function StringifyObject(Obj: TGocciaObjectValue; Indent: Integer): string;
    function StringifyArray(Arr: TGocciaArrayValue; Indent: Integer): string;
    function EscapeJsonString(const Str: string): string;
    function GetIndentString(Level: Integer): string;
  protected
    function JSONParse(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function JSONStringify(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

uses
  Goccia.Values.BooleanValue,
  Goccia.Values.NullValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.FunctionValue,
  Classes;

constructor TGocciaJSON.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONParse, 'parse', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONStringify, 'stringify', 1));

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaJSON.JSONParse(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('JSON.parse expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaStringValue) then
    ThrowError('JSON.parse expects a string argument', 0, 0);

  FJsonText := Args[0].ToString;
  FPosition := 1;
  FLength := Length(FJsonText);

  try
    SkipWhitespace;
    Result := ParseValue;
    SkipWhitespace;

    // Ensure we've consumed the entire input
    if not IsAtEnd then
      RaiseParseError('Unexpected character after JSON value');

  except
    on E: Exception do
      ThrowError('JSON.parse error: ' + E.Message, 0, 0);
  end;
end;

function TGocciaJSON.ParseValue: TGocciaValue;
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

function TGocciaJSON.ParseObject: TGocciaObjectValue;
var
  Key: string;
  Value: TGocciaValue;
  FirstProperty: Boolean;
begin
  Result := TGocciaObjectValue.Create;

  ExpectChar('{');
  SkipWhitespace;

  // Handle empty object
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

    // Parse key
    if PeekChar <> '"' then
      RaiseParseError('Expected string key in object');
    Key := ParseString.ToString;

    SkipWhitespace;
    if not ExpectChar(':') then
      RaiseParseError('Expected colon after object key');

    // Parse value
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

function TGocciaJSON.ParseArray: TGocciaArrayValue;
var
  Value: TGocciaValue;
  FirstElement: Boolean;
begin
  Result := TGocciaArrayValue.Create;

  ExpectChar('[');
  SkipWhitespace;

  // Handle empty array
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

function TGocciaJSON.ParseString: TGocciaStringValue;
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
      Result := TGocciaStringValue.Create(Str);
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
          // Unicode escape - simplified implementation
          Str := Str + '\u';
          // In a full implementation, we'd parse the 4 hex digits
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

function TGocciaJSON.ParseNumber: TGocciaNumberValue;
var
  NumStr: string;
  Ch: Char;
  Value: Double;
begin
  NumStr := '';

  // Handle negative sign
  if PeekChar = '-' then
    NumStr := NumStr + ReadChar;

  // Read integer part
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

  // Handle decimal part
  if not IsAtEnd and (PeekChar = '.') then
  begin
    NumStr := NumStr + ReadChar;
    if IsAtEnd or not (PeekChar in ['0'..'9']) then
      RaiseParseError('Invalid number format after decimal point');

    while not IsAtEnd and (PeekChar in ['0'..'9']) do
      NumStr := NumStr + ReadChar;
  end;

  // Handle exponential part
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
    Result := TGocciaNumberValue.Create(Value);
  except
    on E: Exception do
      RaiseParseError('Invalid number format: ' + NumStr);
  end;
end;

function TGocciaJSON.ParseLiteral: TGocciaValue;
begin
  case PeekChar of
    't': begin
      if (FPosition + 3 <= FLength) and
         (Copy(FJsonText, FPosition, 4) = 'true') then
      begin
        Inc(FPosition, 4);
        Result := TGocciaBooleanValue.Create(True);
      end
      else
        RaiseParseError('Invalid literal starting with t');
    end;
    'f': begin
      if (FPosition + 4 <= FLength) and
         (Copy(FJsonText, FPosition, 5) = 'false') then
      begin
        Inc(FPosition, 5);
        Result := TGocciaBooleanValue.Create(False);
      end
      else
        RaiseParseError('Invalid literal starting with f');
    end;
    'n': begin
      if (FPosition + 3 <= FLength) and
         (Copy(FJsonText, FPosition, 4) = 'null') then
      begin
        Inc(FPosition, 4);
        Result := TGocciaNullValue.Create;
      end
      else
        RaiseParseError('Invalid literal starting with n');
    end;
  else
    RaiseParseError('Unknown literal');
  end;
end;

{ Parser Utilities }

procedure TGocciaJSON.SkipWhitespace;
begin
  while not IsAtEnd and (PeekChar in [' ', #9, #10, #13]) do
    Inc(FPosition);
end;

function TGocciaJSON.PeekChar: Char;
begin
  if IsAtEnd then
    Result := #0
  else
    Result := FJsonText[FPosition];
end;

function TGocciaJSON.ReadChar: Char;
begin
  Result := PeekChar;
  Inc(FPosition);
end;

function TGocciaJSON.ExpectChar(Expected: Char): Boolean; inline;
begin
  SkipWhitespace;
  if IsAtEnd or (PeekChar <> Expected) then
  begin
    Result := False;
    Exit;
  end;
  ReadChar; // consume the expected character
  Result := True;
end;

function TGocciaJSON.IsAtEnd: Boolean; inline;
begin
  Result := FPosition > FLength;
end;

procedure TGocciaJSON.RaiseParseError(const Message: string);
begin
  raise Exception.Create(Message + ' at position ' + IntToStr(FPosition));
end;

{ Stringifier Implementation }

function TGocciaJSON.JSONStringify(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
  JsonString: string;
begin
  if Args.Count < 1 then
    ThrowError('JSON.stringify expects at least 1 argument', 0, 0);

  Value := Args[0];

  if Value is TGocciaUndefinedValue then
  begin
    Result := TGocciaUndefinedValue.Create;
    Exit;
  end;

  try
    JsonString := StringifyValue(Value);
    Result := TGocciaStringValue.Create(JsonString);
  except
    on E: Exception do
      ThrowError('JSON.stringify error: ' + E.Message, 0, 0);
  end;
end;

function TGocciaJSON.StringifyValue(Value: TGocciaValue; Indent: Integer): string;
begin
  if Value is TGocciaNullValue then
    Result := 'null'
  else if Value is TGocciaUndefinedValue then
    Result := 'null' // JSON doesn't have undefined
  else if Value is TGocciaBooleanValue then
  begin
    if Value.ToBoolean then
      Result := 'true'
    else
      Result := 'false';
  end
  else if Value is TGocciaNumberValue then
  begin
    // Handle special number values using TGocciaNumberValue properties
    if TGocciaNumberValue(Value).IsInfinity or TGocciaNumberValue(Value).IsNegativeInfinity then
      Result := 'null'
    else if TGocciaNumberValue(Value).IsNaN then
      Result := 'null'
    else
      Result := FloatToStr(Value.ToNumber);
  end
  else if Value is TGocciaStringValue then
    Result := '"' + EscapeJsonString(Value.ToString) + '"'
  else if Value is TGocciaArrayValue then
    Result := StringifyArray(TGocciaArrayValue(Value), Indent)
  else if Value is TGocciaObjectValue then
    Result := StringifyObject(TGocciaObjectValue(Value), Indent)
  else
    Result := 'null'; // Fallback for unsupported types
end;

function TGocciaJSON.StringifyObject(Obj: TGocciaObjectValue; Indent: Integer): string;
var
  Key: string;
  Value: TGocciaValue;
  Parts: string;
  HasProperties: Boolean;
begin
  Parts := '';
  HasProperties := False;

  for Key in Obj.GetEnumerablePropertyNames do
  begin
    Value := Obj.GetProperty(Key);

    // Skip undefined properties and functions
    if not (Value is TGocciaUndefinedValue) and
       (not (Value is TGocciaFunctionValue) or (Pos('Function', Value.ClassName) = 0)) then
    begin
      if HasProperties then
        Parts := Parts + ',';
      Parts := Parts + '"' + EscapeJsonString(Key) + '":' + StringifyValue(Value, Indent + 1);
      HasProperties := True;
    end;
  end;

  if not HasProperties then
    Result := '{}'
  else
    Result := '{' + Parts + '}';
end;

function TGocciaJSON.StringifyArray(Arr: TGocciaArrayValue; Indent: Integer): string;
var
  I: Integer;
  Parts: string;
begin
  Parts := '';
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    if I > 0 then
      Parts := Parts + ',';
    Parts := Parts + StringifyValue(Arr.Elements[I], Indent + 1);
  end;

  if Parts = '' then
    Result := '[]'
  else
    Result := '[' + Parts + ']';
end;

function TGocciaJSON.EscapeJsonString(const Str: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(Str) do
  begin
    Ch := Str[I];
    case Ch of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      #8: Result := Result + '\b';
      #12: Result := Result + '\f';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #9: Result := Result + '\t';
    else
      if Ord(Ch) < 32 then
        Result := Result + '\u' + IntToHex(Ord(Ch), 4)
      else
        Result := Result + Ch;
    end;
  end;
end;

function TGocciaJSON.GetIndentString(Level: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Level * 2 do // 2 spaces per level
    Result := Result + ' ';
end;

end.
