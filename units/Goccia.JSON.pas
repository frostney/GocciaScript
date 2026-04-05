unit Goccia.JSON;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  JSONParser,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  EGocciaJSONParseError = class(Exception);
  TJSONStringifyMode = (jsmJSON, jsmJSON5);

  TGocciaJSONParser = class
  private
    FCapabilities: TJSONParserCapabilities;
  public
    constructor Create; overload;
    constructor Create(
      const ACapabilities: TJSONParserCapabilities); overload;
    function Parse(const AText: string): TGocciaValue; virtual;
  end;

  TGocciaJSONStringifier = class
  private
    FGap: string;
    FMode: TJSONStringifyMode;
    FPreferredQuoteChar: Char;
    FTraversalStack: TList<TGocciaObjectValue>;
    class function IsIdentifierContinueText(const AText: string): Boolean; static;
    class function IsIdentifierStartText(const AText: string): Boolean; static;
    function ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
    function ChooseQuoteChar(const AStr: string): Char;
    function CircularErrorName: string;
    function EscapeJSONString(const AStr: string): string;
    function EscapeJSON5String(const AStr: string; const AQuote: Char): string;
    function IsJSON5IdentifierKey(const AKey: string): Boolean;
    function ShouldOmitObjectProperty(const AValue: TGocciaValue): Boolean;
    function QuoteJSON5String(const AStr: string): string;
    function SerializeObjectKey(const AKey: string): string;
    function StringifyPreparedValue(const AValue: TGocciaValue; const AIndent: Integer = 0): string;
    function StringifyValue(const AValue: TGocciaValue; const AIndent: Integer = 0; const AKey: string = ''): string;
    function StringifyObject(const AObj: TGocciaObjectValue; const AIndent: Integer): string;
    function StringifyArray(const AArr: TGocciaArrayValue; const AIndent: Integer): string;
    function MakeIndent(const ALevel: Integer): string;
  public
    constructor Create; overload;
    constructor Create(const AMode: TJSONStringifyMode); overload;
    function Stringify(const AValue: TGocciaValue; const AGap: string = '';
      const APreferredQuoteChar: Char = #0): string;
  end;

implementation

uses
  Classes,

  StringBuffer,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionValue,
  Goccia.Values.HoleValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.WrapperPrimitives;

type
  TGocciaJSONVisitor = class(TAbstractJSONParser)
  private
    FStack: TList<TGocciaValue>;
    FKeyStack: TStringList;
    FResult: TGocciaValue;
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
    procedure EmitValue(const AValue: TGocciaValue);
  public
    constructor Create;
    constructor Create(
      const ACapabilities: TJSONParserCapabilities);
    destructor Destroy; override;
    function Parse(const AText: string): TGocciaValue;
  end;

const
  // IEEE-754 binary64 values need up to 17 significant decimal digits to
  // round-trip back to the same Double. This scientific pattern gives us
  // one digit before the decimal point and up to 16 after it.
  JSON_DOUBLE_ROUNDTRIP_SCIENTIFIC_FORMAT = '0.################E+00';

function SameDoubleBits(const ALeft, ARight: Double): Boolean;
var
  LeftValue, RightValue: Double;
  LeftBits: Int64 absolute LeftValue;
  RightBits: Int64 absolute RightValue;
begin
  LeftValue := ALeft;
  RightValue := ARight;
  Result := LeftBits = RightBits;
end;

function TrimTrailingFractionalZeros(const AValue: string): string;
begin
  Result := AValue;
  while (Pos('.', Result) > 0) and (Length(Result) > 0) and (Result[Length(Result)] = '0') do
    Delete(Result, Length(Result), 1);
  if (Length(Result) > 0) and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
end;

function NormalizeExponentNumber(const AValue: string): string;
var
  ExponentIndex, SignIndex: Integer;
  Mantissa, ExponentPart: string;
begin
  ExponentIndex := Pos('E', AValue);
  if ExponentIndex = 0 then
  begin
    Result := TrimTrailingFractionalZeros(AValue);
    Exit;
  end;

  Mantissa := TrimTrailingFractionalZeros(Copy(AValue, 1, ExponentIndex - 1));
  ExponentPart := Copy(AValue, ExponentIndex + 1, MaxInt);

  if (Length(ExponentPart) > 0) and ((ExponentPart[1] = '+') or (ExponentPart[1] = '-')) then
    SignIndex := 2
  else
    SignIndex := 1;

  while (Length(ExponentPart) > SignIndex) and (ExponentPart[SignIndex] = '0') do
    Delete(ExponentPart, SignIndex, 1);

  Result := Mantissa + 'e' + ExponentPart;
end;

function NumericStringRoundTrips(const ASerialized: string; const AValue: Double): Boolean;
var
  ParsedValue: Double;
begin
  if not TryStrToFloat(ASerialized, ParsedValue, DefaultFormatSettings) then
    Exit(False);
  Result := SameDoubleBits(ParsedValue, AValue);
end;

function SerializeJSONNumber(const AValue: Double): string;
begin
  if AValue = 0 then
    Exit('0');

  Result := FloatToStr(AValue, DefaultFormatSettings);
  if NumericStringRoundTrips(Result, AValue) then
    Exit;

  Result := NormalizeExponentNumber(
    FormatFloat(JSON_DOUBLE_ROUNDTRIP_SCIENTIFIC_FORMAT, AValue, DefaultFormatSettings));
end;

procedure AppendEscapedChar(var ASB: TStringBuffer; const AValue: Char);
begin
  ASB.AppendChar(#92);
  ASB.AppendChar(AValue);
end;

{ TGocciaJSONParser }

constructor TGocciaJSONParser.Create;
begin
  Create(JSONParserStrictCapabilities);
end;

constructor TGocciaJSONParser.Create(
  const ACapabilities: TJSONParserCapabilities);
begin
  inherited Create;
  FCapabilities := ACapabilities;
end;

function TGocciaJSONParser.Parse(const AText: string): TGocciaValue;
var
  Visitor: TGocciaJSONVisitor;
begin
  Visitor := TGocciaJSONVisitor.Create(FCapabilities);
  try
    try
      Result := Visitor.Parse(AText);
    except
      on E: EJSONParseError do
        raise EGocciaJSONParseError.Create(E.Message);
    end;
  finally
    Visitor.Free;
  end;
end;

{ TGocciaJSONVisitor }

constructor TGocciaJSONVisitor.Create;
begin
  Create(JSONParserStrictCapabilities);
end;

constructor TGocciaJSONVisitor.Create(
  const ACapabilities: TJSONParserCapabilities);
begin
  inherited Create(ACapabilities);
  FStack := TList<TGocciaValue>.Create;
  FKeyStack := TStringList.Create;
  FResult := nil;
end;

destructor TGocciaJSONVisitor.Destroy;
begin
  FStack.Free;
  FKeyStack.Free;
  inherited;
end;

function TGocciaJSONVisitor.Parse(const AText: string): TGocciaValue;
begin
  DoParse(AText);
  Result := FResult;
end;

procedure TGocciaJSONVisitor.EmitValue(const AValue: TGocciaValue);
var
  Container: TGocciaValue;
  Key: string;
begin
  if FStack.Count = 0 then
    FResult := AValue
  else
  begin
    Container := FStack[FStack.Count - 1];
    if Container is TGocciaArrayValue then
      TGocciaArrayValue(Container).Elements.Add(AValue)
    else if Container is TGocciaObjectValue then
    begin
      Key := FKeyStack[FKeyStack.Count - 1];
      FKeyStack.Delete(FKeyStack.Count - 1);
      TGocciaObjectValue(Container).AssignProperty(Key, AValue);
    end;
  end;
end;

procedure TGocciaJSONVisitor.OnNull;
begin
  EmitValue(TGocciaNullLiteralValue.NullValue);
end;

procedure TGocciaJSONVisitor.OnBoolean(const AValue: Boolean);
begin
  if AValue then
    EmitValue(TGocciaBooleanLiteralValue.TrueValue)
  else
    EmitValue(TGocciaBooleanLiteralValue.FalseValue);
end;

procedure TGocciaJSONVisitor.OnString(const AValue: string);
begin
  EmitValue(TGocciaStringLiteralValue.Create(AValue));
end;

procedure TGocciaJSONVisitor.OnInteger(const AValue: Int64);
begin
  EmitValue(TGocciaNumberLiteralValue.Create(AValue * 1.0));
end;

procedure TGocciaJSONVisitor.OnFloat(const AValue: Double);
begin
  EmitValue(TGocciaNumberLiteralValue.Create(AValue));
end;

procedure TGocciaJSONVisitor.OnBeginObject;
begin
  FStack.Add(TGocciaObjectValue.Create);
end;

procedure TGocciaJSONVisitor.OnObjectKey(const AKey: string);
begin
  FKeyStack.Add(AKey);
end;

procedure TGocciaJSONVisitor.OnEndObject;
var
  Val: TGocciaValue;
begin
  Val := FStack[FStack.Count - 1];
  FStack.Delete(FStack.Count - 1);
  EmitValue(Val);
end;

procedure TGocciaJSONVisitor.OnBeginArray;
begin
  FStack.Add(TGocciaArrayValue.Create);
end;

procedure TGocciaJSONVisitor.OnEndArray;
var
  Val: TGocciaValue;
begin
  Val := FStack[FStack.Count - 1];
  FStack.Delete(FStack.Count - 1);
  EmitValue(Val);
end;

{ TGocciaJSONStringifier }

constructor TGocciaJSONStringifier.Create;
begin
  Create(jsmJSON);
end;

constructor TGocciaJSONStringifier.Create(const AMode: TJSONStringifyMode);
begin
  inherited Create;
  FMode := AMode;
end;

function TGocciaJSONStringifier.Stringify(const AValue: TGocciaValue;
  const AGap: string; const APreferredQuoteChar: Char): string;
var
  PreviousGap: string;
  PreviousPreferredQuoteChar: Char;
  PreviousTraversalStack: TList<TGocciaObjectValue>;
begin
  PreviousGap := FGap;
  PreviousPreferredQuoteChar := FPreferredQuoteChar;
  PreviousTraversalStack := FTraversalStack;

  FGap := AGap;
  FPreferredQuoteChar := APreferredQuoteChar;
  FTraversalStack := TList<TGocciaObjectValue>.Create;
  try
    Result := StringifyValue(AValue);
  finally
    FTraversalStack.Free;
    FTraversalStack := PreviousTraversalStack;
    FPreferredQuoteChar := PreviousPreferredQuoteChar;
    FGap := PreviousGap;
  end;
end;

function TGocciaJSONStringifier.CircularErrorName: string;
begin
  if FMode = jsmJSON5 then
    Result := 'Converting circular structure to JSON5'
  else
    Result := 'Converting circular structure to JSON';
end;

function TGocciaJSONStringifier.MakeIndent(const ALevel: Integer): string;
var
  I: Integer;
begin
  Result := '';
  if FGap = '' then
    Exit;
  for I := 1 to ALevel do
    Result := Result + FGap;
end;

// ES2026 §25.5.2.2 SerializeJSONProperty ( state, key, holder )
function TGocciaJSONStringifier.ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
var
  MethodName: string;
  ToJSONMethod: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  Result := AValue;
  if not (AValue is TGocciaObjectValue) then
    Exit;

  if FMode = jsmJSON5 then
  begin
    MethodName := PROP_TO_JSON5;
    ToJSONMethod := TGocciaObjectValue(AValue).GetProperty(MethodName);
    if not Assigned(ToJSONMethod) or not ToJSONMethod.IsCallable then
    begin
      MethodName := PROP_TO_JSON;
      ToJSONMethod := TGocciaObjectValue(AValue).GetProperty(MethodName);
    end;
  end
  else
  begin
    MethodName := PROP_TO_JSON;
    ToJSONMethod := TGocciaObjectValue(AValue).GetProperty(MethodName);
  end;

  if not Assigned(ToJSONMethod) or not ToJSONMethod.IsCallable then
    Exit;

  Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    Args.Add(TGocciaStringLiteralValue.Create(AKey));
    Result := InvokeCallable(ToJSONMethod, Args, AValue);
  finally
    Args.Free;
  end;
end;

function TGocciaJSONStringifier.ShouldOmitObjectProperty(const AValue: TGocciaValue): Boolean;
begin
  Result := (AValue is TGocciaUndefinedLiteralValue) or
            AValue.IsCallable or
            (AValue is TGocciaSymbolValue);
end;

class function TGocciaJSONStringifier.IsIdentifierStartText(
  const AText: string): Boolean;
begin
  if AText = '' then
    Exit(False);
  if Length(AText) > 1 then
    Exit(True);
  Result := (AText[1] in ['a'..'z', 'A'..'Z', '_', '$']) or
    (Ord(AText[1]) >= 128);
end;

class function TGocciaJSONStringifier.IsIdentifierContinueText(
  const AText: string): Boolean;
begin
  if AText = '' then
    Exit(False);
  if Length(AText) > 1 then
    Exit(True);
  Result := (AText[1] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) or
    (Ord(AText[1]) >= 128);
end;

function TGocciaJSONStringifier.IsJSON5IdentifierKey(const AKey: string): Boolean;
var
  I: Integer;
begin
  if AKey = '' then
    Exit(False);

  if not IsIdentifierStartText(AKey[1]) then
    Exit(False);

  I := 2;
  while I <= Length(AKey) do
  begin
    if not IsIdentifierContinueText(AKey[I]) then
      Exit(False);
    Inc(I);
  end;

  Result := True;
end;

function TGocciaJSONStringifier.ChooseQuoteChar(const AStr: string): Char;
var
  DoubleCount, SingleCount, I: Integer;
begin
  if FPreferredQuoteChar in ['''', '"'] then
    Exit(FPreferredQuoteChar);

  SingleCount := 0;
  DoubleCount := 0;
  for I := 1 to Length(AStr) do
    case AStr[I] of
      '''':
        Inc(SingleCount);
      '"':
        Inc(DoubleCount);
    end;

  if SingleCount <= DoubleCount then
    Result := ''''
  else
    Result := '"';
end;

function TGocciaJSONStringifier.EscapeJSONString(const AStr: string): string;
var
  Ch: Char;
  I: Integer;
  SB: TStringBuffer;
begin
  SB := TStringBuffer.Create(Length(AStr));
  try
    for I := 1 to Length(AStr) do
    begin
      Ch := AStr[I];
      case Ch of
        '"': SB.Append('\"');
        '\': SB.Append('\\');
        '/': SB.Append('\/');
        #8: AppendEscapedChar(SB, 'b');
        #12: AppendEscapedChar(SB, 'f');
        #10: AppendEscapedChar(SB, 'n');
        #13: AppendEscapedChar(SB, 'r');
        #9: AppendEscapedChar(SB, 't');
      else
        if Ord(Ch) < 32 then
        begin
          SB.Append('\u');
          SB.Append(IntToHex(Ord(Ch), 4));
        end
        else
          SB.AppendChar(Ch);
      end;
    end;
    Result := SB.ToString;
  finally
  end;
end;

function TGocciaJSONStringifier.EscapeJSON5String(const AStr: string;
  const AQuote: Char): string;
var
  Ch: Char;
  I: Integer;
  SB: TStringBuffer;
begin
  SB := TStringBuffer.Create(Length(AStr));
  try
    I := 1;
    while I <= Length(AStr) do
    begin
      Ch := AStr[I];
      if Copy(AStr, I, 3) = #$E2#$80#$A8 then
      begin
        SB.Append('\u2028');
        Inc(I, 3);
        Continue;
      end;
      if Copy(AStr, I, 3) = #$E2#$80#$A9 then
      begin
        SB.Append('\u2029');
        Inc(I, 3);
        Continue;
      end;

      case Ch of
        '\':
          SB.Append('\\');
        #8:
          AppendEscapedChar(SB, 'b');
        #12:
          AppendEscapedChar(SB, 'f');
        #10:
          AppendEscapedChar(SB, 'n');
        #13:
          AppendEscapedChar(SB, 'r');
        #9:
          AppendEscapedChar(SB, 't');
        #11:
          AppendEscapedChar(SB, 'v');
        #0:
          begin
            if (I < Length(AStr)) and (AStr[I + 1] in ['0'..'9']) then
              SB.Append('\x00')
            else
              SB.Append('\0');
          end;
        '''', '"':
          if Ch = AQuote then
          begin
            AppendEscapedChar(SB, Ch);
          end
          else
            SB.AppendChar(Ch);
      else
        if Ord(Ch) < 32 then
        begin
          SB.Append('\x');
          SB.Append(IntToHex(Ord(Ch), 2));
        end
        else
          SB.AppendChar(Ch);
      end;
      Inc(I);
    end;
    Result := SB.ToString;
  finally
  end;
end;

function TGocciaJSONStringifier.QuoteJSON5String(const AStr: string): string;
var
  QuoteChar: Char;
begin
  QuoteChar := ChooseQuoteChar(AStr);
  Result := QuoteChar + EscapeJSON5String(AStr, QuoteChar) + QuoteChar;
end;

function TGocciaJSONStringifier.SerializeObjectKey(const AKey: string): string;
begin
  if (FMode = jsmJSON5) and IsJSON5IdentifierKey(AKey) then
    Result := AKey
  else if FMode = jsmJSON5 then
    Result := QuoteJSON5String(AKey)
  else
    Result := '"' + EscapeJSONString(AKey) + '"';
end;

function TGocciaJSONStringifier.StringifyValue(const AValue: TGocciaValue;
  const AIndent: Integer; const AKey: string): string;
begin
  Result := StringifyPreparedValue(ApplyToJSON(AValue, AKey), AIndent);
end;

function TGocciaJSONStringifier.StringifyPreparedValue(const AValue: TGocciaValue;
  const AIndent: Integer): string;
var
  EffectiveValue: TGocciaValue;
begin
  EffectiveValue := UnboxWrappedPrimitive(AValue);

  if EffectiveValue is TGocciaNullLiteralValue then
    Result := 'null'
  else if EffectiveValue is TGocciaUndefinedLiteralValue then
    Result := 'null'
  else if EffectiveValue is TGocciaHoleValue then
    Result := 'null'
  else if EffectiveValue is TGocciaBooleanLiteralValue then
  begin
    if EffectiveValue.ToBooleanLiteral.Value then
      Result := 'true'
    else
      Result := 'false';
  end
  else if EffectiveValue is TGocciaNumberLiteralValue then
  begin
    if TGocciaNumberLiteralValue(EffectiveValue).IsInfinity then
    begin
      if FMode = jsmJSON5 then
        Result := 'Infinity'
      else
        Result := 'null';
    end
    else if TGocciaNumberLiteralValue(EffectiveValue).IsNegativeInfinity then
    begin
      if FMode = jsmJSON5 then
        Result := '-Infinity'
      else
        Result := 'null';
    end
    else if TGocciaNumberLiteralValue(EffectiveValue).IsNaN then
    begin
      if FMode = jsmJSON5 then
        Result := 'NaN'
      else
        Result := 'null';
    end
    else
      Result := SerializeJSONNumber(EffectiveValue.ToNumberLiteral.Value);
  end
  else if EffectiveValue is TGocciaStringLiteralValue then
  begin
    if FMode = jsmJSON5 then
      Result := QuoteJSON5String(EffectiveValue.ToStringLiteral.Value)
    else
      Result := '"' + EscapeJSONString(EffectiveValue.ToStringLiteral.Value) + '"';
  end
  else if EffectiveValue.IsCallable then
    Result := 'null'
  else if EffectiveValue is TGocciaSymbolValue then
    Result := 'null'
  else if EffectiveValue is TGocciaArrayValue then
    Result := StringifyArray(TGocciaArrayValue(EffectiveValue), AIndent)
  else if EffectiveValue is TGocciaObjectValue then
    Result := StringifyObject(TGocciaObjectValue(EffectiveValue), AIndent)
  else
    Result := 'null';
end;

function TGocciaJSONStringifier.StringifyObject(const AObj: TGocciaObjectValue; const AIndent: Integer): string;
var
  SB: TStringBuffer;
  Key: string;
  Value: TGocciaValue;
  HasProperties: Boolean;
  Separator, ChildIndent, CloseIndent: string;
begin
  if FTraversalStack.IndexOf(AObj) <> -1 then
    ThrowTypeError(CircularErrorName);

  FTraversalStack.Add(AObj);
  try
    SB := TStringBuffer.Create;
    try
      HasProperties := False;

      if FGap <> '' then
      begin
        Separator := ',' + #10;
        ChildIndent := MakeIndent(AIndent + 1);
        CloseIndent := MakeIndent(AIndent);
      end
      else
      begin
        Separator := ',';
        ChildIndent := '';
        CloseIndent := '';
      end;

      for Key in AObj.GetEnumerablePropertyNames do
      begin
        Value := ApplyToJSON(AObj.GetProperty(Key), Key);
        if ShouldOmitObjectProperty(Value) then
          Continue;

        if HasProperties then
          SB.Append(Separator);
        SB.Append(ChildIndent);
        SB.Append(SerializeObjectKey(Key));
        SB.Append(':');
        if FGap <> '' then
          SB.AppendChar(' ');
        SB.Append(StringifyPreparedValue(Value, AIndent + 1));
        HasProperties := True;
      end;

      if not HasProperties then
        Result := '{}'
      else if FGap <> '' then
      begin
        if FMode = jsmJSON5 then
          Result := '{' + #10 + SB.ToString + ',' + #10 + CloseIndent + '}'
        else
          Result := '{' + #10 + SB.ToString + #10 + CloseIndent + '}';
      end
      else
        Result := '{' + SB.ToString + '}';
    finally
    end;
  finally
    FTraversalStack.Delete(FTraversalStack.Count - 1);
  end;
end;

function TGocciaJSONStringifier.StringifyArray(const AArr: TGocciaArrayValue; const AIndent: Integer): string;
var
  SB: TStringBuffer;
  I: Integer;
  Value: TGocciaValue;
  Separator, ChildIndent, CloseIndent: string;
begin
  if FTraversalStack.IndexOf(AArr) <> -1 then
    ThrowTypeError(CircularErrorName);

  FTraversalStack.Add(AArr);
  if AArr.Elements.Count = 0 then
  begin
    Result := '[]';
    FTraversalStack.Delete(FTraversalStack.Count - 1);
    Exit;
  end;

  try
    if FGap <> '' then
    begin
      Separator := ',' + #10;
      ChildIndent := MakeIndent(AIndent + 1);
      CloseIndent := MakeIndent(AIndent);
    end
    else
    begin
      Separator := ',';
      ChildIndent := '';
      CloseIndent := '';
    end;

    SB := TStringBuffer.Create;
    try
      for I := 0 to AArr.Elements.Count - 1 do
      begin
        if I > 0 then
          SB.Append(Separator);
        SB.Append(ChildIndent);
        Value := ApplyToJSON(AArr.Elements[I], IntToStr(I));
        SB.Append(StringifyPreparedValue(Value, AIndent + 1));
      end;
      if FGap <> '' then
      begin
        if FMode = jsmJSON5 then
          Result := '[' + #10 + SB.ToString + ',' + #10 + CloseIndent + ']'
        else
          Result := '[' + #10 + SB.ToString + #10 + CloseIndent + ']';
      end
      else
        Result := '[' + SB.ToString + ']';
    finally
    end;
  finally
    FTraversalStack.Delete(FTraversalStack.Count - 1);
  end;
end;

end.
