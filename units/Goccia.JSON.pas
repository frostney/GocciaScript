unit Goccia.JSON;

{$I Goccia.inc}

interface

uses
  Classes,
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
    function Parse(const AText: UTF8String): TGocciaValue; virtual;
    procedure ParseWithSources(const AText: UTF8String;
      out AValue: TGocciaValue; const ASourceTexts: TStringList);
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
  StringBuffer,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.RawJSON,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.WrapperPrimitives;

type
  TGocciaJSONVisitor = class(TAbstractJSONParser)
  private
    FCollectSources: Boolean;
    FKeyStack: TStringList;
    FResult: TGocciaValue;
    FSourceTexts: TStringList;
    FStack: TList<TGocciaValue>;
    FValueStartPosition: Integer;
    procedure RecordSourceText;
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
    procedure OnValueStart; override;
    procedure EmitValue(const AValue: TGocciaValue);
  public
    constructor Create;
    constructor Create(
      const ACapabilities: TJSONParserCapabilities);
    destructor Destroy; override;
    function Parse(const AText: UTF8String): TGocciaValue;
    property CollectSources: Boolean read FCollectSources write FCollectSources;
    property SourceTexts: TStringList read FSourceTexts;
  end;

const
  // IEEE-754 binary64 values need up to 17 significant decimal digits to
  // round-trip back to the same Double. This scientific pattern gives us
  // one digit before the decimal point and up to 16 after it.
  JSON_DOUBLE_ROUNDTRIP_SCIENTIFIC_FORMAT = '0.################E+00';
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
  JSON5_ZERO_WIDTH_NON_JOINER = #$E2#$80#$8C;
  JSON5_ZERO_WIDTH_JOINER = #$E2#$80#$8D;
  JSON5_LINE_SEPARATOR = #$E2#$80#$A8;
  JSON5_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
  JSON5_NARROW_NO_BREAK_SPACE = #$E2#$80#$AF;
  JSON5_MEDIUM_MATHEMATICAL_SPACE = #$E2#$81#$9F;
  JSON5_IDEOGRAPHIC_SPACE = #$E3#$80#$80;
  JSON5_BYTE_ORDER_MARK = #$EF#$BB#$BF;

function UTF8SequenceLengthFromLeadByte(const AChar: Char): Integer;
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
  Result := 0;
end;

function IsJSON5WhitespaceCodePoint(const ACodePoint: Cardinal): Boolean;
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

function TryReadUTF8Sequence(const AText: string; var AIndex: Integer;
  out ASequence: UTF8String): Boolean;
var
  I: Integer;
  SequenceLength: Integer;
begin
  ASequence := '';
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit(False);

  SequenceLength := UTF8SequenceLengthFromLeadByte(AText[AIndex]);
  if AIndex + SequenceLength - 1 > Length(AText) then
    Exit(False);

  ASequence := Copy(AText, AIndex, SequenceLength);
  if SequenceLength > 1 then
    for I := 2 to SequenceLength do
      if (Ord(ASequence[I]) and $C0) <> $80 then
        Exit(False);

  Inc(AIndex, SequenceLength);
  Result := True;
end;

function TryDecodeIdentifierCodePoint(const AText: string;
  out ACodePoint: Cardinal): Boolean;
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

function TGocciaJSONParser.Parse(const AText: UTF8String): TGocciaValue;
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

procedure TGocciaJSONParser.ParseWithSources(const AText: UTF8String;
  out AValue: TGocciaValue; const ASourceTexts: TStringList);
var
  Visitor: TGocciaJSONVisitor;
begin
  Visitor := TGocciaJSONVisitor.Create(FCapabilities);
  try
    Visitor.CollectSources := True;
    try
      AValue := Visitor.Parse(AText);
      ASourceTexts.Assign(Visitor.SourceTexts);
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
  FSourceTexts := TStringList.Create;
  FResult := nil;
  FCollectSources := False;
end;

destructor TGocciaJSONVisitor.Destroy;
begin
  FStack.Free;
  FKeyStack.Free;
  FSourceTexts.Free;
  inherited;
end;

function TGocciaJSONVisitor.Parse(const AText: UTF8String): TGocciaValue;
begin
  DoParse(AText);
  Result := FResult;
end;

procedure TGocciaJSONVisitor.OnValueStart;
begin
  FValueStartPosition := CurrentPosition;
end;

procedure TGocciaJSONVisitor.RecordSourceText;
begin
  if FCollectSources then
    FSourceTexts.Add(Copy(string(SourceTextData), FValueStartPosition,
      CurrentPosition - FValueStartPosition));
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
  RecordSourceText;
  EmitValue(TGocciaNullLiteralValue.NullValue);
end;

procedure TGocciaJSONVisitor.OnBoolean(const AValue: Boolean);
begin
  RecordSourceText;
  if AValue then
    EmitValue(TGocciaBooleanLiteralValue.TrueValue)
  else
    EmitValue(TGocciaBooleanLiteralValue.FalseValue);
end;

procedure TGocciaJSONVisitor.OnString(const AValue: string);
begin
  RecordSourceText;
  EmitValue(TGocciaStringLiteralValue.Create(AValue));
end;

procedure TGocciaJSONVisitor.OnInteger(const AValue: Int64);
begin
  RecordSourceText;
  EmitValue(TGocciaNumberLiteralValue.Create(AValue));
end;

procedure TGocciaJSONVisitor.OnFloat(const AValue: Double);
begin
  RecordSourceText;
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
var
  CodePoint: Cardinal;
begin
  if AText = '' then
    Exit(False);
  if not TryDecodeIdentifierCodePoint(AText, CodePoint) then
    Exit(False);
  if CodePoint <= $7F then
    Exit(Chr(CodePoint) in ['a'..'z', 'A'..'Z', '_', '$']);
  if (CodePoint = $200C) or (CodePoint = $200D) then
    Exit(False);
  Result := not IsJSON5WhitespaceCodePoint(CodePoint);
end;

class function TGocciaJSONStringifier.IsIdentifierContinueText(
  const AText: string): Boolean;
var
  CodePoint: Cardinal;
begin
  if AText = '' then
    Exit(False);
  if not TryDecodeIdentifierCodePoint(AText, CodePoint) then
    Exit(False);
  if CodePoint <= $7F then
    Exit(Chr(CodePoint) in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']);
  if (CodePoint = $200C) or (CodePoint = $200D) then
    Exit(True);
  Result := not IsJSON5WhitespaceCodePoint(CodePoint);
end;

function TGocciaJSONStringifier.IsJSON5IdentifierKey(const AKey: string): Boolean;
var
  I: Integer;
  Sequence: UTF8String;
begin
  if AKey = '' then
    Exit(False);

  I := 1;
  if not TryReadUTF8Sequence(AKey, I, Sequence) or
    not IsIdentifierStartText(Sequence) then
    Exit(False);

  while I <= Length(AKey) do
  begin
    if not TryReadUTF8Sequence(AKey, I, Sequence) or
      not IsIdentifierContinueText(Sequence) then
      Exit(False);
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
  // ES2026 §25.5.2.2 step 4a: If value has [[IsRawJSON]], return its raw text verbatim.
  if AValue is TGocciaRawJSONValue then
    Exit(TGocciaRawJSONValue(AValue).RawText);

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
