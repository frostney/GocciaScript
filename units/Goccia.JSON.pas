unit Goccia.JSON;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  EGocciaJSONParseError = class(Exception);

  TGocciaJSONParser = class
  public
    function Parse(const AText: string): TGocciaValue;
  end;

  TGocciaJSONStringifier = class
  private
    FGap: string;
    FTraversalStack: TList<TGocciaObjectValue>;
    function ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
    function ShouldOmitObjectProperty(const AValue: TGocciaValue): Boolean;
    function StringifyPreparedValue(const AValue: TGocciaValue; const AIndent: Integer = 0): string;
    function StringifyValue(const AValue: TGocciaValue; const AIndent: Integer = 0; const AKey: string = ''): string;
    function StringifyObject(const AObj: TGocciaObjectValue; const AIndent: Integer): string;
    function StringifyArray(const AArr: TGocciaArrayValue; const AIndent: Integer): string;
    function EscapeString(const AStr: string): string;
    function MakeIndent(const ALevel: Integer): string;
  public
    function Stringify(const AValue: TGocciaValue; const AGap: string = ''): string;
  end;

implementation

uses
  Classes,

  JSONParser,
  StringBuffer,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionValue,
  Goccia.Values.HoleValue,
  Goccia.Values.SymbolValue;

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

{ TGocciaJSONParser }

function TGocciaJSONParser.Parse(const AText: string): TGocciaValue;
var
  Visitor: TGocciaJSONVisitor;
begin
  Visitor := TGocciaJSONVisitor.Create;
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
  inherited Create;
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

function TGocciaJSONStringifier.Stringify(const AValue: TGocciaValue; const AGap: string): string;
begin
  FGap := AGap;
  FTraversalStack := TList<TGocciaObjectValue>.Create;
  try
    Result := StringifyValue(AValue);
  finally
    FTraversalStack.Free;
    FTraversalStack := nil;
  end;
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
  ToJSONMethod: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  Result := AValue;
  if not (AValue is TGocciaObjectValue) then
    Exit;

  ToJSONMethod := TGocciaObjectValue(AValue).GetProperty(PROP_TO_JSON);
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

function TGocciaJSONStringifier.StringifyValue(const AValue: TGocciaValue;
  const AIndent: Integer; const AKey: string): string;
begin
  Result := StringifyPreparedValue(ApplyToJSON(AValue, AKey), AIndent);
end;

function TGocciaJSONStringifier.StringifyPreparedValue(const AValue: TGocciaValue;
  const AIndent: Integer): string;
begin
  if AValue is TGocciaNullLiteralValue then
    Result := 'null'
  else if AValue is TGocciaUndefinedLiteralValue then
    Result := 'null'
  else if AValue is TGocciaHoleValue then
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
      Result := SerializeJSONNumber(AValue.ToNumberLiteral.Value);
  end
  else if AValue is TGocciaStringLiteralValue then
    Result := '"' + EscapeString(AValue.ToStringLiteral.Value) + '"'
  else if AValue.IsCallable then
    Result := 'null'
  else if AValue is TGocciaSymbolValue then
    Result := 'null'
  else if AValue is TGocciaArrayValue then
    Result := StringifyArray(TGocciaArrayValue(AValue), AIndent)
  else if AValue is TGocciaObjectValue then
    Result := StringifyObject(TGocciaObjectValue(AValue), AIndent)
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
    ThrowTypeError('Converting circular structure to JSON');

  FTraversalStack.Add(AObj);
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
      SB.AppendChar('"');
      SB.Append(EscapeString(Key));
      SB.Append('":');
      if FGap <> '' then
        SB.AppendChar(' ');
      SB.Append(StringifyPreparedValue(Value, AIndent + 1));
      HasProperties := True;
    end;

    if not HasProperties then
      Result := '{}'
    else if FGap <> '' then
      Result := '{' + #10 + SB.ToString + #10 + CloseIndent + '}'
    else
      Result := '{' + SB.ToString + '}';
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
    ThrowTypeError('Converting circular structure to JSON');

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
    for I := 0 to AArr.Elements.Count - 1 do
    begin
      if I > 0 then
        SB.Append(Separator);
      SB.Append(ChildIndent);
      Value := ApplyToJSON(AArr.Elements[I], IntToStr(I));
      SB.Append(StringifyPreparedValue(Value, AIndent + 1));
    end;
    if FGap <> '' then
      Result := '[' + #10 + SB.ToString + #10 + CloseIndent + ']'
    else
      Result := '[' + SB.ToString + ']';
  finally
    FTraversalStack.Delete(FTraversalStack.Count - 1);
  end;
end;

function TGocciaJSONStringifier.EscapeString(const AStr: string): string;
var
  SB: TStringBuffer;
  I: Integer;
  Ch: Char;
begin
  SB := TStringBuffer.Create(Length(AStr));
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
      begin
        SB.Append('\u');
        SB.Append(IntToHex(Ord(Ch), 4));
      end
      else
        SB.AppendChar(Ch);
    end;
  end;
  Result := SB.ToString;
end;

end.
