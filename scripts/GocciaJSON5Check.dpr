program GocciaJSON5Check;

{$I units/Goccia.inc}

uses
  Classes,
  Math,
  SysUtils,

  GarbageCollector.Generic,
  StringBuffer,

  Goccia.JSON5,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

function EscapeJSONString(const AValue: string): string;
var
  C: Char;
begin
  Result := '';
  for C in AValue do
    case C of
      '"':
        Result := Result + '\"';
      '\':
        Result := Result + '\\';
      #8:
        Result := Result + '\b';
      #9:
        Result := Result + '\t';
      #10:
        Result := Result + '\n';
      #12:
        Result := Result + '\f';
      #13:
        Result := Result + '\r';
    else
      if Ord(C) < 32 then
        Result := Result + '\u' + IntToHex(Ord(C), 4)
      else
        Result := Result + C;
    end;
end;

function QuoteJSONString(const AValue: string): string;
begin
  Result := '"' + EscapeJSONString(AValue) + '"';
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
  while (Pos('.', Result) > 0) and (Length(Result) > 0) and
    (Result[Length(Result)] = '0') do
    Delete(Result, Length(Result), 1);
  if (Length(Result) > 0) and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
end;

function NormalizeExponentNumber(const AValue: string): string;
var
  ExponentIndex, SignIndex: Integer;
  ExponentPart, Mantissa: string;
begin
  ExponentIndex := Pos('E', AValue);
  if ExponentIndex = 0 then
  begin
    Result := TrimTrailingFractionalZeros(AValue);
    Exit;
  end;

  Mantissa := TrimTrailingFractionalZeros(Copy(AValue, 1, ExponentIndex - 1));
  ExponentPart := Copy(AValue, ExponentIndex + 1, MaxInt);

  if (Length(ExponentPart) > 0) and
     ((ExponentPart[1] = '+') or (ExponentPart[1] = '-')) then
    SignIndex := 2
  else
    SignIndex := 1;

  while (Length(ExponentPart) > SignIndex) and
    (ExponentPart[SignIndex] = '0') do
    Delete(ExponentPart, SignIndex, 1);

  Result := Mantissa + 'e' + ExponentPart;
end;

function NumericStringRoundTrips(const ASerialized: string;
  const AValue: Double): Boolean;
var
  FloatFormat: TFormatSettings;
  ParsedValue: Double;
begin
  FloatFormat := DefaultFormatSettings;
  FloatFormat.DecimalSeparator := '.';
  if not TryStrToFloat(ASerialized, ParsedValue, FloatFormat) then
    Exit(False);
  Result := SameDoubleBits(ParsedValue, AValue);
end;

function SerializeJSONNumber(const AValue: Double): string;
const
  JSON_DOUBLE_ROUNDTRIP_SCIENTIFIC_FORMAT = '0.################E+00';
var
  FloatFormat: TFormatSettings;
begin
  FloatFormat := DefaultFormatSettings;
  FloatFormat.DecimalSeparator := '.';
  if AValue = 0 then
    Exit('0');

  Result := FloatToStr(AValue, FloatFormat);
  if NumericStringRoundTrips(Result, AValue) then
    Exit;

  Result := NormalizeExponentNumber(
    FormatFloat(JSON_DOUBLE_ROUNDTRIP_SCIENTIFIC_FORMAT, AValue,
      FloatFormat));
end;

function EncodeNumber(const AValue: TGocciaNumberLiteralValue): string;
begin
  if AValue.IsNaN then
    Exit('{"type":"number","value":"NaN"}');
  if AValue.IsInfinity then
    Exit('{"type":"number","value":"Infinity"}');
  if AValue.IsNegativeInfinity then
    Exit('{"type":"number","value":"-Infinity"}');
  if AValue.IsNegativeZero then
    Exit('{"type":"number","value":"-0"}');
  Result := '{"type":"number","value":' +
    QuoteJSONString(SerializeJSONNumber(AValue.Value)) + '}';
end;

function EncodeValue(const AValue: TGocciaValue): string;
var
  Buffer: TStringBuffer;
  Element: TGocciaValue;
  I: Integer;
  Key: string;
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  if AValue is TGocciaNullLiteralValue then
    Exit('{"type":"null"}');
  if AValue is TGocciaBooleanLiteralValue then
  begin
    if AValue.ToBooleanLiteral.Value then
      Exit('{"type":"boolean","value":true}');
    Exit('{"type":"boolean","value":false}');
  end;
  if AValue is TGocciaStringLiteralValue then
    Exit('{"type":"string","value":' +
      QuoteJSONString(AValue.ToStringLiteral.Value) + '}');
  if AValue is TGocciaNumberLiteralValue then
  begin
    NumberLiteral := AValue.ToNumberLiteral;
    Exit(EncodeNumber(NumberLiteral));
  end;
  if AValue is TGocciaArrayValue then
  begin
    Buffer := TStringBuffer.Create;
    Buffer.Append('{"type":"array","items":[');
    for I := 0 to TGocciaArrayValue(AValue).Elements.Count - 1 do
    begin
      if I > 0 then
        Buffer.Append(',');
      Element := TGocciaArrayValue(AValue).Elements[I];
      Buffer.Append(EncodeValue(Element));
    end;
    Buffer.Append(']}');
    Exit(Buffer.ToString);
  end;
  if AValue is TGocciaObjectValue then
  begin
    Buffer := TStringBuffer.Create;
    Buffer.Append('{"type":"object","entries":[');
    I := 0;
    for Key in TGocciaObjectValue(AValue).GetOwnPropertyKeys do
    begin
      if I > 0 then
        Buffer.Append(',');
      Buffer.Append('{"key":');
      Buffer.Append(QuoteJSONString(Key));
      Buffer.Append(',"value":');
      Buffer.Append(EncodeValue(TGocciaObjectValue(AValue).GetProperty(Key)));
      Buffer.Append('}');
      Inc(I);
    end;
    Buffer.Append(']}');
    Exit(Buffer.ToString);
  end;

  raise Exception.Create('Unsupported JSON5 value type in compliance harness');
end;

var
  ParsedValue: TGocciaValue;
  Parser: TGocciaJSON5Parser;
  Source: TStringList;
begin
  if ParamCount <> 1 then
    Halt(2);

  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;

  Source := TStringList.Create;
  Parser := TGocciaJSON5Parser.Create;
  try
    try
      Source.LoadFromFile(ParamStr(1));
      ParsedValue := Parser.Parse(Source.Text);
      TGarbageCollector.Instance.AddTempRoot(ParsedValue);
      try
        WriteLn(EncodeValue(ParsedValue));
      finally
        TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
      end;
    except
      on E: Exception do
      begin
        WriteLn(E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Parser.Free;
    Source.Free;
    TGarbageCollector.Shutdown;
  end;
end.
