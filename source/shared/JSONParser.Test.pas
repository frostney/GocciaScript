program JSONParser.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  JSONParser,
  TestingPascalLibrary;

type
  TRecordingJSONParser = class(TAbstractJSONParser)
  private
    FEvents: TStringList;
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
  public
    constructor Create; overload; override;
    constructor Create(
      const ACapabilities: TJSONParserCapabilities); overload; override;
    destructor Destroy; override;
    procedure Parse(const AText: UTF8String);
    property Events: TStringList read FEvents;
  end;

  TJSONParserTests = class(TTestSuite)
  private
    // Strict mode: primitives
    procedure TestParseNull;
    procedure TestParseTrue;
    procedure TestParseFalse;
    procedure TestParseInteger;
    procedure TestParseNegativeInteger;
    procedure TestParseZero;
    procedure TestParseFloat;
    procedure TestParseNegativeFloat;
    procedure TestParseFloatWithExponent;
    procedure TestParseFloatWithNegativeExponent;
    procedure TestParseNegativeZero;
    // Strict mode: strings
    procedure TestParseString;
    procedure TestParseStringNewlineEscape;
    procedure TestParseStringTabEscape;
    procedure TestParseStringBackslashEscape;
    procedure TestParseStringForwardSlashEscape;
    procedure TestParseStringQuoteEscape;
    procedure TestParseStringUnicodeEscape;
    procedure TestParseStringSurrogatePair;
    procedure TestParseEmptyString;
    // Strict mode: objects
    procedure TestParseEmptyObject;
    procedure TestParseObjectOneKey;
    procedure TestParseNestedObject;
    // Strict mode: arrays
    procedure TestParseEmptyArray;
    procedure TestParseArrayElements;
    procedure TestParseNestedArray;
    // Strict mode: mixed
    procedure TestParseMixedObjectArray;
    // Error cases: strict mode
    procedure TestErrorEmptyInput;
    procedure TestErrorTrailingGarbage;
    procedure TestErrorUnterminatedString;
    procedure TestErrorUnterminatedObject;
    procedure TestErrorUnterminatedArray;
    procedure TestErrorInvalidLiteral;
    procedure TestErrorLeadingZeros;
    procedure TestErrorSingleQuotesStrict;
    procedure TestErrorTrailingCommaArrayStrict;
    procedure TestErrorCommentsStrict;
    procedure TestErrorPlusSignStrict;
    procedure TestErrorLeadingDecimalStrict;
    // JSON5 mode
    procedure TestJSON5SingleQuotedString;
    procedure TestJSON5TrailingCommaArray;
    procedure TestJSON5TrailingCommaObject;
    procedure TestJSON5LineComment;
    procedure TestJSON5BlockComment;
    procedure TestJSON5HexNumber;
    procedure TestJSON5LeadingPlus;
    procedure TestJSON5LeadingDecimal;
    procedure TestJSON5TrailingDecimal;
    procedure TestJSON5Infinity;
    procedure TestJSON5NegativeInfinity;
    procedure TestJSON5NaN;
    procedure TestJSON5IdentifierKeys;
  public
    procedure SetupTests; override;
  end;

{ TRecordingJSONParser }

constructor TRecordingJSONParser.Create;
begin
  inherited Create;
  FEvents := TStringList.Create;
end;

constructor TRecordingJSONParser.Create(
  const ACapabilities: TJSONParserCapabilities);
begin
  inherited Create(ACapabilities);
  FEvents := TStringList.Create;
end;

destructor TRecordingJSONParser.Destroy;
begin
  FEvents.Free;
  inherited Destroy;
end;

procedure TRecordingJSONParser.Parse(const AText: UTF8String);
begin
  FEvents.Clear;
  DoParse(AText);
end;

procedure TRecordingJSONParser.OnNull;
begin
  FEvents.Add('null');
end;

procedure TRecordingJSONParser.OnBoolean(const AValue: Boolean);
begin
  if AValue then
    FEvents.Add('bool:true')
  else
    FEvents.Add('bool:false');
end;

procedure TRecordingJSONParser.OnString(const AValue: string);
begin
  FEvents.Add('string:' + AValue);
end;

procedure TRecordingJSONParser.OnInteger(const AValue: Int64);
begin
  FEvents.Add('int:' + IntToStr(AValue));
end;

procedure TRecordingJSONParser.OnFloat(const AValue: Double);
var
  Fmt: TFormatSettings;
begin
  Fmt := DefaultFormatSettings;
  Fmt.DecimalSeparator := '.';
  FEvents.Add('float:' + FloatToStr(AValue, Fmt));
end;

procedure TRecordingJSONParser.OnBeginObject;
begin
  FEvents.Add('begin_object');
end;

procedure TRecordingJSONParser.OnObjectKey(const AKey: string);
begin
  FEvents.Add('key:' + AKey);
end;

procedure TRecordingJSONParser.OnEndObject;
begin
  FEvents.Add('end_object');
end;

procedure TRecordingJSONParser.OnBeginArray;
begin
  FEvents.Add('begin_array');
end;

procedure TRecordingJSONParser.OnEndArray;
begin
  FEvents.Add('end_array');
end;

{ TJSONParserTests }

procedure TJSONParserTests.SetupTests;
begin
  // Strict mode: primitives
  Test('Parse null', TestParseNull);
  Test('Parse true', TestParseTrue);
  Test('Parse false', TestParseFalse);
  Test('Parse integer 42', TestParseInteger);
  Test('Parse negative integer -7', TestParseNegativeInteger);
  Test('Parse zero', TestParseZero);
  Test('Parse float 3.14', TestParseFloat);
  Test('Parse negative float -0.5', TestParseNegativeFloat);
  Test('Parse float with exponent 1e10', TestParseFloatWithExponent);
  Test('Parse float with negative exponent 2.5E-3', TestParseFloatWithNegativeExponent);
  Test('Parse -0 as negative zero float', TestParseNegativeZero);
  // Strict mode: strings
  Test('Parse string hello', TestParseString);
  Test('Parse string with newline escape', TestParseStringNewlineEscape);
  Test('Parse string with tab escape', TestParseStringTabEscape);
  Test('Parse string with backslash escape', TestParseStringBackslashEscape);
  Test('Parse string with forward slash escape', TestParseStringForwardSlashEscape);
  Test('Parse string with quote escape', TestParseStringQuoteEscape);
  Test('Parse string with unicode escape \u0041', TestParseStringUnicodeEscape);
  Test('Parse string with surrogate pair', TestParseStringSurrogatePair);
  Test('Parse empty string', TestParseEmptyString);
  // Strict mode: objects
  Test('Parse empty object {}', TestParseEmptyObject);
  Test('Parse object with one key', TestParseObjectOneKey);
  Test('Parse nested object', TestParseNestedObject);
  // Strict mode: arrays
  Test('Parse empty array []', TestParseEmptyArray);
  Test('Parse array with elements [1,2,3]', TestParseArrayElements);
  Test('Parse nested array [[1],[2]]', TestParseNestedArray);
  // Strict mode: mixed
  Test('Parse mixed object with array values', TestParseMixedObjectArray);
  // Error cases: strict mode
  Test('Error on empty input', TestErrorEmptyInput);
  Test('Error on trailing garbage', TestErrorTrailingGarbage);
  Test('Error on unterminated string', TestErrorUnterminatedString);
  Test('Error on unterminated object', TestErrorUnterminatedObject);
  Test('Error on unterminated array', TestErrorUnterminatedArray);
  Test('Error on invalid literal', TestErrorInvalidLiteral);
  Test('Error on leading zeros', TestErrorLeadingZeros);
  Test('Error on single quotes in strict mode', TestErrorSingleQuotesStrict);
  Test('Error on trailing comma in array strict mode', TestErrorTrailingCommaArrayStrict);
  Test('Error on comments in strict mode', TestErrorCommentsStrict);
  Test('Error on plus sign in strict mode', TestErrorPlusSignStrict);
  Test('Error on leading decimal in strict mode', TestErrorLeadingDecimalStrict);
  // JSON5 mode
  Test('JSON5 single-quoted string', TestJSON5SingleQuotedString);
  Test('JSON5 trailing comma in array', TestJSON5TrailingCommaArray);
  Test('JSON5 trailing comma in object', TestJSON5TrailingCommaObject);
  Test('JSON5 line comment', TestJSON5LineComment);
  Test('JSON5 block comment', TestJSON5BlockComment);
  Test('JSON5 hex number 0xFF', TestJSON5HexNumber);
  Test('JSON5 leading plus +5', TestJSON5LeadingPlus);
  Test('JSON5 leading decimal .5', TestJSON5LeadingDecimal);
  Test('JSON5 trailing decimal 5.', TestJSON5TrailingDecimal);
  Test('JSON5 Infinity', TestJSON5Infinity);
  Test('JSON5 negative Infinity', TestJSON5NegativeInfinity);
  Test('JSON5 NaN', TestJSON5NaN);
  Test('JSON5 identifier keys', TestJSON5IdentifierKeys);
end;

{ Strict mode: primitives }

procedure TJSONParserTests.TestParseNull;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('null');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('null');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseTrue;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('true');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('bool:true');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseFalse;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('false');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('bool:false');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseInteger;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('42');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('int:42');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseNegativeInteger;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('-7');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('int:-7');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseZero;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('0');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('int:0');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseFloat;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('3.14');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:3.14');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseNegativeFloat;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('-0.5');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:-0.5');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseFloatWithExponent;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('1e10');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:10000000000');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseFloatWithNegativeExponent;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('2.5E-3');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:0.0025');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseNegativeZero;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('-0');
    Expect<Integer>(P.Events.Count).ToBe(1);
    // -0 is emitted as OnFloat(-0.0); FloatToStr renders -0.0 as '0'
    Expect<string>(P.Events[0]).ToBe('float:0');
  finally
    P.Free;
  end;
end;

{ Strict mode: strings }

procedure TJSONParserTests.TestParseString;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"hello"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:hello');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringNewlineEscape;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"a\nb"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:a' + #10 + 'b');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringTabEscape;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"a\tb"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:a' + #9 + 'b');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringBackslashEscape;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"\\"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:\');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringForwardSlashEscape;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"\/"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:/');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringQuoteEscape;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"\""');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:"');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringUnicodeEscape;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"\u0041"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:A');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseStringSurrogatePair;
var
  P: TRecordingJSONParser;
  Expected: string;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('"\uD83D\uDE00"');
    Expect<Integer>(P.Events.Count).ToBe(1);
    // U+1F600 in UTF-8 is F0 9F 98 80
    Expected := #$F0#$9F#$98#$80;
    Expect<string>(P.Events[0]).ToBe('string:' + Expected);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseEmptyString;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('""');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:');
  finally
    P.Free;
  end;
end;

{ Strict mode: objects }

procedure TJSONParserTests.TestParseEmptyObject;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('{}');
    Expect<Integer>(P.Events.Count).ToBe(2);
    Expect<string>(P.Events[0]).ToBe('begin_object');
    Expect<string>(P.Events[1]).ToBe('end_object');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseObjectOneKey;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('{"a":1}');
    Expect<Integer>(P.Events.Count).ToBe(4);
    Expect<string>(P.Events[0]).ToBe('begin_object');
    Expect<string>(P.Events[1]).ToBe('key:a');
    Expect<string>(P.Events[2]).ToBe('int:1');
    Expect<string>(P.Events[3]).ToBe('end_object');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseNestedObject;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('{"a":{"b":2}}');
    Expect<Integer>(P.Events.Count).ToBe(7);
    Expect<string>(P.Events[0]).ToBe('begin_object');
    Expect<string>(P.Events[1]).ToBe('key:a');
    Expect<string>(P.Events[2]).ToBe('begin_object');
    Expect<string>(P.Events[3]).ToBe('key:b');
    Expect<string>(P.Events[4]).ToBe('int:2');
    Expect<string>(P.Events[5]).ToBe('end_object');
    Expect<string>(P.Events[6]).ToBe('end_object');
  finally
    P.Free;
  end;
end;

{ Strict mode: arrays }

procedure TJSONParserTests.TestParseEmptyArray;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('[]');
    Expect<Integer>(P.Events.Count).ToBe(2);
    Expect<string>(P.Events[0]).ToBe('begin_array');
    Expect<string>(P.Events[1]).ToBe('end_array');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseArrayElements;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('[1,2,3]');
    Expect<Integer>(P.Events.Count).ToBe(5);
    Expect<string>(P.Events[0]).ToBe('begin_array');
    Expect<string>(P.Events[1]).ToBe('int:1');
    Expect<string>(P.Events[2]).ToBe('int:2');
    Expect<string>(P.Events[3]).ToBe('int:3');
    Expect<string>(P.Events[4]).ToBe('end_array');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestParseNestedArray;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('[[1],[2]]');
    Expect<Integer>(P.Events.Count).ToBe(8);
    Expect<string>(P.Events[0]).ToBe('begin_array');
    Expect<string>(P.Events[1]).ToBe('begin_array');
    Expect<string>(P.Events[2]).ToBe('int:1');
    Expect<string>(P.Events[3]).ToBe('end_array');
    Expect<string>(P.Events[4]).ToBe('begin_array');
    Expect<string>(P.Events[5]).ToBe('int:2');
    Expect<string>(P.Events[6]).ToBe('end_array');
    Expect<string>(P.Events[7]).ToBe('end_array');
  finally
    P.Free;
  end;
end;

{ Strict mode: mixed }

procedure TJSONParserTests.TestParseMixedObjectArray;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create;
  try
    P.Parse('{"a":[1,true,null,"x"]}');
    Expect<Integer>(P.Events.Count).ToBe(9);
    Expect<string>(P.Events[0]).ToBe('begin_object');
    Expect<string>(P.Events[1]).ToBe('key:a');
    Expect<string>(P.Events[2]).ToBe('begin_array');
    Expect<string>(P.Events[3]).ToBe('int:1');
    Expect<string>(P.Events[4]).ToBe('bool:true');
    Expect<string>(P.Events[5]).ToBe('null');
    Expect<string>(P.Events[6]).ToBe('string:x');
    Expect<string>(P.Events[7]).ToBe('end_array');
    Expect<string>(P.Events[8]).ToBe('end_object');
  finally
    P.Free;
  end;
end;

{ Error cases: strict mode }

procedure TJSONParserTests.TestErrorEmptyInput;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorTrailingGarbage;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('42 extra');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorUnterminatedString;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('"hello');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorUnterminatedObject;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('{"a":1');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorUnterminatedArray;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('[1,2');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorInvalidLiteral;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('trux');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorLeadingZeros;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('01');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorSingleQuotesStrict;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('''hello''');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorTrailingCommaArrayStrict;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('[1,]');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorCommentsStrict;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('/**/42');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorPlusSignStrict;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('+1');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestErrorLeadingDecimalStrict;
var
  P: TRecordingJSONParser;
  Raised: Boolean;
begin
  P := TRecordingJSONParser.Create;
  try
    Raised := False;
    try
      P.Parse('.5');
    except
      on E: EJSONParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    P.Free;
  end;
end;

{ JSON5 mode }

procedure TJSONParserTests.TestJSON5SingleQuotedString;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('''hello''');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('string:hello');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5TrailingCommaArray;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('[1,2,]');
    Expect<Integer>(P.Events.Count).ToBe(4);
    Expect<string>(P.Events[0]).ToBe('begin_array');
    Expect<string>(P.Events[1]).ToBe('int:1');
    Expect<string>(P.Events[2]).ToBe('int:2');
    Expect<string>(P.Events[3]).ToBe('end_array');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5TrailingCommaObject;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('{"a":1,}');
    Expect<Integer>(P.Events.Count).ToBe(4);
    Expect<string>(P.Events[0]).ToBe('begin_object');
    Expect<string>(P.Events[1]).ToBe('key:a');
    Expect<string>(P.Events[2]).ToBe('int:1');
    Expect<string>(P.Events[3]).ToBe('end_object');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5LineComment;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('// comment' + #10 + '42');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('int:42');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5BlockComment;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('/* comment */42');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('int:42');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5HexNumber;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('0xFF');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:255');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5LeadingPlus;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('+5');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('int:5');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5LeadingDecimal;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('.5');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:0.5');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5TrailingDecimal;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('5.');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:5');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5Infinity;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('Infinity');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:+Inf');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5NegativeInfinity;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('-Infinity');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:-Inf');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5NaN;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('NaN');
    Expect<Integer>(P.Events.Count).ToBe(1);
    Expect<string>(P.Events[0]).ToBe('float:Nan');
  finally
    P.Free;
  end;
end;

procedure TJSONParserTests.TestJSON5IdentifierKeys;
var
  P: TRecordingJSONParser;
begin
  P := TRecordingJSONParser.Create(JSONParserJSON5Capabilities);
  try
    P.Parse('{key: 1}');
    Expect<Integer>(P.Events.Count).ToBe(4);
    Expect<string>(P.Events[0]).ToBe('begin_object');
    Expect<string>(P.Events[1]).ToBe('key:key');
    Expect<string>(P.Events[2]).ToBe('int:1');
    Expect<string>(P.Events[3]).ToBe('end_object');
  finally
    P.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TJSONParserTests.Create('JSONParser'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
