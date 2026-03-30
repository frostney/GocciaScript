unit Goccia.ScriptLoader.JSON;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.Primitives;

type
  TScriptLoaderTiming = record
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    TotalTimeNanoseconds: Int64;
  end;

  TScriptLoaderErrorInfo = record
    ErrorType: string;
    Message: string;
    FileName: string;
    Line: Integer;
    Column: Integer;
  end;

function DefaultScriptLoaderErrorInfo: TScriptLoaderErrorInfo;
function ExceptionToScriptLoaderErrorInfo(const E: Exception): TScriptLoaderErrorInfo;
function BuildSuccessJSON(const AValue: TGocciaValue; const AOutputText: string;
  const ATiming: TScriptLoaderTiming): string;
function BuildErrorJSON(const AOutputText: string;
  const AErrorInfo: TScriptLoaderErrorInfo;
  const ATiming: TScriptLoaderTiming): string;

implementation

uses
  Math,

  StringBuffer,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.JSON,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.VM.Exception;

function EscapeJSONString(const AValue: string): string;
var
  I: Integer;
  Ch: Char;
  Buffer: TStringBuffer;
begin
  Buffer := TStringBuffer.Create(Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    case Ch of
      '"': Buffer.Append('\"');
      '\': Buffer.Append('\\');
      '/': Buffer.Append('\/');
      #8: Buffer.Append('\b');
      #9: Buffer.Append('\t');
      #10: Buffer.Append('\n');
      #12: Buffer.Append('\f');
      #13: Buffer.Append('\r');
    else
      if Ord(Ch) < 32 then
      begin
        Buffer.Append('\u');
        Buffer.Append(IntToHex(Ord(Ch), 4));
      end
      else
        Buffer.AppendChar(Ch);
    end;
  end;
  Result := Buffer.ToString;
end;

function QuoteJSONString(const AValue: string): string;
begin
  Result := '"' + EscapeJSONString(AValue) + '"';
end;

function NanosecondsToMillisecondsText(const ANanoseconds: Int64): string;
begin
  Result := FloatToStr(ANanoseconds / 1000000.0, DefaultFormatSettings);
end;

function SerializeScriptValue(const AValue: TGocciaValue): string;
var
  Stringifier: TGocciaJSONStringifier;
  Serialized: string;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit('null');

  if AValue.IsCallable then
    Exit(QuoteJSONString(AValue.ToStringLiteral.Value));

  Stringifier := TGocciaJSONStringifier.Create;
  try
    Serialized := Stringifier.Stringify(AValue);
  finally
    Stringifier.Free;
  end;

  if (Serialized = 'null') and
     not (AValue is TGocciaNullLiteralValue) and
     not (AValue is TGocciaBooleanLiteralValue) and
     not (AValue is TGocciaNumberLiteralValue) then
    Result := QuoteJSONString(AValue.ToStringLiteral.Value)
  else
    Result := Serialized;
end;

function SerializeNullableInteger(const AValue: Integer): string;
begin
  if AValue <= 0 then
    Result := 'null'
  else
    Result := IntToStr(AValue);
end;

function SerializeNullableString(const AValue: string): string;
begin
  if AValue = '' then
    Result := 'null'
  else
    Result := QuoteJSONString(AValue);
end;

function BuildTimingJSON(const ATiming: TScriptLoaderTiming): string;
begin
  Result :=
    '"timing":{' +
      '"lex_ms":' + NanosecondsToMillisecondsText(ATiming.LexTimeNanoseconds) + ',' +
      '"parse_ms":' + NanosecondsToMillisecondsText(ATiming.ParseTimeNanoseconds) + ',' +
      '"exec_ms":' + NanosecondsToMillisecondsText(ATiming.ExecuteTimeNanoseconds) + ',' +
      '"total_ms":' + NanosecondsToMillisecondsText(ATiming.TotalTimeNanoseconds) +
    '}';
end;

function DefaultScriptLoaderErrorInfo: TScriptLoaderErrorInfo;
begin
  Result.ErrorType := '';
  Result.Message := '';
  Result.FileName := '';
  Result.Line := -1;
  Result.Column := -1;
end;

function TryPopulateErrorInfoFromValue(const AValue: TGocciaValue;
  var AErrorInfo: TScriptLoaderErrorInfo): Boolean;
var
  ErrorObject: TGocciaObjectValue;
  NameValue, MessageValue: TGocciaValue;
begin
  Result := False;
  if not (AValue is TGocciaObjectValue) then
    Exit;

  ErrorObject := TGocciaObjectValue(AValue);
  NameValue := ErrorObject.GetProperty(PROP_NAME);
  MessageValue := ErrorObject.GetProperty(PROP_MESSAGE);

  if Assigned(NameValue) and not (NameValue is TGocciaUndefinedLiteralValue) then
    AErrorInfo.ErrorType := NameValue.ToStringLiteral.Value;
  if Assigned(MessageValue) and not (MessageValue is TGocciaUndefinedLiteralValue) then
    AErrorInfo.Message := MessageValue.ToStringLiteral.Value;

  Result := (AErrorInfo.ErrorType <> '') or (AErrorInfo.Message <> '');
end;

function ExceptionClassToErrorType(const E: Exception): string;
begin
  if E is TGocciaTypeError then
    Result := 'TypeError'
  else if E is TGocciaReferenceError then
    Result := 'ReferenceError'
  else if E is TGocciaSyntaxError then
    Result := 'SyntaxError'
  else if E is TGocciaRuntimeError then
    Result := 'RuntimeError'
  else if E is TGocciaLexerError then
    Result := 'LexerError'
  else
    Result := E.ClassName;
end;

function ExceptionToScriptLoaderErrorInfo(const E: Exception): TScriptLoaderErrorInfo;
var
  GocciaError: TGocciaError;
begin
  Result := DefaultScriptLoaderErrorInfo;

  if E is TGocciaThrowValue then
  begin
    if not TryPopulateErrorInfoFromValue(TGocciaThrowValue(E).Value, Result) then
    begin
      Result.ErrorType := 'Error';
      Result.Message := TGocciaThrowValue(E).Value.ToStringLiteral.Value;
    end;
    Exit;
  end;

  if E is EGocciaBytecodeThrow then
  begin
    if not TryPopulateErrorInfoFromValue(EGocciaBytecodeThrow(E).ThrownValue, Result) then
    begin
      Result.ErrorType := 'Error';
      Result.Message := EGocciaBytecodeThrow(E).Message;
    end;
    Exit;
  end;

  if E is TGocciaError then
  begin
    GocciaError := TGocciaError(E);
    Result.ErrorType := ExceptionClassToErrorType(E);
    Result.Message := E.Message;
    Result.FileName := GocciaError.FileName;
    Result.Line := GocciaError.Line;
    Result.Column := GocciaError.Column;
    Exit;
  end;

  Result.ErrorType := ExceptionClassToErrorType(E);
  Result.Message := E.Message;
end;

function BuildSuccessJSON(const AValue: TGocciaValue; const AOutputText: string;
  const ATiming: TScriptLoaderTiming): string;
begin
  Result :=
    '{' +
      '"ok":true,' +
      '"value":' + SerializeScriptValue(AValue) + ',' +
      '"output":' + QuoteJSONString(AOutputText) + ',' +
      '"error":null,' +
      BuildTimingJSON(ATiming) +
    '}';
end;

function BuildErrorJSON(const AOutputText: string;
  const AErrorInfo: TScriptLoaderErrorInfo;
  const ATiming: TScriptLoaderTiming): string;
begin
  Result :=
    '{' +
      '"ok":false,' +
      '"value":null,' +
      '"output":' + QuoteJSONString(AOutputText) + ',' +
      '"error":{' +
        '"type":' + QuoteJSONString(AErrorInfo.ErrorType) + ',' +
        '"message":' + QuoteJSONString(AErrorInfo.Message) + ',' +
        '"line":' + SerializeNullableInteger(AErrorInfo.Line) + ',' +
        '"column":' + SerializeNullableInteger(AErrorInfo.Column) + ',' +
        '"fileName":' + SerializeNullableString(AErrorInfo.FileName) +
      '},' +
      BuildTimingJSON(ATiming) +
    '}';
end;

end.
