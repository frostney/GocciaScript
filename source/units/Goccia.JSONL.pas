unit Goccia.JSONL;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.JSON,
  Goccia.Values.ArrayValue,
  Goccia.Values.Primitives;

type
  EGocciaJSONLParseError = class(Exception);
  EGocciaJSONLInvalidInputError = class(Exception);

  TGocciaJSONLChunkParseResult = record
    Values: TGocciaArrayValue;
    Read: Integer;
    Done: Boolean;
    ErrorMessage: string;
  end;

  TGocciaJSONLParser = class
  private
    FJSONParser: TGocciaJSONParser;

    class function ClampOffset(const AValue, ALimit: Integer): Integer; static;
    class function FormatParseError(const ALineNumber: Integer;
      const AErrorMessage: string): string; static;
    class function HasUTF8BOM(const ABytes: TBytes;
      const AStart, AEnd: Integer): Boolean; static;
    class function IsBlankLine(const ALineText: string): Boolean; static;
    class function IsIncompleteFinalRecord(const ALineText,
      AErrorMessage: string): Boolean; static;
    class function NormalizeByteRange(const ABytes: TBytes; const AStart,
      AEnd: Integer; out ANormalizedStart, ANormalizedEnd: Integer): Boolean; static;
    class function NormalizeTextRange(const AText: string; const AStart,
      AEnd: Integer; out ANormalizedStart, ANormalizedEnd: Integer): Boolean; static;
    class function SliceBytesToString(const ABytes: TBytes; const AStart,
      AEnd: Integer): string; static;
    function TryParseLine(const ALineText: string; out AValue: TGocciaValue;
      out AErrorMessage: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const AText: UTF8String): TGocciaArrayValue; overload;
    function Parse(const AText: string): TGocciaArrayValue; overload;
    function Parse(const ABytes: TBytes): TGocciaArrayValue; overload;
    function ParseChunk(const AText: string; const AStart: Integer = 0;
      const AEnd: Integer = -1): TGocciaJSONLChunkParseResult; overload;
    function ParseChunk(const ABytes: TBytes; const AStart: Integer = 0;
      const AEnd: Integer = -1): TGocciaJSONLChunkParseResult; overload;
  end;

implementation

uses
  StrUtils,

  BOM,
  TextSemantics;

constructor TGocciaJSONLParser.Create;
begin
  inherited Create;
  FJSONParser := TGocciaJSONParser.Create;
end;

destructor TGocciaJSONLParser.Destroy;
begin
  FJSONParser.Free;
  inherited;
end;

class function TGocciaJSONLParser.ClampOffset(const AValue,
  ALimit: Integer): Integer;
begin
  if AValue < 0 then
    Exit(0);
  if AValue > ALimit then
    Exit(ALimit);
  Result := AValue;
end;

class function TGocciaJSONLParser.FormatParseError(const ALineNumber: Integer;
  const AErrorMessage: string): string;
begin
  Result := Format('JSONL line %d: %s', [ALineNumber, AErrorMessage]);
end;

class function TGocciaJSONLParser.HasUTF8BOM(const ABytes: TBytes;
  const AStart, AEnd: Integer): Boolean;
begin
  Result := HasUTF8BOMBytes(ABytes, AStart, AEnd);
end;

class function TGocciaJSONLParser.IsBlankLine(const ALineText: string): Boolean;
begin
  Result := Trim(ALineText) = '';
end;

class function TGocciaJSONLParser.IsIncompleteFinalRecord(const ALineText,
  AErrorMessage: string): Boolean;
var
  Trimmed: string;
begin
  if StartsText('Unexpected end of JSON input', AErrorMessage) or
     StartsText('Unterminated object', AErrorMessage) or
     StartsText('Unterminated array', AErrorMessage) or
     StartsText('Unterminated string', AErrorMessage) or
     StartsText('Unexpected end in string escape', AErrorMessage) or
     StartsText('Incomplete unicode escape sequence', AErrorMessage) or
     StartsText('Invalid number format after decimal point', AErrorMessage) or
     StartsText('Invalid number format in exponent', AErrorMessage) then
    Exit(True);

  Trimmed := Trim(ALineText);
  if StartsText('Invalid literal', AErrorMessage) then
    Exit(StartsText(Trimmed, 'true') or StartsText(Trimmed, 'false') or
      StartsText(Trimmed, 'null'));

  if StartsText('Invalid number format', AErrorMessage) then
    Exit((Trimmed = '-') or EndsText('.', Trimmed) or EndsText('e', Trimmed) or
      EndsText('E', Trimmed) or EndsText('+', Trimmed) or
      EndsText('-', Trimmed));

  Result := False;
end;

class function TGocciaJSONLParser.NormalizeByteRange(const ABytes: TBytes;
  const AStart, AEnd: Integer; out ANormalizedStart,
  ANormalizedEnd: Integer): Boolean;
var
  EffectiveEnd: Integer;
begin
  if AEnd < 0 then
    EffectiveEnd := Length(ABytes)
  else
    EffectiveEnd := AEnd;

  ANormalizedStart := ClampOffset(AStart, Length(ABytes));
  ANormalizedEnd := ClampOffset(EffectiveEnd, Length(ABytes));
  if ANormalizedEnd < ANormalizedStart then
    ANormalizedEnd := ANormalizedStart;
  Result := ANormalizedStart < ANormalizedEnd;
end;

class function TGocciaJSONLParser.NormalizeTextRange(const AText: string;
  const AStart, AEnd: Integer; out ANormalizedStart,
  ANormalizedEnd: Integer): Boolean;
var
  EffectiveEnd: Integer;
begin
  if AEnd < 0 then
    EffectiveEnd := Length(AText)
  else
    EffectiveEnd := AEnd;

  ANormalizedStart := ClampOffset(AStart, Length(AText));
  ANormalizedEnd := ClampOffset(EffectiveEnd, Length(AText));
  if ANormalizedEnd < ANormalizedStart then
    ANormalizedEnd := ANormalizedStart;
  Result := ANormalizedStart < ANormalizedEnd;
end;

class function TGocciaJSONLParser.SliceBytesToString(const ABytes: TBytes;
  const AStart, AEnd: Integer): string;
var
  RawBytes: RawByteString;
  ByteLength: Integer;
begin
  ByteLength := AEnd - AStart;
  if ByteLength <= 0 then
    Exit('');

  SetLength(RawBytes, ByteLength);
  Move(ABytes[AStart], RawBytes[1], ByteLength);
  Result := RetagUTF8Text(RawBytes);
end;

function TGocciaJSONLParser.TryParseLine(const ALineText: string;
  out AValue: TGocciaValue; out AErrorMessage: string): Boolean;
begin
  try
    AValue := FJSONParser.Parse(ALineText);
    AErrorMessage := '';
    Result := True;
  except
    on E: EGocciaJSONParseError do
    begin
      AValue := nil;
      AErrorMessage := E.Message;
      Result := False;
    end;
  end;
end;

function TGocciaJSONLParser.Parse(const AText: UTF8String): TGocciaArrayValue;
begin
  Result := Parse(RetagUTF8Text(RawByteString(AText)));
end;

function TGocciaJSONLParser.Parse(const AText: string): TGocciaArrayValue;
var
  DelimiterLength: Integer;
  DelimiterStart: Integer;
  ErrorMessage: string;
  HasDelimiter: Boolean;
  LineEnd: Integer;
  LineNumber: Integer;
  LineStart: Integer;
  LineText: string;
  NextLineStart: Integer;
  ParsedValue: TGocciaValue;
  StartIndex: Integer;
  EndIndex: Integer;
begin
  Result := TGocciaArrayValue.Create;
  if not NormalizeTextRange(AText, 0, -1, StartIndex, EndIndex) then
    Exit;

  LineStart := StartIndex;
  LineNumber := 1;
  while LineStart < EndIndex do
  begin
    DelimiterStart := LineStart;
    while (DelimiterStart < EndIndex) and
      not (AText[DelimiterStart + 1] in [#10, #13]) do
      Inc(DelimiterStart);

    HasDelimiter := DelimiterStart < EndIndex;
    if HasDelimiter then
    begin
      LineEnd := DelimiterStart;
      DelimiterLength := 1;
      if (AText[DelimiterStart + 1] = #13) and
         (DelimiterStart + 1 < EndIndex) and
         (AText[DelimiterStart + 2] = #10) then
        DelimiterLength := 2;
      NextLineStart := DelimiterStart + DelimiterLength;
    end
    else
    begin
      LineEnd := EndIndex;
      NextLineStart := EndIndex;
    end;

    LineText := Copy(AText, LineStart + 1, LineEnd - LineStart);
    if not IsBlankLine(LineText) then
    begin
      if not TryParseLine(LineText, ParsedValue, ErrorMessage) then
        raise EGocciaJSONLParseError.Create(
          FormatParseError(LineNumber, ErrorMessage));
      Result.Elements.Add(ParsedValue);
    end;

    LineStart := NextLineStart;
    Inc(LineNumber);
  end;
end;

function TGocciaJSONLParser.Parse(const ABytes: TBytes): TGocciaArrayValue;
var
  DelimiterLength: Integer;
  DelimiterStart: Integer;
  EffectiveStart: Integer;
  EffectiveEnd: Integer;
  ErrorMessage: string;
  HasDelimiter: Boolean;
  LineEnd: Integer;
  LineNumber: Integer;
  LineStart: Integer;
  LineText: string;
  NextLineStart: Integer;
  ParsedValue: TGocciaValue;
begin
  Result := TGocciaArrayValue.Create;
  if not NormalizeByteRange(ABytes, 0, -1, EffectiveStart, EffectiveEnd) then
    Exit;

  if HasUTF8BOM(ABytes, EffectiveStart, EffectiveEnd) then
    Inc(EffectiveStart, 3);

  LineStart := EffectiveStart;
  LineNumber := 1;
  while LineStart < EffectiveEnd do
  begin
    DelimiterStart := LineStart;
    while (DelimiterStart < EffectiveEnd) and
      not (ABytes[DelimiterStart] in [10, 13]) do
      Inc(DelimiterStart);

    HasDelimiter := DelimiterStart < EffectiveEnd;
    if HasDelimiter then
    begin
      LineEnd := DelimiterStart;
      DelimiterLength := 1;
      if (ABytes[DelimiterStart] = 13) and
         (DelimiterStart + 1 < EffectiveEnd) and
         (ABytes[DelimiterStart + 1] = 10) then
        DelimiterLength := 2;
      NextLineStart := DelimiterStart + DelimiterLength;
    end
    else
    begin
      LineEnd := EffectiveEnd;
      NextLineStart := EffectiveEnd;
    end;

    LineText := SliceBytesToString(ABytes, LineStart, LineEnd);
    if not IsBlankLine(LineText) then
    begin
      if not TryParseLine(LineText, ParsedValue, ErrorMessage) then
        raise EGocciaJSONLParseError.Create(
          FormatParseError(LineNumber, ErrorMessage));
      Result.Elements.Add(ParsedValue);
    end;

    LineStart := NextLineStart;
    Inc(LineNumber);
  end;
end;

function TGocciaJSONLParser.ParseChunk(const AText: string; const AStart,
  AEnd: Integer): TGocciaJSONLChunkParseResult;
var
  DelimiterLength: Integer;
  DelimiterStart: Integer;
  EffectiveStart: Integer;
  EffectiveEnd: Integer;
  ErrorMessage: string;
  HasDelimiter: Boolean;
  LineEnd: Integer;
  LineNumber: Integer;
  LineStart: Integer;
  LineText: string;
  NextLineStart: Integer;
  ParsedValue: TGocciaValue;
  ResumeOffset: Integer;
begin
  Result.Values := TGocciaArrayValue.Create;
  Result.Read := ClampOffset(AStart, Length(AText));
  Result.Done := True;
  Result.ErrorMessage := '';
  if not NormalizeTextRange(AText, AStart, AEnd, EffectiveStart, EffectiveEnd) then
  begin
    Result.Read := EffectiveStart;
    Exit;
  end;

  LineStart := EffectiveStart;
  LineNumber := 1;
  ResumeOffset := EffectiveStart;
  while LineStart < EffectiveEnd do
  begin
    DelimiterStart := LineStart;
    while (DelimiterStart < EffectiveEnd) and
      not (AText[DelimiterStart + 1] in [#10, #13]) do
      Inc(DelimiterStart);

    HasDelimiter := DelimiterStart < EffectiveEnd;
    if HasDelimiter then
    begin
      LineEnd := DelimiterStart;
      DelimiterLength := 1;
      if (AText[DelimiterStart + 1] = #13) and
         (DelimiterStart + 1 < EffectiveEnd) and
         (AText[DelimiterStart + 2] = #10) then
        DelimiterLength := 2;
      NextLineStart := DelimiterStart + DelimiterLength;
    end
    else
    begin
      LineEnd := EffectiveEnd;
      NextLineStart := EffectiveEnd;
    end;

    LineText := Copy(AText, LineStart + 1, LineEnd - LineStart);
    if not IsBlankLine(LineText) then
    begin
      if not TryParseLine(LineText, ParsedValue, ErrorMessage) then
      begin
        Result.Read := ResumeOffset;
        Result.Done := False;
        if HasDelimiter or not IsIncompleteFinalRecord(LineText, ErrorMessage) then
          Result.ErrorMessage := FormatParseError(LineNumber, ErrorMessage);
        Exit;
      end;
      Result.Values.Elements.Add(ParsedValue);
    end;

    if HasDelimiter then
      ResumeOffset := DelimiterStart
    else
      ResumeOffset := EffectiveEnd;

    LineStart := NextLineStart;
    Inc(LineNumber);
  end;

  Result.Read := EffectiveEnd;
end;

function TGocciaJSONLParser.ParseChunk(const ABytes: TBytes; const AStart,
  AEnd: Integer): TGocciaJSONLChunkParseResult;
var
  DelimiterLength: Integer;
  DelimiterStart: Integer;
  EffectiveStart: Integer;
  EffectiveEnd: Integer;
  ErrorMessage: string;
  HasDelimiter: Boolean;
  LineEnd: Integer;
  LineNumber: Integer;
  LineStart: Integer;
  LineText: string;
  NextLineStart: Integer;
  ParsedValue: TGocciaValue;
  ResumeOffset: Integer;
begin
  Result.Values := TGocciaArrayValue.Create;
  Result.Read := ClampOffset(AStart, Length(ABytes));
  Result.Done := True;
  Result.ErrorMessage := '';
  if not NormalizeByteRange(ABytes, AStart, AEnd, EffectiveStart, EffectiveEnd) then
  begin
    Result.Read := EffectiveStart;
    Exit;
  end;

  if (EffectiveStart = 0) and HasUTF8BOM(ABytes, EffectiveStart, EffectiveEnd) then
    Inc(EffectiveStart, 3);

  LineStart := EffectiveStart;
  LineNumber := 1;
  ResumeOffset := EffectiveStart;
  while LineStart < EffectiveEnd do
  begin
    DelimiterStart := LineStart;
    while (DelimiterStart < EffectiveEnd) and
      not (ABytes[DelimiterStart] in [10, 13]) do
      Inc(DelimiterStart);

    HasDelimiter := DelimiterStart < EffectiveEnd;
    if HasDelimiter then
    begin
      LineEnd := DelimiterStart;
      DelimiterLength := 1;
      if (ABytes[DelimiterStart] = 13) and
         (DelimiterStart + 1 < EffectiveEnd) and
         (ABytes[DelimiterStart + 1] = 10) then
        DelimiterLength := 2;
      NextLineStart := DelimiterStart + DelimiterLength;
    end
    else
    begin
      LineEnd := EffectiveEnd;
      NextLineStart := EffectiveEnd;
    end;

    LineText := SliceBytesToString(ABytes, LineStart, LineEnd);
    if not IsBlankLine(LineText) then
    begin
      if not TryParseLine(LineText, ParsedValue, ErrorMessage) then
      begin
        Result.Read := ResumeOffset;
        Result.Done := False;
        if HasDelimiter or not IsIncompleteFinalRecord(LineText, ErrorMessage) then
          Result.ErrorMessage := FormatParseError(LineNumber, ErrorMessage);
        Exit;
      end;
      Result.Values.Elements.Add(ParsedValue);
    end;

    if HasDelimiter then
      ResumeOffset := DelimiterStart
    else
      ResumeOffset := EffectiveEnd;

    LineStart := NextLineStart;
    Inc(LineNumber);
  end;

  Result.Read := EffectiveEnd;
end;

end.
