unit Goccia.CSV;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  EGocciaCSVParseError = class(Exception);

  TGocciaCSVChunkParseResult = record
    Values: TGocciaArrayValue;
    Read: Integer;
    Done: Boolean;
    ErrorMessage: string;
  end;

  TGocciaCSVFieldInfo = record
    Value: string;
    Quoted: Boolean;
  end;

  TGocciaCSVParser = class
  private
    class function ClampOffset(const AValue, ALimit: Integer): Integer; static;
    class function HasUTF8BOM(const AText: string): Boolean; static;
    class function SkipBOM(const AText: string;
      const AStart: Integer): Integer; static;
  public
    function Parse(const AText: string; const ADelimiter: Char = ',';
      const AHeaders: Boolean = True;
      const ASkipEmptyLines: Boolean = False): TGocciaArrayValue;
    function ParseChunk(const AText: string; const ADelimiter: Char;
      const AHeaders: Boolean; const ASkipEmptyLines: Boolean;
      const AStart: Integer = 0;
      const AEnd: Integer = -1): TGocciaCSVChunkParseResult;
    function ParseWithFieldInfo(const AText: string;
      const ADelimiter: Char = ','; const AHeaders: Boolean = True;
      const ASkipEmptyLines: Boolean = False):
      TArray<TArray<TGocciaCSVFieldInfo>>;
  end;

  TGocciaCSVStringifier = class
  public
    class function Stringify(const AData: TGocciaValue;
      const ADelimiter: Char = ',';
      const AHeaders: Boolean = True): string; static;
    class function EscapeField(const AValue: string;
      const ADelimiter: Char): string; static;
  end;

implementation

uses
  Classes;

const
  UTF8_BOM_CHAR_1 = #$FEFF;

class function TGocciaCSVParser.ClampOffset(const AValue,
  ALimit: Integer): Integer;
begin
  if AValue < 0 then
    Exit(0);
  if AValue > ALimit then
    Exit(ALimit);
  Result := AValue;
end;

class function TGocciaCSVParser.HasUTF8BOM(const AText: string): Boolean;
begin
  Result := (Length(AText) >= 1) and (AText[1] = UTF8_BOM_CHAR_1);
end;

class function TGocciaCSVParser.SkipBOM(const AText: string;
  const AStart: Integer): Integer;
begin
  if (AStart = 0) and HasUTF8BOM(AText) then
    Result := 1
  else
    Result := AStart;
end;

function ParseRow(const AText: string; const ADelimiter: Char;
  var APos: Integer; const AEnd: Integer;
  out AFields: TArray<TGocciaCSVFieldInfo>;
  out AConsumed: Boolean): Boolean;
var
  FieldCount: Integer;
  FieldCapacity: Integer;
  FieldStart: Integer;
  InQuotes: Boolean;
  Field: TGocciaCSVFieldInfo;
  Builder: string;
  Ch: Char;
begin
  Result := False;
  AConsumed := False;
  if APos > AEnd then
    Exit;

  FieldCount := 0;
  FieldCapacity := 8;
  SetLength(AFields, FieldCapacity);
  InQuotes := False;
  Builder := '';
  Field.Quoted := False;
  FieldStart := APos;

  while APos <= AEnd do
  begin
    Ch := AText[APos];

    if InQuotes then
    begin
      if Ch = '"' then
      begin
        if (APos + 1 <= AEnd) and (AText[APos + 1] = '"') then
        begin
          Builder := Builder + '"';
          Inc(APos, 2);
        end
        else
        begin
          InQuotes := False;
          Inc(APos);
        end;
      end
      else
      begin
        Builder := Builder + Ch;
        Inc(APos);
      end;
    end
    else
    begin
      if Ch = '"' then
      begin
        if Builder = '' then
          Field.Quoted := True;
        InQuotes := True;
        Inc(APos);
      end
      else if Ch = ADelimiter then
      begin
        Field.Value := Builder;
        if FieldCount >= FieldCapacity then
        begin
          FieldCapacity := FieldCapacity * 2;
          SetLength(AFields, FieldCapacity);
        end;
        AFields[FieldCount] := Field;
        Inc(FieldCount);
        Builder := '';
        Field.Quoted := False;
        Inc(APos);
        FieldStart := APos;
      end
      else if (Ch = #13) or (Ch = #10) then
      begin
        Field.Value := Builder;
        if FieldCount >= FieldCapacity then
        begin
          FieldCapacity := FieldCapacity * 2;
          SetLength(AFields, FieldCapacity);
        end;
        AFields[FieldCount] := Field;
        Inc(FieldCount);
        SetLength(AFields, FieldCount);

        Inc(APos);
        if (Ch = #13) and (APos <= AEnd) and (AText[APos] = #10) then
          Inc(APos);
        AConsumed := True;
        Result := True;
        Exit;
      end
      else
      begin
        Builder := Builder + Ch;
        Inc(APos);
      end;
    end;
  end;

  if InQuotes then
  begin
    Result := False;
    Exit;
  end;

  Field.Value := Builder;
  if FieldCount >= FieldCapacity then
  begin
    FieldCapacity := FieldCapacity + 1;
    SetLength(AFields, FieldCapacity);
  end;
  AFields[FieldCount] := Field;
  Inc(FieldCount);
  SetLength(AFields, FieldCount);
  AConsumed := True;
  Result := True;
end;

function IsEmptyRow(const AFields: TArray<TGocciaCSVFieldInfo>): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(AFields) - 1 do
    if AFields[I].Value <> '' then
      Exit(False);
  Result := (Length(AFields) <= 1);
end;

function TGocciaCSVParser.Parse(const AText: string;
  const ADelimiter: Char; const AHeaders: Boolean;
  const ASkipEmptyLines: Boolean): TGocciaArrayValue;
var
  Consumed: Boolean;
  EndIndex: Integer;
  Fields: TArray<TGocciaCSVFieldInfo>;
  HeaderFields: TArray<TGocciaCSVFieldInfo>;
  I: Integer;
  Obj: TGocciaObjectValue;
  Pos: Integer;
  Row: TGocciaArrayValue;
  RowNumber: Integer;
begin
  Result := TGocciaArrayValue.Create;
  if Length(AText) = 0 then
    Exit;

  Pos := 1;
  EndIndex := Length(AText);
  if HasUTF8BOM(AText) then
    Inc(Pos);

  RowNumber := 0;

  if AHeaders then
  begin
    if not ParseRow(AText, ADelimiter, Pos, EndIndex, HeaderFields, Consumed) then
      raise EGocciaCSVParseError.Create('Unterminated quoted field in header row');
    Inc(RowNumber);
  end;

  while Pos <= EndIndex do
  begin
    if not ParseRow(AText, ADelimiter, Pos, EndIndex, Fields, Consumed) then
      raise EGocciaCSVParseError.CreateFmt(
        'Unterminated quoted field at row %d', [RowNumber + 1]);

    Inc(RowNumber);

    if ASkipEmptyLines and IsEmptyRow(Fields) then
      Continue;

    if AHeaders then
    begin
      Obj := TGocciaObjectValue.Create;
      for I := 0 to Length(HeaderFields) - 1 do
      begin
        if I < Length(Fields) then
          Obj.AssignProperty(HeaderFields[I].Value,
            TGocciaStringLiteralValue.Create(Fields[I].Value))
        else
          Obj.AssignProperty(HeaderFields[I].Value,
            TGocciaStringLiteralValue.Create(''));
      end;
      Result.Elements.Add(Obj);
    end
    else
    begin
      Row := TGocciaArrayValue.Create;
      for I := 0 to Length(Fields) - 1 do
        Row.Elements.Add(TGocciaStringLiteralValue.Create(Fields[I].Value));
      Result.Elements.Add(Row);
    end;
  end;
end;

function TGocciaCSVParser.ParseWithFieldInfo(const AText: string;
  const ADelimiter: Char; const AHeaders: Boolean;
  const ASkipEmptyLines: Boolean): TArray<TArray<TGocciaCSVFieldInfo>>;
var
  Consumed: Boolean;
  Count: Integer;
  EndIndex: Integer;
  Fields: TArray<TGocciaCSVFieldInfo>;
  Pos: Integer;
  RowNumber: Integer;
begin
  Count := 0;
  SetLength(Result, 16);
  if Length(AText) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Pos := 1;
  EndIndex := Length(AText);
  if HasUTF8BOM(AText) then
    Inc(Pos);

  RowNumber := 0;
  while Pos <= EndIndex do
  begin
    if not ParseRow(AText, ADelimiter, Pos, EndIndex, Fields, Consumed) then
      raise EGocciaCSVParseError.CreateFmt(
        'Unterminated quoted field at row %d', [RowNumber + 1]);

    Inc(RowNumber);

    if ASkipEmptyLines and IsEmptyRow(Fields) then
      Continue;

    if Count >= Length(Result) then
      SetLength(Result, Length(Result) * 2);
    Result[Count] := Fields;
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

function TGocciaCSVParser.ParseChunk(const AText: string;
  const ADelimiter: Char; const AHeaders: Boolean;
  const ASkipEmptyLines: Boolean; const AStart: Integer;
  const AEnd: Integer): TGocciaCSVChunkParseResult;
var
  Consumed: Boolean;
  EffectiveEnd: Integer;
  EffectiveStart: Integer;
  Fields: TArray<TGocciaCSVFieldInfo>;
  HeaderFields: TArray<TGocciaCSVFieldInfo>;
  I: Integer;
  Obj: TGocciaObjectValue;
  Pos: Integer;
  ResumeOffset: Integer;
  Row: TGocciaArrayValue;
  RowNumber: Integer;
begin
  Result.Values := TGocciaArrayValue.Create;
  Result.Done := True;
  Result.ErrorMessage := '';

  if AEnd < 0 then
    EffectiveEnd := Length(AText)
  else
    EffectiveEnd := ClampOffset(AEnd, Length(AText));

  EffectiveStart := ClampOffset(AStart, Length(AText));
  if EffectiveStart = 0 then
    EffectiveStart := SkipBOM(AText, EffectiveStart);

  Result.Read := EffectiveStart;
  if EffectiveStart >= EffectiveEnd then
    Exit;

  Pos := EffectiveStart + 1;
  ResumeOffset := EffectiveStart;
  RowNumber := 0;

  if AHeaders then
  begin
    if not ParseRow(AText, ADelimiter, Pos, EffectiveEnd, HeaderFields,
      Consumed) then
    begin
      Result.Read := ResumeOffset;
      Result.Done := False;
      Exit;
    end;
    ResumeOffset := Pos - 1;
    Inc(RowNumber);
  end;

  while Pos <= EffectiveEnd do
  begin
    if not ParseRow(AText, ADelimiter, Pos, EffectiveEnd, Fields, Consumed) then
    begin
      Result.Read := ResumeOffset;
      Result.Done := False;
      Exit;
    end;

    Inc(RowNumber);

    if ASkipEmptyLines and IsEmptyRow(Fields) then
    begin
      ResumeOffset := Pos - 1;
      Continue;
    end;

    if AHeaders then
    begin
      Obj := TGocciaObjectValue.Create;
      for I := 0 to Length(HeaderFields) - 1 do
      begin
        if I < Length(Fields) then
          Obj.AssignProperty(HeaderFields[I].Value,
            TGocciaStringLiteralValue.Create(Fields[I].Value))
        else
          Obj.AssignProperty(HeaderFields[I].Value,
            TGocciaStringLiteralValue.Create(''));
      end;
      Result.Values.Elements.Add(Obj);
    end
    else
    begin
      Row := TGocciaArrayValue.Create;
      for I := 0 to Length(Fields) - 1 do
        Row.Elements.Add(TGocciaStringLiteralValue.Create(Fields[I].Value));
      Result.Values.Elements.Add(Row);
    end;

    ResumeOffset := Pos - 1;
  end;

  Result.Read := EffectiveEnd;
end;

class function TGocciaCSVStringifier.EscapeField(const AValue: string;
  const ADelimiter: Char): string;
var
  NeedsQuoting: Boolean;
  I: Integer;
begin
  NeedsQuoting := False;
  for I := 1 to Length(AValue) do
  begin
    if (AValue[I] = ADelimiter) or (AValue[I] = '"') or
       (AValue[I] = #10) or (AValue[I] = #13) then
    begin
      NeedsQuoting := True;
      Break;
    end;
  end;

  if NeedsQuoting then
    Result := '"' + StringReplace(AValue, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := AValue;
end;

class function TGocciaCSVStringifier.Stringify(const AData: TGocciaValue;
  const ADelimiter: Char; const AHeaders: Boolean): string;
var
  Arr: TGocciaArrayValue;
  Builder: TStringList;
  I, J: Integer;
  Item: TGocciaValue;
  Key: string;
  Keys: TArray<string>;
  Line: string;
  Obj: TGocciaObjectValue;
  Row: TGocciaArrayValue;
begin
  if not (AData is TGocciaArrayValue) then
    Exit('');

  Arr := TGocciaArrayValue(AData);
  if Arr.Elements.Count = 0 then
    Exit('');

  Builder := TStringList.Create;
  try
    Builder.LineBreak := #10;

    if (Arr.Elements[0] is TGocciaObjectValue) and
       not (Arr.Elements[0] is TGocciaArrayValue) and AHeaders then
    begin
      Obj := TGocciaObjectValue(Arr.Elements[0]);
      Keys := Obj.GetOwnPropertyKeys;

      Line := '';
      for I := 0 to Length(Keys) - 1 do
      begin
        if I > 0 then
          Line := Line + ADelimiter;
        Line := Line + EscapeField(Keys[I], ADelimiter);
      end;
      Builder.Add(Line);

      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if not (Arr.Elements[I] is TGocciaObjectValue) then
          Continue;
        Obj := TGocciaObjectValue(Arr.Elements[I]);
        Line := '';
        for J := 0 to Length(Keys) - 1 do
        begin
          if J > 0 then
            Line := Line + ADelimiter;
          Item := Obj.GetProperty(Keys[J]);
          if Assigned(Item) and not (Item is TGocciaUndefinedLiteralValue) then
            Line := Line + EscapeField(Item.ToStringLiteral.Value, ADelimiter)
          else
            Line := Line + '';
        end;
        Builder.Add(Line);
      end;
    end
    else
    begin
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if Arr.Elements[I] is TGocciaArrayValue then
        begin
          Row := TGocciaArrayValue(Arr.Elements[I]);
          Line := '';
          for J := 0 to Row.Elements.Count - 1 do
          begin
            if J > 0 then
              Line := Line + ADelimiter;
            Line := Line + EscapeField(
              Row.Elements[J].ToStringLiteral.Value, ADelimiter);
          end;
          Builder.Add(Line);
        end;
      end;
    end;

    Result := Builder.Text;
    if (Length(Result) > 0) and (Result[Length(Result)] = #10) then
      SetLength(Result, Length(Result) - 1);
  finally
    Builder.Free;
  end;
end;

end.
