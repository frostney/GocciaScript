unit Goccia.TSV;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  EGocciaTSVParseError = class(Exception);

  TGocciaTSVChunkParseResult = record
    Values: TGocciaArrayValue;
    Read: Integer;
    Done: Boolean;
    ErrorMessage: string;
  end;

  TGocciaTSVFieldInfo = record
    Value: string;
  end;

  TGocciaTSVParser = class
  private
    class function ClampOffset(const AValue, ALimit: Integer): Integer; static;
    class function HasUTF8BOM(const AText: string): Boolean; static;
    class function UnescapeField(const AField: string): string; static;
  public
    function Parse(const AText: string; const AHeaders: Boolean = True;
      const ASkipEmptyLines: Boolean = False): TGocciaArrayValue;
    function ParseChunk(const AText: string; const AHeaders: Boolean;
      const ASkipEmptyLines: Boolean; const AStart: Integer = 0;
      const AEnd: Integer = -1): TGocciaTSVChunkParseResult;
    function ParseWithFieldInfo(const AText: string;
      const AHeaders: Boolean = True;
      const ASkipEmptyLines: Boolean = False):
      TArray<TArray<TGocciaTSVFieldInfo>>;
  end;

  TGocciaTSVStringifier = class
  public
    class function Stringify(const AData: TGocciaValue;
      const AHeaders: Boolean = True): string; static;
    class function EscapeField(const AValue: string): string; static;
  end;

implementation

uses
  Classes,

  BOM;

class function TGocciaTSVParser.ClampOffset(const AValue,
  ALimit: Integer): Integer;
begin
  if AValue < 0 then
    Exit(0);
  if AValue > ALimit then
    Exit(ALimit);
  Result := AValue;
end;

class function TGocciaTSVParser.HasUTF8BOM(const AText: string): Boolean;
begin
  Result := HasUTF8BOMString(AText);
end;

class function TGocciaTSVParser.UnescapeField(const AField: string): string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(AField) do
  begin
    if (AField[I] = '\') and (I + 1 <= Length(AField)) then
    begin
      case AField[I + 1] of
        't': begin Result := Result + #9; Inc(I, 2); end;
        'n': begin Result := Result + #10; Inc(I, 2); end;
        'r': begin Result := Result + #13; Inc(I, 2); end;
        '\': begin Result := Result + '\'; Inc(I, 2); end;
      else
        Result := Result + AField[I];
        Inc(I);
      end;
    end
    else
    begin
      Result := Result + AField[I];
      Inc(I);
    end;
  end;
end;

function ParseTSVRow(const AText: string; var APos: Integer;
  const AEnd: Integer; out AFields: TArray<string>;
  out AConsumed: Boolean): Boolean;
var
  FieldCount: Integer;
  FieldCapacity: Integer;
  FieldStart: Integer;
  Ch: Char;
begin
  Result := False;
  AConsumed := False;
  if APos > AEnd then
    Exit;

  FieldCount := 0;
  FieldCapacity := 8;
  SetLength(AFields, FieldCapacity);
  FieldStart := APos;

  while APos <= AEnd do
  begin
    Ch := AText[APos];

    if Ch = #9 then
    begin
      if FieldCount >= FieldCapacity then
      begin
        FieldCapacity := FieldCapacity * 2;
        SetLength(AFields, FieldCapacity);
      end;
      AFields[FieldCount] := TGocciaTSVParser.UnescapeField(
        Copy(AText, FieldStart, APos - FieldStart));
      Inc(FieldCount);
      Inc(APos);
      FieldStart := APos;
    end
    else if (Ch = #13) or (Ch = #10) then
    begin
      if FieldCount >= FieldCapacity then
      begin
        FieldCapacity := FieldCapacity + 1;
        SetLength(AFields, FieldCapacity);
      end;
      AFields[FieldCount] := TGocciaTSVParser.UnescapeField(
        Copy(AText, FieldStart, APos - FieldStart));
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
      Inc(APos);
  end;

  if FieldCount >= FieldCapacity then
  begin
    FieldCapacity := FieldCapacity + 1;
    SetLength(AFields, FieldCapacity);
  end;
  AFields[FieldCount] := TGocciaTSVParser.UnescapeField(
    Copy(AText, FieldStart, APos - FieldStart));
  Inc(FieldCount);
  SetLength(AFields, FieldCount);
  AConsumed := False;
  Result := True;
end;

function IsEmptyTSVRow(const AFields: TArray<string>): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(AFields) - 1 do
    if AFields[I] <> '' then
      Exit(False);
  Result := (Length(AFields) <= 1);
end;

function TGocciaTSVParser.Parse(const AText: string;
  const AHeaders: Boolean;
  const ASkipEmptyLines: Boolean): TGocciaArrayValue;
var
  Consumed: Boolean;
  EndIndex: Integer;
  Fields: TArray<string>;
  HeaderFields: TArray<string>;
  I: Integer;
  Obj: TGocciaObjectValue;
  Pos: Integer;
  Row: TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  if Length(AText) = 0 then
    Exit;

  Pos := 1;
  EndIndex := Length(AText);
  if HasUTF8BOM(AText) then
    Inc(Pos, UTF8_BOM_LEN);

  if AHeaders then
  begin
    if not ParseTSVRow(AText, Pos, EndIndex, HeaderFields, Consumed) then
      Exit;
  end;

  while Pos <= EndIndex do
  begin
    if not ParseTSVRow(AText, Pos, EndIndex, Fields, Consumed) then
      Break;

    if ASkipEmptyLines and IsEmptyTSVRow(Fields) then
      Continue;

    if AHeaders then
    begin
      Obj := TGocciaObjectValue.Create;
      for I := 0 to Length(HeaderFields) - 1 do
      begin
        if I < Length(Fields) then
          Obj.AssignProperty(HeaderFields[I],
            TGocciaStringLiteralValue.Create(Fields[I]))
        else
          Obj.AssignProperty(HeaderFields[I],
            TGocciaStringLiteralValue.Create(''));
      end;
      Result.Elements.Add(Obj);
    end
    else
    begin
      Row := TGocciaArrayValue.Create;
      for I := 0 to Length(Fields) - 1 do
        Row.Elements.Add(TGocciaStringLiteralValue.Create(Fields[I]));
      Result.Elements.Add(Row);
    end;
  end;
end;

function TGocciaTSVParser.ParseWithFieldInfo(const AText: string;
  const AHeaders: Boolean;
  const ASkipEmptyLines: Boolean): TArray<TArray<TGocciaTSVFieldInfo>>;
var
  Consumed: Boolean;
  Count: Integer;
  EndIndex: Integer;
  Fields: TArray<string>;
  FieldInfoRow: TArray<TGocciaTSVFieldInfo>;
  I: Integer;
  Pos: Integer;
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
    Inc(Pos, UTF8_BOM_LEN);

  while Pos <= EndIndex do
  begin
    if not ParseTSVRow(AText, Pos, EndIndex, Fields, Consumed) then
      Break;

    if ASkipEmptyLines and IsEmptyTSVRow(Fields) then
      Continue;

    SetLength(FieldInfoRow, Length(Fields));
    for I := 0 to Length(Fields) - 1 do
      FieldInfoRow[I].Value := Fields[I];

    if Count >= Length(Result) then
      SetLength(Result, Length(Result) * 2);
    Result[Count] := FieldInfoRow;
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

function TGocciaTSVParser.ParseChunk(const AText: string;
  const AHeaders: Boolean; const ASkipEmptyLines: Boolean;
  const AStart: Integer; const AEnd: Integer): TGocciaTSVChunkParseResult;
var
  Consumed: Boolean;
  EffectiveEnd: Integer;
  EffectiveStart: Integer;
  Fields: TArray<string>;
  HeaderFields: TArray<string>;
  I: Integer;
  Obj: TGocciaObjectValue;
  Pos: Integer;
  ResumeOffset: Integer;
  Row: TGocciaArrayValue;
begin
  Result.Values := TGocciaArrayValue.Create;
  Result.Done := True;
  Result.ErrorMessage := '';

  if AEnd < 0 then
    EffectiveEnd := Length(AText)
  else
    EffectiveEnd := ClampOffset(AEnd, Length(AText));

  EffectiveStart := ClampOffset(AStart, Length(AText));
  if (EffectiveStart = 0) and HasUTF8BOM(AText) then
    Inc(EffectiveStart, UTF8_BOM_LEN);

  Result.Read := EffectiveStart;
  if EffectiveStart >= EffectiveEnd then
    Exit;

  Pos := EffectiveStart + 1;
  ResumeOffset := EffectiveStart;

  if AHeaders and (AStart = 0) then
  begin
    if not ParseTSVRow(AText, Pos, EffectiveEnd, HeaderFields, Consumed) then
    begin
      Result.Read := ResumeOffset;
      Result.Done := False;
      Exit;
    end;
    if not Consumed then
    begin
      Result.Read := ResumeOffset;
      Result.Done := False;
      Exit;
    end;
    ResumeOffset := Pos - 1;
  end;

  while Pos <= EffectiveEnd do
  begin
    if not ParseTSVRow(AText, Pos, EffectiveEnd, Fields, Consumed) then
    begin
      Result.Read := ResumeOffset;
      Result.Done := False;
      Exit;
    end;
    if not Consumed then
    begin
      Result.Read := ResumeOffset;
      Result.Done := False;
      Exit;
    end;

    if ASkipEmptyLines and IsEmptyTSVRow(Fields) then
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
          Obj.AssignProperty(HeaderFields[I],
            TGocciaStringLiteralValue.Create(Fields[I]))
        else
          Obj.AssignProperty(HeaderFields[I],
            TGocciaStringLiteralValue.Create(''));
      end;
      Result.Values.Elements.Add(Obj);
    end
    else
    begin
      Row := TGocciaArrayValue.Create;
      for I := 0 to Length(Fields) - 1 do
        Row.Elements.Add(TGocciaStringLiteralValue.Create(Fields[I]));
      Result.Values.Elements.Add(Row);
    end;

    ResumeOffset := Pos - 1;
  end;

  Result.Read := EffectiveEnd;
end;

class function TGocciaTSVStringifier.EscapeField(
  const AValue: string): string;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
end;

class function TGocciaTSVStringifier.Stringify(const AData: TGocciaValue;
  const AHeaders: Boolean): string;
var
  Arr: TGocciaArrayValue;
  Builder: TStringList;
  I, J: Integer;
  Item: TGocciaValue;
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
          Line := Line + #9;
        Line := Line + EscapeField(Keys[I]);
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
            Line := Line + #9;
          Item := Obj.GetProperty(Keys[J]);
          if Assigned(Item) and not (Item is TGocciaUndefinedLiteralValue) then
            Line := Line + EscapeField(Item.ToStringLiteral.Value)
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
              Line := Line + #9;
            Line := Line + EscapeField(
              Row.Elements[J].ToStringLiteral.Value);
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
