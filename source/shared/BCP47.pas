unit BCP47;

{$I Shared.inc}

interface

type
  TBcp47Extension = record
    Singleton: Char;
    Value: string;
  end;
  TBcp47ExtensionArray = array of TBcp47Extension;

  TBcp47Tag = record
    Language: string;
    Script: string;
    Region: string;
    Variants: array of string;
    Extensions: TBcp47ExtensionArray;
    PrivateUse: string;
    IsValid: Boolean;
  end;

function ParseBcp47Tag(const ATag: string): TBcp47Tag;
function IsStructurallyValidLanguageTag(const ATag: string): Boolean;
function IsStructurallyValidUnicodeLocaleIdentifierTag(const ATag: string): Boolean;
function CanonicalizeBcp47Tag(const ATag: TBcp47Tag): string;
function NormalizeBcp47Case(const ATag: TBcp47Tag): TBcp47Tag;

implementation

uses
  SysUtils,

  IntlTypes;

type
  TSubtags = array of string;

function SplitByHyphen(const AStr: string): TSubtags;
var
  Count, Start, I: Integer;
begin
  Count := 1;
  for I := 1 to Length(AStr) do
    if AStr[I] = '-' then
      Inc(Count);

  SetLength(Result, Count);
  Count := 0;
  Start := 1;
  for I := 1 to Length(AStr) + 1 do
  begin
    if (I > Length(AStr)) or (AStr[I] = '-') then
    begin
      Result[Count] := Copy(AStr, Start, I - Start);
      Inc(Count);
      Start := I + 1;
    end;
  end;
end;

function IsAlpha(const ACh: Char): Boolean;
begin
  Result := ((ACh >= 'a') and (ACh <= 'z')) or ((ACh >= 'A') and (ACh <= 'Z'));
end;

function IsDigit(const ACh: Char): Boolean;
begin
  Result := (ACh >= '0') and (ACh <= '9');
end;

function IsAlphaNum(const ACh: Char): Boolean;
begin
  Result := IsAlpha(ACh) or IsDigit(ACh);
end;

function IsAllAlpha(const AStr: string): Boolean;
var
  I: Integer;
begin
  Result := Length(AStr) > 0;
  for I := 1 to Length(AStr) do
    if not IsAlpha(AStr[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsAllDigit(const AStr: string): Boolean;
var
  I: Integer;
begin
  Result := Length(AStr) > 0;
  for I := 1 to Length(AStr) do
    if not IsDigit(AStr[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsAllAlphaNum(const AStr: string): Boolean;
var
  I: Integer;
begin
  Result := Length(AStr) > 0;
  for I := 1 to Length(AStr) do
    if not IsAlphaNum(AStr[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function TitleCase(const AStr: string): string;
var
  I: Integer;
begin
  Result := LowerCase(AStr);
  if Length(Result) > 0 then
  begin
    I := 1;
    if (Result[I] >= 'a') and (Result[I] <= 'z') then
      Result[I] := Chr(Ord(Result[I]) - 32);
  end;
end;

function IsLanguageSubtag(const AStr: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(AStr);
  Result := ((Len >= 2) and (Len <= 8)) and IsAllAlpha(AStr);
end;

function IsUnicodeLanguageSubtag(const AStr: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(AStr);
  Result := (((Len >= 2) and (Len <= 3)) or ((Len >= 5) and (Len <= 8))) and
            IsAllAlpha(AStr);
end;

function IsScriptSubtag(const AStr: string): Boolean;
begin
  Result := (Length(AStr) = 4) and IsAllAlpha(AStr);
end;

function IsRegionSubtag(const AStr: string): Boolean;
begin
  Result := ((Length(AStr) = 2) and IsAllAlpha(AStr)) or
            ((Length(AStr) = 3) and IsAllDigit(AStr));
end;

function IsVariantSubtag(const AStr: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(AStr);
  if not IsAllAlphaNum(AStr) then
  begin
    Result := False;
    Exit;
  end;
  Result := ((Len >= 5) and (Len <= 8)) or
            ((Len = 4) and IsDigit(AStr[1]));
end;

function IsSingleton(const AStr: string): Boolean;
begin
  Result := (Length(AStr) = 1) and IsAlphaNum(AStr[1]) and
            (AStr[1] <> 'x') and (AStr[1] <> 'X');
end;

function IsExtensionSubtag(const AStr: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(AStr);
  Result := ((Len >= 2) and (Len <= 8)) and IsAllAlphaNum(AStr);
end;

function IsUnicodeTypeSubtag(const AStr: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(AStr);
  Result := ((Len >= 3) and (Len <= 8)) and IsAllAlphaNum(AStr);
end;

function IsUnicodeExtensionKey(const AStr: string): Boolean;
begin
  Result := (Length(AStr) = 2) and IsAlphaNum(AStr[1]) and IsAlpha(AStr[2]);
end;

function IsTransformExtensionKey(const AStr: string): Boolean;
begin
  Result := (Length(AStr) = 2) and IsAlpha(AStr[1]) and IsDigit(AStr[2]);
end;

function IsPrivateUsePrefix(const AStr: string): Boolean;
begin
  Result := (Length(AStr) = 1) and ((AStr[1] = 'x') or (AStr[1] = 'X'));
end;

function IsPrivateUseSubtag(const AStr: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(AStr);
  Result := ((Len >= 1) and (Len <= 8)) and IsAllAlphaNum(AStr);
end;

function IsGrandfatheredTag(const ATag: string): Boolean;
var
  Lower: string;
begin
  Lower := LowerCase(ATag);
  Result :=
    (Lower = 'en-gb-oed') or
    (Lower = 'i-ami') or
    (Lower = 'i-bnn') or
    (Lower = 'i-default') or
    (Lower = 'i-enochian') or
    (Lower = 'i-hak') or
    (Lower = 'i-klingon') or
    (Lower = 'i-lux') or
    (Lower = 'i-mingo') or
    (Lower = 'i-navajo') or
    (Lower = 'i-pwn') or
    (Lower = 'i-tao') or
    (Lower = 'i-tay') or
    (Lower = 'i-tsu') or
    (Lower = 'sgn-be-fr') or
    (Lower = 'sgn-be-nl') or
    (Lower = 'sgn-ch-de') or
    (Lower = 'art-lojban') or
    (Lower = 'cel-gaulish') or
    (Lower = 'no-bok') or
    (Lower = 'no-nyn') or
    (Lower = 'zh-guoyu') or
    (Lower = 'zh-hakka') or
    (Lower = 'zh-min') or
    (Lower = 'zh-min-nan') or
    (Lower = 'zh-xiang');
end;

function SortVariants(const AVars: array of string): TSubtags;
var
  I, J: Integer;
  Temp: string;
begin
  SetLength(Result, Length(AVars));
  for I := 0 to High(AVars) do
    Result[I] := LowerCase(AVars[I]);

  for I := 0 to High(Result) - 1 do
    for J := I + 1 to High(Result) do
      if CompareStr(Result[I], Result[J]) > 0 then
      begin
        Temp := Result[I];
        Result[I] := Result[J];
        Result[J] := Temp;
      end;
end;

function SortExtensions(const AExts: TBcp47ExtensionArray): TBcp47ExtensionArray;
var
  I, J: Integer;
  Temp: TBcp47Extension;
begin
  SetLength(Result, Length(AExts));
  for I := 0 to High(AExts) do
    Result[I] := AExts[I];

  for I := 0 to High(Result) - 1 do
    for J := I + 1 to High(Result) do
      if Ord(Result[I].Singleton) > Ord(Result[J].Singleton) then
      begin
        Temp := Result[I];
        Result[I] := Result[J];
        Result[J] := Temp;
      end;
end;

function AppendLowerSubtag(const AValue, ASubtag: string): string;
begin
  if ASubtag = '' then
    Result := AValue
  else if AValue = '' then
    Result := LowerCase(ASubtag)
  else
    Result := AValue + '-' + LowerCase(ASubtag);
end;

function HasStringValue(const AValues: array of string; const ACount: Integer;
  const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ACount - 1 do
    if CompareText(AValues[I], AValue) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function IsPermittedGrandfatheredUnicodeLocaleIdentifier(const ATag: string): Boolean;
var
  Lower: string;
begin
  Lower := LowerCase(ATag);
  Result :=
    (Lower = 'art-lojban') or
    (Lower = 'cel-gaulish') or
    (Lower = 'zh-guoyu') or
    (Lower = 'zh-hakka') or
    (Lower = 'zh-xiang');
end;

function IsValidUnicodeExtensionValue(const AValue: string): Boolean;
var
  Parts: TSubtags;
  Index: Integer;
begin
  Parts := SplitByHyphen(AValue);
  if Length(Parts) = 0 then
    Exit(False);

  Index := 0;
  while (Index <= High(Parts)) and IsUnicodeTypeSubtag(Parts[Index]) do
    Inc(Index);

  while Index <= High(Parts) do
  begin
    if not IsUnicodeExtensionKey(Parts[Index]) then
      Exit(False);
    Inc(Index);

    while (Index <= High(Parts)) and not IsUnicodeExtensionKey(Parts[Index]) do
    begin
      if not IsUnicodeTypeSubtag(Parts[Index]) then
        Exit(False);
      Inc(Index);
    end;
  end;

  Result := True;
end;

function ConsumeTransformedLanguage(const AParts: TSubtags; var AIndex: Integer): Boolean;
var
  Variants: TSubtags;
  VariantCount, I: Integer;
begin
  Result := False;
  VariantCount := 0;
  SetLength(Variants, 0);

  if (AIndex > High(AParts)) or not IsUnicodeLanguageSubtag(AParts[AIndex]) then
    Exit;
  Inc(AIndex);

  if (AIndex <= High(AParts)) and IsScriptSubtag(AParts[AIndex]) then
    Inc(AIndex);

  if (AIndex <= High(AParts)) and IsRegionSubtag(AParts[AIndex]) then
    Inc(AIndex);

  while (AIndex <= High(AParts)) and IsVariantSubtag(AParts[AIndex]) do
  begin
    if HasStringValue(Variants, VariantCount, LowerCase(AParts[AIndex])) then
      Exit(False);
    Inc(VariantCount);
    SetLength(Variants, VariantCount);
    Variants[VariantCount - 1] := LowerCase(AParts[AIndex]);
    Inc(AIndex);
  end;

  for I := 0 to High(Variants) do
    if Variants[I] = '' then
      Exit(False);

  Result := True;
end;

function ConsumeTransformedFields(const AParts: TSubtags; var AIndex: Integer): Boolean;
var
  ValueCount: Integer;
begin
  while AIndex <= High(AParts) do
  begin
    if not IsTransformExtensionKey(AParts[AIndex]) then
      Exit(False);
    Inc(AIndex);

    ValueCount := 0;
    while (AIndex <= High(AParts)) and not IsTransformExtensionKey(AParts[AIndex]) do
    begin
      if not IsUnicodeTypeSubtag(AParts[AIndex]) then
        Exit(False);
      Inc(ValueCount);
      Inc(AIndex);
    end;

    if ValueCount = 0 then
      Exit(False);
  end;

  Result := True;
end;

function IsValidTransformedExtensionValue(const AValue: string): Boolean;
var
  Parts: TSubtags;
  Index: Integer;
begin
  Parts := SplitByHyphen(AValue);
  if Length(Parts) = 0 then
    Exit(False);

  Index := 0;
  if IsUnicodeLanguageSubtag(Parts[Index]) then
  begin
    if not ConsumeTransformedLanguage(Parts, Index) then
      Exit(False);
    if Index > High(Parts) then
      Exit(True);
  end;

  Result := ConsumeTransformedFields(Parts, Index);
end;

function IsStructurallyValidUnicodeLocaleIdentifierTag(const ATag: string): Boolean;
var
  Lower: string;
  Parsed: TBcp47Tag;
  I: Integer;
begin
  Lower := LowerCase(ATag);
  if Copy(Lower, 1, 2) = 'x-' then
    Exit(False);

  Parsed := ParseBcp47Tag(ATag);
  if not Parsed.IsValid then
    Exit(False);

  if Pos('-', Parsed.Language) > 0 then
    Exit(IsPermittedGrandfatheredUnicodeLocaleIdentifier(ATag));

  if not IsUnicodeLanguageSubtag(Parsed.Language) then
    Exit(False);

  for I := 0 to High(Parsed.Extensions) do
  begin
    if (Parsed.Extensions[I].Singleton = 'u') and
       not IsValidUnicodeExtensionValue(Parsed.Extensions[I].Value) then
      Exit(False);
    if (Parsed.Extensions[I].Singleton = 't') and
       not IsValidTransformedExtensionValue(Parsed.Extensions[I].Value) then
      Exit(False);
  end;

  Result := True;
end;

function CanonicalizeUnicodeTypeValue(const AKey, AValue: string): string;
var
  Key, Value: string;
begin
  Key := LowerCase(AKey);
  Value := LowerCase(AValue);

  if Value = 'true' then
    Exit('');

  if ((Key = 'kb') or (Key = 'kc') or (Key = 'kh') or
      (Key = 'kk') or (Key = 'kn')) and (Value = 'yes') then
    Exit('');

  if Key = 'ca' then
  begin
    if Value = 'gregorian' then
      Exit('gregory');
    if Value = 'ethiopic-amete-alem' then
      Exit('ethioaa');
    if Value = 'islamicc' then
      Exit('islamic-civil');
  end;

  if Key = 'co' then
  begin
    if Value = 'phonebook' then
      Exit('phonebk');
    if Value = 'traditional' then
      Exit('trad');
    if Value = 'dictionary' then
      Exit('dict');
  end;

  if Key = 'ks' then
  begin
    if Value = 'primary' then
      Exit('level1');
    if Value = 'tertiary' then
      Exit('level3');
  end;

  if Key = 'ms' then
  begin
    if Value = 'imperial' then
      Exit('uksystem');
  end;

  if Key = 'tz' then
  begin
    if Value = 'cnckg' then
      Exit('cnsha');
    if Value = 'eire' then
      Exit('iedub');
    if Value = 'est' then
      Exit('papty');
    if Value = 'gmt0' then
      Exit('gmt');
    if (Value = 'uct') or (Value = 'zulu') then
      Exit('utc');
  end;

  if (Key = 'rg') or (Key = 'sd') then
  begin
    if Value = 'no23' then Exit('no50');
    if Value = 'cn11' then Exit('cnbj');
    if Value = 'cz10a' then Exit('cz110');
    if (Value = 'fra') or (Value = 'frg') then Exit('frges');
    if Value = 'lud' then Exit('lucl');
  end;

  Result := Value;
end;

function CanonicalizeUnicodeExtensionValue(const AValue: string): string;
var
  Parts: TSubtags;
  Attributes, Keys, Values: TSubtags;
  AttributeCount, KeyCount, Index, I, J: Integer;
  Key, Value, Temp: string;
begin
  Parts := SplitByHyphen(LowerCase(AValue));
  AttributeCount := 0;
  KeyCount := 0;
  SetLength(Attributes, Length(Parts));
  SetLength(Keys, Length(Parts));
  SetLength(Values, Length(Parts));

  Index := 0;
  while (Index <= High(Parts)) and IsUnicodeTypeSubtag(Parts[Index]) do
  begin
    if not HasStringValue(Attributes, AttributeCount, Parts[Index]) then
    begin
      Attributes[AttributeCount] := Parts[Index];
      Inc(AttributeCount);
    end;
    Inc(Index);
  end;

  while Index <= High(Parts) do
  begin
    Key := Parts[Index];
    Inc(Index);
    Value := '';
    while (Index <= High(Parts)) and not IsUnicodeExtensionKey(Parts[Index]) do
    begin
      Value := AppendLowerSubtag(Value, Parts[Index]);
      Inc(Index);
    end;

    if HasStringValue(Keys, KeyCount, Key) then
      Continue;

    Keys[KeyCount] := Key;
    Values[KeyCount] := CanonicalizeUnicodeTypeValue(Key, Value);
    Inc(KeyCount);
  end;

  for I := 0 to AttributeCount - 2 do
    for J := I + 1 to AttributeCount - 1 do
      if CompareStr(Attributes[I], Attributes[J]) > 0 then
      begin
        Temp := Attributes[I];
        Attributes[I] := Attributes[J];
        Attributes[J] := Temp;
      end;

  for I := 0 to KeyCount - 2 do
    for J := I + 1 to KeyCount - 1 do
      if CompareStr(Keys[I], Keys[J]) > 0 then
      begin
        Temp := Keys[I];
        Keys[I] := Keys[J];
        Keys[J] := Temp;
        Temp := Values[I];
        Values[I] := Values[J];
        Values[J] := Temp;
      end;

  Result := '';
  for I := 0 to AttributeCount - 1 do
    Result := AppendLowerSubtag(Result, Attributes[I]);
  for I := 0 to KeyCount - 1 do
  begin
    Result := AppendLowerSubtag(Result, Keys[I]);
    Result := AppendLowerSubtag(Result, Values[I]);
  end;
end;

function CanonicalizeTransformedLanguageValue(const AParts: TSubtags;
  const AStartIndex, AEndIndex: Integer): string;
var
  Parsed: TBcp47Tag;
  I, VariantCount: Integer;
  Variants: TSubtags;
begin
  Parsed.Language := LowerCase(AParts[AStartIndex]);
  if Parsed.Language = 'iw' then
    Parsed.Language := 'he'
  else if Parsed.Language = 'ji' then
    Parsed.Language := 'yi'
  else if Parsed.Language = 'in' then
    Parsed.Language := 'id'
  else if Parsed.Language = 'cmn' then
    Parsed.Language := 'zh';

  Parsed.Script := '';
  Parsed.Region := '';
  SetLength(Parsed.Variants, 0);
  SetLength(Parsed.Extensions, 0);
  Parsed.PrivateUse := '';
  Parsed.IsValid := True;

  I := AStartIndex + 1;
  if (I <= AEndIndex) and IsScriptSubtag(AParts[I]) then
  begin
    Parsed.Script := AParts[I];
    Inc(I);
  end;

  if (I <= AEndIndex) and IsRegionSubtag(AParts[I]) then
  begin
    Parsed.Region := AParts[I];
    Inc(I);
  end;

  VariantCount := 0;
  SetLength(Variants, AEndIndex - I + 1);
  while I <= AEndIndex do
  begin
    Variants[VariantCount] := AParts[I];
    Inc(VariantCount);
    Inc(I);
  end;
  SetLength(Parsed.Variants, VariantCount);
  for I := 0 to VariantCount - 1 do
    Parsed.Variants[I] := Variants[I];

  Result := LowerCase(CanonicalizeBcp47Tag(Parsed));
end;

function CanonicalizeTransformedTypeValue(const AKey, AValue: string): string;
begin
  if (LowerCase(AKey) = 'm0') and (LowerCase(AValue) = 'names') then
    Result := 'prprname'
  else
    Result := LowerCase(AValue);
end;

function CanonicalizeTransformedExtensionValue(const AValue: string): string;
var
  Parts, Keys, Values: TSubtags;
  Index, LanguageStart, LanguageEnd, KeyCount, I, J: Integer;
  Key, Value, Temp: string;
begin
  Parts := SplitByHyphen(LowerCase(AValue));
  Result := '';
  Index := 0;

  if (Index <= High(Parts)) and IsUnicodeLanguageSubtag(Parts[Index]) then
  begin
    LanguageStart := Index;
    if ConsumeTransformedLanguage(Parts, Index) then
    begin
      LanguageEnd := Index - 1;
      Result := CanonicalizeTransformedLanguageValue(Parts, LanguageStart, LanguageEnd);
    end
    else
      Index := LanguageStart;
  end;

  KeyCount := 0;
  SetLength(Keys, Length(Parts));
  SetLength(Values, Length(Parts));
  while Index <= High(Parts) do
  begin
    Key := Parts[Index];
    Inc(Index);
    Value := '';
    while (Index <= High(Parts)) and not IsTransformExtensionKey(Parts[Index]) do
    begin
      Value := AppendLowerSubtag(Value, Parts[Index]);
      Inc(Index);
    end;
    Keys[KeyCount] := Key;
    Values[KeyCount] := CanonicalizeTransformedTypeValue(Key, Value);
    Inc(KeyCount);
  end;

  for I := 0 to KeyCount - 2 do
    for J := I + 1 to KeyCount - 1 do
      if CompareStr(Keys[I], Keys[J]) > 0 then
      begin
        Temp := Keys[I];
        Keys[I] := Keys[J];
        Keys[J] := Temp;
        Temp := Values[I];
        Values[I] := Values[J];
        Values[J] := Temp;
      end;

  for I := 0 to KeyCount - 1 do
  begin
    Result := AppendLowerSubtag(Result, Keys[I]);
    Result := AppendLowerSubtag(Result, Values[I]);
  end;
end;

function CanonicalizeExtensionValue(const ASingleton: Char; const AValue: string): string;
begin
  case ASingleton of
    'u': Result := CanonicalizeUnicodeExtensionValue(AValue);
    't': Result := CanonicalizeTransformedExtensionValue(AValue);
  else
    Result := LowerCase(AValue);
  end;
end;

function ParseBcp47Tag(const ATag: string): TBcp47Tag;
var
  Subtags: TSubtags;
  Idx, SubtagCount, VariantCount, ExtCount: Integer;
  ExtSingleton: Char;
  ExtValue: string;
  ExtParts: Integer;
  I, J: Integer;
begin
  Result.Language := '';
  Result.Script := '';
  Result.Region := '';
  SetLength(Result.Variants, 0);
  SetLength(Result.Extensions, 0);
  Result.PrivateUse := '';
  Result.IsValid := False;

  if ATag = '' then
    Exit;

  if ContainsNulCharacter(ATag) then
    Exit;

  if IsGrandfatheredTag(ATag) then
  begin
    Result.Language := LowerCase(ATag);
    Result.IsValid := True;
    Exit;
  end;

  Subtags := SplitByHyphen(ATag);
  SubtagCount := Length(Subtags);
  if SubtagCount = 0 then
    Exit;

  for Idx := 0 to SubtagCount - 1 do
    if Subtags[Idx] = '' then
      Exit;

  Idx := 0;

  if IsPrivateUsePrefix(Subtags[0]) then
  begin
    if SubtagCount < 2 then
      Exit;
    for I := 1 to SubtagCount - 1 do
      if not IsPrivateUseSubtag(Subtags[I]) then
        Exit;
    Result.PrivateUse := ATag;
    Result.IsValid := True;
    Exit;
  end;

  if not IsLanguageSubtag(Subtags[0]) then
    Exit;
  Result.Language := Subtags[0];
  Inc(Idx);

  if (Idx < SubtagCount) and IsScriptSubtag(Subtags[Idx]) then
  begin
    Result.Script := Subtags[Idx];
    Inc(Idx);
  end;

  if (Idx < SubtagCount) and IsRegionSubtag(Subtags[Idx]) then
  begin
    Result.Region := Subtags[Idx];
    Inc(Idx);
  end;

  VariantCount := 0;
  while (Idx < SubtagCount) and IsVariantSubtag(Subtags[Idx]) do
  begin
    Inc(VariantCount);
    SetLength(Result.Variants, VariantCount);
    Result.Variants[VariantCount - 1] := Subtags[Idx];
    Inc(Idx);
  end;

  for I := 0 to High(Result.Variants) - 1 do
    for J := I + 1 to High(Result.Variants) do
      if CompareText(Result.Variants[I], Result.Variants[J]) = 0 then
        Exit;

  ExtCount := 0;
  while (Idx < SubtagCount) and IsSingleton(Subtags[Idx]) do
  begin
    ExtSingleton := LowerCase(Subtags[Idx])[1];
    Inc(Idx);
    ExtValue := '';
    ExtParts := 0;

    while (Idx < SubtagCount) and IsExtensionSubtag(Subtags[Idx]) and
          not IsSingleton(Subtags[Idx]) and not IsPrivateUsePrefix(Subtags[Idx]) do
    begin
      if ExtParts > 0 then
        ExtValue := ExtValue + '-';
      ExtValue := ExtValue + Subtags[Idx];
      Inc(ExtParts);
      Inc(Idx);
    end;

    if ExtParts = 0 then
      Exit;

    Inc(ExtCount);
    SetLength(Result.Extensions, ExtCount);
    Result.Extensions[ExtCount - 1].Singleton := ExtSingleton;
    Result.Extensions[ExtCount - 1].Value := ExtValue;
  end;

  for I := 0 to High(Result.Extensions) - 1 do
    for J := I + 1 to High(Result.Extensions) do
      if Result.Extensions[I].Singleton = Result.Extensions[J].Singleton then
        Exit;

  if (Idx < SubtagCount) and IsPrivateUsePrefix(Subtags[Idx]) then
  begin
    if Idx + 1 >= SubtagCount then
      Exit;

    Result.PrivateUse := '';
    while Idx < SubtagCount do
    begin
      if (Result.PrivateUse <> '') and not IsPrivateUseSubtag(Subtags[Idx]) then
        Exit;
      if Result.PrivateUse <> '' then
        Result.PrivateUse := Result.PrivateUse + '-';
      Result.PrivateUse := Result.PrivateUse + Subtags[Idx];
      Inc(Idx);
    end;
  end;

  if Idx <> SubtagCount then
    Exit;

  Result.IsValid := True;
end;

function IsStructurallyValidLanguageTag(const ATag: string): Boolean;
var
  Parsed: TBcp47Tag;
begin
  Parsed := ParseBcp47Tag(ATag);
  Result := Parsed.IsValid;
end;

function NormalizeBcp47Case(const ATag: TBcp47Tag): TBcp47Tag;
var
  I: Integer;
begin
  Result := ATag;
  Result.Language := LowerCase(ATag.Language);
  Result.Script := TitleCase(ATag.Script);
  Result.Region := UpperCase(ATag.Region);

  SetLength(Result.Variants, Length(ATag.Variants));
  for I := 0 to High(ATag.Variants) do
    Result.Variants[I] := LowerCase(ATag.Variants[I]);

  SetLength(Result.Extensions, Length(ATag.Extensions));
  for I := 0 to High(ATag.Extensions) do
  begin
    Result.Extensions[I].Singleton := ATag.Extensions[I].Singleton;
    Result.Extensions[I].Value := LowerCase(ATag.Extensions[I].Value);
  end;

  Result.PrivateUse := LowerCase(ATag.PrivateUse);
end;

function CanonicalizeBcp47Tag(const ATag: TBcp47Tag): string;
var
  Normalized: TBcp47Tag;
  SortedVars: TSubtags;
  SortedExts: TBcp47ExtensionArray;
  I: Integer;
begin
  if not ATag.IsValid then
  begin
    Result := '';
    Exit;
  end;

  if (ATag.Script = '') and (ATag.Region = '') and
     (Length(ATag.Variants) = 0) and (Length(ATag.Extensions) = 0) and
     (ATag.PrivateUse <> '') and (ATag.Language = '') then
  begin
    Result := ATag.PrivateUse;
    Exit;
  end;

  Normalized := NormalizeBcp47Case(ATag);
  Result := Normalized.Language;

  if Normalized.Script <> '' then
    Result := Result + '-' + Normalized.Script;

  if Normalized.Region <> '' then
    Result := Result + '-' + Normalized.Region;

  if Length(Normalized.Variants) > 0 then
  begin
    SortedVars := SortVariants(Normalized.Variants);
    for I := 0 to High(SortedVars) do
      Result := Result + '-' + SortedVars[I];
  end;

  if Length(Normalized.Extensions) > 0 then
  begin
    SortedExts := SortExtensions(Normalized.Extensions);
    for I := 0 to High(SortedExts) do
      Result := Result + '-' + SortedExts[I].Singleton + '-' +
        CanonicalizeExtensionValue(SortedExts[I].Singleton, SortedExts[I].Value);
  end;

  if Normalized.PrivateUse <> '' then
  begin
    if Result <> '' then
      Result := Result + '-';
    Result := Result + Normalized.PrivateUse;
  end;
end;

end.
