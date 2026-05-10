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

function IsPrivateUsePrefix(const AStr: string): Boolean;
begin
  Result := (Length(AStr) = 1) and ((AStr[1] = 'x') or (AStr[1] = 'X'));
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
    Result.PrivateUse := '';
    while Idx < SubtagCount do
    begin
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
      Result := Result + '-' + SortedExts[I].Singleton + '-' + SortedExts[I].Value;
  end;

  if Normalized.PrivateUse <> '' then
  begin
    if Result <> '' then
      Result := Result + '-';
    Result := Result + Normalized.PrivateUse;
  end;
end;

end.
