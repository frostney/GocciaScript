unit IntlLocaleResolver;

{$I Shared.inc}

interface

uses
  IntlTypes;

function CanonicalizeUnicodeLocaleId(const ATag: string): string;
function CanonicalizeLocaleList(const ATags: IntlTypes.TStringArray): IntlTypes.TStringArray;
function SupportedLocalesOf(const ARequestedLocales: IntlTypes.TStringArray): IntlTypes.TStringArray;
function BestAvailableLocale(const AAvailableLocales: IntlTypes.TStringArray;
  const ALocale: string): string;
function LookupMatcher(const AAvailableLocales, ARequestedLocales: IntlTypes.TStringArray): TIntlResolvedLocale;
function BestFitMatcher(const AAvailableLocales, ARequestedLocales: IntlTypes.TStringArray): TIntlResolvedLocale;
function DefaultLocale: string;

implementation

uses
  SysUtils,

  BCP47,
  IntlICU,

  Goccia.Intl.CLDRData;

var
  AvailableLocalesCache: IntlTypes.TStringArray;
  AvailableLocalesLoaded: Boolean;
  AvailableLocalesLock: TRTLCriticalSection;

procedure AppendUniqueLocale(var ALocales: IntlTypes.TStringArray;
  var ACount: Integer; const ALocale: string);
var
  I: Integer;
begin
  if ALocale = '' then
    Exit;

  for I := 0 to ACount - 1 do
    if CompareText(ALocales[I], ALocale) = 0 then
      Exit;

  if ACount >= Length(ALocales) then
    SetLength(ALocales, ACount + 16);
  ALocales[ACount] := ALocale;
  Inc(ACount);
end;

// ECMA-402 ES2026 §9.2.3 LookupMatchingLocaleByPrefix(availableLocales, requestedLocales)
function LocaleWithoutUnicodeExtension(const AParsed: TBcp47Tag): string; overload;
var
  Parsed: TBcp47Tag;
  FilteredExtensions: TBcp47ExtensionArray;
  I, Count: Integer;
begin
  Result := '';
  if not AParsed.IsValid then
    Exit;

  Parsed := AParsed;
  Count := 0;
  SetLength(FilteredExtensions, Length(Parsed.Extensions));
  for I := 0 to High(Parsed.Extensions) do
  begin
    if Parsed.Extensions[I].Singleton = 'u' then
      Continue;
    FilteredExtensions[Count] := Parsed.Extensions[I];
    Inc(Count);
  end;
  SetLength(FilteredExtensions, Count);
  Parsed.Extensions := FilteredExtensions;

  Result := CanonicalizeBcp47Tag(Parsed);
end;

function LocaleWithoutUnicodeExtension(const ALocale: string): string; overload;
var
  Parsed: TBcp47Tag;
begin
  Parsed := ParseBcp47Tag(ALocale);
  Result := LocaleWithoutUnicodeExtension(Parsed);
end;

// ECMA-402 ES2026 supportedLocalesOf constructors use [[AvailableLocales]].
function AvailableLocaleList: IntlTypes.TStringArray;
var
  Available: IntlTypes.TStringArray;
  Canonical: string;
  I, Count: Integer;
begin
  EnterCriticalSection(AvailableLocalesLock);
  try
    if not AvailableLocalesLoaded then
    begin
      SetLength(AvailableLocalesCache, 0);
      if TryICUGetAvailableLocales(Available) then
      begin
        Count := 0;
        SetLength(AvailableLocalesCache, Length(Available));
        for I := 0 to High(Available) do
        begin
          Canonical := CanonicalizeUnicodeLocaleId(Available[I]);
          AppendUniqueLocale(AvailableLocalesCache, Count, Canonical);
        end;
        SetLength(AvailableLocalesCache, Count);
      end;
      AvailableLocalesLoaded := True;
    end;

    Result := AvailableLocalesCache;
  finally
    LeaveCriticalSection(AvailableLocalesLock);
  end;
end;

function SplitBySeparator(const AValue: string; const ASeparator: Char): IntlTypes.TStringArray;
var
  Count, StartIndex, Index: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  StartIndex := 1;

  for Index := 1 to Length(AValue) + 1 do
  begin
    if (Index > Length(AValue)) or (AValue[Index] = ASeparator) then
    begin
      if Index > StartIndex then
      begin
        Inc(Count);
        SetLength(Result, Count);
        Result[Count - 1] := Copy(AValue, StartIndex, Index - StartIndex);
      end;
      StartIndex := Index + 1;
    end;
  end;
end;

function JoinLocaleBaseName(const AParsed: TBcp47Tag; const AIncludeRegion: Boolean): string;
begin
  Result := LowerCase(AParsed.Language);
  if AParsed.Script <> '' then
    Result := Result + '-' + AParsed.Script;
  if AIncludeRegion and (AParsed.Region <> '') then
    Result := Result + '-' + AParsed.Region;
end;

procedure ApplyLanguageAliasReplacement(var AParsed: TBcp47Tag;
  const AReplacement: string);
var
  ReplacementParsed: TBcp47Tag;
begin
  ReplacementParsed := ParseBcp47Tag(AReplacement);
  if not ReplacementParsed.IsValid then
    Exit;

  AParsed.Language := ReplacementParsed.Language;
  if (ReplacementParsed.Script <> '') and (AParsed.Script = '') then
    AParsed.Script := ReplacementParsed.Script;
  if (ReplacementParsed.Region <> '') and (AParsed.Region = '') then
    AParsed.Region := ReplacementParsed.Region;
  AParsed := NormalizeBcp47Case(AParsed);
end;

procedure ApplyLanguageAlias(var AParsed: TBcp47Tag);
var
  Replacement, AliasKey: string;
begin
  if AParsed.Language = '' then
    Exit;

  if (CompareText(AParsed.Language, 'sgn') = 0) and
     (CompareText(AParsed.Region, 'GR') = 0) then
  begin
    AParsed.Language := 'gss';
    AParsed.Script := '';
    AParsed.Region := '';
    SetLength(AParsed.Variants, 0);
    AParsed := NormalizeBcp47Case(AParsed);
    Exit;
  end;

  AliasKey := LowerCase(JoinLocaleBaseName(AParsed, True));
  if TryGetLanguageAlias(AliasKey, Replacement) then
  begin
    ApplyLanguageAliasReplacement(AParsed, Replacement);
    Exit;
  end;

  if TryGetLanguageAlias(LowerCase(AParsed.Language), Replacement) then
    ApplyLanguageAliasReplacement(AParsed, Replacement);
end;

function RemoveVariant(var AParsed: TBcp47Tag; const AVariant: string): Boolean;
var
  ReadIndex, WriteIndex: Integer;
begin
  Result := False;
  WriteIndex := 0;
  for ReadIndex := 0 to High(AParsed.Variants) do
  begin
    if (not Result) and (CompareText(AParsed.Variants[ReadIndex], AVariant) = 0) then
    begin
      Result := True;
      Continue;
    end;

    if WriteIndex <> ReadIndex then
      AParsed.Variants[WriteIndex] := AParsed.Variants[ReadIndex];
    Inc(WriteIndex);
  end;
  SetLength(AParsed.Variants, WriteIndex);
end;

procedure ApplyRegularGrandfatheredVariantAlias(var AParsed: TBcp47Tag);
var
  Variant, Replacement: string;
  Parts: IntlTypes.TStringArray;
begin
  if (AParsed.Language = '') or (Length(AParsed.Variants) = 0) then
    Exit;

  for Variant in AParsed.Variants do
  begin
    if not TryGetGrandfatheredTag(LowerCase(AParsed.Language + '-' + Variant),
      Replacement) then
      Continue;

    if not RemoveVariant(AParsed, Variant) then
      Exit;

    Parts := SplitBySeparator(Replacement, '-');
    if Length(Parts) = 0 then
      Exit;

    AParsed.Language := Parts[0];
    if Length(Parts) > 1 then
      AParsed.Script := Parts[1];
    if Length(Parts) > 2 then
      AParsed.Region := Parts[2];
    AParsed := NormalizeBcp47Case(AParsed);
    Exit;
  end;
end;

function ContainsReplacementRegion(const AReplacements: IntlTypes.TStringArray;
  const ARegion: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AReplacements) do
    if CompareText(AReplacements[I], ARegion) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

procedure ApplyTerritoryAlias(var AParsed: TBcp47Tag);
var
  Replacement, Maximized: string;
  Replacements: IntlTypes.TStringArray;
  LikelyParsed: TBcp47Tag;
begin
  if AParsed.Region = '' then
    Exit;

  if not TryGetTerritoryAlias(AParsed.Region, Replacement) then
    Exit;

  Replacements := SplitBySeparator(Replacement, ' ');
  if Length(Replacements) = 0 then
    Exit;

  if ((CompareText(AParsed.Region, 'SU') = 0) or
      (CompareText(AParsed.Region, '810') = 0)) and
     (CompareText(AParsed.Script, 'Armn') = 0) and
     ContainsReplacementRegion(Replacements, 'AM') then
  begin
    AParsed.Region := 'AM';
    AParsed := NormalizeBcp47Case(AParsed);
    Exit;
  end;

  if Length(Replacements) > 1 then
  begin
    if TryGetLikelySubtags(LowerCase(JoinLocaleBaseName(AParsed, False)), Maximized) then
    begin
      LikelyParsed := ParseBcp47Tag(Maximized);
      if LikelyParsed.IsValid and ContainsReplacementRegion(Replacements, LikelyParsed.Region) then
      begin
        AParsed.Region := LikelyParsed.Region;
        Exit;
      end;
    end;
  end;

  AParsed.Region := Replacements[0];
  AParsed := NormalizeBcp47Case(AParsed);
end;

procedure ApplyScriptAlias(var AParsed: TBcp47Tag);
var
  Replacement: string;
begin
  if AParsed.Script = '' then
    Exit;

  if TryGetScriptAlias(AParsed.Script, Replacement) then
    AParsed.Script := Replacement;
end;

procedure ApplyLocaleAliases(var AParsed: TBcp47Tag);
begin
  ApplyRegularGrandfatheredVariantAlias(AParsed);
  ApplyLanguageAlias(AParsed);
  ApplyScriptAlias(AParsed);
  ApplyTerritoryAlias(AParsed);
end;

// ECMA-402 ES2026 §6.2.2 CanonicalizeUnicodeLocaleId(locale)
function CanonicalizeUnicodeLocaleId(const ATag: string): string;
var
  Parsed, ICUParsed: TBcp47Tag;
  ICUCanonical, CLDRReplacement: string;
begin
  Result := '';

  if ATag = '' then
    Exit;

  if not IsStructurallyValidUnicodeLocaleIdentifierTag(ATag) then
    Exit;

  Parsed := ParseBcp47Tag(ATag);
  if not Parsed.IsValid then
    Exit;

  if TryGetGrandfatheredTag(LowerCase(ATag), CLDRReplacement) then
  begin
    Parsed := ParseBcp47Tag(CLDRReplacement);
    if Parsed.IsValid then
    begin
      Result := CanonicalizeBcp47Tag(Parsed);
      Exit;
    end;
  end;

  if TryICUCanonicalizeLocale(ATag, ICUCanonical) then
  begin
    ICUParsed := ParseBcp47Tag(ICUCanonical);
    if not ICUParsed.IsValid then
    begin
      Result := ICUCanonical;
      Exit;
    end;
    if (CompareText(Parsed.Language, ICUParsed.Language) = 0) and
       (CompareText(Parsed.Script, ICUParsed.Script) = 0) and
       (CompareText(Parsed.Region, ICUParsed.Region) = 0) then
      Parsed.Variants := ICUParsed.Variants;
  end;

  ApplyLocaleAliases(Parsed);

  Result := CanonicalizeBcp47Tag(Parsed);
end;

// ECMA-402 ES2026 §9.2.1 CanonicalizeLocaleList(locales)
function CanonicalizeLocaleList(const ATags: IntlTypes.TStringArray): IntlTypes.TStringArray;
var
  I, Count: Integer;
  Canonical: string;
  Seen: array of string;

  function AlreadySeen(const AValue: string): Boolean;
  var
    J: Integer;
  begin
    Result := False;
    for J := 0 to Count - 1 do
    begin
      if CompareText(Seen[J], AValue) = 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  Count := 0;
  SetLength(Result, Length(ATags));
  SetLength(Seen, Length(ATags));

  for I := 0 to High(ATags) do
  begin
    Canonical := CanonicalizeUnicodeLocaleId(ATags[I]);
    if Canonical = '' then
      Continue;

    if AlreadySeen(Canonical) then
      Continue;

    Result[Count] := Canonical;
    Seen[Count] := Canonical;
    Inc(Count);
  end;

  SetLength(Result, Count);
end;

// ECMA-402 ES2026 §9.2.9 FilterLocales(availableLocales, requestedLocales, options)
function SupportedLocalesOf(const ARequestedLocales: IntlTypes.TStringArray): IntlTypes.TStringArray;
var
  AvailableLocales: IntlTypes.TStringArray;
  LookupLocale: string;
  I, Count: Integer;
begin
  AvailableLocales := AvailableLocaleList;
  Count := 0;
  SetLength(Result, Length(ARequestedLocales));

  for I := 0 to High(ARequestedLocales) do
  begin
    LookupLocale := LocaleWithoutUnicodeExtension(ARequestedLocales[I]);
    if (LookupLocale <> '') and
       (BestAvailableLocale(AvailableLocales, LookupLocale) <> '') then
    begin
      Result[Count] := ARequestedLocales[I];
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function BestAvailableLocale(const AAvailableLocales: IntlTypes.TStringArray;
  const ALocale: string): string;
var
  Candidate: string;
  I, LastHyphen: Integer;
begin
  Result := '';
  Candidate := ALocale;

  while Candidate <> '' do
  begin
    for I := 0 to High(AAvailableLocales) do
    begin
      if CompareText(AAvailableLocales[I], Candidate) = 0 then
      begin
        Result := AAvailableLocales[I];
        Exit;
      end;
    end;

    LastHyphen := 0;
    for I := Length(Candidate) downto 1 do
    begin
      if Candidate[I] = '-' then
      begin
        LastHyphen := I;
        Break;
      end;
    end;

    if LastHyphen = 0 then
      Exit;

    Candidate := Copy(Candidate, 1, LastHyphen - 1);

    while Length(Candidate) >= 2 do
    begin
      LastHyphen := 0;
      for I := Length(Candidate) downto 1 do
      begin
        if Candidate[I] = '-' then
        begin
          LastHyphen := I;
          Break;
        end;
      end;
      if (LastHyphen > 0) and (Length(Candidate) - LastHyphen = 1) then
        Candidate := Copy(Candidate, 1, LastHyphen - 1)
      else
        Break;
    end;
  end;
end;

function LookupMatcher(const AAvailableLocales, ARequestedLocales: IntlTypes.TStringArray): TIntlResolvedLocale;
var
  I: Integer;
  NoExtensionTag, AvailableLocale: string;
  Parsed: TBcp47Tag;
begin
  Result.Locale := '';
  Result.Extension := '';

  for I := 0 to High(ARequestedLocales) do
  begin
    Parsed := ParseBcp47Tag(ARequestedLocales[I]);
    if not Parsed.IsValid then
      Continue;

    NoExtensionTag := LocaleWithoutUnicodeExtension(Parsed);
    if NoExtensionTag = '' then
      Continue;

    AvailableLocale := BestAvailableLocale(AAvailableLocales, NoExtensionTag);
    if AvailableLocale <> '' then
    begin
      Result.Locale := AvailableLocale;
      if Length(NoExtensionTag) < Length(ARequestedLocales[I]) then
        Result.Extension := ARequestedLocales[I];
      Exit;
    end;
  end;

  Result.Locale := DefaultLocale;
end;

function BestFitMatcher(const AAvailableLocales, ARequestedLocales: IntlTypes.TStringArray): TIntlResolvedLocale;
begin
  Result := LookupMatcher(AAvailableLocales, ARequestedLocales);
end;

function DefaultLocale: string;
begin
  if TryICUGetDefaultLocale(Result) then
    Exit;

  Result := 'en';
end;

initialization
  InitCriticalSection(AvailableLocalesLock);
  AvailableLocalesLoaded := False;

finalization
  DoneCriticalSection(AvailableLocalesLock);
  SetLength(AvailableLocalesCache, 0);

end.
