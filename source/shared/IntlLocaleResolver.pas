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
function LocaleWithoutUnicodeExtension(const ALocale: string): string;
var
  Parsed: TBcp47Tag;
  FilteredExtensions: TBcp47ExtensionArray;
  I, Count: Integer;
begin
  Result := '';
  Parsed := ParseBcp47Tag(ALocale);
  if not Parsed.IsValid then
    Exit;

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

// ECMA-402 ES2026 §6.2.2 CanonicalizeUnicodeLocaleId(locale)
function CanonicalizeUnicodeLocaleId(const ATag: string): string;
var
  Parsed: TBcp47Tag;
  ICUCanonical, CLDRReplacement: string;
begin
  Result := '';

  if ATag = '' then
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
    Result := ICUCanonical;
    Exit;
  end;

  if Parsed.Language <> '' then
  begin
    if TryGetLanguageAlias(LowerCase(Parsed.Language), CLDRReplacement) then
    begin
      Parsed.Language := CLDRReplacement;
      Parsed := NormalizeBcp47Case(Parsed);
    end;
  end;

  if Parsed.Script <> '' then
  begin
    if TryGetScriptAlias(Parsed.Script, CLDRReplacement) then
      Parsed.Script := CLDRReplacement;
  end;

  if Parsed.Region <> '' then
  begin
    if TryGetTerritoryAlias(Parsed.Region, CLDRReplacement) then
      Parsed.Region := CLDRReplacement;
  end;

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

    NoExtensionTag := LocaleWithoutUnicodeExtension(ARequestedLocales[I]);
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
