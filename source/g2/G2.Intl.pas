unit G2.Intl;
{ ECMA-402 (Intl): the objects and their option processing are a JS prelude (prelude/intl.js -> G2.Intl.inc);
  the locale data and the formatting come from the shared units Goccia's Intl uses: BCP47 and
  IntlLocaleResolver (language tags), IntlICU (ICU, loaded at run time) and Goccia.Intl.CLDRData (the CLDR data
  embedded in the binary), through one native, intlOp(op, ...), given to the prelude.
    0 isValidTag(tag)                       -> Boolean (IsStructurallyValidLanguageTag)
    1 canonicalize(tag)                     -> String, or null for a tag that is not valid
    2 availableLocales()                    -> Array of Strings
    3 defaultLocale()                       -> String
    4 calendars(locale)                     -> Array (the locale's preferred first)
    5 collations(locale)                    -> Array
    6 numberingSystems(locale)              -> Array
    7 hourCycles(locale, region)            -> Array
    8 maximize(tag), 9 minimize(tag)        -> String, or null
    10 selectPlural(locale, x, ordinal)     -> category
    11 pluralCategories(locale, ordinal)    -> Array
    12 formatList(locale, items, type, style), 13 its parts
    14 formatRelativeTime(locale, x, unit, numeric, style), 15 its parts
    16 displayName(locale, code, type, style) -> String, or undefined
    17 primaryTimeZones()                   -> Array (IANA primary identifiers)
    20 formatNumber(locale, opts, x, isDecimalString, toParts) -> String or parts
    21 formatNumberRange(locale, opts, x, y, isDecimalString, toParts)
    22 currencyDigits(code)                 -> the currency's minor unit digits (2 if CLDR has none)
    23 regionTimeZones(region)              -> Array, or undefined
    24 textDirection(locale)                -> "ltr" or "rtl"
    25 weekInfo(region)                     -> [firstDay, weekendStart, weekendEnd, minimalDays], or undefined
    26 timeZoneId(tz)                       -> the identifier in its case from the time zone database, or null
    27 systemTimeZone()                     -> String
    50 boundaries(locale, granularity, text) -> [end, wordLike, end, wordLike, ...] in UTF-16 units, or undefined
    41 relativeTimePattern(locale, unit)    -> [future, past] CLDR patterns with {0}, or undefined
    40 compare(locale, a, b, sensitivity, ignorePunctuation, numeric, caseFirst) -> -1, 0 or 1
    30 formatDateTime(locale, opts, ms, toParts)
    31 formatDateTimeRange(locale, opts, ms1, ms2, toParts)
  A date-time format's opts is an Array: dateStyle, timeStyle, calendar, numberingSystem, timeZone, hour12
  (-1 unset, 0, 1), hourCycle, weekday, era, year, month, day, dayPeriod, hour, minute, second,
  fractionalSecondDigits (-1 unset), timeZoneName.
  A number format's opts is an Array: style, currency, currencyDisplay, currencySign, unit, unitDisplay,
  notation, compactDisplay, signDisplay, useGrouping, roundingMode, trailingZeroDisplay, minimumIntegerDigits,
  minimumFractionDigits, maximumFractionDigits, minimumSignificantDigits, maximumSignificantDigits (-1 unset),
  roundingIncrement, roundingPriority, numberingSystem.
  Parts are Arrays of [type, value], or [type, value, source] for a range, [type, value, source, unit] with a unit. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

procedure InitIntl;

implementation

uses
  SysUtils, Math, G2.Core, G2.Bytecode, G2.VM, G2.Snapshot, IntlTypes, BCP47, IntlLocaleResolver,
  IntlICU, Goccia.Intl.CLDRData, Goccia.Temporal.TimeZone, G2.Temporal, G2.Builtins;

{$I G2.Intl.inc}

type
  TArgs = PValue;

function SArg(Args: TArgs; Argc, K: Integer): string;
begin
  if (K <= Argc) and IsString(Args[K]) then Result := UTF8Encode(StrToUnicode(StrOf(Args[K]))) else Result := '';
end;

function DArg(Args: TArgs; Argc, K: Integer): Double;
begin
  if (K <= Argc) and IsNumber(Args[K]) then Result := NumOf(Args[K]) else Result := 0;
end;

function IArg(Args: TArgs; Argc, K: Integer): Integer;
begin
  if (K <= Argc) and IsNumber(Args[K]) then Result := Trunc(NumOf(Args[K])) else Result := 0;
end;

function UStr(const S: string): TValue;
begin
  Result := StrFromUnicode(UTF8Decode(S));
end;

function StrArray(const A: IntlTypes.TStringArray): TValue;
var I: Integer;
begin
  PushRoot(MkPtr(NewArray(0, Length(A) + 1)));
  for I := 0 to High(A) do ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(A[I]));
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ a number the host ICU formatted with ASCII digits for a numbering system it does not know (one newer than its
  data): the system's own digits, from its zero digit (hanidec's are not contiguous: ICU knows it) }
function MapDigits(const S, NS: string): string;
var Z: UnicodeString; Zero, CP: Cardinal; I: Integer; U: UnicodeString;
begin
  Result := S;
  if (NS = '') or (NS = 'latn') or (NS = 'hanidec') then Exit;
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then Break
    else if I = Length(S) then Exit;
  Z := DateTimeZeroDigit(UnicodeString(NS));
  if Z = '' then Exit;
  if (Length(Z) = 2) and (Ord(Z[1]) >= $D800) and (Ord(Z[1]) <= $DBFF) then
    Zero := $10000 + (Cardinal(Ord(Z[1])) - $D800) shl 10 + (Cardinal(Ord(Z[2])) - $DC00)
  else Zero := Ord(Z[1]);
  U := UTF8Decode(S);
  Result := '';
  for I := 1 to Length(U) do
    if (U[I] >= '0') and (U[I] <= '9') then
    begin
      CP := Zero + Cardinal(Ord(U[I]) - Ord('0'));
      if CP >= $10000 then
        Result := Result + UTF8Encode(UnicodeString(WideChar($D800 + ((CP - $10000) shr 10))) +
          UnicodeString(WideChar($DC00 + ((CP - $10000) and $3FF))))
      else Result := Result + UTF8Encode(UnicodeString(WideChar(CP)));
    end
    else Result := Result + UTF8Encode(UnicodeString(U[I]));
end;

procedure MapPartDigits(var P: TIntlFormatPartArray; const NS: string);
var I: Integer;
begin
  for I := 0 to High(P) do P[I].Value := MapDigits(P[I].Value, NS);
end;

function PartsArray(const P: TIntlFormatPartArray): TValue;
var I: Integer;
begin
  PushRoot(MkPtr(NewArray(0, Length(P) + 1)));
  for I := 0 to High(P) do
  begin
    PushRoot(MkPtr(NewArray(0, 2)));
    ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(P[I].PartType));
    ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(P[I].Value));
    if (P[I].Source <> '') or (P[I].UnitIdentifier <> '') then ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(P[I].Source));
    if P[I].UnitIdentifier <> '' then ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(P[I].UnitIdentifier));
    ArrayPush(ObjOf(RootAt(TempTop - 2)), RootAt(TempTop - 1));
    PopRoots(1);
  end;
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ the strings of a JS Array (non-strings are skipped: the prelude passes strings) }
function ArgStrings(Args: TArgs; Argc, K: Integer): IntlTypes.TStringArray;
var A: PObj; I, N: Integer;
begin
  SetLength(Result, 0);
  if (K > Argc) or not IsObject(Args[K]) or (ObjOf(Args[K])^.OKind <> okArray) then Exit;
  A := ObjOf(Args[K]);
  N := ArrayLen(A);
  SetLength(Result, N);
  for I := 0 to N - 1 do
    if IsString(A^.Elements^.Data[I]) then Result[I] := UTF8Encode(StrToUnicode(StrOf(A^.Elements^.Data[I])))
    else Result[I] := '';
end;

function ListType(const S: string): TIntlListFormatType;
begin
  if S = 'disjunction' then Result := ilftDisjunction
  else if S = 'unit' then Result := ilftUnit
  else Result := ilftConjunction;
end;

function ListStyle(const S: string): TIntlListFormatStyle;
begin
  if S = 'short' then Result := ilfsShort else if S = 'narrow' then Result := ilfsNarrow else Result := ilfsLong;
end;

function RelUnit(const S: string): TIntlRelativeTimeUnit;
begin
  if S = 'year' then Result := irtuYear
  else if S = 'quarter' then Result := irtuQuarter
  else if S = 'month' then Result := irtuMonth
  else if S = 'week' then Result := irtuWeek
  else if S = 'day' then Result := irtuDay
  else if S = 'hour' then Result := irtuHour
  else if S = 'minute' then Result := irtuMinute
  else Result := irtuSecond;
end;

function RelStyle(const S: string): TIntlRelativeTimeStyle;
begin
  if S = 'short' then Result := irtsShort else if S = 'narrow' then Result := irtsNarrow else Result := irtsLong;
end;

function DisplayType(const S: string): TIntlDisplayNameType;
begin
  if S = 'region' then Result := idntRegion
  else if S = 'script' then Result := idntScript
  else if S = 'calendar' then Result := idntCalendar
  else if S = 'currency' then Result := idntCurrency
  else if S = 'dateTimeField' then Result := idntDateTimeField
  else Result := idntLanguage;
end;

function DisplayStyle(const S: string): TIntlDisplayNameStyle;
begin
  if S = 'short' then Result := idnsShort else if S = 'narrow' then Result := idnsNarrow else Result := idnsLong;
end;

{ the opts Array of a number format (see above) }
function NumOpts(V: TValue): TIntlNumberFormatOptions;
var A: PObj;
  function E(I: Integer): TValue;
  begin
    if (A <> nil) and (I < ArrayLen(A)) then Result := A^.Elements^.Data[I] else Result := VUndefined;
  end;
  function S(I: Integer): string;
  begin
    if IsString(E(I)) then Result := UTF8Encode(StrToUnicode(StrOf(E(I)))) else Result := '';
  end;
  function N(I: Integer): Integer;
  begin
    if IsNumber(E(I)) then Result := Trunc(NumOf(E(I))) else Result := -1;
  end;
begin
  Result := DefaultNumberFormatOptions;
  if IsObject(V) and (ObjOf(V)^.OKind = okArray) then A := ObjOf(V) else A := nil;
  if S(0) = 'percent' then Result.Style := insPercent
  else if S(0) = 'currency' then Result.Style := insCurrency
  else if S(0) = 'unit' then Result.Style := insUnit
  else Result.Style := insDecimal;
  Result.Currency := S(1);
  if S(2) = 'code' then Result.CurrencyDisplay := incdCode
  else if S(2) = 'narrowSymbol' then Result.CurrencyDisplay := incdNarrowSymbol
  else if S(2) = 'name' then Result.CurrencyDisplay := incdName
  else Result.CurrencyDisplay := incdSymbol;
  if S(3) = 'accounting' then Result.CurrencySign := incsAccounting else Result.CurrencySign := incsStandard;
  Result.UnitIdentifier := S(4);
  if S(5) = 'long' then Result.UnitDisplay := inudLong
  else if S(5) = 'narrow' then Result.UnitDisplay := inudNarrow
  else Result.UnitDisplay := inudShort;
  if S(6) = 'scientific' then Result.Notation := innScientific
  else if S(6) = 'engineering' then Result.Notation := innEngineering
  else if S(6) = 'compact' then Result.Notation := innCompact
  else Result.Notation := innStandard;
  if S(7) = 'long' then Result.CompactDisplay := incdLong else Result.CompactDisplay := incdShort;
  if S(8) = 'never' then Result.SignDisplay := insdNever
  else if S(8) = 'always' then Result.SignDisplay := insdAlways
  else if S(8) = 'exceptZero' then Result.SignDisplay := insdExceptZero
  else if S(8) = 'negative' then Result.SignDisplay := insdNegative
  else Result.SignDisplay := insdAuto;
  if S(9) = 'always' then Result.UseGrouping := inugAlways
  else if S(9) = 'min2' then Result.UseGrouping := inugMin2
  else if S(9) = 'false' then Result.UseGrouping := inugFalse
  else Result.UseGrouping := inugAuto;
  if S(10) = 'ceil' then Result.RoundingMode := inrmCeil
  else if S(10) = 'floor' then Result.RoundingMode := inrmFloor
  else if S(10) = 'expand' then Result.RoundingMode := inrmExpand
  else if S(10) = 'trunc' then Result.RoundingMode := inrmTrunc
  else if S(10) = 'halfCeil' then Result.RoundingMode := inrmHalfCeil
  else if S(10) = 'halfFloor' then Result.RoundingMode := inrmHalfFloor
  else if S(10) = 'halfTrunc' then Result.RoundingMode := inrmHalfTrunc
  else if S(10) = 'halfEven' then Result.RoundingMode := inrmHalfEven
  else Result.RoundingMode := inrmHalfExpand;
  if S(11) = 'stripIfInteger' then Result.TrailingZeroDisplay := intzStripIfInteger
  else Result.TrailingZeroDisplay := intzAuto;
  Result.MinimumIntegerDigits := N(12);
  Result.MinimumFractionDigits := N(13);
  Result.MaximumFractionDigits := N(14);
  Result.MinimumSignificantDigits := N(15);
  Result.MaximumSignificantDigits := N(16);
  Result.RoundingIncrement := N(17);
  if S(18) = 'morePrecision' then Result.RoundingPriority := inrpMorePrecision
  else if S(18) = 'lessPrecision' then Result.RoundingPriority := inrpLessPrecision
  else Result.RoundingPriority := inrpAuto;
  Result.NumberingSystem := S(19);
end;

function StyleOf(const S: string): TIntlDateTimeStyle;
begin
  if S = 'full' then Result := idtsFull
  else if S = 'long' then Result := idtsLong
  else if S = 'medium' then Result := idtsMedium
  else if S = 'short' then Result := idtsShort
  else Result := idtsNone;
end;

{ the opts Array of a date-time format (see above) }
function DTOpts(V: TValue): TIntlDateTimeFormatOptions;
var A: PObj;
  function E(I: Integer): TValue;
  begin
    if (A <> nil) and (I < ArrayLen(A)) then Result := A^.Elements^.Data[I] else Result := VUndefined;
  end;
  function S(I: Integer): string;
  begin
    if IsString(E(I)) then Result := UTF8Encode(StrToUnicode(StrOf(E(I)))) else Result := '';
  end;
  function N(I: Integer): Integer;
  begin
    if IsNumber(E(I)) then Result := Trunc(NumOf(E(I))) else Result := -1;
  end;
begin
  FillChar(Result, SizeOf(Result), 0);
  if IsObject(V) and (ObjOf(V)^.OKind = okArray) then A := ObjOf(V) else A := nil;
  Result.DateStyle := StyleOf(S(0));
  Result.TimeStyle := StyleOf(S(1));
  Result.Calendar := S(2);
  Result.NumberingSystem := S(3);
  Result.TimeZone := S(4);
  Result.Hour12 := N(5);
  Result.HourCycle := S(6);
  Result.Weekday := S(7);
  Result.Era := S(8);
  Result.Year := S(9);
  Result.Month := S(10);
  Result.Day := S(11);
  Result.DayPeriod := S(12);
  Result.Hour := S(13);
  Result.Minute := S(14);
  Result.Second := S(15);
  Result.FractionalSecondDigits := N(16);
  Result.TimeZoneName := S(17);
end;

{ a string the UTF-8 round trip of the ICU calls keeps: no lone surrogate }
function NoLoneSurrogate(const S: UnicodeString): Boolean;
var I: Integer; C: Word;
begin
  I := 1;
  while I <= Length(S) do
  begin
    C := Ord(S[I]);
    if (C >= $D800) and (C <= $DBFF) and (I < Length(S)) and (Ord(S[I + 1]) >= $DC00) and (Ord(S[I + 1]) <= $DFFF) then
      Inc(I, 2)
    else if (C >= $D800) and (C <= $DFFF) then Exit(False)
    else Inc(I);
  end;
  Result := True;
end;

{ G2.Builtins' CaseMapHook: ICU's full case mapping (Locale '' the root's) }
function IntlCaseMap(const S: UnicodeString; const Locale: string; Upper: Boolean; var R: UnicodeString): Boolean;
var O: string;
begin
  if not NoLoneSurrogate(S) then Exit(False);
  if Upper then Result := TryICUUpperCase(Locale, UTF8Encode(S), O) else Result := TryICULowerCase(Locale, UTF8Encode(S), O);
  if Result then R := UTF8Decode(O);
end;

{ G2.Builtins' NormalizeHook }
function IntlNormalize(const S, Form: UnicodeString; var R: UnicodeString): Boolean;
var O: string;
begin
  if not NoLoneSurrogate(S) then Exit(False);
  Result := TryICUNormalize(UTF8Encode(Form), UTF8Encode(S), O);
  if Result then R := UTF8Decode(O);
end;

{ intlOp 20: a number (or a decimal string) formatted, as a string or parts. A numbering system newer than the
  host ICU's data fails there: the number is then formatted in Latin digits and mapped to the system's }
procedure FormatNumberOp(Args: TArgs; Argc: Integer; var Res: TValue);
var Loc, NS, S: string; Opts: TIntlNumberFormatOptions; P: TIntlFormatPartArray; Dec, Parts, Ok: Boolean; Pass: Integer;
begin
  Loc := SArg(Args, Argc, 2);
  Opts := NumOpts(Args[3]);
  NS := Opts.NumberingSystem;
  Dec := (Argc >= 5) and ToBool(Args[5]);
  Parts := (Argc >= 6) and ToBool(Args[6]);
  for Pass := 0 to 1 do
  begin
    if Pass = 1 then
    begin
      if (NS = '') or (DateTimeZeroDigit(UnicodeString(NS)) = '') then Exit;
      Opts.NumberingSystem := '';
      Loc := StringReplace(Loc, '-u-nu-' + NS, '', []);
    end;
    if Parts then
    begin
      if Dec then Ok := TryICUFormatNumberDecimalToParts(Loc, SArg(Args, Argc, 4), Opts, P)
      else Ok := TryICUFormatNumberToParts(Loc, DArg(Args, Argc, 4), Opts, P);
      if Ok then
      begin
        MapPartDigits(P, NS);
        Res := PartsArray(P);
        Exit;
      end;
    end
    else
    begin
      if Dec then Ok := TryICUFormatNumberDecimal(Loc, SArg(Args, Argc, 4), Opts, S)
      else Ok := TryICUFormatNumber(Loc, DArg(Args, Argc, 4), Opts, S);
      if Ok then
      begin
        Res := UStr(MapDigits(S, NS));
        Exit;
      end;
    end;
  end;
end;

function Intl_Op(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, T, Sym1, Sym2: string; U: UnicodeString; Sens: TIntlCollatorSensitivity; RP: TIntlRelativeTimePattern; BI: TIntlBreakIterator;
  G: TIntlSegmenterGranularity; Pos: Integer; Dg, W1, W2, W3, W4: Integer; WA: PObj; A: IntlTypes.TStringArray; P: TIntlFormatPartArray; Rules: TIntlPluralRuleSet;
  Ordinal: Boolean; N: Integer;
  procedure Add(const C: string);
  begin
    SetLength(A, N + 1);
    A[N] := C;
    Inc(N);
  end;
begin
  Result := True;
  Res := VUndefined;
  case IArg(Args, Argc, 1) of
    0: Res := MkBool(IsStructurallyValidLanguageTag(SArg(Args, Argc, 2)));
    1:
      begin
        S := CanonicalizeUnicodeLocaleId(SArg(Args, Argc, 2));
        if S = '' then Res := VNull else Res := UStr(S);
      end;
    2: Res := StrArray(AvailableLocaleList);
    3: Res := UStr(DefaultLocale);
    4:
      if TryICUGetLocaleCalendars(SArg(Args, Argc, 2), A) then Res := StrArray(A)
      else Res := StrArray(nil);
    5:
      if TryICUGetLocaleCollations(SArg(Args, Argc, 2), A) then Res := StrArray(A)
      else Res := StrArray(nil);
    6:
      if TryGetLocaleNumberingSystems(SArg(Args, Argc, 2), A) then Res := StrArray(A)
      else Res := StrArray(nil);
    7:
      if TryGetLocaleHourCycles(SArg(Args, Argc, 2), SArg(Args, Argc, 3), A) then Res := StrArray(A)
      else Res := StrArray(nil);
    8:
      if TryICUMaximizeLocale(SArg(Args, Argc, 2), S) then Res := UStr(S) else Res := VNull;
    9:
      if TryICUMinimizeLocale(SArg(Args, Argc, 2), S) then Res := UStr(S) else Res := VNull;
    10:
      begin
        if (Argc >= 4) and ToBool(Args[4]) then Ordinal := True else Ordinal := False;
        if Ordinal then
        begin
          if TryICUSelectPlural(SArg(Args, Argc, 2), DArg(Args, Argc, 3), iptOrdinal, S) then Res := UStr(S)
          else Res := UStr('other');
        end
        { a compact notation's exponent is a plural operand (CLDR e): selected on the formatted number }
        else if (Argc >= 5) and IsString(Args[5]) and
                TryICUSelectPluralFormatted(SArg(Args, Argc, 2), DArg(Args, Argc, 3), iptCardinal,
                  UTF8Decode(SArg(Args, Argc, 5)), S) then Res := UStr(S)
        else if TryICUSelectPlural(SArg(Args, Argc, 2), DArg(Args, Argc, 3), iptCardinal, S) then Res := UStr(S)
        else Res := UStr('other');
      end;
    11:
      begin
        N := 0;
        SetLength(A, 0);
        Ordinal := (Argc >= 3) and ToBool(Args[3]);
        if TryGetPluralRules(SArg(Args, Argc, 2), not Ordinal, Rules) then
        begin
          if Rules.Zero <> '' then Add('zero');
          if Rules.One <> '' then Add('one');
          if Rules.Two <> '' then Add('two');
          if Rules.Few <> '' then Add('few');
          if Rules.Many <> '' then Add('many');
        end;
        Add('other');
        Res := StrArray(A);
      end;
    12:
      if TryICUFormatList(SArg(Args, Argc, 2), ArgStrings(Args, Argc, 3), ListType(SArg(Args, Argc, 4)),
         ListStyle(SArg(Args, Argc, 5)), S) then Res := UStr(S);
    13:
      if TryICUFormatListToParts(SArg(Args, Argc, 2), ArgStrings(Args, Argc, 3), ListType(SArg(Args, Argc, 4)),
         ListStyle(SArg(Args, Argc, 5)), P) then Res := PartsArray(P);
    14:
      if TryICUFormatRelativeTime(SArg(Args, Argc, 2), DArg(Args, Argc, 3), RelUnit(SArg(Args, Argc, 4)),
         TIntlRelativeTimeNumeric(Ord(SArg(Args, Argc, 5) = 'auto')), RelStyle(SArg(Args, Argc, 6)), S) then
        Res := UStr(S);
    15:
      if TryICUFormatRelativeTimeToParts(SArg(Args, Argc, 2), DArg(Args, Argc, 3), RelUnit(SArg(Args, Argc, 4)),
         TIntlRelativeTimeNumeric(Ord(SArg(Args, Argc, 5) = 'auto')), RelStyle(SArg(Args, Argc, 6)), P) then
        Res := PartsArray(P);
    16:
      begin
        T := SArg(Args, Argc, 4);
        if TryICUGetDisplayName(SArg(Args, Argc, 2), SArg(Args, Argc, 3), DisplayType(T),
           DisplayStyle(SArg(Args, Argc, 5)), S) and (S <> '') then Res := UStr(S);
      end;
    17: Res := StrArray(IntlTypes.TStringArray(GetAvailablePrimaryTimeZoneIdentifiers));
    20: FormatNumberOp(Args, Argc, Res);
    21:
      if (Argc >= 6) and ToBool(Args[6]) then
      begin
        if (Argc >= 7) and ToBool(Args[7]) then
        begin
          if TryICUFormatNumberDecimalRangeToParts(SArg(Args, Argc, 2), SArg(Args, Argc, 4), SArg(Args, Argc, 5),
             NumOpts(Args[3]), P) then Res := PartsArray(P);
        end
        else if TryICUFormatNumberDecimalRange(SArg(Args, Argc, 2), SArg(Args, Argc, 4), SArg(Args, Argc, 5),
          NumOpts(Args[3]), S) then Res := UStr(S);
      end
      else if (Argc >= 7) and ToBool(Args[7]) then
      begin
        if TryICUFormatNumberRangeToParts(SArg(Args, Argc, 2), DArg(Args, Argc, 4), DArg(Args, Argc, 5),
           NumOpts(Args[3]), P) then Res := PartsArray(P);
      end
      else if TryICUFormatNumberRange(SArg(Args, Argc, 2), DArg(Args, Argc, 4), DArg(Args, Argc, 5),
        NumOpts(Args[3]), S) then Res := UStr(S);
    22:
      if TryGetCurrencyInfo('en', SArg(Args, Argc, 2), Sym1, Sym2, Dg) then Res := MkInt(Dg) else Res := MkInt(2);
    50:
      begin
        T := SArg(Args, Argc, 3);
        if T = 'word' then G := isgWord else if T = 'sentence' then G := isgSentence else G := isgGrapheme;
        if TryICUCreateBreakIterator(SArg(Args, Argc, 2), G, SArg(Args, Argc, 4), BI) then
        begin
          PushRoot(MkPtr(NewArray(0, 16)));
          try
            while TryICUBreakIteratorNext(BI, Pos) do
            begin
              ArrayPush(ObjOf(RootAt(TempTop - 1)), MkInt(Pos));
              if G = isgWord then ArrayPush(ObjOf(RootAt(TempTop - 1)), MkBool(TryICUBreakIteratorGetRuleStatus(BI) >= 100))
              else ArrayPush(ObjOf(RootAt(TempTop - 1)), VFalse);
            end;
          finally
            ICUBreakIteratorClose(BI);
          end;
          Res := RootAt(TempTop - 1);
          PopRoots(1);
        end;
      end;
    60:
      { the case mapping of String.prototype.toLocaleLowerCase / toLocaleUpperCase }
      begin
        if IntlCaseMap(StrToUnicode(StrOf(Args[3])), SArg(Args, Argc, 2), Args[4] = VTrue, U) then Res := StrFromUnicode(U)
        else if Args[4] = VTrue then Res := StrFromUnicode(UnicodeUpperCase(StrToUnicode(StrOf(Args[3]))))
        else Res := StrFromUnicode(UnicodeLowerCase(StrToUnicode(StrOf(Args[3]))));
      end;
    41:
      if TryGetRelativeTimePattern(SArg(Args, Argc, 2), RelUnit(SArg(Args, Argc, 3)), RP) then
      begin
        PushRoot(MkPtr(NewArray(0, 2)));
        ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(RP.Future));
        ArrayPush(ObjOf(RootAt(TempTop - 1)), UStr(RP.Past));
        Res := RootAt(TempTop - 1);
        PopRoots(1);
      end;
    40:
      begin
        T := SArg(Args, Argc, 5);
        if T = 'base' then Sens := icsBase else if T = 'accent' then Sens := icsAccent
        else if T = 'case' then Sens := icsCase else Sens := icsVariant;
        if TryICUCompareStrings(SArg(Args, Argc, 2), SArg(Args, Argc, 3), SArg(Args, Argc, 4), Sens,
           (Argc >= 6) and ToBool(Args[6]), (Argc >= 7) and ToBool(Args[7]), SArg(Args, Argc, 8), Dg) then
          Res := MkInt(Sign(Dg))
        else Res := MkInt(Sign(CompareStr(SArg(Args, Argc, 3), SArg(Args, Argc, 4))));
      end;
    26:
      begin
        S := SArg(Args, Argc, 2);
        if SameText(S, 'UTC') then Res := UStr('UTC')
        else if TryCanonicalizeTimeZoneIdentifierCase(S, T) and IsValidTimeZone(T) then Res := UStr(T)
        else if IsValidTimeZone(S) then Res := UStr(S)
        else Res := VNull;
      end;
    27: Res := UStr(GetSystemTimeZoneId);
    30:
      if (Argc >= 5) and ToBool(Args[5]) then
      begin
        if TryICUFormatDateTimeToParts(SArg(Args, Argc, 2), DArg(Args, Argc, 4), DTOpts(Args[3]), P) then
          Res := PartsArray(P);
      end
      else if TryICUFormatDateTime(SArg(Args, Argc, 2), DArg(Args, Argc, 4), DTOpts(Args[3]), S) then Res := UStr(S);
    31:
      if (Argc >= 6) and ToBool(Args[6]) then
      begin
        if TryICUFormatDateTimeRangeToParts(SArg(Args, Argc, 2), DArg(Args, Argc, 4), DArg(Args, Argc, 5),
           DTOpts(Args[3]), P) then Res := PartsArray(P);
      end
      else if TryICUFormatDateTimeRange(SArg(Args, Argc, 2), DArg(Args, Argc, 4), DArg(Args, Argc, 5),
        DTOpts(Args[3]), S) then Res := UStr(S);
    23:
      if TryGetLocaleTimeZones(SArg(Args, Argc, 2), A) then Res := StrArray(A);
    24:
      if TryGetLocaleTextDirection(SArg(Args, Argc, 2), S) then Res := UStr(S) else Res := UStr('ltr');
    25:
      if TryGetLocaleWeekInfo(SArg(Args, Argc, 2), W1, W2, W3, W4) then
      begin
        WA := NewArray(4, 4);
        WA^.Elements^.Data[0] := MkInt(W1); WA^.Elements^.Data[1] := MkInt(W2);
        WA^.Elements^.Data[2] := MkInt(W3); WA^.Elements^.Data[3] := MkInt(W4);
        Res := MkPtr(WA);
      end;
  end;
end;

procedure InitIntl;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue; CA: array[0..3] of TValue; Ok: Boolean;
begin
  CaseMapHook := @IntlCaseMap;
  NormalizeHook := @IntlNormalize;
  if not CompilePrelude('<intl>', IntlSrc, T, Err) then raise Exception.Create('intl prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('intl prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('intl prelude run');
  PushRoot(Fn);
  PushRoot(NewNativeFunction(Intl_Op, 'intlOp', 8));
  PushRoot(NewNativeFunction(MarkIntrinsicClass, 'markIntrinsic', 1));
  CA[0] := GlobalObj;
  CA[1] := RootAt(TempTop - 2);
  CA[2] := RootAt(TempTop - 1);
  CA[3] := TemporalInternals;
  Ok := CallValue(RootAt(TempTop - 3), VUndefined, @CA[0], 4, R);
  PopRoots(2);
  if not Ok then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('intl prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('intl prelude call');
  end;
  PopRoots(1);
end;

end.
