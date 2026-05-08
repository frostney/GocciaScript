unit IntlTypes;

{$I Shared.inc}

interface

type
  TIntlCollatorSensitivity = (icsBase, icsAccent, icsCase, icsVariant);
  TIntlCollatorUsage = (icuSort, icuSearch);
  TIntlNumberStyle = (insDecimal, insCurrency, insPercent, insUnit);
  TIntlNumberNotation = (innStandard, innScientific, innEngineering, innCompact);
  TIntlNumberCompactDisplay = (incdShort, incdLong);
  TIntlNumberCurrencyDisplay = (incdSymbol, incdNarrowSymbol, incdCode, incdName);
  TIntlNumberCurrencySign = (incsStandard, incsAccounting);
  TIntlNumberSignDisplay = (insdAuto, insdNever, insdAlways, insdExceptZero, insdNegative);
  TIntlNumberUnitDisplay = (inudShort, inudLong, inudNarrow);
  TIntlNumberTrailingZeroDisplay = (intzAuto, intzStripIfInteger);
  TIntlNumberRoundingMode = (inrmCeil, inrmFloor, inrmExpand, inrmTrunc,
    inrmHalfCeil, inrmHalfFloor, inrmHalfExpand, inrmHalfTrunc, inrmHalfEven);
  TIntlNumberUseGrouping = (inugAuto, inugAlways, inugMin2, inugFalse);
  TIntlDateTimeStyle = (idtsNone, idtsFull, idtsLong, idtsMedium, idtsShort);
  TIntlPluralType = (iptCardinal, iptOrdinal);
  TIntlRelativeTimeUnit = (irtuYear, irtuQuarter, irtuMonth, irtuWeek,
    irtuDay, irtuHour, irtuMinute, irtuSecond);
  TIntlRelativeTimeNumeric = (irtnAlways, irtnAuto);
  TIntlListFormatType = (ilftConjunction, ilftDisjunction, ilftUnit);
  TIntlListFormatStyle = (ilfsLong, ilfsShort, ilfsNarrow);
  TIntlDisplayNameType = (idntLanguage, idntRegion, idntScript,
    idntCalendar, idntCurrency, idntDateTimeField);
  TIntlDisplayNameStyle = (idnsLong, idnsShort, idnsNarrow);
  TIntlSegmenterGranularity = (isgGrapheme, isgWord, isgSentence);
  TIntlDurationStyle = (idsLong, idsShort, idsNarrow, idsDigital);
  TIntlDurationDisplay = (iddAuto, iddAlways);

  TIntlFormatPart = record
    PartType: string;
    Value: string;
  end;
  TIntlFormatPartArray = array of TIntlFormatPart;

  TIntlNumberFormatOptions = record
    Style: TIntlNumberStyle;
    Currency: string;
    CurrencyDisplay: TIntlNumberCurrencyDisplay;
    CurrencySign: TIntlNumberCurrencySign;
    UnitIdentifier: string;
    UnitDisplay: TIntlNumberUnitDisplay;
    Notation: TIntlNumberNotation;
    CompactDisplay: TIntlNumberCompactDisplay;
    SignDisplay: TIntlNumberSignDisplay;
    UseGrouping: TIntlNumberUseGrouping;
    RoundingMode: TIntlNumberRoundingMode;
    TrailingZeroDisplay: TIntlNumberTrailingZeroDisplay;
    MinimumIntegerDigits: Integer;
    MinimumFractionDigits: Integer;
    MaximumFractionDigits: Integer;
    MinimumSignificantDigits: Integer;
    MaximumSignificantDigits: Integer;
    RoundingIncrement: Integer;
    NumberingSystem: string;
  end;

  TIntlDateTimeFormatOptions = record
    DateStyle: TIntlDateTimeStyle;
    TimeStyle: TIntlDateTimeStyle;
    Calendar: string;
    NumberingSystem: string;
    TimeZone: string;
    Hour12: Integer;
    HourCycle: string;
    Weekday: string;
    Era: string;
    Year: string;
    Month: string;
    Day: string;
    DayPeriod: string;
    Hour: string;
    Minute: string;
    Second: string;
    FractionalSecondDigits: Integer;
    TimeZoneName: string;
  end;

  TIntlListPattern = record
    Start: string;
    Middle: string;
    EndPart: string;
    Pair: string;
  end;

  TIntlRelativeTimePattern = record
    Future: string;
    Past: string;
  end;

  TIntlPluralRuleSet = record
    Zero: string;
    One: string;
    Two: string;
    Few: string;
    Many: string;
    Other: string;
  end;

  TIntlResolvedLocale = record
    Locale: string;
    Extension: string;
  end;

  TIntlBreakIterator = record
    Handle: Pointer;
    Text: UnicodeString;
  end;

  TIntlSegment = record
    Segment: string;
    Index: Integer;
    IsWordLike: Boolean;
  end;

  TStringArray = array of string;

function DefaultNumberFormatOptions: TIntlNumberFormatOptions;
function DefaultDateTimeFormatOptions: TIntlDateTimeFormatOptions;

implementation

function DefaultNumberFormatOptions: TIntlNumberFormatOptions;
begin
  Result.Style := insDecimal;
  Result.Currency := '';
  Result.CurrencyDisplay := incdSymbol;
  Result.CurrencySign := incsStandard;
  Result.UnitIdentifier := '';
  Result.UnitDisplay := inudShort;
  Result.Notation := innStandard;
  Result.CompactDisplay := incdShort;
  Result.SignDisplay := insdAuto;
  Result.UseGrouping := inugAuto;
  Result.RoundingMode := inrmHalfExpand;
  Result.TrailingZeroDisplay := intzAuto;
  Result.MinimumIntegerDigits := 1;
  Result.MinimumFractionDigits := -1;
  Result.MaximumFractionDigits := -1;
  Result.MinimumSignificantDigits := -1;
  Result.MaximumSignificantDigits := -1;
  Result.RoundingIncrement := 1;
  Result.NumberingSystem := '';
end;

function DefaultDateTimeFormatOptions: TIntlDateTimeFormatOptions;
begin
  Result.DateStyle := idtsNone;
  Result.TimeStyle := idtsNone;
  Result.Calendar := '';
  Result.NumberingSystem := '';
  Result.TimeZone := '';
  Result.Hour12 := -1;
  Result.HourCycle := '';
  Result.Weekday := '';
  Result.Era := '';
  Result.Year := '';
  Result.Month := '';
  Result.Day := '';
  Result.DayPeriod := '';
  Result.Hour := '';
  Result.Minute := '';
  Result.Second := '';
  Result.FractionalSecondDigits := -1;
  Result.TimeZoneName := '';
end;

end.
