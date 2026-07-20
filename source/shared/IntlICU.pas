unit IntlICU;

{$I Shared.inc}

interface

uses
  CriticalSections,
  IntlTypes;

type
  TIntlDateTimeFormatter = Pointer;

function IntlICUAvailable: Boolean;

function TryICUCanonicalizeLocale(const ATag: string; out ACanonical: string): Boolean;
function TryICUGetAvailableLocales(out ALocales: IntlTypes.TStringArray): Boolean;
function TryICUMaximizeLocale(const ATag: string; out AMaximized: string): Boolean;
function TryICUMinimizeLocale(const ATag: string; out AMinimized: string): Boolean;
function TryICUGetLocaleCalendars(const ALocale: string;
  out ACalendars: IntlTypes.TStringArray): Boolean;
function TryICUGetLocaleCollations(const ALocale: string;
  out ACollations: IntlTypes.TStringArray): Boolean;

function TryICUCompareStrings(const ALocale: string; const AStr1, AStr2: string;
  ASensitivity: TIntlCollatorSensitivity; AIgnorePunctuation, ANumeric: Boolean;
  const ACaseFirst: string; out AResult: Integer): Boolean;

function TryICUFormatNumber(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatNumberDecimal(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatNumberToParts(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
function TryICUFormatNumberDecimalToParts(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
function TryICUFormatNumberRange(const ALocale: string; AStartValue, AEndValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatNumberRangeToParts(const ALocale: string; AStartValue, AEndValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
function TryICUFormatNumberDecimalRange(const ALocale, AStartValue, AEndValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatNumberDecimalRangeToParts(const ALocale, AStartValue, AEndValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;

function TryICUFormatDateTime(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatDateTimeToParts(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
function TryICUOpenDateTimeFormatter(const ALocale: string;
  const AOptions: TIntlDateTimeFormatOptions;
  out AFormatter: TIntlDateTimeFormatter): Boolean;
procedure ICUCloseDateTimeFormatter(var AFormatter: TIntlDateTimeFormatter);
function TryICUFormatDateTimeWithFormatter(
  const AFormatter: TIntlDateTimeFormatter; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatDateTimeToPartsWithFormatter(
  const AFormatter: TIntlDateTimeFormatter; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
function TryICUFormatDateTimeRange(const ALocale: string; AStartMillis, AEndMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatDateTimeRangeToParts(const ALocale: string; AStartMillis, AEndMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;

function TryICUSelectPlural(const ALocale: string; AValue: Double;
  APluralType: TIntlPluralType; out ACategory: string): Boolean;

function TryICUGetDisplayName(const ALocale, ACode: string;
  ADisplayType: TIntlDisplayNameType; AStyle: TIntlDisplayNameStyle;
  out AName: string): Boolean;

function TryICUCreateBreakIterator(const ALocale: string;
  AGranularity: TIntlSegmenterGranularity; const AText: string;
  out AIterator: TIntlBreakIterator): Boolean;
function TryICUBreakIteratorNext(var AIterator: TIntlBreakIterator;
  out APosition: Integer): Boolean;
function TryICUBreakIteratorGetRuleStatus(var AIterator: TIntlBreakIterator): Integer;
procedure ICUBreakIteratorClose(var AIterator: TIntlBreakIterator);

function TryICUFormatList(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AFormatted: string): Boolean;
function TryICUFormatListToParts(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AParts: TIntlFormatPartArray): Boolean;

function TryICUFormatRelativeTime(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  AStyle: TIntlRelativeTimeStyle; out AFormatted: string): Boolean;
function TryICUFormatRelativeTimeToParts(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  AStyle: TIntlRelativeTimeStyle; out AParts: TIntlFormatPartArray): Boolean;

function TryICUGetDefaultLocale(out ALocale: string): Boolean;

function TryICUNormalize(const AForm, AStr: string; out AResult: string): Boolean;
function TryICUUpperCase(const ALocale, AStr: string; out AResult: string): Boolean;
function TryICULowerCase(const ALocale, AStr: string; out AResult: string): Boolean;

implementation

{$IFDEF LAKON}

{ Lakon's WASM lane has no dynamic libraries, so every probe answers False
  and Intl falls back to its portable tables. Keeping the unit (rather than
  IFDEF-ing the import out of every Goccia.Values.Intl* consumer) preserves
  the one shared surface. }

function IntlICUAvailable: Boolean;
begin
  Result := False;
end;

function TryICUCanonicalizeLocale(const ATag: string; out ACanonical: string): Boolean;
begin
  ACanonical := '';
  Result := False;
end;

function TryICUGetAvailableLocales(out ALocales: IntlTypes.TStringArray): Boolean;
begin
  SetLength(ALocales, 0);
  Result := False;
end;

function TryICUMaximizeLocale(const ATag: string; out AMaximized: string): Boolean;
begin
  AMaximized := '';
  Result := False;
end;

function TryICUMinimizeLocale(const ATag: string; out AMinimized: string): Boolean;
begin
  AMinimized := '';
  Result := False;
end;

function TryICUGetLocaleCalendars(const ALocale: string;
  out ACalendars: IntlTypes.TStringArray): Boolean;
begin
  SetLength(ACalendars, 0);
  Result := False;
end;

function TryICUGetLocaleCollations(const ALocale: string;
  out ACollations: IntlTypes.TStringArray): Boolean;
begin
  SetLength(ACollations, 0);
  Result := False;
end;

function TryICUCompareStrings(const ALocale: string; const AStr1, AStr2: string;
  ASensitivity: TIntlCollatorSensitivity; AIgnorePunctuation, ANumeric: Boolean;
  const ACaseFirst: string; out AResult: Integer): Boolean;
begin
  AResult := 0;
  Result := False;
end;

function TryICUFormatNumber(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatNumberDecimal(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatNumberToParts(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUFormatNumberDecimalToParts(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUFormatNumberRange(const ALocale: string; AStartValue, AEndValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatNumberRangeToParts(const ALocale: string; AStartValue, AEndValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUFormatNumberDecimalRange(const ALocale, AStartValue, AEndValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatNumberDecimalRangeToParts(const ALocale, AStartValue, AEndValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUFormatDateTime(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatDateTimeToParts(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUOpenDateTimeFormatter(const ALocale: string;
  const AOptions: TIntlDateTimeFormatOptions;
  out AFormatter: TIntlDateTimeFormatter): Boolean;
begin
  AFormatter := nil;
  Result := False;
end;

procedure ICUCloseDateTimeFormatter(var AFormatter: TIntlDateTimeFormatter);
begin
  AFormatter := nil;
end;

function TryICUFormatDateTimeWithFormatter(
  const AFormatter: TIntlDateTimeFormatter; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatDateTimeToPartsWithFormatter(
  const AFormatter: TIntlDateTimeFormatter; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUFormatDateTimeRange(const ALocale: string; AStartMillis, AEndMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatDateTimeRangeToParts(const ALocale: string; AStartMillis, AEndMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUSelectPlural(const ALocale: string; AValue: Double;
  APluralType: TIntlPluralType; out ACategory: string): Boolean;
begin
  ACategory := '';
  Result := False;
end;

function TryICUGetDisplayName(const ALocale, ACode: string;
  ADisplayType: TIntlDisplayNameType; AStyle: TIntlDisplayNameStyle;
  out AName: string): Boolean;
begin
  AName := '';
  Result := False;
end;

function TryICUCreateBreakIterator(const ALocale: string;
  AGranularity: TIntlSegmenterGranularity; const AText: string;
  out AIterator: TIntlBreakIterator): Boolean;
begin
  AIterator := Default(TIntlBreakIterator);
  Result := False;
end;

function TryICUBreakIteratorNext(var AIterator: TIntlBreakIterator;
  out APosition: Integer): Boolean;
begin
  APosition := 0;
  Result := False;
end;

function TryICUBreakIteratorGetRuleStatus(var AIterator: TIntlBreakIterator): Integer;
begin
  Result := 0;
end;

procedure ICUBreakIteratorClose(var AIterator: TIntlBreakIterator);
begin
  { no-op }
end;

function TryICUFormatList(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatListToParts(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUFormatRelativeTime(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  AStyle: TIntlRelativeTimeStyle; out AFormatted: string): Boolean;
begin
  AFormatted := '';
  Result := False;
end;

function TryICUFormatRelativeTimeToParts(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  AStyle: TIntlRelativeTimeStyle; out AParts: TIntlFormatPartArray): Boolean;
begin
  SetLength(AParts, 0);
  Result := False;
end;

function TryICUGetDefaultLocale(out ALocale: string): Boolean;
begin
  ALocale := '';
  Result := False;
end;

function TryICUNormalize(const AForm, AStr: string; out AResult: string): Boolean;
begin
  AResult := '';
  Result := False;
end;

function TryICUUpperCase(const ALocale, AStr: string; out AResult: string): Boolean;
begin
  AResult := '';
  Result := False;
end;

function TryICULowerCase(const ALocale, AStr: string; out AResult: string): Boolean;
begin
  AResult := '';
  Result := False;
end;

{$ELSE}

uses
  Math,
  SysUtils,

  DynamicLibraries,
  ICU,
  NumericText,
  TextEncoding;

const
  ICU_SUCCESS = 0;
  ICU_BUFFER_OVERFLOW = 15;
  LOCALE_ID_CAPACITY = 256;
  FORMAT_BUFFER_CAPACITY = 512;
  DISPLAY_BUFFER_CAPACITY = 256;
  UNUM_DECIMAL_NUMBER_STYLE = 1;
  UNUM_CURRENCY_STYLE = 2;
  UNUM_PERCENT_STYLE = 3;
  UNUM_SCIENTIFIC_STYLE = 4;
  UNUM_PATTERN_DECIMAL = 0;
  UNUM_GROUPING_USED = 1;
  UNUM_MAX_INTEGER_DIGITS = 3;
  UNUM_MIN_INTEGER_DIGITS = 4;
  UNUM_MAX_FRACTION_DIGITS = 6;
  UNUM_MIN_FRACTION_DIGITS = 7;
  UNUM_ROUNDING_MODE = 11;
  UNUM_MIN_SIGNIFICANT_DIGITS = 23;
  UNUM_MAX_SIGNIFICANT_DIGITS = 24;
  UNUM_ROUND_CEILING = 0;
  UNUM_ROUND_FLOOR = 1;
  UNUM_ROUND_DOWN = 2;
  UNUM_ROUND_UP = 3;
  UNUM_ROUND_HALFEVEN = 4;
  UNUM_ROUND_HALFDOWN = 5;
  UNUM_ROUND_HALFUP = 6;
  UNUM_ROUND_HALF_CEILING = 9;
  UNUM_ROUND_HALF_FLOOR = 10;
  UNUM_DATTR_ROUNDING_INCREMENT = 0;
  UNUM_CURRENCY_CODE = 5;
  UNUM_INTEGER_FIELD = 0;
  UNUM_FRACTION_FIELD = 1;
  UNUM_DECIMAL_SEPARATOR_FIELD = 2;
  UNUM_EXPONENT_SYMBOL_FIELD = 3;
  UNUM_EXPONENT_SIGN_FIELD = 4;
  UNUM_EXPONENT_FIELD = 5;
  UNUM_GROUPING_SEPARATOR_FIELD = 6;
  UNUM_CURRENCY_FIELD = 7;
  UNUM_PERCENT_FIELD = 8;
  UNUM_PERMILL_FIELD = 9;
  UNUM_SIGN_FIELD = 10;
  UNUM_MEASURE_UNIT_FIELD = 11;
  UNUM_COMPACT_FIELD = 12;
  UNUM_APPROXIMATELY_SIGN_FIELD = 13;
  UDAT_FULL = 0;
  UDAT_LONG = 1;
  UDAT_MEDIUM = 2;
  UDAT_SHORT = 3;
  UDAT_NONE = -1;
  UDAT_PATTERN = -2;
  UDAT_ERA_FIELD = 0;
  UDAT_YEAR_FIELD = 1;
  UDAT_MONTH_FIELD = 2;
  UDAT_DATE_FIELD = 3;
  UDAT_HOUR_OF_DAY1_FIELD = 4;
  UDAT_HOUR_OF_DAY0_FIELD = 5;
  UDAT_MINUTE_FIELD = 6;
  UDAT_SECOND_FIELD = 7;
  UDAT_FRACTIONAL_SECOND_FIELD = 8;
  UDAT_DAY_OF_WEEK_FIELD = 9;
  UDAT_AM_PM_FIELD = 14;
  UDAT_HOUR1_FIELD = 15;
  UDAT_HOUR0_FIELD = 16;
  UDAT_TIMEZONE_FIELD = 17;
  UDAT_TIMEZONE_RFC_FIELD = 23;
  UDAT_TIMEZONE_GENERIC_FIELD = 24;
  UDAT_STANDALONE_MONTH_FIELD = 26;
  UDAT_TIMEZONE_SPECIAL_FIELD = 29;
  UDAT_YEAR_NAME_FIELD = 30;
  UDAT_TIMEZONE_LOCALIZED_GMT_OFFSET_FIELD = 31;
  UDAT_TIMEZONE_ISO_FIELD = 32;
  UDAT_TIMEZONE_ISO_LOCAL_FIELD = 33;
  UDAT_RELATED_YEAR_FIELD = 34;
  UDAT_AM_PM_MIDNIGHT_NOON_FIELD = 35;
  UDAT_FLEXIBLE_DAY_PERIOD_FIELD = 36;
  UDAT_TIME_SEPARATOR_FIELD = 37;
  UFIELD_CATEGORY_DATE = 1;
  UFIELD_CATEGORY_NUMBER = 2;
  UFIELD_CATEGORY_LIST = 3;
  UFIELD_CATEGORY_RELATIVE_DATETIME = 4;
  UFIELD_CATEGORY_LIST_SPAN = 4099;
  UFIELD_CATEGORY_DATE_INTERVAL_SPAN = 4101;
  UFIELD_CATEGORY_NUMBER_RANGE_SPAN = 4098;
  UNUM_RANGE_COLLAPSE_AUTO = 0;
  UNUM_IDENTITY_FALLBACK_APPROXIMATELY = 2;
  UBRK_CHARACTER = 0;
  UBRK_WORD = 1;
  UBRK_LINE = 2;
  UBRK_SENTENCE = 3;
  UBRK_DONE = -1;
  UBRK_WORD_NONE = 0;
  UBRK_WORD_NUMBER = 100;
  UBRK_WORD_LETTER = 200;
  UBRK_WORD_KANA = 300;
  UBRK_WORD_IDEO = 400;
  UCOL_DEFAULT = -1;
  UCOL_PRIMARY = 0;
  UCOL_SECONDARY = 1;
  UCOL_TERTIARY = 2;
  UCOL_QUATERNARY = 3;
  UCOL_IDENTICAL = 15;
  UCOL_NORMALIZATION_MODE = 4;
  UCOL_STRENGTH = 5;
  UCOL_CASE_FIRST = 2;
  UCOL_CASE_LEVEL = 3;
  UCOL_ALTERNATE_HANDLING = 1;
  UCOL_NUMERIC_COLLATION = 7;
  UCOL_OFF = 16;
  UCOL_SHIFTED = 20;
  UCOL_NON_IGNORABLE = 21;
  UCOL_ON = 17;
  UCOL_LOWER_FIRST = 24;
  UCOL_UPPER_FIRST = 25;
  UCOL_LESS = -1;
  UCOL_EQUAL = 0;
  UCOL_GREATER = 1;
  UPLURAL_TYPE_CARDINAL = 0;
  UPLURAL_TYPE_ORDINAL = 1;
  ULISTFMT_TYPE_AND = 0;
  ULISTFMT_TYPE_OR = 1;
  ULISTFMT_TYPE_UNITS = 2;
  ULISTFMT_WIDTH_WIDE = 0;
  ULISTFMT_WIDTH_SHORT = 1;
  ULISTFMT_WIDTH_NARROW = 2;
  ULISTFMT_LITERAL_FIELD = 0;
  ULISTFMT_ELEMENT_FIELD = 1;
  URELDATEFMT_STYLE_LONG = 0;
  URELDATEFMT_STYLE_SHORT = 1;
  URELDATEFMT_STYLE_NARROW = 2;
  UDAT_REL_LITERAL_FIELD = 0;
  UDAT_REL_NUMERIC_FIELD = 1;
  UDAT_RELATIVE_SECONDS = 0;
  UDAT_RELATIVE_MINUTES = 1;
  UDAT_RELATIVE_HOURS = 2;
  UDAT_RELATIVE_DAYS = 3;
  UDAT_RELATIVE_WEEKS = 4;
  UDAT_RELATIVE_MONTHS = 5;
  UDAT_RELATIVE_YEARS = 6;
  UDAT_RELATIVE_QUARTERS = 7;
  UCURR_LONG_NAME = 1;

type
  TICUErrorCode = LongInt;
  TICUCollationResult = LongInt;
  PUChar = PWideChar;
  PPUChar = ^PUChar;
  PLongInt = ^LongInt;
  PByteBool = ^ByteBool;

  TUlocGetDefault = function: PAnsiChar; cdecl;
  TUlocForLanguageTag = function(const ATag: PAnsiChar; ALocaleId: PAnsiChar;
    ALocaleIdCapacity: LongInt; var AParsedLength: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocToLanguageTag = function(const ALocaleId: PAnsiChar; ATag: PAnsiChar;
    ATagCapacity: LongInt; AStrict: ByteBool;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocGetAvailable = function(AIndex: LongInt): PAnsiChar; cdecl;
  TUlocCountAvailable = function: LongInt; cdecl;
  TUlocAddLikelySubtags = function(const ALocaleId: PAnsiChar; AMaximized: PAnsiChar;
    AMaximizedCapacity: LongInt; var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocMinimizeSubtags = function(const ALocaleId: PAnsiChar; AMinimized: PAnsiChar;
    AMinimizedCapacity: LongInt; var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocGetDisplayName = function(const ALocaleId: PAnsiChar; const AInLocaleId: PAnsiChar;
    AResult: PUChar; AMaxResultSize: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocGetDisplayLanguage = function(const ALocaleId: PAnsiChar; const AInLocaleId: PAnsiChar;
    AResult: PUChar; AMaxResultSize: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocGetDisplayScript = function(const ALocaleId: PAnsiChar; const AInLocaleId: PAnsiChar;
    AResult: PUChar; AMaxResultSize: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlocGetDisplayCountry = function(const ALocaleId: PAnsiChar; const AInLocaleId: PAnsiChar;
    AResult: PUChar; AMaxResultSize: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUcurrGetName = function(const ACurrency: PUChar; const ALocale: PAnsiChar;
    ANameStyle: LongInt; AIsChoiceFormat: PByteBool; ALength: PLongInt;
    var AStatus: TICUErrorCode): PUChar; cdecl;
  TUcolOpen = function(const ALocale: PAnsiChar;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUcolClose = procedure(ACollator: Pointer); cdecl;
  TUcolStrcoll = function(ACollator: Pointer; const ASource: PUChar;
    ASourceLength: LongInt; const ATarget: PUChar;
    ATargetLength: LongInt): TICUCollationResult; cdecl;
  TUcolSetAttribute = procedure(ACollator: Pointer; AAttr: LongInt;
    AValue: LongInt; var AStatus: TICUErrorCode); cdecl;
  TUcalGetKeywordValuesForLocale = function(const AKey: PAnsiChar;
    const ALocale: PAnsiChar; ACommonlyUsed: ByteBool;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUcolGetKeywordValuesForLocale = function(const AKey: PAnsiChar;
    const ALocale: PAnsiChar; ACommonlyUsed: ByteBool;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUenumNext = function(AEnumeration: Pointer; AResultLength: PLongInt;
    var AStatus: TICUErrorCode): PAnsiChar; cdecl;
  TUenumClose = procedure(AEnumeration: Pointer); cdecl;
  TUnumOpen = function(AStyle: LongInt; const APattern: PUChar;
    APatternLength: LongInt; const ALocale: PAnsiChar;
    AParseErr: Pointer; var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumClose = procedure(AFormat: Pointer); cdecl;
  TUnumFormatDouble = function(AFormat: Pointer; ANumber: Double;
    AResult: PUChar; AResultLength: LongInt; APos: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUnumFormatDoubleForFields = function(AFormat: Pointer; ANumber: Double;
    AResult: PUChar; AResultLength: LongInt; AFposIter: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUnumSetAttribute = procedure(AFormat: Pointer; AAttr: LongInt;
    ANewValue: LongInt); cdecl;
  TUnumSetDoubleAttribute = procedure(AFormat: Pointer; AAttr: LongInt;
    ANewValue: Double); cdecl;
  TUnumApplyPattern = procedure(AFormat: Pointer; ALocalized: ByteBool;
    const APattern: PUChar; APatternLength: LongInt;
    AParseError: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUnumSetTextAttribute = procedure(AFormat: Pointer; ATag: LongInt;
    const ANewValue: PUChar; ANewValueLength: LongInt;
    var AStatus: TICUErrorCode); cdecl;
  TUnumfOpenForSkeletonAndLocale = function(const ASkeleton: PUChar;
    ASkeletonLength: LongInt; const ALocale: PAnsiChar;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumfOpenResult = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumfFormatDouble = procedure(AFormatter: Pointer; AValue: Double;
    AResult: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUnumfFormatDecimal = procedure(AFormatter: Pointer; const AValue: PAnsiChar;
    AValueLength: LongInt; AResult: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUnumfResultToString = function(AResult: Pointer; ABuffer: PUChar;
    ABufferCapacity: LongInt; var AStatus: TICUErrorCode): LongInt; cdecl;
  TUnumfResultAsValue = function(AResult: Pointer;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumfClose = procedure(AFormatter: Pointer); cdecl;
  TUnumfCloseResult = procedure(AResult: Pointer); cdecl;
  TUdatOpen = function(ATimeStyle: LongInt; ADateStyle: LongInt;
    const ALocale: PAnsiChar; const ATzId: PUChar; ATzIdLength: LongInt;
    const APattern: PUChar; APatternLength: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUdatClose = procedure(AFormat: Pointer); cdecl;
  TUdatFormat = function(AFormat: Pointer; ADateToFormat: Double;
    AResult: PUChar; AResultLength: LongInt; APosition: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUdatFormatForFields = function(AFormat: Pointer; ADateToFormat: Double;
    AResult: PUChar; AResultLength: LongInt; AFposIter: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUdatpgOpen = function(const ALocale: PAnsiChar;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUdatpgClose = procedure(APatternGenerator: Pointer); cdecl;
  TUdatpgGetBestPattern = function(APatternGenerator: Pointer;
    const ASkeleton: PUChar; ASkeletonLength: LongInt;
    ABestPattern: PUChar; ABestPatternCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUFieldPosIterOpen = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUFieldPosIterClose = procedure(AIterator: Pointer); cdecl;
  TUFieldPosIterNext = function(AIterator: Pointer; ABeginIndex, AEndIndex: PLongInt): LongInt; cdecl;
  TUdtitvfmtOpen = function(const ALocale: PAnsiChar;
    const ASkeleton: PUChar; ASkeletonLength: LongInt;
    const ATzId: PUChar; ATzIdLength: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUdtitvfmtClose = procedure(AFormatter: Pointer); cdecl;
  TUdtitvfmtFormat = function(AFormatter: Pointer; AFromDate, AToDate: Double;
    AResult: PUChar; AResultCapacity: LongInt; APosition: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUdtitvfmtOpenResult = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUdtitvfmtFormatToResult = procedure(AFormatter: Pointer; AFromDate, AToDate: Double;
    AResult: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUdtitvfmtResultAsValue = function(AResult: Pointer; var AStatus: TICUErrorCode): Pointer; cdecl;
  TUdtitvfmtCloseResult = procedure(AResult: Pointer); cdecl;
  TUcfposOpen = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUcfposClose = procedure(APosition: Pointer); cdecl;
  TUcfposGetCategory = function(APosition: Pointer; var AStatus: TICUErrorCode): LongInt; cdecl;
  TUcfposGetField = function(APosition: Pointer; var AStatus: TICUErrorCode): LongInt; cdecl;
  TUcfposGetIndexes = procedure(APosition: Pointer; AStart, ALimit: PLongInt;
    var AStatus: TICUErrorCode); cdecl;
  TUfmtvalGetString = function(AFormattedValue: Pointer; ALength: PLongInt;
    var AStatus: TICUErrorCode): PUChar; cdecl;
  TUfmtvalNextPosition = function(AFormattedValue, APosition: Pointer;
    var AStatus: TICUErrorCode): ByteBool; cdecl;
  TUnumrfOpenForSkeletonWithCollapseAndIdentityFallback = function(
    const ASkeleton: PUChar; ASkeletonLength: LongInt; ACollapse: LongInt;
    AIdentityFallback: LongInt; const ALocale: PAnsiChar; AParseError: Pointer;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumrfOpenResult = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumrfFormatDoubleRange = procedure(AFormatter: Pointer; AFirst, ASecond: Double;
    AResult: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUnumrfFormatDecimalRange = procedure(AFormatter: Pointer; const AFirst: PAnsiChar;
    AFirstLength: LongInt; const ASecond: PAnsiChar; ASecondLength: LongInt;
    AResult: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUnumrfResultAsValue = function(AResult: Pointer; var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumrfClose = procedure(AFormatter: Pointer); cdecl;
  TUnumrfCloseResult = procedure(AResult: Pointer); cdecl;
  TUplrulesOpen = function(const ALocale: PAnsiChar;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUplrulesOpenForType = function(const ALocale: PAnsiChar; APluralType: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUplrulesClose = procedure(ARules: Pointer); cdecl;
  TUplrulesSelect = function(ARules: Pointer; ANumber: Double;
    AKeyword: PUChar; ACapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUbrkOpen = function(AType: LongInt; const ALocale: PAnsiChar;
    const AText: PUChar; ATextLength: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUbrkClose = procedure(ABi: Pointer); cdecl;
  TUbrkNext = function(ABi: Pointer): LongInt; cdecl;
  TUbrkGetRuleStatus = function(ABi: Pointer): LongInt; cdecl;
  TUbrkSetText = procedure(ABi: Pointer; const AText: PUChar;
    ATextLength: LongInt; var AStatus: TICUErrorCode); cdecl;
  TUlistfmtOpenForType = function(const ALocale: PAnsiChar;
    AType: LongInt; AWidth: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUlistfmtClose = procedure(AListfmt: Pointer); cdecl;
  TUlistfmtFormat = function(AListfmt: Pointer;
    const AStrings: PPUChar; const AStringLengths: PLongInt;
    AStringCount: LongInt; AResult: PUChar; AResultCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUlistfmtOpenResult = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUlistfmtCloseResult = procedure(AResult: Pointer); cdecl;
  TUlistfmtFormatStringsToResult = procedure(AListfmt: Pointer;
    const AStrings: PPUChar; const AStringLengths: PLongInt;
    AStringCount: LongInt; AResult: Pointer; var AStatus: TICUErrorCode); cdecl;
  TUlistfmtResultAsValue = function(AResult: Pointer;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUreldatefmtOpen = function(const ALocale: PAnsiChar;
    ANfToAdopt: Pointer; AWidth: LongInt; ACapitalizationContext: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUreldatefmtClose = procedure(AReldatefmt: Pointer); cdecl;
  TUreldatefmtFormatNumeric = function(AReldatefmt: Pointer;
    AOffset: Double; ADirection: LongInt;
    AResult: PUChar; AResultCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUreldatefmtFormat = function(AReldatefmt: Pointer;
    AOffset: Double; ADirection: LongInt;
    AResult: PUChar; AResultCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUreldatefmtOpenResult = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUreldatefmtCloseResult = procedure(AResult: Pointer); cdecl;
  TUreldatefmtFormatNumericToResult = procedure(AReldatefmt: Pointer;
    AOffset: Double; AUnit: LongInt; AResult: Pointer;
    var AStatus: TICUErrorCode); cdecl;
  TUreldatefmtFormatToResult = procedure(AReldatefmt: Pointer;
    AOffset: Double; AUnit: LongInt; AResult: Pointer;
    var AStatus: TICUErrorCode); cdecl;
  TUreldatefmtResultAsValue = function(AResult: Pointer;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUCaseMap = Pointer;
  TUcasemapOpen = function(const ALocale: PAnsiChar; AOptions: LongInt;
    var AStatus: TICUErrorCode): TUCaseMap; cdecl;
  TUcasemapClose = procedure(ACsm: TUCaseMap); cdecl;
  TUcasemapUtf8Map = function(ACsm: TUCaseMap; ADest: PAnsiChar;
    ADestCapacity: LongInt; const ASrc: PAnsiChar; ASrcLength: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUcasemapUtf8ToUpper = TUcasemapUtf8Map;
  TUcasemapUtf8ToLower = TUcasemapUtf8Map;
  TUnorm2GetInstance = function(var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnorm2Normalize = function(ANormalizer: Pointer; const ASrc: PUChar;
    ASrcLength: LongInt; ADest: PUChar; ADestCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TIntlICUFunctions = record
    UlocGetDefault: TUlocGetDefault;
    UlocForLanguageTag: TUlocForLanguageTag;
    UlocToLanguageTag: TUlocToLanguageTag;
    UlocGetAvailable: TUlocGetAvailable;
    UlocCountAvailable: TUlocCountAvailable;
    UlocAddLikelySubtags: TUlocAddLikelySubtags;
    UlocMinimizeSubtags: TUlocMinimizeSubtags;
    UlocGetDisplayName: TUlocGetDisplayName;
    UlocGetDisplayLanguage: TUlocGetDisplayLanguage;
    UlocGetDisplayScript: TUlocGetDisplayScript;
    UlocGetDisplayCountry: TUlocGetDisplayCountry;
    UcurrGetName: TUcurrGetName;
    UcolOpen: TUcolOpen;
    UcolClose: TUcolClose;
    UcolStrcoll: TUcolStrcoll;
    UcolSetAttribute: TUcolSetAttribute;
    UcalGetKeywordValuesForLocale: TUcalGetKeywordValuesForLocale;
    UcolGetKeywordValuesForLocale: TUcolGetKeywordValuesForLocale;
    UenumNext: TUenumNext;
    UenumClose: TUenumClose;
    UnumOpen: TUnumOpen;
    UnumClose: TUnumClose;
    UnumFormatDouble: TUnumFormatDouble;
    UnumFormatDoubleForFields: TUnumFormatDoubleForFields;
    UnumSetAttribute: TUnumSetAttribute;
    UnumSetDoubleAttribute: TUnumSetDoubleAttribute;
    UnumApplyPattern: TUnumApplyPattern;
    UnumSetTextAttribute: TUnumSetTextAttribute;
    UnumfOpenForSkeletonAndLocale: TUnumfOpenForSkeletonAndLocale;
    UnumfOpenResult: TUnumfOpenResult;
    UnumfFormatDouble: TUnumfFormatDouble;
    UnumfFormatDecimal: TUnumfFormatDecimal;
    UnumfResultToString: TUnumfResultToString;
    UnumfResultAsValue: TUnumfResultAsValue;
    UnumfClose: TUnumfClose;
    UnumfCloseResult: TUnumfCloseResult;
    UdatOpen: TUdatOpen;
    UdatClose: TUdatClose;
    UdatFormat: TUdatFormat;
    UdatFormatForFields: TUdatFormatForFields;
    UdatpgOpen: TUdatpgOpen;
    UdatpgClose: TUdatpgClose;
    UdatpgGetBestPattern: TUdatpgGetBestPattern;
    UfieldpositerOpen: TUFieldPosIterOpen;
    UfieldpositerClose: TUFieldPosIterClose;
    UfieldpositerNext: TUFieldPosIterNext;
    UdtitvfmtOpen: TUdtitvfmtOpen;
    UdtitvfmtClose: TUdtitvfmtClose;
    UdtitvfmtFormat: TUdtitvfmtFormat;
    UdtitvfmtOpenResult: TUdtitvfmtOpenResult;
    UdtitvfmtFormatToResult: TUdtitvfmtFormatToResult;
    UdtitvfmtResultAsValue: TUdtitvfmtResultAsValue;
    UdtitvfmtCloseResult: TUdtitvfmtCloseResult;
    UcfposOpen: TUcfposOpen;
    UcfposClose: TUcfposClose;
    UcfposGetCategory: TUcfposGetCategory;
    UcfposGetField: TUcfposGetField;
    UcfposGetIndexes: TUcfposGetIndexes;
    UfmtvalGetString: TUfmtvalGetString;
    UfmtvalNextPosition: TUfmtvalNextPosition;
    UnumrfOpenForSkeletonWithCollapseAndIdentityFallback: TUnumrfOpenForSkeletonWithCollapseAndIdentityFallback;
    UnumrfOpenResult: TUnumrfOpenResult;
    UnumrfFormatDoubleRange: TUnumrfFormatDoubleRange;
    UnumrfFormatDecimalRange: TUnumrfFormatDecimalRange;
    UnumrfResultAsValue: TUnumrfResultAsValue;
    UnumrfClose: TUnumrfClose;
    UnumrfCloseResult: TUnumrfCloseResult;
    UplrulesOpen: TUplrulesOpen;
    UplrulesOpenForType: TUplrulesOpenForType;
    UplrulesClose: TUplrulesClose;
    UplrulesSelect: TUplrulesSelect;
    UbrkOpen: TUbrkOpen;
    UbrkClose: TUbrkClose;
    UbrkNext: TUbrkNext;
    UbrkGetRuleStatus: TUbrkGetRuleStatus;
    UbrkSetText: TUbrkSetText;
    UlistfmtOpenForType: TUlistfmtOpenForType;
    UlistfmtClose: TUlistfmtClose;
    UlistfmtFormat: TUlistfmtFormat;
    UlistfmtOpenResult: TUlistfmtOpenResult;
    UlistfmtCloseResult: TUlistfmtCloseResult;
    UlistfmtFormatStringsToResult: TUlistfmtFormatStringsToResult;
    UlistfmtResultAsValue: TUlistfmtResultAsValue;
    UreldatefmtOpen: TUreldatefmtOpen;
    UreldatefmtClose: TUreldatefmtClose;
    UreldatefmtFormatNumeric: TUreldatefmtFormatNumeric;
    UreldatefmtFormat: TUreldatefmtFormat;
    UreldatefmtOpenResult: TUreldatefmtOpenResult;
    UreldatefmtCloseResult: TUreldatefmtCloseResult;
    UreldatefmtFormatNumericToResult: TUreldatefmtFormatNumericToResult;
    UreldatefmtFormatToResult: TUreldatefmtFormatToResult;
    UreldatefmtResultAsValue: TUreldatefmtResultAsValue;
    UcasemapOpen: TUcasemapOpen;
    UcasemapClose: TUcasemapClose;
    UcasemapUtf8ToUpper: TUcasemapUtf8ToUpper;
    UcasemapUtf8ToLower: TUcasemapUtf8ToLower;
    Unorm2GetNFCInstance: TUnorm2GetInstance;
    Unorm2GetNFDInstance: TUnorm2GetInstance;
    Unorm2GetNFKCInstance: TUnorm2GetInstance;
    Unorm2GetNFKDInstance: TUnorm2GetInstance;
    Unorm2Normalize: TUnorm2Normalize;
  end;

  TICUFieldSpan = record
    Category: Integer;
    Field: Integer;
    StartIndex: Integer;
    EndIndex: Integer;
  end;
  TICUFieldSpanArray = array of TICUFieldSpan;
  TICUFieldTypeMapper = function(AField: Integer): string;

var
  IntlFunctions: TIntlICUFunctions;
  IntlLoadAttempted: Boolean;
  IntlLoadSucceeded: Boolean;
  IntlInitLock: TGocciaCriticalSection;

function ICUSucceeded(const AStatus: TICUErrorCode): Boolean;
begin
  Result := AStatus <= ICU_SUCCESS;
end;

function TryEncodeASCIIBytes(const AText: string;
  out ABytes: TBytes): Boolean;
var
  I: Integer;
begin
  SetLength(ABytes, Length(AText) + 1);
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > $7F then
    begin
      SetLength(ABytes, 0);
      Exit(False);
    end;
    ABytes[I - 1] := Byte(Ord(AText[I]));
  end;
  ABytes[Length(AText)] := 0;
  Result := True;
end;

function BytePointer(const ABytes: TBytes): PAnsiChar; {$IFDEF FPC}inline;{$ENDIF}
begin
  if Length(ABytes) = 0 then
    Result := nil
  else
    Result := PAnsiChar(@ABytes[0]);
end;

function DecodeASCIIBytes(const ABytes: PAnsiChar;
  const ALength: Integer): string;
var
  I: Integer;
begin
  if (not Assigned(ABytes)) or (ALength <= 0) then
    Exit('');
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I + 1] := Char(Ord(ABytes[I]));
end;

function DecodeASCIIZ(const ABytes: PAnsiChar): string;
begin
  if not Assigned(ABytes) then
    Exit('');
  Result := DecodeASCIIBytes(ABytes, StrLen(ABytes));
end;

function ResolveSymbol(const AHandle: TLibHandle; const AName: string): Pointer;
begin
  Result := ICUGetProcAddress(AName);
end;

function TryLoadIntlFunctions(const AHandle: TLibHandle): Boolean;
var
  F: TIntlICUFunctions;
  S: Pointer;
begin
  Result := False;
  FillChar(F, SizeOf(F), 0);

  S := ResolveSymbol(AHandle, 'uloc_forLanguageTag');
  if not Assigned(S) then Exit;
  F.UlocForLanguageTag := TUlocForLanguageTag(S);

  S := ResolveSymbol(AHandle, 'uloc_toLanguageTag');
  if not Assigned(S) then Exit;
  F.UlocToLanguageTag := TUlocToLanguageTag(S);

  S := ResolveSymbol(AHandle, 'uloc_getAvailable');
  if not Assigned(S) then Exit;
  F.UlocGetAvailable := TUlocGetAvailable(S);

  S := ResolveSymbol(AHandle, 'uloc_countAvailable');
  if not Assigned(S) then Exit;
  F.UlocCountAvailable := TUlocCountAvailable(S);

  S := ResolveSymbol(AHandle, 'uloc_addLikelySubtags');
  if not Assigned(S) then Exit;
  F.UlocAddLikelySubtags := TUlocAddLikelySubtags(S);

  S := ResolveSymbol(AHandle, 'uloc_minimizeSubtags');
  if not Assigned(S) then Exit;
  F.UlocMinimizeSubtags := TUlocMinimizeSubtags(S);

  S := ResolveSymbol(AHandle, 'uloc_getDisplayName');
  if not Assigned(S) then Exit;
  F.UlocGetDisplayName := TUlocGetDisplayName(S);

  S := ResolveSymbol(AHandle, 'uloc_getDisplayLanguage');
  if not Assigned(S) then Exit;
  F.UlocGetDisplayLanguage := TUlocGetDisplayLanguage(S);

  S := ResolveSymbol(AHandle, 'uloc_getDisplayScript');
  if not Assigned(S) then Exit;
  F.UlocGetDisplayScript := TUlocGetDisplayScript(S);

  S := ResolveSymbol(AHandle, 'uloc_getDisplayCountry');
  if not Assigned(S) then Exit;
  F.UlocGetDisplayCountry := TUlocGetDisplayCountry(S);

  S := ResolveSymbol(AHandle, 'ucurr_getName');
  if not Assigned(S) then Exit;
  F.UcurrGetName := TUcurrGetName(S);

  S := ResolveSymbol(AHandle, 'ucol_open');
  if not Assigned(S) then Exit;
  F.UcolOpen := TUcolOpen(S);

  S := ResolveSymbol(AHandle, 'ucol_close');
  if not Assigned(S) then Exit;
  F.UcolClose := TUcolClose(S);

  S := ResolveSymbol(AHandle, 'ucol_strcoll');
  if not Assigned(S) then Exit;
  F.UcolStrcoll := TUcolStrcoll(S);

  S := ResolveSymbol(AHandle, 'ucol_setAttribute');
  if not Assigned(S) then Exit;
  F.UcolSetAttribute := TUcolSetAttribute(S);

  S := ResolveSymbol(AHandle, 'ucal_getKeywordValuesForLocale');
  if Assigned(S) then F.UcalGetKeywordValuesForLocale := TUcalGetKeywordValuesForLocale(S);

  S := ResolveSymbol(AHandle, 'ucol_getKeywordValuesForLocale');
  if Assigned(S) then F.UcolGetKeywordValuesForLocale := TUcolGetKeywordValuesForLocale(S);

  S := ResolveSymbol(AHandle, 'uenum_next');
  if Assigned(S) then F.UenumNext := TUenumNext(S);

  S := ResolveSymbol(AHandle, 'uenum_close');
  if Assigned(S) then F.UenumClose := TUenumClose(S);

  S := ResolveSymbol(AHandle, 'unum_open');
  if not Assigned(S) then Exit;
  F.UnumOpen := TUnumOpen(S);

  S := ResolveSymbol(AHandle, 'unum_close');
  if not Assigned(S) then Exit;
  F.UnumClose := TUnumClose(S);

  S := ResolveSymbol(AHandle, 'unum_formatDouble');
  if not Assigned(S) then Exit;
  F.UnumFormatDouble := TUnumFormatDouble(S);

  S := ResolveSymbol(AHandle, 'udat_open');
  if not Assigned(S) then Exit;
  F.UdatOpen := TUdatOpen(S);

  S := ResolveSymbol(AHandle, 'udat_close');
  if not Assigned(S) then Exit;
  F.UdatClose := TUdatClose(S);

  S := ResolveSymbol(AHandle, 'udat_format');
  if not Assigned(S) then Exit;
  F.UdatFormat := TUdatFormat(S);

  S := ResolveSymbol(AHandle, 'uloc_getDefault');
  if Assigned(S) then F.UlocGetDefault := TUlocGetDefault(S);

  S := ResolveSymbol(AHandle, 'unum_setAttribute');
  if Assigned(S) then F.UnumSetAttribute := TUnumSetAttribute(S);
  S := ResolveSymbol(AHandle, 'unum_setDoubleAttribute');
  if Assigned(S) then F.UnumSetDoubleAttribute := TUnumSetDoubleAttribute(S);
  S := ResolveSymbol(AHandle, 'unum_applyPattern');
  if Assigned(S) then F.UnumApplyPattern := TUnumApplyPattern(S);
  S := ResolveSymbol(AHandle, 'unum_setTextAttribute');
  if Assigned(S) then F.UnumSetTextAttribute := TUnumSetTextAttribute(S);
  S := ResolveSymbol(AHandle, 'unum_formatDoubleForFields');
  if Assigned(S) then F.UnumFormatDoubleForFields := TUnumFormatDoubleForFields(S);
  S := ResolveSymbol(AHandle, 'unumf_openForSkeletonAndLocale');
  if Assigned(S) then F.UnumfOpenForSkeletonAndLocale := TUnumfOpenForSkeletonAndLocale(S);
  S := ResolveSymbol(AHandle, 'unumf_openResult');
  if Assigned(S) then F.UnumfOpenResult := TUnumfOpenResult(S);
  S := ResolveSymbol(AHandle, 'unumf_formatDouble');
  if Assigned(S) then F.UnumfFormatDouble := TUnumfFormatDouble(S);
  S := ResolveSymbol(AHandle, 'unumf_formatDecimal');
  if Assigned(S) then F.UnumfFormatDecimal := TUnumfFormatDecimal(S);
  S := ResolveSymbol(AHandle, 'unumf_resultToString');
  if Assigned(S) then F.UnumfResultToString := TUnumfResultToString(S);
  S := ResolveSymbol(AHandle, 'unumf_resultAsValue');
  if Assigned(S) then F.UnumfResultAsValue := TUnumfResultAsValue(S);
  S := ResolveSymbol(AHandle, 'unumf_close');
  if Assigned(S) then F.UnumfClose := TUnumfClose(S);
  S := ResolveSymbol(AHandle, 'unumf_closeResult');
  if Assigned(S) then F.UnumfCloseResult := TUnumfCloseResult(S);
  S := ResolveSymbol(AHandle, 'udat_formatForFields');
  if Assigned(S) then F.UdatFormatForFields := TUdatFormatForFields(S);
  S := ResolveSymbol(AHandle, 'udatpg_open');
  if Assigned(S) then F.UdatpgOpen := TUdatpgOpen(S);
  S := ResolveSymbol(AHandle, 'udatpg_close');
  if Assigned(S) then F.UdatpgClose := TUdatpgClose(S);
  S := ResolveSymbol(AHandle, 'udatpg_getBestPattern');
  if Assigned(S) then F.UdatpgGetBestPattern := TUdatpgGetBestPattern(S);
  S := ResolveSymbol(AHandle, 'ufieldpositer_open');
  if Assigned(S) then F.UfieldpositerOpen := TUFieldPosIterOpen(S);
  S := ResolveSymbol(AHandle, 'ufieldpositer_close');
  if Assigned(S) then F.UfieldpositerClose := TUFieldPosIterClose(S);
  S := ResolveSymbol(AHandle, 'ufieldpositer_next');
  if Assigned(S) then F.UfieldpositerNext := TUFieldPosIterNext(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_open');
  if Assigned(S) then F.UdtitvfmtOpen := TUdtitvfmtOpen(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_close');
  if Assigned(S) then F.UdtitvfmtClose := TUdtitvfmtClose(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_format');
  if Assigned(S) then F.UdtitvfmtFormat := TUdtitvfmtFormat(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_openResult');
  if Assigned(S) then F.UdtitvfmtOpenResult := TUdtitvfmtOpenResult(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_formatToResult');
  if Assigned(S) then F.UdtitvfmtFormatToResult := TUdtitvfmtFormatToResult(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_resultAsValue');
  if Assigned(S) then F.UdtitvfmtResultAsValue := TUdtitvfmtResultAsValue(S);
  S := ResolveSymbol(AHandle, 'udtitvfmt_closeResult');
  if Assigned(S) then F.UdtitvfmtCloseResult := TUdtitvfmtCloseResult(S);
  S := ResolveSymbol(AHandle, 'ucfpos_open');
  if Assigned(S) then F.UcfposOpen := TUcfposOpen(S);
  S := ResolveSymbol(AHandle, 'ucfpos_close');
  if Assigned(S) then F.UcfposClose := TUcfposClose(S);
  S := ResolveSymbol(AHandle, 'ucfpos_getCategory');
  if Assigned(S) then F.UcfposGetCategory := TUcfposGetCategory(S);
  S := ResolveSymbol(AHandle, 'ucfpos_getField');
  if Assigned(S) then F.UcfposGetField := TUcfposGetField(S);
  S := ResolveSymbol(AHandle, 'ucfpos_getIndexes');
  if Assigned(S) then F.UcfposGetIndexes := TUcfposGetIndexes(S);
  S := ResolveSymbol(AHandle, 'ufmtval_getString');
  if Assigned(S) then F.UfmtvalGetString := TUfmtvalGetString(S);
  S := ResolveSymbol(AHandle, 'ufmtval_nextPosition');
  if Assigned(S) then F.UfmtvalNextPosition := TUfmtvalNextPosition(S);
  S := ResolveSymbol(AHandle, 'unumrf_openForSkeletonWithCollapseAndIdentityFallback');
  if Assigned(S) then
    F.UnumrfOpenForSkeletonWithCollapseAndIdentityFallback :=
      TUnumrfOpenForSkeletonWithCollapseAndIdentityFallback(S);
  S := ResolveSymbol(AHandle, 'unumrf_openResult');
  if Assigned(S) then F.UnumrfOpenResult := TUnumrfOpenResult(S);
  S := ResolveSymbol(AHandle, 'unumrf_formatDoubleRange');
  if Assigned(S) then F.UnumrfFormatDoubleRange := TUnumrfFormatDoubleRange(S);
  S := ResolveSymbol(AHandle, 'unumrf_formatDecimalRange');
  if Assigned(S) then F.UnumrfFormatDecimalRange := TUnumrfFormatDecimalRange(S);
  S := ResolveSymbol(AHandle, 'unumrf_resultAsValue');
  if Assigned(S) then F.UnumrfResultAsValue := TUnumrfResultAsValue(S);
  S := ResolveSymbol(AHandle, 'unumrf_close');
  if Assigned(S) then F.UnumrfClose := TUnumrfClose(S);
  S := ResolveSymbol(AHandle, 'unumrf_closeResult');
  if Assigned(S) then F.UnumrfCloseResult := TUnumrfCloseResult(S);
  S := ResolveSymbol(AHandle, 'uplrules_open');
  if Assigned(S) then F.UplrulesOpen := TUplrulesOpen(S);
  S := ResolveSymbol(AHandle, 'uplrules_openForType');
  if Assigned(S) then F.UplrulesOpenForType := TUplrulesOpenForType(S);
  S := ResolveSymbol(AHandle, 'uplrules_close');
  if Assigned(S) then F.UplrulesClose := TUplrulesClose(S);
  S := ResolveSymbol(AHandle, 'uplrules_select');
  if Assigned(S) then F.UplrulesSelect := TUplrulesSelect(S);
  S := ResolveSymbol(AHandle, 'ubrk_open');
  if Assigned(S) then F.UbrkOpen := TUbrkOpen(S);
  S := ResolveSymbol(AHandle, 'ubrk_close');
  if Assigned(S) then F.UbrkClose := TUbrkClose(S);
  S := ResolveSymbol(AHandle, 'ubrk_next');
  if Assigned(S) then F.UbrkNext := TUbrkNext(S);
  S := ResolveSymbol(AHandle, 'ubrk_getRuleStatus');
  if Assigned(S) then F.UbrkGetRuleStatus := TUbrkGetRuleStatus(S);
  S := ResolveSymbol(AHandle, 'ubrk_setText');
  if Assigned(S) then F.UbrkSetText := TUbrkSetText(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_openForType');
  if Assigned(S) then F.UlistfmtOpenForType := TUlistfmtOpenForType(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_close');
  if Assigned(S) then F.UlistfmtClose := TUlistfmtClose(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_format');
  if Assigned(S) then F.UlistfmtFormat := TUlistfmtFormat(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_openResult');
  if Assigned(S) then F.UlistfmtOpenResult := TUlistfmtOpenResult(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_closeResult');
  if Assigned(S) then F.UlistfmtCloseResult := TUlistfmtCloseResult(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_formatStringsToResult');
  if Assigned(S) then F.UlistfmtFormatStringsToResult := TUlistfmtFormatStringsToResult(S);
  S := ResolveSymbol(AHandle, 'ulistfmt_resultAsValue');
  if Assigned(S) then F.UlistfmtResultAsValue := TUlistfmtResultAsValue(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_open');
  if Assigned(S) then F.UreldatefmtOpen := TUreldatefmtOpen(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_close');
  if Assigned(S) then F.UreldatefmtClose := TUreldatefmtClose(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_formatNumeric');
  if Assigned(S) then F.UreldatefmtFormatNumeric := TUreldatefmtFormatNumeric(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_format');
  if Assigned(S) then F.UreldatefmtFormat := TUreldatefmtFormat(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_openResult');
  if Assigned(S) then F.UreldatefmtOpenResult := TUreldatefmtOpenResult(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_closeResult');
  if Assigned(S) then F.UreldatefmtCloseResult := TUreldatefmtCloseResult(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_formatNumericToResult');
  if Assigned(S) then F.UreldatefmtFormatNumericToResult := TUreldatefmtFormatNumericToResult(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_formatToResult');
  if Assigned(S) then F.UreldatefmtFormatToResult := TUreldatefmtFormatToResult(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_resultAsValue');
  if Assigned(S) then F.UreldatefmtResultAsValue := TUreldatefmtResultAsValue(S);
  S := ResolveSymbol(AHandle, 'ucasemap_open');
  if Assigned(S) then F.UcasemapOpen := TUcasemapOpen(S);
  S := ResolveSymbol(AHandle, 'ucasemap_close');
  if Assigned(S) then F.UcasemapClose := TUcasemapClose(S);
  S := ResolveSymbol(AHandle, 'ucasemap_utf8ToUpper');
  if Assigned(S) then F.UcasemapUtf8ToUpper := TUcasemapUtf8ToUpper(S);
  S := ResolveSymbol(AHandle, 'ucasemap_utf8ToLower');
  if Assigned(S) then F.UcasemapUtf8ToLower := TUcasemapUtf8ToLower(S);
  S := ResolveSymbol(AHandle, 'unorm2_getNFCInstance');
  if Assigned(S) then F.Unorm2GetNFCInstance := TUnorm2GetInstance(S);
  S := ResolveSymbol(AHandle, 'unorm2_getNFDInstance');
  if Assigned(S) then F.Unorm2GetNFDInstance := TUnorm2GetInstance(S);
  S := ResolveSymbol(AHandle, 'unorm2_getNFKCInstance');
  if Assigned(S) then F.Unorm2GetNFKCInstance := TUnorm2GetInstance(S);
  S := ResolveSymbol(AHandle, 'unorm2_getNFKDInstance');
  if Assigned(S) then F.Unorm2GetNFKDInstance := TUnorm2GetInstance(S);
  S := ResolveSymbol(AHandle, 'unorm2_normalize');
  if Assigned(S) then F.Unorm2Normalize := TUnorm2Normalize(S);

  IntlFunctions := F;
  Result := True;
end;

function EnsureLoaded: Boolean;
var
  Handle: TLibHandle;
begin
  if IntlLoadAttempted then
  begin
    Result := IntlLoadSucceeded;
    Exit;
  end;
  CriticalSectionEnter(IntlInitLock);
  try
    if not IntlLoadAttempted then
    begin
      IntlLoadAttempted := True;
      if TryGetICULibraryHandle(Handle) then
        IntlLoadSucceeded := TryLoadIntlFunctions(Handle);
    end;
    Result := IntlLoadSucceeded;
  finally
    CriticalSectionLeave(IntlInitLock);
  end;
end;

function IntlICUAvailable: Boolean;
begin
  Result := EnsureLoaded;
end;

function UnicodeToString(const ABuf: array of WideChar; ALen: Integer): string;
var
  U: string;
begin
  if ALen <= 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(U, ALen);
  Move(ABuf[0], U[1], ALen * SizeOf(WideChar));
  Result := string(U);
end;

function UnicodePointerToString(AChars: PUChar; ALen: Integer): string;
var
  U: string;
begin
  if (ALen <= 0) or not Assigned(AChars) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(U, ALen);
  Move(AChars^, U[1], ALen * SizeOf(WideChar));
  Result := string(U);
end;

procedure AppendFormatPart(var AParts: TIntlFormatPartArray;
  const APartType, AValue, ASource: string);
var
  Index: Integer;
begin
  if AValue = '' then
    Exit;

  Index := Length(AParts) - 1;
  if (Index >= 0) and (AParts[Index].PartType = APartType) and
     (AParts[Index].Source = ASource) then
  begin
    AParts[Index].Value := AParts[Index].Value + AValue;
    Exit;
  end;

  SetLength(AParts, Length(AParts) + 1);
  Index := Length(AParts) - 1;
  AParts[Index].PartType := APartType;
  AParts[Index].Value := AValue;
  AParts[Index].Source := ASource;
  AParts[Index].UnitIdentifier := '';
end;

procedure AppendFieldSpan(var ASpans: TICUFieldSpanArray;
  ACategory, AField, AStartIndex, AEndIndex: Integer);
var
  Index: Integer;
begin
  if AEndIndex <= AStartIndex then
    Exit;
  SetLength(ASpans, Length(ASpans) + 1);
  Index := Length(ASpans) - 1;
  ASpans[Index].Category := ACategory;
  ASpans[Index].Field := AField;
  ASpans[Index].StartIndex := AStartIndex;
  ASpans[Index].EndIndex := AEndIndex;
end;

procedure AddBoundary(var ABoundaries: array of Integer; var ACount: Integer;
  AValue: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    if ABoundaries[I] = AValue then
      Exit;
  ABoundaries[ACount] := AValue;
  Inc(ACount);
end;

procedure SortBoundaries(var ABoundaries: array of Integer; ACount: Integer);
var
  I, J, Value: Integer;
begin
  for I := 1 to ACount - 1 do
  begin
    Value := ABoundaries[I];
    J := I - 1;
    while (J >= 0) and (ABoundaries[J] > Value) do
    begin
      ABoundaries[J + 1] := ABoundaries[J];
      Dec(J);
    end;
    ABoundaries[J + 1] := Value;
  end;
end;

function NumberFieldToPartType(AField: Integer): string;
begin
  case AField of
    UNUM_INTEGER_FIELD: Result := 'integer';
    UNUM_FRACTION_FIELD: Result := 'fraction';
    UNUM_DECIMAL_SEPARATOR_FIELD: Result := 'decimal';
    UNUM_EXPONENT_SYMBOL_FIELD: Result := 'exponentSeparator';
    UNUM_EXPONENT_SIGN_FIELD: Result := 'exponentMinusSign';
    UNUM_EXPONENT_FIELD: Result := 'exponentInteger';
    UNUM_GROUPING_SEPARATOR_FIELD: Result := 'group';
    UNUM_CURRENCY_FIELD: Result := 'currency';
    UNUM_PERCENT_FIELD: Result := 'percentSign';
    UNUM_PERMILL_FIELD: Result := 'literal';
    UNUM_SIGN_FIELD: Result := 'sign';
    UNUM_MEASURE_UNIT_FIELD: Result := 'unit';
    UNUM_COMPACT_FIELD: Result := 'compact';
    UNUM_APPROXIMATELY_SIGN_FIELD: Result := 'approximatelySign';
  else
    Result := 'literal';
  end;
end;

function DateFieldToPartType(AField: Integer): string;
begin
  case AField of
    UDAT_ERA_FIELD: Result := 'era';
    UDAT_YEAR_FIELD: Result := 'year';
    UDAT_MONTH_FIELD,
    UDAT_STANDALONE_MONTH_FIELD: Result := 'month';
    UDAT_YEAR_NAME_FIELD: Result := 'yearName';
    UDAT_RELATED_YEAR_FIELD: Result := 'relatedYear';
    UDAT_DATE_FIELD: Result := 'day';
    UDAT_HOUR_OF_DAY1_FIELD,
    UDAT_HOUR_OF_DAY0_FIELD,
    UDAT_HOUR1_FIELD,
    UDAT_HOUR0_FIELD: Result := 'hour';
    UDAT_MINUTE_FIELD: Result := 'minute';
    UDAT_SECOND_FIELD: Result := 'second';
    UDAT_FRACTIONAL_SECOND_FIELD: Result := 'fractionalSecond';
    UDAT_DAY_OF_WEEK_FIELD: Result := 'weekday';
    UDAT_AM_PM_FIELD,
    UDAT_AM_PM_MIDNIGHT_NOON_FIELD,
    UDAT_FLEXIBLE_DAY_PERIOD_FIELD: Result := 'dayPeriod';
    UDAT_TIMEZONE_FIELD,
    UDAT_TIMEZONE_RFC_FIELD,
    UDAT_TIMEZONE_GENERIC_FIELD,
    UDAT_TIMEZONE_SPECIAL_FIELD,
    UDAT_TIMEZONE_LOCALIZED_GMT_OFFSET_FIELD,
    UDAT_TIMEZONE_ISO_FIELD,
    UDAT_TIMEZONE_ISO_LOCAL_FIELD: Result := 'timeZoneName';
  else
    Result := 'literal';
  end;
end;

function ListFieldToPartType(AField: Integer): string;
begin
  case AField of
    ULISTFMT_ELEMENT_FIELD: Result := 'element';
  else
    Result := 'literal';
  end;
end;

function BestPartTypeForSegment(const ASpans: TICUFieldSpanArray;
  AFieldCategory, AStartIndex, AEndIndex: Integer;
  AMapper: TICUFieldTypeMapper): string;
var
  I, SpanLength, BestLength, BestField: Integer;
begin
  BestField := -1;
  BestLength := MaxInt;
  for I := 0 to Length(ASpans) - 1 do
    if (ASpans[I].Category = AFieldCategory) and
       (AStartIndex >= ASpans[I].StartIndex) and
       (AEndIndex <= ASpans[I].EndIndex) then
    begin
      SpanLength := ASpans[I].EndIndex - ASpans[I].StartIndex;
      if SpanLength < BestLength then
      begin
        BestLength := SpanLength;
        BestField := ASpans[I].Field;
      end;
    end;

  if BestField >= 0 then
    Result := AMapper(BestField)
  else
    Result := 'literal';
end;

function HasContainingField(const ASpans: TICUFieldSpanArray;
  AFieldCategory, AField, AStartIndex, AEndIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ASpans) - 1 do
    if (ASpans[I].Category = AFieldCategory) and
       ((AField < 0) or (ASpans[I].Field = AField)) and
       (AStartIndex >= ASpans[I].StartIndex) and
       (AEndIndex <= ASpans[I].EndIndex) then
    begin
      Result := True;
      Exit;
    end;
end;

function RangeSourceForSegment(const ASpans: TICUFieldSpanArray;
  ARangeSpanCategory, AStartIndex, AEndIndex: Integer): string;
var
  I: Integer;
begin
  Result := 'shared';
  if ARangeSpanCategory = 0 then
  begin
    Result := '';
    Exit;
  end;

  for I := 0 to Length(ASpans) - 1 do
    if (ASpans[I].Category = ARangeSpanCategory) and
       (AStartIndex >= ASpans[I].StartIndex) and
       (AEndIndex <= ASpans[I].EndIndex) then
    begin
      if ASpans[I].Field = 0 then
        Result := 'startRange'
      else if ASpans[I].Field = 1 then
        Result := 'endRange';
      Exit;
    end;
end;

function BuildPartsFromFieldSpans(const AFormatted: string;
  const ASpans: TICUFieldSpanArray; AFieldCategory, ARangeSpanCategory: Integer;
  AMapper: TICUFieldTypeMapper; out AParts: TIntlFormatPartArray): Boolean;
var
  Boundaries: array of Integer;
  BoundaryCount, I, SegStart, SegEnd, FormattedLength: Integer;
  PartType, Value, Source: string;
begin
  Result := False;
  SetLength(AParts, 0);
  FormattedLength := Length(AFormatted);
  SetLength(Boundaries, (Length(ASpans) * 2) + 2);
  BoundaryCount := 0;
  AddBoundary(Boundaries, BoundaryCount, 0);
  AddBoundary(Boundaries, BoundaryCount, FormattedLength);

  for I := 0 to Length(ASpans) - 1 do
    if (ASpans[I].StartIndex >= 0) and
       (ASpans[I].EndIndex <= FormattedLength) then
    begin
      AddBoundary(Boundaries, BoundaryCount, ASpans[I].StartIndex);
      AddBoundary(Boundaries, BoundaryCount, ASpans[I].EndIndex);
    end;

  SortBoundaries(Boundaries, BoundaryCount);
  for I := 0 to BoundaryCount - 2 do
  begin
    SegStart := Boundaries[I];
    SegEnd := Boundaries[I + 1];
    if SegEnd <= SegStart then
      Continue;

    Value := string(Copy(AFormatted, SegStart + 1, SegEnd - SegStart));
    Source := RangeSourceForSegment(ASpans, ARangeSpanCategory, SegStart, SegEnd);
    PartType := BestPartTypeForSegment(ASpans, AFieldCategory, SegStart, SegEnd, AMapper);
    if PartType = 'sign' then
    begin
      if (Pos('+', Value) > 0) then
        PartType := 'plusSign'
      else
        PartType := 'minusSign';
    end;
    AppendFormatPart(AParts, PartType, Value, Source);
  end;

  Result := True;
end;

function BuildRelativeTimePartsFromFieldSpans(const AFormatted: string;
  const ASpans: TICUFieldSpanArray; const AUnitIdentifier: string;
  out AParts: TIntlFormatPartArray): Boolean;
var
  Boundaries: array of Integer;
  BoundaryCount, I, SegStart, SegEnd, FormattedLength: Integer;
  PartType, Value: string;
  IsNumeric: Boolean;
begin
  Result := False;
  SetLength(AParts, 0);
  FormattedLength := Length(AFormatted);
  SetLength(Boundaries, (Length(ASpans) * 2) + 2);
  BoundaryCount := 0;
  AddBoundary(Boundaries, BoundaryCount, 0);
  AddBoundary(Boundaries, BoundaryCount, FormattedLength);

  for I := 0 to Length(ASpans) - 1 do
    if (ASpans[I].StartIndex >= 0) and
       (ASpans[I].EndIndex <= FormattedLength) and
       ((ASpans[I].Category = UFIELD_CATEGORY_NUMBER) or
        ((ASpans[I].Category = UFIELD_CATEGORY_RELATIVE_DATETIME) and
         (ASpans[I].Field = UDAT_REL_NUMERIC_FIELD))) then
    begin
      AddBoundary(Boundaries, BoundaryCount, ASpans[I].StartIndex);
      AddBoundary(Boundaries, BoundaryCount, ASpans[I].EndIndex);
    end;

  SortBoundaries(Boundaries, BoundaryCount);
  for I := 0 to BoundaryCount - 2 do
  begin
    SegStart := Boundaries[I];
    SegEnd := Boundaries[I + 1];
    if SegEnd <= SegStart then
      Continue;

    Value := string(Copy(AFormatted, SegStart + 1, SegEnd - SegStart));
    IsNumeric := HasContainingField(ASpans, UFIELD_CATEGORY_RELATIVE_DATETIME,
      UDAT_REL_NUMERIC_FIELD, SegStart, SegEnd);
    PartType := BestPartTypeForSegment(ASpans, UFIELD_CATEGORY_NUMBER,
      SegStart, SegEnd, NumberFieldToPartType);
    if (PartType = 'literal') and IsNumeric then
      PartType := 'integer';
    if PartType = 'sign' then
    begin
      if Pos('+', Value) > 0 then
        PartType := 'plusSign'
      else
        PartType := 'minusSign';
    end;
    AppendFormatPart(AParts, PartType, Value, '');
    if IsNumeric and (PartType <> 'literal') then
      AParts[High(AParts)].UnitIdentifier := AUnitIdentifier;
  end;

  Result := True;
end;

procedure CollectIteratorFieldSpans(AIterator: Pointer; ACategory: Integer;
  var ASpans: TICUFieldSpanArray);
var
  Field, StartIndex, EndIndex: LongInt;
begin
  while True do
  begin
    StartIndex := 0;
    EndIndex := 0;
    Field := IntlFunctions.UfieldpositerNext(AIterator, @StartIndex, @EndIndex);
    if Field < 0 then
      Break;
    AppendFieldSpan(ASpans, ACategory, Field, StartIndex, EndIndex);
  end;
end;

function FormattedValueToFieldSpans(AFormattedValue: Pointer;
  out AFormatted: string; out ASpans: TICUFieldSpanArray): Boolean;
var
  Status: TICUErrorCode;
  Len, Category, Field, StartIndex, EndIndex: LongInt;
  UText: PUChar;
  Position: Pointer;
begin
  Result := False;
  AFormatted := '';
  SetLength(ASpans, 0);

  if not Assigned(IntlFunctions.UfmtvalGetString) or
     not Assigned(IntlFunctions.UfmtvalNextPosition) or
     not Assigned(IntlFunctions.UcfposOpen) or
     not Assigned(IntlFunctions.UcfposClose) or
     not Assigned(IntlFunctions.UcfposGetCategory) or
     not Assigned(IntlFunctions.UcfposGetField) or
     not Assigned(IntlFunctions.UcfposGetIndexes) then
    Exit;

  Status := ICU_SUCCESS;
  Len := 0;
  UText := IntlFunctions.UfmtvalGetString(AFormattedValue, @Len, Status);
  if not ICUSucceeded(Status) or not Assigned(UText) then
    Exit;

  AFormatted := UnicodePointerToString(UText, Len);

  Status := ICU_SUCCESS;
  Position := IntlFunctions.UcfposOpen(Status);
  if not ICUSucceeded(Status) or not Assigned(Position) then
    Exit;
  try
    while True do
    begin
      Status := ICU_SUCCESS;
      if not IntlFunctions.UfmtvalNextPosition(AFormattedValue, Position, Status) then
        Break;
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      Category := IntlFunctions.UcfposGetCategory(Position, Status);
      if not ICUSucceeded(Status) then Exit;
      Status := ICU_SUCCESS;
      Field := IntlFunctions.UcfposGetField(Position, Status);
      if not ICUSucceeded(Status) then Exit;
      Status := ICU_SUCCESS;
      StartIndex := 0;
      EndIndex := 0;
      IntlFunctions.UcfposGetIndexes(Position, @StartIndex, @EndIndex, Status);
      if not ICUSucceeded(Status) then Exit;
      AppendFieldSpan(ASpans, Category, Field, StartIndex, EndIndex);
    end;
  finally
    IntlFunctions.UcfposClose(Position);
  end;

  Result := True;
end;

function FormattedValueToParts(AFormattedValue: Pointer; AFieldCategory,
  ARangeSpanCategory: Integer; AMapper: TICUFieldTypeMapper;
  out AFormatted: string; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Len, Category, Field, StartIndex, EndIndex: LongInt;
  UText: PUChar;
  UFormatted: string;
  Position: Pointer;
  Spans: TICUFieldSpanArray;
begin
  Result := False;
  AFormatted := '';
  SetLength(AParts, 0);

  if not Assigned(IntlFunctions.UfmtvalGetString) or
     not Assigned(IntlFunctions.UfmtvalNextPosition) or
     not Assigned(IntlFunctions.UcfposOpen) or
     not Assigned(IntlFunctions.UcfposClose) or
     not Assigned(IntlFunctions.UcfposGetCategory) or
     not Assigned(IntlFunctions.UcfposGetField) or
     not Assigned(IntlFunctions.UcfposGetIndexes) then
    Exit;

  Status := ICU_SUCCESS;
  Len := 0;
  UText := IntlFunctions.UfmtvalGetString(AFormattedValue, @Len, Status);
  if not ICUSucceeded(Status) or not Assigned(UText) then
    Exit;

  AFormatted := UnicodePointerToString(UText, Len);
  UFormatted := string(AFormatted);
  SetLength(Spans, 0);

  Status := ICU_SUCCESS;
  Position := IntlFunctions.UcfposOpen(Status);
  if not ICUSucceeded(Status) or not Assigned(Position) then
    Exit;
  try
    while True do
    begin
      Status := ICU_SUCCESS;
      if not IntlFunctions.UfmtvalNextPosition(AFormattedValue, Position, Status) then
        Break;
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      Category := IntlFunctions.UcfposGetCategory(Position, Status);
      if not ICUSucceeded(Status) then Exit;
      Status := ICU_SUCCESS;
      Field := IntlFunctions.UcfposGetField(Position, Status);
      if not ICUSucceeded(Status) then Exit;
      Status := ICU_SUCCESS;
      StartIndex := 0;
      EndIndex := 0;
      IntlFunctions.UcfposGetIndexes(Position, @StartIndex, @EndIndex, Status);
      if not ICUSucceeded(Status) then Exit;
      AppendFieldSpan(Spans, Category, Field, StartIndex, EndIndex);
    end;
  finally
    IntlFunctions.UcfposClose(Position);
  end;

  Result := BuildPartsFromFieldSpans(UFormatted, Spans, AFieldCategory,
    ARangeSpanCategory, AMapper, AParts);
end;

function TryICUCanonicalizeLocale(const ATag: string; out ACanonical: string): Boolean;
var
  Status: TICUErrorCode;
  LocaleId: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  TagBuf: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  ParsedLen, ResultLen: LongInt;
  TagBytes: TBytes;
begin
  Result := False;
  ACanonical := '';

  if not EnsureLoaded then
    Exit;

  if not TryEncodeASCIIBytes(ATag, TagBytes) then
    Exit;
  FillChar(LocaleId, SizeOf(LocaleId), 0);
  Status := ICU_SUCCESS;
  ParsedLen := 0;

  IntlFunctions.UlocForLanguageTag(BytePointer(TagBytes), @LocaleId[0],
    LOCALE_ID_CAPACITY, ParsedLen, Status);
  if not ICUSucceeded(Status) then
    Exit;

  FillChar(TagBuf, SizeOf(TagBuf), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UlocToLanguageTag(@LocaleId[0], @TagBuf[0],
    LOCALE_ID_CAPACITY, True, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  ACanonical := DecodeASCIIZ(@TagBuf[0]);
  Result := ACanonical <> '';
end;

function TryICUGetAvailableLocales(out ALocales: IntlTypes.TStringArray): Boolean;
var
  Count, I: LongInt;
  LocalePtr: PAnsiChar;
begin
  Result := False;
  SetLength(ALocales, 0);

  if not EnsureLoaded then
    Exit;

  Count := IntlFunctions.UlocCountAvailable;
  if Count <= 0 then
    Exit;

  SetLength(ALocales, Count);
  for I := 0 to Count - 1 do
  begin
    LocalePtr := IntlFunctions.UlocGetAvailable(I);
    if Assigned(LocalePtr) then
      ALocales[I] := DecodeASCIIZ(LocalePtr)
    else
      ALocales[I] := '';
  end;

  Result := True;
end;

function TryICUMaximizeLocale(const ATag: string; out AMaximized: string): Boolean;
var
  Status: TICUErrorCode;
  LocaleId, MaxId: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  TagBuf: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  ParsedLen, ResultLen: LongInt;
  TagBytes: TBytes;
begin
  Result := False;
  AMaximized := '';

  if not EnsureLoaded then
    Exit;

  if not TryEncodeASCIIBytes(ATag, TagBytes) then
    Exit;
  FillChar(LocaleId, SizeOf(LocaleId), 0);
  Status := ICU_SUCCESS;
  ParsedLen := 0;

  IntlFunctions.UlocForLanguageTag(BytePointer(TagBytes), @LocaleId[0],
    LOCALE_ID_CAPACITY, ParsedLen, Status);
  if not ICUSucceeded(Status) then
    Exit;

  FillChar(MaxId, SizeOf(MaxId), 0);
  Status := ICU_SUCCESS;
  IntlFunctions.UlocAddLikelySubtags(@LocaleId[0], @MaxId[0],
    LOCALE_ID_CAPACITY, Status);
  if not ICUSucceeded(Status) then
    Exit;

  FillChar(TagBuf, SizeOf(TagBuf), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UlocToLanguageTag(@MaxId[0], @TagBuf[0],
    LOCALE_ID_CAPACITY, False, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  AMaximized := DecodeASCIIZ(@TagBuf[0]);
  Result := True;
end;

function TryICUMinimizeLocale(const ATag: string; out AMinimized: string): Boolean;
var
  Status: TICUErrorCode;
  LocaleId, MinId: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  TagBuf: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  ParsedLen, ResultLen: LongInt;
  TagBytes: TBytes;
begin
  Result := False;
  AMinimized := '';

  if not EnsureLoaded then
    Exit;

  if not TryEncodeASCIIBytes(ATag, TagBytes) then
    Exit;
  FillChar(LocaleId, SizeOf(LocaleId), 0);
  Status := ICU_SUCCESS;
  ParsedLen := 0;

  IntlFunctions.UlocForLanguageTag(BytePointer(TagBytes), @LocaleId[0],
    LOCALE_ID_CAPACITY, ParsedLen, Status);
  if not ICUSucceeded(Status) then
    Exit;

  FillChar(MinId, SizeOf(MinId), 0);
  Status := ICU_SUCCESS;
  IntlFunctions.UlocMinimizeSubtags(@LocaleId[0], @MinId[0],
    LOCALE_ID_CAPACITY, Status);
  if not ICUSucceeded(Status) then
    Exit;

  FillChar(TagBuf, SizeOf(TagBuf), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UlocToLanguageTag(@MinId[0], @TagBuf[0],
    LOCALE_ID_CAPACITY, False, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  AMinimized := DecodeASCIIZ(@TagBuf[0]);
  Result := True;
end;

function TryICUEnumerationToArray(AEnumeration: Pointer;
  out AValues: IntlTypes.TStringArray): Boolean;
var
  Status: TICUErrorCode;
  ValueLength, Count: LongInt;
  ValuePtr: PAnsiChar;
begin
  Result := False;
  SetLength(AValues, 0);

  if (not Assigned(AEnumeration)) or (not Assigned(IntlFunctions.UenumNext)) or
     (not Assigned(IntlFunctions.UenumClose)) then
    Exit;

  Count := 0;
  try
    repeat
      Status := ICU_SUCCESS;
      ValueLength := 0;
      ValuePtr := IntlFunctions.UenumNext(AEnumeration, @ValueLength, Status);
      if not ICUSucceeded(Status) then
        Exit;
      if not Assigned(ValuePtr) then
        Break;

      Inc(Count);
      SetLength(AValues, Count);
      AValues[Count - 1] := DecodeASCIIBytes(ValuePtr, ValueLength);
    until False;
  finally
    IntlFunctions.UenumClose(AEnumeration);
  end;

  Result := Count > 0;
end;

function TryICUGetLocaleCalendars(const ALocale: string;
  out ACalendars: IntlTypes.TStringArray): Boolean;
var
  Status: TICUErrorCode;
  KeyBytes, LocaleBytes: TBytes;
  Enumeration: Pointer;
begin
  Result := False;
  SetLength(ACalendars, 0);

  if not EnsureLoaded or
     (not Assigned(IntlFunctions.UcalGetKeywordValuesForLocale)) then
    Exit;

  if not TryEncodeASCIIBytes('calendar', KeyBytes) or
     not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Enumeration := IntlFunctions.UcalGetKeywordValuesForLocale(
    BytePointer(KeyBytes), BytePointer(LocaleBytes), True, Status);
  if not ICUSucceeded(Status) or not Assigned(Enumeration) then
    Exit;

  Result := TryICUEnumerationToArray(Enumeration, ACalendars);
end;

function TryICUGetLocaleCollations(const ALocale: string;
  out ACollations: IntlTypes.TStringArray): Boolean;
var
  Status: TICUErrorCode;
  KeyBytes, LocaleBytes: TBytes;
  Enumeration: Pointer;
begin
  Result := False;
  SetLength(ACollations, 0);

  if not EnsureLoaded or
     (not Assigned(IntlFunctions.UcolGetKeywordValuesForLocale)) then
    Exit;

  if not TryEncodeASCIIBytes('collation', KeyBytes) or
     not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Enumeration := IntlFunctions.UcolGetKeywordValuesForLocale(
    BytePointer(KeyBytes), BytePointer(LocaleBytes), True, Status);
  if not ICUSucceeded(Status) or not Assigned(Enumeration) then
    Exit;

  Result := TryICUEnumerationToArray(Enumeration, ACollations);
end;

function TryICUCompareStrings(const ALocale: string; const AStr1, AStr2: string;
  ASensitivity: TIntlCollatorSensitivity; AIgnorePunctuation, ANumeric: Boolean;
  const ACaseFirst: string; out AResult: Integer): Boolean;
var
  Status: TICUErrorCode;
  Collator: Pointer;
  CollResult: TICUCollationResult;
  Strength: LongInt;
  LocaleBytes: TBytes;
begin
  Result := False;
  AResult := 0;

  if not EnsureLoaded then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Collator := IntlFunctions.UcolOpen(BytePointer(LocaleBytes), Status);
  if not ICUSucceeded(Status) or not Assigned(Collator) then
    Exit;

  try
    case ASensitivity of
      icsBase: Strength := UCOL_PRIMARY;
      icsAccent: Strength := UCOL_SECONDARY;
      icsCase: Strength := UCOL_PRIMARY;
      icsVariant: Strength := UCOL_TERTIARY;
    else
      Strength := UCOL_TERTIARY;
    end;

    Status := ICU_SUCCESS;
    IntlFunctions.UcolSetAttribute(Collator, UCOL_STRENGTH, Strength, Status);
    if not ICUSucceeded(Status) then
      Exit;

    Status := ICU_SUCCESS;
    IntlFunctions.UcolSetAttribute(Collator, UCOL_NORMALIZATION_MODE, UCOL_ON, Status);
    if not ICUSucceeded(Status) then
      Exit;

    if ANumeric then
    begin
      Status := ICU_SUCCESS;
      IntlFunctions.UcolSetAttribute(Collator, UCOL_NUMERIC_COLLATION, UCOL_ON, Status);
      if not ICUSucceeded(Status) then
        Exit;
    end;

    if ACaseFirst = 'upper' then
    begin
      Status := ICU_SUCCESS;
      IntlFunctions.UcolSetAttribute(Collator, UCOL_CASE_FIRST, UCOL_UPPER_FIRST, Status);
      if not ICUSucceeded(Status) then
        Exit;
    end
    else if ACaseFirst = 'lower' then
    begin
      Status := ICU_SUCCESS;
      IntlFunctions.UcolSetAttribute(Collator, UCOL_CASE_FIRST, UCOL_LOWER_FIRST, Status);
      if not ICUSucceeded(Status) then
        Exit;
    end
    else if ACaseFirst = 'false' then
    begin
      Status := ICU_SUCCESS;
      IntlFunctions.UcolSetAttribute(Collator, UCOL_CASE_FIRST, UCOL_OFF, Status);
      if not ICUSucceeded(Status) then
        Exit;
    end;

    if ASensitivity = icsCase then
    begin
      Status := ICU_SUCCESS;
      IntlFunctions.UcolSetAttribute(Collator, UCOL_CASE_LEVEL, UCOL_ON, Status);
      if not ICUSucceeded(Status) then
        Exit;
    end;

    Status := ICU_SUCCESS;
    if AIgnorePunctuation then
      IntlFunctions.UcolSetAttribute(Collator, UCOL_ALTERNATE_HANDLING,
        UCOL_SHIFTED, Status)
    else
      IntlFunctions.UcolSetAttribute(Collator, UCOL_ALTERNATE_HANDLING,
        UCOL_NON_IGNORABLE, Status);
    if not ICUSucceeded(Status) then
      Exit;

    CollResult := IntlFunctions.UcolStrcoll(Collator,
      PWideChar(AStr1), Length(AStr1),
      PWideChar(AStr2), Length(AStr2));

    case CollResult of
      UCOL_LESS: AResult := -1;
      UCOL_EQUAL: AResult := 0;
      UCOL_GREATER: AResult := 1;
    else
      AResult := 0;
    end;

    Result := True;
  finally
    IntlFunctions.UcolClose(Collator);
  end;
end;

function NumberStyleToICU(AStyle: TIntlNumberStyle): LongInt;
begin
  case AStyle of
    insDecimal: Result := UNUM_DECIMAL_NUMBER_STYLE;
    insCurrency: Result := UNUM_CURRENCY_STYLE;
    insPercent: Result := UNUM_PERCENT_STYLE;
    insUnit: Result := UNUM_DECIMAL_NUMBER_STYLE;
  else
    Result := UNUM_DECIMAL_NUMBER_STYLE;
  end;
end;

function RoundingModeToICU(AMode: TIntlNumberRoundingMode): LongInt;
begin
  case AMode of
    inrmCeil: Result := UNUM_ROUND_CEILING;
    inrmFloor: Result := UNUM_ROUND_FLOOR;
    inrmExpand: Result := UNUM_ROUND_UP;
    inrmTrunc: Result := UNUM_ROUND_DOWN;
    inrmHalfCeil: Result := UNUM_ROUND_HALF_CEILING;
    inrmHalfFloor: Result := UNUM_ROUND_HALF_FLOOR;
    inrmHalfExpand: Result := UNUM_ROUND_HALFUP;
    inrmHalfTrunc: Result := UNUM_ROUND_HALFDOWN;
    inrmHalfEven: Result := UNUM_ROUND_HALFEVEN;
  else
    Result := UNUM_ROUND_HALFUP;
  end;
end;

procedure ApplySignificantDigitsPattern(AFormatter: Pointer;
  AMinSig, AMaxSig: Integer);
var
  Pattern: string;
  Status: TICUErrorCode;
begin
  if not Assigned(IntlFunctions.UnumApplyPattern) then
    Exit;
  if AMinSig < 1 then AMinSig := 1;
  if AMaxSig < AMinSig then AMaxSig := AMinSig;
  Pattern := string(StringOfChar('@', AMinSig) +
    StringOfChar('#', AMaxSig - AMinSig));
  Status := ICU_SUCCESS;
  IntlFunctions.UnumApplyPattern(AFormatter, False,
    PWideChar(Pattern), Length(Pattern), nil, Status);
end;

procedure ConfigureNumberFormatter(AFormatter: Pointer;
  const AOptions: TIntlNumberFormatOptions);
var
  MinSig, MaxSig: Integer;
  CurrencyUnicode: string;
  Status: TICUErrorCode;
begin
  if not Assigned(IntlFunctions.UnumSetAttribute) then
    Exit;

  if (AOptions.Style = insCurrency) and (AOptions.Currency <> '') and
     Assigned(IntlFunctions.UnumSetTextAttribute) then
  begin
    CurrencyUnicode := string(AOptions.Currency);
    Status := ICU_SUCCESS;
    IntlFunctions.UnumSetTextAttribute(AFormatter, UNUM_CURRENCY_CODE,
      PWideChar(CurrencyUnicode), Length(CurrencyUnicode), Status);
  end;

  if (AOptions.MinimumSignificantDigits > 0) or
     (AOptions.MaximumSignificantDigits > 0) then
  begin
    MinSig := AOptions.MinimumSignificantDigits;
    MaxSig := AOptions.MaximumSignificantDigits;
    if MinSig < 1 then MinSig := 1;
    if MaxSig < 1 then MaxSig := 21;
    ApplySignificantDigitsPattern(AFormatter, MinSig, MaxSig);
  end
  else
  begin
    if AOptions.MinimumFractionDigits >= 0 then
      IntlFunctions.UnumSetAttribute(AFormatter,
        UNUM_MIN_FRACTION_DIGITS, AOptions.MinimumFractionDigits);
    if AOptions.MaximumFractionDigits >= 0 then
      IntlFunctions.UnumSetAttribute(AFormatter,
        UNUM_MAX_FRACTION_DIGITS, AOptions.MaximumFractionDigits);
  end;

  IntlFunctions.UnumSetAttribute(AFormatter,
    UNUM_MIN_INTEGER_DIGITS, AOptions.MinimumIntegerDigits);

  if AOptions.UseGrouping = inugFalse then
    IntlFunctions.UnumSetAttribute(AFormatter, UNUM_GROUPING_USED, 0);

  if AOptions.RoundingIncrement > 1 then
    IntlFunctions.UnumSetAttribute(AFormatter,
      UNUM_ROUNDING_MODE, UNUM_ROUND_HALFUP)
  else
    IntlFunctions.UnumSetAttribute(AFormatter,
      UNUM_ROUNDING_MODE, RoundingModeToICU(AOptions.RoundingMode));
end;

function ApplyRoundingIncrement(AValue: Double;
  const AOptions: TIntlNumberFormatOptions): Double;
var
  Scale, ScaledInt, Remainder: Double;
  Lower, Upper: Double;
  I, Inc: Integer;
  IsNeg: Boolean;
begin
  Result := AValue;
  Inc := AOptions.RoundingIncrement;
  if Inc <= 1 then
    Exit;
  if AOptions.MaximumFractionDigits < 0 then
    Exit;

  Scale := 1.0;
  for I := 1 to AOptions.MaximumFractionDigits do
    Scale := Scale * 10.0;

  IsNeg := AValue < 0;
  ScaledInt := Abs(AValue) * Scale;
  if Abs(ScaledInt - System.Round(ScaledInt)) < 1e-6 then
    ScaledInt := System.Round(ScaledInt);
  Remainder := ScaledInt - Trunc(ScaledInt / Inc) * Inc;
  if Abs(Remainder - Inc) < 1e-9 then
    Remainder := 0;

  if Remainder = 0 then
    Exit;

  Lower := ScaledInt - Remainder;
  Upper := Lower + Inc;

  case AOptions.RoundingMode of
    inrmCeil:
      if IsNeg then ScaledInt := Lower else ScaledInt := Upper;
    inrmFloor:
      if IsNeg then ScaledInt := Upper else ScaledInt := Lower;
    inrmTrunc:
      ScaledInt := Lower;
    inrmExpand:
      ScaledInt := Upper;
    inrmHalfExpand:
      if Remainder * 2 >= Inc then ScaledInt := Upper else ScaledInt := Lower;
    inrmHalfTrunc:
      if Remainder * 2 > Inc then ScaledInt := Upper else ScaledInt := Lower;
    inrmHalfEven:
    begin
      if Remainder * 2 > Inc then
        ScaledInt := Upper
      else if Remainder * 2 < Inc then
        ScaledInt := Lower
      else if Trunc(Lower / Inc) mod 2 = 0 then
        ScaledInt := Lower
      else
        ScaledInt := Upper;
    end;
    inrmHalfCeil:
    begin
      if Remainder * 2 > Inc then
        ScaledInt := Upper
      else if Remainder * 2 < Inc then
        ScaledInt := Lower
      else if IsNeg then
        ScaledInt := Lower
      else
        ScaledInt := Upper;
    end;
    inrmHalfFloor:
    begin
      if Remainder * 2 > Inc then
        ScaledInt := Upper
      else if Remainder * 2 < Inc then
        ScaledInt := Lower
      else if IsNeg then
        ScaledInt := Upper
      else
        ScaledInt := Lower;
    end;
  end;

  if IsNeg then
    Result := -(ScaledInt / Scale)
  else
    Result := ScaledInt / Scale;
end;

function CanonicalizeICUNumberFormatInput(AValue: Double): Double; {$IFDEF FPC}inline;{$ENDIF}
var
  Bits: UInt64;
begin
  if not IsNan(AValue) then
    Exit(AValue);

  // ECMA-402 models NaN as the unsigned abstract value not-a-number.
  // ICU formats a negative NaN payload as "-NaN" on Linux, so normalize
  // the payload before crossing the ICU boundary.
  Bits := UInt64($7FF8000000000000);
  Move(Bits, Result, SizeOf(Result));
end;

procedure AddSkeletonToken(var ASkeleton: string; const AToken: string);
begin
  if AToken = '' then
    Exit;
  if ASkeleton <> '' then
    ASkeleton := ASkeleton + ' ';
  ASkeleton := ASkeleton + AToken;
end;

function BuildFractionPrecisionSkeleton(const AOptions: TIntlNumberFormatOptions): string;
var
  MinFrac, MaxFrac: Integer;
begin
  Result := '';
  if (AOptions.MinimumFractionDigits < 0) and
     (AOptions.MaximumFractionDigits < 0) then
    Exit;

  MinFrac := AOptions.MinimumFractionDigits;
  MaxFrac := AOptions.MaximumFractionDigits;
  if MinFrac < 0 then MinFrac := 0;
  if MaxFrac < MinFrac then MaxFrac := MinFrac;

  if (MinFrac = 0) and (MaxFrac = 0) then
    Result := 'precision-integer'
  else if MaxFrac > 0 then
    Result := '.' + StringOfChar('0', MinFrac) +
      StringOfChar('#', MaxFrac - MinFrac);
end;

function BuildFractionPrecisionStem(const AOptions: TIntlNumberFormatOptions): string;
var
  MinFrac, MaxFrac: Integer;
begin
  MinFrac := AOptions.MinimumFractionDigits;
  MaxFrac := AOptions.MaximumFractionDigits;
  if MinFrac < 0 then MinFrac := 0;
  if MaxFrac < MinFrac then MaxFrac := MinFrac;
  Result := '.' + StringOfChar('0', MinFrac) +
    StringOfChar('#', MaxFrac - MinFrac);
end;

function BuildSignificantPrecisionSkeleton(const AOptions: TIntlNumberFormatOptions): string;
var
  MinSig, MaxSig: Integer;
begin
  Result := '';
  MinSig := AOptions.MinimumSignificantDigits;
  MaxSig := AOptions.MaximumSignificantDigits;
  if MinSig < 1 then MinSig := 1;
  if MaxSig < MinSig then MaxSig := MinSig;
  Result := StringOfChar('@', MinSig) + StringOfChar('#', MaxSig - MinSig);
end;

function RoundingIncrementSkeletonDecimal(ARoundingIncrement,
  AMaximumFractionDigits: Integer): string;
var
  Digits: string;
  IntegerDigits: Integer;
begin
  Digits := IntToStr(ARoundingIncrement);
  if AMaximumFractionDigits <= 0 then
    Exit(Digits);

  if Length(Digits) <= AMaximumFractionDigits then
    Result := '0.' + StringOfChar('0', AMaximumFractionDigits - Length(Digits)) + Digits
  else
  begin
    IntegerDigits := Length(Digits) - AMaximumFractionDigits;
    Result := Copy(Digits, 1, IntegerDigits) + '.' +
      Copy(Digits, IntegerDigits + 1, AMaximumFractionDigits);
  end;
end;

function BuildPrecisionSkeleton(const AOptions: TIntlNumberFormatOptions): string;
begin
  if AOptions.RoundingIncrement > 1 then
    Exit('precision-increment/' +
      RoundingIncrementSkeletonDecimal(AOptions.RoundingIncrement,
        AOptions.MaximumFractionDigits));

  if (AOptions.RoundingPriority <> inrpAuto) and
     (AOptions.MinimumFractionDigits >= 0) and
     (AOptions.MaximumFractionDigits >= 0) and
     (AOptions.MinimumSignificantDigits > 0) and
     (AOptions.MaximumSignificantDigits > 0) then
  begin
    Result := BuildFractionPrecisionStem(AOptions) + '/' +
      BuildSignificantPrecisionSkeleton(AOptions);
    case AOptions.RoundingPriority of
      inrpMorePrecision: Result := Result + 'r';
      inrpLessPrecision: Result := Result + 's';
    end;
    Exit;
  end;

  if (AOptions.MinimumSignificantDigits > 0) or
     (AOptions.MaximumSignificantDigits > 0) then
    Result := BuildSignificantPrecisionSkeleton(AOptions)
  else
    Result := BuildFractionPrecisionSkeleton(AOptions);
end;

function BuildNumberSkeleton(const AOptions: TIntlNumberFormatOptions): string;
begin
  Result := '';

  case AOptions.Style of
    insPercent:
    begin
      AddSkeletonToken(Result, 'percent');
      AddSkeletonToken(Result, 'scale/100');
    end;
    insCurrency:
    begin
      if AOptions.Currency <> '' then
        AddSkeletonToken(Result, 'currency/' + AOptions.Currency);
      case AOptions.CurrencyDisplay of
        incdNarrowSymbol: AddSkeletonToken(Result, 'unit-width-narrow');
        incdCode: AddSkeletonToken(Result, 'unit-width-iso-code');
        incdName: AddSkeletonToken(Result, 'unit-width-full-name');
      end;
    end;
    insUnit:
      if AOptions.UnitIdentifier <> '' then
      begin
        AddSkeletonToken(Result, 'unit/' + AOptions.UnitIdentifier);
        case AOptions.UnitDisplay of
          inudNarrow: AddSkeletonToken(Result, 'unit-width-narrow');
          inudLong: AddSkeletonToken(Result, 'unit-width-full-name');
        end;
      end;
  end;

  if AOptions.NumberingSystem <> '' then
    AddSkeletonToken(Result, 'numbering-system/' + AOptions.NumberingSystem);

  case AOptions.Notation of
    innScientific: AddSkeletonToken(Result, 'scientific');
    innEngineering: AddSkeletonToken(Result, 'engineering');
    innCompact:
      if AOptions.CompactDisplay = incdLong then
        AddSkeletonToken(Result, 'compact-long')
      else
        AddSkeletonToken(Result, 'compact-short');
  end;

  AddSkeletonToken(Result, BuildPrecisionSkeleton(AOptions));

  if AOptions.MinimumIntegerDigits > 1 then
    AddSkeletonToken(Result, 'integer-width/*' +
      StringOfChar('0', AOptions.MinimumIntegerDigits));

  case AOptions.UseGrouping of
    inugFalse: AddSkeletonToken(Result, 'group-off');
    inugMin2: AddSkeletonToken(Result, 'group-min2');
    inugAlways: AddSkeletonToken(Result, 'group-on-aligned');
  end;

  case AOptions.SignDisplay of
    insdNever: AddSkeletonToken(Result, 'sign-never');
    insdAlways: AddSkeletonToken(Result, 'sign-always');
    insdExceptZero: AddSkeletonToken(Result, 'sign-except-zero');
    insdNegative: AddSkeletonToken(Result, 'sign-negative');
  end;

  case AOptions.RoundingMode of
    inrmCeil: AddSkeletonToken(Result, 'rounding-mode-ceiling');
    inrmFloor: AddSkeletonToken(Result, 'rounding-mode-floor');
    inrmExpand: AddSkeletonToken(Result, 'rounding-mode-up');
    inrmTrunc: AddSkeletonToken(Result, 'rounding-mode-down');
    inrmHalfCeil: AddSkeletonToken(Result, 'rounding-mode-half-ceiling');
    inrmHalfFloor: AddSkeletonToken(Result, 'rounding-mode-half-floor');
    inrmHalfExpand: AddSkeletonToken(Result, 'rounding-mode-half-up');
    inrmHalfTrunc: AddSkeletonToken(Result, 'rounding-mode-half-down');
    inrmHalfEven: AddSkeletonToken(Result, 'rounding-mode-half-even');
  end;
end;

function CanUseLegacyNumberFormatter(const AOptions: TIntlNumberFormatOptions): Boolean;
begin
  Result := (AOptions.Notation = innStandard) and
    (AOptions.RoundingPriority = inrpAuto) and
    (AOptions.Style <> insUnit);

  if Result and (AOptions.Style = insCurrency) then
    Result := (AOptions.CurrencyDisplay = incdSymbol) and
      (AOptions.CurrencySign = incsStandard);
end;

function IsEnInLocale(const ALocale: string): Boolean;
begin
  Result := SameText(ALocale, 'en-IN') or
    SameText(Copy(ALocale, 1, 6), 'en-IN-');
end;

function NeedsEnInCompactThousandsFallback(const ALocale: string;
  const AValue: Double; const AOptions: TIntlNumberFormatOptions): Boolean;
const
  EN_IN_COMPACT_THOUSANDS_MIN = 1000.0;
  EN_IN_COMPACT_LAKH_MIN = 100000.0;
var
  Magnitude: Double;
begin
  Magnitude := Abs(AValue);
  Result := IsEnInLocale(ALocale) and (AOptions.Style = insDecimal) and
    (AOptions.Notation = innCompact) and
    (AOptions.CompactDisplay = incdShort) and
    (Magnitude >= EN_IN_COMPACT_THOUSANDS_MIN) and
    (Magnitude < EN_IN_COMPACT_LAKH_MIN);
end;

function TryParseInvariantDouble(const AValue: string; out ANumber: Double): Boolean;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := CreateInvariantFormatSettings;
  Result := TryStrToFloat(AValue, ANumber, FormatSettings);
end;

procedure NormalizeICUNumberFormatResult(const ALocale: string;
  const AValue: Double; const AOptions: TIntlNumberFormatOptions;
  var AFormatted: string);
begin
  if NeedsEnInCompactThousandsFallback(ALocale, AValue, AOptions) and
     (Length(AFormatted) > 0) and (AFormatted[Length(AFormatted)] = 'T') then
    AFormatted[Length(AFormatted)] := 'K';
end;

procedure NormalizeICUNumberFormatParts(const ALocale: string;
  const AValue: Double; const AOptions: TIntlNumberFormatOptions;
  var AParts: TIntlFormatPartArray);
var
  I: Integer;
begin
  if not NeedsEnInCompactThousandsFallback(ALocale, AValue, AOptions) then
    Exit;

  for I := 0 to Length(AParts) - 1 do
    if (AParts[I].PartType = 'compact') and (AParts[I].Value = 'T') then
      AParts[I].Value := 'K';
end;

procedure NormalizeICUNumberRangeFormat(const ALocale: string;
  const AStartValue, AEndValue: Double; const AOptions: TIntlNumberFormatOptions;
  var AFormatted: string; var AParts: TIntlFormatPartArray);
var
  I: Integer;
begin
  if not (NeedsEnInCompactThousandsFallback(ALocale, AStartValue, AOptions) or
          NeedsEnInCompactThousandsFallback(ALocale, AEndValue, AOptions)) then
    Exit;

  AFormatted := StringReplace(AFormatted, 'T', 'K', [rfReplaceAll]);
  for I := 0 to Length(AParts) - 1 do
    if (AParts[I].PartType = 'compact') and (AParts[I].Value = 'T') then
      AParts[I].Value := 'K';
end;

function TryICUFormatNumberSkeleton(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Formatter, FormatResult: Pointer;
  Skeleton: string;
  LocaleBytes: TBytes;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded or
     not Assigned(IntlFunctions.UnumfOpenForSkeletonAndLocale) or
     not Assigned(IntlFunctions.UnumfOpenResult) or
     not Assigned(IntlFunctions.UnumfFormatDouble) or
     not Assigned(IntlFunctions.UnumfResultToString) or
     not Assigned(IntlFunctions.UnumfClose) or
     not Assigned(IntlFunctions.UnumfCloseResult) then
    Exit;

  Skeleton := string(BuildNumberSkeleton(AOptions));
  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumfOpenForSkeletonAndLocale(PWideChar(Skeleton),
    Length(Skeleton), BytePointer(LocaleBytes), Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    Status := ICU_SUCCESS;
    FormatResult := IntlFunctions.UnumfOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(FormatResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      IntlFunctions.UnumfFormatDouble(Formatter, AValue, FormatResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      FillChar(Buffer, SizeOf(Buffer), 0);
      Status := ICU_SUCCESS;
      ResultLen := IntlFunctions.UnumfResultToString(FormatResult,
        @Buffer[0], FORMAT_BUFFER_CAPACITY, Status);
      if not ICUSucceeded(Status) or (ResultLen <= 0) then
        Exit;

      AFormatted := UnicodeToString(Buffer, ResultLen);
      Result := True;
    finally
      IntlFunctions.UnumfCloseResult(FormatResult);
    end;
  finally
    IntlFunctions.UnumfClose(Formatter);
  end;
end;

function TryICUFormatNumberDecimalSkeleton(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Formatter, FormatResult: Pointer;
  Skeleton: string;
  LocaleBytes, ValueBytes: TBytes;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded or
     not Assigned(IntlFunctions.UnumfOpenForSkeletonAndLocale) or
     not Assigned(IntlFunctions.UnumfOpenResult) or
     not Assigned(IntlFunctions.UnumfFormatDecimal) or
     not Assigned(IntlFunctions.UnumfResultToString) or
     not Assigned(IntlFunctions.UnumfClose) or
     not Assigned(IntlFunctions.UnumfCloseResult) then
    Exit;

  Skeleton := string(BuildNumberSkeleton(AOptions));
  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) or
     not TryEncodeASCIIBytes(AValue, ValueBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumfOpenForSkeletonAndLocale(PWideChar(Skeleton),
    Length(Skeleton), BytePointer(LocaleBytes), Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    Status := ICU_SUCCESS;
    FormatResult := IntlFunctions.UnumfOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(FormatResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      IntlFunctions.UnumfFormatDecimal(Formatter, BytePointer(ValueBytes),
        Length(ValueBytes) - 1, FormatResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      FillChar(Buffer, SizeOf(Buffer), 0);
      Status := ICU_SUCCESS;
      ResultLen := IntlFunctions.UnumfResultToString(FormatResult,
        @Buffer[0], FORMAT_BUFFER_CAPACITY, Status);
      if not ICUSucceeded(Status) or (ResultLen <= 0) then
        Exit;

      AFormatted := UnicodeToString(Buffer, ResultLen);
      Result := True;
    finally
      IntlFunctions.UnumfCloseResult(FormatResult);
    end;
  finally
    IntlFunctions.UnumfClose(Formatter);
  end;
end;

function TryICUFormatNumberDirect(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Formatter: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleBytes: TBytes;
  ICUStyle: LongInt;
  FormattedValue: Double;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  ICUStyle := NumberStyleToICU(AOptions.Style);

  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumOpen(ICUStyle, nil, 0,
    BytePointer(LocaleBytes), nil, Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    ConfigureNumberFormatter(Formatter, AOptions);
    FormattedValue := ApplyRoundingIncrement(AValue, AOptions);

    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UnumFormatDouble(Formatter, FormattedValue,
      @Buffer[0], FORMAT_BUFFER_CAPACITY, nil, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    AFormatted := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UnumClose(Formatter);
  end;
end;

function TryICUFormatNumber(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
begin
  AValue := CanonicalizeICUNumberFormatInput(AValue);
  if TryICUFormatNumberSkeleton(ALocale, AValue, AOptions, AFormatted) then
  begin
    NormalizeICUNumberFormatResult(ALocale, AValue, AOptions, AFormatted);
    Exit(True);
  end;
  Result := CanUseLegacyNumberFormatter(AOptions) and
    TryICUFormatNumberDirect(ALocale, AValue, AOptions, AFormatted);
  if Result then
    NormalizeICUNumberFormatResult(ALocale, AValue, AOptions, AFormatted);
end;

function TryICUFormatNumberDecimal(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  NumberValue: Double;
begin
  Result := TryICUFormatNumberDecimalSkeleton(ALocale, AValue, AOptions,
    AFormatted);
  if Result and TryParseInvariantDouble(AValue, NumberValue) then
    NormalizeICUNumberFormatResult(ALocale, NumberValue, AOptions, AFormatted);
end;

function TryICUFormatNumberToPartsSkeleton(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Formatter, FormatResult, FormattedValue: Pointer;
  Skeleton: string;
  LocaleBytes: TBytes;
  Formatted: string;
begin
  Result := False;
  SetLength(AParts, 0);

  if not EnsureLoaded or
     not Assigned(IntlFunctions.UnumfOpenForSkeletonAndLocale) or
     not Assigned(IntlFunctions.UnumfOpenResult) or
     not Assigned(IntlFunctions.UnumfFormatDouble) or
     not Assigned(IntlFunctions.UnumfResultAsValue) or
     not Assigned(IntlFunctions.UnumfClose) or
     not Assigned(IntlFunctions.UnumfCloseResult) then
    Exit;

  Skeleton := string(BuildNumberSkeleton(AOptions));
  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumfOpenForSkeletonAndLocale(PWideChar(Skeleton),
    Length(Skeleton), BytePointer(LocaleBytes), Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    Status := ICU_SUCCESS;
    FormatResult := IntlFunctions.UnumfOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(FormatResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      IntlFunctions.UnumfFormatDouble(Formatter, AValue, FormatResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      FormattedValue := IntlFunctions.UnumfResultAsValue(FormatResult, Status);
      if not ICUSucceeded(Status) or not Assigned(FormattedValue) then
        Exit;

      Result := FormattedValueToParts(FormattedValue, UFIELD_CATEGORY_NUMBER,
        0, NumberFieldToPartType, Formatted, AParts);
    finally
      IntlFunctions.UnumfCloseResult(FormatResult);
    end;
  finally
    IntlFunctions.UnumfClose(Formatter);
  end;
end;

function TryICUFormatNumberDecimalToPartsSkeleton(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Formatter, FormatResult, FormattedValue: Pointer;
  Skeleton: string;
  LocaleBytes, ValueBytes: TBytes;
  Formatted: string;
begin
  Result := False;
  SetLength(AParts, 0);

  if not EnsureLoaded or
     not Assigned(IntlFunctions.UnumfOpenForSkeletonAndLocale) or
     not Assigned(IntlFunctions.UnumfOpenResult) or
     not Assigned(IntlFunctions.UnumfFormatDecimal) or
     not Assigned(IntlFunctions.UnumfResultAsValue) or
     not Assigned(IntlFunctions.UnumfClose) or
     not Assigned(IntlFunctions.UnumfCloseResult) then
    Exit;

  Skeleton := string(BuildNumberSkeleton(AOptions));
  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) or
     not TryEncodeASCIIBytes(AValue, ValueBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumfOpenForSkeletonAndLocale(PWideChar(Skeleton),
    Length(Skeleton), BytePointer(LocaleBytes), Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    Status := ICU_SUCCESS;
    FormatResult := IntlFunctions.UnumfOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(FormatResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      IntlFunctions.UnumfFormatDecimal(Formatter, BytePointer(ValueBytes),
        Length(ValueBytes) - 1, FormatResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      FormattedValue := IntlFunctions.UnumfResultAsValue(FormatResult, Status);
      if not ICUSucceeded(Status) or not Assigned(FormattedValue) then
        Exit;

      Result := FormattedValueToParts(FormattedValue, UFIELD_CATEGORY_NUMBER,
        0, NumberFieldToPartType, Formatted, AParts);
    finally
      IntlFunctions.UnumfCloseResult(FormatResult);
    end;
  finally
    IntlFunctions.UnumfClose(Formatter);
  end;
end;

function TryICUFormatNumberToPartsDirect(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Formatter, Iterator: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleBytes: TBytes;
  ICUStyle: LongInt;
  FormattedValue: Double;
  Spans: TICUFieldSpanArray;
  UFormatted: string;
begin
  Result := False;
  SetLength(AParts, 0);

  if not EnsureLoaded or not Assigned(IntlFunctions.UnumFormatDoubleForFields) or
     not Assigned(IntlFunctions.UfieldpositerOpen) or
     not Assigned(IntlFunctions.UfieldpositerClose) or
     not Assigned(IntlFunctions.UfieldpositerNext) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  ICUStyle := NumberStyleToICU(AOptions.Style);

  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumOpen(ICUStyle, nil, 0,
    BytePointer(LocaleBytes), nil, Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    ConfigureNumberFormatter(Formatter, AOptions);
    FormattedValue := ApplyRoundingIncrement(AValue, AOptions);

    Status := ICU_SUCCESS;
    Iterator := IntlFunctions.UfieldpositerOpen(Status);
    if not ICUSucceeded(Status) or not Assigned(Iterator) then
      Exit;
    try
      FillChar(Buffer, SizeOf(Buffer), 0);
      Status := ICU_SUCCESS;
      ResultLen := IntlFunctions.UnumFormatDoubleForFields(Formatter,
        FormattedValue, @Buffer[0], FORMAT_BUFFER_CAPACITY, Iterator, Status);
      if not ICUSucceeded(Status) or (ResultLen <= 0) then
        Exit;

      UFormatted := string(UnicodeToString(Buffer, ResultLen));
      SetLength(Spans, 0);
      CollectIteratorFieldSpans(Iterator, UFIELD_CATEGORY_NUMBER, Spans);
      Result := BuildPartsFromFieldSpans(UFormatted, Spans,
        UFIELD_CATEGORY_NUMBER, 0, NumberFieldToPartType, AParts);
    finally
      IntlFunctions.UfieldpositerClose(Iterator);
    end;
  finally
    IntlFunctions.UnumClose(Formatter);
  end;
end;

function TryICUFormatNumberToParts(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
begin
  AValue := CanonicalizeICUNumberFormatInput(AValue);
  if TryICUFormatNumberToPartsSkeleton(ALocale, AValue, AOptions, AParts) then
  begin
    NormalizeICUNumberFormatParts(ALocale, AValue, AOptions, AParts);
    Exit(True);
  end;
  Result := CanUseLegacyNumberFormatter(AOptions) and
    TryICUFormatNumberToPartsDirect(ALocale, AValue, AOptions, AParts);
  if Result then
    NormalizeICUNumberFormatParts(ALocale, AValue, AOptions, AParts);
end;

function TryICUFormatNumberDecimalToParts(const ALocale, AValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  NumberValue: Double;
begin
  Result := TryICUFormatNumberDecimalToPartsSkeleton(ALocale, AValue, AOptions,
    AParts);
  if Result and TryParseInvariantDouble(AValue, NumberValue) then
    NormalizeICUNumberFormatParts(ALocale, NumberValue, AOptions, AParts);
end;

function TryICUFormatNumberRangeInternal(const ALocale: string;
  AUseDecimal: Boolean; AStartDouble, AEndDouble: Double;
  const AStartDecimal, AEndDecimal: string; const AOptions: TIntlNumberFormatOptions;
  out AFormatted: string; out AParts: TIntlFormatPartArray;
  AWantParts: Boolean): Boolean;
var
  Status: TICUErrorCode;
  Skeleton: string;
  LocaleBytes, StartBytes, EndBytes: TBytes;
  Formatter, RangeResult, FormattedValue: Pointer;
  StartNumber, EndNumber: Double;
begin
  Result := False;
  AFormatted := '';
  SetLength(AParts, 0);

  if not EnsureLoaded or
     not Assigned(IntlFunctions.UnumrfOpenForSkeletonWithCollapseAndIdentityFallback) or
     not Assigned(IntlFunctions.UnumrfOpenResult) or
     not Assigned(IntlFunctions.UnumrfResultAsValue) or
     not Assigned(IntlFunctions.UnumrfClose) or
     not Assigned(IntlFunctions.UnumrfCloseResult) then
    Exit;
  if AUseDecimal and not Assigned(IntlFunctions.UnumrfFormatDecimalRange) then
    Exit;
  if (not AUseDecimal) and not Assigned(IntlFunctions.UnumrfFormatDoubleRange) then
    Exit;

  Skeleton := string(BuildNumberSkeleton(AOptions));
  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumrfOpenForSkeletonWithCollapseAndIdentityFallback(
    PWideChar(Skeleton), Length(Skeleton), UNUM_RANGE_COLLAPSE_AUTO,
    UNUM_IDENTITY_FALLBACK_APPROXIMATELY, BytePointer(LocaleBytes), nil,
    Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    Status := ICU_SUCCESS;
    RangeResult := IntlFunctions.UnumrfOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(RangeResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      if AUseDecimal then
      begin
        if not TryEncodeASCIIBytes(AStartDecimal, StartBytes) or
           not TryEncodeASCIIBytes(AEndDecimal, EndBytes) then
          Exit;
        IntlFunctions.UnumrfFormatDecimalRange(Formatter,
          BytePointer(StartBytes), Length(StartBytes) - 1,
          BytePointer(EndBytes), Length(EndBytes) - 1,
          RangeResult, Status);
      end
      else
        IntlFunctions.UnumrfFormatDoubleRange(Formatter, AStartDouble,
          AEndDouble, RangeResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      FormattedValue := IntlFunctions.UnumrfResultAsValue(RangeResult, Status);
      if not ICUSucceeded(Status) or not Assigned(FormattedValue) then
        Exit;

      Result := FormattedValueToParts(FormattedValue, UFIELD_CATEGORY_NUMBER,
        UFIELD_CATEGORY_NUMBER_RANGE_SPAN, NumberFieldToPartType,
        AFormatted, AParts);
      if Result then
      begin
        if AUseDecimal then
        begin
          if not TryParseInvariantDouble(AStartDecimal, StartNumber) then
            StartNumber := 0;
          if not TryParseInvariantDouble(AEndDecimal, EndNumber) then
            EndNumber := 0;
        end
        else
        begin
          StartNumber := AStartDouble;
          EndNumber := AEndDouble;
        end;
        NormalizeICUNumberRangeFormat(ALocale, StartNumber, EndNumber,
          AOptions, AFormatted, AParts);
      end;
      if Result and not AWantParts then
        SetLength(AParts, 0);
    finally
      IntlFunctions.UnumrfCloseResult(RangeResult);
    end;
  finally
    IntlFunctions.UnumrfClose(Formatter);
  end;
end;

function TryICUFormatNumberRange(const ALocale: string; AStartValue, AEndValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  Parts: TIntlFormatPartArray;
begin
  Result := TryICUFormatNumberRangeInternal(ALocale, False, AStartValue, AEndValue,
    '', '', AOptions, AFormatted, Parts, False);
end;

function TryICUFormatNumberRangeToParts(const ALocale: string; AStartValue, AEndValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Formatted: string;
begin
  Result := TryICUFormatNumberRangeInternal(ALocale, False, AStartValue, AEndValue,
    '', '', AOptions, Formatted, AParts, True);
end;

function TryICUFormatNumberDecimalRange(const ALocale, AStartValue, AEndValue: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  Parts: TIntlFormatPartArray;
begin
  Result := TryICUFormatNumberRangeInternal(ALocale, True, 0, 0,
    AStartValue, AEndValue, AOptions, AFormatted, Parts, False);
end;

function TryICUFormatNumberDecimalRangeToParts(const ALocale, AStartValue, AEndValue: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Formatted: string;
begin
  Result := TryICUFormatNumberRangeInternal(ALocale, True, 0, 0,
    AStartValue, AEndValue, AOptions, Formatted, AParts, True);
end;

function DateTimeStyleToICU(AStyle: TIntlDateTimeStyle): LongInt;
begin
  case AStyle of
    idtsNone: Result := UDAT_NONE;
    idtsFull: Result := UDAT_FULL;
    idtsLong: Result := UDAT_LONG;
    idtsMedium: Result := UDAT_MEDIUM;
    idtsShort: Result := UDAT_SHORT;
  else
    Result := UDAT_NONE;
  end;
end;

procedure AppendDateSkeletonField(var ASkeleton: string; const AOption,
  ANumericField, ATwoDigitField, AShortField, ALongField, ANarrowField: string);
begin
  if AOption = '' then
    Exit;
  if AOption = '2-digit' then
    ASkeleton := ASkeleton + ATwoDigitField
  else if AOption = 'short' then
    ASkeleton := ASkeleton + AShortField
  else if AOption = 'long' then
    ASkeleton := ASkeleton + ALongField
  else if AOption = 'narrow' then
    ASkeleton := ASkeleton + ANarrowField
  else
    ASkeleton := ASkeleton + ANumericField;
end;

function DateTimeHourSkeletonField(const AHourCycle: string): string;
begin
  if AHourCycle = 'h11' then
    Result := 'K'
  else if AHourCycle = 'h12' then
    Result := 'h'
  else if AHourCycle = 'h23' then
    Result := 'H'
  else if AHourCycle = 'h24' then
    Result := 'k'
  else
    Result := 'j';
end;

function BuildDateTimeSkeleton(const AOptions: TIntlDateTimeFormatOptions): string;
var
  HourField: string;
begin
  Result := '';
  HourField := DateTimeHourSkeletonField(AOptions.HourCycle);
  if (AOptions.DateStyle <> idtsNone) or (AOptions.TimeStyle <> idtsNone) then
  begin
    case AOptions.DateStyle of
      idtsFull: Result := Result + 'yMMMMEEEEd';
      idtsLong: Result := Result + 'yMMMMd';
      idtsMedium: Result := Result + 'yMMMd';
      idtsShort: Result := Result + 'yMd';
    end;
    case AOptions.TimeStyle of
      idtsFull: Result := Result + HourField + 'mszzzz';
      idtsLong: Result := Result + HourField + 'msz';
      idtsMedium: Result := Result + HourField + 'ms';
      idtsShort: Result := Result + HourField + 'm';
    end;
    Exit;
  end;

  AppendDateSkeletonField(Result, AOptions.Weekday, 'E', 'EE', 'E', 'EEEE', 'EEEEE');
  AppendDateSkeletonField(Result, AOptions.Era, 'G', 'GG', 'G', 'GGGG', 'GGGGG');
  AppendDateSkeletonField(Result, AOptions.Year, 'y', 'yy', 'y', 'y', 'y');
  AppendDateSkeletonField(Result, AOptions.Month, 'M', 'MM', 'MMM', 'MMMM', 'MMMMM');
  AppendDateSkeletonField(Result, AOptions.Day, 'd', 'dd', 'd', 'd', 'd');

  if AOptions.Hour = '2-digit' then
    Result := Result + HourField + HourField
  else if AOptions.Hour <> '' then
    Result := Result + HourField;

  AppendDateSkeletonField(Result, AOptions.Minute, 'm', 'mm', 'm', 'm', 'm');
  AppendDateSkeletonField(Result, AOptions.Second, 's', 'ss', 's', 's', 's');
  if AOptions.FractionalSecondDigits > 0 then
    Result := Result + StringOfChar('S', AOptions.FractionalSecondDigits);
  if AOptions.DayPeriod = 'narrow' then
    Result := Result + 'BBBBB'
  else if AOptions.DayPeriod = 'long' then
    Result := Result + 'BBBB'
  else if AOptions.DayPeriod <> '' then
    Result := Result + 'B';

  if AOptions.TimeZoneName = 'long' then
    Result := Result + 'zzzz'
  else if AOptions.TimeZoneName = 'shortOffset' then
    Result := Result + 'O'
  else if AOptions.TimeZoneName = 'longOffset' then
    Result := Result + 'OOOO'
  else if AOptions.TimeZoneName = 'shortGeneric' then
    Result := Result + 'v'
  else if AOptions.TimeZoneName = 'longGeneric' then
    Result := Result + 'vvvv'
  else if AOptions.TimeZoneName <> '' then
    Result := Result + 'z';

  if Result = '' then
    Result := 'yMd';
end;

function DateTimeZeroDigit(const ANumberingSystem: string): string;
var
  CodePoint: Cardinal;
begin
  if ANumberingSystem = 'adlm' then CodePoint := $1E950
  else if ANumberingSystem = 'ahom' then CodePoint := $11730
  else if ANumberingSystem = 'arab' then CodePoint := $660
  else if ANumberingSystem = 'arabext' then CodePoint := $6F0
  else if ANumberingSystem = 'bali' then CodePoint := $1B50
  else if ANumberingSystem = 'beng' then CodePoint := $9E6
  else if ANumberingSystem = 'bhks' then CodePoint := $11C50
  else if ANumberingSystem = 'brah' then CodePoint := $11066
  else if ANumberingSystem = 'cakm' then CodePoint := $11136
  else if ANumberingSystem = 'cham' then CodePoint := $AA50
  else if ANumberingSystem = 'deva' then CodePoint := $966
  else if ANumberingSystem = 'diak' then CodePoint := $11950
  else if ANumberingSystem = 'fullwide' then CodePoint := $FF10
  else if ANumberingSystem = 'gara' then CodePoint := $10D40
  else if ANumberingSystem = 'gong' then CodePoint := $11DA0
  else if ANumberingSystem = 'gonm' then CodePoint := $11D50
  else if ANumberingSystem = 'gujr' then CodePoint := $AE6
  else if ANumberingSystem = 'gukh' then CodePoint := $16130
  else if ANumberingSystem = 'guru' then CodePoint := $A66
  else if ANumberingSystem = 'hanidec' then CodePoint := $3007
  else if ANumberingSystem = 'hmng' then CodePoint := $16B50
  else if ANumberingSystem = 'hmnp' then CodePoint := $1E140
  else if ANumberingSystem = 'java' then CodePoint := $A9D0
  else if ANumberingSystem = 'kali' then CodePoint := $A900
  else if ANumberingSystem = 'kawi' then CodePoint := $11F50
  else if ANumberingSystem = 'khmr' then CodePoint := $17E0
  else if ANumberingSystem = 'knda' then CodePoint := $CE6
  else if ANumberingSystem = 'krai' then CodePoint := $16D70
  else if ANumberingSystem = 'lana' then CodePoint := $1A80
  else if ANumberingSystem = 'lanatham' then CodePoint := $1A90
  else if ANumberingSystem = 'laoo' then CodePoint := $ED0
  else if ANumberingSystem = 'latn' then CodePoint := $30
  else if ANumberingSystem = 'lepc' then CodePoint := $1C40
  else if ANumberingSystem = 'limb' then CodePoint := $1946
  else if ANumberingSystem = 'mathbold' then CodePoint := $1D7CE
  else if ANumberingSystem = 'mathdbl' then CodePoint := $1D7D8
  else if ANumberingSystem = 'mathmono' then CodePoint := $1D7F6
  else if ANumberingSystem = 'mathsanb' then CodePoint := $1D7EC
  else if ANumberingSystem = 'mathsans' then CodePoint := $1D7E2
  else if ANumberingSystem = 'mlym' then CodePoint := $D66
  else if ANumberingSystem = 'modi' then CodePoint := $11650
  else if ANumberingSystem = 'mong' then CodePoint := $1810
  else if ANumberingSystem = 'mroo' then CodePoint := $16A60
  else if ANumberingSystem = 'mtei' then CodePoint := $ABF0
  else if ANumberingSystem = 'mymr' then CodePoint := $1040
  else if ANumberingSystem = 'mymrepka' then CodePoint := $116DA
  else if ANumberingSystem = 'mymrpao' then CodePoint := $116D0
  else if ANumberingSystem = 'mymrshan' then CodePoint := $1090
  else if ANumberingSystem = 'mymrtlng' then CodePoint := $A9F0
  else if ANumberingSystem = 'nagm' then CodePoint := $1E4F0
  else if ANumberingSystem = 'newa' then CodePoint := $11450
  else if ANumberingSystem = 'nkoo' then CodePoint := $7C0
  else if ANumberingSystem = 'olck' then CodePoint := $1C50
  else if ANumberingSystem = 'onao' then CodePoint := $1E5F1
  else if ANumberingSystem = 'orya' then CodePoint := $B66
  else if ANumberingSystem = 'osma' then CodePoint := $104A0
  else if ANumberingSystem = 'outlined' then CodePoint := $1CCF0
  else if ANumberingSystem = 'rohg' then CodePoint := $10D30
  else if ANumberingSystem = 'saur' then CodePoint := $A8D0
  else if ANumberingSystem = 'segment' then CodePoint := $1FBF0
  else if ANumberingSystem = 'shrd' then CodePoint := $111D0
  else if ANumberingSystem = 'sind' then CodePoint := $112F0
  else if ANumberingSystem = 'sinh' then CodePoint := $DE6
  else if ANumberingSystem = 'sora' then CodePoint := $110F0
  else if ANumberingSystem = 'sund' then CodePoint := $1BB0
  else if ANumberingSystem = 'sunu' then CodePoint := $11BF0
  else if ANumberingSystem = 'takr' then CodePoint := $116C0
  else if ANumberingSystem = 'talu' then CodePoint := $19D0
  else if ANumberingSystem = 'tamldec' then CodePoint := $BE6
  else if ANumberingSystem = 'telu' then CodePoint := $C66
  else if ANumberingSystem = 'thai' then CodePoint := $E50
  else if ANumberingSystem = 'tibt' then CodePoint := $F20
  else if ANumberingSystem = 'tirh' then CodePoint := $114D0
  else if ANumberingSystem = 'tnsa' then CodePoint := $16AC0
  else if ANumberingSystem = 'tols' then CodePoint := $11DE0
  else if ANumberingSystem = 'vaii' then CodePoint := $A620
  else if ANumberingSystem = 'wara' then CodePoint := $118E0
  else if ANumberingSystem = 'wcho' then CodePoint := $1E2F0
  else CodePoint := $30;
  Result := TextEncoding.CodePointToUTF16(CodePoint);
end;

function PadTwoDigitHourFormatted(const AFormatted: string;
  const AOptions: TIntlDateTimeFormatOptions): string;
var
  ColonPos: Integer;
  ZeroDigit: string;
begin
  Result := AFormatted;
  if AOptions.Hour <> '2-digit' then
    Exit;

  ColonPos := Pos(':', AFormatted);
  if ColonPos <= 1 then
    Exit;

  ZeroDigit := DateTimeZeroDigit(AOptions.NumberingSystem);
  if ColonPos = Length(ZeroDigit) + 1 then
    Result := ZeroDigit + AFormatted;
  if AOptions.NumberingSystem = 'hanidec' then
    Result := StringReplace(Result, TextEncoding.CodePointToUTF16($202F), ' ',
      [rfReplaceAll]);
end;

procedure PadTwoDigitHourParts(var AParts: TIntlFormatPartArray;
  const AOptions: TIntlDateTimeFormatOptions);
var
  I: Integer;
  ZeroDigit: string;
begin
  if AOptions.Hour <> '2-digit' then
    Exit;

  ZeroDigit := DateTimeZeroDigit(AOptions.NumberingSystem);
  for I := 0 to Length(AParts) - 1 do
  begin
    if (AParts[I].PartType = 'hour') and
       (Length(AParts[I].Value) = Length(ZeroDigit)) then
      AParts[I].Value := ZeroDigit + AParts[I].Value;
    if AOptions.NumberingSystem = 'hanidec' then
      AParts[I].Value := StringReplace(AParts[I].Value,
        TextEncoding.CodePointToUTF16($202F), ' ', [rfReplaceAll]);
  end;
end;

function FindUnicodeExtensionEnd(const ALocaleLower: string; const AUnicodePos: Integer): Integer;
var
  I: Integer;
begin
  Result := Length(ALocaleLower) + 1;
  I := AUnicodePos + 3;
  while I <= Length(ALocaleLower) - 2 do
  begin
    if (ALocaleLower[I] = '-') and (ALocaleLower[I + 2] = '-') and
       (ALocaleLower[I + 1] in ['0'..'9', 'a'..'z']) then
    begin
      Result := I;
      Exit;
    end;
    Inc(I);
  end;
end;

function RemoveUnicodeLocaleKeyword(const ALocale, AKey: string): string;
var
  LocaleLower, Extension, NewExtension, Token, TokenLower, Suffix, KeyLower: string;
  UnicodePos, ExtensionStart, ExtensionEnd, I, TokenStart: Integer;
  Tokens: IntlTypes.TStringArray;

  procedure AddToken(const AToken: string);
  begin
    if AToken = '' then
      Exit;
    SetLength(Tokens, Length(Tokens) + 1);
    Tokens[High(Tokens)] := AToken;
  end;

begin
  LocaleLower := LowerCase(ALocale);
  KeyLower := LowerCase(AKey);
  UnicodePos := Pos('-u-', LocaleLower);
  if UnicodePos = 0 then
    Exit(ALocale);

  ExtensionStart := UnicodePos + 3;
  ExtensionEnd := FindUnicodeExtensionEnd(LocaleLower, UnicodePos);
  Extension := Copy(ALocale, ExtensionStart, ExtensionEnd - ExtensionStart);

  I := 1;
  while I <= Length(Extension) do
  begin
    TokenStart := I;
    while (I <= Length(Extension)) and (Extension[I] <> '-') do
      Inc(I);
    AddToken(Copy(Extension, TokenStart, I - TokenStart));
    while (I <= Length(Extension)) and (Extension[I] = '-') do
      Inc(I);
  end;

  NewExtension := '';
  I := 0;
  while I < Length(Tokens) do
  begin
    Token := Tokens[I];
    TokenLower := LowerCase(Token);
    if TokenLower = KeyLower then
    begin
      Inc(I);
      while (I < Length(Tokens)) and (Length(Tokens[I]) > 2) do
        Inc(I);
      Continue;
    end;

    if NewExtension <> '' then
      NewExtension := NewExtension + '-';
    NewExtension := NewExtension + Token;
    Inc(I);
  end;

  Suffix := Copy(ALocale, ExtensionEnd, Length(ALocale) - ExtensionEnd + 1);
  if NewExtension = '' then
    Result := Copy(ALocale, 1, UnicodePos - 1) + Suffix
  else
    Result := Copy(ALocale, 1, UnicodePos + 2) + NewExtension + Suffix;
end;

function ApplyUnicodeLocaleKeyword(const ALocale, AKey, AValue: string): string;
var
  LocaleLower: string;
  UnicodePos, ExtensionEnd, PrivateUsePos: Integer;
begin
  if AValue = '' then
    Exit(ALocale);

  Result := RemoveUnicodeLocaleKeyword(ALocale, AKey);
  LocaleLower := LowerCase(Result);
  UnicodePos := Pos('-u-', LocaleLower);
  if UnicodePos > 0 then
  begin
    ExtensionEnd := FindUnicodeExtensionEnd(LocaleLower, UnicodePos);
    Result := Copy(Result, 1, ExtensionEnd - 1) + '-' + AKey + '-' +
      AValue + Copy(Result, ExtensionEnd, Length(Result) - ExtensionEnd + 1);
    Exit;
  end;

  PrivateUsePos := Pos('-x-', LocaleLower);
  if PrivateUsePos > 0 then
    Result := Copy(Result, 1, PrivateUsePos - 1) + '-u-' + AKey + '-' +
      AValue + Copy(Result, PrivateUsePos, Length(Result) - PrivateUsePos + 1)
  else
    Result := Result + '-u-' + AKey + '-' + AValue;
end;

function ApplyDateTimeLocaleOptions(const ALocale: string;
  const AOptions: TIntlDateTimeFormatOptions): string;
begin
  Result := ALocale;
  Result := ApplyUnicodeLocaleKeyword(Result, 'ca', AOptions.Calendar);
  Result := ApplyUnicodeLocaleKeyword(Result, 'nu', AOptions.NumberingSystem);
  Result := ApplyUnicodeLocaleKeyword(Result, 'hc', AOptions.HourCycle);
end;

function TryICUGetBestDateTimePattern(const ALocale, ASkeleton: string;
  out APattern: string): Boolean;
var
  Status: TICUErrorCode;
  PatternGenerator: Pointer;
  LocaleBytes: TBytes;
  SkeletonUnicode: string;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
begin
  Result := False;
  APattern := '';

  if not Assigned(IntlFunctions.UdatpgOpen) or
     not Assigned(IntlFunctions.UdatpgClose) or
     not Assigned(IntlFunctions.UdatpgGetBestPattern) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  PatternGenerator := IntlFunctions.UdatpgOpen(BytePointer(LocaleBytes),
    Status);
  if not ICUSucceeded(Status) or not Assigned(PatternGenerator) then
    Exit;
  try
    SkeletonUnicode := string(ASkeleton);
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UdatpgGetBestPattern(PatternGenerator,
      PWideChar(SkeletonUnicode), Length(SkeletonUnicode), @Buffer[0],
      FORMAT_BUFFER_CAPACITY, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) or
       (ResultLen > FORMAT_BUFFER_CAPACITY) then
      Exit;
    APattern := string(UnicodeToString(Buffer, ResultLen));
    Result := APattern <> '';
  finally
    IntlFunctions.UdatpgClose(PatternGenerator);
  end;
end;

function OpenDateFormatter(const ALocale: string;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatter: Pointer): Boolean;
var
  Status: TICUErrorCode;
  EffectiveLocale: string;
  LocaleBytes: TBytes;
  ICUTimeStyle, ICUDateStyle: LongInt;
  Pattern: string;
  TzUnicode: string;
  TzPtr: PUChar;
  TzLen: LongInt;
begin
  Result := False;
  AFormatter := nil;

  EffectiveLocale := ApplyDateTimeLocaleOptions(ALocale, AOptions);
  if not TryEncodeASCIIBytes(EffectiveLocale, LocaleBytes) then
    Exit;

  if AOptions.TimeZone <> '' then
  begin
    TzUnicode := string(AOptions.TimeZone);
    TzPtr := PWideChar(TzUnicode);
    TzLen := Length(TzUnicode);
  end
  else
  begin
    TzPtr := nil;
    TzLen := -1;
  end;

  if (AOptions.TimeStyle <> idtsNone) and
     ((AOptions.HourCycle = 'h23') or (AOptions.HourCycle = 'h24')) and
     TryICUGetBestDateTimePattern(EffectiveLocale,
       BuildDateTimeSkeleton(AOptions), Pattern) then
  begin
    ICUDateStyle := UDAT_PATTERN;
    ICUTimeStyle := UDAT_PATTERN;
  end
  else if (AOptions.DateStyle <> idtsNone) or (AOptions.TimeStyle <> idtsNone) then
  begin
    ICUDateStyle := DateTimeStyleToICU(AOptions.DateStyle);
    ICUTimeStyle := DateTimeStyleToICU(AOptions.TimeStyle);
    Pattern := '';
  end
  else
  begin
    if not TryICUGetBestDateTimePattern(EffectiveLocale,
      BuildDateTimeSkeleton(AOptions), Pattern) then
      Exit;
    ICUDateStyle := UDAT_PATTERN;
    ICUTimeStyle := UDAT_PATTERN;
  end;

  Status := ICU_SUCCESS;
  AFormatter := IntlFunctions.UdatOpen(ICUTimeStyle, ICUDateStyle,
    BytePointer(LocaleBytes), TzPtr, TzLen, PWideChar(Pattern),
    Length(Pattern), Status);
  Result := ICUSucceeded(Status) and Assigned(AFormatter);
end;

function TryICUOpenDateTimeFormatter(const ALocale: string;
  const AOptions: TIntlDateTimeFormatOptions;
  out AFormatter: TIntlDateTimeFormatter): Boolean;
begin
  AFormatter := nil;
  if not EnsureLoaded then
    Exit(False);
  Result := OpenDateFormatter(ALocale, AOptions, AFormatter);
end;

procedure ICUCloseDateTimeFormatter(var AFormatter: TIntlDateTimeFormatter);
begin
  if Assigned(AFormatter) then
  begin
    if Assigned(IntlFunctions.UdatClose) then
      IntlFunctions.UdatClose(AFormatter);
    AFormatter := nil;
  end;
end;

function TryICUFormatDateTimeWithFormatter(
  const AFormatter: TIntlDateTimeFormatter; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
begin
  Result := False;
  AFormatted := '';

  if not Assigned(AFormatter) or not Assigned(IntlFunctions.UdatFormat) then
    Exit;

  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UdatFormat(AFormatter, AMillis,
    @Buffer[0], FORMAT_BUFFER_CAPACITY, nil, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  AFormatted := UnicodeToString(Buffer, ResultLen);
  AFormatted := PadTwoDigitHourFormatted(AFormatted, AOptions);
  Result := True;
end;

function TryICUFormatDateTimeToPartsWithFormatter(
  const AFormatter: TIntlDateTimeFormatter; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions;
  out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Iterator: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  Spans: TICUFieldSpanArray;
  UFormatted: string;
begin
  Result := False;
  SetLength(AParts, 0);

  if not Assigned(AFormatter) or
     not Assigned(IntlFunctions.UdatFormatForFields) or
     not Assigned(IntlFunctions.UfieldpositerOpen) or
     not Assigned(IntlFunctions.UfieldpositerClose) or
     not Assigned(IntlFunctions.UfieldpositerNext) then
    Exit;

  Status := ICU_SUCCESS;
  Iterator := IntlFunctions.UfieldpositerOpen(Status);
  if not ICUSucceeded(Status) or not Assigned(Iterator) then
    Exit;
  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UdatFormatForFields(AFormatter, AMillis,
      @Buffer[0], FORMAT_BUFFER_CAPACITY, Iterator, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    UFormatted := string(UnicodeToString(Buffer, ResultLen));
    SetLength(Spans, 0);
    CollectIteratorFieldSpans(Iterator, UFIELD_CATEGORY_DATE, Spans);
    Result := BuildPartsFromFieldSpans(UFormatted, Spans,
      UFIELD_CATEGORY_DATE, 0, DateFieldToPartType, AParts);
    if Result then
      PadTwoDigitHourParts(AParts, AOptions);
  finally
    IntlFunctions.UfieldpositerClose(Iterator);
  end;
end;

function TryICUFormatDateTime(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
var
  Formatter: TIntlDateTimeFormatter;
begin
  if not TryICUOpenDateTimeFormatter(ALocale, AOptions, Formatter) then
  begin
    AFormatted := '';
    Exit(False);
  end;
  try
    Result := TryICUFormatDateTimeWithFormatter(Formatter, AMillis, AOptions,
      AFormatted);
  finally
    ICUCloseDateTimeFormatter(Formatter);
  end;
end;

function TryICUFormatDateTimeToParts(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Formatter: TIntlDateTimeFormatter;
begin
  if not TryICUOpenDateTimeFormatter(ALocale, AOptions, Formatter) then
  begin
    SetLength(AParts, 0);
    Exit(False);
  end;
  try
    Result := TryICUFormatDateTimeToPartsWithFormatter(Formatter, AMillis,
      AOptions, AParts);
  finally
    ICUCloseDateTimeFormatter(Formatter);
  end;
end;

function OpenDateIntervalFormatter(const ALocale: string;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatter: Pointer): Boolean;
var
  Status: TICUErrorCode;
  EffectiveLocale: string;
  LocaleBytes: TBytes;
  SkeletonUnicode, TzUnicode: string;
  TzPtr: PUChar;
  TzLen: LongInt;
begin
  Result := False;
  AFormatter := nil;

  if not Assigned(IntlFunctions.UdtitvfmtOpen) or
     not Assigned(IntlFunctions.UdtitvfmtClose) then
    Exit;

  if AOptions.TimeZone <> '' then
  begin
    TzUnicode := string(AOptions.TimeZone);
    TzPtr := PWideChar(TzUnicode);
    TzLen := Length(TzUnicode);
  end
  else
  begin
    TzPtr := nil;
    TzLen := -1;
  end;

  EffectiveLocale := ApplyDateTimeLocaleOptions(ALocale, AOptions);
  if not TryEncodeASCIIBytes(EffectiveLocale, LocaleBytes) then
    Exit;
  SkeletonUnicode := string(BuildDateTimeSkeleton(AOptions));
  Status := ICU_SUCCESS;
  AFormatter := IntlFunctions.UdtitvfmtOpen(BytePointer(LocaleBytes),
    PWideChar(SkeletonUnicode), Length(SkeletonUnicode), TzPtr, TzLen, Status);
  Result := ICUSucceeded(Status) and Assigned(AFormatter);
end;

function TryICUFormatDateTimeRange(const ALocale: string; AStartMillis, AEndMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Formatter: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded or not Assigned(IntlFunctions.UdtitvfmtFormat) then
    Exit;

  if not OpenDateIntervalFormatter(ALocale, AOptions, Formatter) then
    Exit;
  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UdtitvfmtFormat(Formatter, AStartMillis,
      AEndMillis, @Buffer[0], FORMAT_BUFFER_CAPACITY, nil, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    AFormatted := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UdtitvfmtClose(Formatter);
  end;
end;

function TryICUFormatDateTimeRangeToParts(const ALocale: string; AStartMillis, AEndMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Formatter, RangeResult, FormattedValue: Pointer;
  Formatted: string;
begin
  Result := False;
  SetLength(AParts, 0);

  if not EnsureLoaded or
     not Assigned(IntlFunctions.UdtitvfmtOpenResult) or
     not Assigned(IntlFunctions.UdtitvfmtFormatToResult) or
     not Assigned(IntlFunctions.UdtitvfmtResultAsValue) or
     not Assigned(IntlFunctions.UdtitvfmtCloseResult) then
    Exit;

  if not OpenDateIntervalFormatter(ALocale, AOptions, Formatter) then
    Exit;
  try
    Status := ICU_SUCCESS;
    RangeResult := IntlFunctions.UdtitvfmtOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(RangeResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      IntlFunctions.UdtitvfmtFormatToResult(Formatter, AStartMillis,
        AEndMillis, RangeResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      FormattedValue := IntlFunctions.UdtitvfmtResultAsValue(RangeResult, Status);
      if not ICUSucceeded(Status) or not Assigned(FormattedValue) then
        Exit;

      Result := FormattedValueToParts(FormattedValue, UFIELD_CATEGORY_DATE,
        UFIELD_CATEGORY_DATE_INTERVAL_SPAN, DateFieldToPartType,
        Formatted, AParts);
    finally
      IntlFunctions.UdtitvfmtCloseResult(RangeResult);
    end;
  finally
    IntlFunctions.UdtitvfmtClose(Formatter);
  end;
end;

function TryICUSelectPlural(const ALocale: string; AValue: Double;
  APluralType: TIntlPluralType; out ACategory: string): Boolean;
var
  Status: TICUErrorCode;
  Rules: Pointer;
  Buffer: array[0..63] of WideChar;
  ResultLen: LongInt;
  LocaleBytes: TBytes;
  ICUType: LongInt;
begin
  Result := False;
  ACategory := 'other';

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UplrulesOpenForType) or
     not Assigned(IntlFunctions.UplrulesClose) or
     not Assigned(IntlFunctions.UplrulesSelect) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  case APluralType of
    iptCardinal: ICUType := UPLURAL_TYPE_CARDINAL;
    iptOrdinal: ICUType := UPLURAL_TYPE_ORDINAL;
  else
    ICUType := UPLURAL_TYPE_CARDINAL;
  end;

  Status := ICU_SUCCESS;
  Rules := IntlFunctions.UplrulesOpenForType(BytePointer(LocaleBytes),
    ICUType, Status);
  if not ICUSucceeded(Status) or not Assigned(Rules) then
    Exit;

  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UplrulesSelect(Rules, AValue,
      @Buffer[0], 64, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    ACategory := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UplrulesClose(Rules);
  end;
end;

function TryICUGetDisplayName(const ALocale, ACode: string;
  ADisplayType: TIntlDisplayNameType; AStyle: TIntlDisplayNameStyle;
  out AName: string): Boolean;
var
  Status: TICUErrorCode;
  Buffer: array[0..DISPLAY_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleBytes, CodeBytes: TBytes;
  DisplayFn: TUlocGetDisplayName;
  CurrencyCode: string;
  CurrencyName: PUChar;
  IsChoiceFormat: ByteBool;
  LocaleId: string;
  CurrencyNameStyle: LongInt;
begin
  Result := False;
  AName := '';

  if not EnsureLoaded then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) or
     not TryEncodeASCIIBytes(ACode, CodeBytes) then
    Exit;
  LocaleId := '';

  if ADisplayType = idntCurrency then
  begin
    case AStyle of
      idnsLong, idnsShort, idnsNarrow: CurrencyNameStyle := UCURR_LONG_NAME;
    else
      CurrencyNameStyle := UCURR_LONG_NAME;
    end;
    CurrencyCode := string(UpperCase(ACode));
    Status := ICU_SUCCESS;
    ResultLen := 0;
    IsChoiceFormat := False;
    CurrencyName := IntlFunctions.UcurrGetName(PWideChar(CurrencyCode),
      BytePointer(LocaleBytes), CurrencyNameStyle, @IsChoiceFormat,
      @ResultLen, Status);
    if not ICUSucceeded(Status) or not Assigned(CurrencyName) or (ResultLen <= 0) then
      Exit;

    AName := UnicodePointerToString(CurrencyName, ResultLen);
    if SameText(AName, string(CurrencyCode)) then
      Exit;
    Result := True;
    Exit;
  end;

  case ADisplayType of
    idntLanguage: DisplayFn := TUlocGetDisplayName(IntlFunctions.UlocGetDisplayLanguage);
    idntRegion:
      begin
        DisplayFn := TUlocGetDisplayName(IntlFunctions.UlocGetDisplayCountry);
        LocaleId := '_' + ACode;
      end;
    idntScript:
      begin
        DisplayFn := TUlocGetDisplayName(IntlFunctions.UlocGetDisplayScript);
        LocaleId := '_' + ACode;
      end;
  else
    DisplayFn := IntlFunctions.UlocGetDisplayName;
  end;

  if LocaleId <> '' then
  begin
    if not TryEncodeASCIIBytes(LocaleId, CodeBytes) then
      Exit;
  end;

  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  ResultLen := DisplayFn(BytePointer(CodeBytes), BytePointer(LocaleBytes),
    @Buffer[0], DISPLAY_BUFFER_CAPACITY, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  AName := UnicodeToString(Buffer, ResultLen);
  Result := True;
end;

function SegmenterGranularityToICU(AGranularity: TIntlSegmenterGranularity): LongInt;
begin
  case AGranularity of
    isgGrapheme: Result := UBRK_CHARACTER;
    isgWord: Result := UBRK_WORD;
    isgSentence: Result := UBRK_SENTENCE;
  else
    Result := UBRK_CHARACTER;
  end;
end;

function TryICUCreateBreakIterator(const ALocale: string;
  AGranularity: TIntlSegmenterGranularity; const AText: string;
  out AIterator: TIntlBreakIterator): Boolean;
var
  Status: TICUErrorCode;
  LocaleBytes: TBytes;
begin
  Result := False;
  AIterator.Handle := nil;
  AIterator.Text := AText;

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UbrkOpen) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;

  AIterator.Handle := IntlFunctions.UbrkOpen(
    SegmenterGranularityToICU(AGranularity),
    BytePointer(LocaleBytes),
    PWideChar(AText), Length(AText), Status);

  Result := ICUSucceeded(Status) and Assigned(AIterator.Handle);
end;

function TryICUBreakIteratorNext(var AIterator: TIntlBreakIterator;
  out APosition: Integer): Boolean;
begin
  Result := False;
  APosition := UBRK_DONE;

  if not Assigned(AIterator.Handle) or not Assigned(IntlFunctions.UbrkNext) then
    Exit;

  APosition := IntlFunctions.UbrkNext(AIterator.Handle);
  Result := APosition <> UBRK_DONE;
end;

function TryICUBreakIteratorGetRuleStatus(var AIterator: TIntlBreakIterator): Integer;
begin
  if Assigned(AIterator.Handle) and Assigned(IntlFunctions.UbrkGetRuleStatus) then
    Result := IntlFunctions.UbrkGetRuleStatus(AIterator.Handle)
  else
    Result := 0;
end;

procedure ICUBreakIteratorClose(var AIterator: TIntlBreakIterator);
begin
  if Assigned(AIterator.Handle) and Assigned(IntlFunctions.UbrkClose) then
    IntlFunctions.UbrkClose(AIterator.Handle);
  AIterator.Handle := nil;
  AIterator.Text := '';
end;

function ListTypeToICU(AType: TIntlListFormatType): LongInt;
begin
  case AType of
    ilftConjunction: Result := ULISTFMT_TYPE_AND;
    ilftDisjunction: Result := ULISTFMT_TYPE_OR;
    ilftUnit: Result := ULISTFMT_TYPE_UNITS;
  else
    Result := ULISTFMT_TYPE_AND;
  end;
end;

function ListStyleToICU(AStyle: TIntlListFormatStyle): LongInt;
begin
  case AStyle of
    ilfsLong: Result := ULISTFMT_WIDTH_WIDE;
    ilfsShort: Result := ULISTFMT_WIDTH_SHORT;
    ilfsNarrow: Result := ULISTFMT_WIDTH_NARROW;
  else
    Result := ULISTFMT_WIDTH_WIDE;
  end;
end;

function RelativeTimeStyleToICU(AStyle: TIntlRelativeTimeStyle): LongInt;
begin
  case AStyle of
    irtsShort: Result := URELDATEFMT_STYLE_SHORT;
    irtsNarrow: Result := URELDATEFMT_STYLE_NARROW;
  else
    Result := URELDATEFMT_STYLE_LONG;
  end;
end;

function TryICUFormatList(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Listfmt: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleBytes: TBytes;
  UStrings: array of string;
  UPtrs: array of PUChar;
  ULens: array of LongInt;
  I: Integer;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UlistfmtOpenForType) or
     not Assigned(IntlFunctions.UlistfmtClose) or
     not Assigned(IntlFunctions.UlistfmtFormat) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Listfmt := IntlFunctions.UlistfmtOpenForType(BytePointer(LocaleBytes),
    ListTypeToICU(AListType), ListStyleToICU(AListStyle), Status);
  if not ICUSucceeded(Status) or not Assigned(Listfmt) then
    Exit;

  try
    SetLength(UStrings, Length(AItems));
    SetLength(UPtrs, Length(AItems));
    SetLength(ULens, Length(AItems));
    for I := 0 to High(AItems) do
    begin
      UStrings[I] := string(AItems[I]);
      UPtrs[I] := PWideChar(UStrings[I]);
      ULens[I] := Length(UStrings[I]);
    end;

    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;

    if Length(AItems) > 0 then
      ResultLen := IntlFunctions.UlistfmtFormat(Listfmt,
        @UPtrs[0], @ULens[0], Length(AItems),
        @Buffer[0], FORMAT_BUFFER_CAPACITY, Status)
    else
      ResultLen := IntlFunctions.UlistfmtFormat(Listfmt,
        nil, nil, 0,
        @Buffer[0], FORMAT_BUFFER_CAPACITY, Status);

    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    AFormatted := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UlistfmtClose(Listfmt);
  end;
end;

function TryICUFormatListToParts(const ALocale: string;
  const AItems: IntlTypes.TStringArray; AListType: TIntlListFormatType;
  AListStyle: TIntlListFormatStyle; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Listfmt, ListResult, FormattedValue: Pointer;
  LocaleBytes: TBytes;
  UStrings: array of string;
  UPtrs: array of PUChar;
  ULens: array of LongInt;
  I: Integer;
  Formatted: string;
begin
  Result := False;
  SetLength(AParts, 0);

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UlistfmtOpenForType) or
     not Assigned(IntlFunctions.UlistfmtClose) or
     not Assigned(IntlFunctions.UlistfmtOpenResult) or
     not Assigned(IntlFunctions.UlistfmtCloseResult) or
     not Assigned(IntlFunctions.UlistfmtFormatStringsToResult) or
     not Assigned(IntlFunctions.UlistfmtResultAsValue) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Listfmt := IntlFunctions.UlistfmtOpenForType(BytePointer(LocaleBytes),
    ListTypeToICU(AListType), ListStyleToICU(AListStyle), Status);
  if not ICUSucceeded(Status) or not Assigned(Listfmt) then
    Exit;

  try
    SetLength(UStrings, Length(AItems));
    SetLength(UPtrs, Length(AItems));
    SetLength(ULens, Length(AItems));
    for I := 0 to High(AItems) do
    begin
      UStrings[I] := string(AItems[I]);
      UPtrs[I] := PWideChar(UStrings[I]);
      ULens[I] := Length(UStrings[I]);
    end;

    Status := ICU_SUCCESS;
    ListResult := IntlFunctions.UlistfmtOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(ListResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      if Length(AItems) > 0 then
        IntlFunctions.UlistfmtFormatStringsToResult(Listfmt,
          @UPtrs[0], @ULens[0], Length(AItems), ListResult, Status)
      else
        IntlFunctions.UlistfmtFormatStringsToResult(Listfmt,
          nil, nil, 0, ListResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      FormattedValue := IntlFunctions.UlistfmtResultAsValue(ListResult, Status);
      if not ICUSucceeded(Status) or not Assigned(FormattedValue) then
        Exit;

      Result := FormattedValueToParts(FormattedValue, UFIELD_CATEGORY_LIST,
        0, ListFieldToPartType, Formatted, AParts);
    finally
      IntlFunctions.UlistfmtCloseResult(ListResult);
    end;
  finally
    IntlFunctions.UlistfmtClose(Listfmt);
  end;
end;

function RelativeTimeUnitToICU(AUnit: TIntlRelativeTimeUnit): LongInt;
begin
  case AUnit of
    irtuSecond: Result := UDAT_RELATIVE_SECONDS;
    irtuMinute: Result := UDAT_RELATIVE_MINUTES;
    irtuHour: Result := UDAT_RELATIVE_HOURS;
    irtuDay: Result := UDAT_RELATIVE_DAYS;
    irtuWeek: Result := UDAT_RELATIVE_WEEKS;
    irtuMonth: Result := UDAT_RELATIVE_MONTHS;
    irtuYear: Result := UDAT_RELATIVE_YEARS;
    irtuQuarter: Result := UDAT_RELATIVE_QUARTERS;
  else
    Result := UDAT_RELATIVE_DAYS;
  end;
end;

function RelativeTimeUnitToPartUnit(AUnit: TIntlRelativeTimeUnit): string;
begin
  case AUnit of
    irtuSecond: Result := 'second';
    irtuMinute: Result := 'minute';
    irtuHour: Result := 'hour';
    irtuDay: Result := 'day';
    irtuWeek: Result := 'week';
    irtuMonth: Result := 'month';
    irtuQuarter: Result := 'quarter';
    irtuYear: Result := 'year';
  else
    Result := 'day';
  end;
end;

function TryICUFormatRelativeTime(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  AStyle: TIntlRelativeTimeStyle; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Reldatefmt: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleBytes: TBytes;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UreldatefmtOpen) or
     not Assigned(IntlFunctions.UreldatefmtClose) or
     not Assigned(IntlFunctions.UreldatefmtFormatNumeric) then
    Exit;

  if (ANumeric = irtnAuto) and not Assigned(IntlFunctions.UreldatefmtFormat) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Reldatefmt := IntlFunctions.UreldatefmtOpen(BytePointer(LocaleBytes),
    nil, RelativeTimeStyleToICU(AStyle), 0, Status);
  if not ICUSucceeded(Status) or not Assigned(Reldatefmt) then
    Exit;

  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    if ANumeric = irtnAuto then
      ResultLen := IntlFunctions.UreldatefmtFormat(Reldatefmt,
        AValue, RelativeTimeUnitToICU(AUnit),
        @Buffer[0], FORMAT_BUFFER_CAPACITY, Status)
    else
      ResultLen := IntlFunctions.UreldatefmtFormatNumeric(Reldatefmt,
        AValue, RelativeTimeUnitToICU(AUnit),
        @Buffer[0], FORMAT_BUFFER_CAPACITY, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    AFormatted := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UreldatefmtClose(Reldatefmt);
  end;
end;

function TryICUFormatRelativeTimeToParts(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  AStyle: TIntlRelativeTimeStyle; out AParts: TIntlFormatPartArray): Boolean;
var
  Status: TICUErrorCode;
  Reldatefmt, RelativeResult, FormattedValue: Pointer;
  LocaleBytes: TBytes;
  Formatted: string;
  Spans: TICUFieldSpanArray;
begin
  Result := False;
  SetLength(AParts, 0);

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UreldatefmtOpen) or
     not Assigned(IntlFunctions.UreldatefmtClose) or
     not Assigned(IntlFunctions.UreldatefmtOpenResult) or
     not Assigned(IntlFunctions.UreldatefmtCloseResult) or
     not Assigned(IntlFunctions.UreldatefmtFormatNumericToResult) or
     not Assigned(IntlFunctions.UreldatefmtResultAsValue) then
    Exit;

  if (ANumeric = irtnAuto) and not Assigned(IntlFunctions.UreldatefmtFormatToResult) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) then
    Exit;
  Status := ICU_SUCCESS;
  Reldatefmt := IntlFunctions.UreldatefmtOpen(BytePointer(LocaleBytes),
    nil, RelativeTimeStyleToICU(AStyle), 0, Status);
  if not ICUSucceeded(Status) or not Assigned(Reldatefmt) then
    Exit;

  try
    Status := ICU_SUCCESS;
    RelativeResult := IntlFunctions.UreldatefmtOpenResult(Status);
    if not ICUSucceeded(Status) or not Assigned(RelativeResult) then
      Exit;
    try
      Status := ICU_SUCCESS;
      if ANumeric = irtnAuto then
        IntlFunctions.UreldatefmtFormatToResult(Reldatefmt,
          AValue, RelativeTimeUnitToICU(AUnit), RelativeResult, Status)
      else
        IntlFunctions.UreldatefmtFormatNumericToResult(Reldatefmt,
          AValue, RelativeTimeUnitToICU(AUnit), RelativeResult, Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      FormattedValue := IntlFunctions.UreldatefmtResultAsValue(RelativeResult, Status);
      if not ICUSucceeded(Status) or not Assigned(FormattedValue) then
        Exit;

      if not FormattedValueToFieldSpans(FormattedValue, Formatted, Spans) then
        Exit;

      Result := BuildRelativeTimePartsFromFieldSpans(string(Formatted),
        Spans, RelativeTimeUnitToPartUnit(AUnit), AParts);
    finally
      IntlFunctions.UreldatefmtCloseResult(RelativeResult);
    end;
  finally
    IntlFunctions.UreldatefmtClose(Reldatefmt);
  end;
end;

function TryICUGetDefaultLocale(out ALocale: string): Boolean;
var
  Status: TICUErrorCode;
  DefaultId: PAnsiChar;
  TagBuf: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  ResultLen: LongInt;
begin
  Result := False;
  ALocale := '';

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UlocGetDefault) then
    Exit;

  DefaultId := IntlFunctions.UlocGetDefault();
  if not Assigned(DefaultId) then
    Exit;
  if DefaultId[0] = #0 then
    Exit;

  FillChar(TagBuf, SizeOf(TagBuf), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UlocToLanguageTag(DefaultId, @TagBuf[0],
    LOCALE_ID_CAPACITY, False, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  ALocale := DecodeASCIIZ(@TagBuf[0]);
  Result := ALocale <> '';
end;

function TryICUNormalize(const AForm, AStr: string; out AResult: string): Boolean;
var
  Getter: TUnorm2GetInstance;
  Normalizer: Pointer;
  Status: TICUErrorCode;
  Source, Normalized: string;
  Buffer: array of WideChar;
  Capacity, ResultLen: LongInt;
begin
  Result := False;
  AResult := AStr;

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.Unorm2Normalize) then
    Exit;

  Getter := nil;
  if AForm = 'NFC' then
    Getter := IntlFunctions.Unorm2GetNFCInstance
  else if AForm = 'NFD' then
    Getter := IntlFunctions.Unorm2GetNFDInstance
  else if AForm = 'NFKC' then
    Getter := IntlFunctions.Unorm2GetNFKCInstance
  else if AForm = 'NFKD' then
    Getter := IntlFunctions.Unorm2GetNFKDInstance;

  if not Assigned(Getter) then
    Exit;

  Status := ICU_SUCCESS;
  Normalizer := Getter(Status);
  if not ICUSucceeded(Status) or not Assigned(Normalizer) then
    Exit;

  Source := string(AStr);
  Capacity := Max(Length(Source) * 4 + 8, 8);
  repeat
    SetLength(Buffer, Capacity);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.Unorm2Normalize(Normalizer,
      PWideChar(Source), Length(Source), @Buffer[0], Capacity, Status);
    if (Status = ICU_BUFFER_OVERFLOW) or (ResultLen > Capacity) then
      Capacity := Max(ResultLen, Capacity * 2)
    else
      Break;
  until False;

  if not ICUSucceeded(Status) or (ResultLen < 0) then
    Exit;

  if ResultLen = 0 then
    AResult := ''
  else
  begin
    SetLength(Normalized, ResultLen);
    Move(Buffer[0], Normalized[1], ResultLen * SizeOf(WideChar));
    AResult := string(Normalized);
  end;
  Result := True;
end;

function TryICUCaseMapUTF8(const ALocale, AStr: string;
  AMap: TUcasemapUtf8ToUpper; out AResult: string): Boolean;
var
  Status: TICUErrorCode;
  CaseMap: TUCaseMap;
  DestBytes, LocaleBytes, SourceBytes: TBytes;
  DestCapacity: LongInt;
  ErrorOffset: Integer;
  ResultLen: LongInt;
begin
  Result := False;
  AResult := AStr;

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UcasemapOpen) or
     not Assigned(IntlFunctions.UcasemapClose) or not Assigned(AMap) then
    Exit;

  if not TryEncodeASCIIBytes(ALocale, LocaleBytes) or
     not TryEncodeUTF8(AStr, SourceBytes, ErrorOffset) then
    Exit;

  Status := ICU_SUCCESS;
  CaseMap := IntlFunctions.UcasemapOpen(BytePointer(LocaleBytes), 0, Status);
  if not ICUSucceeded(Status) or not Assigned(CaseMap) then
    Exit;

  try
    DestCapacity := Max(Length(SourceBytes) * 4 + 8, 8);
    repeat
      SetLength(DestBytes, DestCapacity);
      Status := ICU_SUCCESS;
      ResultLen := AMap(CaseMap, BytePointer(DestBytes), DestCapacity,
        BytePointer(SourceBytes), Length(SourceBytes), Status);
      if (Status = ICU_BUFFER_OVERFLOW) or (ResultLen > DestCapacity) then
        DestCapacity := Max(ResultLen, DestCapacity * 2)
      else
        Break;
    until False;

    if not ICUSucceeded(Status) or (ResultLen < 0) then
      Exit;

    SetLength(DestBytes, ResultLen);
    if not TryDecodeUTF8(DestBytes, AResult, ErrorOffset) then
      Exit;
    Result := True;
  finally
    IntlFunctions.UcasemapClose(CaseMap);
  end;
end;

function TryICUUpperCase(const ALocale, AStr: string; out AResult: string): Boolean;
begin
  if not EnsureLoaded then
  begin
    AResult := AStr;
    Exit(False);
  end;
  Result := TryICUCaseMapUTF8(ALocale, AStr,
    IntlFunctions.UcasemapUtf8ToUpper, AResult);
end;

function TryICULowerCase(const ALocale, AStr: string; out AResult: string): Boolean;
begin
  if not EnsureLoaded then
  begin
    AResult := AStr;
    Exit(False);
  end;
  Result := TryICUCaseMapUTF8(ALocale, AStr,
    IntlFunctions.UcasemapUtf8ToLower, AResult);
end;

initialization
  CriticalSectionInit(IntlInitLock);
  IntlLoadAttempted := False;
  IntlLoadSucceeded := False;
  FillChar(IntlFunctions, SizeOf(IntlFunctions), 0);

finalization
  CriticalSectionDone(IntlInitLock);

{$ENDIF}

end.
