unit IntlICU;

{$I Shared.inc}

interface

uses
  IntlTypes;

function IntlICUAvailable: Boolean;

function TryICUCanonicalizeLocale(const ATag: string; out ACanonical: string): Boolean;
function TryICUGetAvailableLocales(out ALocales: IntlTypes.TStringArray): Boolean;
function TryICUMaximizeLocale(const ATag: string; out AMaximized: string): Boolean;
function TryICUMinimizeLocale(const ATag: string; out AMinimized: string): Boolean;

function TryICUCompareStrings(const ALocale: string; const AStr1, AStr2: UnicodeString;
  ASensitivity: TIntlCollatorSensitivity; AIgnorePunctuation: Boolean;
  out AResult: Integer): Boolean;

function TryICUFormatNumber(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatNumberToParts(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;

function TryICUFormatDateTime(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
function TryICUFormatDateTimeToParts(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;

function TryICUSelectPlural(const ALocale: string; AValue: Double;
  APluralType: TIntlPluralType; out ACategory: string): Boolean;

function TryICUGetDisplayName(const ALocale, ACode: string;
  ADisplayType: TIntlDisplayNameType; AStyle: TIntlDisplayNameStyle;
  out AName: string): Boolean;

function TryICUCreateBreakIterator(const ALocale: string;
  AGranularity: TIntlSegmenterGranularity; const AText: UnicodeString;
  out AIterator: TIntlBreakIterator): Boolean;
function TryICUBreakIteratorNext(var AIterator: TIntlBreakIterator;
  out APosition: Integer): Boolean;
function TryICUBreakIteratorGetRuleStatus(var AIterator: TIntlBreakIterator): Integer;
procedure ICUBreakIteratorClose(var AIterator: TIntlBreakIterator);

function TryICUFormatList(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AFormatted: string): Boolean;

function TryICUFormatRelativeTime(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  out AFormatted: string): Boolean;

function TryICUGetDefaultLocale(out ALocale: string): Boolean;

function TryICUUpperCase(const ALocale, AStr: string; out AResult: string): Boolean;
function TryICULowerCase(const ALocale, AStr: string; out AResult: string): Boolean;

implementation

uses
  SysUtils,

  DynLibs,
  ICU;

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
  UDAT_FULL = 0;
  UDAT_LONG = 1;
  UDAT_MEDIUM = 2;
  UDAT_SHORT = 3;
  UDAT_NONE = -1;
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
  UCOL_STRENGTH = 2;
  UCOL_ALTERNATE_HANDLING = 1;
  UCOL_SHIFTED = 20;
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
  URELDATEFMT_STYLE_LONG = 0;
  URELDATEFMT_STYLE_SHORT = 1;
  URELDATEFMT_STYLE_NARROW = 2;
  UDAT_RELATIVE_SECONDS = 0;
  UDAT_RELATIVE_MINUTES = 1;
  UDAT_RELATIVE_HOURS = 2;
  UDAT_RELATIVE_DAYS = 3;
  UDAT_RELATIVE_WEEKS = 4;
  UDAT_RELATIVE_MONTHS = 5;
  UDAT_RELATIVE_QUARTERS = 6;
  UDAT_RELATIVE_YEARS = 7;

type
  TICUErrorCode = LongInt;
  TICUCollationResult = LongInt;
  PUChar = PWideChar;
  PPUChar = ^PUChar;
  PLongInt = ^LongInt;

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
  TUcolOpen = function(const ALocale: PAnsiChar;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUcolClose = procedure(ACollator: Pointer); cdecl;
  TUcolStrcoll = function(ACollator: Pointer; const ASource: PUChar;
    ASourceLength: LongInt; const ATarget: PUChar;
    ATargetLength: LongInt): TICUCollationResult; cdecl;
  TUcolSetAttribute = procedure(ACollator: Pointer; AAttr: LongInt;
    AValue: LongInt; var AStatus: TICUErrorCode); cdecl;
  TUnumOpen = function(AStyle: LongInt; const APattern: PUChar;
    APatternLength: LongInt; const ALocale: PAnsiChar;
    AParseErr: Pointer; var AStatus: TICUErrorCode): Pointer; cdecl;
  TUnumClose = procedure(AFormat: Pointer); cdecl;
  TUnumFormatDouble = function(AFormat: Pointer; ANumber: Double;
    AResult: PUChar; AResultLength: LongInt; APos: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUnumSetAttribute = procedure(AFormat: Pointer; AAttr: LongInt;
    ANewValue: LongInt); cdecl;
  TUnumSetTextAttribute = procedure(AFormat: Pointer; ATag: LongInt;
    const ANewValue: PUChar; ANewValueLength: LongInt;
    var AStatus: TICUErrorCode); cdecl;
  TUdatOpen = function(ATimeStyle: LongInt; ADateStyle: LongInt;
    const ALocale: PAnsiChar; const ATzId: PUChar; ATzIdLength: LongInt;
    const APattern: PUChar; APatternLength: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUdatClose = procedure(AFormat: Pointer); cdecl;
  TUdatFormat = function(AFormat: Pointer; ADateToFormat: Double;
    AResult: PUChar; AResultLength: LongInt; APosition: Pointer;
    var AStatus: TICUErrorCode): LongInt; cdecl;
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
  TUreldatefmtOpen = function(const ALocale: PAnsiChar;
    ANfToAdopt: Pointer; AWidth: LongInt; ACapitalizationContext: LongInt;
    var AStatus: TICUErrorCode): Pointer; cdecl;
  TUreldatefmtClose = procedure(AReldatefmt: Pointer); cdecl;
  TUreldatefmtFormatNumeric = function(AReldatefmt: Pointer;
    AOffset: Double; ADirection: LongInt;
    AResult: PUChar; AResultCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUCaseMap = Pointer;
  TUcasemapOpen = function(const ALocale: PAnsiChar; AOptions: LongInt;
    var AStatus: TICUErrorCode): TUCaseMap; cdecl;
  TUcasemapClose = procedure(ACsm: TUCaseMap); cdecl;
  TUcasemapUtf8ToUpper = function(ACsm: TUCaseMap; ADest: PAnsiChar;
    ADestCapacity: LongInt; const ASrc: PAnsiChar; ASrcLength: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUcasemapUtf8ToLower = function(ACsm: TUCaseMap; ADest: PAnsiChar;
    ADestCapacity: LongInt; const ASrc: PAnsiChar; ASrcLength: LongInt;
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
    UcolOpen: TUcolOpen;
    UcolClose: TUcolClose;
    UcolStrcoll: TUcolStrcoll;
    UcolSetAttribute: TUcolSetAttribute;
    UnumOpen: TUnumOpen;
    UnumClose: TUnumClose;
    UnumFormatDouble: TUnumFormatDouble;
    UnumSetAttribute: TUnumSetAttribute;
    UnumSetTextAttribute: TUnumSetTextAttribute;
    UdatOpen: TUdatOpen;
    UdatClose: TUdatClose;
    UdatFormat: TUdatFormat;
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
    UreldatefmtOpen: TUreldatefmtOpen;
    UreldatefmtClose: TUreldatefmtClose;
    UreldatefmtFormatNumeric: TUreldatefmtFormatNumeric;
    UcasemapOpen: TUcasemapOpen;
    UcasemapClose: TUcasemapClose;
    UcasemapUtf8ToUpper: TUcasemapUtf8ToUpper;
    UcasemapUtf8ToLower: TUcasemapUtf8ToLower;
  end;

var
  IntlFunctions: TIntlICUFunctions;
  IntlLoadAttempted: Boolean;
  IntlLoadSucceeded: Boolean;
  IntlInitLock: TRTLCriticalSection;

function ICUSucceeded(const AStatus: TICUErrorCode): Boolean;
begin
  Result := AStatus <= ICU_SUCCESS;
end;

function ResolveSymbol(const AHandle: TLibHandle; const AName: string): Pointer;
begin
  Result := GetProcAddress(AHandle, AName);
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
  S := ResolveSymbol(AHandle, 'unum_setTextAttribute');
  if Assigned(S) then F.UnumSetTextAttribute := TUnumSetTextAttribute(S);
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
  S := ResolveSymbol(AHandle, 'ureldatefmt_open');
  if Assigned(S) then F.UreldatefmtOpen := TUreldatefmtOpen(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_close');
  if Assigned(S) then F.UreldatefmtClose := TUreldatefmtClose(S);
  S := ResolveSymbol(AHandle, 'ureldatefmt_formatNumeric');
  if Assigned(S) then F.UreldatefmtFormatNumeric := TUreldatefmtFormatNumeric(S);
  S := ResolveSymbol(AHandle, 'ucasemap_open');
  if Assigned(S) then F.UcasemapOpen := TUcasemapOpen(S);
  S := ResolveSymbol(AHandle, 'ucasemap_close');
  if Assigned(S) then F.UcasemapClose := TUcasemapClose(S);
  S := ResolveSymbol(AHandle, 'ucasemap_utf8ToUpper');
  if Assigned(S) then F.UcasemapUtf8ToUpper := TUcasemapUtf8ToUpper(S);
  S := ResolveSymbol(AHandle, 'ucasemap_utf8ToLower');
  if Assigned(S) then F.UcasemapUtf8ToLower := TUcasemapUtf8ToLower(S);

  IntlFunctions := F;
  Result := True;
end;

function EnsureLoaded: Boolean;
var
  Handle: TLibHandle;
begin
  EnterCriticalSection(IntlInitLock);
  try
    if not IntlLoadAttempted then
    begin
      IntlLoadAttempted := True;
      if TryGetICULibraryHandle(Handle) then
        IntlLoadSucceeded := TryLoadIntlFunctions(Handle);
    end;
    Result := IntlLoadSucceeded;
  finally
    LeaveCriticalSection(IntlInitLock);
  end;
end;

function IntlICUAvailable: Boolean;
begin
  Result := EnsureLoaded;
end;

function UnicodeToString(const ABuf: array of WideChar; ALen: Integer): string;
var
  U: UnicodeString;
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

function TryICUCanonicalizeLocale(const ATag: string; out ACanonical: string): Boolean;
var
  Status: TICUErrorCode;
  LocaleId: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  TagBuf: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  ParsedLen, ResultLen: LongInt;
  TagAnsi: AnsiString;
begin
  Result := False;
  ACanonical := '';

  if not EnsureLoaded then
    Exit;

  TagAnsi := AnsiString(ATag);
  FillChar(LocaleId, SizeOf(LocaleId), 0);
  Status := ICU_SUCCESS;
  ParsedLen := 0;

  IntlFunctions.UlocForLanguageTag(PAnsiChar(TagAnsi), @LocaleId[0],
    LOCALE_ID_CAPACITY, ParsedLen, Status);
  if not ICUSucceeded(Status) then
    Exit;

  FillChar(TagBuf, SizeOf(TagBuf), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UlocToLanguageTag(@LocaleId[0], @TagBuf[0],
    LOCALE_ID_CAPACITY, True, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  ACanonical := string(PAnsiChar(@TagBuf[0]));
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
      ALocales[I] := string(LocalePtr)
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
  TagAnsi: AnsiString;
begin
  Result := False;
  AMaximized := '';

  if not EnsureLoaded then
    Exit;

  TagAnsi := AnsiString(ATag);
  FillChar(LocaleId, SizeOf(LocaleId), 0);
  Status := ICU_SUCCESS;
  ParsedLen := 0;

  IntlFunctions.UlocForLanguageTag(PAnsiChar(TagAnsi), @LocaleId[0],
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

  AMaximized := string(PAnsiChar(@TagBuf[0]));
  Result := True;
end;

function TryICUMinimizeLocale(const ATag: string; out AMinimized: string): Boolean;
var
  Status: TICUErrorCode;
  LocaleId, MinId: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  TagBuf: array[0..LOCALE_ID_CAPACITY - 1] of AnsiChar;
  ParsedLen, ResultLen: LongInt;
  TagAnsi: AnsiString;
begin
  Result := False;
  AMinimized := '';

  if not EnsureLoaded then
    Exit;

  TagAnsi := AnsiString(ATag);
  FillChar(LocaleId, SizeOf(LocaleId), 0);
  Status := ICU_SUCCESS;
  ParsedLen := 0;

  IntlFunctions.UlocForLanguageTag(PAnsiChar(TagAnsi), @LocaleId[0],
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

  AMinimized := string(PAnsiChar(@TagBuf[0]));
  Result := True;
end;

function TryICUCompareStrings(const ALocale: string; const AStr1, AStr2: UnicodeString;
  ASensitivity: TIntlCollatorSensitivity; AIgnorePunctuation: Boolean;
  out AResult: Integer): Boolean;
var
  Status: TICUErrorCode;
  Collator: Pointer;
  CollResult: TICUCollationResult;
  Strength: LongInt;
  LocaleAnsi: AnsiString;
begin
  Result := False;
  AResult := 0;

  if not EnsureLoaded then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  Status := ICU_SUCCESS;
  Collator := IntlFunctions.UcolOpen(PAnsiChar(LocaleAnsi), Status);
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

    if AIgnorePunctuation then
    begin
      Status := ICU_SUCCESS;
      IntlFunctions.UcolSetAttribute(Collator, UCOL_ALTERNATE_HANDLING,
        UCOL_SHIFTED, Status);
    end;

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

function TryICUFormatNumber(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Formatter: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleAnsi: AnsiString;
  ICUStyle: LongInt;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  ICUStyle := NumberStyleToICU(AOptions.Style);

  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UnumOpen(ICUStyle, nil, 0,
    PAnsiChar(LocaleAnsi), nil, Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UnumFormatDouble(Formatter, AValue,
      @Buffer[0], FORMAT_BUFFER_CAPACITY, nil, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    AFormatted := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UnumClose(Formatter);
  end;
end;

function TryICUFormatNumberToParts(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Formatted: string;
begin
  Result := TryICUFormatNumber(ALocale, AValue, AOptions, Formatted);
  if Result then
  begin
    SetLength(AParts, 1);
    AParts[0].PartType := 'literal';
    AParts[0].Value := Formatted;
  end;
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

function TryICUFormatDateTime(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Formatter: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleAnsi: AnsiString;
  ICUTimeStyle, ICUDateStyle: LongInt;
  TzUnicode: UnicodeString;
  TzPtr: PUChar;
  TzLen: LongInt;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded then
    Exit;

  LocaleAnsi := AnsiString(ALocale);

  if (AOptions.DateStyle <> idtsNone) or (AOptions.TimeStyle <> idtsNone) then
  begin
    ICUDateStyle := DateTimeStyleToICU(AOptions.DateStyle);
    ICUTimeStyle := DateTimeStyleToICU(AOptions.TimeStyle);
  end
  else
  begin
    ICUDateStyle := UDAT_MEDIUM;
    ICUTimeStyle := UDAT_MEDIUM;
  end;

  if AOptions.TimeZone <> '' then
  begin
    TzUnicode := UnicodeString(AOptions.TimeZone);
    TzPtr := PWideChar(TzUnicode);
    TzLen := Length(TzUnicode);
  end
  else
  begin
    TzPtr := nil;
    TzLen := -1;
  end;

  Status := ICU_SUCCESS;
  Formatter := IntlFunctions.UdatOpen(ICUTimeStyle, ICUDateStyle,
    PAnsiChar(LocaleAnsi), TzPtr, TzLen, nil, -1, Status);
  if not ICUSucceeded(Status) or not Assigned(Formatter) then
    Exit;

  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UdatFormat(Formatter, AMillis,
      @Buffer[0], FORMAT_BUFFER_CAPACITY, nil, Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    AFormatted := UnicodeToString(Buffer, ResultLen);
    Result := True;
  finally
    IntlFunctions.UdatClose(Formatter);
  end;
end;

function TryICUFormatDateTimeToParts(const ALocale: string; AMillis: Double;
  const AOptions: TIntlDateTimeFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  Formatted: string;
begin
  Result := TryICUFormatDateTime(ALocale, AMillis, AOptions, Formatted);
  if Result then
  begin
    SetLength(AParts, 1);
    AParts[0].PartType := 'literal';
    AParts[0].Value := Formatted;
  end;
end;

function TryICUSelectPlural(const ALocale: string; AValue: Double;
  APluralType: TIntlPluralType; out ACategory: string): Boolean;
var
  Status: TICUErrorCode;
  Rules: Pointer;
  Buffer: array[0..63] of WideChar;
  ResultLen: LongInt;
  LocaleAnsi: AnsiString;
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

  LocaleAnsi := AnsiString(ALocale);
  case APluralType of
    iptCardinal: ICUType := UPLURAL_TYPE_CARDINAL;
    iptOrdinal: ICUType := UPLURAL_TYPE_ORDINAL;
  else
    ICUType := UPLURAL_TYPE_CARDINAL;
  end;

  Status := ICU_SUCCESS;
  Rules := IntlFunctions.UplrulesOpenForType(PAnsiChar(LocaleAnsi), ICUType, Status);
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
  LocaleAnsi, CodeAnsi: AnsiString;
  DisplayFn: TUlocGetDisplayName;
begin
  Result := False;
  AName := '';

  if not EnsureLoaded then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  CodeAnsi := AnsiString(ACode);

  case ADisplayType of
    idntLanguage: DisplayFn := TUlocGetDisplayName(IntlFunctions.UlocGetDisplayLanguage);
    idntRegion: DisplayFn := TUlocGetDisplayName(IntlFunctions.UlocGetDisplayCountry);
    idntScript: DisplayFn := TUlocGetDisplayName(IntlFunctions.UlocGetDisplayScript);
  else
    DisplayFn := IntlFunctions.UlocGetDisplayName;
  end;

  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  ResultLen := DisplayFn(PAnsiChar(CodeAnsi), PAnsiChar(LocaleAnsi),
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
  AGranularity: TIntlSegmenterGranularity; const AText: UnicodeString;
  out AIterator: TIntlBreakIterator): Boolean;
var
  Status: TICUErrorCode;
  LocaleAnsi: AnsiString;
begin
  Result := False;
  AIterator.Handle := nil;
  AIterator.Text := AText;

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UbrkOpen) then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  Status := ICU_SUCCESS;

  AIterator.Handle := IntlFunctions.UbrkOpen(
    SegmenterGranularityToICU(AGranularity),
    PAnsiChar(LocaleAnsi),
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

function TryICUFormatList(const ALocale: string; const AItems: IntlTypes.TStringArray;
  AListType: TIntlListFormatType; AListStyle: TIntlListFormatStyle;
  out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Listfmt: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleAnsi: AnsiString;
  UStrings: array of UnicodeString;
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

  LocaleAnsi := AnsiString(ALocale);
  Status := ICU_SUCCESS;
  Listfmt := IntlFunctions.UlistfmtOpenForType(PAnsiChar(LocaleAnsi),
    ListTypeToICU(AListType), ListStyleToICU(AListStyle), Status);
  if not ICUSucceeded(Status) or not Assigned(Listfmt) then
    Exit;

  try
    SetLength(UStrings, Length(AItems));
    SetLength(UPtrs, Length(AItems));
    SetLength(ULens, Length(AItems));
    for I := 0 to High(AItems) do
    begin
      UStrings[I] := UnicodeString(AItems[I]);
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

function RelativeTimeUnitToICU(AUnit: TIntlRelativeTimeUnit): LongInt;
begin
  case AUnit of
    irtuSecond: Result := UDAT_RELATIVE_SECONDS;
    irtuMinute: Result := UDAT_RELATIVE_MINUTES;
    irtuHour: Result := UDAT_RELATIVE_HOURS;
    irtuDay: Result := UDAT_RELATIVE_DAYS;
    irtuWeek: Result := UDAT_RELATIVE_WEEKS;
    irtuMonth: Result := UDAT_RELATIVE_MONTHS;
    irtuQuarter: Result := UDAT_RELATIVE_QUARTERS;
    irtuYear: Result := UDAT_RELATIVE_YEARS;
  else
    Result := UDAT_RELATIVE_DAYS;
  end;
end;

function TryICUFormatRelativeTime(const ALocale: string; AValue: Double;
  AUnit: TIntlRelativeTimeUnit; ANumeric: TIntlRelativeTimeNumeric;
  out AFormatted: string): Boolean;
var
  Status: TICUErrorCode;
  Reldatefmt: Pointer;
  Buffer: array[0..FORMAT_BUFFER_CAPACITY - 1] of WideChar;
  ResultLen: LongInt;
  LocaleAnsi: AnsiString;
begin
  Result := False;
  AFormatted := '';

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UreldatefmtOpen) or
     not Assigned(IntlFunctions.UreldatefmtClose) or
     not Assigned(IntlFunctions.UreldatefmtFormatNumeric) then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  Status := ICU_SUCCESS;
  Reldatefmt := IntlFunctions.UreldatefmtOpen(PAnsiChar(LocaleAnsi),
    nil, URELDATEFMT_STYLE_LONG, 0, Status);
  if not ICUSucceeded(Status) or not Assigned(Reldatefmt) then
    Exit;

  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Status := ICU_SUCCESS;
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

  DefaultId := IntlFunctions.UlocGetDefault;
  if not Assigned(DefaultId) or (DefaultId[0] = #0) then
    Exit;

  FillChar(TagBuf, SizeOf(TagBuf), 0);
  Status := ICU_SUCCESS;
  ResultLen := IntlFunctions.UlocToLanguageTag(DefaultId, @TagBuf[0],
    LOCALE_ID_CAPACITY, False, Status);
  if not ICUSucceeded(Status) or (ResultLen <= 0) then
    Exit;

  ALocale := string(PAnsiChar(@TagBuf[0]));
  Result := ALocale <> '';
end;

function TryICUUpperCase(const ALocale, AStr: string; out AResult: string): Boolean;
var
  Status: TICUErrorCode;
  CaseMap: TUCaseMap;
  DestBuf: array[0..1023] of AnsiChar;
  ResultLen: LongInt;
  LocaleAnsi, SrcAnsi: AnsiString;
  ResultUtf8: UTF8String;
begin
  Result := False;
  AResult := AStr;

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UcasemapOpen) or
     not Assigned(IntlFunctions.UcasemapClose) or
     not Assigned(IntlFunctions.UcasemapUtf8ToUpper) then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  SrcAnsi := AnsiString(UTF8Encode(UnicodeString(AStr)));

  Status := ICU_SUCCESS;
  CaseMap := IntlFunctions.UcasemapOpen(PAnsiChar(LocaleAnsi), 0, Status);
  if not ICUSucceeded(Status) or not Assigned(CaseMap) then
    Exit;

  try
    FillChar(DestBuf, SizeOf(DestBuf), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UcasemapUtf8ToUpper(CaseMap,
      @DestBuf[0], 1024, PAnsiChar(SrcAnsi), Length(SrcAnsi), Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    SetLength(ResultUtf8, ResultLen);
    if ResultLen > 0 then
      Move(DestBuf[0], ResultUtf8[1], ResultLen);
    AResult := string(UTF8Decode(ResultUtf8));
    Result := True;
  finally
    IntlFunctions.UcasemapClose(CaseMap);
  end;
end;

function TryICULowerCase(const ALocale, AStr: string; out AResult: string): Boolean;
var
  Status: TICUErrorCode;
  CaseMap: TUCaseMap;
  DestBuf: array[0..1023] of AnsiChar;
  ResultLen: LongInt;
  LocaleAnsi, SrcAnsi: AnsiString;
  ResultUtf8: UTF8String;
begin
  Result := False;
  AResult := AStr;

  if not EnsureLoaded then
    Exit;

  if not Assigned(IntlFunctions.UcasemapOpen) or
     not Assigned(IntlFunctions.UcasemapClose) or
     not Assigned(IntlFunctions.UcasemapUtf8ToLower) then
    Exit;

  LocaleAnsi := AnsiString(ALocale);
  SrcAnsi := AnsiString(UTF8Encode(UnicodeString(AStr)));

  Status := ICU_SUCCESS;
  CaseMap := IntlFunctions.UcasemapOpen(PAnsiChar(LocaleAnsi), 0, Status);
  if not ICUSucceeded(Status) or not Assigned(CaseMap) then
    Exit;

  try
    FillChar(DestBuf, SizeOf(DestBuf), 0);
    Status := ICU_SUCCESS;
    ResultLen := IntlFunctions.UcasemapUtf8ToLower(CaseMap,
      @DestBuf[0], 1024, PAnsiChar(SrcAnsi), Length(SrcAnsi), Status);
    if not ICUSucceeded(Status) or (ResultLen <= 0) then
      Exit;

    SetLength(ResultUtf8, ResultLen);
    if ResultLen > 0 then
      Move(DestBuf[0], ResultUtf8[1], ResultLen);
    AResult := string(UTF8Decode(ResultUtf8));
    Result := True;
  finally
    IntlFunctions.UcasemapClose(CaseMap);
  end;
end;

initialization
  InitCriticalSection(IntlInitLock);
  IntlLoadAttempted := False;
  IntlLoadSucceeded := False;
  FillChar(IntlFunctions, SizeOf(IntlFunctions), 0);

finalization
  DoneCriticalSection(IntlInitLock);

end.
