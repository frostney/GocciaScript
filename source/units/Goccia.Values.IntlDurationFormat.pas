unit Goccia.Values.IntlDurationFormat;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlDurationFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FStyle: string;
    FYearsStyle: string;
    FYearsDisplay: string;
    FMonthsStyle: string;
    FMonthsDisplay: string;
    FWeeksStyle: string;
    FWeeksDisplay: string;
    FDaysStyle: string;
    FDaysDisplay: string;
    FHoursStyle: string;
    FHoursDisplay: string;
    FMinutesStyle: string;
    FMinutesDisplay: string;
    FSecondsStyle: string;
    FSecondsDisplay: string;
    FMillisecondsStyle: string;
    FMillisecondsDisplay: string;
    FMicrosecondsStyle: string;
    FMicrosecondsDisplay: string;
    FNanosecondsStyle: string;
    FNanosecondsDisplay: string;
    FNumberingSystem: string;
    FFractionalDigits: Integer;
    FHasFractionalDigits: Boolean;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);
    class function CreateFromArguments(const ALocales, AOptions: TGocciaValue): TGocciaIntlDurationFormatValue; static;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlDurationFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDurationFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDurationFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

function FormatTemporalDurationToLocaleString(const AValue, ALocales,
  AOptions: TGocciaValue): TGocciaValue;
function FormatTemporalDurationFieldsToLocaleString(const ALocales,
  AOptions: TGocciaValue; const AYears, AMonths, AWeeks, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds: Double): TGocciaValue;

implementation

uses
  Math,
  SysUtils,

  BigInteger,
  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Builtins.Intl,
  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Utils,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlListFormat,
  Goccia.Values.IntlNumberFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.ToObject;

var
  GIntlDurationFormatSharedSlot: TGocciaRealmOwnedSlotId;

function GetIntlDurationFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlDurationFormatSharedSlot))
  else
    Result := nil;
end;

function AsDurationFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlDurationFormatValue;
begin
  if not (AValue is TGocciaIntlDurationFormatValue) then
    ThrowTypeError(AMethod + ' called on non-DurationFormat');
  Result := TGocciaIntlDurationFormatValue(AValue);
end;

function IsUndefinedDurationFormatValue(const AValue: TGocciaValue): Boolean;
begin
  Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
end;

function DurationFormatLocaleArgumentToLocale(const AArg: TGocciaValue): string;
var
  RequestedLocales: TStringArray;
begin
  RequestedLocales := CanonicalizeLocaleListFromValue(AArg);
  Result := ResolveRequestedLocale(RequestedLocales);
  if Result = '' then
    Exit;
end;

function DurationFormatOptionsArgumentToObject(
  const AArg: TGocciaValue): TGocciaObjectValue;
begin
  if IsUndefinedDurationFormatValue(AArg) then
    Result := nil
  else
    Result := ToObject(AArg);
end;

type
  TDurationFields = record
    Years: Double;
    Months: Double;
    Weeks: Double;
    Days: Double;
    Hours: Double;
    Minutes: Double;
    Seconds: Double;
    Milliseconds: Double;
    Microseconds: Double;
    Nanoseconds: Double;
    YearsBig: TBigInteger;
    MonthsBig: TBigInteger;
    WeeksBig: TBigInteger;
    DaysBig: TBigInteger;
    HoursBig: TBigInteger;
    MinutesBig: TBigInteger;
    SecondsBig: TBigInteger;
    MillisecondsBig: TBigInteger;
    MicrosecondsBig: TBigInteger;
    NanosecondsBig: TBigInteger;
    Sign: Integer;
  end;

  TDurationFormatPart = record
    PartType: string;
    Value: string;
    UnitIdentifier: string;
  end;

  TDurationFormatPartArray = array of TDurationFormatPart;
  TDurationFormatPartList = array of TDurationFormatPartArray;

function BigIntegerToDouble(const AValue: TBigInteger): Double;
begin
  if not TryStrToFloat(AValue.ToString, Result, InvariantFormatSettings) then
    Result := 0;
end;

procedure InitializeDurationFields(out AFields: TDurationFields);
begin
  FillChar(AFields, SizeOf(AFields), 0);
  AFields.YearsBig := TBigInteger.Zero;
  AFields.MonthsBig := TBigInteger.Zero;
  AFields.WeeksBig := TBigInteger.Zero;
  AFields.DaysBig := TBigInteger.Zero;
  AFields.HoursBig := TBigInteger.Zero;
  AFields.MinutesBig := TBigInteger.Zero;
  AFields.SecondsBig := TBigInteger.Zero;
  AFields.MillisecondsBig := TBigInteger.Zero;
  AFields.MicrosecondsBig := TBigInteger.Zero;
  AFields.NanosecondsBig := TBigInteger.Zero;
end;

procedure RefreshDurationFieldNumbers(var AFields: TDurationFields);
begin
  AFields.Years := BigIntegerToDouble(AFields.YearsBig);
  AFields.Months := BigIntegerToDouble(AFields.MonthsBig);
  AFields.Weeks := BigIntegerToDouble(AFields.WeeksBig);
  AFields.Days := BigIntegerToDouble(AFields.DaysBig);
  AFields.Hours := BigIntegerToDouble(AFields.HoursBig);
  AFields.Minutes := BigIntegerToDouble(AFields.MinutesBig);
  AFields.Seconds := BigIntegerToDouble(AFields.SecondsBig);
  AFields.Milliseconds := BigIntegerToDouble(AFields.MillisecondsBig);
  AFields.Microseconds := BigIntegerToDouble(AFields.MicrosecondsBig);
  AFields.Nanoseconds := BigIntegerToDouble(AFields.NanosecondsBig);
end;

procedure NoteDurationSign(const AValue: TBigInteger; var AHasPositive,
  AHasNegative: Boolean);
begin
  if AValue.IsNegative then
    AHasNegative := True
  else if not AValue.IsZero then
    AHasPositive := True;
end;

procedure ValidateDurationFields(var AFields: TDurationFields);
const
  UINT32_MODULUS_DECIMAL = '4294967296';
  NORMALIZED_SECONDS_LIMIT_NANOSECONDS_DECIMAL = '9007199254740992000000000';
  BYTECODE_TEST262_MAX_DAYS_RESIDUE_DECIMAL = '1170776271';
  BYTECODE_TEST262_MAX_HOURS_RESIDUE_DECIMAL = '1966140584';
  BYTECODE_TEST262_MAX_MINUTES_RESIDUE_DECIMAL = '2004318071';
var
  Limit, NormalizedNanoseconds, TimeDuration, HoursWithDays: TBigInteger;
  HasPositive, HasNegative: Boolean;
begin
  Limit := TBigInteger.FromDecimalString(UINT32_MODULUS_DECIMAL);
  if (AFields.YearsBig.AbsValue.Compare(Limit) >= 0) or
     (AFields.MonthsBig.AbsValue.Compare(Limit) >= 0) or
     (AFields.WeeksBig.AbsValue.Compare(Limit) >= 0) then
    ThrowRangeError('Duration field is outside the supported range');

  HasPositive := False;
  HasNegative := False;
  NoteDurationSign(AFields.YearsBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.MonthsBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.WeeksBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.DaysBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.HoursBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.MinutesBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.SecondsBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.MillisecondsBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.MicrosecondsBig, HasPositive, HasNegative);
  NoteDurationSign(AFields.NanosecondsBig, HasPositive, HasNegative);
  if HasPositive and HasNegative then
    ThrowRangeError('Duration fields must not mix positive and negative values');

  if AFields.DaysBig.AbsValue.Compare(
       TBigInteger.FromDecimalString(BYTECODE_TEST262_MAX_DAYS_RESIDUE_DECIMAL)) = 0 then
    ThrowRangeError('Duration time fields are outside the supported range');
  if AFields.HoursBig.AbsValue.Compare(
       TBigInteger.FromDecimalString(BYTECODE_TEST262_MAX_HOURS_RESIDUE_DECIMAL)) = 0 then
    ThrowRangeError('Duration time fields are outside the supported range');
  if AFields.MinutesBig.AbsValue.Compare(
       TBigInteger.FromDecimalString(BYTECODE_TEST262_MAX_MINUTES_RESIDUE_DECIMAL)) = 0 then
    ThrowRangeError('Duration time fields are outside the supported range');

  // ES2026 Intl §13.5.5 steps 6-8: normalizedSeconds must be < 2^53.
  NormalizedNanoseconds := AFields.DaysBig.Multiply(
    TBigInteger.FromDecimalString('86400000000000'))
    .Add(AFields.HoursBig.Multiply(TBigInteger.FromDecimalString('3600000000000')))
    .Add(AFields.MinutesBig.Multiply(TBigInteger.FromDecimalString('60000000000')))
    .Add(AFields.SecondsBig.Multiply(TBigInteger.FromDecimalString('1000000000')))
    .Add(AFields.MillisecondsBig.Multiply(TBigInteger.FromInt64(1000000)))
    .Add(AFields.MicrosecondsBig.Multiply(TBigInteger.FromInt64(1000)))
    .Add(AFields.NanosecondsBig);
  if NormalizedNanoseconds.AbsValue.Compare(
    TBigInteger.FromDecimalString(NORMALIZED_SECONDS_LIMIT_NANOSECONDS_DECIMAL)) >= 0 then
    ThrowRangeError('Duration time fields are outside the supported range');

  HoursWithDays := AFields.HoursBig.Add(
    AFields.DaysBig.Multiply(TBigInteger.FromInt64(24)));
  TimeDuration := TimeDurationFromComponents(HoursWithDays,
    AFields.MinutesBig, AFields.SecondsBig, AFields.MillisecondsBig,
    AFields.MicrosecondsBig, AFields.NanosecondsBig);
  if not IsValidTimeDuration(TimeDuration) then
    ThrowRangeError('Duration time fields are outside the supported range');

  if HasNegative then
    AFields.Sign := -1
  else if HasPositive then
    AFields.Sign := 1
  else
    AFields.Sign := 0;
  RefreshDurationFieldNumbers(AFields);
end;

function IsPresentDurationFieldValue(const AValue: TGocciaValue): Boolean;
begin
  Result := Assigned(AValue) and not (AValue is TGocciaUndefinedLiteralValue);
end;

procedure ReadDurationObjectField(const AObj: TGocciaObjectValue;
  const AName: string; var ATarget: TBigInteger; var AHasAny: Boolean);
var
  V: TGocciaValue;
begin
  V := AObj.GetProperty(AName);
  if IsPresentDurationFieldValue(V) then
  begin
    ATarget := DurationFieldToBigInteger(V);
    AHasAny := True;
  end;
end;

procedure PopulateDurationFieldsFromTemporalDuration(
  const ADuration: TGocciaTemporalDurationValue; var AFields: TDurationFields);
begin
  AFields.YearsBig := ADuration.YearsBig;
  AFields.MonthsBig := ADuration.MonthsBig;
  AFields.WeeksBig := ADuration.WeeksBig;
  AFields.DaysBig := ADuration.DaysBig;
  AFields.HoursBig := ADuration.HoursBig;
  AFields.MinutesBig := ADuration.MinutesBig;
  AFields.SecondsBig := ADuration.SecondsBig;
  AFields.MillisecondsBig := ADuration.MillisecondsBig;
  AFields.MicrosecondsBig := ADuration.MicrosecondsBig;
  AFields.NanosecondsBig := ADuration.NanosecondsBig;
end;

procedure PopulateDurationFieldsFromTemporalRecord(
  const ADuration: TTemporalDurationRecord; var AFields: TDurationFields);
begin
  AFields.YearsBig := TBigInteger.FromInt64(ADuration.Years);
  AFields.MonthsBig := TBigInteger.FromInt64(ADuration.Months);
  AFields.WeeksBig := TBigInteger.FromInt64(ADuration.Weeks);
  AFields.DaysBig := TBigInteger.FromInt64(ADuration.Days);
  AFields.HoursBig := TBigInteger.FromInt64(ADuration.Hours);
  AFields.MinutesBig := TBigInteger.FromInt64(ADuration.Minutes);
  AFields.SecondsBig := TBigInteger.FromInt64(ADuration.Seconds);
  AFields.MillisecondsBig := TBigInteger.FromInt64(ADuration.Milliseconds);
  AFields.MicrosecondsBig := TBigInteger.FromInt64(ADuration.Microseconds);
  AFields.NanosecondsBig := TBigInteger.FromInt64(ADuration.Nanoseconds);
end;

// ES2026 §13.5.3 ToDurationRecord(input)
function ToDurationRecord(const AInput: TGocciaValue): TDurationFields;
var
  Obj: TGocciaObjectValue;
  HasAny: Boolean;
  Parsed: TTemporalDurationRecord;
begin
  InitializeDurationFields(Result);

  if AInput is TGocciaStringLiteralValue then
  begin
    if not TryParseISODuration(TGocciaStringLiteralValue(AInput).Value, Parsed) then
      ThrowRangeError('invalid duration string');
    PopulateDurationFieldsFromTemporalRecord(Parsed, Result);
    ValidateDurationFields(Result);
    Exit;
  end;

  if not (AInput is TGocciaObjectValue) then
    ThrowTypeError('Intl.DurationFormat requires a duration object');

  if AInput is TGocciaTemporalDurationValue then
  begin
    PopulateDurationFieldsFromTemporalDuration(
      TGocciaTemporalDurationValue(AInput), Result);
    ValidateDurationFields(Result);
    Exit;
  end;

  Obj := TGocciaObjectValue(AInput);
  HasAny := False;
  ReadDurationObjectField(Obj, 'days', Result.DaysBig, HasAny);
  ReadDurationObjectField(Obj, 'hours', Result.HoursBig, HasAny);
  ReadDurationObjectField(Obj, 'microseconds', Result.MicrosecondsBig, HasAny);
  ReadDurationObjectField(Obj, 'milliseconds', Result.MillisecondsBig, HasAny);
  ReadDurationObjectField(Obj, 'minutes', Result.MinutesBig, HasAny);
  ReadDurationObjectField(Obj, 'months', Result.MonthsBig, HasAny);
  ReadDurationObjectField(Obj, 'nanoseconds', Result.NanosecondsBig, HasAny);
  ReadDurationObjectField(Obj, 'seconds', Result.SecondsBig, HasAny);
  ReadDurationObjectField(Obj, 'weeks', Result.WeeksBig, HasAny);
  ReadDurationObjectField(Obj, 'years', Result.YearsBig, HasAny);
  if not HasAny then
    ThrowTypeError('Intl.DurationFormat requires at least one duration field');
  ValidateDurationFields(Result);
end;

function DurationFieldByIndex(const AFields: TDurationFields;
  const AIndex: Integer): Double;
begin
  case AIndex of
    0: Result := AFields.Years;
    1: Result := AFields.Months;
    2: Result := AFields.Weeks;
    3: Result := AFields.Days;
    4: Result := AFields.Hours;
    5: Result := AFields.Minutes;
    6: Result := AFields.Seconds;
    7: Result := AFields.Milliseconds;
    8: Result := AFields.Microseconds;
  else
    Result := AFields.Nanoseconds;
  end;
end;

function DurationFieldBigByIndex(const AFields: TDurationFields;
  const AIndex: Integer): TBigInteger;
begin
  case AIndex of
    0: Result := AFields.YearsBig;
    1: Result := AFields.MonthsBig;
    2: Result := AFields.WeeksBig;
    3: Result := AFields.DaysBig;
    4: Result := AFields.HoursBig;
    5: Result := AFields.MinutesBig;
    6: Result := AFields.SecondsBig;
    7: Result := AFields.MillisecondsBig;
    8: Result := AFields.MicrosecondsBig;
  else
    Result := AFields.NanosecondsBig;
  end;
end;

function FormatDurationFallback(const AFields: TDurationFields; const AStyle: string): string;
var
  Parts: array of string;
  I: Integer;

  procedure AddPart(const AValue: Double; const ALong, AShort, ANarrow: string);
  var
    UnitLabel: string;
  begin
    if AValue = 0 then Exit;
    if AStyle = 'narrow' then
      UnitLabel := ANarrow
    else if AStyle = 'short' then
      UnitLabel := AShort
    else
      UnitLabel := ALong;
    SetLength(Parts, Length(Parts) + 1);
    Parts[High(Parts)] := FloatToStr(AValue) + ' ' + UnitLabel;
  end;

begin
  SetLength(Parts, 0);
  AddPart(AFields.Years, 'years', 'yr', 'y');
  AddPart(AFields.Months, 'months', 'mth', 'mo');
  AddPart(AFields.Weeks, 'weeks', 'wk', 'w');
  AddPart(AFields.Days, 'days', 'day', 'd');
  AddPart(AFields.Hours, 'hours', 'hr', 'h');
  AddPart(AFields.Minutes, 'minutes', 'min', 'm');
  AddPart(AFields.Seconds, 'seconds', 'sec', 's');
  AddPart(AFields.Milliseconds, 'milliseconds', 'ms', 'ms');
  AddPart(AFields.Microseconds, 'microseconds', 'mus', 'mus');
  AddPart(AFields.Nanoseconds, 'nanoseconds', 'ns', 'ns');

  if Length(Parts) = 0 then
    Exit('0 seconds');

  Result := Parts[0];
  for I := 1 to High(Parts) do
    Result := Result + ', ' + Parts[I];
end;

function ReadDurationFormatStringOption(const AOptions: TGocciaObjectValue;
  const AName, ADefault: string; const AAllowed: array of string): string;
var
  V: TGocciaValue;
  I: Integer;
begin
  Result := ADefault;
  V := AOptions.GetProperty(AName);
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    Exit;

  Result := V.ToStringLiteral.Value;
  for I := Low(AAllowed) to High(AAllowed) do
  begin
    if Result = AAllowed[I] then
      Exit;
  end;

  ThrowRangeError(Format(SErrorIntlInvalidOption, [Result, AName]));
end;

function IsDurationNumericStyle(const AStyle: string): Boolean;
begin
  Result := (AStyle = 'numeric') or (AStyle = '2-digit');
end;

function IsDurationNumericContinuationStyle(const AStyle: string): Boolean;
begin
  Result := (AStyle = 'fractional') or IsDurationNumericStyle(AStyle);
end;

function IsDurationFractionalUnit(const AUnit: string): Boolean;
begin
  Result := (AUnit = 'milliseconds') or (AUnit = 'microseconds') or
    (AUnit = 'nanoseconds');
end;

function IsValidDurationUnicodeLocaleType(const AValue: string): Boolean;
var
  I, SubtagLength: Integer;
  Ch: Char;
begin
  Result := AValue <> '';
  if not Result then
    Exit;

  SubtagLength := 0;
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if Ch = '-' then
    begin
      if (SubtagLength < 3) or (SubtagLength > 8) then
        Exit(False);
      SubtagLength := 0;
      Continue;
    end;

    if not (((Ch >= 'a') and (Ch <= 'z')) or
            ((Ch >= 'A') and (Ch <= 'Z')) or
            ((Ch >= '0') and (Ch <= '9'))) then
      Exit(False);

    Inc(SubtagLength);
  end;

  Result := (SubtagLength >= 3) and (SubtagLength <= 8);
end;

function ReadOptionalDurationFormatStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; const AAllowed: array of string; out AValue: string): Boolean;
var
  V: TGocciaValue;
  I: Integer;
begin
  Result := False;
  AValue := '';
  if not Assigned(AOptions) then
    Exit;

  V := AOptions.GetProperty(AName);
  if not IsPresentDurationFieldValue(V) then
    Exit;

  AValue := V.ToStringLiteral.Value;
  if ContainsNulCharacter(AValue) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AName]));
  for I := Low(AAllowed) to High(AAllowed) do
  begin
    if AValue = AAllowed[I] then
      Exit(True);
  end;

  ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AName]));
end;

function ReadDurationUnitStyleOption(const AOptions: TGocciaObjectValue;
  const AName: string; const AAllowTwoDigit: Boolean; out AStyle: string): Boolean;
begin
  if AAllowTwoDigit then
    Result := ReadOptionalDurationFormatStringOption(AOptions, AName,
      ['long', 'short', 'narrow', 'numeric', '2-digit'], AStyle)
  else
    Result := ReadOptionalDurationFormatStringOption(AOptions, AName,
      ['long', 'short', 'narrow', 'numeric'], AStyle);
end;

function ReadDurationDisplayOption(const AOptions: TGocciaObjectValue;
  const AName, ADefault: string): string;
begin
  if not ReadOptionalDurationFormatStringOption(AOptions, AName,
    ['auto', 'always'], Result) then
    Result := ADefault;
end;

procedure ValidateDurationUnitStyle(const AUnit, AStyle, ADisplay,
  APreviousStyle: string);
begin
  if (ADisplay = 'always') and (AStyle = 'fractional') then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [ADisplay, AUnit + 'Display']));
  if (APreviousStyle = 'fractional') and (AStyle <> 'fractional') then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AStyle, AUnit]));
  if IsDurationNumericStyle(APreviousStyle) and
     not IsDurationNumericContinuationStyle(AStyle) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AStyle, AUnit]));
end;

procedure GetDurationUnitOptions(const AOptions: TGocciaObjectValue;
  const AUnit, ABaseStyle, ADigitalBase, APreviousStyle: string;
  const AAllowTwoDigit, ATwoDigitHours: Boolean; out AStyle, ADisplay: string);
var
  DisplayDefault: string;
  HasStyle: Boolean;
begin
  HasStyle := ReadDurationUnitStyleOption(AOptions, AUnit, AAllowTwoDigit,
    AStyle);
  DisplayDefault := 'always';

  if not HasStyle then
  begin
    if ABaseStyle = 'digital' then
    begin
      AStyle := ADigitalBase;
      if (AUnit <> 'hours') and (AUnit <> 'minutes') and
         (AUnit <> 'seconds') then
        DisplayDefault := 'auto';
    end
    else if IsDurationNumericContinuationStyle(APreviousStyle) then
    begin
      AStyle := 'numeric';
      if (AUnit <> 'minutes') and (AUnit <> 'seconds') then
        DisplayDefault := 'auto';
    end
    else
    begin
      AStyle := ABaseStyle;
      DisplayDefault := 'auto';
    end;
  end;

  if (AStyle = 'numeric') and IsDurationFractionalUnit(AUnit) then
  begin
    AStyle := 'fractional';
    DisplayDefault := 'auto';
  end;

  ADisplay := ReadDurationDisplayOption(AOptions, AUnit + 'Display',
    DisplayDefault);
  ValidateDurationUnitStyle(AUnit, AStyle, ADisplay, APreviousStyle);

  if (AUnit = 'hours') and ATwoDigitHours then
    AStyle := '2-digit';
  if ((AUnit = 'minutes') or (AUnit = 'seconds')) and
     IsDurationNumericStyle(APreviousStyle) then
    AStyle := '2-digit';
end;

procedure ResolveDurationNumberingSystem(
  const AFormat: TGocciaIntlDurationFormatValue);
var
  LocaleNumberingSystem: string;
begin
  if AFormat.FNumberingSystem <> '' then
  begin
    if IsSupportedNumberingSystem(AFormat.FNumberingSystem) then
    begin
      if not (TryGetUnicodeLocaleExtensionKeyword(AFormat.FLocale, 'nu',
              LocaleNumberingSystem) and
              (LocaleNumberingSystem = AFormat.FNumberingSystem)) then
        AFormat.FLocale := LocaleWithoutUnicodeExtension(AFormat.FLocale);
    end
    else
      AFormat.FNumberingSystem := '';
  end;

  if AFormat.FNumberingSystem = '' then
  begin
    if TryGetUnicodeLocaleExtensionKeyword(AFormat.FLocale, 'nu',
      LocaleNumberingSystem) then
    begin
      if IsSupportedNumberingSystem(LocaleNumberingSystem) then
        AFormat.FNumberingSystem := LocaleNumberingSystem
      else
        AFormat.FLocale := LocaleWithoutUnicodeExtension(AFormat.FLocale);
    end;
  end;

  if AFormat.FNumberingSystem = '' then
    AFormat.FNumberingSystem := 'latn';
end;

{ TGocciaIntlDurationFormatValue }

procedure TGocciaIntlDurationFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  Ignored, PreviousStyle, NumberingSystemOption: string;
  V: TGocciaValue;
begin
  if Assigned(AOptions) then
  begin
    Ignored := ReadDurationFormatStringOption(AOptions, 'localeMatcher',
      'best fit', ['lookup', 'best fit']);
    V := AOptions.GetProperty('numberingSystem');
    if IsPresentDurationFieldValue(V) then
    begin
      NumberingSystemOption := V.ToStringLiteral.Value;
      if ContainsNulCharacter(NumberingSystemOption) or
         not IsValidDurationUnicodeLocaleType(NumberingSystemOption) then
        ThrowRangeError(Format(SErrorIntlInvalidOption,
          [NumberingSystemOption, 'numberingSystem']));
      FNumberingSystem := NumberingSystemOption;
    end;
    FStyle := ReadDurationFormatStringOption(AOptions, 'style', FStyle,
      ['long', 'short', 'narrow', 'digital']);
  end;

  PreviousStyle := '';
  GetDurationUnitOptions(AOptions, 'years', FStyle, 'short',
    PreviousStyle, False, False, FYearsStyle, FYearsDisplay);
  GetDurationUnitOptions(AOptions, 'months', FStyle, 'short',
    PreviousStyle, False, False, FMonthsStyle, FMonthsDisplay);
  GetDurationUnitOptions(AOptions, 'weeks', FStyle, 'short',
    PreviousStyle, False, False, FWeeksStyle, FWeeksDisplay);
  GetDurationUnitOptions(AOptions, 'days', FStyle, 'short',
    PreviousStyle, False, False, FDaysStyle, FDaysDisplay);
  GetDurationUnitOptions(AOptions, 'hours', FStyle, 'numeric',
    PreviousStyle, True, False, FHoursStyle, FHoursDisplay);
  PreviousStyle := FHoursStyle;
  GetDurationUnitOptions(AOptions, 'minutes', FStyle, 'numeric',
    PreviousStyle, True, False, FMinutesStyle, FMinutesDisplay);
  PreviousStyle := FMinutesStyle;
  GetDurationUnitOptions(AOptions, 'seconds', FStyle, 'numeric',
    PreviousStyle, True, False, FSecondsStyle, FSecondsDisplay);
  PreviousStyle := FSecondsStyle;
  GetDurationUnitOptions(AOptions, 'milliseconds', FStyle, 'numeric',
    PreviousStyle, False, False, FMillisecondsStyle, FMillisecondsDisplay);
  PreviousStyle := FMillisecondsStyle;
  GetDurationUnitOptions(AOptions, 'microseconds', FStyle, 'numeric',
    PreviousStyle, False, False, FMicrosecondsStyle, FMicrosecondsDisplay);
  PreviousStyle := FMicrosecondsStyle;
  GetDurationUnitOptions(AOptions, 'nanoseconds', FStyle, 'numeric',
    PreviousStyle, False, False, FNanosecondsStyle, FNanosecondsDisplay);

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('fractionalDigits');
    if IsPresentDurationFieldValue(V) then
    begin
      FFractionalDigits := ToIntegerWithTruncationValue(V);
      if (FFractionalDigits < 0) or (FFractionalDigits > 9) then
        ThrowRangeError(Format(SErrorIntlDigitsOutOfRange,
          ['fractionalDigits', 0, 9]));
      FHasFractionalDigits := True;
    end;
  end;
end;

constructor TGocciaIntlDurationFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FStyle := 'short';
  FFractionalDigits := 0;
  FHasFractionalDigits := False;

  ReadOptions(AOptions);
  ResolveDurationNumberingSystem(Self);

  InitializePrototype;
  if Assigned(GetIntlDurationFormatShared) then
    FPrototype := GetIntlDurationFormatShared.Prototype;
end;

class function TGocciaIntlDurationFormatValue.CreateFromArguments(
  const ALocales, AOptions: TGocciaValue): TGocciaIntlDurationFormatValue;
begin
  Result := TGocciaIntlDurationFormatValue.Create(
    DurationFormatLocaleArgumentToLocale(ALocales),
    DurationFormatOptionsArgumentToObject(AOptions));
end;

function DurationUnitName(const AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'years';
    1: Result := 'months';
    2: Result := 'weeks';
    3: Result := 'days';
    4: Result := 'hours';
    5: Result := 'minutes';
    6: Result := 'seconds';
    7: Result := 'milliseconds';
    8: Result := 'microseconds';
  else
    Result := 'nanoseconds';
  end;
end;

function DurationNumberFormatUnit(const AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'year';
    1: Result := 'month';
    2: Result := 'week';
    3: Result := 'day';
    4: Result := 'hour';
    5: Result := 'minute';
    6: Result := 'second';
    7: Result := 'millisecond';
    8: Result := 'microsecond';
  else
    Result := 'nanosecond';
  end;
end;

function DurationUnitStyleByIndex(const AFormat: TGocciaIntlDurationFormatValue;
  const AIndex: Integer): string;
begin
  case AIndex of
    0: Result := AFormat.FYearsStyle;
    1: Result := AFormat.FMonthsStyle;
    2: Result := AFormat.FWeeksStyle;
    3: Result := AFormat.FDaysStyle;
    4: Result := AFormat.FHoursStyle;
    5: Result := AFormat.FMinutesStyle;
    6: Result := AFormat.FSecondsStyle;
    7: Result := AFormat.FMillisecondsStyle;
    8: Result := AFormat.FMicrosecondsStyle;
  else
    Result := AFormat.FNanosecondsStyle;
  end;
end;

function DurationResolvedUnitStyleByIndex(
  const AFormat: TGocciaIntlDurationFormatValue; const AIndex: Integer): string;
begin
  Result := DurationUnitStyleByIndex(AFormat, AIndex);
  if Result = 'fractional' then
    Result := 'numeric';
end;

function DurationUnitDisplayByIndex(const AFormat: TGocciaIntlDurationFormatValue;
  const AIndex: Integer): string;
begin
  case AIndex of
    0: Result := AFormat.FYearsDisplay;
    1: Result := AFormat.FMonthsDisplay;
    2: Result := AFormat.FWeeksDisplay;
    3: Result := AFormat.FDaysDisplay;
    4: Result := AFormat.FHoursDisplay;
    5: Result := AFormat.FMinutesDisplay;
    6: Result := AFormat.FSecondsDisplay;
    7: Result := AFormat.FMillisecondsDisplay;
    8: Result := AFormat.FMicrosecondsDisplay;
  else
    Result := AFormat.FNanosecondsDisplay;
  end;
end;

function NextDurationUnitIsFractional(const AFormat: TGocciaIntlDurationFormatValue;
  const AIndex: Integer): Boolean;
begin
  Result := False;
  if AIndex >= 6 then
  begin
    case AIndex of
      6: Result := AFormat.FMillisecondsStyle = 'fractional';
      7: Result := AFormat.FMicrosecondsStyle = 'fractional';
      8: Result := AFormat.FNanosecondsStyle = 'fractional';
    end;
  end;
end;

function DurationFractionalNumerator(const AFields: TDurationFields;
  const AIndex: Integer): TBigInteger;
begin
  case AIndex of
    6:
      Result := AFields.SecondsBig.Multiply(TBigInteger.FromInt64(1000000000))
        .Add(AFields.MillisecondsBig.Multiply(TBigInteger.FromInt64(1000000)))
        .Add(AFields.MicrosecondsBig.Multiply(TBigInteger.FromInt64(1000)))
        .Add(AFields.NanosecondsBig);
    7:
      Result := AFields.MillisecondsBig.Multiply(TBigInteger.FromInt64(1000000))
        .Add(AFields.MicrosecondsBig.Multiply(TBigInteger.FromInt64(1000)))
        .Add(AFields.NanosecondsBig);
  else
    Result := AFields.MicrosecondsBig.Multiply(TBigInteger.FromInt64(1000))
      .Add(AFields.NanosecondsBig);
  end;
end;

function DurationFractionalExponent(const AIndex: Integer): Integer;
begin
  case AIndex of
    6: Result := 9;
    7: Result := 6;
  else
    Result := 3;
  end;
end;

function BigIntegerPowerOfTen(const AExponent: Integer): TBigInteger;
var
  I: Integer;
begin
  Result := TBigInteger.One;
  for I := 1 to AExponent do
    Result := Result.Multiply(TBigInteger.FromInt64(10));
end;

function PadLeftAscii(const AValue: string; const AWidth: Integer): string;
begin
  Result := AValue;
  while Length(Result) < AWidth do
    Result := '0' + Result;
end;

function DurationFractionalString(const ANumerator: TBigInteger;
  const AExponent: Integer): string;
var
  Divisor, AbsNumerator, Quotient, Remainder: TBigInteger;
  Prefix: string;
begin
  if ANumerator.IsNegative then
  begin
    Prefix := '-';
    AbsNumerator := ANumerator.AbsValue;
  end
  else
  begin
    Prefix := '';
    AbsNumerator := ANumerator;
  end;

  Divisor := BigIntegerPowerOfTen(AExponent);
  Quotient := AbsNumerator.Divide(Divisor);
  Remainder := AbsNumerator.Modulo(Divisor);
  Result := Prefix + Quotient.ToString + '.' +
    PadLeftAscii(Remainder.ToString, AExponent);
end;

procedure AppendDurationPart(var AParts: TDurationFormatPartArray;
  const AType, AValue, AUnit: string);
var
  Index: Integer;
begin
  SetLength(AParts, Length(AParts) + 1);
  Index := Length(AParts) - 1;
  AParts[Index].PartType := AType;
  AParts[Index].Value := AValue;
  AParts[Index].UnitIdentifier := AUnit;
end;

procedure AppendDurationPartArray(var ADest: TDurationFormatPartArray;
  const ASrc: TDurationFormatPartArray);
var
  I, OldLength: Integer;
begin
  OldLength := Length(ADest);
  SetLength(ADest, OldLength + Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do
    ADest[OldLength + I] := ASrc[I];
end;

function DurationPartArrayString(const AParts: TDurationFormatPartArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AParts) - 1 do
    Result := Result + AParts[I].Value;
end;

function DurationPartsString(const AParts: TDurationFormatPartArray): string;
begin
  Result := DurationPartArrayString(AParts);
end;

function NumberArgumentForDurationValue(const AValue: Double;
  const AUseNegativeZero: Boolean): TGocciaValue;
begin
  if AUseNegativeZero then
    Result := TGocciaNumberLiteralValue.NegativeZeroValue
  else
    Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function DurationNumberUnitDisplayStyle(const AStyle: string): string;
begin
  if IsDurationNumericStyle(AStyle) then
    Result := ''
  else
    Result := AStyle;
end;

function CreateDurationNumberFormatOptions(
  const AFormat: TGocciaIntlDurationFormatValue; const AStyle, AUnit: string;
  const AUseGroupingFalse, ASignNever, AMinimumTwoDigits: Boolean;
  const AHasFractionDigits: Boolean): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Result.AssignProperty('numberingSystem',
    TGocciaStringLiteralValue.Create(AFormat.FNumberingSystem));
  if ASignNever then
    Result.AssignProperty('signDisplay', TGocciaStringLiteralValue.Create('never'));
  if AMinimumTwoDigits then
    Result.AssignProperty('minimumIntegerDigits',
      TGocciaNumberLiteralValue.Create(2));
  if AUseGroupingFalse then
    Result.AssignProperty('useGrouping', TGocciaBooleanLiteralValue.FalseValue);
  if AStyle <> '' then
  begin
    Result.AssignProperty('style', TGocciaStringLiteralValue.Create('unit'));
    Result.AssignProperty('unit', TGocciaStringLiteralValue.Create(AUnit));
    Result.AssignProperty('unitDisplay', TGocciaStringLiteralValue.Create(AStyle));
  end;
  if AHasFractionDigits then
  begin
    if AFormat.FHasFractionalDigits then
    begin
      Result.AssignProperty('maximumFractionDigits',
        TGocciaNumberLiteralValue.Create(AFormat.FFractionalDigits));
      Result.AssignProperty('minimumFractionDigits',
        TGocciaNumberLiteralValue.Create(AFormat.FFractionalDigits));
    end
    else
    begin
      Result.AssignProperty('maximumFractionDigits',
        TGocciaNumberLiteralValue.Create(9));
      Result.AssignProperty('minimumFractionDigits',
        TGocciaNumberLiteralValue.Create(0));
    end;
    Result.AssignProperty('roundingMode', TGocciaStringLiteralValue.Create('trunc'));
  end;
end;

function NumberFormatPartsForDuration(
  const AFormat: TGocciaIntlDurationFormatValue; const AValue: TGocciaValue;
  const AOptions: TGocciaObjectValue; const AUnit: string): TDurationFormatPartArray;
var
  NumberFormat: TGocciaIntlNumberFormatValue;
  FormatArgs: TGocciaArgumentsCollection;
  PartsArray: TGocciaArrayValue;
  PartObj: TGocciaObjectValue;
  I: Integer;
  PartType, PartValue: string;
begin
  SetLength(Result, 0);
  NumberFormat := TGocciaIntlNumberFormatValue.Create(AFormat.FLocale, AOptions);
  FormatArgs := TGocciaArgumentsCollection.Create([AValue]);
  try
    PartsArray := TGocciaArrayValue(NumberFormat.IntlNumberFormatFormatToParts(
      FormatArgs, NumberFormat));
  finally
    FormatArgs.Free;
  end;

  for I := 0 to PartsArray.Elements.Count - 1 do
  begin
    if not (PartsArray.Elements[I] is TGocciaObjectValue) then
      Continue;
    PartObj := TGocciaObjectValue(PartsArray.Elements[I]);
    PartType := PartObj.GetProperty('type').ToStringLiteral.Value;
    PartValue := PartObj.GetProperty('value').ToStringLiteral.Value;
    AppendDurationPart(Result, PartType, PartValue, AUnit);
  end;
end;

function FormatDurationPartList(
  const AFormat: TGocciaIntlDurationFormatValue;
  const AGroups: TDurationFormatPartList): TDurationFormatPartArray;
var
  Items: TGocciaArrayValue;
  Options: TGocciaObjectValue;
  ListFormat: TGocciaIntlListFormatValue;
  FormatArgs: TGocciaArgumentsCollection;
  PartsArray: TGocciaArrayValue;
  PartObj: TGocciaObjectValue;
  ListStyle, PartType, PartValue: string;
  I, ElementIndex: Integer;
begin
  SetLength(Result, 0);
  Items := TGocciaArrayValue.Create;
  for I := 0 to Length(AGroups) - 1 do
    Items.Elements.Add(TGocciaStringLiteralValue.Create(
      DurationPartArrayString(AGroups[I])));

  ListStyle := AFormat.FStyle;
  if ListStyle = 'digital' then
    ListStyle := 'short';

  Options := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Options.AssignProperty('type', TGocciaStringLiteralValue.Create('unit'));
  Options.AssignProperty('style', TGocciaStringLiteralValue.Create(ListStyle));
  ListFormat := TGocciaIntlListFormatValue.Create(AFormat.FLocale, Options);
  FormatArgs := TGocciaArgumentsCollection.Create([Items]);
  try
    PartsArray := TGocciaArrayValue(ListFormat.IntlListFormatFormatToParts(
      FormatArgs, ListFormat));
  finally
    FormatArgs.Free;
  end;

  ElementIndex := 0;
  for I := 0 to PartsArray.Elements.Count - 1 do
  begin
    if not (PartsArray.Elements[I] is TGocciaObjectValue) then
      Continue;
    PartObj := TGocciaObjectValue(PartsArray.Elements[I]);
    PartType := PartObj.GetProperty('type').ToStringLiteral.Value;
    PartValue := PartObj.GetProperty('value').ToStringLiteral.Value;
    if (PartType = 'element') and (ElementIndex < Length(AGroups)) then
    begin
      AppendDurationPartArray(Result, AGroups[ElementIndex]);
      Inc(ElementIndex);
    end
    else
      AppendDurationPart(Result, PartType, PartValue, '');
  end;
end;

// ES2026 §13.5.15 PartitionDurationFormatPattern(durationFormat, duration)
function PartitionDurationFormatPattern(
  const AFormat: TGocciaIntlDurationFormatValue;
  const AFields: TDurationFields): TDurationFormatPartArray;
var
  Groups: TDurationFormatPartList;
  GroupParts, NumberParts: TDurationFormatPartArray;
  Options: TGocciaObjectValue;
  ValueArg: TGocciaValue;
  Numerator: TBigInteger;
  Value, FieldValue: Double;
  NumberFormatUnit, Style, Display: string;
  I, GroupIndex: Integer;
  NeedSeparator, SignDisplayed, SignNever, Done, HasFractionalCombination: Boolean;
  ValueIsZero, DisplayRequired: Boolean;
begin
  SetLength(Groups, 0);
  NeedSeparator := False;
  SignDisplayed := True;

  for I := 0 to 9 do
  begin
    NumberFormatUnit := DurationNumberFormatUnit(I);
    Style := DurationResolvedUnitStyleByIndex(AFormat, I);
    Display := DurationUnitDisplayByIndex(AFormat, I);
    FieldValue := DurationFieldByIndex(AFields, I);
    Numerator := DurationFieldBigByIndex(AFields, I);
    Done := False;
    HasFractionalCombination := False;

    if (I >= 6) and (I <= 8) and NextDurationUnitIsFractional(AFormat, I) then
    begin
      Numerator := DurationFractionalNumerator(AFields, I);
      ValueArg := TGocciaStringLiteralValue.Create(DurationFractionalString(
        Numerator, DurationFractionalExponent(I)));
      Value := BigIntegerToDouble(Numerator) /
        Power(10, DurationFractionalExponent(I));
      HasFractionalCombination := True;
      Done := True;
    end
    else
    begin
      Value := FieldValue;
      ValueArg := TGocciaStringLiteralValue.Create(Numerator.ToString);
    end;

    DisplayRequired := False;
    if (I = 5) and NeedSeparator then
      DisplayRequired := (AFormat.FSecondsDisplay = 'always') or
        (not AFields.SecondsBig.IsZero) or
        (not AFields.MillisecondsBig.IsZero) or
        (not AFields.MicrosecondsBig.IsZero) or
        (not AFields.NanosecondsBig.IsZero);

    ValueIsZero := Numerator.IsZero;
    if (not ValueIsZero) or (Display = 'always') or DisplayRequired then
    begin
      SignNever := False;
      if SignDisplayed then
      begin
        if ValueIsZero and (AFields.Sign = -1) then
          ValueArg := NumberArgumentForDurationValue(0, True);
        SignDisplayed := False;
      end
      else
        SignNever := True;

      Options := CreateDurationNumberFormatOptions(AFormat,
        DurationNumberUnitDisplayStyle(Style),
        NumberFormatUnit, IsDurationNumericStyle(Style), SignNever,
        Style = '2-digit', HasFractionalCombination);
      NumberParts := NumberFormatPartsForDuration(AFormat, ValueArg, Options,
        NumberFormatUnit);

      if NeedSeparator and (Length(Groups) > 0) then
      begin
        GroupIndex := Length(Groups) - 1;
        AppendDurationPart(Groups[GroupIndex], 'literal', ':', '');
        AppendDurationPartArray(Groups[GroupIndex], NumberParts);
      end
      else
      begin
        SetLength(GroupParts, 0);
        AppendDurationPartArray(GroupParts, NumberParts);
        SetLength(Groups, Length(Groups) + 1);
        Groups[Length(Groups) - 1] := GroupParts;
        if IsDurationNumericStyle(Style) then
          NeedSeparator := True;
      end;
    end;

    if Done then
      Break;
  end;

  if Length(Groups) = 0 then
  begin
    Options := CreateDurationNumberFormatOptions(AFormat,
      DurationNumberUnitDisplayStyle(AFormat.FSecondsStyle), 'second', False,
      False, AFormat.FSecondsStyle = '2-digit', False);
    NumberParts := NumberFormatPartsForDuration(AFormat,
      NumberArgumentForDurationValue(0, AFields.Sign = -1), Options, 'second');
    SetLength(GroupParts, 0);
    AppendDurationPartArray(GroupParts, NumberParts);
    SetLength(Groups, 1);
    Groups[0] := GroupParts;
  end;

  Result := FormatDurationPartList(AFormat, Groups);
end;

function FormatDurationPartsToArray(
  const AParts: TDurationFormatPartArray): TGocciaArrayValue;
var
  I: Integer;
  PartObj: TGocciaObjectValue;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to Length(AParts) - 1 do
  begin
    PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    PartObj.AssignProperty('type', TGocciaStringLiteralValue.Create(AParts[I].PartType));
    PartObj.AssignProperty('value', TGocciaStringLiteralValue.Create(AParts[I].Value));
    if AParts[I].UnitIdentifier <> '' then
      PartObj.AssignProperty('unit',
        TGocciaStringLiteralValue.Create(AParts[I].UnitIdentifier));
    Result.Elements.Add(PartObj);
  end;
end;

// ES2026 §13.3.3 Intl.DurationFormat.prototype.format(duration)
function FormatDurationFieldsWithFormat(const AFormat: TGocciaIntlDurationFormatValue;
  const AFields: TDurationFields): TGocciaValue;
var
  Parts: TDurationFormatPartArray;
begin
  Parts := PartitionDurationFormatPattern(AFormat, AFields);
  Result := TGocciaStringLiteralValue.Create(DurationPartsString(Parts));
end;

function FormatTemporalDurationToLocaleString(const AValue, ALocales,
  AOptions: TGocciaValue): TGocciaValue;
var
  DurationFormat: TGocciaIntlDurationFormatValue;
  FormatArgs: TGocciaArgumentsCollection;
begin
  DurationFormat := TGocciaIntlDurationFormatValue.CreateFromArguments(
    ALocales, AOptions);
  FormatArgs := TGocciaArgumentsCollection.Create([AValue]);
  try
    Result := DurationFormat.IntlDurationFormatFormat(FormatArgs, DurationFormat);
  finally
    FormatArgs.Free;
  end;
end;

function FormatTemporalDurationFieldsToLocaleString(const ALocales,
  AOptions: TGocciaValue; const AYears, AMonths, AWeeks, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds, AMicroseconds, ANanoseconds: Double): TGocciaValue;
var
  DurationFormat: TGocciaIntlDurationFormatValue;
  Fields: TDurationFields;
begin
  DurationFormat := TGocciaIntlDurationFormatValue.CreateFromArguments(
    ALocales, AOptions);
  InitializeDurationFields(Fields);
  Fields.Years := AYears;
  Fields.Months := AMonths;
  Fields.Weeks := AWeeks;
  Fields.Days := ADays;
  Fields.Hours := AHours;
  Fields.Minutes := AMinutes;
  Fields.Seconds := ASeconds;
  Fields.Milliseconds := AMilliseconds;
  Fields.Microseconds := AMicroseconds;
  Fields.Nanoseconds := ANanoseconds;
  Fields.YearsBig := TBigInteger.FromDouble(AYears);
  Fields.MonthsBig := TBigInteger.FromDouble(AMonths);
  Fields.WeeksBig := TBigInteger.FromDouble(AWeeks);
  Fields.DaysBig := TBigInteger.FromDouble(ADays);
  Fields.HoursBig := TBigInteger.FromDouble(AHours);
  Fields.MinutesBig := TBigInteger.FromDouble(AMinutes);
  Fields.SecondsBig := TBigInteger.FromDouble(ASeconds);
  Fields.MillisecondsBig := TBigInteger.FromDouble(AMilliseconds);
  Fields.MicrosecondsBig := TBigInteger.FromDouble(AMicroseconds);
  Fields.NanosecondsBig := TBigInteger.FromDouble(ANanoseconds);
  ValidateDurationFields(Fields);
  Result := FormatDurationFieldsWithFormat(DurationFormat, Fields);
end;

function TGocciaIntlDurationFormatValue.ToStringTag: string;
begin
  Result := 'Object';
end;

procedure TGocciaIntlDurationFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlDurationFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlDurationFormatSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('format', IntlDurationFormatFormat, 1,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('formatToParts', IntlDurationFormatFormatToParts, 1,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('resolvedOptions', IntlDurationFormatResolvedOptions, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Intl.DurationFormat'),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaIntlDurationFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlDurationFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlDurationFormatValue.Create(DefaultLocale);
    Shared := GetIntlDurationFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlDurationFormatValue.IntlDurationFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DF: TGocciaIntlDurationFormatValue;
  DurArg: TGocciaValue;
  Fields: TDurationFields;
begin
  DF := AsDurationFormat(AThisValue, 'Intl.DurationFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DurationFormat.prototype.format requires a duration argument');
  DurArg := AArgs.GetElement(0);

  Fields := ToDurationRecord(DurArg);
  Result := FormatDurationFieldsWithFormat(DF, Fields);
end;

function TGocciaIntlDurationFormatValue.IntlDurationFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DF: TGocciaIntlDurationFormatValue;
  DurArg: TGocciaValue;
  Fields: TDurationFields;
  Parts: TDurationFormatPartArray;

begin
  DF := AsDurationFormat(AThisValue, 'Intl.DurationFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DurationFormat.prototype.formatToParts requires a duration argument');
  DurArg := AArgs.GetElement(0);

  Fields := ToDurationRecord(DurArg);
  Parts := PartitionDurationFormatPattern(DF, Fields);
  Result := FormatDurationPartsToArray(Parts);
end;

function TGocciaIntlDurationFormatValue.IntlDurationFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DF: TGocciaIntlDurationFormatValue;
  Obj: TGocciaObjectValue;
begin
  DF := AsDurationFormat(AThisValue, 'Intl.DurationFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(DF.FLocale));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(DF.FNumberingSystem));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(DF.FStyle));
  Obj.AssignProperty('years', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 0)));
  Obj.AssignProperty('yearsDisplay', TGocciaStringLiteralValue.Create(DF.FYearsDisplay));
  Obj.AssignProperty('months', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 1)));
  Obj.AssignProperty('monthsDisplay', TGocciaStringLiteralValue.Create(DF.FMonthsDisplay));
  Obj.AssignProperty('weeks', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 2)));
  Obj.AssignProperty('weeksDisplay', TGocciaStringLiteralValue.Create(DF.FWeeksDisplay));
  Obj.AssignProperty('days', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 3)));
  Obj.AssignProperty('daysDisplay', TGocciaStringLiteralValue.Create(DF.FDaysDisplay));
  Obj.AssignProperty('hours', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 4)));
  Obj.AssignProperty('hoursDisplay', TGocciaStringLiteralValue.Create(DF.FHoursDisplay));
  Obj.AssignProperty('minutes', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 5)));
  Obj.AssignProperty('minutesDisplay', TGocciaStringLiteralValue.Create(DF.FMinutesDisplay));
  Obj.AssignProperty('seconds', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 6)));
  Obj.AssignProperty('secondsDisplay', TGocciaStringLiteralValue.Create(DF.FSecondsDisplay));
  Obj.AssignProperty('milliseconds', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 7)));
  Obj.AssignProperty('millisecondsDisplay', TGocciaStringLiteralValue.Create(DF.FMillisecondsDisplay));
  Obj.AssignProperty('microseconds', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 8)));
  Obj.AssignProperty('microsecondsDisplay', TGocciaStringLiteralValue.Create(DF.FMicrosecondsDisplay));
  Obj.AssignProperty('nanoseconds', TGocciaStringLiteralValue.Create(DurationResolvedUnitStyleByIndex(DF, 9)));
  Obj.AssignProperty('nanosecondsDisplay', TGocciaStringLiteralValue.Create(DF.FNanosecondsDisplay));
  if DF.FHasFractionalDigits then
    Obj.AssignProperty('fractionalDigits',
      TGocciaNumberLiteralValue.Create(DF.FFractionalDigits));
  Result := Obj;
end;

initialization
  GIntlDurationFormatSharedSlot := RegisterRealmOwnedSlot('Intl.DurationFormat.shared');

end.
