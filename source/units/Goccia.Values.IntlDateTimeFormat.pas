unit Goccia.Values.IntlDateTimeFormat;

{$I Goccia.inc}

interface

uses
  IntlTypes,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlDateTimeFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FDateStyle: string;
    FTimeStyle: string;
    FCalendar: string;
    FNumberingSystem: string;
    FTimeZone: string;
    FHour12: Integer;
    FHourCycle: string;
    FWeekday: string;
    FEra: string;
    FYear: string;
    FMonth: string;
    FDay: string;
    FDayPeriod: string;
    FHour: string;
    FMinute: string;
    FSecond: string;
    FFractionalSecondDigits: Integer;
    FTimeZoneName: string;
    FResolvedOptions: TIntlDateTimeFormatOptions;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlDateTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDateTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,

  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlDateTimeFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlDateTimeFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlDateTimeFormatSharedSlot))
  else
    Result := nil;
end;

function DefaultTimeZone: string;
begin
  Result := 'UTC';
end;

function IsAsciiDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function ReadTwoDigits(const AValue: string; const APosition: Integer;
  out ANumber: Integer): Boolean;
begin
  Result := False;
  ANumber := 0;
  if APosition + 1 > Length(AValue) then
    Exit;
  if not IsAsciiDigit(AValue[APosition]) or not IsAsciiDigit(AValue[APosition + 1]) then
    Exit;
  ANumber := (Ord(AValue[APosition]) - Ord('0')) * 10 +
    (Ord(AValue[APosition + 1]) - Ord('0'));
  Result := True;
end;

function TryParseOffsetTimeZoneIdentifier(const AValue: string;
  out AOffsetMinutes: Integer): Boolean;
var
  Hours, Minutes, Position, Sign: Integer;
begin
  Result := False;
  AOffsetMinutes := 0;

  if Length(AValue) < 3 then
    Exit;

  case AValue[1] of
    '+': Sign := 1;
    '-': Sign := -1;
  else
    Exit;
  end;

  if not ReadTwoDigits(AValue, 2, Hours) then
    Exit;

  Position := 4;
  Minutes := 0;
  if Position <= Length(AValue) then
  begin
    if AValue[Position] = ':' then
      Inc(Position);

    if not ReadTwoDigits(AValue, Position, Minutes) then
      Exit;
    Inc(Position, 2);

    if Position <= Length(AValue) then
      Exit;
  end;

  if (Hours > 23) or (Minutes > 59) then
    Exit;

  AOffsetMinutes := Sign * ((Hours * 60) + Minutes);
  Result := True;
end;

// ECMA-402 §11.1.3 FormatOffsetTimeZoneIdentifier(offsetMinutes).
function FormatOffsetTimeZoneIdentifier(const AOffsetMinutes: Integer): string;
var
  AbsoluteMinutes, Hours, Minutes: Integer;
  Sign: Char;
begin
  if AOffsetMinutes >= 0 then
    Sign := '+'
  else
    Sign := '-';
  AbsoluteMinutes := Abs(AOffsetMinutes);
  Hours := AbsoluteMinutes div 60;
  Minutes := AbsoluteMinutes mod 60;
  Result := Sign + Format('%.2d:%.2d', [Hours, Minutes]);
end;

function NormalizeDateTimeFormatTimeZone(const ATimeZone: string): string;
var
  OffsetMinutes: Integer;
begin
  Result := ATimeZone;
  if ATimeZone = '' then
    Exit;

  if not ((ATimeZone[1] = '+') or (ATimeZone[1] = '-')) then
    Exit;

  // ECMA-402 §11.1.2 normalizes offset time zone identifiers before storing
  // [[TimeZone]], so resolvedOptions() exposes ±HH:MM even for ±HH input.
  if not TryParseOffsetTimeZoneIdentifier(ATimeZone, OffsetMinutes) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [ATimeZone, 'timeZone']));

  Result := FormatOffsetTimeZoneIdentifier(OffsetMinutes);
end;

function ICUDateTimeFormatTimeZone(const ATimeZone: string): string;
begin
  Result := ATimeZone;
  if (ATimeZone <> '') and ((ATimeZone[1] = '+') or (ATimeZone[1] = '-')) then
    Result := 'GMT' + ATimeZone;
end;

function AsDateTimeFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlDateTimeFormatValue;
begin
  if not (AValue is TGocciaIntlDateTimeFormatValue) then
    ThrowTypeError(AMethod + ' called on non-DateTimeFormat');
  Result := TGocciaIntlDateTimeFormatValue(AValue);
end;

function DateStyleStringToEnum(const AValue: string): TIntlDateTimeStyle;
begin
  if AValue = 'full' then
    Result := idtsFull
  else if AValue = 'long' then
    Result := idtsLong
  else if AValue = 'medium' then
    Result := idtsMedium
  else if AValue = 'short' then
    Result := idtsShort
  else
    Result := idtsNone;
end;

{ TGocciaIntlDateTimeFormatValue }

procedure TGocciaIntlDateTimeFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
  Ignored: string;
begin
  if not Assigned(AOptions) then Exit;

  ReadValidatedStringOption(AOptions, 'localeMatcher', Ignored);
  ReadValidatedStringOption(AOptions, 'formatMatcher', Ignored);
  TryReadStringOption(AOptions, 'dateStyle', FDateStyle);
  TryReadStringOption(AOptions, 'timeStyle', FTimeStyle);
  TryReadStringOption(AOptions, 'calendar', FCalendar);
  TryReadStringOption(AOptions, 'numberingSystem', FNumberingSystem);
  ReadValidatedStringOption(AOptions, 'timeZone', FTimeZone);
  FTimeZone := NormalizeDateTimeFormatTimeZone(FTimeZone);
  V := AOptions.GetProperty('hour12');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    if V.ToBooleanLiteral.Value then
      FHour12 := 1
    else
      FHour12 := 0;
  end;
  ReadValidatedStringOption(AOptions, 'hourCycle', FHourCycle);
  ReadValidatedStringOption(AOptions, 'weekday', FWeekday);
  ReadValidatedStringOption(AOptions, 'era', FEra);
  ReadValidatedStringOption(AOptions, 'year', FYear);
  ReadValidatedStringOption(AOptions, 'month', FMonth);
  ReadValidatedStringOption(AOptions, 'day', FDay);
  TryReadStringOption(AOptions, 'dayPeriod', FDayPeriod);
  ReadValidatedStringOption(AOptions, 'hour', FHour);
  ReadValidatedStringOption(AOptions, 'minute', FMinute);
  ReadValidatedStringOption(AOptions, 'second', FSecond);
  V := AOptions.GetProperty('fractionalSecondDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FFractionalSecondDigits := Trunc(V.ToNumberLiteral.Value);
  TryReadStringOption(AOptions, 'timeZoneName', FTimeZoneName);
end;

constructor TGocciaIntlDateTimeFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
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
  FHour12 := -1;
  FFractionalSecondDigits := -1;
  FTimeZone := DefaultTimeZone;

  ReadOptions(AOptions);

  // Default calendar and numberingSystem
  if FCalendar = '' then
    FCalendar := 'gregory';
  if FNumberingSystem = '' then
    FNumberingSystem := 'latn';

  // ECMA-402: when no dateStyle/timeStyle and no component properties,
  // default to { year: "numeric", month: "numeric", day: "numeric" }
  if (FDateStyle = '') and (FTimeStyle = '') and
     (FWeekday = '') and (FEra = '') and (FYear = '') and
     (FMonth = '') and (FDay = '') and (FDayPeriod = '') and
     (FHour = '') and (FMinute = '') and (FSecond = '') and
     (FFractionalSecondDigits < 0) and (FTimeZoneName = '') then
  begin
    FYear := 'numeric';
    FMonth := 'numeric';
    FDay := 'numeric';
  end;

  // Build resolved ICU options
  FResolvedOptions := DefaultDateTimeFormatOptions;
  FResolvedOptions.DateStyle := DateStyleStringToEnum(FDateStyle);
  FResolvedOptions.TimeStyle := DateStyleStringToEnum(FTimeStyle);
  FResolvedOptions.Calendar := FCalendar;
  FResolvedOptions.NumberingSystem := FNumberingSystem;
  FResolvedOptions.TimeZone := ICUDateTimeFormatTimeZone(FTimeZone);
  FResolvedOptions.Hour12 := FHour12;
  FResolvedOptions.HourCycle := FHourCycle;
  FResolvedOptions.Weekday := FWeekday;
  FResolvedOptions.Era := FEra;
  FResolvedOptions.Year := FYear;
  FResolvedOptions.Month := FMonth;
  FResolvedOptions.Day := FDay;
  FResolvedOptions.DayPeriod := FDayPeriod;
  FResolvedOptions.Hour := FHour;
  FResolvedOptions.Minute := FMinute;
  FResolvedOptions.Second := FSecond;
  FResolvedOptions.FractionalSecondDigits := FFractionalSecondDigits;
  FResolvedOptions.TimeZoneName := FTimeZoneName;

  InitializePrototype;
  if Assigned(GetIntlDateTimeFormatShared) then
    FPrototype := GetIntlDateTimeFormatShared.Prototype;
end;

function TGocciaIntlDateTimeFormatValue.ToStringTag: string;
begin
  Result := 'Intl.DateTimeFormat';
end;

procedure TGocciaIntlDateTimeFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlDateTimeFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlDateTimeFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('format', IntlDateTimeFormatFormat, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatToParts', IntlDateTimeFormatFormatToParts, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatRange', IntlDateTimeFormatFormatRange, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlDateTimeFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.DateTimeFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlDateTimeFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlDateTimeFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlDateTimeFormatValue.Create(DefaultLocale);
    Shared := GetIntlDateTimeFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Millis: Double;
  Formatted: string;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.format requires a date value');
  Millis := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUFormatDateTime(DTF.FLocale, Millis, DTF.FResolvedOptions, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(Millis));
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Millis: Double;
  Parts: TIntlFormatPartArray;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatToParts requires a date value');
  Millis := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUFormatDateTimeToParts(DTF.FLocale, Millis, DTF.FResolvedOptions, Parts) then
    Result := FormatPartsToArray(Parts)
  else
    Result := TGocciaArrayValue.Create;
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  StartMillis, EndMillis: Double;
  StartFormatted, EndFormatted: string;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.formatRange');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.DateTimeFormat.prototype.formatRange requires start and end dates');
  StartMillis := AArgs.GetElement(0).ToNumberLiteral.Value;
  EndMillis := AArgs.GetElement(1).ToNumberLiteral.Value;

  // Format each date and join with range separator
  if TryICUFormatDateTime(DTF.FLocale, StartMillis, DTF.FResolvedOptions, StartFormatted) and
     TryICUFormatDateTime(DTF.FLocale, EndMillis, DTF.FResolvedOptions, EndFormatted) then
    Result := TGocciaStringLiteralValue.Create(StartFormatted + ' – ' + EndFormatted)
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(StartMillis) + ' – ' + FloatToStr(EndMillis));
end;

function TGocciaIntlDateTimeFormatValue.IntlDateTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DTF: TGocciaIntlDateTimeFormatValue;
  Obj: TGocciaObjectValue;
begin
  DTF := AsDateTimeFormat(AThisValue, 'Intl.DateTimeFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(DTF.FLocale));
  Obj.AssignProperty('calendar', TGocciaStringLiteralValue.Create(DTF.FCalendar));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(DTF.FNumberingSystem));
  Obj.AssignProperty('timeZone', TGocciaStringLiteralValue.Create(DTF.FTimeZone));
  if DTF.FHour12 >= 0 then
    Obj.AssignProperty('hour12', TGocciaBooleanLiteralValue.Create(DTF.FHour12 = 1));
  if DTF.FHourCycle <> '' then
    Obj.AssignProperty('hourCycle', TGocciaStringLiteralValue.Create(DTF.FHourCycle));
  if DTF.FDateStyle <> '' then
    Obj.AssignProperty('dateStyle', TGocciaStringLiteralValue.Create(DTF.FDateStyle));
  if DTF.FTimeStyle <> '' then
    Obj.AssignProperty('timeStyle', TGocciaStringLiteralValue.Create(DTF.FTimeStyle));
  if (DTF.FDateStyle = '') and (DTF.FTimeStyle = '') then
  begin
    if DTF.FWeekday <> '' then
      Obj.AssignProperty('weekday', TGocciaStringLiteralValue.Create(DTF.FWeekday));
    if DTF.FEra <> '' then
      Obj.AssignProperty('era', TGocciaStringLiteralValue.Create(DTF.FEra));
    if DTF.FYear <> '' then
      Obj.AssignProperty('year', TGocciaStringLiteralValue.Create(DTF.FYear));
    if DTF.FMonth <> '' then
      Obj.AssignProperty('month', TGocciaStringLiteralValue.Create(DTF.FMonth));
    if DTF.FDay <> '' then
      Obj.AssignProperty('day', TGocciaStringLiteralValue.Create(DTF.FDay));
    if DTF.FDayPeriod <> '' then
      Obj.AssignProperty('dayPeriod', TGocciaStringLiteralValue.Create(DTF.FDayPeriod));
    if DTF.FHour <> '' then
      Obj.AssignProperty('hour', TGocciaStringLiteralValue.Create(DTF.FHour));
    if DTF.FMinute <> '' then
      Obj.AssignProperty('minute', TGocciaStringLiteralValue.Create(DTF.FMinute));
    if DTF.FSecond <> '' then
      Obj.AssignProperty('second', TGocciaStringLiteralValue.Create(DTF.FSecond));
    if DTF.FFractionalSecondDigits >= 0 then
      Obj.AssignProperty('fractionalSecondDigits',
        TGocciaNumberLiteralValue.Create(DTF.FFractionalSecondDigits));
    if DTF.FTimeZoneName <> '' then
      Obj.AssignProperty('timeZoneName', TGocciaStringLiteralValue.Create(DTF.FTimeZoneName));
  end;
  Result := Obj;
end;

initialization
  GIntlDateTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.DateTimeFormat.shared');

end.
