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

function FormatPartsToArray(const AParts: TIntlFormatPartArray): TGocciaArrayValue;
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
    Result.Elements.Add(PartObj);
  end;
end;

{ TGocciaIntlDateTimeFormatValue }

procedure TGocciaIntlDateTimeFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
begin
  if not Assigned(AOptions) then Exit;

  V := AOptions.GetProperty('localeMatcher');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    if ContainsNulCharacter(V.ToStringLiteral.Value) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [V.ToStringLiteral.Value, 'localeMatcher']));
  end;
  V := AOptions.GetProperty('formatMatcher');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    if ContainsNulCharacter(V.ToStringLiteral.Value) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [V.ToStringLiteral.Value, 'formatMatcher']));
  end;
  V := AOptions.GetProperty('dateStyle');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FDateStyle := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('timeStyle');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FTimeStyle := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('calendar');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FCalendar := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('numberingSystem');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FNumberingSystem := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('timeZone');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FTimeZone := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FTimeZone) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FTimeZone, 'timeZone']));
  end;
  V := AOptions.GetProperty('hour12');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    if V.ToBooleanLiteral.Value then
      FHour12 := 1
    else
      FHour12 := 0;
  end;
  V := AOptions.GetProperty('hourCycle');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FHourCycle := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FHourCycle) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FHourCycle, 'hourCycle']));
  end;
  V := AOptions.GetProperty('weekday');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FWeekday := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FWeekday) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FWeekday, 'weekday']));
  end;
  V := AOptions.GetProperty('era');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FEra := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FEra) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FEra, 'era']));
  end;
  V := AOptions.GetProperty('year');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FYear := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FYear) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FYear, 'year']));
  end;
  V := AOptions.GetProperty('month');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FMonth := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FMonth) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FMonth, 'month']));
  end;
  V := AOptions.GetProperty('day');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FDay := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FDay) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FDay, 'day']));
  end;
  V := AOptions.GetProperty('dayPeriod');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FDayPeriod := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('hour');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FHour := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FHour) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FHour, 'hour']));
  end;
  V := AOptions.GetProperty('minute');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FMinute := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FMinute) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FMinute, 'minute']));
  end;
  V := AOptions.GetProperty('second');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FSecond := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FSecond) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FSecond, 'second']));
  end;
  V := AOptions.GetProperty('fractionalSecondDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FFractionalSecondDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('timeZoneName');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FTimeZoneName := V.ToStringLiteral.Value;
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

  // Build resolved ICU options
  FResolvedOptions := DefaultDateTimeFormatOptions;
  FResolvedOptions.DateStyle := DateStyleStringToEnum(FDateStyle);
  FResolvedOptions.TimeStyle := DateStyleStringToEnum(FTimeStyle);
  FResolvedOptions.Calendar := FCalendar;
  FResolvedOptions.NumberingSystem := FNumberingSystem;
  FResolvedOptions.TimeZone := FTimeZone;
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
  if DTF.FDateStyle <> '' then
    Obj.AssignProperty('dateStyle', TGocciaStringLiteralValue.Create(DTF.FDateStyle));
  if DTF.FTimeStyle <> '' then
    Obj.AssignProperty('timeStyle', TGocciaStringLiteralValue.Create(DTF.FTimeStyle));
  if DTF.FCalendar <> '' then
    Obj.AssignProperty('calendar', TGocciaStringLiteralValue.Create(DTF.FCalendar));
  if DTF.FNumberingSystem <> '' then
    Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(DTF.FNumberingSystem));
  if DTF.FTimeZone <> '' then
    Obj.AssignProperty('timeZone', TGocciaStringLiteralValue.Create(DTF.FTimeZone));
  if DTF.FHour12 >= 0 then
    Obj.AssignProperty('hour12', TGocciaBooleanLiteralValue.Create(DTF.FHour12 = 1));
  if DTF.FHourCycle <> '' then
    Obj.AssignProperty('hourCycle', TGocciaStringLiteralValue.Create(DTF.FHourCycle));
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
  Result := Obj;
end;

initialization
  GIntlDateTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.DateTimeFormat.shared');

end.
