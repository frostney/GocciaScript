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
    FYearsDisplay: string;
    FMonthsDisplay: string;
    FWeeksDisplay: string;
    FDaysDisplay: string;
    FHoursDisplay: string;
    FMinutesDisplay: string;
    FSecondsDisplay: string;
    FMillisecondsDisplay: string;
    FMicrosecondsDisplay: string;
    FNanosecondsDisplay: string;
    FNumberingSystem: string;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlDurationFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDurationFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDurationFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlDurationFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

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
  end;

function ExtractDurationFields(const AObj: TGocciaObjectValue): TDurationFields;
var
  V: TGocciaValue;
begin
  Result.Years := 0;
  Result.Months := 0;
  Result.Weeks := 0;
  Result.Days := 0;
  Result.Hours := 0;
  Result.Minutes := 0;
  Result.Seconds := 0;
  Result.Milliseconds := 0;
  Result.Microseconds := 0;
  Result.Nanoseconds := 0;

  V := AObj.GetProperty('years');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Years := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('months');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Months := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('weeks');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Weeks := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('days');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Days := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('hours');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Hours := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('minutes');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Minutes := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('seconds');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Seconds := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('milliseconds');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Milliseconds := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('microseconds');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Microseconds := V.ToNumberLiteral.Value;
  V := AObj.GetProperty('nanoseconds');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    Result.Nanoseconds := V.ToNumberLiteral.Value;
end;

function FormatDurationFallback(const AFields: TDurationFields; const AStyle: string): string;
var
  Parts: array of string;

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
    if Frac(AValue) = 0 then
      Parts[Length(Parts) - 1] := IntToStr(Trunc(AValue)) + ' ' + UnitLabel
    else
      Parts[Length(Parts) - 1] := FloatToStr(AValue) + ' ' + UnitLabel;
  end;

var
  Separator: string;
  I: Integer;
begin
  SetLength(Parts, 0);
  AddPart(AFields.Years, 'years', 'yrs', 'y');
  AddPart(AFields.Months, 'months', 'mos', 'mo');
  AddPart(AFields.Weeks, 'weeks', 'wks', 'w');
  AddPart(AFields.Days, 'days', 'days', 'd');
  AddPart(AFields.Hours, 'hours', 'hrs', 'h');
  AddPart(AFields.Minutes, 'minutes', 'mins', 'min');
  AddPart(AFields.Seconds, 'seconds', 'secs', 's');
  AddPart(AFields.Milliseconds, 'milliseconds', 'ms', 'ms');
  AddPart(AFields.Microseconds, 'microseconds', #$C2#$B5's', #$C2#$B5's');
  AddPart(AFields.Nanoseconds, 'nanoseconds', 'ns', 'ns');

  if Length(Parts) = 0 then
  begin
    Result := '0 seconds';
    Exit;
  end;

  if AStyle = 'narrow' then
    Separator := ' '
  else
    Separator := ', ';

  Result := '';
  for I := 0 to Length(Parts) - 1 do
  begin
    if I > 0 then Result := Result + Separator;
    Result := Result + Parts[I];
  end;
end;

function FormatDurationDigital(const AFields: TDurationFields): string;
var
  TotalSeconds: Double;
  H, M, S: Integer;
begin
  TotalSeconds := AFields.Hours * 3600 + AFields.Minutes * 60 + AFields.Seconds;
  H := Trunc(TotalSeconds) div 3600;
  M := (Trunc(TotalSeconds) mod 3600) div 60;
  S := Trunc(TotalSeconds) mod 60;
  Result := Format('%.2d:%.2d:%.2d', [H, M, S]);
end;

{ TGocciaIntlDurationFormatValue }

procedure TGocciaIntlDurationFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
begin
  if not Assigned(AOptions) then Exit;

  V := AOptions.GetProperty('style');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FStyle := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FStyle) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FStyle, 'style']));
  end;
  V := AOptions.GetProperty('yearsDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FYearsDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('monthsDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMonthsDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('weeksDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FWeeksDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('daysDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FDaysDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('hoursDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FHoursDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('minutesDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinutesDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('secondsDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FSecondsDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('millisecondsDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMillisecondsDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('microsecondsDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMicrosecondsDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('nanosecondsDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FNanosecondsDisplay := V.ToStringLiteral.Value;
  V := AOptions.GetProperty('numberingSystem');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FNumberingSystem := V.ToStringLiteral.Value;
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
  FYearsDisplay := 'auto';
  FMonthsDisplay := 'auto';
  FWeeksDisplay := 'auto';
  FDaysDisplay := 'auto';
  FHoursDisplay := 'auto';
  FMinutesDisplay := 'auto';
  FSecondsDisplay := 'auto';
  FMillisecondsDisplay := 'auto';
  FMicrosecondsDisplay := 'auto';
  FNanosecondsDisplay := 'auto';

  ReadOptions(AOptions);

  if FNumberingSystem = '' then
    FNumberingSystem := 'latn';

  InitializePrototype;
  if Assigned(GetIntlDurationFormatShared) then
    FPrototype := GetIntlDurationFormatShared.Prototype;
end;

function TGocciaIntlDurationFormatValue.ToStringTag: string;
begin
  Result := 'Intl.DurationFormat';
end;

procedure TGocciaIntlDurationFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlDurationFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlDurationFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
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
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
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
  Formatted: string;
begin
  DF := AsDurationFormat(AThisValue, 'Intl.DurationFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DurationFormat.prototype.format requires a duration argument');
  DurArg := AArgs.GetElement(0);
  if not (DurArg is TGocciaObjectValue) then
    ThrowTypeError('Intl.DurationFormat.prototype.format requires an object argument');

  Fields := ExtractDurationFields(TGocciaObjectValue(DurArg));

  if DF.FStyle = 'digital' then
    Formatted := FormatDurationDigital(Fields)
  else
    Formatted := FormatDurationFallback(Fields, DF.FStyle);

  Result := TGocciaStringLiteralValue.Create(Formatted);
end;

function TGocciaIntlDurationFormatValue.IntlDurationFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DF: TGocciaIntlDurationFormatValue;
  DurArg: TGocciaValue;
  Fields: TDurationFields;
  Arr: TGocciaArrayValue;

  procedure AddPart(const AType: string; const AValue: Double; const AUnit: string);
  var
    PartObj: TGocciaObjectValue;
  begin
    if AValue = 0 then Exit;
    PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    PartObj.AssignProperty('type', TGocciaStringLiteralValue.Create(AType));
    PartObj.AssignProperty('value', TGocciaStringLiteralValue.Create(FloatToStr(AValue)));
    PartObj.AssignProperty('unit', TGocciaStringLiteralValue.Create(AUnit));
    Arr.Elements.Add(PartObj);
  end;

begin
  DF := AsDurationFormat(AThisValue, 'Intl.DurationFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DurationFormat.prototype.formatToParts requires a duration argument');
  DurArg := AArgs.GetElement(0);
  if not (DurArg is TGocciaObjectValue) then
    ThrowTypeError('Intl.DurationFormat.prototype.formatToParts requires an object argument');

  Fields := ExtractDurationFields(TGocciaObjectValue(DurArg));
  Arr := TGocciaArrayValue.Create;

  AddPart('element', Fields.Years, 'years');
  AddPart('element', Fields.Months, 'months');
  AddPart('element', Fields.Weeks, 'weeks');
  AddPart('element', Fields.Days, 'days');
  AddPart('element', Fields.Hours, 'hours');
  AddPart('element', Fields.Minutes, 'minutes');
  AddPart('element', Fields.Seconds, 'seconds');
  AddPart('element', Fields.Milliseconds, 'milliseconds');
  AddPart('element', Fields.Microseconds, 'microseconds');
  AddPart('element', Fields.Nanoseconds, 'nanoseconds');

  Result := Arr;
end;

function TGocciaIntlDurationFormatValue.IntlDurationFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DF: TGocciaIntlDurationFormatValue;
  Obj: TGocciaObjectValue;
begin
  DF := AsDurationFormat(AThisValue, 'Intl.DurationFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(DF.FLocale));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(DF.FStyle));
  Obj.AssignProperty('yearsDisplay', TGocciaStringLiteralValue.Create(DF.FYearsDisplay));
  Obj.AssignProperty('monthsDisplay', TGocciaStringLiteralValue.Create(DF.FMonthsDisplay));
  Obj.AssignProperty('weeksDisplay', TGocciaStringLiteralValue.Create(DF.FWeeksDisplay));
  Obj.AssignProperty('daysDisplay', TGocciaStringLiteralValue.Create(DF.FDaysDisplay));
  Obj.AssignProperty('hoursDisplay', TGocciaStringLiteralValue.Create(DF.FHoursDisplay));
  Obj.AssignProperty('minutesDisplay', TGocciaStringLiteralValue.Create(DF.FMinutesDisplay));
  Obj.AssignProperty('secondsDisplay', TGocciaStringLiteralValue.Create(DF.FSecondsDisplay));
  Obj.AssignProperty('millisecondsDisplay', TGocciaStringLiteralValue.Create(DF.FMillisecondsDisplay));
  Obj.AssignProperty('microsecondsDisplay', TGocciaStringLiteralValue.Create(DF.FMicrosecondsDisplay));
  Obj.AssignProperty('nanosecondsDisplay', TGocciaStringLiteralValue.Create(DF.FNanosecondsDisplay));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(DF.FNumberingSystem));
  Result := Obj;
end;

initialization
  GIntlDurationFormatSharedSlot := RegisterRealmOwnedSlot('Intl.DurationFormat.shared');

end.
