unit Goccia.Values.IntlRelativeTimeFormat;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlRelativeTimeFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FNumeric: string;
    FStyle: string;
    FNumberingSystem: string;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlRelativeTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlRelativeTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlRelativeTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
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
  GIntlRelativeTimeFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlRelativeTimeFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlRelativeTimeFormatSharedSlot))
  else
    Result := nil;
end;

function AsRelativeTimeFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlRelativeTimeFormatValue;
begin
  if not (AValue is TGocciaIntlRelativeTimeFormatValue) then
    ThrowTypeError(AMethod + ' called on non-RelativeTimeFormat');
  Result := TGocciaIntlRelativeTimeFormatValue(AValue);
end;

function UnitStringToEnum(const AValue: string): TIntlRelativeTimeUnit;
begin
  if (AValue = 'year') or (AValue = 'years') then
    Result := irtuYear
  else if (AValue = 'quarter') or (AValue = 'quarters') then
    Result := irtuQuarter
  else if (AValue = 'month') or (AValue = 'months') then
    Result := irtuMonth
  else if (AValue = 'week') or (AValue = 'weeks') then
    Result := irtuWeek
  else if (AValue = 'day') or (AValue = 'days') then
    Result := irtuDay
  else if (AValue = 'hour') or (AValue = 'hours') then
    Result := irtuHour
  else if (AValue = 'minute') or (AValue = 'minutes') then
    Result := irtuMinute
  else if (AValue = 'second') or (AValue = 'seconds') then
    Result := irtuSecond
  else
  begin
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + AValue);
    Result := irtuSecond;
  end;
end;

function NumericStringToEnum(const AValue: string): TIntlRelativeTimeNumeric;
begin
  if AValue = 'auto' then
    Result := irtnAuto
  else
    Result := irtnAlways;
end;

{ TGocciaIntlRelativeTimeFormatValue }

constructor TGocciaIntlRelativeTimeFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FNumeric := 'always';
  FStyle := 'long';

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('localeMatcher');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      if ContainsNulCharacter(V.ToStringLiteral.Value) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [V.ToStringLiteral.Value, 'localeMatcher']));
    end;
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FNumeric := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FNumeric) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FNumeric, 'numeric']));
    end;
    V := AOptions.GetProperty('style');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FStyle := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FStyle) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FStyle, 'style']));
    end;
    V := AOptions.GetProperty('numberingSystem');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FNumberingSystem := V.ToStringLiteral.Value;
  end;

  InitializePrototype;
  if Assigned(GetIntlRelativeTimeFormatShared) then
    FPrototype := GetIntlRelativeTimeFormatShared.Prototype;
end;

function TGocciaIntlRelativeTimeFormatValue.ToStringTag: string;
begin
  Result := 'Intl.RelativeTimeFormat';
end;

procedure TGocciaIntlRelativeTimeFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlRelativeTimeFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlRelativeTimeFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('format', IntlRelativeTimeFormatFormat, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatToParts', IntlRelativeTimeFormatFormatToParts, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlRelativeTimeFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.RelativeTimeFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlRelativeTimeFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlRelativeTimeFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlRelativeTimeFormatValue.Create(DefaultLocale);
    Shared := GetIntlRelativeTimeFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  NumValue: Double;
  UnitStr: string;
  Formatted: string;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.format');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.RelativeTimeFormat.prototype.format requires value and unit');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;
  UnitStr := AArgs.GetElement(1).ToStringLiteral.Value;
  if ContainsNulCharacter(UnitStr) then
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + UnitStr);

  if TryICUFormatRelativeTime(RTF.FLocale, NumValue, UnitStringToEnum(UnitStr),
    NumericStringToEnum(RTF.FNumeric), Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(NumValue) + ' ' + UnitStr);
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  NumValue: Double;
  UnitStr, Formatted: string;
  Arr: TGocciaArrayValue;
  PartObj: TGocciaObjectValue;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.formatToParts');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.RelativeTimeFormat.prototype.formatToParts requires value and unit');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;
  UnitStr := AArgs.GetElement(1).ToStringLiteral.Value;
  if ContainsNulCharacter(UnitStr) then
    ThrowRangeError('Invalid unit for Intl.RelativeTimeFormat: ' + UnitStr);

  // Produce a single-element parts array with the formatted string
  Arr := TGocciaArrayValue.Create;
  if TryICUFormatRelativeTime(RTF.FLocale, NumValue, UnitStringToEnum(UnitStr),
    NumericStringToEnum(RTF.FNumeric), Formatted) then
  begin
    PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    PartObj.AssignProperty('type', TGocciaStringLiteralValue.Create('literal'));
    PartObj.AssignProperty('value', TGocciaStringLiteralValue.Create(Formatted));
    PartObj.AssignProperty('unit', TGocciaStringLiteralValue.Create(UnitStr));
    Arr.Elements.Add(PartObj);
  end;
  Result := Arr;
end;

function TGocciaIntlRelativeTimeFormatValue.IntlRelativeTimeFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RTF: TGocciaIntlRelativeTimeFormatValue;
  Obj: TGocciaObjectValue;
begin
  RTF := AsRelativeTimeFormat(AThisValue, 'Intl.RelativeTimeFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(RTF.FLocale));
  Obj.AssignProperty('numeric', TGocciaStringLiteralValue.Create(RTF.FNumeric));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(RTF.FStyle));
  if RTF.FNumberingSystem <> '' then
    Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(RTF.FNumberingSystem));
  Result := Obj;
end;

initialization
  GIntlRelativeTimeFormatSharedSlot := RegisterRealmOwnedSlot('Intl.RelativeTimeFormat.shared');

end.
