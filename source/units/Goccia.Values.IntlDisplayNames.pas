unit Goccia.Values.IntlDisplayNames;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlDisplayNamesValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FType: string;
    FStyle: string;
    FFallback: string;
    FLanguageDisplay: string;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlDisplayNamesOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlDisplayNamesResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlCLDRData,
  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlDisplayNamesSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlDisplayNamesShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlDisplayNamesSharedSlot))
  else
    Result := nil;
end;

function AsDisplayNames(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlDisplayNamesValue;
begin
  if not (AValue is TGocciaIntlDisplayNamesValue) then
    ThrowTypeError(AMethod + ' called on non-DisplayNames');
  Result := TGocciaIntlDisplayNamesValue(AValue);
end;

function DisplayTypeStringToEnum(const AValue: string): TIntlDisplayNameType;
begin
  if AValue = 'region' then
    Result := idntRegion
  else if AValue = 'script' then
    Result := idntScript
  else if AValue = 'calendar' then
    Result := idntCalendar
  else if AValue = 'currency' then
    Result := idntCurrency
  else if AValue = 'dateTimeField' then
    Result := idntDateTimeField
  else
    Result := idntLanguage;
end;

function DisplayStyleStringToEnum(const AValue: string): TIntlDisplayNameStyle;
begin
  if AValue = 'short' then
    Result := idnsShort
  else if AValue = 'narrow' then
    Result := idnsNarrow
  else
    Result := idnsLong;
end;

{ TGocciaIntlDisplayNamesValue }

constructor TGocciaIntlDisplayNamesValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
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
  FStyle := 'long';
  FFallback := 'code';
  FLanguageDisplay := 'dialect';

  // The type option is required
  if not Assigned(AOptions) then
    ThrowTypeError('Intl.DisplayNames requires options with a type property');

  V := AOptions.GetProperty('type');
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.DisplayNames requires a type option');
  FType := V.ToStringLiteral.Value;
  if ContainsNulCharacter(FType) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [FType, 'type']));

  V := AOptions.GetProperty('style');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FStyle := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FStyle) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FStyle, 'style']));
  end;
  V := AOptions.GetProperty('fallback');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    FFallback := V.ToStringLiteral.Value;
    if ContainsNulCharacter(FFallback) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FFallback, 'fallback']));
  end;
  V := AOptions.GetProperty('languageDisplay');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FLanguageDisplay := V.ToStringLiteral.Value;

  InitializePrototype;
  if Assigned(GetIntlDisplayNamesShared) then
    FPrototype := GetIntlDisplayNamesShared.Prototype;
end;

function TGocciaIntlDisplayNamesValue.ToStringTag: string;
begin
  Result := 'Intl.DisplayNames';
end;

procedure TGocciaIntlDisplayNamesValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlDisplayNamesShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlDisplayNamesSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('of', IntlDisplayNamesOf, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlDisplayNamesResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.DisplayNames'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlDisplayNamesValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
  DefaultOpts: TGocciaObjectValue;
begin
  Shared := GetIntlDisplayNamesShared;
  if not Assigned(Shared) then
  begin
    DefaultOpts := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    DefaultOpts.AssignProperty('type', TGocciaStringLiteralValue.Create('language'));
    TGocciaIntlDisplayNamesValue.Create(DefaultLocale, DefaultOpts);
    Shared := GetIntlDisplayNamesShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlDisplayNamesValue.IntlDisplayNamesOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DN: TGocciaIntlDisplayNamesValue;
  Code: string;
  DisplayName: string;
begin
  DN := AsDisplayNames(AThisValue, 'Intl.DisplayNames.prototype.of');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.DisplayNames.prototype.of requires a code argument');
  Code := AArgs.GetElement(0).ToStringLiteral.Value;

  if TryICUGetDisplayName(DN.FLocale, Code, DisplayTypeStringToEnum(DN.FType),
    DisplayStyleStringToEnum(DN.FStyle), DisplayName) then
    Result := TGocciaStringLiteralValue.Create(DisplayName)
  else if TryGetDisplayName(DN.FLocale, Code, DisplayTypeStringToEnum(DN.FType),
    DisplayName) then
    Result := TGocciaStringLiteralValue.Create(DisplayName)
  else
  begin
    if DN.FFallback = 'code' then
      Result := TGocciaStringLiteralValue.Create(Code)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaIntlDisplayNamesValue.IntlDisplayNamesResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  DN: TGocciaIntlDisplayNamesValue;
  Obj: TGocciaObjectValue;
begin
  DN := AsDisplayNames(AThisValue, 'Intl.DisplayNames.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(DN.FLocale));
  Obj.AssignProperty('type', TGocciaStringLiteralValue.Create(DN.FType));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(DN.FStyle));
  Obj.AssignProperty('fallback', TGocciaStringLiteralValue.Create(DN.FFallback));
  if DN.FType = 'language' then
    Obj.AssignProperty('languageDisplay', TGocciaStringLiteralValue.Create(DN.FLanguageDisplay));
  Result := Obj;
end;

initialization
  GIntlDisplayNamesSharedSlot := RegisterRealmOwnedSlot('Intl.DisplayNames.shared');

end.
