unit Goccia.Values.IntlLocale;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlLocaleValue = class(TGocciaObjectValue)
  private
    FTag: string;
    FLanguage: string;
    FScript: string;
    FRegion: string;
    FBaseName: string;
    FCalendar: string;
    FCaseFirst: string;
    FCollation: string;
    FHourCycle: string;
    FNumberingSystem: string;
    FNumeric: Boolean;

    procedure InitializePrototype;
    procedure ParseTag(const ATag: string; const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ATag: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlLocaleGetLanguage(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetScript(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetRegion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetBaseName(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetCaseFirst(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetCollation(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetHourCycle(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetNumberingSystem(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetNumeric(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleMaximize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleMinimize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  BCP47,
  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlLocaleSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlLocaleShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlLocaleSharedSlot))
  else
    Result := nil;
end;

function AsLocale(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlLocaleValue;
begin
  if not (AValue is TGocciaIntlLocaleValue) then
    ThrowTypeError(AMethod + ' called on non-Locale');
  Result := TGocciaIntlLocaleValue(AValue);
end;

{ TGocciaIntlLocaleValue }

procedure TGocciaIntlLocaleValue.ParseTag(const ATag: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
  Parsed: TBcp47Tag;
begin
  if ContainsNulCharacter(ATag) then
    ThrowRangeError('Invalid language tag: ' + ATag);
  Canonical := CanonicalizeUnicodeLocaleId(ATag);
  if Canonical = '' then
    ThrowRangeError('Invalid language tag: ' + ATag);
  FTag := Canonical;
  FBaseName := Canonical;

  // Extract subtags from the canonical tag via BCP47 parser
  Parsed := ParseBcp47Tag(Canonical);
  if Parsed.IsValid then
  begin
    FLanguage := LowerCase(Parsed.Language);
    FScript := Parsed.Script;
    FRegion := Parsed.Region;
  end
  else
  begin
    // Fallback: extract language from the first subtag
    if Pos('-', Canonical) > 0 then
      FLanguage := Copy(Canonical, 1, Pos('-', Canonical) - 1)
    else
      FLanguage := Canonical;
  end;

  // Apply options overrides
  if Assigned(AOptions) then
  begin
    ReadValidatedStringOption(AOptions, 'language', FLanguage);
    ReadValidatedStringOption(AOptions, 'script', FScript);
    ReadValidatedStringOption(AOptions, 'region', FRegion);
    ReadValidatedStringOption(AOptions, 'calendar', FCalendar);
    ReadValidatedStringOption(AOptions, 'caseFirst', FCaseFirst);
    ReadValidatedStringOption(AOptions, 'collation', FCollation);
    ReadValidatedStringOption(AOptions, 'hourCycle', FHourCycle);
    ReadValidatedStringOption(AOptions, 'numberingSystem', FNumberingSystem);
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FNumeric := V.ToBooleanLiteral.Value;
  end;
end;

constructor TGocciaIntlLocaleValue.Create(const ATag: string; const AOptions: TGocciaObjectValue);
begin
  inherited Create;
  FNumeric := False;
  ParseTag(ATag, AOptions);
  InitializePrototype;
  if Assigned(GetIntlLocaleShared) then
    FPrototype := GetIntlLocaleShared.Prototype;
end;

function TGocciaIntlLocaleValue.ToStringTag: string;
begin
  Result := 'Intl.Locale';
end;

procedure TGocciaIntlLocaleValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlLocaleShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlLocaleSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('language', IntlLocaleGetLanguage, nil, [pfConfigurable]);
      Members.AddAccessor('script', IntlLocaleGetScript, nil, [pfConfigurable]);
      Members.AddAccessor('region', IntlLocaleGetRegion, nil, [pfConfigurable]);
      Members.AddAccessor('baseName', IntlLocaleGetBaseName, nil, [pfConfigurable]);
      Members.AddAccessor('calendar', IntlLocaleGetCalendar, nil, [pfConfigurable]);
      Members.AddAccessor('caseFirst', IntlLocaleGetCaseFirst, nil, [pfConfigurable]);
      Members.AddAccessor('collation', IntlLocaleGetCollation, nil, [pfConfigurable]);
      Members.AddAccessor('hourCycle', IntlLocaleGetHourCycle, nil, [pfConfigurable]);
      Members.AddAccessor('numberingSystem', IntlLocaleGetNumberingSystem, nil, [pfConfigurable]);
      Members.AddAccessor('numeric', IntlLocaleGetNumeric, nil, [pfConfigurable]);
      Members.AddNamedMethod('maximize', IntlLocaleMaximize, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('minimize', IntlLocaleMinimize, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('toString', IntlLocaleToString, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.Locale'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlLocaleValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlLocaleShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlLocaleValue.Create(DefaultLocale);
    Shared := GetIntlLocaleShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetLanguage(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AsLocale(AThisValue, 'get Intl.Locale.prototype.language').FLanguage);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetScript(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.script');
  if L.FScript = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FScript);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetRegion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.region');
  if L.FRegion = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FRegion);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetBaseName(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AsLocale(AThisValue, 'get Intl.Locale.prototype.baseName').FBaseName);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetCalendar(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.calendar');
  if L.FCalendar = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FCalendar);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetCaseFirst(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.caseFirst');
  if L.FCaseFirst = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FCaseFirst);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetCollation(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.collation');
  if L.FCollation = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FCollation);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetHourCycle(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.hourCycle');
  if L.FHourCycle = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FHourCycle);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetNumberingSystem(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
begin
  L := AsLocale(AThisValue, 'get Intl.Locale.prototype.numberingSystem');
  if L.FNumberingSystem = '' then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := TGocciaStringLiteralValue.Create(L.FNumberingSystem);
end;

function TGocciaIntlLocaleValue.IntlLocaleGetNumeric(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AsLocale(AThisValue, 'get Intl.Locale.prototype.numeric').FNumeric then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaIntlLocaleValue.IntlLocaleMaximize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  Maximized: string;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.maximize');
  if TryICUMaximizeLocale(L.FTag, Maximized) then
    Result := TGocciaIntlLocaleValue.Create(Maximized)
  else
    Result := TGocciaIntlLocaleValue.Create(L.FTag);
end;

function TGocciaIntlLocaleValue.IntlLocaleMinimize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  Minimized: string;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.minimize');
  if TryICUMinimizeLocale(L.FTag, Minimized) then
    Result := TGocciaIntlLocaleValue.Create(Minimized)
  else
    Result := TGocciaIntlLocaleValue.Create(L.FTag);
end;

function TGocciaIntlLocaleValue.IntlLocaleToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AsLocale(AThisValue, 'Intl.Locale.prototype.toString').FTag);
end;

initialization
  GIntlLocaleSharedSlot := RegisterRealmOwnedSlot('Intl.Locale.shared');

end.
