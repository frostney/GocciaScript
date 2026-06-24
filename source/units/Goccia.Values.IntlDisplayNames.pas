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

  BCP47,
  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.Intl.CLDRData,
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
  if AValue = 'language' then
    Result := idntLanguage
  else if AValue = 'region' then
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
  begin
    ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, 'type']));
    Result := idntLanguage; // unreachable; satisfies compiler
  end;
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

function ReadDisplayNamesStringOption(const AOptions: TGocciaObjectValue;
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

function IsAsciiAlpha(const AChar: Char): Boolean;
begin
  Result := ((AChar >= 'A') and (AChar <= 'Z')) or
    ((AChar >= 'a') and (AChar <= 'z'));
end;

function IsAsciiDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function IsAsciiAlphaNumeric(const AChar: Char): Boolean;
begin
  Result := IsAsciiAlpha(AChar) or IsAsciiDigit(AChar);
end;

function IsUnicodeLocaleTypeIdentifier(const AValue: string): Boolean;
var
  I, SubtagLength: Integer;
begin
  Result := AValue <> '';
  if not Result then
    Exit;

  SubtagLength := 0;
  for I := 1 to Length(AValue) do
  begin
    if AValue[I] = '-' then
    begin
      if (SubtagLength < 3) or (SubtagLength > 8) then
        Exit(False);
      SubtagLength := 0;
      Continue;
    end;

    if not IsAsciiAlphaNumeric(AValue[I]) then
      Exit(False);
    Inc(SubtagLength);
  end;

  Result := (SubtagLength >= 3) and (SubtagLength <= 8);
end;

function IsDisplayNamesRegionCode(const ACode: string): Boolean;
begin
  Result := ((Length(ACode) = 2) and IsAsciiAlpha(ACode[1]) and
    IsAsciiAlpha(ACode[2])) or
    ((Length(ACode) = 3) and IsAsciiDigit(ACode[1]) and
    IsAsciiDigit(ACode[2]) and IsAsciiDigit(ACode[3]));
end;

function IsDisplayNamesScriptCode(const ACode: string): Boolean;
begin
  Result := (Length(ACode) = 4) and IsAsciiAlpha(ACode[1]) and
    IsAsciiAlpha(ACode[2]) and IsAsciiAlpha(ACode[3]) and
    IsAsciiAlpha(ACode[4]);
end;

function IsDisplayNamesCurrencyCode(const ACode: string): Boolean;
begin
  Result := (Length(ACode) = 3) and IsAsciiAlpha(ACode[1]) and
    IsAsciiAlpha(ACode[2]) and IsAsciiAlpha(ACode[3]);
end;

function IsDisplayNamesDateTimeFieldCode(const ACode: string): Boolean;
begin
  Result := (ACode = 'era') or (ACode = 'year') or
    (ACode = 'quarter') or (ACode = 'month') or
    (ACode = 'weekOfYear') or (ACode = 'weekday') or
    (ACode = 'day') or (ACode = 'dayPeriod') or
    (ACode = 'hour') or (ACode = 'minute') or
    (ACode = 'second') or (ACode = 'timeZoneName');
end;

function TitleCaseAsciiScriptCode(const ACode: string): string;
begin
  Result := LowerCase(ACode);
  if Result <> '' then
    Result[1] := UpCase(Result[1]);
end;

function CanonicalDisplayNamesCode(const AType, ACode: string): string;
var
  Parsed: TBcp47Tag;
  I: Integer;
begin
  Result := ACode;
  if AType = 'language' then
  begin
    Parsed := ParseBcp47Tag(ACode);
    Result := CanonicalizeUnicodeLocaleId(ACode);
    if (Result = '') or (not Parsed.IsValid) or (Parsed.PrivateUse <> '') or
       (Pos('_', ACode) <> 0) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
    for I := 0 to High(Parsed.Extensions) do
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
  end
  else if AType = 'region' then
  begin
    if not IsDisplayNamesRegionCode(ACode) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
    Result := UpperCase(ACode);
  end
  else if AType = 'script' then
  begin
    if not IsDisplayNamesScriptCode(ACode) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
    Result := TitleCaseAsciiScriptCode(ACode);
  end
  else if AType = 'currency' then
  begin
    if not IsDisplayNamesCurrencyCode(ACode) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
    Result := UpperCase(ACode);
  end
  else if AType = 'calendar' then
  begin
    if not IsUnicodeLocaleTypeIdentifier(ACode) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
    Result := LowerCase(ACode);
  end
  else if AType = 'dateTimeField' then
  begin
    if not IsDisplayNamesDateTimeFieldCode(ACode) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [ACode, 'code']));
  end;
end;

function TryGetRecentCurrencyDisplayName(const ACode: string; out AName: string): Boolean;
begin
  if UpperCase(ACode) = 'XCG' then
  begin
    AName := 'Caribbean guilder';
    Exit(True);
  end
  else if UpperCase(ACode) = 'ZWG' then
  begin
    AName := 'Zimbabwean Gold';
    Exit(True);
  end;

  AName := '';
  Result := False;
end;

procedure DefineDisplayNamesResolvedProperty(const AObject: TGocciaObjectValue;
  const AName, AValue: string);
begin
  AObject.DefineProperty(AName, TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(AValue),
    [pfEnumerable, pfConfigurable, pfWritable]));
end;

{ TGocciaIntlDisplayNamesValue }

constructor TGocciaIntlDisplayNamesValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
  Ignored: string;
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

  Ignored := ReadDisplayNamesStringOption(AOptions, 'localeMatcher',
    'best fit', ['lookup', 'best fit']);
  FStyle := ReadDisplayNamesStringOption(AOptions, 'style', FStyle,
    ['narrow', 'short', 'long']);

  V := AOptions.GetProperty('type');
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.DisplayNames requires a type option');
  FType := V.ToStringLiteral.Value;
  if ContainsNulCharacter(FType) or
     not ((FType = 'language') or (FType = 'region') or
       (FType = 'script') or (FType = 'currency') or
       (FType = 'calendar') or (FType = 'dateTimeField')) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [FType, 'type']));

  FFallback := ReadDisplayNamesStringOption(AOptions, 'fallback',
    FFallback, ['code', 'none']);
  FLanguageDisplay := ReadDisplayNamesStringOption(AOptions,
    'languageDisplay', FLanguageDisplay, ['dialect', 'standard']);

  InitializePrototype;
  if Assigned(GetIntlDisplayNamesShared) then
    FPrototype := GetIntlDisplayNamesShared.Prototype;
end;

function TGocciaIntlDisplayNamesValue.ToStringTag: string;
begin
  Result := 'Object';
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
  Code := CanonicalDisplayNamesCode(DN.FType,
    AArgs.GetElement(0).ToStringLiteral.Value);

  if TryICUGetDisplayName(DN.FLocale, Code, DisplayTypeStringToEnum(DN.FType),
    DisplayStyleStringToEnum(DN.FStyle), DisplayName) then
    Result := TGocciaStringLiteralValue.Create(DisplayName)
  else if TryGetDisplayName(DN.FLocale, Code, DisplayTypeStringToEnum(DN.FType),
    DisplayName) then
    Result := TGocciaStringLiteralValue.Create(DisplayName)
  else if (DN.FType = 'currency') and
          TryGetRecentCurrencyDisplayName(Code, DisplayName) then
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
  DefineDisplayNamesResolvedProperty(Obj, 'locale', DN.FLocale);
  DefineDisplayNamesResolvedProperty(Obj, 'style', DN.FStyle);
  DefineDisplayNamesResolvedProperty(Obj, 'type', DN.FType);
  DefineDisplayNamesResolvedProperty(Obj, 'fallback', DN.FFallback);
  if DN.FType = 'language' then
    DefineDisplayNamesResolvedProperty(Obj, 'languageDisplay',
      DN.FLanguageDisplay);
  Result := Obj;
end;

initialization
  GIntlDisplayNamesSharedSlot := RegisterRealmOwnedSlot('Intl.DisplayNames.shared');

end.
