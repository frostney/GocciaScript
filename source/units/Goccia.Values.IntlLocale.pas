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
    FFirstDayOfWeek: Integer;

    procedure InitializePrototype;
    procedure ParseTag(const ATag: string; const AOptions: TGocciaObjectValue);
    function ResolveLocaleInfoRegion: string;
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
    function IntlLocaleGetCalendars(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetCollations(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetHourCycles(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetNumberingSystems(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetTimeZones(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetTextInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlLocaleGetWeekInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
  Goccia.Intl.CLDRData,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
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

function SplitSubtags(const AValue: string): IntlTypes.TStringArray;
var
  Count, StartIndex, Index: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  StartIndex := 1;

  for Index := 1 to Length(AValue) + 1 do
  begin
    if (Index > Length(AValue)) or (AValue[Index] = '-') then
    begin
      if Index > StartIndex then
      begin
        Inc(Count);
        SetLength(Result, Count);
        Result[Count - 1] := Copy(AValue, StartIndex, Index - StartIndex);
      end;
      StartIndex := Index + 1;
    end;
  end;
end;

function TryGetUnicodeExtensionKeyword(const AParsed: TBcp47Tag;
  const AKey: string; out AValue: string): Boolean;
var
  ExtensionIndex, PartIndex, ValueStart, ValueEnd: Integer;
  Parts: IntlTypes.TStringArray;
begin
  Result := False;
  AValue := '';

  for ExtensionIndex := 0 to High(AParsed.Extensions) do
  begin
    if AParsed.Extensions[ExtensionIndex].Singleton <> 'u' then
      Continue;

    Parts := SplitSubtags(AParsed.Extensions[ExtensionIndex].Value);
    PartIndex := 0;
    while PartIndex <= High(Parts) do
    begin
      if Length(Parts[PartIndex]) = 2 then
      begin
        ValueStart := PartIndex + 1;
        ValueEnd := ValueStart;
        while (ValueEnd <= High(Parts)) and (Length(Parts[ValueEnd]) <> 2) do
          Inc(ValueEnd);

        if Parts[PartIndex] = AKey then
        begin
          if ValueEnd > ValueStart then
          begin
            AValue := Parts[ValueStart];
            Inc(ValueStart);
            while ValueStart < ValueEnd do
            begin
              AValue := AValue + '-' + Parts[ValueStart];
              Inc(ValueStart);
            end;
          end;
          Result := AValue <> '';
          Exit;
        end;

        PartIndex := ValueEnd;
      end
      else
        Inc(PartIndex);
    end;
  end;
end;

function NormalizeCalendarType(const AValue: string): string;
begin
  if AValue = 'gregorian' then
    Result := 'gregory'
  else if AValue = 'ethiopic-amete-alem' then
    Result := 'ethioaa'
  else
    Result := AValue;
end;

function DayIdentifierToNumber(const AValue: string): Integer;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(AValue);
  if (LowerValue = 'mon') or (LowerValue = '1') then
    Result := 1
  else if (LowerValue = 'tue') or (LowerValue = '2') then
    Result := 2
  else if (LowerValue = 'wed') or (LowerValue = '3') then
    Result := 3
  else if (LowerValue = 'thu') or (LowerValue = '4') then
    Result := 4
  else if (LowerValue = 'fri') or (LowerValue = '5') then
    Result := 5
  else if (LowerValue = 'sat') or (LowerValue = '6') then
    Result := 6
  else if (LowerValue = 'sun') or (LowerValue = '7') or (LowerValue = '0') then
    Result := 7
  else
    Result := 0;
end;

function CreateStringArray(const AValues: IntlTypes.TStringArray): TGocciaArrayValue;
var
  Index: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for Index := 0 to High(AValues) do
    Result.Elements.Add(TGocciaStringLiteralValue.Create(AValues[Index]));
end;

function CreateSingleStringArray(const AValue: string): TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  Result.Elements.Add(TGocciaStringLiteralValue.Create(AValue));
end;

function NormalizeCalendarArray(const AValues: IntlTypes.TStringArray): IntlTypes.TStringArray;
var
  Index: Integer;
begin
  SetLength(Result, Length(AValues));
  for Index := 0 to High(AValues) do
    Result[Index] := NormalizeCalendarType(AValues[Index]);
end;

function FilterCollationArray(const AValues: IntlTypes.TStringArray): IntlTypes.TStringArray;
var
  Index, Count: Integer;
begin
  Count := 0;
  SetLength(Result, Length(AValues));
  for Index := 0 to High(AValues) do
  begin
    if (AValues[Index] = '') or (AValues[Index] = 'standard') or
       (AValues[Index] = 'search') then
      Continue;
    Result[Count] := AValues[Index];
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

function IsWeekendDay(ADay, AWeekendStart, AWeekendEnd: Integer): Boolean;
begin
  if AWeekendStart <= AWeekendEnd then
    Result := (ADay >= AWeekendStart) and (ADay <= AWeekendEnd)
  else
    Result := (ADay >= AWeekendStart) or (ADay <= AWeekendEnd);
end;

function CreateWeekendArray(AWeekendStart, AWeekendEnd: Integer): TGocciaArrayValue;
var
  Day: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for Day := 1 to 7 do
  begin
    if IsWeekendDay(Day, AWeekendStart, AWeekendEnd) then
      Result.Elements.Add(TGocciaNumberLiteralValue.Create(Day));
  end;
end;

{ TGocciaIntlLocaleValue }

procedure TGocciaIntlLocaleValue.ParseTag(const ATag: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
  Parsed, BaseParsed: TBcp47Tag;
  ExtensionValue, FirstDayOption: string;
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
    BaseParsed := Parsed;
    SetLength(BaseParsed.Extensions, 0);
    BaseParsed.PrivateUse := '';
    FBaseName := CanonicalizeBcp47Tag(BaseParsed);

    FLanguage := LowerCase(Parsed.Language);
    FScript := Parsed.Script;
    FRegion := Parsed.Region;

    if TryGetUnicodeExtensionKeyword(Parsed, 'ca', ExtensionValue) then
      FCalendar := NormalizeCalendarType(ExtensionValue);
    if TryGetUnicodeExtensionKeyword(Parsed, 'co', ExtensionValue) then
      FCollation := ExtensionValue;
    if TryGetUnicodeExtensionKeyword(Parsed, 'hc', ExtensionValue) then
      FHourCycle := ExtensionValue;
    if TryGetUnicodeExtensionKeyword(Parsed, 'nu', ExtensionValue) then
      FNumberingSystem := ExtensionValue;
    if TryGetUnicodeExtensionKeyword(Parsed, 'fw', ExtensionValue) then
      FFirstDayOfWeek := DayIdentifierToNumber(ExtensionValue);
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
    FCalendar := NormalizeCalendarType(FCalendar);
    ReadValidatedStringOption(AOptions, 'caseFirst', FCaseFirst);
    ReadValidatedStringOption(AOptions, 'collation', FCollation);
    ReadValidatedStringOption(AOptions, 'hourCycle', FHourCycle);
    ReadValidatedStringOption(AOptions, 'numberingSystem', FNumberingSystem);
    if TryReadStringOption(AOptions, 'firstDayOfWeek', FirstDayOption) then
    begin
      if ContainsNulCharacter(FirstDayOption) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FirstDayOption, 'firstDayOfWeek']));
      FFirstDayOfWeek := DayIdentifierToNumber(FirstDayOption);
      if FFirstDayOfWeek = 0 then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FirstDayOption, 'firstDayOfWeek']));
    end;
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FNumeric := V.ToBooleanLiteral.Value;
  end;
end;

constructor TGocciaIntlLocaleValue.Create(const ATag: string; const AOptions: TGocciaObjectValue);
begin
  inherited Create;
  FNumeric := False;
  FFirstDayOfWeek := 0;
  ParseTag(ATag, AOptions);
  InitializePrototype;
  if Assigned(GetIntlLocaleShared) then
    FPrototype := GetIntlLocaleShared.Prototype;
end;

function TGocciaIntlLocaleValue.ToStringTag: string;
begin
  Result := 'Intl.Locale';
end;

function TGocciaIntlLocaleValue.ResolveLocaleInfoRegion: string;
var
  Maximized: string;
  Parsed: TBcp47Tag;
begin
  if FRegion <> '' then
  begin
    Result := FRegion;
    Exit;
  end;

  if TryICUMaximizeLocale(FBaseName, Maximized) then
  begin
    Parsed := ParseBcp47Tag(Maximized);
    if Parsed.IsValid and (Parsed.Region <> '') then
    begin
      Result := Parsed.Region;
      Exit;
    end;
  end;

  if TryGetLikelySubtags(LowerCase(FBaseName), Maximized) then
  begin
    Parsed := ParseBcp47Tag(Maximized);
    if Parsed.IsValid and (Parsed.Region <> '') then
    begin
      Result := Parsed.Region;
      Exit;
    end;
  end;

  Result := '001';
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
      Members.AddNamedMethod('getCalendars', IntlLocaleGetCalendars, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getCollations', IntlLocaleGetCollations, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getHourCycles', IntlLocaleGetHourCycles, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getNumberingSystems', IntlLocaleGetNumberingSystems, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getTimeZones', IntlLocaleGetTimeZones, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getTextInfo', IntlLocaleGetTextInfo, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getWeekInfo', IntlLocaleGetWeekInfo, 0,
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

// ECMA-402 §sec-intl.locale.prototype.getcalendars Intl.Locale.prototype.getCalendars()
function TGocciaIntlLocaleValue.IntlLocaleGetCalendars(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  Calendars: IntlTypes.TStringArray;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getCalendars');
  if L.FCalendar <> '' then
  begin
    Result := CreateSingleStringArray(NormalizeCalendarType(L.FCalendar));
    Exit;
  end;

  if TryICUGetLocaleCalendars(L.FTag, Calendars) then
    Calendars := NormalizeCalendarArray(Calendars)
  else if not TryGetLocaleCalendars(L.ResolveLocaleInfoRegion, Calendars) then
  begin
    SetLength(Calendars, 1);
    Calendars[0] := 'gregory';
  end;

  Result := CreateStringArray(Calendars);
end;

// ECMA-402 §sec-intl.locale.prototype.getcollations Intl.Locale.prototype.getCollations()
function TGocciaIntlLocaleValue.IntlLocaleGetCollations(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  Collations: IntlTypes.TStringArray;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getCollations');
  if L.FCollation <> '' then
  begin
    Result := CreateSingleStringArray(L.FCollation);
    Exit;
  end;

  if TryICUGetLocaleCollations(L.FTag, Collations) then
    Collations := FilterCollationArray(Collations);

  if Length(Collations) = 0 then
  begin
    if not TryGetLocaleCollations(Collations) then
    begin
      SetLength(Collations, 2);
      Collations[0] := 'emoji';
      Collations[1] := 'eor';
    end;
  end;

  Result := CreateStringArray(Collations);
end;

// ECMA-402 §sec-intl.locale.prototype.gethourcycles Intl.Locale.prototype.getHourCycles()
function TGocciaIntlLocaleValue.IntlLocaleGetHourCycles(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  HourCycles: IntlTypes.TStringArray;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getHourCycles');
  if L.FHourCycle <> '' then
  begin
    Result := CreateSingleStringArray(L.FHourCycle);
    Exit;
  end;

  if not TryGetLocaleHourCycles(L.FBaseName, L.ResolveLocaleInfoRegion, HourCycles) then
  begin
    SetLength(HourCycles, 1);
    HourCycles[0] := 'h23';
  end;

  Result := CreateStringArray(HourCycles);
end;

// ECMA-402 §sec-intl.locale.prototype.getnumberingsystems Intl.Locale.prototype.getNumberingSystems()
function TGocciaIntlLocaleValue.IntlLocaleGetNumberingSystems(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  NumberingSystems: IntlTypes.TStringArray;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getNumberingSystems');
  if L.FNumberingSystem <> '' then
  begin
    Result := CreateSingleStringArray(L.FNumberingSystem);
    Exit;
  end;

  if not TryGetLocaleNumberingSystems(L.FBaseName, NumberingSystems) then
  begin
    SetLength(NumberingSystems, 1);
    NumberingSystems[0] := 'latn';
  end;

  Result := CreateStringArray(NumberingSystems);
end;

// ECMA-402 §sec-intl.locale.prototype.gettimezones Intl.Locale.prototype.getTimeZones()
function TGocciaIntlLocaleValue.IntlLocaleGetTimeZones(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  TimeZones: IntlTypes.TStringArray;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getTimeZones');
  if L.FRegion = '' then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if not TryGetLocaleTimeZones(L.FRegion, TimeZones) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := CreateStringArray(TimeZones);
end;

// ECMA-402 §sec-intl.locale.prototype.gettextinfo Intl.Locale.prototype.getTextInfo()
function TGocciaIntlLocaleValue.IntlLocaleGetTextInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  Direction: string;
  Obj: TGocciaObjectValue;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getTextInfo');
  if not TryGetLocaleTextDirection(L.FBaseName, Direction) then
    Direction := 'ltr';

  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('direction', TGocciaStringLiteralValue.Create(Direction));
  Result := Obj;
end;

// ECMA-402 §sec-intl.locale.prototype.getweekinfo Intl.Locale.prototype.getWeekInfo()
function TGocciaIntlLocaleValue.IntlLocaleGetWeekInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  L: TGocciaIntlLocaleValue;
  FirstDay, WeekendStart, WeekendEnd, MinimalDays: Integer;
  Obj: TGocciaObjectValue;
begin
  L := AsLocale(AThisValue, 'Intl.Locale.prototype.getWeekInfo');

  if not TryGetLocaleWeekInfo(L.ResolveLocaleInfoRegion,
    FirstDay, WeekendStart, WeekendEnd, MinimalDays) then
  begin
    FirstDay := 1;
    WeekendStart := 6;
    WeekendEnd := 7;
  end;

  if L.FFirstDayOfWeek <> 0 then
    FirstDay := L.FFirstDayOfWeek;

  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('firstDay', TGocciaNumberLiteralValue.Create(FirstDay));
  Obj.AssignProperty('weekend', CreateWeekendArray(WeekendStart, WeekendEnd));
  Result := Obj;
end;

function TGocciaIntlLocaleValue.IntlLocaleToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AsLocale(AThisValue, 'Intl.Locale.prototype.toString').FTag);
end;

initialization
  GIntlLocaleSharedSlot := RegisterRealmOwnedSlot('Intl.Locale.shared');

end.
