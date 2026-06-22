unit Goccia.Builtins.Intl;

{$I Goccia.inc}

interface

uses
  IntlTypes,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlBuiltin = class(TGocciaBuiltin)
  private
    FIntlNamespace: TGocciaObjectValue;

    function GetCanonicalLocales(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SupportedValuesOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SupportedLocalesOfFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function LocaleConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CollatorConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberFormatConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DateTimeFormatConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PluralRulesConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RelativeTimeFormatConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ListFormatConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DisplayNamesConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SegmenterConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DurationFormatConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function LocaleConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function CollatorConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function NumberFormatConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function DateTimeFormatConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function PluralRulesConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function RelativeTimeFormatConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function ListFormatConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function DisplayNamesConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function SegmenterConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function DurationFormatConstructFn(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function ChainLegacyIntlConstructor(const AConstructorName: string; const AThisValue: TGocciaValue; const AConstructed: TGocciaValue): TGocciaValue;

    procedure RegisterLocale;
    procedure RegisterCollator;
    procedure RegisterNumberFormat;
    procedure RegisterDateTimeFormat;
    procedure RegisterPluralRules;
    procedure RegisterRelativeTimeFormat;
    procedure RegisterListFormat;
    procedure RegisterDisplayNames;
    procedure RegisterSegmenter;
    procedure RegisterDurationFormat;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

function CanonicalizeLocaleListFromValue(const ALocales: TGocciaValue): IntlTypes.TStringArray;
function ResolveRequestedLocale(const ARequestedLocales: IntlTypes.TStringArray): string;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.GarbageCollector,
  Goccia.Intl.CLDRData,
  Goccia.Intl.Helpers,
  Goccia.Realm,
  Goccia.Temporal.TimeZone,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.IntlCollator,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.IntlDisplayNames,
  Goccia.Values.IntlDurationFormat,
  Goccia.Values.IntlListFormat,
  Goccia.Values.IntlLocale,
  Goccia.Values.IntlNumberFormat,
  Goccia.Values.IntlPluralRules,
  Goccia.Values.IntlRelativeTimeFormat,
  Goccia.Values.IntlSegmenter,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

{ TGocciaIntlBuiltin }

constructor TGocciaIntlBuiltin.Create(const AName: string; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
var
  IntlMembers: array[0..2] of TGocciaMemberDefinition;
begin
  inherited Create(AName, AScope, AThrowError);

  FIntlNamespace := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  TGarbageCollector.Instance.AddTempRoot(FIntlNamespace);
  try
    RegisterLocale;
    RegisterCollator;
    RegisterNumberFormat;
    RegisterDateTimeFormat;
    RegisterPluralRules;
    RegisterRelativeTimeFormat;
    RegisterListFormat;
    RegisterDisplayNames;
    RegisterSegmenter;
    RegisterDurationFormat;

    IntlMembers[0] := DefineDataProperty('getCanonicalLocales',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(GetCanonicalLocales, 'getCanonicalLocales', 1),
      [pfConfigurable, pfWritable]);
    IntlMembers[1] := DefineDataProperty('supportedValuesOf',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(SupportedValuesOf, 'supportedValuesOf', 1),
      [pfConfigurable, pfWritable]);
    IntlMembers[2] := DefineSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Intl'),
      [pfConfigurable]);
    RegisterMemberDefinitions(FIntlNamespace, IntlMembers);

    AScope.DefineLexicalBinding(AName, FIntlNamespace, dtLet, True);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(FIntlNamespace);
  end;
end;

function IntlConstructorFromRealm(const ARealm: TGocciaRealm; const AConstructorName: string): TGocciaValue;
var
  ConstructorValue: TGocciaValue;
  IntlValue: TGocciaValue;
  RealmScope: TGocciaScope;
begin
  Result := nil;
  if not Assigned(ARealm) or not (ARealm.GlobalEnv is TGocciaScope) then
    Exit;

  RealmScope := TGocciaScope(ARealm.GlobalEnv);
  if not RealmScope.TryGetBindingValue('Intl', IntlValue) then
    Exit;
  if not (IntlValue is TGocciaObjectValue) then
    Exit;

  ConstructorValue := TGocciaObjectValue(IntlValue).GetProperty(AConstructorName);
  if ConstructorValue is TGocciaObjectValue then
    Result := ConstructorValue;
end;

function IntlConstructorPrototypeFromRealm(const ARealm: TGocciaRealm;
  const AConstructorName: string): TGocciaObjectValue;
var
  ConstructorValue: TGocciaValue;
  PrototypeValue: TGocciaValue;
begin
  Result := nil;
  ConstructorValue := IntlConstructorFromRealm(ARealm, AConstructorName);
  if not (ConstructorValue is TGocciaObjectValue) then
    Exit;

  PrototypeValue := TGocciaObjectValue(ConstructorValue).GetProperty(PROP_PROTOTYPE);
  if PrototypeValue is TGocciaObjectValue then
    Result := TGocciaObjectValue(PrototypeValue);
end;

function IntlPrototypeFromNewTarget(const ANewTarget: TGocciaValue;
  const AConstructorName: string): TGocciaObjectValue;
var
  FallbackRealm: TGocciaRealm;
  PrototypeValue: TGocciaValue;
begin
  PrototypeValue := nil;
  if ANewTarget is TGocciaObjectValue then
    PrototypeValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE);

  if PrototypeValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(PrototypeValue));

  FallbackRealm := nil;
  if ANewTarget is TGocciaFunctionBase then
    FallbackRealm := TGocciaFunctionBase(ANewTarget).CreationRealm;

  Result := IntlConstructorPrototypeFromRealm(FallbackRealm, AConstructorName);
  if Assigned(Result) then
    Exit;

  Result := IntlConstructorPrototypeFromRealm(CurrentRealm, AConstructorName);
  if Assigned(Result) then
    Exit;

  Result := TGocciaObjectValue.SharedObjectPrototype;
end;

procedure ApplyIntlConstructPrototype(const AObject: TGocciaValue;
  const ANewTarget: TGocciaValue; const AConstructorName: string);
begin
  if AObject is TGocciaObjectValue then
    TGocciaObjectValue(AObject).Prototype :=
      IntlPrototypeFromNewTarget(ANewTarget, AConstructorName);
end;

procedure RequireIntlConstructCall(const AThisValue: TGocciaValue;
  const AConstructorName: string);
begin
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Intl.' + AConstructorName + ' cannot be called without new');
end;

function TGocciaIntlBuiltin.ChainLegacyIntlConstructor(
  const AConstructorName: string; const AThisValue: TGocciaValue;
  const AConstructed: TGocciaValue): TGocciaValue;
var
  ConstructorValue: TGocciaValue;
begin
  Result := nil;
  if AThisValue is TGocciaHoleValue then
    Exit;
  if not (AThisValue is TGocciaObjectValue) then
    Exit;

  ConstructorValue := FIntlNamespace.GetProperty(AConstructorName);
  if Assigned(ConstructorValue) and
     OrdinaryHasInstance(ConstructorValue, AThisValue) then
  begin
    TGocciaObjectValue(AThisValue).DefineSymbolProperty(IntlFallbackSymbol,
      TGocciaPropertyDescriptorData.Create(AConstructed, []));
    Result := AThisValue;
  end;
end;

function IsUndefinedIntlValue(const AValue: TGocciaValue): Boolean;
begin
  Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
end;

procedure AppendCanonicalLocale(var ALocales: TStringArray; var ACount: Integer;
  const AValue: TGocciaValue);
var
  Tag, Canonical: string;
  I: Integer;
begin
  if not ((AValue is TGocciaStringLiteralValue) or
          (AValue is TGocciaObjectValue)) then
    ThrowTypeError('locales entries must be strings or objects');

  if AValue is TGocciaIntlLocaleValue then
    Tag := TGocciaIntlLocaleValue(AValue).LocaleTag
  else
    Tag := AValue.ToStringLiteral.Value;

  Canonical := CanonicalizeUnicodeLocaleId(Tag);
  if Canonical = '' then
    ThrowRangeError(Format('invalid language tag: %s', [Tag]));

  for I := 0 to ACount - 1 do
  begin
    if CompareText(ALocales[I], Canonical) = 0 then
      Exit;
  end;

  if ACount >= Length(ALocales) then
    SetLength(ALocales, ACount + 8);
  ALocales[ACount] := Canonical;
  Inc(ACount);
end;

// ECMA-402 ES2026 §9.2.1 CanonicalizeLocaleList(locales)
function CanonicalizeLocaleListFromValue(const ALocales: TGocciaValue): IntlTypes.TStringArray;
var
  O: TGocciaObjectValue;
  KValue: TGocciaValue;
  K, Len, Count: Integer;
  Pk: string;
  ObjectRoot: TGocciaTempRoot;
begin
  Count := 0;
  SetLength(Result, 0);

  if IsUndefinedIntlValue(ALocales) then
    Exit;

  if (ALocales is TGocciaStringLiteralValue) or
     (ALocales is TGocciaIntlLocaleValue) then
  begin
    AppendCanonicalLocale(Result, Count, ALocales);
    SetLength(Result, Count);
    Exit;
  end;

  O := ToObject(ALocales);
  InitializeTempRoot(ObjectRoot);
  AddTempRootIfNeeded(ObjectRoot, O);
  try
    Len := LengthOfArrayLike(O);
    for K := 0 to Len - 1 do
    begin
      Pk := IntToStr(K);
      if not O.HasProperty(Pk) then
        Continue;

      KValue := O.GetProperty(Pk);
      AppendCanonicalLocale(Result, Count, KValue);
    end;
  finally
    RemoveTempRootIfNeeded(ObjectRoot);
  end;

  SetLength(Result, Count);
end;

function ResolveRequestedLocale(const ARequestedLocales: IntlTypes.TStringArray): string;
begin
  Result := ResolveLocaleFromRequested(ARequestedLocales);
end;

// ECMA-402 ES2026 §9.2.11 GetOption for localeMatcher.
procedure ValidateLocaleMatcherOption(const AOptions: TGocciaValue);
var
  OptionsObject: TGocciaObjectValue;
  Value: TGocciaValue;
  Matcher: string;
  ObjectRoot: TGocciaTempRoot;
begin
  if IsUndefinedIntlValue(AOptions) then
    Exit;

  OptionsObject := ToObject(AOptions);
  InitializeTempRoot(ObjectRoot);
  AddTempRootIfNeeded(ObjectRoot, OptionsObject);
  try
    Value := OptionsObject.GetProperty('localeMatcher');
    if IsUndefinedIntlValue(Value) then
      Exit;

    Matcher := Value.ToStringLiteral.Value;
    if ContainsNulCharacter(Matcher) or
       ((Matcher <> 'lookup') and (Matcher <> 'best fit')) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [Matcher, 'localeMatcher']));
  finally
    RemoveTempRootIfNeeded(ObjectRoot);
  end;
end;

{ Intl.getCanonicalLocales }

function TGocciaIntlBuiltin.GetCanonicalLocales(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Canonical: TStringArray;
  ResultArr: TGocciaArrayValue;
  I: Integer;
begin
  Canonical := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));

  ResultArr := TGocciaArrayValue.Create;
  for I := 0 to High(Canonical) do
    ResultArr.Elements.Add(TGocciaStringLiteralValue.Create(Canonical[I]));

  Result := ResultArr;
end;

{ Intl.supportedValuesOf }

function TGocciaIntlBuiltin.SupportedValuesOf(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Key: string;
  Collations: TStringArray;
  TimeZones: TTemporalTimeZoneIdentifierArray;
  ResultArr: TGocciaArrayValue;
  I: Integer;

  procedure AddString(const AValue: string);
  begin
    ResultArr.Elements.Add(TGocciaStringLiteralValue.Create(AValue));
  end;

  procedure SortResultStrings;
  var
    I, J: Integer;
    Temp: TGocciaValue;
  begin
    for I := 0 to ResultArr.Elements.Count - 2 do
      for J := I + 1 to ResultArr.Elements.Count - 1 do
        if CompareStr(ResultArr.Elements[I].ToStringLiteral.Value,
          ResultArr.Elements[J].ToStringLiteral.Value) > 0 then
        begin
          Temp := ResultArr.Elements[I];
          ResultArr.Elements[I] := ResultArr.Elements[J];
          ResultArr.Elements[J] := Temp;
        end;
  end;

  function NormalizeSupportedCollation(const AValue: string): string;
  begin
    if AValue = 'phonebook' then
      Result := 'phonebk'
    else if AValue = 'traditional' then
      Result := 'trad'
    else if AValue = 'dictionary' then
      Result := 'dict'
    else
      Result := AValue;
  end;

  function CollationSupportedByICU(const AValue: string): Boolean;
  const
    ProbeLocales: array[0..9] of string = (
      'en', 'ar', 'de', 'es', 'hi', 'ko', 'ln', 'si', 'sv', 'zh'
    );
  var
    Locale, Collation: string;
    LocaleCollations: TStringArray;
  begin
    Result := False;
    for Locale in ProbeLocales do
    begin
      if not TryICUGetLocaleCollations(Locale, LocaleCollations) then
        Continue;
      for Collation in LocaleCollations do
      begin
        if SameText(NormalizeSupportedCollation(Collation), AValue) then
          Exit(True);
      end;
    end;
  end;

begin
  Key := AArgs.GetElement(0).ToStringLiteral.Value;

  ResultArr := TGocciaArrayValue.Create;

  if Key = 'calendar' then
  begin
    AddString('buddhist');
    AddString('chinese');
    AddString('coptic');
    AddString('dangi');
    AddString('ethioaa');
    AddString('ethiopic');
    AddString('gregory');
    AddString('hebrew');
    AddString('indian');
    AddString('islamic-civil');
    AddString('islamic-tbla');
    AddString('islamic-umalqura');
    AddString('iso8601');
    AddString('japanese');
    AddString('persian');
    AddString('roc');
  end
  else if Key = 'collation' then
  begin
    if IntlICUAvailable and TryGetLocaleCollations(Collations) then
      for I := 0 to High(Collations) do
        if CollationSupportedByICU(Collations[I]) then
          AddString(Collations[I]);
  end
  else if Key = 'currency' then
  begin
    AddString('AED');
    AddString('AFN');
    AddString('ALL');
    AddString('AMD');
    AddString('ANG');
    AddString('AOA');
    AddString('ARS');
    AddString('AUD');
    AddString('AWG');
    AddString('AZN');
    AddString('BAM');
    AddString('BBD');
    AddString('BDT');
    AddString('BGN');
    AddString('BHD');
    AddString('BIF');
    AddString('BMD');
    AddString('BND');
    AddString('BOB');
    AddString('BRL');
    AddString('BSD');
    AddString('BTN');
    AddString('BWP');
    AddString('BYN');
    AddString('BZD');
    AddString('CAD');
    AddString('CDF');
    AddString('CHF');
    AddString('CLP');
    AddString('CNY');
    AddString('COP');
    AddString('CRC');
    AddString('CUP');
    AddString('CVE');
    AddString('CZK');
    AddString('DJF');
    AddString('DKK');
    AddString('DOP');
    AddString('DZD');
    AddString('EGP');
    AddString('ERN');
    AddString('ETB');
    AddString('EUR');
    AddString('FJD');
    AddString('FKP');
    AddString('GBP');
    AddString('GEL');
    AddString('GHS');
    AddString('GIP');
    AddString('GMD');
    AddString('GNF');
    AddString('GTQ');
    AddString('GYD');
    AddString('HKD');
    AddString('HNL');
    AddString('HRK');
    AddString('HTG');
    AddString('HUF');
    AddString('IDR');
    AddString('ILS');
    AddString('INR');
    AddString('IQD');
    AddString('IRR');
    AddString('ISK');
    AddString('JMD');
    AddString('JOD');
    AddString('JPY');
    AddString('KES');
    AddString('KGS');
    AddString('KHR');
    AddString('KMF');
    AddString('KPW');
    AddString('KRW');
    AddString('KWD');
    AddString('KYD');
    AddString('KZT');
    AddString('LAK');
    AddString('LBP');
    AddString('LKR');
    AddString('LRD');
    AddString('LSL');
    AddString('LYD');
    AddString('MAD');
    AddString('MDL');
    AddString('MGA');
    AddString('MKD');
    AddString('MMK');
    AddString('MNT');
    AddString('MOP');
    AddString('MRU');
    AddString('MUR');
    AddString('MVR');
    AddString('MWK');
    AddString('MXN');
    AddString('MYR');
    AddString('MZN');
    AddString('NAD');
    AddString('NGN');
    AddString('NIO');
    AddString('NOK');
    AddString('NPR');
    AddString('NZD');
    AddString('OMR');
    AddString('PAB');
    AddString('PEN');
    AddString('PGK');
    AddString('PHP');
    AddString('PKR');
    AddString('PLN');
    AddString('PYG');
    AddString('QAR');
    AddString('RON');
    AddString('RSD');
    AddString('RUB');
    AddString('RWF');
    AddString('SAR');
    AddString('SBD');
    AddString('SCR');
    AddString('SDG');
    AddString('SEK');
    AddString('SGD');
    AddString('SHP');
    AddString('SLE');
    AddString('SOS');
    AddString('SRD');
    AddString('SSP');
    AddString('STN');
    AddString('SVC');
    AddString('SYP');
    AddString('SZL');
    AddString('THB');
    AddString('TJS');
    AddString('TMT');
    AddString('TND');
    AddString('TOP');
    AddString('TRY');
    AddString('TTD');
    AddString('TWD');
    AddString('TZS');
    AddString('UAH');
    AddString('UGX');
    AddString('USD');
    AddString('UYU');
    AddString('UZS');
    AddString('VES');
    AddString('VND');
    AddString('VUV');
    AddString('WST');
    AddString('XAF');
    AddString('XCD');
    AddString('XOF');
    AddString('XPF');
    AddString('YER');
    AddString('ZAR');
    AddString('ZMW');
    AddString('ZWL');
    AddString('ADP');
    AddString('AFA');
    AddString('ALK');
    AddString('AOK');
    AddString('AON');
    AddString('AOR');
    AddString('ARA');
    AddString('ARL');
    AddString('ARM');
    AddString('ARP');
    AddString('ATS');
    AddString('AZM');
    AddString('BAD');
    AddString('BAN');
    AddString('BEC');
    AddString('BEF');
    AddString('BEL');
    AddString('BGL');
    AddString('BGM');
    AddString('BGO');
    AddString('BOL');
    AddString('BOP');
    AddString('BOV');
    AddString('BRB');
    AddString('BRC');
    AddString('BRE');
    AddString('BRN');
    AddString('BRR');
    AddString('BRZ');
    AddString('BUK');
    AddString('BYB');
    AddString('BYR');
    AddString('CHE');
    AddString('CHW');
    AddString('CLE');
    AddString('CLF');
    AddString('CNH');
    AddString('CNX');
    AddString('COU');
    AddString('CSD');
    AddString('CSK');
    AddString('CUC');
    AddString('CYP');
    AddString('DDM');
    AddString('DEM');
    AddString('ECS');
    AddString('ECV');
    AddString('EEK');
    AddString('ESA');
    AddString('ESB');
    AddString('ESP');
    AddString('FIM');
    AddString('FRF');
    AddString('GEK');
    AddString('GHC');
    AddString('GNS');
    AddString('GQE');
    AddString('GRD');
    AddString('GWE');
    AddString('GWP');
    AddString('HRD');
    AddString('IEP');
    AddString('ILP');
    AddString('ILR');
    AddString('ISJ');
    AddString('ITL');
    AddString('KRH');
    AddString('KRO');
    AddString('LTL');
    AddString('LTT');
    AddString('LUC');
    AddString('LUF');
    AddString('LUL');
    AddString('LVL');
    AddString('LVR');
    AddString('MAF');
    AddString('MCF');
    AddString('MDC');
    AddString('MGF');
    AddString('MKN');
    AddString('MLF');
    AddString('MRO');
    AddString('MTL');
    AddString('MTP');
    AddString('MVP');
    AddString('MXP');
    AddString('MXV');
    AddString('MZE');
    AddString('MZM');
    AddString('NIC');
    AddString('NLG');
    AddString('PEI');
    AddString('PES');
    AddString('PLZ');
    AddString('PTE');
    AddString('RHD');
    AddString('ROL');
    AddString('RUR');
    AddString('SDD');
    AddString('SDP');
    AddString('SIT');
    AddString('SKK');
    AddString('SLL');
    AddString('SRG');
    AddString('STD');
    AddString('SUR');
    AddString('TJR');
    AddString('TMM');
    AddString('TPE');
    AddString('TRL');
    AddString('UAK');
    AddString('UGS');
    AddString('USN');
    AddString('USS');
    AddString('UYI');
    AddString('UYP');
    AddString('UYW');
    AddString('VEB');
    AddString('VED');
    AddString('VEF');
    AddString('VNN');
    AddString('XAG');
    AddString('XAU');
    AddString('XBA');
    AddString('XBB');
    AddString('XBC');
    AddString('XBD');
    AddString('XCG');
    AddString('XDR');
    AddString('XEU');
    AddString('XFO');
    AddString('XFU');
    AddString('XPD');
    AddString('XPT');
    AddString('XRE');
    AddString('XSU');
    AddString('XTS');
    AddString('XUA');
    AddString('XXX');
    AddString('YDD');
    AddString('YUD');
    AddString('YUM');
    AddString('YUN');
    AddString('YUR');
    AddString('ZAL');
    AddString('ZMK');
    AddString('ZRN');
    AddString('ZRZ');
    AddString('ZWD');
    AddString('ZWG');
    AddString('ZWR');
  end
  else if Key = 'numberingSystem' then
  begin
    AddString('adlm');
    AddString('ahom');
    AddString('arab');
    AddString('arabext');
    AddString('bali');
    AddString('beng');
    AddString('bhks');
    AddString('brah');
    AddString('cakm');
    AddString('cham');
    AddString('deva');
    AddString('fullwide');
    AddString('gong');
    AddString('gonm');
    AddString('gujr');
    AddString('guru');
    AddString('hanidec');
    AddString('hmng');
    AddString('java');
    AddString('kali');
    AddString('khmr');
    AddString('knda');
    AddString('lana');
    AddString('lanatham');
    AddString('laoo');
    AddString('latn');
    AddString('lepc');
    AddString('limb');
    AddString('mathbold');
    AddString('mathdbl');
    AddString('mathmono');
    AddString('mathsanb');
    AddString('mathsans');
    AddString('mlym');
    AddString('modi');
    AddString('mong');
    AddString('mroo');
    AddString('mtei');
    AddString('mymr');
    AddString('mymrshan');
    AddString('mymrtlng');
    AddString('newa');
    AddString('nkoo');
    AddString('olck');
    AddString('orya');
    AddString('osma');
    AddString('rohg');
    AddString('saur');
    AddString('shrd');
    AddString('sind');
    AddString('sinh');
    AddString('sora');
    AddString('sund');
    AddString('takr');
    AddString('talu');
    AddString('tamldec');
    AddString('telu');
    AddString('thai');
    AddString('tibt');
    AddString('tirh');
    AddString('vaii');
    AddString('wara');
    AddString('wcho');
    AddString('diak');
    AddString('gara');
    AddString('gukh');
    AddString('hmnp');
    AddString('kawi');
    AddString('krai');
    AddString('mymrepka');
    AddString('mymrpao');
    AddString('nagm');
    AddString('onao');
    AddString('outlined');
    AddString('segment');
    AddString('sunu');
    AddString('tnsa');
    AddString('tols');
  end
  else if Key = 'timeZone' then
  begin
    TimeZones := GetAvailablePrimaryTimeZoneIdentifiers;
    for I := 0 to Length(TimeZones) - 1 do
      AddString(TimeZones[I]);
  end
  else if Key = 'unit' then
  begin
    AddString('acre');
    AddString('bit');
    AddString('byte');
    AddString('celsius');
    AddString('centimeter');
    AddString('day');
    AddString('degree');
    AddString('fahrenheit');
    AddString('fluid-ounce');
    AddString('foot');
    AddString('gallon');
    AddString('gigabit');
    AddString('gigabyte');
    AddString('gram');
    AddString('hectare');
    AddString('hour');
    AddString('inch');
    AddString('kilobit');
    AddString('kilobyte');
    AddString('kilogram');
    AddString('kilometer');
    AddString('liter');
    AddString('megabit');
    AddString('megabyte');
    AddString('meter');
    AddString('microsecond');
    AddString('mile');
    AddString('mile-scandinavian');
    AddString('milliliter');
    AddString('millimeter');
    AddString('millisecond');
    AddString('minute');
    AddString('month');
    AddString('nanosecond');
    AddString('ounce');
    AddString('percent');
    AddString('petabyte');
    AddString('pound');
    AddString('second');
    AddString('stone');
    AddString('terabit');
    AddString('terabyte');
    AddString('week');
    AddString('yard');
    AddString('year');
  end
  else
    ThrowRangeError(Format('invalid key: "%s"', [Key]));

  SortResultStrings;
  Result := ResultArr;
end;

{ supportedLocalesOf — shared by all constructors }

function TGocciaIntlBuiltin.SupportedLocalesOfFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Canonical: TStringArray;
  Supported: TStringArray;
  ResultArr: TGocciaArrayValue;
  I: Integer;
begin
  ValidateLocaleMatcherOption(AArgs.GetElement(1));
  Canonical := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Supported := SupportedLocalesOf(Canonical);

  ResultArr := TGocciaArrayValue.Create;
  for I := 0 to High(Supported) do
    ResultArr.Elements.Add(TGocciaStringLiteralValue.Create(Supported[I]));

  Result := ResultArr;
end;

{ Locale }

function TGocciaIntlBuiltin.LocaleConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TagArg, OptionsArg: TGocciaValue;
  Tag: string;
  Options: TGocciaObjectValue;
begin
  RequireIntlConstructCall(AThisValue, 'Locale');

  if AArgs.Length < 1 then
    ThrowTypeError('Intl.Locale requires a locale tag argument');

  TagArg := AArgs.GetElement(0);
  if not ((TagArg is TGocciaStringLiteralValue) or
          (TagArg is TGocciaObjectValue)) then
    ThrowTypeError('Intl.Locale requires a string or object locale tag');

  Tag := TagArg.ToStringLiteral.Value;
  if Tag = '' then
    ThrowRangeError('invalid language tag: empty string');

  Options := nil;
  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if not IsUndefinedIntlValue(OptionsArg) then
      Options := ToObject(OptionsArg);
  end;

  Result := TGocciaIntlLocaleValue.Create(Tag, Options);
end;

function TGocciaIntlBuiltin.LocaleConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := LocaleConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'Locale');
end;

procedure TGocciaIntlBuiltin.RegisterLocale;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(LocaleConstructorFn, 'Locale', 1);
  ConstructorMethod.ConstructCallback := LocaleConstructFn;
  TGocciaIntlLocaleValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('Locale',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ Collator }

function CollatorLocaleArgumentToLocale(const AArg: TGocciaValue): string;
var
  Element: TGocciaValue;
  Tag, Canonical, FirstUnicodeExtensionValue: string;
  RequestedLocales: TStringArray;
  FirstUnicodeExtension, SecondUnicodeExtension: Integer;
  LowerTag, Tail: string;
begin
  // TODO: CollatorLocaleArgumentToLocale duplicates LocaleCompareArgumentToLocale;
  // extract both into a shared Intl locale parsing utility.
  Result := '';
  if (AArg is TGocciaUndefinedLiteralValue) or (AArg = nil) then
    Exit;

  if AArg is TGocciaStringLiteralValue then
    Tag := TGocciaStringLiteralValue(AArg).Value
  else if AArg is TGocciaArrayValue then
  begin
    if TGocciaArrayValue(AArg).GetLength = 0 then
      Exit;
    Element := TGocciaArrayValue(AArg).GetElement(0);
    if Element is TGocciaStringLiteralValue then
      Tag := TGocciaStringLiteralValue(Element).Value
    else if Element is TGocciaObjectValue then
      Tag := Element.ToStringLiteral.Value
    else
      ThrowTypeError('locales array elements must be strings or objects');
  end
  else if AArg is TGocciaObjectValue then
    Tag := AArg.ToStringLiteral.Value
  else
    ThrowTypeError('locales argument must be a string, object, array, or undefined');

  Canonical := CanonicalizeUnicodeLocaleId(Tag);
  if Canonical = '' then
  begin
    LowerTag := LowerCase(Tag);
    FirstUnicodeExtension := Pos('-u-', LowerTag);
    if FirstUnicodeExtension <> 0 then
    begin
      Tail := Copy(LowerTag, FirstUnicodeExtension + 3, MaxInt);
      SecondUnicodeExtension := Pos('-u-', Tail);
      if SecondUnicodeExtension <> 0 then
      begin
        SecondUnicodeExtension := FirstUnicodeExtension + 3 +
          SecondUnicodeExtension - 1;
        FirstUnicodeExtensionValue := Copy(LowerTag, FirstUnicodeExtension + 3,
          SecondUnicodeExtension - FirstUnicodeExtension - 3);
        if FirstUnicodeExtensionValue = 'va-posix' then
          Canonical := CanonicalizeUnicodeLocaleId(
            Copy(Tag, 1, SecondUnicodeExtension - 1));
      end;
    end;
    if Canonical = '' then
      ThrowRangeError(Format('invalid language tag: %s', [Tag]));
  end;
  SetLength(RequestedLocales, 1);
  RequestedLocales[0] := Canonical;
  Result := ResolveRequestedLocale(RequestedLocales);
end;

function TGocciaIntlBuiltin.CollatorConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := CollatorLocaleArgumentToLocale(AArgs.GetElement(0));
  Options := nil;
  if (AArgs.Length >= 2) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    Options := ToObject(AArgs.GetElement(1));
  Result := TGocciaIntlCollatorValue.Create(Locale, Options);
end;

function TGocciaIntlBuiltin.CollatorConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := CollatorConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'Collator');
end;

procedure TGocciaIntlBuiltin.RegisterCollator;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(CollatorConstructorFn, 'Collator', 0);
  ConstructorMethod.ConstructCallback := CollatorConstructFn;
  TGocciaIntlCollatorValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('Collator',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ NumberFormat }

function TGocciaIntlBuiltin.NumberFormatConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  LocalesArg, OptionsArg: TGocciaValue;
  Constructed: TGocciaValue;
begin
  if AArgs.Length >= 1 then
    LocalesArg := AArgs.GetElement(0)
  else
    LocalesArg := nil;
  if AArgs.Length >= 2 then
    OptionsArg := AArgs.GetElement(1)
  else
    OptionsArg := nil;
  Constructed := TGocciaIntlNumberFormatValue.CreateFromArguments(LocalesArg, OptionsArg);
  Result := ChainLegacyIntlConstructor('NumberFormat', AThisValue, Constructed);
  if not Assigned(Result) then
    Result := Constructed;
end;

function TGocciaIntlBuiltin.NumberFormatConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := NumberFormatConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'NumberFormat');
end;

procedure TGocciaIntlBuiltin.RegisterNumberFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(NumberFormatConstructorFn, 'NumberFormat', 0);
  ConstructorMethod.ConstructCallback := NumberFormatConstructFn;
  TGocciaIntlNumberFormatValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('NumberFormat',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ DateTimeFormat }

function TGocciaIntlBuiltin.DateTimeFormatConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Constructed: TGocciaValue;
  OptionsArg: TGocciaValue;
  Options: TGocciaObjectValue;
  RequestedLocales: TStringArray;
begin
  RequestedLocales := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Locale := ResolveRequestedLocale(RequestedLocales);
  Options := nil;
  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if not IsUndefinedIntlValue(OptionsArg) then
      Options := ToObject(OptionsArg);
  end;
  Constructed := TGocciaIntlDateTimeFormatValue.Create(Locale, Options);
  Result := ChainLegacyIntlConstructor('DateTimeFormat', AThisValue, Constructed);
  if not Assigned(Result) then
    Result := Constructed;
end;

function TGocciaIntlBuiltin.DateTimeFormatConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := DateTimeFormatConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'DateTimeFormat');
end;

procedure TGocciaIntlBuiltin.RegisterDateTimeFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DateTimeFormatConstructorFn, 'DateTimeFormat', 0);
  ConstructorMethod.ConstructCallback := DateTimeFormatConstructFn;
  TGocciaIntlDateTimeFormatValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('DateTimeFormat',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ PluralRules }

function TGocciaIntlBuiltin.PluralRulesConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
  RequestedLocales: TStringArray;
begin
  RequireIntlConstructCall(AThisValue, 'PluralRules');
  RequestedLocales := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Locale := ResolveRequestedLocale(RequestedLocales);
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlPluralRulesValue.Create(Locale, Options);
end;

function TGocciaIntlBuiltin.PluralRulesConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := PluralRulesConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'PluralRules');
end;

procedure TGocciaIntlBuiltin.RegisterPluralRules;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PluralRulesConstructorFn, 'PluralRules', 0);
  ConstructorMethod.ConstructCallback := PluralRulesConstructFn;
  TGocciaIntlPluralRulesValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('PluralRules',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ RelativeTimeFormat }

function TGocciaIntlBuiltin.RelativeTimeFormatConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
  OptionsArg: TGocciaValue;
  RequestedLocales: TStringArray;
begin
  RequireIntlConstructCall(AThisValue, 'RelativeTimeFormat');
  RequestedLocales := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Locale := ResolveRequestedLocale(RequestedLocales);
  Options := nil;
  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if not IsUndefinedIntlValue(OptionsArg) then
      Options := ToObject(OptionsArg);
  end;
  Result := TGocciaIntlRelativeTimeFormatValue.Create(Locale, Options);
end;

function TGocciaIntlBuiltin.RelativeTimeFormatConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := RelativeTimeFormatConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'RelativeTimeFormat');
end;

procedure TGocciaIntlBuiltin.RegisterRelativeTimeFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(RelativeTimeFormatConstructorFn, 'RelativeTimeFormat', 0);
  ConstructorMethod.ConstructCallback := RelativeTimeFormatConstructFn;
  TGocciaIntlRelativeTimeFormatValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('RelativeTimeFormat',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ ListFormat }

function TGocciaIntlBuiltin.ListFormatConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
  RequestedLocales: TStringArray;
begin
  RequireIntlConstructCall(AThisValue, 'ListFormat');

  RequestedLocales := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Locale := ResolveRequestedLocale(RequestedLocales);

  Options := nil;
  if not IsUndefinedIntlValue(AArgs.GetElement(1)) then
  begin
    if not (AArgs.GetElement(1) is TGocciaObjectValue) then
      ThrowTypeError('Intl.ListFormat options must be an object');
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  end;
  Result := TGocciaIntlListFormatValue.Create(Locale, Options);
end;

function TGocciaIntlBuiltin.ListFormatConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := ListFormatConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'ListFormat');
end;

procedure TGocciaIntlBuiltin.RegisterListFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(ListFormatConstructorFn, 'ListFormat', 0);
  ConstructorMethod.ConstructCallback := ListFormatConstructFn;
  TGocciaIntlListFormatValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('ListFormat',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ DisplayNames }

function TGocciaIntlBuiltin.DisplayNamesConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
  RequestedLocales: TStringArray;
begin
  RequireIntlConstructCall(AThisValue, 'DisplayNames');
  RequestedLocales := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Locale := ResolveRequestedLocale(RequestedLocales);
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlDisplayNamesValue.Create(Locale, Options);
end;

function TGocciaIntlBuiltin.DisplayNamesConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  PrototypeValue: TGocciaObjectValue;
begin
  PrototypeValue := IntlPrototypeFromNewTarget(ANewTarget, 'DisplayNames');
  Result := DisplayNamesConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  if Result is TGocciaObjectValue then
    TGocciaObjectValue(Result).Prototype := PrototypeValue;
end;

procedure TGocciaIntlBuiltin.RegisterDisplayNames;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DisplayNamesConstructorFn, 'DisplayNames', 2);
  ConstructorMethod.ConstructCallback := DisplayNamesConstructFn;
  TGocciaIntlDisplayNamesValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('DisplayNames',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ Segmenter }

function TGocciaIntlBuiltin.SegmenterConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
  OptionsArg: TGocciaValue;
  RequestedLocales: TStringArray;
begin
  RequireIntlConstructCall(AThisValue, 'Segmenter');
  RequestedLocales := CanonicalizeLocaleListFromValue(AArgs.GetElement(0));
  Locale := ResolveRequestedLocale(RequestedLocales);
  Options := nil;
  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if not IsUndefinedIntlValue(OptionsArg) then
      Options := ToObject(OptionsArg);
  end;
  Result := TGocciaIntlSegmenterValue.Create(Locale, Options);
end;

function TGocciaIntlBuiltin.SegmenterConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := SegmenterConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'Segmenter');
end;

procedure TGocciaIntlBuiltin.RegisterSegmenter;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(SegmenterConstructorFn, 'Segmenter', 0);
  ConstructorMethod.ConstructCallback := SegmenterConstructFn;
  TGocciaIntlSegmenterValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('Segmenter',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

{ DurationFormat }

function TGocciaIntlBuiltin.DurationFormatConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  LocalesArg, OptionsArg: TGocciaValue;
begin
  RequireIntlConstructCall(AThisValue, 'DurationFormat');
  if AArgs.Length >= 1 then
    LocalesArg := AArgs.GetElement(0)
  else
    LocalesArg := nil;
  if AArgs.Length >= 2 then
    OptionsArg := AArgs.GetElement(1)
  else
    OptionsArg := nil;
  Result := TGocciaIntlDurationFormatValue.CreateFromArguments(LocalesArg, OptionsArg);
end;

function TGocciaIntlBuiltin.DurationFormatConstructFn(const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := DurationFormatConstructorFn(AArgs, TGocciaHoleValue.HoleValue);
  ApplyIntlConstructPrototype(Result, ANewTarget, 'DurationFormat');
end;

procedure TGocciaIntlBuiltin.RegisterDurationFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DurationFormatConstructorFn, 'DurationFormat', 0);
  ConstructorMethod.ConstructCallback := DurationFormatConstructFn;
  TGocciaIntlDurationFormatValue.ExposePrototype(ConstructorMethod);
  ConstructorMethod.DefineProperty('supportedLocalesOf',
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SupportedLocalesOfFn, 'supportedLocalesOf', 1),
      [pfConfigurable, pfWritable]));
  FIntlNamespace.DefineProperty('DurationFormat',
    TGocciaPropertyDescriptorData.Create(ConstructorMethod, [pfConfigurable, pfWritable]));
end;

end.
