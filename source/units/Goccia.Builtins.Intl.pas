unit Goccia.Builtins.Intl;

{$I Goccia.inc}

interface

uses
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

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
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
  Goccia.Values.SymbolValue;

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

{ Intl.getCanonicalLocales }

function TGocciaIntlBuiltin.GetCanonicalLocales(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  Tags: TStringArray;
  Canonical: TStringArray;
  ResultArr: TGocciaArrayValue;
  I, Len: Integer;
  Tag: string;
begin
  Arg := AArgs.GetElement(0);

  if Arg is TGocciaArrayValue then
  begin
    Len := TGocciaArrayValue(Arg).GetLength;
    SetLength(Tags, Len);
    for I := 0 to Len - 1 do
      Tags[I] := TGocciaArrayValue(Arg).GetElement(I).ToStringLiteral.Value;
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    SetLength(Tags, 1);
    Tags[0] := TGocciaStringLiteralValue(Arg).Value;
  end
  else if (Arg is TGocciaUndefinedLiteralValue) or (Arg = nil) then
  begin
    SetLength(Tags, 0);
  end
  else
  begin
    ThrowTypeError('locales argument must be a string or an array');
    Result := nil;
    Exit;
  end;

  // Validate each tag before canonicalization
  for I := 0 to High(Tags) do
  begin
    Tag := CanonicalizeUnicodeLocaleId(Tags[I]);
    if Tag = '' then
      ThrowRangeError(Format('invalid language tag: %s', [Tags[I]]));
  end;

  Canonical := CanonicalizeLocaleList(Tags);

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
  ResultArr: TGocciaArrayValue;

  procedure AddString(const AValue: string);
  begin
    ResultArr.Elements.Add(TGocciaStringLiteralValue.Create(AValue));
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
    AddString('islamic');
    AddString('islamic-civil');
    AddString('islamic-rgsa');
    AddString('islamic-tbla');
    AddString('islamic-umalqura');
    AddString('iso8601');
    AddString('japanese');
    AddString('persian');
    AddString('roc');
  end
  else if Key = 'collation' then
  begin
    AddString('big5han');
    AddString('compat');
    AddString('dict');
    AddString('direct');
    AddString('ducet');
    AddString('emoji');
    AddString('eor');
    AddString('gb2312');
    AddString('phonebk');
    AddString('phonetic');
    AddString('pinyin');
    AddString('reformed');
    AddString('search');
    AddString('searchjl');
    AddString('standard');
    AddString('stroke');
    AddString('trad');
    AddString('unihan');
    AddString('zhuyin');
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
  end
  else if Key = 'timeZone' then
  begin
    AddString('Africa/Abidjan');
    AddString('Africa/Cairo');
    AddString('Africa/Casablanca');
    AddString('Africa/Johannesburg');
    AddString('Africa/Lagos');
    AddString('Africa/Nairobi');
    AddString('America/Anchorage');
    AddString('America/Argentina/Buenos_Aires');
    AddString('America/Bogota');
    AddString('America/Chicago');
    AddString('America/Denver');
    AddString('America/Halifax');
    AddString('America/Los_Angeles');
    AddString('America/Mexico_City');
    AddString('America/New_York');
    AddString('America/Phoenix');
    AddString('America/Santiago');
    AddString('America/Sao_Paulo');
    AddString('America/St_Johns');
    AddString('America/Toronto');
    AddString('America/Vancouver');
    AddString('Asia/Baghdad');
    AddString('Asia/Bangkok');
    AddString('Asia/Calcutta');
    AddString('Asia/Dhaka');
    AddString('Asia/Dubai');
    AddString('Asia/Hong_Kong');
    AddString('Asia/Istanbul');
    AddString('Asia/Jakarta');
    AddString('Asia/Jerusalem');
    AddString('Asia/Karachi');
    AddString('Asia/Kathmandu');
    AddString('Asia/Riyadh');
    AddString('Asia/Seoul');
    AddString('Asia/Shanghai');
    AddString('Asia/Singapore');
    AddString('Asia/Taipei');
    AddString('Asia/Tehran');
    AddString('Asia/Tokyo');
    AddString('Atlantic/Reykjavik');
    AddString('Australia/Melbourne');
    AddString('Australia/Perth');
    AddString('Australia/Sydney');
    AddString('Europe/Amsterdam');
    AddString('Europe/Athens');
    AddString('Europe/Belgrade');
    AddString('Europe/Berlin');
    AddString('Europe/Brussels');
    AddString('Europe/Bucharest');
    AddString('Europe/Budapest');
    AddString('Europe/Copenhagen');
    AddString('Europe/Dublin');
    AddString('Europe/Helsinki');
    AddString('Europe/Kyiv');
    AddString('Europe/Lisbon');
    AddString('Europe/London');
    AddString('Europe/Madrid');
    AddString('Europe/Moscow');
    AddString('Europe/Oslo');
    AddString('Europe/Paris');
    AddString('Europe/Prague');
    AddString('Europe/Rome');
    AddString('Europe/Stockholm');
    AddString('Europe/Vienna');
    AddString('Europe/Warsaw');
    AddString('Europe/Zurich');
    AddString('Pacific/Auckland');
    AddString('Pacific/Fiji');
    AddString('Pacific/Honolulu');
    AddString('UTC');
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

  Result := ResultArr;
end;

{ supportedLocalesOf — shared by all constructors }

function TGocciaIntlBuiltin.SupportedLocalesOfFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg, OptionsArg, V: TGocciaValue;
  Tags: TStringArray;
  Canonical: TStringArray;
  Available: TStringArray;
  AvailableTags: TStringArray;
  ResultArr: TGocciaArrayValue;
  I, Len, AvailCount: Integer;
  AvailTag: string;
begin
  Arg := AArgs.GetElement(0);

  // Validate localeMatcher option for NUL characters
  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if Assigned(OptionsArg) and (OptionsArg is TGocciaObjectValue) then
    begin
      V := TGocciaObjectValue(OptionsArg).GetProperty('localeMatcher');
      if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      begin
        if ContainsNulCharacter(V.ToStringLiteral.Value) then
          ThrowRangeError(Format(SErrorIntlInvalidOption, [V.ToStringLiteral.Value, 'localeMatcher']));
      end;
    end;
  end;

  if Arg is TGocciaArrayValue then
  begin
    Len := TGocciaArrayValue(Arg).GetLength;
    SetLength(Tags, Len);
    for I := 0 to Len - 1 do
      Tags[I] := TGocciaArrayValue(Arg).GetElement(I).ToStringLiteral.Value;
  end
  else if Arg is TGocciaStringLiteralValue then
  begin
    SetLength(Tags, 1);
    Tags[0] := TGocciaStringLiteralValue(Arg).Value;
  end
  else if (Arg is TGocciaUndefinedLiteralValue) or (Arg = nil) then
  begin
    SetLength(Tags, 0);
  end
  else
  begin
    ThrowTypeError('locales argument must be a string or an array');
    Result := nil;
    Exit;
  end;

  Canonical := CanonicalizeLocaleList(Tags);

  // Get available locales from ICU and convert to BCP 47 tags
  SetLength(AvailableTags, 0);
  if TryICUGetAvailableLocales(Available) then
  begin
    AvailCount := 0;
    SetLength(AvailableTags, Length(Available));
    for I := 0 to High(Available) do
    begin
      if TryICUCanonicalizeLocale(Available[I], AvailTag) then
      begin
        AvailableTags[AvailCount] := AvailTag;
        Inc(AvailCount);
      end;
    end;
    SetLength(AvailableTags, AvailCount);
  end;

  ResultArr := TGocciaArrayValue.Create;
  for I := 0 to High(Canonical) do
  begin
    if BestAvailableLocale(AvailableTags, Canonical[I]) <> '' then
      ResultArr.Elements.Add(TGocciaStringLiteralValue.Create(Canonical[I]));
  end;

  Result := ResultArr;
end;

{ Locale }

function TGocciaIntlBuiltin.LocaleConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Tag: string;
  Options: TGocciaObjectValue;
begin
  // Intl.Locale must be called with new
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Intl.Locale cannot be called without new');

  if AArgs.Length < 1 then
    ThrowTypeError('Intl.Locale requires a locale tag argument');

  Tag := AArgs.GetElement(0).ToStringLiteral.Value;
  if Tag = '' then
    ThrowRangeError('invalid language tag: empty string');

  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));

  Result := TGocciaIntlLocaleValue.Create(Tag, Options);
end;

procedure TGocciaIntlBuiltin.RegisterLocale;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(LocaleConstructorFn, 'Locale', 1);
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

function TGocciaIntlBuiltin.CollatorConstructorFn(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Locale: string;
  Options: TGocciaObjectValue;
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlCollatorValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterCollator;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(CollatorConstructorFn, 'Collator', 0);
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
  Locale: string;
  Options: TGocciaObjectValue;
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlNumberFormatValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterNumberFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(NumberFormatConstructorFn, 'NumberFormat', 0);
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
  Options: TGocciaObjectValue;
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlDateTimeFormatValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterDateTimeFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DateTimeFormatConstructorFn, 'DateTimeFormat', 0);
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
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlPluralRulesValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterPluralRules;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(PluralRulesConstructorFn, 'PluralRules', 0);
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
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlRelativeTimeFormatValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterRelativeTimeFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(RelativeTimeFormatConstructorFn, 'RelativeTimeFormat', 0);
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
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlListFormatValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterListFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(ListFormatConstructorFn, 'ListFormat', 0);
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
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlDisplayNamesValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterDisplayNames;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DisplayNamesConstructorFn, 'DisplayNames', 2);
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
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlSegmenterValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterSegmenter;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(SegmenterConstructorFn, 'Segmenter', 0);
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
  Locale: string;
  Options: TGocciaObjectValue;
begin
  Locale := '';
  if AArgs.Length >= 1 then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  Options := nil;
  if (AArgs.Length >= 2) and (AArgs.GetElement(1) is TGocciaObjectValue) then
    Options := TGocciaObjectValue(AArgs.GetElement(1));
  Result := TGocciaIntlDurationFormatValue.Create(Locale, Options);
end;

procedure TGocciaIntlBuiltin.RegisterDurationFormat;
var
  ConstructorMethod: TGocciaNativeFunctionValue;
begin
  ConstructorMethod := TGocciaNativeFunctionValue.Create(DurationFormatConstructorFn, 'DurationFormat', 0);
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
