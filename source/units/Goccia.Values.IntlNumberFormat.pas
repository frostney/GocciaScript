unit Goccia.Values.IntlNumberFormat;

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
  TGocciaIntlNumberFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FStyle: string;
    FCurrency: string;
    FCurrencyDisplay: string;
    FCurrencySign: string;
    FUnitIdentifier: string;
    FUnitDisplay: string;
    FNotation: string;
    FCompactDisplay: string;
    FSignDisplay: string;
    FUseGrouping: string;
    FRoundingMode: string;
    FMinimumIntegerDigits: Integer;
    FMinimumFractionDigits: Integer;
    FMaximumFractionDigits: Integer;
    FMinimumSignificantDigits: Integer;
    FMaximumSignificantDigits: Integer;
    FRoundingIncrement: Integer;
    FRoundingPriority: string;
    FTrailingZeroDisplay: string;
    FNumberingSystem: string;
    FNumberingSystemOptionPresent: Boolean;
    FResolvedOptions: TIntlNumberFormatOptions;
    FBoundFormat: TGocciaValue;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);
    class function CreateFromArguments(const ALocales, AOptions: TGocciaValue): TGocciaIntlNumberFormatValue; static;

    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlNumberFormatFormatGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatFormatRangeToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlNumberFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,
  SysUtils,

  IntlICU,
  IntlLocaleResolver,

  Goccia.Error.Messages,
  Goccia.GarbageCollector,
  Goccia.Intl.CLDRData,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntObjectValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlLocale,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ProxyValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

var
  GIntlNumberFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

type
  TGocciaIntlNumberFormatBoundFormatValue = class(TGocciaNativeFunctionValue)
  private
    FNumberFormat: TGocciaIntlNumberFormatValue;
    function Format(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ANumberFormat: TGocciaIntlNumberFormatValue);
    procedure MarkReferences; override;
  end;

function GetIntlNumberFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlNumberFormatSharedSlot))
  else
    Result := nil;
end;

function AsNumberFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlNumberFormatValue;
var
  FallbackValue: TGocciaValue;
begin
  if AValue is TGocciaIntlNumberFormatValue then
    Exit(TGocciaIntlNumberFormatValue(AValue));

  if AValue is TGocciaObjectValue then
  begin
    FallbackValue := TGocciaObjectValue(AValue).GetSymbolProperty(
      IntlFallbackSymbol);
    if FallbackValue is TGocciaIntlNumberFormatValue then
      Exit(TGocciaIntlNumberFormatValue(FallbackValue));
  end;

  ThrowTypeError(AMethod + ' called on non-NumberFormat');
  Result := nil;
end;

function LegacyConstructedNumberFormatSymbol(
  const AValue: TGocciaValue): TGocciaSymbolValue;
var
  Obj: TGocciaObjectValue;
  Symbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  Result := nil;
  if AValue is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(AValue).Target is TGocciaObjectValue then
      Obj := TGocciaObjectValue(TGocciaProxyValue(AValue).Target)
    else
      Exit;
  end
  else if AValue is TGocciaObjectValue then
    Obj := TGocciaObjectValue(AValue)
  else
    Exit;

  Symbols := Obj.GetOwnSymbols;
  for I := Low(Symbols) to High(Symbols) do
    if Symbols[I].Description = 'IntlLegacyConstructedSymbol' then
      Exit(Symbols[I]);
end;

function UnwrapNumberFormat(const AValue: TGocciaValue;
  const AMethod: string): TGocciaIntlNumberFormatValue;
var
  Obj: TGocciaObjectValue;
  FallbackSymbol: TGocciaSymbolValue;
  FallbackValue: TGocciaValue;
begin
  if AValue is TGocciaIntlNumberFormatValue then
    Exit(TGocciaIntlNumberFormatValue(AValue));

  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' called on non-NumberFormat');

  Obj := TGocciaObjectValue(AValue);
  FallbackSymbol := LegacyConstructedNumberFormatSymbol(AValue);
  if Assigned(FallbackSymbol) then
  begin
    FallbackValue := Obj.GetSymbolPropertyWithReceiver(FallbackSymbol, AValue);
    if FallbackValue is TGocciaIntlNumberFormatValue then
      Exit(TGocciaIntlNumberFormatValue(FallbackValue));
  end;

  ThrowTypeError(AMethod + ' called on non-NumberFormat');
  Result := nil;
end;

function IsAllowedStringOptionValue(const AValue: string;
  const AAllowedValues: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AAllowedValues) to High(AAllowedValues) do
    if AValue = AAllowedValues[I] then
      Exit(True);
end;

function TryReadNumberFormatStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; const AAllowedValues: array of string;
  var AValue: string): Boolean;
var
  V: TGocciaValue;
  S: string;
begin
  Result := False;
  V := AOptions.GetProperty(AName);
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    Exit;

  S := V.ToStringLiteral.Value;
  if ContainsNulCharacter(S) or
     not IsAllowedStringOptionValue(S, AAllowedValues) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [S, AName]));

  AValue := S;
  Result := True;
end;

function TryReadNumberFormatIntegerOption(const AOptions: TGocciaObjectValue;
  const AName: string; const AMinimum, AMaximum: Integer;
  var AValue: Integer): Boolean;
var
  V: TGocciaValue;
  NumberValue: TGocciaNumberLiteralValue;
begin
  Result := False;
  V := AOptions.GetProperty(AName);
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    Exit;

  NumberValue := V.ToNumberLiteral;
  if NumberValue.IsNaN or NumberValue.IsInfinity or
     NumberValue.IsNegativeInfinity or
     (NumberValue.Value < AMinimum) or (NumberValue.Value > AMaximum) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange,
      [AName, AMinimum, AMaximum]));

  AValue := Trunc(NumberValue.Value);
  Result := True;
end;

function IsWellFormedUnicodeTypeIdentifier(const AValue: string): Boolean;
var
  I, SubtagLength: Integer;
  C: Char;
begin
  Result := False;
  if AValue = '' then
    Exit;

  SubtagLength := 0;
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    if C = '-' then
    begin
      if (SubtagLength < 3) or (SubtagLength > 8) then
        Exit;
      SubtagLength := 0;
    end
    else if C in ['A'..'Z', 'a'..'z', '0'..'9'] then
      Inc(SubtagLength)
    else
      Exit;
  end;

  Result := (SubtagLength >= 3) and (SubtagLength <= 8);
end;

function IsAsciiAlpha(const C: Char): Boolean; inline;
begin
  Result := (C in ['A'..'Z']) or (C in ['a'..'z']);
end;

function AsciiUpper(const AValue: string): string;
var
  I: Integer;
begin
  Result := AValue;
  for I := 1 to Length(Result) do
    if Result[I] in ['a'..'z'] then
      Result[I] := Chr(Ord(Result[I]) - Ord('a') + Ord('A'));
end;

function AsciiLower(const AValue: string): string;
var
  I: Integer;
begin
  Result := AValue;
  for I := 1 to Length(Result) do
    if Result[I] in ['A'..'Z'] then
      Result[I] := Chr(Ord(Result[I]) - Ord('A') + Ord('a'));
end;

function IsWellFormedCurrencyCode(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := Length(AValue) = 3;
  if not Result then
    Exit;
  for I := 1 to Length(AValue) do
    if not IsAsciiAlpha(AValue[I]) then
      Exit(False);
end;

function UTF8FromCodePoint(const ACodePoint: Cardinal): string;
begin
  if ACodePoint <= $7F then
    Result := Chr(ACodePoint)
  else if ACodePoint <= $7FF then
    Result := Chr($C0 or (ACodePoint shr 6)) +
      Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $FFFF then
    Result := Chr($E0 or (ACodePoint shr 12)) +
      Chr($80 or ((ACodePoint shr 6) and $3F)) +
      Chr($80 or (ACodePoint and $3F))
  else
    Result := Chr($F0 or (ACodePoint shr 18)) +
      Chr($80 or ((ACodePoint shr 12) and $3F)) +
      Chr($80 or ((ACodePoint shr 6) and $3F)) +
      Chr($80 or (ACodePoint and $3F));
end;

function TryGetSimpleNumberingSystemZero(const AValue: string;
  out ACodePoint: Cardinal): Boolean;
begin
  Result := True;
  if AValue = 'adlm' then ACodePoint := $1E950
  else if AValue = 'ahom' then ACodePoint := $11730
  else if AValue = 'arab' then ACodePoint := $660
  else if AValue = 'arabext' then ACodePoint := $6F0
  else if AValue = 'bali' then ACodePoint := $1B50
  else if AValue = 'beng' then ACodePoint := $9E6
  else if AValue = 'bhks' then ACodePoint := $11C50
  else if AValue = 'brah' then ACodePoint := $11066
  else if AValue = 'cakm' then ACodePoint := $11136
  else if AValue = 'cham' then ACodePoint := $AA50
  else if AValue = 'deva' then ACodePoint := $966
  else if AValue = 'diak' then ACodePoint := $11950
  else if AValue = 'fullwide' then ACodePoint := $FF10
  else if AValue = 'gara' then ACodePoint := $10D40
  else if AValue = 'gong' then ACodePoint := $11DA0
  else if AValue = 'gonm' then ACodePoint := $11D50
  else if AValue = 'gujr' then ACodePoint := $AE6
  else if AValue = 'gukh' then ACodePoint := $16130
  else if AValue = 'guru' then ACodePoint := $A66
  else if AValue = 'hmng' then ACodePoint := $16B50
  else if AValue = 'hmnp' then ACodePoint := $1E140
  else if AValue = 'java' then ACodePoint := $A9D0
  else if AValue = 'kali' then ACodePoint := $A900
  else if AValue = 'kawi' then ACodePoint := $11F50
  else if AValue = 'khmr' then ACodePoint := $17E0
  else if AValue = 'knda' then ACodePoint := $CE6
  else if AValue = 'krai' then ACodePoint := $16D70
  else if AValue = 'lana' then ACodePoint := $1A80
  else if AValue = 'lanatham' then ACodePoint := $1A90
  else if AValue = 'laoo' then ACodePoint := $ED0
  else if AValue = 'latn' then ACodePoint := $30
  else if AValue = 'lepc' then ACodePoint := $1C40
  else if AValue = 'limb' then ACodePoint := $1946
  else if AValue = 'mathbold' then ACodePoint := $1D7CE
  else if AValue = 'mathdbl' then ACodePoint := $1D7D8
  else if AValue = 'mathmono' then ACodePoint := $1D7F6
  else if AValue = 'mathsanb' then ACodePoint := $1D7EC
  else if AValue = 'mathsans' then ACodePoint := $1D7E2
  else if AValue = 'mlym' then ACodePoint := $D66
  else if AValue = 'modi' then ACodePoint := $11650
  else if AValue = 'mong' then ACodePoint := $1810
  else if AValue = 'mroo' then ACodePoint := $16A60
  else if AValue = 'mtei' then ACodePoint := $ABF0
  else if AValue = 'mymr' then ACodePoint := $1040
  else if AValue = 'mymrepka' then ACodePoint := $116DA
  else if AValue = 'mymrpao' then ACodePoint := $116D0
  else if AValue = 'mymrshan' then ACodePoint := $1090
  else if AValue = 'mymrtlng' then ACodePoint := $A9F0
  else if AValue = 'nagm' then ACodePoint := $1E4F0
  else if AValue = 'newa' then ACodePoint := $11450
  else if AValue = 'nkoo' then ACodePoint := $7C0
  else if AValue = 'olck' then ACodePoint := $1C50
  else if AValue = 'onao' then ACodePoint := $1E5F1
  else if AValue = 'orya' then ACodePoint := $B66
  else if AValue = 'osma' then ACodePoint := $104A0
  else if AValue = 'outlined' then ACodePoint := $1CCF0
  else if AValue = 'rohg' then ACodePoint := $10D30
  else if AValue = 'saur' then ACodePoint := $A8D0
  else if AValue = 'segment' then ACodePoint := $1FBF0
  else if AValue = 'shrd' then ACodePoint := $111D0
  else if AValue = 'sind' then ACodePoint := $112F0
  else if AValue = 'sinh' then ACodePoint := $DE6
  else if AValue = 'sora' then ACodePoint := $110F0
  else if AValue = 'sund' then ACodePoint := $1BB0
  else if AValue = 'sunu' then ACodePoint := $11BF0
  else if AValue = 'takr' then ACodePoint := $116C0
  else if AValue = 'talu' then ACodePoint := $19D0
  else if AValue = 'tamldec' then ACodePoint := $BE6
  else if AValue = 'telu' then ACodePoint := $C66
  else if AValue = 'thai' then ACodePoint := $E50
  else if AValue = 'tibt' then ACodePoint := $F20
  else if AValue = 'tirh' then ACodePoint := $114D0
  else if AValue = 'tnsa' then ACodePoint := $16AC0
  else if AValue = 'tols' then ACodePoint := $11DE0
  else if AValue = 'vaii' then ACodePoint := $A620
  else if AValue = 'wara' then ACodePoint := $118E0
  else if AValue = 'wcho' then ACodePoint := $1E2F0
  else
    Result := False;
end;

function IsSupportedNumberFormatNumberingSystem(const AValue: string): Boolean;
var
  CodePoint: Cardinal;
begin
  Result := IsSupportedNumberingSystem(AValue) or
    TryGetSimpleNumberingSystemZero(AValue, CodePoint) or
    (AValue = 'hanidec');
end;

function SimpleNumberingSystemDigit(const ANumberingSystem: string;
  const ADigit: Integer): string;
var
  CodePoint: Cardinal;
const
  HanidecDigits: array[0..9] of Cardinal = (
    $3007, $4E00, $4E8C, $4E09, $56DB, $4E94, $516D, $4E03, $516B, $4E5D);
begin
  if (ADigit < 0) or (ADigit > 9) then
    Exit(IntToStr(ADigit));
  if ANumberingSystem = 'hanidec' then
    Exit(UTF8FromCodePoint(HanidecDigits[ADigit]));
  if TryGetSimpleNumberingSystemZero(ANumberingSystem, CodePoint) then
    Exit(UTF8FromCodePoint(CodePoint + Cardinal(ADigit)));
  Result := IntToStr(ADigit);
end;

function ApplySimpleNumberingSystemDigits(const AFormatted,
  ANumberingSystem: string): string;
var
  I: Integer;
begin
  if (ANumberingSystem = '') or (ANumberingSystem = 'latn') then
    Exit(AFormatted);
  Result := '';
  for I := 1 to Length(AFormatted) do
  begin
    if AFormatted[I] in ['0'..'9'] then
      Result := Result + SimpleNumberingSystemDigit(ANumberingSystem,
        Ord(AFormatted[I]) - Ord('0'))
    else
      Result := Result + AFormatted[I];
  end;
end;

function IsSanctionedSimpleUnitIdentifier(const AValue: string): Boolean;
const
  SanctionedUnits: array[0..44] of string = (
    'acre', 'bit', 'byte', 'celsius', 'centimeter', 'day', 'degree',
    'fahrenheit', 'fluid-ounce', 'foot', 'gallon', 'gigabit', 'gigabyte',
    'gram', 'hectare', 'hour', 'inch', 'kilobit', 'kilobyte', 'kilogram',
    'kilometer', 'liter', 'megabit', 'megabyte', 'meter', 'microsecond',
    'mile', 'mile-scandinavian', 'milliliter', 'millimeter', 'millisecond',
    'minute', 'month', 'nanosecond', 'ounce', 'percent', 'petabyte',
    'pound', 'second', 'stone', 'terabit', 'terabyte', 'week', 'yard',
    'year');
var
  I: Integer;
begin
  Result := False;
  for I := Low(SanctionedUnits) to High(SanctionedUnits) do
    if AValue = SanctionedUnits[I] then
      Exit(True);
end;

function IsWellFormedUnitIdentifier(const AValue: string): Boolean;
var
  Separator: Integer;
  Numerator, Denominator: string;
begin
  Separator := Pos('-per-', AValue);
  if Separator = 0 then
    Exit(IsSanctionedSimpleUnitIdentifier(AValue));

  Numerator := Copy(AValue, 1, Separator - 1);
  Denominator := Copy(AValue, Separator + Length('-per-'), MaxInt);
  Result := (Pos('-per-', Denominator) = 0) and
    IsSanctionedSimpleUnitIdentifier(Numerator) and
    IsSanctionedSimpleUnitIdentifier(Denominator);
end;

function ReadUseGroupingOption(const AOptions: TGocciaObjectValue;
  const ADefaultValue: string): string;
var
  V: TGocciaValue;
  S: string;
begin
  V := AOptions.GetProperty('useGrouping');
  if not Assigned(V) or (V is TGocciaUndefinedLiteralValue) then
    Exit(ADefaultValue);

  if (V is TGocciaBooleanLiteralValue) and
     TGocciaBooleanLiteralValue(V).Value then
    Exit('always');

  if not V.ToBooleanLiteral.Value then
    Exit('false');

  S := V.ToStringLiteral.Value;
  if (S = 'true') or (S = 'false') then
    Exit(ADefaultValue);
  if (S = 'min2') or (S = 'auto') or (S = 'always') then
    Exit(S);

  ThrowRangeError(Format(SErrorIntlInvalidOption, [S, 'useGrouping']));
  Result := ADefaultValue;
end;

{ TGocciaIntlNumberFormatBoundFormatValue }

constructor TGocciaIntlNumberFormatBoundFormatValue.Create(
  const ANumberFormat: TGocciaIntlNumberFormatValue);
begin
  inherited CreateWithoutPrototype(Format, '', 1);
  FNumberFormat := ANumberFormat;
end;

function TGocciaIntlNumberFormatBoundFormatValue.Format(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FNumberFormat.IntlNumberFormatFormat(AArgs, FNumberFormat);
end;

procedure TGocciaIntlNumberFormatBoundFormatValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FNumberFormat) then
    FNumberFormat.MarkReferences;
end;

function StyleStringToEnum(const AValue: string): TIntlNumberStyle;
begin
  if AValue = 'currency' then
    Result := insCurrency
  else if AValue = 'percent' then
    Result := insPercent
  else if AValue = 'unit' then
    Result := insUnit
  else
    Result := insDecimal;
end;

function CurrencyDisplayStringToEnum(const AValue: string): TIntlNumberCurrencyDisplay;
begin
  if AValue = 'narrowSymbol' then
    Result := incdNarrowSymbol
  else if AValue = 'code' then
    Result := incdCode
  else if AValue = 'name' then
    Result := incdName
  else
    Result := incdSymbol;
end;

function CurrencySignStringToEnum(const AValue: string): TIntlNumberCurrencySign;
begin
  if AValue = 'accounting' then
    Result := incsAccounting
  else
    Result := incsStandard;
end;

function SignDisplayStringToEnum(const AValue: string): TIntlNumberSignDisplay;
begin
  if AValue = 'never' then
    Result := insdNever
  else if AValue = 'always' then
    Result := insdAlways
  else if AValue = 'exceptZero' then
    Result := insdExceptZero
  else if AValue = 'negative' then
    Result := insdNegative
  else
    Result := insdAuto;
end;

function NotationStringToEnum(const AValue: string): TIntlNumberNotation;
begin
  if AValue = 'scientific' then
    Result := innScientific
  else if AValue = 'engineering' then
    Result := innEngineering
  else if AValue = 'compact' then
    Result := innCompact
  else
    Result := innStandard;
end;

function CompactDisplayStringToEnum(const AValue: string): TIntlNumberCompactDisplay;
begin
  if AValue = 'long' then
    Result := incdLong
  else
    Result := incdShort;
end;

function UnitDisplayStringToEnum(const AValue: string): TIntlNumberUnitDisplay;
begin
  if AValue = 'long' then
    Result := inudLong
  else if AValue = 'narrow' then
    Result := inudNarrow
  else
    Result := inudShort;
end;

function UseGroupingStringToEnum(const AValue: string): TIntlNumberUseGrouping;
begin
  if AValue = 'always' then
    Result := inugAlways
  else if AValue = 'min2' then
    Result := inugMin2
  else if (AValue = 'false') or (AValue = '') then
    Result := inugFalse
  else
    Result := inugAuto;
end;

function RoundingModeStringToEnum(const AValue: string): TIntlNumberRoundingMode;
begin
  if AValue = 'ceil' then
    Result := inrmCeil
  else if AValue = 'floor' then
    Result := inrmFloor
  else if AValue = 'expand' then
    Result := inrmExpand
  else if AValue = 'trunc' then
    Result := inrmTrunc
  else if AValue = 'halfCeil' then
    Result := inrmHalfCeil
  else if AValue = 'halfFloor' then
    Result := inrmHalfFloor
  else if AValue = 'halfTrunc' then
    Result := inrmHalfTrunc
  else if AValue = 'halfEven' then
    Result := inrmHalfEven
  else
    Result := inrmHalfExpand;
end;

function RoundingPriorityStringToEnum(const AValue: string): TIntlNumberRoundingPriority;
begin
  if AValue = 'morePrecision' then
    Result := inrpMorePrecision
  else if AValue = 'lessPrecision' then
    Result := inrpLessPrecision
  else
    Result := inrpAuto;
end;

function TrailingZeroDisplayStringToEnum(
  const AValue: string): TIntlNumberTrailingZeroDisplay;
begin
  if AValue = 'stripIfInteger' then
    Result := intzStripIfInteger
  else
    Result := intzAuto;
end;

function InsertGroupingSeparator(const AIntPart, ASep: string): string;
var
  Len, I, GroupCount: Integer;
begin
  Len := Length(AIntPart);
  if Len <= 3 then
  begin
    Result := AIntPart;
    Exit;
  end;

  Result := '';
  GroupCount := 0;
  for I := Len downto 1 do
  begin
    if (GroupCount > 0) and (GroupCount mod 3 = 0) then
      Result := ASep + Result;
    Result := AIntPart[I] + Result;
    Inc(GroupCount);
  end;
end;

function FormatNumberWithCLDR(AValue: Double; const ALocale: string;
  const AOptions: TIntlNumberFormatOptions): string;
var
  DecimalSep, GroupSep, PercentSign, MinusSign: string;
  IsNeg: Boolean;
  AbsValue: Double;
  MinFrac, MaxFrac: Integer;
  IntPart, FracPart, RawStr: string;
  DotPos: Integer;
  CurrSymbol, CurrNarrow: string;
  CurrDigits: Integer;
  FormatSpec: string;
  InvariantFS: TFormatSettings;
begin
  if not TryGetNumberSymbol(ALocale, 'decimal', DecimalSep) then
    DecimalSep := '.';
  if not TryGetNumberSymbol(ALocale, 'group', GroupSep) then
    GroupSep := ',';
  if not TryGetNumberSymbol(ALocale, 'percentSign', PercentSign) then
    PercentSign := '%';
  if not TryGetNumberSymbol(ALocale, 'minusSign', MinusSign) then
    MinusSign := '-';

  IsNeg := AValue < 0;
  AbsValue := Abs(AValue);

  case AOptions.Style of
    insPercent:
      AbsValue := AbsValue * 100;
    insCurrency:
    begin
      if not TryGetCurrencyInfo(ALocale, AOptions.Currency,
        CurrSymbol, CurrNarrow, CurrDigits) then
      begin
        CurrDigits := 2;
        if AOptions.Currency = 'USD' then CurrSymbol := '$'
        else if AOptions.Currency = 'EUR' then CurrSymbol := #$E2#$82#$AC
        else if AOptions.Currency = 'GBP' then CurrSymbol := #$C2#$A3
        else if (AOptions.Currency = 'JPY') or (AOptions.Currency = 'CNY') then
        begin
          CurrSymbol := #$C2#$A5;
          CurrDigits := 0;
        end
        else
          CurrSymbol := AOptions.Currency;
      end;
    end;
  end;

  MinFrac := AOptions.MinimumFractionDigits;
  MaxFrac := AOptions.MaximumFractionDigits;
  if (MinFrac < 0) and (MaxFrac < 0) then
  begin
    case AOptions.Style of
      insCurrency:
      begin
        MinFrac := CurrDigits;
        MaxFrac := CurrDigits;
      end;
      insPercent:
      begin
        MinFrac := 0;
        MaxFrac := 0;
      end;
    else
      MinFrac := 0;
      MaxFrac := 3;
    end;
  end;

  if MinFrac < 0 then MinFrac := 0;
  if MaxFrac < 0 then MaxFrac := 3;
  if MaxFrac < MinFrac then MaxFrac := MinFrac;

  FormatSpec := '0.' + StringOfChar('0', MaxFrac);
  InvariantFS := DefaultFormatSettings;
  InvariantFS.DecimalSeparator := '.';
  RawStr := FormatFloat(FormatSpec, AbsValue, InvariantFS);

  DotPos := Pos('.', RawStr);
  if DotPos > 0 then
  begin
    IntPart := Copy(RawStr, 1, DotPos - 1);
    FracPart := Copy(RawStr, DotPos + 1, Length(RawStr) - DotPos);

    // Trim trailing zeros down to MinFrac
    while (Length(FracPart) > MinFrac) and
          (Length(FracPart) > 0) and (FracPart[Length(FracPart)] = '0') do
      Delete(FracPart, Length(FracPart), 1);
  end
  else
  begin
    IntPart := RawStr;
    FracPart := '';
  end;

  // Pad integer part to minimumIntegerDigits
  while Length(IntPart) < AOptions.MinimumIntegerDigits do
    IntPart := '0' + IntPart;

  // Apply grouping separator
  if AOptions.UseGrouping <> inugFalse then
    IntPart := InsertGroupingSeparator(IntPart, GroupSep);

  if Length(FracPart) > 0 then
    Result := IntPart + DecimalSep + FracPart
  else
    Result := IntPart;

  case AOptions.Style of
    insPercent:
      Result := Result + PercentSign;
    insCurrency:
    begin
      case AOptions.CurrencyDisplay of
        incdCode:
          Result := AOptions.Currency + ' ' + Result;
        incdNarrowSymbol:
          Result := CurrNarrow + Result;
      else
        Result := CurrSymbol + Result;
      end;
    end;
  end;

  if IsNeg then
    Result := MinusSign + Result;
end;

function NormalizeDecimalIntegerString(const AValue: string; out AIsNegative: Boolean;
  out ADigits: string): Boolean;
var
  I, StartIndex: Integer;
begin
  Result := False;
  AIsNegative := False;
  ADigits := Trim(AValue);
  if ADigits = '' then
    Exit;

  StartIndex := 1;
  if ADigits[1] in ['+', '-'] then
  begin
    AIsNegative := ADigits[1] = '-';
    StartIndex := 2;
  end;

  if StartIndex > Length(ADigits) then
    Exit;

  for I := StartIndex to Length(ADigits) do
    if not (ADigits[I] in ['0'..'9']) then
      Exit;

  ADigits := Copy(ADigits, StartIndex, MaxInt);
  while (Length(ADigits) > 1) and (ADigits[1] = '0') do
    Delete(ADigits, 1, 1);
  if ADigits = '0' then
    AIsNegative := False;

  Result := True;
end;

function FormatDecimalStringWithCLDR(const AValue, ALocale: string;
  const AOptions: TIntlNumberFormatOptions): string;
var
  DecimalSep, GroupSep, PercentSign, MinusSign, PlusSign: string;
  IsNeg, IsZero: Boolean;
  IntPart, FracPart: string;
  MinFrac, MaxFrac: Integer;
  CurrSymbol, CurrNarrow: string;
  CurrDigits: Integer;
begin
  if not NormalizeDecimalIntegerString(AValue, IsNeg, IntPart) then
    Exit(AValue);
  IsZero := IntPart = '0';

  if not TryGetNumberSymbol(ALocale, 'decimal', DecimalSep) then
    DecimalSep := '.';
  if not TryGetNumberSymbol(ALocale, 'group', GroupSep) then
    GroupSep := ',';
  if not TryGetNumberSymbol(ALocale, 'percentSign', PercentSign) then
    PercentSign := '%';
  if not TryGetNumberSymbol(ALocale, 'minusSign', MinusSign) then
    MinusSign := '-';
  if not TryGetNumberSymbol(ALocale, 'plusSign', PlusSign) then
    PlusSign := '+';

  case AOptions.Style of
    insPercent:
      IntPart := IntPart + '00';
    insCurrency:
    begin
      if not TryGetCurrencyInfo(ALocale, AOptions.Currency,
        CurrSymbol, CurrNarrow, CurrDigits) then
      begin
        CurrDigits := 2;
        if AOptions.Currency = 'USD' then CurrSymbol := '$'
        else if AOptions.Currency = 'EUR' then CurrSymbol := #$E2#$82#$AC
        else if AOptions.Currency = 'GBP' then CurrSymbol := #$C2#$A3
        else if (AOptions.Currency = 'JPY') or (AOptions.Currency = 'CNY') then
        begin
          CurrSymbol := #$C2#$A5;
          CurrDigits := 0;
        end
        else
          CurrSymbol := AOptions.Currency;
        CurrNarrow := CurrSymbol;
      end;
    end;
  end;

  MinFrac := AOptions.MinimumFractionDigits;
  MaxFrac := AOptions.MaximumFractionDigits;
  if (MinFrac < 0) and (MaxFrac < 0) then
  begin
    case AOptions.Style of
      insCurrency:
      begin
        MinFrac := CurrDigits;
        MaxFrac := CurrDigits;
      end;
      insPercent:
      begin
        MinFrac := 0;
        MaxFrac := 0;
      end;
    else
      MinFrac := 0;
      MaxFrac := 3;
    end;
  end;
  if MinFrac < 0 then MinFrac := 0;
  if MaxFrac < 0 then MaxFrac := 3;
  if MaxFrac < MinFrac then MaxFrac := MinFrac;

  while Length(IntPart) < AOptions.MinimumIntegerDigits do
    IntPart := '0' + IntPart;
  if AOptions.UseGrouping <> inugFalse then
    IntPart := InsertGroupingSeparator(IntPart, GroupSep);

  FracPart := StringOfChar('0', MinFrac);
  if FracPart <> '' then
    Result := IntPart + DecimalSep + FracPart
  else
    Result := IntPart;

  case AOptions.Style of
    insPercent:
      Result := Result + PercentSign;
    insCurrency:
    begin
      case AOptions.CurrencyDisplay of
        incdCode:
          Result := AOptions.Currency + ' ' + Result;
        incdNarrowSymbol:
          Result := CurrNarrow + Result;
      else
        Result := CurrSymbol + Result;
      end;
    end;
  end;

  case AOptions.SignDisplay of
    insdNever:
      ;
    insdAlways:
      if IsNeg then
        Result := MinusSign + Result
      else
        Result := PlusSign + Result;
    insdExceptZero:
      if IsNeg then
        Result := MinusSign + Result
      else if not IsZero then
        Result := PlusSign + Result;
    insdNegative:
      if IsNeg then
        Result := MinusSign + Result;
  else
    if IsNeg then
      Result := MinusSign + Result;
  end;
end;

function CanFormatDecimalStringWithCLDR(
  const AOptions: TIntlNumberFormatOptions): Boolean;
begin
  Result :=
    (AOptions.Notation = innStandard) and
    (AOptions.Style <> insUnit) and
    (AOptions.NumberingSystem = 'latn') and
    (AOptions.CurrencyDisplay <> incdName) and
    (AOptions.CurrencySign = incsStandard) and
    (AOptions.UseGrouping <> inugMin2) and
    (AOptions.MinimumSignificantDigits < 0) and
    (AOptions.MaximumSignificantDigits < 0) and
    (AOptions.RoundingIncrement = 1) and
    (AOptions.RoundingMode = inrmHalfExpand) and
    (AOptions.RoundingPriority = inrpAuto) and
    (AOptions.TrailingZeroDisplay = intzAuto);
end;

function TryFormatDecimalStringFallback(const ALocale, ADecimal: string;
  const AOptions: TIntlNumberFormatOptions; out AFormatted: string): Boolean;
var
  NumberValue: Double;
begin
  if CanFormatDecimalStringWithCLDR(AOptions) then
  begin
    AFormatted := FormatDecimalStringWithCLDR(ADecimal, ALocale, AOptions);
    Exit(True);
  end;

  Result := TryStrToFloat(ADecimal, NumberValue, InvariantFormatSettings) and
    TryICUFormatNumber(ALocale, NumberValue, AOptions, AFormatted);
end;

function TryFormatDecimalStringPartsFallback(const ALocale, ADecimal: string;
  const AOptions: TIntlNumberFormatOptions; out AParts: TIntlFormatPartArray): Boolean;
var
  NumberValue: Double;
begin
  SetLength(AParts, 0);
  if CanFormatDecimalStringWithCLDR(AOptions) then
  begin
    SetLength(AParts, 1);
    AParts[0].PartType := 'literal';
    AParts[0].Value := FormatDecimalStringWithCLDR(ADecimal, ALocale, AOptions);
    Exit(True);
  end;

  Result := TryStrToFloat(ADecimal, NumberValue, InvariantFormatSettings) and
    TryICUFormatNumberToParts(ALocale, NumberValue, AOptions, AParts);
end;

function NumberRangeSeparator(const AOptions: TIntlNumberFormatOptions): string;
begin
  if (AOptions.Style = insDecimal) and (AOptions.SignDisplay = insdAuto) then
    Result := '–'
  else
    Result := ' – ';
end;

procedure SetPartsSource(var AParts: TIntlFormatPartArray; const ASource: string);
var
  I: Integer;
begin
  for I := 0 to Length(AParts) - 1 do
    AParts[I].Source := ASource;
end;

procedure AppendPartArray(var ADest: TIntlFormatPartArray;
  const ASrc: TIntlFormatPartArray);
var
  I, OldLength: Integer;
begin
  OldLength := Length(ADest);
  SetLength(ADest, OldLength + Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do
    ADest[OldLength + I] := ASrc[I];
end;

function TryGetNumberPartsWithFallback(const ALocale: string; AValue: Double;
  const AOptions: TIntlNumberFormatOptions; const ASource: string;
  out AParts: TIntlFormatPartArray): Boolean;
var
  Formatted: string;
begin
  Result := True;
  if not TryICUFormatNumberToParts(ALocale, AValue, AOptions, AParts) then
  begin
    if TryICUFormatNumber(ALocale, AValue, AOptions, Formatted) then
    begin
      SetLength(AParts, 1);
      AParts[0].PartType := 'literal';
      AParts[0].Value := Formatted;
    end
    else
    begin
      SetLength(AParts, 1);
      AParts[0].PartType := 'literal';
      AParts[0].Value := FormatNumberWithCLDR(AValue, ALocale, AOptions);
    end;
  end;
  SetPartsSource(AParts, ASource);
end;

procedure NormalizeNumberFormatParts(var AParts: TIntlFormatPartArray;
  const AValue: Double; const AOptions: TIntlNumberFormatOptions);
var
  I: Integer;
begin
  for I := 0 to Length(AParts) - 1 do
  begin
    if Math.IsNaN(AValue) and (AParts[I].PartType = 'integer') then
      AParts[I].PartType := 'nan'
    else if Math.IsInfinite(AValue) and (AParts[I].PartType = 'integer') then
      AParts[I].PartType := 'infinity'
    else if (AOptions.Style = insUnit) and
            (AParts[I].PartType = 'percentSign') then
      AParts[I].PartType := 'unit';
  end;
end;

function UsesParenthesizedAccounting(const ALocale: string): Boolean;
begin
  Result := Copy(AsciiLower(ALocale), 1, 2) <> 'de';
end;

procedure ApplyAccountingToFormatted(var AFormatted: string; const ALocale: string;
  const AOptions: TIntlNumberFormatOptions);
begin
  if (AOptions.Style = insCurrency) and
     (AOptions.CurrencySign = incsAccounting) and
     UsesParenthesizedAccounting(ALocale) and
     (Length(AFormatted) > 0) and (AFormatted[1] = '-') then
    AFormatted := '(' + Copy(AFormatted, 2, MaxInt) + ')';
end;

procedure ApplyAccountingToParts(var AParts: TIntlFormatPartArray;
  const ALocale: string; const AOptions: TIntlNumberFormatOptions);
var
  I, MinusIndex: Integer;
begin
  if (AOptions.Style <> insCurrency) or
     (AOptions.CurrencySign <> incsAccounting) or
     not UsesParenthesizedAccounting(ALocale) then
    Exit;

  MinusIndex := -1;
  for I := 0 to Length(AParts) - 1 do
    if AParts[I].PartType = 'minusSign' then
    begin
      MinusIndex := I;
      Break;
    end;

  if MinusIndex < 0 then
    Exit;

  for I := MinusIndex to Length(AParts) - 2 do
    AParts[I] := AParts[I + 1];
  SetLength(AParts, Length(AParts) - 1);

  SetLength(AParts, Length(AParts) + 2);
  for I := Length(AParts) - 2 downto 1 do
    AParts[I] := AParts[I - 1];
  AParts[0].PartType := 'literal';
  AParts[0].Value := '(';
  AParts[0].Source := '';
  AParts[0].UnitIdentifier := '';
  AParts[High(AParts)].PartType := 'literal';
  AParts[High(AParts)].Value := ')';
  AParts[High(AParts)].Source := '';
  AParts[High(AParts)].UnitIdentifier := '';
end;

procedure InsertNumberFormatPart(var AParts: TIntlFormatPartArray;
  const AIndex: Integer; const AType, AValue: string);
var
  I, OldLength: Integer;
begin
  OldLength := Length(AParts);
  SetLength(AParts, OldLength + 1);
  for I := OldLength downto AIndex + 1 do
    AParts[I] := AParts[I - 1];
  AParts[AIndex].PartType := AType;
  AParts[AIndex].Value := AValue;
  AParts[AIndex].Source := '';
  AParts[AIndex].UnitIdentifier := '';
end;

function HasLiteralSpaceBeforePart(const AParts: TIntlFormatPartArray;
  const AIndex: Integer): Boolean;
begin
  Result := (AIndex > 0) and (AParts[AIndex - 1].PartType = 'literal') and
    (AParts[AIndex - 1].Value = ' ');
end;

procedure NormalizeUnitFormatted(var AFormatted: string; const ALocale: string;
  const AOptions: TIntlNumberFormatOptions);
var
  LocaleLower: string;
  P: Integer;
begin
  if (AOptions.Style <> insUnit) or
     (AOptions.UnitIdentifier <> 'kilometer-per-hour') then
    Exit;

  LocaleLower := AsciiLower(ALocale);
  if (Copy(LocaleLower, 1, 2) = 'ja') and
     (AOptions.UnitDisplay = inudLong) then
  begin
    if Pos('時速 ', AFormatted) <> 1 then
      AFormatted := '時速 ' + Copy(AFormatted, Length('時速') + 1, MaxInt);
    P := Pos('キロメートル', AFormatted);
    if (P > 1) and (Copy(AFormatted, P - 1, 1) <> ' ') then
      Insert(' ', AFormatted, P);
  end
  else if Copy(LocaleLower, 1, 5) = 'zh-tw' then
  begin
    if AOptions.UnitDisplay = inudShort then
    begin
      P := Pos('公里/小時', AFormatted);
      if (P > 1) and (Copy(AFormatted, P - 1, 1) <> ' ') then
        Insert(' ', AFormatted, P);
    end
    else if AOptions.UnitDisplay = inudLong then
    begin
      if Pos('每小時 ', AFormatted) <> 1 then
        AFormatted := '每小時 ' + Copy(AFormatted, Length('每小時') + 1, MaxInt);
      P := Pos('公里', AFormatted);
      if (P > 1) and (Copy(AFormatted, P - 1, 1) <> ' ') then
        Insert(' ', AFormatted, P);
    end;
  end;
end;

procedure NormalizeUnitParts(var AParts: TIntlFormatPartArray;
  const ALocale: string; const AOptions: TIntlNumberFormatOptions);
var
  LocaleLower: string;
  I: Integer;
begin
  if (AOptions.Style <> insUnit) or
     (AOptions.UnitIdentifier <> 'kilometer-per-hour') then
    Exit;

  LocaleLower := AsciiLower(ALocale);
  if (Copy(LocaleLower, 1, 2) = 'ja') and
     (AOptions.UnitDisplay = inudLong) then
  begin
    if (Length(AParts) > 1) and (AParts[0].PartType = 'unit') and
       not ((AParts[1].PartType = 'literal') and (AParts[1].Value = ' ')) then
      InsertNumberFormatPart(AParts, 1, 'literal', ' ');
    for I := 0 to Length(AParts) - 1 do
      if (AParts[I].PartType = 'unit') and
         (AParts[I].Value = 'キロメートル') and
         not HasLiteralSpaceBeforePart(AParts, I) then
      begin
        InsertNumberFormatPart(AParts, I, 'literal', ' ');
        Break;
      end;
  end
  else if Copy(LocaleLower, 1, 5) = 'zh-tw' then
  begin
    if AOptions.UnitDisplay = inudShort then
    begin
      for I := 0 to Length(AParts) - 1 do
        if (AParts[I].PartType = 'unit') and
           (AParts[I].Value = '公里/小時') and
           not HasLiteralSpaceBeforePart(AParts, I) then
        begin
          InsertNumberFormatPart(AParts, I, 'literal', ' ');
          Break;
        end;
    end
    else if AOptions.UnitDisplay = inudLong then
    begin
      if (Length(AParts) > 1) and (AParts[0].PartType = 'unit') and
         not ((AParts[1].PartType = 'literal') and (AParts[1].Value = ' ')) then
        InsertNumberFormatPart(AParts, 1, 'literal', ' ');
      for I := 0 to Length(AParts) - 1 do
        if (AParts[I].PartType = 'unit') and
           (AParts[I].Value = '公里') and
           not HasLiteralSpaceBeforePart(AParts, I) then
        begin
          InsertNumberFormatPart(AParts, I, 'literal', ' ');
          Break;
        end;
    end;
  end;
end;

procedure NormalizeFormattedNumberParts(var AParts: TIntlFormatPartArray;
  const AValue: Double; const ALocale: string;
  const AOptions: TIntlNumberFormatOptions);
begin
  NormalizeNumberFormatParts(AParts, AValue, AOptions);
  NormalizeUnitParts(AParts, ALocale, AOptions);
  ApplyAccountingToParts(AParts, ALocale, AOptions);
end;

function IsIntlDecimalString(const AValue: string): Boolean;
var
  I: Integer;
  HasDigit: Boolean;
begin
  Result := False;
  HasDigit := False;
  if AValue = '' then
    Exit;

  for I := 1 to Length(AValue) do
  begin
    if (AValue[I] >= '0') and (AValue[I] <= '9') then
      HasDigit := True
    else if not (AValue[I] in ['+', '-', '.', 'e', 'E']) then
      Exit;
  end;

  Result := HasDigit;
end;

function TryReadDecimalRangeInput(const AValue: TGocciaValue;
  out ANumber: Double; out ADecimal: string; out AHasDecimal: Boolean): Boolean;
var
  S: string;
begin
  Result := False;
  ADecimal := '';
  AHasDecimal := False;

  if AValue is TGocciaBigIntValue then
  begin
    ADecimal := TGocciaBigIntValue(AValue).Value.ToString;
    AHasDecimal := True;
    if not TryStrToFloat(ADecimal, ANumber, InvariantFormatSettings) then
      ANumber := 0;
    Exit(True);
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    S := Trim(TGocciaStringLiteralValue(AValue).Value);
    if IsIntlDecimalString(S) then
    begin
      ADecimal := S;
      AHasDecimal := True;
    end;
  end;

  ANumber := AValue.ToNumberLiteral.Value;
  Result := not IsNan(ANumber);
end;

function TryReadBigIntDecimalInput(const AValue: TGocciaValue;
  out ADecimal: string): Boolean;
begin
  Result := True;
  if AValue is TGocciaBigIntValue then
    ADecimal := TGocciaBigIntValue(AValue).Value.ToString
  else if (AValue is TGocciaBigIntObjectValue) and
          (TGocciaBigIntObjectValue(AValue).Primitive is TGocciaBigIntValue) then
    ADecimal := TGocciaBigIntValue(TGocciaBigIntObjectValue(AValue).Primitive).Value.ToString
  else
    Result := False;
end;

function TryReadIntlDecimalInput(const AValue: TGocciaValue;
  out ADecimal: string): Boolean;
begin
  if TryReadBigIntDecimalInput(AValue, ADecimal) then
    Exit(True);

  Result := False;
  if AValue is TGocciaStringLiteralValue then
  begin
    ADecimal := Trim(TGocciaStringLiteralValue(AValue).Value);
    Result := IsIntlDecimalString(ADecimal);
  end;
end;

function NumberFormatLocaleArgumentToLocale(const AArg: TGocciaValue): string;
var
  Tag, Canonical: string;
  O: TGocciaObjectValue;
  Element: TGocciaValue;
  K, Len: Integer;
  ObjectRoot: TGocciaTempRoot;
  FoundLocale: Boolean;
  RequestedLocales: TStringArray;

  function CanonicalizeNumberFormatLocaleTag(const ARawTag: string): string;
  begin
    Result := CanonicalizeUnicodeLocaleId(ARawTag);
    if Result = '' then
      ThrowRangeError(Format('invalid language tag: %s', [ARawTag]));
  end;
begin
  Result := '';
  Tag := '';
  if (AArg = nil) or (AArg is TGocciaUndefinedLiteralValue) then
    Exit;

  if AArg is TGocciaStringLiteralValue then
    Tag := TGocciaStringLiteralValue(AArg).Value
  else
  begin
    O := ToObject(AArg);

    InitializeTempRoot(ObjectRoot);
    AddTempRootIfNeeded(ObjectRoot, O);
    try
      Len := LengthOfArrayLike(O);
      FoundLocale := False;
      for K := 0 to Len - 1 do
      begin
        if not O.HasProperty(IntToStr(K)) then
          Continue;
        Element := O.GetProperty(IntToStr(K));
        if not ((Element is TGocciaStringLiteralValue) or
                (Element is TGocciaObjectValue)) then
          ThrowTypeError('locales entries must be strings or objects');
        if Element is TGocciaIntlLocaleValue then
          Canonical := CanonicalizeNumberFormatLocaleTag(
            TGocciaIntlLocaleValue(Element).LocaleTag)
        else
          Canonical := CanonicalizeNumberFormatLocaleTag(
            Element.ToStringLiteral.Value);
        if not FoundLocale then
        begin
          Tag := Canonical;
          FoundLocale := True;
        end;
      end;
    finally
      RemoveTempRootIfNeeded(ObjectRoot);
    end;
  end;

  if Tag = '' then
    Exit;

  SetLength(RequestedLocales, 1);
  RequestedLocales[0] := CanonicalizeNumberFormatLocaleTag(Tag);
  Result := ResolveLocaleFromRequested(RequestedLocales);
end;

function NumberFormatOptionsArgumentToObject(
  const AArg: TGocciaValue): TGocciaObjectValue;
begin
  if (AArg = nil) or (AArg is TGocciaUndefinedLiteralValue) then
    Result := nil
  else if AArg is TGocciaNullLiteralValue then
    ThrowTypeError('Cannot convert null or undefined to object')
  else if AArg is TGocciaObjectValue then
    Result := TGocciaObjectValue(AArg)
  else
    Result := nil;
end;

{ TGocciaIntlNumberFormatValue }

procedure TGocciaIntlNumberFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  Ignored: string;
  HasNumberingSystem, HasCurrency: Boolean;
begin
  if not Assigned(AOptions) then Exit;

  TryReadNumberFormatStringOption(AOptions, 'localeMatcher',
    ['lookup', 'best fit'], Ignored);
  HasNumberingSystem := TryReadStringOption(AOptions, 'numberingSystem',
    FNumberingSystem);
  FNumberingSystemOptionPresent := HasNumberingSystem;
  if HasNumberingSystem and
     (ContainsNulCharacter(FNumberingSystem) or
      not IsWellFormedUnicodeTypeIdentifier(FNumberingSystem)) then
    ThrowRangeError(Format(SErrorIntlInvalidOption,
      [FNumberingSystem, 'numberingSystem']));
  if HasNumberingSystem then
    FNumberingSystem := AsciiLower(FNumberingSystem);

  TryReadNumberFormatStringOption(AOptions, 'style',
    ['decimal', 'percent', 'currency', 'unit'], FStyle);
  HasCurrency := TryReadStringOption(AOptions, 'currency', FCurrency);
  if HasCurrency and
     (ContainsNulCharacter(FCurrency) or
      not IsWellFormedCurrencyCode(FCurrency)) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [FCurrency, 'currency']));
  if HasCurrency then
    FCurrency := AsciiUpper(FCurrency);
  TryReadNumberFormatStringOption(AOptions, 'currencyDisplay',
    ['code', 'symbol', 'narrowSymbol', 'name'], FCurrencyDisplay);
  TryReadNumberFormatStringOption(AOptions, 'currencySign',
    ['standard', 'accounting'], FCurrencySign);
  TryReadStringOption(AOptions, 'unit', FUnitIdentifier);
  if (FUnitIdentifier <> '') and ContainsNulCharacter(FUnitIdentifier) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [FUnitIdentifier, 'unit']));
  TryReadNumberFormatStringOption(AOptions, 'unitDisplay',
    ['short', 'narrow', 'long'], FUnitDisplay);

  TryReadNumberFormatStringOption(AOptions, 'notation',
    ['standard', 'scientific', 'engineering', 'compact'], FNotation);

  TryReadNumberFormatIntegerOption(AOptions, 'minimumIntegerDigits',
    1, 21, FMinimumIntegerDigits);
  TryReadNumberFormatIntegerOption(AOptions, 'minimumFractionDigits',
    0, 100, FMinimumFractionDigits);
  TryReadNumberFormatIntegerOption(AOptions, 'maximumFractionDigits',
    0, 100, FMaximumFractionDigits);
  TryReadNumberFormatIntegerOption(AOptions, 'minimumSignificantDigits',
    1, 21, FMinimumSignificantDigits);
  TryReadNumberFormatIntegerOption(AOptions, 'maximumSignificantDigits',
    1, 21, FMaximumSignificantDigits);
  TryReadNumberFormatIntegerOption(AOptions, 'roundingIncrement',
    1, 5000, FRoundingIncrement);
  TryReadNumberFormatStringOption(AOptions, 'roundingMode',
    ['ceil', 'floor', 'expand', 'trunc', 'halfCeil', 'halfFloor',
     'halfExpand', 'halfTrunc', 'halfEven'], FRoundingMode);
  TryReadNumberFormatStringOption(AOptions, 'roundingPriority',
    ['auto', 'morePrecision', 'lessPrecision'], FRoundingPriority);
  TryReadNumberFormatStringOption(AOptions, 'trailingZeroDisplay',
    ['auto', 'stripIfInteger'], FTrailingZeroDisplay);

  TryReadNumberFormatStringOption(AOptions, 'compactDisplay',
    ['short', 'long'], FCompactDisplay);
  if FNotation = 'compact' then
    FUseGrouping := ReadUseGroupingOption(AOptions, 'min2')
  else
    FUseGrouping := ReadUseGroupingOption(AOptions, 'auto');
  TryReadNumberFormatStringOption(AOptions, 'signDisplay',
    ['auto', 'never', 'always', 'exceptZero', 'negative'], FSignDisplay);
end;

constructor TGocciaIntlNumberFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical, CurrSymbol, CurrNarrow, LocaleNumberingSystem: string;
  CurrDigits, MinimumFractionDefault, MaximumFractionDefault: Integer;
  HasFractionDigits, HasSignificantDigits: Boolean;
  NeedFractionDigits, NeedSignificantDigits: Boolean;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FStyle := 'decimal';
  FCurrencyDisplay := 'symbol';
  FCurrencySign := 'standard';
  FUnitDisplay := 'short';
  FNotation := 'standard';
  FCompactDisplay := 'short';
  FSignDisplay := 'auto';
  FUseGrouping := 'auto';
  FRoundingMode := 'halfExpand';
  FRoundingIncrement := 1;
  FRoundingPriority := 'auto';
  FTrailingZeroDisplay := 'auto';
  FMinimumIntegerDigits := 1;
  FMinimumFractionDigits := -1;
  FMaximumFractionDigits := -1;
  FMinimumSignificantDigits := -1;
  FMaximumSignificantDigits := -1;
  FNumberingSystemOptionPresent := False;

  ReadOptions(AOptions);

  if FNumberingSystem <> '' then
  begin
    if IsSupportedNumberFormatNumberingSystem(FNumberingSystem) then
    begin
      if FNumberingSystemOptionPresent and
         not (TryGetUnicodeLocaleExtensionKeyword(FLocale, 'nu', LocaleNumberingSystem) and
              (LocaleNumberingSystem = FNumberingSystem)) then
        FLocale := LocaleWithoutUnicodeExtension(FLocale);
    end
    else
      FNumberingSystem := '';
  end;
  if FNumberingSystem = '' then
  begin
    if TryGetUnicodeLocaleExtensionKeyword(FLocale, 'nu', LocaleNumberingSystem) then
    begin
      if IsSupportedNumberFormatNumberingSystem(LocaleNumberingSystem) then
      begin
        FNumberingSystem := LocaleNumberingSystem
      end
      else
        FLocale := LocaleWithoutUnicodeExtension(FLocale);
    end;
  end;
  if TryGetUnicodeLocaleExtensionKeyword(FLocale, 'nu', LocaleNumberingSystem) then
    FLocale := LocaleWithoutUnicodeExtension(FLocale) + '-u-nu-' + FNumberingSystem
  else
    FLocale := LocaleWithoutUnicodeExtension(FLocale);

  // Validate unit option values even when the selected style does not use them.
  if FCurrency <> '' then
  begin
    if not IsWellFormedCurrencyCode(FCurrency) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [FCurrency, 'currency']));
    FCurrency := AsciiUpper(FCurrency);
  end;
  if (FStyle = 'currency') and (FCurrency = '') then
    ThrowTypeError(SErrorIntlMissingCurrency);

  if (FUnitIdentifier <> '') and
     not IsWellFormedUnitIdentifier(FUnitIdentifier) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [FUnitIdentifier, 'unit']));
  if (FStyle = 'unit') and (FUnitIdentifier = '') then
    ThrowTypeError(SErrorIntlMissingUnit);

  if FStyle <> 'currency' then
    FCurrency := '';
  if FStyle <> 'unit' then
    FUnitIdentifier := '';

  if FNumberingSystem = '' then
    FNumberingSystem := 'latn';

  // Validate digit ranges
  if (FMinimumIntegerDigits < 1) or (FMinimumIntegerDigits > 21) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumIntegerDigits', 1, 21]));
  if (FMinimumFractionDigits >= 0) and (FMinimumFractionDigits > 100) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumFractionDigits', 0, 100]));
  if (FMaximumFractionDigits >= 0) and (FMaximumFractionDigits > 100) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumFractionDigits', 0, 100]));
  if (FMinimumSignificantDigits >= 0) and
     ((FMinimumSignificantDigits < 1) or (FMinimumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['minimumSignificantDigits', 1, 21]));
  if (FMaximumSignificantDigits >= 0) and
     ((FMaximumSignificantDigits < 1) or (FMaximumSignificantDigits > 21)) then
    ThrowRangeError(Format(SErrorIntlDigitsOutOfRange, ['maximumSignificantDigits', 1, 21]));
  if (FRoundingIncrement <> 1) and (FRoundingIncrement <> 2) and
     (FRoundingIncrement <> 5) and (FRoundingIncrement <> 10) and
     (FRoundingIncrement <> 20) and (FRoundingIncrement <> 25) and
     (FRoundingIncrement <> 50) and (FRoundingIncrement <> 100) and
     (FRoundingIncrement <> 200) and (FRoundingIncrement <> 250) and
     (FRoundingIncrement <> 500) and (FRoundingIncrement <> 1000) and
     (FRoundingIncrement <> 2000) and (FRoundingIncrement <> 2500) and
     (FRoundingIncrement <> 5000) then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [IntToStr(FRoundingIncrement), 'roundingIncrement']));
  if (FRoundingPriority <> 'auto') and (FRoundingPriority <> 'morePrecision') and
     (FRoundingPriority <> 'lessPrecision') then
    ThrowRangeError(Format(SErrorIntlInvalidOption, [FRoundingPriority, 'roundingPriority']));

  HasSignificantDigits := (FMinimumSignificantDigits >= 0) or
    (FMaximumSignificantDigits >= 0);
  HasFractionDigits := (FMinimumFractionDigits >= 0) or
    (FMaximumFractionDigits >= 0);

  CurrDigits := 2;
  if (FStyle = 'currency') and (FNotation = 'standard') then
  begin
    if not TryGetCurrencyInfo(FLocale, FCurrency, CurrSymbol, CurrNarrow, CurrDigits) then
      CurrDigits := 2;
    MinimumFractionDefault := CurrDigits;
    MaximumFractionDefault := CurrDigits;
  end
  else
  begin
    MinimumFractionDefault := 0;
    if FStyle = 'percent' then
      MaximumFractionDefault := 0
    else
      MaximumFractionDefault := 3;
  end;

  if FRoundingIncrement <> 1 then
    MaximumFractionDefault := MinimumFractionDefault;

  NeedSignificantDigits := True;
  NeedFractionDigits := True;
  if FRoundingPriority = 'auto' then
  begin
    NeedSignificantDigits := HasSignificantDigits;
    if NeedSignificantDigits or
       ((not HasFractionDigits) and (FNotation = 'compact')) then
      NeedFractionDigits := False;
  end;

  if NeedSignificantDigits then
  begin
    if HasSignificantDigits then
    begin
      if FMinimumSignificantDigits < 0 then
        FMinimumSignificantDigits := 1;
      if FMaximumSignificantDigits < 0 then
        FMaximumSignificantDigits := 21;
      if FMinimumSignificantDigits > FMaximumSignificantDigits then
        ThrowRangeError(Format(SErrorIntlDigitsOutOfRange,
          ['maximumSignificantDigits', FMinimumSignificantDigits, 21]));
    end
    else
    begin
      FMinimumSignificantDigits := 1;
      FMaximumSignificantDigits := 21;
    end;
  end;

  if NeedFractionDigits then
  begin
    if HasFractionDigits then
    begin
      if FMinimumFractionDigits < 0 then
        FMinimumFractionDigits := Min(MinimumFractionDefault, FMaximumFractionDigits)
      else if FMaximumFractionDigits < 0 then
        FMaximumFractionDigits := Max(MaximumFractionDefault, FMinimumFractionDigits)
      else if FMinimumFractionDigits > FMaximumFractionDigits then
        ThrowRangeError(Format(SErrorIntlDigitsOutOfRange,
          ['maximumFractionDigits', FMinimumFractionDigits, 100]));
    end
    else
    begin
      FMinimumFractionDigits := MinimumFractionDefault;
      FMaximumFractionDigits := MaximumFractionDefault;
    end;
  end;

  if (not NeedSignificantDigits) and (not NeedFractionDigits) then
  begin
    FMinimumFractionDigits := 0;
    FMaximumFractionDigits := 0;
    FMinimumSignificantDigits := 1;
    FMaximumSignificantDigits := 2;
    FRoundingPriority := 'morePrecision';
  end;

  if FRoundingIncrement <> 1 then
  begin
    if (FRoundingPriority <> 'auto') or HasSignificantDigits then
      ThrowTypeError('roundingIncrement requires fraction digit rounding');
    if FMinimumFractionDigits <> FMaximumFractionDigits then
      ThrowRangeError('roundingIncrement requires matching fraction digit bounds');
  end;

  // Build resolved ICU options
  FResolvedOptions := DefaultNumberFormatOptions;
  FResolvedOptions.Style := StyleStringToEnum(FStyle);
  FResolvedOptions.Currency := FCurrency;
  FResolvedOptions.CurrencyDisplay := CurrencyDisplayStringToEnum(FCurrencyDisplay);
  FResolvedOptions.CurrencySign := CurrencySignStringToEnum(FCurrencySign);
  FResolvedOptions.UnitIdentifier := FUnitIdentifier;
  FResolvedOptions.UnitDisplay := UnitDisplayStringToEnum(FUnitDisplay);
  FResolvedOptions.Notation := NotationStringToEnum(FNotation);
  FResolvedOptions.CompactDisplay := CompactDisplayStringToEnum(FCompactDisplay);
  FResolvedOptions.SignDisplay := SignDisplayStringToEnum(FSignDisplay);
  FResolvedOptions.UseGrouping := UseGroupingStringToEnum(FUseGrouping);
  FResolvedOptions.MinimumIntegerDigits := FMinimumIntegerDigits;
  FResolvedOptions.MinimumFractionDigits := FMinimumFractionDigits;
  FResolvedOptions.MaximumFractionDigits := FMaximumFractionDigits;
  FResolvedOptions.MinimumSignificantDigits := FMinimumSignificantDigits;
  FResolvedOptions.MaximumSignificantDigits := FMaximumSignificantDigits;
  FResolvedOptions.RoundingMode := RoundingModeStringToEnum(FRoundingMode);
  FResolvedOptions.RoundingIncrement := FRoundingIncrement;
  FResolvedOptions.RoundingPriority := RoundingPriorityStringToEnum(FRoundingPriority);
  FResolvedOptions.TrailingZeroDisplay :=
    TrailingZeroDisplayStringToEnum(FTrailingZeroDisplay);
  FResolvedOptions.NumberingSystem := FNumberingSystem;

  InitializePrototype;
  if Assigned(GetIntlNumberFormatShared) then
    FPrototype := GetIntlNumberFormatShared.Prototype;
end;

class function TGocciaIntlNumberFormatValue.CreateFromArguments(
  const ALocales, AOptions: TGocciaValue): TGocciaIntlNumberFormatValue;
begin
  Result := TGocciaIntlNumberFormatValue.Create(
    NumberFormatLocaleArgumentToLocale(ALocales),
    NumberFormatOptionsArgumentToObject(AOptions));
end;

function TGocciaIntlNumberFormatValue.ToStringTag: string;
begin
  Result := 'Object';
end;

procedure TGocciaIntlNumberFormatValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FBoundFormat) then
    FBoundFormat.MarkReferences;
end;

procedure TGocciaIntlNumberFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlNumberFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlNumberFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('format', IntlNumberFormatFormatGetter, nil,
        [pfConfigurable]);
      Members.AddNamedMethod('formatToParts', IntlNumberFormatFormatToParts, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatRange', IntlNumberFormatFormatRange, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatRangeToParts', IntlNumberFormatFormatRangeToParts, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlNumberFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.NumberFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlNumberFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlNumberFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlNumberFormatValue.Create(DefaultLocale);
    Shared := GetIntlNumberFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormatGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
begin
  NF := UnwrapNumberFormat(AThisValue, 'get Intl.NumberFormat.prototype.format');
  if not Assigned(NF.FBoundFormat) then
    NF.FBoundFormat := TGocciaIntlNumberFormatBoundFormatValue.Create(NF);
  Result := NF.FBoundFormat;
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  InputValue: TGocciaValue;
  NumValue: Double;
  DecimalValue, Formatted: string;
begin
  NF := UnwrapNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.format');
  InputValue := AArgs.GetElement(0);

  if TryReadIntlDecimalInput(InputValue, DecimalValue) then
  begin
    if TryICUFormatNumberDecimal(NF.FLocale, DecimalValue,
      NF.FResolvedOptions, Formatted) then
    begin
      ApplyAccountingToFormatted(Formatted, NF.FLocale, NF.FResolvedOptions);
      NormalizeUnitFormatted(Formatted, NF.FLocale, NF.FResolvedOptions);
      Formatted := ApplySimpleNumberingSystemDigits(Formatted,
        NF.FNumberingSystem);
      Exit(TGocciaStringLiteralValue.Create(Formatted));
    end;
    if TryFormatDecimalStringFallback(NF.FLocale, DecimalValue,
      NF.FResolvedOptions, Formatted) then
    begin
      ApplyAccountingToFormatted(Formatted, NF.FLocale, NF.FResolvedOptions);
      NormalizeUnitFormatted(Formatted, NF.FLocale, NF.FResolvedOptions);
      Formatted := ApplySimpleNumberingSystemDigits(Formatted,
        NF.FNumberingSystem);
      Exit(TGocciaStringLiteralValue.Create(Formatted));
    end;
    ThrowTypeError(
      'Intl.NumberFormat BigInt fallback cannot format this option set without ICU decimal support');
  end;

  NumValue := InputValue.ToNumberLiteral.Value;
  if not TryICUFormatNumber(NF.FLocale, NumValue, NF.FResolvedOptions, Formatted) then
    Formatted := FormatNumberWithCLDR(NumValue, NF.FLocale, NF.FResolvedOptions);
  ApplyAccountingToFormatted(Formatted, NF.FLocale, NF.FResolvedOptions);
  NormalizeUnitFormatted(Formatted, NF.FLocale, NF.FResolvedOptions);
  Formatted := ApplySimpleNumberingSystemDigits(Formatted, NF.FNumberingSystem);
  Result := TGocciaStringLiteralValue.Create(Formatted);
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  InputValue: TGocciaValue;
  NumValue: Double;
  DecimalValue: string;
  Parts: TIntlFormatPartArray;
begin
  NF := UnwrapNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatToParts');
  InputValue := AArgs.GetElement(0);

  if TryReadIntlDecimalInput(InputValue, DecimalValue) then
  begin
    if TryICUFormatNumberDecimalToParts(NF.FLocale, DecimalValue,
      NF.FResolvedOptions, Parts) then
    begin
      NormalizeFormattedNumberParts(Parts, 0, NF.FLocale, NF.FResolvedOptions);
      Exit(FormatPartsToArray(Parts));
    end;
    if not TryFormatDecimalStringPartsFallback(NF.FLocale, DecimalValue,
      NF.FResolvedOptions, Parts) then
      ThrowTypeError(
        'Intl.NumberFormat BigInt fallback cannot format this option set without ICU decimal support');
    NormalizeFormattedNumberParts(Parts, 0, NF.FLocale, NF.FResolvedOptions);
    Exit(FormatPartsToArray(Parts));
  end;

  NumValue := InputValue.ToNumberLiteral.Value;
  if TryICUFormatNumberToParts(NF.FLocale, NumValue, NF.FResolvedOptions, Parts) then
  begin
    NormalizeFormattedNumberParts(Parts, NumValue, NF.FLocale, NF.FResolvedOptions);
    Result := FormatPartsToArray(Parts)
  end
  else
  begin
    SetLength(Parts, 1);
    Parts[0].PartType := 'literal';
    Parts[0].Value := FormatNumberWithCLDR(NumValue, NF.FLocale, NF.FResolvedOptions);
    NormalizeFormattedNumberParts(Parts, NumValue, NF.FLocale, NF.FResolvedOptions);
    Result := FormatPartsToArray(Parts);
  end;
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormatRange(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  StartValue, EndValue: Double;
  StartDecimal, EndDecimal, Formatted, StartFormatted, EndFormatted: string;
  StartHasDecimal, EndHasDecimal: Boolean;
begin
  NF := UnwrapNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatRange');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.NumberFormat.prototype.formatRange requires start and end values');
  if (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) or
     (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.NumberFormat.prototype.formatRange requires defined start and end values');

  if not TryReadDecimalRangeInput(AArgs.GetElement(0), StartValue, StartDecimal, StartHasDecimal) then
    ThrowRangeError('Intl.NumberFormat.prototype.formatRange start value must not be NaN');
  if not TryReadDecimalRangeInput(AArgs.GetElement(1), EndValue, EndDecimal, EndHasDecimal) then
    ThrowRangeError('Intl.NumberFormat.prototype.formatRange end value must not be NaN');

  if StartHasDecimal and EndHasDecimal and
     TryICUFormatNumberDecimalRange(NF.FLocale, StartDecimal, EndDecimal,
       NF.FResolvedOptions, Formatted) then
    Exit(TGocciaStringLiteralValue.Create(Formatted));

  if TryICUFormatNumberRange(NF.FLocale, StartValue, EndValue,
    NF.FResolvedOptions, Formatted) then
    Exit(TGocciaStringLiteralValue.Create(Formatted));

  if not TryICUFormatNumber(NF.FLocale, StartValue, NF.FResolvedOptions, StartFormatted) then
    StartFormatted := FormatNumberWithCLDR(StartValue, NF.FLocale, NF.FResolvedOptions);
  if not TryICUFormatNumber(NF.FLocale, EndValue, NF.FResolvedOptions, EndFormatted) then
    EndFormatted := FormatNumberWithCLDR(EndValue, NF.FLocale, NF.FResolvedOptions);

  if StartFormatted = EndFormatted then
    Formatted := '~' + StartFormatted
  else
    Formatted := StartFormatted + NumberRangeSeparator(NF.FResolvedOptions) + EndFormatted;
  Result := TGocciaStringLiteralValue.Create(Formatted);
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormatRangeToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  StartValue, EndValue: Double;
  StartDecimal, EndDecimal, StartFormatted, EndFormatted: string;
  StartHasDecimal, EndHasDecimal: Boolean;
  Parts, StartParts, EndParts: TIntlFormatPartArray;
  Index: Integer;
begin
  NF := UnwrapNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatRangeToParts');
  if AArgs.Length < 2 then
    ThrowTypeError('Intl.NumberFormat.prototype.formatRangeToParts requires start and end values');
  if (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) or
     (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    ThrowTypeError('Intl.NumberFormat.prototype.formatRangeToParts requires defined start and end values');

  if not TryReadDecimalRangeInput(AArgs.GetElement(0), StartValue, StartDecimal, StartHasDecimal) then
    ThrowRangeError('Intl.NumberFormat.prototype.formatRangeToParts start value must not be NaN');
  if not TryReadDecimalRangeInput(AArgs.GetElement(1), EndValue, EndDecimal, EndHasDecimal) then
    ThrowRangeError('Intl.NumberFormat.prototype.formatRangeToParts end value must not be NaN');

  if StartHasDecimal and EndHasDecimal and
     TryICUFormatNumberDecimalRangeToParts(NF.FLocale, StartDecimal, EndDecimal,
       NF.FResolvedOptions, Parts) then
    Exit(FormatPartsToArray(Parts));

  if TryICUFormatNumberRangeToParts(NF.FLocale, StartValue, EndValue,
    NF.FResolvedOptions, Parts) then
    Exit(FormatPartsToArray(Parts));

  if not TryICUFormatNumber(NF.FLocale, StartValue, NF.FResolvedOptions, StartFormatted) then
    StartFormatted := FormatNumberWithCLDR(StartValue, NF.FLocale, NF.FResolvedOptions);
  if not TryICUFormatNumber(NF.FLocale, EndValue, NF.FResolvedOptions, EndFormatted) then
    EndFormatted := FormatNumberWithCLDR(EndValue, NF.FLocale, NF.FResolvedOptions);

  if StartFormatted = EndFormatted then
  begin
    TryGetNumberPartsWithFallback(NF.FLocale, StartValue, NF.FResolvedOptions,
      'shared', StartParts);
    SetLength(Parts, 1);
    Parts[0].PartType := 'approximatelySign';
    Parts[0].Value := '~';
    Parts[0].Source := 'shared';
    AppendPartArray(Parts, StartParts);
  end
  else
  begin
    TryGetNumberPartsWithFallback(NF.FLocale, StartValue, NF.FResolvedOptions,
      'startRange', StartParts);
    TryGetNumberPartsWithFallback(NF.FLocale, EndValue, NF.FResolvedOptions,
      'endRange', EndParts);
    SetLength(Parts, 0);
    AppendPartArray(Parts, StartParts);
    SetLength(Parts, Length(Parts) + 1);
    Index := Length(Parts) - 1;
    Parts[Index].PartType := 'literal';
    Parts[Index].Value := NumberRangeSeparator(NF.FResolvedOptions);
    Parts[Index].Source := 'shared';
    AppendPartArray(Parts, EndParts);
  end;

  Result := FormatPartsToArray(Parts);
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  Obj: TGocciaObjectValue;
begin
  NF := UnwrapNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.CreateDataPropertyOrThrow('locale', TGocciaStringLiteralValue.Create(NF.FLocale));
  Obj.CreateDataPropertyOrThrow('numberingSystem',
    TGocciaStringLiteralValue.Create(NF.FNumberingSystem));
  Obj.CreateDataPropertyOrThrow('style', TGocciaStringLiteralValue.Create(NF.FStyle));
  if NF.FStyle = 'currency' then
  begin
    Obj.CreateDataPropertyOrThrow('currency', TGocciaStringLiteralValue.Create(NF.FCurrency));
    Obj.CreateDataPropertyOrThrow('currencyDisplay',
      TGocciaStringLiteralValue.Create(NF.FCurrencyDisplay));
    Obj.CreateDataPropertyOrThrow('currencySign',
      TGocciaStringLiteralValue.Create(NF.FCurrencySign));
  end;
  if NF.FStyle = 'unit' then
  begin
    Obj.CreateDataPropertyOrThrow('unit',
      TGocciaStringLiteralValue.Create(NF.FUnitIdentifier));
    Obj.CreateDataPropertyOrThrow('unitDisplay',
      TGocciaStringLiteralValue.Create(NF.FUnitDisplay));
  end;
  Obj.CreateDataPropertyOrThrow('minimumIntegerDigits',
    TGocciaNumberLiteralValue.Create(NF.FMinimumIntegerDigits));
  if NF.FMinimumFractionDigits >= 0 then
    Obj.CreateDataPropertyOrThrow('minimumFractionDigits',
      TGocciaNumberLiteralValue.Create(NF.FMinimumFractionDigits));
  if NF.FMaximumFractionDigits >= 0 then
    Obj.CreateDataPropertyOrThrow('maximumFractionDigits',
      TGocciaNumberLiteralValue.Create(NF.FMaximumFractionDigits));
  if NF.FMinimumSignificantDigits >= 0 then
    Obj.CreateDataPropertyOrThrow('minimumSignificantDigits',
      TGocciaNumberLiteralValue.Create(NF.FMinimumSignificantDigits));
  if NF.FMaximumSignificantDigits >= 0 then
    Obj.CreateDataPropertyOrThrow('maximumSignificantDigits',
      TGocciaNumberLiteralValue.Create(NF.FMaximumSignificantDigits));
  if NF.FUseGrouping = 'false' then
    Obj.CreateDataPropertyOrThrow('useGrouping', TGocciaBooleanLiteralValue.FalseValue)
  else
    Obj.CreateDataPropertyOrThrow('useGrouping',
      TGocciaStringLiteralValue.Create(NF.FUseGrouping));
  Obj.CreateDataPropertyOrThrow('notation', TGocciaStringLiteralValue.Create(NF.FNotation));
  if NF.FNotation = 'compact' then
    Obj.CreateDataPropertyOrThrow('compactDisplay',
      TGocciaStringLiteralValue.Create(NF.FCompactDisplay));
  Obj.CreateDataPropertyOrThrow('signDisplay',
    TGocciaStringLiteralValue.Create(NF.FSignDisplay));
  Obj.CreateDataPropertyOrThrow('roundingIncrement',
    TGocciaNumberLiteralValue.Create(NF.FRoundingIncrement));
  Obj.CreateDataPropertyOrThrow('roundingMode',
    TGocciaStringLiteralValue.Create(NF.FRoundingMode));
  Obj.CreateDataPropertyOrThrow('roundingPriority',
    TGocciaStringLiteralValue.Create(NF.FRoundingPriority));
  Obj.CreateDataPropertyOrThrow('trailingZeroDisplay',
    TGocciaStringLiteralValue.Create(NF.FTrailingZeroDisplay));
  Result := Obj;
end;

initialization
  GIntlNumberFormatSharedSlot := RegisterRealmOwnedSlot('Intl.NumberFormat.shared');

end.
