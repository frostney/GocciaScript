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
    FResolvedOptions: TIntlNumberFormatOptions;
    FBoundFormat: TGocciaValue;

    procedure InitializePrototype;
    procedure ReadOptions(const AOptions: TGocciaObjectValue);
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

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
  Goccia.Intl.CLDRData,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

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
begin
  if not (AValue is TGocciaIntlNumberFormatValue) then
    ThrowTypeError(AMethod + ' called on non-NumberFormat');
  Result := TGocciaIntlNumberFormatValue(AValue);
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

{ TGocciaIntlNumberFormatValue }

procedure TGocciaIntlNumberFormatValue.ReadOptions(const AOptions: TGocciaObjectValue);
var
  V: TGocciaValue;
  Ignored: string;
begin
  if not Assigned(AOptions) then Exit;

  ReadValidatedStringOption(AOptions, 'localeMatcher', Ignored);
  ReadValidatedStringOption(AOptions, 'style', FStyle);
  ReadValidatedStringOption(AOptions, 'currency', FCurrency);
  ReadValidatedStringOption(AOptions, 'currencyDisplay', FCurrencyDisplay);
  TryReadStringOption(AOptions, 'currencySign', FCurrencySign);
  TryReadStringOption(AOptions, 'unit', FUnitIdentifier);
  TryReadStringOption(AOptions, 'unitDisplay', FUnitDisplay);
  TryReadStringOption(AOptions, 'notation', FNotation);
  TryReadStringOption(AOptions, 'compactDisplay', FCompactDisplay);
  TryReadStringOption(AOptions, 'signDisplay', FSignDisplay);
  TryReadStringOption(AOptions, 'useGrouping', FUseGrouping);
  TryReadStringOption(AOptions, 'roundingMode', FRoundingMode);
  V := AOptions.GetProperty('minimumIntegerDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinimumIntegerDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('minimumFractionDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinimumFractionDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('maximumFractionDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMaximumFractionDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('minimumSignificantDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMinimumSignificantDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('maximumSignificantDigits');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FMaximumSignificantDigits := Trunc(V.ToNumberLiteral.Value);
  V := AOptions.GetProperty('roundingIncrement');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    FRoundingIncrement := Trunc(V.ToNumberLiteral.Value);
  TryReadStringOption(AOptions, 'roundingPriority', FRoundingPriority);
  TryReadStringOption(AOptions, 'trailingZeroDisplay', FTrailingZeroDisplay);
  TryReadStringOption(AOptions, 'numberingSystem', FNumberingSystem);
end;

constructor TGocciaIntlNumberFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical, CurrSymbol, CurrNarrow: string;
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

  ReadOptions(AOptions);

  // Validate style-dependent required options
  if (FStyle = 'currency') and (FCurrency = '') then
    ThrowTypeError(SErrorIntlMissingCurrency);
  if (FStyle = 'unit') and (FUnitIdentifier = '') then
    ThrowTypeError(SErrorIntlMissingUnit);

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

  // Default numberingSystem to "latn"
  if FNumberingSystem = '' then
    FNumberingSystem := 'latn';

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
  FResolvedOptions.NumberingSystem := FNumberingSystem;

  InitializePrototype;
  if Assigned(GetIntlNumberFormatShared) then
    FPrototype := GetIntlNumberFormatShared.Prototype;
end;

function TGocciaIntlNumberFormatValue.ToStringTag: string;
begin
  Result := 'Intl.NumberFormat';
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
  NF := AsNumberFormat(AThisValue, 'get Intl.NumberFormat.prototype.format');
  if not Assigned(NF.FBoundFormat) then
    NF.FBoundFormat := TGocciaIntlNumberFormatBoundFormatValue.Create(NF);
  Result := NF.FBoundFormat;
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  NumValue: Double;
  Formatted: string;
begin
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.NumberFormat.prototype.format requires a value');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUFormatNumber(NF.FLocale, NumValue, NF.FResolvedOptions, Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(
      FormatNumberWithCLDR(NumValue, NF.FLocale, NF.FResolvedOptions));
end;

function TGocciaIntlNumberFormatValue.IntlNumberFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NF: TGocciaIntlNumberFormatValue;
  NumValue: Double;
  Parts: TIntlFormatPartArray;
begin
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.NumberFormat.prototype.formatToParts requires a value');
  NumValue := AArgs.GetElement(0).ToNumberLiteral.Value;

  if TryICUFormatNumberToParts(NF.FLocale, NumValue, NF.FResolvedOptions, Parts) then
    Result := FormatPartsToArray(Parts)
  else
  begin
    SetLength(Parts, 1);
    Parts[0].PartType := 'literal';
    Parts[0].Value := FormatNumberWithCLDR(NumValue, NF.FLocale, NF.FResolvedOptions);
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
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatRange');
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
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.formatRangeToParts');
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
  NF := AsNumberFormat(AThisValue, 'Intl.NumberFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(NF.FLocale));
  Obj.AssignProperty('numberingSystem', TGocciaStringLiteralValue.Create(NF.FNumberingSystem));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(NF.FStyle));
  if NF.FStyle = 'currency' then
  begin
    Obj.AssignProperty('currency', TGocciaStringLiteralValue.Create(NF.FCurrency));
    Obj.AssignProperty('currencyDisplay', TGocciaStringLiteralValue.Create(NF.FCurrencyDisplay));
    Obj.AssignProperty('currencySign', TGocciaStringLiteralValue.Create(NF.FCurrencySign));
  end;
  if NF.FStyle = 'unit' then
  begin
    Obj.AssignProperty('unit', TGocciaStringLiteralValue.Create(NF.FUnitIdentifier));
    Obj.AssignProperty('unitDisplay', TGocciaStringLiteralValue.Create(NF.FUnitDisplay));
  end;
  Obj.AssignProperty('minimumIntegerDigits',
    TGocciaNumberLiteralValue.Create(NF.FMinimumIntegerDigits));
  if NF.FMinimumFractionDigits >= 0 then
    Obj.AssignProperty('minimumFractionDigits',
      TGocciaNumberLiteralValue.Create(NF.FMinimumFractionDigits));
  if NF.FMaximumFractionDigits >= 0 then
    Obj.AssignProperty('maximumFractionDigits',
      TGocciaNumberLiteralValue.Create(NF.FMaximumFractionDigits));
  if NF.FMinimumSignificantDigits >= 0 then
    Obj.AssignProperty('minimumSignificantDigits',
      TGocciaNumberLiteralValue.Create(NF.FMinimumSignificantDigits));
  if NF.FMaximumSignificantDigits >= 0 then
    Obj.AssignProperty('maximumSignificantDigits',
      TGocciaNumberLiteralValue.Create(NF.FMaximumSignificantDigits));
  Obj.AssignProperty('useGrouping', TGocciaStringLiteralValue.Create(NF.FUseGrouping));
  Obj.AssignProperty('notation', TGocciaStringLiteralValue.Create(NF.FNotation));
  if NF.FNotation = 'compact' then
    Obj.AssignProperty('compactDisplay', TGocciaStringLiteralValue.Create(NF.FCompactDisplay));
  Obj.AssignProperty('signDisplay', TGocciaStringLiteralValue.Create(NF.FSignDisplay));
  Obj.AssignProperty('roundingMode', TGocciaStringLiteralValue.Create(NF.FRoundingMode));
  Obj.AssignProperty('roundingIncrement',
    TGocciaNumberLiteralValue.Create(NF.FRoundingIncrement));
  Obj.AssignProperty('roundingPriority', TGocciaStringLiteralValue.Create(NF.FRoundingPriority));
  Obj.AssignProperty('trailingZeroDisplay', TGocciaStringLiteralValue.Create(NF.FTrailingZeroDisplay));
  Result := Obj;
end;

initialization
  GIntlNumberFormatSharedSlot := RegisterRealmOwnedSlot('Intl.NumberFormat.shared');

end.
