unit Goccia.Values.IntlCollator;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlCollatorValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FICULocale: string;
    FSensitivity: string;
    FUsage: string;
    FIgnorePunctuation: Boolean;
    FNumeric: Boolean;
    FCaseFirst: string;
    FCollation: string;
    FBoundCompare: TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
    function CompareStrings(const AString1, AString2: string): Integer;
  published
    function IntlCollatorCompareGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlCollatorCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlCollatorResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlCollatorSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

type
  TGocciaIntlCollatorBoundCompareValue = class(TGocciaNativeFunctionValue)
  private
    FCollator: TGocciaIntlCollatorValue;
    function Compare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ACollator: TGocciaIntlCollatorValue);
    procedure MarkReferences; override;
  end;

function GetIntlCollatorShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlCollatorSharedSlot))
  else
    Result := nil;
end;

function AsCollator(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlCollatorValue;
begin
  if not (AValue is TGocciaIntlCollatorValue) then
    ThrowTypeError(AMethod + ' called on non-Collator');
  Result := TGocciaIntlCollatorValue(AValue);
end;

{ TGocciaIntlCollatorBoundCompareValue }

constructor TGocciaIntlCollatorBoundCompareValue.Create(
  const ACollator: TGocciaIntlCollatorValue);
begin
  inherited CreateWithoutPrototype(Compare, '', 2);
  FCollator := ACollator;
end;

function TGocciaIntlCollatorBoundCompareValue.Compare(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FCollator.IntlCollatorCompare(AArgs, FCollator);
end;

procedure TGocciaIntlCollatorBoundCompareValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCollator) then
    FCollator.MarkReferences;
end;

function SensitivityStringToEnum(const AValue: string): TIntlCollatorSensitivity;
begin
  if AValue = 'base' then
    Result := icsBase
  else if AValue = 'accent' then
    Result := icsAccent
  else if AValue = 'case' then
    Result := icsCase
  else
    Result := icsVariant;
end;

function IsValidCaseFirstValue(const AValue: string): Boolean;
begin
  Result := (AValue = 'upper') or (AValue = 'lower') or (AValue = 'false');
end;

function NormalizeCollationValue(const AValue: string): string;
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

function IsSupportedCollationValue(const ALocale, AValue: string): Boolean;
const
  SupportedCollations: array[0..16] of string = (
    'big5han', 'compat', 'dict', 'direct', 'ducet', 'emoji', 'eor',
    'gb2312', 'phonebk', 'phonetic', 'pinyin', 'reformed', 'searchjl',
    'stroke', 'trad', 'unihan', 'zhuyin');
var
  Collations: IntlTypes.TStringArray;
  I: Integer;
begin
  Result := False;
  if (AValue = '') or (AValue = 'standard') or (AValue = 'search') then
    Exit;

  if TryICUGetLocaleCollations(LocaleWithoutUnicodeExtension(ALocale), Collations) then
  begin
    for I := 0 to High(Collations) do
    begin
      if SameText(NormalizeCollationValue(Collations[I]), AValue) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Exit;
  end;

  for I := Low(SupportedCollations) to High(SupportedCollations) do
  begin
    if AValue = SupportedCollations[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TryGetUnicodeExtensionKey(const ALocale, AKey: string; out AValue: string): Boolean;
var
  ExtensionStart, Index, NextDash: Integer;
  Tail, Subtag, KeyLower: string;
begin
  Result := False;
  AValue := '';
  KeyLower := LowerCase(AKey);
  ExtensionStart := Pos('-u-', LowerCase(ALocale));
  if ExtensionStart = 0 then
    Exit;

  Tail := Copy(ALocale, ExtensionStart + 3, MaxInt);
  Index := 1;
  while Index <= Length(Tail) do
  begin
    NextDash := Pos('-', Copy(Tail, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(Tail) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := LowerCase(Copy(Tail, Index, NextDash - Index));
    Index := NextDash + 1;

    if Length(Subtag) <> 2 then
      Continue;

    if Subtag = KeyLower then
    begin
      Result := True;
      while Index <= Length(Tail) do
      begin
        NextDash := Pos('-', Copy(Tail, Index, MaxInt));
        if NextDash = 0 then
          NextDash := Length(Tail) + 1
        else
          NextDash := Index + NextDash - 1;
        Subtag := LowerCase(Copy(Tail, Index, NextDash - Index));
        if Length(Subtag) = 2 then
          Exit;
        if AValue = '' then
          AValue := Subtag
        else
          AValue := AValue + '-' + Subtag;
        Index := NextDash + 1;
      end;
      Exit;
    end;
  end;
end;

function RemoveUnicodeExtensionKey(const ALocale, AKey: string): string;
var
  ExtensionStart, Index, NextDash: Integer;
  Base, Tail, Subtag, KeyLower, NewTail: string;
  Removing: Boolean;
begin
  KeyLower := LowerCase(AKey);
  ExtensionStart := Pos('-u-', LowerCase(ALocale));
  if ExtensionStart = 0 then
  begin
    Result := ALocale;
    Exit;
  end;

  Base := Copy(ALocale, 1, ExtensionStart - 1);
  Tail := Copy(ALocale, ExtensionStart + 3, MaxInt);
  NewTail := '';
  Removing := False;
  Index := 1;
  while Index <= Length(Tail) do
  begin
    NextDash := Pos('-', Copy(Tail, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(Tail) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := Copy(Tail, Index, NextDash - Index);
    Index := NextDash + 1;

    if Length(Subtag) = 2 then
      Removing := LowerCase(Subtag) = KeyLower;
    if Removing then
      Continue;
    if NewTail <> '' then
      NewTail := NewTail + '-';
    NewTail := NewTail + Subtag;
  end;

  if NewTail = '' then
    Result := Base
  else
    Result := Base + '-u-' + NewTail;
end;

function AddUnicodeExtensionKey(const ALocale, AKey, AValue: string): string;
var
  ExtensionStart: Integer;
  Addition: string;
begin
  Addition := AKey;
  if AValue <> '' then
    Addition := Addition + '-' + AValue;

  ExtensionStart := Pos('-u-', LowerCase(ALocale));
  if ExtensionStart = 0 then
    Result := ALocale + '-u-' + Addition
  else
    Result := ALocale + '-' + Addition;
end;

{ TGocciaIntlCollatorValue }

constructor TGocciaIntlCollatorValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical, RawLocale: string;
  V: TGocciaValue;
  Ignored, LocaleNumeric, LocaleCaseFirst, LocaleCollation, CollationOption: string;
  NumericOptionPresent, CaseFirstOptionPresent, CollationOptionPresent: Boolean;
begin
  inherited Create;
  RawLocale := ALocale;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FSensitivity := 'variant';
  FUsage := 'sort';
  FIgnorePunctuation := False;
  FNumeric := False;
  FCaseFirst := 'false';
  FCollation := 'default';
  NumericOptionPresent := False;
  CaseFirstOptionPresent := False;
  CollationOptionPresent := False;

  if TryGetUnicodeExtensionKey(FLocale, 'kn', LocaleNumeric) or
     TryGetUnicodeExtensionKey(RawLocale, 'kn', LocaleNumeric) then
  begin
    if (LocaleNumeric = '') or (LocaleNumeric = 'true') then
    begin
      FNumeric := True
    end
    else if LocaleNumeric = 'false' then
      FNumeric := False
    else
      FLocale := RemoveUnicodeExtensionKey(FLocale, 'kn');
    if not TryGetUnicodeExtensionKey(FLocale, 'kn', Ignored) and
       ((LocaleNumeric = '') or (LocaleNumeric = 'true') or (LocaleNumeric = 'false')) then
    begin
      if LocaleNumeric = 'true' then
        FLocale := AddUnicodeExtensionKey(FLocale, 'kn', '')
      else
        FLocale := AddUnicodeExtensionKey(FLocale, 'kn', LocaleNumeric);
    end;
  end;

  if TryGetUnicodeExtensionKey(FLocale, 'kf', LocaleCaseFirst) or
     TryGetUnicodeExtensionKey(RawLocale, 'kf', LocaleCaseFirst) then
  begin
    if IsValidCaseFirstValue(LocaleCaseFirst) then
    begin
      FCaseFirst := LocaleCaseFirst
    end
    else
      FLocale := RemoveUnicodeExtensionKey(FLocale, 'kf');
    if not TryGetUnicodeExtensionKey(FLocale, 'kf', Ignored) and
       IsValidCaseFirstValue(LocaleCaseFirst) then
      FLocale := AddUnicodeExtensionKey(FLocale, 'kf', LocaleCaseFirst);
  end;

  if TryGetUnicodeExtensionKey(FLocale, 'co', LocaleCollation) or
     TryGetUnicodeExtensionKey(RawLocale, 'co', LocaleCollation) then
  begin
    LocaleCollation := NormalizeCollationValue(LocaleCollation);
    if IsSupportedCollationValue(FLocale, LocaleCollation) then
    begin
      FCollation := LocaleCollation;
      if TryGetUnicodeExtensionKey(FLocale, 'co', Ignored) and
         (NormalizeCollationValue(Ignored) <> LocaleCollation) then
        FLocale := RemoveUnicodeExtensionKey(FLocale, 'co');
      if not TryGetUnicodeExtensionKey(FLocale, 'co', Ignored) then
        FLocale := AddUnicodeExtensionKey(FLocale, 'co', LocaleCollation);
    end
    else
      FLocale := RemoveUnicodeExtensionKey(FLocale, 'co');
  end;

  if Assigned(AOptions) then
  begin
    ReadValidatedStringOption(AOptions, 'sensitivity', FSensitivity);
    ReadValidatedStringOption(AOptions, 'usage', FUsage);
    V := AOptions.GetProperty('ignorePunctuation');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FIgnorePunctuation := V.ToBooleanLiteral.Value;
    V := AOptions.GetProperty('numeric');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      NumericOptionPresent := True;
      FNumeric := V.ToBooleanLiteral.Value;
    end;
    CaseFirstOptionPresent := TryReadStringOption(AOptions, 'caseFirst', Ignored);
    if CaseFirstOptionPresent then
    begin
      if ContainsNulCharacter(Ignored) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [Ignored, 'caseFirst']));
      FCaseFirst := Ignored;
    end;
    CollationOptionPresent := TryReadStringOption(AOptions, 'collation', CollationOption);
    CollationOption := NormalizeCollationValue(CollationOption);
    if CollationOptionPresent and IsSupportedCollationValue(FLocale, CollationOption) then
      FCollation := CollationOption;
    ReadValidatedStringOption(AOptions, 'localeMatcher', Ignored);
  end;

  if NumericOptionPresent and TryGetUnicodeExtensionKey(FLocale, 'kn', LocaleNumeric) then
  begin
    if ((LocaleNumeric = '') or (LocaleNumeric = 'true')) <> FNumeric then
      FLocale := RemoveUnicodeExtensionKey(FLocale, 'kn');
  end;
  if CaseFirstOptionPresent and TryGetUnicodeExtensionKey(FLocale, 'kf', LocaleCaseFirst) then
  begin
    if LocaleCaseFirst <> FCaseFirst then
      FLocale := RemoveUnicodeExtensionKey(FLocale, 'kf');
  end;
  if CollationOptionPresent and TryGetUnicodeExtensionKey(FLocale, 'co', LocaleCollation) then
  begin
    if NormalizeCollationValue(LocaleCollation) <> FCollation then
      FLocale := RemoveUnicodeExtensionKey(FLocale, 'co');
  end;

  FICULocale := LocaleWithoutUnicodeExtension(FLocale);
  if FCollation <> 'default' then
    FICULocale := AddUnicodeExtensionKey(FICULocale, 'co', FCollation);

  InitializePrototype;
  if Assigned(GetIntlCollatorShared) then
    FPrototype := GetIntlCollatorShared.Prototype;
end;

function TGocciaIntlCollatorValue.ToStringTag: string;
begin
  Result := 'Intl.Collator';
end;

procedure TGocciaIntlCollatorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FBoundCompare) then
    FBoundCompare.MarkReferences;
end;

procedure TGocciaIntlCollatorValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlCollatorShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlCollatorSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('compare', IntlCollatorCompareGetter, nil,
        [pfConfigurable]);
      Members.AddNamedMethod('resolvedOptions', IntlCollatorResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.Collator'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlCollatorValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlCollatorShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlCollatorValue.Create(DefaultLocale);
    Shared := GetIntlCollatorShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlCollatorValue.IntlCollatorCompareGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  C: TGocciaIntlCollatorValue;
begin
  C := AsCollator(AThisValue, 'get Intl.Collator.prototype.compare');
  if not Assigned(C.FBoundCompare) then
    C.FBoundCompare := TGocciaIntlCollatorBoundCompareValue.Create(C);
  Result := C.FBoundCompare;
end;

function TGocciaIntlCollatorValue.IntlCollatorCompare(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  C: TGocciaIntlCollatorValue;
  Str1, Str2: UnicodeString;
begin
  C := AsCollator(AThisValue, 'Intl.Collator.prototype.compare');
  // Per ECMA-402, missing arguments are ToString-coerced (undefined -> "undefined").
  Str1 := UnicodeString(AArgs.GetElement(0).ToStringLiteral.Value);
  Str2 := UnicodeString(AArgs.GetElement(1).ToStringLiteral.Value);

  Result := TGocciaNumberLiteralValue.Create(C.CompareStrings(string(Str1), string(Str2)));
end;

function TGocciaIntlCollatorValue.CompareStrings(const AString1, AString2: string): Integer;
begin
  if TryICUCompareStrings(FICULocale, UnicodeString(AString1), UnicodeString(AString2),
    SensitivityStringToEnum(FSensitivity), FIgnorePunctuation, FNumeric,
    FCaseFirst, Result) then
    Exit;
  Result := CompareStr(AString1, AString2);
end;

function TGocciaIntlCollatorValue.IntlCollatorResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  C: TGocciaIntlCollatorValue;
  Obj: TGocciaObjectValue;
begin
  C := AsCollator(AThisValue, 'Intl.Collator.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(C.FLocale));
  Obj.AssignProperty('usage', TGocciaStringLiteralValue.Create(C.FUsage));
  Obj.AssignProperty('sensitivity', TGocciaStringLiteralValue.Create(C.FSensitivity));
  Obj.AssignProperty('ignorePunctuation', TGocciaBooleanLiteralValue.Create(C.FIgnorePunctuation));
  Obj.AssignProperty('numeric', TGocciaBooleanLiteralValue.Create(C.FNumeric));
  Obj.AssignProperty('caseFirst', TGocciaStringLiteralValue.Create(C.FCaseFirst));
  Obj.AssignProperty('collation', TGocciaStringLiteralValue.Create(C.FCollation));
  Result := Obj;
end;

initialization
  GIntlCollatorSharedSlot := RegisterRealmOwnedSlot('Intl.Collator.shared');

end.
