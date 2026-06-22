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

procedure ValidateStringOptionValue(const AValue, AName: string; const AAllowed: array of string);
var
  I: Integer;
begin
  for I := Low(AAllowed) to High(AAllowed) do
  begin
    if AValue = AAllowed[I] then
      Exit;
  end;
  ThrowRangeError(Format(SErrorIntlInvalidOption, [AValue, AName]));
end;

function ReadCollatorStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; var AValue: string; const AAllowed: array of string): Boolean;
var
  S: string;
begin
  Result := False;
  if TryReadStringOption(AOptions, AName, S) then
  begin
    if ContainsNulCharacter(S) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [S, AName]));
    ValidateStringOptionValue(S, AName, AAllowed);
    AValue := S;
    Result := True;
  end;
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

function RemoveUnicodeLocaleExtensionAttributes(const ALocale: string): string;
var
  LocaleLower, Tail, Suffix, Subtag, NewTail: string;
  UnicodePos, ExtensionEnd, Index, NextDash, SubtagStart: Integer;
  SeenKeyword: Boolean;

  procedure AppendSubtag(const ASubtag: string);
  begin
    if NewTail <> '' then
      NewTail := NewTail + '-';
    NewTail := NewTail + ASubtag;
  end;

begin
  LocaleLower := LowerCase(ALocale);
  UnicodePos := Pos('-u-', LocaleLower);
  if UnicodePos = 0 then
    Exit(ALocale);

  ExtensionEnd := Length(ALocale) + 1;
  Index := UnicodePos + 3;
  while Index <= Length(ALocale) do
  begin
    SubtagStart := Index;
    NextDash := Pos('-', Copy(ALocale, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(ALocale) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := LowerCase(Copy(ALocale, Index, NextDash - Index));
    if Length(Subtag) = 1 then
    begin
      ExtensionEnd := SubtagStart - 1;
      Break;
    end;
    Index := NextDash + 1;
  end;

  Tail := Copy(ALocale, UnicodePos + 3, ExtensionEnd - UnicodePos - 3);
  Suffix := Copy(ALocale, ExtensionEnd, MaxInt);
  NewTail := '';
  SeenKeyword := False;
  Index := 1;
  while Index <= Length(Tail) do
  begin
    NextDash := Pos('-', Copy(Tail, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(Tail) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := Copy(Tail, Index, NextDash - Index);
    if Length(Subtag) = 2 then
      SeenKeyword := True;
    if SeenKeyword then
      AppendSubtag(Subtag);
    Index := NextDash + 1;
  end;

  if NewTail = '' then
    Result := Copy(ALocale, 1, UnicodePos - 1) + Suffix
  else
    Result := Copy(ALocale, 1, UnicodePos + 2) + NewTail + Suffix;
end;

function CollatorLocaleLanguage(const ALocale: string): string;
var
  DashPos: Integer;
begin
  Result := LowerCase(LocaleWithoutUnicodeExtension(ALocale));
  DashPos := Pos('-', Result);
  if DashPos > 0 then
    Result := Copy(Result, 1, DashPos - 1);
end;

function DefaultIgnorePunctuationForLocale(const ALocale: string): Boolean;
begin
  Result := CollatorLocaleLanguage(ALocale) = 'th';
end;

function KeepCollatorLocaleExtensionKeywords(const ALocale: string): string;
var
  Value: string;
begin
  Result := LocaleWithoutUnicodeExtension(ALocale);
  if TryGetUnicodeLocaleExtensionKeyword(ALocale, 'co', Value) then
    Result := AddUnicodeLocaleExtensionKeyword(Result, 'co', Value);
  if TryGetUnicodeLocaleExtensionKeyword(ALocale, 'kf', Value) then
    Result := AddUnicodeLocaleExtensionKeyword(Result, 'kf', Value);
  if TryGetUnicodeLocaleExtensionKeyword(ALocale, 'kn', Value) then
    Result := AddUnicodeLocaleExtensionKeyword(Result, 'kn', Value);
end;

function IsSupportedCollationValue(const ALocale, AValue: string): Boolean;
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
  end;
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
    FLocale := RemoveUnicodeLocaleExtensionAttributes(Canonical);

  // Defaults
  FSensitivity := 'variant';
  FUsage := 'sort';
  FIgnorePunctuation := DefaultIgnorePunctuationForLocale(FLocale);
  FNumeric := False;
  FCaseFirst := 'false';
  FCollation := 'default';
  NumericOptionPresent := False;
  CaseFirstOptionPresent := False;
  CollationOptionPresent := False;

  if TryGetUnicodeLocaleExtensionKeyword(FLocale, 'kn', LocaleNumeric) or
     TryGetUnicodeLocaleExtensionKeyword(RawLocale, 'kn', LocaleNumeric) then
  begin
    if (LocaleNumeric = '') or (LocaleNumeric = 'true') then
    begin
      FNumeric := True
    end
    else if LocaleNumeric = 'false' then
      FNumeric := False
    else
      FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'kn');
    if not TryGetUnicodeLocaleExtensionKeyword(FLocale, 'kn', Ignored) and
       ((LocaleNumeric = '') or (LocaleNumeric = 'true') or (LocaleNumeric = 'false')) then
    begin
      if LocaleNumeric = 'true' then
        FLocale := AddUnicodeLocaleExtensionKeyword(FLocale, 'kn', '')
      else
        FLocale := AddUnicodeLocaleExtensionKeyword(FLocale, 'kn', LocaleNumeric);
    end;
  end;

  if TryGetUnicodeLocaleExtensionKeyword(FLocale, 'kf', LocaleCaseFirst) or
     TryGetUnicodeLocaleExtensionKeyword(RawLocale, 'kf', LocaleCaseFirst) then
  begin
    if IsValidCaseFirstValue(LocaleCaseFirst) then
    begin
      FCaseFirst := LocaleCaseFirst
    end
    else
      FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'kf');
    if not TryGetUnicodeLocaleExtensionKeyword(FLocale, 'kf', Ignored) and
       IsValidCaseFirstValue(LocaleCaseFirst) then
      FLocale := AddUnicodeLocaleExtensionKeyword(FLocale, 'kf', LocaleCaseFirst);
  end;

  if TryGetUnicodeLocaleExtensionKeyword(FLocale, 'co', LocaleCollation) or
     TryGetUnicodeLocaleExtensionKeyword(RawLocale, 'co', LocaleCollation) then
  begin
    LocaleCollation := NormalizeCollationValue(LocaleCollation);
    if IsSupportedCollationValue(FLocale, LocaleCollation) then
    begin
      FCollation := LocaleCollation;
      if TryGetUnicodeLocaleExtensionKeyword(FLocale, 'co', Ignored) and
         (NormalizeCollationValue(Ignored) <> LocaleCollation) then
        FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'co');
      if not TryGetUnicodeLocaleExtensionKeyword(FLocale, 'co', Ignored) then
        FLocale := AddUnicodeLocaleExtensionKeyword(FLocale, 'co', LocaleCollation);
    end
    else
      FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'co');
  end;

  if Assigned(AOptions) then
  begin
    ReadCollatorStringOption(AOptions, 'sensitivity', FSensitivity,
      ['base', 'accent', 'case', 'variant']);
    ReadCollatorStringOption(AOptions, 'usage', FUsage, ['sort', 'search']);
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
      if not IsValidCaseFirstValue(Ignored) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [Ignored, 'caseFirst']));
      FCaseFirst := Ignored;
    end;
    CollationOptionPresent := TryReadStringOption(AOptions, 'collation', CollationOption);
    CollationOption := NormalizeCollationValue(CollationOption);
    if CollationOptionPresent and IsSupportedCollationValue(FLocale, CollationOption) then
      FCollation := CollationOption;
    ReadCollatorStringOption(AOptions, 'localeMatcher', Ignored, ['lookup', 'best fit']);
  end;

  if NumericOptionPresent and TryGetUnicodeLocaleExtensionKeyword(FLocale, 'kn', LocaleNumeric) then
  begin
    if ((LocaleNumeric = '') or (LocaleNumeric = 'true')) <> FNumeric then
      FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'kn');
  end;
  if CaseFirstOptionPresent and TryGetUnicodeLocaleExtensionKeyword(FLocale, 'kf', LocaleCaseFirst) then
  begin
    if LocaleCaseFirst <> FCaseFirst then
      FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'kf');
  end;
  if FUsage = 'search' then
  begin
    FCollation := 'default';
    FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'co');
  end
  else if CollationOptionPresent and TryGetUnicodeLocaleExtensionKeyword(FLocale, 'co', LocaleCollation) then
  begin
    if NormalizeCollationValue(LocaleCollation) <> FCollation then
      FLocale := RemoveUnicodeLocaleExtensionKeyword(FLocale, 'co');
  end;

  FLocale := KeepCollatorLocaleExtensionKeywords(FLocale);
  FICULocale := LocaleWithoutUnicodeExtension(FLocale);
  if FUsage = 'search' then
    FICULocale := AddUnicodeLocaleExtensionKeyword(FICULocale, 'co', 'search')
  else if FCollation <> 'default' then
    FICULocale := AddUnicodeLocaleExtensionKeyword(FICULocale, 'co', FCollation);

  InitializePrototype;
  if Assigned(GetIntlCollatorShared) then
    FPrototype := GetIntlCollatorShared.Prototype;
end;

function TGocciaIntlCollatorValue.ToStringTag: string;
begin
  Result := 'Object';
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
  Obj.CreateDataPropertyOrThrow('locale', TGocciaStringLiteralValue.Create(C.FLocale));
  Obj.CreateDataPropertyOrThrow('usage', TGocciaStringLiteralValue.Create(C.FUsage));
  Obj.CreateDataPropertyOrThrow('sensitivity', TGocciaStringLiteralValue.Create(C.FSensitivity));
  Obj.CreateDataPropertyOrThrow('ignorePunctuation', TGocciaBooleanLiteralValue.Create(C.FIgnorePunctuation));
  Obj.CreateDataPropertyOrThrow('collation', TGocciaStringLiteralValue.Create(C.FCollation));
  Obj.CreateDataPropertyOrThrow('numeric', TGocciaBooleanLiteralValue.Create(C.FNumeric));
  Obj.CreateDataPropertyOrThrow('caseFirst', TGocciaStringLiteralValue.Create(C.FCaseFirst));
  Result := Obj;
end;

initialization
  GIntlCollatorSharedSlot := RegisterRealmOwnedSlot('Intl.Collator.shared');

end.
