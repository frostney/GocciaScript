unit Goccia.Values.IntlListFormat;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaIntlListFormatValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FType: string;
    FStyle: string;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);
  published
    function IntlListFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlListFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlListFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.GarbageCollector,
  Goccia.Intl.CLDRData,
  Goccia.Intl.Helpers,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlListFormatSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlListFormatShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlListFormatSharedSlot))
  else
    Result := nil;
end;

function AsListFormat(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlListFormatValue;
begin
  if not (AValue is TGocciaIntlListFormatValue) then
    ThrowTypeError(AMethod + ' called on non-ListFormat');
  Result := TGocciaIntlListFormatValue(AValue);
end;

function ListTypeStringToEnum(const AValue: string): TIntlListFormatType;
begin
  if AValue = 'disjunction' then
    Result := ilftDisjunction
  else if AValue = 'unit' then
    Result := ilftUnit
  else
    Result := ilftConjunction;
end;

function ListStyleStringToEnum(const AValue: string): TIntlListFormatStyle;
begin
  if AValue = 'short' then
    Result := ilfsShort
  else if AValue = 'narrow' then
    Result := ilfsNarrow
  else
    Result := ilfsLong;
end;

function ReadListFormatStringOption(const AOptions: TGocciaObjectValue;
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

function ExtractStringArray(const AValue: TGocciaValue): TStringArray;
var
  Iterator: TGocciaIteratorValue;
  Item: TGocciaValue;
  Done: Boolean;
  Count: Integer;
  IteratorRoot, ItemRoot: TGocciaTempRoot;
begin
  SetLength(Result, 0);

  Iterator := GetIteratorFromValue(AValue);
  if not Assigned(Iterator) then
    ThrowTypeError('Intl.ListFormat requires an iterable');

  Count := 0;
  InitializeTempRoot(IteratorRoot);
  InitializeTempRoot(ItemRoot);
  AddTempRootIfNeeded(IteratorRoot, Iterator);
  try
    try
      Item := Iterator.DirectNext(Done);
      while not Done do
      begin
        AddTempRootIfNeeded(ItemRoot, Item);
        try
          if not (Item is TGocciaStringLiteralValue) then
            ThrowTypeError('Intl.ListFormat iterable yielded a non-string value');
          SetLength(Result, Count + 1);
          Result[Count] := TGocciaStringLiteralValue(Item).Value;
          Inc(Count);
        finally
          RemoveTempRootIfNeeded(ItemRoot);
        end;
        Item := Iterator.DirectNext(Done);
      end;
    except
      CloseIteratorPreservingError(Iterator);
      raise;
    end;
  finally
    RemoveTempRootIfNeeded(IteratorRoot);
  end;
end;

function ApplyListTemplate(const ATemplate, ALeft, ARight: string): string;
var
  Tmp: string;
begin
  Tmp := StringReplace(ATemplate, '{0}', ALeft, []);
  Result := StringReplace(Tmp, '{1}', ARight, []);
end;

function FormatListWithCLDR(const AItems: TStringArray; const ALocale: string;
  AType: TIntlListFormatType; AStyle: TIntlListFormatStyle): string;
var
  Pattern: TIntlListPattern;
  Count, I: Integer;
begin
  Count := Length(AItems);
  if Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  if Count = 1 then
  begin
    Result := AItems[0];
    Exit;
  end;

  if not TryGetListPattern(ALocale, AType, AStyle, Pattern) then
  begin
    // Bare fallback: comma-join
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if I > 0 then Result := Result + ', ';
      Result := Result + AItems[I];
    end;
    Exit;
  end;

  if Count = 2 then
  begin
    if Pattern.Pair <> '' then
      Result := ApplyListTemplate(Pattern.Pair, AItems[0], AItems[1])
    else if Pattern.EndPart <> '' then
      Result := ApplyListTemplate(Pattern.EndPart, AItems[0], AItems[1])
    else
      Result := AItems[0] + ', ' + AItems[1];
    Exit;
  end;

  // 3+ items: build from the end
  if Pattern.EndPart <> '' then
    Result := ApplyListTemplate(Pattern.EndPart, AItems[Count - 2], AItems[Count - 1])
  else
    Result := AItems[Count - 2] + ', ' + AItems[Count - 1];

  for I := Count - 3 downto 1 do
  begin
    if Pattern.Middle <> '' then
      Result := ApplyListTemplate(Pattern.Middle, AItems[I], Result)
    else
      Result := AItems[I] + ', ' + Result;
  end;

  if Pattern.Start <> '' then
    Result := ApplyListTemplate(Pattern.Start, AItems[0], Result)
  else
    Result := AItems[0] + ', ' + Result;
end;

procedure AppendListPart(var AParts: TIntlFormatPartArray;
  const AType, AValue: string);
var
  Index: Integer;
begin
  if AValue = '' then Exit;
  SetLength(AParts, Length(AParts) + 1);
  Index := Length(AParts) - 1;
  AParts[Index].PartType := AType;
  AParts[Index].Value := AValue;
  AParts[Index].Source := '';
  AParts[Index].UnitIdentifier := '';
end;

procedure AppendListPartArray(var ADest: TIntlFormatPartArray;
  const ASrc: TIntlFormatPartArray);
var
  I, OldLength: Integer;
begin
  OldLength := Length(ADest);
  SetLength(ADest, OldLength + Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do
    ADest[OldLength + I] := ASrc[I];
end;

function FormatListElementPart(const AValue: string): TIntlFormatPartArray;
begin
  SetLength(Result, 0);
  AppendListPart(Result, 'element', AValue);
end;

function ApplyListTemplateParts(const ATemplate: string;
  const ALeft, ARight: TIntlFormatPartArray): TIntlFormatPartArray;
var
  I: Integer;
  Literal: string;
begin
  SetLength(Result, 0);
  Literal := '';
  I := 1;
  while I <= Length(ATemplate) do
  begin
    if Copy(ATemplate, I, 3) = '{0}' then
    begin
      AppendListPart(Result, 'literal', Literal);
      Literal := '';
      AppendListPartArray(Result, ALeft);
      Inc(I, 3);
    end
    else if Copy(ATemplate, I, 3) = '{1}' then
    begin
      AppendListPart(Result, 'literal', Literal);
      Literal := '';
      AppendListPartArray(Result, ARight);
      Inc(I, 3);
    end
    else
    begin
      Literal := Literal + ATemplate[I];
      Inc(I);
    end;
  end;
  AppendListPart(Result, 'literal', Literal);
end;

function FormatListPartsWithCLDR(const AItems: TStringArray; const ALocale: string;
  AType: TIntlListFormatType; AStyle: TIntlListFormatStyle): TIntlFormatPartArray;
var
  Pattern: TIntlListPattern;
  Count, I: Integer;
  Template: string;
begin
  SetLength(Result, 0);
  Count := Length(AItems);
  if Count = 0 then
    Exit;
  if Count = 1 then
    Exit(FormatListElementPart(AItems[0]));

  if not TryGetListPattern(ALocale, AType, AStyle, Pattern) then
  begin
    for I := 0 to Count - 1 do
    begin
      if I > 0 then
        AppendListPart(Result, 'literal', ', ');
      AppendListPartArray(Result, FormatListElementPart(AItems[I]));
    end;
    Exit;
  end;

  if Count = 2 then
  begin
    if Pattern.Pair <> '' then
      Template := Pattern.Pair
    else if Pattern.EndPart <> '' then
      Template := Pattern.EndPart
    else
      Template := '{0}, {1}';
    Exit(ApplyListTemplateParts(Template, FormatListElementPart(AItems[0]),
      FormatListElementPart(AItems[1])));
  end;

  if Pattern.EndPart <> '' then
    Template := Pattern.EndPart
  else
    Template := '{0}, {1}';
  Result := ApplyListTemplateParts(Template, FormatListElementPart(AItems[Count - 2]),
    FormatListElementPart(AItems[Count - 1]));

  for I := Count - 3 downto 1 do
  begin
    if Pattern.Middle <> '' then
      Template := Pattern.Middle
    else
      Template := '{0}, {1}';
    Result := ApplyListTemplateParts(Template, FormatListElementPart(AItems[I]), Result);
  end;

  if Pattern.Start <> '' then
    Template := Pattern.Start
  else
    Template := '{0}, {1}';
  Result := ApplyListTemplateParts(Template, FormatListElementPart(AItems[0]), Result);
end;

{ TGocciaIntlListFormatValue }

constructor TGocciaIntlListFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  Ignored: string;
begin
  inherited Create;
  Canonical := CanonicalizeUnicodeLocaleId(ALocale);
  if Canonical = '' then
    FLocale := DefaultLocale
  else
    FLocale := Canonical;

  // Defaults
  FType := 'conjunction';
  FStyle := 'long';

  if Assigned(AOptions) then
  begin
    Ignored := ReadListFormatStringOption(AOptions, 'localeMatcher',
      'best fit', ['lookup', 'best fit']);
    FType := ReadListFormatStringOption(AOptions, 'type', FType,
      ['conjunction', 'disjunction', 'unit']);
    FStyle := ReadListFormatStringOption(AOptions, 'style', FStyle,
      ['long', 'short', 'narrow']);
  end;

  InitializePrototype;
  if Assigned(GetIntlListFormatShared) then
    FPrototype := GetIntlListFormatShared.Prototype;
end;

function TGocciaIntlListFormatValue.ToStringTag: string;
begin
  Result := 'Object';
end;

procedure TGocciaIntlListFormatValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlListFormatShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlListFormatSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('format', IntlListFormatFormat, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('formatToParts', IntlListFormatFormatToParts, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlListFormatResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.ListFormat'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaIntlListFormatValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlListFormatShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlListFormatValue.Create(DefaultLocale);
    Shared := GetIntlListFormatShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlListFormatValue.IntlListFormatFormat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LF: TGocciaIntlListFormatValue;
  Items: TStringArray;
  Formatted: string;
begin
  LF := AsListFormat(AThisValue, 'Intl.ListFormat.prototype.format');

  Items := ExtractStringArray(AArgs.GetElement(0));
  if TryICUFormatList(LF.FLocale, Items, ListTypeStringToEnum(LF.FType),
    ListStyleStringToEnum(LF.FStyle), Formatted) then
    Result := TGocciaStringLiteralValue.Create(Formatted)
  else
    Result := TGocciaStringLiteralValue.Create(
      FormatListWithCLDR(Items, LF.FLocale, ListTypeStringToEnum(LF.FType),
        ListStyleStringToEnum(LF.FStyle)));
end;

function TGocciaIntlListFormatValue.IntlListFormatFormatToParts(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LF: TGocciaIntlListFormatValue;
  Items: TStringArray;
  Parts: TIntlFormatPartArray;
begin
  LF := AsListFormat(AThisValue, 'Intl.ListFormat.prototype.formatToParts');

  Items := ExtractStringArray(AArgs.GetElement(0));
  if TryICUFormatListToParts(LF.FLocale, Items, ListTypeStringToEnum(LF.FType),
    ListStyleStringToEnum(LF.FStyle), Parts) then
    Exit(FormatPartsToArray(Parts));

  Result := FormatPartsToArray(FormatListPartsWithCLDR(Items, LF.FLocale,
    ListTypeStringToEnum(LF.FType), ListStyleStringToEnum(LF.FStyle)));
end;

function TGocciaIntlListFormatValue.IntlListFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LF: TGocciaIntlListFormatValue;
  Obj: TGocciaObjectValue;
begin
  LF := AsListFormat(AThisValue, 'Intl.ListFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.CreateDataPropertyOrThrow('locale', TGocciaStringLiteralValue.Create(LF.FLocale));
  Obj.CreateDataPropertyOrThrow('type', TGocciaStringLiteralValue.Create(LF.FType));
  Obj.CreateDataPropertyOrThrow('style', TGocciaStringLiteralValue.Create(LF.FStyle));
  Result := Obj;
end;

initialization
  GIntlListFormatSharedSlot := RegisterRealmOwnedSlot('Intl.ListFormat.shared');

end.
