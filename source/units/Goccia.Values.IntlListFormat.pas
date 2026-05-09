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

  IntlCLDRData,
  IntlICU,
  IntlLocaleResolver,
  IntlTypes,

  Goccia.Error.Messages,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
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

function ExtractStringArray(const AValue: TGocciaValue): TStringArray;
var
  ArrValue: TGocciaArrayValue;
  I: Integer;
begin
  SetLength(Result, 0);
  if AValue is TGocciaArrayValue then
  begin
    ArrValue := TGocciaArrayValue(AValue);
    SetLength(Result, ArrValue.Elements.Count);
    for I := 0 to ArrValue.Elements.Count - 1 do
      Result[I] := ArrValue.Elements[I].ToStringLiteral.Value;
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

{ TGocciaIntlListFormatValue }

constructor TGocciaIntlListFormatValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
var
  Canonical: string;
  V: TGocciaValue;
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
    V := AOptions.GetProperty('type');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FType := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FType) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FType, 'type']));
    end;
    V := AOptions.GetProperty('style');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      FStyle := V.ToStringLiteral.Value;
      if ContainsNulCharacter(FStyle) then
        ThrowRangeError(Format(SErrorIntlInvalidOption, [FStyle, 'style']));
    end;
  end;

  InitializePrototype;
  if Assigned(GetIntlListFormatShared) then
    FPrototype := GetIntlListFormatShared.Prototype;
end;

function TGocciaIntlListFormatValue.ToStringTag: string;
begin
  Result := 'Intl.ListFormat';
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
  I: Integer;
begin
  LF := AsListFormat(AThisValue, 'Intl.ListFormat.prototype.format');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.ListFormat.prototype.format requires a list argument');

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
  Formatted: string;
  Arr: TGocciaArrayValue;
  PartObj: TGocciaObjectValue;
  I: Integer;
begin
  LF := AsListFormat(AThisValue, 'Intl.ListFormat.prototype.formatToParts');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.ListFormat.prototype.formatToParts requires a list argument');

  Items := ExtractStringArray(AArgs.GetElement(0));
  Arr := TGocciaArrayValue.Create;

  if not TryICUFormatList(LF.FLocale, Items, ListTypeStringToEnum(LF.FType),
    ListStyleStringToEnum(LF.FStyle), Formatted) then
    Formatted := FormatListWithCLDR(Items, LF.FLocale,
      ListTypeStringToEnum(LF.FType), ListStyleStringToEnum(LF.FStyle));

  for I := 0 to Length(Items) - 1 do
  begin
    if I > 0 then
    begin
      PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
      PartObj.AssignProperty('type', TGocciaStringLiteralValue.Create('literal'));
      PartObj.AssignProperty('value', TGocciaStringLiteralValue.Create(', '));
      Arr.Elements.Add(PartObj);
    end;
    PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    PartObj.AssignProperty('type', TGocciaStringLiteralValue.Create('element'));
    PartObj.AssignProperty('value', TGocciaStringLiteralValue.Create(Items[I]));
    Arr.Elements.Add(PartObj);
  end;
  Result := Arr;
end;

function TGocciaIntlListFormatValue.IntlListFormatResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LF: TGocciaIntlListFormatValue;
  Obj: TGocciaObjectValue;
begin
  LF := AsListFormat(AThisValue, 'Intl.ListFormat.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(LF.FLocale));
  Obj.AssignProperty('type', TGocciaStringLiteralValue.Create(LF.FType));
  Obj.AssignProperty('style', TGocciaStringLiteralValue.Create(LF.FStyle));
  Result := Obj;
end;

initialization
  GIntlListFormatSharedSlot := RegisterRealmOwnedSlot('Intl.ListFormat.shared');

end.
