unit Goccia.Values.IntlSegmenter;

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
  TGocciaIntlSegmenterValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FGranularity: string;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale: string; const AOptions: TGocciaObjectValue = nil);

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Locale: string read FLocale;
    property Granularity: string read FGranularity;
  published
    function IntlSegmenterSegment(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlSegmenterResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaIntlSegmentsValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FGranularity: string;
    FText: string;

    procedure InitializePrototype;
  public
    constructor Create(const ALocale, AGranularity, AText: string);

    function ToStringTag: string; override;
  published
    function IntlSegmentsContaining(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlSegmentsSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaIntlSegmentIteratorValue = class(TGocciaObjectValue)
  private
    FLocale: string;
    FGranularity: string;
    FText: UnicodeString;
    FSegments: array of TIntlSegment;
    FIndex: Integer;

    procedure InitializePrototype;
    procedure BuildSegments;
  public
    constructor Create(const ALocale, AGranularity: string; const AText: UnicodeString);

    function ToStringTag: string; override;
  published
    function IntlSegmentIteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,

  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GIntlSegmenterSharedSlot: TGocciaRealmOwnedSlotId;
  GIntlSegmentsSharedSlot: TGocciaRealmOwnedSlotId;
  GIntlSegmentIteratorSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FSegmenterPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FSegmentsPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FSegmentIteratorPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetIntlSegmenterShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlSegmenterSharedSlot))
  else
    Result := nil;
end;

function GetIntlSegmentsShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlSegmentsSharedSlot))
  else
    Result := nil;
end;

function GetIntlSegmentIteratorShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlSegmentIteratorSharedSlot))
  else
    Result := nil;
end;

function AsSegmenter(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlSegmenterValue;
begin
  if not (AValue is TGocciaIntlSegmenterValue) then
    ThrowTypeError(AMethod + ' called on non-Segmenter');
  Result := TGocciaIntlSegmenterValue(AValue);
end;

function AsSegments(const AValue: TGocciaValue; const AMethod: string): TGocciaIntlSegmentsValue;
begin
  if not (AValue is TGocciaIntlSegmentsValue) then
    ThrowTypeError(AMethod + ' called on non-Segments');
  Result := TGocciaIntlSegmentsValue(AValue);
end;

function GranularityStringToEnum(const AValue: string): TIntlSegmenterGranularity;
begin
  if AValue = 'word' then
    Result := isgWord
  else if AValue = 'sentence' then
    Result := isgSentence
  else
    Result := isgGrapheme;
end;

function CreateSegmentObject(const ASeg: TIntlSegment; const AIsWordGranularity: Boolean): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Result.AssignProperty('segment', TGocciaStringLiteralValue.Create(ASeg.Segment));
  Result.AssignProperty('index', TGocciaNumberLiteralValue.Create(ASeg.Index));
  Result.AssignProperty('input', TGocciaStringLiteralValue.Create(ASeg.Segment));
  if AIsWordGranularity then
    Result.AssignProperty('isWordLike', TGocciaBooleanLiteralValue.Create(ASeg.IsWordLike));
end;

{ TGocciaIntlSegmenterValue }

constructor TGocciaIntlSegmenterValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
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

  // Default
  FGranularity := 'grapheme';

  if Assigned(AOptions) then
  begin
    V := AOptions.GetProperty('granularity');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
      FGranularity := V.ToStringLiteral.Value;
  end;

  InitializePrototype;
  if Assigned(GetIntlSegmenterShared) then
    FPrototype := GetIntlSegmenterShared.Prototype;
end;

function TGocciaIntlSegmenterValue.ToStringTag: string;
begin
  Result := 'Intl.Segmenter';
end;

procedure TGocciaIntlSegmenterValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlSegmenterShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlSegmenterSharedSlot, Shared);
  if Length(FSegmenterPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('segment', IntlSegmenterSegment, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('resolvedOptions', IntlSegmenterResolvedOptions, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Intl.Segmenter'),
        [pfConfigurable]);
      FSegmenterPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FSegmenterPrototypeMembers);
end;

class procedure TGocciaIntlSegmenterValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetIntlSegmenterShared;
  if not Assigned(Shared) then
  begin
    TGocciaIntlSegmenterValue.Create(DefaultLocale);
    Shared := GetIntlSegmenterShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaIntlSegmenterValue.IntlSegmenterSegment(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaIntlSegmenterValue;
  InputStr: string;
begin
  S := AsSegmenter(AThisValue, 'Intl.Segmenter.prototype.segment');
  if AArgs.Length < 1 then
    ThrowTypeError('Intl.Segmenter.prototype.segment requires a string argument');
  InputStr := AArgs.GetElement(0).ToStringLiteral.Value;
  Result := TGocciaIntlSegmentsValue.Create(S.FLocale, S.FGranularity, InputStr);
end;

function TGocciaIntlSegmenterValue.IntlSegmenterResolvedOptions(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaIntlSegmenterValue;
  Obj: TGocciaObjectValue;
begin
  S := AsSegmenter(AThisValue, 'Intl.Segmenter.prototype.resolvedOptions');
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Obj.AssignProperty('locale', TGocciaStringLiteralValue.Create(S.FLocale));
  Obj.AssignProperty('granularity', TGocciaStringLiteralValue.Create(S.FGranularity));
  Result := Obj;
end;

{ TGocciaIntlSegmentsValue }

constructor TGocciaIntlSegmentsValue.Create(const ALocale, AGranularity, AText: string);
begin
  inherited Create;
  FLocale := ALocale;
  FGranularity := AGranularity;
  FText := AText;
  InitializePrototype;
  if Assigned(GetIntlSegmentsShared) then
    FPrototype := GetIntlSegmentsShared.Prototype;
end;

function TGocciaIntlSegmentsValue.ToStringTag: string;
begin
  Result := 'Segments';
end;

procedure TGocciaIntlSegmentsValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlSegmentsShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlSegmentsSharedSlot, Shared);
  if Length(FSegmentsPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('containing', IntlSegmentsContaining, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        IntlSegmentsSymbolIterator,
        0,
        [pfConfigurable, pfWritable]);
      FSegmentsPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FSegmentsPrototypeMembers);
end;

function TGocciaIntlSegmentsValue.IntlSegmentsContaining(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Segs: TGocciaIntlSegmentsValue;
  Idx: Integer;
  Iter: TGocciaIntlSegmentIteratorValue;
  I: Integer;
  Seg: TIntlSegment;
begin
  Segs := AsSegments(AThisValue, 'Segments.prototype.containing');
  if AArgs.Length < 1 then
    Idx := 0
  else
    Idx := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  // Build all segments and find the one containing the index
  Iter := TGocciaIntlSegmentIteratorValue.Create(Segs.FLocale, Segs.FGranularity,
    UnicodeString(Segs.FText));
  for I := 0 to Length(Iter.FSegments) - 1 do
  begin
    Seg := Iter.FSegments[I];
    if (Idx >= Seg.Index) and (Idx < Seg.Index + Length(Seg.Segment)) then
    begin
      Result := CreateSegmentObject(Seg, Segs.FGranularity = 'word');
      Exit;
    end;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaIntlSegmentsValue.IntlSegmentsSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Segs: TGocciaIntlSegmentsValue;
begin
  Segs := AsSegments(AThisValue, 'Segments.prototype[Symbol.iterator]');
  Result := TGocciaIntlSegmentIteratorValue.Create(Segs.FLocale, Segs.FGranularity,
    UnicodeString(Segs.FText));
end;

{ TGocciaIntlSegmentIteratorValue }

constructor TGocciaIntlSegmentIteratorValue.Create(const ALocale, AGranularity: string;
  const AText: UnicodeString);
begin
  inherited Create;
  FLocale := ALocale;
  FGranularity := AGranularity;
  FText := AText;
  FIndex := 0;
  BuildSegments;
  InitializePrototype;
  if Assigned(GetIntlSegmentIteratorShared) then
    FPrototype := GetIntlSegmentIteratorShared.Prototype;
end;

procedure TGocciaIntlSegmentIteratorValue.BuildSegments;
var
  Iterator: TIntlBreakIterator;
  PrevPos, CurrPos: Integer;
  Seg: TIntlSegment;
  RuleStatus: Integer;
begin
  SetLength(FSegments, 0);
  if not TryICUCreateBreakIterator(FLocale, GranularityStringToEnum(FGranularity),
    FText, Iterator) then
  begin
    // Fallback: single segment covering the whole string
    if Length(FText) > 0 then
    begin
      SetLength(FSegments, 1);
      FSegments[0].Segment := string(FText);
      FSegments[0].Index := 0;
      FSegments[0].IsWordLike := False;
    end;
    Exit;
  end;

  try
    PrevPos := 0;
    while TryICUBreakIteratorNext(Iterator, CurrPos) do
    begin
      if CurrPos > PrevPos then
      begin
        Seg.Segment := string(Copy(FText, PrevPos + 1, CurrPos - PrevPos));
        Seg.Index := PrevPos;
        if FGranularity = 'word' then
        begin
          RuleStatus := TryICUBreakIteratorGetRuleStatus(Iterator);
          Seg.IsWordLike := RuleStatus >= 100;
        end
        else
          Seg.IsWordLike := False;
        SetLength(FSegments, Length(FSegments) + 1);
        FSegments[Length(FSegments) - 1] := Seg;
      end;
      PrevPos := CurrPos;
    end;
  finally
    ICUBreakIteratorClose(Iterator);
  end;
end;

function TGocciaIntlSegmentIteratorValue.ToStringTag: string;
begin
  Result := 'Segment Iterator';
end;

procedure TGocciaIntlSegmentIteratorValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetIntlSegmentIteratorShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GIntlSegmentIteratorSharedSlot, Shared);
  if Length(FSegmentIteratorPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('next', IntlSegmentIteratorNext, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        IntlSegmentIteratorNext,
        0,
        [pfConfigurable, pfWritable]);
      FSegmentIteratorPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FSegmentIteratorPrototypeMembers);
end;

function TGocciaIntlSegmentIteratorValue.IntlSegmentIteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
begin
  if not (AThisValue is TGocciaIntlSegmentIteratorValue) then
    ThrowTypeError('Segment Iterator next called on non-iterator');

  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if TGocciaIntlSegmentIteratorValue(AThisValue).FIndex >= Length(TGocciaIntlSegmentIteratorValue(AThisValue).FSegments) then
  begin
    Obj.AssignProperty('value', TGocciaUndefinedLiteralValue.UndefinedValue);
    Obj.AssignProperty('done', TGocciaBooleanLiteralValue.TrueValue);
  end
  else
  begin
    Obj.AssignProperty('value', CreateSegmentObject(
      TGocciaIntlSegmentIteratorValue(AThisValue).FSegments[TGocciaIntlSegmentIteratorValue(AThisValue).FIndex],
      TGocciaIntlSegmentIteratorValue(AThisValue).FGranularity = 'word'));
    Obj.AssignProperty('done', TGocciaBooleanLiteralValue.FalseValue);
    Inc(TGocciaIntlSegmentIteratorValue(AThisValue).FIndex);
  end;
  Result := Obj;
end;

initialization
  GIntlSegmenterSharedSlot := RegisterRealmOwnedSlot('Intl.Segmenter.shared');
  GIntlSegmentsSharedSlot := RegisterRealmOwnedSlot('Intl.Segments.shared');
  GIntlSegmentIteratorSharedSlot := RegisterRealmOwnedSlot('Intl.SegmentIterator.shared');

end.
