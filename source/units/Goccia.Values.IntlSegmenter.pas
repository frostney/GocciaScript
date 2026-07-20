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
    FOriginalText: string;
    FText: string;
    FSegments: array of TIntlSegment;
    FIndex: Integer;

    procedure InitializePrototype;
    procedure BuildSegments;
    procedure BuildSegmentsFallback;
  public
    constructor Create(const ALocale, AGranularity, AText: string);

    function ToStringTag: string; override;
  published
    function IntlSegmentIteratorNext(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function IntlSegmentIteratorSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  IntlICU,
  IntlLocaleResolver,
  TextSemantics,

  Goccia.Error.Messages,
  Goccia.ObjectModel.Types,
  Goccia.Realm,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
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

procedure ClearThreadvarMembers;
begin
  SetLength(FSegmenterPrototypeMembers, 0);
  SetLength(FSegmentsPrototypeMembers, 0);
  SetLength(FSegmentIteratorPrototypeMembers, 0);
end;

function GetIntlSegmenterShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlSegmenterSharedSlot))
  else
    Result := nil;
end;

function GetIntlSegmentsShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GIntlSegmentsSharedSlot))
  else
    Result := nil;
end;

function GetIntlSegmentIteratorShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
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

function CreateSegmentObject(const ASeg: TIntlSegment; const AIsWordGranularity: Boolean;
  const AInput: string): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Result.DefineProperty('segment', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(ASeg.Segment),
    [pfEnumerable, pfConfigurable, pfWritable]));
  Result.DefineProperty('index', TGocciaPropertyDescriptorData.Create(
    TGocciaNumberLiteralValue.Create(ASeg.Index),
    [pfEnumerable, pfConfigurable, pfWritable]));
  Result.DefineProperty('input', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(AInput),
    [pfEnumerable, pfConfigurable, pfWritable]));
  if AIsWordGranularity then
    Result.DefineProperty('isWordLike', TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(ASeg.IsWordLike),
      [pfEnumerable, pfConfigurable, pfWritable]));
end;

function ReadSegmenterStringOption(const AOptions: TGocciaObjectValue;
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

{ TGocciaIntlSegmenterValue }

constructor TGocciaIntlSegmenterValue.Create(const ALocale: string; const AOptions: TGocciaObjectValue);
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

  // Default
  FGranularity := 'grapheme';

  if Assigned(AOptions) then
  begin
    Ignored := ReadSegmenterStringOption(AOptions, 'localeMatcher',
      'best fit', ['lookup', 'best fit']);
    FGranularity := ReadSegmenterStringOption(AOptions, 'granularity',
      FGranularity, ['grapheme', 'word', 'sentence']);
  end;

  InitializePrototype;
  if (GetIntlSegmenterShared <> nil) then
    FPrototype := GetIntlSegmenterShared.Prototype;
end;

function TGocciaIntlSegmenterValue.ToStringTag: string;
begin
  Result := 'Object';
end;

procedure TGocciaIntlSegmenterValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetIntlSegmenterShared <> nil) then Exit;

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
  Obj.DefineProperty('locale', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(S.FLocale),
    [pfEnumerable, pfConfigurable, pfWritable]));
  Obj.DefineProperty('granularity', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(S.FGranularity),
    [pfEnumerable, pfConfigurable, pfWritable]));
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
  if (GetIntlSegmentsShared <> nil) then
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
  if (CurrentRealm = nil) then Exit;
  if (GetIntlSegmentsShared <> nil) then Exit;

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
    Idx := ToIntegerFromArgs(AArgs, 0);

  // Build all segments and find the one containing the index
  Iter := TGocciaIntlSegmentIteratorValue.Create(Segs.FLocale, Segs.FGranularity,
    Segs.FText);
  try
    for I := 0 to Length(Iter.FSegments) - 1 do
    begin
      Seg := Iter.FSegments[I];
      if (Idx >= Seg.Index) and
         (Idx < Seg.Index + UTF16CodeUnitLength(Seg.Segment)) then
      begin
        Result := CreateSegmentObject(Seg, Segs.FGranularity = 'word', Segs.FText);
        Exit;
      end;
    end;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  finally
    Iter.Free;
  end;
end;

function TGocciaIntlSegmentsValue.IntlSegmentsSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Segs: TGocciaIntlSegmentsValue;
begin
  Segs := AsSegments(AThisValue, 'Segments.prototype[Symbol.iterator]');
  Result := TGocciaIntlSegmentIteratorValue.Create(Segs.FLocale, Segs.FGranularity,
    Segs.FText);
end;

{ TGocciaIntlSegmentIteratorValue }

constructor TGocciaIntlSegmentIteratorValue.Create(const ALocale, AGranularity,
  AText: string);
begin
  inherited Create;
  FLocale := ALocale;
  FGranularity := AGranularity;
  FOriginalText := AText;
  FText := string(AText);
  FIndex := 0;
  BuildSegments;
  InitializePrototype;
  if (GetIntlSegmentIteratorShared <> nil) then
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
    BuildSegmentsFallback;
    Exit;
  end;

  try
    PrevPos := 0;
    while TryICUBreakIteratorNext(Iterator, CurrPos) do
    begin
      if CurrPos > PrevPos then
      begin
        Seg.Segment := UTF16Substring(FOriginalText, PrevPos, CurrPos - PrevPos);
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

function IsWordBoundaryChar(C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9) or (C = #10) or (C = #13) or
            (C = ',') or (C = '.') or (C = '!') or (C = '?') or
            (C = ';') or (C = ':') or (C = '(') or (C = ')') or
            (C = '[') or (C = ']') or (C = '{') or (C = '}') or
            (C = '"') or (C = '''') or (C = '-');
end;

function IsSentenceEndChar(C: Char): Boolean;
begin
  Result := (C = '.') or (C = '!') or (C = '?');
end;

procedure TGocciaIntlSegmentIteratorValue.BuildSegmentsFallback;
var
  Text: string;
  Idx, SeqLen, Utf16Index: Integer;
  CodePoint: Cardinal;
  Seg: TIntlSegment;
  SegStart: Integer;
  C: Char;
begin
  Text := FOriginalText;

  if FGranularity = 'grapheme' then
  begin
    // Split into individual code points via UTF-8 iteration
    Idx := 1;
    Utf16Index := 0;
    while Idx <= Length(Text) do
    begin
      if TryReadCodePointAt(Text, Idx, CodePoint, SeqLen) then
      begin
        Seg.Segment := Copy(Text, Idx, SeqLen);
        Seg.Index := Utf16Index;
        Seg.IsWordLike := False;
        SetLength(FSegments, Length(FSegments) + 1);
        FSegments[Length(FSegments) - 1] := Seg;
        Inc(Utf16Index, UTF16CodeUnitLength(Seg.Segment));
        Inc(Idx, SeqLen);
      end
      else
      begin
        Inc(Utf16Index);
        Inc(Idx);
      end;
    end;
  end
  else if FGranularity = 'word' then
  begin
    // Split on word boundaries (whitespace/punctuation)
    Idx := 1;
    Utf16Index := 0;
    while Idx <= Length(Text) do
    begin
      SegStart := Idx;
      C := Text[Idx];

      if IsWordBoundaryChar(C) then
      begin
        // Non-word segment: consume consecutive boundary chars
        while (Idx <= Length(Text)) and IsWordBoundaryChar(Text[Idx]) do
          Inc(Idx);
        Seg.Segment := Copy(Text, SegStart, Idx - SegStart);
        Seg.Index := Utf16Index;
        Seg.IsWordLike := False;
      end
      else
      begin
        // Word segment: consume until boundary
        while (Idx <= Length(Text)) and not IsWordBoundaryChar(Text[Idx]) do
          Inc(Idx);
        Seg.Segment := Copy(Text, SegStart, Idx - SegStart);
        Seg.Index := Utf16Index;
        Seg.IsWordLike := True;
      end;

      SetLength(FSegments, Length(FSegments) + 1);
      FSegments[Length(FSegments) - 1] := Seg;
      Inc(Utf16Index, UTF16CodeUnitLength(Seg.Segment));
    end;
  end
  else if FGranularity = 'sentence' then
  begin
    // Split on sentence endings: .!? followed by whitespace or end of string
    SegStart := 1;
    Idx := 1;
    Utf16Index := 0;
    while Idx <= Length(Text) do
    begin
      if IsSentenceEndChar(Text[Idx]) then
      begin
        // Consume trailing whitespace after sentence end
        Inc(Idx);
        while (Idx <= Length(Text)) and
              ((Text[Idx] = ' ') or (Text[Idx] = #10) or
               (Text[Idx] = #13) or (Text[Idx] = #9)) do
          Inc(Idx);

        Seg.Segment := Copy(Text, SegStart, Idx - SegStart);
        Seg.Index := Utf16Index;
        Seg.IsWordLike := False;
        SetLength(FSegments, Length(FSegments) + 1);
        FSegments[Length(FSegments) - 1] := Seg;
        Inc(Utf16Index, UTF16CodeUnitLength(Seg.Segment));
        SegStart := Idx;
      end
      else
        Inc(Idx);
    end;

    // Remaining text as final segment
    if SegStart <= Length(Text) then
    begin
      Seg.Segment := Copy(Text, SegStart, Length(Text) - SegStart + 1);
      Seg.Index := Utf16Index;
      Seg.IsWordLike := False;
      SetLength(FSegments, Length(FSegments) + 1);
      FSegments[Length(FSegments) - 1] := Seg;
    end;
  end
  else
  begin
    // Unknown granularity: single segment
    if Length(Text) > 0 then
    begin
      SetLength(FSegments, 1);
      FSegments[0].Segment := Text;
      FSegments[0].Index := 0;
      FSegments[0].IsWordLike := False;
    end;
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
  if (CurrentRealm = nil) then Exit;
  if (GetIntlSegmentIteratorShared <> nil) then Exit;

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
        IntlSegmentIteratorSymbolIterator,
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
      TGocciaIntlSegmentIteratorValue(AThisValue).FGranularity = 'word',
      TGocciaIntlSegmentIteratorValue(AThisValue).FOriginalText));
    Obj.AssignProperty('done', TGocciaBooleanLiteralValue.FalseValue);
    Inc(TGocciaIntlSegmentIteratorValue(AThisValue).FIndex);
  end;
  Result := Obj;
end;

function TGocciaIntlSegmentIteratorValue.IntlSegmentIteratorSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaIntlSegmentIteratorValue) then
    ThrowTypeError('Segment Iterator [Symbol.iterator] called on non-iterator');
  Result := AThisValue;
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);
  GIntlSegmenterSharedSlot := RegisterRealmOwnedSlot('Intl.Segmenter.shared');
  GIntlSegmentsSharedSlot := RegisterRealmOwnedSlot('Intl.Segments.shared');
  GIntlSegmentIteratorSharedSlot := RegisterRealmOwnedSlot('Intl.SegmentIterator.shared');

end.
