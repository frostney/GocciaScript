unit Goccia.Values.Iterator.Concrete;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaArrayIteratorKind = (akValues, akKeys, akEntries);

  TGocciaArrayIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
    FKind: TGocciaArrayIteratorKind;
  public
    constructor Create(const ASource: TGocciaValue; const AKind: TGocciaArrayIteratorKind);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

  TGocciaStringIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
    function TryReadAndAdvance(out AText: string): Boolean;
  public
    constructor Create(const ASource: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

  TGocciaMapIteratorKind = (mkEntries, mkKeys, mkValues);

  TGocciaMapIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
    FKind: TGocciaMapIteratorKind;
  public
    constructor Create(const ASource: TGocciaValue; const AKind: TGocciaMapIteratorKind);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

  TGocciaSetIteratorKind = (skValues, skEntries);

  TGocciaSetIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
    FKind: TGocciaSetIteratorKind;
  public
    constructor Create(const ASource: TGocciaValue; const AKind: TGocciaSetIteratorKind);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

  TGocciaURLSearchParamsIteratorKind = (spikKeys, spikValues, spikEntries);

  TGocciaURLSearchParamsIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
    FKind: TGocciaURLSearchParamsIteratorKind;
  public
    constructor Create(const ASource: TGocciaValue;
      const AKind: TGocciaURLSearchParamsIteratorKind);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

  TGocciaHeadersIteratorKind = (hikEntries, hikKeys, hikValues);

  TGocciaHeadersIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
    FKind: TGocciaHeadersIteratorKind;
  public
    constructor Create(const ASource: TGocciaValue;
      const AKind: TGocciaHeadersIteratorKind);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

function EnsureArrayIteratorPrototype: TGocciaObjectValue;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.HeadersValue,
  Goccia.Values.MapValue,
  Goccia.Values.SetValue,
  Goccia.Values.ToObject,
  Goccia.Values.URLSearchParamsValue;

var
  GArrayIteratorPrototypeSlot: TGocciaRealmSlotId;
  GStringIteratorPrototypeSlot: TGocciaRealmSlotId;
  GMapIteratorPrototypeSlot: TGocciaRealmSlotId;
  GSetIteratorPrototypeSlot: TGocciaRealmSlotId;

function AddTempRootIfNeeded(const AValue: TGocciaValue): Boolean;
begin
  Result := Assigned(TGarbageCollector.Instance) and Assigned(AValue) and
    not TGarbageCollector.Instance.IsTempRoot(AValue);
  if Result then
    TGarbageCollector.Instance.AddTempRoot(AValue);
end;

procedure RemoveTempRootIfNeeded(
  const AValue: TGocciaValue; const AWasRooted: Boolean);
begin
  if AWasRooted then
    TGarbageCollector.Instance.RemoveTempRoot(AValue);
end;

{ TGocciaArrayIteratorValue }

function EnsureArrayIteratorPrototype: TGocciaObjectValue;
begin
  Result := TGocciaArrayIteratorValue.EnsureConcreteIteratorPrototype(
    GArrayIteratorPrototypeSlot, 'Array Iterator');
end;

constructor TGocciaArrayIteratorValue.Create(const ASource: TGocciaValue; const AKind: TGocciaArrayIteratorKind);
var
  SharedPrototype: TGocciaObjectValue;
  SourceWasRooted: Boolean;
begin
  SourceWasRooted := AddTempRootIfNeeded(ASource);
  try
    inherited Create;
    SharedPrototype := EnsureArrayIteratorPrototype;
    if Assigned(SharedPrototype) then
      FPrototype := SharedPrototype;
    FSource := ASource;
    FIndex := 0;
    FKind := AKind;
  finally
    RemoveTempRootIfNeeded(ASource, SourceWasRooted);
  end;
end;

function GetArrayIteratorLen(const ASource: TGocciaValue): Integer;
begin
  if ASource is TGocciaArrayValue then
    Result := TGocciaArrayValue(ASource).Elements.Count
  else
    Result := LengthOfArrayLike(TGocciaObjectValue(ASource));
end;

function GetArrayIteratorElement(const ASource: TGocciaValue; const AIndex: Integer): TGocciaValue;
begin
  // ES2026 §23.1.5.2.1 step 15.b: Let elementValue be ? Get(array, elementKey).
  Result := TGocciaObjectValue(ASource).GetProperty(IntToStr(AIndex));
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  EntryArray: TGocciaArrayValue;
  Element: TGocciaValue;
  Len: Integer;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  Len := GetArrayIteratorLen(FSource);
  if FIndex >= Len then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  case FKind of
    akValues:
    begin
      Element := GetArrayIteratorElement(FSource, FIndex);
      Result := CreateIteratorResult(Element, False);
    end;
    akKeys:
      Result := CreateIteratorResult(TGocciaNumberLiteralValue.Create(FIndex), False);
    akEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(TGocciaNumberLiteralValue.Create(FIndex));
      Element := GetArrayIteratorElement(FSource, FIndex);
      EntryArray.Elements.Add(Element);
      Result := CreateIteratorResult(EntryArray, False);
    end;
  end;
  Inc(FIndex);
end;

function TGocciaArrayIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  EntryArray: TGocciaArrayValue;
  Element: TGocciaValue;
  Len: Integer;
begin
  Len := GetArrayIteratorLen(FSource);
  if FDone or (FIndex >= Len) then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ADone := False;
  case FKind of
    akValues:
    begin
      Element := GetArrayIteratorElement(FSource, FIndex);
      Result := Element;
    end;
    akKeys:
      Result := TGocciaNumberLiteralValue.Create(FIndex);
    akEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(TGocciaNumberLiteralValue.Create(FIndex));
      Element := GetArrayIteratorElement(FSource, FIndex);
      EntryArray.Elements.Add(Element);
      Result := EntryArray;
    end;
  end;
  Inc(FIndex);
end;

function TGocciaArrayIteratorValue.ToStringTag: string;
begin
  Result := 'Array Iterator';
end;

procedure TGocciaArrayIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;

{ TGocciaStringIteratorValue }

constructor TGocciaStringIteratorValue.Create(const ASource: TGocciaValue);
var
  SharedPrototype: TGocciaObjectValue;
  SourceWasRooted: Boolean;
begin
  SourceWasRooted := AddTempRootIfNeeded(ASource);
  try
    inherited Create;
    SharedPrototype := EnsureConcreteIteratorPrototype(
      GStringIteratorPrototypeSlot, 'String Iterator');
    if Assigned(SharedPrototype) then
      FPrototype := SharedPrototype;
    FSource := ASource;
    FIndex := 0;
  finally
    RemoveTempRootIfNeeded(ASource, SourceWasRooted);
  end;
end;

function TGocciaStringIteratorValue.TryReadAndAdvance(
  out AText: string): Boolean;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  StrVal: string;
begin
  Result := False;
  AText := '';
  if FDone then
    Exit;

  StrVal := TGocciaStringLiteralValue(FSource).Value;
  if FIndex >= Length(StrVal) then
  begin
    FDone := True;
    Exit;
  end;

  if not TextSemantics.TryReadUTF8CodePointAllowSurrogates(StrVal,
     FIndex + 1, CodePoint, ByteLength) then
    ByteLength := 1;
  AText := Copy(StrVal, FIndex + 1, ByteLength);
  Inc(FIndex, ByteLength);
  Result := True;
end;

function TGocciaStringIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  Text: string;
begin
  if TryReadAndAdvance(Text) then
    Result := CreateIteratorResult(TGocciaStringLiteralValue.Create(Text),
      False)
  else
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue,
      True);
  end;
end;

function TGocciaStringIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  Text: string;
begin
  if TryReadAndAdvance(Text) then
  begin
    ADone := False;
    Result := TGocciaStringLiteralValue.Create(Text);
  end
  else
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaStringIteratorValue.ToStringTag: string;
begin
  Result := 'String Iterator';
end;

procedure TGocciaStringIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;

{ TGocciaMapIteratorValue }

constructor TGocciaMapIteratorValue.Create(const ASource: TGocciaValue; const AKind: TGocciaMapIteratorKind);
var
  SharedPrototype: TGocciaObjectValue;
  SourceWasRooted: Boolean;
begin
  SourceWasRooted := AddTempRootIfNeeded(ASource);
  try
    inherited Create;
    SharedPrototype := EnsureConcreteIteratorPrototype(
      GMapIteratorPrototypeSlot, 'Map Iterator');
    if Assigned(SharedPrototype) then
      FPrototype := SharedPrototype;
    FSource := ASource;
    FIndex := 0;
    FKind := AKind;
  finally
    RemoveTempRootIfNeeded(ASource, SourceWasRooted);
  end;
end;

function TGocciaMapIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  MapVal: TGocciaMapValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  MapVal := TGocciaMapValue(FSource);
  if FIndex >= MapVal.Entries.Count then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  case FKind of
    mkEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(MapVal.Entries[FIndex].Key);
      EntryArray.Elements.Add(MapVal.Entries[FIndex].Value);
      Result := CreateIteratorResult(EntryArray, False);
    end;
    mkKeys:
      Result := CreateIteratorResult(MapVal.Entries[FIndex].Key, False);
    mkValues:
      Result := CreateIteratorResult(MapVal.Entries[FIndex].Value, False);
  end;
  Inc(FIndex);
end;

function TGocciaMapIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  MapVal: TGocciaMapValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  MapVal := TGocciaMapValue(FSource);
  if FIndex >= MapVal.Entries.Count then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ADone := False;
  case FKind of
    mkEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(MapVal.Entries[FIndex].Key);
      EntryArray.Elements.Add(MapVal.Entries[FIndex].Value);
      Result := EntryArray;
    end;
    mkKeys:
      Result := MapVal.Entries[FIndex].Key;
    mkValues:
      Result := MapVal.Entries[FIndex].Value;
  end;
  Inc(FIndex);
end;

function TGocciaMapIteratorValue.ToStringTag: string;
begin
  Result := 'Map Iterator';
end;

procedure TGocciaMapIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;

{ TGocciaSetIteratorValue }

constructor TGocciaSetIteratorValue.Create(const ASource: TGocciaValue; const AKind: TGocciaSetIteratorKind);
var
  SharedPrototype: TGocciaObjectValue;
  SourceWasRooted: Boolean;
begin
  SourceWasRooted := AddTempRootIfNeeded(ASource);
  try
    inherited Create;
    SharedPrototype := EnsureConcreteIteratorPrototype(
      GSetIteratorPrototypeSlot, 'Set Iterator');
    if Assigned(SharedPrototype) then
      FPrototype := SharedPrototype;
    FSource := ASource;
    FIndex := 0;
    FKind := AKind;
  finally
    RemoveTempRootIfNeeded(ASource, SourceWasRooted);
  end;
end;

function TGocciaSetIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  SetVal: TGocciaSetValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  SetVal := TGocciaSetValue(FSource);
  if FIndex >= SetVal.Items.Count then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  case FKind of
    skValues:
      Result := CreateIteratorResult(SetVal.Items[FIndex], False);
    skEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(SetVal.Items[FIndex]);
      EntryArray.Elements.Add(SetVal.Items[FIndex]);
      Result := CreateIteratorResult(EntryArray, False);
    end;
  end;
  Inc(FIndex);
end;

function TGocciaSetIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  SetVal: TGocciaSetValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  SetVal := TGocciaSetValue(FSource);
  if FIndex >= SetVal.Items.Count then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ADone := False;
  case FKind of
    skValues:
      Result := SetVal.Items[FIndex];
    skEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(SetVal.Items[FIndex]);
      EntryArray.Elements.Add(SetVal.Items[FIndex]);
      Result := EntryArray;
    end;
  end;
  Inc(FIndex);
end;

function TGocciaSetIteratorValue.ToStringTag: string;
begin
  Result := 'Set Iterator';
end;

procedure TGocciaSetIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;

{ TGocciaURLSearchParamsIteratorValue }

constructor TGocciaURLSearchParamsIteratorValue.Create(const ASource: TGocciaValue;
  const AKind: TGocciaURLSearchParamsIteratorKind);
var
  SourceWasRooted: Boolean;
begin
  SourceWasRooted := AddTempRootIfNeeded(ASource);
  try
    inherited Create;
    FSource := ASource;
    FIndex := 0;
    FKind := AKind;
  finally
    RemoveTempRootIfNeeded(ASource, SourceWasRooted);
  end;
end;

function TGocciaURLSearchParamsIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  Params: TGocciaURLSearchParamsValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  Params := TGocciaURLSearchParamsValue(FSource);
  if FIndex >= Params.List.Count then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  case FKind of
    spikKeys:
      Result := CreateIteratorResult(
        TGocciaStringLiteralValue.Create(Params.List[FIndex].Name), False);
    spikValues:
      Result := CreateIteratorResult(
        TGocciaStringLiteralValue.Create(Params.List[FIndex].Value), False);
    spikEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(Params.List[FIndex].Name));
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(Params.List[FIndex].Value));
      Result := CreateIteratorResult(EntryArray, False);
    end;
  end;
  Inc(FIndex);
end;

function TGocciaURLSearchParamsIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  Params: TGocciaURLSearchParamsValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Params := TGocciaURLSearchParamsValue(FSource);
  if FIndex >= Params.List.Count then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ADone := False;
  case FKind of
    spikKeys:
      Result := TGocciaStringLiteralValue.Create(Params.List[FIndex].Name);
    spikValues:
      Result := TGocciaStringLiteralValue.Create(Params.List[FIndex].Value);
    spikEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(Params.List[FIndex].Name));
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(Params.List[FIndex].Value));
      Result := EntryArray;
    end;
  end;
  Inc(FIndex);
end;

function TGocciaURLSearchParamsIteratorValue.ToStringTag: string;
begin
  Result := 'URLSearchParams Iterator';
end;

procedure TGocciaURLSearchParamsIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;

{ TGocciaHeadersIteratorValue }

constructor TGocciaHeadersIteratorValue.Create(const ASource: TGocciaValue;
  const AKind: TGocciaHeadersIteratorKind);
var
  SourceWasRooted: Boolean;
begin
  SourceWasRooted := AddTempRootIfNeeded(ASource);
  try
    inherited Create;
    FSource := ASource;
    FIndex := 0;
    FKind := AKind;
  finally
    RemoveTempRootIfNeeded(ASource, SourceWasRooted);
  end;
end;

function TGocciaHeadersIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  H: TGocciaHeadersValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  H := TGocciaHeadersValue(FSource);
  if FIndex >= H.Entries.Count then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  case FKind of
    hikEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(H.Entries[FIndex].Name));
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(H.Entries[FIndex].Value));
      Result := CreateIteratorResult(EntryArray, False);
    end;
    hikKeys:
      Result := CreateIteratorResult(
        TGocciaStringLiteralValue.Create(H.Entries[FIndex].Name), False);
    hikValues:
      Result := CreateIteratorResult(
        TGocciaStringLiteralValue.Create(H.Entries[FIndex].Value), False);
  end;
  Inc(FIndex);
end;

function TGocciaHeadersIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  H: TGocciaHeadersValue;
  EntryArray: TGocciaArrayValue;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  H := TGocciaHeadersValue(FSource);
  if FIndex >= H.Entries.Count then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ADone := False;
  case FKind of
    hikEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(H.Entries[FIndex].Name));
      EntryArray.Elements.Add(
        TGocciaStringLiteralValue.Create(H.Entries[FIndex].Value));
      Result := EntryArray;
    end;
    hikKeys:
      Result := TGocciaStringLiteralValue.Create(H.Entries[FIndex].Name);
    hikValues:
      Result := TGocciaStringLiteralValue.Create(H.Entries[FIndex].Value);
  end;
  Inc(FIndex);
end;

function TGocciaHeadersIteratorValue.ToStringTag: string;
begin
  Result := 'Headers Iterator';
end;

procedure TGocciaHeadersIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;

initialization
  GArrayIteratorPrototypeSlot := RegisterRealmSlot('ArrayIterator.prototype');
  GStringIteratorPrototypeSlot := RegisterRealmSlot('StringIterator.prototype');
  GMapIteratorPrototypeSlot := RegisterRealmSlot('MapIterator.prototype');
  GSetIteratorPrototypeSlot := RegisterRealmSlot('SetIterator.prototype');

end.
