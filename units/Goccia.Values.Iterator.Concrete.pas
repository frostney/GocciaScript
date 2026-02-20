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
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

  TGocciaStringIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
    FIndex: Integer;
  public
    constructor Create(const ASource: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
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
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.Values.ArrayValue,
  Goccia.Values.MapValue,
  Goccia.Values.SetValue;

{ TGocciaArrayIteratorValue }

constructor TGocciaArrayIteratorValue.Create(const ASource: TGocciaValue; const AKind: TGocciaArrayIteratorKind);
begin
  inherited Create;
  FSource := ASource;
  FIndex := 0;
  FKind := AKind;
end;

function TGocciaArrayIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  Arr: TGocciaArrayValue;
  EntryArray: TGocciaArrayValue;
  Element: TGocciaValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  Arr := TGocciaArrayValue(FSource);
  if FIndex >= Arr.Elements.Count then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  case FKind of
    akValues:
    begin
      Element := Arr.Elements[FIndex];
      if Element = nil then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;
      Result := CreateIteratorResult(Element, False);
    end;
    akKeys:
      Result := CreateIteratorResult(TGocciaNumberLiteralValue.SmallInt(FIndex), False);
    akEntries:
    begin
      EntryArray := TGocciaArrayValue.Create;
      EntryArray.Elements.Add(TGocciaNumberLiteralValue.SmallInt(FIndex));
      Element := Arr.Elements[FIndex];
      if Element = nil then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;
      EntryArray.Elements.Add(Element);
      Result := CreateIteratorResult(EntryArray, False);
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
begin
  inherited Create;
  FSource := ASource;
  FIndex := 0;
end;

function TGocciaStringIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  StrVal: string;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  StrVal := TGocciaStringLiteralValue(FSource).Value;
  if FIndex >= Length(StrVal) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;
  Result := CreateIteratorResult(TGocciaStringLiteralValue.Create(StrVal[FIndex + 1]), False);
  Inc(FIndex);
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
begin
  inherited Create;
  FSource := ASource;
  FIndex := 0;
  FKind := AKind;
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
begin
  inherited Create;
  FSource := ASource;
  FIndex := 0;
  FKind := AKind;
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

end.
