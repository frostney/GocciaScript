unit Goccia.Runtime.Collections;

{$I Goccia.inc}

interface

uses
  Math,
  SysUtils,

  Souffle.Heap,
  Souffle.Value;

type
  TSouffleMapEntry = record
    Key: TSouffleValue;
    Value: TSouffleValue;
  end;

  TGocciaSouffleMap = class(TSouffleHeapObject)
  private
    FEntries: array of TSouffleMapEntry;
    FCount: Integer;
  public
    constructor Create(const ACapacity: Integer = 4);

    function FindEntry(const AKey: TSouffleValue): Integer;
    procedure SetEntry(const AKey, AValue: TSouffleValue);
    function GetEntry(const AKey: TSouffleValue): TSouffleValue;
    function HasKey(const AKey: TSouffleValue): Boolean;
    function DeleteEntry(const AKey: TSouffleValue): Boolean;
    procedure Clear;

    function GetKeyAt(const AIndex: Integer): TSouffleValue; inline;
    function GetValueAt(const AIndex: Integer): TSouffleValue; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
  end;

  TGocciaSouffleSet = class(TSouffleHeapObject)
  private
    FItems: array of TSouffleValue;
    FCount: Integer;
  public
    constructor Create(const ACapacity: Integer = 4);

    function Contains(const AValue: TSouffleValue): Boolean;
    function Add(const AValue: TSouffleValue): Boolean;
    function Delete(const AValue: TSouffleValue): Boolean;
    procedure Clear;

    function GetItemAt(const AIndex: Integer): TSouffleValue; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
  end;

  TSouffleMapIterKind = (mikKeys, mikValues, mikEntries);

  TGocciaSouffleMapIterator = class(TSouffleHeapObject)
  private
    FMap: TGocciaSouffleMap;
    FIndex: Integer;
    FKind: TSouffleMapIterKind;
  public
    constructor Create(const AMap: TGocciaSouffleMap; const AKind: TSouffleMapIterKind);
    function Next(out ADone: Boolean): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

  TSouffleSetIterKind = (sikValues, sikEntries);

  TGocciaSouffleSetIterator = class(TSouffleHeapObject)
  private
    FSet: TGocciaSouffleSet;
    FIndex: Integer;
    FKind: TSouffleSetIterKind;
  public
    constructor Create(const ASet: TGocciaSouffleSet; const AKind: TSouffleSetIterKind);
    function Next(out ADone: Boolean): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

function SouffleSameValueZero(const A, B: TSouffleValue): Boolean;

implementation

uses
  GarbageCollector.Generic,
  Souffle.Compound;

function SouffleSameValueZero(const A, B: TSouffleValue): Boolean;
var
  FA, FB: Double;
begin
  if A.Kind <> B.Kind then
  begin
    { Cross-compare integer 0 and float -0.0/+0.0 }
    if (A.Kind = svkInteger) and (B.Kind = svkFloat) then
      Exit((A.AsInteger = 0) and (B.AsFloat = 0.0));
    if (A.Kind = svkFloat) and (B.Kind = svkInteger) then
      Exit((A.AsFloat = 0.0) and (B.AsInteger = 0));
    Exit(False);
  end;

  case A.Kind of
    svkNil:
      Result := A.Flags = B.Flags;
    svkBoolean:
      Result := A.AsBoolean = B.AsBoolean;
    svkInteger:
      Result := A.AsInteger = B.AsInteger;
    svkFloat:
    begin
      FA := A.AsFloat;
      FB := B.AsFloat;
      if IsNan(FA) and IsNan(FB) then
        Result := True
      else
        Result := FA = FB;
    end;
    svkString:
      Result := SouffleGetString(A) = SouffleGetString(B);
    svkReference:
      Result := A.AsReference = B.AsReference;
  else
    Result := False;
  end;
end;

{ TGocciaSouffleMap }

constructor TGocciaSouffleMap.Create(const ACapacity: Integer);
begin
  inherited Create(0);
  SetLength(FEntries, ACapacity);
  FCount := 0;
end;

function TGocciaSouffleMap.FindEntry(const AKey: TSouffleValue): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if SouffleSameValueZero(FEntries[I].Key, AKey) then
      Exit(I);
  Result := -1;
end;

procedure TGocciaSouffleMap.SetEntry(const AKey, AValue: TSouffleValue);
var
  Idx: Integer;
begin
  Idx := FindEntry(AKey);
  if Idx >= 0 then
    FEntries[Idx].Value := AValue
  else
  begin
    if FCount >= Length(FEntries) then
      SetLength(FEntries, FCount * 2 + 4);
    FEntries[FCount].Key := AKey;
    FEntries[FCount].Value := AValue;
    Inc(FCount);
  end;
end;

function TGocciaSouffleMap.GetEntry(const AKey: TSouffleValue): TSouffleValue;
var
  Idx: Integer;
begin
  Idx := FindEntry(AKey);
  if Idx >= 0 then
    Result := FEntries[Idx].Value
  else
    Result := SouffleNilWithFlags(0);
end;

function TGocciaSouffleMap.HasKey(const AKey: TSouffleValue): Boolean;
begin
  Result := FindEntry(AKey) >= 0;
end;

function TGocciaSouffleMap.DeleteEntry(const AKey: TSouffleValue): Boolean;
var
  Idx, I: Integer;
begin
  Idx := FindEntry(AKey);
  if Idx < 0 then
    Exit(False);
  for I := Idx to FCount - 2 do
    FEntries[I] := FEntries[I + 1];
  Dec(FCount);
  FEntries[FCount].Key := SouffleNil;
  FEntries[FCount].Value := SouffleNil;
  Result := True;
end;

procedure TGocciaSouffleMap.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    FEntries[I].Key := SouffleNil;
    FEntries[I].Value := SouffleNil;
  end;
  FCount := 0;
end;

function TGocciaSouffleMap.GetKeyAt(const AIndex: Integer): TSouffleValue;
begin
  Result := FEntries[AIndex].Key;
end;

function TGocciaSouffleMap.GetValueAt(const AIndex: Integer): TSouffleValue;
begin
  Result := FEntries[AIndex].Value;
end;

procedure TGocciaSouffleMap.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCount - 1 do
  begin
    if SouffleIsReference(FEntries[I].Key) and Assigned(FEntries[I].Key.AsReference)
       and not FEntries[I].Key.AsReference.GCMarked then
      FEntries[I].Key.AsReference.MarkReferences;
    if SouffleIsReference(FEntries[I].Value) and Assigned(FEntries[I].Value.AsReference)
       and not FEntries[I].Value.AsReference.GCMarked then
      FEntries[I].Value.AsReference.MarkReferences;
  end;
end;

function TGocciaSouffleMap.DebugString: string;
begin
  Result := Format('SouffleMap(%d)', [FCount]);
end;

{ TGocciaSouffleSet }

constructor TGocciaSouffleSet.Create(const ACapacity: Integer);
begin
  inherited Create(0);
  SetLength(FItems, ACapacity);
  FCount := 0;
end;

function TGocciaSouffleSet.Contains(const AValue: TSouffleValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if SouffleSameValueZero(FItems[I], AValue) then
      Exit(True);
  Result := False;
end;

function TGocciaSouffleSet.Add(const AValue: TSouffleValue): Boolean;
begin
  if Contains(AValue) then
    Exit(False);
  if FCount >= Length(FItems) then
    SetLength(FItems, FCount * 2 + 4);
  FItems[FCount] := AValue;
  Inc(FCount);
  Result := True;
end;

function TGocciaSouffleSet.Delete(const AValue: TSouffleValue): Boolean;
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I := 0 to FCount - 1 do
    if SouffleSameValueZero(FItems[I], AValue) then
    begin
      Idx := I;
      Break;
    end;
  if Idx < 0 then
    Exit(False);
  for I := Idx to FCount - 2 do
    FItems[I] := FItems[I + 1];
  Dec(FCount);
  FItems[FCount] := SouffleNil;
  Result := True;
end;

procedure TGocciaSouffleSet.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FItems[I] := SouffleNil;
  FCount := 0;
end;

function TGocciaSouffleSet.GetItemAt(const AIndex: Integer): TSouffleValue;
begin
  Result := FItems[AIndex];
end;

procedure TGocciaSouffleSet.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCount - 1 do
    if SouffleIsReference(FItems[I]) and Assigned(FItems[I].AsReference)
       and not FItems[I].AsReference.GCMarked then
      FItems[I].AsReference.MarkReferences;
end;

function TGocciaSouffleSet.DebugString: string;
begin
  Result := Format('SouffleSet(%d)', [FCount]);
end;

{ TGocciaSouffleMapIterator }

constructor TGocciaSouffleMapIterator.Create(const AMap: TGocciaSouffleMap;
  const AKind: TSouffleMapIterKind);
begin
  inherited Create(0);
  FMap := AMap;
  FIndex := 0;
  FKind := AKind;
end;

function TGocciaSouffleMapIterator.Next(out ADone: Boolean): TSouffleValue;
var
  Arr: TSouffleArray;
begin
  if FIndex >= FMap.Count then
  begin
    ADone := True;
    Exit(SouffleNilWithFlags(0));
  end;
  ADone := False;
  case FKind of
    mikKeys:
      Result := FMap.GetKeyAt(FIndex);
    mikValues:
      Result := FMap.GetValueAt(FIndex);
    mikEntries:
    begin
      Arr := TSouffleArray.Create(2);
      Arr.Push(FMap.GetKeyAt(FIndex));
      Arr.Push(FMap.GetValueAt(FIndex));
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Arr);
      Result := SouffleReference(Arr);
    end;
  end;
  Inc(FIndex);
end;

procedure TGocciaSouffleMapIterator.MarkReferences;
begin
  inherited;
  if Assigned(FMap) and not FMap.GCMarked then
    FMap.MarkReferences;
end;

function TGocciaSouffleMapIterator.DebugString: string;
begin
  Result := 'MapIterator';
end;

{ TGocciaSouffleSetIterator }

constructor TGocciaSouffleSetIterator.Create(const ASet: TGocciaSouffleSet;
  const AKind: TSouffleSetIterKind);
begin
  inherited Create(0);
  FSet := ASet;
  FIndex := 0;
  FKind := AKind;
end;

function TGocciaSouffleSetIterator.Next(out ADone: Boolean): TSouffleValue;
var
  Arr: TSouffleArray;
begin
  if FIndex >= FSet.Count then
  begin
    ADone := True;
    Exit(SouffleNilWithFlags(0));
  end;
  ADone := False;
  case FKind of
    sikValues:
      Result := FSet.GetItemAt(FIndex);
    sikEntries:
    begin
      Arr := TSouffleArray.Create(2);
      Arr.Push(FSet.GetItemAt(FIndex));
      Arr.Push(FSet.GetItemAt(FIndex));
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Arr);
      Result := SouffleReference(Arr);
    end;
  end;
  Inc(FIndex);
end;

procedure TGocciaSouffleSetIterator.MarkReferences;
begin
  inherited;
  if Assigned(FSet) and not FSet.GCMarked then
    FSet.MarkReferences;
end;

function TGocciaSouffleSetIterator.DebugString: string;
begin
  Result := 'SetIterator';
end;

end.
