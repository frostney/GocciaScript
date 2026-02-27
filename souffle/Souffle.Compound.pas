unit Souffle.Compound;

{$I Souffle.inc}

interface

uses
  Souffle.Heap,
  Souffle.Value;

type
  { Dense dynamic array of TSouffleValue — universal across languages
    (JS arrays, Python lists, Lua array part, Wren lists, WASM GC arrays) }
  TSouffleArray = class(TSouffleHeapObject)
  private
    FElements: array of TSouffleValue;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow;
  public
    constructor Create(const AInitialCapacity: Integer = 0);

    procedure Push(const AValue: TSouffleValue);
    function Get(const AIndex: Integer): TSouffleValue; inline;
    procedure Put(const AIndex: Integer; const AValue: TSouffleValue); inline;
    function Pop: TSouffleValue;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity;
  end;

  TSouffleTableEntry = record
    Key: string;
    Value: TSouffleValue;
    Hash: UInt32;
    Occupied: Boolean;
  end;

  { String-keyed ordered hash map of TSouffleValue — covers JS objects,
    Python dicts (str keys), Lua hash part, Wren ObjMap (string keys) }
  TSouffleTable = class(TSouffleHeapObject)
  private
    FEntries: array of TSouffleTableEntry;
    FOrder: array of Integer;
    FCount: Integer;
    FCapacity: Integer;
    function HashKey(const AKey: string): UInt32;
    function FindSlot(const AKey: string; const AHash: UInt32): Integer;
    procedure Grow;
  public
    constructor Create(const AInitialCapacity: Integer = 0);

    function Get(const AKey: string; out AValue: TSouffleValue): Boolean;
    procedure Put(const AKey: string; const AValue: TSouffleValue);
    function Delete(const AKey: string): Boolean;
    function Has(const AKey: string): Boolean;

    function GetOrderedKey(const AIndex: Integer): string; inline;
    function GetOrderedValue(const AIndex: Integer): TSouffleValue; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
  end;

implementation

uses
  SysUtils;

const
  MIN_ARRAY_CAPACITY = 8;
  MIN_TABLE_CAPACITY = 8;
  TABLE_MAX_LOAD_FACTOR = 75; // percent

{ TSouffleArray }

constructor TSouffleArray.Create(const AInitialCapacity: Integer);
begin
  inherited Create(SOUFFLE_HEAP_ARRAY);
  FCount := 0;
  if AInitialCapacity > 0 then
    FCapacity := AInitialCapacity
  else
    FCapacity := 0;
  SetLength(FElements, FCapacity);
end;

procedure TSouffleArray.Grow;
var
  NewCapacity: Integer;
begin
  if FCapacity < MIN_ARRAY_CAPACITY then
    NewCapacity := MIN_ARRAY_CAPACITY
  else
    NewCapacity := FCapacity * 2;
  FCapacity := NewCapacity;
  SetLength(FElements, FCapacity);
end;

procedure TSouffleArray.Push(const AValue: TSouffleValue);
begin
  if FCount >= FCapacity then
    Grow;
  FElements[FCount] := AValue;
  Inc(FCount);
end;

function TSouffleArray.Get(const AIndex: Integer): TSouffleValue;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    Result := SouffleNil
  else
    Result := FElements[AIndex];
end;

procedure TSouffleArray.Put(const AIndex: Integer; const AValue: TSouffleValue);
var
  I: Integer;
begin
  if AIndex < 0 then
    Exit;
  if AIndex >= FCapacity then
  begin
    while FCapacity <= AIndex do
    begin
      if FCapacity < MIN_ARRAY_CAPACITY then
        FCapacity := MIN_ARRAY_CAPACITY
      else
        FCapacity := FCapacity * 2;
    end;
    SetLength(FElements, FCapacity);
  end;
  if AIndex >= FCount then
  begin
    for I := FCount to AIndex - 1 do
      FElements[I] := SouffleNil;
    FCount := AIndex + 1;
  end;
  FElements[AIndex] := AValue;
end;

function TSouffleArray.Pop: TSouffleValue;
begin
  if FCount = 0 then
    Exit(SouffleNil);
  Dec(FCount);
  Result := FElements[FCount];
  FElements[FCount] := SouffleNil;
end;

procedure TSouffleArray.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCount - 1 do
    if SouffleIsReference(FElements[I]) and Assigned(FElements[I].AsReference) then
      FElements[I].AsReference.MarkReferences;
end;

function TSouffleArray.DebugString: string;
begin
  Result := '<array:' + IntToStr(FCount) + '>';
end;

{ TSouffleTable }

constructor TSouffleTable.Create(const AInitialCapacity: Integer);
var
  Cap: Integer;
begin
  inherited Create(SOUFFLE_HEAP_TABLE);
  FCount := 0;
  if AInitialCapacity > MIN_TABLE_CAPACITY then
    Cap := AInitialCapacity
  else
    Cap := MIN_TABLE_CAPACITY;
  FCapacity := Cap;
  SetLength(FEntries, FCapacity);
  SetLength(FOrder, 0);
end;

function TSouffleTable.HashKey(const AKey: string): UInt32;
var
  I: Integer;
  H: UInt64;
begin
  H := 2166136261;
  for I := 1 to Length(AKey) do
    H := ((H xor UInt64(Ord(AKey[I]))) * UInt64(16777619)) and $FFFFFFFF;
  Result := UInt32(H);
end;

function TSouffleTable.FindSlot(const AKey: string; const AHash: UInt32): Integer;
var
  Idx: Integer;
begin
  Idx := Integer(AHash mod UInt32(FCapacity));
  while True do
  begin
    if not FEntries[Idx].Occupied then
      Exit(Idx);
    if (FEntries[Idx].Hash = AHash) and (FEntries[Idx].Key = AKey) then
      Exit(Idx);
    Idx := (Idx + 1) mod FCapacity;
  end;
end;

procedure TSouffleTable.Grow;
var
  OldEntries: array of TSouffleTableEntry;
  OldCount, OldCapacity, I, Slot: Integer;
  NewCapacity: Integer;
begin
  OldEntries := FEntries;
  OldCapacity := FCapacity;
  OldCount := FCount;
  NewCapacity := FCapacity * 2;

  FCapacity := NewCapacity;
  SetLength(FEntries, FCapacity);
  for I := 0 to FCapacity - 1 do
    FEntries[I].Occupied := False;

  SetLength(FOrder, OldCount);
  FCount := 0;

  for I := 0 to OldCapacity - 1 do
  begin
    if OldEntries[I].Occupied then
    begin
      Slot := FindSlot(OldEntries[I].Key, OldEntries[I].Hash);
      FEntries[Slot] := OldEntries[I];
      FOrder[FCount] := Slot;
      Inc(FCount);
    end;
  end;
end;

function TSouffleTable.Get(const AKey: string; out AValue: TSouffleValue): Boolean;
var
  Hash: UInt32;
  Slot: Integer;
begin
  if FCount = 0 then
  begin
    AValue := SouffleNil;
    Exit(False);
  end;
  Hash := HashKey(AKey);
  Slot := FindSlot(AKey, Hash);
  if FEntries[Slot].Occupied then
  begin
    AValue := FEntries[Slot].Value;
    Result := True;
  end
  else
  begin
    AValue := SouffleNil;
    Result := False;
  end;
end;

procedure TSouffleTable.Put(const AKey: string; const AValue: TSouffleValue);
var
  Hash: UInt32;
  Slot: Integer;
begin
  if (FCount + 1) * 100 > FCapacity * TABLE_MAX_LOAD_FACTOR then
    Grow;

  Hash := HashKey(AKey);
  Slot := FindSlot(AKey, Hash);

  if not FEntries[Slot].Occupied then
  begin
    FEntries[Slot].Key := AKey;
    FEntries[Slot].Hash := Hash;
    FEntries[Slot].Occupied := True;
    if FCount >= Length(FOrder) then
      SetLength(FOrder, FCount + 8);
    FOrder[FCount] := Slot;
    Inc(FCount);
  end;

  FEntries[Slot].Value := AValue;
end;

function TSouffleTable.Delete(const AKey: string): Boolean;
var
  Hash: UInt32;
  Slot, I: Integer;
begin
  if FCount = 0 then
    Exit(False);

  Hash := HashKey(AKey);
  Slot := FindSlot(AKey, Hash);

  if not FEntries[Slot].Occupied then
    Exit(False);

  // Tombstone: mark empty but keep probe chain intact
  FEntries[Slot].Occupied := False;
  FEntries[Slot].Key := '';
  FEntries[Slot].Value := SouffleNil;

  // Remove from insertion order
  for I := 0 to FCount - 1 do
  begin
    if FOrder[I] = Slot then
    begin
      Move(FOrder[I + 1], FOrder[I], (FCount - I - 1) * SizeOf(Integer));
      Dec(FCount);
      Break;
    end;
  end;

  Result := True;
end;

function TSouffleTable.Has(const AKey: string): Boolean;
var
  Hash: UInt32;
  Slot: Integer;
begin
  if FCount = 0 then
    Exit(False);
  Hash := HashKey(AKey);
  Slot := FindSlot(AKey, Hash);
  Result := FEntries[Slot].Occupied;
end;

function TSouffleTable.GetOrderedKey(const AIndex: Integer): string;
begin
  Result := FEntries[FOrder[AIndex]].Key;
end;

function TSouffleTable.GetOrderedValue(const AIndex: Integer): TSouffleValue;
begin
  Result := FEntries[FOrder[AIndex]].Value;
end;

procedure TSouffleTable.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCapacity - 1 do
    if FEntries[I].Occupied and
       SouffleIsReference(FEntries[I].Value) and
       Assigned(FEntries[I].Value.AsReference) then
      FEntries[I].Value.AsReference.MarkReferences;
end;

function TSouffleTable.DebugString: string;
begin
  Result := '<table:' + IntToStr(FCount) + '>';
end;

end.
