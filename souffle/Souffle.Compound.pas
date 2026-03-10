unit Souffle.Compound;

{$I Souffle.inc}

interface

uses
  Souffle.Heap,
  Souffle.Value;

const
  SOUFFLE_PROP_WRITABLE     = $01;
  SOUFFLE_PROP_CONFIGURABLE = $02;
  SOUFFLE_PROP_ENUMERABLE   = $04;
  SOUFFLE_PROP_DEFAULT      = SOUFFLE_PROP_WRITABLE or SOUFFLE_PROP_CONFIGURABLE or SOUFFLE_PROP_ENUMERABLE;

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
    procedure Clear;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity;
  end;

  TSouffleRecordEntry = record
    Key: string;
    Value: TSouffleValue;
    Hash: UInt32;
    Occupied: Boolean;
    Deleted: Boolean;
    Flags: Byte;
  end;

  TSouffleBlueprint = class;

  { String-keyed ordered hash map with optional blueprint (type descriptor)
    and indexed slots. Without a blueprint: plain key-value record (covers
    JS objects, Python dicts, Lua hash part). With a blueprint: structured
    object with fast O(1) slot access plus dynamic named properties. }
  TSouffleRecord = class(TSouffleHeapObject)
  private
    FEntries: array of TSouffleRecordEntry;
    FOrder: array of Integer;
    FCount: Integer;
    FDeletedCount: Integer;
    FCapacity: Integer;
    FBlueprint: TSouffleBlueprint;
    FSlots: array of TSouffleValue;
    FExtensible: Boolean;
    FGetters: TSouffleRecord;
    FSetters: TSouffleRecord;
    function HashKey(const AKey: string): UInt32;
    function FindEntry(const AKey: string; const AHash: UInt32): Integer;
    function FindInsertSlot(const AKey: string; const AHash: UInt32): Integer;
    procedure Grow;
    function GetGetters: TSouffleRecord;
    function GetSetters: TSouffleRecord;
  public
    constructor Create(const AInitialCapacity: Integer = 0);
    constructor CreateFromBlueprint(const ABlueprint: TSouffleBlueprint);
    destructor Destroy; override;

    function Get(const AKey: string; out AValue: TSouffleValue): Boolean;
    procedure Put(const AKey: string; const AValue: TSouffleValue);
    procedure PutWithFlags(const AKey: string; const AValue: TSouffleValue;
      const AFlags: Byte);
    function PutChecked(const AKey: string;
      const AValue: TSouffleValue): Boolean;
    function Delete(const AKey: string): Boolean;
    function DeleteChecked(const AKey: string): Boolean;
    function Has(const AKey: string): Boolean;
    function GetEntryFlags(const AKey: string): Byte;
    function SetEntryFlags(const AKey: string; const AFlags: Byte): Boolean;
    procedure Freeze;

    function GetOrderedKey(const AIndex: Integer): string; inline;
    function GetOrderedValue(const AIndex: Integer): TSouffleValue; inline;

    function GetSlot(const AIndex: Integer): TSouffleValue; inline;
    procedure SetSlot(const AIndex: Integer; const AValue: TSouffleValue); inline;

    function HasGetters: Boolean; inline;
    function HasSetters: Boolean; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    procedure PreventExtensions; inline;

    property Count: Integer read FCount;
    property Blueprint: TSouffleBlueprint read FBlueprint;
    property Extensible: Boolean read FExtensible;
    property Getters: TSouffleRecord read GetGetters;
    property Setters: TSouffleRecord read GetSetters;
  end;

  { Blueprint — type descriptor with method table and optional super link.
    Language-agnostic equivalent of a class/struct definition. }
  TSouffleBlueprint = class(TSouffleHeapObject)
  private
    FName: string;
    FSlotCount: Integer;
    FMethods: TSouffleRecord;
    FSuperBlueprint: TSouffleBlueprint;
    FPrototype: TSouffleRecord;
    FGetters: TSouffleRecord;
    FSetters: TSouffleRecord;
    FStaticGetters: TSouffleRecord;
    FStaticSetters: TSouffleRecord;
    FStaticFields: TSouffleRecord;
    function GetPrototype: TSouffleRecord;
    function GetGetters: TSouffleRecord;
    function GetSetters: TSouffleRecord;
    function GetStaticGetters: TSouffleRecord;
    function GetStaticSetters: TSouffleRecord;
    function GetStaticFields: TSouffleRecord;
  public
    constructor Create(const AName: string; const ASlotCount: Integer);
    destructor Destroy; override;

    procedure MarkReferences; override;
    function DebugString: string; override;

    function HasGetters: Boolean; inline;
    function HasSetters: Boolean; inline;
    function HasStaticGetters: Boolean; inline;
    function HasStaticSetters: Boolean; inline;
    function HasStaticFields: Boolean; inline;

    property Name: string read FName;
    property SlotCount: Integer read FSlotCount write FSlotCount;
    property Methods: TSouffleRecord read FMethods;
    property SuperBlueprint: TSouffleBlueprint read FSuperBlueprint write FSuperBlueprint;
    property Prototype: TSouffleRecord read GetPrototype;
    property Getters: TSouffleRecord read GetGetters;
    property Setters: TSouffleRecord read GetSetters;
    property StaticGetters: TSouffleRecord read GetStaticGetters;
    property StaticSetters: TSouffleRecord read GetStaticSetters;
    property StaticFields: TSouffleRecord read GetStaticFields;
  end;

implementation

uses
  SysUtils;

const
  MIN_ARRAY_CAPACITY = 8;
  MIN_RECORD_CAPACITY = 8;
  RECORD_MAX_LOAD_FACTOR = 75; // percent

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

procedure TSouffleArray.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FElements[I] := SouffleNil;
  FCount := 0;
end;

procedure TSouffleArray.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCount - 1 do
    if SouffleIsReference(FElements[I]) and Assigned(FElements[I].AsReference)
      and not FElements[I].AsReference.GCMarked then
      FElements[I].AsReference.MarkReferences;
end;

function TSouffleArray.DebugString: string;
begin
  Result := '<array:' + IntToStr(FCount) + '>';
end;

{ TSouffleRecord }

constructor TSouffleRecord.Create(const AInitialCapacity: Integer);
var
  Cap: Integer;
begin
  inherited Create(SOUFFLE_HEAP_RECORD);
  FCount := 0;
  FDeletedCount := 0;
  FBlueprint := nil;
  FExtensible := True;
  FGetters := nil;
  FSetters := nil;
  if AInitialCapacity > MIN_RECORD_CAPACITY then
    Cap := AInitialCapacity
  else
    Cap := MIN_RECORD_CAPACITY;
  FCapacity := Cap;
  SetLength(FEntries, FCapacity);
  SetLength(FOrder, 0);
end;

constructor TSouffleRecord.CreateFromBlueprint(const ABlueprint: TSouffleBlueprint);
begin
  Create;
  FBlueprint := ABlueprint;
  SetLength(FSlots, ABlueprint.SlotCount);
end;

destructor TSouffleRecord.Destroy;
begin
  FGetters.Free;
  FSetters.Free;
  inherited;
end;

function TSouffleRecord.HashKey(const AKey: string): UInt32;
var
  I: Integer;
  H: UInt64;
begin
  H := 2166136261;
  for I := 1 to Length(AKey) do
    H := ((H xor UInt64(Ord(AKey[I]))) * UInt64(16777619)) and $FFFFFFFF;
  Result := UInt32(H);
end;

function TSouffleRecord.FindEntry(const AKey: string; const AHash: UInt32): Integer;
var
  Idx: Integer;
begin
  Idx := Integer(AHash mod UInt32(FCapacity));
  while True do
  begin
    if not FEntries[Idx].Occupied then
    begin
      if not FEntries[Idx].Deleted then
        Exit(Idx);
    end
    else if (FEntries[Idx].Hash = AHash) and (FEntries[Idx].Key = AKey) then
      Exit(Idx);
    Idx := (Idx + 1) mod FCapacity;
  end;
end;

function TSouffleRecord.FindInsertSlot(const AKey: string; const AHash: UInt32): Integer;
var
  Idx, FirstAvail: Integer;
begin
  Idx := Integer(AHash mod UInt32(FCapacity));
  FirstAvail := -1;
  while True do
  begin
    if not FEntries[Idx].Occupied then
    begin
      if FEntries[Idx].Deleted then
      begin
        if FirstAvail < 0 then
          FirstAvail := Idx;
      end
      else
      begin
        if FirstAvail >= 0 then
          Exit(FirstAvail)
        else
          Exit(Idx);
      end;
    end
    else if (FEntries[Idx].Hash = AHash) and (FEntries[Idx].Key = AKey) then
      Exit(Idx);
    Idx := (Idx + 1) mod FCapacity;
  end;
end;

procedure TSouffleRecord.Grow;
var
  OldEntries: array of TSouffleRecordEntry;
  OldOrder: array of Integer;
  OldCount, I, Slot, OldSlot: Integer;
  NewCapacity: Integer;
begin
  OldEntries := FEntries;
  OldCount := FCount;
  NewCapacity := FCapacity * 2;

  SetLength(OldOrder, OldCount);
  for I := 0 to OldCount - 1 do
    OldOrder[I] := FOrder[I];

  FCapacity := NewCapacity;
  FDeletedCount := 0;
  SetLength(FEntries, FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    FEntries[I].Occupied := False;
    FEntries[I].Deleted := False;
  end;

  SetLength(FOrder, OldCount);
  FCount := 0;

  for I := 0 to OldCount - 1 do
  begin
    OldSlot := OldOrder[I];
    Slot := FindEntry(OldEntries[OldSlot].Key, OldEntries[OldSlot].Hash);
    FEntries[Slot] := OldEntries[OldSlot];
    FEntries[Slot].Deleted := False;
    FOrder[FCount] := Slot;
    Inc(FCount);
  end;
end;

function TSouffleRecord.Get(const AKey: string; out AValue: TSouffleValue): Boolean;
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
  Slot := FindEntry(AKey, Hash);
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

procedure TSouffleRecord.Put(const AKey: string; const AValue: TSouffleValue);
var
  Hash: UInt32;
  Slot: Integer;
  WasTombstone: Boolean;
begin
  if (FCount + FDeletedCount + 1) * 100 > FCapacity * RECORD_MAX_LOAD_FACTOR then
    Grow;

  Hash := HashKey(AKey);
  Slot := FindInsertSlot(AKey, Hash);

  if not FEntries[Slot].Occupied then
  begin
    WasTombstone := FEntries[Slot].Deleted;
    FEntries[Slot].Key := AKey;
    FEntries[Slot].Hash := Hash;
    FEntries[Slot].Occupied := True;
    FEntries[Slot].Deleted := False;
    FEntries[Slot].Flags := SOUFFLE_PROP_DEFAULT;
    if WasTombstone then
      Dec(FDeletedCount);
    if FCount >= Length(FOrder) then
      SetLength(FOrder, FCount + 8);
    FOrder[FCount] := Slot;
    Inc(FCount);
  end;

  FEntries[Slot].Value := AValue;
end;

procedure TSouffleRecord.PutWithFlags(const AKey: string;
  const AValue: TSouffleValue; const AFlags: Byte);
var
  Hash: UInt32;
  Slot: Integer;
  WasTombstone: Boolean;
begin
  if (FCount + FDeletedCount + 1) * 100 > FCapacity * RECORD_MAX_LOAD_FACTOR then
    Grow;

  Hash := HashKey(AKey);
  Slot := FindInsertSlot(AKey, Hash);

  if not FEntries[Slot].Occupied then
  begin
    WasTombstone := FEntries[Slot].Deleted;
    FEntries[Slot].Key := AKey;
    FEntries[Slot].Hash := Hash;
    FEntries[Slot].Occupied := True;
    FEntries[Slot].Deleted := False;
    if WasTombstone then
      Dec(FDeletedCount);
    if FCount >= Length(FOrder) then
      SetLength(FOrder, FCount + 8);
    FOrder[FCount] := Slot;
    Inc(FCount);
  end;

  FEntries[Slot].Value := AValue;
  FEntries[Slot].Flags := AFlags;
end;

function TSouffleRecord.PutChecked(const AKey: string;
  const AValue: TSouffleValue): Boolean;
var
  Hash: UInt32;
  Slot: Integer;
  WasTombstone: Boolean;
begin
  if (FCount + FDeletedCount + 1) * 100 > FCapacity * RECORD_MAX_LOAD_FACTOR then
    Grow;

  Hash := HashKey(AKey);
  Slot := FindInsertSlot(AKey, Hash);

  if FEntries[Slot].Occupied then
  begin
    if FEntries[Slot].Flags and SOUFFLE_PROP_WRITABLE = 0 then
      Exit(False);
    FEntries[Slot].Value := AValue;
    Exit(True);
  end;

  if not FExtensible then
    Exit(False);

  WasTombstone := FEntries[Slot].Deleted;
  FEntries[Slot].Key := AKey;
  FEntries[Slot].Hash := Hash;
  FEntries[Slot].Occupied := True;
  FEntries[Slot].Deleted := False;
  FEntries[Slot].Flags := SOUFFLE_PROP_DEFAULT;
  if WasTombstone then
    Dec(FDeletedCount);
  if FCount >= Length(FOrder) then
    SetLength(FOrder, FCount + 8);
  FOrder[FCount] := Slot;
  Inc(FCount);
  FEntries[Slot].Value := AValue;
  Result := True;
end;

function TSouffleRecord.Delete(const AKey: string): Boolean;
var
  Hash: UInt32;
  Slot, I: Integer;
begin
  if FCount = 0 then
    Exit(False);

  Hash := HashKey(AKey);
  Slot := FindEntry(AKey, Hash);

  if not FEntries[Slot].Occupied then
    Exit(False);

  FEntries[Slot].Occupied := False;
  FEntries[Slot].Deleted := True;
  FEntries[Slot].Key := '';
  FEntries[Slot].Value := SouffleNil;
  Inc(FDeletedCount);

  for I := 0 to FCount - 1 do
  begin
    if FOrder[I] = Slot then
    begin
      if I < FCount - 1 then
        Move(FOrder[I + 1], FOrder[I], (FCount - I - 1) * SizeOf(Integer));
      Dec(FCount);
      Break;
    end;
  end;

  Result := True;
end;

function TSouffleRecord.DeleteChecked(const AKey: string): Boolean;
var
  Hash: UInt32;
  Slot, I: Integer;
begin
  if FCount = 0 then
    Exit(True);

  Hash := HashKey(AKey);
  Slot := FindEntry(AKey, Hash);

  if not FEntries[Slot].Occupied then
    Exit(True);

  if FEntries[Slot].Flags and SOUFFLE_PROP_CONFIGURABLE = 0 then
    Exit(False);

  FEntries[Slot].Occupied := False;
  FEntries[Slot].Deleted := True;
  FEntries[Slot].Key := '';
  FEntries[Slot].Value := SouffleNil;
  FEntries[Slot].Flags := SOUFFLE_PROP_DEFAULT;
  Inc(FDeletedCount);

  for I := 0 to FCount - 1 do
  begin
    if FOrder[I] = Slot then
    begin
      if I < FCount - 1 then
        Move(FOrder[I + 1], FOrder[I], (FCount - I - 1) * SizeOf(Integer));
      Dec(FCount);
      Break;
    end;
  end;

  Result := True;
end;

function TSouffleRecord.Has(const AKey: string): Boolean;
var
  Hash: UInt32;
  Slot: Integer;
begin
  if FCount = 0 then
    Exit(False);
  Hash := HashKey(AKey);
  Slot := FindEntry(AKey, Hash);
  Result := FEntries[Slot].Occupied;
end;

function TSouffleRecord.GetEntryFlags(const AKey: string): Byte;
var
  Hash: UInt32;
  Slot: Integer;
begin
  if FCount = 0 then
    Exit(SOUFFLE_PROP_DEFAULT);
  Hash := HashKey(AKey);
  Slot := FindEntry(AKey, Hash);
  if not FEntries[Slot].Occupied then
    Exit(SOUFFLE_PROP_DEFAULT);
  Result := FEntries[Slot].Flags;
end;

function TSouffleRecord.SetEntryFlags(const AKey: string;
  const AFlags: Byte): Boolean;
var
  Hash: UInt32;
  Slot: Integer;
begin
  if FCount = 0 then
    Exit(False);
  Hash := HashKey(AKey);
  Slot := FindEntry(AKey, Hash);
  if not FEntries[Slot].Occupied then
    Exit(False);
  FEntries[Slot].Flags := AFlags;
  Result := True;
end;

procedure TSouffleRecord.Freeze;
var
  I: Integer;
begin
  FExtensible := False;
  for I := 0 to FCapacity - 1 do
    if FEntries[I].Occupied then
      FEntries[I].Flags := FEntries[I].Flags and SOUFFLE_PROP_ENUMERABLE;
end;

procedure TSouffleRecord.PreventExtensions;
begin
  FExtensible := False;
end;

function TSouffleRecord.GetGetters: TSouffleRecord;
begin
  if not Assigned(FGetters) then
    FGetters := TSouffleRecord.Create;
  Result := FGetters;
end;

function TSouffleRecord.GetSetters: TSouffleRecord;
begin
  if not Assigned(FSetters) then
    FSetters := TSouffleRecord.Create;
  Result := FSetters;
end;

function TSouffleRecord.HasGetters: Boolean;
begin
  Result := Assigned(FGetters) and (FGetters.Count > 0);
end;

function TSouffleRecord.HasSetters: Boolean;
begin
  Result := Assigned(FSetters) and (FSetters.Count > 0);
end;

function TSouffleRecord.GetOrderedKey(const AIndex: Integer): string;
begin
  Result := FEntries[FOrder[AIndex]].Key;
end;

function TSouffleRecord.GetOrderedValue(const AIndex: Integer): TSouffleValue;
begin
  Result := FEntries[FOrder[AIndex]].Value;
end;

function TSouffleRecord.GetSlot(const AIndex: Integer): TSouffleValue;
begin
  if (AIndex >= 0) and (AIndex < Length(FSlots)) then
    Result := FSlots[AIndex]
  else
    Result := SouffleNil;
end;

procedure TSouffleRecord.SetSlot(const AIndex: Integer; const AValue: TSouffleValue);
begin
  if (AIndex >= 0) and (AIndex < Length(FSlots)) then
    FSlots[AIndex] := AValue;
end;

procedure TSouffleRecord.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCapacity - 1 do
    if FEntries[I].Occupied and
       SouffleIsReference(FEntries[I].Value) and
       Assigned(FEntries[I].Value.AsReference) and
       not FEntries[I].Value.AsReference.GCMarked then
      FEntries[I].Value.AsReference.MarkReferences;
  if Assigned(FBlueprint) and not FBlueprint.GCMarked then
    FBlueprint.MarkReferences;
  for I := 0 to High(FSlots) do
    if SouffleIsReference(FSlots[I]) and Assigned(FSlots[I].AsReference)
      and not FSlots[I].AsReference.GCMarked then
      FSlots[I].AsReference.MarkReferences;
  if Assigned(FGetters) and not FGetters.GCMarked then
    FGetters.MarkReferences;
  if Assigned(FSetters) and not FSetters.GCMarked then
    FSetters.MarkReferences;
end;

function TSouffleRecord.DebugString: string;
begin
  if Assigned(FBlueprint) then
    Result := '<' + FBlueprint.Name + '>'
  else
    Result := '<record:' + IntToStr(FCount) + '>';
end;

{ TSouffleBlueprint }

constructor TSouffleBlueprint.Create(const AName: string; const ASlotCount: Integer);
begin
  inherited Create(SOUFFLE_HEAP_BLUEPRINT);
  FName := AName;
  FSlotCount := ASlotCount;
  FMethods := TSouffleRecord.Create;
  FSuperBlueprint := nil;
  FPrototype := nil;
  FGetters := nil;
  FSetters := nil;
  FStaticGetters := nil;
  FStaticSetters := nil;
  FStaticFields := nil;
end;

destructor TSouffleBlueprint.Destroy;
begin
  FPrototype.Free;
  FMethods.Free;
  FGetters.Free;
  FSetters.Free;
  FStaticGetters.Free;
  FStaticSetters.Free;
  FStaticFields.Free;
  inherited;
end;

function TSouffleBlueprint.GetPrototype: TSouffleRecord;
begin
  if not Assigned(FPrototype) then
  begin
    if Assigned(FSuperBlueprint) then
      FPrototype := TSouffleRecord.CreateFromBlueprint(FSuperBlueprint)
    else
      FPrototype := TSouffleRecord.Create;
  end;
  Result := FPrototype;
end;

function TSouffleBlueprint.GetGetters: TSouffleRecord;
begin
  if not Assigned(FGetters) then
    FGetters := TSouffleRecord.Create;
  Result := FGetters;
end;

function TSouffleBlueprint.GetSetters: TSouffleRecord;
begin
  if not Assigned(FSetters) then
    FSetters := TSouffleRecord.Create;
  Result := FSetters;
end;

function TSouffleBlueprint.GetStaticGetters: TSouffleRecord;
begin
  if not Assigned(FStaticGetters) then
    FStaticGetters := TSouffleRecord.Create;
  Result := FStaticGetters;
end;

function TSouffleBlueprint.GetStaticSetters: TSouffleRecord;
begin
  if not Assigned(FStaticSetters) then
    FStaticSetters := TSouffleRecord.Create;
  Result := FStaticSetters;
end;

function TSouffleBlueprint.HasGetters: Boolean;
begin
  Result := Assigned(FGetters) and (FGetters.Count > 0);
end;

function TSouffleBlueprint.HasSetters: Boolean;
begin
  Result := Assigned(FSetters) and (FSetters.Count > 0);
end;

function TSouffleBlueprint.HasStaticGetters: Boolean;
begin
  Result := Assigned(FStaticGetters) and (FStaticGetters.Count > 0);
end;

function TSouffleBlueprint.HasStaticSetters: Boolean;
begin
  Result := Assigned(FStaticSetters) and (FStaticSetters.Count > 0);
end;

function TSouffleBlueprint.GetStaticFields: TSouffleRecord;
begin
  if not Assigned(FStaticFields) then
    FStaticFields := TSouffleRecord.Create;
  Result := FStaticFields;
end;

function TSouffleBlueprint.HasStaticFields: Boolean;
begin
  Result := Assigned(FStaticFields) and (FStaticFields.Count > 0);
end;

procedure TSouffleBlueprint.MarkReferences;
begin
  inherited;
  if Assigned(FMethods) and not FMethods.GCMarked then
    FMethods.MarkReferences;
  if Assigned(FSuperBlueprint) and not FSuperBlueprint.GCMarked then
    FSuperBlueprint.MarkReferences;
  if Assigned(FPrototype) and not FPrototype.GCMarked then
    FPrototype.MarkReferences;
  if Assigned(FGetters) and not FGetters.GCMarked then
    FGetters.MarkReferences;
  if Assigned(FSetters) and not FSetters.GCMarked then
    FSetters.MarkReferences;
  if Assigned(FStaticGetters) and not FStaticGetters.GCMarked then
    FStaticGetters.MarkReferences;
  if Assigned(FStaticSetters) and not FStaticSetters.GCMarked then
    FStaticSetters.MarkReferences;
  if Assigned(FStaticFields) and not FStaticFields.GCMarked then
    FStaticFields.MarkReferences;
end;

function TSouffleBlueprint.DebugString: string;
begin
  Result := '<blueprint:' + FName + '>';
end;

end.
