{
  TOrderedMap<TKey, TValue> - Insertion-order-preserving generic map.

  Inherits TBaseMap<TKey, TValue> for shared types and iteration.

  Hash and equality are protected virtual methods so subclasses can
  override them for specific key types (e.g. TOrderedStringMap for
  proper string hashing). Default implementation: DJB2 over raw key
  bytes + byte-level equality — correct for all fixed-size value types.

  Performance note on virtual hash/equality:
  The VMT hop per probe step is negligible in practice. For string
  keys the comparison itself dominates. For small value types the
  byte loop is branch-predictor friendly. The benefit is one ordered
  map implementation instead of two.
}

unit OrderedMap;

{$I Shared.inc}

interface

uses
  SysUtils,

  BaseMap;

type
  TOrderedMap<TKey, TValue> = class(TBaseMap<TKey, TValue>)
  public type
    TEntry = record
      Key: TKey;
      Value: TValue;
      Hash: Cardinal;
      Active: Boolean;
    end;

    TEntryArray = array of TEntry;

    TEnumerator = record
    private
      FEntries: TEntryArray;
      FEntryCount: Integer;
      FIndex: Integer;
      FCurrent: TBaseMap<TKey, TValue>.TKeyValuePair;
      function GetCurrent: TBaseMap<TKey, TValue>.TKeyValuePair; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: TBaseMap<TKey, TValue>.TKeyValuePair read GetCurrent;
    end;

  private const
    EMPTY_SLOT          = -1;
    DELETED_SLOT        = -2;
    INITIAL_CAPACITY    = 16;
    LOAD_FACTOR_PERCENT = 70;

  private
    FEntries: TEntryArray;
    FBuckets: array of Int32;
    FCount: Integer;
    FEntryCount: Integer;
    FBucketCount: Integer;

    function FindBucket(const AKey: TKey; AHash: Cardinal;
      out ABucketIdx: Integer): Boolean;
    procedure Grow;
    procedure Rehash(ANewBucketCount: Integer);
    procedure Compact;

  protected
    function HashKey(const AKey: TKey): Cardinal; virtual;
    function KeysEqual(const A, B: TKey): Boolean; virtual;

    function GetCount: Integer; override;
    function GetValue(const AKey: TKey): TValue; override;
    procedure SetValue(const AKey: TKey; const AValue: TValue); override;
    function GetNextEntry(var AIterState: Integer;
      out AKey: TKey; out AValue: TValue): Boolean; override;

  public
    constructor Create; overload;
    constructor Create(AInitialCapacity: Integer); overload;
    destructor Destroy; override;

    procedure Add(const AKey: TKey; const AValue: TValue); override;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean; override;
    function ContainsKey(const AKey: TKey): Boolean; override;
    function Remove(const AKey: TKey): Boolean; override;
    procedure Clear; override;

    function GetEnumerator: TEnumerator; inline;
    function EntryAt(AIndex: Integer): TBaseMap<TKey, TValue>.TKeyValuePair;

    property Capacity: Integer read FBucketCount;
  end;

implementation

{ Hash / Equality — default: DJB2 over raw key bytes }

{$PUSH}{$R-}{$Q-}
function TOrderedMap<TKey, TValue>.HashKey(const AKey: TKey): Cardinal;
var
  P: PByte;
  I: Integer;
  V4: Cardinal;
  V8: QWord;
begin
  if SizeOf(TKey) = SizeOf(QWord) then
  begin
    Move(AKey, V8, 8);
    V8 := (V8 xor (V8 shr 4)) * QWord(11400714819323198485);
    Result := Cardinal(V8 xor (V8 shr 32));
  end
  else if SizeOf(TKey) = SizeOf(Cardinal) then
  begin
    Move(AKey, V4, 4);
    V4 := V4 * Cardinal(2654435761);
    Result := V4;
  end
  else
  begin
    Result := 5381;
    P := @AKey;
    for I := 0 to SizeOf(TKey) - 1 do
      Result := Result * 33 + P[I];
  end;
end;
{$POP}

function TOrderedMap<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if SizeOf(TKey) = SizeOf(QWord) then
    Result := PQWord(@A)^ = PQWord(@B)^
  else if SizeOf(TKey) = SizeOf(Cardinal) then
    Result := PCardinal(@A)^ = PCardinal(@B)^
  else
    Result := CompareMem(@A, @B, SizeOf(TKey));
end;

{ Probe }

function TOrderedMap<TKey, TValue>.FindBucket(const AKey: TKey; AHash: Cardinal;
  out ABucketIdx: Integer): Boolean;
var
  Idx, EntryIdx, FirstDeleted: Integer;
begin
  Result := False;
  FirstDeleted := -1;
  Idx := AHash and Cardinal(FBucketCount - 1);

  while True do
  begin
    EntryIdx := FBuckets[Idx];

    if EntryIdx = EMPTY_SLOT then
    begin
      if FirstDeleted >= 0 then
        ABucketIdx := FirstDeleted
      else
        ABucketIdx := Idx;
      Exit;
    end;

    if EntryIdx = DELETED_SLOT then
    begin
      if FirstDeleted < 0 then
        FirstDeleted := Idx;
    end
    else if (FEntries[EntryIdx].Hash = AHash) and
            FEntries[EntryIdx].Active and
            KeysEqual(FEntries[EntryIdx].Key, AKey) then
    begin
      ABucketIdx := Idx;
      Result := True;
      Exit;
    end;

    Idx := (Idx + 1) and (FBucketCount - 1);
  end;
end;

{ Resize }

procedure TOrderedMap<TKey, TValue>.Grow;
var
  N: Integer;
begin
  N := FBucketCount * 2;
  if N < INITIAL_CAPACITY then
    N := INITIAL_CAPACITY;
  Rehash(N);
end;

procedure TOrderedMap<TKey, TValue>.Rehash(ANewBucketCount: Integer);
var
  I, Idx: Integer;
begin
  FBucketCount := ANewBucketCount;
  SetLength(FBuckets, FBucketCount);
  for I := 0 to FBucketCount - 1 do
    FBuckets[I] := EMPTY_SLOT;

  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
    begin
      Idx := FEntries[I].Hash and Cardinal(FBucketCount - 1);
      while FBuckets[Idx] >= 0 do
        Idx := (Idx + 1) and (FBucketCount - 1);
      FBuckets[Idx] := I;
    end;
end;

procedure TOrderedMap<TKey, TValue>.Compact;
var
  NewEntries: TEntryArray;
  I, J: Integer;
begin
  SetLength(NewEntries, FCount);
  J := 0;
  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
    begin
      NewEntries[J] := FEntries[I];
      Inc(J);
    end;
  FEntries := NewEntries;
  FEntryCount := FCount;
  Rehash(FBucketCount);
end;

{ Constructor / Destructor }

constructor TOrderedMap<TKey, TValue>.Create;
begin
  Create(INITIAL_CAPACITY);
end;

constructor TOrderedMap<TKey, TValue>.Create(AInitialCapacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCount := 0;
  FEntryCount := 0;

  if AInitialCapacity < INITIAL_CAPACITY then
    AInitialCapacity := INITIAL_CAPACITY;

  FBucketCount := INITIAL_CAPACITY;
  while FBucketCount < AInitialCapacity do
    FBucketCount := FBucketCount * 2;

  SetLength(FBuckets, FBucketCount);
  for I := 0 to FBucketCount - 1 do
    FBuckets[I] := EMPTY_SLOT;
end;

destructor TOrderedMap<TKey, TValue>.Destroy;
begin
  FEntries := nil;
  FBuckets := nil;
  inherited;
end;

{ Core operations }

procedure TOrderedMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  Hash: Cardinal;
  BucketIdx, EntryIdx: Integer;
begin
  Hash := HashKey(AKey);

  if FindBucket(AKey, Hash, BucketIdx) then
  begin
    FEntries[FBuckets[BucketIdx]].Value := AValue;
    Exit;
  end;

  if (FEntryCount + 1) * 100 > FBucketCount * LOAD_FACTOR_PERCENT then
  begin
    if FCount < FEntryCount div 2 then
      Compact
    else
      Grow;
    FindBucket(AKey, Hash, BucketIdx);
  end;

  EntryIdx := FEntryCount;
  Inc(FEntryCount);
  if FEntryCount > Length(FEntries) then
    SetLength(FEntries, FEntryCount * 2);

  FEntries[EntryIdx].Key := AKey;
  FEntries[EntryIdx].Value := AValue;
  FEntries[EntryIdx].Hash := Hash;
  FEntries[EntryIdx].Active := True;

  FBuckets[BucketIdx] := EntryIdx;
  Inc(FCount);
end;

function TOrderedMap<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
  if Result then
    AValue := FEntries[FBuckets[BucketIdx]].Value
  else
    AValue := Default(TValue);
end;

function TOrderedMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
end;

function TOrderedMap<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  Hash: Cardinal;
  BucketIdx, EntryIdx: Integer;
begin
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
  if not Result then
    Exit;

  EntryIdx := FBuckets[BucketIdx];
  FEntries[EntryIdx].Active := False;
  FEntries[EntryIdx].Key := Default(TKey);
  FEntries[EntryIdx].Value := Default(TValue);
  FBuckets[BucketIdx] := DELETED_SLOT;
  Dec(FCount);
end;

procedure TOrderedMap<TKey, TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to FBucketCount - 1 do
    FBuckets[I] := EMPTY_SLOT;
  SetLength(FEntries, 0);
  FCount := 0;
  FEntryCount := 0;
end;

{ Accessors }

function TOrderedMap<TKey, TValue>.GetCount: Integer;
begin
  Result := FCount;
end;

function TOrderedMap<TKey, TValue>.GetValue(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise Exception.Create('Key not found in ordered map');
end;

procedure TOrderedMap<TKey, TValue>.SetValue(const AKey: TKey;
  const AValue: TValue);
begin
  Add(AKey, AValue);
end;

{ TOrderedMap.TEnumerator }

function TOrderedMap<TKey, TValue>.TEnumerator.GetCurrent:
  TBaseMap<TKey, TValue>.TKeyValuePair;
begin
  Result := FCurrent;
end;

function TOrderedMap<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  while FIndex < FEntryCount do
  begin
    if FEntries[FIndex].Active then
    begin
      FCurrent.Key := FEntries[FIndex].Key;
      FCurrent.Value := FEntries[FIndex].Value;
      Inc(FIndex);
      Result := True;
      Exit;
    end;
    Inc(FIndex);
  end;
  Result := False;
end;

{ Iteration }

function TOrderedMap<TKey, TValue>.GetEnumerator: TEnumerator;
begin
  Result.FEntries := FEntries;
  Result.FEntryCount := FEntryCount;
  Result.FIndex := 0;
  Result.FCurrent.Key := Default(TKey);
  Result.FCurrent.Value := Default(TValue);
end;

function TOrderedMap<TKey, TValue>.GetNextEntry(var AIterState: Integer;
  out AKey: TKey; out AValue: TValue): Boolean;
begin
  while AIterState < FEntryCount do
  begin
    if FEntries[AIterState].Active then
    begin
      AKey := FEntries[AIterState].Key;
      AValue := FEntries[AIterState].Value;
      Inc(AIterState);
      Result := True;
      Exit;
    end;
    Inc(AIterState);
  end;
  Result := False;
end;

function TOrderedMap<TKey, TValue>.EntryAt(
  AIndex: Integer): TBaseMap<TKey, TValue>.TKeyValuePair;
var
  I, J: Integer;
begin
  if FCount = 0 then
    raise ERangeError.CreateFmt('EntryAt index %d out of range: map is empty',
      [AIndex]);
  if (AIndex < 0) or (AIndex >= FCount) then
    raise ERangeError.CreateFmt('EntryAt index %d out of range [0..%d]',
      [AIndex, FCount - 1]);

  J := 0;
  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
    begin
      if J = AIndex then
      begin
        Result.Key := FEntries[I].Key;
        Result.Value := FEntries[I].Value;
        Exit;
      end;
      Inc(J);
    end;
end;

end.
