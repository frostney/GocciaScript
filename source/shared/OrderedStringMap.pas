{
  TOrderedStringMap<TValue> - String-keyed insertion-order-preserving map.

  Standalone implementation (does not inherit from TOrderedMap) with
  static inline DJB2 hashing and ECMAScript UTF-16 string equality. This avoids
  the virtual dispatch overhead of TOrderedMap<TKey, TValue>.HashKey on
  every lookup — critical for property maps, module exports, and other
  hot paths where string-keyed maps dominate.

  Use case: JS object string properties, class methods, module exports.
}

unit OrderedStringMap;

{$I Shared.inc}

interface

uses
  SysUtils,

  BaseMap;

type
  TOrderedStringMap<TValue> = class(TBaseMap<string, TValue>)
  public type
    TEntry = record
      Key: string;
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
      FCurrent: TBaseMap<string, TValue>.TKeyValuePair;
      function GetCurrent: TBaseMap<string, TValue>.TKeyValuePair; {$IFDEF FPC}inline;{$ENDIF}
    public
      function MoveNext: Boolean; {$IFDEF FPC}inline;{$ENDIF}
      property Current: TBaseMap<string, TValue>.TKeyValuePair read GetCurrent;
    end;

  private const
    EMPTY_SLOT          = -1;
    DELETED_SLOT        = -2;
    INITIAL_CAPACITY    = 16;
    LOAD_FACTOR_PERCENT = 70;
    // Storage growth whose transient footprint is smaller than this is not
    // reported to RequireStorageBytes. The hook exists to bound runaway
    // growth, and every small map in a program — one per object literal —
    // would otherwise pay a virtual call for blocks far too small to matter.
    // Growth is geometric, so a map that does run away crosses the threshold
    // within a few appends and is reported at every doubling from then on.
    //
    // The threshold buys that at the price of a precisely known blind spot.
    // With SizeOf(TEntry) = 24 on a 64-bit target the entry array's capacity
    // runs 2, 6, 14, 30, 62, 126, … and the transient (old + new) first
    // reaches 4096 bytes at the 62 -> 126 step, so a map that never holds
    // more than 62 entries is never reported at all; the bucket array's
    // transient first reaches it at 512 -> 1024 buckets, around 358 entries.
    // Lowering the threshold does not measurably shrink the gate's real hole
    // — that hole is the *used* figure the request is compared against, not
    // the request-size filter — so the blind spot is documented rather than
    // paid down. See ADR 0106 for the measurements.
    GATED_GROWTH_MIN_BYTES = 4096;

  private
    FEntries: TEntryArray;
    FBuckets: array of Int32;
    FCount: Integer;
    FDeletedCount: Integer;
    FEntryCount: Integer;
    FBucketCount: Integer;
    // Monotonic stamp for entry-index inline caches.  Unique across all map
    // instances of this instantiation (seeded from the shared class counter)
    // and re-stamped by every operation that can invalidate a previously
    // observed entry index (Remove, Compact, Clear).  Adds never invalidate:
    // entries append.
    FEntryVersion: Cardinal;
    class var FEntryVersionCounter: LongInt;
    // Interlocked so maps created on worker threads (parallel test runner)
    // still receive unique stamps; a stamp recorded against one map instance
    // can therefore never validate against a different instance that happens
    // to reuse the same address.
    class function NextEntryVersion: Cardinal; static; {$IFDEF FPC}inline;{$ENDIF}

    class function HashKey(const AKey: string): Cardinal; static; {$IFDEF FPC}inline;{$ENDIF}
    class function KeysEqual(const A, B: string): Boolean; static; {$IFDEF FPC}inline;{$ENDIF}

    function DeletedSlotsNeedCompaction: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function FindBucket(const AKey: string; AHash: Cardinal;
      out ABucketIdx: Integer): Boolean;
    procedure Grow(const APendingValue: TValue);
    procedure Rehash(ANewBucketCount: Integer);
    procedure Compact(const APendingValue: TValue);
    // Reports a storage reallocation to RequireStorageBytes, but only when its
    // transient footprint clears GATED_GROWTH_MIN_BYTES.
    //
    // AOldBytes is the block that is still live while ANewBytes is being
    // allocated. SetLength on a dynamic array of records with managed fields
    // is free to allocate a fresh block and copy rather than extend in place,
    // and Compact holds both arrays at once by construction, so the peak the
    // gate is asked to bound is the sum — reporting only the new block would
    // under-report the real peak by up to a third.
    procedure GateStorageGrowth(const AOldBytes, ANewBytes: Int64;
      const APendingValue: TValue);

  protected
    // Growth gate: called with the transient byte footprint of a storage
    // reallocation — the block being allocated plus the block still live while
    // it is — before the map mutates anything. The default permits every
    // request; this unit is generic infrastructure and owns no budget. A
    // subclass whose contents are sized by untrusted input (JS object property
    // storage) overrides it and raises, which aborts the Add with the map
    // still in its pre-Add state.
    // It runs only when storage actually grows past GATED_GROWTH_MIN_BYTES —
    // growth is geometric, so O(log Count) times over the map's life, never
    // once per Add and never at all for the small maps that dominate.
    //
    // APendingValue is the value the in-flight Add has not stored yet. Every
    // growth point is reached from Add and from nowhere else — Grow and Compact
    // are private and Add is their only caller — so it is always the real
    // pending value, never a default. It is passed because a gate that can
    // collect — and the JS property-storage subclass's will — must be able to
    // root what the caller is holding: the value is not in the map yet, so
    // nothing else in the engine can see it. Threading it here rather than
    // having callers root before every Add is what keeps the cost on the
    // O(log Count) growth path instead of the per-store one.
    procedure RequireStorageBytes(const ABytes: Int64;
      const APendingValue: TValue); virtual;
    function GetCount: Integer; override;
    function GetValue(const AKey: string): TValue; override;
    procedure SetValue(const AKey: string; const AValue: TValue); override;
    function GetNextEntry(var AIterState: Integer;
      out AKey: string; out AValue: TValue): Boolean; override;

  public
    constructor Create; overload;
    constructor Create(AInitialCapacity: Integer); overload;
    destructor Destroy; override;

    procedure Add(const AKey: string; const AValue: TValue); override;
    function TryGetValue(const AKey: string; out AValue: TValue): Boolean; override;
    function ContainsKey(const AKey: string): Boolean; override;
    function Remove(const AKey: string): Boolean; override;
    procedure Clear; override;

    function GetEnumerator: TEnumerator; {$IFDEF FPC}inline;{$ENDIF}
    // Random-access lookup by active-entry position: O(AIndex), since it scans
    // active entries from the start. For sequential iteration use the enumerator
    // (`for Pair in Map do`); driving EntryAt from a `for I := 0 to Count - 1`
    // loop re-scans the prefix on every call and is O(Count^2).
    function EntryAt(AIndex: Integer): TBaseMap<string, TValue>.TKeyValuePair;

    // Non-virtual live-entry count for hot validation paths (the Count
    // property dispatches through TBaseMap's virtual GetCount).
    function CountFast: Integer; {$IFDEF FPC}inline;{$ENDIF}
    // Entry-index access for version-validated inline caches: look up an
    // entry index once, then re-read its value directly while EntryVersion
    // is unchanged.
    function TryGetEntryIndex(const AKey: string; out AIndex: Integer): Boolean;
    function TryGetValueAtEntry(const AIndex: Integer;
      out AValue: TValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function KeyAtEntry(const AIndex: Integer): string;

    property Capacity: Integer read FBucketCount;
    property DeletedCount: Integer read FDeletedCount;
    property EntryVersion: Cardinal read FEntryVersion;
  end;

  TStringStringMap = TOrderedStringMap<string>;

implementation

uses
  TextSemantics;

{ Hash / Equality — ECMAScript strings compare as UTF-16 code-unit sequences,
  even when their internal UTF-8/WTF-8 byte encodings differ. }

{$IFDEF FPC}
  {$PUSH}
{$ENDIF}
{$R-}{$Q-}
class function TOrderedStringMap<TValue>.NextEntryVersion: Cardinal;
begin
  {$IFDEF FPC}
  Result := Cardinal(InterLockedIncrement(FEntryVersionCounter));
  {$ELSE}
  Result := Cardinal(System.AtomicIncrement(FEntryVersionCounter, 1));
  {$ENDIF}
end;

class function TOrderedStringMap<TValue>.HashKey(const AKey: string): Cardinal;
begin
  Result := UTF16StringHash(AKey);
end;
{$IFDEF FPC}
  {$POP}
{$ELSE}
  {$IFNDEF PRODUCTION}{$R+}{$Q+}{$ENDIF}
{$ENDIF}

class function TOrderedStringMap<TValue>.KeysEqual(const A, B: string): Boolean;
begin
  Result := UTF16StringsEqual(A, B);
end;

function TOrderedStringMap<TValue>.DeletedSlotsNeedCompaction: Boolean;
begin
  Result := FDeletedCount > FCount;
end;

{ Probe }

function TOrderedStringMap<TValue>.FindBucket(const AKey: string; AHash: Cardinal;
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

procedure TOrderedStringMap<TValue>.RequireStorageBytes(const ABytes: Int64;
  const APendingValue: TValue);
begin
  // No budget at this layer; see the declaration.
end;

procedure TOrderedStringMap<TValue>.GateStorageGrowth(const AOldBytes,
  ANewBytes: Int64; const APendingValue: TValue);
var
  Transient: Int64;
begin
  Transient := AOldBytes + ANewBytes;
  if Transient >= GATED_GROWTH_MIN_BYTES then
    RequireStorageBytes(Transient, APendingValue);
end;

procedure TOrderedStringMap<TValue>.Grow(const APendingValue: TValue);
var
  N: Integer;
begin
  N := FBucketCount * 2;
  if N < INITIAL_CAPACITY then
    N := INITIAL_CAPACITY;
  GateStorageGrowth(Int64(FBucketCount) * SizeOf(Int32),
    Int64(N) * SizeOf(Int32), APendingValue);
  Rehash(N);
end;

procedure TOrderedStringMap<TValue>.Rehash(ANewBucketCount: Integer);
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

  FDeletedCount := 0;
end;

procedure TOrderedStringMap<TValue>.Compact(const APendingValue: TValue);
var
  NewEntries: TEntryArray;
  I, J: Integer;
begin
  // Compaction can never end up larger than the map already is, so it cannot
  // run away — but it holds the old and the new entry array at the same time,
  // which is a real transient the gate would otherwise never see. Reporting it
  // here, before either array is touched, keeps a refusal in Compact as clean
  // as one in Add: the map is still exactly as the caller found it. Rehash
  // below reuses FBucketCount, so its SetLength is a no-op and contributes
  // nothing to the peak.
  //
  // A refusal here abandons a compaction that would have *reduced* steady
  // state. That is the right trade only because it can happen only when the
  // budget is already exhausted, and the alternative is committing a peak the
  // budget exists to forbid.
  GateStorageGrowth(Int64(FEntryCount) * SizeOf(TEntry),
    Int64(FCount) * SizeOf(TEntry), APendingValue);
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
  FEntryVersion := NextEntryVersion;
  Rehash(FBucketCount);
end;

{ Constructor / Destructor }

constructor TOrderedStringMap<TValue>.Create;
begin
  Create(0);
end;

constructor TOrderedStringMap<TValue>.Create(AInitialCapacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCount := 0;
  FDeletedCount := 0;
  FEntryCount := 0;
  FEntryVersion := NextEntryVersion;

  if AInitialCapacity <= 0 then
  begin
    FBucketCount := 0;
    Exit;
  end;

  FBucketCount := INITIAL_CAPACITY;
  while FBucketCount < AInitialCapacity do
    FBucketCount := FBucketCount * 2;

  SetLength(FBuckets, FBucketCount);
  for I := 0 to FBucketCount - 1 do
    FBuckets[I] := EMPTY_SLOT;
end;

destructor TOrderedStringMap<TValue>.Destroy;
begin
  FEntries := nil;
  FBuckets := nil;
  inherited;
end;

{ Core operations }

function TOrderedStringMap<TValue>.CountFast: Integer;
begin
  Result := FCount;
end;

procedure TOrderedStringMap<TValue>.Add(const AKey: string; const AValue: TValue);
var
  Hash: Cardinal;
  BucketIdx, EntryIdx, NewEntryCapacity: Integer;
begin
  Hash := HashKey(AKey);

  if FBucketCount = 0 then
    Grow(AValue);

  if FindBucket(AKey, Hash, BucketIdx) then
  begin
    FEntries[FBuckets[BucketIdx]].Value := AValue;
    Exit;
  end;

  if (FEntryCount + 1) * 100 > FBucketCount * LOAD_FACTOR_PERCENT then
  begin
    if FCount < FEntryCount div 2 then
      Compact(AValue)
    else
      Grow(AValue);
    FindBucket(AKey, Hash, BucketIdx);
  end;

  if DeletedSlotsNeedCompaction then
  begin
    Compact(AValue);
    FindBucket(AKey, Hash, BucketIdx);
  end;

  EntryIdx := FEntryCount;
  // Grow the entry array before claiming the slot, so a refused growth leaves
  // the map exactly as the caller found it.
  if EntryIdx >= Length(FEntries) then
  begin
    NewEntryCapacity := (EntryIdx + 1) * 2;
    GateStorageGrowth(Int64(Length(FEntries)) * SizeOf(TEntry),
      Int64(NewEntryCapacity) * SizeOf(TEntry), AValue);
    SetLength(FEntries, NewEntryCapacity);
  end;
  Inc(FEntryCount);

  FEntries[EntryIdx].Key := AKey;
  FEntries[EntryIdx].Value := AValue;
  FEntries[EntryIdx].Hash := Hash;
  FEntries[EntryIdx].Active := True;

  if FBuckets[BucketIdx] = DELETED_SLOT then
    Dec(FDeletedCount);
  FBuckets[BucketIdx] := EntryIdx;
  Inc(FCount);
end;

function TOrderedStringMap<TValue>.TryGetValue(const AKey: string;
  out AValue: TValue): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  if FBucketCount = 0 then
  begin
    AValue := Default(TValue);
    Result := False;
    Exit;
  end;
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
  if Result then
    AValue := FEntries[FBuckets[BucketIdx]].Value
  else
    AValue := Default(TValue);
end;

function TOrderedStringMap<TValue>.ContainsKey(const AKey: string): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  if FBucketCount = 0 then
  begin
    Result := False;
    Exit;
  end;
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
end;

function TOrderedStringMap<TValue>.Remove(const AKey: string): Boolean;
var
  Hash: Cardinal;
  BucketIdx, EntryIdx: Integer;
begin
  if FBucketCount = 0 then
  begin
    Result := False;
    Exit;
  end;
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
  if not Result then
    Exit;

  EntryIdx := FBuckets[BucketIdx];
  FEntries[EntryIdx].Active := False;
  FEntries[EntryIdx].Key := '';
  FEntries[EntryIdx].Value := Default(TValue);
  FBuckets[BucketIdx] := DELETED_SLOT;
  Inc(FDeletedCount);
  Dec(FCount);
  FEntryVersion := NextEntryVersion;
end;

procedure TOrderedStringMap<TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to FBucketCount - 1 do
    FBuckets[I] := EMPTY_SLOT;
  SetLength(FEntries, 0);
  FCount := 0;
  FDeletedCount := 0;
  FEntryCount := 0;
  FEntryVersion := NextEntryVersion;
end;

{ Accessors }

function TOrderedStringMap<TValue>.GetCount: Integer;
begin
  Result := FCount;
end;

function TOrderedStringMap<TValue>.GetValue(const AKey: string): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise Exception.Create('Key not found in ordered string map');
end;

procedure TOrderedStringMap<TValue>.SetValue(const AKey: string;
  const AValue: TValue);
begin
  Add(AKey, AValue);
end;

{ TOrderedStringMap.TEnumerator }

function TOrderedStringMap<TValue>.TEnumerator.GetCurrent:
  TBaseMap<string, TValue>.TKeyValuePair;
begin
  Result := FCurrent;
end;

function TOrderedStringMap<TValue>.TEnumerator.MoveNext: Boolean;
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

function TOrderedStringMap<TValue>.GetEnumerator: TEnumerator;
begin
  Result.FEntries := FEntries;
  Result.FEntryCount := FEntryCount;
  Result.FIndex := 0;
  Result.FCurrent.Key := '';
  Result.FCurrent.Value := Default(TValue);
end;

function TOrderedStringMap<TValue>.GetNextEntry(var AIterState: Integer;
  out AKey: string; out AValue: TValue): Boolean;
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

function TOrderedStringMap<TValue>.EntryAt(
  AIndex: Integer): TBaseMap<string, TValue>.TKeyValuePair;
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

function TOrderedStringMap<TValue>.TryGetEntryIndex(const AKey: string;
  out AIndex: Integer): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  AIndex := -1;
  if FBucketCount = 0 then
    Exit(False);
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
  if Result then
    AIndex := FBuckets[BucketIdx];
end;

function TOrderedStringMap<TValue>.TryGetValueAtEntry(const AIndex: Integer;
  out AValue: TValue): Boolean;
begin
  // Callers must pair this with an EntryVersion check; while the version is
  // unchanged an index obtained from TryGetEntryIndex stays active and keeps
  // its key (Remove/Compact/Clear re-stamp the version).
  Result := (AIndex >= 0) and (AIndex < FEntryCount) and FEntries[AIndex].Active;
  if Result then
    AValue := FEntries[AIndex].Value
  else
    AValue := Default(TValue);
end;

function TOrderedStringMap<TValue>.KeyAtEntry(const AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FEntryCount) then
    Result := FEntries[AIndex].Key
  else
    Result := '';
end;

end.
