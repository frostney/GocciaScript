{
  TOrderedMap<TValue> - Insertion-order-preserving string-keyed map.

  - Entries in a flat array preserve insertion order.
  - Open-addressed bucket array for O(1) average lookup.
  - Tombstoned deletes keep probe chains intact.
  - Auto-resize at 70% load; compacts when tombstones dominate.
}

unit OrderedMap;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TOrderedMap<TValue> = class
  public type
    TEntry = record
      Key: string;
      Value: TValue;
      Hash: Cardinal;
      Active: Boolean;
    end;

    TKeyValuePair = record
      Key: string;
      Value: TValue;
    end;

    TEntryArray = array of TEntry;
    TKeyValueArray = array of TKeyValuePair;
    TKeyArray = array of string;
    TValueArray = array of TValue;
    TForEachCallback = procedure(const AKey: string; const AValue: TValue) of object;

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

    class function HashKey(const AKey: string): Cardinal; static; inline;
    function FindBucket(const AKey: string; AHash: Cardinal; out ABucketIdx: Integer): Boolean;
    procedure Grow;
    procedure Rehash(ANewBucketCount: Integer);
    procedure Compact;

    function GetValue(const AKey: string): TValue;
    procedure SetValue(const AKey: string; const AValue: TValue);
    function GetCapacity: Integer;

  public
    constructor Create; overload;
    constructor Create(AInitialCapacity: Integer); overload;
    destructor Destroy; override;

    procedure Add(const AKey: string; const AValue: TValue);
    procedure AddOrSetValue(const AKey: string; const AValue: TValue); inline;
    function TryGetValue(const AKey: string; out AValue: TValue): Boolean;
    function ContainsKey(const AKey: string): Boolean;
    function Remove(const AKey: string): Boolean;
    procedure Clear;

    function ToArray: TKeyValueArray;
    procedure ForEach(ACallback: TForEachCallback);
    function Keys: TKeyArray;
    function Values: TValueArray;
    function EntryAt(AIndex: Integer): TKeyValuePair;

    property Items[const AKey: string]: TValue read GetValue write SetValue; default;
    property Count: Integer read FCount;
    property Capacity: Integer read GetCapacity;
  end;

implementation

{ Hash / Probe }

{$PUSH}{$R-}{$Q-}
class function TOrderedMap<TValue>.HashKey(const AKey: string): Cardinal;
var
  I: Integer;
begin
  Result := 5381;
  for I := 1 to Length(AKey) do
    Result := Result * 33 + Ord(AKey[I]);
end;
{$POP}

function TOrderedMap<TValue>.FindBucket(const AKey: string; AHash: Cardinal;
  out ABucketIdx: Integer): Boolean;
var
  Idx, EntryIdx, FirstDeleted: Integer;
begin
  Result := False;
  FirstDeleted := -1;
  Idx := AHash mod Cardinal(FBucketCount);

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
            (FEntries[EntryIdx].Key = AKey) then
    begin
      ABucketIdx := Idx;
      Result := True;
      Exit;
    end;

    Idx := (Idx + 1) mod FBucketCount;
  end;
end;

{ Resize }

procedure TOrderedMap<TValue>.Grow;
var
  N: Integer;
begin
  N := FBucketCount * 2;
  if N < INITIAL_CAPACITY then
    N := INITIAL_CAPACITY;
  Rehash(N);
end;

procedure TOrderedMap<TValue>.Rehash(ANewBucketCount: Integer);
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
      Idx := FEntries[I].Hash mod Cardinal(FBucketCount);
      while FBuckets[Idx] >= 0 do
        Idx := (Idx + 1) mod FBucketCount;
      FBuckets[Idx] := I;
    end;
end;

procedure TOrderedMap<TValue>.Compact;
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

constructor TOrderedMap<TValue>.Create;
begin
  Create(INITIAL_CAPACITY);
end;

constructor TOrderedMap<TValue>.Create(AInitialCapacity: Integer);
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

destructor TOrderedMap<TValue>.Destroy;
begin
  FEntries := nil;
  FBuckets := nil;
  inherited;
end;

{ Core operations }

procedure TOrderedMap<TValue>.Add(const AKey: string; const AValue: TValue);
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

procedure TOrderedMap<TValue>.AddOrSetValue(const AKey: string; const AValue: TValue);
begin
  Add(AKey, AValue);
end;

function TOrderedMap<TValue>.TryGetValue(const AKey: string; out AValue: TValue): Boolean;
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

function TOrderedMap<TValue>.ContainsKey(const AKey: string): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
end;

function TOrderedMap<TValue>.Remove(const AKey: string): Boolean;
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
  FEntries[EntryIdx].Key := '';
  FEntries[EntryIdx].Value := Default(TValue);
  FBuckets[BucketIdx] := DELETED_SLOT;
  Dec(FCount);
end;

procedure TOrderedMap<TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to FBucketCount - 1 do
    FBuckets[I] := EMPTY_SLOT;
  SetLength(FEntries, 0);
  FCount := 0;
  FEntryCount := 0;
end;

function TOrderedMap<TValue>.GetValue(const AKey: string): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise Exception.Create('Key not found in ordered map');
end;

procedure TOrderedMap<TValue>.SetValue(const AKey: string; const AValue: TValue);
begin
  Add(AKey, AValue);
end;

function TOrderedMap<TValue>.GetCapacity: Integer;
begin
  Result := FBucketCount;
end;

{ Iteration }

function TOrderedMap<TValue>.ToArray: TKeyValueArray;
var
  I, J: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  J := 0;
  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
    begin
      Result[J].Key := FEntries[I].Key;
      Result[J].Value := FEntries[I].Value;
      Inc(J);
    end;
end;

procedure TOrderedMap<TValue>.ForEach(ACallback: TForEachCallback);
var
  I: Integer;
begin
  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
      ACallback(FEntries[I].Key, FEntries[I].Value);
end;

function TOrderedMap<TValue>.Keys: TKeyArray;
var
  I, J: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  J := 0;
  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
    begin
      Result[J] := FEntries[I].Key;
      Inc(J);
    end;
end;

function TOrderedMap<TValue>.Values: TValueArray;
var
  I, J: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  J := 0;
  for I := 0 to FEntryCount - 1 do
    if FEntries[I].Active then
    begin
      Result[J] := FEntries[I].Value;
      Inc(J);
    end;
end;

function TOrderedMap<TValue>.EntryAt(AIndex: Integer): TKeyValuePair;
var
  I, J: Integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise ERangeError.CreateFmt('EntryAt index %d out of range [0..%d]', [AIndex, FCount - 1]);

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
