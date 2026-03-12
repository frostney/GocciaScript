{
  TOrderedStringMap<TValue> - String-keyed insertion-order-preserving map.

  Standalone implementation (does not inherit from TOrderedMap) with
  static inline DJB2 string hash and native string equality. This avoids
  the virtual dispatch overhead of TOrderedMap<TKey, TValue>.HashKey on
  every lookup — critical for property maps, module exports, and other
  hot paths where string-keyed maps dominate.

  Use case: JS object string properties, class methods, module exports.
}

unit OrderedStringMap;

{$I Goccia.inc}

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
      function GetCurrent: TBaseMap<string, TValue>.TKeyValuePair; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: TBaseMap<string, TValue>.TKeyValuePair read GetCurrent;
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

    class function HashKey(const AKey: string): Cardinal; static; inline;
    class function KeysEqual(const A, B: string): Boolean; static; inline;

    function FindBucket(const AKey: string; AHash: Cardinal;
      out ABucketIdx: Integer): Boolean;
    procedure Grow;
    procedure Rehash(ANewBucketCount: Integer);
    procedure Compact;

  protected
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

    function GetEnumerator: TEnumerator; inline;
    function EntryAt(AIndex: Integer): TBaseMap<string, TValue>.TKeyValuePair;

    property Capacity: Integer read FBucketCount;
  end;

  TStringStringMap = TOrderedStringMap<string>;

implementation

{ Hash / Equality — static inline: DJB2 on string characters }

{$PUSH}{$R-}{$Q-}
class function TOrderedStringMap<TValue>.HashKey(const AKey: string): Cardinal;
var
  I: Integer;
begin
  Result := 5381;
  for I := 1 to Length(AKey) do
    Result := Result * 33 + Ord(AKey[I]);
end;
{$POP}

class function TOrderedStringMap<TValue>.KeysEqual(const A, B: string): Boolean;
begin
  Result := A = B;
end;

{ Probe }

function TOrderedStringMap<TValue>.FindBucket(const AKey: string; AHash: Cardinal;
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
            KeysEqual(FEntries[EntryIdx].Key, AKey) then
    begin
      ABucketIdx := Idx;
      Result := True;
      Exit;
    end;

    Idx := (Idx + 1) mod FBucketCount;
  end;
end;

{ Resize }

procedure TOrderedStringMap<TValue>.Grow;
var
  N: Integer;
begin
  N := FBucketCount * 2;
  if N < INITIAL_CAPACITY then
    N := INITIAL_CAPACITY;
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
      Idx := FEntries[I].Hash mod Cardinal(FBucketCount);
      while FBuckets[Idx] >= 0 do
        Idx := (Idx + 1) mod FBucketCount;
      FBuckets[Idx] := I;
    end;
end;

procedure TOrderedStringMap<TValue>.Compact;
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

constructor TOrderedStringMap<TValue>.Create;
begin
  Create(INITIAL_CAPACITY);
end;

constructor TOrderedStringMap<TValue>.Create(AInitialCapacity: Integer);
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

destructor TOrderedStringMap<TValue>.Destroy;
begin
  FEntries := nil;
  FBuckets := nil;
  inherited;
end;

{ Core operations }

procedure TOrderedStringMap<TValue>.Add(const AKey: string; const AValue: TValue);
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

function TOrderedStringMap<TValue>.TryGetValue(const AKey: string;
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

function TOrderedStringMap<TValue>.ContainsKey(const AKey: string): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  Hash := HashKey(AKey);
  Result := FindBucket(AKey, Hash, BucketIdx);
end;

function TOrderedStringMap<TValue>.Remove(const AKey: string): Boolean;
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

procedure TOrderedStringMap<TValue>.Clear;
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

end.
