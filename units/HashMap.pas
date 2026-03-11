{
  THashMap<TKey, TValue> - Lightweight open-addressed generic hash map.

  Use case: GC tracking, caches, interning.

  Inherits TBaseMap<TKey, TValue> for shared types and iteration.

  Hash/equality are static inline — no virtual dispatch on the hot path.
  Backshift deletion — no tombstones ever.
}

unit HashMap;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BaseMap;

type
  THashMap<TKey, TValue> = class(TBaseMap<TKey, TValue>)
  private type
    TSlot = record
      Key: TKey;
      Value: TValue;
      Hash: Cardinal;
      Used: Boolean;
    end;

  private const
    INITIAL_CAPACITY    = 16;
    LOAD_FACTOR_PERCENT = 70;

  private
    FSlots: array of TSlot;
    FCount: Integer;
    FCapacity: Integer;

    class function HashKey(const AKey: TKey): Cardinal; static; inline;
    class function KeysEqual(const A, B: TKey): Boolean; static; inline;

    function FindSlot(const AKey: TKey; AHash: Cardinal): Integer; inline;
    procedure Grow;
    procedure Reinsert(const ASlot: TSlot);

  protected
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

    property Capacity: Integer read FCapacity;
  end;

implementation

{ Hash / Equality — static inline: DJB2 over raw key bytes }

{$PUSH}{$R-}{$Q-}
class function THashMap<TKey, TValue>.HashKey(const AKey: TKey): Cardinal;
var
  P: PByte;
  I: Integer;
begin
  Result := 5381;
  P := @AKey;
  for I := 0 to SizeOf(TKey) - 1 do
    Result := Result * 33 + P[I];
end;
{$POP}

class function THashMap<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(TKey));
end;

{ Slot lookup }

function THashMap<TKey, TValue>.FindSlot(const AKey: TKey;
  AHash: Cardinal): Integer;
var
  Idx: Integer;
begin
  Idx := Integer(AHash mod Cardinal(FCapacity));
  while FSlots[Idx].Used do
  begin
    if (FSlots[Idx].Hash = AHash) and KeysEqual(FSlots[Idx].Key, AKey) then
    begin
      Result := Idx;
      Exit;
    end;
    Idx := (Idx + 1) mod FCapacity;
  end;
  Result := Idx;
end;

{ Resize }

procedure THashMap<TKey, TValue>.Grow;
var
  OldSlots: array of TSlot;
  OldCapacity, I: Integer;
begin
  OldSlots := FSlots;
  OldCapacity := FCapacity;
  FCapacity := FCapacity * 2;
  SetLength(FSlots, FCapacity);
  for I := 0 to FCapacity - 1 do
    FSlots[I].Used := False;
  FCount := 0;
  for I := 0 to OldCapacity - 1 do
    if OldSlots[I].Used then
      Reinsert(OldSlots[I]);
end;

procedure THashMap<TKey, TValue>.Reinsert(const ASlot: TSlot);
var
  Idx: Integer;
begin
  Idx := Integer(ASlot.Hash mod Cardinal(FCapacity));
  while FSlots[Idx].Used do
    Idx := (Idx + 1) mod FCapacity;
  FSlots[Idx] := ASlot;
  Inc(FCount);
end;

{ Constructor / Destructor }

constructor THashMap<TKey, TValue>.Create;
begin
  Create(INITIAL_CAPACITY);
end;

constructor THashMap<TKey, TValue>.Create(AInitialCapacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCount := 0;

  if AInitialCapacity < INITIAL_CAPACITY then
    AInitialCapacity := INITIAL_CAPACITY;

  FCapacity := INITIAL_CAPACITY;
  while FCapacity < AInitialCapacity do
    FCapacity := FCapacity * 2;

  SetLength(FSlots, FCapacity);
  for I := 0 to FCapacity - 1 do
    FSlots[I].Used := False;
end;

destructor THashMap<TKey, TValue>.Destroy;
begin
  FSlots := nil;
  inherited;
end;

{ Core operations }

procedure THashMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  Hash: Cardinal;
  Idx: Integer;
begin
  if (FCount + 1) * 100 > FCapacity * LOAD_FACTOR_PERCENT then
    Grow;

  Hash := HashKey(AKey);
  Idx := FindSlot(AKey, Hash);

  if FSlots[Idx].Used then
  begin
    FSlots[Idx].Value := AValue;
    Exit;
  end;

  FSlots[Idx].Key := AKey;
  FSlots[Idx].Value := AValue;
  FSlots[Idx].Hash := Hash;
  FSlots[Idx].Used := True;
  Inc(FCount);
end;

function THashMap<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
var
  Hash: Cardinal;
  Idx: Integer;
begin
  Hash := HashKey(AKey);
  Idx := FindSlot(AKey, Hash);
  Result := FSlots[Idx].Used;
  if Result then
    AValue := FSlots[Idx].Value
  else
    AValue := Default(TValue);
end;

function THashMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
var
  Hash: Cardinal;
  Idx: Integer;
begin
  Hash := HashKey(AKey);
  Idx := FindSlot(AKey, Hash);
  Result := FSlots[Idx].Used;
end;

function THashMap<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  Hash: Cardinal;
  Idx, J, K: Integer;
begin
  Hash := HashKey(AKey);
  Idx := FindSlot(AKey, Hash);

  if not FSlots[Idx].Used then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  Dec(FCount);

  FSlots[Idx].Used := False;
  FSlots[Idx].Key := Default(TKey);
  FSlots[Idx].Value := Default(TValue);

  { Backshift: displaced entries shift to fill the gap }
  J := Idx;
  while True do
  begin
    J := (J + 1) mod FCapacity;
    if not FSlots[J].Used then
      Break;

    K := Integer(FSlots[J].Hash mod Cardinal(FCapacity));

    { Shift if K is NOT in the range (Idx, J] (circular) — i.e. the
      entry at J had to probe past Idx to reach its current slot }
    if ((Idx < J) and ((K <= Idx) or (K > J))) or
       ((Idx > J) and ((K <= Idx) and (K > J))) then
    begin
      FSlots[Idx] := FSlots[J];
      FSlots[J].Used := False;
      FSlots[J].Key := Default(TKey);
      FSlots[J].Value := Default(TValue);
      Idx := J;
    end;
  end;
end;

procedure THashMap<TKey, TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to FCapacity - 1 do
    FSlots[I].Used := False;
  FCount := 0;
end;

{ Accessors }

function THashMap<TKey, TValue>.GetCount: Integer;
begin
  Result := FCount;
end;

function THashMap<TKey, TValue>.GetValue(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise Exception.Create('Key not found in hash map');
end;

procedure THashMap<TKey, TValue>.SetValue(const AKey: TKey;
  const AValue: TValue);
begin
  Add(AKey, AValue);
end;

{ Iteration }

function THashMap<TKey, TValue>.GetNextEntry(var AIterState: Integer;
  out AKey: TKey; out AValue: TValue): Boolean;
begin
  while AIterState < FCapacity do
  begin
    if FSlots[AIterState].Used then
    begin
      AKey := FSlots[AIterState].Key;
      AValue := FSlots[AIterState].Value;
      Inc(AIterState);
      Result := True;
      Exit;
    end;
    Inc(AIterState);
  end;
  Result := False;
end;

end.
