{
  TBaseMap<TKey, TValue> - Abstract generic base for all map types.

  Provides:
  - Shared type aliases (TKeyValuePair, TKeyValueArray, etc.)
  - Abstract API contract (Add, TryGetValue, ContainsKey, Remove, Clear)
  - Concrete iteration (ToArray, Keys, Values, ForEach) built on
    a single abstract GetNextEntry primitive.
  - AddOrSetValue convenience wrapper for TDictionary API compatibility.

  Performance notes:
  - No hash or equality logic lives here. Each subclass brings its own,
    typically as static/inline class methods — zero virtual overhead on
    the inner probe loops.
  - GetNextEntry is virtual but only invoked during bulk iteration,
    never on the hot lookup path.
  - Core operations (Add, TryGetValue, etc.) are virtual for polymorphism
    but the actual work (hashing, probing, scanning) is private to each
    subclass and fully inlined.
}

unit BaseMap;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TBaseMap<TKey, TValue> = class
  public type
    TKeyValuePair = record
      Key: TKey;
      Value: TValue;
    end;

    TKeyValueArray = array of TKeyValuePair;
    TKeyArray = array of TKey;
    TValueArray = array of TValue;
    TForEachCallback = procedure(const AKey: TKey; const AValue: TValue) of object;

  protected
    function GetCount: Integer; virtual; abstract;
    function GetValue(const AKey: TKey): TValue; virtual; abstract;
    procedure SetValue(const AKey: TKey; const AValue: TValue); virtual; abstract;

    { Iteration primitive. Subclasses advance AIterState (opaque integer)
      and return the next active entry. Return False when exhausted.
      AIterState is initialized to 0 before the first call. }
    function GetNextEntry(var AIterState: Integer;
      out AKey: TKey; out AValue: TValue): Boolean; virtual; abstract;

  public
    procedure Add(const AKey: TKey; const AValue: TValue); virtual; abstract;
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean; virtual; abstract;
    function ContainsKey(const AKey: TKey): Boolean; virtual; abstract;
    function Remove(const AKey: TKey): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;

    function ToArray: TKeyValueArray;
    procedure ForEach(ACallback: TForEachCallback);
    function Keys: TKeyArray;
    function Values: TValueArray;

    property Items[const AKey: TKey]: TValue read GetValue write SetValue; default;
    property Count: Integer read GetCount;
  end;

implementation

{ TBaseMap<TKey, TValue> }

procedure TBaseMap<TKey, TValue>.AddOrSetValue(const AKey: TKey; const AValue: TValue);
begin
  Add(AKey, AValue);
end;

function TBaseMap<TKey, TValue>.ToArray: TKeyValueArray;
var
  IterState, I: Integer;
  K: TKey;
  V: TValue;
begin
  Result := nil;
  SetLength(Result, Count);
  IterState := 0;
  I := 0;
  while GetNextEntry(IterState, K, V) do
  begin
    Result[I].Key := K;
    Result[I].Value := V;
    Inc(I);
  end;
end;

procedure TBaseMap<TKey, TValue>.ForEach(ACallback: TForEachCallback);
var
  IterState: Integer;
  K: TKey;
  V: TValue;
begin
  IterState := 0;
  while GetNextEntry(IterState, K, V) do
    ACallback(K, V);
end;

function TBaseMap<TKey, TValue>.Keys: TKeyArray;
var
  IterState, I: Integer;
  K: TKey;
  V: TValue;
begin
  Result := nil;
  SetLength(Result, Count);
  IterState := 0;
  I := 0;
  while GetNextEntry(IterState, K, V) do
  begin
    Result[I] := K;
    Inc(I);
  end;
end;

function TBaseMap<TKey, TValue>.Values: TValueArray;
var
  IterState, I: Integer;
  K: TKey;
  V: TValue;
begin
  Result := nil;
  SetLength(Result, Count);
  IterState := 0;
  I := 0;
  while GetNextEntry(IterState, K, V) do
  begin
    Result[I] := V;
    Inc(I);
  end;
end;

end.
