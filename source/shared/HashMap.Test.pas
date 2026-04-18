program HashMap.Test;

{$I Shared.inc}

uses
  SysUtils,

  BaseMap,
  HashMap,
  TestingPascalLibrary;

type
  THashMapTests = class(TTestSuite)
  private
    { Empty map }
    procedure TestEmptyMapCountIsZero;
    procedure TestEmptyMapContainsKeyFalse;
    procedure TestEmptyMapRemoveFalse;
    procedure TestEmptyMapEnumeratorYieldsNothing;
    procedure TestEmptyMapToArrayIsEmpty;

    { Basic CRUD }
    procedure TestAddAndRetrieve;
    procedure TestAddOverwritesExistingValue;
    procedure TestAddOverwritePreservesCount;
    procedure TestContainsKeyTrue;
    procedure TestContainsKeyFalse;
    procedure TestTryGetValueFound;
    procedure TestTryGetValueNotFound;
    procedure TestRemoveExistingKey;
    procedure TestRemoveNonExistentKey;
    procedure TestRemoveDecreasesCount;

    { GetValue / Items[] }
    procedure TestGetValueRaisesOnMissingKey;
    procedure TestItemsPropertyReadWrite;

    { AddOrSetValue }
    procedure TestAddOrSetValueNewKey;
    procedure TestAddOrSetValueExistingKey;

    { Clear }
    procedure TestClearResetsCount;
    procedure TestClearThenReuse;

    { Grow / rehash }
    procedure TestGrowOnLoadFactor;
    procedure TestAllEntriesSurviveGrow;

    { Backshift deletion }
    procedure TestBackshiftKeepsDisplacedNeighbors;
    procedure TestRemoveAndReaddSameKey;
    procedure TestRemoveFirstMiddleLast;

    { Enumerator }
    procedure TestEnumeratorVisitsExactlyCountEntries;
    procedure TestEnumeratorSumValues;

    { ToArray / Keys / Values }
    procedure TestToArrayLength;
    procedure TestKeysContainsAllKeys;
    procedure TestValuesContainsAllValues;

    { ForEach }
    procedure TestForEachVisitsAllEntries;

    { QWord keys (exercises 8-byte hash branch) }
    procedure TestQWordKeys;

    { Stress }
    procedure TestStressAddRemoveVerify;

    { String values }
    procedure TestStringValues;

    { Custom initial capacity }
    procedure TestCustomInitialCapacity;
  public
    procedure SetupTests; override;
  end;

procedure THashMapTests.SetupTests;
begin
  { Empty map }
  Test('Empty map has count zero', TestEmptyMapCountIsZero);
  Test('Empty map ContainsKey returns False', TestEmptyMapContainsKeyFalse);
  Test('Empty map Remove returns False', TestEmptyMapRemoveFalse);
  Test('Empty map enumerator yields nothing', TestEmptyMapEnumeratorYieldsNothing);
  Test('Empty map ToArray is empty', TestEmptyMapToArrayIsEmpty);

  { Basic CRUD }
  Test('Add and retrieve a value', TestAddAndRetrieve);
  Test('Add with existing key overwrites value', TestAddOverwritesExistingValue);
  Test('Add overwrite does not change count', TestAddOverwritePreservesCount);
  Test('ContainsKey returns True for existing key', TestContainsKeyTrue);
  Test('ContainsKey returns False for missing key', TestContainsKeyFalse);
  Test('TryGetValue returns True and value for existing key', TestTryGetValueFound);
  Test('TryGetValue returns False for missing key', TestTryGetValueNotFound);
  Test('Remove existing key returns True', TestRemoveExistingKey);
  Test('Remove non-existent key returns False', TestRemoveNonExistentKey);
  Test('Remove decreases count by one', TestRemoveDecreasesCount);

  { GetValue / Items[] }
  Test('GetValue raises on missing key', TestGetValueRaisesOnMissingKey);
  Test('Items[] property reads and writes', TestItemsPropertyReadWrite);

  { AddOrSetValue }
  Test('AddOrSetValue inserts new key', TestAddOrSetValueNewKey);
  Test('AddOrSetValue updates existing key', TestAddOrSetValueExistingKey);

  { Clear }
  Test('Clear resets count to zero', TestClearResetsCount);
  Test('Clear then reuse works correctly', TestClearThenReuse);

  { Grow / rehash }
  Test('Map grows when load factor exceeded', TestGrowOnLoadFactor);
  Test('All entries survive grow', TestAllEntriesSurviveGrow);

  { Backshift deletion }
  Test('Backshift keeps displaced neighbors reachable', TestBackshiftKeepsDisplacedNeighbors);
  Test('Remove and re-add same key works correctly', TestRemoveAndReaddSameKey);
  Test('Remove first, middle, and last added keys', TestRemoveFirstMiddleLast);

  { Enumerator }
  Test('Enumerator visits exactly Count entries', TestEnumeratorVisitsExactlyCountEntries);
  Test('Enumerator sums all values', TestEnumeratorSumValues);

  { ToArray / Keys / Values }
  Test('ToArray returns correct length', TestToArrayLength);
  Test('Keys contains all added keys', TestKeysContainsAllKeys);
  Test('Values contains all added values', TestValuesContainsAllValues);

  { ForEach }
  Test('ForEach visits all entries', TestForEachVisitsAllEntries);

  { QWord keys }
  Test('QWord keys use 8-byte hash branch', TestQWordKeys);

  { Stress }
  Test('Stress: add many, remove half, verify remaining', TestStressAddRemoveVerify);

  { String values }
  Test('String values stored and retrieved correctly', TestStringValues);

  { Custom initial capacity }
  Test('Custom initial capacity is respected', TestCustomInitialCapacity);
end;

{ ── Empty map ────────────────────────────────────────────────────── }

procedure THashMapTests.TestEmptyMapCountIsZero;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Expect<Integer>(Map.Count).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestEmptyMapContainsKeyFalse;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(0)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(-1)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestEmptyMapRemoveFalse;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Expect<Boolean>(Map.Remove(42)).ToBe(False);
    Expect<Integer>(Map.Count).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestEmptyMapEnumeratorYieldsNothing;
var
  Map: THashMap<Integer, Integer>;
  Pair: THashMap<Integer, Integer>.TKeyValuePair;
  Visited: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Visited := 0;
    for Pair in Map do
      Inc(Visited);
    Expect<Integer>(Visited).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestEmptyMapToArrayIsEmpty;
var
  Map: THashMap<Integer, Integer>;
  Arr: TBaseMap<Integer, Integer>.TKeyValueArray;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Arr := Map.ToArray;
    Expect<Integer>(Length(Arr)).ToBe(0);
  finally
    Map.Free;
  end;
end;

{ ── Basic CRUD ───────────────────────────────────────────────────── }

procedure THashMapTests.TestAddAndRetrieve;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Expect<Integer>(Map.Count).ToBe(1);
    Expect<Boolean>(Map.TryGetValue(1, V)).ToBe(True);
    Expect<string>(V).ToBe('one');
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestAddOverwritesExistingValue;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'first');
    Map.Add(1, 'second');
    Map.TryGetValue(1, V);
    Expect<string>(V).ToBe('second');
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestAddOverwritePreservesCount;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(5, 50);
    Map.Add(5, 99);
    Map.Add(5, 200);
    Expect<Integer>(Map.Count).ToBe(1);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestContainsKeyTrue;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(42, 100);
    Expect<Boolean>(Map.ContainsKey(42)).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestContainsKeyFalse;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Expect<Boolean>(Map.ContainsKey(99)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestTryGetValueFound;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(5, 'five');
    Expect<Boolean>(Map.TryGetValue(5, V)).ToBe(True);
    Expect<string>(V).ToBe('five');
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestTryGetValueNotFound;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Expect<Boolean>(Map.TryGetValue(999, V)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestRemoveExistingKey;
var
  Map: THashMap<Integer, string>;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Expect<Boolean>(Map.Remove(1)).ToBe(True);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(2)).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestRemoveNonExistentKey;
var
  Map: THashMap<Integer, string>;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Expect<Boolean>(Map.Remove(42)).ToBe(False);
    Expect<Integer>(Map.Count).ToBe(1);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestRemoveDecreasesCount;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Expect<Integer>(Map.Count).ToBe(3);
    Map.Remove(2);
    Expect<Integer>(Map.Count).ToBe(2);
    Map.Remove(1);
    Expect<Integer>(Map.Count).ToBe(1);
    Map.Remove(3);
    Expect<Integer>(Map.Count).ToBe(0);
  finally
    Map.Free;
  end;
end;

{ ── GetValue / Items[] ───────────────────────────────────────────── }

procedure THashMapTests.TestGetValueRaisesOnMissingKey;
var
  Map: THashMap<Integer, Integer>;
  Raised: Boolean;
  Dummy: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Raised := False;
    try
      Dummy := Map[999];
    except
      on E: Exception do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestItemsPropertyReadWrite;
var
  Map: THashMap<Integer, string>;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map[10] := 'ten';
    Expect<Integer>(Map.Count).ToBe(1);
    Expect<string>(Map[10]).ToBe('ten');
    Map[10] := 'TEN';
    Expect<Integer>(Map.Count).ToBe(1);
    Expect<string>(Map[10]).ToBe('TEN');
  finally
    Map.Free;
  end;
end;

{ ── AddOrSetValue ────────────────────────────────────────────────── }

procedure THashMapTests.TestAddOrSetValueNewKey;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.AddOrSetValue(7, 'seven');
    Expect<Integer>(Map.Count).ToBe(1);
    Map.TryGetValue(7, V);
    Expect<string>(V).ToBe('seven');
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestAddOrSetValueExistingKey;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(7, 'seven');
    Map.AddOrSetValue(7, 'SEVEN');
    Expect<Integer>(Map.Count).ToBe(1);
    Map.TryGetValue(7, V);
    Expect<string>(V).ToBe('SEVEN');
  finally
    Map.Free;
  end;
end;

{ ── Clear ────────────────────────────────────────────────────────── }

procedure THashMapTests.TestClearResetsCount;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Map.Clear;
    Expect<Integer>(Map.Count).ToBe(0);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(2)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(3)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestClearThenReuse;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Map.Clear;
    Map.Add(3, 'three');
    Expect<Integer>(Map.Count).ToBe(1);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Expect<Boolean>(Map.TryGetValue(3, V)).ToBe(True);
    Expect<string>(V).ToBe('three');
  finally
    Map.Free;
  end;
end;

{ ── Grow / rehash ────────────────────────────────────────────────── }

procedure THashMapTests.TestGrowOnLoadFactor;
var
  Map: THashMap<Integer, Integer>;
  I: Integer;
begin
  Map := THashMap<Integer, Integer>.Create(16);
  try
    { 16 * 0.70 = 11.2, so adding 12 entries forces a grow }
    for I := 0 to 11 do
      Map.Add(I, I * 10);
    Expect<Boolean>(Map.Capacity > 16).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestAllEntriesSurviveGrow;
var
  Map: THashMap<Integer, Integer>;
  I, V: Integer;
begin
  Map := THashMap<Integer, Integer>.Create(16);
  try
    for I := 0 to 49 do
      Map.Add(I, I * 3);
    Expect<Integer>(Map.Count).ToBe(50);
    for I := 0 to 49 do
    begin
      Expect<Boolean>(Map.TryGetValue(I, V)).ToBe(True);
      Expect<Integer>(V).ToBe(I * 3);
    end;
  finally
    Map.Free;
  end;
end;

{ ── Backshift deletion ───────────────────────────────────────────── }

procedure THashMapTests.TestBackshiftKeepsDisplacedNeighbors;
var
  Map: THashMap<Integer, Integer>;
  I, V: Integer;
begin
  Map := THashMap<Integer, Integer>.Create(16);
  try
    { Fill enough to create probe chains }
    for I := 0 to 9 do
      Map.Add(I, I * 100);
    { Remove entries from the middle of potential chains }
    Map.Remove(2);
    Map.Remove(4);
    Map.Remove(6);
    Expect<Integer>(Map.Count).ToBe(7);
    { All surviving entries must still be reachable }
    for I := 0 to 9 do
    begin
      if (I = 2) or (I = 4) or (I = 6) then
        Expect<Boolean>(Map.ContainsKey(I)).ToBe(False)
      else
      begin
        Expect<Boolean>(Map.TryGetValue(I, V)).ToBe(True);
        Expect<Integer>(V).ToBe(I * 100);
      end;
    end;
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestRemoveAndReaddSameKey;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'first');
    Map.Remove(1);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Map.Add(1, 'second');
    Expect<Integer>(Map.Count).ToBe(1);
    Map.TryGetValue(1, V);
    Expect<string>(V).ToBe('second');
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestRemoveFirstMiddleLast;
var
  Map: THashMap<Integer, Integer>;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(10, 100);
    Map.Add(20, 200);
    Map.Add(30, 300);
    Map.Add(40, 400);
    Map.Add(50, 500);
    { Remove first added }
    Expect<Boolean>(Map.Remove(10)).ToBe(True);
    { Remove a middle one }
    Expect<Boolean>(Map.Remove(30)).ToBe(True);
    { Remove last added }
    Expect<Boolean>(Map.Remove(50)).ToBe(True);
    Expect<Integer>(Map.Count).ToBe(2);
    Expect<Boolean>(Map.ContainsKey(20)).ToBe(True);
    Expect<Boolean>(Map.ContainsKey(40)).ToBe(True);
  finally
    Map.Free;
  end;
end;

{ ── Enumerator ───────────────────────────────────────────────────── }

procedure THashMapTests.TestEnumeratorVisitsExactlyCountEntries;
var
  Map: THashMap<Integer, Integer>;
  Pair: THashMap<Integer, Integer>.TKeyValuePair;
  Visited: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Map.Add(4, 40);
    Visited := 0;
    for Pair in Map do
      Inc(Visited);
    Expect<Integer>(Visited).ToBe(4);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestEnumeratorSumValues;
var
  Map: THashMap<Integer, Integer>;
  Pair: THashMap<Integer, Integer>.TKeyValuePair;
  Sum: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Sum := 0;
    for Pair in Map do
      Sum := Sum + Pair.Value;
    Expect<Integer>(Sum).ToBe(60);
  finally
    Map.Free;
  end;
end;

{ ── ToArray / Keys / Values ──────────────────────────────────────── }

procedure THashMapTests.TestToArrayLength;
var
  Map: THashMap<Integer, Integer>;
  Arr: TBaseMap<Integer, Integer>.TKeyValueArray;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Arr := Map.ToArray;
    Expect<Integer>(Length(Arr)).ToBe(3);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestKeysContainsAllKeys;
var
  Map: THashMap<Integer, Integer>;
  K: TBaseMap<Integer, Integer>.TKeyArray;
  Found1, Found2, Found3: Boolean;
  I: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(10, 100);
    Map.Add(20, 200);
    Map.Add(30, 300);
    K := Map.Keys;
    Expect<Integer>(Length(K)).ToBe(3);
    Found1 := False;
    Found2 := False;
    Found3 := False;
    for I := 0 to Length(K) - 1 do
    begin
      if K[I] = 10 then Found1 := True;
      if K[I] = 20 then Found2 := True;
      if K[I] = 30 then Found3 := True;
    end;
    Expect<Boolean>(Found1).ToBe(True);
    Expect<Boolean>(Found2).ToBe(True);
    Expect<Boolean>(Found3).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure THashMapTests.TestValuesContainsAllValues;
var
  Map: THashMap<Integer, Integer>;
  V: TBaseMap<Integer, Integer>.TValueArray;
  Sum: Integer;
  I: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    Map.Add(1, 100);
    Map.Add(2, 200);
    Map.Add(3, 300);
    V := Map.Values;
    Expect<Integer>(Length(V)).ToBe(3);
    Sum := 0;
    for I := 0 to Length(V) - 1 do
      Sum := Sum + V[I];
    Expect<Integer>(Sum).ToBe(600);
  finally
    Map.Free;
  end;
end;

{ ── ForEach ──────────────────────────────────────────────────────── }

var
  ForEachKeySum: Integer;
  ForEachValueSum: Integer;

type
  TForEachHelper = class
    procedure Callback(const AKey: Integer; const AValue: Integer);
  end;

procedure TForEachHelper.Callback(const AKey: Integer; const AValue: Integer);
begin
  ForEachKeySum := ForEachKeySum + AKey;
  ForEachValueSum := ForEachValueSum + AValue;
end;

procedure THashMapTests.TestForEachVisitsAllEntries;
var
  Map: THashMap<Integer, Integer>;
  Helper: TForEachHelper;
begin
  Map := THashMap<Integer, Integer>.Create;
  Helper := TForEachHelper.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    ForEachKeySum := 0;
    ForEachValueSum := 0;
    Map.ForEach(Helper.Callback);
    Expect<Integer>(ForEachKeySum).ToBe(6);
    Expect<Integer>(ForEachValueSum).ToBe(60);
  finally
    Helper.Free;
    Map.Free;
  end;
end;

{ ── QWord keys ───────────────────────────────────────────────────── }

procedure THashMapTests.TestQWordKeys;
var
  Map: THashMap<QWord, string>;
  V: string;
  K1, K2, K3: QWord;
begin
  Map := THashMap<QWord, string>.Create;
  try
    K1 := QWord(1) shl 40;
    K2 := QWord(1) shl 40 + 1;
    K3 := QWord(High(Cardinal)) + 1;
    Map.Add(K1, 'a');
    Map.Add(K2, 'b');
    Map.Add(K3, 'c');
    Expect<Integer>(Map.Count).ToBe(3);
    Expect<Boolean>(Map.TryGetValue(K1, V)).ToBe(True);
    Expect<string>(V).ToBe('a');
    Expect<Boolean>(Map.TryGetValue(K2, V)).ToBe(True);
    Expect<string>(V).ToBe('b');
    Expect<Boolean>(Map.TryGetValue(K3, V)).ToBe(True);
    Expect<string>(V).ToBe('c');
    Expect<Boolean>(Map.Remove(K2)).ToBe(True);
    Expect<Integer>(Map.Count).ToBe(2);
    Expect<Boolean>(Map.ContainsKey(K2)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(K1)).ToBe(True);
  finally
    Map.Free;
  end;
end;

{ ── Stress ───────────────────────────────────────────────────────── }

procedure THashMapTests.TestStressAddRemoveVerify;
var
  Map: THashMap<Integer, Integer>;
  I, V: Integer;
begin
  Map := THashMap<Integer, Integer>.Create;
  try
    { Add 200 keys }
    for I := 0 to 199 do
      Map.Add(I, I * 7);
    Expect<Integer>(Map.Count).ToBe(200);

    { Remove even keys (100 removals) }
    for I := 0 to 99 do
      Map.Remove(I * 2);
    Expect<Integer>(Map.Count).ToBe(100);

    { Verify remaining odd keys }
    for I := 0 to 99 do
    begin
      Expect<Boolean>(Map.ContainsKey(I * 2)).ToBe(False);
      Expect<Boolean>(Map.TryGetValue(I * 2 + 1, V)).ToBe(True);
      Expect<Integer>(V).ToBe((I * 2 + 1) * 7);
    end;

    { Re-add removed keys }
    for I := 0 to 99 do
      Map.Add(I * 2, I * 2 * 11);
    Expect<Integer>(Map.Count).ToBe(200);

    { Verify all 200 }
    for I := 0 to 199 do
      Expect<Boolean>(Map.ContainsKey(I)).ToBe(True);
  finally
    Map.Free;
  end;
end;

{ ── String values ────────────────────────────────────────────────── }

procedure THashMapTests.TestStringValues;
var
  Map: THashMap<Integer, string>;
  V: string;
begin
  Map := THashMap<Integer, string>.Create;
  try
    Map.Add(1, 'alpha');
    Map.Add(2, 'beta');
    Map.Add(3, 'gamma');
    Map.Add(2, 'BETA');
    Expect<Integer>(Map.Count).ToBe(3);
    Map.TryGetValue(2, V);
    Expect<string>(V).ToBe('BETA');
    Map.Remove(1);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Map.TryGetValue(3, V);
    Expect<string>(V).ToBe('gamma');
  finally
    Map.Free;
  end;
end;

{ ── Custom initial capacity ──────────────────────────────────────── }

procedure THashMapTests.TestCustomInitialCapacity;
var
  Map: THashMap<Integer, Integer>;
begin
  { Requesting 64 should give at least 64 }
  Map := THashMap<Integer, Integer>.Create(64);
  try
    Expect<Boolean>(Map.Capacity >= 64).ToBe(True);
    Map.Add(1, 10);
    Expect<Integer>(Map.Count).ToBe(1);
  finally
    Map.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(THashMapTests.Create('HashMap'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
