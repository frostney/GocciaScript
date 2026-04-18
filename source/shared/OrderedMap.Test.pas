program OrderedMap.Test;

{$I Shared.inc}

uses
  SysUtils,

  BaseMap,
  OrderedMap,
  TestingPascalLibrary;

type
  TOrderedMapTests = class(TTestSuite)
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

    { Insertion order }
    procedure TestInsertionOrderPreserved;
    procedure TestOverwritePreservesInsertionPosition;
    procedure TestRemoveAndReaddAppearsAtEnd;

    { EntryAt }
    procedure TestEntryAtReturnsCorrectPair;
    procedure TestEntryAtSkipsDeletedEntries;
    procedure TestEntryAtRaisesOnEmptyMap;
    procedure TestEntryAtRaisesOnPositiveOutOfRange;
    procedure TestEntryAtRaisesOnNegativeIndex;

    { Clear }
    procedure TestClearResetsCount;
    procedure TestClearThenReuse;

    { Grow / rehash }
    procedure TestGrowPreservesInsertionOrder;
    procedure TestAllEntriesSurviveGrow;

    { Compaction }
    procedure TestCompactTriggersAndPreservesOrder;

    { Enumerator }
    procedure TestEnumeratorFollowsInsertionOrder;
    procedure TestEnumeratorVisitsExactlyCountEntries;
    procedure TestEnumeratorAfterRemovals;

    { ToArray / Keys / Values }
    procedure TestToArrayInInsertionOrder;
    procedure TestKeysInInsertionOrder;
    procedure TestValuesInInsertionOrder;

    { ForEach }
    procedure TestForEachVisitsInInsertionOrder;

    { Stress }
    procedure TestStressAddRemoveVerify;

    { Remove all then re-add }
    procedure TestRemoveAllThenAdd;
  public
    procedure SetupTests; override;
  end;

procedure TOrderedMapTests.SetupTests;
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

  { Insertion order }
  Test('Insertion order is preserved', TestInsertionOrderPreserved);
  Test('Overwrite preserves insertion position', TestOverwritePreservesInsertionPosition);
  Test('Remove and re-add same key appears at end', TestRemoveAndReaddAppearsAtEnd);

  { EntryAt }
  Test('EntryAt returns correct pair by index', TestEntryAtReturnsCorrectPair);
  Test('EntryAt skips deleted entries correctly', TestEntryAtSkipsDeletedEntries);
  Test('EntryAt raises ERangeError on empty map', TestEntryAtRaisesOnEmptyMap);
  Test('EntryAt raises ERangeError on positive out-of-range', TestEntryAtRaisesOnPositiveOutOfRange);
  Test('EntryAt raises ERangeError on negative index', TestEntryAtRaisesOnNegativeIndex);

  { Clear }
  Test('Clear resets count to zero', TestClearResetsCount);
  Test('Clear then reuse works correctly', TestClearThenReuse);

  { Grow / rehash }
  Test('Grow preserves insertion order', TestGrowPreservesInsertionOrder);
  Test('All entries survive grow', TestAllEntriesSurviveGrow);

  { Compaction }
  Test('Compaction triggers and preserves order', TestCompactTriggersAndPreservesOrder);

  { Enumerator }
  Test('Enumerator follows insertion order', TestEnumeratorFollowsInsertionOrder);
  Test('Enumerator visits exactly Count entries', TestEnumeratorVisitsExactlyCountEntries);
  Test('Enumerator skips removed entries', TestEnumeratorAfterRemovals);

  { ToArray / Keys / Values }
  Test('ToArray returns entries in insertion order', TestToArrayInInsertionOrder);
  Test('Keys returns keys in insertion order', TestKeysInInsertionOrder);
  Test('Values returns values in insertion order', TestValuesInInsertionOrder);

  { ForEach }
  Test('ForEach visits entries in insertion order', TestForEachVisitsInInsertionOrder);

  { Stress }
  Test('Stress: add 100, remove 90, verify 10, add 10 more', TestStressAddRemoveVerify);

  { Remove all then re-add }
  Test('Remove all entries then add new ones', TestRemoveAllThenAdd);
end;

{ -- Empty map -------------------------------------------------------- }

procedure TOrderedMapTests.TestEmptyMapCountIsZero;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Expect<Integer>(Map.Count).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEmptyMapContainsKeyFalse;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(0)).ToBe(False);
    Expect<Boolean>(Map.ContainsKey(-1)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEmptyMapRemoveFalse;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Expect<Boolean>(Map.Remove(42)).ToBe(False);
    Expect<Integer>(Map.Count).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEmptyMapEnumeratorYieldsNothing;
var
  Map: TOrderedMap<Integer, Integer>;
  Pair: TOrderedMap<Integer, Integer>.TKeyValuePair;
  Visited: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Visited := 0;
    for Pair in Map do
      Inc(Visited);
    Expect<Integer>(Visited).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEmptyMapToArrayIsEmpty;
var
  Map: TOrderedMap<Integer, Integer>;
  Arr: TBaseMap<Integer, Integer>.TKeyValueArray;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Arr := Map.ToArray;
    Expect<Integer>(Length(Arr)).ToBe(0);
  finally
    Map.Free;
  end;
end;

{ -- Basic CRUD -------------------------------------------------------- }

procedure TOrderedMapTests.TestAddAndRetrieve;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Expect<Integer>(Map.Count).ToBe(1);
    Expect<Boolean>(Map.TryGetValue(1, V)).ToBe(True);
    Expect<string>(V).ToBe('one');
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestAddOverwritesExistingValue;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'first');
    Map.Add(1, 'second');
    Map.TryGetValue(1, V);
    Expect<string>(V).ToBe('second');
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestAddOverwritePreservesCount;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(5, 50);
    Map.Add(5, 99);
    Map.Add(5, 200);
    Expect<Integer>(Map.Count).ToBe(1);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestContainsKeyTrue;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(42, 100);
    Expect<Boolean>(Map.ContainsKey(42)).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestContainsKeyFalse;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Expect<Boolean>(Map.ContainsKey(99)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestTryGetValueFound;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(5, 'five');
    Expect<Boolean>(Map.TryGetValue(5, V)).ToBe(True);
    Expect<string>(V).ToBe('five');
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestTryGetValueNotFound;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Expect<Boolean>(Map.TryGetValue(999, V)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestRemoveExistingKey;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
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

procedure TOrderedMapTests.TestRemoveNonExistentKey;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Expect<Boolean>(Map.Remove(42)).ToBe(False);
    Expect<Integer>(Map.Count).ToBe(1);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestRemoveDecreasesCount;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
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

{ -- GetValue / Items[] ------------------------------------------------ }

procedure TOrderedMapTests.TestGetValueRaisesOnMissingKey;
var
  Map: TOrderedMap<Integer, Integer>;
  Raised: Boolean;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Raised := False;
    try
      Map[999];
    except
      on E: Exception do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestItemsPropertyReadWrite;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
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

{ -- AddOrSetValue ----------------------------------------------------- }

procedure TOrderedMapTests.TestAddOrSetValueNewKey;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.AddOrSetValue(7, 'seven');
    Expect<Integer>(Map.Count).ToBe(1);
    Map.TryGetValue(7, V);
    Expect<string>(V).ToBe('seven');
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestAddOrSetValueExistingKey;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
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

{ -- Insertion order --------------------------------------------------- }

procedure TOrderedMapTests.TestInsertionOrderPreserved;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(30, 'thirty');
    Map.Add(10, 'ten');
    Map.Add(20, 'twenty');
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(30);
    Expect<string>(Map.EntryAt(0).Value).ToBe('thirty');
    Expect<Integer>(Map.EntryAt(1).Key).ToBe(10);
    Expect<Integer>(Map.EntryAt(2).Key).ToBe(20);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestOverwritePreservesInsertionPosition;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Map.Add(3, 'three');
    { Overwrite key 2 -- it should stay at index 1 }
    Map.Add(2, 'TWO');
    Expect<Integer>(Map.Count).ToBe(3);
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(1);
    Expect<Integer>(Map.EntryAt(1).Key).ToBe(2);
    Expect<string>(Map.EntryAt(1).Value).ToBe('TWO');
    Expect<Integer>(Map.EntryAt(2).Key).ToBe(3);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestRemoveAndReaddAppearsAtEnd;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Map.Add(3, 'three');
    Map.Remove(2);
    Map.Add(2, 'two-again');
    { Key 2 was removed and re-added, so it appears at the end }
    Expect<Integer>(Map.Count).ToBe(3);
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(1);
    Expect<Integer>(Map.EntryAt(1).Key).ToBe(3);
    Expect<Integer>(Map.EntryAt(2).Key).ToBe(2);
    Expect<string>(Map.EntryAt(2).Value).ToBe('two-again');
  finally
    Map.Free;
  end;
end;

{ -- EntryAt ----------------------------------------------------------- }

procedure TOrderedMapTests.TestEntryAtReturnsCorrectPair;
var
  Map: TOrderedMap<Integer, string>;
  Pair: TBaseMap<Integer, string>.TKeyValuePair;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(10, 'ten');
    Map.Add(20, 'twenty');
    Map.Add(30, 'thirty');
    Pair := Map.EntryAt(1);
    Expect<Integer>(Pair.Key).ToBe(20);
    Expect<string>(Pair.Value).ToBe('twenty');
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEntryAtSkipsDeletedEntries;
var
  Map: TOrderedMap<Integer, string>;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Map.Add(3, 'three');
    Map.Add(4, 'four');
    Map.Add(5, 'five');
    Map.Remove(2);
    Map.Remove(4);
    { Active entries: 1, 3, 5 }
    Expect<Integer>(Map.Count).ToBe(3);
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(1);
    Expect<Integer>(Map.EntryAt(1).Key).ToBe(3);
    Expect<Integer>(Map.EntryAt(2).Key).ToBe(5);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEntryAtRaisesOnEmptyMap;
var
  Map: TOrderedMap<Integer, Integer>;
  Raised: Boolean;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Raised := False;
    try
      Map.EntryAt(0);
    except
      on E: ERangeError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEntryAtRaisesOnPositiveOutOfRange;
var
  Map: TOrderedMap<Integer, Integer>;
  Raised: Boolean;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Raised := False;
    try
      Map.EntryAt(5);
    except
      on E: ERangeError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEntryAtRaisesOnNegativeIndex;
var
  Map: TOrderedMap<Integer, Integer>;
  Raised: Boolean;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Raised := False;
    try
      Map.EntryAt(-1);
    except
      on E: ERangeError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Map.Free;
  end;
end;

{ -- Clear ------------------------------------------------------------- }

procedure TOrderedMapTests.TestClearResetsCount;
var
  Map: TOrderedMap<Integer, Integer>;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
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

procedure TOrderedMapTests.TestClearThenReuse;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Map.Clear;
    Map.Add(3, 'three');
    Map.Add(4, 'four');
    Expect<Integer>(Map.Count).ToBe(2);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);
    Expect<Boolean>(Map.TryGetValue(3, V)).ToBe(True);
    Expect<string>(V).ToBe('three');
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(3);
    Expect<Integer>(Map.EntryAt(1).Key).ToBe(4);
  finally
    Map.Free;
  end;
end;

{ -- Grow / rehash ----------------------------------------------------- }

procedure TOrderedMapTests.TestGrowPreservesInsertionOrder;
var
  Map: TOrderedMap<Integer, Integer>;
  I: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create(16);
  try
    for I := 0 to 29 do
      Map.Add(I, I * 10);
    Expect<Boolean>(Map.Capacity > 16).ToBe(True);
    { Verify insertion order survived the grow }
    for I := 0 to 29 do
    begin
      Expect<Integer>(Map.EntryAt(I).Key).ToBe(I);
      Expect<Integer>(Map.EntryAt(I).Value).ToBe(I * 10);
    end;
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestAllEntriesSurviveGrow;
var
  Map: TOrderedMap<Integer, Integer>;
  I, V: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create(16);
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

{ -- Compaction -------------------------------------------------------- }

procedure TOrderedMapTests.TestCompactTriggersAndPreservesOrder;
var
  Map: TOrderedMap<Integer, Integer>;
  I: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create(16);
  try
    { Add 10 entries, remove 9 so deleted > live, then trigger compact via Add }
    for I := 0 to 9 do
      Map.Add(I, I);
    for I := 0 to 8 do
      Map.Remove(I);
    Expect<Integer>(Map.Count).ToBe(1);

    { Adding enough entries should trigger compaction (deleted=9, live=1,
      then as we add more the load-factor check with compact path fires) }
    for I := 100 to 109 do
      Map.Add(I, I * 5);
    Expect<Integer>(Map.Count).ToBe(11);

    { Verify order: key 9 first, then 100..109 }
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(9);
    Expect<Integer>(Map.EntryAt(0).Value).ToBe(9);
    for I := 1 to 10 do
    begin
      Expect<Integer>(Map.EntryAt(I).Key).ToBe(99 + I);
      Expect<Integer>(Map.EntryAt(I).Value).ToBe((99 + I) * 5);
    end;
  finally
    Map.Free;
  end;
end;

{ -- Enumerator -------------------------------------------------------- }

procedure TOrderedMapTests.TestEnumeratorFollowsInsertionOrder;
var
  Map: TOrderedMap<Integer, Integer>;
  Pair: TOrderedMap<Integer, Integer>.TKeyValuePair;
  Expected: array[0..2] of Integer;
  Idx: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(30, 300);
    Map.Add(10, 100);
    Map.Add(20, 200);
    Expected[0] := 30;
    Expected[1] := 10;
    Expected[2] := 20;
    Idx := 0;
    for Pair in Map do
    begin
      Expect<Integer>(Pair.Key).ToBe(Expected[Idx]);
      Inc(Idx);
    end;
    Expect<Integer>(Idx).ToBe(3);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEnumeratorVisitsExactlyCountEntries;
var
  Map: TOrderedMap<Integer, Integer>;
  Pair: TOrderedMap<Integer, Integer>.TKeyValuePair;
  Visited: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Map.Add(4, 40);
    Map.Add(5, 50);
    Visited := 0;
    for Pair in Map do
      Inc(Visited);
    Expect<Integer>(Visited).ToBe(5);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestEnumeratorAfterRemovals;
var
  Map: TOrderedMap<Integer, Integer>;
  Pair: TOrderedMap<Integer, Integer>.TKeyValuePair;
  Expected: array[0..1] of Integer;
  Idx: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(1, 10);
    Map.Add(2, 20);
    Map.Add(3, 30);
    Map.Remove(2);
    Expected[0] := 1;
    Expected[1] := 3;
    Idx := 0;
    for Pair in Map do
    begin
      Expect<Integer>(Pair.Key).ToBe(Expected[Idx]);
      Inc(Idx);
    end;
    Expect<Integer>(Idx).ToBe(2);
  finally
    Map.Free;
  end;
end;

{ -- ToArray / Keys / Values ------------------------------------------- }

procedure TOrderedMapTests.TestToArrayInInsertionOrder;
var
  Map: TOrderedMap<Integer, string>;
  Arr: TBaseMap<Integer, string>.TKeyValueArray;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(30, 'thirty');
    Map.Add(10, 'ten');
    Map.Add(20, 'twenty');
    Arr := Map.ToArray;
    Expect<Integer>(Length(Arr)).ToBe(3);
    Expect<Integer>(Arr[0].Key).ToBe(30);
    Expect<string>(Arr[0].Value).ToBe('thirty');
    Expect<Integer>(Arr[1].Key).ToBe(10);
    Expect<Integer>(Arr[2].Key).ToBe(20);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestKeysInInsertionOrder;
var
  Map: TOrderedMap<Integer, Integer>;
  K: TBaseMap<Integer, Integer>.TKeyArray;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(50, 500);
    Map.Add(10, 100);
    Map.Add(30, 300);
    K := Map.Keys;
    Expect<Integer>(Length(K)).ToBe(3);
    Expect<Integer>(K[0]).ToBe(50);
    Expect<Integer>(K[1]).ToBe(10);
    Expect<Integer>(K[2]).ToBe(30);
  finally
    Map.Free;
  end;
end;

procedure TOrderedMapTests.TestValuesInInsertionOrder;
var
  Map: TOrderedMap<Integer, Integer>;
  V: TBaseMap<Integer, Integer>.TValueArray;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    Map.Add(50, 500);
    Map.Add(10, 100);
    Map.Add(30, 300);
    V := Map.Values;
    Expect<Integer>(Length(V)).ToBe(3);
    Expect<Integer>(V[0]).ToBe(500);
    Expect<Integer>(V[1]).ToBe(100);
    Expect<Integer>(V[2]).ToBe(300);
  finally
    Map.Free;
  end;
end;

{ -- ForEach ----------------------------------------------------------- }

var
  ForEachKeys: array of Integer;
  ForEachIdx: Integer;

type
  TForEachHelper = class
    procedure Callback(const AKey: Integer; const AValue: Integer);
  end;

procedure TForEachHelper.Callback(const AKey: Integer; const AValue: Integer);
begin
  if ForEachIdx < Length(ForEachKeys) then
    ForEachKeys[ForEachIdx] := AKey;
  Inc(ForEachIdx);
end;

procedure TOrderedMapTests.TestForEachVisitsInInsertionOrder;
var
  Map: TOrderedMap<Integer, Integer>;
  Helper: TForEachHelper;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  Helper := TForEachHelper.Create;
  try
    Map.Add(30, 300);
    Map.Add(10, 100);
    Map.Add(20, 200);
    SetLength(ForEachKeys, 3);
    ForEachIdx := 0;
    Map.ForEach(Helper.Callback);
    Expect<Integer>(ForEachIdx).ToBe(3);
    Expect<Integer>(ForEachKeys[0]).ToBe(30);
    Expect<Integer>(ForEachKeys[1]).ToBe(10);
    Expect<Integer>(ForEachKeys[2]).ToBe(20);
  finally
    Helper.Free;
    Map.Free;
  end;
end;

{ -- Stress ------------------------------------------------------------ }

procedure TOrderedMapTests.TestStressAddRemoveVerify;
var
  Map: TOrderedMap<Integer, Integer>;
  I, V: Integer;
begin
  Map := TOrderedMap<Integer, Integer>.Create;
  try
    { Add 100 keys }
    for I := 0 to 99 do
      Map.Add(I, I * 7);
    Expect<Integer>(Map.Count).ToBe(100);

    { Remove first 90 }
    for I := 0 to 89 do
      Map.Remove(I);
    Expect<Integer>(Map.Count).ToBe(10);

    { Verify remaining 10 (keys 90..99) in insertion order }
    for I := 0 to 9 do
    begin
      Expect<Integer>(Map.EntryAt(I).Key).ToBe(90 + I);
      Expect<Boolean>(Map.TryGetValue(90 + I, V)).ToBe(True);
      Expect<Integer>(V).ToBe((90 + I) * 7);
    end;

    { Add 10 more }
    for I := 200 to 209 do
      Map.Add(I, I * 11);
    Expect<Integer>(Map.Count).ToBe(20);

    { Verify all 20 entries exist and order is correct }
    for I := 0 to 9 do
      Expect<Integer>(Map.EntryAt(I).Key).ToBe(90 + I);
    for I := 0 to 9 do
      Expect<Integer>(Map.EntryAt(10 + I).Key).ToBe(200 + I);
  finally
    Map.Free;
  end;
end;

{ -- Remove all then re-add -------------------------------------------- }

procedure TOrderedMapTests.TestRemoveAllThenAdd;
var
  Map: TOrderedMap<Integer, string>;
  V: string;
begin
  Map := TOrderedMap<Integer, string>.Create;
  try
    Map.Add(1, 'one');
    Map.Add(2, 'two');
    Map.Add(3, 'three');
    Map.Remove(1);
    Map.Remove(2);
    Map.Remove(3);
    Expect<Integer>(Map.Count).ToBe(0);
    Expect<Boolean>(Map.ContainsKey(1)).ToBe(False);

    Map.Add(10, 'ten');
    Map.Add(20, 'twenty');
    Expect<Integer>(Map.Count).ToBe(2);
    Expect<Boolean>(Map.TryGetValue(10, V)).ToBe(True);
    Expect<string>(V).ToBe('ten');
    Expect<Integer>(Map.EntryAt(0).Key).ToBe(10);
    Expect<Integer>(Map.EntryAt(1).Key).ToBe(20);
  finally
    Map.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TOrderedMapTests.Create('OrderedMap'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
