program Goccia.Values.OrderedValueMap.Test;

{$I Goccia.inc}

uses
  Math,
  SysUtils,

  BigInteger,
  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.Values.BigIntValue,
  Goccia.Values.OrderedValueMap,
  Goccia.Values.Primitives;

type
  TOrderedValueMapTests = class(TTestSuite)
  private
    { Hash/equality consistency: SameValueZero-equal keys (distinct instances)
      must collapse to one entry. A hash inconsistent with the equality would
      route them to different buckets and silently keep both. }
    procedure TestNaNKeysCollapse;
    procedure TestSignedZeroKeysCollapse;
    procedure TestEqualNumberInstancesCollapse;
    procedure TestEqualStringInstancesCollapse;
    procedure TestEquivalentUTF16StringEncodingsCollapse;
    procedure TestEqualBigIntInstancesCollapse;
    procedure TestLookupWithDistinctEqualInstance;

    { No false collapse: SameValueZero-distinct keys stay separate. }
    procedure TestNumberAndStringAreDistinct;
    procedure TestDistinctNumbersAreDistinct;
    procedure TestBooleanAndNumberAreDistinct;
    procedure TestNullAndUndefinedAreDistinct;

    { -0 canonicalization (ES2026 §24.5.1). }
    procedure TestNegativeZeroStoredAsPositiveZero;
    procedure TestCanonicalizeKeyMapsNegativeZero;
    procedure TestAddSetMemberStoresCanonicalKeyAndValue;

    { Insertion-ordered cursor with tombstone deletes. }
    procedure TestCursorVisitsInInsertionOrder;
    procedure TestCursorSkipsTombstonesAndReaddAppends;

    { Bounded iteration for spec-bounded Set operations. }
    procedure TestNextEntryBoundedSkipsTombstonesAndExcludesAppends;

    { Compaction gating for live iterators. }
    procedure TestIteratorRetainReleaseCounter;
  public
    procedure SetupTests; override;
  end;

procedure TOrderedValueMapTests.SetupTests;
begin
  Test('NaN keys collapse to one entry', TestNaNKeysCollapse);
  Test('-0 and +0 collapse to one entry', TestSignedZeroKeysCollapse);
  Test('Equal number instances collapse', TestEqualNumberInstancesCollapse);
  Test('Equal string instances collapse', TestEqualStringInstancesCollapse);
  Test('Equivalent UTF-16 string encodings collapse',
    TestEquivalentUTF16StringEncodingsCollapse);
  Test('Equal BigInt instances collapse', TestEqualBigIntInstancesCollapse);
  Test('Lookup succeeds with a distinct equal instance', TestLookupWithDistinctEqualInstance);
  Test('Number 1 and string "1" are distinct keys', TestNumberAndStringAreDistinct);
  Test('Distinct numbers are distinct keys', TestDistinctNumbersAreDistinct);
  Test('Boolean and number are distinct keys', TestBooleanAndNumberAreDistinct);
  Test('null and undefined are distinct keys', TestNullAndUndefinedAreDistinct);
  Test('-0 is stored as +0', TestNegativeZeroStoredAsPositiveZero);
  Test('CanonicalizeKey maps -0 to +0', TestCanonicalizeKeyMapsNegativeZero);
  Test('AddSetMember stores the canonical element as key and value', TestAddSetMemberStoresCanonicalKeyAndValue);
  Test('Cursor visits entries in insertion order', TestCursorVisitsInInsertionOrder);
  Test('Cursor skips tombstones; re-add appends at end', TestCursorSkipsTombstonesAndReaddAppends);
  Test('NextEntryBounded skips tombstones and excludes appends past the limit', TestNextEntryBoundedSkipsTombstonesAndExcludesAppends);
  Test('RetainIterator/ReleaseIterator track active count', TestIteratorRetainReleaseCounter);
end;

function Num(const AValue: Double): TGocciaValue; inline;
begin
  Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function Str(const AValue: string): TGocciaValue; inline;
begin
  Result := TGocciaStringLiteralValue.Create(AValue);
end;

procedure TOrderedValueMapTests.TestNaNKeysCollapse;
var
  Store: TGocciaOrderedValueMap;
  V: TGocciaValue;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    // Two distinct NaN instances.
    Store.SetEntry(Num(Math.NaN), Str('first'));
    Store.SetEntry(Num(Math.NaN), Str('second'));
    Expect<Integer>(Store.Count).ToBe(1);
    Expect<Boolean>(Store.TryGetValue(Num(Math.NaN), V)).ToBe(True);
    Expect<string>(TGocciaStringLiteralValue(V).Value).ToBe('second');
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestSignedZeroKeysCollapse;
var
  Store: TGocciaOrderedValueMap;
  V: TGocciaValue;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(TGocciaNumberLiteralValue.NegativeZeroValue, Str('neg'));
    Store.SetEntry(TGocciaNumberLiteralValue.ZeroValue, Str('pos'));
    Expect<Integer>(Store.Count).ToBe(1);
    Expect<Boolean>(Store.TryGetValue(Num(0), V)).ToBe(True);
    Expect<string>(TGocciaStringLiteralValue(V).Value).ToBe('pos');
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestEqualNumberInstancesCollapse;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Num(42), Str('a'));
    Store.SetEntry(Num(42), Str('b'));
    Expect<Integer>(Store.Count).ToBe(1);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestEqualStringInstancesCollapse;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    // Distinct string instances with equal content (no string interning):
    // proves content hashing, not reference identity.
    Store.SetEntry(Str('ab'), Str('a'));
    Store.SetEntry(Str('a' + 'b'), Str('b'));
    Expect<Integer>(Store.Count).ToBe(1);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestEquivalentUTF16StringEncodingsCollapse;
const
  UTF8_GRINNING_FACE = #$F0#$9F#$98#$80;
  UTF8_SURROGATE_PAIR = #$ED#$A0#$BD#$ED#$B8#$80;
var
  Store: TGocciaOrderedValueMap;
  V: TGocciaValue;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Str(UTF8_GRINNING_FACE), Str('first'));
    Store.SetEntry(Str(UTF8_SURROGATE_PAIR), Str('second'));
    Expect<Integer>(Store.Count).ToBe(1);
    Expect<Boolean>(Store.TryGetValue(Str(UTF8_GRINNING_FACE), V)).ToBe(True);
    Expect<string>(TGocciaStringLiteralValue(V).Value).ToBe('second');
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestEqualBigIntInstancesCollapse;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(TGocciaBigIntValue.Create(TBigInteger.FromInt64(123)), Str('a'));
    Store.SetEntry(TGocciaBigIntValue.Create(TBigInteger.FromInt64(123)), Str('b'));
    Expect<Integer>(Store.Count).ToBe(1);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestLookupWithDistinctEqualInstance;
var
  Store: TGocciaOrderedValueMap;
  V: TGocciaValue;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Str('key'), Str('value'));
    Expect<Boolean>(Store.TryGetValue(Str('key'), V)).ToBe(True);
    Expect<string>(TGocciaStringLiteralValue(V).Value).ToBe('value');
    Expect<Boolean>(Store.ContainsKey(Str('absent'))).ToBe(False);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestNumberAndStringAreDistinct;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Num(1), Str('num'));
    Store.SetEntry(Str('1'), Str('str'));
    Expect<Integer>(Store.Count).ToBe(2);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestDistinctNumbersAreDistinct;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Num(1), Str('one'));
    Store.SetEntry(Num(2), Str('two'));
    Expect<Integer>(Store.Count).ToBe(2);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestBooleanAndNumberAreDistinct;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(TGocciaBooleanLiteralValue.TrueValue, Str('bool'));
    Store.SetEntry(Num(1), Str('num'));
    Expect<Integer>(Store.Count).ToBe(2);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestNullAndUndefinedAreDistinct;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(TGocciaNullLiteralValue.NullValue, Str('null'));
    Store.SetEntry(TGocciaUndefinedLiteralValue.UndefinedValue, Str('undef'));
    Expect<Integer>(Store.Count).ToBe(2);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestNegativeZeroStoredAsPositiveZero;
var
  Store: TGocciaOrderedValueMap;
  Cursor: Integer;
  Key, Value: TGocciaValue;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(TGocciaNumberLiteralValue.NegativeZeroValue, Str('z'));
    Cursor := 0;
    Expect<Boolean>(Store.NextEntry(Cursor, Key, Value)).ToBe(True);
    Expect<Boolean>(TGocciaNumberLiteralValue(Key).IsNegativeZero).ToBe(False);
    Expect<Double>(TGocciaNumberLiteralValue(Key).Value).ToBe(0);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestCanonicalizeKeyMapsNegativeZero;
var
  Canonical: TGocciaValue;
begin
  Canonical := TGocciaOrderedValueMap.CanonicalizeKey(
    TGocciaNumberLiteralValue.NegativeZeroValue);
  Expect<Boolean>(TGocciaNumberLiteralValue(Canonical).IsNegativeZero).ToBe(False);
  // A non -0 key is returned unchanged.
  Expect<Boolean>(TGocciaOrderedValueMap.CanonicalizeKey(Num(5)) = nil).ToBe(False);
end;

procedure TOrderedValueMapTests.TestAddSetMemberStoresCanonicalKeyAndValue;
var
  Store: TGocciaOrderedValueMap;
  Cursor: Integer;
  Key, Value: TGocciaValue;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.AddSetMember(TGocciaNumberLiteralValue.NegativeZeroValue);
    Expect<Integer>(Store.Count).ToBe(1);
    Cursor := 0;
    Expect<Boolean>(Store.NextEntry(Cursor, Key, Value)).ToBe(True);
    // Both the key and the value slot hold the canonical +0 (not -0).
    Expect<Boolean>(TGocciaNumberLiteralValue(Key).IsNegativeZero).ToBe(False);
    Expect<Boolean>(TGocciaNumberLiteralValue(Value).IsNegativeZero).ToBe(False);
    Expect<Boolean>(Key = Value).ToBe(True);
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestCursorVisitsInInsertionOrder;
var
  Store: TGocciaOrderedValueMap;
  Cursor: Integer;
  Key, Value: TGocciaValue;
  Order: string;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Str('a'), Num(1));
    Store.SetEntry(Str('b'), Num(2));
    Store.SetEntry(Str('c'), Num(3));
    // Updating b keeps its position.
    Store.SetEntry(Str('b'), Num(99));
    Order := '';
    Cursor := 0;
    while Store.NextEntry(Cursor, Key, Value) do
      Order := Order + TGocciaStringLiteralValue(Key).Value;
    Expect<string>(Order).ToBe('abc');
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestCursorSkipsTombstonesAndReaddAppends;
var
  Store: TGocciaOrderedValueMap;
  Cursor: Integer;
  Key, Value: TGocciaValue;
  Order: string;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Str('a'), Num(1));
    Store.SetEntry(Str('b'), Num(2));
    Store.SetEntry(Str('c'), Num(3));
    Expect<Boolean>(Store.Remove(Str('b'))).ToBe(True);
    Expect<Integer>(Store.Count).ToBe(2);

    Order := '';
    Cursor := 0;
    while Store.NextEntry(Cursor, Key, Value) do
      Order := Order + TGocciaStringLiteralValue(Key).Value;
    Expect<string>(Order).ToBe('ac');

    // Re-adding a removed key appends it at the end.
    Store.SetEntry(Str('b'), Num(4));
    Order := '';
    Cursor := 0;
    while Store.NextEntry(Cursor, Key, Value) do
      Order := Order + TGocciaStringLiteralValue(Key).Value;
    Expect<string>(Order).ToBe('acb');
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestNextEntryBoundedSkipsTombstonesAndExcludesAppends;
var
  Store: TGocciaOrderedValueMap;
  Cursor, Limit: Integer;
  Key, Value: TGocciaValue;
  Order: string;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Store.SetEntry(Str('a'), Num(1));
    Store.SetEntry(Str('b'), Num(2));
    Store.SetEntry(Str('c'), Num(3));
    Limit := Store.EntrySlotCount;
    Expect<Integer>(Limit).ToBe(3);
    // Tombstone a slot below the limit and append one past it.
    Expect<Boolean>(Store.Remove(Str('b'))).ToBe(True);
    Store.SetEntry(Str('d'), Num(4));
    Order := '';
    Cursor := 0;
    while Store.NextEntryBounded(Cursor, Limit, Key, Value) do
      Order := Order + TGocciaStringLiteralValue(Key).Value;
    // 'b' skipped (tombstone), 'd' excluded (appended at a slot >= Limit).
    Expect<string>(Order).ToBe('ac');
  finally
    Store.Free;
  end;
end;

procedure TOrderedValueMapTests.TestIteratorRetainReleaseCounter;
var
  Store: TGocciaOrderedValueMap;
begin
  Store := TGocciaOrderedValueMap.Create;
  try
    Expect<Integer>(Store.ActiveIterators).ToBe(0);
    Store.RetainIterator;
    Store.RetainIterator;
    Expect<Integer>(Store.ActiveIterators).ToBe(2);
    Store.ReleaseIterator;
    Expect<Integer>(Store.ActiveIterators).ToBe(1);
    Store.ReleaseIterator;
    Expect<Integer>(Store.ActiveIterators).ToBe(0);
    // Release never drives the counter negative.
    Store.ReleaseIterator;
    Expect<Integer>(Store.ActiveIterators).ToBe(0);
  finally
    Store.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TOrderedValueMapTests.Create('OrderedValueMap'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
