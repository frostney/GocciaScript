program Goccia.OrderedStringMap.Test;

{$I Goccia.inc}

uses
  SysUtils,

  OrderedStringMap,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  TTestOrderedStringMap = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestReuseDeletedBucketClearsDeletedCount;
    procedure TestDeleteHeavyInsertCompactsTombstones;
    procedure TestEntryIndexSurvivesAddsAndSeesUpdates;
    procedure TestEntryVersionInvalidatesOnRemoveCompactClear;
    procedure TestEntryVersionsDifferAcrossInstances;
  end;

procedure TTestOrderedStringMap.SetupTests;
begin
  Test('Reuse deleted bucket clears deleted count',
    TestReuseDeletedBucketClearsDeletedCount);
  Test('Delete-heavy insert compacts tombstones',
    TestDeleteHeavyInsertCompactsTombstones);
  Test('Entry index survives adds and sees value updates',
    TestEntryIndexSurvivesAddsAndSeesUpdates);
  Test('Entry version invalidates on remove, compact, and clear',
    TestEntryVersionInvalidatesOnRemoveCompactClear);
  Test('Entry versions differ across instances',
    TestEntryVersionsDifferAcrossInstances);
end;

procedure TTestOrderedStringMap.TestReuseDeletedBucketClearsDeletedCount;
var
  Map: TOrderedStringMap<Integer>;
begin
  Map := TOrderedStringMap<Integer>.Create(16);
  try
    Map.Add('a', 1);
    Map.Add('b', 2);

    Expect<Boolean>(Map.Remove('a')).ToBe(True);
    Expect<Integer>(Map.DeletedCount).ToBe(1);

    // "q" hashes to the same initial bucket as "a" in a 16-slot table.
    Map.Add('q', 3);

    Expect<Integer>(Map.DeletedCount).ToBe(0);
    Expect<Integer>(Map.Count).ToBe(2);
    Expect<Boolean>(Map.ContainsKey('b')).ToBe(True);
    Expect<Boolean>(Map.ContainsKey('q')).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestDeleteHeavyInsertCompactsTombstones;
var
  Map: TOrderedStringMap<Integer>;
  I: Integer;
begin
  Map := TOrderedStringMap<Integer>.Create(16);
  try
    for I := 0 to 9 do
      Map.Add('key' + IntToStr(I), I);

    for I := 0 to 8 do
      Expect<Boolean>(Map.Remove('key' + IntToStr(I))).ToBe(True);

    Expect<Integer>(Map.Count).ToBe(1);
    Expect<Integer>(Map.DeletedCount).ToBe(9);

    Map.Add('new-key', 99);

    Expect<Integer>(Map.Capacity).ToBe(16);
    Expect<Integer>(Map.Count).ToBe(2);
    Expect<Integer>(Map.DeletedCount).ToBe(0);
    Expect<string>(Map.EntryAt(0).Key).ToBe('key9');
    Expect<string>(Map.EntryAt(1).Key).ToBe('new-key');
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestEntryIndexSurvivesAddsAndSeesUpdates;
var
  Map: TOrderedStringMap<Integer>;
  Index: Integer;
  Version: Cardinal;
  Value, I: Integer;
begin
  Map := TOrderedStringMap<Integer>.Create(16);
  try
    Map.Add('cached', 1);
    Expect<Boolean>(Map.TryGetEntryIndex('cached', Index)).ToBe(True);
    Version := Map.EntryVersion;
    Expect<string>(Map.KeyAtEntry(Index)).ToBe('cached');

    // Adds (including bucket growth) never invalidate an entry index.
    for I := 0 to 99 do
      Map.Add('filler' + IntToStr(I), I);
    Expect<Boolean>(Map.EntryVersion = Version).ToBe(True);
    Expect<Boolean>(Map.TryGetValueAtEntry(Index, Value)).ToBe(True);
    Expect<Integer>(Value).ToBe(1);

    // Value updates through the same key are visible at the cached index.
    Map.Add('cached', 2);
    Expect<Boolean>(Map.TryGetValueAtEntry(Index, Value)).ToBe(True);
    Expect<Integer>(Value).ToBe(2);
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestEntryVersionInvalidatesOnRemoveCompactClear;
var
  Map: TOrderedStringMap<Integer>;
  Version: Cardinal;
  I: Integer;
begin
  Map := TOrderedStringMap<Integer>.Create(16);
  try
    Map.Add('a', 1);
    Map.Add('b', 2);

    Version := Map.EntryVersion;
    Expect<Boolean>(Map.Remove('a')).ToBe(True);
    Expect<Boolean>(Map.EntryVersion <> Version).ToBe(True);

    // Delete-heavy insert triggers Compact, which shifts entry indices.
    for I := 0 to 9 do
      Map.Add('key' + IntToStr(I), I);
    for I := 0 to 8 do
      Map.Remove('key' + IntToStr(I));
    Version := Map.EntryVersion;
    Map.Add('compact-trigger', 99);
    Expect<Boolean>(Map.EntryVersion <> Version).ToBe(True);

    Version := Map.EntryVersion;
    Map.Clear;
    Expect<Boolean>(Map.EntryVersion <> Version).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestEntryVersionsDifferAcrossInstances;
var
  First, Second: TOrderedStringMap<Integer>;
begin
  // A version stamp recorded against one instance must never validate
  // against another instance (e.g. after free + reallocation at the same
  // address), so every instance starts from a fresh stamp.
  First := TOrderedStringMap<Integer>.Create(16);
  try
    Second := TOrderedStringMap<Integer>.Create(16);
    try
      Expect<Boolean>(First.EntryVersion <> Second.EntryVersion).ToBe(True);
    finally
      Second.Free;
    end;
  finally
    First.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestOrderedStringMap.Create('OrderedStringMap'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
