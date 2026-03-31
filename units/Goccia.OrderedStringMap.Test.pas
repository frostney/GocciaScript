program Goccia.OrderedStringMap.Test;

{$I Goccia.inc}

uses
  SysUtils,

  OrderedStringMap,
  TestRunner,

  Goccia.TestSetup;

type
  TTestOrderedStringMap = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestReuseDeletedBucketClearsDeletedCount;
    procedure TestDeleteHeavyInsertCompactsTombstones;
  end;

procedure TTestOrderedStringMap.SetupTests;
begin
  Test('Reuse deleted bucket clears deleted count',
    TestReuseDeletedBucketClearsDeletedCount);
  Test('Delete-heavy insert compacts tombstones',
    TestDeleteHeavyInsertCompactsTombstones);
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

begin
  TestRunnerProgram.AddSuite(TTestOrderedStringMap.Create('OrderedStringMap'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
