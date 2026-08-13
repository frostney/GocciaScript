program Goccia.OrderedStringMap.Test;

{$I Goccia.inc}

uses
  SysUtils,

  OrderedStringMap,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  { Refusal raised by the instrumented gate below. A distinct class so a test
    cannot mistake an unrelated failure inside Add for a budget refusal. }
  EStorageRefused = class(Exception);

  { Instrumented stand-in for the real gated subclass (TGocciaShapedPropertyMap,
    which forwards to the memory budget). Records every call so a test can pin
    *when* the gate is consulted, and refuses on demand so a test can pin what
    a refusal mid-Add leaves behind. }
  TGatedStringMap = class(TOrderedStringMap<Integer>)
  private
    FArmed: Boolean;
    FCalls: Integer;
    FLastBytes: Int64;
    FCountAtFirstCall: Integer;
  protected
    procedure RequireStorageBytes(const ABytes: Int64); override;
  public
    constructor Create(AInitialCapacity: Integer);
    property Armed: Boolean read FArmed write FArmed;
    property Calls: Integer read FCalls;
    property LastBytes: Int64 read FLastBytes;
    property CountAtFirstCall: Integer read FCountAtFirstCall;
  end;

  TTestOrderedStringMap = class(TTestSuite)
  private
    { Adds key0.. until the armed gate refuses one, and returns the number of
      keys that were accepted. Fails the test if nothing was refused within
      ALimit, so a gate that stopped firing shows up as a failure rather than
      as a test that silently asserts nothing. }
    function AddUntilRefused(const AMap: TGatedStringMap;
      const ALimit: Integer): Integer;
  public
    procedure SetupTests; override;

    procedure TestReuseDeletedBucketClearsDeletedCount;
    procedure TestDeleteHeavyInsertCompactsTombstones;
    procedure TestEntryIndexSurvivesAddsAndSeesUpdates;
    procedure TestEntryVersionInvalidatesOnRemoveCompactClear;
    procedure TestEntryVersionsDifferAcrossInstances;
    procedure TestSmallMapNeverConsultsTheGate;
    procedure TestGrowingMapConsultsTheGateWithTheTransientSize;
    procedure TestRefusedGrowthLeavesTheMapUnchanged;
    procedure TestMapStaysUsableAfterARefusal;
    procedure TestRefusedCompactionLeavesTheMapUnchanged;
  end;

constructor TGatedStringMap.Create(AInitialCapacity: Integer);
begin
  inherited Create(AInitialCapacity);
  FArmed := False;
  FCalls := 0;
  FLastBytes := 0;
  FCountAtFirstCall := -1;
end;

procedure TGatedStringMap.RequireStorageBytes(const ABytes: Int64);
begin
  if FCalls = 0 then
    FCountAtFirstCall := Count;
  Inc(FCalls);
  FLastBytes := ABytes;
  if FArmed then
    raise EStorageRefused.CreateFmt('refused %d bytes', [ABytes]);
end;

function TTestOrderedStringMap.AddUntilRefused(const AMap: TGatedStringMap;
  const ALimit: Integer): Integer;
var
  I: Integer;
begin
  AMap.Armed := True;
  for I := 0 to ALimit - 1 do
  begin
    try
      AMap.Add('key' + IntToStr(I), I);
    except
      on EStorageRefused do
        Exit(I);
    end;
  end;
  Fail(Format('The gate refused nothing within %d adds', [ALimit]));
  Result := ALimit;
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
  Test('Small map never consults the storage gate',
    TestSmallMapNeverConsultsTheGate);
  Test('Growing map consults the storage gate with the transient size',
    TestGrowingMapConsultsTheGateWithTheTransientSize);
  Test('Refused growth leaves the map unchanged',
    TestRefusedGrowthLeavesTheMapUnchanged);
  Test('Map stays usable after a refusal',
    TestMapStaysUsableAfterARefusal);
  Test('Refused compaction leaves the map unchanged',
    TestRefusedCompactionLeavesTheMapUnchanged);
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

procedure TTestOrderedStringMap.TestSmallMapNeverConsultsTheGate;
var
  Map: TGatedStringMap;
  I: Integer;
begin
  Map := TGatedStringMap.Create(16);
  try
    // 32 entries outgrows the initial bucket array twice over, so this covers
    // bucket rehashes as well as several entry-array growths.
    for I := 0 to 31 do
      Map.Add('key' + IntToStr(I), I);
    Expect<Integer>(Map.Count).ToBe(32);

    // The gate's documented blind spot: a map this small is never checked at
    // all. ADR 0106 records what that costs. Pinning it here means a change to
    // GATED_GROWTH_MIN_BYTES or to SizeOf(TEntry) cannot move the boundary out
    // from under the documentation without a test going red.
    Expect<Integer>(Map.Calls).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestGrowingMapConsultsTheGateWithTheTransientSize;
var
  Map: TGatedStringMap;
  I, CallsBefore, CapacityBefore, Steps: Integer;
begin
  // A bucket array large enough that no rehash happens over this range, so
  // every gate call below is an entry-array growth and its size is derivable
  // from the entry count alone.
  Map := TGatedStringMap.Create(4096);
  try
    Steps := 0;
    for I := 0 to 599 do
    begin
      CallsBefore := Map.Calls;
      // Nothing is ever removed here, so the live count equals the entry
      // array's capacity exactly when the next Add has to grow it.
      CapacityBefore := Map.Count;
      Map.Add('key' + IntToStr(I), I);
      if Map.Calls > CallsBefore then
      begin
        // Old block plus new block. An implementation that reported only the
        // new block would understate the peak by a third, and that is the
        // whole difference this pins.
        Expect<Int64>(Map.LastBytes).ToBe(
          (Int64(CapacityBefore) + (Int64(CapacityBefore) + 1) * 2) *
          SizeOf(TGatedStringMap.TEntry));
        Inc(Steps);
      end;
    end;

    Expect<Boolean>(Steps > 0).ToBe(True);
    // ...and it is only consulted once the map has left the small-map band.
    Expect<Boolean>(Map.CountAtFirstCall > 32).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestRefusedGrowthLeavesTheMapUnchanged;
var
  Map: TGatedStringMap;
  Accepted, I, Value: Integer;
begin
  Map := TGatedStringMap.Create(16);
  try
    Accepted := AddUntilRefused(Map, 5000);
    Expect<Boolean>(Accepted > 0).ToBe(True);

    // The refusal aborted an Add part-way through. None of it may have landed:
    // no slot claimed, no count moved, no half-written entry.
    Expect<Integer>(Map.Count).ToBe(Accepted);
    Expect<Boolean>(Map.ContainsKey('key' + IntToStr(Accepted))).ToBe(False);

    // Every earlier key still reads back its own value, through the hash
    // lookup and through the insertion-ordered entry list alike.
    for I := 0 to Accepted - 1 do
    begin
      Expect<Boolean>(Map.TryGetValue('key' + IntToStr(I), Value)).ToBe(True);
      Expect<Integer>(Value).ToBe(I);
    end;
    Expect<string>(Map.EntryAt(0).Key).ToBe('key0');
    Expect<string>(Map.EntryAt(Accepted - 1).Key).ToBe(
      'key' + IntToStr(Accepted - 1));
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestMapStaysUsableAfterARefusal;
var
  Map: TGatedStringMap;
  Accepted, Value: Integer;
  Refused: string;
begin
  Map := TGatedStringMap.Create(16);
  try
    Accepted := AddUntilRefused(Map, 5000);
    Refused := 'key' + IntToStr(Accepted);

    // Budget back: the refused Add goes through and lands exactly where it
    // would have landed had it never been refused.
    Map.Armed := False;
    Map.Add(Refused, Accepted);
    Expect<Integer>(Map.Count).ToBe(Accepted + 1);
    Expect<Boolean>(Map.TryGetValue(Refused, Value)).ToBe(True);
    Expect<Integer>(Value).ToBe(Accepted);
    Expect<string>(Map.EntryAt(Accepted).Key).ToBe(Refused);

    // Delete and re-add still behave: the key disappears, then reappears at
    // the end of the insertion order rather than at its old position.
    Expect<Boolean>(Map.Remove('key0')).ToBe(True);
    Expect<Integer>(Map.Count).ToBe(Accepted);
    Expect<Boolean>(Map.ContainsKey('key0')).ToBe(False);

    Map.Add('key0', 1000);
    Expect<Boolean>(Map.TryGetValue('key0', Value)).ToBe(True);
    Expect<Integer>(Value).ToBe(1000);
    Expect<string>(Map.EntryAt(Map.Count - 1).Key).ToBe('key0');
  finally
    Map.Free;
  end;
end;

procedure TTestOrderedStringMap.TestRefusedCompactionLeavesTheMapUnchanged;
var
  Map: TGatedStringMap;
  I, Value, Survivors: Integer;
  Raised: Boolean;
begin
  // Bucket array large enough that the triggering Add below cannot reach the
  // load-factor branch, so tombstone compaction is the only allocation left
  // for it to reach.
  Map := TGatedStringMap.Create(2048);
  try
    for I := 0 to 999 do
      Map.Add('key' + IntToStr(I), I);
    for I := 0 to 899 do
      Expect<Boolean>(Map.Remove('key' + IntToStr(I))).ToBe(True);

    Survivors := Map.Count;
    Expect<Integer>(Survivors).ToBe(100);
    Expect<Boolean>(Map.DeletedCount > Survivors).ToBe(True);

    Map.Armed := True;
    Raised := False;
    try
      Map.Add('compaction-trigger', -1);
    except
      on EStorageRefused do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);

    // Compaction builds a second entry array while the first is still live.
    // That sum is the transient the gate has to see; the surviving entries
    // alone would understate it more than tenfold here.
    Expect<Int64>(Map.LastBytes).ToBe(
      (Int64(1000) + Survivors) * SizeOf(TGatedStringMap.TEntry));

    Expect<Integer>(Map.Count).ToBe(Survivors);
    Expect<Boolean>(Map.ContainsKey('compaction-trigger')).ToBe(False);
    for I := 900 to 999 do
    begin
      Expect<Boolean>(Map.TryGetValue('key' + IntToStr(I), Value)).ToBe(True);
      Expect<Integer>(Value).ToBe(I);
    end;
    Expect<string>(Map.EntryAt(0).Key).ToBe('key900');

    // Refused, not broken: with the budget back the same Add compacts and
    // lands as it always would have.
    Map.Armed := False;
    Map.Add('compaction-trigger', -1);
    Expect<Integer>(Map.Count).ToBe(Survivors + 1);
    Expect<Integer>(Map.DeletedCount).ToBe(0);
    Expect<Boolean>(Map.TryGetValue('compaction-trigger', Value)).ToBe(True);
    Expect<Integer>(Value).ToBe(-1);
  finally
    Map.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestOrderedStringMap.Create('OrderedStringMap'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
