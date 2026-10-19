program Goccia.HeapSnapshot.Builtins.Test;

{$I Goccia.inc}

{ ADR 0148: built-ins are named, not written. A restore resolves each name
  in a fresh engine, building what the name crosses; a realm slot filled on
  first use is built by the materializer or the global path its unit
  registered. Every slot a run can fill must resolve on its own, whatever
  else the snapshot names. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Engine,
  Goccia.ExecutionContext,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.HeapSnapshot.TestSupport,
  Goccia.Timeout,
  Goccia.Realm,
  Goccia.Scope.BindingMap,
  Goccia.SharedPrototype,
  Goccia.Shims,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue,
  Goccia.VM;

type
  // Shim loads on a VM another thread entered first.
  TShimLoadOnThread = class(TThread)
  public
    VM: TGocciaVM;
    Template: TGocciaFunctionTemplate;
    SawOtherCode: Boolean;
    Outcome: string;
    procedure Execute; override;
  end;

  THeapSnapshotBuiltinsTests = class(TTestSuite)
  private
    procedure TestEverySlotNameResolvesOnItsOwn;
    procedure TestSlotsNothingFillsAreKnown;
    procedure TestShimLoadedNativelyRestores;
    procedure TestDateRefusesUntilItsCodec;
    procedure TestBuiltinInternalsAreNotMissed;
    procedure TestShimTakesInRunFunction;
    procedure TestShimLoadCallsAnotherEngine;
    procedure TestShimCodeIsNotItsFileName;
    procedure TestEveryDifferenceRestores;
    procedure TestLateBuiltinDifferences;
    procedure TestHostileDifferencesRefuse;
    procedure TestRefusedRestoreLeavesBuiltins;
    procedure TestRefusalWhileApplyingLeavesBuiltins;
    procedure TestTimeoutWhileRestoringLeavesBuiltins;
    procedure TestIndexKeyIsNeverForgotten;
    procedure TestDifferenceTruncationsAndMutants;
    procedure TestRestoredRunSnapshotsAgain;
    procedure TestShimLoadOnAMovedVM;
  public
    procedure SetupTests; override;
  end;

const
  SEP = HEAP_SNAPSHOT_PATH_SEPARATOR;

  // A getter the run puts on Object.prototype, which Object.defineProperty
  // reads through each descriptor the Date shim passes it while it loads:
  // it swaps in the run's own Date.now, which holds an object of its own.
  // The script it is compiled in sets `now` to the Date the shim made.
  POLLUTE_GETTER = '{ get() { if (typeof this.value === "function" && ' +
    'this.value.name === "now") { const hidden = { x: 42 }; ' +
    'this.value = ({ now() { return hidden.x; } }).now; } return false; }, ' +
    'configurable: true }';
  POLLUTE_DATE_NOW = 'Object.defineProperty(Object.prototype, "enumerable", ' +
    POLLUTE_GETTER + '); ';
  LOAD_POLLUTED_DATE = 'const d = Date; ' +
    'delete Object.prototype.enumerable; ';

  // Realm and owned slots a fresh engine leaves empty and nothing a script
  // does fills: the host installs eval; import.meta's cache holds no shared
  // prototype; the rest belong to runtime extensions (Fetch, URL, encoding,
  // events), which this engine does not install. (Object.prototype has its
  // own hasOwnProperty, so no script installs that shim, but its slot has a
  // materializer: native code can load it.)
  UNFILLED_SLOTS: array[0..11] of string = (
    'eval',
    'ImportMeta.cache',
    'Headers.shared',
    'URL.shared',
    'URLSearchParams.shared',
    'Response.shared',
    'TextDecoder.shared',
    'TextEncoder.shared',
    'Event.shared',
    'EventTarget.shared',
    'AbortController.shared',
    'AbortSignal.shared');

{ THeapSnapshotBuiltinsTests }

procedure THeapSnapshotBuiltinsTests.SetupTests;
begin
  Test('Every slot a run can fill resolves by its name on its own',
    TestEverySlotNameResolvesOnItsOwn);
  Test('The slots nothing fills are the known ones',
    TestSlotsNothingFillsAreKnown);
  Test('A shim loaded natively, not through its global, restores',
    TestShimLoadedNativelyRestores);
  Test('A Date refuses the snapshot until a codec covers its time value',
    TestDateRefusesUntilItsCodec);
  Test('Built-ins written in JavaScript are not missed objects; the run''s are',
    TestBuiltinInternalsAreNotMissed);
  Test('A shim loads with the engine''s built-ins, whatever the run did to ' +
    'the globals', TestShimTakesInRunFunction);
  Test('A shim load that ran the run''s code keeps nothing it made, on any ' +
    'engine', TestShimLoadCallsAnotherEngine);
  Test('Shim code is what the engine compiled as a shim, not a file name',
    TestShimCodeIsNotItsFileName);
  Test('Every kind of change to a built-in restores as its difference',
    TestEveryDifferenceRestores);
  Test('Built-ins the run built, then changed, restore with the change',
    TestLateBuiltinDifferences);
  Test('A difference that cannot be is refused', TestHostileDifferencesRefuse);
  Test('A refused restore leaves this engine''s built-ins as they were',
    TestRefusedRestoreLeavesBuiltins);
  Test('A restore refused at any memory ceiling changed no built-in',
    TestRefusalWhileApplyingLeavesBuiltins);
  Test('A restore stopped at any point by its deadline changed no built-in',
    TestTimeoutWhileRestoringLeavesBuiltins);
  Test('A changed built-in that has an index key keeps the fact',
    TestIndexKeyIsNeverForgotten);
  Test('Truncations and mutants of differences refuse, or restore',
    TestDifferenceTruncationsAndMutants);
  Test('A restored run with differences snapshots and restores again',
    TestRestoredRunSnapshotsAgain);
  Test('A shim load on a VM another thread entered first counts its frames',
    TestShimLoadOnAMovedVM);
end;

// Builds everything an engine builds lazily: every own property of every
// object reachable from the global object and the realm's slots is read as
// a descriptor (which builds a lazy one), and every slot filled on first use
// with a materializer or a global path is filled, until nothing new appears.
procedure BuildEverything(const AEngine: TGocciaEngine);
var
  Seen: TDictionary<TObject, Boolean>;
  Queue: TList<TGocciaObjectValue>;

  procedure Enqueue(const AValue: TObject);
  begin
    if (AValue is TGocciaObjectValue) and not Seen.ContainsKey(AValue) then
    begin
      Seen.Add(AValue, True);
      Queue.Add(TGocciaObjectValue(AValue));
    end;
  end;

  procedure Visit(const AObject: TGocciaObjectValue);
  var
    Keys: TArray<string>;
    Symbols: TArray<TGocciaSymbolValue>;
    Descriptor: TGocciaPropertyDescriptor;
    I: Integer;
  begin
    Enqueue(AObject.Prototype);
    if AObject is TGocciaClassValue then
    begin
      Enqueue(TGocciaClassValue(AObject).GetConstructorPrototype);
      Enqueue(TGocciaClassValue(AObject).Prototype);
    end;
    Keys := AObject.GetOwnPropertyKeys;
    for I := 0 to High(Keys) do
    begin
      Descriptor := AObject.GetOwnPropertyDescriptor(Keys[I]);
      if Descriptor is TGocciaPropertyDescriptorData then
        Enqueue(TGocciaPropertyDescriptorData(Descriptor).Value)
      else if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        Enqueue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
        Enqueue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
      end;
      ReleaseDescriptor(Descriptor);
    end;
    Symbols := AObject.GetOwnSymbols;
    for I := 0 to High(Symbols) do
    begin
      Descriptor := AObject.GetOwnSymbolPropertyDescriptor(Symbols[I]);
      if Descriptor is TGocciaPropertyDescriptorData then
        Enqueue(TGocciaPropertyDescriptorData(Descriptor).Value)
      else if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        Enqueue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
        Enqueue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
      end;
    end;
  end;

  // Fills what can be filled and queues the slots' objects; True when a
  // slot was filled since the last call.
  function SweepSlots: Boolean;
  var
    Owned: TObject;
    I: Integer;
  begin
    Result := False;
    for I := 0 to RealmSlotCount - 1 do
    begin
      if not Assigned(AEngine.Realm.GetSlot(I)) and
         MaterializeSnapshotSlot(RealmSlotName(I)) and
         Assigned(AEngine.Realm.GetSlot(I)) then
        Result := True;
      Enqueue(AEngine.Realm.GetSlot(I));
    end;
    for I := 0 to RealmOwnedSlotCount - 1 do
    begin
      if not Assigned(AEngine.Realm.GetOwnedSlot(I)) and
         MaterializeSnapshotSlot(RealmOwnedSlotName(I)) and
         Assigned(AEngine.Realm.GetOwnedSlot(I)) then
        Result := True;
      Owned := AEngine.Realm.GetOwnedSlot(I);
      if Owned is TGocciaSharedPrototype then
      begin
        Enqueue(TGocciaSharedPrototype(Owned).Prototype);
        Enqueue(TGocciaSharedPrototype(Owned).MethodHost);
      end;
    end;
  end;

var
  Next: Integer;
  Grew: Boolean;
begin
  Seen := TDictionary<TObject, Boolean>.Create;
  Queue := TList<TGocciaObjectValue>.Create;
  try
    Enqueue(AEngine.Realm.GlobalObject);
    Next := 0;
    repeat
      while Next < Queue.Count do
      begin
        Visit(Queue[Next]);
        Inc(Next);
      end;
      Grew := SweepSlots;
    until not Grew and (Next >= Queue.Count);
  finally
    Queue.Free;
    Seen.Free;
  end;
end;

// The slot a name starts at: '%slot' and its slot, or '%owned' and its slot.
function SlotRoot(const APath: string): string;
var
  First, Second: Integer;
begin
  First := Pos(SEP, APath);
  Second := Pos(SEP, Copy(APath, First + 1, MaxInt));
  if Second = 0 then
    Result := APath
  else
    Result := Copy(APath, 1, First + Second - 1);
end;

procedure THeapSnapshotBuiltinsTests.TestEverySlotNameResolvesOnItsOwn;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Table: TGocciaHeapSnapshotBuiltinTable;
  Resolver: TGocciaBuiltinPathResolver;
  Context: TGocciaExecutionContextScope;
  Roots: TDictionary<string, TStringList>;
  Root: TPair<string, TStringList>;
  Failed: TStringList;
  Path: string;
  I: Integer;
begin
  Lines := TStringList.Create;
  Roots := TDictionary<string, TStringList>.Create;
  Failed := TStringList.Create;
  try
    // Every name a table gives through a slot, in an engine that built all
    // it builds lazily: a table names a filled slot by the slot first.
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Table := TGocciaHeapSnapshotBuiltinTable.Create;
    try
      BuildEverything(Engine);
      BuildBuiltinTable(Table, Engine, False);
      for I := 0 to Table.Count - 1 do
      begin
        Path := Table.PathAt(I);
        if (Copy(Path, 1, 5) <> '%slot') and (Copy(Path, 1, 6) <> '%owned') then
          Continue;
        if not Roots.ContainsKey(SlotRoot(Path)) then
          Roots.Add(SlotRoot(Path), TStringList.Create);
        Roots[SlotRoot(Path)].Add(Path);
      end;
    finally
      Table.Free;
      Engine.Free;
    end;
    Expect<Boolean>(Roots.Count > 100).ToBe(True);
    // Each slot's names resolve in a fresh engine where nothing else was
    // resolved first.
    for Root in Roots do
    begin
      Engine := TGocciaEngine.Create(TEST_PATH, Lines);
      Context := Engine.ActivateRealmExecutionContext;
      Table := TGocciaHeapSnapshotBuiltinTable.Create;
      Resolver := nil;
      try
        BuildBuiltinTable(Table, Engine, False);
        Resolver := TGocciaBuiltinPathResolver.Create(Table, Engine);
        for I := 0 to Root.Value.Count - 1 do
          if not Assigned(Resolver.Resolve(Root.Value[I])) then
            Failed.Add(StringReplace(Root.Value[I], SEP, '.', [rfReplaceAll]));
      finally
        Resolver.Free;
        Table.Free;
        Context.Free;
        Engine.Free;
      end;
    end;
    Failed.Sort;
    Expect<string>(Failed.CommaText).ToBe('');
  finally
    for Root in Roots do
      Root.Value.Free;
    Roots.Free;
    Failed.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestSlotsNothingFillsAreKnown;
var
  Engine: TGocciaEngine;
  Lines, Unfilled, Expected: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Owned: TObject;
  I: Integer;
begin
  Lines := TStringList.Create;
  Unfilled := TStringList.Create;
  Expected := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    BuildEverything(Engine);
    // What only using a built-in fills: iterators of every kind, iterator
    // helpers, generators, Intl's segments.
    Engine.EvaluateGlobalEval('[...[1], ..."a", ...new Map([[1, 1]]), ' +
      '...new Set([1]), ..."a".matchAll(/a/g)]; ' +
      '[1].values().map((x) => x).toArray(); ' +
      'Iterator.from({ next() { return { done: true }; } }).toArray(); ' +
      '({ *g() {} }).g().next(); ' +
      '[...new Intl.Segmenter("en").segment("a b")]; 0', TEST_PATH, Stage);
    for I := 0 to RealmSlotCount - 1 do
      if not Assigned(Engine.Realm.GetSlot(I)) then
        Unfilled.Add(RealmSlotName(I));
    for I := 0 to RealmOwnedSlotCount - 1 do
    begin
      Owned := Engine.Realm.GetOwnedSlot(I);
      if not Assigned(Owned) then
        Unfilled.Add(RealmOwnedSlotName(I));
    end;
  finally
    Engine.Free;
  end;
  try
    for I := Low(UNFILLED_SLOTS) to High(UNFILLED_SLOTS) do
      Expected.Add(UNFILLED_SLOTS[I]);
    Unfilled.Sort;
    Expected.Sort;
    Expect<string>(Unfilled.CommaText).ToBe(Expected.CommaText);
  finally
    Expected.Free;
    Unfilled.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestShimLoadedNativelyRestores;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  Stream: TBytesStream;
  Bytes: TBytes;
  I: Integer;
begin
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    // A runtime extension takes %Date% natively (file stats do): the Date
    // shim is loaded without its global being read, so the snapshot names
    // the shim's slots alone.
    GetDateIntrinsic(Engine.ShimLoader);
    Value := Engine.EvaluateGlobalEval('({ a: 1 })', TEST_PATH, Stage);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Session.AddToBaseline(Value);
      Session.Snapshot([Value], Stream);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
    Bytes := Copy(Stream.Bytes, 0, Stream.Size);
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
  Expect<string>(RestoreAndCheck(Bytes, 'String(restored.a) + "," + ' +
    'new Date(5).getTime()')).ToBe('1,5');
  // No global path builds the hasOwnProperty shim (Object.prototype has its
  // own), but native code can load it, and the snapshot then names it.
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    Value := nil;
    for I := 0 to DefaultShimCount - 1 do
      if DefaultShim(I).Name = 'hasOwnProperty' then
        Value := LoadShimValue(Engine.ShimLoader, I);
    Expect<Boolean>(Assigned(Value)).ToBe(True);
    Session.Snapshot([Value], Stream);
    Bytes := Copy(Stream.Bytes, 0, Stream.Size);
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
  Expect<string>(RestoreAndCheck(Bytes, '[restored.call({ x: 1 }, "x"), ' +
    'restored.call({}, "x"), restored === Object.prototype.hasOwnProperty]' +
    '.join()')).ToBe('true,false,false');
end;

procedure THeapSnapshotBuiltinsTests.TestDateRefusesUntilItsCodec;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // The time value is kept outside the object: written as a plain object, a
  // Date would restore as no Date at all.
  Snapshot('({ d: new Date(5) })', Refusal);
  Expect<Boolean>(Pos('Date''s time value', Refusal) > 0).ToBe(True);
  Snapshot('(() => { class Later extends Date {} return [new Later(1)]; })()',
    Refusal);
  Expect<Boolean>(Refusal <> '').ToBe(True);
  // A run that used Dates and kept none restores, and its Dates work there.
  Bytes := Snapshot('(() => { const t = new Date(7).getTime(); ' +
    'return { t, proto: Date.prototype }; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, '[restored.t, ' +
    'restored.proto === Date.prototype, new Date(9).getTime()].join()'))
    .ToBe('7,true,9');
end;

// The refusal of a snapshot of a fresh object in a run whose top-level
// binding `kept` holds an object nothing else does, after the run used
// Date (whose shim's scope chain reaches the global scope).
function KeptBindingRefusal: string;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  Stream: TBytesStream;
begin
  Result := '';
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    Engine.GlobalScope.DefineLexicalBinding('kept',
      Engine.EvaluateGlobalEval('new Date(0).getTime(); ({ k: 1 })',
        TEST_PATH, Stage), dtConst);
    Value := Engine.EvaluateGlobalEval('({ a: 1 })', TEST_PATH, Stage);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Session.AddToBaseline(Value);
      try
        Session.Snapshot([Value], Stream);
      except
        on E: EGocciaHeapSnapshotRefused do
          Result := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestBuiltinInternalsAreNotMissed;
const
  // Each loads a shim: its module scope, cells and private functions are
  // held by the built-in it exports.
  SOURCES: array[0..2] of string = (
    '(() => { new Date(0).getTime(); return { a: 1 }; })()',
    '(() => { [1.5].toLocaleString(); (2.5).toLocaleString(); ' +
      'return { a: 1 }; })()',
    '(() => { ({}).__proto__; ({}).__lookupGetter__("x"); ' +
      'return { a: 1, n: parseInt("12"), s: btoa("x") }; })()');
var
  Refusal: string;
  I: Integer;
begin
  for I := Low(SOURCES) to High(SOURCES) do
  begin
    Snapshot(SOURCES[I], Refusal, True);
    Expect<string>(Refusal).ToBe('');
  end;
  // What only the run's own top-level binding holds is still missed, though
  // a shim's scope chain reaches the global scope that holds it.
  Refusal := KeptBindingRefusal;
  Expect<Boolean>(Pos('live objects the snapshot does not hold', Refusal) > 0)
    .ToBe(True);
  Expect<Boolean>(Pos('Bytecode', Refusal) = 0).ToBe(True);
  // A mark stack that cannot grow is recovered by tracing every marked
  // object again, the global scope too: the snapshot is refused rather than
  // the binding's object skipped.
  TGarbageCollector.LimitMarkStackForTesting(1);
  try
    Refusal := KeptBindingRefusal;
  finally
    TGarbageCollector.LimitMarkStackForTesting(0);
  end;
  Expect<Boolean>(Pos('mark stack overflowed', Refusal) > 0).ToBe(True);
end;

procedure THeapSnapshotBuiltinsTests.TestShimTakesInRunFunction;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Refusal: string;
  Bytes: TBytes;
begin
  // The parseInt shim exports the engine's Number.parseInt, not what the
  // global held when it loaded.
  Bytes := Snapshot('(() => { const original = Number.parseInt; ' +
    'Number.parseInt = (() => { const hidden = { x: 1 }; ' +
    'return (a, b) => hidden; })(); const saved = globalThis.parseInt; ' +
    'Number.parseInt = original; return saved; })()', Refusal, True);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(restored("12"))')).ToBe('12');
  Bytes := Snapshot('(() => { const original = Number.parseFloat; ' +
    'Number.parseFloat = Math.floor; const saved = globalThis.parseFloat; ' +
    'Number.parseFloat = original; return saved; })()', Refusal, True);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(restored("1.5"))'))
    .ToBe('1.5');
  // Nor does a native function standing in for Object.defineProperty
  // reach the Date shim's load: its Date has its statics.
  Expect<string>(RoundTrip('(() => { const original = ' +
    'Object.defineProperty; Object.defineProperty = Object.freeze; ' +
    'const C = Date; Object.defineProperty = original; ' +
    'return { now: typeof C.now, parse: C.parse("1970-01-01T00:00:00.005Z") }; ' +
    '})()', '[restored.now, restored.parse].join()')).ToBe('function,5');
  // The Date shim keeps its time values in the engine's WeakMap, whatever
  // the global held when it loaded, so a Date is one, and refuses.
  Snapshot('(() => { const original = WeakMap; WeakMap = Map; ' +
    'const C = Date; WeakMap = original; return new C(5); })()', Refusal,
    True);
  Expect<Boolean>(Pos('Date''s time value', Refusal) > 0).ToBe(True);
  // Nor does a binding of the run's shadow what the shim loads with.
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    Engine.EvaluateGlobalEval('const __GocciaWeakMap = Map; 0', TEST_PATH,
      Stage);
    Expect<string>(FloatToStr(TGocciaNumberLiteralValue(
      Engine.EvaluateGlobalEval('new Date(5).getTime()', TEST_PATH,
      Stage)).Value)).ToBe('5');
  finally
    Engine.Free;
    Lines.Free;
  end;
  // A function's length and name are its own properties, though synthesized:
  // deleting one is a difference.
  Expect<string>(RoundTrip('(() => { delete Math.max.name; ' +
    'return { a: 1 }; })()', '[Object.hasOwn(Math.max, "name"), ' +
    'Math.max.length].join()')).ToBe('false,2');
  Expect<string>(RoundTrip('(() => { delete Math.max.length; ' +
    'return { a: 1 }; })()', '[Object.hasOwn(Math.max, "length"), ' +
    'Math.max.name].join()')).ToBe('false,max');
end;

// Snapshots AValue in ASession with the accounting check; the refusal, or ''.
function SnapshotRefusal(const ASession: TGocciaHeapSnapshotSession;
  const AValue: TGocciaValue): string;
var
  Stream: TBytesStream;
begin
  Result := '';
  Stream := TBytesStream.Create;
  TGarbageCollector.Instance.AddTempRoot(AValue);
  try
    ASession.AddToBaseline(AValue);
    try
      ASession.Snapshot([AValue], Stream);
    except
      on E: EGocciaHeapSnapshotRefused do
        Result := E.Message;
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(AValue);
    Stream.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestShimLoadCallsAnotherEngine;
var
  Engine, Other: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value, Getter: TGocciaValue;
  Refusal: string;
  Bytes: TBytes;
begin
  // On this engine: the getter runs while Date loads, and its Date.now is
  // the run's. Unchecked, that Date.now is named as the built-in, which a
  // restore builds as the shim's.
  // What the load made is counted: the shim's cells among it.
  Snapshot('(() => { ' + POLLUTE_DATE_NOW + LOAD_POLLUTED_DATE +
    'return { a: d.now() }; })()', Refusal, True);
  Expect<Boolean>(Pos('TGocciaBytecodeCell', Refusal) > 0).ToBe(True);
  Bytes := Snapshot('(() => { ' + POLLUTE_DATE_NOW + LOAD_POLLUTED_DATE +
    'return { a: d.now(), now: d.now }; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<Boolean>(Pos('Date', RestoreRefusal(Bytes)) > 0).ToBe(True);
  // The getter of another engine on the thread, which runs on that engine's
  // VM.
  Lines := TStringList.Create;
  Other := TGocciaEngine.Create(TEST_PATH, Lines);
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := nil;
  try
    // Made before the run, so not the run's; installed during it.
    Getter := TGocciaObjectValue(Other.EvaluateGlobalEval('(' +
      POLLUTE_GETTER + ')', TEST_PATH, Stage)).GetProperty('get');
    TGocciaObjectValue(Other.Realm.GlobalObject).AssignProperty('getter',
      Getter);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    TGocciaObjectValue(TGocciaObjectValue(TGocciaObjectValue(
      Engine.Realm.GlobalObject).GetProperty('Object')).GetProperty(
      'prototype')).DefineProperty('enumerable',
      TGocciaPropertyDescriptorAccessor.Create(Getter, nil, [pfConfigurable]));
    Value := Engine.EvaluateGlobalEval('(() => { ' + LOAD_POLLUTED_DATE +
      'return { a: d.now() }; })()', TEST_PATH, Stage);
    Expect<string>(FloatToStr(TGocciaNumberLiteralValue(
      TGocciaObjectValue(Value).GetProperty('a')).Value)).ToBe('42');
    Refusal := SnapshotRefusal(Session, Value);
    Expect<Boolean>(Pos('TGocciaBytecodeCell', Refusal) > 0).ToBe(True);
  finally
    Session.Free;
    Engine.Free;
    Other.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestShimCodeIsNotItsFileName;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  // The run's code under a file name like a shim's is still the run's.
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    Value := Engine.EvaluateGlobalEval('(() => { ' + POLLUTE_DATE_NOW +
      LOAD_POLLUTED_DATE + 'return { a: d.now() }; })()', '<shim:user>',
      Stage);
    Expect<string>(FloatToStr(TGocciaNumberLiteralValue(
      TGocciaObjectValue(Value).GetProperty('a')).Value)).ToBe('42');
    Expect<Boolean>(Pos('TGocciaBytecodeCell',
      SnapshotRefusal(Session, Value)) > 0).ToBe(True);
  finally
    Session.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

function U32At(const ABytes: TBytes; const AAt: Integer): Cardinal;
begin
  Move(ABytes[AAt], Result, 4);
  Result := LEtoN(Result);
end;

procedure PutU32(var ABytes: TBytes; const AAt: Integer;
  const AValue: Cardinal);
var
  V: Cardinal;
begin
  V := NtoLE(AValue);
  Move(V, ABytes[AAt], 4);
end;

// Where a snapshot's differences begin: right after its object table, which
// must hold an object.
function DifferencesAt(const ABytes: TBytes): Integer;
var
  First, Last: Integer;
begin
  First := ObjectEntryAt(ABytes, 0);
  Last := ObjectEntryAt(ABytes, Integer(U32At(ABytes, First - 4)) - 1);
  Result := Last + 2 + 4 + Integer(U32At(ABytes, Last + 2));
end;

// The string index of the snapshot's first codec's tag.
function FirstCodecTagIndex(const ABytes: TBytes): Cardinal;
var
  At, I: Integer;
  Count: Cardinal;
begin
  At := 4 + 2;
  At := At + 4 + 2 * Integer(U32At(ABytes, At));
  Count := U32At(ABytes, At);
  Inc(At, 4);
  for I := 1 to Integer(Count) do
    At := At + 4 + 2 * Integer(U32At(ABytes, At));
  Result := U32At(ABytes, At + 4);
end;

// The length of the value written at AAt.
function ValueLength(const ABytes: TBytes; const AAt: Integer): Integer;
begin
  case ABytes[AAt] of
    HSV_NUMBER:
      Result := 9;
    HSV_STRING, HSV_OBJECT, HSV_BUILTIN, HSV_REGISTERED_SYMBOL:
      Result := 5;
    HSV_BIGINT:
      Result := 1 + 1 + 4 + 4 * Integer(U32At(ABytes, AAt + 2));
  else
    Result := 1;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestEveryDifferenceRestores;
begin
  // Added, with its identity: the run's object is written, the built-in
  // holds it after the restore.
  Expect<string>(RoundTrip('(() => { const o = { v: 1 }; Math.extra = o; ' +
    'return o; })()', '[Math.extra === restored, Math.extra.v].join()'))
    .ToBe('true,1');
  // Deleted, and redefined to another built-in.
  Expect<string>(RoundTrip('(() => { delete Math.max; Math.abs = Math.min; ' +
    'return {}; })()', '[typeof Math.max, Math.abs === Math.min].join()'))
    .ToBe('undefined,true');
  // Attributes.
  Expect<string>(RoundTrip('(() => { Object.defineProperty(Math, "min", ' +
    '{ enumerable: true }); return {}; })()',
    'String(Object.keys(Math).includes("min"))')).ToBe('true');
  // An accessor.
  Expect<string>(RoundTrip('(() => { Object.defineProperty(Math, "acc", ' +
    '{ get: Math.random, configurable: true }); return {}; })()',
    'String(Object.getOwnPropertyDescriptor(Math, "acc").get === ' +
    'Math.random)')).ToBe('true');
  // Symbol-keyed: the run's symbol added, a built-in one deleted.
  Expect<string>(RoundTrip('(() => { const s = Symbol("k"); Math[s] = 1; ' +
    'delete Math[Symbol.toStringTag]; return s; })()',
    '[Math[restored], Math[Symbol.toStringTag]].join()')).ToBe('1,');
  // The final order of the keys, deleted and added again at the end.
  Expect<string>(RoundTrip('(() => { delete Math.max; Math.max = Math.min; ' +
    'const s = Symbol("o"); Math[s] = 1; delete Math[Symbol.toStringTag]; ' +
    'Math[Symbol.toStringTag] = "M"; ' +
    'return Reflect.ownKeys(Math).map(String).join(); })()',
    'String(restored === Reflect.ownKeys(Math).map(String).join())'))
    .ToBe('true');
  // A prototype.
  Expect<string>(RoundTrip('(() => { const p = { inherited: 2 }; ' +
    'Object.setPrototypeOf(Math, p); return p; })()',
    '[Object.getPrototypeOf(Math) === restored, Math.inherited].join()'))
    .ToBe('true,2');
  // Extensibility, sealing, freezing, of functions and classes too, whose
  // synthesized length and name the freeze defines.
  Expect<string>(RoundTrip('(() => { Object.freeze(Math.max); ' +
    'Object.seal(Math.min); Object.freeze(Map); return {}; })()',
    '[Object.isFrozen(Math.max), Object.isSealed(Math.min), ' +
    'Object.isFrozen(Map), Math.max.name, Map.length].join()'))
    .ToBe('true,true,true,max,0');
  Expect<string>(RoundTrip('(() => { Object.seal(JSON); return {}; })()',
    '[Object.isSealed(JSON), Object.isFrozen(JSON), ' +
    'JSON.stringify([1])].join()')).ToBe('true,false,[1]');
  Expect<string>(RoundTrip('(() => { Object.freeze(Math); return {}; })()',
    '[Object.isFrozen(Math), Math.max(1, 2)].join()')).ToBe('true,2');
  // A function's own length, redefined.
  Expect<string>(RoundTrip('(() => { Object.defineProperty(Math.max, ' +
    '"length", { value: 7 }); return {}; })()', 'String(Math.max.length)'))
    .ToBe('7');
  // A class's synthesized name and length, deleted, and the place a name
  // defined again takes.
  Expect<string>(RoundTrip('(() => { delete Map.name; delete Map.length; ' +
    'return {}; })()', '[Object.hasOwn(Map, "name"), ' +
    'Object.hasOwn(Map, "length")].join()')).ToBe('false,false');
  Expect<string>(RoundTrip('(() => { delete Map.name; ' +
    'Object.defineProperty(Map, "name", { value: "M", configurable: true }); ' +
    'return Reflect.ownKeys(Map).map(String).join(); })()',
    '[restored === Reflect.ownKeys(Map).map(String).join(), Map.name].join()'))
    .ToBe('true,M');
  // A built-in array.
  Expect<string>(RoundTrip('(() => { Goccia.shims.push("mine"); ' +
    'return {}; })()', 'String(Goccia.shims.includes("mine"))')).ToBe('true');
  // The global object: identifiers read it.
  Expect<string>(RoundTrip('(() => { globalThis.answer = 42; ' +
    'delete globalThis.JSON; return {}; })()',
    '[answer, typeof JSON].join()')).ToBe('42,undefined');
end;

procedure THeapSnapshotBuiltinsTests.TestLateBuiltinDifferences;
begin
  // Each was built during the run, recorded as built, and changed after.
  Expect<string>(RoundTrip('(() => { Reflect.extra = 1; return {}; })()',
    'String(Reflect.extra)')).ToBe('1');
  Expect<string>(RoundTrip('(() => { Date.prototype.extra = 2; ' +
    'return {}; })()', '[Date.prototype.extra, new Date(5).getTime()].join()'))
    .ToBe('2,5');
  Expect<string>(RoundTrip('(() => { delete Temporal.PlainDate; ' +
    'return {}; })()', 'String(typeof Temporal.PlainDate)'))
    .ToBe('undefined');
  // A lazy member the run replaced after it was built.
  Expect<string>(RoundTrip('(() => { Temporal.Now; Temporal.Now = 3; ' +
    'return {}; })()', 'String(Temporal.Now)')).ToBe('3');
end;

procedure THeapSnapshotBuiltinsTests.TestHostileDifferencesRefuse;
const
  SYNTHESIZED_LEFT: array[0..1] of string = (
    '(() => { delete Math.max.name; return {}; })()',
    '(() => { delete Map.name; return {}; })()');
var
  Refusal: string;
  Bytes, Mutant: TBytes;
  At, Count, I: Integer;
begin
  Bytes := Snapshot('(() => { Math.extra = 1; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  // One difference: Math, its prototype Object.prototype (a built-in
  // reference), its four flags, its parts.
  At := DifferencesAt(Bytes);
  Expect<Integer>(Integer(U32At(Bytes, At))).ToBe(1);
  Inc(At, 4);
  Expect<Integer>(Bytes[At + 4]).ToBe(HSV_BUILTIN);
  Mutant := Copy(Bytes);
  Mutant[At + 10] := 1;  // frozen, not sealed
  Expect<string>(RestoreRefusal(Mutant))
    .ToBe('restore: integrity flags that contradict each other');
  Mutant := Copy(Bytes);
  Mutant[At + 13] := HSB_FUNCTION;
  Expect<Boolean>(Pos('parts that are not its kind', RestoreRefusal(Mutant)) > 0)
    .ToBe(True);
  // Object.prototype given itself for a prototype.
  Mutant := Copy(Bytes);
  PutU32(Mutant, At, U32At(Bytes, At + 5));
  Expect<string>(RestoreRefusal(Mutant))
    .ToBe('restore: a changed prototype of Object.prototype');

  // Sealed while a synthesized length is still configurable: a function's,
  // whose name the run deleted, and a class's.
  for I := Low(SYNTHESIZED_LEFT) to High(SYNTHESIZED_LEFT) do
  begin
    Bytes := Snapshot(SYNTHESIZED_LEFT[I], Refusal);
    Expect<string>(Refusal).ToBe('');
    At := DifferencesAt(Bytes) + 4;
    Expect<Integer>(Bytes[At + 4]).ToBe(HSV_BUILTIN);
    Mutant := Copy(Bytes);
    Mutant[At + 9] := 0;
    Mutant[At + 10] := 1;
    Mutant[At + 11] := 1;
    Expect<string>(RestoreRefusal(Mutant))
      .ToBe('restore: a sealed object with a configurable property');
  end;

  // A loop through a restored object: Math's prototype is o, o's is Math.
  Bytes := Snapshot('(() => { const o = {}; Object.setPrototypeOf(Math, o); ' +
    'return o; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes,
    'String(Object.getPrototypeOf(Math) === restored)')).ToBe('true');
  At := DifferencesAt(Bytes) + 4;
  Mutant := Copy(Bytes);
  I := ObjectEntryAt(Mutant, 0) + 2 + 4 + 1;
  Expect<Integer>(Mutant[I]).ToBe(HSV_BUILTIN);
  PutU32(Mutant, I + 1, U32At(Bytes, At));
  Expect<string>(RestoreRefusal(Mutant))
    .ToBe('restore: a prototype chain that loops');

  // A kept member of a key the built-in does not have.
  Bytes := Snapshot('(() => { globalThis.answer = 42; return {}; })()',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  At := DifferencesAt(Bytes) + 4;
  // The global object: its reference, prototype, flags, parts, members.
  At := At + 4 + ValueLength(Bytes, At + 4) + 4 + 1;
  Count := Integer(U32At(Bytes, At));
  Inc(At, 4);
  I := 0;
  while (I < Count) and (Bytes[At + 4] <> HSD_KEEP) do
  begin
    At := At + 4 + 1 + 1;
    if Bytes[At - 2] = HSD_ACCESSOR then
      At := At + ValueLength(Bytes, At) +
        ValueLength(Bytes, At + ValueLength(Bytes, At))
    else
      At := At + ValueLength(Bytes, At);
    Inc(I);
  end;
  Expect<Boolean>(I < Count).ToBe(True);
  Mutant := Copy(Bytes);
  PutU32(Mutant, At, FirstCodecTagIndex(Bytes));
  Expect<string>(RestoreRefusal(Mutant))
    .ToBe('restore: a kept property the built-in does not have here');
end;

procedure THeapSnapshotBuiltinsTests.TestRefusedRestoreLeavesBuiltins;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Refusal: string;
  Bytes: TBytes;
begin
  Bytes := Snapshot('(() => { Math.extra = 1; delete Math.max; ' +
    'return { a: 1 }; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    // Refused at its last byte, after the differences were read.
    Expect<Boolean>(RestoreRefusalIn(Engine,
      Copy(Bytes, 0, Length(Bytes) - 1)) <> '').ToBe(True);
    Expect<string>(Engine.EvaluateGlobalEval('[typeof Math.extra, ' +
      'typeof Math.max].join()', TEST_PATH, Stage).ToStringLiteral.Value)
      .ToBe('undefined,function');
    Expect<string>(RestoreRefusalIn(Engine, Bytes)).ToBe('');
    Expect<string>(Engine.EvaluateGlobalEval('[typeof Math.extra, ' +
      'typeof Math.max].join()', TEST_PATH, Stage).ToStringLiteral.Value)
      .ToBe('number,undefined');
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

// Restores mutants of ABytes, changed from AStart on, in AEngine, which is
// replaced after each restore that succeeds (it changed the engine's
// built-ins) or, with AFreshEach, after every restore (resolving a name
// builds lazy built-ins, which a later restore in the engine cannot tell
// from changes). Returns how many restored.
function RestoreMutants(var AEngine: TGocciaEngine; const ALines: TStringList;
  const ABytes: TBytes; const AStart, ACount: Integer;
  const AFreshEach: Boolean): Integer;
var
  Mutant: TBytes;
  I: Integer;
  Restored: Boolean;
begin
  Result := 0;
  for I := 1 to ACount do
  begin
    Mutant := Copy(ABytes);
    Mutant[AStart + Random(Length(ABytes) - AStart)] := Random(256);
    if Random(2) = 0 then
      Mutant[AStart + Random(Length(ABytes) - AStart)] := Random(256);
    Restored := RestoreRefusalIn(AEngine, Mutant) = '';
    if Restored then
      Inc(Result);
    if Restored or AFreshEach then
    begin
      FreeAndNil(AEngine);
      AEngine := TGocciaEngine.Create(TEST_PATH, ALines);
    end;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestRefusalWhileApplyingLeavesBuiltins;
const
  STATE = '[Array.prototype.length, Goccia.shims.length, typeof Math.extra]' +
    '.join()';
  STEP = 256 * 1024;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  GC: TGarbageCollector;
  Refusal, Pristine: string;
  Bytes: TBytes;
  Previous, Floor: Int64;
  I, Refused: Integer;
  Restored: Boolean;
begin
  // Two built-in arrays the run filled: what applying them takes is built
  // aside first, so a ceiling it does not fit refuses before either changes.
  Bytes := Snapshot('(() => { const o = {}; Math.extra = o; ' +
    'const fill = Array.from({ length: 200000 }, () => o); ' +
    'Array.prototype.push(...fill); Goccia.shims.push(...fill); ' +
    'return o; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  GC := TGarbageCollector.Instance;
  Previous := GC.MaxBytes;
  Refused := 0;
  Restored := False;
  try
    Pristine := Engine.EvaluateGlobalEval(STATE, TEST_PATH,
      Stage).ToStringLiteral.Value;
    GC.Collect;
    Floor := GC.BytesAllocated;
    for I := 0 to 160 do
    begin
      GC.MaxBytes := Floor + Int64(I) * STEP;
      Refusal := RestoreRefusalIn(Engine, Bytes);
      GC.MaxBytes := Previous;
      if Refusal = '' then
      begin
        Restored := True;
        Break;
      end;
      Inc(Refused);
      Expect<string>(Engine.EvaluateGlobalEval(STATE, TEST_PATH,
        Stage).ToStringLiteral.Value).ToBe(Pristine);
    end;
    Expect<Boolean>(Restored and (Refused > 0)).ToBe(True);
    Expect<string>(Engine.EvaluateGlobalEval(STATE, TEST_PATH,
      Stage).ToStringLiteral.Value).ToBe('200000,' + IntToStr(200000 +
      StrToInt(Copy(Pristine, Pos(',', Pristine) + 1,
      LastDelimiter(',', Pristine) - Pos(',', Pristine) - 1))) + ',object');
  finally
    GC.MaxBytes := Previous;
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestTimeoutWhileRestoringLeavesBuiltins;
const
  STATE = '[Array.prototype.length, typeof Math.extra].join()';
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Refusal, Pristine: string;
  Bytes: TBytes;
  Deadline, Refused: Integer;
  Restored: Boolean;
begin
  // Built-in differences, then many symbols registered only once the
  // restore succeeds, the last thing it polls for: wherever the deadline
  // falls, no built-in changed unless the restore did.
  Bytes := Snapshot('(() => { const o = {}; Math.extra = o; ' +
    'Array.prototype.push(...Array.from({ length: 30000 }, () => o)); ' +
    'return Array.from({ length: 30000 }, (_, i) => ' +
    'Symbol.for("restore-deadline-" + i)); })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Refused := 0;
  Restored := False;
  try
    Pristine := Engine.EvaluateGlobalEval(STATE, TEST_PATH,
      Stage).ToStringLiteral.Value;
    Deadline := 1;
    while Deadline < 20000 do
    begin
      StartExecutionTimeout(Deadline);
      try
        try
          Refusal := RestoreRefusalIn(Engine, Bytes);
        except
          on E: TGocciaTimeoutError do
            Refusal := E.Message;
        end;
      finally
        ClearExecutionTimeout;
      end;
      if Refusal = '' then
      begin
        Restored := True;
        Break;
      end;
      Inc(Refused);
      Expect<string>(Engine.EvaluateGlobalEval(STATE, TEST_PATH,
        Stage).ToStringLiteral.Value).ToBe(Pristine);
      Deadline := Deadline + 1 + Deadline div 8;
    end;
    Expect<Boolean>(Restored and (Refused > 0)).ToBe(True);
    Expect<string>(Engine.EvaluateGlobalEval(STATE, TEST_PATH,
      Stage).ToStringLiteral.Value).ToBe('30000,object');
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestIndexKeyIsNeverForgotten;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Refusal: string;
  Bytes: TBytes;
  At: Integer;
begin
  Bytes := Snapshot('(() => { Object.prototype[5] = 1; return {}; })()',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  // Object.prototype: its reference, its null prototype, then extensible,
  // frozen, sealed and had-an-index-key. A snapshot that says it never had
  // one, while giving it one, is not believed.
  At := DifferencesAt(Bytes) + 4;
  Expect<Integer>(Bytes[At + 4]).ToBe(HSV_NIL);
  Expect<Integer>(Bytes[At + 8]).ToBe(1);
  Bytes[At + 8] := 0;
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    Expect<string>(RestoreRefusalIn(Engine, Bytes)).ToBe('');
    Expect<Boolean>(TGocciaObjectValue(Engine.EvaluateGlobalEval(
      'Object.prototype', TEST_PATH, Stage)).HasEverHadIndexedOwnProperty)
      .ToBe(True);
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestDifferenceTruncationsAndMutants;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Refusal: string;
  Bytes: TBytes;
  I, Start, Restored, Failures: Integer;
begin
  Bytes := Snapshot('(() => { const o = { v: [1, "s"] }; Math.extra = o; ' +
    'delete Math.max; Math[Symbol("k")] = o; Object.seal(JSON); ' +
    'Array.prototype[1] = o; globalThis.answer = 42; ' +
    'const p = {}; Object.setPrototypeOf(Number, p); return [o, p]; })()',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    // Every cut from where the differences begin: none restores, so the
    // engine stays as it was.
    Start := DifferencesAt(Bytes);
    Failures := 0;
    for I := Start to Length(Bytes) - 1 do
      if RestoreRefusalIn(Engine, Copy(Bytes, 0, I)) = '' then
        Inc(Failures);
    Expect<Integer>(Failures).ToBe(0);
    Expect<string>(RestoreRefusalIn(Engine, Bytes)).ToBe('');
    FreeAndNil(Engine);
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    // Mutants of the differences and the roots: restored or refused, never
    // a fault.
    RandSeed := 1489;
    Restored := RestoreMutants(Engine, Lines, Bytes, Start, 10000, False);
    Expect<Boolean>((Restored > 0) and (Restored < 10000)).ToBe(True);
    // Differences of built-ins the run built: each in a fresh engine.
    Bytes := Snapshot('(() => { const o = { v: 1 }; Reflect.extra = o; ' +
      'Temporal.PlainDate.prototype.extra = o; delete Temporal.Now; ' +
      'return o; })()', Refusal);
    Expect<string>(Refusal).ToBe('');
    Restored := RestoreMutants(Engine, Lines, Bytes, DifferencesAt(Bytes),
      1000, True);
    Expect<Boolean>((Restored > 0) and (Restored < 1000)).ToBe(True);
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestRestoredRunSnapshotsAgain;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Refusal: string;
  Bytes: TBytes;
  Input: TBytesStream;
  Output: TBytesStream;
begin
  Bytes := Snapshot('(() => { const o = { v: 1 }; Math.extra = o; ' +
    'delete Math.max; Reflect.extra = 3; return o; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The session comes first, as for any run: the restore is a change.
  Lines := TStringList.Create;
  Input := TBytesStream.Create(Bytes);
  Output := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  Restore := nil;
  try
    Restore := TGocciaHeapSnapshotRestore.Create(Engine, Input,
      TGocciaHeapSnapshotLimits.Defaults);
    Session.Snapshot([Restore.Roots[0]], Output);
    Bytes := Copy(Output.Bytes, 0, Output.Size);
  finally
    Restore.Free;
    Session.Free;
    Engine.Free;
    Output.Free;
    Input.Free;
    Lines.Free;
  end;
  Expect<string>(RestoreAndCheck(Bytes, '[Math.extra === restored, ' +
    'restored.v, typeof Math.max, Reflect.extra].join()'))
    .ToBe('true,1,undefined,3');
end;

procedure TShimLoadOnThread.Execute;
var
  Token: Int64;
begin
  try
    // The VM binds to this thread only when code first enters it, inside
    // this first load.
    Token := VM.BeginShimLoad;
    VM.ExecuteFunction(Template);
    VM.EndShimLoad(Token);
    // A second load that runs code no shim compiled sees it.
    Token := VM.BeginShimLoad;
    VM.ExecuteFunction(Template);
    SawOtherCode := VM.EndShimLoad(Token);
    Outcome := 'ok';
  except
    on E: Exception do
      Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

procedure THeapSnapshotBuiltinsTests.TestShimLoadOnAMovedVM;
var
  VM: TGocciaVM;
  Template: TGocciaFunctionTemplate;
  Worker: TShimLoadOnThread;
begin
  VM := TGocciaVM.Create;
  Template := TGocciaFunctionTemplate.Create('run');
  Worker := TShimLoadOnThread.Create(True);
  try
    Template.MaxRegisters := 1;
    Template.EmitInstruction(EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    // Entered here first, so bound to this thread.
    VM.ExecuteFunction(Template);
    Worker.VM := VM;
    Worker.Template := Template;
    Worker.Start;
    Worker.WaitFor;
    Expect<string>(Worker.Outcome).ToBe('ok');
    Expect<Boolean>(Worker.SawOtherCode).ToBe(True);
    // Nothing was left counting on this thread either.
    Expect<Boolean>(VM.EndShimLoad(VM.BeginShimLoad)).ToBe(False);
  finally
    Worker.Free;
    Template.Free;
    VM.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    THeapSnapshotBuiltinsTests.Create('Heap snapshot built-ins'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
