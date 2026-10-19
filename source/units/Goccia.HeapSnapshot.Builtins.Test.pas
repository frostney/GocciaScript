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

  Goccia.Engine,
  Goccia.ExecutionContext,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.HeapSnapshot.TestSupport,
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
  Goccia.Values.SymbolValue;

type
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
  public
    procedure SetupTests; override;
  end;

const
  SEP = HEAP_SNAPSHOT_PATH_SEPARATOR;

  // A run's replacement for Object.defineProperty, to be finished by the
  // caller: while the Date shim defines Date.now, it swaps in its own
  // function, which holds an object of its own.
  DEFINE_NOW = '(target, key, descriptor) => { if (key === "now") { ' +
    'const hidden = { x: 42 }; descriptor.value = ' +
    '(class { static now() { return hidden.x; } }).now; } ';

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
  Test('A run''s function a shim took in while it loaded is not a built-in',
    TestShimTakesInRunFunction);
  Test('A shim load that calls another engine''s code keeps nothing it made',
    TestShimLoadCallsAnotherEngine);
  Test('Shim code is what the engine compiled as a shim, not a file name',
    TestShimCodeIsNotItsFileName);
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
  Refusal: string;
  Bytes: TBytes;
begin
  // The parseInt shim exports what Number.parseInt holds when it loads.
  // What the run's function holds is the run's, though the shim's export
  // reaches it.
  Snapshot('(() => { const original = Number.parseInt; ' +
    'Number.parseInt = (() => { const hidden = { x: 1 }; ' +
    'return (a, b) => hidden; })(); const saved = globalThis.parseInt; ' +
    'Number.parseInt = original; return saved; })()', Refusal, True);
  Expect<Boolean>(Pos('live objects the snapshot does not hold', Refusal) > 0)
    .ToBe(True);
  // A function that holds nothing is named as the built-in, and restores
  // as no other engine builds it.
  Bytes := Snapshot('(() => { const original = Number.parseInt; ' +
    'Number.parseInt = (a, b) => 42; const saved = globalThis.parseInt; ' +
    'Number.parseInt = original; return saved; })()', Refusal, True);
  Expect<string>(Refusal).ToBe('');
  Refusal := RestoreRefusal(Bytes);
  Expect<Boolean>(Pos('parseInt', Refusal) > 0).ToBe(True);
  // A shim whose load calls the run's function (Object.defineProperty,
  // replaced while the Date shim loads): what the load made is not taken for
  // the shim's own, so the run's state it holds is still missed.
  Snapshot('(() => { const define = Object.defineProperty; ' +
    'Object.defineProperty = ' + DEFINE_NOW +
    'return define(target, key, descriptor); }; ' +
    'const d = Date; Object.defineProperty = define; ' +
    'return { a: d.now() }; })()', Refusal, True);
  Expect<Boolean>(Pos('live objects the snapshot does not hold', Refusal) > 0)
    .ToBe(True);
  // A function's length and name are its own properties, though synthesized.
  Snapshot('(() => { delete Math.max.name; return { a: 1 }; })()', Refusal);
  Expect<Boolean>(Pos('Math', Refusal) > 0).ToBe(True);
  Snapshot('(() => { delete Math.max.length; return { a: 1 }; })()', Refusal);
  Expect<Boolean>(Pos('Math', Refusal) > 0).ToBe(True);
end;

procedure THeapSnapshotBuiltinsTests.TestShimLoadCallsAnotherEngine;
var
  Engine, Other: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  ObjectConstructor: TGocciaObjectValue;
  Trap, Value: TGocciaValue;
  Stream: TBytesStream;
  Refusal: string;
begin
  // A Proxy of another engine on the thread, whose apply trap runs on that
  // engine's VM, stands in for Object.defineProperty while Date loads.
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Other := TGocciaEngine.Create(TEST_PATH, Lines);
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := nil;
  try
    ObjectConstructor := TGocciaObjectValue(
      TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('Object'));
    TGocciaObjectValue(Other.Realm.GlobalObject).AssignProperty('realDefine',
      ObjectConstructor.GetProperty('defineProperty'));
    Trap := Other.EvaluateGlobalEval('new Proxy(realDefine, { apply: ' +
      '(real, self, args) => (' + DEFINE_NOW + 'return 0; })' +
      '(...args) || Reflect.apply(real, self, args) })', TEST_PATH, Stage);
    ObjectConstructor.AssignProperty('defineProperty', Trap);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Value := Engine.EvaluateGlobalEval('(() => { const d = Date; ' +
      'return { a: d.now() }; })()', TEST_PATH, Stage);
    Expect<Boolean>(Value is TGocciaObjectValue).ToBe(True);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Session.AddToBaseline(Value);
      Refusal := '';
      try
        Session.Snapshot([Value], Stream);
      except
        on E: EGocciaHeapSnapshotRefused do
          Refusal := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
    Expect<string>(FloatToStr(TGocciaNumberLiteralValue(
      TGocciaObjectValue(Value).GetProperty('a')).Value)).ToBe('42');
    Expect<Boolean>(Pos('live objects the snapshot does not hold', Refusal) > 0)
      .ToBe(True);
  finally
    Session.Free;
    Engine.Free;
    Other.Free;
    Stream.Free;
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
  Stream: TBytesStream;
  Refusal: string;
begin
  // The run's code under a file name like a shim's is still the run's.
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    Value := Engine.EvaluateGlobalEval('(() => { ' +
      'const define = Object.defineProperty; Object.defineProperty = ' +
      DEFINE_NOW + 'return define(target, key, descriptor); }; ' +
      'const d = Date; Object.defineProperty = define; ' +
      'return { a: d.now() }; })()', '<shim:user>', Stage);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Session.AddToBaseline(Value);
      Refusal := '';
      try
        Session.Snapshot([Value], Stream);
      except
        on E: EGocciaHeapSnapshotRefused do
          Refusal := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
    Expect<Boolean>(Pos('live objects the snapshot does not hold', Refusal) > 0)
      .ToBe(True);
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    THeapSnapshotBuiltinsTests.Create('Heap snapshot built-ins'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
