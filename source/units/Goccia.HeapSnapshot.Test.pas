program Goccia.HeapSnapshot.Test;

{$I Goccia.inc}

{ ADR 0148: heap snapshots. A graph built in one engine is written, then
  restored in a fresh engine and checked there by script: values, identity,
  cycles, symbols, property attributes, integrity, prototypes and built-ins
  named rather than written. Refusals (no codec, a changed built-in, a live
  object nothing writes, state outside the heap) are normal outcomes. The
  reader is fed every truncation and thousands of mutants of a real
  snapshot, and must refuse each it cannot restore without faulting. The
  pause checkpoint is reached only when the run waits on host calls alone. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.HeapSnapshot.TestSupport,
  Goccia.MicrotaskQueue,
  Goccia.Scope.BindingMap,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Timeout,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.VM.Registers;

const
  MUTANTS = 10000;

type
  { Owns the run's host calls: hostCall() returns a promise it answers with
    42 in its idle pass. }
  THostCallExtension = class(TGocciaEngineExtension)
  private
    FPending: TGocciaPromiseValue;
  public
    function HostCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure WaitForIdle; override;
    function HasPendingWork: Boolean; override;
  end;

  { Has work of its own until its first idle pass. }
  TBusyExtension = class(TGocciaEngineExtension)
  private
    FBusy: Boolean;
  public
    constructor Create;
    procedure WaitForIdle; override;
    function HasPendingWork: Boolean; override;
  end;

  { Holds state outside the heap that no codec covers. }
  TNativeStateExtension = class(TGocciaEngineExtension)
  public
    function HeapSnapshotRefusal: string; override;
  end;

  THeapSnapshotTests = class(TTestSuite)
  private
    FCheckpoints: Integer;
    FPauseAtCheckpoint: Boolean;
    FCheckpointRefusal: string;
    // When set, the checkpoint handler takes a snapshot through it and keeps
    // its refusal in FSnapshotRefusal.
    FSession: TGocciaHeapSnapshotSession;
    FSnapshotRefusal: string;
    FModuleSource: string;
    // snapNow(): a snapshot through FSession while the calling frame runs.
    function SnapNow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure TestClosuresShareTheirCells;
    procedure TestGeneratorResumesWhereItStopped;
    procedure TestGeneratorKeepsItsHandlers;
    procedure TestFunctionNeedsItsModuleCompiled;
    procedure TestRefusesPromisesUntilTheirCodec;
    procedure TestRestoredFrameKeepsItsCharge;
    procedure TestSameCodeCompiledTwiceIsNamed;
    procedure OnCheckpoint(const AEngine: TGocciaEngine; var APause: Boolean);
    function RunHostCall(const AOwnerFirst, ABusyExtension,
      ANativeState: Boolean; out AResult: string): string;

    procedure TestPrimitives;
    procedure TestObjectsAndArrays;
    procedure TestIdentityAndCycles;
    procedure TestPropertyAttributes;
    procedure TestIntegrity;
    procedure TestPrototypes;
    procedure TestSymbols;
    procedure TestBuiltinsAreNamed;
    procedure TestLazyBuiltinsResolve;
    procedure TestBuiltinsTheRunBuiltRestore;
    function ChainRoundTrip(const ALength: Integer): string;
    procedure TestChain;
    procedure TestDeepChain;
    procedure TestDeepNestingAndPrototypes;
    procedure TestLoneSurrogates;
    procedure TestRootsNeedNoRootingByTheCaller;
    procedure TestRefusesReplacedLazyMember;
    procedure TestRefusesPrototypeCycle;
    procedure TestRefusesChangedLateBuiltin;
    procedure TestRegistersSymbolsOnlyOnSuccess;
    procedure TestRefusesWithoutCodec;
    procedure TestRefusesChangedBuiltin;
    procedure TestAccountingRefusesMissedObject;
    procedure TestRefusesWhenCollectionQueuesWork;
    procedure TestRejectsForeignInput;
    procedure TestRejectsOverLimits;
    procedure TestRestoreChargesTheMemoryCeiling;
    procedure TestEveryTruncationRefuses;
    procedure TestMutantsRefuseOrRestore;
    procedure TestVMStateMutantsRefuseOrResume;
    procedure TestCheckpointPauses;
    procedure TestCheckpointDeclined;
    procedure TestNoCheckpointWhileOtherWorkPending;
    procedure TestRefusesStateOutsideHeap;
    procedure TestFingerprintsStayBounded;
    procedure TestSnapshotNeedsAnIdleRun;
    procedure TestRestoredBigIntKeepsItsCharge;
    procedure TestHoleOnlyInElements;
    procedure TestRestoredFrozenLiteralStaysFrozen;
    procedure TestRestoredAccessorLiteralKeepsItsSetter;
    procedure TestLargeIndexPropertyRoundTrips;
    procedure TestSnapshotEndsTheStream;
    procedure TestBuiltinsARunBuildsRestoreOrRefuseEarly;
    procedure TestSnapshotCreatesNothing;
    procedure TestHugeKeyOnBuiltinStaysBounded;
    procedure TestNestedEngineBuildsAreItsOwn;
  public
    procedure SetupTests; override;
  end;

{ THostCallExtension }

function THostCallExtension.HostCall(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FPending := TGocciaPromiseValue.Create;
  TGarbageCollector.Instance.AddTempRoot(FPending);
  Result := FPending;
end;

procedure THostCallExtension.WaitForIdle;
var
  Promise: TGocciaPromiseValue;
begin
  if not Assigned(FPending) then
    Exit;
  Promise := FPending;
  FPending := nil;
  Promise.Resolve(TGocciaNumberLiteralValue.Create(42));
  TGarbageCollector.Instance.RemoveTempRoot(Promise);
end;

function THostCallExtension.HasPendingWork: Boolean;
begin
  Result := Assigned(FPending);
end;

{ TBusyExtension }

constructor TBusyExtension.Create;
begin
  inherited Create;
  FBusy := True;
end;

procedure TBusyExtension.WaitForIdle;
begin
  FBusy := False;
end;

function TBusyExtension.HasPendingWork: Boolean;
begin
  Result := FBusy;
end;

{ TNativeStateExtension }

function TNativeStateExtension.HeapSnapshotRefusal: string;
begin
  Result := 'a native handle';
end;

{ THeapSnapshotTests }

procedure THeapSnapshotTests.SetupTests;
begin
  Test('Primitives round-trip, -0, NaN and large BigInts included',
    TestPrimitives);
  Test('Closures restore with the cells they share',
    TestClosuresShareTheirCells);
  Test('A suspended generator resumes where it stopped',
    TestGeneratorResumesWhereItStopped);
  Test('A suspended generator keeps its exception handlers',
    TestGeneratorKeepsItsHandlers);
  Test('A function restores only where its module is compiled',
    TestFunctionNeedsItsModuleCompiled);
  Test('A restored frame is charged before it is allocated, and kept',
    TestRestoredFrameKeepsItsCharge);
  Test('Code compiled twice is named by its bytecode either time',
    TestSameCodeCompiledTwiceIsNamed);
  Test('Promises, async generators and yield* refuse until their codecs land',
    TestRefusesPromisesUntilTheirCodec);
  Test('Objects and arrays round-trip with holes and lengths',
    TestObjectsAndArrays);
  Test('Shared references and cycles keep their identity',
    TestIdentityAndCycles);
  Test('Property attributes and accessors round-trip',
    TestPropertyAttributes);
  Test('Frozen, sealed and non-extensible objects stay so', TestIntegrity);
  Test('Prototypes round-trip, null and restored ones included',
    TestPrototypes);
  Test('Local, registered and well-known symbols round-trip', TestSymbols);
  Test('Built-ins are named and resolve to the fresh engine''s own',
    TestBuiltinsAreNamed);
  Test('A lazily built built-in resolves by walking its name',
    TestLazyBuiltinsResolve);
  Test('The built-ins ordinary code builds are checked and restore',
    TestBuiltinsTheRunBuiltRestore);
  Test('A chain is written and read without native recursion',
    TestChain);
  Test('A million-deep chain is written and read without native recursion',
    TestDeepChain);
  Test('Deeply nested arrays and prototype chains round-trip',
    TestDeepNestingAndPrototypes);
  Test('Strings keep lone surrogates and every UTF-16 code unit',
    TestLoneSurrogates);
  Test('Roots held only by the caller survive the snapshot''s collection',
    TestRootsNeedNoRootingByTheCaller);
  Test('A lazy member the run replaced, or a primitive it changed, restores',
    TestRefusesReplacedLazyMember);
  Test('A snapshot whose prototypes loop is refused',
    TestRefusesPrototypeCycle);
  Test('A built-in the run built and then changed restores; a native one refuses',
    TestRefusesChangedLateBuiltin);
  Test('A refused restore registers no symbol; a restored one does',
    TestRegistersSymbolsOnlyOnSuccess);
  Test('A live object with no codec refuses the snapshot',
    TestRefusesWithoutCodec);
  Test('A built-in the run changed restores as it changed',
    TestRefusesChangedBuiltin);
  Test('The accounting check refuses a live object nothing writes',
    TestAccountingRefusesMissedObject);
  Test('A collection that queues a cleanup job refuses the snapshot',
    TestRefusesWhenCollectionQueuesWork);
  Test('Foreign input, another format, build or schema is refused',
    TestRejectsForeignInput);
  Test('Input over the reader''s limits is refused before allocation',
    TestRejectsOverLimits);
  Test('A restore over the run''s memory ceiling is refused',
    TestRestoreChargesTheMemoryCeiling);
  Test('Every truncation of a real snapshot is refused',
    TestEveryTruncationRefuses);
  Test('Mutants of a real snapshot restore or are refused, never fault',
    TestMutantsRefuseOrRestore);
  Test('Truncations and mutants of VM state refuse, or restore and resume',
    TestVMStateMutantsRefuseOrResume);
  Test('A run waiting only on host calls pauses at the checkpoint',
    TestCheckpointPauses);
  Test('A declined checkpoint goes on waiting as usual',
    TestCheckpointDeclined);
  Test('No checkpoint while another extension has pending work',
    TestNoCheckpointWhileOtherWorkPending);
  Test('State outside the heap no codec covers means no checkpoint',
    TestRefusesStateOutsideHeap);
  Test('A huge value on a built-in is written past the fingerprint cap',
    TestFingerprintsStayBounded);
  Test('A snapshot needs an idle run: no job, waiter or frame',
    TestSnapshotNeedsAnIdleRun);
  Test('A restored BigInt keeps the charge for its digits',
    TestRestoredBigIntKeepsItsCharge);
  Test('The array hole is read only as an array element',
    TestHoleOnlyInElements);
  Test('A restored frozen literal stays frozen whatever its fast flag says',
    TestRestoredFrozenLiteralStaysFrozen);
  Test('An array keeps an index property past the largest array index',
    TestLargeIndexPropertyRoundTrips);
  Test('A snapshot ends the stream it is written to',
    TestSnapshotEndsTheStream);
  Test('Built-ins a run builds restore, the legacy Intl and Segmenter ones too',
    TestBuiltinsARunBuildsRestoreOrRefuseEarly);
  Test('Taking a snapshot creates no managed object',
    TestSnapshotCreatesNothing);
  Test('A huge key on a built-in is written past the fingerprint cap',
    TestHugeKeyOnBuiltinStaysBounded);
  Test('Built-ins a nested engine builds are not the outer session''s',
    TestNestedEngineBuildsAreItsOwn);
  Test('A restored literal with an accessor keeps it whatever its fast flag says',
    TestRestoredAccessorLiteralKeepsItsSetter);
end;

// Builds a graph with ASource in a fresh engine and writes what it returns;
// ARefusal is the refusal's message, or '' when the snapshot was taken.
procedure THeapSnapshotTests.TestClosuresShareTheirCells;
begin
  Expect<string>(RoundTripModule(
    '(() => { let n = 0; const inc = () => { n += 1; return n; }; inc(); ' +
    'inc(); capture({ inc, peek: () => n, twice: (f) => f() + f() }); })();',
    '[restored.inc(), restored.peek(), restored.inc(), restored.peek(), ' +
    'restored.twice(() => 21)].join()'))
    .ToBe('3,3,4,4,42');
end;

procedure THeapSnapshotTests.TestGeneratorResumesWhereItStopped;
begin
  Expect<string>(RoundTripModule(
    '(() => { const source = { *steps(start) { const a = yield start; ' +
    'const b = yield a * 2; return a + b + start; } }; ' +
    'const g = source.steps(1); g.next(); g.next(10); capture(g); })();',
    '(() => { const r = restored.next(5); const done = restored.next(); ' +
    'return [r.value, r.done, done.done].join(); })()'))
    .ToBe('16,true,true');
  // A binding still in its temporal dead zone stays in it until the
  // generator runs its declaration.
  Expect<string>(RoundTripModule(
    '(() => { const source = { *late() { const peek = () => x; ' +
    'yield peek; let x = 5; yield peek(); } }; const g = source.late(); ' +
    'capture({ g, peek: g.next().value }); })();',
    '(() => { let before; try { restored.peek(); before = "read"; } ' +
    'catch (e) { before = e.constructor.name; } ' +
    'return [before, restored.g.next().value, restored.peek()].join(); })()'))
    .ToBe('ReferenceError,5,5');
  // More arguments than a 16-bit count holds: the frame restores as the
  // call made it.
  Expect<string>(RoundTripModule(
    '(() => { const source = { *wide(...args) { yield 0; ' +
    'yield args.length + args[args.length - 1]; } }; ' +
    'const g = source.wide(...Array.from({ length: 70000 }, (_, i) => i)); ' +
    'g.next(); capture(g); })();',
    'restored.next().value')).ToBe('139999');
end;

procedure THeapSnapshotTests.TestGeneratorKeepsItsHandlers;
begin
  Expect<string>(RoundTripModule(
    '(() => { const log = []; const source = { *guarded() { try { yield 1; ' +
    'yield 2; } finally { log.push("finally"); } } }; ' +
    'const g = source.guarded(); g.next(); capture({ g, log }); })();',
    '(() => { const r = restored.g.return(7); ' +
    'return [r.value, r.done, restored.log.join()].join("|"); })()'))
    .ToBe('7|true|finally');
end;

procedure THeapSnapshotTests.TestFunctionNeedsItsModuleCompiled;
var
  Refusal: string;
  Bytes: TBytes;
begin
  Bytes := SnapshotModule('capture({ f: () => 1 });', Refusal);
  Expect<string>(Refusal).ToBe('');
  // An engine that never compiled that module.
  Expect<Boolean>(Pos('module this engine has not compiled',
    RestoreRefusal(Bytes)) > 0).ToBe(True);
  // A module body's functions close over its scope, which the module codecs
  // write; until then they refuse.
  SnapshotModule('const k = 1; capture({ f: () => k });', Refusal, True);
  Expect<Boolean>(Pos('no snapshot codec for TGocciaScope', Refusal) > 0)
    .ToBe(True);
end;

procedure THeapSnapshotTests.TestRefusesPromisesUntilTheirCodec;
var
  Refusal: string;
begin
  SnapshotModule('(() => { const later = async () => 1; ' +
    'capture({ p: later() }); })();', Refusal);
  Expect<Boolean>(Pos('no snapshot codec for TGocciaPromiseValue', Refusal) > 0)
    .ToBe(True);
  // A generator inside yield* holds an iterator the iterator codecs write.
  SnapshotModule('(() => { const source = { *inner() { yield 1; yield 2; }, ' +
    '*outer() { yield* source.inner(); } }; const g = source.outer(); ' +
    'g.next(); capture(g); })();', Refusal);
  Expect<Boolean>(Pos('a generator inside yield*', Refusal) > 0).ToBe(True);
  SnapshotModule('(() => { const source = { async *g() { yield 1; } }; ' +
    'capture(source.g()); })();', Refusal);
  Expect<Boolean>(Pos('no snapshot codec for ' +
    'TGocciaBytecodeAsyncGeneratorObjectValue', Refusal) > 0).ToBe(True);
end;

procedure THeapSnapshotTests.TestRestoredFrameKeepsItsCharge;
const
  SOURCE = '(() => { const source = { *g(...xs) { yield 0; ' +
    'yield xs.length; } }; const g = source.g(...Array.from({ length: 4000 }, ' +
    '(_, i) => i)); g.next(); capture(g); })();';
  // The generator's arguments alone: 4,000 registers.
  FRAME_BYTES = 4000 * SizeOf(TGocciaRegister);
var
  Refusal: string;
  Bytes: TBytes;
  Engine: TGocciaEngine;
  Lines: TStringList;
  Capture: TCapture;
  Restore: TGocciaHeapSnapshotRestore;
  Stream: TBytesStream;
  GC: TGarbageCollector;
  Value: TGocciaValue;
  Before, Previous: Int64;
begin
  Bytes := SnapshotModule(SOURCE, Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Capture := TCapture.Create;
  Engine := nil;
  GC := TGarbageCollector.Instance;
  try
    Lines.Text := SOURCE;
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Engine.GlobalScope.DefineLexicalBinding('capture',
      TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
    Engine.Execute;
    TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Capture.Value := nil;
    GC.Collect;
    Before := GC.BytesAllocated;
    // Under a ceiling the frame does not fit: refused before allocating it.
    Previous := GC.MaxBytes;
    GC.MaxBytes := Before + FRAME_BYTES div 2;
    Stream := TBytesStream.Create(Bytes);
    try
      try
        TGocciaHeapSnapshotRestore.Create(Engine, Stream,
          TGocciaHeapSnapshotLimits.Defaults).Free;
        Refusal := '';
      except
        on E: EGocciaHeapSnapshotInvalid do
          Refusal := E.Message;
      end;
    finally
      GC.MaxBytes := Previous;
      Stream.Free;
    end;
    Expect<Boolean>(Pos('memory', Refusal) > 0).ToBe(True);
    GC.Collect;
    Before := GC.BytesAllocated;
    Stream := TBytesStream.Create(Bytes);
    try
      Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream,
        TGocciaHeapSnapshotLimits.Defaults);
      Value := TGocciaValue(Restore.Roots[0]);
      GC.AddTempRoot(Value);
      Restore.Free;
    finally
      Stream.Free;
    end;
    GC.Collect;
    // The restore's reservations are gone; the frame's charge is the
    // generator's now.
    Expect<Boolean>(GC.BytesAllocated - Before >= FRAME_BYTES).ToBe(True);
    GC.RemoveTempRoot(Value);
    GC.Collect;
    Expect<Boolean>(GC.BytesAllocated - Before < FRAME_BYTES).ToBe(True);
  finally
    Engine.Free;
    Capture.Free;
    Lines.Free;
  end;
  // A frame wider than its template, one register per argument: resumed
  // with its arguments.
  Expect<string>(RoundTripModuleBytes(SOURCE, Bytes,
    'String(restored.next().value)')).ToBe('4000');
end;

procedure THeapSnapshotTests.TestSameCodeCompiledTwiceIsNamed;
const
  SOURCE = 'capture({ f: () => 7 });';
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Capture: TCapture;
  Stream: TBytesStream;
  Bytes: TBytes;
begin
  Lines := TStringList.Create;
  Capture := TCapture.Create;
  Stream := TBytesStream.Create;
  Engine := nil;
  Session := nil;
  try
    Lines.Text := SOURCE;
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Engine.GlobalScope.DefineLexicalBinding('capture',
      TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Session.CheckAccounting := False;
    // Compiled twice: the function captured last is the second compilation's.
    Engine.Execute;
    Engine.Execute;
    Expect<Boolean>(Engine.RetainedModuleCount >= 2).ToBe(True);
    Session.Snapshot([Capture.Value], Stream);
    Bytes := Copy(Stream.Bytes, 0, Stream.Size);
  finally
    if Assigned(Capture.Value) then
      TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Session.Free;
    Engine.Free;
    Stream.Free;
    Capture.Free;
    Lines.Free;
  end;
  Expect<string>(RoundTripModuleBytes(SOURCE, Bytes, 'String(restored.f())'))
    .ToBe('7');
end;

procedure THeapSnapshotTests.TestPrimitives;
begin
  Expect<string>(RoundTrip(
    '[1.5, -0, NaN, Infinity, -Infinity, "text", "", "é ☃ 😀", true, ' +
    'false, null, undefined, 123456789012345678901234567890n, -7n, 0n]',
    '[Object.is(restored[1], -0), Number.isNaN(restored[2]), ' +
    'restored[3] === Infinity, restored[4] === -Infinity, restored[0], ' +
    'restored[5], restored[6] === "", restored[7], restored[8], restored[9], ' +
    'restored[10] === null, restored[11] === undefined, ' +
    'restored[12] === 123456789012345678901234567890n, restored[13] === -7n, ' +
    'restored[14] === 0n].join()'))
    .ToBe('true,true,true,true,1.5,text,true,é ☃ 😀,true,false,true,true,' +
      'true,true,true');
end;

procedure THeapSnapshotTests.TestObjectsAndArrays;
begin
  Expect<string>(RoundTrip(
    '(() => { const a = [1, , 3]; a.length = 6; a.extra = "x"; ' +
    'const o = { a, n: { deep: [ { k: "v" } ] }, "key with space": 1, 2: "two" }; ' +
    'return o; })()',
    'JSON.stringify(restored) + "|" + restored.a.length + "|" + ' +
    '(1 in restored.a) + "|" + restored.a.extra + "|" + ' +
    'Object.keys(restored).join() + "|" + Array.isArray(restored.a)'))
    .ToBe('{"2":"two","a":[1,null,3,null,null,null],"n":{"deep":[{"k":"v"}]},' +
      '"key with space":1}|6|false|x|2,a,n,key with space|true');
end;

procedure THeapSnapshotTests.TestIdentityAndCycles;
begin
  Expect<string>(RoundTrip(
    '(() => { const shared = { s: 1 }; const o = { x: shared, y: shared, ' +
    'list: [shared] }; o.self = o; o.list.push(o.list); return o; })()',
    '[restored.self === restored, restored.x === restored.y, ' +
    'restored.list[0] === restored.x, restored.list[1] === restored.list]' +
    '.join()'))
    .ToBe('true,true,true,true');
end;

procedure THeapSnapshotTests.TestPropertyAttributes;
begin
  Expect<string>(RoundTrip(
    '(() => { const o = {}; ' +
    'Object.defineProperty(o, "hidden", { value: 1, enumerable: false, ' +
    'writable: false, configurable: false }); ' +
    'Object.defineProperty(o, "acc", { get: Object.prototype.valueOf, ' +
    'enumerable: true, configurable: true }); ' +
    'o.plain = 2; return o; })()',
    '(() => { const h = Object.getOwnPropertyDescriptor(restored, "hidden"); ' +
    'const a = Object.getOwnPropertyDescriptor(restored, "acc"); ' +
    'return [h.value, h.enumerable, h.writable, h.configurable, ' +
    'a.get === Object.prototype.valueOf, a.set, a.enumerable, a.configurable, ' +
    'Object.keys(restored).join("+")].join(); })()'))
    .ToBe('1,false,false,false,true,,true,true,acc+plain');
end;

procedure THeapSnapshotTests.TestIntegrity;
begin
  Expect<string>(RoundTrip(
    '[Object.freeze({ a: 1 }), Object.seal({ b: 2 }), ' +
    'Object.preventExtensions({ c: 3 }), Object.freeze([1, 2])]',
    '[Object.isFrozen(restored[0]), Object.isSealed(restored[1]), ' +
    'Object.isFrozen(restored[1]), Object.isExtensible(restored[2]), ' +
    'Object.isSealed(restored[2]), Object.isFrozen(restored[3])].join()'))
    .ToBe('true,true,false,false,false,true');
end;

procedure THeapSnapshotTests.TestPrototypes;
begin
  Expect<string>(RoundTrip(
    '(() => { const base = { greet: "hi" }; const child = Object.create(base); ' +
    'child.own = 1; return [child, base, Object.create(null), [], {}]; })()',
    '[Object.getPrototypeOf(restored[0]) === restored[1], restored[0].greet, ' +
    'Object.getPrototypeOf(restored[2]) === null, ' +
    'Object.getPrototypeOf(restored[3]) === Array.prototype, ' +
    'Object.getPrototypeOf(restored[4]) === Object.prototype].join()'))
    .ToBe('true,hi,true,true,true');
end;

procedure THeapSnapshotTests.TestSymbols;
begin
  Expect<string>(RoundTrip(
    '(() => { const local = Symbol("local"); const o = { [local]: 1, ' +
    '[Symbol.for("shared")]: 2, [Symbol.iterator]: 3, sym: local, ' +
    'anon: Symbol() }; return o; })()',
    '(() => { const keys = Object.getOwnPropertySymbols(restored); ' +
    'return [keys.length, keys[0] === restored.sym, restored[keys[0]], ' +
    'keys[1] === Symbol.for("shared"), restored[Symbol.for("shared")], ' +
    'keys[2] === Symbol.iterator, restored[Symbol.iterator], ' +
    'restored.sym.description, restored.anon.description === undefined, ' +
    'Symbol.keyFor(restored.sym) === undefined].join(); })()'))
    .ToBe('3,true,1,true,2,true,3,local,true,true');
end;

procedure THeapSnapshotTests.TestBuiltinsAreNamed;
begin
  Expect<string>(RoundTrip(
    '({ m: Math, f: Array.prototype.map, p: Object.prototype, ' +
    'g: globalThis, j: JSON.parse, it: [][Symbol.iterator] })',
    '[restored.m === Math, restored.f === Array.prototype.map, ' +
    'restored.p === Object.prototype, restored.g === globalThis, ' +
    'restored.j === JSON.parse, ' +
    'restored.it === Array.prototype[Symbol.iterator]].join()'))
    .ToBe('true,true,true,true,true,true');
end;

procedure THeapSnapshotTests.TestLazyBuiltinsResolve;
begin
  Expect<string>(RoundTrip(
    '({ t: Temporal.PlainDate, now: Temporal.Now.instant, ' +
    'i: Intl.NumberFormat.prototype })',
    '[restored.t === Temporal.PlainDate, ' +
    'restored.now === Temporal.Now.instant, ' +
    'restored.i === Intl.NumberFormat.prototype].join()'))
    .ToBe('true,true,true');
end;

// A linked list ALength long, walked in the fresh engine: its length and the
// sum of its values.
function THeapSnapshotTests.ChainRoundTrip(const ALength: Integer): string;
begin
  Result := RoundTrip(
    'Array.from({ length: ' + IntToStr(ALength) + ' }, (_, i) => i)' +
    '.reduce((next, i) => ({ i, next }), null)',
    '(() => { const chain = { [Symbol.iterator]() { let p = restored; ' +
    'return { next() { if (p === null) return { done: true, value: 0 }; ' +
    'const value = p.i; p = p.next; return { done: false, value }; } }; } }; ' +
    'let n = 0; let sum = 0; for (const i of chain) { n++; sum += i; } ' +
    'return n + "," + sum; })()');
end;

procedure THeapSnapshotTests.TestChain;
begin
  Expect<string>(ChainRoundTrip(5000)).ToBe('5000,12497500');
end;

procedure THeapSnapshotTests.TestBuiltinsTheRunBuiltRestore;
begin
  // Iterators of every kind, spreads, collections, strings and RegExps, JSON,
  // Temporal and Intl: whatever the engine builds on first use is named,
  // fingerprinted and built again on restore. (No promise job: outside a run
  // it would stay queued for the tests after this one.)
  Expect<string>(RoundTrip(
    '(() => { const out = []; for (const x of [1, 2]) out.push(x); ' +
    'out.push(...new Set([3]), ...new Map([[4, 4]]).keys(), ...("ab")); ' +
    'out.push("a-b".split(/-/).length, [..."x".matchAll(/x/g)].length); ' +
    'out.push(JSON.stringify({ k: 1 })); ' +
    'out.push(Temporal.PlainDate.from("2026-10-19").day); ' +
    'out.push(new Intl.NumberFormat("en").format(1234)); ' +
    'out.push([1, 2].entries().next().value[1], Object.entries({ e: 5 })[0][1]); ' +
    'return { out, it: [][Symbol.iterator]() .__proto__ === undefined }; })()',
    'restored.out.join() + "|" + restored.it'))
    .ToBe('1,2,3,4,a,b,2,1,{"k":1},19,1,234,1,5|false');
end;

procedure THeapSnapshotTests.TestDeepChain;
begin
  Expect<string>(ChainRoundTrip(1000000)).ToBe('1000000,499999500000');
end;

procedure THeapSnapshotTests.TestDeepNestingAndPrototypes;
begin
  // 100,000 arrays each holding the next, and 100,000 objects each the
  // prototype of the next.
  Expect<string>(RoundTrip(
    '[Array.from({ length: 100000 }, (_, i) => i)' +
    '.reduce((inner) => [inner], []), ' +
    'Array.from({ length: 100000 }, (_, i) => i)' +
    '.reduce((proto, i) => { const o = Object.create(proto); o.i = i; ' +
    'return o; }, null)]',
    '(() => { const walk = (start, step) => ({ [Symbol.iterator]() { ' +
    'let p = start; return { next() { if (p === null || p === undefined) ' +
    'return { done: true, value: 0 }; const value = p; p = step(p); ' +
    'return { done: false, value }; } }; } }); ' +
    'let arrays = 0; for (const a of walk(restored[0], (a) => a[0])) arrays++; ' +
    'let protos = 0; let top = 0; for (const o of walk(restored[1], ' +
    '(o) => Object.getPrototypeOf(o))) { if (protos === 0) top = o.i; ' +
    'protos++; } return arrays + "," + protos + "," + top; })()'))
    .ToBe('100001,100000,99999');
end;

procedure THeapSnapshotTests.TestLoneSurrogates;
begin
  Expect<string>(RoundTrip(
    '({ lone: "a\uD800b\uDFFFc", pair: "\uD83D\uDE00", ' +
    '"\uDC00key": String.fromCharCode(0, 65535) })',
    '[restored.lone.length, restored.lone.charCodeAt(1), ' +
    'restored.lone.charCodeAt(3), restored.pair.codePointAt(0), ' +
    'Object.keys(restored)[2].charCodeAt(0), ' +
    'restored["\uDC00key"].charCodeAt(0), ' +
    'restored["\uDC00key"].charCodeAt(1)].join()'))
    .ToBe('5,55296,57343,128512,56320,0,65535');
end;

procedure THeapSnapshotTests.TestRootsNeedNoRootingByTheCaller;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Stream: TBytesStream;
  Bytes: TBytes;
begin
  Lines := TStringList.Create;
  Engine := nil;
  Session := nil;
  Stream := TBytesStream.Create;
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    // The accounting check collects first; the value is held only by the
    // open array handed to Snapshot.
    Session.Snapshot([Engine.EvaluateGlobalEval(
      '({ list: Array.from({ length: 1000 }, (_, i) => ({ i })) })',
      TEST_PATH, Stage)], Stream);
    Bytes := Copy(Stream.Bytes, 0, Stream.Size);
  finally
    Stream.Free;
    Session.Free;
    Engine.Free;
    Lines.Free;
  end;
  Expect<string>(RestoreAndCheck(Bytes, 'restored.list.length + "," + ' +
    'restored.list[999].i')).ToBe('1000,999');
end;

procedure THeapSnapshotTests.TestRefusesReplacedLazyMember;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // Building a lazy member is not a change...
  Snapshot('(() => { Temporal.PlainDate; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  // ...replacing one is, before it was built or after: a difference.
  Bytes := Snapshot('(() => { Object.defineProperty(globalThis, "Temporal", ' +
    '{ value: { mine: 1 }, configurable: true, writable: true }); ' +
    'return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(Temporal.mine)')).ToBe('1');
  Bytes := Snapshot('(() => { Temporal.PlainDate; ' +
    'Object.defineProperty(globalThis, "Temporal", { value: { mine: 2 }, ' +
    'configurable: true, writable: true }); return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(Temporal.mine)')).ToBe('2');
  // A primitive changed in place, true to false.
  Bytes := Snapshot('(() => { Array.prototype[Symbol.unscopables].at = ' +
    'false; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes,
    'String(Array.prototype[Symbol.unscopables].at)')).ToBe('false');
end;

// Rewrites the prototype of the third object a snapshot of [a, b] (b being
// a's prototype) writes, b, from Object.prototype to a, leaving every length
// as it was. The layout is the format's own: header, string table, codecs,
// built-in names, then the object table.
procedure THeapSnapshotTests.TestRefusesPrototypeCycle;
var
  Refusal: string;
  Bytes: TBytes;
  Pos_: Integer;
begin
  Bytes := Snapshot('(() => { const b = {}; const a = Object.create(b); ' +
    'return [a, b]; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, String('Object.getPrototypeOf(' +
    'restored[0]) === restored[1]'))).ToBe('true');
  // Object 2 is b, an object literal: its payload is its literal flag, then
  // its prototype, a built-in reference.
  Pos_ := ObjectEntryAt(Bytes, 2) + 2 + 4 + 1;
  Expect<Integer>(Bytes[Pos_]).ToBe(HSV_BUILTIN);
  Bytes[Pos_] := HSV_OBJECT;
  Bytes[Pos_ + 1] := 1;
  Bytes[Pos_ + 2] := 0;
  Bytes[Pos_ + 3] := 0;
  Bytes[Pos_ + 4] := 0;
  Expect<string>(RestoreRefusal(Bytes))
    .ToBe('restore: a prototype chain that loops');
end;

procedure THeapSnapshotTests.TestFingerprintsStayBounded;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // Over the fingerprint cap: the built-in fingerprints as changed without
  // the value being copied first, and is written as its difference.
  Bytes := Snapshot('(() => { Math.big = "x".repeat(6 * 1024 * 1024); ' +
    'return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(Math.big.length)'))
    .ToBe('6291456');
  Bytes := Snapshot('(() => { Math.big = 1n << 40000000n; return {}; })()',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(Math.big > 0n)'))
    .ToBe('true');
end;

function THeapSnapshotTests.SnapNow(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Stream: TBytesStream;
begin
  FSnapshotRefusal := '';
  Stream := TBytesStream.Create;
  try
    try
      FSession.Snapshot([], Stream);
    except
      on E: EGocciaHeapSnapshotRefused do
        FSnapshotRefusal := E.Message;
    end;
  finally
    Stream.Free;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure THeapSnapshotTests.TestSnapshotNeedsAnIdleRun;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Stream: TBytesStream;

  function RefusalAfter(const ASource: string;
    const AAccounting: Boolean): string;
  begin
    Result := '';
    Engine.EvaluateGlobalEval(ASource, TEST_PATH, Stage);
    Session.CheckAccounting := AAccounting;
    try
      Session.Snapshot([], Stream);
    except
      on E: EGocciaHeapSnapshotRefused do
        Result := E.Message;
    end;
  end;

begin
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    // A queued job would be dropped, with the accounting check or without.
    Expect<string>(RefusalAfter('Promise.resolve().then(() => 1); 0',
      False)).ToBe('a job is queued');
    Expect<string>(RefusalAfter('0', True)).ToBe('a job is queued');
    TGocciaMicrotaskQueue.Instance.DrainQueue;
    Expect<string>(RefusalAfter('0', False)).ToBe('');
    // So would a waiter's subscription and deadline.
    Expect<string>(RefusalAfter('globalThis.w = Atomics.waitAsync(' +
      'new Int32Array(new SharedArrayBuffer(4)), 0, 0); 0', False))
      .ToBe('an Atomics.waitAsync waiter is pending');
    Expect<Boolean>(Engine.IsAtPauseCheckpoint).ToBe(False);
    // From a host function the script called, its frame still running.
    FSession := Session;
    Engine.GlobalScope.DefineLexicalBinding('snapNow',
      TGocciaNativeFunctionValue.Create(SnapNow, 'snapNow', 0), dtConst);
    Engine.EvaluateGlobalEval('(() => { snapNow(); return 0; })()',
      TEST_PATH, Stage);
    Expect<string>(FSnapshotRefusal).ToBe('a bytecode frame is running');
  finally
    FSession := nil;
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
    TGocciaMicrotaskQueue.Instance.DrainQueue;
  end;
end;

procedure THeapSnapshotTests.TestRestoredBigIntKeepsItsCharge;
const
  // 2^800000: 25,001 limbs, about 100 KB of digits.
  LIMB_BYTES = 25001 * 4;
var
  Refusal: string;
  Bytes: TBytes;
  Engine: TGocciaEngine;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Stream: TBytesStream;
  GC: TGarbageCollector;
  Value: TGocciaValue;
  Before: Int64;
begin
  Bytes := Snapshot('1n << 800000n', Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Stream := TBytesStream.Create(Bytes);
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  GC := TGarbageCollector.Instance;
  try
    GC.Collect;
    Before := GC.BytesAllocated;
    Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream,
      TGocciaHeapSnapshotLimits.Defaults);
    Value := TGocciaValue(Restore.Roots[0]);
    GC.AddTempRoot(Value);
    Restore.Free;
    GC.Collect;
    // The restore's own reservations are gone; the digits' charge is not.
    Expect<Boolean>(GC.BytesAllocated - Before >= LIMB_BYTES).ToBe(True);
    GC.RemoveTempRoot(Value);
    GC.Collect;
    Expect<Boolean>(GC.BytesAllocated - Before < LIMB_BYTES).ToBe(True);
  finally
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotTests.TestHoleOnlyInElements;
var
  Refusal: string;
  Undefined, Null: TBytes;
  I, At: Integer;
begin
  Undefined := Snapshot('({ a: undefined })', Refusal);
  Expect<string>(Refusal).ToBe('');
  Null := Snapshot('({ a: null })', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The two differ only in the property value's tag.
  Expect<Integer>(Length(Null)).ToBe(Length(Undefined));
  At := -1;
  for I := 0 to High(Null) do
    if Null[I] <> Undefined[I] then
    begin
      Expect<Integer>(At).ToBe(-1);
      At := I;
    end;
  Expect<Integer>(Undefined[At]).ToBe(HSV_UNDEFINED);
  Undefined[At] := HSV_HOLE;
  Expect<string>(RestoreRefusal(Undefined))
    .ToBe('restore: an array hole outside an array''s elements');
  // In an array's elements it is a hole.
  Expect<string>(RestoreAndCheck(Snapshot('[1, , 3]', Refusal),
    '(1 in restored) + "," + restored.length')).ToBe('false,3');
end;

procedure THeapSnapshotTests.TestRestoredFrozenLiteralStaysFrozen;
var
  Refusal: string;
  Bytes: TBytes;
  Pos_: Integer;
begin
  Bytes := Snapshot('Object.freeze({ x: 1 })', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The literal's payload starts with its fast flag, off once frozen; a
  // snapshot claiming it on must not make the object take new properties.
  Pos_ := ObjectEntryAt(Bytes, 0) + 2 + 4;
  Expect<Integer>(Bytes[Pos_]).ToBe(0);
  Bytes[Pos_] := 1;
  Expect<string>(RestoreAndCheck(Bytes, '(() => { try { restored.y = 2; } ' +
    'catch (e) {} return [Object.isFrozen(restored), "y" in restored]' +
    '.join(); })()')).ToBe('true,false');
end;

procedure THeapSnapshotTests.TestRestoredAccessorLiteralKeepsItsSetter;
var
  Refusal: string;
  Bytes: TBytes;
  Pos_: Integer;
begin
  // Built-in accessors, which this phase names; neither changes the literal,
  // which would leave the fast mode on its own.
  Bytes := Snapshot('(() => { const o = { a: 1 }; ' +
    'Object.defineProperty(o, "x", { get: Object.prototype.valueOf, ' +
    'set: Object.prototype.valueOf, configurable: true }); return o; })()',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  // The literal's payload starts with its fast flag, off once an accessor is
  // defined; a snapshot claiming it on must not let assignments replace the
  // accessor once the property-write cache stops priming.
  Pos_ := ObjectEntryAt(Bytes, 0) + 2 + 4;
  Expect<Integer>(Bytes[Pos_]).ToBe(0);
  Bytes[Pos_] := 1;
  // Strict code: the loose store never takes the literal fast path.
  Expect<string>(RestoreAndCheck(Bytes, '(() => { "use strict"; ' +
    'for (const i of Array.from({ length: 200 }, (_, n) => n)) ' +
    'restored.x = i; ' +
    'const d = Object.getOwnPropertyDescriptor(restored, "x"); ' +
    'return [typeof d.set, "value" in d].join(); })()'))
    .ToBe('function,false');
end;

procedure THeapSnapshotTests.TestLargeIndexPropertyRoundTrips;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // 2^32 - 1 is no array index: an ordinary property, whatever the length.
  Bytes := Snapshot('(() => { const a = [1]; a["4294967295"] = "x"; ' +
    'return a; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes,
    'restored["4294967295"] + restored.length')).ToBe('x1');
end;

procedure THeapSnapshotTests.TestSnapshotEndsTheStream;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Stream: TBytesStream;
  Value: TGocciaValue;
  Junk: TBytes;
  Bytes: TBytes;
begin
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    Value := Engine.EvaluateGlobalEval('({ a: 1 })', TEST_PATH, Stage);
    Session.AddToBaseline(Value);
    // A reused stream, longer than the snapshot, written from its start.
    SetLength(Junk, 1024 * 1024);
    FillChar(Junk[0], Length(Junk), $AB);
    Stream.WriteBuffer(Junk[0], Length(Junk));
    Stream.Position := 0;
    Session.Snapshot([Value], Stream);
    Expect<Boolean>(Stream.Size < Length(Junk)).ToBe(True);
    Bytes := Copy(Stream.Bytes, 0, Stream.Size);
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
  Expect<string>(RestoreAndCheck(Bytes, 'String(restored.a)')).ToBe('1');
end;

procedure THeapSnapshotTests.TestBuiltinsARunBuildsRestoreOrRefuseEarly;
const
  SOURCES: array[0..32] of string = (
    'Temporal.Now.instant(); ({})',
    'Temporal.PlainDate.from("2026-01-02").add({ days: 1 }); ({})',
    'Temporal.Duration.from({ hours: 1 }); ({})',
    'Temporal.PlainTime.from("10:00"); ({})',
    'Temporal.ZonedDateTime.from("2026-01-02T00:00[UTC]"); ({})',
    'Temporal.PlainYearMonth.from("2026-01"); Temporal.PlainMonthDay.from("01-02"); ({})',
    'Temporal.PlainDateTime.from("2026-01-02T03:04"); Temporal.Instant.from("2026-01-02T00:00Z"); ({})',
    'new Intl.NumberFormat("en").format(1); ({})',
    'Intl.NumberFormat.call(Object.create(Intl.NumberFormat.prototype)); ({})',
    'Intl.DateTimeFormat.call(Object.create(Intl.DateTimeFormat.prototype)); ({})',
    'new Intl.DateTimeFormat("en").format(0); ({})',
    'new Intl.Collator("en").compare("a", "b"); ({})',
    'new Intl.Segmenter("en").segment("a b"); ({})',
    '[...new Intl.Segmenter("en").segment("a b")]; ({})',
    'new Intl.Locale("en"); ({})',
    'new Intl.ListFormat("en").format(["a"]); ({})',
    'new Intl.DurationFormat("en").format({ hours: 1 }); ({})',
    'new Intl.DisplayNames("en", { type: "region" }).of("US"); ({})',
    'new Intl.PluralRules("en").select(1); ({})',
    'new Intl.RelativeTimeFormat("en").format(1, "day"); ({})',
    'new Date(0).toISOString(); ({})',
    'Date.now(); new Date().toLocaleString(); ({})',
    'new DisposableStack(); new AsyncDisposableStack(); ({})',
    'parseInt("1"); atob("YQ=="); btoa("a"); isNaN(1); ({})',
    '({}).hasOwnProperty("a"); ({}).__proto__; ({})',
    '[1, 2].values().map((x) => x).toArray(); "ab".matchAll(/a/g).next(); ({})',
    'new Map([[1, 2]]).entries().next(); new Set([1]).values().next(); "a"[Symbol.iterator]().next(); ({})',
    'const o = { *g() { yield 1; } }; o.g().next(); ({})',
    'const o = { async *g() { yield 1; } }; o.g().next(); ({})',
    'const f = async () => 1; f(); ({})',
    'Iterator.from({ next() { return { done: true }; } }).next(); ({})',
    'new Proxy({}, {}); Reflect.ownKeys({}); ({})',
    '[1.5].toLocaleString(); (1).toLocaleString(); ({})');
var
  Refusal: string;
  I, Restored: Integer;
  Bytes: TBytes;
begin
  Restored := 0;
  for I := 0 to High(SOURCES) do
  begin
    Bytes := Snapshot(SOURCES[I], Refusal);
    // What an async source left queued is the next source's problem
    // otherwise: the queue is the thread's. It belongs to the engine
    // Snapshot has freed, so it is discarded, never run: a job run after its
    // engine reads the freed realm (GocciaScript #1821).
    TGocciaMicrotaskQueue.Instance.ClearQueue;
    // A snapshot the same engine build wrote restores.
    if Refusal = '' then
    begin
      Expect<string>(SOURCES[I] + ' => ' + RestoreRefusal(Bytes))
        .ToBe(SOURCES[I] + ' => ');
      Inc(Restored);
    end;
  end;
  // Every source but the async generator's, whose next() leaves a job
  // queued.
  Expect<Integer>(Restored).ToBe(Length(SOURCES) - 1);
end;

procedure THeapSnapshotTests.TestSnapshotCreatesNothing;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Stream: TBytesStream;
  Value: TGocciaValue;
  GC: TGarbageCollector;
  Before: Integer;
begin
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  GC := TGarbageCollector.Instance;
  try
    // Array built-ins included: their length is fingerprinted too.
    Value := Engine.EvaluateGlobalEval('[1, { a: [2] }]', TEST_PATH, Stage);
    GC.AddTempRoot(Value);
    Session.AddToBaseline(Value);
    GC.Collect;
    Before := GC.ManagedObjectCount;
    Session.Snapshot([Value], Stream);
    Session.Snapshot([Value], Stream);
    // What the writer created would outlive its collection, uncounted.
    Expect<Integer>(GC.ManagedObjectCount).ToBe(Before);
    GC.RemoveTempRoot(Value);
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotTests.TestHugeKeyOnBuiltinStaysBounded;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // The key alone is past the cap: changed, the key copied nowhere to tell,
  // and written as the difference.
  Bytes := Snapshot('(() => { Math["k".repeat(5 * 1024 * 1024)] = 1; ' +
    'return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(Object.keys(Math)[0].length)'))
    .ToBe('5242880');
  // So are many symbols.
  Bytes := Snapshot('(() => { for (const i of Array.from({ length: 300000 }, ' +
    '(_, i) => i)) Math[Symbol()] = i; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes,
    'String(Object.getOwnPropertySymbols(Math).length)')).ToBe('300001');
  // A built-in the run built, then grew: walked one key at a time, and the
  // restoring engine gets the change.
  Bytes := Snapshot('(() => { const p = Temporal.PlainDate.prototype; ' +
    'p["k".repeat(5 * 1024 * 1024)] = 1; ' +
    'for (const i of Array.from({ length: 100000 }, (_, i) => i)) ' +
    'p[Symbol("s".repeat(i % 7))] = i; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(Object.getOwnPropertySymbols(' +
    'Temporal.PlainDate.prototype).length)')).ToBe('100001');
end;

procedure THeapSnapshotTests.TestNestedEngineBuildsAreItsOwn;
var
  Outer, Nested: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Stream: TBytesStream;
  Value: TGocciaValue;
begin
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Outer := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Outer);
  try
    // Another engine on the thread builds lazy built-ins of its own, then
    // goes away: what it built is garbage, not the outer run's.
    Nested := TGocciaEngine.Create(TEST_PATH, Lines);
    try
      Nested.EvaluateGlobalEval('Temporal.PlainDate.from("2026-01-02"); ' +
        'new Intl.NumberFormat("en").format(1); 0', TEST_PATH, Stage);
    finally
      Nested.Free;
    end;
    Value := Outer.EvaluateGlobalEval('({ a: 1 })', TEST_PATH, Stage);
    Session.AddToBaseline(Value);
    Session.CheckAccounting := True;
    Session.Snapshot([Value], Stream);
    Expect<Boolean>(Stream.Size > 0).ToBe(True);
  finally
    Session.Free;
    Outer.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotTests.TestRefusesChangedLateBuiltin;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // Built by the run, then changed: recorded as it was built, so the change
  // is a difference.
  Bytes := Snapshot('(() => { Temporal.PlainDate.prototype.extra = 1; ' +
    'return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes,
    'String(Temporal.PlainDate.prototype.extra)')).ToBe('1');
  // Built natively on first use, never seen as built: the restoring engine
  // builds it and compares.
  Bytes := Snapshot('(() => { Object.getPrototypeOf([][Symbol.iterator]())' +
    '.extra = 1; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<Boolean>(Pos('ArrayIterator', RestoreRefusal(Bytes)) > 0).ToBe(True);
end;

procedure THeapSnapshotTests.TestRegistersSymbolsOnlyOnSuccess;
var
  Refusal: string;
  Bytes: TBytes;
  Engine: TGocciaEngine;
  Lines: TStringList;
begin
  Bytes := Snapshot('({ s: Symbol.for("staged-key"), n: 1 })', Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    // Cut short after the symbol: refused, and the registry is as it was.
    Expect<Boolean>(RestoreRefusalIn(Engine,
      Copy(Bytes, 0, Length(Bytes) - 1)) <> '').ToBe(True);
    Expect<Boolean>(Assigned(FindRegisteredSymbol('staged-key'))).ToBe(False);
    Expect<string>(RestoreRefusalIn(Engine, Bytes)).ToBe('');
    Expect<Boolean>(Assigned(FindRegisteredSymbol('staged-key'))).ToBe(True);
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotTests.TestRefusesWithoutCodec;
var
  Refusal: string;
begin
  // Eval code has no bytecode another engine can match: its functions refuse.
  Snapshot('({ list: [1, { f: () => 1 }] })', Refusal);
  Expect<Boolean>(Pos('a function whose code is in no module the snapshot ' +
    'can name', Refusal) > 0).ToBe(True);
  // The chain names how the snapshot reached it.
  Expect<Boolean>(Pos('reached through', Refusal) > 0).ToBe(True);
  Snapshot('({ m: new Map() })', Refusal);
  Expect<Boolean>(Pos('no snapshot codec for', Refusal) > 0).ToBe(True);
  Snapshot('(() => { class L extends Array {} return new L(); })()', Refusal);
  Expect<Boolean>(Refusal <> '').ToBe(True);
end;

procedure THeapSnapshotTests.TestRefusesChangedBuiltin;
begin
  Expect<string>(RoundTrip('(() => { Object.prototype.polluted = 1; ' +
    'return {}; })()', 'String(({}).polluted)')).ToBe('1');
  Expect<string>(RoundTrip('(() => { Object.preventExtensions(Math); ' +
    'return {}; })()', 'String(Object.isExtensible(Math))')).ToBe('false');
  // Array.prototype is an array: its elements and length are its state too.
  Expect<string>(RoundTrip('(() => { Array.prototype[0] = 1; return {}; })()',
    'String(Array.prototype[0])')).ToBe('1');
  Expect<string>(RoundTrip('(() => { Array.prototype.length = 3; ' +
    'return {}; })()', 'String(Array.prototype.length)')).ToBe('3');
end;

procedure THeapSnapshotTests.TestAccountingRefusesMissedObject;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Root, Missed: TGocciaValue;
  Stream: TBytesStream;
  Refusal: string;
begin
  Refusal := '';
  Lines := TStringList.Create;
  Engine := nil;
  Session := nil;
  Stream := TBytesStream.Create;
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Root := Engine.EvaluateGlobalEval('({ a: 1 })', TEST_PATH, Stage);
    TGarbageCollector.Instance.AddTempRoot(Root);
    // Alive (a temp root) but reachable from nothing the snapshot writes.
    Missed := TGocciaObjectValue.Create;
    TGarbageCollector.Instance.AddTempRoot(Missed);
    try
      try
        Session.Snapshot([Root], Stream);
      except
        on E: EGocciaHeapSnapshotRefused do
          Refusal := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Missed);
      TGarbageCollector.Instance.RemoveTempRoot(Root);
    end;
  finally
    Stream.Free;
    Session.Free;
    Engine.Free;
    Lines.Free;
  end;
  Expect<Boolean>(Pos('live objects the snapshot does not hold', Refusal) > 0)
    .ToBe(True);
  Expect<Boolean>(Pos('TGocciaObjectValue=1', Refusal) > 0).ToBe(True);
end;

procedure THeapSnapshotTests.TestRefusesWhenCollectionQueuesWork;
var
  Outcome: string;
begin
  // At the checkpoint the registered target is garbage: the collection
  // before the snapshot queues the registry's cleanup job, and the run is
  // not idle any more.
  FCheckpoints := 0;
  FPauseAtCheckpoint := False;
  FModuleSource := 'const registry = new FinalizationRegistry(() => {}); ' +
    '(() => { registry.register({}, "held"); })(); ' +
    'const v = await hostCall(); globalThis.result = v;';
  try
    Expect<string>(RunHostCall(True, False, False, Outcome)).ToBe('');
  finally
    FModuleSource := '';
  end;
  Expect<Integer>(FCheckpoints).ToBe(1);
  Expect<Boolean>(Pos('queued a job', FSnapshotRefusal) > 0).ToBe(True);
end;

procedure THeapSnapshotTests.TestRejectsForeignInput;
var
  Refusal: string;
  Bytes, Changed: TBytes;
  I: Integer;
begin
  Bytes := Snapshot('({ a: [1, 2] })', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreRefusal(TEncoding.UTF8.GetBytes('{"not":"one"}')))
    .ToBe('restore: not a heap snapshot');
  Expect<string>(RestoreRefusal(nil)).ToBe('restore: truncated snapshot');
  // The format version follows the magic.
  Changed := Copy(Bytes);
  Changed[4] := Changed[4] + 1;
  Expect<Boolean>(Pos('format version', RestoreRefusal(Changed)) > 0)
    .ToBe(True);
  // The build identity's text follows the version and its length.
  Changed := Copy(Bytes);
  Changed[10] := Ord('#');
  Expect<Boolean>(Pos('written by engine build', RestoreRefusal(Changed)) > 0)
    .ToBe(True);
  // A codec's schema version: the 16-bit word after a codec's tag index.
  // Changing every schema word is easiest by rewriting the codec table, so
  // look for the refusal over a range of single-byte changes instead.
  for I := 0 to High(Bytes) do
  begin
    Changed := Copy(Bytes);
    Changed[I] := Changed[I] xor $40;
    if Pos('schema', RestoreRefusal(Changed)) > 0 then
      Break;
  end;
  Expect<Boolean>(I < High(Bytes)).ToBe(True);
end;

procedure THeapSnapshotTests.TestRejectsOverLimits;
var
  Refusal: string;
  Bytes: TBytes;
  Limits: TGocciaHeapSnapshotLimits;
begin
  Bytes := Snapshot('[{ a: 1 }, { b: 2 }, { c: "a longer string" }]',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  Limits := TGocciaHeapSnapshotLimits.Defaults;
  Limits.MaxObjects := 2;
  Expect<string>(RestoreRefusal(Bytes, Limits)).ToBe('restore: too many objects');
  Limits := TGocciaHeapSnapshotLimits.Defaults;
  Limits.MaxInputBytes := Length(Bytes) - 1;
  Expect<string>(RestoreRefusal(Bytes, Limits))
    .ToBe('restore: the snapshot is larger than the limit');
  Limits := TGocciaHeapSnapshotLimits.Defaults;
  Limits.MaxStringBytes := 4;
  Expect<string>(RestoreRefusal(Bytes, Limits))
    .ToBe('restore: a string longer than the limit');
  // Within the limits it restores.
  Expect<string>(RestoreAndCheck(Bytes, 'restored[2].c')).ToBe(
    'a longer string');
end;

procedure THeapSnapshotTests.TestRestoreChargesTheMemoryCeiling;
var
  Refusal: string;
  Bytes: TBytes;
  Engine: TGocciaEngine;
  Lines: TStringList;
  GC: TGarbageCollector;
  Previous: Int64;
begin
  // About 4 MiB of strings: a restore under a ceiling 1 MiB above what the
  // fresh engine holds is refused, not allowed past it.
  Bytes := Snapshot('Array.from({ length: 64 }, (_, i) => String(i)' +
    '.padEnd(65536, "x"))', Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  GC := TGarbageCollector.Instance;
  Previous := GC.MaxBytes;
  try
    // What earlier tests left is garbage; the ceiling is set over what is
    // live.
    GC.Collect;
    GC.MaxBytes := GC.BytesAllocated + 1024 * 1024;
    Refusal := RestoreRefusalIn(Engine, Bytes);
    // Under a ceiling the restore's own tables do not fit either: refused as
    // invalid all the same, never another exception.
    GC.MaxBytes := GC.BytesAllocated + 1;
    Expect<Boolean>(Pos('memory', RestoreRefusalIn(Engine, Bytes)) > 0)
      .ToBe(True);
  finally
    GC.MaxBytes := Previous;
    Engine.Free;
    Lines.Free;
  end;
  Expect<Boolean>(Pos('restore: ', Refusal) = 1).ToBe(True);
  Expect<Boolean>(Pos('memory', Refusal) > 0).ToBe(True);
  Expect<string>(RestoreAndCheck(Bytes, 'restored.length + "," + ' +
    'restored[63].length')).ToBe('64,65536');
end;

procedure THeapSnapshotTests.TestEveryTruncationRefuses;
var
  Refusal: string;
  Bytes: TBytes;
  Len, Refused: Integer;
  Engine: TGocciaEngine;
  Lines: TStringList;
begin
  Bytes := Snapshot('({ list: [1, "two", { three: 3n }], s: Symbol("s") })',
    Refusal);
  Expect<string>(Refusal).ToBe('');
  Refused := 0;
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    for Len := 0 to Length(Bytes) - 1 do
      if RestoreRefusalIn(Engine, Copy(Bytes, 0, Len)) <> '' then
        Inc(Refused);
    Expect<string>(RestoreRefusalIn(Engine, Bytes)).ToBe('');
  finally
    Engine.Free;
    Lines.Free;
  end;
  Expect<Integer>(Refused).ToBe(Length(Bytes));
end;

{$PUSH}{$Q-}{$R-}
procedure THeapSnapshotTests.TestMutantsRefuseOrRestore;
var
  Refusal: string;
  Bytes, Mutant: TBytes;
  Seed: Cardinal;
  I, J, Edits, Restored, Refused: Integer;
  Engine: TGocciaEngine;
  Lines: TStringList;

  function NextRandom: Cardinal;
  begin
    // xorshift32: the same mutants on every run.
    Seed := Seed xor (Seed shl 13);
    Seed := Seed xor (Seed shr 17);
    Seed := Seed xor (Seed shl 5);
    Result := Seed;
  end;

begin
  Bytes := Snapshot('(() => { const shared = { s: "shared" }; ' +
    'const o = { list: [1, , "two", 3n, shared, [shared]], ' +
    '[Symbol("k")]: Symbol.for("r"), m: Math, frozen: Object.freeze({ x: 1 }) }; ' +
    'o.self = o; return o; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Seed := $C0FFEE11;
  Restored := 0;
  Refused := 0;
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
  for I := 1 to MUTANTS do
  begin
    Mutant := Copy(Bytes);
    Edits := 1 + Integer(NextRandom mod 4);
    for J := 1 to Edits do
      case NextRandom mod 4 of
        0: Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
             Byte(NextRandom);
        1: Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
             Mutant[NextRandom mod Cardinal(Length(Mutant))] xor
             (1 shl (NextRandom mod 8));
        // A length or count set to its largest values.
        2: Mutant[NextRandom mod Cardinal(Length(Mutant))] := $FF;
        3: Mutant[NextRandom mod Cardinal(Length(Mutant))] := 0;
      end;
    // Every outcome other than a restore or a refusal propagates and fails
    // the test.
    if RestoreRefusalIn(Engine, Mutant) = '' then
      Inc(Restored)
    else
      Inc(Refused);
    // What the restores left behind is garbage; collect it now and then.
    if I mod 256 = 0 then
      TGarbageCollector.Instance.Collect;
  end;
  finally
    Engine.Free;
    Lines.Free;
  end;
  Expect<Integer>(Restored + Refused).ToBe(MUTANTS);
  Expect<Boolean>(Refused > 0).ToBe(True);
end;

procedure THeapSnapshotTests.TestVMStateMutantsRefuseOrResume;
const
  SOURCE = '(() => { let n = 0; const inc = () => { n += 1; return n; }; ' +
    'const log = []; const source = { *steps(start) { try { ' +
    'const a = yield start; const b = yield a * 2; return a + b + start; } ' +
    'finally { log.push(n); } } }; const g = source.steps(1); g.next(); ' +
    'g.next(10); inc(); capture({ inc, g, log }); })();';
  MUTANT_CHECK_TIMEOUT_MS = 1000;
  // Uses whatever restored, whatever a mutant made of it.
  CHECK = '(() => { const out = []; for (const step of [' +
    '() => restored.inc(), () => restored.g.next(5).value, ' +
    '() => restored.g.return(7).value, () => restored.g.next().done, ' +
    '() => restored.log.join()]) { try { out.push(String(step())); } ' +
    'catch (e) { out.push("threw"); } } return out.join("|"); })()';
var
  Refusal, Outcome: string;
  Bytes, Mutant: TBytes;
  Seed: Cardinal;
  I, J, Len, Edits, Restored, Refused: Integer;
  Engine: TGocciaEngine;
  Lines: TStringList;
  Capture: TCapture;
  Stage: TGocciaGlobalEvalStage;

  function NextRandom: Cardinal;
  begin
    Seed := Seed xor (Seed shl 13);
    Seed := Seed xor (Seed shr 17);
    Seed := Seed xor (Seed shl 5);
    Result := Seed;
  end;

  // '' when ABytes restored and the check ran, else the refusal.
  function RestoreAndResume(const ABytes: TBytes): string;
  var
    Restore: TGocciaHeapSnapshotRestore;
    Stream: TBytesStream;
  begin
    Result := '';
    Restore := nil;
    Stream := TBytesStream.Create(ABytes);
    try
      try
        Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream,
          TGocciaHeapSnapshotLimits.Defaults);
      except
        on E: EGocciaHeapSnapshotInvalid do
          Exit(E.Message);
      end;
      TGocciaObjectValue(Engine.Realm.GlobalObject).AssignProperty('restored',
        TGocciaValue(Restore.Roots[0]));
      // A mutant can make the check run long (an array whose length grew
      // to billions): the run's timeout ends it, as it would any script.
      StartExecutionTimeout(MUTANT_CHECK_TIMEOUT_MS);
      try
        try
          Outcome := Engine.EvaluateGlobalEval(CHECK, TEST_PATH,
            Stage).ToStringLiteral.Value;
        except
          on E: TGocciaTimeoutError do
            Outcome := 'timed out';
        end;
      finally
        ClearExecutionTimeout;
      end;
    finally
      Restore.Free;
      Stream.Free;
    end;
  end;

begin
  Bytes := SnapshotModule(SOURCE, Refusal);
  Expect<string>(Refusal).ToBe('');
  Lines := TStringList.Create;
  Capture := TCapture.Create;
  Engine := nil;
  Refused := 0;
  Restored := 0;
  try
    Lines.Text := SOURCE;
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Engine.GlobalScope.DefineLexicalBinding('capture',
      TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
    Engine.Execute;
    Expect<string>(RestoreAndResume(Bytes)).ToBe('');
    Expect<string>(Outcome).ToBe('2|16|7|true|2');
    for Len := 0 to Length(Bytes) - 1 do
      if RestoreAndResume(Copy(Bytes, 0, Len)) <> '' then
        Inc(Refused);
    Expect<Integer>(Refused).ToBe(Length(Bytes));
    Refused := 0;
    Seed := $5EED0C0D;
    for I := 1 to MUTANTS do
    begin
      Mutant := Copy(Bytes);
      Edits := 1 + Integer(NextRandom mod 4);
      for J := 1 to Edits do
        case NextRandom mod 4 of
          0: Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
               Byte(NextRandom);
          1: Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
               Mutant[NextRandom mod Cardinal(Length(Mutant))] xor
               (1 shl (NextRandom mod 8));
          2: Mutant[NextRandom mod Cardinal(Length(Mutant))] := $FF;
          3: Mutant[NextRandom mod Cardinal(Length(Mutant))] := 0;
        end;
      // A restore runs the check; anything but a restore or a refusal
      // propagates and fails the test.
      if RestoreAndResume(Mutant) = '' then
        Inc(Restored)
      else
        Inc(Refused);
      if I mod 256 = 0 then
        TGarbageCollector.Instance.Collect;
    end;
  finally
    if Assigned(Capture.Value) then
      TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Engine.Free;
    Capture.Free;
    Lines.Free;
  end;
  Expect<Integer>(Restored + Refused).ToBe(MUTANTS);
  Expect<Boolean>(Refused > 0).ToBe(True);
  Expect<Boolean>(Restored > 0).ToBe(True);
end;
{$POP}

procedure THeapSnapshotTests.OnCheckpoint(const AEngine: TGocciaEngine;
  var APause: Boolean);
var
  Stream: TBytesStream;
begin
  Inc(FCheckpoints);
  Expect<Boolean>(AEngine.IsAtPauseCheckpoint).ToBe(True);
  FCheckpointRefusal := AEngine.HeapSnapshotRefusal;
  FSnapshotRefusal := '';
  if Assigned(FSession) then
  begin
    Stream := TBytesStream.Create;
    try
      try
        FSession.Snapshot([], Stream);
      except
        on E: EGocciaHeapSnapshotRefused do
          FSnapshotRefusal := E.Message;
      end;
    finally
      Stream.Free;
    end;
  end;
  APause := FPauseAtCheckpoint;
end;

// Runs a module that awaits hostCall(). Returns the class name of the
// exception that ended it, or '' when it finished; AResult is what it stored.
function THeapSnapshotTests.RunHostCall(const AOwnerFirst, ABusyExtension,
  ANativeState: Boolean; out AResult: string): string;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  Owner: THostCallExtension;
  Busy: TBusyExtension;
begin
  Result := '';
  AResult := '';
  Source := TStringList.Create;
  Engine := nil;
  try
    if FModuleSource <> '' then
      Source.Text := FModuleSource
    else
      Source.Text := 'const v = await hostCall(); globalThis.result = v;';
    Engine := TGocciaEngine.Create(TEST_PATH, Source);
    Engine.SourceType := stModule;
    Owner := THostCallExtension.Create;
    Busy := nil;
    if ABusyExtension then
      Busy := TBusyExtension.Create;
    if AOwnerFirst then
    begin
      Engine.AddExtension(Owner);
      if Assigned(Busy) then
        Engine.AddExtension(Busy);
    end
    else
    begin
      if Assigned(Busy) then
        Engine.AddExtension(Busy);
      Engine.AddExtension(Owner);
    end;
    if ANativeState then
      Engine.AddExtension(TNativeStateExtension.Create);
    Engine.GlobalScope.DefineLexicalBinding('hostCall',
      TGocciaNativeFunctionValue.Create(Owner.HostCall, 'hostCall', 0),
      dtConst);
    Engine.HostCallOwner := Owner;
    Engine.OnPauseCheckpoint := OnCheckpoint;
    FSession := TGocciaHeapSnapshotSession.Create(Engine);
    try
      Engine.Execute;
      AResult := TGocciaObjectValue(Engine.Realm.GlobalObject)
        .GetProperty('result').ToStringLiteral.Value;
    except
      on E: Exception do
        Result := E.ClassName;
    end;
  finally
    FreeAndNil(FSession);
    Engine.Free;
    Source.Free;
  end;
end;

procedure THeapSnapshotTests.TestCheckpointPauses;
var
  Outcome: string;
begin
  FCheckpoints := 0;
  FPauseAtCheckpoint := True;
  Expect<string>(RunHostCall(True, False, False, Outcome))
    .ToBe(EGocciaRunPaused.ClassName);
  Expect<Integer>(FCheckpoints).ToBe(1);
  Expect<string>(FCheckpointRefusal).ToBe('');
end;

procedure THeapSnapshotTests.TestCheckpointDeclined;
var
  Outcome: string;
begin
  FCheckpoints := 0;
  FPauseAtCheckpoint := False;
  Expect<string>(RunHostCall(True, False, False, Outcome)).ToBe('');
  Expect<string>(Outcome).ToBe('42');
  Expect<Integer>(FCheckpoints).ToBe(1);
end;

procedure THeapSnapshotTests.TestNoCheckpointWhileOtherWorkPending;
var
  Outcome: string;
begin
  // The owner comes first, so the other extension still has work when the
  // owner would wait: no checkpoint, and the run finishes as usual.
  FCheckpoints := 0;
  FPauseAtCheckpoint := True;
  Expect<string>(RunHostCall(True, True, False, Outcome)).ToBe('');
  Expect<string>(Outcome).ToBe('42');
  Expect<Integer>(FCheckpoints).ToBe(0);
  // After it, the other extension's pass has run: a checkpoint.
  FCheckpoints := 0;
  Expect<string>(RunHostCall(False, True, False, Outcome))
    .ToBe(EGocciaRunPaused.ClassName);
  Expect<Integer>(FCheckpoints).ToBe(1);
end;

procedure THeapSnapshotTests.TestRefusesStateOutsideHeap;
var
  Outcome: string;
begin
  // Nothing is pending, but a snapshot would lose the extension's native
  // state: not a checkpoint, and the run goes on as usual.
  FCheckpoints := 0;
  FPauseAtCheckpoint := True;
  Expect<string>(RunHostCall(True, False, True, Outcome)).ToBe('');
  Expect<string>(Outcome).ToBe('42');
  Expect<Integer>(FCheckpoints).ToBe(0);
end;

begin
  TestRunnerProgram.AddSuite(THeapSnapshotTests.Create('Heap snapshots'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
