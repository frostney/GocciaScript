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
  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.Scope.BindingMap,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue;

const
  TEST_PATH = '<heap-snapshot-test>';
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
    function Snapshot(const ASource: string; out ARefusal: string;
      const AAccounting: Boolean = False): TBytes;
    function RestoreAndCheck(const ABytes: TBytes; const ACheck: string;
      const ALimits: TGocciaHeapSnapshotLimits): string; overload;
    function RestoreAndCheck(const ABytes: TBytes;
      const ACheck: string): string; overload;
    function RestoreRefusal(const ABytes: TBytes;
      const ALimits: TGocciaHeapSnapshotLimits): string; overload;
    function RestoreRefusal(const ABytes: TBytes): string; overload;
    // In an engine the caller keeps for many restores.
    function RestoreRefusalIn(const AEngine: TGocciaEngine;
      const ABytes: TBytes): string;
    function RoundTrip(const ASource, ACheck: string): string;
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
    function ChainRoundTrip(const ALength: Integer): string;
    procedure TestChain;
    procedure TestDeepChain;
    procedure TestDeepNestingAndPrototypes;
    procedure TestLoneSurrogates;
    procedure TestRootsNeedNoRootingByTheCaller;
    procedure TestRefusesReplacedLazyMember;
    procedure TestRefusesPrototypeCycle;
    procedure TestRefusesWithoutCodec;
    procedure TestRefusesChangedBuiltin;
    procedure TestAccountingRefusesMissedObject;
    procedure TestRefusesWhenCollectionQueuesWork;
    procedure TestRejectsForeignInput;
    procedure TestRejectsOverLimits;
    procedure TestRestoreChargesTheMemoryCeiling;
    procedure TestEveryTruncationRefuses;
    procedure TestMutantsRefuseOrRestore;
    procedure TestCheckpointPauses;
    procedure TestCheckpointDeclined;
    procedure TestNoCheckpointWhileOtherWorkPending;
    procedure TestRefusesStateOutsideHeap;
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
  Test('A lazy member the run replaced, or a primitive it changed, refuses',
    TestRefusesReplacedLazyMember);
  Test('A snapshot whose prototypes loop is refused',
    TestRefusesPrototypeCycle);
  Test('A live object with no codec refuses the snapshot',
    TestRefusesWithoutCodec);
  Test('A built-in the run changed refuses the snapshot',
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
  Test('A run waiting only on host calls pauses at the checkpoint',
    TestCheckpointPauses);
  Test('A declined checkpoint goes on waiting as usual',
    TestCheckpointDeclined);
  Test('No checkpoint while another extension has pending work',
    TestNoCheckpointWhileOtherWorkPending);
  Test('State outside the heap no codec covers means no checkpoint',
    TestRefusesStateOutsideHeap);
end;

// Builds a graph with ASource in a fresh engine and writes what it returns;
// ARefusal is the refusal's message, or '' when the snapshot was taken.
function THeapSnapshotTests.Snapshot(const ASource: string;
  out ARefusal: string; const AAccounting: Boolean): TBytes;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  Stream: TBytesStream;
begin
  Result := nil;
  ARefusal := '';
  Lines := TStringList.Create;
  Engine := nil;
  Session := nil;
  Stream := TBytesStream.Create;
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Session.CheckAccounting := AAccounting;
    Value := Engine.EvaluateGlobalEval(ASource, TEST_PATH, Stage);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Session.AddToBaseline(Value);
      try
        Session.Snapshot([Value], Stream);
        Result := Copy(Stream.Bytes, 0, Stream.Size);
      except
        on E: EGocciaHeapSnapshotRefused do
          ARefusal := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
  finally
    Stream.Free;
    Session.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

function THeapSnapshotTests.RestoreAndCheck(const ABytes: TBytes;
  const ACheck: string; const ALimits: TGocciaHeapSnapshotLimits): string;
var
  Engine: TGocciaEngine;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Stream: TBytesStream;
  Stage: TGocciaGlobalEvalStage;
begin
  Lines := TStringList.Create;
  Engine := nil;
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream, ALimits);
    TGocciaObjectValue(Engine.Realm.GlobalObject).AssignProperty('restored',
      TGocciaValue(Restore.Roots[0]));
    Result := Engine.EvaluateGlobalEval(ACheck, TEST_PATH,
      Stage).ToStringLiteral.Value;
  finally
    Restore.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

function THeapSnapshotTests.RestoreAndCheck(const ABytes: TBytes;
  const ACheck: string): string;
begin
  Result := RestoreAndCheck(ABytes, ACheck, TGocciaHeapSnapshotLimits.Defaults);
end;

// The refusal's message, or '' when the input restored.
function THeapSnapshotTests.RestoreRefusal(const ABytes: TBytes;
  const ALimits: TGocciaHeapSnapshotLimits): string;
var
  Engine: TGocciaEngine;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Stream: TBytesStream;
begin
  Result := '';
  Lines := TStringList.Create;
  Engine := nil;
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    try
      Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream, ALimits);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Result := E.Message;
    end;
  finally
    Restore.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

function THeapSnapshotTests.RestoreRefusal(const ABytes: TBytes): string;
begin
  Result := RestoreRefusal(ABytes, TGocciaHeapSnapshotLimits.Defaults);
end;

function THeapSnapshotTests.RestoreRefusalIn(const AEngine: TGocciaEngine;
  const ABytes: TBytes): string;
var
  Restore: TGocciaHeapSnapshotRestore;
  Stream: TBytesStream;
begin
  Result := '';
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    try
      Restore := TGocciaHeapSnapshotRestore.Create(AEngine, Stream,
        TGocciaHeapSnapshotLimits.Defaults);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Result := E.Message;
    end;
  finally
    Restore.Free;
    Stream.Free;
  end;
end;

function THeapSnapshotTests.RoundTrip(const ASource, ACheck: string): string;
var
  Refusal: string;
  Bytes: TBytes;
begin
  Bytes := Snapshot(ASource, Refusal);
  if Refusal <> '' then
    Exit('refused: ' + Refusal);
  Result := RestoreAndCheck(Bytes, ACheck);
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
begin
  // Building a lazy member is not a change...
  Snapshot('(() => { Temporal.PlainDate; return {}; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  // ...replacing one is, before it was built or after.
  Snapshot('(() => { Object.defineProperty(globalThis, "Temporal", ' +
    '{ value: {}, configurable: true, writable: true }); return {}; })()',
    Refusal);
  Expect<Boolean>(Pos('the run changed built-ins', Refusal) > 0).ToBe(True);
  Snapshot('(() => { Temporal.PlainDate; ' +
    'Object.defineProperty(globalThis, "Temporal", { value: {}, ' +
    'configurable: true, writable: true }); return {}; })()', Refusal);
  Expect<Boolean>(Pos('the run changed built-ins', Refusal) > 0).ToBe(True);
  // A primitive changed in place, true to false.
  Snapshot('(() => { Array.prototype[Symbol.unscopables].at = false; ' +
    'return {}; })()', Refusal);
  Expect<Boolean>(Pos('the run changed built-ins', Refusal) > 0).ToBe(True);
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
  Count, I: Cardinal;

  function U32At(const AAt: Integer): Cardinal;
  begin
    Move(Bytes[AAt], Result, 4);
    Result := LEtoN(Result);
  end;

begin
  Bytes := Snapshot('(() => { const b = {}; const a = Object.create(b); ' +
    'return [a, b]; })()', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, String('Object.getPrototypeOf(' +
    'restored[0]) === restored[1]'))).ToBe('true');
  Pos_ := 4 + 2;
  Pos_ := Pos_ + 4 + 2 * Integer(U32At(Pos_));       // build identity
  Count := U32At(Pos_);                                // strings
  Inc(Pos_, 4);
  for I := 1 to Count do
    Pos_ := Pos_ + 4 + 2 * Integer(U32At(Pos_));
  Pos_ := Pos_ + 4 + 6 * Integer(U32At(Pos_));       // codecs
  Pos_ := Pos_ + 4 + 4 * Integer(U32At(Pos_));       // built-in names
  Count := U32At(Pos_);                                // objects
  Inc(Pos_, 4);
  Expect<Integer>(Integer(Count)).ToBe(3);
  // Objects 0 (the array) and 1 (a): skip their entries.
  for I := 1 to 2 do
    Pos_ := Pos_ + 2 + 4 + Integer(U32At(Pos_ + 2));
  // b is an object literal: its payload is its literal flag, then its
  // prototype, a built-in reference.
  Inc(Pos_, 2 + 4 + 1);
  Expect<Integer>(Bytes[Pos_]).ToBe(HSV_BUILTIN);
  Bytes[Pos_] := HSV_OBJECT;
  Bytes[Pos_ + 1] := 1;
  Bytes[Pos_ + 2] := 0;
  Bytes[Pos_ + 3] := 0;
  Bytes[Pos_ + 4] := 0;
  Expect<string>(RestoreRefusal(Bytes))
    .ToBe('restore: a prototype chain that loops');
end;

procedure THeapSnapshotTests.TestRefusesWithoutCodec;
var
  Refusal: string;
begin
  Snapshot('({ list: [1, { f: () => 1 }] })', Refusal);
  Expect<Boolean>(Pos('no snapshot codec for TGocciaBytecodeFunctionValue',
    Refusal) > 0).ToBe(True);
  // The chain names how the snapshot reached it.
  Expect<Boolean>(Pos('reached through', Refusal) > 0).ToBe(True);
  Snapshot('({ m: new Map() })', Refusal);
  Expect<Boolean>(Pos('no snapshot codec for', Refusal) > 0).ToBe(True);
  Snapshot('(() => { class L extends Array {} return new L(); })()', Refusal);
  Expect<Boolean>(Refusal <> '').ToBe(True);
end;

procedure THeapSnapshotTests.TestRefusesChangedBuiltin;
var
  Refusal: string;
begin
  Snapshot('(() => { Object.prototype.polluted = 1; return {}; })()',
    Refusal);
  Expect<Boolean>(Pos('the run changed built-ins', Refusal) > 0).ToBe(True);
  Snapshot('(() => { Object.preventExtensions(Math); return {}; })()',
    Refusal);
  Expect<Boolean>(Pos('the run changed built-ins', Refusal) > 0).ToBe(True);
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
