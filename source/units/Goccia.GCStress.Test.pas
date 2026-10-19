program Goccia.GCStress.Test;

{ Engine creation, the lazy built-in initialisers and the native construction
  paths, run with a full collection before every reservation
  (TGarbageCollector.CollectBeforeEveryReservation) and with the objects each
  sweep takes kept in quarantine (QuarantineSwept). In an ordinary run a
  collection lands at a reservation only near the --max-memory ceiling; here
  it lands at every one, so a value that native code holds only in a local
  while it makes another string, buffer or object is swept at once. Kept in
  quarantine rather than freed, it fails the test the moment a later
  collection reaches it through whatever stored it, naming its class, in any
  build (issue #1816).

  Each case names the window it pins: before its fix, the case reported a
  quarantined object. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.AST,
  Goccia.RuntimeProfiles.Loader,
  Goccia.TestSetup,
  Goccia.Threading,
  Goccia.Threading.Init;

type
  TGCStressTests = class(TTestSuite)
  private
    procedure BeginStress;
    procedure EndStress(const AWhat: string);
    function CreateStressedEngine(const AFileName: string;
      const ASource: TStringList): TGocciaEngine;
    procedure RunStressed(const AFileName, ASource: string);
    procedure ExpectNoQuarantinedReached(const AWhat: string);
  public
    procedure SetupTests; override;

    procedure TestEngineCreation;
    procedure TestLazyGlobals;
    procedure TestIteratorPrototypes;
    procedure TestFunctionObjectIntrinsics;
    procedure TestIntegrityLevels;
    procedure TestIntrinsicNameAndLength;
    procedure TestBoundFunction;
    procedure TestDecorators;
    procedure TestImportMeta;
    procedure TestDOMExceptionAndClone;
    procedure TestAbortSignalStatics;
    procedure TestForEachArguments;
    procedure TestStringReplaceArguments;
    procedure TestRegExpSpeciesAndCaptures;
    procedure TestProxyTraps;
    procedure TestIntlResolvedOptions;
    procedure TestDurationFormat;
    procedure TestASTParse;
    procedure TestCustomMatchers;
    procedure TestResponseBodies;
    procedure TestSemverNamespace;
    procedure TestArrayMethods;
    procedure TestArrayFromAsync;
    procedure TestTemporalDifferences;
    procedure TestToPrimitive;
    procedure TestIntlConstructPrototype;
    procedure TestSegmentsContaining;
    procedure TestDisposalErrors;
    procedure TestAsyncGeneratorReceivers;
  end;

const
  { Reads every global binding, which materializes every lazy one (shims,
    Temporal, Intl, the disposable stacks, ...), and then the members of each
    constructor and namespace, which builds their prototypes. }
  LAZY_GLOBALS_SOURCE =
    'const seen = [];' + LineEnding +
    'for (const name of Object.getOwnPropertyNames(globalThis)) {' + LineEnding +
    '  const value = globalThis[name];' + LineEnding +
    '  seen.push(value);' + LineEnding +
    '  if (value !== null && (typeof value === "object" || typeof value === "function")) {' + LineEnding +
    '    for (const key of Reflect.ownKeys(value)) {' + LineEnding +
    '      const descriptor = Reflect.getOwnPropertyDescriptor(value, key);' + LineEnding +
    '      seen.push(descriptor);' + LineEnding +
    '    }' + LineEnding +
    '    const proto = value.prototype;' + LineEnding +
    '    if (proto !== null && typeof proto === "object") {' + LineEnding +
    '      for (const key of Reflect.ownKeys(proto)) seen.push(key);' + LineEnding +
    '    }' + LineEnding +
    '  }' + LineEnding +
    '}' + LineEnding +
    'seen.length;';

procedure TGCStressTests.SetupTests;
begin
  Test('engine creation', TestEngineCreation);
  Test('every lazy global and its members', TestLazyGlobals);
  Test('concrete iterator prototypes (EnsureConcreteIteratorPrototype)',
    TestIteratorPrototypes);
  Test('generator, async and async generator intrinsics',
    TestFunctionObjectIntrinsics);
  Test('Object.freeze, seal, isFrozen and isSealed key lists',
    TestIntegrityLevels);
  Test('defining a function''s name or length beside its intrinsic',
    TestIntrinsicNameAndLength);
  Test('a bound function''s length and name', TestBoundFunction);
  Test('decorator context objects', TestDecorators);
  Test('import.meta', TestImportMeta);
  Test('DOMException creation and structuredClone', TestDOMExceptionAndClone);
  Test('AbortSignal.abort and AbortSignal.timeout', TestAbortSignalStatics);
  Test('Headers and URLSearchParams forEach arguments',
    TestForEachArguments);
  Test('String replace and replaceAll replacer arguments',
    TestStringReplaceArguments);
  Test('RegExp species constructors and replacer captures',
    TestRegExpSpeciesAndCaptures);
  Test('Proxy traps a handler getter returns, and the stored value',
    TestProxyTraps);
  Test('Intl resolvedOptions and DisplayNames bootstrap',
    TestIntlResolvedOptions);
  Test('Intl.DurationFormat and Temporal.Duration toLocaleString',
    TestDurationFormat);
  Test('goccia:ast parse nodes', TestASTParse);
  Test('custom matcher hints and extractor results', TestCustomMatchers);
  Test('Response body promises', TestResponseBodies);
  Test('the goccia:semver namespace', TestSemverNamespace);
  Test('Array.prototype methods: receiver wrappers and results',
    TestArrayMethods);
  Test('Array.fromAsync with a this-constructor', TestArrayFromAsync);
  Test('Temporal until and since with a property bag and option getters',
    TestTemporalDifferences);
  Test('ToPrimitive with a fresh Symbol.toPrimitive method',
    TestToPrimitive);
  Test('Intl constructors with a newTarget', TestIntlConstructPrototype);
  Test('Intl Segments containing', TestSegmentsContaining);
  Test('DisposableStack errors across disposers', TestDisposalErrors);
  Test('async generator methods on a wrong receiver',
    TestAsyncGeneratorReceivers);
end;

procedure TGCStressTests.BeginStress;
var
  GC: TGarbageCollector;
begin
  TGarbageCollector.Initialize;
  GC := TGarbageCollector.Instance;
  GC.Collect;
  TGarbageCollector.ResetQuarantineViolations;
  GC.QuarantineSwept := True;
  GC.CollectBeforeEveryReservation := True;
end;

procedure TGCStressTests.ExpectNoQuarantinedReached(const AWhat: string);
begin
  if TGarbageCollector.QuarantineViolations > 0 then
    Fail(Format('%s: %d swept object(s) reached again, the first a %s ' +
      'swept by collection %d',
      [AWhat, TGarbageCollector.QuarantineViolations,
       TGarbageCollector.FirstQuarantineViolation,
       TGarbageCollector.FirstQuarantineViolationSweptIn]));
  Expect<Integer>(TGarbageCollector.QuarantineViolations).ToBe(0);
end;

procedure TGCStressTests.EndStress(const AWhat: string);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  GC.CollectBeforeEveryReservation := False;
  // Frees what the sweeps kept.
  GC.QuarantineSwept := False;
  ExpectNoQuarantinedReached(AWhat);
end;

{ The engine with the runtime the loader and the test runner give it (fetch,
  URL, structuredClone, the goccia:* modules, Goccia.ast), all built under
  stress too. }
function TGCStressTests.CreateStressedEngine(const AFileName: string;
  const ASource: TStringList): TGocciaEngine;
var
  Runtime: TGocciaRuntimeCore;
begin
  Result := TGocciaEngine.Create(AFileName, ASource);
  try
    Runtime := AttachRuntime(Result);
    ApplyLoaderRuntimeProfile(Runtime);
    Runtime.Install(TGocciaASTRuntimeExtension.Create);
  except
    Result.Free;
    raise;
  end;
end;

{ Creates the engine and runs ASource under stress, then collects once more
  while the engine is alive, so a swept object something stored at the end is
  reached too. }
procedure TGCStressTests.RunStressed(const AFileName, ASource: string);
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := nil;
  BeginStress;
  try
    Source.Text := ASource;
    Engine := CreateStressedEngine(AFileName, Source);
    Engine.Execute;
    TGarbageCollector.Instance.Collect;
  finally
    Engine.Free;
    Source.Free;
    EndStress(AFileName);
  end;
end;

// #1816: GetThrowTypeErrorIntrinsic, the builtins' constructor fields, the
// shared prototypes' member collections and the type definitions.
procedure TGCStressTests.TestEngineCreation;
begin
  RunStressed('engine-creation.js', '1;');
end;

procedure TGCStressTests.TestLazyGlobals;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := nil;
  BeginStress;
  try
    Source.Text := LAZY_GLOBALS_SOURCE;
    Engine := CreateStressedEngine('lazy-globals.js', Source);
    // The globals the test runner warms on every worker, then everything.
    WarmUpSharedLazyGlobals(Engine);
    Engine.Execute;
    TGarbageCollector.Instance.Collect;
  finally
    Engine.Free;
    Source.Free;
    EndStress('lazy globals');
  end;
end;

// #1816: EnsureConcreteIteratorPrototype held the new prototype only in a local.
procedure TGCStressTests.TestIteratorPrototypes;
begin
  RunStressed('iterators.js',
    'const out = [];' + LineEnding +
    'out.push([1, 2].values().next(), [1, 2].keys().next(), [1, 2].entries().next());' + LineEnding +
    'out.push(new Map([[1, "a"]]).entries().next(), new Set(["a"]).values().next());' + LineEnding +
    'out.push("ab"[Symbol.iterator]().next(), new Uint8Array(2).entries().next());' + LineEnding +
    'out.push(/a/g[Symbol.matchAll]("aa").next());' + LineEnding +
    'out.push(Iterator.from([1, 2, 3]).map((x) => x * 2).filter((x) => x > 2).toArray());' + LineEnding +
    'out.push(Iterator.concat([1], new Set([2])).toArray());' + LineEnding +
    'out.push([...new Intl.Segmenter("en").segment("a b")]);' + LineEnding +
    'out.length;');
end;

procedure TGCStressTests.TestFunctionObjectIntrinsics;
begin
  RunStressed('function-objects.js',
    'const o = { *gen() { yield 1; }, async *agen() { yield 2; } };' + LineEnding +
    'const f = async () => 3;' + LineEnding +
    'const out = [o.gen().next(), o.agen(), f(), Object.getPrototypeOf(o.gen)];' + LineEnding +
    'out.length;');
end;

// OwnPropertyKeyValues and Freeze/Seal/TestIntegrity* held fresh key strings
// only in a Pascal array.
procedure TGCStressTests.TestIntegrityLevels;
begin
  RunStressed('integrity.js',
    'const make = () => ({ alpha: 1, beta: 2, gamma: 3, delta: 4, [Symbol("s")]: 5 });' + LineEnding +
    'const a = Object.freeze(make());' + LineEnding +
    'const b = Object.seal(make());' + LineEnding +
    'const c = Object.preventExtensions(make());' + LineEnding +
    '[Object.isFrozen(a), Object.isSealed(b), Object.isFrozen(c), Object.isSealed(c),' + LineEnding +
    ' Object.isFrozen(new Proxy(make(), {}))];');
end;

// MaterializeIntrinsicProperty allocated the intrinsic name or length while
// the caller's pending descriptor or value was held only by the caller.
procedure TGCStressTests.TestIntrinsicNameAndLength;
begin
  RunStressed('intrinsics.js',
    'const f = () => 1;' + LineEnding +
    'Object.defineProperty(f, "name", { value: "renamed-" + "x".repeat(8) });' + LineEnding +
    'Object.defineProperty(f, "length", { value: 42 });' + LineEnding +
    'class K {}' + LineEnding +
    'Object.defineProperty(K, "name", { value: "K-" + "y".repeat(8) });' + LineEnding +
    '[f.name, f.length, K.name];');
end;

// InitializeBoundFunctionProperties made the name string while only the
// unregistered bound function held the length number.
procedure TGCStressTests.TestBoundFunction;
begin
  RunStressed('bound.js',
    'const named = (a, b) => a + b;' + LineEnding +
    'const out = [named.bind(null), named.bind(null, 1), named.bind(null, 1, 2)];' + LineEnding +
    'out.map((f) => f.name + f.length);');
end;

procedure TGCStressTests.TestDecorators;
begin
  RunStressed('decorators.js',
    'const seen = [];' + LineEnding +
    'const element = (value, context) => { seen.push(context.kind, context.name); };' + LineEnding +
    'const accessor = (value, context) => ({ get() { return 1; }, set(v) {}, init(v) { return v; } });' + LineEnding +
    'const whole = (cls, context) => { seen.push(context.kind, context.name); };' + LineEnding +
    '@whole' + LineEnding +
    'class C {' + LineEnding +
    '  @element method() {}' + LineEnding +
    '  @element get value() { return 1; }' + LineEnding +
    '  @element field = 1;' + LineEnding +
    '  @accessor accessor auto = 2;' + LineEnding +
    '}' + LineEnding +
    'seen.length;');
end;

// GetOrCreateImportMeta held the meta object in a local while it made the
// url string.
procedure TGCStressTests.TestImportMeta;
begin
  RunStressed('import-meta.mjs',
    'const meta = import.meta;' + LineEnding +
    'const out = [meta.url, meta.resolve("./other.js")];' + LineEnding +
    'export default out;');
end;

// CreateDOMExceptionObject, CreatePermissionDeniedError and CloneDOMException
// made the stack or the capability strings while only a local held the error.
procedure TGCStressTests.TestDOMExceptionAndClone;
begin
  RunStressed('dom-exception.js',
    'const e = new DOMException("message " + "x".repeat(4), "AbortError");' + LineEnding +
    'const clone = structuredClone(e);' + LineEnding +
    'let thrown = null;' + LineEnding +
    'try { structuredClone(() => 1); } catch (error) { thrown = error; }' + LineEnding +
    '[e.name, clone.message, thrown.name];');
end;

procedure TGCStressTests.TestAbortSignalStatics;
begin
  RunStressed('abort.js',
    'const a = AbortSignal.abort();' + LineEnding +
    'const b = AbortSignal.timeout(1000);' + LineEnding +
    '[a.aborted, a.reason.name, b.aborted];');
end;

// The open-array argument lists made the name string after the value string
// with nothing holding the value.
procedure TGCStressTests.TestForEachArguments;
begin
  RunStressed('foreach.js',
    'const out = [];' + LineEnding +
    'new Headers({ "x-one": "first value", "x-two": "second value" }).forEach((v, k) => out.push(k + v));' + LineEnding +
    'new URLSearchParams("alpha=one&beta=two").forEach((v, k) => out.push(k + v));' + LineEnding +
    'out;');
end;

procedure TGCStressTests.TestStringReplaceArguments;
begin
  RunStressed('replace.js',
    '[' + LineEnding +
    '  "hello world".replace("world", (m, p, s) => m + p + s),' + LineEnding +
    '  "abc".replaceAll("", (m, p, s) => "[" + p + s + "]"),' + LineEnding +
    '];');
end;

procedure TGCStressTests.TestRegExpSpeciesAndCaptures;
begin
  RunStressed('regexp.js',
    'class R extends RegExp {}' + LineEnding +
    'const out = [];' + LineEnding +
    'out.push("a-b-c".split(new R("-")));' + LineEnding +
    'out.push([...new R("(b)", "g")[Symbol.matchAll]("abab")]);' + LineEnding +
    'out.push("xaybzc".replace(/([a-c])/g, (m, c1, p) => c1.toUpperCase() + p));' + LineEnding +
    'out.push(/a/gi.flags);' + LineEnding +
    'out.length;');
end;

// The traps a handler getter returns fresh, and a value the VM boxed for a
// store, were held only in locals while the trap's arguments were made.
procedure TGCStressTests.TestProxyTraps;
begin
  RunStressed('proxy.js',
    'const handler = {' + LineEnding +
    '  get set() { return (t, k, v) => { t[k] = v; return true; }; },' + LineEnding +
    '  get defineProperty() { return (t, k, d) => Reflect.defineProperty(t, k, d); },' + LineEnding +
    '  get get() { return (t, k) => t[k]; },' + LineEnding +
    '  get has() { return (t, k) => k in t; },' + LineEnding +
    '  get ownKeys() { return (t) => Reflect.ownKeys(t); },' + LineEnding +
    '};' + LineEnding +
    'const p = new Proxy({}, handler);' + LineEnding +
    'let n = 1.5;' + LineEnding +
    'p.first = n * 2;' + LineEnding +
    'Object.defineProperty(p, "second", { value: n * 3, writable: true, configurable: true, enumerable: true });' + LineEnding +
    'const child = Object.create(p);' + LineEnding +
    'child.third = n * 4;' + LineEnding +
    '[p.first, p.second, "first" in p, Object.keys(p).length, child.third];');
end;

procedure TGCStressTests.TestIntlResolvedOptions;
begin
  RunStressed('intl.js',
    '[' + LineEnding +
    '  new Intl.DisplayNames("en", { type: "region" }).resolvedOptions(),' + LineEnding +
    '  new Intl.RelativeTimeFormat("en").resolvedOptions(),' + LineEnding +
    '  new Intl.DurationFormat("en").resolvedOptions(),' + LineEnding +
    '  new Intl.Collator("en").resolvedOptions(),' + LineEnding +
    '  new Intl.ListFormat("en").resolvedOptions(),' + LineEnding +
    '];');
end;

procedure TGCStressTests.TestDurationFormat;
begin
  RunStressed('duration.js',
    'const d = { hours: 1, minutes: 2, seconds: 3, milliseconds: 4 };' + LineEnding +
    '[' + LineEnding +
    '  new Intl.DurationFormat("en", { style: "digital" }).format(d),' + LineEnding +
    '  new Intl.DurationFormat("en", { style: "long" }).formatToParts(d),' + LineEnding +
    '  Temporal.Duration.from({ hours: 2, minutes: 30 }).toLocaleString("en"),' + LineEnding +
    '];');
end;

// MaterializeNode filled a node before its parent attached it.
procedure TGCStressTests.TestASTParse;
begin
  RunStressed('ast.mjs',
    'import { parse } from "goccia:ast";' + LineEnding +
    'const tree = parse("const answer = (a, b) => a + b * 2; // note");' + LineEnding +
    'export default [tree.root.children.length, tree.comments.length];');
end;

procedure TGCStressTests.TestCustomMatchers;
begin
  RunStressed('matchers.js',
    'const Even = { [Symbol.customMatcher](subject, hint) { return subject % 2 === 0; } };' + LineEnding +
    'const Pair = { [Symbol.customMatcher](subject, hint) {' + LineEnding +
    '  return subject.kind === "pair" ? [subject.left, subject.right] : false; } };' + LineEnding +
    'let sum = 0;' + LineEnding +
    'if ({ kind: "pair", left: 2, right: 3 } is Pair(const left, const right)) sum = left + right;' + LineEnding +
    '[4 is Even, 5 is Even, sum];');
end;

procedure TGCStressTests.TestResponseBodies;
begin
  RunStressed('response.js',
    '[' + LineEnding +
    '  new Response("plain body text").text(),' + LineEnding +
    '  new Response("{\"a\": [1, 2, \"three\"]}").json(),' + LineEnding +
    '  new Response("bytes").arrayBuffer(),' + LineEnding +
    '];');
end;

// CreateSemverNamespace held its prototypes and namespace objects only in
// locals and the host's plain fields while it built them.
procedure TGCStressTests.TestSemverNamespace;
begin
  RunStressed('semver.mjs',
    'import * as semver from "goccia:semver";' + LineEnding +
    'const out = [semver.parse("1.2.3-beta.1"), semver.satisfies("1.2.3", "^1.0.0"),' + LineEnding +
    '  new semver.Range(">=1.0.0 <2.0.0").test("1.5.0")];' + LineEnding +
    'export default out;');
end;

// The receiver wrapper of a primitive this and the result were held only by
// locals while getters, proxy traps and callbacks ran.
procedure TGCStressTests.TestArrayMethods;
begin
  RunStressed('array-methods.js',
    'const out = [];' + LineEnding +
    'const spreadable = { length: 2, 0: "a" + "b", 1: "c" + "d",' + LineEnding +
    '  get [Symbol.isConcatSpreadable]() { return true; } };' + LineEnding +
    'out.push([1].concat(spreadable, Object("xy")));' + LineEnding +
    'out.push(Array.prototype.map.call("text", (c) => c + "!"));' + LineEnding +
    'out.push(Array.prototype.filter.call("text", (c) => c !== "e"));' + LineEnding +
    'out.push(Array.prototype.join.call("abc", "-"));' + LineEnding +
    'out.push([[1, [2]], new Proxy([3, [4]], {})].flat(2));' + LineEnding +
    'out.push(new Proxy([1, 2, 3, 4], {}).slice(1, 3));' + LineEnding +
    'out.push([1, 2, 3].with(1, "x"), [1, 2, 3].toReversed(), [3, 1, 2].toSorted());' + LineEnding +
    'const spliced = [1, 2, 3, 4]; out.push(spliced.splice(1, 2, "a", "b"));' + LineEnding +
    'out.push(Array.prototype.reduce.call("abc", (a, c) => a + c, ""));' + LineEnding +
    'out.push(Array.prototype.indexOf.call("abc", "b"), Array.prototype.at.call("abc", -1));' + LineEnding +
    'out.length;');
end;

// The iterator @@iterator returned was held only by locals while the
// this-constructor ran.
procedure TGCStressTests.TestArrayFromAsync;
begin
  RunStressed('from-async.js',
    'class Holder { constructor() { this.made = "made " + "here"; } }' + LineEnding +
    'const p1 = Array.fromAsync.call(Holder, [1, Promise.resolve(2), 3]);' + LineEnding +
    'const p2 = Array.fromAsync({ length: 2, 0: "a", 1: "b" }, (v) => v + v);' + LineEnding +
    'const p3 = Array.fromAsync(new Set(["x", "y"]).values());' + LineEnding +
    '[p1, p2, p3];');
end;

// A property bag coerced to a fresh value was held only by a local while the
// options getters ran.
procedure TGCStressTests.TestTemporalDifferences;
begin
  RunStressed('temporal-diff.js',
    'const options = { get largestUnit() { return "day" + ""; },' + LineEnding +
    '  get smallestUnit() { return "minute"; }, get roundingMode() { return "halfExpand"; } };' + LineEnding +
    'const a = Temporal.PlainDateTime.from("2024-01-01T00:00");' + LineEnding +
    'const bag = { year: 2024, month: 3, day: 5, hour: 7 };' + LineEnding +
    '[a.until(bag, options), a.since(bag, options),' + LineEnding +
    ' Temporal.PlainDate.from("2024-01-01").until({ year: 2024, month: 2, day: 1 },' + LineEnding +
    '   { get largestUnit() { return "month" + ""; } }),' + LineEnding +
    ' Temporal.PlainTime.from("01:00").since({ hour: 3 }, { get largestUnit() { return "hour"; } })];');
end;

// A fresh Symbol.toPrimitive method, and the input it converts, were held
// only by locals while the hint string was made.
procedure TGCStressTests.TestToPrimitive;
begin
  RunStressed('to-primitive.js',
    'const make = () => ({ get [Symbol.toPrimitive]() { return (hint) => hint + "!"; } });' + LineEnding +
    'const out = [make() + "", `${make()}`, +{ valueOf() { return 4; } }];' + LineEnding +
    'out.push(new Float64Array([3, 1, 2]).sort((a, b) => ({ [Symbol.toPrimitive]() { return a - b; } })));' + LineEnding +
    'out.length;');
end;

// The new instance was held only by its constructor while newTarget's
// prototype was looked up.
procedure TGCStressTests.TestIntlConstructPrototype;
begin
  RunStressed('intl-construct.js',
    'class Sub extends Intl.Collator {}' + LineEnding +
    '[Reflect.construct(Intl.NumberFormat, ["en"], Sub), new Sub("en"),' + LineEnding +
    ' Reflect.construct(Intl.Segmenter, [], Intl.Segmenter)];');
end;

// containing() freed a managed iterator it had not rooted, which a
// collection could already have swept.
procedure TGCStressTests.TestSegmentsContaining;
begin
  RunStressed('segments.js',
    'const segments = new Intl.Segmenter("en", { granularity: "word" }).segment("hello brave new world");' + LineEnding +
    '[segments.containing(0), segments.containing(7), segments.containing(99)];');
end;

procedure TGCStressTests.TestDisposalErrors;
begin
  RunStressed('disposal.js',
    'const stack = new DisposableStack();' + LineEnding +
    'stack.defer(() => { throw new Error("first " + "error"); });' + LineEnding +
    'stack.defer(() => { throw new Error("second " + "error"); });' + LineEnding +
    'stack.defer(() => { throw new Error("third " + "error"); });' + LineEnding +
    'let caught = null;' + LineEnding +
    'try { stack.dispose(); } catch (error) { caught = error; }' + LineEnding +
    '[caught.name, caught.suppressed.name];');
end;

procedure TGCStressTests.TestAsyncGeneratorReceivers;
begin
  RunStressed('async-generators.js',
    'const proto = Object.getPrototypeOf(({ async *g() {} }).g.prototype);' + LineEnding +
    '[proto.next.call({}), proto.return.call(1), proto.throw.call(null)]' + LineEnding +
    '  .map((p) => p.catch((error) => error.name));');
end;

begin
  TestRunnerProgram.AddSuite(TGCStressTests.Create('GC stress'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
  // Frees what the collector still manages, so a heaptrc build accounts for
  // every block (the memory-safety job checks it reports none unfreed).
  ShutdownMainThreadRuntime;
end.
