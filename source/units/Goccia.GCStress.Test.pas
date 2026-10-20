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
  quarantined object, or, for a window where the swept value is only read
  and never stored again, an invalid read under Valgrind memcheck.

  With GOCCIA_GC_STRESS_FREE=1 in the environment the sweeps free what they
  take instead of keeping it, so memcheck (or heaptrc's keepreleased) sees a
  read of a swept value directly; the scheduled memory-safety job runs the
  program that way under Valgrind. }

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
    procedure AddCase(const AName: string; const AMethod: TTestMethod);
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
    procedure TestErroredModuleImport;
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

{ GOCCIA_GC_STRESS_ONLY=<text> runs only the cases whose name contains it. }
procedure TGCStressTests.AddCase(const AName: string;
  const AMethod: TTestMethod);
var
  Only: string;
begin
  Only := GetEnvironmentVariable('GOCCIA_GC_STRESS_ONLY');
  if (Only = '') or (Pos(Only, AName) > 0) then
    Test(AName, AMethod);
end;

procedure TGCStressTests.SetupTests;
begin
  AddCase('engine creation', TestEngineCreation);
  AddCase('every lazy global and its members', TestLazyGlobals);
  AddCase('concrete iterator prototypes (EnsureConcreteIteratorPrototype)',
    TestIteratorPrototypes);
  AddCase('generator, async and async generator intrinsics',
    TestFunctionObjectIntrinsics);
  AddCase('Object.freeze, seal, isFrozen and isSealed key lists',
    TestIntegrityLevels);
  AddCase('defining a function''s name or length beside its intrinsic',
    TestIntrinsicNameAndLength);
  AddCase('a bound function''s length and name', TestBoundFunction);
  AddCase('decorator context objects', TestDecorators);
  AddCase('import.meta', TestImportMeta);
  AddCase('DOMException creation and structuredClone', TestDOMExceptionAndClone);
  AddCase('AbortSignal.abort and AbortSignal.timeout', TestAbortSignalStatics);
  AddCase('Headers and URLSearchParams forEach arguments',
    TestForEachArguments);
  AddCase('String replace and replaceAll replacer arguments',
    TestStringReplaceArguments);
  AddCase('RegExp species constructors and replacer captures',
    TestRegExpSpeciesAndCaptures);
  AddCase('Proxy traps a handler getter returns, and the stored value',
    TestProxyTraps);
  AddCase('Intl resolvedOptions and DisplayNames bootstrap',
    TestIntlResolvedOptions);
  AddCase('Intl.DurationFormat and Temporal.Duration toLocaleString',
    TestDurationFormat);
  AddCase('goccia:ast parse nodes', TestASTParse);
  AddCase('custom matcher hints and extractor results', TestCustomMatchers);
  AddCase('Response body promises', TestResponseBodies);
  AddCase('the goccia:semver namespace', TestSemverNamespace);
  AddCase('Array.prototype methods: receiver wrappers and results',
    TestArrayMethods);
  AddCase('Array.fromAsync with a this-constructor', TestArrayFromAsync);
  AddCase('Temporal until and since with a property bag and option getters',
    TestTemporalDifferences);
  AddCase('ToPrimitive with a fresh Symbol.toPrimitive method',
    TestToPrimitive);
  AddCase('Intl constructors with a newTarget', TestIntlConstructPrototype);
  AddCase('Intl Segments containing', TestSegmentsContaining);
  AddCase('DisposableStack errors across disposers', TestDisposalErrors);
  AddCase('async generator methods on a wrong receiver',
    TestAsyncGeneratorReceivers);
  AddCase('a second import of a module whose evaluation threw',
    TestErroredModuleImport);
end;

procedure TGCStressTests.BeginStress;
var
  GC: TGarbageCollector;
begin
  TGarbageCollector.Initialize;
  GC := TGarbageCollector.Instance;
  GC.Collect;
  TGarbageCollector.ResetQuarantineViolations;
  GC.QuarantineSwept := GetEnvironmentVariable('GOCCIA_GC_STRESS_FREE') <> '1';
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
  // A get trap's fresh result was held only by a local while the target's
  // descriptor was read, which runs the inner proxy's trap.
  RunStressed('proxy-get.js',
    'const sym = Symbol("s");' + LineEnding +
    'const inner = new Proxy({}, { getOwnPropertyDescriptor(t, k) {' + LineEnding +
    '  const junk = [String(k) + "!", { k }]; return junk.length > 9 ? undefined : Reflect.getOwnPropertyDescriptor(t, k); } });' + LineEnding +
    'const outer = new Proxy(inner, { get(t, k) { return { fresh: String(k) + "?" }; } });' + LineEnding +
    'const got = [outer.name, outer[sym], Reflect.get(outer, sym, {}), Reflect.get(outer, "other", {})];' + LineEnding +
    'got.map((v) => v.fresh).join();');
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
    // Each matcher allocates, so a collection reaches the hint object
    // through the arguments while they are still live.
    'const seen = [];' + LineEnding +
    'const Even = { [Symbol.customMatcher](subject, hint) {' + LineEnding +
    '  seen.push(hint.matchType + "!"); return subject % 2 === 0; } };' + LineEnding +
    'const Pair = { [Symbol.customMatcher](subject, hint) {' + LineEnding +
    '  seen.push(hint.matchType + "!");' + LineEnding +
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
  // Values read from getters, held only by locals while further getters,
  // setters or traps ran: pop and shift results, reverse's pair, copyWithin's
  // buffer, reduce's accumulator, and a primitive receiver's wrapper while
  // its length is coerced.
  RunStressed('array-intermediates.js',
    'const fresh = (tag) => ({ tag: tag + "!" });' + LineEnding +
    'const churn = (v) => [String(v) + "~", { v }];' + LineEnding +
    'const like = () => ({ get length() { return 3; }, set length(v) { churn(v); },' + LineEnding +
    '  get 0() { return fresh("a"); }, set 0(v) { churn(v); },' + LineEnding +
    '  get 1() { return fresh("b"); }, set 1(v) { churn(v); },' + LineEnding +
    '  get 2() { return fresh("c"); }, set 2(v) { churn(v); } });' + LineEnding +
    'const out = [];' + LineEnding +
    'out.push(Array.prototype.pop.call(like()), Array.prototype.shift.call(like()));' + LineEnding +
    'const stored = {}; const target = { get length() { return 4; }, set 0(v) { stored.zero = v; churn(v); },' + LineEnding +
    '  set 3(v) { stored.three = v; churn(v); }, get 0() { return fresh("low"); }, get 3() { return fresh("high"); },' + LineEnding +
    '  get 1() { return fresh("x"); }, get 2() { return fresh("y"); }, set 1(v) { stored.one = v; churn(v); },' + LineEnding +
    '  set 2(v) { stored.two = v; churn(v); } };' + LineEnding +
    'Array.prototype.reverse.call(target);' + LineEnding +
    'Array.prototype.copyWithin.call(target, 0, 1, 3);' + LineEnding +
    'out.push(stored);' + LineEnding +
    'const hasTrap = new Proxy([1, 2, 3], { has(t, k) { churn(k); return Reflect.has(t, k); } });' + LineEnding +
    'out.push(Array.prototype.reduce.call(hasTrap, (a, v) => ({ sum: (a.sum ?? a) + v }), 0));' + LineEnding +
    'out.push(Array.prototype.reduceRight.call(hasTrap, (a, v) => ({ sum: (a.sum ?? a) + v })));' + LineEnding +
    'Object.defineProperty(Number.prototype, "length", { configurable: true,' + LineEnding +
    '  get() { return { valueOf() { churn(this); return 2; } }; } });' + LineEnding +
    'out.push(Array.prototype.map.call(7, (v) => v));' + LineEnding +
    'delete Number.prototype.length;' + LineEnding +
    'out.length;');
  // The string an object operand's toString made was held only by a local
  // while the other operand became a string.
  RunStressed('addition.js',
    'const fn = (a) => a + 1;' + LineEnding +
    'const tagged = { toString() { return "tag" + "ged"; } };' + LineEnding +
    '[1.5 + fn, fn + 2.5, 3 + tagged, tagged + 4, 5n + String(tagged)].join("|").length;');
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
  // A duration made from a string or a property bag was held only by a local
  // while the overflow option's getter ran.
  RunStressed('temporal-add.js',
    'const churn = (v) => [String(v) + "~", { v }];' + LineEnding +
    'const options = { get overflow() { churn(1); return "constrain"; } };' + LineEnding +
    '[Temporal.PlainDate.from("2024-01-31").add({ months: 1, days: 2 }, options),' + LineEnding +
    ' Temporal.PlainDate.from("2024-01-31").subtract("P1M2D", options),' + LineEnding +
    ' Temporal.PlainDateTime.from("2024-01-31T10:00").add({ months: 1, hours: 3 }, options),' + LineEnding +
    ' Temporal.PlainYearMonth.from("2024-01").add("P1Y2M", options),' + LineEnding +
    ' Temporal.PlainYearMonth.from("2024-01").subtract({ years: 1 }, options),' + LineEnding +
    ' Temporal.ZonedDateTime.from("2024-01-31T10:00[UTC]").add({ months: 1, hours: 2 }, options),' + LineEnding +
    ' Temporal.ZonedDateTime.from("2024-01-31T10:00[UTC]").subtract("P1MT2H", options)].map(String);');
  // A field read from a property bag (a getter's fresh number) was held only
  // by a local while the next field's getter made a string.
  RunStressed('temporal-from-bag.js',
    'const date = Temporal.PlainDate.from("2024-03-15");' + LineEnding +
    'const hebrew = new Temporal.PlainDate(2024, 1, 1, "hebrew");' + LineEnding +
    '[Temporal.PlainMonthDay.from(date), Temporal.PlainYearMonth.from(date),' + LineEnding +
    ' Temporal.PlainMonthDay.from(hebrew), Temporal.PlainYearMonth.from(hebrew),' + LineEnding +
    ' Temporal.PlainMonthDay.from({ get month() { return 3 + 0; }, get monthCode() { return "M0" + "3"; },' + LineEnding +
    '   get day() { return 15 + 0; } })].map(String);');
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

// The loader kept an errored module's evaluation error in a map nothing
// marked, so a collection between two imports freed the error the second
// import rethrows.
procedure TGCStressTests.TestErroredModuleImport;
var
  Directory: string;
  Module: TStringList;
begin
  Directory := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('goccia-gc-stress-%d', [GetProcessID]);
  ForceDirectories(Directory);
  Module := TStringList.Create;
  try
    Module.Text := 'throw new Error("evaluation " + "failed");';
    Module.SaveToFile(IncludeTrailingPathDelimiter(Directory) + 'errored.js');
    Module.Text := 'throw new Error("deferred " + "failed");' + LineEnding +
      'export const unreached = true;';
    Module.SaveToFile(IncludeTrailingPathDelimiter(Directory) + 'deferred.js');
    // Nothing in the script keeps the first error, so between the two reads
    // only the loader's record of it does; the second one rethrows it.
    RunStressed(IncludeTrailingPathDelimiter(Directory) + 'main.mjs',
      'import defer * as ns from "./deferred.js";' + LineEnding +
      'const read = () => { try { return ns.unreached; } catch (e) { return e.message; } };' + LineEnding +
      'read();' + LineEnding +
      'let filler = ["a", "b", "c"].map((s) => s.repeat(3));' + LineEnding +
      'const deferred = read();' + LineEnding +
      // A finished async function's frame no longer holds the first error.
      'const attempt = async () => { try { await import("./errored.js"); return "loaded"; } catch { return "threw"; } };' + LineEnding +
      'const first = await attempt();' + LineEnding +
      'filler = filler.map((s) => s.repeat(2));' + LineEnding +
      'let message = "none";' + LineEnding +
      'try { await import("./errored.js"); } catch (again) { message = again.message; }' + LineEnding +
      'if (deferred !== "deferred failed") throw new Error("deferred: " + deferred);' + LineEnding +
      'if (first !== "threw") throw new Error("first import: " + first);' + LineEnding +
      'if (message !== "evaluation failed") throw new Error("second import: " + message);' + LineEnding +
      'globalThis.secondImport = [deferred, message, filler.length];');
  finally
    Module.Free;
    DeleteFile(IncludeTrailingPathDelimiter(Directory) + 'deferred.js');
    DeleteFile(IncludeTrailingPathDelimiter(Directory) + 'errored.js');
    RemoveDir(Directory);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TGCStressTests.Create('GC stress'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
  // Frees what the collector still manages, so a heaptrc build accounts for
  // every block (the memory-safety job checks it reports none unfreed).
  ShutdownMainThreadRuntime;
end.
