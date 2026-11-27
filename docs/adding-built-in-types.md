# Adding a New Built-in Type

*For contributors adding a new built-in type to the engine — a step-by-step recipe.*

## Executive Summary

- **Two ways to write it** — Native Pascal in a `source/engine` unit, or JavaScript in a prelude under `source/engine/prelude`. Use a prelude when the spec's steps matter more than speed; keep hot paths native.
- **Registration** — A language built-in is created by an Init procedure that `InitIntrinsics` (`Goccia.Engine`) calls for every realm. A host API (console, URL, `goccia:` modules) is a host prelude that `InstallHostGlobals` (`Goccia.HostAPI`) runs.
- **A new prelude file** — Add a row to `GOCCIA_PRELUDES` in `build.pas`, include the generated `.inc` in the unit that installs it, and compile it with `CompilePrelude` or `CallPrelude`.
- **Tests** — JavaScript tests under `tests/built-ins/<Type>/`, run with `GocciaTestRunner`; a GC stress run for native code that allocates; test262 for spec built-ins.
- **Checklist included** — Complete checklist at the end of the document for verification

This guide walks through adding a built-in type such as `Map`, `Date` or `WeakRef` to the engine under `source/engine`. Read [Value System](value-system.md) first: it describes `TValue`, objects, shapes and the rooting rules the code below follows.

## Overview

### Native or prelude

| | Native (`Goccia.<Unit>`) | Prelude (`source/engine/prelude/*.js`) |
|---|---|---|
| Written in | Pascal against `Goccia.Core` and `Goccia.VM` | JavaScript, by the spec's steps |
| Internal slots | the object's `OKind`, `Aux1`, `Aux2`, `Pad` | private fields of a class (`#slot`) |
| Brand check | `OKind` test | `#slot in o` |
| Speed | as fast as the engine | as fast as compiled JavaScript |
| Examples | `Goccia.Date`, `Goccia.Promise`, `Goccia.TypedArrays`, `Goccia.JSON` | `builtins5.js` (WeakMap, WeakRef, FinalizationRegistry), `temporal.js`, `intl.js`, `iterhelpers.js` |

The two mix: a prelude can receive natives for what JavaScript cannot do (`Goccia.Weak`'s ephemeron tables for WeakMap, `Goccia.Temporal`'s time zone reader, `Goccia.Intl`'s `intlOp`), and a native fast path can stand in for a prelude while the objects involved are unmodified (RegExp, `String.prototype.replace`; see [Engine design § Built-ins](engine/design.md#built-ins)).

### Engine built-in or host API

| What | Installed by | Where it is visible |
|------|--------------|---------------------|
| ECMA-262 / ECMA-402 built-ins and the proposals GocciaScript supports | an Init procedure listed in `InitIntrinsics` (`Goccia.Engine`) | every realm, every host, including the test262 host |
| Runtime APIs of the CLI hosts (console, URL, fetch, events, timers, `goccia:*` modules) | a host prelude called from `InstallHostGlobals` (`Goccia.HostAPI`) | the CLI applications' first realm |
| Test and benchmark APIs | `testing.js` and `microbench.js` through `InstallTestingAPI` and `InstallBenchmarkAPI` | GocciaTestRunner, GocciaBenchmarkRunner |

`InstallHostGlobals` has a `Bare` mode (GocciaScriptLoaderBare) that installs only the modules, web and host preludes; a runtime API stays out of it.

### Files a new type touches

| Step | File(s) | Purpose |
|------|---------|---------|
| 1 | `source/engine/Goccia.<Unit>.pas` or `source/engine/prelude/<name>.js` | The type |
| 2 | `source/engine/Goccia.Engine.pas` or `source/engine/Goccia.HostAPI.pas` | Registration |
| 3 | `build.pas` (`GOCCIA_PRELUDES`) | A new prelude file only |
| 4 | `source/engine/prelude/web.js`, `source/engine/Goccia.HeapSnapshot.pas` | structuredClone and heap snapshots, if they apply |
| 5 | `tests/built-ins/<Type>/` | JavaScript tests |
| 6 | `benchmarks/<type>.js` | Benchmarks |
| 7 | `docs/built-ins.md`, `README.md`, other `docs/*` as needed | Documentation |

## Step 1a: A Native Type

### Pick an object kind

An instance of a native type is a `TObj` whose `OKind` names its kind. `Goccia.Core` declares the core kinds (`okPlain` to `okModuleNS`); units that own a built-in declare theirs next to it (`okTypedArray` in `Goccia.Typed`, `okPromise` in `Goccia.Promise`, `okDate` in `Goccia.Date`, `okRawJSON` in `Goccia.JSON`, ...). Search for the existing values before taking one (`grep -rn "ok[A-Z][A-Za-z]* *= *[0-9]" source/engine`), and declare the same constant in every unit that tests for it.

Keep the instance's state in the fields the collector traces: `Aux1` and `Aux2` (a number, a string, a value array, another object). `Pad` holds 32 untraced bits. `Native` is an untraced pointer, for memory outside the heap; such memory is released from a collector hook (`GCFinalizeHook`, as `Goccia.Shared` releases SharedArrayBuffer blocks). Bit 8 of `OFlags` means "not extensible" for every object; check the other bits' uses before taking one for your kind.

For native storage owned by one engine, give its owner a zero-initialized `THeapExternalCharge` from `Goccia.Core`. Call `TryGrow(bytes)` before allocating. A failed reservation should return a memory-limit `RangeError`. Release the charge when the storage is freed, including when initialization fails. When a resize may hold both allocations during the copy, reserve the new size and release the old size. Root heap values before a reservation that can collect. A native holding a raw pointer into the moving heap can pass `False` for `ACanCollect` and refuse without running the collector. See [Memory Ceiling](garbage-collector.md#memory-ceiling).

### Write the natives

A native has the `TNativeFn` signature. `Args[0]` is `this`, `Args[1..Argc]` the arguments. It writes the result to `Res` and returns `True`, or returns `False` with the exception in `Pending` (`ThrowTypeError`, `ThrowError`). Under `new`, `CurNewTarget` holds new.target.

```pascal
unit Goccia.Foo;
{ Foo objects: a counter in Aux1. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

procedure InitFoo;

implementation

uses
  SysUtils, Goccia.Core, Goccia.VM;

const
  okFoo = 99;   { an object kind no other unit uses }

threadvar
  FooProtoV: TValue;   { %Foo.prototype% of the running realm }

function Foo_Ctor(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var D: Double; P: TValue; O: PObj;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor Foo requires ''new'''));
  PushRoot(CurNewTarget);
  D := 0;
  if (Argc >= 1) and not ToNumberV(Args[1], D) then begin PopRoots(1); Exit(False); end;
  { the conversion may have run JavaScript: new.target back from its root }
  CurNewTarget := RootAt(TempTop - 1);
  PopRoots(1);
  { OrdinaryCreateFromConstructor (ES2026 §10.1.13) after the argument conversions }
  if not ProtoFromNewTarget(FooProtoV, P) then Exit(False);
  O := NewObject(RootShapeFor(P), 0, okFoo);
  O^.Aux1 := MkNumber(D);   { a number: no write barrier needed }
  Res := MkPtr(O);
  Result := True;
end;

function ThisFoo(Args: PValue; var O: PObj): Boolean;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okFoo) then
    Exit(ThrowTypeError('Foo.prototype method called on an incompatible receiver'));
  O := ObjOf(Args[0]);
  Result := True;
end;

function FooProto_Increment(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O: PObj;
begin
  if not ThisFoo(Args, O) then Exit(False);
  O^.Aux1 := MkNumber(NumOf(O^.Aux1) + 1);
  Res := O^.Aux1;
  Result := True;
end;

function FooProto_GetCount(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O: PObj;
begin
  if not ThisFoo(Args, O) then Exit(False);
  Res := O^.Aux1;
  Result := True;
end;
```

Rules the natives follow:

- **Brand-check `this`** through `OKind`, never assume it. A method can be called on any value with `call`.
- **Root across allocation and calls.** Anything that allocates or runs JavaScript (`GetV`, `CallValue`, `ToNumberV`, `ToStringV`, `NewObject`, `DefineOwn`, ...) can move young cells. `Args` is on the value stack and stays valid; read `Args[0]` again after such a call. Keep other values on the temporary root stack (`PushRoot`, `RootAt`, `PopRoots`) and pop what you push on every exit.
- **Write barrier.** A pointer stored straight into `Aux1` or `Aux2` of an object that may be old needs `WriteBarrier(@O^.Hdr, V)`. `SetSlot`, `AddProperty` and `DefineOwn` do it themselves.
- **Spec operations through the routines.** Use `GetV`, `SetVT`, `GetIteratorRec`, `IteratorStepRec`, `IteratorCloseV`, `ToPropKey`, `ToObjectV`, `DefinePropertyOrThrow` and the like, so proxies, accessors and user-defined `valueOf` behave as the spec says.
- **One native, several methods.** `NewNativeFunctionData` stores a small integer in the function, which the native reads from `NativeData`.
- **Loops the script sizes.** A loop that runs as often as a script-controlled length says, without calls or back edges, polls `PollInterrupt` each step so timeouts and the memory limit still apply.

### Write the Init procedure

The Init procedure builds the prototype and constructor on the running realm's global object:

```pascal
procedure InitFoo;
var C: TValue;
begin
  AddRoot(@FooProtoV);
  RealmVals([@FooProtoV]);
  FooProtoV := MkPtr(NewPlainObject(ObjectProto, 8));
  C := NewNativeFunction(Foo_Ctor, 'Foo', 1);
  PushRoot(C);
  DefineOwn(RootAt(TempTop - 1), AtomPrototype, FooProtoV, 0);
  DefineOwn(FooProtoV, AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf('Foo'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
  DefineNative(FooProtoV, 'increment', FooProto_Increment, 0);
  DefineNativeGetterKey(FooProtoV, AtomOf('count'), 'get count', FooProto_GetCount);
  DefineOwn(FooProtoV, SymKey(SymToStringTagW), MkPtr(AtomOf('Foo')), pfConfigurable);
end;
```

- **Realm state.** Every threadvar that holds a value is a root (`AddRoot`) and, when it belongs to the realm (a prototype, a cached intrinsic), a realm slot (`RealmVals`; `RealmPtrs` and `RealmInts` for other types). `CreateRealm` runs the Init procedures again for each new realm; `AddRoot` and `RealmVals` ignore the repeats. State that must be set up once per engine instead uses `FirstInEngine`.
- **Attributes.** `pfHidden` (writable, configurable, not enumerable) for methods and the constructor's global binding; `0` for the constructor's `prototype`; `pfConfigurable` for `Symbol.toStringTag`.
- **Built-in constructor.** A native given a `prototype` property while the intrinsics are built is marked a built-in constructor, which `new` and `IsConstructorV` rely on.
- **`Symbol.toStringTag`.** `Object.prototype.toString` reads it through the prototype chain; without it the type prints as `[object Object]`.

## Step 1b: A Prelude Type

### Inside an existing prelude

When the type belongs with one that exists, add it to that prelude. `builtins5.js` is the engine's general prelude: one function called with the global object and the natives it needs, and its intrinsics captured at the top so later changes to the globals are not observed. A built-in class there looks like this (from `WeakMap`):

```javascript
class WeakMap {
  #t = tableNew();   // the internal slot: a private field
  static #check(o, name) {
    if (!isObject(o) || !(#t in o)) throw new TypeErr("WeakMap.prototype." + name + " called on an incompatible receiver");
  }
  has(key) { WeakMap.#check(this, "has"); if (!canHoldWeakly(key)) return false; return tableHas(this.#t, key); }
  // ...
}
defineProperty(WeakMap.prototype, Symbol.toStringTag, { value: "WeakMap", writable: false, enumerable: false, configurable: true });
markIntrinsic(WeakMap);
def(G, "WeakMap", WeakMap);
```

- **`markIntrinsic`** marks the class a built-in, so a construction whose new.target has no object `prototype` falls back to the class's own prototype, as a built-in constructor's does.
- **Non-enumerable globals.** `def` defines the global writable, configurable and not enumerable, as built-ins are.
- **Captured intrinsics.** Call only what the prelude captured when it ran, never a global or a prototype method a script can replace; see [Captured intrinsics](#captured-intrinsics).
- **Native source.** A prelude's functions keep no source text: `Function.prototype.toString` gives them native-function syntax.
- **ASCII only.** The build turns each prelude into a Pascal string constant and fails on a non-ASCII character; write `\u` escapes instead.
- **The whole language.** Preludes compile under the unrestricted language profile, whatever the host's compatibility flags.

### Captured intrinsics

A prelude runs before any script, so what it captures then is the realm's own. A built-in it defines looks up
nothing a script can replace at call time, except where the specification says the lookup is observable (a
species constructor, a user iterator's `next`, `Get(options, "calendar")`), as an ECMA-262 algorithm calls abstract
operations rather than observable methods.

- **Globals.** Bind each global a function body uses at the top: `const { TypeError, String } = G;`, or a
  renamed local (`var TypeErr = TypeError`). Capture static functions too (`var ObjectKeys = Object.keys`,
  `MathFloor = Math.floor`): `Math.floor(x)` reads `floor` from the live `Math`.
- **Prototype methods.** Capture each one uncurried and call it with the receiver first:
  `var uncurryThis = Function.prototype.bind.bind(Function.prototype.call);`
  `var StringPrototypeSlice = uncurryThis(String.prototype.slice);` then `StringPrototypeSlice(s, 1, 2)`. Getters
  the same way (`uncurryThis(getOwnPropertyDescriptor(Map.prototype, 'size').get)`), including a typed array's
  `length`, `buffer` and `byteOffset`. `ReflectApply(f, this, args)` replaces `f.call` and `f.apply`. The VM and the
  baseline JIT call an uncurried method as fast as a method call ([Bytecode VM](bytecode-vm.md)).
- **Lists.** Append with `defineProperty` (an index setter on `Array.prototype` must not see the value), or make a
  list that never reaches a script without a prototype (`setPrototypeOf([], null)`) and store by index, which is as
  fast as `push`. Do not call
  `map`, `filter`, `slice`, `concat`, `splice` or `flat` on a list the prelude made: they look up the array's
  `constructor` and `Symbol.species`; write the loop. The same holds for a typed array's `slice` and `subarray`
  and an `ArrayBuffer`'s `slice`.
- **Iteration.** No `for...of`, spread or array destructuring over a list, string, `Map` or `Set` the prelude
  made: each looks up `Symbol.iterator` and `next` on the live prototypes. Index loops, `MapPrototypeForEach`, and
  `ReflectApply(f, undefined, args)` instead of `f(...args)`. Keep the protocol where the specification iterates a
  value the caller passed (Web IDL sequences, `GetIterator`).
- **RegExp.** `RegExpPrototypeExec(re, s)` (RegExpBuiltinExec) for a pattern the prelude made; `test`, `match`,
  `replace` and `split` look up `exec`, `flags` and the `Symbol` methods.
- **Promises.** `await` a promise rather than calling its `then`, as the specification's Await does.

`tests/built-ins/prelude-intrinsics` holds a tripwire test for each prelude: `installTripwire()` turns every
configurable property of the global object, of each intrinsic it reaches and of the hidden iterator prototypes into
an accessor that records its own lookup, and `invoke` / `construct` / `covers` run a built-in and return those
lookups. A test allows only what the specification makes observable for its input, and `uncovered(owner)` requires
a case for each method and getter. `scripts/check-test-structure.ts` fails when a prelude has no tripwire test.

### A new prelude file

1. Create `source/engine/prelude/<name>.js` holding one parenthesized function expression. The file compiles as an expression; its value is the function the installer calls:

   ```javascript
   // Foo: what it implements and which natives it is called with.
   (function (G, n, shared) {
     'use strict';
     // ...
   })
   ```

2. Add a row to `GOCCIA_PRELUDES` in `build.pas` and raise the array's upper bound by one: the file name, the include it becomes and the constant it defines.

   ```pascal
   ('foo.js', 'Goccia.FooPrelude.inc', 'FooSrc'),
   ```

   `GenerateEnginePreludes` writes `build/engine/gen/Goccia.FooPrelude.inc` on every engine build, with the source as the constant `FooSrc`.

3. Include it in the implementation section of the unit that installs it: `{$I Goccia.FooPrelude.inc}`.

4. Install it.
   - **Engine prelude.** In an Init procedure listed in `InitIntrinsics`: `CompilePrelude` (`Goccia.Snapshot`) compiles the source, `MakeScriptClosure` and `CallValue` evaluate it to the function, and a second `CallValue` calls it with its arguments. Raise an `Exception` on failure: a prelude that does not run is an engine bug. `InitBuiltins5` (`Goccia.Builtins5`) is the full example; the short form:

     ```pascal
     procedure InitFoo;
     var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue; A: array[0..1] of TValue; Ok: Boolean;
     begin
       if not CompilePrelude('<foo>', FooSrc, T, Err) then
         raise Exception.Create('foo prelude: ' + string(Err));
       if not MakeScriptClosure(T, Clo) or not CallValue(Clo, VUndefined, nil, 0, Fn) then
         raise Exception.Create('foo prelude run');
       PushRoot(Fn);
       PushRoot(NewNativeFunction(MarkIntrinsicClass, 'markIntrinsic', 1));
       A[0] := GlobalObj;
       A[1] := RootAt(TempTop - 1);
       Ok := CallValue(RootAt(TempTop - 2), VUndefined, @A[0], 2, R);
       PopRoots(2);
       if not Ok then raise Exception.Create('foo prelude call');
     end;
     ```

   - **Host prelude.** Add a `CallPrelude('<goccia:foo>', FooSrc, Base, 3, R, Err)` to the chain in `InstallHostGlobals`, at the point in the install order where what it uses exists. Host preludes take `(G, n, shared)`: the global object, the host's natives (`HostNatives`, where a new native is one more `DefineNative` line) and the preludes' shared internals. A global it installs is added to `shared.runtimeGlobals`, which the `runtimeGlobals` list on the `Goccia` global reports; a module is registered as `shared.hostModules['goccia:foo'] = () => exports`.

5. Build. The build compiles each program once, runs it to snapshot the preludes it compiles (`Goccia.Snapshot`, `SnapshotEnginePreludes` in `build.pas`) and compiles it again with their bytecode embedded, so start-up does not parse them. `CompilePrelude` takes a snapshot entry only while the prelude's source still matches it, and otherwise parses the source.

## Step 2: Registration

### Engine built-ins: `Goccia.Engine`

Add the unit to `Goccia.Engine`'s implementation `uses` clause and call its Init procedure from `InitIntrinsics`. The order matters: an Init procedure can read only the intrinsics made before it (`InitBuiltins` makes `Object`, `Function`, `Array` and the well-known symbols first; the preludes in `InitBuiltins5` and later capture what exists when they run). `InitIntrinsics` is also the realm initializer (`RealmInitProc`), so the same call covers ShadowRealm and `$262.createRealm`.

On Linux x86-64, `GOCCIA_REALMCHECK=<size of the TLS segment>` makes `CreateRealm` report the thread-local bytes a new realm's Init procedures changed that switching back did not restore: realm state that no `RealmVals` call registered.

### Host APIs: `Goccia.HostAPI`

A host prelude is registered in `InstallHostGlobals` as above. A new realm of a CLI host gets the intrinsics, then `CLIRealmInit`; host preludes run only in the host's own realm.

## Step 3: structuredClone and Heap Snapshots

- **structuredClone** is `web.js`'s. It refuses an object of a native kind it does not know (`plainCloneable` accepts only `okPlain`, `okArray` and `okArgumentsOrd`) and an instance of any prelude class, with a `DataCloneError`. A cloneable type needs a branch in `structuredClone`'s `clone` function, brand-checked through an intrinsic captured at the top of the section, before the plain-object case.
- **Heap snapshots** (`Goccia.HeapSnapshot`) refuse every object kind without a codec, which `GocciaTestRunner --heap-snapshots` reports as refused, not failed. Add the kind to `KindName` so the refusal names it.

## Step 4: Tests

Create test files under `tests/built-ins/YourType/`. Follow the file layout conventions from [testing.md](testing.md#file-naming-and-layout-conventions):

- **One method per file** — each prototype method, static method, and constructor variant gets its own test file.
- **Prototype methods in `prototype/`** — instance methods live in `YourType/prototype/methodName.js`.
- **Static methods at the top level** — `YourType/staticMethod.js` (no separate `static/` folder).
- **Edge cases are co-located** — NaN handling, boundary conditions, error cases belong in the same file as the happy-path tests for that method. Do **not** create a separate `edge-cases.js`.

```text
tests/built-ins/YourType/
  constructor.js           # new YourType(...) constructor variants + error cases
  toString-tag.js          # Symbol.toStringTag
  from.js, of.js           # Static methods at top level
  prototype/               # Instance methods — one file per method
    methodA.js             # Happy paths + edge cases for methodA
    methodB.js             # Happy paths + edge cases for methodB
```

Each file uses the built-in test framework:

```javascript
describe("YourType constructor", () => {
  test("creates an instance", () => {
    const obj = new YourType();
    expect(obj instanceof YourType).toBe(true);
  });

  test("throws TypeError without new", () => {
    expect(() => YourType()).toThrow(TypeError);
  });

  test("subclasses get their own prototype", () => {
    class Sub extends YourType {}
    expect(Object.getPrototypeOf(new Sub())).toBe(Sub.prototype);
  });
});
```

Cover what a native type tends to get wrong: methods called on the wrong receiver, subclassing, a constructor's argument conversions that run user code (`valueOf`), and `Object.prototype.toString`. If the type supports structuredClone, add `tests/built-ins/structuredClone/yourtype.js`.

Run them:

```bash
./build.pas testrunner && ./build/GocciaTestRunner -P tests/built-ins/YourType
```

- **GC stress** — For native code that allocates, run the tests with a 16 KB nursery and heap verification after every collection: `./build.pas engine && python3 scripts/engine/gcstress-tests.py build/engine/goccia 8 tests/built-ins/YourType`. A file reported `BAD` reads a value an allocation moved or misses a root.
- **test262** — For an ECMA-262 or ECMA-402 built-in, run its test262 directory ([test262](test262.md)). CI expects both the suite and test262 to pass in full: any failure fails the job.

## Step 5: Benchmarks

Create `benchmarks/yourtype.js`:

```javascript
/*---
description: YourType operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("YourType creation", () => {
  bench("create YourType", () => {
    const obj = new YourType();
  });
});

group("YourType methods", () => {
  bench("method call", ({ *setup() {
    const obj = new YourType();
    yield () => {
      obj.method();
    };
  } }).setup);
});
```

Run it with `./build.pas benchmarkrunner && ./build/GocciaBenchmarkRunner benchmarks/yourtype.js` ([Benchmarks](benchmarks.md)).

## Step 6: Documentation

### `docs/built-ins.md`

Add a section with method tables:

```markdown
### YourType

| Method/Property | Description |
|--------|-------------|
| `new YourType(args)` | Create a new instance |
| `yourType.method()` | Description |
| `yourType.property` | Description |
```

### `README.md`

Add `YourType` to the built-in objects list.

### Other documentation

- Update [docs/built-ins.md](built-ins.md) and [README.md](../README.md) as needed.
- If you introduce a new major engine layer or file-level component worth listing, update [docs/architecture.md](architecture.md) (or the doc index in [CONTRIBUTING.md](../CONTRIBUTING.md)) instead of maintaining a separate table elsewhere.

## Checklist

Use this checklist when adding a new built-in type:

- [ ] Native: an object kind no other unit uses, declared in each unit that tests it
- [ ] Native: state in `Aux1` / `Aux2` (traced) or `Pad`; nothing collectible in `Native`
- [ ] Native: budgeted storage is charged before allocation and uncharged when freed, including on failure
- [ ] Native: every method brand-checks `this`; values rooted across allocation and calls; write barrier on direct pointer stores
- [ ] Native: Init procedure with `AddRoot` and `RealmVals` for each threadvar it fills
- [ ] Native: Init procedure called from `InitIntrinsics` in `Goccia.Engine`, after what it reads
- [ ] Prelude: internal slots as private fields, `markIntrinsic` on the class, intrinsics captured, ASCII only
- [ ] New prelude file: `GOCCIA_PRELUDES` row and bound in `build.pas`, `{$I}` of the generated include, installed by an Init procedure or `InstallHostGlobals`
- [ ] `Symbol.toStringTag` on the prototype
- [ ] structuredClone branch in `web.js` (if cloneable); `KindName` entry in `Goccia.HeapSnapshot` (native kinds)
- [ ] JavaScript tests in `tests/built-ins/YourType/`
- [ ] GC stress run over the new tests (native code that allocates)
- [ ] test262 for the feature (spec built-ins)
- [ ] Benchmarks in `benchmarks/yourtype.js`
- [ ] Documentation in `docs/built-ins.md`, `README.md`, and other docs as needed (see [CONTRIBUTING.md](../CONTRIBUTING.md))
- [ ] All existing tests still pass

## Common Pitfalls

1. **A stale `TValue` after an allocation** — A pointer read into a local before `NewObject`, `GetV` or `CallValue` may point at the cell's old place. Root it or re-read it. The GC stress run finds these.
2. **Heap values in untraced fields** — `Native` and `Pad` are not traced: a value kept there is freed or moved under it.
3. **A missing write barrier** — Storing a young object into an old object's `Aux1` without `WriteBarrier` hides it from the next minor collection, which then moves it and leaves `Aux1` pointing at its old place.
4. **Unregistered realm state** — A threadvar the Init procedure fills but does not pass to `RealmVals` is not saved and restored by `SwitchRealm`, so every realm sees the value the newest realm's Init procedure left there. `GOCCIA_REALMCHECK` finds these.
5. **Calling through replaceable globals in a prelude** — `Array.prototype.push`, `Math.floor`, a `for...of` over an array or `re.test(s)` read at call time observe a script's changes. Capture what the prelude calls when it runs ([Captured intrinsics](#captured-intrinsics)); the prelude's tripwire test fails on any lookup it missed.
6. **Forgetting `markIntrinsic`** — Without it, `Reflect.construct(PreludeClass, [], newTargetWithoutPrototype)` gets `Object.prototype` instead of the class's prototype.
7. **Exceptions for JavaScript errors** — A native signals a throw by returning `False` with `Pending` set, never by raising a Pascal exception.
8. **Formatting** — The engine's Pascal is outside `./format.pas`; follow the style of the surrounding unit.
