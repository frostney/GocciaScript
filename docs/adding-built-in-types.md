# Adding a New Built-in Type

*For contributors adding a new built-in type to the engine — a step-by-step recipe.*

## Executive Summary

- **Two ways to write it** — Native Pascal in a `source/g2` unit, or JavaScript in a prelude under `source/g2/prelude`. Use a prelude when the spec's steps matter more than speed; keep hot paths native.
- **Registration** — A language built-in is created by an Init procedure that `InitIntrinsics` (`G2.Engine`) calls for every realm. A host API (console, URL, `goccia:` modules) is a host prelude that `InstallHostGlobals` (`G2.HostAPI`) runs.
- **A new prelude file** — Add a row to `G2_PRELUDES` in `build.pas`, include the generated `.inc` in the unit that installs it, and compile it with `CompilePrelude` or `CallPrelude`.
- **Tests** — JavaScript tests under `tests/built-ins/<Type>/`, run with `GocciaTestRunner`; a GC stress run for native code that allocates; test262 for spec built-ins.
- **Checklist included** — Complete checklist at the end of the document for verification

This guide walks through adding a built-in type such as `Map`, `Date` or `WeakRef` to G2, the engine under `source/g2`. Read [Value System](value-system.md) first: it describes `TValue`, objects, shapes and the rooting rules the code below follows.

## Overview

### Native or prelude

| | Native (`G2.<Unit>`) | Prelude (`source/g2/prelude/*.js`) |
|---|---|---|
| Written in | Pascal against `G2.Core` and `G2.VM` | JavaScript, by the spec's steps |
| Internal slots | the object's `OKind`, `Aux1`, `Aux2`, `Pad` | private fields of a class (`#slot`) |
| Brand check | `OKind` test | `#slot in o` |
| Speed | as fast as the engine | as fast as compiled JavaScript |
| Examples | `G2.Date`, `G2.Promise`, `G2.TypedArrays`, `G2.JSON` | `builtins5.js` (WeakMap, WeakRef, FinalizationRegistry), `temporal.js`, `intl.js`, `iterhelpers.js` |

The two mix: a prelude can receive natives for what JavaScript cannot do (`G2.Weak`'s ephemeron tables for WeakMap, `G2.Temporal`'s time zone reader, `G2.Intl`'s `intlOp`), and a native fast path can stand in for a prelude while the objects involved are unmodified (RegExp, `String.prototype.replace`; see [G2 design § Built-ins](g2/design.md#built-ins)).

### Engine built-in or host API

| What | Installed by | Where it is visible |
|------|--------------|---------------------|
| ECMA-262 / ECMA-402 built-ins and the proposals GocciaScript supports | an Init procedure listed in `InitIntrinsics` (`G2.Engine`) | every realm, every host, including the test262 host |
| Runtime APIs of the CLI hosts (console, URL, fetch, events, timers, `goccia:*` modules) | a host prelude called from `InstallHostGlobals` (`G2.HostAPI`) | the CLI applications' first realm |
| Test and benchmark APIs | `testing.js` and `microbench.js` through `InstallTestingAPI` and `InstallBenchmarkAPI` | GocciaTestRunner, GocciaBenchmarkRunner |

`InstallHostGlobals` has a `Bare` mode (GocciaScriptLoaderBare) that installs only the modules, web and host preludes; a runtime API stays out of it.

### Files a new type touches

| Step | File(s) | Purpose |
|------|---------|---------|
| 1 | `source/g2/G2.<Unit>.pas` or `source/g2/prelude/<name>.js` | The type |
| 2 | `source/g2/G2.Engine.pas` or `source/g2/G2.HostAPI.pas` | Registration |
| 3 | `build.pas` (`G2_PRELUDES`) | A new prelude file only |
| 4 | `source/g2/prelude/web.js`, `source/g2/G2.HeapSnapshot.pas` | structuredClone and heap snapshots, if they apply |
| 5 | `tests/built-ins/<Type>/` | JavaScript tests |
| 6 | `benchmarks/<type>.js` | Benchmarks |
| 7 | `docs/built-ins.md`, `README.md`, other `docs/*` as needed | Documentation |

## Step 1a: A Native Type

### Pick an object kind

An instance of a native type is a `TObj` whose `OKind` names its kind. `G2.Core` declares the core kinds (`okPlain` to `okModuleNS`); units that own a built-in declare theirs next to it (`okTypedArray` in `G2.Typed`, `okPromise` in `G2.Promise`, `okDate` in `G2.Date`, `okRawJSON` in `G2.JSON`, ...). Search for the existing values before taking one (`grep -rn "ok[A-Z][A-Za-z]* *= *[0-9]" source/g2`), and declare the same constant in every unit that tests for it.

Keep the instance's state in the fields the collector traces: `Aux1` and `Aux2` (a number, a string, a value array, another object). `Pad` holds 32 untraced bits. `Native` is an untraced pointer, for memory outside the heap; such memory is released from a collector hook (`GCFinalizeHook`, as `G2.Shared` releases SharedArrayBuffer blocks). Bit 8 of `OFlags` means "not extensible" for every object; check the other bits' uses before taking one for your kind.

### Write the natives

A native has the `TNativeFn` signature. `Args[0]` is `this`, `Args[1..Argc]` the arguments. It writes the result to `Res` and returns `True`, or returns `False` with the exception in `Pending` (`ThrowTypeError`, `ThrowError`). Under `new`, `CurNewTarget` holds new.target.

```pascal
unit G2.Foo;
{ Foo objects: a counter in Aux1. }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

procedure InitFoo;

implementation

uses
  SysUtils, G2.Core, G2.VM;

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
- **Captured intrinsics.** Call through captured functions (`apply`, `defineProperty`, `MapCtor`), never through globals a script can replace.
- **Native source.** A prelude's functions keep no source text: `Function.prototype.toString` gives them native-function syntax.
- **ASCII only.** The build turns each prelude into a Pascal string constant and fails on a non-ASCII character; write `\u` escapes instead.
- **The whole language.** Preludes compile under the unrestricted language profile, whatever the host's compatibility flags.

### A new prelude file

1. Create `source/g2/prelude/<name>.js` holding one parenthesized function expression. The file compiles as an expression; its value is the function the installer calls:

   ```javascript
   // Foo for G2: what it implements and which natives it is called with.
   (function (G, n, shared) {
     'use strict';
     // ...
   })
   ```

2. Add a row to `G2_PRELUDES` in `build.pas` and raise the array's upper bound by one: the file name, the include it becomes and the constant it defines.

   ```pascal
   ('foo.js', 'G2.FooPrelude.inc', 'FooSrc'),
   ```

   `GenerateG2Preludes` writes `build/g2/gen/G2.FooPrelude.inc` on every G2 build, with the source as the constant `FooSrc`.

3. Include it in the implementation section of the unit that installs it: `{$I G2.FooPrelude.inc}`.

4. Install it.
   - **Engine prelude.** In an Init procedure listed in `InitIntrinsics`: `CompilePrelude` (`G2.Snapshot`) compiles the source, `MakeScriptClosure` and `CallValue` evaluate it to the function, and a second `CallValue` calls it with its arguments. Raise an `Exception` on failure: a prelude that does not run is an engine bug. `InitBuiltins5` (`G2.Builtins5`) is the full example; the short form:

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

5. Build. The build compiles each program once, runs it to snapshot the preludes it compiles (`G2.Snapshot`, `SnapshotG2Preludes` in `build.pas`) and compiles it again with their bytecode embedded, so start-up does not parse them. `CompilePrelude` takes a snapshot entry only while the prelude's source still matches it, and otherwise parses the source.

## Step 2: Registration

### Engine built-ins: `G2.Engine`

Add the unit to `G2.Engine`'s implementation `uses` clause and call its Init procedure from `InitIntrinsics`. The order matters: an Init procedure can read only the intrinsics made before it (`InitBuiltins` makes `Object`, `Function`, `Array` and the well-known symbols first; the preludes in `InitBuiltins5` and later capture what exists when they run). `InitIntrinsics` is also the realm initializer (`RealmInitProc`), so the same call covers ShadowRealm and `$262.createRealm`.

On Linux x86-64, `G2_REALMCHECK=<size of the TLS segment>` makes `CreateRealm` report the thread-local bytes a new realm's Init procedures changed that switching back did not restore: realm state that no `RealmVals` call registered.

### Host APIs: `G2.HostAPI`

A host prelude is registered in `InstallHostGlobals` as above. A new realm of a CLI host gets the intrinsics, then `CLIRealmInit`; host preludes run only in the host's own realm.

## Step 3: structuredClone and Heap Snapshots

- **structuredClone** is `web.js`'s. It refuses an object of a native kind it does not know (`plainCloneable` accepts only `okPlain`, `okArray` and `okArgumentsOrd`) and an instance of any prelude class, with a `DataCloneError`. A cloneable type needs a branch in `structuredClone`'s `clone` function, brand-checked through an intrinsic captured at the top of the section, before the plain-object case.
- **Heap snapshots** (`G2.HeapSnapshot`) refuse every object kind without a codec, which `GocciaTestRunner --heap-snapshots` reports as refused, not failed. Add the kind to `KindName` so the refusal names it.

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

- **GC stress** — For native code that allocates, run the tests with a 16 KB nursery and heap verification after every collection: `./build.pas g2 && python3 scripts/g2/gcstress-tests.py build/g2/g2 8 tests/built-ins/YourType`. A file reported `BAD` reads a value an allocation moved or misses a root.
- **test262** — For an ECMA-262 or ECMA-402 built-in, run its test262 directory ([test262](test262.md)). CI holds the suite and test262 to `scripts/g2/known-failures`: a new failure fails the job, and a listed failure that now passes should be removed from the list.

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
- [ ] Native: every method brand-checks `this`; values rooted across allocation and calls; write barrier on direct pointer stores
- [ ] Native: Init procedure with `AddRoot` and `RealmVals` for each threadvar it fills
- [ ] Native: Init procedure called from `InitIntrinsics` in `G2.Engine`, after what it reads
- [ ] Prelude: internal slots as private fields, `markIntrinsic` on the class, intrinsics captured, ASCII only
- [ ] New prelude file: `G2_PRELUDES` row and bound in `build.pas`, `{$I}` of the generated include, installed by an Init procedure or `InstallHostGlobals`
- [ ] `Symbol.toStringTag` on the prototype
- [ ] structuredClone branch in `web.js` (if cloneable); `KindName` entry in `G2.HeapSnapshot` (native kinds)
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
4. **Unregistered realm state** — A threadvar the Init procedure fills but does not pass to `RealmVals` is not saved and restored by `SwitchRealm`, so every realm sees the value the newest realm's Init procedure left there. `G2_REALMCHECK` finds these.
5. **Calling through replaceable globals in a prelude** — `Array.prototype.push` or `Object.defineProperty` read at call time observe a script's changes. Capture them when the prelude runs.
6. **Forgetting `markIntrinsic`** — Without it, `Reflect.construct(PreludeClass, [], newTargetWithoutPrototype)` gets `Object.prototype` instead of the class's prototype.
7. **Exceptions for JavaScript errors** — A native signals a throw by returning `False` with `Pending` set, never by raising a Pascal exception.
8. **Formatting** — G2's Pascal is outside `./format.pas`; follow the style of the surrounding unit.
