# Adding a New Built-in Type

*For contributors adding a new built-in type to the engine — a step-by-step recipe.*

## Executive Summary

- **10-step recipe** — Value type, built-in registration, class value subclass, engine/runtime integration, constants, structuredClone, tests, benchmarks, documentation
- **Key patterns** — Shared prototype singleton (GC-pinned), `ThisValue` for method callbacks (not `Self`), `MarkReferences` for GC
- **Engine/runtime integration** — Core language built-ins are registered by the engine; host/runtime globals belong in runtime extensions; add `TGocciaGlobalBuiltin` flags only for special-purpose built-ins (TestAssertions, Benchmark, FFI)
- **Checklist included** — Complete checklist at the end of the document for verification

This guide walks through every step needed to add a new built-in type to GocciaScript. Follow the steps in order; each section references the exact files and patterns involved.

## Overview

**Before you start:** built-in prototype objects are not module-level singletons. They live in a per-engine [realm](core-patterns.md#realm-ownership--slot-registration) (`Goccia.Realm.pas`) so that two engines on the same worker thread see independent intrinsics. Your value type registers a realm slot at unit `initialization` time and stores its shared prototype in that slot — never in a `threadvar`, class var, or static singleton, because cached pointers go stale across engine recreation. The slot pattern is shown in Step 1 below.

Adding a built-in type like `Set`, `Map`, or `ArrayBuffer` requires changes across several files:

| Step | File(s) | Purpose |
|------|---------|---------|
| 1 | `source/units/Goccia.Values.YourValue.pas` | Value type class (data storage, prototype methods) |
| 2 | `source/units/Goccia.Builtins.GlobalYour.pas` | Built-in registration (constructor function, static methods) |
| 3 | `source/units/Goccia.Values.ClassValue.pas` | Class value subclass (`CreateNativeInstance`) |
| 4 | `source/units/Goccia.Engine.pas` | Engine integration (enum, globals, registration, cleanup) |
| 5 | `source/units/Goccia.Constants.ConstructorNames.pas` | Constructor name constant |
| 6 | `source/units/Goccia.Constants.PropertyNames.pas` | Property name constants (if needed) |
| 7 | `source/units/Goccia.Builtins.Globals.pas` | structuredClone support (if cloneable) |
| 8 | `tests/built-ins/YourType/` | JavaScript tests |
| 9 | `benchmarks/yourtype.js` | Benchmarks |
| 10 | `docs/built-ins.md`, `README.md`, other `docs/*` as needed | Documentation |

## Step 1: Value Type (`Goccia.Values.YourValue.pas`)

Create a new unit inheriting from `TGocciaInstanceValue`. This is the runtime representation of your type.

### Template

```pascal
unit Goccia.Values.YourValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaYourValue = class(TGocciaInstanceValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    // Internal data storage
    FData: ...; // e.g., TBytes, TGocciaValueList, etc.

    // Prototype method implementations (use ThisValue, not Self)
    function YourMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

constructor TGocciaYourValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  // Initialize internal data
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

destructor TGocciaYourValue.Destroy;
begin
  // Free owned data (non-GC-managed)
  inherited;
end;
```

### Key Patterns

**Shared prototype singleton (realm-owned)** -- All instances of one engine share a single prototype object that lives in the engine's realm. Register a realm-owned slot at unit `initialization` time and look up the shared prototype through it on every call. **Do not** store `FShared` in a class var or `threadvar` — cached pointers survive engine destruction and become dangling references on the next engine.

```pascal
var
  GYourSharedSlot: TGocciaRealmOwnedSlotId;

function GetYourShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GYourSharedSlot))
  else
    Result := nil;
end;

procedure TGocciaYourValue.InitializePrototype;
var
  Shared: TGocciaSharedPrototype;
  Members: TGocciaMemberCollection;
  Definitions: array of TGocciaMemberDefinition;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetYourShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('methodName', YourMethod, 1);
    Members.AddSymbolMethod(
      TGocciaSymbolValue.WellKnownIterator,
      '[Symbol.iterator]', YourIteratorMethod, 0,
      [pfConfigurable, pfWritable]);
    Members.AddAccessor(
      'propertyName', GetterMethod, nil, [pfConfigurable]);
    Definitions := Members.ToDefinitions;
    Definitions[0].MemberFlags := [gmfNoFunctionPrototype];
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(Shared.Prototype, Definitions);
  CurrentRealm.SetOwnedSlot(GYourSharedSlot, Shared);
end;

initialization
  GYourSharedSlot := RegisterRealmOwnedSlot('YourType.SharedPrototype');
```

The `Goccia.Realm` unit owns `RegisterRealmOwnedSlot`, `CurrentRealm`, and the `TGocciaRealmOwnedSlotId` type — add it to your `uses` clause. For prototypes that are plain `TGocciaObjectValue` instances (no `TGocciaSharedPrototype` wrapper), use `RegisterRealmSlot` and `SetSlot`/`GetSlot` instead — see [Core patterns § Realm Ownership & Slot Registration](core-patterns.md#realm-ownership--slot-registration) for the raw-slot variant. `TGocciaSharedPrototype.Destroy` unpins both `FPrototype` and `FMethodHost`, so realm tear-down releases the prototype graph atomically.

The preferred pattern is:

- keep runtime semantics on the value type
- keep constructor/static methods on the built-in wrapper
- declare exposed members with `Define*` helpers, ideally via `TGocciaMemberCollection`
- register them via `RegisterMemberDefinitions`

This keeps the JS-visible surface in one place and avoids repeating `RegisterNativeMethod`, `DefineProperty`, and `CreateWithoutPrototype` boilerplate in every type.

**ExposePrototype** -- Must NOT free the created instance (it becomes the pinned method host). Look up the shared prototype through the realm rather than caching it:

```pascal
class procedure TGocciaYourValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetYourShared;
  if not Assigned(Shared) then
  begin
    TGocciaYourValue.Create;  // populates the realm slot, do NOT call .Free
    Shared := GetYourShared;
  end;
  ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;
```

**Prototype method callbacks** -- Always use `AThisValue` (the receiver), never `Self` (the method host singleton):

```pascal
function TGocciaYourValue.YourMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaYourValue;
begin
  if not (AThisValue is TGocciaYourValue) then
    ThrowTypeError('...');
  Obj := TGocciaYourValue(AThisValue);
  // Work with Obj, not Self
end;
```

**GetProperty** -- Override to handle computed properties (like `size`, `byteLength`):

```pascal
function TGocciaYourValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FItems.Count)
  else
    Result := inherited GetProperty(AName);
end;
```

**MarkReferences** -- Mark all `TGocciaValue` references held by this object:

```pascal
procedure TGocciaYourValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;
  // Mark each TGocciaValue reference
  for I := 0 to FItems.Count - 1 do
    if Assigned(FItems[I]) then
      FItems[I].MarkReferences;
end;
```

If your type only holds non-`TGocciaValue` data (e.g., `TBytes`), the body can just call `inherited`.

**ToStringTag** -- Return the constructor name:

```pascal
function TGocciaYourValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_YOUR;
end;
```

**InitializeNativeFromArguments** -- Called when `new YourType(args)` is used via the class constructor path:

```pascal
procedure TGocciaYourValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
begin
  // Validate and process constructor arguments
end;
```

## Step 2: Built-in Registration (`Goccia.Builtins.GlobalYour.pas`)

This unit creates the constructor function and any static methods.

```pascal
unit Goccia.Builtins.GlobalYour;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.YourValue;

type
  TGocciaGlobalYour = class(TGocciaBuiltin)
  private
    FYourConstructor: TGocciaNativeFunctionValue;
    function YourConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.ErrorHelper;

constructor TGocciaGlobalYour.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FYourConstructor := TGocciaNativeFunctionValue.Create(
    YourConstructorFn, 'YourType', 1);
  TGocciaYourValue.ExposePrototype(FYourConstructor);

  // Register static methods on FBuiltinObject
  FBuiltinObject.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      StaticMethod, 'staticMethod', 1));
end;

function TGocciaGlobalYour.YourConstructorFn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Validate arguments and create instance
  Result := TGocciaYourValue.Create;
end;
```

## Step 3: Class Value Subclass (`Goccia.Values.ClassValue.pas`)

Add a class value subclass so `new YourType()` works via the standard class instantiation path.

**In the interface section** (after existing class values):

```pascal
TGocciaYourClassValue = class(TGocciaClassValue)
  function CreateNativeInstance(
    const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
end;
```

**In the implementation section:**

```pascal
{ TGocciaYourClassValue }

function TGocciaYourClassValue.CreateNativeInstance(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaYourValue.Create;
end;
```

**In the implementation uses clause**, add `Goccia.Values.YourValue`.

## Step 4: Engine Or Runtime Integration

Core language built-ins belong in `Goccia.Engine.pas`. Host/runtime globals that are not part of the language core belong in a runtime extension such as `Goccia.Runtime.pas`.

For a core language built-in, make these changes in the engine:

### 4a. Interface uses clause

Add `Goccia.Builtins.GlobalYour` (alphabetically sorted).

### 4b. Enum flag (special-purpose built-ins only)

Core language built-ins are always registered and do not need an enum flag. Add a flag to `TGocciaGlobalBuiltin` only for special-purpose built-ins that should be opt-in:

```pascal
TGocciaGlobalBuiltin = (ggTestAssertions, ggBenchmark, ggFFI);
```

Most new language built-in types should skip this step entirely. Runtime extension membership should be represented with the extension's own configuration type, not `TGocciaGlobalBuiltin`.

### 4c. Field declaration

```pascal
FBuiltinYour: TGocciaGlobalYour;
```

### 4d. RegisterBuiltIns

For core language built-ins (no flag-gating needed):

```pascal
FBuiltinYour := TGocciaGlobalYour.Create(
  CONSTRUCTOR_YOUR, Scope, ThrowError);
```

For special-purpose built-ins, guard with a flag check: `if ggYourType in FGlobals then ...`

### 4e. RegisterBuiltinConstructors

```pascal
begin
  TypeDef.ConstructorName := CONSTRUCTOR_YOUR;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaYourClassValue;
  TypeDef.ExposePrototype := @ExposeYourPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinYour);
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter,
    GenericConstructor);
  YourConstructor := TGocciaYourClassValue(GenericConstructor);
end;
```

For built-ins that use an existing shared prototype rather than `ExposePrototype`, set `ExposePrototype := nil` and provide `PrototypeProvider := @YourPrototypeProvider`.

### 4f. Destructor

```pascal
FBuiltinYour.Free;
```

### 4g. Implementation uses clause

Add `Goccia.Values.YourValue` (alphabetically sorted).

### 4h. Property (optional)

```pascal
property BuiltinYour: TGocciaGlobalYour read FBuiltinYour;
```

### 4i. Runtime-extension registration

For a host/runtime global, use the same built-in/value unit patterns but wire it through `Goccia.Runtime.pas` instead of `Goccia.Engine.pas`:

1. Add `Goccia.Builtins.GlobalYour` to the `Goccia.Runtime.pas` interface `uses` clause.
2. Add an extension-specific config entry such as `rgYour` to `TGocciaRuntimeGlobal`; do not add a `TGocciaGlobalBuiltin` flag unless the feature is a special-purpose opt-in like tests, benchmarks, or FFI.
3. Add a private field such as `FBuiltinYour: TGocciaGlobalYour` to `TGocciaRuntimeExtension`.
4. Instantiate it from `TGocciaRuntimeExtension.RegisterBuiltIns` or register its constructor from `RegisterRuntimeConstructors`, mirroring the engine's `RegisterBuiltIns` / constructor-registration pattern.
5. Free the field in `TGocciaRuntimeExtension.Destroy`.
6. If the feature adds importable file types, update `ConfigureModuleExtensions` and `LoadRuntimeModule` so the extension participates only when its `rgYour` config flag is enabled.
7. Expose host setup through `TGocciaRuntime.Create(..., RuntimeGlobals)` or `AttachRuntimeExtension(Engine, RuntimeGlobals)`. CLI frontends attach the runtime in their engine-configuration hook; embedders can use the same runtime constructor or extension attach entry.

## Step 5: Constructor Name Constant

In `source/units/Goccia.Constants.ConstructorNames.pas`:

```pascal
CONSTRUCTOR_YOUR = 'YourType';
```

## Step 6: Property Name Constants (if needed)

In `source/units/Goccia.Constants.PropertyNames.pas`:

```pascal
PROP_YOUR_PROPERTY = 'yourProperty';
```

## Step 7: structuredClone Support (if cloneable)

In `source/units/Goccia.Builtins.Globals.pas`:

### 7a. Add to implementation uses clause

```pascal
Goccia.Values.YourValue,
```

### 7b. Add clone function

```pascal
function CloneYour(const AObj: TGocciaYourValue;
  const AMemory: TDictionary<TGocciaValue, TGocciaValue>): TGocciaYourValue;
begin
  Result := TGocciaYourValue.Create;
  AMemory.Add(AObj, Result);
  // Copy internal data from AObj to Result
end;
```

### 7c. Add branch in `StructuredCloneValue`

Add before the `TGocciaObjectValue` catch-all branch:

```pascal
else if AValue is TGocciaYourValue then
  Result := CloneYour(TGocciaYourValue(AValue), AMemory)
```

The order matters -- specific types must be checked before `TGocciaObjectValue` since they inherit from it.

## Step 8: Tests

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

  test("throws RangeError for invalid argument", () => {
    expect(() => new YourType(-1)).toThrow(RangeError);
  });
});
```

If the type supports structuredClone, add `tests/built-ins/structuredClone/yourtype.js`.

## Step 9: Benchmarks

Create `benchmarks/yourtype.js`:

```javascript
/*---
description: YourType operation benchmarks
---*/

suite("YourType creation", () => {
  bench("create YourType", {
    run: () => {
      const obj = new YourType();
    },
  });
});

suite("YourType methods", () => {
  bench("method call", {
    setup: () => new YourType(),
    run: (obj) => {
      obj.method();
    },
  });
});
```

## Step 10: Documentation

### `docs/built-ins.md`

Add a section with method tables:

```markdown
### YourType (`Goccia.Builtins.GlobalYour.pas`)

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

- [ ] Value type unit (`Goccia.Values.YourValue.pas`) with realm slot registered in `initialization`
- [ ] Built-in registration unit (`Goccia.Builtins.GlobalYour.pas`)
- [ ] Class value subclass in `Goccia.Values.ClassValue.pas`
- [ ] Engine: enum flag in `TGocciaGlobalBuiltin` (special-purpose built-ins only)
- [ ] Engine: field declaration
- [ ] Engine: `RegisterBuiltIns` registration
- [ ] Engine: `RegisterBuiltinConstructors` constructor
- [ ] Engine: `Destroy` cleanup
- [ ] Engine: interface and implementation uses clauses
- [ ] Constructor name constant in `Goccia.Constants.ConstructorNames.pas`
- [ ] Property name constants in `Goccia.Constants.PropertyNames.pas` (if needed)
- [ ] structuredClone support in `Goccia.Builtins.Globals.pas` (if cloneable)
- [ ] JavaScript tests in `tests/built-ins/YourType/`
- [ ] Benchmarks in `benchmarks/yourtype.js`
- [ ] Documentation in `docs/built-ins.md`, `README.md`, and other docs as needed (see [CONTRIBUTING.md](../CONTRIBUTING.md))
- [ ] All existing tests still pass
- [ ] Build succeeds (`./build.pas clean loader`)

## GC Considerations

- **Shared prototype** is pinned automatically by `TGocciaSharedPrototype.Create` and unpinned by `TGocciaSharedPrototype.Destroy`. Registering the helper in a realm-owned slot ties its lifetime to the engine's realm, so tear-down releases everything atomically.
- **Realm slots** pin the values they hold. `RegisterRealmSlot` (for `TGCManagedObject` prototypes) pins on `SetSlot` and unpins at realm tear-down; `RegisterRealmOwnedSlot` (for plain-`TObject` helpers) calls `.Free` at tear-down before pin release.
- **`MarkReferences`** must mark all `TGocciaValue` fields reachable from the instance. If you only hold non-value data (e.g., `TBytes`), calling `inherited` is sufficient.
- **`ExposePrototype`** creates a sentinel instance that becomes the method host. Do NOT free this instance -- it is pinned by the GC and owned by the realm.
- **Temp roots**: If you hold `TGocciaValue` references in Pascal variables during a long operation (not in any scope), protect them with `AddTempRoot`/`RemoveTempRoot`.
- **Non-owning lists**: Use `TObjectList<T>.Create(False)` for lists that store GC-managed values.

## Common Pitfalls

1. **Freeing the ExposePrototype sentinel** -- Causes access violations when prototype methods are called.
2. **Using `Self` in prototype callbacks** -- `Self` is the method host singleton, not the instance. Always use `AThisValue`.
3. **Caching realm-scoped objects in `threadvar`s or class vars** -- The cached pointer survives engine destruction and becomes a dangling reference on the next engine. Always read the shared prototype through `CurrentRealm.GetSlot`/`GetOwnedSlot`. See [Core patterns § Realm Ownership & Slot Registration](core-patterns.md#realm-ownership--slot-registration) for the stale-cache antipattern.
4. **`IsInfinity` vs `IsInfinite`** -- `IsInfinity` checks only positive infinity. Use `IsInfinite` to check both positive and negative.
5. **Circular interface dependencies** -- If your value type and `ClassValue` need each other, put the value type in `ClassValue`'s *implementation* uses clause (not interface).
6. **Stale build artifacts** -- After adding new units, run `./build.pas clean loader` to avoid FPC internal errors from stale `.ppu` files.
7. **Use property accessors for special numbers** -- `TGocciaNumberLiteralValue` stores a single `Double` in `FValue` using standard IEEE 754 bit patterns. Use the `IsNaN`, `IsInfinity`, and `IsNegativeZero` property accessors rather than raw `FValue` comparisons when you need to distinguish special values. For negative zero specifically, `IsNegativeZero` uses an endian-neutral sign-bit check. See `Goccia.Values.TypedArrayValue.WriteNumberLiteral` and `Goccia.Evaluator.Arithmetic.pas` for the canonical patterns.
