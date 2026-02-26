# Adding a New Built-in Type

This guide walks through every step needed to add a new built-in type to GocciaScript. Follow the steps in order; each section references the exact files and patterns involved.

## Overview

Adding a built-in type like `Set`, `Map`, or `ArrayBuffer` requires changes across several files:

| Step | File(s) | Purpose |
|------|---------|---------|
| 1 | `units/Goccia.Values.YourValue.pas` | Value type class (data storage, prototype methods) |
| 2 | `units/Goccia.Builtins.GlobalYour.pas` | Built-in registration (constructor function, static methods) |
| 3 | `units/Goccia.Values.ClassValue.pas` | Class value subclass (`CreateNativeInstance`) |
| 4 | `units/Goccia.Engine.pas` | Engine integration (enum, globals, registration, cleanup) |
| 5 | `units/Goccia.Constants.ConstructorNames.pas` | Constructor name constant |
| 6 | `units/Goccia.Constants.PropertyNames.pas` | Property name constants (if needed) |
| 7 | `units/Goccia.Builtins.Globals.pas` | structuredClone support (if cloneable) |
| 8 | `tests/built-ins/YourType/` | JavaScript tests |
| 9 | `benchmarks/yourtype.js` | Benchmarks |
| 10 | `docs/built-ins.md`, `README.md`, `AGENTS.md` | Documentation |

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

**Shared prototype singleton** -- All instances share a single prototype object. The `InitializePrototype` method is guarded by `if Assigned(FShared) then Exit` so it only runs once:

```pascal
procedure TGocciaYourValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  // Register prototype methods
  FShared.Prototype.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      YourMethod, 'methodName', 1));  // 1 = expected arg count

  // Register Symbol.toStringTag
  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_YOUR),
      [pfConfigurable]
    )
  );

  // Register accessor properties (e.g., getters)
  FShared.Prototype.DefineProperty('propertyName',
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        GetterMethod, 'get propertyName', 0),
      nil, [pfConfigurable]));
end;
```

**ExposePrototype** -- Must NOT free the created instance (it becomes the pinned method host):

```pascal
class procedure TGocciaYourValue.ExposePrototype(
  const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaYourValue.Create;  // Do NOT call .Free on this
  FShared.ExposeOnConstructor(AConstructor);
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

## Step 4: Engine Integration (`Goccia.Engine.pas`)

Six changes needed:

### 4a. Interface uses clause

Add `Goccia.Builtins.GlobalYour` (alphabetically sorted).

### 4b. Enum flag

Add to `TGocciaGlobalBuiltin`:

```pascal
TGocciaGlobalBuiltin = (
  ...
  ggYourType
);
```

### 4c. DefaultGlobals

Add the flag to the `DefaultGlobals` set (if the built-in should be available by default):

```pascal
const DefaultGlobals: TGocciaGlobalBuiltins = [..., ggYourType];
```

### 4d. Field declaration

```pascal
FBuiltinYour: TGocciaGlobalYour;
```

### 4e. RegisterBuiltIns

```pascal
if ggYourType in FGlobals then
  FBuiltinYour := TGocciaGlobalYour.Create(
    CONSTRUCTOR_YOUR, Scope, ThrowError);
```

### 4f. RegisterBuiltinConstructors

```pascal
if ggYourType in FGlobals then
begin
  YourConstructor := TGocciaYourClassValue.Create(CONSTRUCTOR_YOUR, nil);
  TGocciaYourValue.ExposePrototype(YourConstructor);
  YourConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinYour) then
    for Key in FBuiltinYour.BuiltinObject.GetAllPropertyNames do
      YourConstructor.SetProperty(Key,
        FBuiltinYour.BuiltinObject.GetProperty(Key));
  FInterpreter.GlobalScope.DefineLexicalBinding(
    CONSTRUCTOR_YOUR, YourConstructor, dtConst);
end;
```

### 4g. Destructor

```pascal
FBuiltinYour.Free;
```

### 4h. Implementation uses clause

Add `Goccia.Values.YourValue` (alphabetically sorted).

### 4i. Property (optional)

```pascal
property BuiltinYour: TGocciaGlobalYour read FBuiltinYour;
```

## Step 5: Constructor Name Constant

In `units/Goccia.Constants.ConstructorNames.pas`:

```pascal
CONSTRUCTOR_YOUR = 'YourType';
```

## Step 6: Property Name Constants (if needed)

In `units/Goccia.Constants.PropertyNames.pas`:

```pascal
PROP_YOUR_PROPERTY = 'yourProperty';
```

## Step 7: structuredClone Support (if cloneable)

In `units/Goccia.Builtins.Globals.pas`:

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

```
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

### `AGENTS.md`

- Add `ggYourType` to the `DefaultGlobals` reference in the Built-in Objects section.
- Add value type and built-in unit entries to the component table.
- Add test directory reference if it contains noteworthy test patterns.

## Checklist

Use this checklist when adding a new built-in type:

- [ ] Value type unit (`Goccia.Values.YourValue.pas`)
- [ ] Built-in registration unit (`Goccia.Builtins.GlobalYour.pas`)
- [ ] Class value subclass in `Goccia.Values.ClassValue.pas`
- [ ] Engine: enum flag in `TGocciaGlobalBuiltin`
- [ ] Engine: add to `DefaultGlobals` (if default)
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
- [ ] Documentation in `docs/built-ins.md`, `README.md`, `AGENTS.md`
- [ ] All existing tests still pass
- [ ] Build succeeds (`./build.pas clean loader`)

## GC Considerations

- **Shared prototype** is pinned automatically by `TGocciaSharedPrototype.Create` (calls `PinValue` on both the prototype object and the method host).
- **`MarkReferences`** must mark all `TGocciaValue` fields reachable from the instance. If you only hold non-value data (e.g., `TBytes`), calling `inherited` is sufficient.
- **`ExposePrototype`** creates a sentinel instance that becomes the method host. Do NOT free this instance -- it is pinned by the GC.
- **Temp roots**: If you hold `TGocciaValue` references in Pascal variables during a long operation (not in any scope), protect them with `AddTempRoot`/`RemoveTempRoot`.
- **Non-owning lists**: Use `TObjectList<T>.Create(False)` for lists that store GC-managed values.

## Common Pitfalls

1. **Freeing the ExposePrototype sentinel** -- Causes access violations when prototype methods are called.
2. **Using `Self` in prototype callbacks** -- `Self` is the method host singleton, not the instance. Always use `AThisValue`.
3. **`IsInfinity` vs `IsInfinite`** -- `IsInfinity` checks only positive infinity. Use `IsInfinite` to check both positive and negative.
4. **Circular interface dependencies** -- If your value type and `ClassValue` need each other, put the value type in `ClassValue`'s *implementation* uses clause (not interface).
5. **Stale build artifacts** -- After adding new units, run `./build.pas clean loader` to avoid FPC internal errors from stale `.ppu` files.
6. **Never use `.ToNumberLiteral.Value` for special numbers** -- `NaN`, `Infinity`, `-Infinity`, and `-0` all store `FValue = 0.0` internally (see [design-decisions.md](design-decisions.md#number-representation)). Code that reads `.Value` without first checking `IsNaN`/`IsInfinite`/`IsNegativeZero` will silently treat these as zero. Always check the special value flags first and handle each case explicitly. This applies to any code that converts a `TGocciaNumberLiteralValue` to a raw `Double` for storage, comparison, or arithmetic -- including binary buffer writes, search comparisons, and fill values. See `Goccia.Values.TypedArrayValue.WriteNumberLiteral` and `Goccia.Evaluator.Arithmetic.pas` for the canonical patterns.
