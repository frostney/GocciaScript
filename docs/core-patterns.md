# Core patterns

*Recurring implementation conventions in the Pascal codebase (scopes, parser, prototypes, built-ins) and shared vocabulary (Define vs Assign, bindings, …).*

## Executive Summary

- **Singleton pattern** — Primitive singletons (`undefined`, `null`, `true`, `false`, `NaN`) use class-var + class-function, pinned by the GC
- **Factory method** — Scopes are created via `CreateChild`, never directly instantiated
- **Builder pattern** — Built-in objects constructed by creating a `TGocciaObjectValue` and adding methods one at a time
- **Shared prototype singleton** — Each built-in type shares a single class-level prototype instance via `TGocciaSharedPrototype`
- **Define vs Assign** — `Define` creates a new binding; `Assign` changes an existing one — distinct operations throughout the codebase

For **pipelines and layers**, see [Architecture](architecture.md). For the **execution backends**, see [Interpreter](interpreter.md) (tree-walk) and [Bytecode VM](bytecode-vm.md) (register VM, `.gbc`).

## Recurring implementation patterns

These patterns show up across layers (values, scope, parser, built-ins). They are implementation conventions, not separate subsystems.

### Singleton pattern (special values)

Primitive singletons use a **class var** plus a **class function** that creates on first use (see `Goccia.Values.Primitives.pas`). They are **pinned** during engine startup via `PinPrimitiveSingletons` so the GC does not collect them.

```pascal
TGocciaUndefinedLiteralValue = class(TGocciaValue)
private
  class var FUndefinedValue: TGocciaUndefinedLiteralValue;
public
  class function UndefinedValue: TGocciaUndefinedLiteralValue;
end;

class function TGocciaUndefinedLiteralValue.UndefinedValue: TGocciaUndefinedLiteralValue;
begin
  if not Assigned(FUndefinedValue) then
    FUndefinedValue := TGocciaUndefinedLiteralValue.Create;
  Result := FUndefinedValue;
end;
```

The same pattern applies to `TGocciaNullLiteralValue.NullValue`, `TGocciaBooleanLiteralValue.TrueValue` / `FalseValue`, and the shared number literals (`NaNValue`, `ZeroValue`, …).

### Factory method (scope creation)

Scopes are created via `CreateChild`, never directly instantiated:

```pascal
// Correct
ChildScope := ParentScope.CreateChild(skBlock);

// Correct — with capacity hint for function call scopes
CallScope := FClosure.CreateChild(skFunction, ParamCount + 4);

// WRONG — never do this
ChildScope := TGocciaScope.Create(ParentScope, skBlock);
```

### Builder pattern (built-in registration)

Built-in objects are constructed by creating a `TGocciaObjectValue` and adding methods one at a time:

```pascal
MathObj := TGocciaObjectValue.Create;
MathObj.DefineProperty('PI', TGocciaNumberLiteralValue.Create(Pi), False);
MathObj.DefineProperty('floor', TGocciaNativeFunction.Create(@MathFloor), False);
```

### Parser combinator (binary expressions)

All left-associative binary operator parsers delegate to a shared `ParseBinaryExpression` helper:

```pascal
function TGocciaParser.ParseBinaryExpression(
  NextLevel: TParseFunction;
  const Operators: array of TGocciaTokenType
): TGocciaExpression;
```

Each precedence level is a one-liner:

```pascal
function TGocciaParser.Equality: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Comparison, [gttStrictEqual, gttStrictNotEqual]);
end;
```

### Shared prototype singleton pattern

GocciaScript uses a **shared prototype object** for each built-in type so every instance has the same `[[Prototype]]` as in ECMA-262 (for example all `Set` instances share the one `Set.prototype` object from the spec). **That layout matches the standard**; it is not an extra semantic on top of ECMAScript. In Pascal we realize it with `TGocciaSharedPrototype`, `TGocciaMemberCollection`, and `RegisterMemberDefinitions` (pinning, method tables, wiring the constructor’s `prototype` property).

The following matches **`TGocciaSetValue.InitializePrototype`** in `Goccia.Values.SetValue.pas` (abbreviated):

```pascal
class var FShared: TGocciaSharedPrototype;
class var FPrototypeMembers: array of TGocciaMemberDefinition;

procedure TGocciaSetValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('has', SetHas, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      // ... more AddNamedMethod / AddSymbolMethod ...
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        SetSymbolIterator,
        0,
        [pfConfigurable, pfWritable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

constructor TGocciaSetValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FItems := TGocciaValueList.Create(False);
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

class procedure TGocciaSetValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaSetValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;
```

**Variant:** `TGocciaStringObjectValue` uses a shared `TGocciaObjectValue` as `FSharedStringPrototype` and pins it explicitly in `InitializePrototype` — same idea (one prototype for all string objects), different helper type.

Prototype **methods** take `(AArgs, AThisValue)`; use **`AThisValue`** for the real instance — **`Self`** is the method host singleton. For chaining (`Set.add`, `Map.set`), return **`AThisValue`**, not `Self`.

### Realm Ownership & Slot Registration

Built-in prototypes are not module-level singletons; they live in a per-engine **realm** (`Goccia.Realm.pas`, `TGocciaRealm`). Each `TGocciaEngine` constructs its own realm and frees it in `Destroy`, which unpins every prototype the realm owns. The next engine on the same worker thread starts from pristine intrinsics — userland mutations of `Array.prototype`, including non-configurable property additions, do not leak across engine boundaries.

Two slot kinds are registered at unit `initialization` time:

| API | Stores | Lifecycle |
|-----|--------|-----------|
| `RegisterRealmSlot('Name')` → `TGocciaRealmSlotId` | A `TGCManagedObject` (typically a prototype object) | `SetSlot` pins via the GC; realm tear-down unpins everything ever stored in this slot. |
| `RegisterRealmOwnedSlot('Name')` → `TGocciaRealmOwnedSlotId` | A plain `TObject` helper (e.g. `TGocciaSharedPrototype`) | Realm calls `.Free` at tear-down, before the pinned-slot release pass, so the helper's destructor can still unpin the GC objects it owns. |

#### Raw-slot pattern (a single `TGocciaObjectValue` prototype)

This is what `TGocciaIteratorValue` does — see `Goccia.Values.IteratorValue.pas`:

```pascal
var
  GIteratorPrototypeSlot: TGocciaRealmSlotId;

function GetSharedIteratorPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GIteratorPrototypeSlot))
  else
    Result := nil;
end;

procedure TGocciaIteratorValue.InitializePrototype;
var
  SharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(CurrentRealm.GetSlot(GIteratorPrototypeSlot)) then Exit;

  SharedPrototype := TGocciaObjectValue.Create;
  // ... register methods on SharedPrototype ...
  CurrentRealm.SetSlot(GIteratorPrototypeSlot, SharedPrototype);
end;

initialization
  GIteratorPrototypeSlot := RegisterRealmSlot('Iterator.prototype');
```

Read live every time — never cache the prototype pointer in a Pascal variable that outlives a single call site, because the cached pointer becomes a dangling reference the moment the engine that owns it is freed.

#### Owned-slot pattern (a `TGocciaSharedPrototype` helper)

This is what `Goccia.Values.MapValue.pas` and the other ~22 `TGocciaSharedPrototype` users do:

```pascal
var
  GMapSharedSlot: TGocciaRealmOwnedSlotId;

function GetMapShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GMapSharedSlot))
  else
    Result := nil;
end;

procedure TGocciaMapValue.InitializePrototype;
var
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetMapShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  // ... register methods on Shared.Prototype ...
  CurrentRealm.SetOwnedSlot(GMapSharedSlot, Shared);
end;

initialization
  GMapSharedSlot := RegisterRealmOwnedSlot('Map.SharedPrototype');
```

`TGocciaSharedPrototype.Destroy` unpins both `FPrototype` and `FMethodHost`, so realm tear-down freeing the helper releases everything atomically — even before the next GC pass runs.

#### Stale-cache antipattern (do not do this)

`TGocciaClassValue` previously cached `Function.prototype` in a `threadvar FDefaultPrototype` and reused it on every `new`-able class. After engine recreation that threadvar still pointed at the dead `Function.prototype` from the previous realm, so `Object.getPrototypeOf(NewConstructor) === Function.prototype` started returning `false` on the first construct of every fresh engine. The fix was to read the prototype live each call via `TGocciaFunctionBase.GetSharedPrototype`. **Do not cache realm-scoped objects in `threadvar`s, class vars, or static singletons.** If you need a prototype, look it up through the realm every time.

## Terminology

The codebase uses specific terminology consistently:

| Term | Meaning |
|------|---------|
| **Define** | Create a new variable binding in the current scope (`DefineLexicalBinding`). Also used for built-in registration. |
| **Assign** | Re-assign the value of an existing binding (`AssignLexicalBinding`) |
| **Binding** | A name-to-value association in a scope (not a raw variable) |
| **Literal** | A value that appears directly in source code |
| **Native function** | A built-in function implemented in Pascal |
| **User function** | A function defined in GocciaScript (arrow function) |

### Define vs Assign

This distinction is critical in the codebase:

- `DefineLexicalBinding` — Creates a **new** variable in the current scope. Used for `let`/`const` declarations, function parameters, and built-in registration. Built-ins are registered using `DefineLexicalBinding(..., dtLet)` — there is no separate `DefineBuiltin` method.
- `AssignLexicalBinding` — Changes the value of an **existing** variable, walking up the scope chain. Throws `ReferenceError` if not found, `TypeError` if `const`.

## Design Rationale

### Virtual Dispatch Value System

Values follow a small class hierarchy rooted at `TGocciaValue`, with property access unified through virtual methods on the base class:

```mermaid
classDiagram
    TGocciaValue <|-- TGocciaNullLiteralValue
    TGocciaValue <|-- TGocciaUndefinedLiteralValue
    TGocciaValue <|-- TGocciaBooleanLiteralValue
    TGocciaValue <|-- TGocciaNumberLiteralValue
    TGocciaValue <|-- TGocciaStringLiteralValue
    TGocciaValue <|-- TGocciaObjectValue
    TGocciaValue <|-- TGocciaError

    TGocciaObjectValue <|-- TGocciaArrayValue
    TGocciaObjectValue <|-- TGocciaFunctionValue
    TGocciaObjectValue <|-- TGocciaClassValue
    TGocciaObjectValue <|-- TGocciaInstanceValue
```

The base `TGocciaValue` declares virtual `GetProperty` and `SetProperty` methods with safe defaults (`nil` / no-op). Each value type overrides these to implement its property semantics — objects walk the prototype chain, arrays handle numeric indices, instances invoke getters/setters, etc.

Beyond property access, the base class provides two additional virtual methods for type discrimination:

- **`IsPrimitive`** — Returns `False` by default; overridden to return `True` by all primitive types (`Null`, `Undefined`, `Boolean`, `Number`, `String`). Replaces 5-way `is` check chains at call sites like `ToPrimitive`.
- **`IsCallable`** — Returns `False` by default; overridden to return `True` by `TGocciaFunctionBase` (all function types) and `TGocciaClassValue` (callable via `new`). Replaces 2-way `is` check chains at call sites like `Function.prototype.call/apply/bind` and array callback validation.

**Why virtual dispatch?**

- **Single hierarchy** — Every type that supports property access is in the `TGocciaValue` hierarchy. Virtual methods leverage this directly without extra interface indirection.
- **Simple call sites** — `Value.GetProperty(Name)` is a single virtual call. `Value.IsPrimitive` and `Value.IsCallable` are likewise single VMT calls. No capability queries, no casting.
- **Safe defaults** — The base class returns `nil` for `GetProperty`, no-ops for `SetProperty`, `False` for `IsPrimitive`, and `False` for `IsCallable`, so the evaluator can call these on any value without type-checking first.
- **Extensible** — New value types added to the hierarchy automatically participate by overriding the virtual methods.
- **Performance** — A single VMT call replaces multi-`is` type check chains. For `IsPrimitive`, this replaces five sequential `is` checks; for `IsCallable`, two. Benchmarks show 10-20% improvement in class-related operations where these checks are on the hot path. See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md) for the benchmark analysis comparing virtual, interface, and manual VMT dispatch.

### Centralized Keyword Constants

JavaScript keyword literals are defined as named constants in two units — `Goccia.Keywords.Reserved.pas` for reserved keywords and `Goccia.Keywords.Contextual.pas` for contextual keywords:

```pascal
// Goccia.Keywords.Reserved.pas
const
  KEYWORD_THIS      = 'this';
  KEYWORD_SUPER     = 'super';
  KEYWORD_NULL      = 'null';
  // ... 33 reserved keywords total

// Goccia.Keywords.Contextual.pas
const
  KEYWORD_GET       = 'get';
  KEYWORD_SET       = 'set';
  KEYWORD_TYPE      = 'type';
  KEYWORD_INTERFACE = 'interface';
  // ... 12 contextual keywords total
```

**Why dedicated units?**

- **No magic strings** — The evaluator, scope, parser, and other units reference `KEYWORD_THIS` or `KEYWORD_GET` instead of `'this'` or `'get'`, preventing typos and enabling compiler-checked usage.
- **Reserved vs contextual** — Reserved keywords always produce a dedicated token type and cannot be used as identifiers. Contextual keywords have special meaning only in specific syntactic positions (e.g., `get`/`set` in object literals, `type`/`interface` in declaration position) but are otherwise valid identifiers.
- **Minimal dependencies** — Neither unit has `uses` clause dependencies, so any unit can import them without introducing circular references.
- **Single source of truth** — All keyword strings are defined once. The lexer's token mapping, the parser's contextual checks, and the evaluator's identifier handling all reference the same constants.

### Inlining Hot-Path Methods

Small, frequently-called non-virtual methods are marked `inline` to eliminate call overhead:

| Method | Unit | Rationale |
|--------|------|-----------|
| `GetValue(Name)` | `Goccia.Scope` | Called on every identifier lookup |
| `ResolveIdentifier(Name)` | `Goccia.Scope` | Unifies `this`/keyword checks with scope lookup |
| `ContainsOwnLexicalBinding(Name)` | `Goccia.Scope` | Dictionary lookup wrapper |
| `Contains(Name)` | `Goccia.Scope` | Scope chain containment check |
| `IsNegativeZero(Value)` | `Goccia.Evaluator.Comparison` | Trivial enum comparison |

**Why selective inlining?**

- **Virtual methods cannot be inlined** — `GetProperty`, `IsPrimitive`, `IsCallable`, and scope chain walkers (`GetThisValue`, `GetOwningClass`, `GetSuperClass`) rely on VMT dispatch and are never candidates for inlining.
- **Only non-virtual wrappers** — Inlined methods are thin wrappers (dictionary lookups, enum comparisons) where the call overhead is significant relative to the method body.
- **Measurable on hot paths** — Scope lookups happen on every identifier reference. Eliminating function call overhead here compounds across deeply nested expressions.

### Singleton Special Values

Special values like `undefined`, `null`, `true`, `false`, `NaN`, `Infinity`, and `-Infinity` are singletons:

```pascal
function UndefinedValue: TGocciaValue;  // Always returns the same instance
function NullValue: TGocciaValue;       // Always returns the same instance
```

**Why singletons?**

- **Identity comparison** — `Value = UndefinedValue` is a fast pointer comparison instead of type checking.
- **Memory efficiency** — These values are created once and shared.
- **Semantic correctness** — There's only one `undefined` in JavaScript; the implementation reflects this.

### No Global Mutable State

The codebase enforces a strict rule: **no global mutable state**. All runtime state flows through explicit parameters — the `TGocciaEvaluationContext` record, the scope chain, and value objects.

- **`OnError` propagation** — The error handler callback is stored on `TGocciaScope` (`FOnError` field) and propagated to child scopes via `CreateChild`. Functions retrieve it from their closure scope, which is always the scope where they were defined.
- **`LoadModule` propagation** — The module loading callback is stored on `TGocciaScope` (`FLoadModule` field) and propagated to child scopes via `CreateChild`, following the same pattern as `OnError`. Functions retrieve it from their closure scope. This enables dynamic `import()` expressions (ES2026 §13.3.10) to work inside functions, conditionals, and callbacks — not just at the top level.
- **`CurrentFilePath` propagation** — Each `TGocciaEvaluationContext` carries the path of the file being evaluated. The interpreter sets this to `FFileName` for the main script and to the resolved module path for each module. The evaluator passes it to `LoadModule` so import paths are resolved relative to the importing file, not the working directory.

This keeps the evaluator fully reentrant — all dependencies are explicit, making the code safe for concurrent execution and trivial to reason about.

### Configurable Built-ins

Built-ins are registered via a `TGocciaGlobalBuiltins` set of flags:

```pascal
TGocciaGlobalBuiltin = (ggTestAssertions, ggBenchmark, ggFFI);
```

Standard built-ins (console, Math, Array, Number, Promise, JSON, Symbol, Set, Map, Temporal, ArrayBuffer, etc.) are always registered -- no flag-gating required. The enum now contains only special-purpose built-ins that are opt-in.

**Why configurable for special-purpose built-ins?**

- **Testing** — The GocciaTestRunner enables `ggTestAssertions` to inject `describe`, `test`, and `expect` without polluting the normal runtime.
- **Benchmarking** — The GocciaBenchmarkRunner enables `ggBenchmark` to inject `suite` and `bench`.
- **FFI** — `ggFFI` enables the Foreign Function Interface for calling native shared libraries, which is disabled by default for security.

### Global Function Placement

`parseInt`, `parseFloat`, `isNaN`, and `isFinite` are available **only** as `Number.*` static methods, not as global functions. In ECMAScript, these exist in both places — the global versions are legacy leftovers. `parseInt` and `parseFloat` behave identically to their `Number.*` counterparts, but global `isNaN` and `isFinite` coerce their argument to a number first, while `Number.isNaN` and `Number.isFinite` return `false` for any non-number. GocciaScript keeps these functions on the `Number` object where they belong, avoiding global namespace pollution. See [language.md](language.md) for the polyfill pattern.

### Standardized Argument Validation

Built-in functions use `TGocciaArgumentValidator` (`Goccia.Arguments.Validator.pas`) for consistent argument count and type checking:

```pascal
TGocciaArgumentValidator.RequireExactly(Args, 1, 'Array.isArray');
TGocciaArgumentValidator.RequireAtLeast(Args, 1, 'Array.from');
```

Benefits:

- **Consistent error messages** — All argument errors follow the same format: `"FunctionName expected N arguments, but got M"`.
- **Single point of change** — Validation logic and error formatting live in one place.
- **Reduced boilerplate** — Each call site is a single line instead of a multi-line if/then/throw pattern.

### String Interning — Attempted and Rejected

String interning (caching `TGocciaStringLiteralValue` instances in a `TDictionary<string, TGocciaStringLiteralValue>` keyed by content, returning cached instances from `RuntimeCopy` and `ToStringLiteral`) was implemented and benchmarked. The results showed a **net -4% regression** across 172 benchmarks, with 49 regressions, only 3 improvements, and 120 unchanged.

**Why it doesn't help:**

- **Dictionary lookup cost exceeds allocation cost.** FreePascal's allocator is fast. A `TDictionary.TryGetValue` call involves hashing the string (O(n) in string length) plus a hash-table probe, which is more expensive than simply allocating a short-lived `TGocciaStringLiteralValue` and letting the GC reclaim it later.
- **Low hit rate on hot paths.** `ToStringLiteral` on numbers produces mostly unique strings (`"42"`, `"3.14"`, etc.) that never hit the cache, paying the hash cost with zero benefit. This path is called frequently in arithmetic-heavy benchmarks.
- **`RuntimeCopy` is the wrong interception point.** Every string literal evaluation goes through `RuntimeCopy`. Adding a dictionary lookup to this universal hot path penalizes all string operations, including those that create one-off strings (concatenation results, method return values).
- **GC pressure is not the bottleneck.** The SmallInt cache works for numbers because integer equality is a single comparison. String equality requires content comparison, so the lookup cost scales with string length rather than being O(1).

**The `SmallInt` cache works because:** integer comparison is a single machine instruction, the cache is a fixed-size array (no hashing), and the hit rate for integers 0–255 is very high in typical code. None of these properties hold for arbitrary strings.

**Do not re-attempt** dictionary-based string interning. If string allocation becomes a measurable bottleneck in future profiling, consider instead: (a) pre-allocated singletons for a small fixed set of ultra-common strings (like `SmallInt` but for `"length"`, `"undefined"`, etc.), or (b) arena/pool allocation for `TGocciaStringLiteralValue` objects to reduce per-object GC overhead without per-string hashing.

## Related documents

- [Architecture](architecture.md) — Pipelines, main layers, design direction, duplication boundaries
- [Interpreter](interpreter.md) — Tree-walk pipeline and evaluator model
- [Bytecode VM](bytecode-vm.md) — Register VM and bytecode format
- [Value system](value-system.md) — `TGocciaValue` hierarchy and property access
- [Contributing](../CONTRIBUTING.md) — Workflow and Pascal code style when you modify the repository
