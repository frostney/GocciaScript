# Design Decisions

This document explains the key design decisions behind GocciaScript — the "why" behind the choices.

## Pure Evaluator Functions

The evaluator (`Goccia.Evaluator.pas`) is designed around pure functions — given the same AST node and evaluation context, the evaluator always produces the same result with no side effects.

**Why this matters:**

- **Testability** — Pure functions are trivially testable in isolation.
- **Reasoning** — No hidden state mutations make the evaluation logic easier to understand and debug.
- **Parallelism potential** — Pure evaluation is inherently safe for concurrent execution.
- **Composability** — Evaluator sub-modules (`Arithmetic`, `Bitwise`, `Comparison`, etc.) compose cleanly because they don't share mutable state.
- **ECMAScript conformance** — `ToPrimitive` (`Goccia.Values.ToPrimitive.pas`) is a standalone abstract operation (trying `valueOf` then `toString` on objects) used by the `+` operator and available to any module. The arithmetic module uses floating-point modulo (not integer) with proper NaN/Infinity propagation. The comparison module implements the Abstract Relational Comparison algorithm with type coercion.

State changes (variable bindings, object mutations) happen through the scope and value objects passed in the `TGocciaEvaluationContext`, not through evaluator-internal state.

## Interface-Based Value System

Values follow a small class hierarchy rooted at `TGocciaValue`, with capabilities expressed through FreePascal interfaces:

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

Capabilities are expressed through interfaces:

| Interface | Purpose |
|-----------|---------|
| `IPropertyMethods` | Object-like property access (`GetProperty`, `AssignProperty`, `DefineProperty`, etc.) |
| `IIndexMethods` | Array-like indexed access (`GetLength`, `GetElement`, `SetElement`) |

**Why interfaces over inheritance?**

- **Interface segregation** — A value type only implements what it supports. Arrays implement `IIndexMethods`; objects implement `IPropertyMethods`. There's no god-class with unused virtual methods.
- **Capability checking** — The evaluator checks `if Value is IPropertyMethods` rather than maintaining a type enum, making it extensible without modifying existing code.
- **FreePascal compatibility** — Interfaces provide reference counting, which helps with memory management.
- **Lean surface** — Interfaces that were never queried at runtime (`IValueOf`, `IStringTag`, `IFunctionMethods`) have been removed. The methods they declared still exist on the class hierarchy but without the interface indirection overhead.

## Singleton Special Values

Special values like `undefined`, `null`, `true`, `false`, `NaN`, `Infinity`, and `-Infinity` are singletons:

```pascal
function UndefinedValue: TGocciaValue;  // Always returns the same instance
function NullValue: TGocciaValue;       // Always returns the same instance
```

**Why singletons?**

- **Identity comparison** — `Value = UndefinedValue` is a fast pointer comparison instead of type checking.
- **Memory efficiency** — These values are created once and shared.
- **Semantic correctness** — There's only one `undefined` in JavaScript; the implementation reflects this.

## Number Representation

Numbers use a dual representation — a `Double` for normal values and a `TGocciaNumberSpecialValue` enum for `NaN`, `+Infinity`, `-Infinity`, and `-0`:

**Why not just `Double`?**

- **NaN identity** — IEEE 754 `NaN ≠ NaN`, but JavaScript needs `NaN` to be identifiable. A dedicated enum avoids floating-point comparison pitfalls.
- **Negative zero** — `-0` and `+0` are equal in IEEE 754 but distinguishable in JavaScript (`Object.is(-0, +0)` is `false`). Explicit tracking prevents this from being lost.
- **Display correctness** — `NaN.toString()` must return `"NaN"`, not some floating-point artifact.

## Scope Chain Design

Scopes form a tree with parent pointers, implementing lexical scoping:

- **`CreateChild` factory method** — Scopes are never instantiated directly. `CreateChild` ensures proper parent linkage and scope kind propagation.
- **Temporal Dead Zone** — `let`/`const` bindings are registered before initialization, enforcing TDZ semantics (accessing before `=` throws `ReferenceError`).
- **Specialized scopes** — `TGocciaCatchScope`, `TGocciaGlobalScope`, and `TGocciaClassScope` handle their unique semantics (catch parameter shadowing, global object, superclass tracking).

## Property Descriptor System

Object properties follow ECMAScript's property descriptor model:

- **Data descriptors** — `{ value, writable, enumerable, configurable }`
- **Accessor descriptors** — `{ get, set, enumerable, configurable }`
- **Insertion order** — Properties maintain their creation order, matching JavaScript's `Object.keys()` ordering guarantee.

This is more complex than a simple key-value map, but it's necessary for `Object.defineProperty`, getters/setters, and non-enumerable properties like prototype methods.

Class getters and setters are stored as accessor descriptors on the class prototype. `TGocciaObjectValue.GetProperty` and `AssignProperty` are `virtual`, and `TGocciaInstanceValue` overrides both to intercept property access — checking the prototype for accessor descriptors and invoking getter/setter functions with the instance as `this` context.

## Private Field Storage

Private fields use **composite keys** (`ClassName:FieldName`) in the instance's private property dictionary. This solves the inheritance shadowing problem where a base class and a derived class both declare a private field with the same name — in JavaScript, `Base.#x` and `Derived.#x` are completely separate slots.

- **Storage** — Private fields are stored on `TGocciaInstanceValue.FPrivateProperties` using keys like `"Base:x"` and `"Derived:x"`.
- **Access resolution** — When a method accesses `this.#x`, the evaluator resolves which class declared the method (via `__owning_class__` in the scope chain) and uses that class name to build the composite key.
- **Private getters/setters** — Stored separately from public ones on `TGocciaClassValue` in `FPrivateGetters`/`FPrivateSetters`, because they don't participate in the prototype's property descriptor chain.
- **Declaration order** — Instance property initializers run in source declaration order, enforced via `TStringList` order tracking from the parser through to the class value.

## Error Handling Strategy

GocciaScript uses a layered error approach:

1. **Compile-time errors** (lexer/parser) use Pascal exceptions (`TGocciaLexerError`, `TGocciaSyntaxError`) — these terminate parsing immediately.
2. **Runtime errors** use a callback pattern (`OnError` in `TGocciaEvaluationContext`) — this keeps evaluator functions pure.
3. **JavaScript-level errors** use `TGocciaThrowValue` for `throw` statements and `try/catch` — these flow through the evaluator's return path.
4. **`try-finally` without `catch`** — The evaluator wraps the Pascal `try...except` in a Pascal `try...finally` to guarantee the JS `finally` block runs before exceptions propagate, even when no `catch` clause exists.
5. **`break` in `switch`** — Uses `TGocciaBreakSignal` (a Pascal exception) to exit `switch` cases. The `EvaluateSwitch` function catches this signal to implement JavaScript's fall-through-until-break semantics.

**Centralized error construction** — `Goccia.Values.ErrorHelper.pas` provides `ThrowTypeError`, `ThrowRangeError`, `ThrowReferenceError`, and `CreateErrorObject` helpers. All error throw sites across the codebase use these helpers instead of manually building error objects, reducing duplication and ensuring consistent error formatting.

**Why not exceptions everywhere?** Pascal exceptions disrupt the pure-function model of the evaluator. The callback pattern allows the evaluator to signal errors without unwinding the call stack, making control flow explicit. The exceptions to this rule (`TGocciaThrowValue`, `TGocciaReturnValue`, `TGocciaBreakSignal`) are used only for non-local exits where unwinding is the intended behavior.

## Mark-and-Sweep Garbage Collector

GocciaScript runs inside a FreePascal host with manual memory management, but the interpreter itself has no built-in memory management for the values it creates. In long-running contexts — benchmarking, the REPL, or extended user sessions — the heap grows unboundedly without automatic reclamation. The runtime therefore uses a tracing garbage collector (`Goccia.GarbageCollector.pas`) to manage the lifecycle of interpreter-created values.

**Why not manual memory management?**

- **Aliased references** — A value assigned to multiple variables, captured in a closure, and stored in an array has no single owner. Determining when to free it requires tracking all references.
- **Shared prototypes** — Array methods, function prototypes, and class prototypes are shared across many instances. Manual lifetime tracking would be fragile.
- **Closure captures** — Arrow functions capture their enclosing scope, creating non-obvious reference chains between scopes and values.

**Why not reference counting (via `TInterfacedObject`)?**

`TGocciaValue` inherits from `TInterfacedObject`, which provides automatic reference counting. However, values are stored as class references (`TGocciaValue`), not interface references. Switching to interface variables throughout the evaluator would require a large-scale refactor and introduce circular reference issues (objects referencing their prototypes and vice versa).

**Why mark-and-sweep?**

- **Simplicity** — Two phases (mark reachable, sweep unreachable) with straightforward implementation.
- **Handles cycles** — Circular references between objects, closures, and scopes are collected correctly.
- **Measurable impact** — Running `GC.Collect` before benchmark measurement rounds reduced ops/sec variance from 20-30% to 1-3%.

**AST literal ownership:**

The parser creates `TGocciaValue` instances (numbers, strings, booleans) and stores them inside `TGocciaLiteralExpression` AST nodes. These values are owned by the AST, not the GC. `TGocciaLiteralExpression.Create` calls `TGocciaGC.Instance.UnregisterValue` to remove the value from GC tracking, and `TGocciaLiteralExpression.Destroy` frees the value (unless it is a singleton like `UndefinedValue`, `TrueValue`, or `FalseValue`).

When the evaluator encounters a literal expression, it calls `Value.RuntimeCopy` to produce a fresh GC-managed runtime value. This cleanly separates compile-time constants (owned by the AST) from runtime values (managed by the GC). The overhead is minimal: integers 0-255 hit the `SmallInt` cache (zero allocation), booleans return singletons, and strings benefit from FreePascal's copy-on-write semantics.

## Configurable Built-ins

Built-ins are registered via a `TGocciaGlobalBuiltins` set of flags:

```pascal
TGocciaGlobalBuiltin = (ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
                         ggGlobalNumber, ggPromise, ggJSON, ggSymbol,
                         ggSet, ggMap, ggTestAssertions);
```

**Why configurable?**

- **Security** — Embedding environments can restrict available APIs. A sandboxed script might not get `console`.
- **Testing** — The TestRunner enables `ggTestAssertions` to inject `describe`, `test`, and `expect` without polluting the normal runtime.
- **Minimal footprint** — Only register what's needed.

## Global Function Placement

`parseInt`, `parseFloat`, `isNaN`, and `isFinite` are available **only** as `Number.*` static methods, not as global functions. In ECMAScript, these exist in both places — the global versions are legacy leftovers. `parseInt` and `parseFloat` behave identically to their `Number.*` counterparts, but global `isNaN` and `isFinite` coerce their argument to a number first, while `Number.isNaN` and `Number.isFinite` return `false` for any non-number. GocciaScript keeps these functions on the `Number` object where they belong, avoiding global namespace pollution. See [language-restrictions.md](language-restrictions.md) for the polyfill pattern.

## Build System

The build script (`build.pas`) is a FreePascal script executed via `instantfpc` — a cross-platform, out-of-the-box solution within the FreePascal ecosystem that requires no external build tools.

## Testing Strategy

JavaScript end-to-end tests are the **primary** testing mechanism. Every new feature or bug fix must include JavaScript tests that validate the behavior through the full pipeline (lexer → parser → evaluator).

- **Specification by example** — Each test file is a runnable specification of expected behavior.
- **End-to-end validation** — Tests exercise the full pipeline, catching integration issues that unit tests would miss.
- **Readable specifications** — JavaScript test files are readable by anyone familiar with Jest/Vitest conventions.
- **Source of truth** — If a behavior isn't covered by a JavaScript test, it isn't guaranteed.

Pascal unit tests (`*.Test.pas`) exist as a secondary layer for internal implementation details not reachable from script code.
