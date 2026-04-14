# Code Style

*Authoritative FreePascal naming, structure, and performance conventions for the GocciaScript codebase.*

**At a glance** (details follow in the subsections):

- **Naming** — PascalCase functions and methods; `TGoccia*` classes; **`F`** = private fields; **`A`** = multi-letter parameters; full words, no abbreviations (exceptions: `AST`, `JSON`, `REPL`, `ISO`, `Utils`); locals: PascalCase, no underscores, no numeric suffixes
- **Constants** — `Goccia.Constants.*`, `Goccia.Keywords.*`, `Goccia.FileExtensions` (not string literals)
- **Performance** — `TStringBuffer` not `TStringBuilder`; purpose-built maps on hot paths; `TObjectList<T>` **named aliases** across units
- **Formatting** — `./format.pas` + Lefthook; rules under [Tooling](tooling.md)

## Pascal Conventions

### Compiler Directives

All units include the shared compiler directives file:

```pascal
{$I Goccia.inc}
```

Which sets:

```pascal
{$mode delphi}           // Delphi-compatible mode
{H+}                     // Long strings by default
{$overflowchecks on}     // Runtime overflow checking
{$rangechecks on}        // Runtime range checking
{$modeswitch advancedrecords}   // Records with methods
```

Overflow and range checks are **enabled** — correctness is prioritized over raw performance.

### Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| Units | `Goccia.<Category>.<Name>.pas` | `Goccia.Values.Primitives.pas` |
| Classes | `TGoccia<Name>` prefix | `TGocciaObjectValue` |
| Interfaces | `I<Name>` prefix | `IGocciaSerializable` |
| Private fields | `F` prefix | `FValue`, `FPrototype` |
| Functions/Procedures | PascalCase | `Evaluate`, `EvaluateBinary` |
| Methods | PascalCase | `GetProperty`, `ToStringLiteral` |
| Constants | PascalCase or UPPER_CASE | `DefaultPreprocessors`, `NaN` |
| Parameters | `A` prefix (multi-letter only) | `AScope`, `AValue`, `AFileName` |
| Local variables | PascalCase; **no** underscores; **no** trailing digit suffixes (`Value1`, `temp2`) | `CandidateScope`, `ResolvedName` |
| Enums | `TGoccia<Name>` for type, lowercase prefix for values | `TGocciaScopeKind`, `skGlobal` |

Do **not** use `snake_case` or `mixed_Case` for locals — use full words in PascalCase. Do **not** disambiguate with numeric suffixes; choose a descriptive name (`PrimaryScope`, `FallbackScope`) instead. Short single-letter names in very small scopes (e.g. loop `I`, `J`) remain acceptable.

### Centralized Constants

Use centralized constant units instead of hardcoded string literals:

- **Keywords** — Use `Goccia.Keywords.Reserved` (`KEYWORD_THIS`, `KEYWORD_SUPER`, etc.) and `Goccia.Keywords.Contextual` (`KEYWORD_GET`, `KEYWORD_SET`, etc.) instead of raw `'this'`, `'get'` strings.
- **File extensions** — Use `Goccia.FileExtensions` constants (`EXT_JS`, `EXT_JSX`, `EXT_TS`, `EXT_TSX`, `EXT_MJS`, `EXT_JSON`, `EXT_JSON5`, `EXT_JSONL`, `EXT_TOML`, `EXT_YAML`, `EXT_YML`, `EXT_TXT`, `EXT_MD`, `EXT_GBC`) instead of raw string literals. Use the appropriate shared arrays and helpers (`ScriptExtensions`, `ModuleImportExtensions`, `IsScriptExtension`, `IsTextAssetExtension`, `IsJSXNativeExtension`, etc.) instead of duplicating extension lists.

Adding a new keyword or file extension requires a single change in the constants unit — all consumers pick it up automatically.

### No Magic Numbers

Avoid bare numeric literals in the `implementation` section. Extract them into named constants so the value is defined once and the name conveys intent:

```pascal
// Wrong — magic number repeated and unexplained
if ACapacity > 0 then
  Result.FCap := ACapacity
else
  Result.FCap := 64;

// Correct — named constant, defined once
const
  DEFAULT_CAPACITY = 64;

if ACapacity > 0 then
  Result.FCap := ACapacity
else
  Result.FCap := DEFAULT_CAPACITY;
```

When the same constant is used in both the `interface` section (e.g., as a default parameter value) and the `implementation` section (e.g., as a fallback), declare it in `interface` so both sites can reference it. Implementation-only constants stay in `implementation`.

Trivial literals that are self-explanatory in context (`0`, `1`, `-1`, `''`, `True`, `False`) do not need extraction.

### ECMAScript Spec Annotations

When implementing ECMAScript-specified behavior, annotate each function or method with a comment referencing the relevant specification section. Place the annotation immediately above the function body in the `implementation` section. For multi-step spec algorithms, also annotate individual steps inline within the function body:

```pascal
// ES2026 §23.1.3.18 Array.prototype.map(callbackfn [, thisArg])
function TGocciaArrayValue.Map(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue(AThisValue);
  // ES2026 §23.1.3.18 step 4: ArraySpeciesCreate(O, len)
  ResultArray := ArraySpeciesCreate(Arr, Arr.Elements.Count);
  // ...
end;
```

**Format:** `// ESYYYY §X.Y.Z SpecMethodName(specParams)`

- `YYYY` is the current edition year of the ECMA-262 specification (e.g., `ES2026` for 2026, `ES2027` for 2027). Use the year matching the current year.
- The section numbers reference [ECMA-262](https://tc39.es/ecma262/), the living standard.
- **The method name and parameter list must match the spec's pseudo-code exactly** — use `Array.prototype.map(callbackfn [, thisArg])`, not the Pascal implementation name `TGocciaArrayValue.Map(AArgs, AThisValue)`. The annotation is a spec cross-reference, not a Pascal signature.
- Use the full qualified name as it appears in the spec (e.g., `Array.prototype.map`, `Object.keys`, `Number.parseInt`).
- For abstract operations, use the spec's operation name and parameters (e.g., `Await(value)`, `ToPrimitive(input [, preferredType])`, `IteratorNext(iteratorRecord [, value])`).
- For individual algorithm steps within a function body, use `// ESYYYY §X.Y.Z step N: description`.

**What to annotate:**

| Category | Example |
|----------|---------|
| Built-in prototype methods | `// ES2026 §22.1.3.22 String.prototype.slice(start, end)` |
| Built-in static methods | `// ES2026 §20.1.2.1 Object.assign(target, ...sources)` |
| Built-in constructors | `// ES2026 §23.1.1.1 Array(len)` |
| Abstract operations | `// ES2026 §7.1.1 ToPrimitive(input [, preferredType])` |
| Internal algorithms | `// ES2026 §7.3.35 ArraySpeciesCreate(originalArray, length)` |
| Algorithm steps (inline) | `// ES2026 §23.1.3.18 step 4: ArraySpeciesCreate(O, len)` |

**TC39 proposals** not yet merged into ECMA-262 use the proposal name instead of a section number:

```pascal
// TC39 Temporal §5.5.3 Temporal.Duration.prototype.add(other)
// TC39 Iterator Helpers §2.1.3.1 Iterator.prototype.map(mapper)
// TC39 Set Methods §2.1 Set.prototype.union(other)
```

**What not to annotate:** Internal GocciaScript helpers that don't correspond to a spec algorithm (e.g., `EvaluateStatements`, `SpreadIterableInto`, Pascal-specific utilities).

### No Abbreviations

Class names, function names, method names, and type names must use **full words** — do not abbreviate. This keeps the codebase consistent and self-documenting.

```pascal
// Correct
TGarbageCollector
MarkReferences
IsExternalDeclaration
DateTimeAdd

// Wrong — abbreviated
TGocciaGC
GCMarkReferences
IsExternalDecl
DTAdd
```

**Exceptions:** Industry-standard abbreviations are kept as-is: `AST`, `JSON`, `REPL`, `ISO`, `Utils`.

### Generic Lists for Class Types

Prefer `TObjectList<T>` over `TList<T>` when `T` is a class. `TObjectList` makes ownership semantics explicit via `OwnsObjects` — use `Create` (or `Create(True)`) for owning collections, `Create(False)` for non-owning references.

**Named type aliases:** When a generic specialization like `TObjectList<TSomeClass>` is used across multiple compilation units, define a **single named type alias** in the unit that declares `TSomeClass`. This ensures FPC produces one VMT for the specialization, avoiding "Invalid type cast" failures when `{$OBJECTCHECKS ON}` performs cross-unit type checks.

```pascal
// In Goccia.Values.Primitives.pas (where TGocciaValue is declared)
TGocciaValueList = TObjectList<TGocciaValue>;

// In Goccia.Scope.pas (where TGocciaScope is declared)
TGocciaScopeList = TObjectList<TGocciaScope>;
```

All consumers import the alias from the declaring unit — never re-specialize `TObjectList<TGocciaValue>` or `TObjectList<TGocciaScope>` locally:

```pascal
// Correct — uses the shared alias
FElements: TGocciaValueList;
FManagedScopes: TGocciaScopeList;

// Wrong — local re-specialization creates a separate VMT
FElements: TObjectList<TGocciaValue>;
FManagedScopes: TObjectList<TGocciaScope>;
```

**Why `TObjectList(False)` instead of `TList`?** Even when the collection does not own its elements (e.g., the GC's managed scopes list, which uses manual mark-and-sweep), using `TObjectList<T>.Create(False)` with a named alias keeps the VMT consistent. `TList<T>` and `TObjectList<T>` produce incompatible VMTs, so mixing them across units reintroduces the same cross-unit type check failures. See [spikes/fpc-generics-performance.md](../spikes/fpc-generics-performance.md) for the benchmark analysis confirming generics have zero runtime cost.

### Hash Map Selection

The codebase provides purpose-built hash maps that replace `TDictionary` on hot paths. Choose based on key type and ordering requirements:

| Use case | Map type | Notes |
|----------|----------|-------|
| String keys, insertion order | `TOrderedStringMap<V>` | 4-6x faster inserts than `TDictionary` at N=20-100; `static inline` DJB2 hash/equality; tracks deleted buckets and compacts after delete-heavy phases to bound probe chains |
| Generic keys, insertion order | `TOrderedMap<K,V>` | Virtual `HashKey`/`KeysEqual`; default: DJB2 over raw key bytes |
| Any key, unordered | `THashMap<K,V>` | Backshift deletion (no tombstones); `static inline` hash/equality; 2x faster inserts for pointer keys |
| Scope bindings | `TOrderedStringMap<V>` | Hash-based O(1) lookup per scope level; chain walking in `TGocciaScope` |
| Cold-path / diagnostic | `TDictionary<K,V>` | Acceptable where performance is not critical |

**Never use** `TFPDataHashTable` — it has catastrophic insert performance (400,000 ns/insert vs 50 ns for `TOrderedStringMap` at N=20). See [spikes/fpc-hashmap-performance.md](../spikes/fpc-hashmap-performance.md) for the full benchmark analysis.

**API compatibility:** All custom maps share the same core API as `TDictionary`: `Add`, `AddOrSetValue`, `TryGetValue`, `ContainsKey`, `Remove`, `Clear`. Iteration uses `Keys`, `Values`, or `ToArray` returning dynamic arrays (not enumerators), so use indexed `for` loops instead of `for...in`.

### Function and Method Names

All `function`, `procedure`, `constructor`, and `destructor` names must be **PascalCase** — the first letter of each word is uppercase, no underscores. This applies to both free functions and class methods:

```pascal
// Correct
function EvaluateBinary(const AExpr: TGocciaBinaryExpression): TGocciaValue;
procedure RegisterBuiltin(const AName: string; const AValue: TGocciaValue);
class function CreateFromPairs(const APairs: TGocciaArrayValue): TGocciaMapValue;

// Wrong — camelCase or snake_case
function evaluateBinary(const AExpr: TGocciaBinaryExpression): TGocciaValue;
procedure register_builtin(const AName: string; const AValue: TGocciaValue);
```

**Exception:** External C function bindings (declared with `external`) retain their original C naming (e.g., `clock_gettime`).

This is auto-fixed by `./format.pas`.

### Uses Clauses

Each unit in the `uses` clause must appear on its own line, following [Embarcadero's recommended style](https://docwiki.embarcadero.com/RADStudio/Athens/en/Source_Code_Files_Units_and_Their_Structure). Units are grouped by category with a blank line between groups, and sorted alphabetically within each group:

1. **System units** — FPC standard library (`Classes`, `SysUtils`, `Math`, `Generics.Collections`, etc.)
2. **Third-party / non-prefixed project units** — units without `Goccia.*` prefix and without an `in` path (`TimingUtils`, etc.)
3. **Project units** — `Goccia.*` namespaced units
4. **Relative units** — units with an explicit `in` path (`FileUtils in 'units/FileUtils.pas'`, etc.)

```pascal
uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Scope,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';
```

This ordering is enforced automatically by `./format.pas` via Lefthook pre-commit hook.

### Parameters

All function and procedure parameters must follow these rules:

1. **`A` prefix** — Every parameter name with two or more characters starts with `A` (e.g., `AScope`, `AValue`, `AFileName`). This distinguishes parameters from fields (`F` prefix) and local variables. **Single-letter names** (e.g., `A`, `B`, `E`, `T`) are left as-is — the `A` prefix is not applied to them.

2. **`const` where possible** — Use `const` for parameters that are not modified within the function body. This applies to all types: objects, strings, integers, records, etc. For records, `const` prevents field modification, so only omit it when the function needs to mutate the record locally.

```pascal
// Correct — multi-letter parameters get A prefix + const
procedure ProcessValue(const AValue: TGocciaValue; const AName: string);
function CreateChild(const AKind: TGocciaScopeKind): TGocciaScope;

// Correct — single-letter parameters keep their name
function DefaultCompare(constref A, B: TGocciaValue): Integer;
function DoSubtract(const A, B: Double): Double;

// Wrong — missing A prefix on multi-letter name, missing const
procedure ProcessValue(Value: TGocciaValue; Name: string);
```

Exceptions to `const`:
- Parameters that are genuinely modified inside the function (e.g., loop counters, accumulator records)
- `out` parameters (which are written, not read)
- `var` parameters (which are both read and written)

### File Organization

Each unit follows this structure:

```pascal
unit Goccia.Category.Name;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.Scope,
  Goccia.Values.Primitives;

type
  // Type declarations (classes, interfaces, records, enums)

// Free function declarations (for evaluator modules)

implementation

uses
  Goccia.Evaluator;

// Implementation

end.
```

**Key principle:** Use the `implementation uses` clause to break circular dependencies. The interface section declares only what's needed for the public API; heavy dependencies go in the implementation section.

### Unit Naming

Units are organized by category using dot-separated names:

| Category | Purpose | Examples |
|----------|---------|---------|
| `Goccia.Values.*` | Value type hierarchy | `Primitives`, `ObjectValue`, `ArrayValue`, `ClassValue` |
| `Goccia.Builtins.*` | Built-in object implementations | `Console`, `Math`, `JSON`, `GlobalObject` |
| `Goccia.Evaluator.*` | Evaluator sub-modules | `Arithmetic`, `Bitwise`, `Comparison`, `Assignment` |
| `Goccia.AST.*` | AST node definitions | `Node`, `Expressions`, `Statements` |
| `Goccia.Arguments.*` | Function argument handling | `Collection`, `Converter`, `Validator` |

### Collection Return Types

Avoid returning `TArray<T>` from public API methods. Prefer indexed access (`GetItem(Index)` + `Count`) or returning the dictionary's own `TKeyCollection` / `TValueCollection` which support `for..in` without allocating an intermediate array. This is consistent with how the codebase iterates dictionaries throughout (e.g., `for Key in Dictionary.Keys do`).

When a method only needs to expose elements for iteration, an indexed getter with a count property is the most lightweight approach — no allocation, no ownership ambiguity.

## Code Organization Principles

1. **Explicitness** — Modules, classes, methods, and properties use explicit, descriptive names even at the cost of verbosity. Shortcuts and abbreviations are avoided.

2. **OOP over everything** — Rely on type safety of specialized classes. Each concept gets its own class rather than using generic data structures.

3. **Separation of concerns** — Each unit has a single, clear responsibility. The evaluator doesn't know about built-ins; the engine doesn't know about AST structure.

4. **Minimal public API** — Units expose only what's needed. Implementation details stay in the `implementation` section.

5. **No global mutable state** — State flows through parameters (evaluation context, scope) rather than global variables. The only globals are immutable singletons.
