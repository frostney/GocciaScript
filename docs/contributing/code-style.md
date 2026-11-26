# Code Style

*Authoritative FreePascal naming, structure, and performance conventions for the GocciaScript codebase.*

## Executive Summary

- **Naming** — PascalCase functions, `TGoccia*` classes, `F` prefix fields, `A` prefix parameters, no abbreviations
- **Constants** — Use centralized constant units (`Goccia.Constants.*`, `Goccia.Keywords.*`, `Goccia.FileExtensions`) instead of hardcoded literals
- **Performance** — Use `TStringBuffer` (not `TStringBuilder`), purpose-built hash maps (not `TDictionary`) on hot paths, and `TObjectList<T>` type aliases for cross-unit generics
- **Auto-formatting** — `./format.pas` enforces uses clause ordering, PascalCase naming, and parameter prefixes; see [Tooling](tooling.md) for setup

**At a glance** (details follow in the subsections):

- **Naming** — PascalCase functions and methods; `TGoccia*` classes; **`F`** = private fields; **`A`** = multi-letter parameters; full words, no abbreviations (exceptions: `AST`, `JSON`, `GocciaREPL`, `ISO`, `Utils`); locals: PascalCase, no underscores, no numeric suffixes
- **Constants** — `Goccia.Constants.*`, `Goccia.Keywords.*`, `Goccia.FileExtensions` (not string literals)
- **Performance** — `TStringBuffer` not `TStringBuilder`; purpose-built maps on hot paths; `TObjectList<T>` **named aliases** across units
- **Formatting** — `./format.pas` + Lefthook; rules under [Tooling](tooling.md)

## Pascal Conventions

### Compiler Directives

Every unit outside `source/engine` includes the shared compiler directives file:

```pascal
{$I Goccia.inc}
```

Which includes `Shared.inc` and so sets:

```pascal
{$mode delphiunicode}    // Delphi-compatible mode, UnicodeString by default
{$H+}                     // Long strings by default
{$overflowchecks on}     // Runtime overflow checking (off with -dPRODUCTION)
{$rangechecks on}        // Runtime range checking (off with -dPRODUCTION)
{$modeswitch advancedrecords}   // Records with methods
```

Overflow and range checks are **enabled** in a development build of the Pascal test programs, so they catch an
out-of-range index or a wrapped integer there. The applications always build with `-dPRODUCTION`
([Build System — Build Mode Flags](../build-system.md#build-mode-flags)), which turns them off, and the engine's
own units in `source/engine` turn them off in every build (`Goccia.Checks.inc`).

Keep the entire compiler-conditional routine directive sequence on the
declaration's final line:

```pascal
function NormalizeValue(const AValue: Double): Double; {$IFDEF FPC}inline;{$ENDIF}
```

### Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| Units | `Goccia.<Category>.<Name>.pas` | `Goccia.Modules.Resolver.pas` |
| Classes | `TGoccia<Name>` prefix | `TGocciaProfiler` |
| Interfaces | `I<Name>` prefix | `IGocciaHostClock` |
| Private fields | `F` prefix | `FSource`, `FMode` |
| Functions/Procedures | PascalCase | `NumberToUint32`, `NumberToString` |
| Methods | PascalCase | `RegisterTemplate`, `BeginEntry` |
| Constants | PascalCase or UPPER_CASE | `ModuleImportExtensions`, `EXT_JSON` |
| Parameters | `A` prefix (multi-letter only) | `AScope`, `AValue`, `AFileName` |
| Local variables | PascalCase; **no** underscores; **no** trailing digit suffixes (`Value1`, `temp2`) | `CandidateScope`, `ResolvedName` |
| Enums | `TGoccia<Name>` for type, lowercase prefix for values | `TGocciaProfileModeItem`, `pmOpcodes` |

### Source Directory Layout and Namespacing

Source files live in five directories under `source/`, each with different naming rules:

| Directory | Purpose | Unit prefix | Include file | Example |
|-----------|---------|-------------|-------------|---------|
| `source/engine/` | The engine: compiler, bytecode, VM, object model, collector and built-ins, plus its JavaScript preludes in `source/engine/prelude/` | `Goccia.*` | `{$I Goccia.Checks.inc}` after the unit's own mode directives | `Goccia.VM.pas` |
| `source/units/` | GocciaScript-specific units the engine and the applications share: lexer, parser and AST, the RegExp compiler, modules and packages, coverage and profiling, error text | `Goccia.*` | `{$I Goccia.inc}` | `Goccia.Parser.pas` |
| `source/shared/` | Reusable infrastructure — not GocciaScript-specific | No prefix | `{$I Shared.inc}` | `HashMap.pas`, `StringBuffer.pas`, `CLI.Options.pas` |
| `source/generated/` | Generated source and resources consumed by the runtime; do not edit by hand | No required prefix | Generator-defined | `Generated.TimeZoneData.pas`, `Generated.TimeZoneData.res` |
| `source/app/` | CLI application entrypoints and app-level wiring | `Goccia.*` for units, `Goccia` prefix (no dot) for programs | `{$I Goccia.inc}` | `GocciaTestRunner.dpr`, `Goccia.CLI.Application.pas` |

Generated units should contain generated data, generated resource links, and the minimum schema needed to describe that data. Hand-authored lookup, decoding, validation, and other behavior belongs in `source/units/` or `source/shared/`, depending on ownership.

**When to use `Goccia.*`:** Any GocciaScript-specific unit (parser, modules, CLI wiring, reports, etc.). The engine's own units live in `source/engine/` and use `Goccia.*` too.

**`source/engine/` is outside these Pascal rules.** `./format.pas` covers `source/app`, `source/units` and `source/shared`, and the pre-commit format hook skips `source/engine`. The engine's units keep their own style (parameters without the `A` prefix, short type names such as `TValue` and `PObj`, threadvars for engine state) and follow the engine's own [working rules](../engine/README.md#working-rules).

**When to omit the prefix:** Generic data structures, utilities, and framework code that could be extracted to a standalone library. These live in `source/shared/` and use `{$I Shared.inc}` instead of `{$I Goccia.inc}`.

**CLI program naming:** Application `.dpr` files use `Goccia` as a prefix without a dot separator — `GocciaTestRunner.dpr`, `GocciaRunner.dpr`, `GocciaREPL.dpr`. This produces binary names like `build/GocciaTestRunner`.

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

### Locale-Independent Number Formatting

Never use bare `FloatToStr` or `FormatFloat` without an explicit `TFormatSettings` argument — these use the system decimal separator which varies by locale and breaks deterministic output.

Use `NumberToString` (from `NumericText`, in `source/shared/`) for any float-to-string conversion that must produce ECMAScript-compliant output (property keys, error messages, serialization). For formatted output that needs a specific pattern (e.g., timing, report columns), pass the settings `CreateInvariantFormatSettings` (same unit) returns to `FormatFloat`:

```pascal
// Wrong — locale-dependent, breaks on systems with comma decimal separator
Result := FloatToStr(AValue);
Result := FormatFloat('0.###', AValue);

// Correct — ES2026 §6.1.6.1.20 Number::toString, always uses '.'
Result := NumberToString(AValue);

// Correct — formatted output with invariant decimal separator
FormatSettings := CreateInvariantFormatSettings;
Result := FormatFloat('0.###', AValue, FormatSettings);
```

### ECMAScript Spec Annotations

When implementing ECMAScript-specified behavior, annotate each function or method with a comment referencing the relevant specification section. Place the annotation immediately above the function body in the `implementation` section. For multi-step spec algorithms, also annotate individual steps inline within the function body:

```pascal
// ES2026 §7.1.8 ToUint32(argument)
function NumberToUint32(const AValue: Double): UInt32;
var
  IntegerValue, WrappedValue: Double;
begin
  // ES2026 §7.1.8 step 2: non-finite values and either zero become +0.
  if IsNan(AValue) or IsInfinite(AValue) or (AValue = 0.0) then
    Exit(0);

  // ES2026 §7.1.8 steps 3-4: truncate, then reduce modulo 2^32.
  // ...
end;
```

This is `Goccia.NumberConversion`. The engine's units in `source/engine` usually cite the clause number in a brace comment instead (`{ 20.2.1.1.1 CreateDynamicFunction: ... }`).

**Format:** `// ESYYYY §X.Y.Z SpecMethodName(specParams)`

- `YYYY` is the current edition year of the ECMA-262 specification (e.g., `ES2026` for 2026, `ES2027` for 2027). Use the year matching the current year.
- The section numbers reference [ECMA-262](https://tc39.es/ecma262/), the living standard.
- **The method name and parameter list must match the spec's pseudo-code exactly** — use `ToUint32(argument)`, not the Pascal implementation name `NumberToUint32(AValue)`. The annotation is a spec cross-reference, not a Pascal signature.
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

**What not to annotate:** Internal GocciaScript helpers that don't correspond to a spec algorithm (e.g., report writers, CLI option parsing, Pascal-specific utilities).

### Documentation — Edition Year vs Proposal Stage

In the **built-in documentation** (`docs/built-ins*.md`), do not annotate features with their ECMAScript edition year. If a feature is part of the ratified standard, it is simply "the standard" — no `(ES2025)` or `(ES2026)` suffix. Only annotate features that are still **TC39 proposals** with their current stage (e.g., "Stage 2", "Stage 3"). This avoids documentation churn every time a new edition is ratified.

- Standard feature: `Math.sumPrecise(iterable)` — Precise summation of iterables.
- Proposal: `Math.clamp(x, min, max)` — Clamp to range (Stage 2 [proposal-math-clamp](https://github.com/tc39/proposal-math-clamp)).

The **language tables** (`docs/language-tables.md`) are the one place where edition years are listed, as a feature-provenance reference. **Source code** annotations (`// ES2026 §X.Y.Z`) always use the current edition year per the rules above.

### Comments That Quote JavaScript

Use `(* ... *)` rather than brace delimiters for any comment containing a
JavaScript brace. A brace comment ends at the first `}` inside it, and the
compiler then reports an "illegal character" at whatever token follows — a
column with no relation to the cause.

Neither comment form nests with itself, so a comment that discusses these
delimiters spells them out in words instead of quoting the characters.

### No Abbreviations

Class names, function names, method names, and type names must use **full words** — do not abbreviate. This keeps the codebase consistent and self-documenting.

```pascal
// Correct
TGocciaCoverageTracker
RegisterTemplate
IsScriptExtension
NumberToUint32

// Wrong — abbreviated
TGocciaCovTracker
RegTemplate
IsScriptExt
NumToU32
```

**Exceptions:** Industry-standard abbreviations are kept as-is: `AST`, `JSON`, `GocciaREPL`, `ISO`, `Utils`.

### Generic Lists for Class Types

Prefer `TObjectList<T>` over `TList<T>` when `T` is a class. `TObjectList` makes ownership semantics explicit via `OwnsObjects` — use `Create` (or `Create(True)`) for owning collections, `Create(False)` for non-owning references.

**Named type aliases:** When a generic specialization like `TObjectList<TSomeClass>` is used across multiple compilation units, define a **single named type alias** in the unit that declares `TSomeClass`. This ensures FPC produces one VMT for the specialization, avoiding "Invalid type cast" failures when `{$OBJECTCHECKS ON}` performs cross-unit type checks.

```pascal
// In Goccia.TOML.pas (where TGocciaTOMLNode is declared)
TGocciaTOMLNodeList = TObjectList<TGocciaTOMLNode>;

// In Goccia.Packages.Lockfile.pas (where TGocciaLockedPackage is declared)
TGocciaLockedPackageList = TObjectList<TGocciaLockedPackage>;
```

All consumers import the alias from the declaring unit — never re-specialize `TObjectList<TGocciaTOMLNode>` or `TObjectList<TGocciaLockedPackage>` locally:

```pascal
// Correct — uses the shared alias
FNodes: TGocciaTOMLNodeList;
FPackages: TGocciaLockedPackageList;

// Wrong — local re-specialization creates a separate VMT
FNodes: TObjectList<TGocciaTOMLNode>;
FPackages: TObjectList<TGocciaLockedPackage>;
```

**Why `TObjectList(False)` instead of `TList`?** Even when the collection does not own its elements, using `TObjectList<T>.Create(False)` with a named alias keeps the VMT consistent. `TList<T>` and `TObjectList<T>` produce incompatible VMTs, so mixing them across units reintroduces the same cross-unit type check failures. See [spikes/fpc-generics-performance.md](../spikes/fpc-generics-performance.md) for the benchmark analysis confirming generics have zero runtime cost.

### Hash Map Selection

The codebase provides purpose-built hash maps that replace `TDictionary` on hot paths. Choose based on key type and ordering requirements:

| Use case | Map type | Notes |
|----------|----------|-------|
| String keys, insertion order | `TOrderedStringMap<V>` | 4-6x faster inserts than `TDictionary` at N=20-100; `static inline` DJB2 hash/equality; tracks deleted buckets and compacts after delete-heavy phases to bound probe chains |
| Generic keys, insertion order | `TOrderedMap<K,V>` | Virtual `HashKey`/`KeysEqual`; default: DJB2 over raw key bytes |
| Any key, unordered | `THashMap<K,V>` | Backshift deletion (no tombstones); `static inline` hash/equality; 2x faster inserts for pointer keys |
| Cold-path / diagnostic | `TDictionary<K,V>` | Acceptable where performance is not critical |

**Never use** `TFPDataHashTable` — it has catastrophic insert performance (400,000 ns/insert vs 50 ns for `TOrderedStringMap` at N=20). See [spikes/fpc-hashmap-performance.md](../spikes/fpc-hashmap-performance.md) for the full benchmark analysis.

**API compatibility:** All custom maps share the same core API as `TDictionary`: `Add`, `AddOrSetValue`, `TryGetValue`, `ContainsKey`, `Remove`, `Clear`. The `Keys`, `Values`, and `ToArray` accessors return dynamic arrays, so index those with a `for` loop rather than `for...in`.

**Sequential iteration:** `TOrderedStringMap` and `TOrderedMap` also expose a key/value-pair enumerator — prefer `for Pair in Map do`, which yields entries in declaration order at O(1) amortized per step (this is how the parser and the module code iterate these maps). Do **not** iterate by driving the random-access `EntryAt(Index)` from a `for Index := 0 to Count - 1` loop: `EntryAt` scans active entries from the start (O(Index)), so the loop is O(N²). The enumerator copies each key; on a `TOrderedStringMap`, a hot loop that needs only the values, such as a destructor, walks `for Index := 0 to Map.EntryCount - 1` with `TryGetValueAtEntry`, which skips removed entries and copies no key.

### Function and Method Names

All `function`, `procedure`, `constructor`, and `destructor` names must be **PascalCase** — the first letter of each word is uppercase, no underscores. This applies to both free functions and class methods:

```pascal
// Correct
function NumberToUint32(const AValue: Double): UInt32;
procedure BeginEntry;
function RegisterTemplate(const AName, ASourceFile: string; const ALine: Integer): Integer;

// Wrong — camelCase or snake_case
function numberToUint32(const AValue: Double): UInt32;
procedure begin_entry;
```

**Exception:** External C function bindings (declared with `external`) retain their original C naming (e.g., `clock_gettime`).

This is auto-fixed by `./format.pas`.

### Uses Clauses

Each unit in the `uses` clause must appear on its own line, following [Embarcadero's recommended style](https://docwiki.embarcadero.com/RADStudio/Athens/en/Source_Code_Files_Units_and_Their_Structure). Units are grouped by category with a blank line between groups, and sorted alphabetically within each group:

1. **System units** — FPC standard library (`Classes`, `SysUtils`, `Math`, `Generics.Collections`, etc.)
2. **Third-party / non-prefixed project units** — units without `Goccia.*` prefix and without an `in` path (`TimingUtils`, etc.)
3. **Project units** — `Goccia.*` namespaced units
4. **Relative units** — units with an explicit `in` path (`FileUtils in 'source/shared/FileUtils.pas'`, etc.)

```pascal
uses
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.AST.Node,
  Goccia.Parser,

  FileUtils in 'source/shared/FileUtils.pas';
```

This ordering is enforced automatically by `./format.pas` via Lefthook pre-commit hook.

### Parameters

All function and procedure parameters must follow these rules:

1. **`A` prefix** — Every parameter name with two or more characters starts with `A` (e.g., `ASource`, `AValue`, `AFileName`). This distinguishes parameters from fields (`F` prefix) and local variables. **Single-letter names** (e.g., `A`, `B`, `E`, `T`) are left as-is — the `A` prefix is not applied to them.

2. **`const` where possible** — Use `const` for parameters that are not modified within the function body. This applies to all types: objects, strings, integers, records, etc. For records, `const` prevents field modification, so only omit it when the function needs to mutate the record locally.

```pascal
// Correct — multi-letter parameters get A prefix + const
procedure ProcessValue(const AValue: Double; const AName: string);
function RegisterTemplate(const AName, ASourceFile: string; const ALine: Integer): Integer;

// Correct — single-letter parameters keep their name
function DoSubtract(const A, B: Double): Double;

// Wrong — missing A prefix on multi-letter name, missing const
procedure ProcessValue(Value: Double; Name: string);
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

  Goccia.AST.Node,
  Goccia.SourceSpan;

type
  // Type declarations (classes, interfaces, records, enums)

// Free function declarations (for helper units)

implementation

uses
  Goccia.Error.Messages;

// Implementation

end.
```

**Key principle:** Use the `implementation uses` clause to break circular dependencies. The interface section declares only what's needed for the public API; heavy dependencies go in the implementation section.

### Unit Naming

Units are organized by category using dot-separated names:

| Category | Purpose | Examples |
|----------|---------|---------|
| `Goccia.AST.*` | AST node definitions | `Node`, `Expressions`, `Statements`, `BindingPatterns` |
| `Goccia.RegExp.*` | Regular expression compiler and matcher | `Compiler`, `VM`, `Engine`, `UnicodeData` |
| `Goccia.Modules.*` | Module resolution and loading configuration | `Resolver`, `Configuration`, `NodeResolution`, `Phases` |
| `Goccia.Packages.*` | Package installation | `Install`, `Lockfile`, `Store`, `Crawl` |
| `Goccia.Profiler.*` | Profile reports | `Compile`, `Report` |
| `Goccia.Temporal.*` | Temporal calendar and time zone data | `Calendar`, `TimeZone`, `TimeZoneData`, `Utils` |

The engine's units in `source/engine/` take a flat `Goccia.<Name>` instead (`Goccia.Core`, `Goccia.Compiler`, `Goccia.VM`, `Goccia.ObjModel`).

### Collection Return Types

Avoid returning `TArray<T>` from public API methods. Prefer indexed access (`GetItem(Index)` + `Count`) or returning the dictionary's own `TKeyCollection` / `TValueCollection` which support `for..in` without allocating an intermediate array. This is consistent with how the codebase iterates dictionaries throughout (e.g., `for Key in Dictionary.Keys do`).

When a method only needs to expose elements for iteration, an indexed getter with a count property is the most lightweight approach — no allocation, no ownership ambiguity.

## Code Organization Principles

1. **Explicitness** — Modules, classes, methods, and properties use explicit, descriptive names even at the cost of verbosity. Shortcuts and abbreviations are avoided.

2. **OOP over everything** — Rely on type safety of specialized classes. Each concept gets its own class rather than using generic data structures.

3. **Separation of concerns** — Each unit has a single, clear responsibility. The parser doesn't know about built-ins; the VM (`Goccia.VM`) runs the bytecode `Goccia.Compiler` makes from the AST, never the AST itself.

4. **Minimal public API** — Units expose only what's needed. Implementation details stay in the `implementation` section.

5. **No global mutable state** — In `source/units`, `source/shared` and `source/app`, state flows through parameters and objects rather than global variables; per-thread state (a profiler, a coverage tracker) is a threadvar instance. The engine is the exception by design: every piece of its state is a threadvar, one engine per thread (`Goccia.Engine`).
