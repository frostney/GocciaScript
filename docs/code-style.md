# Code Style

This document describes the coding conventions and patterns used throughout the GocciaScript codebase.

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
{$modeswitch multihelpers}      // Multiple class helpers
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
| Constants | PascalCase or UPPER_CASE | `DefaultGlobals`, `NaN` |
| Parameters | `A` prefix (multi-letter only) | `AScope`, `AValue`, `AFileName` |
| Enums | `TGoccia<Name>` for type, lowercase prefix for values | `TGocciaScopeKind`, `skGlobal` |

### Centralized Constants

Use centralized constant units instead of hardcoded string literals:

- **Keywords** — Use `Goccia.Keywords.Reserved` (`KEYWORD_THIS`, `KEYWORD_SUPER`, etc.) and `Goccia.Keywords.Contextual` (`KEYWORD_GET`, `KEYWORD_SET`, etc.) instead of raw `'this'`, `'get'` strings.
- **File extensions** — Use `Goccia.FileExtensions` constants (`EXT_JS`, `EXT_JSX`, `EXT_TS`, `EXT_TSX`, `EXT_MJS`, `EXT_JSON`) instead of raw `'.js'`, `'.mjs'` strings. Use the `ScriptExtensions` array and `IsScriptExtension`/`IsJSXNativeExtension` helpers instead of duplicating extension lists.

Adding a new keyword or file extension requires a single change in the constants unit — all consumers pick it up automatically.

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

**Format:** `// ESYYYY §X.Y.Z FullMethodName(params)`

- `YYYY` is the current edition year of the ECMA-262 specification (e.g., `ES2026` for 2026, `ES2027` for 2027). Use the year matching the current year.
- The section numbers reference [ECMA-262](https://tc39.es/ecma262/), the living standard.
- Use the full qualified name as it appears in the spec (e.g., `Array.prototype.map`, `Object.keys`, `Number.parseInt`).
- Include the parameter list from the spec signature (e.g., `(callbackfn [, thisArg])`) for quick lookup.
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

**What not to annotate:** Internal GocciaScript helpers that don't correspond to a spec algorithm (e.g., `EvaluateStatementsSafe`, `SpreadIterableInto`, Pascal-specific utilities).

### No Abbreviations

Class names, function names, method names, and type names must use **full words** — do not abbreviate. This keeps the codebase consistent and self-documenting.

```pascal
// Correct
TGocciaGarbageCollector
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

**Why `TObjectList(False)` instead of `TList`?** Even when the collection does not own its elements (e.g., the GC's managed scopes list, which uses manual mark-and-sweep), using `TObjectList<T>.Create(False)` with a named alias keeps the VMT consistent. `TList<T>` and `TObjectList<T>` produce incompatible VMTs, so mixing them across units reintroduces the same cross-unit type check failures.

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

## Platform-Specific Pitfalls

### `Double(Int64)` on FPC 3.2.2 AArch64

On FPC 3.2.2 targeting AArch64 (Apple Silicon), an explicit `Double(Int64Var)` cast performs a **bit reinterpretation** instead of a value conversion. This produces garbage floating-point values (e.g., `Double(Int64(1000))` yields `~4.94e-315` instead of `1000.0`).

**Workaround:** Use implicit promotion via arithmetic instead of explicit casts:

```pascal
// WRONG on AArch64 — bit-casts instead of converting
Result := Double(FEpochMilliseconds) * 1000000.0;

// CORRECT — implicit promotion via multiplication
Result := FEpochMilliseconds * 1000000.0;
```

This affects any code that converts `Int64` fields to `Double` for floating-point arithmetic. The same issue applies to intermediate `Int64` local variables cast to `Double`.

## Design Patterns

### Singleton Pattern (Special Values)

Special values use unit-level initialization with lazy or eager singleton creation:

```pascal
var
  _UndefinedValue: TGocciaUndefinedLiteralValue;

function UndefinedValue: TGocciaValue;
begin
  Result := _UndefinedValue;
end;

initialization
  _UndefinedValue := TGocciaUndefinedLiteralValue.Create;
```

### Factory Method (Scope Creation)

Scopes are created via `CreateChild`, never directly instantiated:

```pascal
// Correct
ChildScope := ParentScope.CreateChild(skBlock);

// Correct — with capacity hint for function call scopes
CallScope := FClosure.CreateChild(skFunction, ParamCount + 4);

// WRONG — never do this
ChildScope := TGocciaScope.Create(ParentScope, skBlock);
```

### Context Object (Evaluation)

The evaluator threads state through a `TGocciaEvaluationContext` record rather than using instance variables or globals:

```pascal
TGocciaEvaluationContext = record
  Scope: TGocciaScope;
  OnError: TGocciaThrowErrorCallback;
  LoadModule: TLoadModuleCallback;
end;
```

This keeps evaluator functions pure — all dependencies are explicit parameters. The `OnError` callback is also stored on `TGocciaScope` and propagated to child scopes, so closures always have access to the error handler without global mutable state.

### Builder Pattern (Built-in Registration)

Built-in objects are constructed by creating a `TGocciaObjectValue` and adding methods one at a time:

```pascal
MathObj := TGocciaObjectValue.Create;
MathObj.DefineProperty('PI', TGocciaNumberLiteralValue.Create(Pi), False);
MathObj.DefineProperty('floor', TGocciaNativeFunction.Create(@MathFloor), False);
```

### Visitor-like Dispatch (Evaluator)

The evaluator dispatches on AST node type using `is` checks:

```pascal
if Node is TGocciaBinaryExpression then
  Result := EvaluateBinary(TGocciaBinaryExpression(Node), Context)
else if Node is TGocciaCallExpression then
  Result := EvaluateCall(TGocciaCallExpression(Node), Context)
// ...
```

### Parser Combinator (Binary Expressions)

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

### Shared Prototype Singleton Pattern

Types that provide prototype methods (String, Array, Set, Map, Function) use a shared class-level singleton instead of creating a per-instance prototype:

```pascal
class var FSharedPrototype: TGocciaObjectValue;
class var FPrototypeMethodHost: TMyValue;

procedure TMyValue.InitializePrototype;
begin
  if Assigned(FSharedPrototype) then Exit;  // Guard: create once
  FSharedPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;
  FSharedPrototype.RegisterNativeMethod(...);
  TGocciaGarbageCollector.Instance.PinValue(FSharedPrototype);
  TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
end;
```

The constructor calls `InitializePrototype` and assigns `FPrototype := FSharedPrototype`. All method callbacks must use `ThisValue` (not `Self`) to access instance data:

```pascal
function TMyValue.MyMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TMyValue;
begin
  Inst := TMyValue(AThisValue);  // Cast once at method entry
  // Use Inst.FData, Inst.Items, etc. — NOT Self.FData
end;
```

Methods that return `Self` for chaining (e.g., `Set.add`, `Map.set`) must return `AThisValue` instead.

### Evaluator Helper Patterns

**`EvaluateStatementsSafe`** — Wraps statement list execution with standardized exception handling (re-raises GocciaScript signals, wraps unexpected exceptions). Used wherever a list of AST nodes is evaluated in sequence.

**`SpreadIterableInto` / `SpreadIterableIntoArgs`** — Unified spread expansion for arrays, strings, sets, and maps. Used by `EvaluateCall`, `EvaluateArray`, and `EvaluateObject`.

**`EvaluateSimpleNumericBinaryOp`** — Shared helper for subtraction, multiplication, and exponentiation, which all share the same pattern of numeric coercion, NaN propagation, and a single-operation callback.

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

## Code Organization Principles

1. **Explicitness** — Modules, classes, methods, and properties use explicit, descriptive names even at the cost of verbosity. Shortcuts and abbreviations are avoided.

2. **OOP over everything** — Rely on type safety of specialized classes. Each concept gets its own class rather than using generic data structures.

3. **Separation of concerns** — Each unit has a single, clear responsibility. The evaluator doesn't know about built-ins; the engine doesn't know about AST structure.

4. **Minimal public API** — Units expose only what's needed. Implementation details stay in the `implementation` section.

5. **No global mutable state** — State flows through parameters (evaluation context, scope) rather than global variables. The only globals are immutable singletons.

## Auto-Formatting

The project includes `./format.pas`, an instantfpc script that formats Pascal source files. It runs automatically as a pre-commit hook via [Lefthook](https://github.com/evilmartians/lefthook) and can also be invoked manually — no build step needed.

### Setup

Install [Lefthook](https://github.com/evilmartians/lefthook) using your platform's package manager, then register the git hooks:

```bash
# macOS
brew install lefthook

# Linux (Snap)
sudo snap install lefthook

# Linux (APT — Debian/Ubuntu)
# See https://github.com/evilmartians/lefthook/blob/master/docs/install.md
curl -1sLf 'https://dl.cloudsmith.io/public/evilmartians/lefthook/setup.deb.sh' | sudo -E bash
sudo apt install lefthook

# Windows (Scoop)
scoop install lefthook

# Windows (Chocolatey)
choco install lefthook

# Any platform with Go installed
go install github.com/evilmartians/lefthook@latest

# Any platform with npm
npm install -g lefthook
```

After installation, register the hooks in the repository:

```bash
lefthook install
```

### Manual Usage

```bash
# Format all project Pascal files
./format.pas

# Format specific files
./format.pas units/Goccia.Engine.pas

# Check only (exit 1 if changes needed)
./format.pas --check
```

### What It Enforces

All of the following are **auto-fixed** (not just warned about):

- **Uses clauses**: one unit per line, grouped (System > Third-party > Project > Relative), alphabetically sorted within each group, blank line between groups. Units with an `in` path are always in the Relative group; `Goccia.*` units are Project; known FPC standard library units are System; everything else is Third-party.
- **Function naming**: capitalizes the first letter of function, procedure, constructor, and destructor names to enforce PascalCase. Renames all references within the same file. External C bindings are excluded.
- **Parameter naming**: adds the `A` prefix to multi-letter parameters (e.g., `Value` → `AValue`) and renames all references within the function scope (declaration, local variables, and body). Single-letter parameters and Pascal keyword conflicts are skipped.
- **Stray spaces**: removes spurious spaces before `;`, `)`, and `,` (e.g., `string ;` → `string;`). String literals and comments are left untouched.

## Editor Configuration

### `.editorconfig`

The project uses `.editorconfig` for consistent formatting:

- **Indent:** 2 spaces (no tabs)
- **Line endings:** LF
- **Trailing whitespace:** trimmed
- **Final newline:** inserted
- **Charset:** UTF-8

### VSCode / Cursor Setup

The repository includes `.vscode/settings.json` and `.vscode/extensions.json` for a zero-config experience in VSCode and Cursor.

#### Recommended Extensions

Open the project and accept the "Install Recommended Extensions" prompt, or install them manually:

| Extension | ID | Purpose |
|-----------|----|---------|
| Pascal | `alefragnani.pascal` | Syntax highlighting, code navigation, and symbol search for Pascal/Delphi |
| Run on Save | `emeraldwalk.RunOnSave` | Triggers `./format.pas` automatically when a `.pas` or `.dpr` file is saved |
| EditorConfig | `editorconfig.editorconfig` | Applies `.editorconfig` rules (indent size, line endings, etc.) |

These are declared in `.vscode/extensions.json` so VSCode/Cursor will prompt to install them on first open.

#### Format on Save

`.vscode/settings.json` configures the [Run on Save](https://marketplace.visualstudio.com/items?itemName=emeraldwalk.RunOnSave) extension to run `./format.pas` on every `.pas` and `.dpr` file when saved. This keeps code style consistent without manual intervention — the formatter fixes uses clause ordering, PascalCase naming, parameter prefixes, and stray spaces in the background.

The `runOnSave` command runs silently (`"runIn": "backend"`), so it will not open a terminal or interrupt your workflow. The file is re-read by the editor after formatting, so changes appear immediately.

> **Note:** This requires `instantfpc` (ships with FreePascal) to be on your `PATH`. If you installed FreePascal via the methods in [Getting Started](../README.md#getting-started), this is already the case.

#### How the Layers Work Together

| Layer | When it runs | What it does |
|-------|-------------|--------------|
| `.editorconfig` | While typing | Sets indent size, line endings, trailing whitespace, charset |
| `runOnSave` | On file save | Runs `./format.pas` to auto-fix code conventions |
| Lefthook pre-commit | On `git commit` | Runs `./format.pas` on staged files as a safety net |
| CI `--check` | On push / PR | Fails the build if any file needs formatting |

All four layers enforce the same rules, providing defence in depth. The typical developer experience is: EditorConfig handles whitespace while you type, format-on-save fixes everything else when you save, and the pre-commit hook and CI catch anything that slips through.
