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
| Interfaces | `I<Name>` prefix | `IGocciaCallable` |
| Private fields | `F` prefix | `FValue`, `FPrototype` |
| Methods | PascalCase | `GetProperty`, `ToStringLiteral` |
| Free functions | PascalCase | `Evaluate`, `EvaluateBinary` |
| Constants | PascalCase or UPPER_CASE | `DefaultGlobals`, `NaN` |
| Parameters | `A` prefix | `AScope`, `AValue`, `AFileName` |
| Enums | `TGoccia<Name>` for type, lowercase prefix for values | `TGocciaScopeKind`, `skGlobal` |

### File Organization

Each unit follows this structure:

```pascal
unit Goccia.Category.Name;

{$I Goccia.inc}

interface

uses
  // Standard library units first
  SysUtils, Classes, Generics.Collections,
  // Project units
  Goccia.Values.Primitives, Goccia.Scope;

type
  // Type declarations (classes, interfaces, records, enums)

// Free function declarations (for evaluator modules)

implementation

uses
  // Implementation-only dependencies (avoids circular references)
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
  TGocciaGC.Instance.PinValue(FSharedPrototype);
  TGocciaGC.Instance.PinValue(FPrototypeMethodHost);
end;
```

The constructor calls `InitializePrototype` and assigns `FPrototype := FSharedPrototype`. All method callbacks must use `ThisValue` (not `Self`) to access instance data:

```pascal
function TMyValue.MyMethod(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TMyValue;
begin
  Inst := TMyValue(ThisValue);  // Cast once at method entry
  // Use Inst.FData, Inst.Items, etc. — NOT Self.FData
end;
```

Methods that return `Self` for chaining (e.g., `Set.add`, `Map.set`) must return `ThisValue` instead.

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

## Editor Configuration

The project uses `.editorconfig` for consistent formatting:

- **Indent:** 2 spaces (no tabs)
- **Line endings:** LF
- **Trailing whitespace:** trimmed
- **Final newline:** inserted
- **Charset:** UTF-8
