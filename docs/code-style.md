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
| Interfaces | `I<Name>` prefix | `IPropertyMethods` |
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
  Goccia.Values.Interfaces, Goccia.Scope;

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

// WRONG — never do this
ChildScope := TGocciaScope.Create(ParentScope, skBlock);
```

### Context Object (Evaluation)

The evaluator threads state through a `TGocciaEvaluationContext` record rather than using instance variables or globals:

```pascal
TGocciaEvaluationContext = record
  Scope: TGocciaScope;
  OnError: TThrowErrorCallback;
  ModuleLoader: TGocciaModuleLoader;
end;
```

This keeps evaluator functions pure — all dependencies are explicit parameters.

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

## Terminology

The codebase uses specific terminology consistently:

| Term | Meaning |
|------|---------|
| **Define** | Create a new variable binding in the current scope (`DefineLexicalBinding`) |
| **Assign** | Re-assign the value of an existing binding (`AssignLexicalBinding`) |
| **Binding** | A name-to-value association in a scope (not a raw variable) |
| **Literal** | A value that appears directly in source code |
| **Native function** | A built-in function implemented in Pascal |
| **User function** | A function defined in GocciaScript (arrow function) |

### Define vs Assign

This distinction is critical in the codebase:

- `DefineLexicalBinding` — Creates a **new** variable in the current scope. Used for `let`/`const` declarations and function parameters.
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
