# AGENTS.md

Instructions for AI coding assistants working on GocciaScript. For deep-dive documentation, see the [docs/](docs/) folder.

## Project Overview

GocciaScript is a subset of ECMAScript 2020 implemented in FreePascal. It provides a JavaScript-like scripting language with intentional limitations for security and simplicity. See [docs/language-restrictions.md](docs/language-restrictions.md) for the full rationale.

## Quick Reference

### Build Commands

```bash
./build.pas                  # Build everything
./build.pas loader           # Build ScriptLoader only
./build.pas repl             # Build REPL only
./build.pas testrunner       # Build TestRunner only
./build.pas tests            # Build Pascal unit tests only
```

### Run Commands

```bash
./build/ScriptLoader example.js                  # Execute a script
./build/REPL                                      # Start interactive REPL
./build/TestRunner tests/                         # Run all JavaScript tests
./build/TestRunner tests/language/expressions/    # Run a test category
```

### Compile and Run (Common Workflows)

```bash
# Compile and run a script
./build.pas loader && ./build/ScriptLoader ./example.js

# Compile and run all tests
./build.pas testrunner && ./build/TestRunner tests

# Compile and run a specific test
./build.pas testrunner && ./build/TestRunner tests/language/expressions/addition/basic-addition.js
```

### Direct FPC Compilation

```bash
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
```

## Architecture

See [docs/architecture.md](docs/architecture.md) for the full architecture deep-dive.

**Pipeline:** Source → Lexer → Parser → Interpreter → Evaluator → Result

**Key components:**

| Component | File | Role |
|-----------|------|------|
| Engine | `Goccia.Engine.pas` | Top-level orchestration, built-in registration |
| Lexer | `Goccia.Lexer.pas` | Source → tokens |
| Parser | `Goccia.Parser.pas` | Tokens → AST |
| Interpreter | `Goccia.Interpreter.pas` | AST execution, module loading, scope ownership |
| Evaluator | `Goccia.Evaluator.pas` | Pure AST evaluation (+ sub-modules) |
| Scope | `Goccia.Scope.pas` | Lexical scoping, variable bindings, TDZ |

## Critical Rules

These rules **must** be followed when modifying the codebase:

### 1. Evaluator Purity

Functions in `Goccia.Evaluator.pas` (and its sub-modules) are **pure functions**. They cannot have side effects and must return the same output for the same input. State changes happen through the scope and value objects passed via `TGocciaEvaluationContext`, never through evaluator-internal state.

### 2. Scope Creation

`TGocciaScope` must **never** be instantiated directly. Always use the `CreateChild` factory method on an existing scope:

```pascal
// Correct
ChildScope := ParentScope.CreateChild(skBlock);

// WRONG — do not do this
ChildScope := TGocciaScope.Create(ParentScope, skBlock);
```

### 3. Testing Requirements

JavaScript end-to-end tests are the **primary** way of testing GocciaScript. When implementing a new feature or fixing a bug:
- Add JavaScript tests under the `tests/` directory.
- Keep tests isolated and grouped by feature/filename.
- Follow the existing directory structure (`tests/language/` for language features, `tests/built-ins/` for built-in objects).
- Each test file should focus on a single concern.
- Always verify changes by running: `./build.pas testrunner && ./build/TestRunner tests`

### 4. Language Restrictions

GocciaScript intentionally excludes these JavaScript features — do **not** add support for them:
- `var` declarations (use `let`/`const`)
- `function` keyword (use arrow functions)
- `==` and `!=` loose equality (use `===`/`!==`)
- `eval()` and `arguments` object
- Automatic semicolon insertion (semicolons are required)
- `with` statement

See [docs/language-restrictions.md](docs/language-restrictions.md) for the full list and rationale.

## Code Style

See [docs/code-style.md](docs/code-style.md) for the complete style guide.

### Key Conventions

- **Unit naming:** `Goccia.<Category>.<Name>.pas` (dot-separated hierarchy)
- **Class naming:** `TGoccia<Name>` prefix
- **Interface naming:** `I<Name>` prefix
- **Private fields:** `F` prefix
- **Parameters:** `A` prefix
- **Indentation:** 2 spaces (see `.editorconfig`)
- **Compiler directives:** All units include `{$I Goccia.inc}`

### Terminology

- **Define** = create a new variable binding (`DefineLexicalBinding`)
- **Assign** = re-assign an existing binding (`AssignLexicalBinding`)
- These are distinct operations — do not conflate them.

### Design Patterns in Use

- **Singleton** for special values (`undefined`, `null`, `true`, `false`, `NaN`, `Infinity`)
- **Factory method** for scope creation (`CreateChild`)
- **Context object** for evaluation state (`TGocciaEvaluationContext`)
- **Interface segregation** for value capabilities (`IPropertyMethods`, `IIndexMethods`, `IFunctionMethods`)
- **Chain of responsibility** for scope lookup
- **Recursive descent** for parsing

## Value System

See [docs/value-system.md](docs/value-system.md) for the complete value system documentation.

All values inherit from `TGocciaValue`. Capabilities are expressed through interfaces:
- `IPropertyMethods` — object-like property access
- `IIndexMethods` — array-like indexed access
- `IFunctionMethods` — callable values
- `IValueOf` — primitive value extraction

## Built-in Objects

See [docs/built-ins.md](docs/built-ins.md) for documentation on all built-ins and how to add new ones.

Built-ins are registered by the engine via `TGocciaGlobalBuiltins` flags:

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
                  ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap];
```

The TestRunner adds `ggTestAssertions` for the test framework (`describe`, `test`, `expect`).

## Testing

See [docs/testing.md](docs/testing.md) for the complete testing guide.

- **Primary:** JavaScript end-to-end tests in `tests/` directory — these are the source of truth for correctness
- **Secondary:** Pascal unit tests in `units/*.Test.pas` — only for internal implementation details
- Test framework: built-in `describe`/`test`/`expect` (enabled via `ggTestAssertions`)

## Build System

See [docs/build-system.md](docs/build-system.md) for build system details.

- Build script: `./build.pas` (FreePascal script via `instantfpc`)
- Compiler config: `config.cfg`
- Shared directives: `units/Goccia.inc`
- Output directory: `build/`
- CI: GitHub Actions on Linux, macOS, Windows (x64 + ARM)

## Documentation Index

| Document | Description |
|----------|-------------|
| [docs/architecture.md](docs/architecture.md) | Pipeline overview, component responsibilities, data flow |
| [docs/design-decisions.md](docs/design-decisions.md) | Rationale behind key technical choices |
| [docs/code-style.md](docs/code-style.md) | Naming conventions, patterns, file organization |
| [docs/value-system.md](docs/value-system.md) | Type hierarchy, interfaces, primitives, objects |
| [docs/built-ins.md](docs/built-ins.md) | Available built-ins, registration system, adding new ones |
| [docs/testing.md](docs/testing.md) | Test organization, writing tests, running tests |
| [docs/build-system.md](docs/build-system.md) | Build commands, configuration, CI/CD |
| [docs/language-restrictions.md](docs/language-restrictions.md) | Supported/excluded features with rationale |
