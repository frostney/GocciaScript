# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. For comprehensive documentation, see [AGENTS.md](AGENTS.md) and the [docs/](docs/) folder.

## Project Overview

GocciaScript is a subset of ECMAScript 2020 implemented in FreePascal. It provides a JavaScript-like scripting language with intentional limitations for security and simplicity.

## Build Commands

### Building the Project
```bash
# Build all components (default)
./build.pas

# Build specific components
./build.pas repl        # Build REPL only
./build.pas loader      # Build ScriptLoader only
./build.pas testrunner  # Build TestRunner only
./build.pas tests       # Build unit tests only

# Or use FreePascal directly
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- REPL.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- ScriptLoader.dpr
fpc @config.cfg -vw-n-h-i-l-d-u-t-p-c-x- TestRunner.dpr
```

### Running Tests
```bash
# Run all JavaScript tests
./build.pas testrunner && ./build/TestRunner tests

# Run a specific test file
./build.pas testrunner && ./build/TestRunner tests/language/expressions/addition/basic-addition.js
```

### Running Scripts
```bash
# Execute a GocciaScript file
./build.pas loader && ./build/ScriptLoader example.js

# Start the REPL
./build.pas repl && ./build/REPL
```

## Critical Rules

1. **Evaluator purity** — Functions in `Goccia.Evaluator.pas` are pure functions. No side effects. Same input → same output.
2. **Scope creation** — Never instantiate `TGocciaScope` directly. Use `CreateChild` on an existing scope.
3. **Testing** — New features require tests in `tests/`, isolated and grouped by filename.
4. **Language restrictions** — Do not add `var`, `function` keyword, `==`/`!=`, `eval`, ASI, or traditional loops (`for`, `while`, `do-while`). Iteration is done through array methods.

## Architecture

See [docs/architecture.md](docs/architecture.md) for details.

**Pipeline:** Source → Lexer → Parser → Interpreter → Evaluator → Result

### Core Components

1. **Lexer** (`Goccia.Lexer.pas`) — Tokenizes source code
2. **Parser** (`Goccia.Parser.pas`) — Builds AST from tokens
3. **Evaluator** (`Goccia.Evaluator.pas`) — Evaluates AST nodes (pure functions)
4. **Interpreter** (`Goccia.Interpreter.pas`) — Core interpreter that executes AST
5. **Engine** (`Goccia.Engine.pas`) — High-level engine that manages globals and built-ins

### Value System

See [docs/value-system.md](docs/value-system.md) for details.

All values inherit from `TGocciaValue`. Capabilities via interfaces: `IPropertyMethods`, `IIndexMethods`, `IFunctionMethods`, `IValueOf`, `IStringTag`.

### Built-in Objects

See [docs/built-ins.md](docs/built-ins.md) for details.

Located in `units/Goccia.Builtins.*`. Includes `Console`, `Math`, `JSON`, `Object` (with `freeze`, `fromEntries`, `getPrototypeOf`), `Array` (with `from`, `of`, `sort`, `splice`, `shift`, `unshift`), `Number` (with constants like `MAX_SAFE_INTEGER`, `EPSILON`), `String`, `Symbol`, `Set`, `Map`, and error constructors. Controlled by `TGocciaGlobalBuiltins` flags.

Default globals: `[ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap]`

### Global Built-ins Configuration

```pascal
TGocciaEngine.RunScriptFromStringList(Source, FileName, CustomGlobals);
```

## Documentation Index

| Document | Description |
|----------|-------------|
| [AGENTS.md](AGENTS.md) | Consolidated AI assistant instructions |
| [docs/architecture.md](docs/architecture.md) | Pipeline, components, data flow |
| [docs/design-decisions.md](docs/design-decisions.md) | Rationale behind technical choices |
| [docs/code-style.md](docs/code-style.md) | Naming conventions, patterns |
| [docs/value-system.md](docs/value-system.md) | Type hierarchy, interfaces |
| [docs/built-ins.md](docs/built-ins.md) | Built-in objects API reference |
| [docs/testing.md](docs/testing.md) | Test organization and writing tests |
| [docs/build-system.md](docs/build-system.md) | Build commands, configuration, CI |
| [docs/language-restrictions.md](docs/language-restrictions.md) | Supported/excluded features |
