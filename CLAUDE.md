# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
./build/TestRunner tests/

# Run a specific test file
./build/TestRunner tests/language/expressions/addition/basic-addition.js

# Run Node.js tests (if package.json is configured)
npm test
```

### Running Scripts
```bash
# Execute a GocciaScript file
./build/ScriptLoader example.js

# Start the REPL
./build/REPL
```

## Architecture

### Core Components

1. **Lexer** (`Goccia.Lexer.pas`) - Tokenizes source code
2. **Parser** (`Goccia.Parser.pas`) - Builds AST from tokens  
3. **Evaluator** (`Goccia.Evaluator.pas`) - Evaluates AST nodes
4. **Interpreter** (`Goccia.Interpreter.pas`) - Core interpreter that executes AST
5. **Engine** (`Goccia.Engine.pas`) - High-level engine that manages globals and built-ins

### Value System

All values inherit from base interfaces in `Goccia.Values.Interfaces.pas`:
- Primitives: `TGocciaNumberLiteralValue`, `TGocciaStringLiteralValue`, `TGocciaBooleanLiteralValue`, `TGocciaNullLiteralValue`, `TGocciaUndefinedValue`
- Objects: `TGocciaObjectValue`, `TGocciaArrayValue`, `TGocciaFunctionValue`, `TGocciaClassValue`
- Special: `TGocciaError` for runtime errors

### Built-in Objects

Located in `units/Goccia.Builtins.*`:
- `Console` - Console I/O operations
- `Math` - Mathematical functions and constants
- `JSON` - JSON parsing and stringification
- `GlobalObject` - Object constructor and methods
- `GlobalArray` - Array constructor and methods
- `GlobalNumber` - Number constructor and methods
- `TestAssertions` - Testing framework (enabled with `ggTestAssertions` flag)

### Global Built-ins Configuration

The engine manages global built-ins through `TGocciaGlobalBuiltin` flags:
- Default globals: `TGocciaEngine.DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON]`
- Test runner adds: `ggTestAssertions`

To run scripts with custom globals:
```pascal
TGocciaEngine.RunScriptFromStringList(Source, FileName, CustomGlobals);
```

## Language Restrictions

GocciaScript intentionally excludes:
- `var` declarations (use `let` or `const`)
- `function` keyword (use arrow functions)
- Traditional loops (use array methods)
- `==` and `!=` (use `===` and `!==`)
- `eval()` and `arguments` object
- Automatic semicolon insertion (semicolons required)

## Testing Strategy

Tests are organized in `tests/` directory:
- `language/` - Core language features
- `built-ins/` - Built-in object tests

Each test file uses the built-in test framework with `describe()` and `test()` functions when `ggTestAssertions` is enabled.

## Development Notes

- All Pascal units use `{$I Goccia.inc}` for shared compiler directives
- The `config.cfg` file contains FreePascal compiler options
- Build output goes to `build/` directory
- Use `TGocciaEngine` class methods to execute scripts:
  - `RunScript` - Execute a string of code
  - `RunScriptFromFile` - Execute a file
  - `RunScriptFromStringList` - Execute from a TStringList