# Embedding the Engine

*For FreePascal developers who want to embed the GocciaScript engine in their own applications.*

## Executive Summary

- **Quick start** — `TGocciaEngine.RunScript('code')` for one-shot execution; `TGocciaEngine.Create(...)` for long-lived engines
- **Sandboxing** — Control available built-ins via `TGocciaGlobalBuiltins` flags; inject custom globals via `DefineLexicalBinding`; enforce execution limits via timeout or instruction cap
- **Module resolution** — Pluggable resolver with extensionless imports, import maps, custom content providers, and global modules
- **Transparent GC** — Mark-and-sweep GC initializes automatically; FPU exceptions are masked for IEEE 754 semantics

GocciaScript is designed to be embedded in FreePascal applications. The `TGocciaEngine` class provides the public API for executing scripts, controlling available built-ins, and injecting custom globals.

## Quick Start

The simplest way to run a script:

```pascal
uses
  Goccia.Engine;

TGocciaEngine.RunScript('console.log("hello from GocciaScript");');
```

To execute a file:

```pascal
TGocciaEngine.RunScriptFromFile('app.js');
```

Both methods create an engine with the default built-in set, execute the code, and clean up automatically.

## Engine API

### Class Methods (One-Shot Execution)

These are convenience methods that create an engine, execute, and clean up in a single call. Use these for fire-and-forget script execution.

| Method | Description |
|--------|-------------|
| `RunScript(Source)` | Execute a string of code with default globals |
| `RunScript(Source, FileName, Globals)` | Execute a string with custom globals and filename |
| `RunScriptFromFile(FileName)` | Execute a file with default globals |
| `RunScriptFromFile(FileName, Globals)` | Execute a file with custom globals |
| `RunScriptFromStringList(Source, FileName)` | Execute from a `TStringList` with default globals |
| `RunScriptFromStringList(Source, FileName, Globals)` | Execute from a `TStringList` with custom globals |
All methods return `TGocciaScriptResult` — a record containing the result value, per-phase timing (in microseconds), and the filename.

### Instance Usage (Long-Lived Engine)

For interactive sessions (REPL, editor integration) or when you need to execute multiple scripts in the same scope, create an engine instance directly:

```pascal
uses
  Classes,

  Goccia.Engine,
  Goccia.Values.Primitives;

var
  Engine: TGocciaEngine;
  Source: TStringList;
  Result: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('session', Source, []);
  try
    // First execution — defines a variable
    Source.Text := 'const x = 42;';
    Engine.Execute;

    // Second execution — uses the variable from the first
    Source.Text := 'console.log(x);'; // prints 42
    Engine.Execute;
  finally
    Engine.Free;
    Source.Free;
  end;
end;
```

The `TStringList` is passed by reference — update its contents and call `Execute` again to run new code in the same global scope. Variables, functions, and classes defined in previous executions remain available.

### Automatic Semicolon Insertion

ASI is disabled by default. To enable ECMAScript-compliant automatic semicolon insertion (ES2026 §12.10), set the `ASIEnabled` property after creating the engine:

```pascal
Engine := TGocciaEngine.Create('app.js', Source, []);
Engine.ASIEnabled := True;  // Semicolons are now optional per ES2026 rules
Engine.Execute;
```

When enabled, the parser inserts virtual semicolons at newline boundaries, before `}`, and at EOF. Restricted productions (`return`, `throw`, `break`) follow the `[no LineTerminator here]` rules.

### Timing

All `RunScript*` methods and the `Execute` method return a `TGocciaScriptResult` record that includes nanosecond-precision timing for each pipeline phase:

```pascal
uses
  TimingUtils,

  Goccia.Engine;

var
  ScriptResult: TGocciaScriptResult;
begin
  ScriptResult := TGocciaEngine.RunScript(Source, 'bench.js', []);
  WriteLn('Lex: ', FormatDuration(ScriptResult.LexTimeNanoseconds));
  WriteLn('Parse: ', FormatDuration(ScriptResult.ParseTimeNanoseconds));
  WriteLn('Execute: ', FormatDuration(ScriptResult.ExecuteTimeNanoseconds));
  WriteLn('Total: ', FormatDuration(ScriptResult.TotalTimeNanoseconds));
end;
```

`FormatDuration` (from `TimingUtils`) automatically selects the appropriate unit: `ns` for values below 0.5μs, `μs` for values below 0.5ms, `ms` with two decimal places for values up to 10s, and `s` above that.

`TimingUtils` provides three clock functions: `GetNanoseconds` and `GetMilliseconds` for monotonic duration timing (`clock_gettime(CLOCK_MONOTONIC)` on Unix/macOS, `QueryPerformanceCounter` on Windows), and `GetEpochNanoseconds` for wall-clock epoch time (`clock_gettime(CLOCK_REALTIME)` on Unix/macOS, `GetSystemTimeAsFileTime` on Windows).

## Module Resolution

The engine uses a pluggable module resolver (`TGocciaModuleResolver`) that supports extensionless imports, import-map-style aliases, and index file resolution.

### Extension-Free Imports

Import paths can omit file extensions. The resolver tries extensions in order: `.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`, `.json`, `.json5`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.txt`, `.md`:

```javascript
// These all resolve through the shared module extension list:
import { add } from "./math-utils.js";  // explicit extension
import { add } from "./math-utils";     // extension resolved automatically
import { content } from "./notes";      // resolves to ./notes.txt or ./notes.md
```

Directory imports resolve to `index` files:

```javascript
import { setup } from "./utils";  // resolves to ./utils/index.js (or .ts, .jsx, etc.)
```

### Path Aliases

Aliases follow WHATWG import map matching rules:

```pascal
Engine.AddAlias('lodash', 'vendor/lodash/index.js'); // exact match only
Engine.AddAlias('@/', 'src/');                       // prefix match
Engine.AddAlias('@/components/', 'ui/lib/');         // more specific prefix
```

The alias target is resolved relative to the entry script's directory.

**Exact vs prefix matching:** A key without a trailing `/` is an exact match only. A key with a trailing `/` is a prefix match and appends the unmatched suffix to the target. This means `lodash` matches `import "lodash"` but not `import "lodash/fp"`, while `@/` matches `@/utils/math`.

**Longest-prefix matching:** When multiple prefix aliases overlap (e.g., `@/` and `@/components/`), the resolver always picks the longest matching key. This means `@/components/Button` uses the `@/components/` alias, not `@/`.

Scripts can then import using the alias:

```javascript
import { formatDate } from "@/utils/dates";
```

`TGocciaModuleResolver` also exposes `LoadImportMap(path)` and `DiscoverProjectConfig(startDirectory)` helpers for browser-style import map JSON and `goccia.json` project configuration files. The shared CLI frontends (`GocciaScriptLoader`, `GocciaTestRunner`, `GocciaBenchmarkRunner`, and `GocciaREPL`) all use the same `Goccia.Modules.Configuration.ConfigureModuleResolver(...)` helper on top of this resolver surface.

### Custom Resolver

For advanced resolution logic (e.g., `node_modules` lookup, URL imports, or in-memory modules), subclass `TGocciaModuleResolver` and override the `Resolve` method:

```pascal
uses
  Goccia.Modules.Resolver;

type
  TMyResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath, AImportingFilePath: string): string; override;
  end;

function TMyResolver.Resolve(const AModulePath, AImportingFilePath: string): string;
begin
  // Custom logic here — fall back to inherited for standard resolution
  Result := inherited Resolve(AModulePath, AImportingFilePath);
end;

var
  Resolver: TMyResolver;
  Engine: TGocciaEngine;
begin
  Resolver := TMyResolver.Create('/path/to/project');
  Engine := TGocciaEngine.Create('app.js', Source, [], Resolver);
  try
    Engine.Execute;
  finally
    Engine.Free;
    Resolver.Free;  // caller owns the resolver when injected
  end;
end;
```

When no custom resolver is provided, the engine creates a default `TGocciaModuleResolver` whose base directory is the entry script's directory.

### Custom Content Provider

Resolution and content loading are separate concerns. A custom resolver decides **which** module path to load; a custom content provider decides **how** the module text or JSON is retrieved once that path is resolved.

Use `TGocciaModuleContentProvider` when your modules come from memory, an archive, a database, or some other non-filesystem source:

```pascal
uses
  Classes,
  SysUtils,

  Goccia.Engine,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver;

type
  TMemoryResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath, AImportingFilePath: string): string; override;
  end;

  TMemoryContentProvider = class(TGocciaModuleContentProvider)
  public
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

function TMemoryResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
begin
  Result := AModulePath;
end;

function TMemoryContentProvider.Exists(const APath: string): Boolean;
begin
  Result := APath = 'memory:/dep.js';
end;

function TMemoryContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
begin
  if APath = 'memory:/dep.js' then
    Exit(TGocciaModuleContent.Create('export const value = 42;', 0));

  raise Exception.Create('Module content not found: ' + APath);
end;

function TMemoryContentProvider.TryGetLastModified(const APath: string;
  out ALastModified: TDateTime): Boolean;
begin
  ALastModified := 0;
  Result := False;
end;

var
  Engine: TGocciaEngine;
  ModuleLoader: TGocciaModuleLoader;
  Resolver: TMemoryResolver;
  Provider: TMemoryContentProvider;
begin
  Resolver := TMemoryResolver.Create;
  Provider := TMemoryContentProvider.Create;
  try
    ModuleLoader := TGocciaModuleLoader.Create('memory:/app.js', Resolver,
      Provider);
    try
      Engine := TGocciaEngine.Create('memory:/app.js', Source,
        [], ModuleLoader);
      try
        Engine.Execute;
      finally
        Engine.Free;
      end;
    finally
      ModuleLoader.Free;  // caller owns injected module loaders
    end;
  finally
    Provider.Free;  // caller owns injected providers
    Resolver.Free;
  end;
end;
```

`TGocciaEngine` also accepts an injected module loader via its constructor. When no loader is supplied, it creates a default `TGocciaModuleLoader`, which in turn uses a `TGocciaFileSystemModuleContentProvider` and the standard filesystem-backed resolver.

### Global Modules

Global modules are bare-specifier imports that bypass file resolution entirely. They are checked before the resolver runs:

```pascal
uses
  Goccia.Modules;

var
  Module: TGocciaModule;
begin
  Module := TGocciaModule.Create('my-lib');
  Module.ExportsTable.Add('version', TGocciaStringLiteralValue.Create('1.0.0'));
  Engine.RegisterGlobalModule('my-lib', Module);
end;
```

Scripts can then import from the global module by name:

```javascript
import { version } from "my-lib";
console.log(version);  // "1.0.0"
```

This provides the foundation for future built-in module packages that can be coupled to `TGocciaGlobalBuiltins` flags.

## Console Output Capture

By default, `console.log` and friends write directly to stdout. The `OutputCallback` property lets you intercept all console output programmatically:

```pascal
uses
  Classes,

  Goccia.Builtins.Console,
  Goccia.Engine;

type
  TMyLogger = class
    procedure OnConsoleOutput(const AMethod, ALine: string);
  end;

procedure TMyLogger.OnConsoleOutput(const AMethod, ALine: string);
begin
  // AMethod is 'log', 'warn', 'error', 'info', 'debug', 'dir',
  //   'assert', 'count', 'timeEnd', 'timeLog', 'trace', 'table', 'group'
  // ALine is the fully formatted output string
  WriteLn('[', AMethod, '] ', ALine);
end;

var
  Engine: TGocciaEngine;
  Source: TStringList;
  Logger: TMyLogger;
begin
  Logger := TMyLogger.Create;
  Source := TStringList.Create;
  Source.Text := 'console.log("hello"); console.warn("careful");';
  Engine := TGocciaEngine.Create('app.js', Source, []);
  try
    Engine.BuiltinConsole.OutputCallback := Logger.OnConsoleOutput;
    Engine.Execute;
    // Output:
    //   [log] hello
    //   [warn] Warning: careful
  finally
    Engine.Free;
    Source.Free;
    Logger.Free;
  end;
end;
```

The callback type is:

```pascal
TGocciaConsoleOutputCallback = procedure(const AMethod, ALine: string) of object;
```

When `OutputCallback` is assigned, it takes priority over both `OutputLines` (the `TStrings` capture property) and the default `WriteLn` path. When not assigned, existing behavior is unchanged.

### LogCallback (Independent Logging Channel)

`LogCallback` fires on every console call regardless of the `Enabled` flag, independent of the primary output path. This makes it safe for worker threads where `Enabled` is `False` to suppress stdout:

```pascal
Engine.BuiltinConsole.LogCallback := MyHandler.OnLog;
Engine.BuiltinConsole.Enabled := False;  // no stdout, but LogCallback still fires
```

The CLI tools use `LogCallback` internally for `--log=<file>`, which captures console output to a log file in `[method] line` format. The TestRunner silences workers via `Enabled := False` (not by replacing JS methods), so `LogCallback` fires on every console call even in parallel mode. File writes are serialized with a critical section so `--log` is thread-safe even with `--jobs=N`.

## Built-in Registration

The standard built-ins (Console, Math, Object, Array, etc.) are documented in [Built-ins — Registration System](built-ins.md#registration-system). They are always registered unconditionally and are not flag-gated.

### Special-Purpose Flags

The `TGocciaGlobalBuiltins` set controls three special-purpose built-ins that are off by default:

| Flag | Provides | Notes |
|------|----------|-------|
| `ggTestAssertions` | `describe`, `test`, `expect` | Testing framework (GocciaTestRunner adds this) |
| `ggBenchmark` | `suite`, `bench` | Benchmark framework (GocciaBenchmarkRunner adds this) |
| `ggFFI` | `FFI.open`, `FFILibrary`, `FFIPointer` | Foreign Function Interface for native shared libraries |

When embedding, pass `ggFFI` in the `TGocciaGlobalBuiltins` set during engine creation to enable the FFI global. CLI tools (ScriptLoader, REPL, TestRunner, BenchmarkRunner, Bundler) expose this as the `--unsafe-ffi` flag.

To add the test framework for a custom test runner:

```pascal
TGocciaEngine.RunScriptFromFile('tests/my-test.js', [ggTestAssertions]);
```

### Preprocessors and Compatibility

| System | Type | Default | Purpose |
|--------|------|---------|---------|
| `Preprocessors` | `TGocciaPreprocessors` | `[ppJSX]` | Source transformations before parsing |
| `Compatibility` | `TGocciaCompatibilityFlags` | `[]` | Parser behavior toggles |
| `StrictTypes` | `Boolean` | `False` (interpreter), `True` (bytecode) | Type enforcement for annotated variables |

```pascal
Engine := TGocciaEngine.Create('app.js', Source, []);
Engine.Preprocessors := [];              // Disable JSX
Engine.ASIEnabled := True;               // Enable ASI (convenience for cfASI)
Engine.StrictTypes := True;              // Enable strict type enforcement
```

## Injecting Custom Globals

You can inject Pascal functions and values into the script's global scope by working with the engine's interpreter scope directly.

### Injecting a Value

```pascal
uses
  Classes,
  SysUtils,

  Goccia.Engine,
  Goccia.Scope,
  Goccia.Values.Primitives;

var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := 'console.log("version: " + APP_VERSION);';
  Engine := TGocciaEngine.Create('app.js', Source, []);
  try
    // Inject a constant into the global scope
    Engine.Interpreter.GlobalScope.DefineLexicalBinding(
      'APP_VERSION',
      TGocciaStringLiteralValue.Create('1.2.3'),
      dtConst
    );
    Engine.Execute; // prints "version: 1.2.3"
  finally
    Engine.Free;
    Source.Free;
  end;
end;
```

### Injecting a Native Function

Native functions are Pascal methods exposed to GocciaScript. They receive arguments and a `this` value, and return a `TGocciaValue`.

```pascal
uses
  Classes,
  DateUtils,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TMyHost = class
    function GetTimestamp(AArgs: TGocciaArgumentsCollection;
      AThisValue: TGocciaValue): TGocciaValue;
  end;

function TMyHost.GetTimestamp(AArgs: TGocciaArgumentsCollection;
  AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(DateTimeToUnix(Now));
end;

var
  Engine: TGocciaEngine;
  Source: TStringList;
  Host: TMyHost;
  Func: TGocciaNativeFunctionValue;
begin
  Host := TMyHost.Create;
  Source := TStringList.Create;
  Source.Text := 'const ts = getTimestamp(); console.log(ts);';
  Engine := TGocciaEngine.Create('app.js', Source, []);
  try
    // Create a native function: callback, name, arity (-1 for variadic)
    Func := TGocciaNativeFunctionValue.Create(Host.GetTimestamp, 'getTimestamp', 0);
    Engine.Interpreter.GlobalScope.DefineLexicalBinding('getTimestamp', Func, dtConst);
    Engine.Execute;
  finally
    Engine.Free;
    Source.Free;
    Host.Free;
  end;
end;
```

The callback signature is:

```pascal
TGocciaNativeFunctionCallback = function(
  Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue
): TGocciaValue of object;
```

- `Args` — The arguments collection. Use `Args.Length` and `Args.GetElement(I)` to access arguments.
- `ThisValue` — The `this` binding (relevant for method calls).
- Return value — Must be a `TGocciaValue`. Use `TGocciaUndefinedLiteralValue.UndefinedValue` for void functions.

### Injecting a Native Object with Methods

For a more structured API, create a `TGocciaObjectValue` and register native methods on it:

```pascal
uses
  Classes,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TFileSystemAPI = class
    function ReadFile(AArgs: TGocciaArgumentsCollection;
      AThisValue: TGocciaValue): TGocciaValue;
    function Exists(AArgs: TGocciaArgumentsCollection;
      AThisValue: TGocciaValue): TGocciaValue;
  end;

function TFileSystemAPI.ReadFile(AArgs: TGocciaArgumentsCollection;
  AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Content: TStringList;
begin
  Path := AArgs.GetElement(0).ToStringLiteral.Value;
  Content := TStringList.Create;
  try
    Content.LoadFromFile(Path);
    Result := TGocciaStringLiteralValue.Create(Content.Text);
  finally
    Content.Free;
  end;
end;

function TFileSystemAPI.Exists(AArgs: TGocciaArgumentsCollection;
  AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    FileExists(AArgs.GetElement(0).ToStringLiteral.Value)
  );
end;

var
  Engine: TGocciaEngine;
  Source: TStringList;
  API: TFileSystemAPI;
  FSObject: TGocciaObjectValue;
begin
  API := TFileSystemAPI.Create;
  Source := TStringList.Create;
  Source.Text := 'const content = fs.readFile("data.txt"); console.log(content);';
  Engine := TGocciaEngine.Create('app.js', Source, []);
  try
    FSObject := TGocciaObjectValue.Create;
    FSObject.RegisterNativeMethod(
      TGocciaNativeFunctionValue.Create(API.ReadFile, 'readFile', 1));
    FSObject.RegisterNativeMethod(
      TGocciaNativeFunctionValue.Create(API.Exists, 'exists', 1));

    Engine.Interpreter.GlobalScope.DefineLexicalBinding('fs', FSObject, dtConst);
    Engine.Execute;
  finally
    Engine.Free;
    Source.Free;
    API.Free;
  end;
end;
```

Scripts can then call `fs.readFile("path")` and `fs.exists("path")` as if they were built-in.

## Reading Return Values

The engine returns the result of the last evaluated expression as a `TGocciaValue`. Use the type conversion methods to extract Pascal values:

```pascal
var
  Result: TGocciaValue;
begin
  Result := TGocciaEngine.RunScript('40 + 2;');

  // Type checking
  if Result is TGocciaNumberLiteralValue then
    WriteLn(TGocciaNumberLiteralValue(Result).Value);  // 42.0

  // Generic conversion methods (available on all TGocciaValue)
  WriteLn(Result.ToStringLiteral.Value);   // "42"
  WriteLn(Result.ToNumberLiteral.Value);   // 42.0
  WriteLn(Result.ToBooleanLiteral.Value);  // True
  WriteLn(Result.TypeOf);                  // "number"
end;
```

### Value Type Hierarchy

| Pascal Class | JavaScript Type | Key Properties |
|-------------|----------------|----------------|
| `TGocciaNumberLiteralValue` | `number` | `.Value: Double`, `.IsNaN`, `.IsInfinity` |
| `TGocciaStringLiteralValue` | `string` | `.Value: string` |
| `TGocciaBooleanLiteralValue` | `boolean` | `.Value: Boolean` |
| `TGocciaNullLiteralValue` | `null` | Singleton via `.NullValue` |
| `TGocciaUndefinedLiteralValue` | `undefined` | Singleton via `.UndefinedValue` |
| `TGocciaObjectValue` | `object` | `.GetProperty(Name)`, `.AssignProperty(Name, Value)` |
| `TGocciaArrayValue` | `object` (array) | `.Elements: TGocciaValueList` |
| `TGocciaFunctionValue` | `function` | `.Call(Args, ThisValue)` |
| `TGocciaInstanceValue` | `object` (class instance) | `.ClassValue`, `.GetProperty(Name)` |

## Error Handling

GocciaScript errors surface as Pascal exceptions. See [Errors](errors.md) for the JavaScript-side error types, display format, and JSON output envelope. On the Pascal side, wrap execution in `try...except`:

```pascal
uses
  SysUtils,

  Goccia.Engine,
  Goccia.Error;

try
  TGocciaEngine.RunScript('undeclaredVariable;');
except
  on E: TGocciaRuntimeError do
    WriteLn('Runtime error: ', E.Message);
  on E: TGocciaSyntaxError do
    WriteLn('Syntax error: ', E.Message);
  on E: TGocciaLexerError do
    WriteLn('Lexer error: ', E.Message);
  on E: Exception do
    WriteLn('Unexpected error: ', E.Message);
end;
```

| Exception Class | When |
|----------------|------|
| `TGocciaLexerError` | Invalid tokens (unterminated strings, invalid characters) |
| `TGocciaSyntaxError` | Parse failures (unexpected token, missing semicolon) |
| `TGocciaRuntimeError` | Execution errors (type errors, reference errors, throw statements) |
| `TGocciaTypeError` | Type-specific runtime error |
| `TGocciaReferenceError` | Undefined variable access |
| `TGocciaThrowValue` | JavaScript `throw` — wraps any thrown value including `RangeError` |

## Execution Limits

Two mechanisms prevent runaway scripts: wall-clock timeouts and instruction limits. Both use thread-local storage and are safe with parallel workers — each thread gets its own independent counter.

### Timeout

`StartExecutionTimeout` arms a wall-clock deadline. `ClearExecutionTimeout` disarms it. The check is sampled (every 1024th call site) to minimize overhead.

```pascal
uses
  Goccia.Timeout;

StartExecutionTimeout(5000); // 5 000 ms
try
  Engine.Execute;
finally
  ClearExecutionTimeout;
end;
```

Raises `TGocciaTimeoutError` when the deadline is exceeded. A value of zero disables the timeout.

### Instruction Limit

`StartInstructionLimit` caps the number of execution steps. In bytecode mode the counter increments on every dispatched instruction (exact). In interpreter mode it increments at function-call and loop-iteration checkpoints (approximate).

```pascal
uses
  Goccia.InstructionLimit;

StartInstructionLimit(1000000); // 1 000 000 steps
try
  Engine.Execute;
finally
  ClearInstructionLimit;
end;
```

Raises `TGocciaInstructionLimitError` when the limit is reached. A value of zero (the default) skips all counter increments and limit comparisons — only the guard read of `GMaxInstructions` remains on the hot path.

The CLI tools expose both limits as flags; see [Build System — Run Commands](build-system.md) for usage.

## FPU Exception Mask

FPU exceptions — divide-by-zero, overflow, underflow, invalid operation, denormalized operand, and precision loss — are hardware signals raised by the floating-point unit when an operation produces a special result. By default, FreePascal leaves some of these unmasked, which causes runtime exceptions on operations like `0.0 / 0.0` instead of returning `NaN`.

Both `TGocciaEngine` and `TGocciaVM` mask all FPU exceptions on creation (via `SetExceptionMask`) to enable IEEE 754 semantics (`NaN`, `Infinity`, `-0`). The previous mask is saved in the constructor and **restored in the destructor**, so the host application's FPU state is not permanently altered. This is transparent for one-shot execution (`RunScript`), but embedders creating long-lived engine instances should be aware that FPU exceptions are suppressed while the engine exists. If the host application depends on FPU exception handlers for its own error handling, those handlers will not fire while a `TGocciaEngine` or `TGocciaVM` instance is alive.

## Microtask Queue (Promises)

The engine initializes a singleton microtask queue (`TGocciaMicrotaskQueue`) alongside the GC. Promise `.then()` callbacks are enqueued as microtasks and **drained automatically** after each `Execute`, `ExecuteWithTiming`, or `ExecuteProgram` call. Embedders do not need to drain the queue manually.

This means:

- All synchronous code in the script runs to completion first.
- All pending `.then()` callbacks fire after the script finishes.
- Chained `.then()` handlers are processed in the same drain cycle.

The execution ordering follows ECMAScript specification semantics — the script is one macrotask, and microtasks drain after it completes. Thenable adoption is deferred via a microtask per the spec's PromiseResolveThenableJob.

```pascal
// Promises work automatically — no manual queue management needed
Source.Text := 'Promise.resolve(42).then((v) => console.log(v));';
Engine.Execute;  // prints "42" (microtasks drain after Execute)
```

For long-lived engines (REPL-style), each `Execute` call drains its own microtasks. Promise callbacks from one execution will not leak into the next — even if the script throws an exception, the engine clears any pending microtasks in a `finally` block.

## Garbage Collector

The engine initializes a mark-and-sweep garbage collector (`TGarbageCollector`) automatically. In most embedding scenarios, no manual GC interaction is needed. The GC collects unreachable values during execution.

For long-running engines (REPL-style), the GC runs automatically. If you need to trigger collection manually between script executions:

```pascal
uses
  Goccia.GarbageCollector;

TGarbageCollector.Instance.Collect;
```

**Temporary roots:** If your Pascal code holds references to `TGocciaValue` objects outside of any GocciaScript scope (e.g., in a Pascal list while the engine runs), protect them from collection:

```pascal
TGarbageCollector.Instance.AddTempRoot(MyValue);
try
  Engine.Execute; // MyValue won't be collected during execution
finally
  TGarbageCollector.Instance.RemoveTempRoot(MyValue);
end;
```

## Existing Embeddings

The repository includes five embedding examples:

| Program | File | Description |
|---------|------|-------------|
| `GocciaScriptLoader` | `source/app/GocciaScriptLoader.dpr` | Executes script files (`.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`) from disk or stdin, with optional JSON output, injected globals, and execution timeouts for one-shot automation |
| `GocciaREPL` | `source/app/GocciaREPL.dpr` | Interactive read-eval-print loop (long-lived engine) |
| `GocciaTestRunner` | `source/app/GocciaTestRunner.dpr` | Runs test suites with `ggTestAssertions` enabled |
| `GocciaBenchmarkRunner` | `source/app/GocciaBenchmarkRunner.dpr` | Runs benchmarks with `ggBenchmark` enabled from files or stdin |
| `GocciaBundler` | `source/app/GocciaBundler.dpr` | Standalone bytecode compiler — compiles source files to `.gbc` without execution |

These serve as reference implementations for the patterns described above.

## Application Base Class

`TGocciaApplication` (`Goccia.Application.pas`) provides the standard lifecycle for any GocciaScript host application. It manages GC initialization/shutdown and unified error handling, with no CLI dependency.

```pascal
type
  TMyApp = class(TGocciaApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  // Your application logic here
  // GC is already initialized; errors are caught by HandleError
end;

begin
  ExitCode := TGocciaApplication.RunApplication(TMyApp, 'MyApp');
end.
```

**What the base class handles:**

- `TGarbageCollector.Initialize` / `Shutdown` lifecycle
- Exception dispatch via virtual `HandleError` (supports `TGocciaError`, `TGocciaThrowValue`, `EGocciaBytecodeThrow` with full source context and colored output)
- Exit code management (0 = success, 1 = error)

**Overridable hooks:**

- `Execute` (abstract) — your application logic
- `HandleError(AException)` — customize error display (e.g., JSON output)
- `GlobalBuiltins` — return `TGocciaGlobalBuiltins` set for optional built-in registration

For CLI tools, use `TGocciaCLIApplication` instead, which adds argument parsing, help generation, and singleton lifecycle management on top of `TGocciaApplication`.

## Minimal Embedding Checklist

1. Add `source/units/` and `source/shared/` to your FreePascal unit search path (or use `config.cfg`)
2. `uses Goccia.Engine, Goccia.Values.Primitives;`
3. Call `TGocciaEngine.RunScript(...)` or create an instance with `TGocciaEngine.Create(...)`
4. Choose your built-in set via `TGocciaGlobalBuiltins`
5. Inject custom globals via `Engine.Interpreter.GlobalScope.DefineLexicalBinding(...)`
6. Handle exceptions from `Goccia.Error`
7. Free the engine when done (the GC cleans up all runtime values)
For host-provided configuration data, `TGocciaEngine` exposes `InjectGlobalsFromJSON(...)`, `InjectGlobalsFromJSON5(...)`, `InjectGlobalsFromTOML(...)`, `InjectGlobalsFromYAML(...)`, and `InjectGlobalsFromModule(...)`. JSON5- and TOML-backed globals use the same top-level-object/table contract as JSON/YAML object globals.
