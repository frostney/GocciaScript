<!-- doc-length-limit: 1000 -->
# Embedding the Engine

*For FreePascal developers who want to embed the GocciaScript engine in their own applications.*

## Executive Summary

- **Quick start** — `TGocciaRuntime.Create(...)` for the standard host/runtime surface; `TGocciaEngine.Create(...)` for core-language-only embedders. `TGocciaEngine.Create(...)` requires an explicit executor (`TGocciaInterpreterExecutor` for tree-walk or `TGocciaBytecodeExecutor` for bytecode VM); `TGocciaRuntime.Create(...)` is the convenience that picks one for you
- **Sandboxing** — Choose runtime globals and special-purpose tools with `TGocciaRuntimeGlobals`; inject custom globals via `DefineLexicalBinding`; enforce execution limits via timeout or instruction cap
- **Module resolution** — Pluggable resolver with extensionless imports, import maps, custom content providers, and global modules
- **Transparent GC** — Mark-and-sweep GC initializes automatically; FPU exceptions are masked for IEEE 754 semantics

GocciaScript is designed to be embedded in FreePascal applications. `TGocciaRuntime` is the default embedding entry point for host globals such as `console`, `fetch`, `URL`, JSON5, TOML, YAML, CSV/TSV, and SemVer. `TGocciaEngine` remains available through `Runtime.Engine` and as a core-language-only API for embedders that intentionally do not want runtime globals.

## Quick Start

The simplest way to run a script with the standard runtime surface:

```pascal
Source := TStringList.Create;
Source.Text := 'console.log("hello from GocciaScript");';
Runtime := TGocciaRuntime.Create('<inline>', Source);
try
  Runtime.Execute;
finally
  Runtime.Free;
  Source.Free;
end;
```

For files, use `TGocciaRuntime.RunScriptFromFile('app.js')` or load `app.js` into the `Source` list and pass the real filename to `TGocciaRuntime.Create('app.js', Source)`. Use `TGocciaEngine.RunScript*` only for core-language scripts that do not need runtime globals or file loading.

## Engine API

### Class Methods (One-Shot Execution)

These are convenience methods that create a core engine, execute, and clean up in a single call. They do not attach `TGocciaRuntime`, so host/runtime globals such as `console`, `fetch`, `URL`, JSON5, TOML, YAML, CSV/TSV, and SemVer are not available.

| Method | Description |
|--------|-------------|
| `TGocciaEngine.RunScript(Source)` | Execute a source string with core language globals only |
| `TGocciaEngine.RunScript(Source, FileName)` | Execute a source string with a diagnostic filename |
| `TGocciaEngine.RunScriptFromStringList(Source, FileName)` | Execute from a caller-provided `TStringList` |
| `TGocciaRuntime.RunScript(Source, FileName, RuntimeGlobals)` | Execute a source string with a runtime surface |
| `TGocciaRuntime.RunScriptFromFile(FileName, RuntimeGlobals)` | Load and execute a file through the runtime |
| `TGocciaRuntime.RunScriptFromStringList(Source, FileName, RuntimeGlobals)` | Execute from a `TStringList` with runtime globals |
All methods return `TGocciaScriptResult` — a record containing the result value, per-phase timing (in microseconds), and the filename.

`SourceType` is an engine-level language option, not a runtime option. Set `Engine.SourceType` (or use the CLI `--source-type=script|module`) to choose Script vs Module entry execution. File loading is separate: `TGocciaEngine` one-shot helpers accept source text or source lists only, while `TGocciaRuntime.RunScriptFromFile` is the runtime convenience API for loading an entry file.

### Instance Usage (Long-Lived Engine)

For interactive sessions (REPL, editor integration) or when you need to execute multiple scripts in the same scope, create an engine instance directly:

```pascal
uses
  Classes,

  Goccia.Engine,
  Goccia.Values.Primitives;

var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
  ScriptResult: TGocciaScriptResult;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('session', Source, Executor);
    try
      // First execution — defines a variable
      Source.Text := 'const x = 42;';
      Engine.Execute;

      // Second execution — uses the variable from the first
      Source.Text := 'x + 1;';
      ScriptResult := Engine.Execute;
      WriteLn(ScriptResult.Result.ToStringLiteral.Value); // 43
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
    Source.Free;
  end;
end;
```

The `TStringList` is passed by reference — update its contents and call `Execute` again to run new code in the same global scope. Variables, functions, and classes defined in previous executions remain available.

### Engine Constructor and Executor Ownership

Callers must pass an explicit executor — `TGocciaInterpreterExecutor` (in `Goccia.Engine`) for tree-walk or `TGocciaBytecodeExecutor` (in `Goccia.Engine.Backend`) for the bytecode VM. The engine does not own the executor; the caller frees it after the engine. `TGocciaRuntime`'s file/source convenience overloads handle this internally for embedders who want the default interpreter setup.

### Automatic Semicolon Insertion

ASI is disabled by default. To enable ECMAScript-compliant automatic semicolon insertion (ES2026 §12.10), set the `ASIEnabled` property after creating the engine:

```pascal
Executor := TGocciaInterpreterExecutor.Create;
try
  Engine := TGocciaEngine.Create('app.js', Source, Executor);
  Engine.ASIEnabled := True;  // Semicolons are now optional per ES2026 rules
  Engine.Execute;
finally
  Engine.Free;
  Executor.Free;
end;
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
  ScriptResult := TGocciaEngine.RunScript(Source, 'bench.js');
  WriteLn('Lex: ', FormatDuration(ScriptResult.LexTimeNanoseconds));
  WriteLn('Parse: ', FormatDuration(ScriptResult.ParseTimeNanoseconds));
  WriteLn('Execute: ', FormatDuration(ScriptResult.ExecuteTimeNanoseconds));
  WriteLn('Total: ', FormatDuration(ScriptResult.TotalTimeNanoseconds));
end;
```

`FormatDuration` (from `TimingUtils`) automatically selects the appropriate unit: `ns` for values below 0.5μs, `μs` for values below 0.5ms, `ms` with two decimal places for values up to 10s, and `s` above that.

`TimingUtils` provides three clock functions: `GetNanoseconds` and `GetMilliseconds` for monotonic duration timing (`clock_gettime(CLOCK_MONOTONIC)` on Unix/macOS, `QueryPerformanceCounter` on Windows), and `GetEpochNanoseconds` for wall-clock epoch time (`clock_gettime(CLOCK_REALTIME)` on Unix/macOS, `GetSystemTimeAsFileTime` on Windows).

## Engine Lifecycle & Realm Isolation

Every `TGocciaEngine` owns a `TGocciaRealm` (`Goccia.Realm.pas`) — a per-engine container for the mutable intrinsic prototype objects (`Array.prototype`, `Object.prototype`, `Map.prototype`, every error prototype, every Temporal prototype, and so on). The realm is created in the engine constructor and torn down in the engine destructor; tear-down unpins every prototype the realm owns so the GC can collect them before the next engine starts up.

This is the **strongest** isolation boundary the engine provides. Two engines created back-to-back on the same thread see pristine intrinsics — userland mutations on one engine's `Array.prototype` cannot leak into the next engine's `Array.prototype`, even if the mutation added a non-configurable property that JS-level cleanup cannot reverse.

### When this matters for embedders

- **Test runners and conformance harnesses** — Per-file engine recreation is the cleanest way to give each test file fresh intrinsics. `GocciaTestRunner` does this automatically; if you build a custom runner, free the engine between files rather than reusing it.
- **Sandboxes evaluating untrusted scripts** — Each script can mutate `Object.prototype`. Recreating the engine between scripts is the only reliable way to guarantee the next script starts from a clean slate.
- **REPLs and long-lived sessions** — These intentionally **share** intrinsics across `Execute` calls (so users can mutate `Array.prototype` from one line and observe the change on the next). Reuse the same engine instance.

### What you do not need to do

- **Do not** call `SetCurrentRealm` directly. The engine sets `CurrentRealm` (a thread-local pointer) on construction and clears it on tear-down.
- **Do not** pin or unpin prototype objects manually after engine startup. The realm pins everything stored in a slot via `SetSlot`, and unpins them all at tear-down.
- **Do not** cache prototype object pointers in long-lived Pascal state. Those objects are realm-scoped — they become invalid the moment the engine that owns them is freed. If you need the current `Array.prototype`, look it up live (e.g. via `Engine.Interpreter.GlobalScope`).

### Threading

`CurrentRealm` is a `threadvar`. Each worker thread that constructs an engine gets its own realm pointer. The thread pool used by `GocciaTestRunner --jobs=N` relies on this: each worker creates and destroys engines on its own thread, and realm tear-down on one worker has no effect on intrinsics seen by the others.

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

`TGocciaModuleResolver` also exposes `LoadImportMap(path)` and `DiscoverProjectConfig(startDirectory)` helpers for browser-style import map JSON and `goccia.json` project configuration files. The shared CLI frontends (`GocciaScriptLoader`, `GocciaTestRunner`, and `GocciaBenchmarkRunner`) all use `Goccia.Modules.Configuration.ConfigureModuleResolver(...)` on top of this resolver surface.

**Config file discovery is automatic for CLI apps** — `TGocciaCLIApplication` discovers `goccia.toml` / `goccia.json5` / `goccia.json` (priority order: TOML > JSON5 > JSON) from the entry file's directory upward and applies option defaults before execution. When embedding the engine directly, this does not happen automatically. To replicate it, use the general-purpose `CLI.ConfigFile` unit (`DiscoverConfigFile`, `ApplyConfigFile`). Note that `ApplyConfigFile` only handles `.json` by default — to support `.json5` and `.toml`, register their parsers first via `RegisterConfigParser` (see `Goccia.CLI.Application.pas` for the pattern). For import-map resolution only, use `TGocciaModuleResolver.DiscoverProjectConfig` and `LoadImportMap`. See [Configuration File](build-system.md#configuration-file-gocciajson) for the full reference.

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
  Executor: TGocciaInterpreterExecutor;
begin
  Resolver := TMyResolver.Create('/path/to/project');
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('app.js', Source, Resolver, Executor);
    try
      Engine.Execute;
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
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
  Executor: TGocciaInterpreterExecutor;
  ModuleLoader: TGocciaModuleLoader;
  Resolver: TMemoryResolver;
  Provider: TMemoryContentProvider;
begin
  Resolver := TMemoryResolver.Create;
  Provider := TMemoryContentProvider.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    ModuleLoader := TGocciaModuleLoader.Create('memory:/app.js', Resolver,
      Provider);
    try
      Engine := TGocciaEngine.Create('memory:/app.js', Source,
        ModuleLoader, Executor);
      try
        Engine.Execute;
      finally
        Engine.Free;
      end;
    finally
      ModuleLoader.Free;  // caller owns injected module loaders
    end;
  finally
    Executor.Free;
    Provider.Free;  // caller owns injected providers
    Resolver.Free;
  end;
end;
```

`TGocciaEngine` also accepts an injected module loader via its constructor. When no loader is supplied, it creates a default `TGocciaModuleLoader` with the standard resolver but no filesystem content provider. `TGocciaRuntime` installs the filesystem provider when attached; core-language-only embedders that need imports should inject their own provider.

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

This provides the foundation for future built-in module packages that can be coupled to runtime-global configuration.

## Console Output Capture

By default, `console.log` and friends write directly to stdout. The `OutputCallback` property lets you intercept all console output programmatically:

```pascal
uses
  Classes,

  Goccia.Builtins.Console,
  Goccia.Runtime;

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
  Runtime: TGocciaRuntime;
  Source: TStringList;
  Logger: TMyLogger;
begin
  Logger := TMyLogger.Create;
  Source := TStringList.Create;
  Source.Text := 'console.log("hello"); console.warn("careful");';
  Runtime := TGocciaRuntime.Create('app.js', Source);
  try
    Runtime.BuiltinConsole.OutputCallback := Logger.OnConsoleOutput;
    Runtime.Execute;
    // Output:
    //   [log] hello
    //   [warn] Warning: careful
  finally
    Runtime.Free;
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
Runtime.BuiltinConsole.LogCallback := MyHandler.OnLog;
Runtime.BuiltinConsole.Enabled := False;  // no stdout, but LogCallback still fires
```

The `TGocciaCLIApplication`-based frontends that attach the default runtime (ScriptLoader, TestRunner, BenchmarkRunner, and REPL) use `LogCallback` internally for `--log=<file>`, which captures console output to a log file in `[method] line` format. The TestRunner silences workers via `Enabled := False` (not by replacing JS methods), so `LogCallback` fires on every console call even in parallel mode. File writes are serialized with a critical section so `--log` is thread-safe even with `--jobs=N`.

## Built-in Registration

Core language built-ins (Math, Object, Array, JSON, Promise, Temporal, typed arrays, etc.) are registered by `TGocciaEngine`. Runtime globals that are not part of the language core (Console, CSV, JSON5, JSONL, TOML, TSV, YAML, TextEncoder/TextDecoder, URL, fetch, performance, SemVer) are provided by `Goccia.Runtime`. Hosts should use `TGocciaRuntime.Create(...)` for the standard surface or pass a `TGocciaRuntimeGlobals` set to choose a smaller runtime surface.

When you already have an engine, pass it to the runtime constructor:

```pascal
Executor := TGocciaInterpreterExecutor.Create;
try
  Engine := TGocciaEngine.Create('app.js', Source, Executor);
  Runtime := TGocciaRuntime.Create(Engine);
  try
    Runtime.Execute;
  finally
    Runtime.Free;
    Engine.Free;
  end;
finally
  Executor.Free;
end;
```

Passing an engine does not transfer ownership by default. Use `TGocciaRuntime.Create(Engine, True)` or `TGocciaRuntime.Create(Engine, RuntimeGlobals, True)` when the runtime should free the engine. The executor is always owned by the caller — free it after the engine.

### Special-Purpose Runtime Globals

`TGocciaRuntimeGlobals` controls three special-purpose globals that are off by default. The runtime registers them only when the matching entry is present:

| Runtime global | Provides | Notes |
|------|----------|-------|
| `rgTestAssertions` | `describe`, `test`, `expect` | Testing framework (GocciaTestRunner adds this) |
| `rgBenchmark` | `suite`, `bench` | Benchmark framework (GocciaBenchmarkRunner adds this) |
| `rgFFI` | `FFI.open`, `FFILibrary`, `FFIPointer` | Foreign Function Interface for native shared libraries |

When embedding, pass `rgFFI` in the `TGocciaRuntimeGlobals` set during runtime creation to enable the FFI global. CLI tools (ScriptLoader, REPL, TestRunner, BenchmarkRunner, Bundler) expose this as the `--unsafe-ffi` flag.

To add the test framework for a custom test runner:

```pascal
Executor := TGocciaInterpreterExecutor.Create;
try
  Engine := TGocciaEngine.Create('tests/my-test.js', Source, Executor);
  Runtime := TGocciaRuntime.Create(Engine,
    DefaultRuntimeGlobals + [rgTestAssertions], True);
  try
    Runtime.Execute;
  finally
    Runtime.Free;
  end;
finally
  Executor.Free;
end;
```

Runtime globals can be reduced by passing a smaller `TGocciaRuntimeGlobals` set, for example `TGocciaRuntime.Create(Engine, [rgConsole], True)`.

### Preprocessors and Compatibility

| System | Type | Default | Purpose |
|--------|------|---------|---------|
| `Preprocessors` | `TGocciaPreprocessors` | `[ppJSX]` | Source transformations before parsing |
| `Compatibility` | `TGocciaCompatibilityFlags` | `[]` | Parser behavior toggles |
| `SourceType` | `TGocciaSourceType` | `stScript` | Load entry as a Script (default) or Module |
| `StrictTypes` | `Boolean` | `False` | Runtime enforcement of type annotations (works in both interpreter and bytecode); setter propagates to the active executor and interpreter scope |

```pascal
Executor := TGocciaInterpreterExecutor.Create;
try
  Engine := TGocciaEngine.Create('app.js', Source, Executor);
  Engine.Preprocessors := [];              // Disable JSX
  Engine.ASIEnabled := True;               // Enable ASI (convenience for cfASI)
  Engine.SourceType := stModule;           // Run entry as a Module (top-level this is undefined; import.meta resolves)
  Engine.StrictTypes := True;              // Enforce type annotations in both modes
finally
  Executor.Free;
end;
```

When `SourceType` is `stModule`, `Execute` runs the entry program in a fresh module scope (`skModule`) with `this = undefined`, mirroring the semantics imported modules already receive from the module loader (ES2026 §16.2.1.6.4). The CLI surface for this is `--source-type=script|module` and the matching `goccia.json` key `"source-type"`.

**Top-level binding persistence differs between the two modes.** In default `stScript` mode the engine reuses `Interpreter.GlobalScope` across every call to `Execute`, which is what makes the [long-lived engine pattern](#instance-usage-long-lived-engine) above work — `const x = 42` defined in one `Execute` is visible to the next. In `stModule` mode each `Execute` allocates a brand-new `skModule` child scope, so top-level `let`/`const`/`class` declarations live and die with that single call. If callers need cross-`Execute` persistence, keep `SourceType` at `stScript` (the default) or expose the desired symbols through the global scope (e.g. `Engine.RegisterGlobal(...)` or `Engine.Interpreter.GlobalScope.DefineLexicalBinding(...)`).

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
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := 'APP_VERSION;';
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('app.js', Source, Executor);
    try
      // Inject a constant into the global scope
      Engine.Interpreter.GlobalScope.DefineLexicalBinding(
        'APP_VERSION',
        TGocciaStringLiteralValue.Create('1.2.3'),
        dtConst
      );
      Engine.Execute; // evaluates "1.2.3"
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
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
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
  Host: TMyHost;
  Func: TGocciaNativeFunctionValue;
begin
  Host := TMyHost.Create;
  Source := TStringList.Create;
  Source.Text := 'getTimestamp();';
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('app.js', Source, Executor);
    try
      // Create a native function: callback, name, arity (-1 for variadic)
      Func := TGocciaNativeFunctionValue.Create(Host.GetTimestamp, 'getTimestamp', 0);
      Engine.Interpreter.GlobalScope.DefineLexicalBinding('getTimestamp', Func, dtConst);
      Engine.Execute;
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
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
  Executor: TGocciaInterpreterExecutor;
  Source: TStringList;
  API: TFileSystemAPI;
  FSObject: TGocciaObjectValue;
begin
  API := TFileSystemAPI.Create;
  Source := TStringList.Create;
  Source.Text := 'fs.readFile("data.txt");';
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('app.js', Source, Executor);
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
    end;
  finally
    Executor.Free;
    Source.Free;
    API.Free;
  end;
end;
```

Scripts can then call `fs.readFile("path")` and `fs.exists("path")` as if they were built-in.

## Reading Return Values

`TGocciaEngine.RunScript`, `TGocciaRuntime.RunScript`, and `Execute` return a `TGocciaScriptResult` record. Its `Result` field holds the last evaluated expression as a `TGocciaValue`; use the type conversion methods on that value to extract Pascal values:

```pascal
var
  ScriptResult: TGocciaScriptResult;
begin
  ScriptResult := TGocciaEngine.RunScript('40 + 2;');

  // Type checking
  if ScriptResult.Result is TGocciaNumberLiteralValue then
    WriteLn(TGocciaNumberLiteralValue(ScriptResult.Result).Value);  // 42.0

  // Generic conversion methods (available on all TGocciaValue)
  WriteLn(ScriptResult.Result.ToStringLiteral.Value);   // "42"
  WriteLn(ScriptResult.Result.ToNumberLiteral.Value);   // 42.0
  WriteLn(ScriptResult.Result.ToBooleanLiteral.Value);  // True
  WriteLn(ScriptResult.Result.TypeOf);                  // "number"
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

### Call Stack Depth Limit

`SetMaxStackDepth` caps the number of nested function calls. Exceeding the limit throws a JavaScript `RangeError` with the message `"Maximum call stack size exceeded"` (matching V8 convention). The default is 3 500 frames. A value of zero disables the limit entirely.

In bytecode mode the VM uses a trampoline: bytecode-to-bytecode calls are dispatched iteratively via an explicit frame stack, so the Pascal call stack stays flat regardless of JS call depth. The interpreter mode uses Pascal recursion and relies on the depth check to prevent overflow.

```pascal
uses
  Goccia.StackLimit;

SetMaxStackDepth(5000);   // custom limit
// SetMaxStackDepth(0);   // no limit
Engine.Execute;
```

The CLI tools expose all three limits as flags; see [Build System — Run Commands](build-system.md) for usage.

## FPU Exception Mask

FPU exceptions — divide-by-zero, overflow, underflow, invalid operation, denormalized operand, and precision loss — are hardware signals raised by the floating-point unit when an operation produces a special result. By default, FreePascal leaves some of these unmasked, which causes runtime exceptions on operations like `0.0 / 0.0` instead of returning `NaN`.

Both `TGocciaEngine` and `TGocciaVM` mask all FPU exceptions on creation (via `SetExceptionMask`) to enable IEEE 754 semantics (`NaN`, `Infinity`, `-0`). The previous mask is saved in the constructor and **restored in the destructor**, so the host application's FPU state is not permanently altered. This is transparent for one-shot execution (`RunScript`), but embedders creating long-lived engine instances should be aware that FPU exceptions are suppressed while the engine exists. If the host application depends on FPU exception handlers for its own error handling, those handlers will not fire while a `TGocciaEngine` or `TGocciaVM` instance is alive.

## Microtask Queue (Promises)

The engine initializes a singleton microtask queue (`TGocciaMicrotaskQueue`) alongside the GC. Promise `.then()` callbacks are enqueued as microtasks and **drained automatically** after each `Execute` or `ExecuteProgram` call. Fetch completions are also pumped before these calls return, then their Promise reactions drain through the same microtask queue. Embedders do not need to drain either queue manually.

This means:

- All synchronous code in the script runs to completion first.
- All pending `.then()` callbacks fire after the script finishes.
- Chained `.then()` handlers are processed in the same drain cycle.
- On successful execution, pending `fetch()` requests complete before `Execute` returns; if execution throws, pending fetches are detached and late completions are discarded. The microtask queue is still only used for Promise reactions, not for network I/O.

The execution ordering follows ECMAScript specification semantics — the script is one macrotask, and microtasks drain after it completes. Thenable adoption is deferred via a microtask per the spec's PromiseResolveThenableJob.

```pascal
// Promises work automatically — no manual queue management needed
Source.Text := 'Promise.resolve(42).then((v) => { globalThis.answer = v; });';
Engine.Execute;  // microtasks drain before Execute returns
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

**Memory ceiling:** The GC auto-detects physical memory and defaults to half of RAM, capped at 8 GB on 64-bit or 700 MB on 32-bit (512 MB fallback when detection fails). Override with `MaxBytes` to impose a custom limit. Allocations exceeding it raise a JavaScript `RangeError`:

```pascal
TGarbageCollector.Instance.MaxBytes := 10 * 1024 * 1024; // 10 MB limit
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
| `GocciaTestRunner` | `source/app/GocciaTestRunner.dpr` | Runs test suites with `rgTestAssertions` enabled |
| `GocciaBenchmarkRunner` | `source/app/GocciaBenchmarkRunner.dpr` | Runs benchmarks with `rgBenchmark` enabled from files or stdin |
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

For CLI tools, use `TGocciaCLIApplication` instead, which adds argument parsing, help generation, singleton lifecycle management, and a `ConfigureCreatedEngine` hook where tools attach `TGocciaRuntime` with their chosen `TGocciaRuntimeGlobals`.

## Minimal Embedding Checklist

1. Add `source/units/` and `source/shared/` to your FreePascal unit search path (or use `config.cfg`)
2. `uses Goccia.Runtime, Goccia.Values.Primitives;`
3. Create `TGocciaRuntime.Create(...)` for the standard runtime surface
4. Use `Runtime.Engine` for engine-level options such as ASI, source type, and compatibility flags
5. Choose your runtime surface and special-purpose globals via `TGocciaRuntimeGlobals`
6. Inject custom globals via `Runtime.Engine.Interpreter.GlobalScope.DefineLexicalBinding(...)`
7. Handle exceptions from `Goccia.Error`
8. Free the runtime when done; it owns and frees the engine only when it created the engine itself, or when you used an ownership-transfer overload such as `TGocciaRuntime.Create(Engine, True)`. `TGocciaRuntime.Create(Engine)` is non-owning by default, so embedders wrapping an existing engine must also free that engine.

For host-provided configuration data, `Runtime.Engine` exposes `InjectGlobalsFromJSON(...)`, `InjectGlobalsFromJSON5(...)`, `InjectGlobalsFromTOML(...)`, `InjectGlobalsFromYAML(...)`, and `InjectGlobalsFromModule(...)`. Strict JSON is core. JSON5, TOML, and YAML injection require the runtime extension installed by `TGocciaRuntime`; they use the same top-level-object/table contract as JSON object globals. Embedders that intentionally need only the core language can still use `TGocciaEngine` directly.
