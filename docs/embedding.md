# Embedding the Engine

GocciaScript is designed to be embedded in FreePascal applications. The `TGocciaEngine` class provides the public API for executing scripts, controlling available built-ins, and injecting custom globals.

## Quick Start

The simplest way to run a script:

```pascal
uses Goccia.Engine;

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
uses Classes, Goccia.Engine, Goccia.Values.Primitives;

var
  Engine: TGocciaEngine;
  Source: TStringList;
  Result: TGocciaValue;
begin
  Source := TStringList.Create;
  Engine := TGocciaEngine.Create('session', Source, TGocciaEngine.DefaultGlobals);
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

### Timing

All `RunScript*` methods and the `Execute` method return a `TGocciaScriptResult` record that includes nanosecond-precision timing for each pipeline phase:

```pascal
uses Goccia.Engine, TimingUtils;

var
  ScriptResult: TGocciaScriptResult;
begin
  ScriptResult := TGocciaEngine.RunScript(Source, 'bench.js', TGocciaEngine.DefaultGlobals);
  WriteLn('Lex: ', FormatDuration(ScriptResult.LexTimeNanoseconds));
  WriteLn('Parse: ', FormatDuration(ScriptResult.ParseTimeNanoseconds));
  WriteLn('Execute: ', FormatDuration(ScriptResult.ExecuteTimeNanoseconds));
  WriteLn('Total: ', FormatDuration(ScriptResult.TotalTimeNanoseconds));
end;
```

`FormatDuration` (from `TimingUtils`) automatically selects the appropriate unit: `ns` for values below 0.5μs, `μs` for values below 0.5ms, `ms` with two decimal places for values up to 10s, and `s` above that.

`TimingUtils` provides three clock functions: `GetNanoseconds` and `GetMilliseconds` for monotonic duration timing (`clock_gettime(CLOCK_MONOTONIC)` on Unix/macOS, `QueryPerformanceCounter` on Windows), and `GetEpochNanoseconds` for wall-clock epoch time (`clock_gettime(CLOCK_REALTIME)` on Unix/macOS, `GetSystemTimeAsFileTime` on Windows).

## Controlling Built-ins

The `TGocciaGlobalBuiltins` set controls which built-in objects are available to scripts. This is the primary sandboxing mechanism.

### Default Set

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
                  ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap];
```

### Available Flags

| Flag | Provides | Notes |
|------|----------|-------|
| `ggConsole` | `console.log` | Output to stdout |
| `ggMath` | `Math.*` | Constants and functions |
| `ggGlobalObject` | `Object.*` | `keys`, `assign`, `create`, `freeze`, etc. |
| `ggGlobalArray` | `Array.isArray`, `Array.from`, `Array.of` | Static array methods |
| `ggGlobalNumber` | `Number.*` | `parseInt`, `isNaN`, constants, etc. |
| `ggJSON` | `JSON.parse`, `JSON.stringify` | Serialization |
| `ggSymbol` | `Symbol`, `Symbol.for`, `Symbol.keyFor` | Unique property keys |
| `ggSet` | `Set` constructor and methods | Unique value collections |
| `ggMap` | `Map` constructor and methods | Key-value collections |
| `ggPromise` | `Promise` | Async operations with microtask queue |
| `ggTestAssertions` | `describe`, `test`, `expect` | Testing framework |
| `ggBenchmark` | `suite`, `bench` | Benchmark framework |

### Sandboxed Execution

To run untrusted code with minimal capabilities:

```pascal
// No console, no JSON, no collections — just core language + math
TGocciaEngine.RunScript(UntrustedCode, 'sandbox.js',
  [ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber]);
```

To add the test framework for a custom test runner:

```pascal
TGocciaEngine.RunScriptFromFile('tests/my-test.js',
  TGocciaEngine.DefaultGlobals + [ggTestAssertions]);
```

## Injecting Custom Globals

You can inject Pascal functions and values into the script's global scope by working with the engine's interpreter scope directly.

### Injecting a Value

```pascal
uses Goccia.Engine, Goccia.Values.Primitives, Goccia.Scope;

var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Text := 'console.log("version: " + APP_VERSION);';
  Engine := TGocciaEngine.Create('app.js', Source, TGocciaEngine.DefaultGlobals);
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
uses Goccia.Engine, Goccia.Values.Primitives, Goccia.Values.NativeFunction,
     Goccia.Arguments.Collection, Goccia.Scope;

type
  TMyHost = class
    function GetTimestamp(Args: TGocciaArgumentsCollection;
      ThisValue: TGocciaValue): TGocciaValue;
  end;

function TMyHost.GetTimestamp(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
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
  Engine := TGocciaEngine.Create('app.js', Source, TGocciaEngine.DefaultGlobals);
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
uses Goccia.Engine, Goccia.Values.Primitives, Goccia.Values.ObjectValue,
     Goccia.Values.NativeFunction, Goccia.Arguments.Collection, Goccia.Scope;

type
  TFileSystemAPI = class
    function ReadFile(Args: TGocciaArgumentsCollection;
      ThisValue: TGocciaValue): TGocciaValue;
    function Exists(Args: TGocciaArgumentsCollection;
      ThisValue: TGocciaValue): TGocciaValue;
  end;

function TFileSystemAPI.ReadFile(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Content: TStringList;
begin
  Path := Args.GetElement(0).ToStringLiteral.Value;
  Content := TStringList.Create;
  try
    Content.LoadFromFile(Path);
    Result := TGocciaStringLiteralValue.Create(Content.Text);
  finally
    Content.Free;
  end;
end;

function TFileSystemAPI.Exists(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    FileExists(Args.GetElement(0).ToStringLiteral.Value)
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
  Engine := TGocciaEngine.Create('app.js', Source, TGocciaEngine.DefaultGlobals);
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
| `TGocciaArrayValue` | `object` (array) | `.Elements: TList<TGocciaValue>` |
| `TGocciaFunctionValue` | `function` | `.Call(Args, ThisValue)` |
| `TGocciaInstanceValue` | `object` (class instance) | `.ClassValue`, `.GetProperty(Name)` |

## Error Handling

GocciaScript errors surface as Pascal exceptions. Wrap execution in `try...except`:

```pascal
uses Goccia.Engine, Goccia.Error;

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
| `TGocciaRangeError` | Value out of range |

## Microtask Queue (Promises)

When `ggPromise` is included in the globals set, the engine initializes a singleton microtask queue (`TGocciaMicrotaskQueue`) alongside the GC. Promise `.then()` callbacks are enqueued as microtasks and **drained automatically** after each `Execute`, `ExecuteWithTiming`, or `ExecuteProgram` call. Embedders do not need to drain the queue manually.

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

The engine initializes a mark-and-sweep garbage collector (`TGocciaGarbageCollector`) automatically. In most embedding scenarios, no manual GC interaction is needed. The GC collects unreachable values during execution.

For long-running engines (REPL-style), the GC runs automatically. If you need to trigger collection manually between script executions:

```pascal
uses Goccia.GarbageCollector;

TGocciaGarbageCollector.Instance.Collect;
```

**Temporary roots:** If your Pascal code holds references to `TGocciaValue` objects outside of any GocciaScript scope (e.g., in a Pascal list while the engine runs), protect them from collection:

```pascal
TGocciaGarbageCollector.Instance.AddTempRoot(MyValue);
try
  Engine.Execute; // MyValue won't be collected during execution
finally
  TGocciaGarbageCollector.Instance.RemoveTempRoot(MyValue);
end;
```

## Existing Embeddings

The repository includes four embedding examples:

| Program | File | Description |
|---------|------|-------------|
| `ScriptLoader` | `ScriptLoader.dpr` | Executes `.js` files from disk (one-shot) |
| `REPL` | `REPL.dpr` | Interactive read-eval-print loop (long-lived engine) |
| `TestRunner` | `TestRunner.dpr` | Runs test suites with `ggTestAssertions` enabled |
| `BenchmarkRunner` | `BenchmarkRunner.dpr` | Runs benchmarks with `ggBenchmark` enabled |

These serve as reference implementations for the patterns described above.

## Minimal Embedding Checklist

1. Add `units/` to your FreePascal unit search path (or use `config.cfg`)
2. `uses Goccia.Engine, Goccia.Values.Primitives;`
3. Call `TGocciaEngine.RunScript(...)` or create an instance with `TGocciaEngine.Create(...)`
4. Choose your built-in set via `TGocciaGlobalBuiltins`
5. Inject custom globals via `Engine.Interpreter.GlobalScope.DefineLexicalBinding(...)`
6. Handle exceptions from `Goccia.Error`
7. Free the engine when done (the GC cleans up all runtime values)
