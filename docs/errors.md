# Errors

*How GocciaScript reports errors to script authors, CLI users, and embedders.*

## Executive Summary

- **Error types** -- `Error`, `TypeError`, `ReferenceError`, `RangeError`, `SyntaxError`, `URIError`, `AggregateError`, `SuppressedError`, plus `TimeoutError` for the `--timeout` flag
- **Parser errors** -- Displayed with source context, a caret pointing to the exact column, and optional suggestion text (e.g., "Use 'let' or 'const' instead")
- **Runtime errors** -- Carry `name`, `message`, `stack`, and optional `cause`; catchable with `try`/`catch`/`finally`
- **JSON output** -- `--output=json` wraps every execution result in a structured envelope with `ok`, `error.type`, `error.message`, `error.line`, and `error.column`
- **`Error.cause`** -- All error constructors accept an options bag with a `cause` property for error chaining (ES2022+)

## Error Types

GocciaScript supports the standard ECMAScript error constructors plus two additional types. All JavaScript-visible error types inherit from `Error` and work with `instanceof`. `TimeoutError` is CLI-only and not exposed as a JavaScript constructor.

| Type | Thrown when | MDN |
|------|-----------|-----|
| [`Error`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error) | Generic errors; base class for all error types | [Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error) |
| [`TypeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) | Property access on `null`/`undefined`, calling a non-function, reassigning `const`, calling a constructor without `new` | [TypeError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) |
| [`ReferenceError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) | Accessing an undeclared variable, using a variable before initialization (TDZ) | [ReferenceError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) |
| [`RangeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) | Invalid array length, negative `ArrayBuffer` size, out-of-range numeric conversions, call stack depth exceeded (`--stack-size`) | [RangeError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) |
| [`SyntaxError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) | Invalid syntax detected by the parser or lexer; also throwable at runtime via `new SyntaxError(...)` | [SyntaxError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) |
| [`URIError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) | Malformed URI passed to `encodeURI`, `decodeURI`, `encodeURIComponent`, or `decodeURIComponent` | [URIError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) |
| [`AggregateError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AggregateError) | Multiple errors wrapped together; used by `Promise.any` when all promises reject | [AggregateError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AggregateError) |
| [`SuppressedError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SuppressedError) | Disposal error during explicit resource management (`using`/`await using`); wraps both the new and suppressed error | [SuppressedError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SuppressedError) |
| `TimeoutError` | Execution exceeded the `--timeout` limit (CLI only; not a JS-visible constructor) | -- |

### Inheritance

All error types follow the standard prototype chain:

```javascript
const e = new TypeError("bad type");
e instanceof TypeError; // true
e instanceof Error;     // true
e instanceof RangeError; // false
```

Errors thrown internally by the engine (e.g., property access on `null`) use the same prototype chain as user-constructed errors, so `instanceof` checks work consistently in `catch` blocks.

### Error.isError

GocciaScript implements the ES2026 [`Error.isError`](https://tc39.es/ecma262/#sec-error.iserror) static method, which checks for the internal `[[ErrorData]]` slot:

```javascript
Error.isError(new TypeError("x")); // true
Error.isError({ message: "fake" }); // false — plain objects are not errors
```

## Parser Error Display

When the parser encounters invalid syntax, it displays a detailed error with source context. The format includes the error name, message, file location, the offending source line, and a caret (`^`) pointing to the exact column:

```text
SyntaxError: Expected ';' after expression
  --> script.js:3:12
   1 | const x = 1
   2 | const y = 2
   3 | const z = 3 + +
                     ^
   4 | console.log(z);
```

Up to 2 lines of context are shown before and after the error line.

### Suggestions

Some parser errors include a suggestion line that recommends an alternative. GocciaScript intentionally excludes certain JavaScript features and uses suggestions to guide users toward the supported alternatives:

```text
SyntaxError: 'var' declarations are not supported in GocciaScript
  Suggestion: Use 'let' or 'const' instead
  --> script.js:1:1
   1 | var x = 42;
     ^
```

Other features that produce suggestions include:

- `var` declarations -- suggests `let` or `const`
- `function` declarations and expressions -- suggests arrow functions
- `==` / `!=` (loose equality) -- suggests `===` / `!==`
- Traditional loops (`for`, `while`, `do...while`) -- suggests `for...of` or array methods
- `with` statements -- no alternative (excluded for security)
- Default imports/exports -- suggests named imports/exports

See [Language](language.md) for the full list of excluded features and their rationale.

## Runtime Error Display

When an uncaught runtime error reaches the top level, GocciaScript displays the error with the same source-context format used by parser errors. The stack trace from the error's `stack` property is used to locate the offending line in source:

```text
TypeError: Cannot read properties of undefined (reading 'x')
  --> script.js:5:10
   3 | const getX = (obj) => {
   4 |   return obj.x;
   5 |   return obj.nested.x;
               ^
   6 | };
   7 | getX(undefined);
```

Stdin input uses `<stdin>` as the filename and retains full source context for error display. If source context is not available (e.g., errors originating inside native built-in callbacks without a JavaScript source location), GocciaScript falls back to displaying the stack trace string.

## Error Properties

Every error object has the following properties:

| Property | Type | Description |
|----------|------|-------------|
| `name` | `string` | Error type name (e.g., `"TypeError"`, `"RangeError"`) |
| `message` | `string` | Human-readable error description |
| `stack` | `string` | Formatted stack trace (see [Stack Traces](#stack-traces)) |
| `cause` | any | Optional; present only when constructed with `{ cause }` option (see [Error.cause](#errorcause)) |

`AggregateError` adds:

| Property | Type | Description |
|----------|------|-------------|
| `errors` | `Array` | The array of errors passed to the constructor |

`SuppressedError` adds:

| Property | Type | Description |
|----------|------|-------------|
| `error` | any | The error that triggered the suppression |
| `suppressed` | any | The original error that was suppressed |

### Stack Traces

The `stack` property contains a V8-style formatted string with the error header followed by `at` frames:

```text
TypeError: Cannot read properties of null
    at inner (script.js:2:10)
    at middle (script.js:5:3)
    at outer (script.js:8:3)
```

Each frame shows the function name (or `<anonymous>`), the file path, and the line and column number. Frames are listed from innermost (most recent) to outermost.

## Error.cause

Most error constructors accept an options object with a `cause` property, following [ES2022 Error Cause](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/cause). The options argument position varies by constructor: second for `Error`, `TypeError`, `RangeError`, `ReferenceError`, `SyntaxError`, and `URIError`; third for `AggregateError` (`new AggregateError(errors, message, options)`); fourth for `SuppressedError` (`new SuppressedError(error, suppressed, message, options)`).

```javascript
const original = new Error("disk full");
const wrapped = new Error("save failed", { cause: original });

wrapped.message;        // "save failed"
wrapped.cause.message;  // "disk full"
```

Error cause chaining works across error types:

```javascript
const root = new RangeError("out of bounds");
const mid = new TypeError("invalid type", { cause: root });
const top = new Error("operation failed", { cause: mid });

top.cause.cause.message; // "out of bounds"
```

The `cause` property:

- Can be any value (string, number, object, another error, `null`, `undefined`)
- Is writable and configurable but **not enumerable**
- Is only present when the options object has a `cause` property (not present by default)
- Works with all error types: `Error`, `TypeError`, `RangeError`, `ReferenceError`, `SyntaxError`, `URIError`, `AggregateError`, and `SuppressedError`

## try / catch / finally

Error handling follows standard ECMAScript semantics. See [Language](language.md) for the full syntax reference.

### Basic try/catch

```javascript
try {
  const x = null;
  x.property; // throws TypeError
} catch (e) {
  console.log(e.name);    // "TypeError"
  console.log(e.message); // "Cannot read properties of null (reading 'property')"
}
```

### Optional catch binding

The catch parameter can be omitted (ES2019+):

```javascript
try {
  riskyOperation();
} catch {
  console.log("something went wrong");
}
```

### finally

The `finally` block always runs, whether or not an error was thrown:

```javascript
try {
  return computeResult();
} finally {
  cleanup(); // runs even when try returns
}
```

Per the spec, if `finally` contains a `return`, `throw`, or `break`, it overrides the try/catch result.

### Throwing any value

GocciaScript allows throwing any value, not just error objects:

```javascript
try {
  throw "string error";
} catch (e) {
  console.log(e); // "string error"
}

try {
  throw 42;
} catch (e) {
  console.log(e); // 42
}
```

### Type-checking caught errors

Use `instanceof` to differentiate error types in a catch block:

```javascript
try {
  someOperation();
} catch (e) {
  if (e instanceof TypeError) {
    console.log("type error:", e.message);
  } else if (e instanceof RangeError) {
    console.log("range error:", e.message);
  } else {
    throw e; // rethrow unknown errors
  }
}
```

## SuppressedError and Explicit Resource Management

When using `using` or `await using` declarations for [explicit resource management](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/using), if both the block body and a resource's `[Symbol.dispose]()` method throw, the runtime wraps both errors in a `SuppressedError`:

```javascript
let caught;
try {
  using resource = {
    [Symbol.dispose]() { throw new Error("disposal failed"); }
  };
  throw new Error("block failed");
} catch (e) {
  caught = e;
}

caught instanceof SuppressedError; // true
caught.error.message;              // "disposal failed"
caught.suppressed.message;         // "block failed"
```

If multiple disposals fail, errors are chained: each new disposal error wraps the previous `SuppressedError` as its `suppressed` property.

`SuppressedError` can also be constructed directly:

```javascript
const err = new SuppressedError(
  new Error("new"),       // error
  new Error("old"),       // suppressed
  "An error was suppressed" // message (optional)
);
err.name;       // "SuppressedError"
err.error;      // Error: new
err.suppressed; // Error: old
err.message;    // "An error was suppressed"
```

## JSON Output Format

When running with `--output=json`, GocciaScript wraps every execution result in a structured JSON envelope. This is useful for programmatic consumers and embedding scenarios.

### Success

```json
{
  "ok": true,
  "build": {
    "version": "0.1.0-dev",
    "date": "2026-04-27",
    "commit": "abc1234",
    "os": "darwin",
    "arch": "aarch64"
  },
  "stdout": "hello\n",
  "stderr": "",
  "output": ["hello"],
  "error": null,
  "timing": {
    "lex_ns": 500000,
    "parse_ns": 1200000,
    "compile_ns": 0,
    "exec_ns": 3100000,
    "total_ns": 4800000
  },
  "memory": {
    "gc": {
      "liveBytes": 2048,
      "startLiveBytes": 0,
      "endLiveBytes": 2048,
      "peakLiveBytes": 4096,
      "deltaLiveBytes": 2048,
      "allocatedDuringRunBytes": 4096,
      "limitBytes": 536870912,
      "startObjectCount": 0,
      "endObjectCount": 24,
      "collections": 0,
      "collectedObjects": 0
    },
    "heap": {
      "startAllocatedBytes": 16384,
      "endAllocatedBytes": 32768,
      "deltaAllocatedBytes": 16384,
      "startFreeBytes": 8192,
      "endFreeBytes": 4096,
      "deltaFreeBytes": -4096
    }
  },
  "workers": { "used": 1, "available": 1, "parallel": false },
  "files": [
    {
      "fileName": "script.js",
      "ok": true,
      "stdout": "hello\n",
      "stderr": "",
      "output": ["hello"],
      "error": null,
      "result": 42,
      "timing": {
        "lex_ns": 500000,
        "parse_ns": 1200000,
        "compile_ns": 0,
        "exec_ns": 3100000,
        "total_ns": 4800000
      },
      "memory": { "gc": { "liveBytes": 2048 }, "heap": { "endAllocatedBytes": 32768 } }
    }
  ]
}
```

### Error

```json
{
  "ok": false,
  "build": {
    "version": "0.1.0-dev",
    "date": "2026-04-27",
    "commit": "abc1234",
    "os": "darwin",
    "arch": "aarch64"
  },
  "stdout": "",
  "stderr": "",
  "output": [],
  "error": {
    "type": "TypeError",
    "message": "Cannot read properties of null (reading 'x')",
    "line": 5,
    "column": 10,
    "fileName": "script.js"
  },
  "timing": {
    "lex_ns": 500000,
    "parse_ns": 1200000,
    "compile_ns": 0,
    "exec_ns": 100000,
    "total_ns": 1800000
  },
  "memory": { "gc": { "liveBytes": 2048 }, "heap": { "endAllocatedBytes": 32768 } },
  "workers": { "used": 1, "available": 1, "parallel": false },
  "files": [
    {
      "fileName": "script.js",
      "ok": false,
      "stdout": "",
      "stderr": "",
      "output": [],
      "error": {
        "type": "TypeError",
        "message": "Cannot read properties of null (reading 'x')",
        "line": 5,
        "column": 10,
        "fileName": "script.js"
      },
      "result": null,
      "timing": {
        "lex_ns": 500000,
        "parse_ns": 1200000,
        "compile_ns": 0,
        "exec_ns": 100000,
        "total_ns": 1800000
      },
      "memory": { "gc": { "liveBytes": 2048 }, "heap": { "endAllocatedBytes": 32768 } }
    }
  ]
}
```

| Field | Type | Description |
|-------|------|-------------|
| `ok` | `boolean` | `true` for success, `false` for error |
| `build` | `object` | Build identity, including `version`, `date`, `commit`, `os`, and `arch` |
| `stdout` | `string` | Unformatted stdout-oriented console output; present even when empty |
| `stderr` | `string` | Unformatted stderr-oriented console output; present even when empty |
| `output` | `string[]` | Formatted console output split into lines |
| `error` | `object \| null` | First failed file's error details, or `null` when the run succeeds |
| `error.type` | `string` | Error type name (`"TypeError"`, `"SyntaxError"`, `"TimeoutError"`, etc.) |
| `error.message` | `string` | Error message text |
| `error.line` | `number \| null` | Source line number (1-based), or `null` if unavailable |
| `error.column` | `number \| null` | Source column number (1-based), or `null` if unavailable |
| `error.fileName` | `string \| null` | Source file path, or `null` if unavailable |
| `timing` | `object` | Cumulative phase-level timings in nanoseconds (`*_ns`) |
| `memory` | `object \| null` | GC and application heap measurements for the run |
| `memory.gc.liveBytes` | `number` | GC-managed bytes live at the measurement endpoint. This is the report equivalent of `Goccia.gc.bytesAllocated` |
| `memory.gc.allocatedDuringRunBytes` | `number` | Total GC-managed bytes allocated during the measured run, including allocations later collected |
| `memory.gc.peakLiveBytes` | `number` | Highest live GC-managed byte count observed during the measurement |
| `memory.gc.limitBytes` | `number` | Active GC byte ceiling from `--max-memory` or the auto-detected default |
| `memory.heap.deltaFreeBytes` | `number` | Change in FreePascal memory-manager free space. Negative values are valid and mean the process heap had less reusable free space at the end |
| `workers` | `object` | Worker logistics: used worker count, available worker count, and whether the run was parallel |
| `files` | `object[]` | Per-input results. Single-file runs use the same structure with one element |
| `files[].fileName` | `string` | Input file path or `<stdin>` |
| `files[].result` | any | The script completion value for that input. Serializes as `null` for both errors and JavaScript `undefined`; use `files[].ok` and `files[].error` to distinguish those cases. |

### TimeoutError in JSON

When execution exceeds the `--timeout` limit, the JSON envelope reports a `TimeoutError`:

```json
{
  "ok": false,
  "build": { "version": "0.1.0-dev", "date": "2026-04-27", "commit": "abc1234", "os": "darwin", "arch": "aarch64" },
  "stdout": "",
  "stderr": "",
  "output": [],
  "error": {
    "type": "TimeoutError",
    "message": "Execution timed out after 100ms",
    "line": null,
    "column": null,
    "fileName": null
  },
  "timing": { "lex_ns": 100000, "parse_ns": 200000, "compile_ns": 0, "exec_ns": 100000000, "total_ns": 100300000 },
  "memory": {
    "gc": {
      "liveBytes": 8192,
      "startLiveBytes": 0,
      "endLiveBytes": 8192,
      "peakLiveBytes": 16384,
      "deltaLiveBytes": 8192,
      "allocatedDuringRunBytes": 16384,
      "limitBytes": 536870912,
      "startObjectCount": 0,
      "endObjectCount": 80,
      "collections": 0,
      "collectedObjects": 0
    },
    "heap": {
      "startAllocatedBytes": 16384,
      "endAllocatedBytes": 32768,
      "deltaAllocatedBytes": 16384,
      "startFreeBytes": 8192,
      "endFreeBytes": 4096,
      "deltaFreeBytes": -4096
    }
  },
  "workers": { "used": 1, "available": 1, "parallel": false },
  "files": [
    {
      "fileName": "<stdin>",
      "ok": false,
      "stdout": "",
      "stderr": "",
      "output": [],
      "error": {
        "type": "TimeoutError",
        "message": "Execution timed out after 100ms",
        "line": null,
        "column": null,
        "fileName": null
      },
      "result": null,
      "timing": { "lex_ns": 100000, "parse_ns": 200000, "compile_ns": 0, "exec_ns": 100000000, "total_ns": 100300000 },
      "memory": { "gc": { "liveBytes": 8192 }, "heap": { "endAllocatedBytes": 32768 } }
    }
  ]
}
```

## Embedding

For Pascal-side error handling when embedding GocciaScript in FreePascal applications, see [Embedding the Engine](embedding.md). The engine raises `TGocciaError` subclasses (`TGocciaSyntaxError`, `TGocciaTypeError`, `TGocciaReferenceError`) on the Pascal side and `TGocciaThrowValue` for JS-level `throw` statements.

## Related Documents

- [Language](language.md) -- Supported features, excluded features, and rationale
- [Built-in Objects](built-ins.md) -- Error constructors and API reference
- [Embedding the Engine](embedding.md) -- Pascal-side error handling for embedders
- [Testing](testing.md) -- Writing tests that assert on error behavior
