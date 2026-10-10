# JSON Output

*The `--output=json` and `--output=compact-json` envelopes: their success and error shapes, and how a timeout is reported.*

## Executive Summary

- **Envelope**: every run is wrapped in one JSON object with its result, timing, memory, output and, on failure, the `error`.
- **Errors**: `error` carries the type, message, file, line and column of the thrown value; see [Errors](errors.md) for how they are displayed.
- **Compact form**: `--output=compact-json` prints the same envelope without the `build`, `memory`, `stdout` and `stderr` fields.

## Overview

When running with `--output=json`, GocciaScript wraps every execution result in a structured JSON envelope. This is useful for programmatic consumers and embedding scenarios.

The `memory` block has two different scopes:

- `memory.gc` reports the GocciaScript GC's approximate managed-object accounting. It tracks `TGCManagedObject.InstanceSize`, not all memory held by strings, dynamic arrays, or the FreePascal runtime. `allocatedDuringRunBytes` is cumulative allocation churn during the measured run, so it can be much larger than `liveBytes`.
- `memory.heap` reports coarse FreePascal process heap-manager counters from `GetHeapStatus`. These are allocator diagnostics, not JavaScript heap size. `deltaFreeBytes` may be negative when the process heap has less reusable free space at the end of the run.

For parallel runs, the top-level `memory.gc` block combines one measurement per worker thread plus the main thread. It does not sum per-file live snapshots, because each worker can process many files with the same thread-local GC. Per-file `files[].memory` is only populated by hosts that can measure a file independently.

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
      "timing": {
        "lex_ns": 500000,
        "parse_ns": 1200000,
        "compile_ns": 0,
        "exec_ns": 3100000,
        "total_ns": 4800000
      },
      "memory": null,
      "result": 42
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
    "line": 2,
    "column": 13,
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
        "line": 2,
        "column": 13,
        "fileName": "script.js"
      },
      "timing": {
        "lex_ns": 500000,
        "parse_ns": 1200000,
        "compile_ns": 0,
        "exec_ns": 100000,
        "total_ns": 1800000
      },
      "memory": null,
      "result": null
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
| `error.type` | `string` | Error type name (`"TypeError"`, `"SyntaxError"`, `"TimeoutError"`, `"MemoryLimitError"`, etc.) |
| `error.message` | `string` | Error message text |
| `error.line` | `number \| null` | Source line number (1-based), or `null` if unavailable. For a parse error, where parsing failed. For a thrown error, where the engine recorded the error was created, the location the human-readable `-->` line shows. A thrown value that is not an engine-created error has no location, and neither does an error the interpreter raises at the top level ([#1273](https://github.com/frostney/GocciaScript/issues/1273)) |
| `error.column` | `number \| null` | Source column number (1-based), or `null` if unavailable, as for `error.line` |
| `error.fileName` | `string \| null` | Source file path, or `null` if unavailable. For a thrown error with a recorded location, the file that location is in, which can be a module the input imported |
| `timing` | `object` | Cumulative phase-level timings in nanoseconds (`*_ns`) |
| `memory` | `object \| null` | GC and application heap measurements for the run |
| `memory.gc.liveBytes` | `number` | GC-managed bytes live at the measurement endpoint. This is the report equivalent of `Goccia.gc.bytesAllocated` |
| `memory.gc.allocatedDuringRunBytes` | `number` | Total GC-managed bytes allocated during the measured run, including allocations later collected |
| `memory.gc.peakLiveBytes` | `number` | Highest live GC-managed byte count observed during the measurement |
| `memory.gc.limitBytes` | `number` | Active GC byte ceiling from `--max-memory` or the auto-detected default |
| `memory.heap.deltaAllocatedBytes` | `number` | Change in FreePascal heap-manager allocated bytes for the measured process/thread scope |
| `memory.heap.deltaFreeBytes` | `number` | Change in FreePascal memory-manager free space. Negative values are valid and mean the process heap had less reusable free space at the end |
| `workers` | `object` | Worker logistics: used worker count, available worker count, and whether the run was parallel |
| `files` | `object[]` | Per-input results. Single-file runs use the same structure with one element |
| `files[].fileName` | `string` | Input file path or `<stdin>` |
| `files[].result` | any | The script completion value for that input. Serializes as `null` for both errors and JavaScript `undefined`; use `files[].ok` and `files[].error` to distinguish those cases. |

### Compact JSON Output

`--output=compact-json` emits the same envelope as `--output=json` with the `build`, `memory`, `stdout`, and `stderr` fields omitted at both the top level and per-file. All console output is still available through the normalized `output` array (lines from `console.log`/`info`/`debug` and prefixed lines like `Error: ...` or `Warning: ...` from `console.error`/`warn`); script errors remain available through the structured `error` object. Use this format when you do not need build identity, memory measurements, or the raw stdout/stderr split — and want a smaller payload.

```json
{
  "ok": true,
  "output": ["hello", "Error: oops"],
  "error": null,
  "timing": {
    "lex_ns": 500000,
    "parse_ns": 1200000,
    "compile_ns": 0,
    "exec_ns": 3100000,
    "total_ns": 4800000
  },
  "workers": { "used": 1, "available": 1, "parallel": false },
  "files": [
    {
      "fileName": "script.js",
      "ok": true,
      "output": ["hello", "Error: oops"],
      "error": null,
      "timing": {
        "lex_ns": 500000,
        "parse_ns": 1200000,
        "compile_ns": 0,
        "exec_ns": 3100000,
        "total_ns": 4800000
      },
      "result": 42
    }
  ]
}
```

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
    "message": "file timed out after 100ms",
    "line": null,
    "column": null,
    "fileName": "<stdin>"
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
        "message": "file timed out after 100ms",
        "line": null,
        "column": null,
        "fileName": "<stdin>"
      },
      "timing": { "lex_ns": 100000, "parse_ns": 200000, "compile_ns": 0, "exec_ns": 100000000, "total_ns": 100300000 },
      "memory": null,
      "result": null
    }
  ]
}
```

## Related Documents

- [Errors](errors.md) -- Error types, display, and stack traces
- [Build System](build-system.md) -- `--output` and the other runner options
